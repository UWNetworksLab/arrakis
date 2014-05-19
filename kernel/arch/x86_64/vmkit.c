/**
 * \file
 * \brief VMKit Kernel interface.
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <vmkit.h>
#include <x86.h>
#include <dispatch.h>
#include <exec.h>
#include <barrelfish_kpi/vmkit.h>
#include <barrelfish_kpi/syscalls.h>

#include <amd_vmcb_dev.h>

// SVM relevant CPUID info
#define CPUID_AMD_EXTFEAT       0x80000001
#define AMD_EXTFEAT_ECX_SVM     (1 << 2)

// some EXITCODE values
#define VMEXIT_INTR     0x60
#define VMEXIT_NMI      0x61
#define VMEXIT_SMI      0x62
#define VMEXIT_VMMCALL  0x81

/**
 * \brief The storage area where SVM puts the host state during guest exec.
 */
static uint8_t host_save_area[BASE_PAGE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

/**
 * \brief VMCB for the host to save its state.
 */
static uint8_t host_vmcb[BASE_PAGE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

static void
vmkit_init (void)
{
    static bool executed = false;

    if (executed) {
        return;
    }

    executed = true;
    memset(host_save_area, 0x0, BASE_PAGE_SIZE);
    memset(host_vmcb, 0x0, BASE_PAGE_SIZE);
}

/**
 * \brief Tries to enable hardware assisted virtualization.
 *
 * Checks whether hardware assisted virtualization is available on the platform
 * and enables this feature.
 *
 * \Return Returns VMKIT_ERR_OK on successful initialization of the subsystem
 *         or VMKIT_ERR_UNAVAIL if virtualization is unavailable.
 */
errval_t
vmkit_enable_virtualization (void)
{
    vmkit_init ();

    // first check what CPUID tells us about SVM support
    uint32_t cpuid_ecx;
    cpuid(CPUID_AMD_EXTFEAT, NULL, NULL, &cpuid_ecx, NULL);
    if (!(cpuid_ecx & AMD_EXTFEAT_ECX_SVM)) {
        return SYS_ERR_VMKIT_UNAVAIL;
    }

    // check whether SVM support is deactivated
    uint64_t msr_vmcr = rdmsr(MSR_AMD_VMCR);
    if (msr_vmcr & AMD_VMCR_SVMDIS) {
        return SYS_ERR_VMKIT_UNAVAIL;
    }

    // from here on we assume that SVM is avail and may be enabled

    // check whether SVM is already enabled
    uint64_t msr_efer = rdmsr(MSR_IA32_EFER);
    if (msr_efer & IA32_EFER_SVME) {
        // SVM is already enabled
        return SYS_ERR_OK;
    }
    // enable SVM
    addmsr(MSR_IA32_EFER, IA32_EFER_SVME);
    // check whether SVM is now enabled
    msr_efer = rdmsr(MSR_IA32_EFER);
    if (msr_efer & IA32_EFER_SVME) {
        // SVM enabled
        // set the host save area
        wrmsr(MSR_AMD_VM_HSAVE, mem_to_local_phys((lvaddr_t)host_save_area));
        return SYS_ERR_OK;
    } else {
        printk(LOG_WARN, "VMKit: Unable to enable SVM although the hardware "
               "claims to support it.\n");
        return SYS_ERR_VMKIT_UNAVAIL;
    }
}

static inline void
vm_exec (struct dcb *dcb)
{
    lpaddr_t lpaddr = gen_phys_to_local_phys(dcb->guest_desc.ctrl.cap.u.frame.base);
    struct guest_control *ctrl = (void *)local_phys_to_mem(lpaddr);
    register uintptr_t rbx __asm("rbx") = ctrl->regs.rbx;
    register uintptr_t rcx __asm("rcx") = ctrl->regs.rcx;
    register uintptr_t rdx __asm("rdx") = ctrl->regs.rdx;
    register uintptr_t rsi __asm("rsi") = ctrl->regs.rsi;
    register uintptr_t rdi __asm("rdi") = ctrl->regs.rdi;
    register uintptr_t r8  __asm("r8")  = ctrl->regs.r8;
    register uintptr_t r9  __asm("r9")  = ctrl->regs.r9;
    register uintptr_t r10 __asm("r10") = ctrl->regs.r10;
    register uintptr_t r11 __asm("r11") = ctrl->regs.r11;
    register uintptr_t r12 __asm("r12") = ctrl->regs.r12;
    register uintptr_t r13 __asm("r13") = ctrl->regs.r13;
    register uintptr_t r14 __asm("r14") = ctrl->regs.r14;
    register uintptr_t r15 __asm("r15") = ctrl->regs.r15;
#ifdef NDEBUG
    register uintptr_t rbp __asm("rbp") = ctrl->regs.rbp;

    __asm volatile ("sti\n\t"       // allow intr to happen inside the host
                    "vmrun\n\t"     // execute the guest
                    "cli\n\t"       // disable intr in the host again
                    "stgi\n\t"      // enable the global intr flag
        : "+r" (rbx), "+r" (rcx), "+r" (rdx), "+r" (rbp), "+r" (rsi), "+r" (rdi),
          "+r" (r8), "+r" (r9), "+r" (r10), "+r" (r11), "+r" (r12), "+r" (r13),
          "+r" (r14), "+r" (r15)
        : "a" (dcb->guest_desc.vmcb.cap.u.frame.base)
        : "memory");
#else
    static uintptr_t rbp, srbp;

    rbp = ctrl->regs.rbp;

    __asm volatile ("mov %%rbp, %[srbp]\n\t" :: [srbp] "m" (srbp));

    __asm volatile ("mov %[nrbp], %%rbp\n\t"
                    "sti\n\t"       // allow intr to happen inside the host
                    "vmrun\n\t"     // execute the guest
                    "cli\n\t"       // disable intr in the host again
                    "stgi\n\t"      // enable the global intr flag
                    "mov %%rbp, %[nrbp]\n\t"
        : "+r" (rbx), "+r" (rcx), "+r" (rdx), [nrbp] "+m" (rbp),
                    "+r" (rsi), "+r" (rdi), "+r" (r8), "+r" (r9), "+r" (r10),
                    "+r" (r11), "+r" (r12), "+r" (r13), "+r" (r14), "+r" (r15)
        : "a" (dcb->guest_desc.vmcb.cap.u.frame.base)
        : "memory");

    __asm volatile ("mov %[srbp], %%rbp\n\t"
                    : [srbp] "+m" (srbp));
#endif

    ctrl->regs.rbx = rbx;
    ctrl->regs.rcx = rcx;
    ctrl->regs.rdx = rdx;
    ctrl->regs.rbp = rbp;
    ctrl->regs.rsi = rsi;
    ctrl->regs.rdi = rdi;
    ctrl->regs.r8 = r8;
    ctrl->regs.r9 = r9;
    ctrl->regs.r10 = r10;
    ctrl->regs.r11 = r11;
    ctrl->regs.r12 = r12;
    ctrl->regs.r13 = r13;
    ctrl->regs.r14 = r14;
    ctrl->regs.r15 = r15;
}

static inline void
vmload (lpaddr_t vmcb) {
    __asm volatile ("vmload" : : "a" (vmcb) : "memory");
}

static inline void
vmsave (lpaddr_t vmcb) {
    __asm volatile ("vmsave" : : "a" (vmcb) : "memory");
}

static inline void
vmkit_switch_to (struct dcb *dcb)
{
    assert(dcb != NULL);
    assert(dcb->is_vm_guest);

    // save the host state
    vmsave(mem_to_local_phys((lvaddr_t)host_vmcb));
    // load the guest state
    vmload(gen_phys_to_local_phys(dcb->guest_desc.vmcb.cap.u.frame.base));
}

static inline void
vmkit_switch_from (struct dcb *dcb)
{
    assert(dcb != NULL);
    assert(dcb->is_vm_guest);

    // save the guest state
    vmsave(gen_phys_to_local_phys(dcb->guest_desc.vmcb.cap.u.frame.base));
    // load the host state
    vmload(mem_to_local_phys((lvaddr_t)host_vmcb));
}

void __attribute__ ((noreturn))
vmkit_vmexec (struct dcb *dcb, lvaddr_t entry)
{
  dispatcher_handle_t handle = dcb->disp;
  struct dispatcher_shared_generic *disp = get_dispatcher_shared_generic(handle);
  lpaddr_t lpaddr = gen_phys_to_local_phys(dcb->guest_desc.ctrl.cap.u.frame.base);
  struct guest_control *ctrl = (void *)local_phys_to_mem(lpaddr);
  lpaddr = gen_phys_to_local_phys(dcb->guest_desc.vmcb.cap.u.frame.base);
  amd_vmcb_t vmcb;
  amd_vmcb_initialize(&vmcb, (void *)local_phys_to_mem(lpaddr));

  memset(&ctrl->regs, 0, sizeof(struct registers_x86_64));
  ctrl->regs.rdi = disp->udisp;
  amd_vmcb_rip_wr(&vmcb, disp->dispatcher_run);
  amd_vmcb_rsp_wr(&vmcb, 0);
  amd_vmcb_rax_wr(&vmcb, 0);
  amd_vmcb_rflags_wr_raw(&vmcb, USER_RFLAGS);
  amd_vmcb_fs_selector_wr(&vmcb, 0);
  amd_vmcb_gs_selector_wr(&vmcb, 0);
  vmkit_vmenter(dcb);
}

struct sysret sys_syscall(uint64_t syscall, uint64_t arg0, uint64_t arg1,
                          uint64_t *args, uint64_t rflags, uint64_t rip);

extern uint64_t user_stack_save;

void __attribute__ ((noreturn))
vmkit_vmenter (struct dcb *dcb)
{
    lpaddr_t lpaddr = gen_phys_to_local_phys(dcb->guest_desc.ctrl.cap.u.frame.base);
    struct guest_control *ctrl = (void *)local_phys_to_mem(lpaddr);

    assert(dcb != NULL);
    assert(dcb->vspace != 0);
    assert(dcb->is_vm_guest);

    lpaddr = gen_phys_to_local_phys(dcb->guest_desc.vmcb.cap.u.frame.base);
    amd_vmcb_t vmcb;
    amd_vmcb_initialize(&vmcb, (void *)local_phys_to_mem(lpaddr));

    /* We need to set the page translation mode. If nested paging is disabled
     * then we need to set the guest cr3 to the value of the domains vspace. If
     * nested paging is enabled then we need to copy the domains vspace into the
     * ncr3 field of the vmcb. */
    if (amd_vmcb_np_rd(&vmcb).enable) {
        amd_vmcb_ncr3_wr(&vmcb, dcb->vspace);
    } else {
        amd_vmcb_cr3_wr(&vmcb, dcb->vspace);
    }

 vmenter_loop:

    /* printf("vmenter IN\n"); */

    // Enter the guest
    vmkit_switch_to(dcb);
    vm_exec(dcb);
    vmkit_switch_from(dcb);

    /* printf("vmenter OUT\n"); */

    // Here we exited the guest due to some intercept triggered a vm exit
    // our state is automatically restored by SVM

    uint64_t ec = amd_vmcb_exitcode_rd(&vmcb);
    /* We treat exits due to pysical interrupts (INTR, NMI, SMI) specially since
     * they need to be processed by the kernel interrupt service routines */
    switch(ec) {
    case VMEXIT_INTR:
    case VMEXIT_NMI:
    case VMEXIT_SMI:
      {
	arch_registers_state_t *area = NULL;

	/* printf("INT at %" PRIx64 "\n", amd_vmcb_rip_rd(&vmcb)); */

        ctrl->num_vm_exits_without_monitor_invocation++;

	// Store user state into corresponding save area
	if(dispatcher_is_disabled_ip(dcb->disp, amd_vmcb_rip_rd(&vmcb))) {
	  area = dispatcher_get_disabled_save_area(dcb->disp);
	  dcb->disabled = true;
	} else {
	  area = dispatcher_get_enabled_save_area(dcb->disp);
	  dcb->disabled = false;
	}
	memcpy(area, &ctrl->regs, sizeof(arch_registers_state_t));
	area->rax = amd_vmcb_rax_rd(&vmcb);
	area->rip = amd_vmcb_rip_rd(&vmcb);
	area->rsp = amd_vmcb_rsp_rd(&vmcb);
	area->eflags = amd_vmcb_rflags_rd_raw(&vmcb);
	area->fs = amd_vmcb_fs_selector_rd(&vmcb);
	area->gs = amd_vmcb_gs_selector_rd(&vmcb);

        // wait for interrupt will enable interrupts and therefore trigger their
        // corresponding handlers (which may be the monitor)
        wait_for_interrupt();
      }
      break;

    case VMEXIT_VMMCALL:
      {
	// Translate this to a SYSCALL
	struct registers_x86_64 *regs = &ctrl->regs;
	uint64_t args[10] = {
	  regs->r10, regs->r8, regs->r9, regs->r12, regs->r13, regs->r14,
	  regs->r15, amd_vmcb_rax_rd(&vmcb), regs->rbp, regs->rbx
	};

	/* printf("VMMCALL\n"); */

	// Advance guest RIP to next instruction
	amd_vmcb_rip_wr(&vmcb, amd_vmcb_rip_rd(&vmcb) + 3);
	user_stack_save = amd_vmcb_rsp_rd(&vmcb);

	struct sysret ret =
	  sys_syscall(regs->rdi, regs->rsi, regs->rdx, args,
		      amd_vmcb_rflags_rd_raw(&vmcb),
		      amd_vmcb_rip_rd(&vmcb));

	amd_vmcb_rax_wr(&vmcb, ret.error);
	regs->rdx = ret.value;
      }
      goto vmenter_loop;

    default:
        ctrl->num_vm_exits_with_monitor_invocation++;
        /* the guest exited not due to an interrupt but some condition the
         * monitor has to handle, therefore notify the monitor */

	/* printf("OTHER\n"); */

        assert(dcb->is_vm_guest);

        // disable the domain
        scheduler_remove(dcb);

        // call the monitor
        errval_t err = lmp_deliver_notification(&dcb->guest_desc.monitor_ep.cap);
        if (err_is_fail(err)) {
            printk(LOG_ERR, "Unexpected error delivering VMEXIT");
        }

        // run the monitor
        dispatch(dcb->guest_desc.monitor_ep.cap.u.endpoint.listener);
	break;
    }
}
