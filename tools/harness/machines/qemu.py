##########################################################################
# Copyright (c) 2009, 2010, 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, signal, tempfile, subprocess, shutil
import debug, machines
from machines import Machine

GRUB_IMAGE_PATH = 'tools/grub-qemu.img' # relative to source tree
QEMU_CMD_X64 = 'qemu-system-x86_64'
QEMU_CMD_X32 = 'qemu-system-i386'
QEMU_CMD_ARM = 'qemu-system-arm'
QEMU_ARGS_GENERIC = '-nographic -no-reboot'.split()
QEMU_ARGS_X64 = '-net nic,model=ne2k_pci -net user -m 512'.split()
QEMU_ARGS_X32 = '-net nic,model=ne2k_pci -net user -m 512'.split()
QEMU_ARGS_ARM = '-m 256'.split()

class QEMUMachineBase(Machine):
    def __init__(self, options):
        super(QEMUMachineBase, self).__init__(options)
        self.child = None
        self.tftp_dir = None
        self.options = options

    def get_coreids(self):
        return range(0, self.get_ncores())

    def get_tickrate(self):
        return None

    def get_boot_timeout(self):
        # shorter time limit for running a qemu test
        # FIXME: ideally this should somehow be expressed in CPU time / cycles
        return 60

    def get_tftp_dir(self):
        if self.tftp_dir is None:
            debug.verbose('creating temporary directory for QEMU TFTP files')
            self.tftp_dir = tempfile.mkdtemp(prefix='harness_qemu_')
            debug.verbose('QEMU TFTP directory is %s' % self.tftp_dir)
        return self.tftp_dir

    def _write_menu_lst(self, data, path):
        debug.verbose('writing %s' % path)
        debug.debug(data)
        f = open(path, 'w')
        f.write(data)
        f.close()

    def set_bootmodules(self, modules):
        path = os.path.join(self.get_tftp_dir(), 'menu.lst')
        self._write_menu_lst(modules.get_menu_data('/'), path)

    def lock(self):
        pass

    def unlock(self):
        pass

    def setup(self):
        pass

    def _get_cmdline(self):
        raise NotImplementedError

    def _kill_child(self):
        # terminate child if running
        if self.child:
            os.kill(self.child.pid, signal.SIGTERM)
            self.child.wait()
            self.child = None

    def reboot(self):
        self._kill_child()
        cmd = self._get_cmdline()
        debug.verbose('starting "%s"' % ' '.join(cmd))
        devnull = open(os.devnull, 'r')
        self.child = subprocess.Popen(cmd, stdout=subprocess.PIPE, stdin=devnull)
        devnull.close()

    def shutdown(self):
        self._kill_child()
        # try to cleanup tftp tree if needed
        if self.tftp_dir and os.path.isdir(self.tftp_dir):
            shutil.rmtree(self.tftp_dir, ignore_errors=True)
        self.tftp_dir = None

    def get_output(self):
        return self.child.stdout

class QEMUMachineX64(QEMUMachineBase):
    def _get_cmdline(self):
        grub_image = os.path.join(self.options.sourcedir, GRUB_IMAGE_PATH)
        s = '-smp %d -fda %s -tftp %s' % (self.get_ncores(), grub_image,
                                          self.get_tftp_dir())
        return [QEMU_CMD_X64] + QEMU_ARGS_GENERIC + QEMU_ARGS_X64 + s.split()

    def get_bootarch(self):
        return "x86_64"

class QEMUMachineX32(QEMUMachineBase):
    def _get_cmdline(self):
        grub_image = os.path.join(self.options.sourcedir, GRUB_IMAGE_PATH)
        s = '-smp %d -fda %s -tftp %s' % (self.get_ncores(), grub_image,
                                          self.get_tftp_dir())
        return [QEMU_CMD_X32] + QEMU_ARGS_GENERIC + QEMU_ARGS_X32 + s.split()

    def get_bootarch(self):
        return "x86_32"

@machines.add_machine
class QEMUMachineUniproc(QEMUMachineX64):
    '''Uniprocessor x86_64 QEMU'''
    name = 'qemu1'

    def get_ncores(self):
        return 1

@machines.add_machine
class QEMUMachineMultiproc(QEMUMachineX64):
    '''Multiprocessor x86_64 QEMU (4 CPUs)'''
    name = 'qemu4'

    def get_ncores(self):
        return 4

@machines.add_machine
class QEMUMachineX32Uniproc(QEMUMachineX32):
    '''Uniprocessor x86_32 QEMU'''
    name = 'qemu1_x32'

    def get_ncores(self):
        return 1

@machines.add_machine
class QEMUMachineX32Multiproc(QEMUMachineX32):
    '''Multiprocessor x86_32 QEMU (4 CPUs)'''
    name = 'qemu4_x32'

    def get_ncores(self):
        return 4

@machines.add_machine
class QEMUMachineARMUniproc(QEMUMachineBase):
    '''Uniprocessor ARM QEMU (currently only capable of running the kernel)'''
    name = 'qemu_armv5'

    def get_ncores(self):
        return 1

    def get_bootarch(self):
        return "armv5"

    def set_bootmodules(self, modules):
        # store path to kernel for _get_cmdline to use
        tftp_dir = self.get_tftp_dir()
        self.kernel_img = os.path.join(tftp_dir, modules.kernel[0])

        # write menu.lst
        menu_lst = 'armv5/menu.lst'
        menu_lst_path = os.path.join(tftp_dir, menu_lst)
        self._write_menu_lst(modules.get_menu_data('/'), menu_lst_path)

        # produce ROM image
        rom_name = 'romfs.cpio'
        self.rom_img = os.path.join(tftp_dir, rom_name)
        cmd = os.path.join(self.options.sourcedir, 'tools', 'arm-mkbootcpio.sh')
        debug.checkcmd([os.path.abspath(cmd), menu_lst, rom_name], cwd=tftp_dir)

    def _get_cmdline(self):
        return ([QEMU_CMD_ARM] + QEMU_ARGS_GENERIC + QEMU_ARGS_ARM
                + ['-kernel', self.kernel_img, '-initrd', self.rom_img])

@machines.add_machine
class QEMUMachineARMUniproc(QEMUMachineBase):
    '''Uniprocessor Netronome (currently only good for building)'''
    name = 'qemu_xscale'

    def get_ncores(self):
        return 1

    def get_bootarch(self):
        return "xscale"

    def _get_cmdline(self):
        raise NotImplementedError

@machines.add_machine
class QEMUMachineSCC(QEMUMachineBase):
    '''SCC-like QEMU'''
    name = 'qemu_scc'

    def _get_cmdline(self):
        raise NotImplementedError

    def get_bootarch(self):
        return "scc"
