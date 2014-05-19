# Initial ARM port only targets QEMU.
set remote Z-packet on
set tdesc filename ../barrelfish/tools/arm_gem5/arm-with-neon.xml
#file arm_gem5_kernel
target remote localhost:7000
display/i $pc

# -- Procedures

define show-process

    p (char*)(((struct dispatcher_shared_generic*)$r9)->name)
 
end

define change-process

    ## Load symbols for process shown by `show-process'. 
    ## NB User has to prefix process name with path, e.g. arm/sbin/mem_serv.
    ## XXX No string concat in gdb (?). alt is use gdb with python support.

    #-- Flush old symbols
    symbol-file
    #-- Reload cpu driver symbols
    file armv7/sbin/cpu_arm_gem5
    #-- Add process symbols at default process load address
    add-symbol-file $arg0 0x00400000

end

# -- Misc 

add-symbol-file armv7/sbin/cpu_arm_gem5 0x81ff0000
#add-symbol-file arm_gem5/sbin/monitor 0x400000
#set kernel_log_subsystem_mask = 0x7fffffff


break cp15_enable_mmu
#break panic
#break user_panic
#break err_push

#break caps_retype
#break caps_map_l1
#break caps_map_l2

#break create_modules_from_initrd
#commands
#    set kernel_loglevel = 0x7fffff
#    set kernel_log_subsystem_mask = 0x7fffff
#    cont
#end




