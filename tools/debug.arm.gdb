# Initial ARM port only targets QEMU.
target remote localhost:1234
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
    file arm/sbin/cpu
    #-- Add process symbols at default process load address
    add-symbol-file $arg0 0x00400000

end

# -- Misc 

#add-symbol-file arm/sbin/init 0x400000
#set kernel_log_subsystem_mask = 0x7fffffff

break panic
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




