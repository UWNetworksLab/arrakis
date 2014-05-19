# BEGIN LICENSE BLOCK
# Version: CMPL 1.1
#
# The contents of this file are subject to the Cisco-style Mozilla Public
# License Version 1.1 (the "License"); you may not use this file except
# in compliance with the License.  You may obtain a copy of the License
# at www.eclipse-clp.org/license.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
# the License for the specific language governing rights and limitations
# under the License. 
# 
# The Original Code is  The ECLiPSe Constraint Logic Programming System. 
# The Initial Developer of the Original Code is  Cisco Systems, Inc. 
# Portions created by the Initial Developer are
# Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
# 
# Contributor(s): ECRC GmbH
# 
# END LICENSE BLOCK
#
# Worker manager menu
#

# Set global variables 
set selhost $machines(0)
set global_halt 0

# used for testing menu.tcl without the worker manager
#set num_machines 2
#set machines(0) [exec hostname]
#set machines(1) rincewind
#set workers(rincewind) 2
#set awake_workers(rincewind) 0
#set selhost $machines(0)
#set global_halt 0

option add *HighlightThickness 0
option add *Menubutton.padY 3
option add *Button.padY 0

# procedures for creating the status canvas
proc make_status_canvas {} {
    global num_machines

    set canvas_height [expr $num_machines * 20 + 60]
#    canvas .status -width 8.5c -height 4c
    canvas .status -width 8.5c -height $canvas_height
    .status create text 0.5c 1.0c -text "Machine" -anchor w -tags {header mctag}
    .status create text 4.0c 1.0c -text "Awake" -anchor w -tags {header awaketag}
    .status create text 5.5c 1.0c -text "Asleep" -anchor w -tags {header sleeptag}
    .status create text 7.0c 1.0c -text "Total" -anchor w -tags {header totaltag}
    set box [.status bbox header]
    scan $box "%d %d %d %d" x1 y1 x2 y2
    .status create line $x1 1.5c $x2 1.5c -tags headerline
    make_mc_status
}

proc make_box {box} {
    scan $box "%d %d %d %d" x1 y1 x2 y2
    .status create rectangle $x1 $y1 $x2 $y2 -outline red -tags {currect mc_status}
}


proc make_mc_status {} {
    global num_machines
    global machines
    global workers
    global awake_workers
    global selhost

    set i 0
    set canvas_height [expr $num_machines * 20 + 60]
    .status configure -height $canvas_height
    scan [.status coords headerline] "%f %f" mcx mcy
    set mcy [expr $mcy + 20.0]
    scan [.status coords awaketag] "%f" ax
    scan [.status coords sleeptag] "%f" sx
    scan [.status coords totaltag] "%f" tx
    while {$i < $num_machines}      {
	set mcname $machines($i)
#	if {$workers($mcname) > 0}  {
	    set sleeping [expr $workers($mcname) - $awake_workers($mcname)]
	    .status create text $mcx $mcy -text $mcname -anchor w -tags "$mcname mc_status"
	    .status create text $ax $mcy -text $awake_workers($mcname) -anchor w -tags mc_status
	    .status create text $sx $mcy -text $sleeping -anchor w -tags mc_status
	    .status create text $tx $mcy -text $workers($mcname) -anchor w -tags mc_status
	    .status bind $mcname <Button-1> {
		set oldhost $selhost
		set textval [.status itemconfigure current -text]
		set selhost [lindex [split $textval " "] 4]
		if {$oldhost != $selhost} {
		    .status delete currect
		    set box [.status bbox $selhost]
		    make_box $box
		} 
	    }
	    if {$selhost == $mcname} {
		set box [.status bbox $selhost]
		make_box $box
	    }
	    incr i
	    set mcy [expr $mcy + 20.0]
#	}
    }
}

label .message -relief sunken -width 30

proc msg {text} { 
    .message configure -text $text 
    update
}

proc make_sleep_menu {} {
    global selhost
    global awake_workers
    .sleep_button.menu delete 0 999
    for {set i 1} {$i <= $awake_workers($selhost)} {incr i} {
	.sleep_button.menu add command -label $i -command "sleep_n $i"
    }
    .sleep_button.menu entryconfigure [.sleep_button.menu index last] -state disabled
}

proc make_wake_menu {} {
    global selhost
    global awake_workers
    global workers
    .wake_button.menu delete 0 999
    set sleeping [expr $workers($selhost) - $awake_workers($selhost)]
    for {set i 1} {$i <= $sleeping} {incr i} {
	.wake_button.menu add command -label $i -command "wake_n $i"
    }
}

menubutton .add_button -text Add -menu .add_button.menu -relief raised
menu .add_button.menu -tearoff 0
for {set i 1} {$i <= 5} {incr i} {
    .add_button.menu add command -label $i -command "add_n $i"
}
.add_button.menu add command -label "More" -command "add_workers"

menubutton .sleep_button -text Sleep -menu .sleep_button.menu -relief raised
menu .sleep_button.menu -tearoff 0
make_sleep_menu

menubutton .wake_button -text Wakeup -menu .wake_button.menu -relief raised
menu .wake_button.menu -tearoff 0
make_wake_menu


button .perfmeter -text "Perfmeter" -command "create_perf_window" 
button .remove -text "Remove" -command "remove_workers" -state disabled
button .dismiss -text "Dismiss" -command "close_window"
button .quit -text "Exit"  -command { \
	halt_confirm; \
	if {$global_halt == 1} { \
	    destroy . ; \
	    puts stdout "\nbye"; \
            halt ; \
            exit -1} }
make_status_canvas
pack .message -side top -fill x -expand 1
pack .status -side top
pack .add_button .sleep_button .wake_button -side left -fill both -expand 1 
pack .perfmeter .dismiss .quit -side left -fill both -expand 1



proc remake_status {} {          ;# redraw status
    .status delete mc_status
#    .status delete all
#    make_status_canvas
    make_mc_status
    make_sleep_menu
    make_wake_menu
}

proc remove_workers {} {
    global selhost
    if {$selhost != ""} {
	do_wakeup $selhost
    } else {
	make_active_mclist remove_workers do_remove
    }
}
proc add_workers {} {
    global selhost
    if {$selhost != ""} {
	do_add $selhost
    } else {
	make_mclist add_workers do_add 
    }
}

proc wakeup_workers {} {
    global selhost
    if {$selhost != ""} {
	do_wakeup $selhost
    } else {
	make_active_mclist wakeup do_wakeup
    }
}

proc sleep_workers {} {
    global selhost
    if {$selhost != "" } {
	do_sleep $selhost
    } else {
	make_active_mclist set_sleeping do_sleep
    }
}

proc make_active_mclist {wmname pname} {
     global machines
     global workers
     global num_machines

     toplevel .$wmname
	
     scrollbar .$wmname.scroll -command ".$wmname.list yview"
     listbox .$wmname.list -yscroll ".$wmname.scroll set"
     button .$wmname.close -text "close" -command "destroy .$wmname" 
     pack .$wmname.close -side bottom
     pack .$wmname.scroll -side right -fill y
     pack .$wmname.list -side left -expand yes -fill both

     set i 0
     while {$i < $num_machines}      {
	 set mcname $machines($i)
	 if {$workers($mcname) > 0}  {
	     .$wmname.list insert end $machines($i)
	 }
	 incr i
     }
     if {$pname == "do_sleep"} {
	 bind .$wmname.list <Double-Button-1> {do_sleep [selection get]}
     } elseif {$pname == "do_wakeup"} {
	 bind .$wmname.list <Double-Button-1> {do_wakeup [selection get]}
     } else { 
	 puts stdout "procedure name unknown in make_active_list"
     }
}

proc do_sleep {machine} {
    global awake_workers
    global workers
    
    set max_num [expr $awake_workers($machine) - 1]
    if {$max_num > 0} {
	set num [get-value $machine sleep 1 $max_num]
	if {[string compare $num cancel] != 0}     {
	    if {$num > 0 && $awake_workers($machine) > $num} {
		apply_sleep $machine $num
	    }
	}
    } else {
	error_box "Only one worker awake on $machine"
    }
}

proc do_wakeup {machine} {
    global awake_workers
    global workers

    set sleeping_workers  [expr $workers($machine) - $awake_workers($machine)]
    if {$sleeping_workers > 0} {
	set num [get-value $machine wakeup 1 $sleeping_workers]
	if {[string compare $num cancel] != 0} {
	    if {$num > 0 && $sleeping_workers >= $num} {
		apply_wakeup $machine $num
	    }
	}
    } else {
	error_box "No Workers Asleep On $machine"
    }
}


proc make_mclist {wmname pname} {
     global num_machines
     global machines
     toplevel .$wmname
	
     scrollbar .$wmname.scroll -command ".$wmname.list yview"
     listbox .$wmname.list -yscroll ".$wmname.scroll set"
     button .$wmname.close -text "close" -command "destroy .$wmname" 
     pack .$wmname.close -side bottom
     pack .$wmname.scroll -side right -fill y
     pack .$wmname.list -side left -expand yes -fill both

     set i 0
     while {$i < $num_machines} {
	.$wmname.list insert end $machines($i)
	incr i
	}
	if {$pname == "do_add"} {
	    bind .$wmname.list <Double-Button-1> {do_add [selection get]}
	} else { 
	    puts stdout "Procedure name unknown in make_mclist" 
	}
}

proc do_add {machine} {
    global workers
    global awake_workers
    set num [get-value $machine add 6 32]
    if {[string compare $num cancel] != 0}   {
	if {$num > 0 } {
	    msg "Adding $num on $machine ..."
	    apply_add $machine $num
	    msg ""
	}
    }
}

proc add_n {num} {
    global selhost

    msg "Adding $num on $selhost ..."
    apply_add $selhost $num
    msg ""
}

proc wake_n {num} {
    global selhost

    msg "Waking up $num on $selhost ..."
    apply_wakeup $selhost $num
    msg ""
}

proc sleep_n {num} {
    global selhost

    msg "Sending $num to sleep on $selhost ..."
    apply_sleep $selhost $num
    msg ""
}

proc do_remove {machine} {
    global workers
    set num [get-value $machine remove 1]
    if {[string compare $num cancel] != 0} {
	if {$num > 0 && $workers($machine) >= $num} {
	    set workers($machine) [expr $workers($machine) - $num]
	}
    }
}

proc get-value {Machine Request Defval MaxVal} {
    global value
    
    toplevel .workers
    set temp [concat $Machine $Request]
    wm title .workers [join $temp _]

    frame .workers.middle
    frame .workers.bottom
    pack .workers.middle -side top -expand yes -pady .5c
    pack .workers.bottom -side top -expand yes -fill x

    label .workers.label -text "Workers"
    scale .workers.scale -from 0 -to $MaxVal -length 5c -orient horizontal
    .workers.scale set $Defval
    button .workers.up -width 2 -text + -command "tc_inc 1"
    button .workers.down -width 2 -text - -command "tc_inc -1"
    pack .workers.label -in .workers.middle -side top -anchor w
    pack .workers.down -in .workers.middle -side left -padx .25c
    pack .workers.scale -in .workers.middle -side left
    pack .workers.up -in .workers.middle -side left -padx .25c
    
    button .workers.ok -text "OK" -command {
	set value [.workers.scale get]
	destroy .workers
    }
    button .workers.cancel -text "Cancel" -command {
	set value 0
	destroy .workers
    }
#    pack .workers.ok -side left -padx 10 -pady 10
#    pack .workers.cancel -side right  -padx 10 -pady 10
    pack .workers.ok -in .workers.bottom -side left -fill both -expand 1
    pack .workers.cancel -in .workers.bottom -side right -fill both -expand 1
    tkwait visibility .workers				
    grab set .workers
    tkwait var value
    return $value
}

proc tc_inc {inc} {
    .workers.scale set [expr [.workers.scale get]+$inc]
}

proc error_box {error_mess} {
    tk_dialog .dialog "Error" $error_mess "" 0 OK
}

proc halt_confirm {} {
    global global_halt
    set i [tk_dialog .dialog "Confirm" "Send a kill (-9) signal to all workers and exit?" \
	    question 0 Confirm Cancel]

    if {$i == 0} {
	set global_halt 1
    }

}

###################################################################
# Routines for displaying the Performance Monitor 
###################################################################

# Colors to display 
proc init_stat_colors {} {
    global stat_color
    set stat_color(working) gold
    set stat_color(idling) white
    set stat_color(scheduling) palegreen
    set stat_color(copying) red
    set stat_color(recomputing) magenta
    set stat_color(user_cpu) gold
    set stat_color(system_cpu) palegreen
    set stat_color(other) white
}

proc create_perf_window {} {
    global perfon
    global eclipse_display

    set scale_factor 4
    toplevel .perfwindow
    wm title .perfwindow "ECLiPSe Performance Monitor"
    .add_button configure -state disabled
    init_stat_colors
    set perfon 1
    set eclipse_display 0
    reset_stat

    canvas .perfwindow.perf -width [expr 100 * $scale_factor + 100] 
    make_perf_canvas $scale_factor

    menubutton .perfwindow.rmode -text "Refresh Mode" -menu .perfwindow.rmode.menu -relief raised
    menu .perfwindow.rmode.menu -tearoff 0
    .perfwindow.rmode.menu add command -label "Snapshot"  -command "set snapshot 1"
    .perfwindow.rmode.menu add command -label "Integrate" -command "set snapshot 0"
    menubutton .perfwindow.refresh -text "Refresh rate" -menu .perfwindow.refresh.menu -relief raised

    menu .perfwindow.refresh.menu -tearoff 0
    for {set i 0.5} {$i <= 5.0} {set i [expr $i + 0.5]} {
	.perfwindow.refresh.menu add command -label "$i sec" -command "set_refresh_rate $i"
    }
    
    button .perfwindow.display_mode -text "System Monitor" -command "toggle_display"
    button .perfwindow.reset -text "Reset" -command "reset_stat; remake_perf_canvas" 
    button .perfwindow.dismiss -text "Dismiss" -command "close_perf_window"

    pack .perfwindow.perf -side top
    pack   .perfwindow.display_mode .perfwindow.refresh .perfwindow.rmode .perfwindow.reset .perfwindow.dismiss -side left -fill both -expand 1
}

proc close_perf_window {} {
    disable_perfon
    destroy .perfwindow
    .add_button configure -state active
}

# Draws the peformance bar charts and the elapsed time
proc make_perf_canvas {scale_factor} {
    global num_workers
    global cur_time 
    
    set cur_time 0.0
    calc_num_workers
    set canvas_height [expr $num_workers * 20 + 50]
    .perfwindow.perf configure -height $canvas_height

    write_hostnames
    write_legend
    draw_ticks $scale_factor
    draw_bars $scale_factor
}


proc make_legend_rect {color y x} {
    set x1 [expr $x - 5]
    set x2 [expr $x + 5]
    set y1 [expr $y + 10]
    set y2 [expr $y + 20]
    
    .perfwindow.perf create rectangle $x1 $y1 $x2 $y2 -fill $color -tags legend
}

proc write_legend {} {
    global stat_color
    global num_workers
    global eclipse_display
    
    set y1 [expr $num_workers * 20 + 30 + 20]
    if {$eclipse_display > 0} {
	make_legend_rect $stat_color(working) $y1 110
	.perfwindow.perf create text 110 $y1 -text "Working" -anchor center -tags legend
	make_legend_rect $stat_color(idling) $y1 190
	.perfwindow.perf create text 190 $y1 -text "Idling" -anchor center -tags legend
	make_legend_rect $stat_color(scheduling) $y1 270
	.perfwindow.perf create text 270 $y1 -text "Scheduling" -anchor center -tags legend
	make_legend_rect $stat_color(copying) $y1 350
	.perfwindow.perf create text 350 $y1 -text "Copying" -anchor center -tags legend
	make_legend_rect $stat_color(recomputing) $y1 430
	.perfwindow.perf create text 430 $y1 -text "Recomputing" -anchor center -tags legend
    } else {
	make_legend_rect $stat_color(user_cpu) $y1 150
	.perfwindow.perf create text 150 $y1 -text "User_cpu" -anchor center -tags legend
	make_legend_rect $stat_color(system_cpu) $y1 275
	.perfwindow.perf create text 275 $y1 -text "System_cpu" -anchor center -tags legend
	make_legend_rect $stat_color(other) $y1 400
	.perfwindow.perf create text 400 $y1 -text "Idle" -anchor center -tags legend
    }
}    

#Write the hostname of each worker next to its performance bar
proc write_hostnames {} {
    global worker_machine
    global num_workers

    set y1 30    
    for {set i 1} {$i <= $num_workers} {incr i} {
	.perfwindow.perf create text 50 [expr $y1 + 10] -text $worker_machine($i)
	set y1 [expr $y1 + 20]
    }
}

proc draw_ticks {scale_factor} {
    set startx 80
    set starty 25

    for {set i 0} {$i <= 100} {set i [expr $i + 20]} {
	.perfwindow.perf create text [expr $startx + $i * $scale_factor] $starty -text $i
    }
}

proc draw_bars {scale_factor} {
    global num_workers
    global stat_color
    global worker_stat
    global eclipse_display
    global cur_time

    calc_num_workers
    set canvas_height [expr $num_workers * 20 + 80]
    .perfwindow.perf configure -height $canvas_height

    .perfwindow.perf create text [expr 90 * $scale_factor + 80] 10 -text $cur_time -tags bar 

    set y1 30
    
    for {set i 1} {$i <= $num_workers} {incr i} {
	set startx 80
	if {$eclipse_display > 0} {
	    set x2 [expr $startx + $worker_stat($i,working) * $scale_factor]
	    set y2 [expr $y1 + 15]
	    .perfwindow.perf create rectangle $startx $y1 $x2 $y2 -fill $stat_color(working) -tags bar
	    set x1 $x2
	    set x2 [expr $x1 + $worker_stat($i,idling) * $scale_factor]
	    .perfwindow.perf create rectangle $x1 $y1 $x2 $y2 -fill $stat_color(idling) -tags bar
	    set x1 $x2
	    set x2 [expr $x1 + $worker_stat($i,scheduling) * $scale_factor]
	    .perfwindow.perf create rectangle $x1 $y1 $x2 $y2 -fill $stat_color(scheduling) -tags bar
	    set x1 $x2
	    set x2 [expr $x1 + $worker_stat($i,copying) * $scale_factor]
	    .perfwindow.perf create rectangle $x1 $y1 $x2 $y2 -fill $stat_color(copying) -tags bar
	    set x1 $x2
	    set x2 [expr $x1 + $worker_stat($i,recomputing) * $scale_factor]
	    .perfwindow.perf create rectangle $x1 $y1 $x2 $y2 -fill $stat_color(recomputing) -tags bar
	    set y1 [expr $y1 + 20]
	} else {
	    set x2 [expr $startx + $worker_stat($i,user_cpu) * $scale_factor]
	    set y2 [expr $y1 + 15]
	    .perfwindow.perf create rectangle $startx $y1 $x2 $y2 -fill $stat_color(user_cpu) -tags bar
	    set x1 $x2
	    set x2 [expr $x1 + $worker_stat($i,system_cpu) * $scale_factor]
	    .perfwindow.perf create rectangle $x1 $y1 $x2 $y2 -fill $stat_color(system_cpu) -tags bar
	    set x1 $x2
	    set x2 [expr $x1 + $worker_stat($i,other) * $scale_factor]
	    .perfwindow.perf create rectangle $x1 $y1 $x2 $y2 -fill $stat_color(other) -tags bar
	    set y1 [expr $y1 + 20]
	}
    }
}

proc toggle_display {} {
    global eclipse_display

    if {$eclipse_display > 0 } {
	set eclipse_display 0
	.perfwindow.display_mode configure -text "System Monitor"
    } else {
	set eclipse_display 1
	.perfwindow.display_mode configure -text "CPU Monitor"
    }
    .perfwindow.perf delete bar legend
    write_legend
    draw_bars 4
}
proc remake_perf_canvas {} {
    .perfwindow.perf delete bar
    draw_bars 4
}    

proc calc_num_workers {} {
    global num_machines
    global workers
    global machines
    global num_workers

    set num_workers 0
    for {set i 0} {$i < $num_machines} {incr i} {
	set mcname $machines($i)
	set num_workers [expr $num_workers + $workers($mcname)]
    }
}
