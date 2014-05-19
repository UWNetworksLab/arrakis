%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% by hand from data sheet

ht_link(cpu0,cpu1).
ht_link(cpu0,cpu4).
ht_link(cpu0,pci0).
% ht_link(cpu0,mem0).
ht_link(cpu1,cpu0).
ht_link(cpu1,cpu2).
ht_link(cpu1,cpu6).
% ht_link(cpu1,mem1).
ht_link(cpu2,cpu1).
ht_link(cpu2,cpu3).
ht_link(cpu2,cpu4).
% ht_link(cpu2,mem2).
ht_link(cpu3,cpu2).
ht_link(cpu3,cpu4).
ht_link(cpu3,cpu5).
% ht_link(cpu3, mem3).
ht_link(cpu4,cpu3).
ht_link(cpu4,cpu2).
ht_link(cpu4,cpu5).
% ht_link(cpu4, mem4).
ht_link(cpu5, cpu3).
ht_link(cpu5, cpu4).
ht_link(cpu5, cpu6).
% ht_link(cpu5, mem5).
ht_link(cpu6, cpu5).
ht_link(cpu6, cpu1).
ht_link(cpu6, cpu7).
% ht_link(cpu6, mem6).
ht_link(cpu7, cpu6).
ht_link(cpu7, cpu0).
ht_link(cpu7, pci1).
% ht_link(cpu7, mem7).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% by hand from data sheet

rootcomplex_addr(pci0, addr(0,0,0)).
rootcomplex_addr(pci1, addr(128,0,0)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% might be possible at runtime

cpu_thread(4, 0, 0, 0).
cpu_thread(5, 0, 1, 0).
cpu_thread(6, 0, 2, 0).
cpu_thread(7, 0, 3, 0).
cpu_thread(8, 1, 0, 0).
cpu_thread(9, 1, 1, 0).
cpu_thread(10, 1, 2, 0).
cpu_thread(11, 1, 3, 0).
cpu_thread(12, 2, 0, 0).
cpu_thread(13, 2, 1, 0).
cpu_thread(14, 2, 2, 0).
cpu_thread(15, 2, 3, 0).
cpu_thread(16, 3, 0, 0).
cpu_thread(17, 3, 1, 0).
cpu_thread(18, 3, 2, 0).
cpu_thread(19, 3, 3, 0).
cpu_thread(20, 4, 0, 0).
cpu_thread(21, 4, 1, 0).
cpu_thread(22, 4, 2, 0).
cpu_thread(23, 4, 3, 0).
cpu_thread(24, 5, 0, 0).
cpu_thread(25, 5, 1, 0).
cpu_thread(26, 5, 2, 0).
cpu_thread(27, 5, 3, 0).
cpu_thread(28, 6, 0, 0).
cpu_thread(29, 6, 1, 0).
cpu_thread(30, 6, 2, 0).
cpu_thread(31, 6, 3, 0).
cpu_thread(32, 7, 0, 0).
cpu_thread(33, 7, 1, 0).
cpu_thread(34, 7, 2, 0).
cpu_thread(35, 7, 3, 0).

