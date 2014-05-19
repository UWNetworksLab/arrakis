##########################################################################
# Copyright (c) 2013, University of Washington.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

machines = {
    'bigfish': {'ncores'      : 64,
                'machine_name' : 'bigfish',
                'bootarch' : 'x86_64',
                'buildarchs' : ['x86_64', 'x86_32'],
                'cores_per_socket': 12,
                'perfcount_type': 'amd10',
                'tickrate'    : 1600,
                'boot_timeout': 360,
                'kernel_args' : ['serial=0x2f8']},
    'bigfish-32': {'ncores'      : 64,
                   'machine_name' : 'bigfish',
                   'bootarch' : 'x86_32',
                   'buildarchs' : ['x86_64', 'x86_32'],
                   'cores_per_socket': 12,
                   'perfcount_type': 'amd10',
                   'tickrate'    : 1600,
                   'boot_timeout': 360,
                   'kernel_args' : ['serial=0x2f8']}
    }
