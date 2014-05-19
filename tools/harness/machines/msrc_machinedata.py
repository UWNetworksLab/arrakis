##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

machines = {
    'barrelfish-5': {'ncores'      : 16,
                     'machine_name' : 'barrelfish-5',
                     'bootarch' : 'x86_64',
                     'buildarchs' : ['x86_64', 'x86_32'],
                     'cores_per_socket': 4,
                     'perfcount_type': 'amd10',
                     'tickrate'    : 2500},
    'barrelfish-6': {'ncores'      : 16,
                     'machine_name' : 'barrelfish-6',
                     'bootarch' : 'x86_64',                        
                     'buildarchs' : ['x86_64', 'x86_32'],
                     'cores_per_socket': 4,
                     'perfcount_type': 'amd10',
                     'tickrate'    : 2500},

    'barrelfish-5-32': {'ncores'      : 16,
                        'machine_name' : 'barrelfish-5',
                        'bootarch' : 'x86_32',
                        'buildarchs' : ['x86_64', 'x86_32'],
                        'cores_per_socket': 4,
                        'perfcount_type': 'amd10',
                        'tickrate'    : 2500},
    'barrelfish-6-32': {'ncores'      : 16,
                        'machine_name' : 'barrelfish-6',
                        'bootarch' : 'x86_32',                        
                        'buildarchs' : ['x86_64', 'x86_32'],
                        'cores_per_socket': 4,
                        'perfcount_type': 'amd10',
                        'tickrate'    : 2500},
    }
