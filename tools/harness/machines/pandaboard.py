##########################################################################
# Copyright (c) 2014, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import debug, machines
from machines import Machine

@machines.add_machine
class PandaboardMachine(Machine):
    name = 'pandaboard'

    def __init__(self, options):
        super(PandaboardMachine, self).__init__(options)
        self.options = options

    def get_bootarch(self):
        return 'armv7'
