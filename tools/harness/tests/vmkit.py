##########################################################################
# Copyright (c) 2009, 2010, 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import socket, datetime
import tests, siteconfig
from common import TestCommon
from results import PassFailResult

VMKIT_TIMEOUT = datetime.timedelta(minutes=15)

@tests.add_test
class VMKitTest(TestCommon):
    '''VMKit test'''
    name = "vmkit"

    def get_modules(self, build, machine):
        cardName = "e1000"
        modules = super(VMKitTest, self).get_modules(build, machine)
        modules.add_module("serial")
        modules.add_module("lpc_timer")
        modules.add_module("e1000n", ["core=%d" % machine.get_coreids()[3]])
        modules.add_module("NGD_mng", ["core=%d" % machine.get_coreids()[2],
                                    "cardname=%s"%cardName])
        modules.add_module("netd", ["core=%d" % machine.get_coreids()[2],
                                    "cardname=%s"%cardName])

        nfsip = socket.gethostbyname(siteconfig.get('WEBSERVER_NFS_HOST'))
        modules.add_module("vmkitmon", [cardName,
                                       "nfs://" + nfsip + "/local/nfs/harness"])
        return modules

    def get_finish_string(self):
        return "bash-4.0#"

    def boot(self, *args):
        super(VMKitTest, self).boot(*args)
        self.set_timeout(VMKIT_TIMEOUT)

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        for line in rawiter:
            lastline = line
        passed = lastline.startswith(self.get_finish_string())
        return PassFailResult(passed)
