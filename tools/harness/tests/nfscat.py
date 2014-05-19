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

use_emmentaler = False
NFS_TIMEOUT = datetime.timedelta(minutes=5)

@tests.add_test
class NFSTest(TestCommon):
    '''NFS benchmark'''
    name = "nfscat"

    def get_modules(self, build, machine):
        cardName = "e1000"
        modules = super(NFSTest, self).get_modules(build, machine)
        modules.add_module("e1000n", ["core=%d" % machine.get_coreids()[1]])
        modules.add_module("NGD_mng", ["core=%d" % machine.get_coreids()[2],
                                    "cardname=%s"%cardName])
        modules.add_module("netd", ["core=%d" % machine.get_coreids()[2],
                                    "cardname=%s"%cardName])
        if use_emmentaler :
            nfsip = socket.gethostbyname(siteconfig.get('WEBSERVER_NFS_HOST'))
            nfspath = "/local/nfs/harness_nfs/"
        else :
            nfsip = socket.gethostbyname(siteconfig.get('NFS_SERVER_HOST'))
            nfspath = "/shared/harness_nfs/"

        modules.add_module("netthroughput",
                ["core=%d" % machine.get_coreids()[2], "nfs://" + nfsip +
                          nfspath , "/nfs/G1.file"])
        return modules

    def get_finish_string(self):
        return "## Data size ="

    def boot(self, *args):
        super(NFSTest, self).boot(*args)
        self.set_timeout(NFS_TIMEOUT)

    def process_data(self, testdir, rawiter):
        # the test passed iff the last line is the finish string
        lastline = ''
        for line in rawiter:
            lastline = line
        passed = lastline.startswith(self.get_finish_string())
        return PassFailResult(passed)
