##########################################################################
# Copyright (c) 2013, University of Washington.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import getpass
import siteconfig

LOADGEN_HOSTS = []

class UW(siteconfig.BaseSite):
    # site-specific configuration variables for UW
    WEBSERVER_NFS_HOST = 'triangle'
    # NFS_SERVER_HOST = 'tomme1.in.barrelfish.org'
    WEBSERVER_NFS_PATH = '/home/netos/notexist'
    WEBSERVER_LOCAL_PATH = WEBSERVER_NFS_PATH
    HTTPERF_PATH = 'httperf.notexist'
    HTTPERF_MAXCLIENTS = len(LOADGEN_HOSTS * 2) # max number of load generators
    IPBENCH_PATH = 'ipbench.notexist'
    IPBENCHD_PATH = 'ipbenchd.notexist'
    SSH_ARGS='-x -o StrictHostKeyChecking=no -o ControlPath=none'

    def __init__(self):
        self._loadgen_hosts = LOADGEN_HOSTS

    def get_load_generator(self):
        # take the first host, but put it on the back in case we
        # need more clients than available hosts (ie. rotate the list)
        host = self._loadgen_hosts.pop(0)
        self._loadgen_hosts.append(host)
        return getpass.getuser(), host

siteconfig.site = UW()

# also cause the UW machines to be loaded/initialised
import machines.uw
