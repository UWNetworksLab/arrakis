##########################################################################
# Copyright (c) 2009, 2013, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import socket, os
import debug

class BaseSite(object):
    def __getattr__(self, name):
        raise AttributeError('site configuration (%s) has no parameter named %s'
                             % (self.__name__, name))

    def get_load_generator(self):
        """Returns a (username, hostname/IP) tuple for a host to which we can 
        SSH to run network load generators. May be called multiple times."""
        raise NotImplementedError

# this refers to the singleton site instance after importing the correct module
site = None

# Are we at ETH?
if os.path.isdir('/home/netos') and socket.getfqdn().endswith('.ethz.ch'):
    import eth
elif socket.getfqdn().endswith('.europe.corp.microsoft.com'):
    import msrc
elif socket.getfqdn().endswith('triangle') or socket.getfqdn().endswith('.cs.washington.edu'):
    import uw
else:
    debug.warning("unable to guess site, using ETH... expect breakage!")
    import eth

# shortcut to lookup a configuration parameter for a site
def get(name):
    return getattr(site, name)
