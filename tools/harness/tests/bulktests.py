##########################################################################
# Copyright (c) 2009, 2010, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import re
import debug, tests
from common import TestCommon, TimeoutError
from results import RawResults

class BulkTestCommon(TestCommon):
    use_memcpy = None

    def get_module_name(self):
        return "bulkbench"

    def get_build_targets(self, build, machine):
        targets = super(BulkTestCommon, self).get_build_targets(build, machine)
        targets.append('%s/sbin/%s' %
                       (machine.get_bootarch(), self.get_module_name()))
        return targets

    def run(self, build, machine, testdir):
        # compute two core IDs on different sockets to benchmark between
        sendcore = machine.get_coreids()[0]
        recvcore = machine.get_coreids()[machine.get_cores_per_socket()]
    
        # Iterate over all bulk block sizes
        for i in [2048]:
            debug.log('running %s block size %d' % (self.name, i))
            modules = self.get_modules(build, machine)
            modules.add_module(self.get_module_name(),
                               ["core=%d" % sendcore, i, "send", self.use_memcpy])
            modules.add_module(self.get_module_name(),
                               ["core=%d" % recvcore, i, "recv", self.use_memcpy])
            self.boot(machine, modules)
            for line in self.collect_data(machine):
                yield line

    def process_data(self, testdir, rawiter):
        results = RawResults('buffersize')
        data = []
        for line in rawiter:
            m = re.match("rawresult (\d+)", line)
            if m:
                data.append(2048 / int(m.group(1)))

        results.add_group("2048", data)
        return results

@tests.add_test
class BulkThroughputTest(BulkTestCommon):
    ''' Bulk transport throughput microbenchmark '''
    name = "bulk"
    use_memcpy = "nomemcpy"

@tests.add_test
class BulkMemThroughputTest(BulkTestCommon):
    ''' Bulk transport throughput microbenchmark with memcpy on receiver '''
    name = "bulk_memcpy"
    use_memcpy = "memcpy"
