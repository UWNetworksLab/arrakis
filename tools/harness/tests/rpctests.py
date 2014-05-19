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
from results import RawResults, PassFailResult

class RpcTestCommon(TestCommon):
    benchmark_type = None

    def get_module_name(self):
        raise NotImplementedError

    def get_modules(self, build, machine):
        modules = super(RpcTestCommon, self).get_modules(build, machine)
        modules.add_module(self.get_module_name())
        return modules

    def run(self, build, machine, testdir):
        modules = self.get_modules(build, machine)
        args = []
        if self.benchmark_type != None:
            args = [str(self.benchmark_type)]
        modules.reset_module(self.get_module_name(), args)
        self.boot(machine, modules)
        return self.collect_data(machine)

    def process_data(self, testdir, rawiter):
        results = RawResults('core')
        times = []
        core = None
        for line in rawiter:
            m = re.match("Running \w+ between core \d+ and core (\d+)", line)
            if m:
                if times:
                    results.add_group(core, times)
                core = int(m.group(1))
                times = []
                continue

            m = re.match("page \d+ took (\d+)", line)
            if m:
                assert(core is not None)
                times.append(int(m.group(1)))

        if len(times) != 0:
            results.add_group(core, times)
        return results

@tests.add_test
class TransportLatencyTest(RpcTestCommon):
    ''' Transport Latency microbenchmark '''
    name = "transport_latency"

    def get_module_name(self):
        return "transport_bench"

    def process_data(self, testdir, rawiter):
        results = RawResults('iteration')
        times = []
        iteration = None
        for line in rawiter:
            m = re.match("transport_bench: iteration (\d+)", line)
            if m:
                if times:
                    results.add_group(iteration, times)
                iteration = int(m.group(1))
                times = []
                continue

            m = re.match("page \d+ took (\d+)", line)
            if m:
                assert(iteration is not None)
                times.append(int(m.group(1)))

        if len(times) != 0:
            results.add_group(iteration, times)
        return results

@tests.add_test
class FlounderStubsEmptyTest(RpcTestCommon):
    ''' Empty flounder stubs '''
    name = "flounder_stubs_empty"

    def get_module_name(self):
        return "flounder_stubs_empty_bench"

@tests.add_test
class FlounderStubsPayloadGlueTest(RpcTestCommon):
    ''' Payload_Glue flounder stubs '''
    name = "flounder_stubs_payload_glue"
    benchmark_type = 0

    def get_module_name(self):
        return "flounder_stubs_payload_glue_bench"

@tests.add_test
class FlounderStubsPayload1GlueTest(RpcTestCommon):
    ''' Payload_Glue1 flounder stubs '''
    name = "flounder_stubs_payload1_glue"
    benchmark_type = 2

    def get_module_name(self):
        return "flounder_stubs_payload_glue_bench"

@tests.add_test
class FlounderStubsPayload2GlueTest(RpcTestCommon):
    ''' Payload_Glue2 flounder stubs '''
    name = "flounder_stubs_payload2_glue"
    benchmark_type = 3

    def get_module_name(self):
        return "flounder_stubs_payload_glue_bench"

@tests.add_test
class FlounderStubsPayload8GlueTest(RpcTestCommon):
    ''' Payload_Glue8 flounder stubs '''
    name = "flounder_stubs_payload8_glue"
    benchmark_type = 4

    def get_module_name(self):
        return "flounder_stubs_payload_glue_bench"

@tests.add_test
class FlounderStubsPayload16GlueTest(RpcTestCommon):
    ''' Payload_Glue16 flounder stubs '''
    name = "flounder_stubs_payload16_glue"
    benchmark_type = 5

    def get_module_name(self):
        return "flounder_stubs_payload_glue_bench"

@tests.add_test
class FlounderStubsEmptyGlueTest(RpcTestCommon):
    ''' Empty_Glue flounder stubs '''
    name = "flounder_stubs_empty_glue"
    benchmark_type = 1

    def get_module_name(self):
        return "flounder_stubs_payload_glue_bench"

@tests.add_test
class FlounderStubsPayloadTest(RpcTestCommon):
    ''' Payload flounder stubs '''
    name = "flounder_stubs_payload"

    def get_module_name(self):
        return "flounder_stubs_payload_bench"

@tests.add_test
class FlounderStubsPayload1Test(RpcTestCommon):
    ''' Payload1 flounder stubs '''
    name = "flounder_stubs_payload1"
    benchmark_type = 2

    def get_module_name(self):
        return "flounder_stubs_payload_bench"

@tests.add_test
class FlounderStubsPayload2Test(RpcTestCommon):
    ''' Payload2 flounder stubs '''
    name = "flounder_stubs_payload2"
    benchmark_type = 3

    def get_module_name(self):
        return "flounder_stubs_payload_bench"

@tests.add_test
class FlounderStubsPayload8Test(RpcTestCommon):
    ''' Payload8 flounder stubs '''
    name = "flounder_stubs_payload8"
    benchmark_type = 4

    def get_module_name(self):
        return "flounder_stubs_payload_bench"

@tests.add_test
class FlounderStubsPayload16Test(RpcTestCommon):
    ''' Payload16 flounder stubs '''
    name = "flounder_stubs_payload16"
    benchmark_type = 5

    def get_module_name(self):
        return "flounder_stubs_payload_bench"

@tests.add_test
class FlounderStubsBufferTest(RpcTestCommon):
    ''' Buffer flounder stubs '''
    name = "flounder_stubs_buffer"

    def get_module_name(self):
        return "flounder_stubs_buffer_bench"

@tests.add_test
class RouteTest(RpcTestCommon):
    ''' routing microbenchmark '''
    name = "route_bench"

    def get_module_name(self):
        return "route_bench"

@tests.add_test
class UmpLatencyTest(RpcTestCommon):
    ''' UMP latency microbenchmark '''
    name = "ump_latency"

    def get_module_name(self):
        return "ump_latency"

@tests.add_test
class UmpExchangeTest(RpcTestCommon):
    ''' UMP exchange microbenchmark '''
    name = "ump_exchange"

    def get_module_name(self):
        return "ump_exchange"

@tests.add_test
class UmpThroughputTest(RpcTestCommon):
    ''' UMP throughput microbenchmark '''
    name = "ump_throughput"

    def get_module_name(self):
        return "ump_throughput"

@tests.add_test
class UmpSendTest(RpcTestCommon):
    ''' UMP send microbenchmark '''
    name = "ump_send"

    def get_module_name(self):
        return "ump_send"

@tests.add_test
class UmpReceiveTest(RpcTestCommon):
    ''' UMP receive microbenchmark '''
    name = "ump_receive"

    def get_module_name(self):
        return "ump_receive"

@tests.add_test
class UmpCacheTest(RpcTestCommon):
    ''' UMP cache utilisation microbenchmark '''
    name = "ump_latency_cache"

    def get_module_name(self):
        return "ump_latency_cache"

    # XXX: don't attempt to run on an intel machine, as perfmon is unsupported
    def run(self, build, machine, testdir):
        perftype = machine.get_perfcount_type()
        if perftype.startswith('intel'):
            return ["Test skipped: %s performance monitoring unsupported\n" % perftype]
        else:
            return super(RpcTestCommon, self).run(build, machine, testdir)

    def process_data(self, testdir, raw_iter):
        # check the first line to see if the test actually ran
        try:
            firstline = raw_iter.next()
            if firstline.startswith('Test skipped'):
                return PassFailResult(True)
        except StopIteration: # empty results file
            return PassFailResult(False)

        # process the rest of the file
        results = {}
        data = {}
        index = {}
        iteration = {}
        results['Data'] = RawResults('iter', name='dcache')
        results['Instruction'] = RawResults('iter', name='icache')
        for cache in ['Data', 'Instruction']:
            data[cache] = []
            index[cache] = None
            iteration[cache] = 1

        for line in raw_iter:
            m = re.match("(Data|Instruction) cache miss\s+(\d+)\s+(\d+)\s+(\d+)",
                         line)
            if m:
                cache = m.group(1)
                i = int(m.group(2))
                value = int(m.group(4)) - int(m.group(3))
                if index[cache] is not None and i < index[cache]: # new iteration
                    results[cache].add_group(iteration[cache], data[cache])
                    iteration[cache] += 1
                    data[cache] = [value]
                else:
                    data[cache].append(value)
                index[cache] = i

        ret = []
        for cache in ['Data', 'Instruction']:
            results[cache].add_group(iteration[cache], data[cache])
            ret.append(results[cache])
        return ret

@tests.add_test
class LrpcTest(RpcTestCommon):
    ''' LRPC microbenchmark '''
    name = "lrpc"

    def get_module_name(self):
        return "lrpc_bench"

    def get_modules(self, build, machine):
        modules = super(RpcTestCommon, self).get_modules(build, machine)
        modules.add_module("lrpc_bench", ["server"])
        modules.add_module("lrpc_bench", ["client"])
        return modules

    def get_finish_string(self):
        return "End of benchmarks."

    def run(self, *args):
        # XXX: want to use the base single-boot implementation
        return TestCommon.run(self, *args)

    def process_data(self, testdir, raw_iter):
        results = RawResults('iter')
        data = []
        index = None
        iteration = 1
        for line in raw_iter:
            m = re.match("page\s+(\d+)\s+took\s+(\d+)", line)
            if m:
                if index is not None and int(m.group(1)) < index:
                    # new iteration
                    results.add_group(iteration, data)
                    iteration += 1
                    data = [int(m.group(2))]
                else:
                    data.append(int(m.group(2)))
                index = int(m.group(1))
        results.add_group(iteration, data)
        return results
