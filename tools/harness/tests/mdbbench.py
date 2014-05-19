
import re, datetime
import tests, debug
from common import TestCommon
from results import PassFailResult, RowResults
from itertools import product

@tests.add_test
class MdbBench(TestCommon):
    '''mdbbench'''
    name = "mdbbench"

    #resets = ["random_nat_ram", "propszrand_nat_ram", "szprob_cp_nat_ram"]
    resets = ["szprob_cp_nat_ram"]
    counts = [1<<x for x in range(5, 13+1)]
    #counts = [1<<10]
    impls = ["mdb_bench", "mdb_bench_old"]
    measures = [
        "insert_one",
        "remove_one",
        "iterate_1",
        "iterate_10",
        "iterate_100",
        "iterate_1000",
        "has_copies",
        "has_ancestors",
        "has_descendants",
    ]
    impl_measures = {
        "mdb_bench": [
            "query_address"
        ],
    }
    dump = True

    def get_build_targets(self, build, machine):
        targets = super(MdbBench, self).get_build_targets(build, machine)
        for impl in self.impls:
            targets.append('%s/sbin/%s' % (machine.get_bootarch(), impl))
        return targets

    def run_one(self, build, machine, testdir, count, measure, reset, impl, runs):
        debug.log('running %s m=%s c=%s' % (impl, measure, count))
        yield "[mdb_bench of %s]\n" % impl
        modules = self.get_modules(build, machine)
        bench_args = ["count=%s"%count, "reset=%s"%reset, "runs=%d"%runs, "measure=%s"%measure]
        modules.add_module(impl, bench_args)
        self.boot(machine, modules)
        self.set_timeout(datetime.timedelta(hours=1))
        for line in self.collect_data(machine):
            yield line

    def run(self, build, machine, testdir):
        for c, m, r, i in product(self.counts, self.measures, self.resets, self.impls):
            for l in self.run_one(build, machine, testdir, c, m, r, i, 1000):
                yield l

        for i in set(self.impls) & set(self.impl_measures.keys()):
            for c, m, r in product(self.counts, self.impl_measures[i], self.resets):
                for l in self.run_one(build, machine, testdir, c, m, r, i, 1000):
                    yield l

        if self.dump:
            for c, r in product(self.counts, self.resets):
                for l in self.run_one(build, machine, testdir, c, "dump", r, "mdb_bench", 10):
                    yield l

    def process_data(self, testdir, rawiter):
        results = RowResults(['impl', 'reset', 'measure', 'count', 'ticks'])
        caps = RowResults(['run', 'reset', 'count', 'base', 'bits', 'flags'], name="caps")
        impl = None
        reset = None
        measure = None
        count = None
        dumping = False
        for line in rawiter:
            m = re.match(r"\[mdb_bench of (\w+)\]", line)
            if m:
                impl = m.group(1)
                continue
            m = re.match(r"\[mdb_bench dumping\]", line)
            if m:
                dumping = True
            if self.boot_phase:
                continue
            m = re.match(r"([^:/]+)/(\d+):dump:([^:]+): 0x([0-9a-fA-F]+)/(\d+) ([c.][a.][d.])", line)
            if m:
                caps.add_row([m.group(3), m.group(1), m.group(2), m.group(4), m.group(5), m.group(6)])
                continue
            m = re.match(r"([^:]+):([^:]+): (\d+)/(\d+)", line)
            if m:
                reset = m.group(1)
                measure = m.group(2)
                count = m.group(4)
                ticks = m.group(3)
                results.add_row([impl, reset, measure, count, ticks])
                continue

        results = [results]
        if dumping:
            results.append(caps)
        return results
