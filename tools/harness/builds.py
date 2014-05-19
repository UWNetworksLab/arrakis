##########################################################################
# Copyright (c) 2009, 2010, 2011, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import sys, os, errno, re
import debug


class Build(object):
    name = None # should be overriden by a subclass

    def __init__(self, options):
        self.build_dir = None
        self.options = options

    def _make_build_dir(self, build_dir=None):
        if build_dir is None:
            build_dir = os.path.join(self.options.buildbase, self.name.lower())
        self.build_dir = build_dir
        debug.verbose('creating build directory %s' % build_dir)
        try:
            os.makedirs(build_dir)
        except OSError, e:
            if e.errno == errno.EEXIST:
                debug.log("reusing existing build in directory %s" % build_dir)
            else:
                raise

    def configure(self, checkout):
        raise NotImplementedError

    def build(self, targets):
        raise NotImplementedError

    def install(self, targets, path):
        """install to the given path"""
        raise NotImplementedError


class HakeBuildBase(Build):
    def _run_hake(self, srcdir, archs):
        # if srcdir is relative, adjust to be wrt build_dir
        print archs
        if not os.path.isabs(srcdir):
            srcdir = os.path.relpath(srcdir, self.build_dir)
        debug.checkcmd([os.path.join(srcdir, "hake", "hake.sh"), "--source-dir", srcdir],
                       cwd=self.build_dir)

    def _get_hake_conf(self, srcdir, archs):
        default_config = {
            "source_dir": "\"%s\"" % srcdir,
            "architectures": "[" + ", ".join("\"%s\"" % a for a in archs) + "]",
            "install_dir": "\".\""
        }
        return default_config

    def _write_hake_conf(self, srcdir, archs):
        # create hake dir
        hakedir = os.path.join(self.build_dir, 'hake')
        if not os.path.isdir(hakedir):
            os.mkdir(hakedir)

        # read default config template
        with open(os.path.join(srcdir, 'hake', 'Config.hs.template')) as fh:
            conf_template = fh.readlines()

        # if srcdir is relative, adjust to be wrt build_dir
        if os.path.isabs(srcdir):
            rel_srcdir = srcdir
        else:
            rel_srcdir = os.path.relpath(srcdir, self.build_dir)

        # get custom configuration options as a dictionary
        conf = self._get_hake_conf(rel_srcdir, archs)

        # create a new config file: template and then local options
        newconf = []
        for line in conf_template:
            # XXX: exclude options from the defaults that are set locally
            # where is the haskell parsing library for python? :)
            if any([line.startswith(k) and re.match(' +=', line[len(k):])
                        for k in conf.keys()]):
                line = '-- ' + line
            newconf.append(line)
        newconf.extend(['\n', '\n', '-- Added by test harness:\n'])
        for item in conf.items():
            newconf.append("%s = %s\n" % item)

        # write it, only if it's different or the old one doesn't exist
        try:
            with open(os.path.join(hakedir, 'Config.hs'), 'r') as fh:
                if fh.readlines() == newconf:
                    return # identical files
        except IOError:
            pass

        with open(os.path.join(hakedir, 'Config.hs'), 'w') as fh:
            fh.writelines(newconf)

    def configure(self, checkout, archs):
        srcdir = checkout.get_base_dir()
        self._make_build_dir()
        self._write_hake_conf(srcdir, archs)
        self._run_hake(srcdir, archs)

        # this should be a nop -- building it here causes us to stop early
        # with any tool or dependency-generation errors before doing test setup
        self.build(["Makefile"])

    @staticmethod
    def split_env(e):
        def split_reduce_env(state, c):
            if not state[0] and c == '\\':
                return True, state[1]
            elif not state[0] and c.isspace():
                state[1].append('')
            elif state[0]:
                ec = '\\'+c
                s = ec.decode('string_escape')
                if s == ec:
                    # decode had no effect, just drop backslash
                    s = c
                state[1][-1] += s
            else:
                state[1][-1] += c
            return False, state[1]

        e = e.lstrip()
        e = reduce(split_reduce_env, e, (False, ['']))[1]
        e = filter(bool, e)
        return e

    def build(self, targets):
        makeopts = self.split_env(os.environ.get('MAKEOPTS', ''))
        debug.checkcmd(["make"] + makeopts + targets, cwd=self.build_dir)

    def install(self, targets, path):
        debug.checkcmd(["make", "install",
                               "INSTALL_PREFIX=%s" % path,
                               "MODULES=%s" % (" ".join(targets))],
                       cwd=self.build_dir)


class HakeReleaseBuild(HakeBuildBase):
    """Release build (optimisations, no debug information)"""
    name = 'release'

    def _get_hake_conf(self, *args):
        conf = super(HakeReleaseBuild, self)._get_hake_conf(*args)
        conf["cOptFlags"] = "\"-O2 -DNDEBUG\""
        return conf

class HakeReleaseGem5Build(HakeReleaseBuild):
    """Release build (optimisations, no debug information)"""
    name = 'release_gem5'

    def _get_hake_conf(self, *args):
        conf = super(HakeReleaseGem5Build, self)._get_hake_conf(*args)
        conf["armv7_platform"] = '"gem5"'
        return conf


class HakeReleaseTraceBuild(HakeBuildBase):
    """optimisations, no debug information, and tracing """
    name = 'release_trace'

    def _get_hake_conf(self, *args):
        conf = super(HakeReleaseBuild, self)._get_hake_conf(*args)
        conf["cOptFlags"] = "\"-O2 -DNDEBUG\""
        conf["trace"] = "True"
        return conf

class HakeDebugBuild(HakeBuildBase):
    """Default Hake build: debug symbols, optimisations, assertions"""
    name = 'debug'

    def _get_hake_conf(self, *args):
        conf = super(HakeDebugBuild, self)._get_hake_conf(*args)
        conf["cOptFlags"] = "\"-g -O2\""
        return conf

class HakeDebugTraceBuild(HakeBuildBase):
    """debug symbols, optimisations, assertions, and tracing"""
    name = 'debug_trace'

    def _get_hake_conf(self, *args):
        conf = super(HakeDebugTraceBuild, self)._get_hake_conf(*args)
        conf["cOptFlags"] = "\"-g -O2\""
        conf["trace"] = "True"
        return conf


all_builds = [HakeReleaseBuild, HakeDebugBuild, HakeReleaseTraceBuild,
              HakeDebugTraceBuild, HakeReleaseGem5Build]

def mk_libc_builds():
    def newlib_conf(self, *args):
        conf = super(self.__class__, self)._get_hake_conf(*args)
        conf['libc'] = '"newlib"'
        return conf
    def oldc_conf(self, *args):
        conf = super(self.__class__, self)._get_hake_conf(*args)
        conf['libc'] = '"oldc"'
        return conf
    for b in list(all_builds):
        c = type(b.__name__ + 'Newlib',
                (b,),
                {'name' : b.name + '_newlib', '_get_hake_conf': newlib_conf})
        all_builds.append(c)
        c = type(b.__name__ + 'Oldlib',
                (b,),
                {'name' : b.name + '_oldc', '_get_hake_conf': oldc_conf})
        all_builds.append(c)

mk_libc_builds()


class ExistingBuild(HakeBuildBase):
    '''Dummy build class for an existing Hake build dir.'''
    name = 'existing'

    def __init__(self, options, build_dir):
        super(ExistingBuild, self).__init__(options)
        debug.verbose('using existing build directory %s' % build_dir)
        self.build_dir = build_dir

    def configure(self, *args):
        pass


def existingbuild(*args):
    """construct the build class for an existing build"""
    return ExistingBuild(*args)
