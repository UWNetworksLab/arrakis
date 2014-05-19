##########################################################################
# Copyright (c) 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import sys, os, signal, time, getpass, subprocess, socket, pty
import debug, machines, msrc_machinedata
from machines import Machine, MachineLockedError

TFTP_PATH='/tftpboot'
TOOLS_PATH='/home/netos/tools/bin'
RACKBOOT=os.path.join(TOOLS_PATH, 'rackboot.sh')
RACKPOWER=os.path.join(TOOLS_PATH, 'rackpower')

class MSRCMachine(Machine):
    _msrc_machines = msrc_machinedata.machines

    def __init__(self, options):
        super(MSRCMachine, self).__init__(options)
        self.lockprocess = None

    def get_bootarch(self):
        b = self._msrc_machines[self.name]['bootarch']
        assert(b in self.get_buildarchs())
        return b

    def get_machine_name(self):
        return self._msrc_machines[self.name]['machine_name']

    def get_buildarchs(self):
        return self._msrc_machines[self.name]['buildarchs']

    def get_ncores(self):
        return self._msrc_machines[self.name]['ncores']

    def get_cores_per_socket(self):
        return self._msrc_machines[self.name]['cores_per_socket']

    def get_tickrate(self):
        return self._msrc_machines[self.name]['tickrate']

    def get_perfcount_type(self):
        return self._msrc_machines[self.name]['perfcount_type']

    def get_hostname(self):
        return self.get_machine_name()

    def get_ip(self):
        return socket.gethostbyname(self.get_hostname())

    def get_tftp_dir(self):
        user = getpass.getuser()
        return os.path.join(TFTP_PATH, user, self.name + "_harness")

    def _write_menu_lst(self, data, path):
        debug.verbose('writing %s' % path)
        debug.debug(data)
        f = open(path, 'w')
        f.write(data)
        f.close()

    def _set_menu_lst(self, relpath):
        ip_menu_name = os.path.join(TFTP_PATH, "menu.lst." + self.get_ip())
        debug.verbose('relinking %s to %s' % (ip_menu_name, relpath))
        os.remove(ip_menu_name)
        os.symlink(relpath, ip_menu_name)

    def set_bootmodules(self, modules):
        fullpath = os.path.join(self.get_tftp_dir(), 'menu.lst')
        relpath = os.path.relpath(fullpath, TFTP_PATH)
        tftppath = '/' + os.path.relpath(self.get_tftp_dir(), TFTP_PATH)
        self._write_menu_lst(modules.get_menu_data(tftppath), fullpath)
        self._set_menu_lst(relpath)

    def lock(self):
        """Use conserver to lock the machine."""

        # find out current status of console
        debug.verbose('executing "console -i %s" to check state' %
                      self.get_machine_name())
        proc = subprocess.Popen(["console", "-i", self.get_machine_name()],
                                stdout=subprocess.PIPE)
        line = proc.communicate()[0]
        assert(proc.returncode == 0)

        # check that nobody else has it open for writing
        myuser = getpass.getuser()
        parts = line.strip().split(':')
        conname, child, contype, details, users, state = parts[:6]
        if users:
            for userinfo in users.split(','):
                mode, username, host, port = userinfo.split('@')[:4]
                if 'w' in mode and username != myuser:
                    raise MachineLockedError # Machine is not free

        # run a console in the background to 'hold' the lock and read output
        debug.verbose('starting "console %s"' % self.get_machine_name())
        # run on a PTY to work around terminal mangling code in console
        (masterfd, slavefd) = pty.openpty()
        self.lockprocess = subprocess.Popen(["console", self.get_machine_name()],
                                            close_fds=True,
                                            stdout=slavefd, stdin=slavefd)
        os.close(slavefd)
        # XXX: open in binary mode with no buffering
        # otherwise select.select() may block when there is data in the buffer
        self.console_out = os.fdopen(masterfd, 'rb', 0)

    def unlock(self):
        if self.lockprocess is None:
            return # noop
        debug.verbose('terminating console process (%d)' % self.lockprocess.pid)
        os.kill(self.lockprocess.pid, signal.SIGTERM)
        self.lockprocess.wait()
        self.lockprocess = None

    def __rackboot(self, args):
        debug.checkcmd([RACKBOOT] + args + [self.get_machine_name()])

    def setup(self):
        self.__rackboot(["-b", "-n"])

    def __rackpower(self, arg):
        try:
            debug.checkcmd([RACKPOWER, arg, self.get_machine_name()])
        except subprocess.CalledProcessError:
            debug.warning("rackpower %s %s failed" %
                          (arg, self.get_machine_name()))

    def reboot(self):
        self.__rackpower('-r')

    def shutdown(self):
        self.__rackpower('-d')

    def get_output(self):
        return self.console_out


for n in sorted(MSRCMachine._msrc_machines.keys()):
    class TmpMachine(MSRCMachine):
        name = n
    machines.add_machine(TmpMachine)
