This is the startup service.  It reads through the local /bootmodules file 
and starts up any distributed services and applications (general user 
domains) it finds.

It first starts up distributed services (identified by 'dist-serv' on their 
command line) one by one, waiting for each to complete before starting the 
next one.  

Then it starts up all the apps (those with no 'boot' or 'dist-serv' on
their command line). For apps it does not wait for each to complete 
before starting the next one.

Note that startd only runs on one core. It delegates actual startup of 
dispatchers to appropriate spawnds depending on the the distributed 
service's or application's 'core=' command line argument.

An example menu.lst file:

timeout 0

title   Barrelfish
root    (nd)
kernel  /x86_64/sbin/elver loglevel=4
module  /x86_64/sbin/cpu loglevel=4
module  /x86_64/sbin/init

# Domains spawned by init
module  /x86_64/sbin/mem_serv
module  /x86_64/sbin/monitor

# Special boot time domains spawned by monitor
module  /x86_64/sbin/chips boot
module  /x86_64/sbin/ramfsd boot
module  /x86_64/sbin/skb boot
modulenounzip /skb_ramfs.cpio.gz nospawn
module  /x86_64/sbin/pci boot
module  /x86_64/sbin/spawnd boot
module  /x86_64/sbin/startd boot

# Distributed domains
module  /x86_64/sbin/mem_serv_dist dist-serv core=0 -ma

# General user domains
module  /x86_64/sbin/serial core=0
module  /x86_64/sbin/fish core=0
