This is the distributed (per core) mem_serv.  

It should be started on one core, and takes arguments 
telling it how to further proceed.  The possible 
arguments are:

-m: be a master
-w: be a worker 
-a: spawn on all available cores
-c <list>: spawn on the given list of cores
-x <list>: don't spawn on the given list of cores
-n <num>: spawn on a maximum of 'num' cores
-r <num>: each core should be responsible for <num> bytes of memory

Typically the -w argument is only passed by the master to 
workers on other cores when spawning them.

The included menu.lst file shows an example of how to start 
the distributed mem_serv.

Note that this also requires a modified spawnd that knows 
about mem_serv_dist.

There are also some simple benchmark/test programs included. They all
perform variations on allocating a bunch of memory.  When run with the
distributed mem_serv it will use the local mem_serv, when 
run without it will use the global mem_serv.

Note that there are several alternative implementations of mem_serv_dist.
- no stealing: this version does not steal ram caps from the other mem_serv's 
  when a local server runs out of memory.  This means that each core only has
  access to a limited amount of memory.
- stealing: this version steals ram caps from other mem_serv's when a local 
  server runs out of memory.  Each core therefore has potential access to all 
  the system memory.
  There are two versions of the stealing mem_serv_dist:
  + hybrid: uses THC stubs for the stealing. However, it uses traditional 
    stack-ripped IDC code for starting and running the mem_serv and handling 
    its events.
  + thc: uses THC stubs for stealing.  It also uses THC and THC language 
    extensions for starting and running the mem_serv itself.

The code is structured as follows:
- mem_serv.c: 
  contains all the common mem_serv functionlity, shared by the stealing 
  and non_stealing versions
- steal.c:
  contains code specific to the stealing versions.  Since the stealing 
  versions all use THC stubs, this also contains the thcmain() function.
- no_steal.c:
  contains some dummy stubs for the non stealing version.  It also 
  conatins the main() function for the non stealing version.
- hybrid_support.c:
  contains wrapper functions for doing the non-THC IDC and the dispatch loop.
- thc_support.c:
  contains wrapper functions for doing THC-based communication and the server 
  loop.
