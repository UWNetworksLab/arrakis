Make sure you have the THC compiler in your path (for example,
/home/netos/tools/barrelfish_tools/i686-pc-linux-gnu/bin/clang)

This example only works for x86_64, and x86_32.  It needs
to go in the appropriate sections in symbolic_targets.mk (i.e., 
not in MODULES_COMMON)

Make sure you have the xmplthc.if file in the /if directory and
an appropriate entry in /if/Hakefile under flounderGenDefs.

Also for THC stubs you also have to include the interface under 
flounderTHCFile in /if/Hakefile.  For example:


-- these are for THC stubs
[ flounderTHCFile (options arch) f
      | f <- [ "bench",
               "ping_pong",
               "xmplthc" ],
             arch <- allArchitectures
]
