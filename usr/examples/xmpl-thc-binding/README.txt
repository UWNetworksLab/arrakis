Make sure you have the xmplthc.if file in the /if directory.

Also for THC stubs you have to modify the /if/Hakefile and 
include the interface under flounderTHCFile.  For example:


-- these are for THC stubs
[ flounderTHCFile (options arch) f
      | f <- [ "bench",
               "ping_pong",
               "xmplthc" ],
             arch <- allArchitectures
]
