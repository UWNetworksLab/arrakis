The interface definition file xmplcr.if must be in /if.  Also 
the /if/Hakefile must specify this interface under flounderGenDefs. 
For example:

-- whereas these are using the new-style bindings
[ flounderGenDefs (options arch) f
      | f <- [ "bcast",
               "bench",
	       ...
               "xmplcr"]
             arch <- allArchitectures
] ++
