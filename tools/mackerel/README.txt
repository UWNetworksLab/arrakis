MACKEREL

Mackerel is an IDL for devices, a bit like a modern version of Devil.
It is in an early stage of development.   It is written in Haskell
and is designed to translate a .dev file in the dev_defs directory
into a single C header file containing all relevant functions for
accessing the device. 

To use it manually, type:

> runghc Main.hs ../../devices/xapic.dev > xapic.h

 - to see what it does, or see the documentation for more information.
