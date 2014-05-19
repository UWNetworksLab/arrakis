{- 
   Arch.hs: Architecture-specific information needed for stub generation.

  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module Arch (Arch (..), parse_arch) where

import Syntax

-- everything the generic LMP backend needs to know about the architecture
data Arch = Arch {
    archname :: String, -- name of the architecture
    
    -- architecture-specific sizes
    wordsize :: Int,    -- size of words, in bits
    ptrsize  :: Int,    -- size of pointers, in bits
    sizesize :: Int,    -- size of size_t, in bits
    enum_type :: TypeBuiltin, -- type of an enumeration

    -- details of the barrelfish LMP channel implementation for this arch
    lmp_words :: Int,   -- payload of an LMP message, in number of words
    lrpc_words :: Int   -- payload of LRPC, in number of words
}

x86_64 = Arch {
    archname = "x86_64",
    wordsize = 64,
    ptrsize = 64,
    sizesize = 64,
    enum_type = Int32,
    lmp_words = 10,
    lrpc_words = 4
}

x86_32 = Arch {
    archname = "x86_32",
    wordsize = 32,
    ptrsize = 32,
    sizesize = 32,
    enum_type = Int32,
    lmp_words = 4,
    lrpc_words = 0
}

arm = Arch {
    archname = "arm",
    wordsize = 32,
    ptrsize = 32,
    sizesize = 32,
    enum_type = Int32,
    lmp_words = 4,
    lrpc_words = 0
}

all_archs = [x86_64, x86_32, arm]

-- for option parsing: find the matching arch info
parse_arch :: String -> Maybe Arch
parse_arch n = case [a | a <- all_archs, archname a == n] of
        [a] -> Just a
        _ -> Nothing
