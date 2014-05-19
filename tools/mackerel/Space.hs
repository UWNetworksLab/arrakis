{- 
  Space: dealing with various register address spaces
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module Space where

import Text.ParserCombinators.Parsec

data SpaceType = BYTEWISE Integer | VALUEWISE | UNDEF
               deriving (Show, Eq)

data Rec = Builtin { n :: String,
                     d :: String,
                     t :: SpaceType }
         | Defined { n :: String,
                     a :: [ String ],
                     d :: String,
                     devname :: String,
                     t :: SpaceType,
                     p :: SourcePos }
         | UndefinedSpace
         | NoSpace
           deriving (Eq,Show)

make :: String -> [String] -> String -> SpaceType -> SourcePos -> Rec
make name args desc tpe pos = 
    Defined { n = name, a = args, d = desc, devname = "", t = tpe, p = pos }

builtins :: [ Rec ]
builtins = [ Builtin { n = "addr",
                       d = "Physical address space", 
                       t = BYTEWISE 1},
             Builtin { n = "io", 
                       d = "I/O port space",
                       t = BYTEWISE 1},
             Builtin { n = "pci",  
                       d = "PCI configuration space",
                       t = BYTEWISE 1}
           ]

lookup :: String -> [Rec] -> Rec
lookup sn spt = 
    let rl = [ s | s <- spt, (n s) == sn ]
    in if length rl == 0 then UndefinedSpace else head rl

is_builtin :: Rec -> Bool
is_builtin (Builtin _ _ _) = True
is_builtin _ = False

{--
data Interval = Interval RegSpace String Integer Integer
              deriving Ord

make_interval_from_regloc :: RegLoc -> Integer -> Interval
make_interval_from_regloc (RegLoc space base expr) sz =
    Interval space base offset size 
        where
          offset = evaluate expr
          size = if size_matters space 
                 then size 
                 else 1 

intervals_overlap :: Interval -> Interval -> Bool
intervals_overlap (Interval sp1 b1 o1 s1) (Interval sp2 b2 o2 s2) 
    | sp1 != sp2 = false
    | b1 != b2 = false
    | (o1 <= o2) and (o1 + sz) > o2 = true
    | (o2 <= o1) and (o2 + sz) > o1 = true
    | otherwise = false

make_intlist_from_array :: RegLoc -> ArrayLoc -> Integer -> [ Interval ]
make_intlist_from_array (RegLoc space base expr) (ArrayListLoc l) sz = 
--}
