{- 
   Poly: limited polynomial arithmetic
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module Poly where

import Data.List

reduce :: [ (Integer, [String]) ] -> [ (Integer, [String]) ]
reduce p = [ (i, sort s) | (i,s) <- reduce1 (sort p), i /= 0 ]

reduce1 :: [ (Integer, [String]) ] -> [ (Integer, [String]) ]
reduce1 [] = []
reduce1 [h] = [h]
reduce1 ((i1, idlist1):(i2, idlist2):t)
    | idlist1 == idlist2 = 
        reduce1 ((i1+i2, idlist1):t)
    | otherwise = 
        (i1, idlist1):(reduce1 ((i2, idlist2):t))

