{- 
  Register table: list of all registers
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module RegisterTable where

import MackerelParser
import Text.ParserCombinators.Parsec
import Attr
import qualified Fields as F
import qualified TypeTable as TT
import qualified TypeName as TN
import qualified Space

{--------------------------------------------------------------------
Register table: list of all registers 
--------------------------------------------------------------------}

data Rec = Rec { name :: String,        -- Unqualified name of register 
                 fl :: [F.Rec],         -- List of fields (may be empty)
                 attr :: Attr,          -- Attribute (for non-field types)
                 tpe :: TT.Rec,         -- Type of this register
                 origtype :: String,    -- Original name of register type
                 size :: Integer,       -- Width in bits
                 also :: Bool,          -- Only register at this address?
                 desc :: String,        -- Description string
                 spc_id :: String,      -- Address space identifier
                 spc :: Space.Rec,      -- Address space record
                 base :: String,        -- Base variable name
                 offset :: Integer,     -- Offset of register from base
                 arr :: ArrayLoc,       -- Array of locations
                 pos :: SourcePos       -- Source code position
               }
         deriving Show

--
-- Building the register table
--
make_table :: [TT.Rec] -> [AST] -> String -> BitOrder -> [Space.Rec] -> [Rec]
make_table rtinfo decls dn order spt = 
    concat [ (make_reginfo rtinfo d dn order spt) | d <- decls ]


make_regproto :: String -> Attr -> Bool -> RegLoc -> String -> SourcePos -> [Space.Rec] -> TT.Rec -> String
                 -> Rec
make_regproto n atrv als rloc dsc p spt t tname =
    let (si, s, b, o) = get_location rloc spt
    in
      Rec { name = n,
            fl = [],
            attr =atrv, 
            tpe = t,
            origtype = tname,
            size = 0,
            also = als,
            desc = dsc, 
            spc_id = si,
            spc = s,
            base = b, 
            offset = o, 
            arr = (ArrayListLoc []),
            pos = p } 

make_reginfo :: [TT.Rec] -> AST -> String -> BitOrder -> [Space.Rec] -> [Rec]

make_reginfo rtinfo (RegArray n atrv als rloc aloc dsc (TypeDefn decls) p) dn order spt =
    let t = (TT.get_rtrec rtinfo (TN.fromParts dn n))
        r = make_regproto n atrv als rloc dsc p spt t "<inline>"
    in
      [ r { fl = F.make_list dn atrv order 0 decls,
            size = TT.tt_size t,
            arr = aloc } ]

make_reginfo rtinfo (RegArray n atrv als rloc aloc dsc tr@(TypeRef tname dname) p) dn order spt = 
  let tn = TN.fromRef tr dn
      rt = (TT.get_rtrec rtinfo tn)
      r = make_regproto n atrv als rloc dsc p spt rt tname
  in case rt of
    t@(TT.Primitive {}) -> [ r { size = (TT.tt_size rt),
                                 arr = aloc } ]
    t@(TT.RegFormat {}) -> [ r { fl = F.inherit_list atrv (TT.fields rt),
                                 size = (TT.tt_size rt),
                                 arr = aloc } ]
    t@(TT.DataFormat {}) -> [ r { fl = F.inherit_list atrv (TT.fields rt),
                                 size = (TT.tt_size rt),
                                 arr = aloc } ]
    t@(TT.ConstType {}) -> [ r { size = case (TT.tt_width rt) of
                                   Nothing -> -1
                                   Just i  -> i,
                                 arr = aloc } ]

make_reginfo rtinfo (Register n atrv als rloc dsc (TypeDefn decls) p) dn order spt =
    let tn = TN.fromParts dn n
        td = TT.get_rtrec rtinfo tn
        r = make_regproto n atrv als rloc dsc p spt td "<inline>"
    in
      [ r { fl = F.make_list dn atrv order 0 decls,
            size = TT.tt_size td,
            arr = (ArrayListLoc []) } ]

make_reginfo rtinfo (Register n atrv als rloc dsc tr@(TypeRef tname dname) p) dn order spt =
  let tn = TN.fromRef tr dn
      rt = (TT.get_rtrec rtinfo tn)
      r = make_regproto n atrv als rloc dsc p spt rt tname
  in case rt of
    t@(TT.Primitive {}) -> [ r { size = (TT.tt_size rt),
                                 arr = (ArrayListLoc []) } ]
    t@(TT.RegFormat {}) -> [ r { fl = F.inherit_list atrv (TT.fields rt),
                                 size = (TT.tt_size rt),
                                 arr = (ArrayListLoc []) } ]
    t@(TT.DataFormat {}) -> [ r { fl = F.inherit_list atrv (TT.fields rt),
                                 size = (TT.tt_size rt),
                                 arr = (ArrayListLoc []) } ]
    t@(TT.ConstType {}) -> [ r { size = case (TT.tt_width rt) of
                                   Nothing -> -1
                                   Just i  -> i,
                                 arr = (ArrayListLoc []) } ]

make_reginfo rtinfo _ _ _ _ = []

get_location :: RegLoc -> [Space.Rec] -> ( String, Space.Rec, String, Integer )
get_location RegNoLoc _ = 
    ( "", Space.NoSpace, "", 0)
get_location (RegLoc s b o) spt = 
    ( s, Space.lookup s spt, b, o)

overlap :: Rec -> Rec -> Bool
overlap r1 r2 
    | spc_id r1 /= spc_id r2 = False
    | base r1 /= base r2 = False
    | otherwise = 
        any extent_overlap [ (e1, e2) | e1 <- extents r1, e2 <- extents r2 ]

extents :: Rec -> [ (Integer, Integer) ]
extents r 
  | spc r == Space.NoSpace = []
  | otherwise = [ ((offset r) + o, (extentsz (Space.t (spc r)) (size r)))
                | o <- arrayoffsets (arr r) (size r)]
extentsz :: Space.SpaceType -> Integer -> Integer
extentsz (Space.BYTEWISE s) sz = sz `div` 8 `div` s
extentsz _ sz = 1

    

arrayoffsets :: ArrayLoc -> Integer -> [ Integer ]
arrayoffsets (ArrayListLoc []) _ = [0]
arrayoffsets (ArrayListLoc l) _ = l
arrayoffsets (ArrayStepLoc n 0) sz = enumFromThenTo 0 (sz `div` 8) (sz* (n-1) `div` 8)
arrayoffsets (ArrayStepLoc n s) _ = enumFromThenTo 0 s (s* (n-1))

extent_overlap :: ( (Integer, Integer),  (Integer, Integer)) -> Bool
extent_overlap ( (b1, o1), (b2, o2) )
    |  b1 > b2 = ( b2 + o2 > b1 )
    | otherwise = ( b1 + o1 > b2 )

--
-- Lookups
-- 
lookup_reg :: [Rec] -> String -> Rec 
lookup_reg reginfo n = 
    head l where l = [ r | r <- reginfo, (name r) == n ]

lookup_size :: [Rec] -> String -> Integer              
lookup_size reginfo n = (size (lookup_reg reginfo n ))

-- 
-- Properties of registers
--

is_writeable :: Rec -> Bool
is_writeable r@Rec{ attr=a, tpe=t } = 
    case t of
      (TT.Primitive {} ) -> attr_is_writeable a
      (TT.ConstType {} ) -> attr_is_writeable a
      _ -> any F.is_writeable (fl r)

is_readable :: Rec -> Bool
is_readable r@Rec{ attr=a, tpe=t } = 
    case t of
      (TT.Primitive {} ) -> attr_is_readable a
      (TT.ConstType {} ) -> attr_is_readable a
      _ -> any F.is_readable (fl r)

is_writeonly :: Rec -> Bool
is_writeonly r@Rec{ attr=a, tpe=t } = 
    case t of
      (TT.Primitive {} ) -> attr_is_writeonly a
      (TT.ConstType {} ) -> attr_is_writeonly a
      _ -> any F.is_writeonly (fl r)

needs_shadow :: Rec -> Bool
needs_shadow r = is_writeonly r

typename :: Rec -> TN.Name
typename r = (TT.tt_name (tpe r))

is_array :: Rec -> Bool
is_array (Rec { arr = (ArrayListLoc []) } ) = False
is_array r = True

is_noaddr :: Rec -> Bool
is_noaddr (Rec { spc = Space.NoSpace } ) = True
is_noaddr _ = False

num_elements :: Rec -> Integer
num_elements Rec { arr = (ArrayListLoc l) } = toInteger (length l)
num_elements Rec { arr = (ArrayStepLoc num _) } = num

needs_read_before_write :: Rec -> Bool
needs_read_before_write r = any F.is_rsvd (fl r)



data Shadow = Shadow String TN.Name
--                   name   type
get_shadows :: [Rec] -> [Shadow]
get_shadows reginfo = 
    [ Shadow (name r) (typename r) | r <- reginfo, needs_shadow r ]

get_shadow_registers :: [Rec] -> [Rec]
get_shadow_registers reginfo = [ r | r <- reginfo, needs_shadow r ]
