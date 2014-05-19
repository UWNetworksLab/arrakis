{- 
  Type table: list of all register types
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module TypeTable where

import MackerelParser
import Attr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import qualified Fields as F
import qualified TypeName as TN

{--------------------------------------------------------------------

--------------------------------------------------------------------}

data Val = Val { cname :: String,
                 cval :: Expr,
                 cdesc :: String,
                 ctype :: TN.Name,
                 cpos :: SourcePos }
         deriving Show

data Rec = RegFormat  { tt_name :: TN.Name, 
                        tt_size :: Integer,
                        fields :: [F.Rec],
                        tt_desc :: String,
                        pos :: SourcePos }
         | DataFormat { tt_name :: TN.Name, 
                        tt_size :: Integer,
                        fields :: [F.Rec],
                        tt_desc :: String,
                        wordsize :: Integer,
                        pos :: SourcePos }
         | ConstType  { tt_name :: TN.Name,
                        tt_size :: Integer,
                        tt_vals :: [ Val ], 
                        tt_width :: Maybe Integer,
                        tt_desc :: String,
                        pos  :: SourcePos }
         | Primitive  { tt_name :: TN.Name,
                        tt_size :: Integer,
                        tt_attr :: Attr }
           deriving Show 
              
type_name :: Rec -> String
type_name r = TN.toString $ tt_name r

devname :: Rec -> String
devname r = TN.devName $ tt_name r

type_kind :: Rec -> String
type_kind RegFormat  {} = "Register"
type_kind DataFormat {} = "Data"
type_kind ConstType  {} = "Constant"
type_kind Primitive  {} = "Primitive"

-- Is this a primitive (i.e. non-record-like) type.  A key issue here
-- is that this includes constants types; otherwise this is equivalent
-- to is_builtin below.
is_primitive :: Rec -> Bool
is_primitive Primitive {} = True
is_primitive ConstType {} = True
is_primitive _ = False

is_builtin :: Rec -> Bool
is_builtin Primitive { tt_name = n } = TN.is_builtin_type n
is_builtin _ = False

builtin_size :: String -> Integer
builtin_size "uint8" = 8
builtin_size "uint16" = 16
builtin_size "uint32" = 32
builtin_size "uint64" = 64

make_rtypetable :: DeviceFile -> [Rec]
make_rtypetable (DeviceFile (Device devname bitorder _ _ decls) _) = 
  (concat [ make_rtrec d devname bitorder | d <- decls ])
  ++ 
  [ Primitive (TN.fromParts devname ("uint" ++ (show w))) w NOATTR 
  | w <- [ 8, 16, 32, 64 ] ] 

make_rtrec :: AST -> String -> BitOrder -> [Rec]
make_rtrec (RegType nm dsc (TypeDefn decls) p) dev order = 
    [ RegFormat { tt_name = TN.fromParts dev nm,
                  tt_size = (calc_tt_size decls),
                  fields = F.make_list dev NOATTR order 0 decls,
                  tt_desc = dsc,
                  pos = p } ]

make_rtrec (Register nm tt_attrib _ _ dsc (TypeDefn decls) p) dev order = 
    [ RegFormat { tt_name = TN.fromParts dev nm,
                  tt_size = (calc_tt_size decls),
                  fields = F.make_list dev tt_attrib order 0 decls,
                  tt_desc = "Implicit type of " ++ dsc ++ " register",
                  pos = p } ]

make_rtrec (RegArray nm tt_attrib _ _ _ dsc (TypeDefn decls) p) dev order = 
    [ RegFormat { tt_name = TN.fromParts dev nm,
                  tt_size = (calc_tt_size decls),
                  fields = F.make_list dev NOATTR order 0 decls,
                  tt_desc = "Implicit type of " ++ dsc ++ " register array",
                  pos = p } ]

make_rtrec (DataType nm dsc (TypeDefn decls) o w p) dev devorder = 
    let order = if o == NOORDER then devorder else o
        sz = calc_tt_size decls
    in
      [ DataFormat { tt_name = TN.fromParts dev nm,
                     tt_size = sz,
                     fields = F.make_list dev RW order w decls,
                     tt_desc = dsc,
                     wordsize = if w == 0 then sz else w,
                     pos = p } ]
make_rtrec (Constants nm d vs w p) dev devorder = 
  let tn = TN.fromParts dev nm 
      vl = [ make_val tn v | v <- vs ]
  in
   [ ConstType { tt_name = tn,
                 tt_size = case w of 
                   Nothing -> calc_const_size vl
                   Just t -> t,
                 tt_vals = vl,
                 tt_desc = d,
                 tt_width = w,
                 pos = p } ]
make_rtrec _ _ _ = []
                   
calc_const_size :: [Val] -> Integer
calc_const_size vs = 
  let m = maximum [ i | t@Val { cval = (ExprConstant i) } <- vs ] 
  in
   if m <= 0xff then 8
   else if m <= 0xffff then 16
        else if m <= 0xffffffff then 32
             else 64

-- Building constant lists
make_val :: TN.Name -> AST -> Val
make_val tn (ConstVal i e d p) 
    = Val { cname = i, cval = e, cdesc = d, ctype = tn, cpos = p }

calc_tt_size :: [AST] -> Integer
calc_tt_size decls = sum [ sz | (RegField _ sz _ _ _ _) <- decls ]

get_rtrec :: [Rec] -> TN.Name -> Rec
get_rtrec rtinfo nm = 
    let l  = [ rt | rt <- rtinfo, (tt_name rt) == nm ]
    in
      if (length l) > 0
      then head l
      else RegFormat { tt_name = TN.null,
                       tt_size = 32,
                       fields = [],
                       tt_desc = "Failed to find type" ++ show nm,
                       pos = initialPos "no file" }
