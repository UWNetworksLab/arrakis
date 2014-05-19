{- 
   Checks: Mackerel compile-time checks
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module Checks where

import MackerelParser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import System.FilePath
import qualified TypeName as TN
import qualified TypeTable as TT
import qualified RegisterTable as RT
import qualified Space
import qualified Dev
import qualified Fields
import qualified Data.Maybe

import Text.Printf
import Data.List
import System.Environment
import System.Exit
import System.IO

data MacError = MacError SourcePos String 
              deriving Show

data CheckResult = Either String [ MacError ]

check_all :: String -> Dev.Rec -> Maybe [String]
check_all inf dev = 
    let errors = (check_devname inf dev) ++
                 (check_rous dev) ++
                 (check_dous dev) ++
                 (check_undef_consts dev) ++
                 (check_undef_regtypes dev) ++
                 (check_dup_types dev ) ++
                 (check_dup_regs dev ) ++
                 (check_dup_vals dev ) ++
                 (check_overlap dev ) ++
                 (check_undef_spaces dev )
    in
      if (length errors) > 0
      then
         let sort_errs = [ ((sourceName p), 
                            (sourceLine p), 
                            (sourceColumn p), 
                            s) | (MacError p s) <- errors ]
         in Just [ printf "%s:%d:%d: %s" n l c s | (n,l,c,s) <- sort sort_errs ]
      else Nothing
      

check_devname :: String -> Dev.Rec -> [ MacError ]
check_devname inf dev = 
  let (devname_f, ext) = splitExtension $ takeFileName inf
      devname_d = Dev.name dev
  in
   if devname_f /= devname_d
   then [ (MacError (initialPos inf)
           (printf "File %s describes dev %s not %s" inf devname_d devname_f))]
   else []

--
-- Check for Registers of Unusual Size
--

check_rous :: Dev.Rec -> [ MacError ]
check_rous d = 
    [ make_rous_error t 
    | t@(TT.RegFormat {}) <- (Dev.types d), check_rous_type t ]
    ++
    [ make_rous_error t 
    | RT.Rec { RT.tpe = t@(TT.ConstType {}) } <- (Dev.registers d), check_rous_type t ]

check_rous_type t = notElem (TT.tt_size t) [ 8, 16, 32, 64 ]
make_rous_error t = 
    (MacError (TT.pos t)
     (if TT.tt_size t == -1 
      then 
        (printf "Register type '%s' (%s) has no width() specifier"
         (TT.type_name t) (TT.tt_desc t))
      else
        (printf "Type '%s' (%s) is a Register Of Unusual Size (%d bits)"
         (TT.type_name t) (TT.tt_desc t) (TT.tt_size t))))
      
--
-- Check for Data types of Unusual Size
--

check_dous :: Dev.Rec -> [ MacError ]
check_dous d = 
    [ make_dous_error t | t@(TT.DataFormat {}) <- (Dev.types d), check_dous_type t ]

-- XXX Make this a bit more lenient. 
check_dous_type t = notElem (TT.tt_size t) [ 8, 16, 32, 64, 96, 128, 160, 224, 
                                             256, 384, 512 ] 
make_dous_error t = 
    (MacError (TT.pos t)
     (printf "Data type '%s' (%s) is a Datatype Of Unusual Size (%d bits)"
                 (TT.type_name t)
                 (TT.tt_desc t)
                 (TT.tt_size t)))
 
-- 
-- Check for undefined constant types: every use of a constant type in
-- a register definition must have a corresponding constant type
-- definition.
--
check_undef_consts :: Dev.Rec -> [ MacError ]
check_undef_consts d = 
    let clist = [ (TT.tt_name c) | c@(TT.ConstType {}) <- Dev.all_types d ]
    in 
    concat [ check_undef_consts_reg r clist | r <- (Dev.registers d) ]
           
check_undef_consts_reg :: RT.Rec -> [ TN.Name ] -> [MacError]
check_undef_consts_reg r clist = 
    [ make r f | f <- (RT.fl r), check r clist f ]
    where
      check r clist f = 
          case Fields.tpe f of
            Nothing -> False
            Just t -> notElem t clist
      make r f = 
          (MacError (Fields.pos f)
           (printf "Field '%s' (%s) of register '%s' (%s) is of undefined type '%s'"
            (Fields.name f) (Fields.desc f) (RT.name r) (RT.desc r) (TN.toString $ Data.Maybe.fromJust $ Fields.tpe f)))

-- 
-- Check for undefined register types (every register must have a type)
--
check_undef_regtypes :: Dev.Rec -> [ MacError ]
check_undef_regtypes d = 
    [ make r | r <- (Dev.registers d), not (check r (Dev.types d)) ]
    where
      check r ttbl = TN.is_builtin_type (RT.typename r)
                     || elem (RT.typename r) [ (TT.tt_name t) | t <- (Dev.types d)]
      make r = (MacError (RT.pos r)
                (printf "Register '%s' (%s) is of undefined type '%s'"
                            (RT.name r) (RT.desc r) (RT.origtype r) ))



check_dups :: [String] -> ( String -> MacError ) -> [ MacError ]
check_dups names errfn = [ errfn n | n <- names \\ nub names ]

--
-- Duplicate types
-- 
check_dup_types :: Dev.Rec -> [ MacError ]
check_dup_types d = 
    let names = map TT.tt_name (Dev.types d)
        dups = [ n | n <- names \\ nub names ]
    in [ make_dup_type_error d n | n <- dups ]
                   
make_dup_type_error :: Dev.Rec -> TN.Name -> MacError
make_dup_type_error d n =
    let cl = [ (TT.pos c, TT.tt_desc c) | c <- (Dev.types d), (TT.tt_name c) == n ]
        l = sort cl
        (p, _) = head l
    in
      (MacError p
       (printf "Type name '%s' is multiply defined, as:%s" (TN.toString n)
               (concat [ (printf "\n  '%s' (%s)" td (show tp))::String | (tp, td) <- l ])))

-- 
-- Duplicate register names
--
check_dup_regs :: Dev.Rec -> [ MacError ]
check_dup_regs d = 
    check_dups (map RT.name rtbl) (make_dup_reg_error rtbl)
    where rtbl = Dev.registers d

make_dup_reg_error :: [RT.Rec] -> String -> MacError
make_dup_reg_error rtbl n =
    let l = [ (RT.pos c, RT.desc c) | c <- rtbl, (RT.name c) == n ]
        (p, _) = head l
    in
      (MacError p
       (printf "Register '%s' is multiply defined, as:%s" n
               (concat [ (printf "\n  '%s' (%s)" td (show tp))::String | (tp, td) <- l ])))

-- 
-- Duplicate constant values
--
check_dup_vals :: Dev.Rec -> [ MacError ]
check_dup_vals d = 
    let cvals = concat([ [ v | v <- TT.tt_vals c ] | c@(TT.ConstType {}) <- (Dev.types d) ])
    in check_dups (map TT.cname cvals) (make_dup_val_error cvals)
       
make_dup_val_error cvl n =
    let l = [ (TT.cpos c, TT.cdesc c) | c <- cvl, (TT.cname c) == n ]
        (p, _) = head l
    in
      (MacError p
       (printf "Constant value '%s' is multiply defined, as:%s" n
        (concat [ (printf "\n  '%s' (%s)" td (show tp))::String | (tp, td) <- l ])))

-- 
-- Undefined address spaces
--
check_undef_spaces :: Dev.Rec -> [MacError ]
check_undef_spaces d =
    let l = [ (RT.spc_id r, RT.pos r) 
              | r <- (Dev.registers d), (RT.spc r) == Space.UndefinedSpace ]
    in
      [ MacError p (printf "Undefined address space '%s'" n) 
        | (n,p) <- l ]

--
-- Registers overlapping
--
check_overlap :: Dev.Rec -> [ MacError ]
check_overlap d = 
    let l = [ r | r <- Dev.registers d, not (RT.also r) ]
    in
      check_overlap1 l

check_overlap1 [] = []
check_overlap1 [s] = []
check_overlap1 (h:t) = 
    (check_overlap1 t)
    ++ 
    [ make_overlap_error h te | te <- t, RT.overlap h te ]

make_overlap_error :: RT.Rec -> RT.Rec -> MacError
make_overlap_error r1 r2 = 
    MacError (RT.pos r1) (printf "Register '%s' overlaps with register '%s' at '%s'" (RT.name r1) (RT.name r2) (show (RT.pos r2)) )
