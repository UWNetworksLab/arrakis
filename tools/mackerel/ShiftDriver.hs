{- 
   ShiftDriver: Mackerel backend for device drivers
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, 2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module ShiftDriver where

import System.IO
import System.Exit
import Data.List
import Data.Bits
import Text.Printf
import MackerelParser
import Checks

import Attr
import qualified Space
import qualified CAbsSyntax as C
import qualified TypeName as TN
import qualified TypeTable as TT
import qualified RegisterTable as RT
import qualified Fields
import qualified Dev

------------------------------------------------------------------------
-- Standardized names of C variables
------------------------------------------------------------------------
cv_i = "_i"             -- Index for register arrays
cv_dev = "_dev"         -- Device structure 
cv_avail = "_avail"     -- Available buffer space for snprintf
cv_size = "_size"       -- Size of buffer for snprintf
cv_s = "_s"             -- Buffer ptr for snprint
cv_rc = "_rc"           -- Return value from snprintf
cv_r = "_r"             -- Accumulator for snprintf values
cv_regval = "_regval"   -- Value of type register contents
cv_fieldval = "_fieldval" -- Value of type field contents
cv_dtptr = "_dtptr"     -- Value of type pointer to datatype struct
cv_e = "_e"             -- Enumeration type value

-------------------------------------------------------------------------
-- The C Language mapping: top level name definitions
-------------------------------------------------------------------------

--
-- Device-related names
--

device_c_name :: String
device_c_name = "__DN(t)"

device_shadow_field_name :: RT.Rec -> String
device_shadow_field_name rt = (RT.name rt) ++ "_shadow"

device_initialize_fn_name :: Dev.Rec -> String
device_initialize_fn_name d = qual_devname d [ "initialize" ]

device_print_fn_name :: Dev.Rec -> String
device_print_fn_name d = qual_devname d [ "pr" ]

device_prefix_macro_name :: Dev.Rec -> String
device_prefix_macro_name d = qual_devname d ["PREFIX"]

device_initial_enum_name :: Dev.Rec -> String
device_initial_enum_name d = qual_devname d ["initials"]

--
-- Space-related names
-- 
space_read_fn_name :: Space.Rec -> Integer -> String
space_read_fn_name s w = 
  printf "__DN(%s)" (concat $ intersperse "_" [ Space.n s, "read", show w ])

space_write_fn_name :: Space.Rec -> Integer -> String
space_write_fn_name s w = 
  printf "__DN(%s)" (concat $ intersperse "_" [ Space.n s, "write", show w ])

--
-- Constants-related names
--
constants_c_name :: TT.Rec -> String
constants_c_name c = qual_typerec c ["t"]

constants_elem_c_name :: TT.Val -> String
constants_elem_c_name v = qual_device (TT.ctype v) [ TT.cname v ]

constants_print_fn_name :: TN.Name -> String
constants_print_fn_name c = qual_typename c ["prtval"]

constants_describe_fn_name :: TT.Rec -> String
constants_describe_fn_name c = qual_typerec c ["describe" ]

--
-- Register and datatype-related names
--
regtype_c_name :: TT.Rec -> String
regtype_c_name rt 
    | TT.is_builtin rt = (TN.typeName $ TT.tt_name rt) ++ "_t"
    | otherwise = qual_typerec rt ["t"]

regtype_initial_macro_name :: TT.Rec -> String
regtype_initial_macro_name rt = qual_typerec rt ["default"]

regtype_extract_fn_name :: TT.Rec -> Fields.Rec -> String
regtype_extract_fn_name rt f = qual_typerec rt [ Fields.name f, "extract" ]

regtype_insert_fn_name :: TT.Rec -> Fields.Rec -> String
regtype_insert_fn_name rt f = qual_typerec rt [ Fields.name f, "insert" ]

regtype_print_fn_name :: TT.Rec -> String
regtype_print_fn_name rt = qual_typerec rt [ "prtval"]

datatype_array_c_name :: TT.Rec -> String
datatype_array_c_name rt = qual_typerec rt [ "array", "t"]

datatype_size_macro_name :: TT.Rec -> String
datatype_size_macro_name rt = qual_typerec rt ["size"]

--
-- Register- and register array-related names
--
register_initial_name :: RT.Rec -> String
register_initial_name r = qual_register r [ "initial" ]

register_read_fn_name :: RT.Rec -> String
register_read_fn_name r = qual_register r ["rd"]

register_write_fn_name :: RT.Rec -> String
register_write_fn_name r = qual_register r ["wr"]

register_rawread_fn_name :: RT.Rec -> String
register_rawread_fn_name r = qual_register r ["rawrd"]

register_rawwrite_fn_name :: RT.Rec -> String
register_rawwrite_fn_name r = qual_register r ["rawwr"]

register_shadow_name :: RT.Rec -> String
register_shadow_name r = qual_register r ["shadow"]

register_c_name :: RT.Rec -> String
register_c_name r = regtype_c_name $ RT.tpe r

field_c_name :: Fields.Rec -> String
field_c_name f = 
    case Fields.tpe f of 
      Nothing -> round_field_size $ Fields.size f
      Just t -> qual_typename t ["t"]

register_print_fn_name :: RT.Rec -> String
register_print_fn_name rt = qual_register rt ["pr"]

register_read_field_fn_name :: RT.Rec -> Fields.Rec -> String
register_read_field_fn_name r f = qual_register r [ Fields.name f, "rdf"]

register_read_field_from_shadow_fn_name :: RT.Rec -> Fields.Rec -> String
register_read_field_from_shadow_fn_name r f = 
  qual_register r [Fields.name f, "rd", "shadow"]

register_write_field_fn_name :: RT.Rec -> Fields.Rec -> String
register_write_field_fn_name r f = qual_register r [ Fields.name f, "wrf"]

regarray_length_macro_name :: RT.Rec -> String
regarray_length_macro_name r = qual_register r [ "length" ]

regarray_print_fn_name :: RT.Rec -> String
regarray_print_fn_name rt = qual_register rt ["pri"]

-------------------------------------------------------------------------
-- Convenience functions for generating the C mapping
-------------------------------------------------------------------------

--
-- Given a field width in bits, return the C type of the smallest
-- possible unsigned integer capable of holding it.
--
round_field_size w 
    | w <= 8 =                  "uint8_t" 
    | ( w > 8 && w <= 16 ) =    "uint16_t" 
    | ( w > 16 && w <= 32 ) =   "uint32_t" 
    | otherwise =               "uint64_t"      

--
-- Take a list of scope names and translate to a C identifier. 
-- 

qual_devname :: Dev.Rec -> [ String ] -> String
qual_devname d l = 
  concat $ intersperse "_" ([Dev.name d] ++ l)

qual_device :: TN.Name -> [ String ] -> String
qual_device t l =
  concat $ intersperse "_" ([TN.devName t] ++ l)

qual_typename :: TN.Name -> [ String ] -> String
qual_typename (TN.Name dn tn) l = concat $ intersperse "_" ([dn, tn] ++ l)

qual_typerec :: TT.Rec -> [ String ] -> String
qual_typerec t l = qual_typename (TT.tt_name t) l 

qual_register :: RT.Rec -> [ String ] -> String
qual_register r l = qual_device (RT.typename r) ([RT.name r] ++ l)
                    
--
-- Generate a simple automatic variable declaration with optional initializer.
--
simple_var :: String -> String -> Maybe C.Expr -> C.Stmt
simple_var t n e
    = C.VarDecl C.NoScope C.NonConst (C.TypeName t) n e

--
-- Generate a simple for loop with i = 0 to something.
--
simple_for :: C.Expr -> [ C.Stmt ] -> C.Stmt
simple_for end body 
    = C.For (C.Assignment (C.Variable cv_i) (C.NumConstant 0))
             (C.Binary C.LessThan (C.Variable cv_i) end)
             (C.PostInc (C.Variable cv_i))
             body

--
-- Given a field width in bits, return the C snprintf format
-- specifying to correctly format it.
--    
field_fmt_str size 
    | size <= 8 = "PRIx8"
    | size <= 16 = "PRIx16"
    | size <= 32 = "PRIx32"
    | otherwise = "PRIx64"

--
-- Percent-escape a string so it can be used in a format to snprintf.
--
percent_escape :: String -> String
percent_escape s 
    = concat [ if c == '%' then "%%" else [c] | c <- s ]


--
-- Define a static inline function which looks like an snprintf in its
-- calling conventions, and maintains its internal buffer variables
-- accordingly.
--
snprintf_like_defn :: String -> [ C.Param ] -> [ C.Stmt ] -> C.Unit
snprintf_like_defn name extra_args main_body = 
    C.StaticInline (C.TypeName "int") name args body
    where args = [ C.Param (C.Ptr $ C.TypeName "char") cv_s,
                   C.Param (C.TypeName "size_t") cv_size ] ++ extra_args
          body = [ simple_var "int" cv_r (Just $ C.NumConstant 0),
                   simple_var "int" cv_avail Nothing,
                   simple_var "int" cv_rc Nothing ]
                 ++ 
                 main_body
                 ++ 
                 [ C.Return $ C.Variable cv_r ]

--
-- Wrap a call to a function which acts like snprintf (i.e. takes a
-- buffer, and a size, and tries to fit the output into the buffer).
-- The code generated here can be safely nested inside another
-- snprintf-like function as long as the variable names 'avail',
-- 'size', 'r', and 'rc' are declared.
--
snprintf_like_call :: String -> [C.Expr] -> C.Stmt
snprintf_like_call n a =
    C.StmtList [ C.Ex $ C.Assignment (C.Variable cv_avail) $ 
                         C.Ternary 
                               (C.Binary C.GreaterThan
                                      (C.Variable cv_r) 
                                      (C.Variable cv_size)) 
                               (C.NumConstant 0) 
                               (C.Binary C.Minus 
                                      (C.Variable cv_size) 
                                      (C.Variable cv_r)),
               C.Ex $ C.Assignment 
                     (C.Variable cv_rc) 
                     (C.Call n ([ C.Binary C.Plus 
                                       (C.Variable cv_s)
                                       (C.Variable cv_r),
                                  C.Variable cv_avail 
                                ] ++ a)),
               C.If 
                     (C.Binary C.And 
                            (C.Binary C.GreaterThan
                                   (C.Variable cv_rc)
                                   (C.NumConstant 0))
                            (C.Binary C.LessThan
                                   (C.Variable cv_rc)
                                   (C.Variable cv_avail)))
                     [ C.Ex $ C.Assignment (C.Variable cv_r) 
                                 (C.Binary C.Plus
                                        (C.Variable cv_r)
                                        (C.Variable cv_rc)) ]
                     []
             ]

snputs_like_call :: String -> C.Stmt
snputs_like_call s = snprintf_like_call "snprintf" [ C.StringConstant $ percent_escape s ]

--
-- Functions to generate masks to select or deselect a subfield of bits
--
select_mask :: (Num a, Bits a) => Integer -> Integer -> Integer -> a
select_mask word_size start width = 
    foldl setBit 0 (enumFromTo (fromInteger $ start) 
                               (fromInteger $ start + width - 1))

deselect_mask :: (Num a, Bits a) => Integer -> Integer -> Integer -> a
deselect_mask word_size start width = 
    foldl complementBit (select_mask word_size start width) 
              (enumFromTo 0 (fromInteger word_size - 1))

--
-- Functions to generate the builtin Mackerel access functions
--
mackerel_read_fn_name :: String -> Integer -> String
mackerel_read_fn_name typename size = 
    printf "mackerel_read_%s_%s" typename (show size)

mackerel_write_fn_name :: String -> Integer -> String
mackerel_write_fn_name typename size = 
    printf "mackerel_write_%s_%s" typename (show size)

 
--
-- Generate a string describing a register field
--           
field_dump :: Fields.Rec -> String
field_dump f 
    = printf "  %s\t(size %d, offset %d, init %x):\t%s\t%s"
      (Fields.name f)
      (Fields.size f)
      (Fields.offset f)
      (Fields.initial f)
      (show $ Fields.attr f)
      (Fields.desc f )

-- translation function mapped to every argument, for generic conversion
-- (eg. types or names) before rendering those arguments in C code

convert_arg (Arg "addr" x) = Arg "mackerel_addr_t" x
convert_arg (Arg "pci" x) = Arg "mackerel_pci_t" x
convert_arg (Arg "io" x) = Arg "mackerel_io_t" x


-------------------------------------------------------------------------
-- Top-level header file rendering code
-------------------------------------------------------------------------

-- Top-level create-a-header-file
compile :: String -> String -> Dev.Rec -> String
compile infile outfile dev = 
    unlines $ C.pp_unit $ device_header_file dev infile

device_header_file_string :: Dev.Rec -> String -> String
device_header_file_string d hdr
    = unlines $ C.pp_unit $ device_header_file d hdr

device_header_file :: Dev.Rec -> String -> C.Unit 
device_header_file d hdr = 
    let sym = "__" ++ (Dev.name d) ++ "_DEV_H"
    in
      C.IfNDef sym ([ C.Define sym [] "1"] ++ (device_def d hdr)) [] 

-- Body of the generated file
device_def :: Dev.Rec -> String -> [ C.Unit ]
device_def dev header = 
                ( [device_preamble dev]
                  ++
                  std_header_files dev
                  ++ 
                  device_prefix_defs dev
                  ++
                  concat [ constants_decl d 
                         | d@(TT.ConstType {}) <- Dev.types dev]
                  ++
                  concat [ regtype_decl d 
                         | d@(TT.RegFormat {}) <- Dev.types dev ] 
                  ++
                  concat [ datatype_decl d 
                         | d@(TT.DataFormat {}) <- Dev.types dev]
                  ++ 
                  (device_struct_def dev)
                  ++
                  (device_initial_values dev)
                  ++ 
                  (device_initialize_fn dev)
                  ++
                  -- Not currently implemented in command line opts
                  -- (device_space_includes dev header) 
                  -- ++
                  concat [ register_decl d | d <- (Dev.registers dev) ]
                  ++
                  [(device_print_fn dev)]
                  ++
                  (device_prefix_undefs dev)
                )

device_preamble :: Dev.Rec -> C.Unit
device_preamble dev = 
    C.MultiComment [ 
           "DEVICE DEFINITION: " ++ (Dev.desc dev),
           "",
           "Copyright (c) 2010, ETH Zurich.",
           "All rights reserved.",
           "",
           "This file is distributed under the terms in the attached LICENSE",
           "file. If you do not find this file, copies can be found by",
           "writing to:",
           "ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich.",
           "Attn: Systems Group.",
           "",
           "THIS FILE IS AUTOMATICALLY GENERATED BY MACKEREL: DO NOT EDIT!" ]

device_c_type :: C.TypeSpec
device_c_type = C.TypeName device_c_name

-- Undefine macros used by the header file
device_prefix_undefs :: Dev.Rec -> [ C.Unit ]
device_prefix_undefs d = [ C.Undef "__DN" ]

-- Define macros used by the header file
device_prefix_defs :: Dev.Rec -> [ C.Unit ]
device_prefix_defs d = 
    let name = Dev.name d 
        prefix = device_prefix_macro_name d
    in
      (device_prefix_undefs d)
      ++
      [ C.Define "__DN" ["x"] (name ++ " ## _ ## x") ]

-- Header files info
std_header_files :: Dev.Rec -> [ C.Unit ]
std_header_files dev = 
    map (C.Include C.Standard) inclist
    where 
      inclist = [ "mackerel/mackerel.h", "inttypes.h" ]
                ++
                [ i ++ ".dev.h" | i <- Dev.imports dev ] 

-- Device representation structure generator              
device_struct_def :: Dev.Rec -> [ C.Unit ]
device_struct_def d 
    = [ C.MultiComment ["Device representation structure"],
        C.StructDecl device_c_name params,
        C.TypeDef (C.Struct device_c_name) device_c_name ]
    where
      params = [ C.Param (C.TypeName n) v 
                     | Arg n v <- map convert_arg (Dev.args d) ]
               ++
               [ device_struct_shadow_field r
                     | r <- RT.get_shadow_registers $ Dev.registers d ]

device_struct_shadow_field :: RT.Rec -> C.Param
device_struct_shadow_field rt =
    let t = if RT.is_array rt then
                C.Array (RT.num_elements rt) (regtype_c_type $ RT.tpe rt) 
            else
                regtype_c_type $ RT.tpe rt
    in
      C.Param t (device_shadow_field_name rt)

device_initial_values :: Dev.Rec -> [ C.Unit ] 
device_initial_values d@( Dev.Rec{ Dev.registers = [] } )
    = [ C.Blank, C.Comment "No registers in this device", C.Blank ]
device_initial_values d
    = [ C.Blank, 
        C.MultiComment ["Initial register values (currently 0)"],
        C.EnumDecl (device_initial_enum_name d)
             [ C.EnumItem (register_initial_name r) (Just $ C.HexConstant $ 0) 
                   | r <- (Dev.registers d) ],
        C.Blank ]

device_initialize_field :: RT.Rec -> C.Stmt
device_initialize_field rt
    = let val = C.Variable $ register_initial_name rt
      in
        if RT.is_array rt then
            C.Block [ simple_var "int" cv_i Nothing,
                       simple_for (C.NumConstant $ RT.num_elements rt)
                                      [ C.Ex $ C.Assignment 
                                                  (regarray_shadow_ref rt) 
                                                  val
                                      ]
                     ]
        else
            C.Ex $ C.Assignment (register_shadow_ref rt) val

device_initialize_arg :: String -> C.Stmt
device_initialize_arg v 
    = C.Ex $ C.Assignment (C.DerefField (C.Variable cv_dev) v) (C.Variable v)

-- Device init function 
device_initialize_fn :: Dev.Rec -> [ C.Unit ]
device_initialize_fn d = 
    [ C.MultiComment [ "Device Initialization function" ],
      C.StaticInline C.Void (device_initialize_fn_name d) params body ]
    where
      args = Dev.args d
      params = [ C.Param (C.Ptr device_c_type) cv_dev ]
               ++
               [ C.Param (C.TypeName n) v 
                     | (Arg n v) <- map convert_arg args ]
      body = [ device_initialize_arg v | (Arg _ v) <- args ]
             -- XXX: Shadow copy initialization broken
             -- ++
             -- [ device_initialize_field rt 
             -- | rt <- RT.get_shadow_registers $ Dev.registers d ]

device_print_fn :: Dev.Rec -> C.Unit
device_print_fn d = 
    snprintf_like_defn (device_print_fn_name d) args body
    where args = [ C.Param (C.Ptr device_c_type) cv_dev ]
          body = [ snputs_like_call "-------------------------\n",
                   snputs_like_call (printf "Dump of device %s (%s):\n" 
                                     (Dev.name d) (percent_escape (Dev.desc d)))
                 ]
                 ++
                 [ device_print_eachreg r | r <- (Dev.registers d)]
                 ++
                 [ snputs_like_call (printf "End of dump of device %s\n" 
                                     (Dev.name d) ),
                   snputs_like_call "-------------------------\n"
                 ]

device_print_eachreg r = 
    snprintf_like_call (register_print_fn_name r) [ C.Variable cv_dev ]

-- XXX: This needs more thorough examination. I don't know how the
-- commandline is interacting with device spaces. Currently, when
-- there are no device spaces defined, the commandline is ignored.
device_space_includes :: Dev.Rec -> String -> [ C.Unit ]
device_space_includes d header
    | all Space.is_builtin (Dev.spaces d) = 
        [ C.MultiComment [ "No user-defined spaces" ] ]
    | header /= "" =
        [ C.MultiComment [ "Space access include overridden by cmd line:"],
          C.Include C.Local header ]
    | otherwise = 
        [ C.MultiComment [ "Include access functions for user-defined spaces"],
          C.Include C.Local $ printf "%s_spaces.h" (Dev.name d) ]

-------------------------------------------------------------------------
-- Render 'constants' declarations
-------------------------------------------------------------------------

-- 
-- Everything we need for a constants definition
--

constants_decl :: TT.Rec -> [ C.Unit ]
constants_decl c = 
    [ constants_comment c,
      constants_typedef c ] ++
    ( constants_enum c ) ++
    [ C.Blank,
      constants_describe_fn c,
      constants_print_fn c ]

constants_c_type :: TT.Rec -> C.TypeSpec
constants_c_type c = C.TypeName $ constants_c_name c 

constants_comment :: TT.Rec -> C.Unit      
constants_comment c =
    C.MultiComment [ printf "Constants defn: %s (%s)" (TN.toString $ TT.tt_name c) (TT.tt_desc c), 
                     case TT.tt_width c of
                       Nothing -> " - no width specified"
                       Just w -> printf " - width %d bits" w ]

constants_enum :: TT.Rec -> [ C.Unit ]
constants_enum c = 
  [ C.Define (constants_elem_c_name v) [] (constants_eval c v) | v <- TT.tt_vals c ]

constants_typedef :: TT.Rec -> C.Unit
constants_typedef c = 
    C.TypeDef (C.TypeName $ round_field_size $ TT.tt_size c) (constants_c_name c)
                     
constants_eval :: TT.Rec -> TT.Val -> String
constants_eval c v = 
  printf "((%s)%s)" (constants_c_name c) (case TT.cval v of 
                                             ExprConstant (-1) -> "(-1LL)"
                                             ExprConstant i -> printf "0x%x" i
                                         )

constants_print_fn :: TT.Rec -> C.Unit
constants_print_fn c = 
    C.StaticInline (C.TypeName "int") (constants_print_fn_name $ TT.tt_name c)
          [ C.Param (C.Ptr $ C.TypeName "char") cv_s,
            C.Param (C.TypeName "size_t") cv_size,
            C.Param (constants_c_type c) cv_e ]
    [ C.VarDecl C.NoScope C.NonConst (C.Ptr $ C.TypeName "char") "d"
      (Just $ C.Call (constants_describe_fn_name c) [ C.Variable cv_e ]),
      C.If (C.Variable "d") 
        [ C.Return $ C.Call "snprintf" 
          [ C.Variable cv_s, 
            C.Variable cv_size, 
            C.StringConstant "%s", 
            C.Variable "d" 
          ] 
        ]
        [ C.Return $ C.Call "snprintf" 
          [ C.Variable cv_s, 
            C.Variable cv_size,
            C.StringCat [ C.QStr "Unknown constant %s value 0x%", 
                          C.NStr "PRIx64" ],
            C.StringConstant (constants_c_name c),
            C.Cast (C.TypeName "uint64_t") (C.Variable cv_e)
          ]
        ]
      ]   

constants_describe_fn :: TT.Rec -> C.Unit
constants_describe_fn c =
    let 
      rep v = C.StringConstant $ printf "%s: %s" (TT.cname v) (TT.cdesc v)
    in
     C.StaticInline (C.Ptr $ C.TypeName "char") (constants_describe_fn_name c)
     [ C.Param (constants_c_type c) cv_e ]
     [ C.Switch (C.Variable cv_e) 
       [ C.Case (C.Variable $ constants_elem_c_name v)
         [ C.Return $ rep v ] 
       | v <- TT.tt_vals c ]
       [ C.Return $ C.Variable "NULL" ]
     ]


-------------------------------------------------------------------------
-- Render register type definitions
-------------------------------------------------------------------------

regtype_c_type :: TT.Rec -> C.TypeSpec
regtype_c_type rt = C.TypeName $ regtype_c_name rt

--
-- All the generated declarations for a register type.
--
regtype_decl :: TT.Rec -> [ C.Unit ]
regtype_decl rt = 
    [ regtype_dump rt,
      regtype_typedef rt,
      regtype_initial_macro rt
    ]
    ++
    (regtype_access_fns rt) 
    ++
    [
      regtype_print_fn rt
    ]


--
-- Emit a comment describing the register type.
--
regtype_dump :: TT.Rec -> C.Unit
regtype_dump rt = 
    C.MultiComment ([ (TT.type_kind rt) ++ " type: " ++ (regtype_c_name rt),
                      "Description: " ++ (TT.tt_desc rt),
                      "Fields:"
                    ]
                    ++ 
                    [ field_dump f | f <- TT.fields rt ])
--
-- Define the register type to be an unsigned integer of appropriate size
--
regtype_typedef :: TT.Rec -> C.Unit
regtype_typedef rt =
    C.TypeDef 
          (C.TypeName $ round_field_size $ TT.tt_size rt)
          (regtype_c_name rt)

--
-- Emit macro for initial register value
--

regtype_initial_macro :: TT.Rec -> C.Unit
regtype_initial_macro rt =
    C.Define sym [] (C.pp_expr $ C.HexConstant val)
    where
      sym = regtype_initial_macro_name rt
      fields = TT.fields rt
      val = foldl (.|.) 0 [ Fields.initial_mask f | f <- fields ]

--
-- Emit functions to extract and insert each field from a value of
-- register contents.
--
regtype_access_fns :: TT.Rec -> [ C.Unit ]
regtype_access_fns rt = 
    concat [ [ regtype_field_extract_fn rt f,
               regtype_field_insert_fn rt f ] 
             | f <- TT.fields rt, not $ Fields.is_anon f ]

--
-- Return the C type name for a field or a register
--

field_c_type :: Fields.Rec -> C.TypeSpec 
field_c_type f = C.TypeName $ field_c_name f

--
-- Emit a function to extract a field from a register type value
--

regtype_field_extract_fn :: TT.Rec -> Fields.Rec -> C.Unit
regtype_field_extract_fn rt f = 
    let t = field_c_type f
        n = regtype_extract_fn_name rt f
        sz = TT.tt_size rt
        arg = C.Param (regtype_c_type rt) cv_regval
        -- ( r & (Fields.extract_mask f) ) >> (Fields.extract_shift f)
        body = C.Return $ 
               C.Cast t (C.Binary C.RightShift 
                         (C.Binary C.BitwiseAnd 
                          (C.Variable cv_regval) 
                          (C.HexConstant $ Fields.extract_mask f sz))
                         (C.NumConstant $ Fields.offset f))
    in
      C.StaticInline t n [ arg ] [ body ]

--
-- Emit a function to insert a field value into a register type value
--
regtype_field_insert_fn :: TT.Rec -> Fields.Rec -> C.Unit
regtype_field_insert_fn rt f = 
    let t = field_c_type f
        n = regtype_insert_fn_name rt f
        rtn = regtype_c_type rt
        sz = TT.tt_size rt
        arg1 = C.Param rtn cv_regval
        arg2 = C.Param t cv_fieldval
        -- return (r & Fields.insert_mask f) | ((rtn)v << (Fields.offset f) & (Fields.insert_mask f))
        -- Note that we cast the field type to the register type, to
        -- ensure that it's large enough when we do the shift
        body = C.Return $ 
               C.Binary C.BitwiseOr
                 (C.Binary C.BitwiseAnd
                    (C.Variable cv_regval)
                    (C.HexConstant $ Fields.insert_mask f sz) )
                 (C.Binary C.BitwiseAnd
                    (C.HexConstant $ Fields.extract_mask f sz)
                    (C.Binary C.LeftShift
                       (C.Cast rtn (C.Variable cv_fieldval))
                       (C.NumConstant $ Fields.offset f)))
    in
      C.StaticInline rtn n [ arg1, arg2 ] [ body ]

--
-- Print out a value of the register or data type
--
regtype_print_fn :: TT.Rec -> C.Unit
regtype_print_fn rt =
    snprintf_like_defn (regtype_print_fn_name rt) args body
    where
        fields = TT.fields rt
        args = [ C.Param (regtype_c_type rt) cv_regval ]
        body = [ field_print_block rt f | f <- fields, not $ Fields.is_anon f ]

--
-- Return a statement (or list) which will correctly format a register
-- field as part of a larger snprintf-like function.
--
field_print_block :: TT.Rec -> Fields.Rec -> C.Stmt
field_print_block _ f@(Fields.Rec { Fields.is_anon = True }) = 
    C.SComment ((Fields.name f) ++ " is anonymous")
field_print_block rt f = 
    case Fields.tpe f of
      Nothing -> 
          let fmt = C.StringCat [ C.QStr $ printf " %s =\t%%" (Fields.name f),
                                   C.NStr (field_fmt_str $ Fields.size f),
                                   C.QStr $ printf "\t(%s)\n" (percent_escape $ Fields.desc f) ]
              val = C.Call (regtype_extract_fn_name rt f) 
                      [ C.Variable cv_regval ]
          in
            snprintf_like_call "snprintf" [fmt, val]
      Just t -> 
          C.StmtList [snputs_like_call $ printf " %s =\t" (Fields.name f),
                      snprintf_like_call (constants_print_fn_name t) 
                                           [ C.Call (regtype_extract_fn_name rt f) [(C.Variable cv_regval)] ],
                      snputs_like_call $ printf "\t(%s)\n" (Fields.desc f)
                     ]

-------------------------------------------------------------------------
-- Render data type definitions
-------------------------------------------------------------------------

datatype_decl :: TT.Rec -> [ C.Unit ]
datatype_decl dt = 
    [ regtype_dump dt, 
      datatype_typedef dt,
      datatype_array_typedef dt,
      datatype_size_macro dt 
    ]
    ++ datatype_access_fns dt
    ++ [ regtype_print_fn dt ]


datatype_typedef :: TT.Rec -> C.Unit
datatype_typedef dt =
    C.TypeDef (C.Ptr $ C.TypeName "uint8_t") (regtype_c_name dt)

datatype_array_typedef :: TT.Rec -> C.Unit
datatype_array_typedef dt =
    let sz = ((TT.tt_size dt) + 7) `div` 8
    in
      C.TypeDef (C.Array sz $ C.TypeName "uint8_t") (datatype_array_c_name dt) 

datatype_size_macro :: TT.Rec -> C.Unit
datatype_size_macro dt = 
    C.GVarDecl C.Static C.Const (C.TypeName "size_t") 
         (datatype_size_macro_name dt) 
         (Just $ C.SizeOfT $ C.TypeName $ datatype_array_c_name dt)

datatype_field_load_size :: Fields.Rec -> Integer
datatype_field_load_size Fields.Rec {Fields.offset=o, Fields.size=s}
    | s + (o `mod` 8) <= 8 = 8
    | s + (o `mod` 16) <= 16 = 16
    | s + (o `mod` 32) <= 32 = 32
    | s + (o `mod` 64) <= 64 = 64
    | otherwise = 0

--
-- Emit functions to extract and insert each field from a value of
-- register contents.
--
datatype_access_fns :: TT.Rec -> [ C.Unit ]
datatype_access_fns rt = 
    concat [ [ datatype_field_extract_fn rt f,
               datatype_field_insert_fn rt f ] 
             | f <- TT.fields rt, not $ Fields.is_anon f ]

--
-- Emit a function to extract a field from a data type value
--
datatype_field_extract_fn :: TT.Rec -> Fields.Rec -> C.Unit
datatype_field_extract_fn rt f = 
    let t = field_c_type f
        n = regtype_extract_fn_name rt f
        arg = C.Param (regtype_c_type rt) cv_dtptr
        load_size = datatype_field_load_size f
        bits_offset = (Fields.offset f) `mod` load_size
        word_offset = ((Fields.offset f) - bits_offset) `div` 8
        mask = select_mask load_size bits_offset (Fields.size f)
        load_c_type = C.TypeName $ round_field_size load_size
        -- ( r & (Fields.extract_mask f) ) >> (Fields.extract_shift f)
        body = C.Return $
               C.Binary C.RightShift 
                     (C.Binary C.BitwiseAnd 
                            (C.DerefPtr 
                                   (C.Cast 
                                          (C.Ptr load_c_type)
                                          (C.Binary C.Plus
                                                 (C.NumConstant word_offset)
                                                 (C.Variable cv_dtptr))))
                            (C.HexConstant $ mask))
                      (C.NumConstant $ bits_offset)
    in
      C.StaticInline t n [ arg ] [ body ]

--
-- Emit a function to insert a field value into a data type value
--
datatype_field_insert_fn :: TT.Rec -> Fields.Rec -> C.Unit
datatype_field_insert_fn rt f = 
    let t = field_c_type f
        n = regtype_insert_fn_name rt f
        rtn = C.TypeName $ round_field_size $ TT.wordsize rt
        arg1 = C.Param (regtype_c_type rt) cv_dtptr
        arg2 = C.Param t cv_fieldval
        load_size = datatype_field_load_size f
        bits_offset = (Fields.offset f) `mod` load_size
        word_offset = ((Fields.offset f) - bits_offset) `div` 8
        smask = select_mask load_size bits_offset (Fields.size f)
        dmask = deselect_mask load_size bits_offset (Fields.size f)
        load_c_type = C.TypeName $ round_field_size load_size
        load_expr = (C.DerefPtr 
                           (C.Cast 
                                  (C.Ptr load_c_type)
                                  (C.Binary C.Plus
                                         (C.NumConstant word_offset)
                                         (C.Variable cv_dtptr))))

        -- return (r & Fields.insert_mask f) | (v << (Fields.offset f) & (Fields.insert_mask f))
        body = C.Ex $ 
               C.Assignment load_expr 
                     (C.Binary C.BitwiseOr
                            (C.Binary C.BitwiseAnd 
                               load_expr 
                               (C.HexConstant dmask))
                            (C.Binary C.BitwiseAnd 
                               (C.HexConstant smask)
                               (C.Binary C.LeftShift
                                (C.Variable cv_fieldval)
                                (C.NumConstant bits_offset)
                               ))
                            )
   in
      C.StaticInline C.Void n [ arg1, arg2 ] [ body ]

-------------------------------------------------------------------------
-- Render register definitions
-------------------------------------------------------------------------

register_c_type :: RT.Rec -> C.TypeSpec
register_c_type r = C.TypeName $ register_c_name r

register_shadow_ref :: RT.Rec -> C.Expr
register_shadow_ref r =
    let deref = C.DerefField (C.Variable cv_dev) (device_shadow_field_name r)
    in 
      if RT.is_array r then
          C.SubscriptOf deref (C.Variable cv_i)
      else
          deref

regarray_shadow_ref :: RT.Rec -> C.Expr
regarray_shadow_ref rt 
    = C.SubscriptOf (register_shadow_ref rt) (C.Variable cv_i)

--
-- All the declarations for a given register.  Note that all
-- type-related stuff is handled above here by regtype_*; these
-- declarations are specific to the register itself.
--
register_decl :: RT.Rec -> [ C.Unit ]
register_decl r = [ register_dump_comment r,
                    regarray_length_macro r,
                    register_rawread_fn r,
                    register_read_fn r,
                    register_rawwrite_fn r,
                    register_write_fn r
                  ] 
                  ++
                  ( register_print_fn r)
                  ++ 
                  (if not $ TT.is_primitive $ RT.tpe r then
                       [ register_read_field_fn r f 
                             | f <- RT.fl r, attr_user_can_read $ Fields.attr f
                         ]
                       ++
                       [ register_read_field_from_shadow_fn r f 
                             | f <- RT.fl r, attr_is_writeonly $ Fields.attr f
                         ]
                       ++
                       [ register_write_field_fn r f 
                             | f <- RT.fl r, attr_user_can_write $ Fields.attr f
                       ]
                   else 
                       []
                  )
          
register_dump_comment :: RT.Rec -> C.Unit
register_dump_comment r 
    = C.MultiComment ([name, typedesc] ++ fields)
      where title = if (RT.is_array r) then " array" else ""
            name = printf "Register%s %s: %s" title (RT.name r) (RT.desc r)
            typedesc = printf "Type: %s (%s)" 
                         (TN.toString $ RT.typename r)
                         (if TT.is_primitive $ RT.tpe r
                          then "primitive type" 
                          else TT.tt_desc $ RT.tpe r)
            fields = if TT.is_primitive $ RT.tpe r
                     then []
                     else [ field_dump f | f <- RT.fl r ]


--
-- Return a declaration for the length of a register array. 
--
regarray_length_macro :: RT.Rec -> C.Unit
regarray_length_macro r 
    | RT.is_array r = 
        (C.GVarDecl 
          C.Static C.Const (C.TypeName "size_t") 
               (regarray_length_macro_name r) 
               (Just $ C.NumConstant $ RT.num_elements r))
    | otherwise = C.NoOp

-- 
-- Do a raw read from a register, if the address is available.
-- 
register_rawread_fn :: RT.Rec -> C.Unit
register_rawread_fn r =
    let 
      rtn = regtype_c_type $ RT.tpe r
      args = (register_arg_list [] r [])
      n = register_rawread_fn_name r    
    in
     if RT.is_noaddr r then
       C.Comment (printf "%s has no address, user must supply %s" 
                 (RT.name r) n)
     else
       C.StaticInline rtn n args [ C.Return (loc_read r) ]

--
-- Read from the register, or from a shadow copy if it's not readable. 
-- 
register_read_fn :: RT.Rec -> C.Unit
register_read_fn r = 
    let rtn = regtype_c_type $ RT.tpe r
        name = register_read_fn_name r
        args = (register_arg_list [] r [])
    in 
      if RT.is_readable r then
          C.StaticInline rtn name args [ C.Return (loc_read r) ]
      else 
          C.StaticInline rtn name args [ C.Return (register_shadow_ref r) ]

-- 
-- Do a write read top a register, if the address is available.
-- 
register_rawwrite_fn :: RT.Rec -> C.Unit
register_rawwrite_fn r =
    let 
      args = register_arg_list [] r [ C.Param (regtype_c_type $ RT.tpe r) cv_regval ]
      n = register_rawwrite_fn_name r    
    in
     if RT.is_noaddr r then
       C.Comment (printf "%s has no address, user must supply %s" 
                 (RT.name r) n)
     else
       C.StaticInline C.Void n args [ C.Ex $ loc_write r cv_regval ]

--
-- Write to register.  Harder than it sounds. 
-- 
-- To do this properly involves: 

--  1) Take the value to be written
--  2) AND together the MB0 and RSVD fields' insert masks, and AND this with
--     the value.  
--  3) OR together the MB1 fields' select masks, and OR this with the
--     value.  
--  4) OR together the RSVD fields' select masks.  If this is non-zero, 
--     AND this mask with a read from the register, and OR this into the value. 
--  5) Write this to the register, and to the shadow, if present.
-- 
register_write_fn :: RT.Rec -> C.Unit
register_write_fn r = 
    let name = register_write_fn_name r
        args = register_arg_list [] r [ C.Param (regtype_c_type $ RT.tpe r) cv_regval ]
        fields = RT.fl r
        size = RT.size r
        nomask = 0xffffffffffffffff
        mb0mask :: Integer
        mb0mask = foldl (.&.) nomask [ Fields.insert_mask f size | f <- fields,
                                     (attr_zero_before_write $ Fields.attr f) ||
                                     (attr_preserve_on_write $ Fields.attr f) ]
        mb1mask :: Integer
        mb1mask = foldl (.|.) 0 [ Fields.extract_mask f size | f <- fields,
                                  (attr_set_before_write $ Fields.attr f) ]
        prsvmask :: Integer
        prsvmask = foldl (.|.) 0 [ Fields.extract_mask f size | f <- fields,
                                  (attr_preserve_on_write $ Fields.attr f) ]
        body = [ (if mb0mask /= nomask then
                      (C.Ex $ C.Assignment 
                             (C.Variable cv_regval) 
                             (C.Binary C.BitwiseAnd 
                                    (C.Variable cv_regval)
                                    (C.HexConstant mb0mask)))
                  else
                      C.SComment "No MB0 or RSVD fields present"
                 ),
                 (if mb1mask /= 0 then
                      (C.Ex $ C.Assignment 
                             (C.Variable cv_regval) 
                             (C.Binary C.BitwiseOr 
                                    (C.Variable cv_regval)
                                    (C.HexConstant mb1mask)))
                  else
                      C.SComment "No MB1 fields present"
                 ),
                 (if prsvmask /= 0 then
                      (C.Ex $ C.Assignment 
                             (C.Variable cv_regval) 
                             (C.Binary C.BitwiseOr 
                                    (C.Variable cv_regval)
                                    (C.Binary C.BitwiseAnd
                                          (C.HexConstant prsvmask)
                                          (loc_read r))))
                  else
                      C.SComment "No pre-read of register required"
                 ),
                 C.Ex $ loc_write r cv_regval
                 ]
    in 
      if RT.is_writeable r then
          C.StaticInline C.Void name args body
      else
          C.Comment $ printf "Register %s is not writeable" (RT.name r)

--          
-- Get the arguments right for array- and non-array registers
--
register_arg_list :: [C.Param] -> RT.Rec -> [C.Param] -> [C.Param]
register_arg_list pre r post 
    = (pre ++ [ C.Param (C.Ptr device_c_type) cv_dev ] 
       ++ 
       (if RT.is_array r then 
            [ C.Param (C.TypeName "int") cv_i ]
        else [] 
       )
       ++ post)

register_callarg_list :: [C.Param] -> RT.Rec -> [C.Param] -> [C.Param]
register_callarg_list pre r post 
    = (pre ++ [ C.Param (C.Ptr device_c_type) cv_dev ] 
       ++ 
       (if RT.is_array r then 
            [ C.Param (C.TypeName "int") cv_i ]
        else [] 
       )
       ++ post)

--
-- Generate an expression for a read or write of a register,
-- regardless of address space or whether it's an array or not.
-- 
loc_read :: RT.Rec -> C.Expr
loc_read r = 
  case RT.spc r of
      Space.NoSpace -> 
          C.Call (register_rawread_fn_name r)
            [ C.Variable cv_dev ]
      Space.Builtin { Space.n = name } -> 
          C.Call (mackerel_read_fn_name name (RT.size r))
            [ C.DerefField (C.Variable cv_dev) (RT.base r), loc_array_offset r ]
      s@Space.Defined {} -> 
          C.Call (space_read_fn_name s (RT.size r))
                [ C.Variable cv_dev, loc_array_offset r ]

loc_write :: RT.Rec -> String -> C.Expr
loc_write r val = 
    case RT.spc r of
      Space.NoSpace -> 
          C.Call (register_rawwrite_fn_name r)
            [ C.Variable cv_dev, C.Variable val ]
      Space.Builtin { Space.n = name } -> 
          C.Call (mackerel_write_fn_name name (RT.size r))
                [ C.DerefField (C.Variable cv_dev) (RT.base r),
                  loc_array_offset r,
                  C.Variable val ]
      s@Space.Defined {} -> 
          C.Call (space_write_fn_name s (RT.size r)) 
                [ C.Variable cv_dev, loc_array_offset r, C.Variable val ]
    
--
-- Calculate the C expression for an appropriate offset for a register
-- array element, taking into account whether the address space is
-- Bytewise or Valuewise, and whether the array is a list or a step
-- format.
-- 
-- XXX List locations are not well handled right now!
--
loc_array_offset :: RT.Rec -> C.Expr
loc_array_offset r 
    = case (Space.t $ RT.spc r, RT.offset r, RT.arr r, RT.size r) of
        (_, off, ArrayListLoc [], _) ->
            C.HexConstant off
        (Space.VALUEWISE, off, ArrayStepLoc _ 0, _) -> 
            C.Binary C.Plus (C.HexConstant off) (C.Variable cv_i)
        (Space.BYTEWISE s, off, ArrayStepLoc _ 0, sz) ->
            C.Binary C.Plus 
                  (C.HexConstant off)
                  (C.Binary C.Times
                         (C.Variable cv_i) 
                         (C.Binary C.Divide 
                                (C.NumConstant (sz `div` s)) 
                                (C.NumConstant 8)))
        (_, off, ArrayStepLoc _ step, _) ->
            C.Binary C.Plus 
                  (C.HexConstant off)
                  (C.Binary C.Times (C.Variable cv_i) (C.NumConstant step))
        (_, _, ArrayListLoc locations, _) ->
            C.StringConstant $ show locations -- Like here for instance. 

--
-- Emit a function to extract a field from a register type value
--
register_read_field_fn :: RT.Rec -> Fields.Rec -> C.Unit
register_read_field_fn r f = 
    C.StaticInline (field_c_type f) name args body
    where
      args = register_arg_list [] r []
      name = register_read_field_fn_name r f
      extr = regtype_extract_fn_name (RT.tpe r) f
      body = [ register_print_init r,
               C.Return $ C.Call extr [ C.Variable cv_regval ] 
             ] 

--
-- Emit a function to extract a field from a register type value
--
register_read_field_from_shadow_fn :: RT.Rec -> Fields.Rec -> C.Unit
register_read_field_from_shadow_fn r f = 
    C.StaticInline (field_c_type f) name args body
    where
      args = register_arg_list [] r []
      name = register_read_field_from_shadow_fn_name r f
      extr = regtype_extract_fn_name (RT.tpe r) f
      body = [ C.Return $ C.Call extr [ register_shadow_ref r ] ]

--
-- Writing a field of a register is complicated.  We need:
--  0) An initial value consisting of the field value masked/shifted into place
--  1) A mask of all field values to read from the register. 
--  2) A mask of all field values to read from the shadow. 
--  3) A mask of all field values which must be zeroed. 
--  4) A mask of all field values which must be one. 

register_write_field_fn :: RT.Rec -> Fields.Rec -> C.Unit
register_write_field_fn r f = 
    C.StaticInline C.Void name args body
    where
      args = register_arg_list [] r [ C.Param (field_c_type f) cv_fieldval ]
      name = register_write_field_fn_name r f
      fl = delete f $ RT.fl r
      size = RT.size r
      rtn = regtype_c_type $ RT.tpe r
      nomask = 0xffffffffffffffff
      prsvmask :: Integer
      prsvmask = foldl (.|.) 0 [ Fields.extract_mask f' size | f' <- fl,
                                 (attr_can_init_from_reg $ Fields.attr f') ]
      shadmask :: Integer
      shadmask = foldl (.|.) 0 [ Fields.extract_mask f' size | f' <- fl,
                                 (attr_is_writeonly $ Fields.attr f') ]
      mb0mask :: Integer
      mb0mask = foldl (.&.) nomask [ Fields.insert_mask f' size | f' <- fl,
                                     attr_zero_before_write $ Fields.attr f' ]
      mb1mask :: Integer
      mb1mask = foldl (.|.) 0 [ Fields.extract_mask f' size | f' <- fl,
                                attr_set_before_write $ Fields.attr f' ]
      body = [ C.VarDecl C.NoScope C.NonConst (register_c_type r) cv_regval 
                              (Just $ (C.Binary C.BitwiseAnd
                                       (C.HexConstant $ Fields.extract_mask f size)
                                       (C.Binary C.LeftShift
                                        (C.Cast rtn (C.Variable cv_fieldval))
                                        (C.NumConstant $ Fields.offset f)))),
               (if prsvmask /= 0 then
                    (C.Ex $ C.Assignment 
                           (C.Variable cv_regval) 
                           (C.Binary C.BitwiseOr 
                                  (C.Variable cv_regval)
                                  (C.Binary C.BitwiseAnd
                                         (C.HexConstant prsvmask)
                                         (loc_read r))))
                else
                    C.SComment "No pre-read of register required"
               ),
               (if shadmask /= 0 then
                    (C.Ex $ C.Assignment 
                           (C.Variable cv_regval) 
                           (C.Binary C.BitwiseOr 
                                  (C.Variable cv_regval)
                                  (C.Binary C.BitwiseAnd
                                         (C.HexConstant shadmask)
                                         (register_shadow_ref r))))
                else
                    C.SComment "No read of register shadow required"
               ),
               (if mb0mask /= nomask then
                    (C.Ex $ C.Assignment 
                           (C.Variable cv_regval) 
                           (C.Binary C.BitwiseAnd 
                                  (C.Variable cv_regval)
                                  (C.HexConstant mb0mask)))
                else
                    C.SComment "No MB0 fields present"
               ),
               (if mb1mask /= 0 then
                    (C.Ex $ C.Assignment 
                           (C.Variable cv_regval) 
                           (C.Binary C.BitwiseOr 
                                  (C.Variable cv_regval)
                                  (C.HexConstant mb1mask)))
                else
                    C.SComment "No MB1 fields present"
               ),
               C.Ex $ loc_write r cv_regval,
               (if RT.needs_shadow r then
                    C.Ex $ C.Assignment 
                          (register_shadow_ref r) 
                          (C.Variable cv_regval)
                else
                    C.SComment "No shadow register to write to"
               )
             ]

-- Print out a value of the register type
register_print_fn :: RT.Rec -> [ C.Unit ]
register_print_fn r 
    | RT.is_array r = 
        [ register_print_array_element r, register_print_array r ]
    | otherwise = 
        [ register_print_single r ]

register_print_array_element :: RT.Rec -> C.Unit
register_print_array_element r = 
    snprintf_like_defn (regarray_print_fn_name r) args body 
    where
      args = [ C.Param (C.Ptr device_c_type) cv_dev,
               C.Param (C.TypeName "int") cv_i ]
      body = 
          [ register_print_init r,
            snprintf_like_call "snprintf" 
                                   [ C.StringConstant "Register %s[%d] (%s): ",
                                     C.StringConstant $ RT.name r,
                                     C.Variable cv_i,
                                     C.StringConstant $ RT.desc r ]
          ] ++ (register_print_value r)

register_print_array :: RT.Rec -> C.Unit
register_print_array r =
    snprintf_like_defn (register_print_fn_name r) args body
    where
      args = [ C.Param (C.Ptr device_c_type) cv_dev ]
      body = [ simple_var "int" cv_i Nothing,
               simple_for (C.NumConstant $ RT.num_elements r)
                  [ snprintf_like_call (regarray_print_fn_name r) 
                    [ C.Variable cv_dev, C.Variable cv_i ]
                  ]
             ]

register_print_single :: RT.Rec -> C.Unit
register_print_single r = 
    snprintf_like_defn (register_print_fn_name r) args body
    where
      args = [ C.Param (C.Ptr device_c_type) cv_dev ]
      body = [ register_print_init r,
               snputs_like_call $ printf 
                                    "Register %s (%s): " (RT.name r) (RT.desc r)
             ] ++ register_print_value r 

register_print_value :: RT.Rec -> [ C.Stmt ] 
register_print_value r = 
    case RT.tpe r of
      TT.RegFormat {} -> [ snputs_like_call "\n" ] 
                         ++ [ field_print_block (RT.tpe r) f | f <- (RT.fl r) ]
      TT.DataFormat {} -> [ snputs_like_call "\n" ] 
                         ++ [ field_print_block (RT.tpe r) f | f <- (RT.fl r) ]
      TT.Primitive {} -> [ register_print_primitive r ]
      TT.ConstType {} -> [ register_print_consttype r ]

register_print_primitive :: RT.Rec -> C.Stmt 
register_print_primitive r = 
    let extra = 
            if RT.needs_shadow r then " (SHADOW copy)"
            else ""
        fmt = C.StringCat [ C.QStr "\t%", 
                            C.NStr $ field_fmt_str $ RT.size r, 
                            C.QStr (extra ++ "\n") ]
    in snprintf_like_call "snprintf" [ fmt, C.Variable cv_regval ]

register_print_consttype :: RT.Rec -> C.Stmt 
register_print_consttype r = 
    let extra = 
            if RT.needs_shadow r then " (SHADOW copy)"
            else ""
        c = constants_print_fn_name $ TT.tt_name $ RT.tpe r
    in snprintf_like_call c [ C.Variable cv_regval ]

register_print_init :: RT.Rec -> C.Stmt
register_print_init r =
    C.VarDecl C.NoScope C.NonConst (register_c_type r) cv_regval (Just expr)
    where expr = 
              if RT.is_readable r then loc_read r
              else register_shadow_ref r
