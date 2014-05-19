{- 
   Loopback.hs: Flounder stub generator for dummy loopback stubs

  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module Loopback where

import qualified CAbsSyntax as C
import qualified Backend
import Syntax
import BackendCommon hiding (can_send_fn_def, register_send_fn_def)

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

drvname = "loopback"

-- Name of the init function
loopback_init_fn_name n = ifscope n "loopback_init"

-- Name of the transmit vtable
loopback_vtbl_name ifn = ifscope ifn "loopback_tx_vtbl"

-- Name of the transmit function
tx_fn_name ifn mn = idscope ifn mn "loopback_send"

change_waitset_fn_name ifn = ifscope ifn "loopback_change_waitset"

------------------------------------------------------------------------
-- Language mapping: Create the header file for this interconnect driver
------------------------------------------------------------------------

header :: String -> String -> Interface -> String
header infile outfile intf@(Interface name descr decls) = 
    unlines $ C.pp_unit $ header_file intf header_body
    where
        header_file :: Interface -> [C.Unit] -> C.Unit
        header_file interface@(Interface name _ _) body = 
            let sym = "__" ++ name ++ "_LOOPBACK_H"
            in C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []

        header_body = [
            intf_preamble infile name descr,
            C.Blank,
            C.MultiComment [ "Loopback interconnect driver" ],
            C.Blank,
            loopback_init_function_proto name]

loopback_init_function_proto :: String -> C.Unit
loopback_init_function_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope C.Void params) name Nothing
    where
      name = loopback_init_fn_name n
      params = [C.Param (C.Ptr $ C.Struct (intf_bind_type n)) intf_bind_var]

------------------------------------------------------------------------
-- Language mapping: Create the stub (implementation) for this interconnect driver
------------------------------------------------------------------------

stub :: String -> String -> Interface -> String
stub infile outfile intf = 
    unlines $ C.pp_unit $ loopback_stub_body infile intf

loopback_stub_body :: String -> Interface -> C.Unit
loopback_stub_body infile intf@(Interface ifn descr decls) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,
    C.MultiComment [ "Generated Loopback stub" ],
    C.Blank,

    C.Define "_USE_XOPEN" [] "/* for strdup() */",
    C.Include C.Standard "string.h",
    C.Include C.Standard "barrelfish/barrelfish.h",
    C.Include C.Standard "flounder/flounder_support.h",
    C.Include C.Standard ("if/" ++ ifn ++ "_defs.h"),
    C.Include C.Standard ("if/" ++ ifn ++ "_loopback_defs.h"),
    C.Blank,

    C.MultiComment [ "Message sender functions" ],
    C.UnitList [ tx_fn ifn m | m <- messages ],
    C.Blank,

    C.MultiComment [ "Send vtable" ],
    tx_vtbl ifn messages,
    
    C.MultiComment [ "Control functions" ],
    can_send_fn_def ifn,
    register_send_fn_def ifn,
    default_error_handler_fn_def drvname ifn,
    change_waitset_fn_def ifn,
    generic_control_fn_def drvname ifn,

    C.MultiComment [ "Function to initialise the binding state" ],
    loopback_init_fn ifn]
    where
        (types, messagedecls) = Backend.partitionTypesMessages decls
        messages = rpcs_to_msgs messagedecls

loopback_init_fn :: String -> C.Unit
loopback_init_fn ifn
    = C.FunctionDef C.NoScope C.Void (loopback_init_fn_name ifn) params [
        C.StmtList common_init,
        C.Ex $ C.Assignment (common_field "change_waitset")
                                (C.Variable $ change_waitset_fn_name ifn),
        C.Ex $ C.Assignment (common_field "control")
                                (C.Variable $ generic_control_fn_name drvname ifn)
    ]
    where 
      params = [C.Param (C.Ptr $ C.Struct (intf_bind_type ifn)) intf_bind_var]
      common_field f = (C.Variable intf_bind_var) `C.DerefField` f
      common_init = binding_struct_init "loopback" ifn
        (C.DerefPtr $ C.Variable intf_bind_var)
        (C.Variable "NULL")
        (C.Variable $ loopback_vtbl_name ifn)

can_send_fn_def :: String -> C.Unit
can_send_fn_def ifn = 
    C.FunctionDef C.Static (C.TypeName "bool") (can_send_fn_name drvname ifn) params [
        C.Return $ C.Variable "true"]
    where
        params = [ C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) "b" ]

register_send_fn_def :: String -> C.Unit
register_send_fn_def ifn =
    C.FunctionDef C.Static (C.TypeName "errval_t") (register_send_fn_name drvname ifn) params [
        C.Return $ C.Variable "ERR_NOTIMP"
    ]
    where
        params = [ C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) "b",
                   C.Param (C.Ptr $ C.Struct "waitset") "ws",
                   C.Param (C.Struct "event_closure") intf_cont_var ]

change_waitset_fn_def :: String -> C.Unit
change_waitset_fn_def ifn =
    C.FunctionDef C.Static (C.TypeName "errval_t") (change_waitset_fn_name ifn) params [
        C.Return $ C.Variable "ERR_NOTIMP"
    ]
    where
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
                  C.Param (C.Ptr $ C.Struct "waitset") "ws"]

tx_fn :: String -> MessageDef -> C.Unit
tx_fn ifn msg@(Message _ mn args _) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (tx_fn_name ifn mn) params body
    where
        params = [binding_param ifn, cont_param] ++ (
                    concat [ msg_argdecl TX ifn a | a <- args ])
        cont_param = C.Param (C.Struct "event_closure") intf_cont_var
        body = [
            C.StmtList $ if length arrayargs > 0 then
                [C.SComment "copy array arguments",
                 C.StmtList $ concat $ map copyarray arrayargs
                ] else [],
            C.SComment "call rx handler",
            C.Ex $ C.Call "assert" [C.Binary C.NotEquals handler (C.Variable "NULL")],
            C.Ex $ C.CallInd handler ((C.Variable intf_bind_var):(concat $ map mkvars args)),
            C.SBlank,
            C.SComment "run continuation, if any",
            C.If (C.Binary C.NotEquals
                                (C.Variable intf_cont_var `C.FieldOf` "handler")
                                (C.Variable "NULL"))
                [C.Ex $ C.CallInd (C.Variable intf_cont_var `C.FieldOf` "handler")
                                [C.Variable intf_cont_var `C.FieldOf` "arg"]] [],
            C.SBlank,
            C.Return $ C.Variable "SYS_ERR_OK"
            ]

        arrayargs = [a | a@(Arg _ (DynamicArray _ _)) <- args]

        copyarray (Arg tr (DynamicArray n l)) = [
            localvar array_type (array_copy_name n)
                $ Just $ C.Call "malloc" [size],
            C.If (C.Binary C.Equals copyvar (C.Variable "NULL"))
                [C.Return $ C.Variable "LIB_ERR_MALLOC_FAIL"] [],
            C.Ex $ C.Call "memcpy" [copyvar, srcvar, size]
            ] where
                srcvar = C.Variable n
                copyvar = C.Variable $ array_copy_name n
                array_type = C.Ptr $ type_c_type ifn tr
                size = C.Binary C.Times (C.SizeOfT $ type_c_type ifn tr) (C.Variable l)

        -- string and array arguments need special treatment
        mkvars (Arg (Builtin String) (Name n)) = [C.Call "strdup" [C.Variable n]]
        mkvars (Arg _ (DynamicArray n l)) = [C.Variable $ array_copy_name n, C.Variable l]
        mkvars (Arg _ (Name n)) = [C.Variable n]

        array_copy_name n = "_copy_of_" ++ n

        binding = C.Variable intf_bind_var
        handler = C.FieldOf (C.DerefField binding "rx_vtbl") mn

tx_vtbl :: String -> [MessageDef] -> C.Unit
tx_vtbl ifn ml =
    C.StructDef C.Static (intf_vtbl_type ifn TX) (loopback_vtbl_name ifn) fields
    where
        fields = [let mn = msg_name m in (mn, tx_fn_name ifn mn) | m <- ml]
