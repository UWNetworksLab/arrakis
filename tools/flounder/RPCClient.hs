{- 
  RPCClient.hs: Flounder stub generator for RPC client-side stubs

  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module RPCClient where

import qualified CAbsSyntax as C
import qualified Backend
import BackendCommon hiding (errvar)
import GHBackend (msg_signature_generic, intf_vtbl_param)
import Syntax

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

-- Name of the binding struct
rpc_bind_type :: String -> String
rpc_bind_type ifn = ifscope ifn "rpc_client"

-- Name of the binding parameter
rpc_bind_var = "_rpc" :: String

-- Name of the RPC function
rpc_fn_name ifn mn = idscope ifn mn "rpc"

-- Name of the receive handler
rpc_rx_handler_fn_name ifn mn = idscope ifn mn "rpc_rx_handler"

-- Name of the RPC vtable
rpc_vtbl_name ifn = ifscope ifn "rpc_vtbl"

-- Name of the init function
rpc_init_fn_name :: String -> String
rpc_init_fn_name ifn = ifscope ifn "rpc_client_init"

-- Name of the error handler
rpc_error_fn_name :: String -> String
rpc_error_fn_name ifn = ifscope ifn "rpc_client_error"

-- Name of the struct type for the method vtable
rpc_vtbl_type :: String -> String
rpc_vtbl_type ifn = ifscope ifn "rpc_vtbl"

------------------------------------------------------------------------
-- Language mapping: Create the header file for this interconnect driver
------------------------------------------------------------------------

header :: String -> String -> Interface -> String
header infile outfile intf = 
    unlines $ C.pp_unit $ header_file intf (rpc_header_body infile intf)
    where
        header_file :: Interface -> [C.Unit] -> C.Unit
        header_file interface@(Interface name _ _) body = 
            let sym = "__" ++ name ++ "_RPC_CLIENT_H"
            in C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []

rpc_header_body :: String -> Interface -> [C.Unit]
rpc_header_body infile interface@(Interface name descr decls) = [
    intf_preamble infile name descr,
    C.Blank,
    C.MultiComment [ "RPC client" ],
    C.Blank,
    C.Include C.Standard ("if/" ++ name ++ "_defs.h"),
    C.Blank,
    C.MultiComment [ "Forward declaration of binding type" ],
    C.StructForwardDecl (rpc_bind_type name),
    C.Blank,
    C.MultiComment [ "Function signatures" ],
    C.UnitList [ msg_signature_generic TX name types (rpc_binding_param name) m
                | m <- rpcs ],
    C.Blank,
    C.MultiComment [ "VTable struct definition for the interface" ],
    rpc_vtbl_decl name rpcs,
    C.Blank,
    C.MultiComment [ "The Binding structure" ],
    rpc_binding_struct name,
    C.Blank,
    C.MultiComment [ "Function to initialise an RPC client" ],
    rpc_init_fn_proto name,
    C.Blank]
    where
        (types, messagedecls) = Backend.partitionTypesMessages decls
        rpcs = [m | m@(RPC _ _ _) <- messagedecls]

rpc_vtbl_decl :: String -> [MessageDef] -> C.Unit
rpc_vtbl_decl n ml = 
    C.StructDecl (rpc_vtbl_type n) [ intf_vtbl_param n m TX | m <- ml ]

rpc_binding_param :: String -> C.Param
rpc_binding_param ifname = C.Param (C.Ptr $ C.Struct $ rpc_bind_type ifname) rpc_bind_var

rpc_binding_struct :: String -> C.Unit
rpc_binding_struct name = C.StructDecl (rpc_bind_type name) fields
  where
    fields = [
        C.Param (C.Ptr $ C.Struct $ intf_bind_type name) "b",
        C.Param (C.Struct $ rpc_vtbl_type name) "vtbl",
        C.Param (C.TypeName "bool") "rpc_in_progress",
        C.Param (C.TypeName "bool") "reply_present",
        C.Param (C.TypeName "errval_t") "async_error",
        C.Param (C.Struct "waitset") "rpc_waitset",
        C.Param (C.Struct "waitset_chanstate") "dummy_chanstate"]

rpc_init_fn_proto :: String -> C.Unit
rpc_init_fn_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope (C.TypeName "errval_t") (rpc_init_fn_params n)) name Nothing
    where 
      name = rpc_init_fn_name n

------------------------------------------------------------------------
-- Language mapping: Create the stub (implementation) for this interconnect driver
------------------------------------------------------------------------

stub :: String -> String -> Interface -> String
stub infile outfile intf = unlines $ C.pp_unit $ rpc_stub_body infile intf

rpc_stub_body :: String -> Interface -> C.Unit
rpc_stub_body infile intf@(Interface ifn descr decls) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,
    C.MultiComment [ "Generated Stub for RPC" ],
    C.Blank,

    C.Include C.Standard "barrelfish/barrelfish.h",
    C.Include C.Standard "flounder/flounder_support.h",
    C.Include C.Standard ("if/" ++ ifn ++ "_rpcclient_defs.h"),
    C.Blank,

    C.MultiComment [ "RPC wrapper functions" ],
    C.UnitList [ rpc_fn ifn types m | m <- rpcs ],
    C.Blank,

    C.MultiComment [ "Receive handlers" ],
    C.UnitList [ rpc_rx_handler_fn ifn types m | m <- rpcs ],
    C.Blank,

    C.MultiComment [ "RPC Vtable" ],
    rpc_vtbl ifn rpcs,
    C.Blank,

    C.MultiComment [ "Error handler" ],
    rpc_error_fn ifn,
    C.Blank,

    C.MultiComment [ "Init function" ],
    rpc_init_fn ifn rpcs]
    where
        (types, messagedecls) = Backend.partitionTypesMessages decls
        rpcs = [m | m@(RPC _ _ _) <- messagedecls]

rpc_fn :: String -> [TypeDef] -> MessageDef -> C.Unit
rpc_fn ifn typedefs msg@(RPC n args _) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (rpc_fn_name ifn n) params [
        localvar (C.TypeName "errval_t") errvar_name (Just $ C.Variable "SYS_ERR_OK"),
        C.Ex $ C.Call "assert" [C.Unary C.Not rpc_progress_var],
        C.Ex $ C.Call "assert" [C.Binary C.Equals async_err_var (C.Variable "SYS_ERR_OK")],
        C.Ex $ C.Assignment rpc_progress_var (C.Variable "true"),
        C.Ex $ C.Assignment reply_present_var (C.Variable "false"),
        C.SBlank,
        C.SComment "call send function",
        C.Ex $ C.Assignment errvar $ C.CallInd tx_func tx_func_args,
        C.If (C.Call "err_is_fail" [errvar]) [C.Goto "out"] [],
        C.SBlank,
        C.SComment "wait for message to be sent and reply or error to be present",
        C.While (C.Binary C.And
                (C.Binary C.Or (C.Unary C.Not reply_present_var)
                    (C.Unary C.Not $ C.CallInd (bindvar `C.DerefField` "can_send") [bindvar]))
                (C.Binary C.Equals async_err_var (C.Variable "SYS_ERR_OK"))) [
            C.Ex $ C.Assignment errvar $ C.Call "event_dispatch" [waitset_var],
            C.If (C.Call "err_is_fail" [errvar])
                [C.Ex $ C.Assignment errvar $ C.Call "err_push"
                        [errvar, C.Variable "LIB_ERR_EVENT_DISPATCH"],
                 C.Goto "out"] []
            ],
        C.SBlank,
        C.If (C.Call "err_is_fail" [async_err_var])
            [C.Ex $ C.Assignment errvar async_err_var,
             C.Ex $ C.Assignment async_err_var (C.Variable "SYS_ERR_OK"),
             C.Goto "out"] [],
        C.SBlank,
        C.StmtList $ if length rxargs > 0 then [
            C.SComment "grab reply variables out of binding",
            localvar (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var
                $ Just $ C.DerefField rpcvar "b",
            C.StmtList [C.Ex $ C.Assignment (C.DerefPtr $ C.Variable an) (rpc_rx_union_elem n an)
                        | an <- concat $ map arg_names rxargs],
            C.SBlank]
            else [],
        C.Label "out",
        C.Ex $ C.Assignment rpc_progress_var (C.Variable "false"),
        C.Return errvar
    ]
    where
        params = [rpc_binding_param ifn]
                 ++ concat [rpc_argdecl2 ifn typedefs a | a <- args]
        rpcvar = C.Variable rpc_bind_var
        reply_present_var = C.DerefField rpcvar "reply_present"
        rpc_progress_var = C.DerefField rpcvar "rpc_in_progress"
        async_err_var = C.DerefField rpcvar "async_error"
        waitset_var = C.AddressOf $ C.DerefField rpcvar "rpc_waitset"
        bindvar = C.DerefField rpcvar "b"
        tx_func = C.DerefField bindvar "tx_vtbl" `C.FieldOf` (rpc_call_name n)
        tx_func_args = [bindvar, C.Variable "NOP_CONT"]
            ++ (map C.Variable $ concat $ map mkargs txargs)
        mkargs (Arg _ (Name an)) = [an]
        mkargs (Arg _ (DynamicArray an al)) = [an, al]
        (txargs, rxargs) = partition_rpc_args args

rpc_vtbl :: String -> [MessageDef] -> C.Unit
rpc_vtbl ifn ml =
    C.StructDef C.Static (rpc_vtbl_type ifn) (rpc_vtbl_name ifn) fields
    where
        fields = [let mn = msg_name m in (mn, rpc_fn_name ifn mn) | m <- ml]

rpc_rx_handler_fn :: String -> [TypeDef] -> MessageDef -> C.Unit
rpc_rx_handler_fn ifn typedefs msg@(RPC mn args _) =
    C.FunctionDef C.Static C.Void (rpc_rx_handler_fn_name ifn mn) params [
        C.SComment "get RPC client state pointer",
        localvar (C.Ptr $ C.Struct $ rpc_bind_type ifn) rpc_bind_var $
            Just $ C.DerefField bindvar "st",
        C.SBlank,
        C.SComment "XXX: stash reply parameters in binding object",
        C.SComment "depending on the interconnect driver, they're probably already there",
        C.StmtList [rx_arg_assignment ifn typedefs mn a | a <- rxargs ],
        C.SBlank,
        C.SComment "notify RPC function, and we're done",
        C.Ex $ C.Assignment
                (C.Variable rpc_bind_var `C.DerefField` "reply_present")
                (C.Variable "true")
    ]
    where
        params = [binding_param ifn] ++ concat [msg_argdecl RX ifn a | a <- rxargs]
        bindvar = C.Variable intf_bind_var
        (_, rxargs) = partition_rpc_args args

-- XXX: this mirrors BackendCommon.tx_arg_assignment
rx_arg_assignment :: String -> [TypeDef] -> String -> MessageArgument -> C.Stmt
rx_arg_assignment ifn typedefs mn (Arg tr v) = case v of
    Name an -> C.Ex $ C.Assignment (rpc_rx_union_elem mn an) (srcarg an)
    DynamicArray an len -> C.StmtList [
        C.Ex $ C.Assignment (rpc_rx_union_elem mn an) (C.Variable an),
        C.Ex $ C.Assignment (rpc_rx_union_elem mn len) (C.Variable len)]
    where
        typespec = type_c_type ifn tr
        srcarg an = 
          case lookup_typeref typedefs tr of
            -- XXX: I have no idea why GCC requires a cast for the array type
            TArray _ _ _ -> C.Cast (C.Ptr typespec) (C.Variable an)
            _ -> C.Variable an

arg_names :: MessageArgument -> [String]
arg_names (Arg _ v) = var_names v
    where
        var_names (Name n) = [n]
        var_names (DynamicArray n1 n2) = [n1, n2]

rpc_error_fn :: String -> C.Unit
rpc_error_fn ifn = C.FunctionDef C.Static C.Void (rpc_error_fn_name ifn)
    [binding_param ifn, C.Param (C.TypeName "errval_t") errvar_name]
    [C.SComment "get RPC client state pointer",
     localvar (C.Ptr $ C.Struct $ rpc_bind_type ifn) rpc_bind_var $
        Just $ C.DerefField bindvar "st",
     C.SBlank,
     C.If (rpcvar `C.DerefField` "rpc_in_progress")
        [C.Ex $ C.Call "assert" [C.Call "err_is_fail" [errvar]],
         C.Ex $ C.Assignment (C.DerefField rpcvar "async_error") errvar,
         C.SComment "kick waitset with dummy event",
         C.Ex $ C.Call "flounder_support_register"
                    [waitset_addr, chanstate_addr,
                     C.Variable "dummy_event_closure", C.Variable "true"]]
        [C.Ex $ C.Call "USER_PANIC_ERR" [errvar, C.StringConstant "async error in RPC"]]
    ]
    where
        rpcvar = C.Variable rpc_bind_var
        waitset_addr = C.AddressOf $ C.DerefField rpcvar "rpc_waitset"
        chanstate_addr = C.AddressOf $ C.DerefField rpcvar "dummy_chanstate"

rpc_init_fn :: String -> [MessageDef] -> C.Unit
rpc_init_fn ifn ml = C.FunctionDef C.NoScope (C.TypeName "errval_t")
                            (rpc_init_fn_name ifn) (rpc_init_fn_params ifn) $
    [localvar (C.TypeName "errval_t") errvar_name Nothing,
     C.SBlank,
     C.SComment "Setup state of RPC client object",
     C.Ex $ C.Assignment (C.DerefField rpcvar "b") bindvar,
     C.Ex $ C.Assignment (C.DerefField rpcvar "reply_present") (C.Variable "false"),
     C.Ex $ C.Assignment (C.DerefField rpcvar "rpc_in_progress") (C.Variable "false"),
     C.Ex $ C.Assignment (C.DerefField rpcvar "async_error") (C.Variable "SYS_ERR_OK"),
     C.Ex $ C.Call "waitset_init" [waitset_addr],
     C.Ex $ C.Call "flounder_support_waitset_chanstate_init"
                        [C.AddressOf $ C.DerefField rpcvar "dummy_chanstate"],
     C.Ex $ C.Assignment (C.DerefField rpcvar "vtbl") (C.Variable $ rpc_vtbl_name ifn),
     C.Ex $ C.Assignment (C.DerefField bindvar "st") rpcvar,
     C.SBlank,
     C.SComment "Change waitset on binding",
     C.Ex $ C.Assignment errvar $
        C.CallInd (C.DerefField bindvar "change_waitset")
                [bindvar, waitset_addr],
     C.If (C.Call "err_is_fail" [errvar])
        [C.Ex $ C.Call "waitset_destroy" [waitset_addr],
         C.Return $ C.Call "err_push" [errvar, C.Variable "FLOUNDER_ERR_CHANGE_WAITSET"]]
        [],
     C.SBlank,
     C.SComment "Set RX handlers on binding object for RPCs",
     C.StmtList [C.Ex $ C.Assignment (C.FieldOf (C.DerefField bindvar "rx_vtbl")
                                        (rpc_resp_name mn))
         (C.Variable $ rpc_rx_handler_fn_name ifn mn) | RPC mn _ _ <- ml],
     C.SBlank,
     C.SComment "Set error handler on binding object",
     C.Ex $ C.Assignment (bindvar `C.DerefField` "error_handler")
                          (C.Variable $ rpc_error_fn_name ifn),
     C.SBlank,
     C.Return $ C.Variable "SYS_ERR_OK"]
    where
        rpcvar = C.Variable "rpc"
        bindvar = C.Variable "binding"
        waitset_addr = C.AddressOf $ C.DerefField rpcvar "rpc_waitset"

rpc_init_fn_params n = [C.Param (C.Ptr $ C.Struct (rpc_bind_type n)) "rpc",
                        C.Param (C.Ptr $ C.Struct (intf_bind_type n)) "binding"]

rpc_rx_union_elem :: String -> String -> C.Expr
rpc_rx_union_elem mn fn =
   C.FieldOf (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "rx_union")
                    (rpc_resp_name mn)) fn

errvar_name = "_err"
errvar = C.Variable errvar_name
