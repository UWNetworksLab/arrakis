{- 
  THCBackend: generate interface to Flounder THC stubs
   
  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module THCBackend where

import Data.List

import qualified CAbsSyntax as C
import qualified BackendCommon as BC
import Syntax
import Backend

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

-- Name of the struct holding message args for SAR
msg_argstruct_name :: String -> String -> String
msg_argstruct_name ifn n = idscope ifn n "args_t"

rpc_argstruct_name :: String -> String -> String -> String
rpc_argstruct_name ifn n inout = idscope ifn n (inout ++ "_args_t")

rpc_union_name :: String -> String -> String 
rpc_union_name ifn n = idscope ifn n "_union_t"

-- Name of the enumeration of message numbers
msg_enum_name :: String -> String
msg_enum_name ifn = ifscope ifn "msg_enum_t"

call_msg_enum_name :: String -> String
call_msg_enum_name ifn = ifscope ifn "call_msg_enum_t"

resp_msg_enum_name :: String -> String
resp_msg_enum_name ifn = ifscope ifn "resp_msg_enum_t"

-- Name of each element of the message number enumeration
msg_enum_elem_name :: String -> String -> String
msg_enum_elem_name ifn mn = ifscope ifn mn

call_msg_enum_elem_name :: String -> String -> String
call_msg_enum_elem_name ifn mn = ifscope ifn ("_call_" ++ mn)

resp_msg_enum_elem_name :: String -> String -> String
resp_msg_enum_elem_name ifn mn = ifscope ifn ("_resp_" ++ mn)

-- Name of the union type holding all the arguments for a message
binding_arg_union_type :: String -> String
binding_arg_union_type ifn = ifscope ifn "thc_arg_union"

-- Scope a list of strings
ifscope :: String -> String -> String
ifscope ifn s = ifn ++ "_" ++ s

idscope :: String -> String -> String -> String
idscope ifn s suffix  = ifscope ifn (s ++ "__" ++ suffix)

-- Name of the binding struct for an interface type
intf_bind_type :: String -> String -> String
intf_bind_type ifn sender = ifscope ifn $ "thc_" ++ sender ++ "_binding_t"

-- Variable used to refer to a binding
intf_bind_var = "_thc_binding"

-- Name of the type of a message function
msg_sig_type :: String -> String -> String -> MessageDef -> String
msg_sig_type ifn sender sendrecv m = idscope ifn (BC.msg_name m) "thc_" ++ sender ++ "_" ++ sendrecv ++ "_t"
msg_sig_type_x :: String -> String -> String -> MessageDef -> String
msg_sig_type_x ifn sender sendrecv m = idscope ifn (BC.msg_name m) "thc_" ++ sender ++ "_" ++ sendrecv ++ "_t_x"

-- Name of the type of an RPC call function
call_sig_type :: String -> MessageDef -> String
call_sig_type ifn m@(RPC n _ _) = idscope ifn n "thc_call__t"

call_sig_type_x :: String -> MessageDef -> String
call_sig_type_x ifn m@(RPC n _ _) = idscope ifn n "thc_call__t_x"

call_ooo_sig_type :: String -> MessageDef -> String
call_ooo_sig_type ifn m@(RPC n _ _) = idscope ifn n "thc_ooo_call__t"

call_ooo_sig_type_x :: String -> MessageDef -> String
call_ooo_sig_type_x ifn m@(RPC n _ _) = idscope ifn n "thc_ooo_call__t_x"

-- Name of the type of a receive-any function
rx_any_sig_type :: String -> String -> String
rx_any_sig_type ifn receiver = idscope ifn "recv_any" "thc_" ++ receiver ++ "_t"

rx_any_sig_type_x :: String -> String -> String
rx_any_sig_type_x ifn receiver = idscope ifn "recv_any" "thc_" ++ receiver ++ "_t_x"

-- Name of the structure in which receive-any supplies a message
rx_any_struct_name :: String -> String -> String
rx_any_struct_name ifn receiver = ifscope ifn (receiver ++ "_msg")

rx_any_type_name :: String -> String -> String
rx_any_type_name ifn receiver = ifscope ifn (receiver ++ "_msg_t")

-- Name of the struct type for the method vtable
intf_vtbl_type :: String -> String -> String -> String
intf_vtbl_type ifn sender sendrecv = ifscope ifn $ "thc_" ++ sender ++ "_" ++ sendrecv 

intf_vtbl_type_x :: String -> String -> String -> String
intf_vtbl_type_x ifn sender sendrecv = ifscope ifn $ "thc_" ++ sender ++ "_" ++ sendrecv ++ "_x"

intf_selector_type :: String -> String -> String
intf_selector_type ifn sender = ifscope ifn (sender ++ "_selector")

-- Name of the struct types for the RPC call vtables
rpc_seq_vtbl_type :: String -> String
rpc_seq_vtbl_type ifn = ifscope ifn $ "thc_rpc_seq"

rpc_fifo_vtbl_type :: String -> String
rpc_fifo_vtbl_type ifn = ifscope ifn $ "thc_rpc_fifo"

rpc_ooo_vtbl_type :: String -> String
rpc_ooo_vtbl_type ifn = ifscope ifn $ "thc_rpc_ooo"

rpc_seq_vtbl_type_x :: String -> String
rpc_seq_vtbl_type_x ifn = ifscope ifn $ "thc_rpc_seq_x"

rpc_fifo_vtbl_type_x :: String -> String
rpc_fifo_vtbl_type_x ifn = ifscope ifn $ "thc_rpc_fifo_x"

rpc_ooo_vtbl_type_x :: String -> String
rpc_ooo_vtbl_type_x ifn = ifscope ifn $ "thc_rpc_ooo_x"

-- Name of a function to initialize a client/server THC binding
init_client_name :: String -> String
init_client_name ifn = ifscope ifn $ "thc_init_client"

init_service_name :: String -> String
init_service_name ifn = ifscope ifn $ "thc_init_service"

-- Type and name for the sequencer used for OOO RPC IDs
ooo_rpc_seq_type = "thc_seq_t"
ooo_rpc_seq_name = "ooo_rpc_seq"

-- Type and name for the generic per-binding state
thc_per_binding_state_struct = "thc_per_binding_state_t"
thc_per_binding_state_name = "thc_per_binding"

thc_binding_lock_name = "thc_binding_lock"

-- Type and name for the generic per-receivable-message state
thc_per_recv_state_struct = "thc_per_recv_t"
thc_per_recv_state_name = "thc_per_recv"

-- Type for the export_info / connect_info struct
thc_export_info_struct_name ifn = ifscope ifn "thc_export_info"
thc_export_info_t ifn = C.Struct $ thc_export_info_struct_name ifn
thc_connect_info_struct_name ifn = ifscope ifn "thc_connect_info"
thc_connect_info_t ifn = C.Struct $ thc_connect_info_struct_name ifn

-- Names for the THC export/accept/connect functions
thc_export_fn_name ifn = ifscope ifn "thc_export"
thc_accept_fn_name ifn = ifscope ifn "thc_accept"
thc_connect_fn_name ifn = ifscope ifn "thc_connect"
thc_connect_by_name_fn_name ifn = ifscope ifn "thc_connect_by_name"

------------------------------------------------------------------------
-- Language mapping: Create the THC header file for the interface
------------------------------------------------------------------------

compile :: String -> String -> Interface -> String
compile infile outfile interface =
    unlines $ C.pp_unit $ intf_thc_header_file infile interface

intf_thc_header_file :: String -> Interface -> C.Unit
intf_thc_header_file infile interface@(Interface name _ _) = 
    let sym = "__" ++ name ++ "_THC_IF_H"
    in
      C.IfNDef sym ([ C.Define sym [] "1"] ++ (intf_thc_header_body infile interface)) []

intf_thc_header_body :: String -> Interface -> [C.Unit]
intf_thc_header_body infile interface@(Interface name descr decls) = 
    let (types, messages) = partitionTypesMessages decls
    in [ BC.intf_preamble infile name descr,

         C.IfDef "BARRELFISH" [ C.Include C.Local "thc/thcstubs.h",
                                C.Include C.Local ("if/" ++ name ++ "_defs.h") ]
                              [ C.Include C.Local "thcstubs.h",
                                C.Include C.Local (name ++ ".h") ],
         C.Blank,
         C.MultiComment [ "Typedefs for binding structures" ],
         C.Blank,
         C.TypeDef (C.Struct $ intf_bind_type name "client") (intf_bind_type name "client"),
         C.TypeDef (C.Struct $ intf_bind_type name "service") (intf_bind_type name "service"),
         
         C.Blank,
         C.MultiComment [ "Struct type for holding the args for each msg" ],
         C.UnitList [ msg_argstruct name m | m <- messages ],
         C.Blank,

         C.MultiComment [ "Union type for all message arguments" ],
         intf_union name messages,
         C.Blank,

         C.MultiComment [ "Enumerations for message numbers" ],
         msg_enums msg_enum_name msg_enum_elem_name name messages,
         msg_enums call_msg_enum_name call_msg_enum_elem_name name [ m | m <- messages, isForward m ],
         msg_enums resp_msg_enum_name resp_msg_enum_elem_name name [ m | m <- messages, isBackward m ],
         C.Blank,

         C.Blank,
         C.MultiComment [ "Signatures for individual send/receive operations" ],
         C.Blank,
         C.UnitList [ send_signature ClientSide "client" name m | m <- messages, isForward m ],
         C.UnitList [ send_signature_x ClientSide "client" name m | m <- messages, isForward m ],
         C.UnitList [ receive_signature ServerSide "service" name m | m <- messages, isForward m ],
         C.UnitList [ receive_signature_x ServerSide "service" name m | m <- messages, isForward m ],
         C.UnitList [ send_signature ServerSide "service" name m | m <- messages, isBackward m ],
         C.UnitList [ send_signature_x ServerSide "service" name m | m <- messages, isBackward m ],
         C.UnitList [ receive_signature ClientSide "client" name m | m <- messages, isBackward m ],
         C.UnitList [ receive_signature_x ClientSide "client" name m | m <- messages, isBackward m ],
         C.UnitList [ call_signature name m | m <- messages, isRPC m ],
         C.UnitList [ call_signature_x name m | m <- messages, isRPC m ],
         C.UnitList [ call_ooo_signature name m | m <- messages, isOOORPC m ],
         C.UnitList [ call_ooo_signature_x name m | m <- messages, isOOORPC m ],

         C.Blank,
         C.MultiComment [ "VTables of send/receive operations" ],
         C.Blank,
         C.StructDecl (intf_vtbl_type name "client" "send_vtbl") [ intf_vtbl_op name "client" "send" m | m <- messages, isForward m] ,
         C.StructDecl (intf_vtbl_type_x name "client" "send_vtbl") [ intf_vtbl_op_x name "client" "send" m | m <- messages, isForward m] ,
         C.StructDecl (intf_vtbl_type name "service" "receive_vtbl") [ intf_vtbl_op name "service" "recv" m | m <- messages, isForward m] ,
         C.StructDecl (intf_vtbl_type_x name "service" "receive_vtbl") [ intf_vtbl_op_x name "service" "recv" m | m <- messages, isForward m] ,
         C.StructDecl (intf_selector_type name "service") [ intf_selector_op name "service" "receive_handler" m | m <- messages, isForward m] ,
         C.StructDecl (intf_vtbl_type name "service" "send_vtbl") [ intf_vtbl_op name "service" "send" m | m <- messages, isBackward m] ,
         C.StructDecl (intf_vtbl_type_x name "service" "send_vtbl") [ intf_vtbl_op_x name "service" "send" m | m <- messages, isBackward m] ,
         C.StructDecl (intf_vtbl_type name "client" "receive_vtbl") [ intf_vtbl_op name "client" "recv" m | m <- messages, isBackward m] ,
         C.StructDecl (intf_vtbl_type_x name "client" "receive_vtbl") [ intf_vtbl_op_x name "client" "recv" m | m <- messages, isBackward m] ,
         C.StructDecl (intf_selector_type name "client") [ intf_selector_op name "client" "receive_handler" m | m <- messages, isBackward m] ,

         C.Blank,
         C.MultiComment [ "VTables of RPC operations" ],
         C.Blank,
         C.StructDecl (rpc_seq_vtbl_type name) [ rpc_seq_vtbl_op name m | m <- messages, isRPC m ],
         C.StructDecl (rpc_seq_vtbl_type_x name) [ rpc_seq_vtbl_op_x name m | m <- messages, isRPC m ],
         C.StructDecl (rpc_fifo_vtbl_type name) [ rpc_fifo_vtbl_op name m | m <- messages, isRPC m ],
         C.StructDecl (rpc_fifo_vtbl_type_x name) [ rpc_fifo_vtbl_op_x name m | m <- messages, isRPC m ],
         C.StructDecl (rpc_ooo_vtbl_type name) [ rpc_ooo_vtbl_op name m | m <- messages, isOOORPC m ],
         C.StructDecl (rpc_ooo_vtbl_type_x name) [ rpc_ooo_vtbl_op_x name m | m <- messages, isOOORPC m ],

         C.Blank,
         C.MultiComment [ "Types for recv_any operations" ],
         C.Blank,
         C.UnitList [ receive_any_types name "client" ],
         C.UnitList [ receive_any_types name "service" ],

         C.Blank,
         C.MultiComment [ "Binding structures" ],
         C.Blank,
         C.UnitList [ binding_struct ClientSide name [ m | m <- messages, isBackward m ] ],
         C.UnitList [ binding_struct ServerSide name [ m | m <- messages, isForward m ] ],

         C.Blank,
         C.MultiComment [ "Initialize a THC binding over an IDC binding",
                          "(defined in THC-stubs backend)" ],
         C.Blank,
         C.GVarDecl C.Extern C.NonConst 
            (C.Function C.NoScope (C.TypeName "errval_t") [
                C.Param (C.Ptr $ C.TypeName $ intf_bind_type name "client") "thc",
                C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type name) "idc_c2s",
                C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type name) "idc_s2c"
            ]) (init_client_name name) Nothing,
         C.GVarDecl C.Extern C.NonConst 
            (C.Function C.NoScope (C.TypeName "errval_t") [
                C.Param (C.Ptr $ C.TypeName $ intf_bind_type name "service") "thc",
                C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type name) "idc_c2s",
                C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type name) "idc_s2c"
            ]) (init_service_name name) Nothing,

         C.Blank,
         C.MultiComment [ "THC helper functions for establishing connections" ],
         C.StructDecl (thc_export_info_struct_name name) [
            C.Param (C.TypeName "thc_sem_t") "export_cb_done_sem",
            C.Param (C.TypeName "thc_sem_t") "connect_cb_done_sem",
            C.Param (C.TypeName "thc_sem_t") "accept_call_present_sem",
            C.Param (C.TypeName "thc_lock_t") "next_accept_lock",
            C.Param (C.Ptr $ C.Ptr $ C.Struct $ BC.intf_bind_type name) "b",
            C.Param (C.TypeName "thc_lock_t") "info_lock",
            C.Param (C.TypeName "errval_t") "err",
            C.Param (C.ConstT $ C.Ptr $ C.TypeName "char") "service_name",
            C.Param (C.TypeName "iref_t") "iref",
            C.Param (C.Ptr $ C.TypeName "iref_t") "iref_ptr" 
         ],
         C.StructDecl (thc_connect_info_struct_name name) [
            C.Param (C.TypeName "thc_sem_t") "bind_cb_done_sem",
            C.Param (C.TypeName "errval_t") "err",
            C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type name) "b"
         ],
         C.GVarDecl C.Extern C.NonConst
            (C.Function C.NoScope (C.TypeName "errval_t") [
                C.Param (C.Ptr $ thc_export_info_t name) "info",
                C.Param (C.ConstT $ C.Ptr $ C.TypeName "char") "service_name",
                C.Param (C.Ptr $ C.Struct "waitset") "ws",
                C.Param (C.TypeName "idc_export_flags_t") "flags",
                C.Param (C.Ptr $ C.TypeName "iref_t") "iref"
            ]) (thc_export_fn_name name) Nothing,
         C.GVarDecl C.Extern C.NonConst
            (C.Function C.NoScope (C.TypeName "errval_t") [
                C.Param (C.Ptr $ thc_export_info_t name) "info",
                C.Param (C.Ptr $ C.Ptr $ C.Struct $ BC.intf_bind_type name) "b"
            ]) (thc_accept_fn_name name) Nothing,
         C.GVarDecl C.Extern C.NonConst
            (C.Function C.NoScope (C.TypeName "errval_t") [
                C.Param (C.TypeName "iref_t") "iref",
                C.Param (C.Ptr $ C.Struct "waitset") "ws",
                C.Param (C.TypeName "idc_bind_flags_t") "flags",
                C.Param (C.Ptr $ C.Ptr $ C.Struct $ BC.intf_bind_type name) "b"
            ]) (thc_connect_fn_name name) Nothing,
         C.GVarDecl C.Extern C.NonConst
            (C.Function C.NoScope (C.TypeName "errval_t") [
                C.Param (C.ConstT $ C.Ptr $ C.TypeName "char") "service_name",
                C.Param (C.Ptr $ C.Struct "waitset") "ws",
                C.Param (C.TypeName "idc_bind_flags_t") "flags",
                C.Param (C.Ptr $ C.Ptr $ C.Struct $ BC.intf_bind_type name) "b"
            ]) (thc_connect_by_name_fn_name name) Nothing

         ]

isRPC :: MessageDef -> Bool
isRPC (RPC _ _ _) = True
isRPC _ = False

isOOORPC :: MessageDef -> Bool
isOOORPC (RPC _ ((RPCArgIn (Builtin UInt64) (Name "seq_in")):(RPCArgOut (Builtin UInt64) (Name "seq_out")):_) _) = True
isOOORPC _ = False

binding_struct :: Side -> String -> [MessageDef] -> C.Unit
binding_struct side ifn messages =
   let end = (case side of 
                ClientSide -> "client" 
                ServerSide -> "service")
       nmessages = length messages
       rpcparams ServerSide = []
       rpcparams ClientSide = [C.Param (C.Struct $ rpc_seq_vtbl_type ifn) "call_seq",
                               C.Param (C.Struct $ rpc_fifo_vtbl_type ifn) "call_fifo",
                               C.Param (C.Struct $ rpc_ooo_vtbl_type ifn) "call",
                               C.Param (C.Struct $ rpc_seq_vtbl_type_x ifn) "call_seq_x",
                               C.Param (C.Struct $ rpc_fifo_vtbl_type_x ifn) "call_fifo_x",
                               C.Param (C.Struct $ rpc_ooo_vtbl_type_x ifn) "call_x",
                               C.Param (C.TypeName $ ooo_rpc_seq_type) ooo_rpc_seq_name
                                ]
   in
    C.StructDecl (intf_bind_type ifn end) $ concat [
        [C.Param (C.Struct $ thc_per_binding_state_struct) thc_per_binding_state_name],
        [C.Param (C.Struct $ intf_vtbl_type ifn end "send_vtbl") "send"],
        [C.Param (C.Struct $ intf_vtbl_type_x ifn end "send_vtbl") "send_x"],
        [C.Param (C.Struct $ intf_vtbl_type ifn end "receive_vtbl") "recv"],
        [C.Param (C.Struct $ intf_vtbl_type_x ifn end "receive_vtbl") "recv_x"],
        [C.Param (C.TypeName $ rx_any_sig_type ifn end) "recv_any"],
        [C.Param (C.TypeName $ rx_any_sig_type_x ifn end) "recv_any_x"],
        rpcparams side,
        [C.Param (C.Array (fromIntegral nmessages) $ C.Struct $ thc_per_recv_state_struct) thc_per_recv_state_name],
        [C.Param (C.Ptr $ C.TypeName "void") "_c2s_st"],
        [C.Param (C.Ptr $ C.TypeName "void") "_s2c_st"]
    ]

receive_any_types :: String -> String -> C.Unit
receive_any_types ifn receiver =
   let
    params = [ C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifn receiver) intf_bind_var,
               C.Param (C.Ptr $ C.Struct $ rx_any_struct_name ifn receiver) "msg",
               C.Param (C.Struct $ intf_selector_type ifn receiver) "ops" ]
    rx_t_name = rx_any_sig_type ifn receiver 
    rx_t_name_x = rx_any_sig_type_x ifn receiver 
   in
    C.UnitList [
      C.StructDecl (rx_any_struct_name ifn receiver) 
        [ C.Param (C.Enum $ msg_enum_name ifn) "msg",
          C.Param (C.Union $ binding_arg_union_type ifn ) "args"
          ],
      C.TypeDef (C.Struct $ rx_any_struct_name ifn receiver) (rx_any_type_name ifn receiver),
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) ("(*" ++ rx_t_name ++ ")"),
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) ("(*" ++ rx_t_name_x ++ ")")
     ]

rpc_seq_vtbl_op :: String -> MessageDef -> C.Param
rpc_seq_vtbl_op ifn m@(RPC n _ _) =
    C.Param (C.TypeName $ call_sig_type ifn m) n

rpc_fifo_vtbl_op :: String -> MessageDef -> C.Param
rpc_fifo_vtbl_op ifn m@(RPC n _ _) =
    C.Param (C.TypeName $ call_sig_type ifn m) n

rpc_ooo_vtbl_op :: String -> MessageDef -> C.Param
rpc_ooo_vtbl_op ifn m@(RPC n _ _) =
    C.Param (C.TypeName $ call_ooo_sig_type ifn m) n

rpc_seq_vtbl_op_x :: String -> MessageDef -> C.Param
rpc_seq_vtbl_op_x ifn m@(RPC n _ _) =
    C.Param (C.TypeName $ call_sig_type_x ifn m) n

rpc_fifo_vtbl_op_x :: String -> MessageDef -> C.Param
rpc_fifo_vtbl_op_x ifn m@(RPC n _ _) =
    C.Param (C.TypeName $ call_sig_type_x ifn m) n

rpc_ooo_vtbl_op_x :: String -> MessageDef -> C.Param
rpc_ooo_vtbl_op_x ifn m@(RPC n _ _) =
    C.Param (C.TypeName $ call_ooo_sig_type_x ifn m) n

intf_vtbl_op :: String -> String -> String -> MessageDef -> C.Param
intf_vtbl_op ifn sender sendrecv m =
    C.Param (C.TypeName $ msg_sig_type ifn sender sendrecv m) (BC.msg_name m)

intf_vtbl_op_x :: String -> String -> String -> MessageDef -> C.Param
intf_vtbl_op_x ifn sender sendrecv m =
    C.Param (C.TypeName $ msg_sig_type_x ifn sender sendrecv m) (BC.msg_name m)

intf_selector_op :: String -> String -> String -> MessageDef -> C.Param
intf_selector_op ifn sender sendrecv m =
    C.Param (C.TypeName "int") (BC.msg_name m)

-- Should do this properly rather than string munging.
fnptr :: String -> String
fnptr s = "(*" ++ s ++ ")"

call_signature :: String -> MessageDef -> C.Unit
call_signature ifname m@(RPC s args _) =
    let name = call_sig_type ifname m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname "client") 
                  intf_bind_var 
        params = [ binding ] ++ concat [ rpc_call_argdecl ifname a | a <- args ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

call_signature_x :: String -> MessageDef -> C.Unit
call_signature_x ifname m@(RPC s args _) =
    let name = call_sig_type_x ifname m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname "client") 
                  intf_bind_var 
        params = [ binding ] ++ concat [ rpc_call_argdecl ifname a | a <- args ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

call_ooo_signature :: String -> MessageDef -> C.Unit
call_ooo_signature ifname m@(RPC s args _) =
    let name = call_ooo_sig_type ifname m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname "client") 
                  intf_bind_var 
        params = [ binding ] ++ concat [ rpc_call_argdecl ifname a | a <- (tail (tail args)) ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

call_ooo_signature_x :: String -> MessageDef -> C.Unit
call_ooo_signature_x ifname m@(RPC s args _) =
    let name = call_ooo_sig_type_x ifname m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname "client") 
                  intf_bind_var 
        params = [ binding ] ++ concat [ rpc_call_argdecl ifname a | a <- (tail (tail args)) ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

send_signature :: Side -> String -> String -> MessageDef -> C.Unit

send_signature side sender ifname m@(Message dir _ args _) =
    let name = msg_sig_type ifname sender "send" m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname sender) 
                  intf_bind_var 
        params = [ binding ] ++ concat [ BC.msg_argdecl BC.TX ifname a | a <- args ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

send_signature side sender ifname m@(RPC s args _) =
    let name = msg_sig_type ifname sender "send" m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname sender) 
                  intf_bind_var 
        params = [ binding ] ++ concat [ rpc_send_argdecl side ifname a | a <- args ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

receive_signature side receiver ifname m@(Message dir _ args _) =
    let name = msg_sig_type ifname receiver "recv" m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname receiver) 
                  intf_bind_var 
        params = [ binding ] ++ concat [ receive_msg_argdecl ifname a | a <- args ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

receive_signature side receiver ifname m@(RPC s args _) =
    let name = msg_sig_type ifname receiver "recv" m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname receiver) 
                  intf_bind_var 
        params = [ binding ] ++ concat [ rpc_receive_argdecl side ifname a | a <- args ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

send_signature_x side sender ifname m@(Message dir _ args _) =
    let name = msg_sig_type_x ifname sender "send" m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname sender) 
                  intf_bind_var 
        params = [ binding ] ++ concat [ BC.msg_argdecl BC.TX ifname a | a <- args ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

send_signature_x side sender ifname m@(RPC s args _) =
    let name = msg_sig_type_x ifname sender "send" m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname sender) 
                  intf_bind_var 
        params = [ binding ] ++ concat [ rpc_send_argdecl side ifname a | a <- args ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

receive_signature_x side receiver ifname m@(Message dir _ args _) =
    let name = msg_sig_type_x ifname receiver "recv" m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname receiver) 
                  intf_bind_var 
        params = [ binding ] ++ concat [ receive_msg_argdecl ifname a | a <- args ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

receive_signature_x side receiver ifname m@(RPC s args _) =
    let name = msg_sig_type_x ifname receiver "recv" m
        binding = C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifname receiver) 
                  intf_bind_var 
        params = [ binding ] ++ concat [ rpc_receive_argdecl side ifname a | a <- args ]
    in
      C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) $ fnptr name

rpc_call_argdecl :: String -> RPCArgument -> [C.Param]
rpc_call_argdecl ifn (RPCArgIn tr v) = BC.msg_argdecl BC.TX ifn (Arg tr v)
rpc_call_argdecl ifn (RPCArgOut tr v) = receive_msg_argdecl ifn (Arg tr v)

rpc_send_argdecl :: Side -> String -> RPCArgument -> [C.Param]
rpc_send_argdecl ClientSide ifn (RPCArgIn tr v) = BC.msg_argdecl BC.TX ifn (Arg tr v)
rpc_send_argdecl ClientSide ifn (RPCArgOut  _ _) = []
rpc_send_argdecl ServerSide ifn (RPCArgIn _ _) = []
rpc_send_argdecl ServerSide ifn (RPCArgOut tr v)  = BC.msg_argdecl BC.TX ifn (Arg tr v)

receive_msg_argdecl :: String -> MessageArgument -> [C.Param]
receive_msg_argdecl ifn (Arg tr (Name n)) = 
    [ C.Param (C.Ptr $ BC.type_c_type ifn tr) n ]
receive_msg_argdecl ifn (Arg tr (DynamicArray n l)) = 
    [ C.Param (C.Ptr $ C.Ptr $ BC.type_c_type ifn tr) n, 
      C.Param (C.Ptr $ BC.type_c_type ifn size) l ]

rpc_receive_argdecl :: Side -> String -> RPCArgument -> [C.Param]
rpc_receive_argdecl ClientSide ifn (RPCArgOut tr v) = receive_msg_argdecl ifn (Arg tr v)
rpc_receive_argdecl ClientSide ifn (RPCArgIn  _ _) = []
rpc_receive_argdecl ServerSide ifn (RPCArgOut _ _) = []
rpc_receive_argdecl ServerSide ifn (RPCArgIn tr v)  = receive_msg_argdecl ifn (Arg tr v)

msg_enums :: (String -> String) -> (String -> String -> String) -> String -> [MessageDef] -> C.Unit
msg_enums enum element ifname msgs = 
    C.EnumDecl (enum ifname) 
         ([ C.EnumItem (element ifname n) Nothing 
                | m@(Message _ n _ _) <- msgs ]
          ++
          [ C.EnumItem (element ifname n) Nothing 
                | m@(RPC n _ _) <- msgs ]
         )

--
-- Generate a struct to hold the arguments of a message while it's being sent.
-- 
msg_argstruct :: String -> MessageDef -> C.Unit
msg_argstruct ifname m@(Message _ n [] _) = C.NoOp
msg_argstruct ifname m@(Message _ n args _) =
    let tn = msg_argstruct_name ifname n
    in
      C.StructDecl tn (concat [ BC.msg_argdecl BC.RX ifname a | a <- args ])
msg_argstruct ifname m@(RPC n args _) = 
    C.UnitList [
      C.StructDecl (rpc_argstruct_name ifname n "in") 
           (concat [ rpc_argdecl ClientSide ifname a | a <- args ]),
      C.StructDecl (rpc_argstruct_name ifname n "out") 
           (concat [ rpc_argdecl ServerSide ifname a | a <- args ]),
      C.UnionDecl (rpc_union_name ifname n) [
        C.Param (C.Struct $ rpc_argstruct_name ifname n "in") "in",
        C.Param (C.Struct $ rpc_argstruct_name ifname n "out") "out"
       ]
     ]

--
-- Generate a union of all the above
-- 
intf_union :: String -> [MessageDef] -> C.Unit
intf_union ifn msgs = 
    C.UnionDecl (binding_arg_union_type ifn)
         ([ C.Param (C.Struct $ msg_argstruct_name ifn n) n
            | m@(Message _ n a _) <- msgs, 0 /= length a ]
          ++
          [ C.Param (C.Union $ rpc_union_name ifn n) n 
            | m@(RPC n a _) <- msgs, 0 /= length a ])

rpc_argdecl :: Side -> String -> RPCArgument -> [C.Param]
rpc_argdecl ClientSide ifn (RPCArgIn tr v) = BC.msg_argdecl BC.RX ifn (Arg tr v)
rpc_argdecl ClientSide ifn (RPCArgOut _ _) = []
rpc_argdecl ServerSide ifn (RPCArgOut tr v) = BC.msg_argdecl BC.RX ifn (Arg tr v)
rpc_argdecl ServerSide ifn (RPCArgIn _ _) = []
