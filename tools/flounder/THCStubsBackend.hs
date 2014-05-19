{- 
  THCBackend: generate interface to Flounder THC stubs
   
  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module THCStubsBackend where

import Data.List

import qualified CAbsSyntax as C
import qualified BackendCommon as BC
import qualified THCBackend as THC
import Syntax
import Backend

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

-- Scope a list of strings
ifscope :: String -> String -> String
ifscope ifn s = ifn ++ "_" ++ s

idscope :: String -> String -> String -> String
idscope ifn s suffix  = ifscope ifn (s ++ "__" ++ suffix)

-- Name of the binding struct for an interface type
intf_bind_type :: String -> String -> String
intf_bind_type ifn sender = ifscope ifn $ "thc_" ++ sender ++ "_binding_t"

-- Variable used to refer to a THC binding in the generated code
intf_bind_var = "_thc_binding"

-- Variable used to refer to the underlying IDC binding in the generated code
intf_c2s_idc_bind_var = "_idc_binding"
intf_s2c_idc_bind_var = "_idc_binding"
intf_init_c2s_idc_bind_var = "_c2s_idc_binding"
intf_init_s2c_idc_bind_var = "_s2c_idc_binding"
intf_bh_idc_bind_var = "_idc_binding"

-- Name of the functions to call at start/end of send/receive functions
thc_await_send_fn_name = "thc_await_send"
thc_await_send_fn_name_x = "thc_await_send_x"
thc_init_per_binding_state = "thc_init_per_binding_state"
thc_init_per_recv_state = "thc_init_per_recv_state"
thc_complete_send_fn_name = "thc_complete_send"

thc_start_bh = "thc_start_bh"
thc_start_demuxable_bh = "thc_start_demuxable_bh"
thc_end_bh = "thc_end_bh"

start_send_fn_name = "thc_start_send"
end_send_fn_name = "thc_end_send"

receive_fn_name = "thc_receive"
receive_fn_name_x = "thc_receive_x"

start_receive_demux_fn_name = "thc_start_receive_demux"
cancel_receive_demux_fn_name = "thc_cancel_receive_demux"
receive_demux_fn_name = "thc_receive_demux"
receive_demux_fn_name_x = "thc_receive_demux_x"

start_receive_any_fn_name = "thc_start_receive_any"
start_receive_case_fn_name = "thc_start_receiving"
start_receive_ooo_fn_name = "thc_start_receive_ooo_rpc"
receive_any_wait_fn_name = "thc_wait_receive_any"
receive_any_wait_fn_name_x = "thc_wait_receive_any_x"
end_receive_case_fn_name = "thc_stop_receiving"
end_receive_any_fn_name = "thc_end_receive_any"
end_receive_ooo_fn_name = "thc_end_receive_ooo_rpc"

thc_receiver_info = "thc_receiver_info"

-- Name of the type of a receive-any function
rx_any_sig_type :: String -> String -> String
rx_any_sig_type ifn receiver = idscope ifn "recv_any" "thc_" ++ receiver ++ "_t"

rx_any_fn_name ifn receiver = idscope ifn "recv_any" "thc_" ++ receiver ++ "_fn"
rx_any_fn_name_x ifn receiver = idscope ifn "recv_any" "thc_" ++ receiver ++ "_fn_x"

-- Name of the concrete send/receive functions
send_fn_name :: Side -> String -> String -> String
send_fn_name ClientSide ifn mn = idscope ifn ("client_" ++ mn) "send" 
send_fn_name ServerSide ifn mn = idscope ifn ("service_" ++ mn) "send" 
send_fn_name_x :: Side -> String -> String -> String
send_fn_name_x ClientSide ifn mn = idscope ifn ("client_" ++ mn) "send_x" 
send_fn_name_x ServerSide ifn mn = idscope ifn ("service_" ++ mn) "send_x" 

bh_recv_fn_name :: Side -> String -> String -> String
bh_recv_fn_name ClientSide ifn mn = idscope ifn ("client_" ++ mn) "bh_recv" 
bh_recv_fn_name ServerSide ifn mn = idscope ifn ("service_" ++ mn) "bh_recv" 

recv_fn_name :: Side -> String -> String -> String
recv_fn_name ClientSide ifn mn = idscope ifn ("client_" ++ mn) "recv" 
recv_fn_name ServerSide ifn mn = idscope ifn ("service_" ++ mn) "recv" 
recv_fn_name_x :: Side -> String -> String -> String
recv_fn_name_x ClientSide ifn mn = idscope ifn ("client_" ++ mn) "recv_x" 
recv_fn_name_x ServerSide ifn mn = idscope ifn ("service_" ++ mn) "recv_x" 

-- Name of the funtcion to call to initialize client/service bindings
thc_init_bindings_name = "thc_init_binding_states"

-- Send continuation
send_cont_ex = (C.Variable "(MKCONT(thc_complete_send_cb, _idc_binding))")

-- Tx-busy error
err_tx_busy_ex = C.Variable "FLOUNDER_ERR_TX_BUSY"

-- Name of the struct holding message args for SAR
ptr_msg_argstruct_name :: String -> String -> String
ptr_msg_argstruct_name ifn n = idscope ifn n "ptr_args_t"

ptr_rpc_argstruct_name :: String -> String -> String -> String
ptr_rpc_argstruct_name ifn n inout = idscope ifn n (inout ++ "_ptr_args_t")

ptr_rpc_union_name :: String -> String -> String 
ptr_rpc_union_name ifn n = idscope ifn n "_ptr_union_t"

-- Name of the struct type holding all the arguments for recv any
ptr_binding_arg_struct_type :: String -> String
ptr_binding_arg_struct_type ifn = ifscope ifn "thc_ptr_arg_struct"

-- Names for the RPC layer functinos
call_seq_fn_name :: String -> String -> String
call_seq_fn_name ifn mn = idscope ifn mn "call_seq" 

call_seq_fn_name_x :: String -> String -> String
call_seq_fn_name_x ifn mn = idscope ifn mn "call_seq_x" 

call_fifo_fn_name :: String -> String -> String
call_fifo_fn_name ifn mn = idscope ifn mn "call_fifo" 

call_fifo_fn_name_x :: String -> String -> String
call_fifo_fn_name_x ifn mn = idscope ifn mn "call_fifo_x" 

call_ooo_fn_name :: String -> String -> String
call_ooo_fn_name ifn mn = idscope ifn mn "call_ooo" 

call_ooo_fn_name_x :: String -> String -> String
call_ooo_fn_name_x ifn mn = idscope ifn mn "call_ooo_x" 

data IDCChannel = C2S | S2C;

data Cancelable = CANCELABLE | NONCANCELABLE;

select_idc :: Side -> BC.Direction -> IDCChannel 
select_idc ClientSide BC.TX = C2S
select_idc ClientSide BC.RX = S2C
select_idc ServerSide BC.TX = S2C
select_idc ServerSide BC.RX = C2S

intf_idc_bind_var :: Side -> BC.Direction -> String
intf_idc_bind_var ClientSide BC.TX = intf_c2s_idc_bind_var
intf_idc_bind_var ClientSide BC.RX = intf_s2c_idc_bind_var
intf_idc_bind_var ServerSide BC.TX = intf_s2c_idc_bind_var
intf_idc_bind_var ServerSide BC.RX = intf_c2s_idc_bind_var

intf_init_idc_bind_var :: Side -> BC.Direction -> String
intf_init_idc_bind_var ClientSide BC.TX = intf_init_c2s_idc_bind_var
intf_init_idc_bind_var ClientSide BC.RX = intf_init_s2c_idc_bind_var
intf_init_idc_bind_var ServerSide BC.TX = intf_init_s2c_idc_bind_var
intf_init_idc_bind_var ServerSide BC.RX = intf_init_c2s_idc_bind_var

------------------------------------------------------------------------
-- Language mapping: Create the THC dummy stubs implementation
------------------------------------------------------------------------

compile :: String -> String -> Interface -> String
compile infile outfile interface =
    unlines $ C.pp_unit $ C.UnitList $ intf_thc_stubs_file infile interface

intf_thc_stubs_file :: String -> Interface -> [ C.Unit ]
intf_thc_stubs_file infile interface@(Interface name descr decls) = 
    let (types, messages) = partitionTypesMessages decls
        nmessages = length messages
    in [ 
        intf_thc_stubs_preamble infile name descr,
        C.Blank,
        C.Include C.Standard "stddef.h",
        C.IfDef "BARRELFISH" [ C.Include C.Standard "barrelfish/barrelfish.h",
                               C.Include C.Standard "barrelfish/nameservice_client.h",
                               C.Include C.Local ("if/" ++ name ++ "_thc.h"),
                               C.Include C.Local "thc/thc.h" ]
                             [ C.Include C.Local (name ++ "_thc.h"),
                               C.Include C.Local "thc.h" ],
        C.Blank,
          
        C.MultiComment [ "Send functions" ],
        C.UnitList [ send_function NONCANCELABLE ClientSide name m | m <- messages, isForward m ],
        C.UnitList [ send_function NONCANCELABLE ServerSide name m | m <- messages, isBackward m ],
        C.UnitList [ send_function CANCELABLE ClientSide name m | m <- messages, isForward m ],
        C.UnitList [ send_function CANCELABLE ServerSide name m | m <- messages, isBackward m ],
        C.Blank,

        C.Blank,
        C.MultiComment [ "Struct type for holding pointers to the args for each msg" ],
        C.UnitList [ msg_argstruct name m | m <- messages ],
        C.Blank,

        C.MultiComment [ "Struct type for Receive-any and Bottom-half receive functions to hold pointers-to-message-argument structs" ],
        intf_struct name messages,
        C.Blank,

        C.MultiComment [ "Receive functions" ],
        C.UnitList [ recv_function NONCANCELABLE ClientSide name m | m <- messages, isBackward m ],
        C.UnitList [ recv_function NONCANCELABLE ServerSide name m | m <- messages, isForward m ],
        C.UnitList [ recv_function CANCELABLE ClientSide name m | m <- messages, isBackward m ],
        C.UnitList [ recv_function CANCELABLE ServerSide name m | m <- messages, isForward m ],
        C.Blank,

        C.MultiComment [ "Receive-any functions" ],
        gen_receive_any_fn NONCANCELABLE ClientSide name [ m | m <- messages, isBackward m],
        gen_receive_any_fn NONCANCELABLE ServerSide name [ m | m <- messages, isForward m],
        gen_receive_any_fn CANCELABLE ClientSide name [ m | m <- messages, isBackward m],
        gen_receive_any_fn CANCELABLE ServerSide name [ m | m <- messages, isForward m],

        C.MultiComment [ "Bottom-half receive functions" ],
        C.UnitList [ bh_recv_function ClientSide name m | m <- messages, isBackward m ],
        C.UnitList [ bh_recv_function ServerSide name m | m <- messages, isForward m ],
        C.Blank,

        C.MultiComment [ "RPC-layer functions" ],
        C.UnitList [ gen_call_seq NONCANCELABLE name m | m <- messages, THC.isRPC m ],
        C.UnitList [ gen_call_fifo NONCANCELABLE name m | m <- messages, THC.isRPC m ],
        C.UnitList [ gen_call_ooo NONCANCELABLE name m | m <- messages, THC.isOOORPC m ],
        C.UnitList [ gen_call_seq CANCELABLE name m | m <- messages, THC.isRPC m ],
        C.UnitList [ gen_call_fifo CANCELABLE name m | m <- messages, THC.isRPC m ],
        C.UnitList [ gen_call_ooo CANCELABLE name m | m <- messages, THC.isOOORPC m ],

        C.MultiComment [ "Initialization functions" ],
        init_function ClientSide name messages,
        init_function ServerSide name messages,

        C.Blank,
        C.MultiComment [ "Connection-management functions" ],
        export_cb_function name,
        connect_cb_function name,
        export_function name,
        accept_function name,
        bind_cb_function name,
        connect_function name,
        connect_by_name_function name,
  
        C.Blank


        ]

intf_thc_stubs_preamble :: String -> String -> Maybe String -> C.Unit
intf_thc_stubs_preamble infile name descr = 
    let dstr = case descr of
                 Nothing -> "not specified"
                 Just s -> s
    in
    C.MultiComment [ 
          "Copyright (c) 2010, ETH Zurich.",
          "All rights reserved.",
          "",
          "INTERFACE NAME: " ++ name,
          "INTEFACE FILE: " ++ infile,
          "INTERFACE DESCRIPTION: " ++ dstr,
          "",
          "This file is distributed under the terms in the attached LICENSE",
          "file. If you do not find this file, copies can be found by",
          "writing to:",
          "ETH Zurich D-INFK, Universitaetstr.6, CH-8092 Zurich.",
          "Attn: Systems Group.",
          "",
          "THIS FILE IS AUTOMATICALLY GENERATED BY FLOUNDER: DO NOT EDIT!" ]

msg_argname :: MessageArgument -> [C.Expr]
msg_argname (Arg tr (Name n)) = 
    [ C.Variable n ]
msg_argname (Arg tr (DynamicArray n l)) = 
    [ C.Variable n,
      C.Variable l ]

rpc_argdecl :: BC.Direction -> Side -> String -> RPCArgument -> [C.Param]
rpc_argdecl dir ClientSide ifn (RPCArgIn tr v) = BC.msg_argdecl dir ifn (Arg tr v)
rpc_argdecl dir ClientSide ifn (RPCArgOut _ _ ) = []
rpc_argdecl dir ServerSide ifn (RPCArgOut tr v) = BC.msg_argdecl dir ifn (Arg tr v)
rpc_argdecl dir ServerSide ifn (RPCArgIn _ _ ) = []

rpc_argname :: Side -> RPCArgument -> [C.Expr]
rpc_argname ClientSide (RPCArgIn tr v) = msg_argname (Arg tr v)
rpc_argname ServerSide (RPCArgOut tr v) = msg_argname (Arg tr v)
rpc_argname ClientSide (RPCArgOut _ _ ) = []
rpc_argname ServerSide (RPCArgIn _ _ ) = []

rx_rpc_argdecl :: Side -> String -> RPCArgument -> [C.Param]
rx_rpc_argdecl ServerSide ifn (RPCArgIn tr v) = BC.msg_argdecl BC.RX ifn (Arg tr v)
rx_rpc_argdecl ServerSide ifn (RPCArgOut _ _ ) = []
rx_rpc_argdecl ClientSide ifn (RPCArgOut tr v) = BC.msg_argdecl BC.RX ifn (Arg tr v)
rx_rpc_argdecl ClientSide ifn (RPCArgIn _ _ ) = []

receive_rpc_argdecl :: Side -> String -> RPCArgument -> [C.Param]
receive_rpc_argdecl ClientSide ifn (RPCArgOut tr v) = THC.receive_msg_argdecl ifn (Arg tr v)
receive_rpc_argdecl ClientSide ifn (RPCArgIn _ _ ) = []
receive_rpc_argdecl ServerSide ifn (RPCArgIn tr v) = THC.receive_msg_argdecl ifn (Arg tr v)
receive_rpc_argdecl ServerSide ifn (RPCArgOut _ _ ) = []

call_rpc_argdecl :: String -> RPCArgument -> [C.Param]
call_rpc_argdecl ifn (RPCArgIn tr v) = BC.msg_argdecl BC.TX ifn (Arg tr v)
call_rpc_argdecl ifn (RPCArgOut tr v) = THC.receive_msg_argdecl ifn (Arg tr v)

startend_call :: String -> String -> String -> C.Stmt
startend_call fn ifn mn =
   C.Ex $ C.Call fn [ 
             C.Variable intf_bind_var
          ]

-- struct foo_binding *_idc_binding = 
--     (struct foo_binding *)((_thc_binding) -> st)

init_idc_binding_var :: IDCChannel -> String -> C.Stmt
init_idc_binding_var C2S ifn =
   C.VarDecl C.NoScope C.NonConst idc_binding_type intf_c2s_idc_bind_var (Just initializer)
   where 
     idc_binding_type = C.Ptr $ C.Struct $ BC.intf_bind_type ifn
     initializer = C.Cast (idc_binding_type) (C.DerefField (C.Variable intf_bind_var) "_c2s_st")
init_idc_binding_var S2C ifn =
   C.VarDecl C.NoScope C.NonConst idc_binding_type intf_s2c_idc_bind_var (Just initializer)
   where 
     idc_binding_type = C.Ptr $ C.Struct $ BC.intf_bind_type ifn
     initializer = C.Cast (idc_binding_type) (C.DerefField (C.Variable intf_bind_var) "_s2c_st")


init_thc_binding_var :: Side -> String -> C.Stmt
init_thc_binding_var side ifn =
   C.VarDecl C.NoScope C.NonConst thc_binding_type intf_bind_var (Just initializer)
   where 
     thc_binding_type = C.Ptr $ C.Struct $ THC.intf_bind_type ifn (show side)
     initializer = C.Cast (thc_binding_type) (C.DerefField (C.Variable intf_bh_idc_bind_var) "st")


-- Generate palceholder-receive functions for each message.
-- These are installed in the rx_vtbl to collect messages when
-- no THC sender is presend.
--
--
--     static void bh_recv_foo_t(struct foo_binding *binding,
--                                        uint64_t arg1) {
--       struct foo_binding_thc *thc;
--       foo_rx_method_fn *fn;
--       fn = thc_start_bh(thc, binding);
--       fn(binding, arg1);
--       thc_end_bh(thc, binding);
--     }

bh_recv_function :: Side -> String -> MessageDef -> C.Unit
bh_recv_function side ifn m@(Message _ n args _) = 
   let pb = C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name
       perrx_ nm = C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ nm) 
       perrx m@(Message _ n args _) = perrx_ $ recvEnum side ifn n
       perrx m@(RPC n args _) = perrx_ $ recvEnum side ifn n
       recvEnum ClientSide = THC.resp_msg_enum_elem_name
       recvEnum ServerSide = THC.call_msg_enum_elem_name
       common = C.Variable intf_bh_idc_bind_var
       sidename = show side
       recv_function_args = 
         concat [ 
           [C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type ifn) intf_bh_idc_bind_var], 
           (concat [ BC.msg_argdecl BC.RX ifn a | a <- args ]) ]
       decl_fn_var x = 
           C.VarDecl C.NoScope C.NonConst (C.Ptr $ C.Struct thc_receiver_info) "rxi" (Just x)
       decl_args_var =
           C.VarDecl C.NoScope C.NonConst (C.Ptr $ C.Struct $ ptr_binding_arg_struct_type ifn) "__attribute__((unused)) _args" (Just (C.DerefField (C.Variable "rxi") "args"))
       assignment (Arg _ (Name an)) =
           [ C.Ex $ C.Assignment (C.DerefPtr ((C.FieldOf ((C.DerefField (C.Variable "_args") n)) an))) (C.Variable an) ]
       assignment (Arg _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (C.DerefPtr ((C.FieldOf ((C.DerefField (C.Variable "_args") n)) an))) (C.Variable an),
             C.Ex $ C.Assignment (C.DerefPtr ((C.FieldOf ((C.DerefField (C.Variable "_args") n)) al))) (C.Variable al) ]
       recv_function_body = [ 
           init_thc_binding_var side ifn,
           decl_fn_var (C.Call thc_start_bh [ pb, common, ( perrx m ) ]),
           C.If (C.Binary C.Equals (C.Variable "rxi") (C.Variable "NULL"))
             [ C.ReturnVoid ]
             [ ],
           decl_args_var,
           C.Ex $ C.Assignment (C.DerefPtr ((C.DerefField (C.Variable "rxi") "msg"))) (C.Cast (C.TypeName "int") (C.Variable $ THC.msg_enum_elem_name ifn n)) ]
         ++ concat [ assignment a | a <- args ]
         ++ [ C.Ex $ C.Call thc_end_bh [ pb, common, ( perrx m ), (C.Variable "rxi") ]
         ]
   in 
     C.FunctionDef C.Static (C.Void) (bh_recv_fn_name side ifn n) 
          recv_function_args
          recv_function_body;


bh_recv_function side ifn m@(RPC n args _) = 
   let pb = C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name
       perrx_ nm = C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ nm) 
       perrx m@(Message _ n args _) = perrx_ $ recvEnum side ifn n
       perrx m@(RPC n args _) = perrx_ $ recvEnum side ifn n
       recvEnum ClientSide = THC.resp_msg_enum_elem_name
       recvEnum ServerSide = THC.call_msg_enum_elem_name
       common = C.Variable intf_bh_idc_bind_var
       sidename = show side
       recv_function_args = 
         concat [ 
           [C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type ifn) intf_bh_idc_bind_var], 
           (concat [ rx_rpc_argdecl side ifn a | a <- args ]) ]
       opname ClientSide n = n ++ "_response"
       opname ServerSide n = n ++ "_call"
       assignment (RPCArgIn _ (Name an)) =
           [ C.Ex $ C.Assignment (C.DerefPtr ((C.FieldOf (C.FieldOf ((C.DerefField (C.Variable "args") n)) "in" ) an))) (C.Variable an) ]
       assignment (RPCArgIn _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (C.DerefPtr ((C.FieldOf (C.FieldOf ((C.DerefField (C.Variable "args") n)) "in" ) an))) (C.Variable an),
             C.Ex $ C.Assignment (C.DerefPtr ((C.FieldOf (C.FieldOf ((C.DerefField (C.Variable "args") n)) "in" ) al))) (C.Variable al) ]
       assignment (RPCArgOut _ (Name an)) =
           [ C.Ex $ C.Assignment (C.DerefPtr ((C.FieldOf (C.FieldOf ((C.DerefField (C.Variable "args") n)) "out" ) an))) (C.Variable an) ]
       assignment (RPCArgOut _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (C.DerefPtr ((C.FieldOf (C.FieldOf ((C.DerefField (C.Variable "args") n)) "out" ) an))) (C.Variable an),
             C.Ex $ C.Assignment (C.DerefPtr ((C.FieldOf (C.FieldOf ((C.DerefField (C.Variable "args") n)) "out" ) al))) (C.Variable al) ]
       decl_fn_var x = 
           C.VarDecl C.NoScope C.NonConst (C.Ptr $ C.Struct thc_receiver_info) "rxi" (Just x)
       decl_args_var =
           C.VarDecl C.NoScope C.NonConst (C.Ptr $ C.Struct $ ptr_binding_arg_struct_type ifn) "__attribute__((unused)) args" (Just (C.DerefField (C.Variable "rxi") "args"))
       idc_rx_args ClientSide = idc_rx_args_out
       idc_rx_args ServerSide = idc_rx_args_in
       idc_rx_args_in = concat [rpc_argname ClientSide a | a <- args]
       idc_rx_args_out = concat [rpc_argname ServerSide a | a <- args]
       dir_args ServerSide = [ a | a@(RPCArgIn _ _) <- args ]
       dir_args ClientSide = [ a | a@(RPCArgOut _ _) <- args ]
       start_fn ClientSide m = if (THC.isOOORPC m) then thc_start_demuxable_bh else thc_start_bh
       start_fn ServerSide _ = thc_start_bh                               
       demux_args ClientSide m = if (THC.isOOORPC m) then [ C.Variable "seq_out" ] else []
       demux_args ServerSide _ = []
       recv_function_body = [ 
           init_thc_binding_var side ifn,
           decl_fn_var (C.Call (start_fn side m) (concat [[ pb, common, ( perrx m ) ], (demux_args side m)])),
           C.If (C.Binary C.Equals (C.Variable "rxi") (C.Variable "NULL"))
             [ C.ReturnVoid ]
             [ ],
           decl_args_var,
           C.Ex $ C.Assignment (C.DerefPtr ((C.DerefField (C.Variable "rxi") "msg"))) (C.Cast (C.TypeName "int") (C.Variable $ THC.msg_enum_elem_name ifn n)) ]
         ++ concat [ assignment a | a <- dir_args side ]
         ++ [C.Ex $ C.Call thc_end_bh [ pb, common, ( perrx m ), (C.Variable "rxi") ]
         ]
   in 
     C.FunctionDef C.Static (C.Void) (bh_recv_fn_name side ifn n) 
          recv_function_args
          recv_function_body;



-- Generate send functions for each message
--
--
--     static errval_t send_foo_t(struct ...binding_thc *thc,
--                                uint64_t id, 
--                                uint64_t value1, 
--                                uint64_t value2) {
--       ...binding b = (...) (thc->st);
--       do {
--         errval_t r = b->tx_vtbl.foo(b, id, value1, value2);
--         if (r != FLOUNDER_ERR_TX_BUSY) {
--           return r;
--         }
--         thc_await_send(thc, b);
--       } while (true);
--     }

send_function :: Cancelable -> Side -> String -> MessageDef -> C.Unit
send_function cb side ifn m@(Message _ n args _) = 
   let fn_name CANCELABLE = send_fn_name_x side ifn n
       fn_name NONCANCELABLE = send_fn_name side ifn n
       sidename = show side
       sem_p CANCELABLE = C.If (C.Binary C.Equals (C.Call "thc_sem_p_x" [ C.AddressOf $ C.FieldOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name) "thc_next_sender" ]) (C.Variable "THC_CANCELED")) [ C.Return $ C.Variable "THC_CANCELED" ] []
       sem_p NONCANCELABLE = C.Ex $ C.Call "thc_sem_p" [ C.AddressOf $ C.FieldOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name) "thc_next_sender" ]
       await_send_branch CANCELABLE = 
         [ C.If (C.Binary C.Equals (C.Call thc_await_send_fn_name_x [ C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name,
                                                                      C.AddressOf $ C.DerefField (C.Variable (intf_idc_bind_var side BC.TX)) "st" ]) (C.Variable "THC_CANCELED"))
           [ C.Ex $ C.Call "thc_sem_v" [ C.AddressOf $ C.FieldOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name) "thc_next_sender" ],
             C.Return $ C.Variable "THC_CANCELED" ] [ ] ]
       await_send_branch NONCANCELABLE = 
         [ C.Ex $ C.Call thc_await_send_fn_name [ C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name,
                                                  C.AddressOf $ C.DerefField (C.Variable (intf_idc_bind_var side BC.TX)) "st" ] ] 
       send_function_args = 
         concat [ 
           [C.Param (C.Ptr $ C.Struct $ THC.intf_bind_type ifn sidename) intf_bind_var], 
           (concat [ BC.msg_argdecl BC.TX ifn a | a <- args ]) ]
       send_function_body = [ 
           init_idc_binding_var (select_idc side BC.TX) ifn,
           sem_p cb,
           C.Ex $ C.Assignment ( C.FieldOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name) "thc_send_complete" ) ( C.NumConstant 0 ),
           C.Ex $ C.Call "THCIncSendCount" [],
           C.DoWhile (C.NumConstant 1) [
             C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "_r"
                (Just $ C.CallInd idc_tx_fn idc_tx_args),
             C.If (C.Binary C.Equals (C.Variable "_r") err_tx_busy_ex) 
                ( await_send_branch cb )
                [ C.Ex $ C.Call thc_complete_send_fn_name [ C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name,
                                                            C.AddressOf $ C.DerefField (C.Variable (intf_idc_bind_var side BC.TX)) "st" ],
                  C.Ex $ C.Call "thc_sem_v" [ C.AddressOf $ C.FieldOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name) "thc_next_sender" ],
                  C.Return $ C.Variable "_r" ]
           ]
         ]
       idc_binding = C.Variable (intf_idc_bind_var side BC.TX)
       idc_tx_vtbl = C.DerefField idc_binding "tx_vtbl"
       idc_tx_fn = C.FieldOf idc_tx_vtbl n 
       idc_tx_args = [ idc_binding, send_cont_ex ]
                     ++
                     (concat [ msg_argname a | a <- args])
   in 
     C.FunctionDef C.Static (C.TypeName "errval_t") (fn_name cb)
          send_function_args
          send_function_body;

send_function cb side ifn m@(RPC n args _) = 
   let fn_name = (case cb of 
                    CANCELABLE -> send_fn_name_x side ifn n
                    NONCANCELABLE -> send_fn_name side ifn n)
       sidename = show side
       sem_p CANCELABLE = C.If (C.Binary C.Equals (C.Call "thc_sem_p_x" [ C.AddressOf $ C.FieldOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name) "thc_next_sender" ]) (C.Variable "THC_CANCELED")) [ C.Return $ C.Variable "THC_CANCELED" ] []
       sem_p NONCANCELABLE = C.Ex $ C.Call "thc_sem_p" [ C.AddressOf $ C.FieldOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name) "thc_next_sender" ]
       await_send_branch CANCELABLE = 
         [ C.If (C.Binary C.Equals (C.Call thc_await_send_fn_name_x [ C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name,
                                                                      C.AddressOf $ C.DerefField (C.Variable (intf_idc_bind_var side BC.TX)) "st" ]) (C.Variable "THC_CANCELED"))
           [ C.Ex $ C.Call "thc_sem_v" [ C.AddressOf $ C.FieldOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name) "thc_next_sender" ],
             C.Return $ C.Variable "THC_CANCELED" ] [ ] ]
       await_send_branch NONCANCELABLE = 
         [ C.Ex $ C.Call thc_await_send_fn_name [ C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name,
                                                  C.AddressOf $ C.DerefField (C.Variable (intf_idc_bind_var side BC.TX)) "st" ] ] 
       send_function_args = 
         concat [ 
           [C.Param (C.Ptr $ C.Struct $ THC.intf_bind_type ifn sidename) intf_bind_var], 
           (concat [ rpc_argdecl BC.TX side ifn a | a <- args ]) ]
       send_function_body = [ 
           init_idc_binding_var (select_idc side BC.TX) ifn,
           sem_p cb,
           C.Ex $ C.Assignment ( C.FieldOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name) "thc_send_complete" ) ( C.NumConstant 0 ),
           C.Ex $ C.Call "THCIncSendCount" [],
           C.DoWhile (C.NumConstant 1) [
             C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "_r"
                (Just $ C.CallInd idc_tx_fn (idc_tx_args side)),
             C.If (C.Binary C.Equals (C.Variable "_r") err_tx_busy_ex) 
                ( await_send_branch cb )
                [ C.Ex $ C.Call thc_complete_send_fn_name [ C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name,
                                                            C.AddressOf $ C.DerefField (C.Variable (intf_idc_bind_var side BC.TX)) "st" ],
                  C.Ex $ C.Call "thc_sem_v" [ C.AddressOf $ C.FieldOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name) "thc_next_sender" ],
                  C.Return $ C.Variable "_r" ]
           ]
         ]
       rpc_name ClientSide n = BC.rpc_call_name n
       rpc_name ServerSide n = BC.rpc_resp_name n
       idc_binding = C.Variable (intf_idc_bind_var side BC.TX)
       idc_tx_vtbl = C.DerefField idc_binding "tx_vtbl"
       idc_tx_fn = C.FieldOf idc_tx_vtbl (rpc_name side n) 
       idc_tx_args ClientSide = idc_tx_args_in
       idc_tx_args ServerSide = idc_tx_args_out
       idc_tx_args_in = [ idc_binding, send_cont_ex ]
                        ++
                        (concat [ rpc_argname ClientSide a | a <- args ])
       idc_tx_args_out = [ idc_binding, send_cont_ex ]
                         ++
                         (concat [ rpc_argname ServerSide a | a <- args ])
   in 
     C.FunctionDef C.Static (C.TypeName "errval_t") fn_name
          send_function_args
          send_function_body;

-- Initialization functions

init_function :: Side -> String -> [MessageDef] -> C.Unit
init_function side ifn messages =
    let init_name_for ClientSide = THC.init_client_name ifn
        init_name_for ServerSide = THC.init_service_name ifn
        init_name = init_name_for side
        rpc_name ClientSide n = BC.rpc_call_name n
        rpc_name ServerSide n = BC.rpc_resp_name n
        filterSend ClientSide = isForward
        filterSend ServerSide = isBackward
        filterRecv ClientSide = isBackward
        filterRecv ServerSide = isForward
        recvEnum ClientSide = THC.resp_msg_enum_elem_name
        recvEnum ServerSide = THC.call_msg_enum_elem_name
        opname ClientSide n = n ++ "_response"
        opname ServerSide n = n ++ "_call"
        init_args = [
                C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifn (show side)) intf_bind_var,
                C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type ifn) intf_init_c2s_idc_bind_var,
                C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type ifn) intf_init_s2c_idc_bind_var ]
        init_send_fn CANCELABLE m@(Message _ n args _) = 
                C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "send_x") n) (C.AddressOf $ C.Variable $ send_fn_name_x side ifn n)
        init_send_fn CANCELABLE m@(RPC n args _) = 
                C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "send_x") n) (C.AddressOf $ C.Variable $ send_fn_name_x side ifn n)
        init_send_fn NONCANCELABLE m@(Message _ n args _) = 
                C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "send") n) (C.AddressOf $ C.Variable $ send_fn_name side ifn n)
        init_send_fn NONCANCELABLE m@(RPC n args _) = 
                C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "send") n) (C.AddressOf $ C.Variable $ send_fn_name side ifn n)
        init_recv_ n = C.Ex $ C.Call thc_init_per_recv_state [ C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ n) ]
        init_recv m@(Message _ n args _) = init_recv_ $ recvEnum side ifn n
        init_recv m@(RPC n args _) = init_recv_ $ recvEnum side ifn n
        init_bh m@(Message _ n args _) = 
                C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable (intf_init_idc_bind_var side BC.RX)) "rx_vtbl") n) (C.AddressOf $ C.Variable $ bh_recv_fn_name side ifn n)
        init_bh m@(RPC n args _) = 
                C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable (intf_init_idc_bind_var side BC.RX)) "rx_vtbl") (opname side n)) (C.AddressOf $ C.Variable $ bh_recv_fn_name side ifn n)
        init_recv_fn NONCANCELABLE m@(Message _ n args _) = 
                C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "recv") n) (C.AddressOf $ C.Variable $ recv_fn_name side ifn n)
        init_recv_fn NONCANCELABLE m@(RPC n args _) = 
                C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "recv") n) (C.AddressOf $ C.Variable $ recv_fn_name side ifn n)
        init_recv_fn CANCELABLE m@(Message _ n args _) = 
                C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "recv_x") n) (C.AddressOf $ C.Variable $ recv_fn_name_x side ifn n)
        init_recv_fn CANCELABLE m@(RPC n args _) = 
                C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "recv_x") n) (C.AddressOf $ C.Variable $ recv_fn_name_x side ifn n)
        init_rpc_seq _ ServerSide _ = []
        init_rpc_seq NONCANCELABLE ClientSide m@(RPC n _ _) =
           [ C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "call_seq") n) (C.Variable $ call_seq_fn_name ifn n) ]
        init_rpc_seq CANCELABLE ClientSide m@(RPC n _ _) =
           [ C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "call_seq_x") n) (C.Variable $ call_seq_fn_name_x ifn n) ]
        init_rpc_fifo _ ServerSide _ = []
        init_rpc_fifo NONCANCELABLE ClientSide m@(RPC n _ _) =
           [ C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "call_fifo") n) (C.Variable $ call_fifo_fn_name ifn n) ]
        init_rpc_fifo CANCELABLE ClientSide m@(RPC n _ _) =
           [ C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "call_fifo_x") n) (C.Variable $ call_fifo_fn_name_x ifn n) ]
        init_rpc_ooo _ ServerSide _ = []
        init_rpc_ooo NONCANCELABLE ClientSide m@(RPC n (_:_:args) _) =
           [ C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "call") n) (C.Variable $ call_ooo_fn_name ifn n) ]
        init_rpc_ooo CANCELABLE ClientSide m@(RPC n (_:_:args) _) =
           [ C.Ex $ C.Assignment (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "call_x") n) (C.Variable $ call_ooo_fn_name_x ifn n) ]
        client_only ServerSide _ = []
        client_only ClientSide x = [x]
        check_field fn = C.Ex $ C.Call "CHECK_FIELD" [ C.Variable ("struct " ++ (BC.intf_bind_type ifn)), C.Variable fn ]
        init_stmts = [ check_field "st",
                       check_field "waitset",
                       check_field "mutex",
                       check_field "can_send",
                       check_field "register_send",
                       check_field "change_waitset",
                       check_field "control",
                       check_field "error_handler",
                       C.Ex $ C.Assignment (C.DerefField (C.Variable intf_bind_var) "_c2s_st") (C.Variable intf_init_c2s_idc_bind_var), 
                       C.Ex $ C.Assignment (C.DerefField (C.Variable intf_bind_var) "_s2c_st") (C.Variable intf_init_s2c_idc_bind_var) ]
                ++ [ C.Ex $ C.Assignment ((C.DerefField (C.Variable intf_init_c2s_idc_bind_var)) "st") (C.Variable intf_bind_var) ]
                ++ [ C.Ex $ C.Assignment ((C.DerefField (C.Variable intf_init_s2c_idc_bind_var)) "st") (C.Variable intf_bind_var) ]
                ++ concat [ client_only side $ C.Ex $ C.Call "thc_seq_init" [ C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.ooo_rpc_seq_name ] ]
                ++ [ C.Ex $ C.Call thc_init_per_binding_state [ C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name] ]
                ++ [ init_send_fn NONCANCELABLE m | m <- messages, (filterSend side) m ]
                ++ [ init_send_fn CANCELABLE m | m <- messages, (filterSend side) m ]
                ++ [ init_recv m | m <- messages, (filterRecv side) m ]
                ++ [ init_bh m | m <- messages, (filterRecv side) m ]
                ++ [ init_recv_fn NONCANCELABLE m | m <- messages, (filterRecv side) m ]
                ++ [ init_recv_fn CANCELABLE m | m <- messages, (filterRecv side) m ]
                ++ [ C.Ex $ C.Assignment (C.DerefField (C.Variable intf_bind_var) "recv_any") (C.Variable $ rx_any_fn_name ifn (show side)) ]
                ++ [ C.Ex $ C.Assignment (C.DerefField (C.Variable intf_bind_var) "recv_any_x") (C.Variable $ rx_any_fn_name_x ifn (show side)) ]
                ++ concat [ init_rpc_seq NONCANCELABLE side m | m <- messages, THC.isRPC m ]
                ++ concat [ init_rpc_fifo NONCANCELABLE side m | m <- messages, THC.isRPC m ]
                ++ concat [ init_rpc_ooo NONCANCELABLE side m | m <- messages, THC.isOOORPC m ]
                ++ concat [ init_rpc_seq CANCELABLE side m | m <- messages, THC.isRPC m ]
                ++ concat [ init_rpc_fifo CANCELABLE side m | m <- messages, THC.isRPC m ]
                ++ concat [ init_rpc_ooo CANCELABLE side m | m <- messages, THC.isOOORPC m ]
                ++ [ C.Return $ C.NumConstant 0 ]

    in
        C.FunctionDef C.NoScope (C.TypeName "errval_t") init_name init_args init_stmts


--
-- Generate a struct to hold the arguments of a message while it's being sent.
-- 
msg_argstruct :: String -> MessageDef -> C.Unit
msg_argstruct ifname m@(Message _ n [] _) = C.NoOp
msg_argstruct ifname m@(Message _ n args _) =
    let tn = ptr_msg_argstruct_name ifname n
    in
      C.StructDecl tn (concat [ ptr_msg_argdecl ifname a | a <- args ])
msg_argstruct ifname m@(RPC n args _) = 
    C.UnitList [
      C.StructDecl (ptr_rpc_argstruct_name ifname n "in") 
           (concat [ ptr_rpc_argdecl ClientSide ifname a | a <- args ]),
      C.StructDecl (ptr_rpc_argstruct_name ifname n "out") 
           (concat [ ptr_rpc_argdecl ServerSide ifname a | a <- args ]),
      C.UnionDecl (ptr_rpc_union_name ifname n) [
        C.Param (C.Struct $ ptr_rpc_argstruct_name ifname n "in") "in",
        C.Param (C.Struct $ ptr_rpc_argstruct_name ifname n "out") "out"
       ]
     ]

--
-- Generate a union of all the above
-- 
intf_struct :: String -> [MessageDef] -> C.Unit
intf_struct ifn msgs = 
    C.StructDecl (ptr_binding_arg_struct_type ifn)
         ([ C.Param (C.Struct $ ptr_msg_argstruct_name ifn n) n
            | m@(Message _ n a _) <- msgs, 0 /= length a ]
          ++
          [ C.Param (C.Union $ ptr_rpc_union_name ifn n) n 
            | m@(RPC n a _) <- msgs, 0 /= length a ])

ptr_msg_argdecl :: String -> MessageArgument -> [C.Param]
ptr_msg_argdecl ifn (Arg tr (Name n)) = 
    [ C.Param (C.Ptr $ BC.type_c_type ifn tr) n ]
ptr_msg_argdecl ifn (Arg tr (DynamicArray n l)) = 
    [ C.Param (C.Ptr $ C.Ptr $ BC.type_c_type ifn tr) n, 
      C.Param (C.Ptr $ BC.type_c_type ifn size) l ]

ptr_rpc_argdecl :: Side -> String -> RPCArgument -> [C.Param]
ptr_rpc_argdecl ClientSide ifn (RPCArgIn tr v) = ptr_msg_argdecl ifn (Arg tr v)
ptr_rpc_argdecl ClientSide ifn (RPCArgOut _ _) = []
ptr_rpc_argdecl ServerSide ifn (RPCArgOut tr v) = ptr_msg_argdecl ifn (Arg tr v)
ptr_rpc_argdecl ServerSide ifn (RPCArgIn _ _) = []

-- Generate recv functions

recv_function_rpc_body assign cb side std_receive_fn ifn m@(RPC n args _) = 
   let pb = C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name
       sidename = show side
       recvEnum ClientSide = THC.resp_msg_enum_elem_name
       recvEnum ServerSide = THC.call_msg_enum_elem_name
       assignment (RPCArgIn _ (Name an)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") an))) (C.Variable an) ]
       assignment (RPCArgIn _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") an))) (C.Variable an),
             C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") al))) (C.Variable al) ]
       assignment (RPCArgOut _ (Name an)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") an))) (C.Variable an) ]
       assignment (RPCArgOut _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") an))) (C.Variable an),
             C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") al))) (C.Variable al) ]
       dir_args ServerSide = [ a | a@(RPCArgIn _ _) <- args ]
       dir_args ClientSide = [ a | a@(RPCArgOut _ _) <- args ]
    in [ 
           C.VarDecl C.NoScope C.NonConst (C.Struct $ ptr_binding_arg_struct_type ifn) "_args" Nothing,
           C.VarDecl C.NoScope C.NonConst (C.Struct thc_receiver_info) "_rxi" Nothing,
           C.VarDecl C.NoScope C.NonConst (C.TypeName "int") "_msg" Nothing ]
         ++ concat [ assignment a | a <- dir_args side ]
         ++ [ C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "waiter") $ C.Variable "NULL",
           C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "args") $ C.AddressOf $ C.Variable "_args",
           C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "msg") $ C.AddressOf $ C.Variable "_msg",
           C.Ex $ C.Assignment (C.Variable assign) $ C.Call std_receive_fn [
              pb,
              C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ recvEnum side ifn n),
              C.AddressOf $ C.Variable "_rxi"
            ]
         ]

recv_function :: Cancelable -> Side -> String -> MessageDef -> C.Unit
recv_function cb side ifn m@(Message _ n args _) = 
   let fn_name CANCELABLE = recv_fn_name_x side ifn n
       fn_name NONCANCELABLE = recv_fn_name side ifn n
       std_receive_fn_name CANCELABLE = receive_fn_name_x
       std_receive_fn_name NONCANCELABLE = receive_fn_name
       pb = C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name
       sidename = show side
       recvEnum ClientSide = THC.resp_msg_enum_elem_name
       recvEnum ServerSide = THC.call_msg_enum_elem_name
       recv_function_args = 
         concat [ 
           [C.Param (C.Ptr $ C.Struct $ THC.intf_bind_type ifn sidename) intf_bind_var], 
           (concat [ THC.receive_msg_argdecl ifn a | a <- args ]) ]
       assignment (Arg _ (Name an)) =
           [ C.Ex $ C.Assignment (((C.FieldOf ((C.FieldOf (C.Variable "_args") n)) an))) (C.Variable an) ]
       assignment (Arg _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (((C.FieldOf ((C.FieldOf (C.Variable "_args") n)) an))) (C.Variable an),
             C.Ex $ C.Assignment (((C.FieldOf ((C.FieldOf (C.Variable "_args") n)) al))) (C.Variable al) ]
       recv_function_body = [ 
           C.VarDecl C.NoScope C.NonConst (C.Struct $ ptr_binding_arg_struct_type ifn) "_args" Nothing,
           C.VarDecl C.NoScope C.NonConst (C.Struct thc_receiver_info) "_rxi" Nothing,
           C.VarDecl C.NoScope C.NonConst (C.TypeName "int") "_msg" Nothing ]
         ++ concat [ assignment a | a <- args ]
         ++ [ C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "waiter") $ C.Variable "NULL",
           C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "args") $ C.AddressOf $ C.Variable "_args",
           C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "msg") $ C.AddressOf $ C.Variable "_msg",
           C.Return $ C.Call (std_receive_fn_name cb) [
              pb,
              C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ recvEnum side ifn n),
              C.AddressOf $ C.Variable "_rxi"
            ]
         ]
   in 
     C.FunctionDef C.Static (C.TypeName "errval_t") (fn_name cb)
          recv_function_args
          recv_function_body;

recv_function cb side ifn m@(RPC n args _) = 
   let fn_name CANCELABLE = recv_fn_name_x side ifn n
       fn_name NONCANCELABLE = recv_fn_name side ifn n
       std_receive_fn_name CANCELABLE = receive_fn_name_x
       std_receive_fn_name NONCANCELABLE = receive_fn_name
       pb = C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name
       sidename = show side
       recvEnum ClientSide = THC.resp_msg_enum_elem_name
       recvEnum ServerSide = THC.call_msg_enum_elem_name
       recv_function_args = 
         concat [ 
           [C.Param (C.Ptr $ C.Struct $ THC.intf_bind_type ifn sidename) intf_bind_var], 
           (concat [ receive_rpc_argdecl side ifn a | a <- args ]) ]
       assignment (RPCArgIn _ (Name an)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") an))) (C.Variable an) ]
       assignment (RPCArgIn _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") an))) (C.Variable an),
             C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") al))) (C.Variable al) ]
       assignment (RPCArgOut _ (Name an)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") an))) (C.Variable an) ]
       assignment (RPCArgOut _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") an))) (C.Variable an),
             C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") al))) (C.Variable al) ]
       dir_args ServerSide = [ a | a@(RPCArgIn _ _) <- args ]
       dir_args ClientSide = [ a | a@(RPCArgOut _ _) <- args ]
       recv_function_body = [ 
           C.VarDecl C.NoScope C.NonConst (C.Struct $ ptr_binding_arg_struct_type ifn) "_args" Nothing,
           C.VarDecl C.NoScope C.NonConst (C.Struct thc_receiver_info) "_rxi" Nothing,
           C.VarDecl C.NoScope C.NonConst (C.TypeName "int") "_msg" Nothing ]
         ++ concat [ assignment a | a <- dir_args side ]
         ++ [ C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "waiter") $ C.Variable "NULL",
           C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "args") $ C.AddressOf $ C.Variable "_args",
           C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "msg") $ C.AddressOf $ C.Variable "_msg",
           C.Return $ C.Call (std_receive_fn_name cb) [
              pb,
              C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ recvEnum side ifn n),
              C.AddressOf $ C.Variable "_rxi"
            ]
         ]
   in 
     C.FunctionDef C.Static (C.TypeName "errval_t") (fn_name cb)
          recv_function_args
          ([ C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "_result" Nothing ] ++
          (recv_function_rpc_body "_result" cb side (std_receive_fn_name cb) ifn m) ++
          [ C.Return $ C.Variable "_result" ]);

-- Generate receive-any functions

gen_receive_any_fn :: Cancelable -> Side -> String -> [MessageDef] -> C.Unit
gen_receive_any_fn cb side ifn ms =
    let fn_name CANCELABLE = rx_any_fn_name_x ifn end
        fn_name NONCANCELABLE = rx_any_fn_name ifn end
        wait_call CANCELABLE =
             C.Ex $ C.Assignment (C.Variable "_r") (C.Call (receive_any_wait_fn_name_x) [ pb, C.AddressOf $ C.Variable "_rxi" ])
        wait_call NONCANCELABLE =
             C.Ex $ C.Call (receive_any_wait_fn_name) [ pb, C.AddressOf $ C.Variable "_rxi" ]
        end = show side
        pb = C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name
        interested m@(Message _ mn _ _) stmts = 
             C.If (C.Binary C.NotEquals (C.FieldOf (C.Variable "ops") mn) (C.NumConstant 0) )  stmts []
        interested m@(RPC mn _ _) stmts = 
             C.If (C.Binary C.NotEquals (C.FieldOf (C.Variable "ops") mn) (C.NumConstant 0) )  stmts []
        recvEnum ClientSide = THC.resp_msg_enum_elem_name
        recvEnum ServerSide = THC.call_msg_enum_elem_name
        receive_any_fn_args = [
             C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifn end) intf_bind_var,
             C.Param (C.Ptr $ C.Struct $ THC.rx_any_struct_name ifn end) "msg",
             C.Param (C.Struct $ THC.intf_selector_type ifn end) "ops"            
          ]
        per_rx_state m@(RPC n args _) = C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ recvEnum side ifn n)
        per_rx_state m@(Message _ n args _) = C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ recvEnum side ifn n)
        p_rxi = C.AddressOf $ C.Variable "_rxi"
        rpc_assignment n (RPCArgIn _ (Name an)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") an))) $ C.AddressOf (C.FieldOf (C.FieldOf (C.FieldOf (C.DerefField (C.Variable "msg") "args") n) "in") an) ]
        rpc_assignment n (RPCArgIn _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") an))) $ C.AddressOf (C.FieldOf (C.FieldOf (C.FieldOf (C.DerefField (C.Variable "msg") "args") n) "in") an),
             C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") al))) $ C.AddressOf (C.FieldOf (C.FieldOf (C.FieldOf (C.DerefField (C.Variable "msg") "args") n) "in") al) ]
        rpc_assignment n (RPCArgOut _ (Name an)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") an))) $ C.AddressOf (C.FieldOf (C.FieldOf (C.FieldOf (C.DerefField (C.Variable "msg") "args") n) "out") an) ]
        rpc_assignment n (RPCArgOut _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") an))) $ C.AddressOf (C.FieldOf (C.FieldOf (C.FieldOf (C.DerefField (C.Variable "msg") "args") n) "out") an),
             C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") al))) $ C.AddressOf (C.FieldOf (C.FieldOf (C.FieldOf (C.DerefField (C.Variable "msg") "args") n) "out") al) ]
        message_assignment n (Arg _ (Name an)) =
           [ C.Ex $ C.Assignment (((C.FieldOf ((C.FieldOf (C.Variable "_args") n)) an))) $ C.AddressOf (C.FieldOf (C.FieldOf (C.DerefField (C.Variable "msg") "args") n) an) ]
        message_assignment n (Arg _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (((C.FieldOf ((C.FieldOf (C.Variable "_args") n)) an))) $ C.AddressOf (C.FieldOf (C.FieldOf (C.DerefField (C.Variable "msg") "args") n) an),
             C.Ex $ C.Assignment (((C.FieldOf ((C.FieldOf (C.Variable "_args") n)) al))) $ C.AddressOf (C.FieldOf (C.FieldOf (C.DerefField (C.Variable "msg") "args") n) al) ]
        dir_args ServerSide args = [ a | a@(RPCArgIn _ _) <- args ]
        dir_args ClientSide args = [ a | a@(RPCArgOut _ _) <- args ]
        assignments m@(RPC n args _) = concat [ rpc_assignment n a | a <- dir_args side args ]
        assignments m@(Message _ n args _) = concat [ message_assignment n a | a <- args ]
        start_receiving m = (assignments m) ++ [
            C.Ex $ C.Call start_receive_case_fn_name [ pb, per_rx_state m, p_rxi ]
         ]
        receive_any_fn_body = [
             C.VarDecl C.NoScope C.NonConst (C.Struct $ ptr_binding_arg_struct_type ifn) "_args" Nothing,
             C.VarDecl C.NoScope C.NonConst (C.Struct thc_receiver_info) "_rxi" Nothing,
             C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "_r" (Just (C.NumConstant 0)),
             C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "waiter") $ C.Variable "NULL",
             C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "args") $ C.AddressOf $ C.Variable "_args",
             C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "msg") $ C.Cast (C.Ptr $ C.TypeName "int") (C.AddressOf $ C.DerefField (C.Variable "msg") "msg"),
             C.Ex $ C.Call (start_receive_any_fn_name) [ pb ] ]
          ++ [ interested m $ start_receiving m | m <- ms ]
          ++ [ wait_call cb ]
          ++ [ interested m [ C.Ex $ C.Call end_receive_case_fn_name [ pb, per_rx_state m, p_rxi ] ] | m <- ms ]
          ++ [
             C.Ex $ C.Call (end_receive_any_fn_name) [ pb ],
             C.Return $ C.Variable "_r"
          ]
    in C.FunctionDef C.Static (C.TypeName "errval_t") (fn_name cb)
          receive_any_fn_args
          receive_any_fn_body;

-- RPC layer

gen_call_seq cb ifn m@(RPC n args _) = 
   let fn_name CANCELABLE = call_seq_fn_name_x ifn n
       fn_name NONCANCELABLE = call_seq_fn_name ifn n
       call_function_args  = 
         concat [ 
           [C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifn "client") intf_bind_var], 
           (concat [ call_rpc_argdecl ifn a | a <- args ]) ]
       pb = C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name
       call_function_body CANCELABLE = [
            C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "_result" Nothing,
            C.Ex $ C.Assignment (C.Variable "_result") (C.CallInd (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "send_x") n) $ concat [ 
               [ C.Variable intf_bind_var ],
               concat [ send_arg a | a <- args ]
             ] ),
            C.If (C.Binary C.Equals (C.Variable "_result") (C.Variable "THC_CANCELED")) 
              [ C.Return (C.Variable "THC_CANCELED") ]
              ((recv_function_rpc_body "_result" cb ClientSide receive_fn_name_x ifn m) ++ 
                [ C.If (C.Binary C.Equals (C.Variable "_result") (C.Variable "THC_CANCELED")) 
                   [ C.Ex $ C.Call "thc_discard" [ 
                               pb,
                               C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ THC.resp_msg_enum_elem_name ifn n),
                               C.NumConstant 1 ] ]
                   [],
                  C.Return $ C.Variable "_result" ])

          ]
       call_function_body NONCANCELABLE = [ 
            C.Ex $ C.CallInd (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "send") n) $ concat [ 
               [ C.Variable intf_bind_var ],
               concat [ send_arg a | a <- args ]
             ],
            C.Return $ C.CallInd (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "recv") n) $ concat [
               [ C.Variable intf_bind_var ],
               concat [ receive_arg a | a <- args ]
             ]
          ]
       send_arg (RPCArgIn tr (Name an)) = [ C.Variable an ]
       send_arg (RPCArgIn tr (DynamicArray an al)) = [ C.Variable an, C.Variable al ]
       send_arg (RPCArgOut _ _ ) = [ ]
       receive_arg (RPCArgOut tr (Name an)) = [ C.Variable an ]
       receive_arg (RPCArgOut tr (DynamicArray an al)) = [ C.Variable an, C.Variable al ]
       receive_arg (RPCArgIn _ _ ) = [ ]
   in 
        C.FunctionDef C.Static (C.TypeName "errval_t") (fn_name cb) 
          call_function_args
          ( call_function_body cb )


gen_call_fifo cb ifn m@(RPC n args _) = 
   let fn_name CANCELABLE = call_fifo_fn_name_x ifn n
       fn_name NONCANCELABLE = call_fifo_fn_name ifn n
       call_function_args  = 
         concat [ 
           [C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifn "client") intf_bind_var], 
           (concat [ call_rpc_argdecl ifn a | a <- args ]) ]
       pb = C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name
       perrx_ nm = C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ nm) 
       perrx m@(RPC n args _) = perrx_ $ recvEnum ClientSide ifn n
       recvEnum ClientSide = THC.resp_msg_enum_elem_name
       call_function_body CANCELABLE = [ 
            C.VarDecl C.NoScope C.NonConst (C.TypeName "uint64_t") "_bailed" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "_result" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.TypeName "thc_queue_entry_t") "_q" Nothing,
            C.Ex $ C.Call "thc_lock_acquire" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_lock" ],
            C.Ex $ C.Assignment (C.Variable "_result") $ C.CallInd (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "send_x") n) $ concat [ 
               [ C.Variable intf_bind_var ],
               concat [ send_arg a | a <- args ]
             ],
            C.If (C.Binary C.Equals (C.Variable "_result") (C.Variable "THC_CANCELED")) 
              [ C.Ex $ C.Call "thc_lock_release" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_lock" ],
                C.Return (C.Variable "THC_CANCELED") ]
              [ ],
            C.Ex $ C.Call "thc_queue_enter" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_q", C.AddressOf $ C.Variable "_q" ],
            C.Ex $ C.Call "thc_lock_release" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_lock" ],
            C.Ex $ C.Assignment (C.Variable "_result") $ C.Call "thc_queue_await_turn_x" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_q", C.AddressOf $ C.Variable "_q" ],
            C.If (C.Binary C.Equals (C.Variable "_result") (C.Variable "THC_CANCELED")) 
              [ C.Ex $ C.Assignment (C.Variable "_bailed") $ C.Call "thc_queue_leave" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_q", C.AddressOf $ C.Variable "_q" ],
                C.Ex $ C.Call "thc_discard" [ 
                                pb,
                                C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ THC.resp_msg_enum_elem_name ifn n),
                                C.Variable "_bailed" ],
                C.Return (C.Variable "THC_CANCELED") ]
              [ ]
          ] ++ (recv_function_rpc_body "_result" cb ClientSide receive_fn_name_x ifn m) ++ [
            C.Ex $ C.Assignment (C.Variable "_bailed") $ C.Call "thc_queue_leave" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_q", C.AddressOf $ C.Variable "_q" ],
            C.Ex $ C.Call "thc_discard" [ 
                            pb,
                            C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ THC.resp_msg_enum_elem_name ifn n),
                            C.Variable "_bailed" ],
            C.Return $ C.Variable "_result"
          ]
       call_function_body NONCANCELABLE = [ 
            C.VarDecl C.NoScope C.NonConst (C.TypeName "uint64_t") "_bailed" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "_result" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.TypeName "thc_queue_entry_t") "_q" Nothing,
            C.Ex $ C.Call "thc_lock_acquire" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_lock" ],
            C.Ex $ C.CallInd (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "send") n) $ concat [ 
               [ C.Variable intf_bind_var ],
               concat [ send_arg a | a <- args ]
             ],
            C.Ex $ C.Call "thc_queue_enter" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_q", C.AddressOf $ C.Variable "_q" ],
            C.Ex $ C.Call "thc_lock_release" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_lock" ],
            C.Ex $ C.Call "thc_queue_await_turn" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_q", C.AddressOf $ C.Variable "_q" ],
            C.Ex $ C.Assignment (C.Variable "_result") $ C.CallInd (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "recv") n) $ concat [
               [ C.Variable intf_bind_var ],
               concat [ receive_arg a | a <- args ]
             ],
            C.Ex $ C.Assignment (C.Variable "_bailed") $ C.Call "thc_queue_leave" [C.AddressOf $ C.DerefField (perrx m) "fifo_rpc_q", C.AddressOf $ C.Variable "_q" ],
            C.Ex $ C.Call "thc_discard" [ 
                            pb,
                            C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ THC.resp_msg_enum_elem_name ifn n),
                            C.Variable "_bailed" ],
            C.Return $ C.Variable "_result"
          ]
       send_arg (RPCArgIn tr (Name an)) = [ C.Variable an ]
       send_arg (RPCArgIn tr (DynamicArray an al)) = [ C.Variable an, C.Variable al ]
       send_arg (RPCArgOut _ _ ) = [ ]
       receive_arg (RPCArgOut tr (Name an)) = [ C.Variable an ]
       receive_arg (RPCArgOut tr (DynamicArray an al)) = [ C.Variable an, C.Variable al ]
       receive_arg (RPCArgIn _ _ ) = [ ]
   in 
        C.FunctionDef C.Static (C.TypeName "errval_t") (fn_name cb) 
          call_function_args
          ( call_function_body cb )


gen_call_ooo cb ifn m@(RPC n (_:_:args) _) = 
   let fn_name CANCELABLE = call_ooo_fn_name_x ifn n
       fn_name NONCANCELABLE = call_ooo_fn_name ifn n
       pb = C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.thc_per_binding_state_name
       call_function_args  = 
         concat [ 
           [C.Param (C.Ptr $ C.TypeName $ intf_bind_type ifn "client") intf_bind_var], 
           (concat [ call_rpc_argdecl ifn a | a <- args ]) ]
       assignment (RPCArgIn _ (Name an)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") an))) (C.Variable an) ]
       assignment (RPCArgIn _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") an))) (C.Variable an),
             C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "in") al))) (C.Variable al) ]
       assignment (RPCArgOut _ (Name an)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") an))) (C.Variable an) ]
       assignment (RPCArgOut _ (DynamicArray an al)) =
           [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") an))) (C.Variable an),
             C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") al))) (C.Variable al) ]
       call_function_body CANCELABLE = [ 
            C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "_result" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.Struct $ ptr_binding_arg_struct_type ifn) "_args" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.Struct thc_receiver_info) "_rxi" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.TypeName "int") "_msg" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.TypeName "uint64_t") "_seq" Nothing ]
          ++ concat [ assignment a | a@(RPCArgOut _ _) <- args ]
          ++ [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") "seq_out"))) (C.AddressOf $ C.Variable "_seq") ]
          ++ [
            C.Ex $ C.Assignment (C.Variable "_seq") (C.Call "thc_seq_ticket" [C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.ooo_rpc_seq_name ]),
            C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "waiter") $ C.Variable "NULL",
            C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "args") $ C.AddressOf $ C.Variable "_args",
            C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "msg") $ C.AddressOf $ C.Variable "_msg",
            C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "demux") $ C.Variable "_seq",
            C.Ex $ C.Call start_receive_demux_fn_name [
              pb,
              C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ THC.resp_msg_enum_elem_name ifn n),
              C.AddressOf $ C.Variable "_rxi"
            ],
            C.Ex $ C.Assignment (C.Variable "_result") $ C.CallInd (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "send_x") n) $ concat [ 
               [ C.Variable intf_bind_var,
                 C.Variable "_seq" ],
               concat [ send_arg a | a <- args ]
             ],
            C.If (C.Binary C.Equals (C.Variable "_result") (C.Variable "THC_CANCELED")) 
              [ C.Return $ C.Call cancel_receive_demux_fn_name [
                 pb,
                 C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ THC.resp_msg_enum_elem_name ifn n),
                 C.AddressOf $ C.Variable "_rxi"
                ]
              ]
              [ C.Return $ C.Call receive_demux_fn_name_x [
                 pb,
                 C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ THC.resp_msg_enum_elem_name ifn n),
                 C.AddressOf $ C.Variable "_rxi"
                ]
              ]
          ]
       call_function_body NONCANCELABLE = [ 
            C.VarDecl C.NoScope C.NonConst (C.Struct $ ptr_binding_arg_struct_type ifn) "_args" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.Struct thc_receiver_info) "_rxi" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.TypeName "int") "_msg" Nothing,
            C.VarDecl C.NoScope C.NonConst (C.TypeName "uint64_t") "_seq" Nothing ]
          ++ concat [ assignment a | a@(RPCArgOut _ _) <- args ]
          ++ [ C.Ex $ C.Assignment (((C.FieldOf (C.FieldOf (C.FieldOf (C.Variable "_args") n) "out") "seq_out"))) (C.AddressOf $ C.Variable "_seq") ]
          ++ [
            C.Ex $ C.Assignment (C.Variable "_seq") (C.Call "thc_seq_ticket" [C.AddressOf $ C.DerefField (C.Variable intf_bind_var) THC.ooo_rpc_seq_name ]),
            C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "waiter") $ C.Variable "NULL",
            C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "args") $ C.AddressOf $ C.Variable "_args",
            C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "msg") $ C.AddressOf $ C.Variable "_msg",
            C.Ex $ C.Assignment (C.FieldOf (C.Variable "_rxi") "demux") $ C.Variable "_seq",
            C.Ex $ C.Call start_receive_demux_fn_name [
              pb,
              C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ THC.resp_msg_enum_elem_name ifn n),
              C.AddressOf $ C.Variable "_rxi"
            ],
            C.Ex $ C.CallInd (C.FieldOf (C.DerefField (C.Variable intf_bind_var) "send") n) $ concat [ 
               [ C.Variable intf_bind_var,
                 C.Variable "_seq" ],
               concat [ send_arg a | a <- args ]
             ],
            C.Return $ C.Call receive_demux_fn_name [
              pb,
              C.AddressOf $ C.SubscriptOf (C.DerefField (C.Variable intf_bind_var) THC.thc_per_recv_state_name) (C.Variable $ THC.resp_msg_enum_elem_name ifn n),
              C.AddressOf $ C.Variable "_rxi"
            ]
          ]
       send_arg (RPCArgIn tr (Name an)) = [ C.Variable an ]
       send_arg (RPCArgIn tr (DynamicArray an al)) = [ C.Variable an, C.Variable al ]
       send_arg (RPCArgOut _ _ ) = [ ]
       receive_arg (RPCArgOut tr (Name an)) = [ C.Variable an ]
       receive_arg (RPCArgOut tr (DynamicArray an al)) = [ C.Variable an, C.Variable al ]
       receive_arg (RPCArgIn _ _ ) = [ ]
   in 
        C.FunctionDef C.Static (C.TypeName "errval_t") (fn_name cb) 
          call_function_args
          (call_function_body cb)

-- static void ping_pong_thc_export_export_cb(void *st, 
--                                            errval_t err, 
--                                            iref_t iref) {
--  struct ping_pong_thc_export_info *info;
--  info = (struct ping_pong_thc_export_info*) st;
--  thc_lock_acquire(&info->info_lock);
--  if (err_is_fail(err)) {
--    info->err = err;
--  } else {
--    if (info->service_name != NULL) {
--      info->err = nameservice_register(info->service_name,
--                                       iref);
--    }
--    if (info->iref_ptr != NULL) {
--      *(info->iref_ptr) = iref;
--    }
--  }    
--  thc_sem_v(&info->export_cb_done_sem);
-- }

export_cb_function :: String -> C.Unit
export_cb_function ifn =
   let info_ptr_t = C.Ptr $ THC.thc_export_info_t ifn
       info_err = C.DerefField (C.Variable "info") "err" 
       info_service_name = C.DerefField (C.Variable "info") "service_name" 
       info_iref_ptr = C.DerefField (C.Variable "info") "iref_ptr" 
       ptr_info_info_lock = C.AddressOf $ C.DerefField (C.Variable "info") "info_lock" 
       var_st = C.Variable "st"
       var_err = C.Variable "err"
       var_iref = C.Variable "iref"
   in
    C.FunctionDef C.Static (C.TypeName "void") (ifscope ifn "thc_export_cb")
      [ C.Param (C.Ptr $ C.TypeName "void") "st",
        C.Param (C.TypeName "errval_t") "err",
        C.Param (C.TypeName "iref_t") "iref" ]
      [
        C.VarDecl C.NoScope C.NonConst info_ptr_t "info" Nothing,
        C.Ex $ C.Assignment (C.Variable "info") (C.Cast info_ptr_t var_st),
        C.Ex $ C.Call "thc_lock_acquire" [ptr_info_info_lock],
        C.If (C.Call "err_is_fail" [ var_err ])
        -- Error passed in to us
        [C.Ex $ C.Assignment info_err var_err ]
        -- OK so far
        [ C.If (C.Binary C.NotEquals info_service_name (C.Variable "NULL"))
          [ C.Ex $ C.Assignment info_err (C.Call "nameservice_register" [ info_service_name, var_iref]) ] [ ],
          C.If (C.Binary C.NotEquals info_iref_ptr (C.Variable "NULL"))
            [ C.Ex $ C.Assignment (C.DerefPtr info_iref_ptr) var_iref ] [ ]
        ],
        -- Wake THC export call
        C.Ex $ C.Call "thc_sem_v" [ C.AddressOf $ C.DerefField (C.Variable "info") "export_cb_done_sem"]
      ]

-- static errval_t ping_pong_thc_export_connect_cb(void *st,
--                                                struct ping_pong_binding *b) {
--  struct ping_pong_thc_export_info *info;
--  info = (struct ping_pong_thc_export_info*) st;
--
--  // Wait for top-half accept call to be present
--  thc_sem_p(&info->accept_call_present_sem);
--
--  // Transfer information to top-half
--  thc_lock_acquire(&info->info_lock);
--  *(info->b) = b;
--
--  // Signal that information has arrived
--  thc_sem_v(&info->connect_cb_done_sem);
--  return SYS_ERR_OK;
-- }

connect_cb_function :: String -> C.Unit
connect_cb_function ifn =
   let info_ptr_t = C.Ptr $ THC.thc_export_info_t ifn
       info_b = C.DerefField (C.Variable "info") "b" 
       ptr_info_accept_call_present_sem = C.AddressOf $ C.DerefField (C.Variable "info") "accept_call_present_sem" 
       ptr_info_connect_cb_done_sem = C.AddressOf $ C.DerefField (C.Variable "info") "connect_cb_done_sem" 
       ptr_info_info_lock = C.AddressOf $ C.DerefField (C.Variable "info") "info_lock" 
       var_st = C.Variable "st"
       var_b = C.Variable "b"
   in
    C.FunctionDef C.Static (C.TypeName "errval_t") (ifscope ifn "thc_connect_cb")
      [ C.Param (C.Ptr $ C.TypeName "void") "st",
        C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type ifn) "b" ]
      [
        C.VarDecl C.NoScope C.NonConst info_ptr_t "info" Nothing,
        C.Ex $ C.Assignment (C.Variable "info") (C.Cast info_ptr_t var_st),
        C.Ex $ C.Call "thc_sem_p" [ptr_info_accept_call_present_sem],
        C.Ex $ C.Call "thc_lock_acquire" [ptr_info_info_lock],
        C.Ex $ C.Assignment (C.DerefPtr info_b) var_b,
        C.Ex $ C.Call "thc_sem_v" [ptr_info_connect_cb_done_sem],
        C.Return $ C.Variable "SYS_ERR_OK"
      ]

-- errval_t ping_pong_thc_export(struct ping_pong_thc_export_info *info,
--                                      const char *service_name,
--                                      struct waitset *ws,
--                                      idc_export_flags_t flags,
--                                      iref_t iref_ptr) {
--   errval_t err;
-- 
--   thc_sem_init(&info->export_cb_done_sem, 0);
--   thc_sem_init(&info->connect_cb_done_sem, 0);
--   thc_sem_init(&info->accept_call_present_sem, 0);
--   thc_lock_init(&info->info_lock);
--   thc_lock_init(&info->next_accept_lock);
--   info->service_name = service_name;
--   info->err = SYS_ERR_OK;
--   info->iref_ptr = iref_ptr;
--   err = ping_pong_export(info,
--                          ping_pong_thc_export_export_cb,
--                          ping_pong_thc_export_connect_cb,
--                          ws,
--                          flags);
--   if (err_is_ok(err)) {
--     thc_sem_p(&info->export_cb_done_sem);
--     err = info->err;
--     thc_lock_release(&info->info_lock);
--   }
-- 
--   return err;
-- }

export_function :: String -> C.Unit
export_function ifn =
   let info_ptr_t = C.Ptr $ THC.thc_export_info_t ifn
       info_service_name = C.DerefField (C.Variable "info") "service_name" 
       info_err = C.DerefField (C.Variable "info") "err" 
       info_iref_ptr = C.DerefField (C.Variable "info") "iref_ptr" 
       ptr_info_export_cb_done_sem = C.AddressOf $ C.DerefField (C.Variable "info") "export_cb_done_sem" 
       ptr_info_connect_cb_done_sem = C.AddressOf $ C.DerefField (C.Variable "info") "connect_cb_done_sem" 
       ptr_info_accept_call_present_sem = C.AddressOf $ C.DerefField (C.Variable "info") "accept_call_present_sem" 
       ptr_info_info_lock = C.AddressOf $ C.DerefField (C.Variable "info") "info_lock" 
       ptr_info_next_accept_lock = C.AddressOf $ C.DerefField (C.Variable "info") "next_accept_lock" 
       var_err = C.Variable "err"
       var_info = C.Variable "info"
       var_ws = C.Variable "ws"
       var_flags = C.Variable "flags"
       var_service_name = C.Variable "service_name"
       var_iref_ptr = C.Variable "iref_ptr"
   in
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (THC.thc_export_fn_name ifn)
      [ C.Param (C.Ptr $ THC.thc_export_info_t ifn) "info",
        C.Param (C.ConstT $ C.Ptr $ C.TypeName "char") "service_name",
        C.Param (C.Ptr $ C.Struct "waitset") "ws",
        C.Param (C.TypeName "idc_export_flags_t") "flags",
        C.Param (C.Ptr $ C.TypeName "iref_t") "iref_ptr" ]
      [
        C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "err" Nothing,
        C.Ex $ C.Call "thc_sem_init" [ptr_info_export_cb_done_sem, C.NumConstant 0],
        C.Ex $ C.Call "thc_sem_init" [ptr_info_connect_cb_done_sem, C.NumConstant 0],
        C.Ex $ C.Call "thc_sem_init" [ptr_info_accept_call_present_sem, C.NumConstant 0],
        C.Ex $ C.Call "thc_lock_init" [ptr_info_info_lock],
        C.Ex $ C.Call "thc_lock_init" [ptr_info_next_accept_lock],
        C.Ex $ C.Assignment info_service_name var_service_name,
        C.Ex $ C.Assignment info_err (C.Variable "SYS_ERR_OK"),
        C.Ex $ C.Assignment info_iref_ptr var_iref_ptr,
        C.Ex $ C.Assignment var_err (C.Call (ifn ++ "_export") 
                 [ var_info,
                   C.Variable $ ifscope ifn "thc_export_cb",
                   C.Variable $ ifscope ifn "thc_connect_cb",
                   var_ws,
                   var_flags ]),
        C.If ( C.Call "err_is_ok" [ var_err ])
        -- No error on export, wait for callback to finish
        [ C.Ex $ C.Call "thc_sem_p" [ ptr_info_export_cb_done_sem ],
          C.Ex $ C.Assignment var_err info_err,
          C.Ex $ C.Call "thc_lock_release" [ ptr_info_info_lock ]
        ]
        -- Error on export
        [ ],
        C.Return var_err
      ]


-- errval_t ping_pong_thc_accept(struct ping_pong_thc_export_info *info,
--                                      struct ping_pong_binding **b) {
--   struct ping_pong_binding *priv_b;
-- 
--   // Wait to be the next accepter
--   thc_lock_acquire(&info->next_accept_lock);
--   info->b = &priv_b;
-- 
--   // Signal to the bottom half that we are present
--   thc_sem_v(&info->accept_call_present_sem);
-- 
--   // Wait for the bottom half to fill in the results
--   thc_sem_p(&info->connect_cb_done_sem);
--   errval_t err = info->err;
--   thc_lock_release(&info->info_lock);
--   thc_lock_release(&info->next_accept_lock);
-- 
--   if (err_is_ok(err)) {
--     if (b != NULL) {
--       *b = priv_b;
--     }
--   }
-- 
--   return err;
-- }
-- 

accept_function :: String -> C.Unit
accept_function ifn =
   let info_service_name = C.DerefField (C.Variable "info") "service_name" 
       info_err = C.DerefField (C.Variable "info") "err" 
       info_b = C.DerefField (C.Variable "info") "b" 
       ptr_info_export_cb_done_sem = C.AddressOf $ C.DerefField (C.Variable "info") "export_cb_done_sem" 
       ptr_info_connect_cb_done_sem = C.AddressOf $ C.DerefField (C.Variable "info") "connect_cb_done_sem" 
       ptr_info_accept_call_present_sem = C.AddressOf $ C.DerefField (C.Variable "info") "accept_call_present_sem" 
       ptr_info_info_lock = C.AddressOf $ C.DerefField (C.Variable "info") "info_lock" 
       ptr_info_next_accept_lock = C.AddressOf $ C.DerefField (C.Variable "info") "next_accept_lock" 
       var_priv_b = C.Variable "priv_b"
       var_err = C.Variable "err"
       var_b = C.Variable "b"
       var_sv = C.Variable "sv"
       var_info = C.Variable "info"
       var_ws = C.Variable "ws"
       var_flags = C.Variable "flags"
       var_service_name = C.Variable "service_name"
   in
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (THC.thc_accept_fn_name ifn)
      [ C.Param (C.Ptr $ THC.thc_export_info_t ifn) "info",
        C.Param (C.Ptr $ C.Ptr $ C.Struct $ BC.intf_bind_type ifn) "b" ]
      [
        C.VarDecl C.NoScope C.NonConst (C.Ptr $ C.Struct $ BC.intf_bind_type ifn) "priv_b" Nothing,
        -- Wait to be the next accepter
        C.Ex $ C.Call "thc_lock_acquire" [ ptr_info_next_accept_lock ],
        C.Ex $ C.Assignment info_b $ C.AddressOf var_priv_b,
        -- Signal to the bottom half that we are present
        C.Ex $ C.Call "thc_sem_v" [ ptr_info_accept_call_present_sem ],
        -- Wait for the bottom half to fill in the results
        C.Ex $ C.Call "thc_sem_p" [ ptr_info_connect_cb_done_sem ],
        C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "err" (Just info_err),
        C.Ex $ C.Call "thc_lock_release" [ ptr_info_info_lock ],
        C.Ex $ C.Call "thc_lock_release" [ ptr_info_next_accept_lock ],
        -- If we're OK so far...
        C.If ( C.Call "err_is_ok" [ var_err ])
        [
          -- Return "b" if requested
          C.If ( C.Binary C.NotEquals var_b (C.Variable "NULL"))
          [ C.Ex $ C.Assignment (C.DerefPtr var_b) var_priv_b
          ] [ ]
        ] [ ],
        -- Done
        C.Return var_err
      ]

-- static void ping_pong_thc_bind_cb(void *st,
--                                   errval_t err,
--                                   struct ping_pong_binding *b) {
--   struct ping_pong_thc_connect_info *info;
--   info = (struct ping_pong_thc_connect_info *) st;
--   info->err = err;
--   if (err_is_ok(err)) {
--     info->b = b;
--   }
--   thc_sem_v(&info->bind_cb_done_sem);
-- }

bind_cb_function :: String -> C.Unit
bind_cb_function ifn =
   let info_ptr_t = C.Ptr $ THC.thc_connect_info_t ifn
       info_err = C.DerefField (C.Variable "info") "err" 
       info_b = C.DerefField (C.Variable "info") "b" 
       ptr_info_bind_cb_done_sem = C.AddressOf $ C.DerefField (C.Variable "info") "bind_cb_done_sem" 
       var_st = C.Variable "st"
       var_err = C.Variable "err"
       var_b = C.Variable "b"
   in
    C.FunctionDef C.Static (C.TypeName "void") (ifscope ifn "thc_bind_cb")
      [ C.Param (C.Ptr $ C.TypeName "void") "st",
        C.Param (C.TypeName "errval_t") "err",
        C.Param (C.Ptr $ C.Struct $ BC.intf_bind_type ifn) "b" ]
      [
        C.VarDecl C.NoScope C.NonConst info_ptr_t "info" Nothing,
        C.Ex $ C.Assignment (C.Variable "info") (C.Cast info_ptr_t var_st),
        C.Ex $ C.Assignment info_err var_err,
        C.If (C.Call "err_is_ok" [ var_err ])
        [ -- No error passed to us
          C.Ex $ C.Assignment info_b var_b
        ] [],
        C.Ex $ C.Call "thc_sem_v" [ ptr_info_bind_cb_done_sem ]
      ]

-- static errval_t ping_pong_thc_bind(const char *service_name,
--                                    struct waitset *ws,
--                                    int flags,
--                                    struct ping_pong_binding **b) {
--   struct ping_pong_thc_connect_info info;
--   errval_t err;
--   iref_t iref;
--   thc_sem_init(&info.bind_cb_done_sem, 0);
--   info.err = SYS_ERR_OK;
--   info.b = NULL;
--   err = nameservice_blocking_lookup(service_name, &iref);
--   if (err_is_ok(err)) {
--     err = ping_pong_bind(iref, 
--                          ping_pong_thc_bind_cb,
--                          &info,
--                          ws,
--                          flags);
--     if (err_is_ok(err)) {
--       thc_sem_p(&info.bind_cb_done_sem);
--       err = info.err;
--       if (err_is_ok(err)) {
--         if (b != NULL) {
--           *b = info.b;
--         }
--       }
--     }
--   }
--   return err;
-- }

connect_by_name_function :: String -> C.Unit
connect_by_name_function ifn =
   let var_err = C.Variable "err"
       var_service_name = C.Variable "service_name"
       var_ws = C.Variable "ws"
       var_b = C.Variable "b"
       var_flags = C.Variable "flags"
       var_iref = C.Variable "iref"
       ptr_iref = C.AddressOf var_iref
   in
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (THC.thc_connect_by_name_fn_name ifn)
      [ C.Param (C.ConstT $ C.Ptr $ C.TypeName "char") "service_name",
        C.Param (C.Ptr $ C.Struct "waitset") "ws",
        C.Param (C.TypeName "idc_bind_flags_t") "flags",
        C.Param (C.Ptr $ C.Ptr $ C.Struct $ BC.intf_bind_type ifn) "b" ]
      [
        C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "err" Nothing,
        C.VarDecl C.NoScope C.NonConst (C.TypeName "iref_t") "iref" Nothing,
        -- Name service lookup
        C.Ex $ C.Assignment var_err
          (C.Call "nameservice_blocking_lookup" 
             [ var_service_name, ptr_iref ]),
        C.If (C.Call "err_is_ok" [ var_err ] )
          [ -- Name service lookup OK
            C.Ex $ C.Assignment var_err
              (C.Call (THC.thc_connect_fn_name ifn)
               [ var_iref, var_ws, var_flags, var_b ])
          ] [ ],
        C.Return var_err ]

connect_function :: String -> C.Unit
connect_function ifn =
   let var_err = C.Variable "err"
       var_service_name = C.Variable "service_name"
       var_ws = C.Variable "ws"
       var_b = C.Variable "b"
       var_flags = C.Variable "flags"
       var_iref = C.Variable "iref"
       var_info = C.Variable "info"
       var_cl = C.Variable "cl"
       ptr_info_bind_cb_done_sem = C.AddressOf $ C.FieldOf (C.Variable "info") "bind_cb_done_sem" 
       ptr_info = C.AddressOf var_info
       info_err = C.FieldOf (C.Variable "info") "err" 
       info_b = C.FieldOf (C.Variable "info") "b" 
   in
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (THC.thc_connect_fn_name ifn)
      [ C.Param (C.TypeName "iref_t") "iref",
        C.Param (C.Ptr $ C.Struct "waitset") "ws",
        C.Param (C.TypeName "idc_bind_flags_t") "flags",
        C.Param (C.Ptr $ C.Ptr $ C.Struct $ BC.intf_bind_type ifn) "b" ]
      [
        C.VarDecl C.NoScope C.NonConst (THC.thc_connect_info_t ifn) "info" Nothing,
        C.VarDecl C.NoScope C.NonConst (C.TypeName "errval_t") "err" Nothing,
        C.Ex $ C.Call "thc_sem_init" [ ptr_info_bind_cb_done_sem,
                                       (C.NumConstant 0) ],
        C.Ex $ C.Assignment info_err (C.Variable "SYS_ERR_OK"),
        C.Ex $ C.Assignment info_b (C.Variable "NULL"),
        C.Ex $ C.Assignment var_err
          (C.Call (ifn ++ "_bind")
           [ var_iref, (C.Variable $ ifscope ifn "thc_bind_cb"), ptr_info, var_ws, var_flags ]),
        C.If (C.Call "err_is_ok" [ var_err ])
          [ -- Bind call OK
            C.Ex $ C.Call "thc_sem_p" [ptr_info_bind_cb_done_sem],
            C.Ex $ C.Assignment var_err info_err,
            C.If (C.Call "err_is_ok" [ var_err ])
            [ -- Bind callback OK
              -- Return "b" if requested
              C.If ( C.Binary C.NotEquals var_b (C.Variable "NULL"))
                [ C.Ex $ C.Assignment (C.DerefPtr var_b) info_b ] [ ]
            ] [ ]
          ] [ ],
    C.Return var_err ]
