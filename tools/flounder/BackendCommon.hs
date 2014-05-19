{- 
   BackendCommon: Common code used by most backends
   
  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module BackendCommon where

import qualified CAbsSyntax as C
import Syntax

data Direction = TX | RX
    deriving (Show, Eq)

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

-- Scope a list of strings
ifscope :: String -> String -> String
--ifscope ifn s = ifn ++ "$" ++ s
ifscope ifn s = ifn ++ "_" ++ s

idscope :: String -> String -> String -> String
idscope ifn s suffix  = ifscope ifn (s ++ "__" ++ suffix)

drvscope :: String -> String -> String -> String
drvscope drv ifn s = ifscope ifn (drv ++ "_" ++ s)

-- Name of the binding struct for an interface type
intf_bind_type :: String -> String
intf_bind_type ifn = ifscope ifn "binding"

-- Variable used to refer to a binding
intf_bind_var = "_binding"

-- Name of the bind continuation function type for an interface type
intf_bind_cont_type :: String -> String
intf_bind_cont_type ifn = ifscope ifn "bind_continuation_fn"

-- Variable used to refer to a continuation
intf_cont_var = "_continuation"

-- name of the export state struct
export_type n = ifscope n "export"

-- Name of the enumeration of message numbers
msg_enum_name :: String -> String
msg_enum_name ifn = ifscope ifn "msg_enum"

-- Name of each element of the message number enumeration
msg_enum_elem_name :: String -> String -> String
msg_enum_elem_name ifn mn = idscope ifn mn "msgnum" 

-- Name of the type of a message function
msg_sig_type :: String -> MessageDef -> Direction -> String
msg_sig_type ifn m@(RPC _ _ _) _ = idscope ifn (msg_name m) "rpc_method_fn"
msg_sig_type ifn m TX = idscope ifn (msg_name m) "tx_method_fn"
msg_sig_type ifn m RX =  idscope ifn (msg_name m) "rx_method_fn"

-- Name of a given message definition
msg_name :: MessageDef -> String
msg_name (Message _ n _ _) = n
msg_name (RPC n _ _) = n

-- Name of the static inline wrapper for sending messages
tx_wrapper_name :: String -> String -> String
tx_wrapper_name ifn mn = idscope ifn mn "tx"

-- Names of the underlying messages that are constructed from an RPC
rpc_call_name n = n ++ "_call"
rpc_resp_name n = n ++ "_response"

-- Name of the struct holding message args for SAR
msg_argstruct_name :: String -> String -> String
msg_argstruct_name ifn n = idscope ifn n "args"

-- Name of the union type holding all the arguments for a message
binding_arg_union_type :: String -> String
binding_arg_union_type ifn = ifscope ifn "arg_union"

-- Name of the C type for a concrete flounder type, struct, or enum
type_c_struct, type_c_enum :: String -> String -> String
type_c_struct ifn n = "_" ++ idscope ifn n "struct"
type_c_enum ifn e = ifscope ifn e

type_c_name :: String -> TypeRef -> String
type_c_name ifn (Builtin Cap) = undefined
type_c_name ifn (Builtin GiveAwayCap) = undefined
type_c_name ifn (Builtin String) = undefined
type_c_name ifn (Builtin t) = (show t) ++ "_t"
type_c_name ifn (TypeVar t) = type_c_name1 ifn t
type_c_name ifn (TypeAlias t _) = type_c_name1 ifn t

type_c_name1 :: String -> String -> String
type_c_name1 ifn tn = (ifscope ifn tn) ++ "_t"

type_c_type :: String -> TypeRef -> C.TypeSpec
type_c_type ifn (Builtin Cap) = C.Struct "capref"
type_c_type ifn (Builtin GiveAwayCap) = C.Struct "capref"
type_c_type ifn (Builtin Char) = C.TypeName "char"
type_c_type ifn (Builtin Bool) = C.TypeName "bool"
type_c_type ifn (Builtin String) = C.Ptr $ C.TypeName "char"
type_c_type ifn t = C.TypeName $ type_c_name ifn t

-- TX pointers should be const
type_c_type_dir :: Direction -> String -> TypeRef -> C.TypeSpec
type_c_type_dir TX ifn tr = case type_c_type ifn tr of
    C.Ptr t -> C.Ptr $ C.ConstT t
    t -> t
type_c_type_dir RX ifn tr = type_c_type ifn tr

-- Array types in the msg args struct should only be pointers to the storage
type_c_type_msgstruct :: String -> [TypeDef] -> TypeRef -> C.TypeSpec
type_c_type_msgstruct ifn typedefs t
    = case lookup_typeref typedefs t of
        TArray tr n _ -> C.Ptr $ type_c_type ifn t
        _ -> type_c_type ifn t

-- Name of the struct type for the method vtable
intf_vtbl_type :: String -> Direction -> String
intf_vtbl_type ifn TX = ifscope ifn "tx_vtbl"
intf_vtbl_type ifn RX = ifscope ifn "rx_vtbl"

connect_callback_name n = ifscope n "connect_fn" 
drv_connect_handler_name drv n = drvscope drv n "connect_handler"
can_send_fn_name drv n = drvscope drv n "can_send"
register_send_fn_name drv n = drvscope drv n "register_send"
default_error_handler_fn_name drv n = drvscope drv n "default_error_handler"
generic_control_fn_name drv n = drvscope drv n "control"

can_send_fn_type ifn = ifscope ifn "can_send_fn"
register_send_fn_type ifn = ifscope ifn "register_send_fn"
change_waitset_fn_type ifn = ifscope ifn "change_waitset_fn"
control_fn_type ifn = ifscope ifn "control_fn"
error_handler_fn_type ifn = ifscope ifn "error_handler_fn"


------------------------------------------------------------------------
-- Code shared by backend implementations
------------------------------------------------------------------------

intf_preamble :: String -> String -> Maybe String -> C.Unit
intf_preamble infile name descr = 
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

--
-- Convert each RPC definition to a pair of underlying call/response messages
--
rpcs_to_msgs :: [MessageDef] -> [MessageDef]
rpcs_to_msgs ml = concat $ map rpc_to_msgs ml

rpc_to_msgs :: MessageDef -> [MessageDef]
rpc_to_msgs (RPC n rpcargs bckargs) = [Message MCall (rpc_call_name n) inargs bckargs,
                                       Message MResponse (rpc_resp_name n) outargs bckargs]
    where
        (inargs, outargs) = partition_rpc_args rpcargs
rpc_to_msgs m = [m]


-- partition a list of RPC arguments to lists of input and output arguments
partition_rpc_args :: [RPCArgument] -> ([MessageArgument], [MessageArgument])
partition_rpc_args [] = ([], [])
partition_rpc_args (first:rest) = case first of
    RPCArgIn t v -> ((Arg t v):restin, restout)
    RPCArgOut t v -> (restin, (Arg t v):restout)
    where
        (restin, restout) = partition_rpc_args rest

msg_argdecl :: Direction -> String -> MessageArgument -> [C.Param]
msg_argdecl dir ifn (Arg tr (Name n)) = 
    [ C.Param (type_c_type_dir dir ifn tr) n ]
msg_argdecl RX ifn (Arg tr (DynamicArray n l)) = 
    [ C.Param (C.Ptr $ type_c_type_dir RX ifn tr) n, 
      C.Param (type_c_type_dir RX ifn size) l ]
msg_argdecl TX ifn (Arg tr (DynamicArray n l)) = 
    [ C.Param (C.Ptr $ C.ConstT $ type_c_type_dir TX ifn tr) n, 
      C.Param (type_c_type_dir TX ifn size) l ]

msg_argstructdecl :: String -> [TypeDef] -> MessageArgument -> [C.Param]
msg_argstructdecl ifn typedefs (Arg tr (Name n)) = 
    [ C.Param (type_c_type_msgstruct ifn typedefs tr) n ]
msg_argstructdecl ifn typedefs a = msg_argdecl RX ifn a

rpc_argdecl :: String -> RPCArgument -> [C.Param]
rpc_argdecl ifn (RPCArgIn tr v) = msg_argdecl TX ifn (Arg tr v)
rpc_argdecl ifn (RPCArgOut tr (Name n)) = [ C.Param (C.Ptr $ type_c_type ifn tr) n ]
rpc_argdecl ifn (RPCArgOut tr (DynamicArray n l)) = 
    [ C.Param (C.Ptr $ C.Ptr $ type_c_type ifn tr) n, 
      C.Param (C.Ptr $ type_c_type ifn size) l ]

-- XXX: kludge wrapper to pass array types by reference in RPC
rpc_argdecl2 :: String -> [TypeDef] -> RPCArgument -> [C.Param]
rpc_argdecl2 ifn typedefs arg@(RPCArgOut tr (Name n))
    = case lookup_typeref typedefs tr of
      TArray _ _ _ -> [ C.Param (C.Ptr $ C.Ptr $ type_c_type ifn tr) n ]
      _ -> rpc_argdecl ifn arg
rpc_argdecl2 ifn _ arg = rpc_argdecl ifn arg

-- binding parameter for a function
binding_param ifname = C.Param (C.Ptr $ C.Struct $ intf_bind_type ifname) intf_bind_var


--
-- Generate the code to initialise/destroy a binding structure instance
--
binding_struct_init :: String -> String -> C.Expr -> C.Expr ->  C.Expr -> [C.Stmt]
binding_struct_init drv ifn binding_var waitset_ex tx_vtbl_ex = [
    C.Ex $ C.Assignment (C.FieldOf binding_var "st") (C.Variable "NULL"),
    C.Ex $ C.Assignment (C.FieldOf binding_var "waitset") waitset_ex,
    C.Ex $ C.Call "event_mutex_init" [C.AddressOf $ C.FieldOf binding_var "mutex", waitset_ex],
    C.Ex $ C.Assignment (C.FieldOf binding_var "can_send")
                                (C.Variable $ can_send_fn_name drv ifn),
    C.Ex $ C.Assignment (C.FieldOf binding_var "register_send")
                                (C.Variable $ register_send_fn_name drv ifn),
    C.Ex $ C.Assignment (C.FieldOf binding_var "error_handler")
                                (C.Variable $ default_error_handler_fn_name drv ifn),
    C.Ex $ C.Assignment (C.FieldOf binding_var "tx_vtbl") tx_vtbl_ex,
    C.Ex $ C.Call "memset" [C.AddressOf $ C.FieldOf binding_var "rx_vtbl",
                            C.NumConstant 0,
                            C.Call "sizeof" [C.FieldOf binding_var "rx_vtbl"]],
    C.Ex $ C.Call "flounder_support_waitset_chanstate_init"
            [C.AddressOf $ C.FieldOf binding_var "register_chanstate"],
    C.Ex $ C.Call "flounder_support_waitset_chanstate_init"
            [C.AddressOf $ C.FieldOf binding_var "tx_cont_chanstate"],
    C.StmtList
        [C.Ex $ C.Assignment (C.FieldOf binding_var f) (C.NumConstant 0)
         | f <- ["tx_msgnum", "rx_msgnum", "tx_msg_fragment", "rx_msg_fragment",
                 "tx_str_pos", "rx_str_pos", "tx_str_len", "rx_str_len"]],
    C.Ex $ C.Assignment (C.FieldOf binding_var "bind_cont") (C.Variable "NULL")]

binding_struct_destroy :: String -> C.Expr -> [C.Stmt]
binding_struct_destroy ifn binding_var
    = [C.Ex $ C.Call "flounder_support_waitset_chanstate_destroy"
            [C.AddressOf $ C.FieldOf binding_var "register_chanstate"],
       C.Ex $ C.Call "flounder_support_waitset_chanstate_destroy"
            [C.AddressOf $ C.FieldOf binding_var "tx_cont_chanstate"]]

--
-- Generate a generic can_send function
--
can_send_fn_def :: String -> String -> C.Unit
can_send_fn_def drv ifn = 
    C.FunctionDef C.Static (C.TypeName "bool") (can_send_fn_name drv ifn) params [
        C.Return $ C.Binary C.Equals (bindvar `C.DerefField` "tx_msgnum") (C.NumConstant 0)
    ]
    where
        params = [ C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) "b" ]
        bindvar = C.Variable "b"

--
-- generate a generic register_send function
--
register_send_fn_def :: String -> String -> C.Unit
register_send_fn_def drv ifn =
    C.FunctionDef C.Static (C.TypeName "errval_t") (register_send_fn_name drv ifn) params [
        C.Return $ C.Call "flounder_support_register"
            [C.Variable "ws",
             C.AddressOf $ bindvar `C.DerefField` "register_chanstate",
             C.Variable intf_cont_var,
             C.Call (can_send_fn_name drv ifn) [bindvar]]
    ]
    where
        params = [ C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) "b",
                   C.Param (C.Ptr $ C.Struct "waitset") "ws",
                   C.Param (C.Struct "event_closure") intf_cont_var ]
        bindvar = C.Variable "b"

--
-- generate a default error handler (which the user should replace!)
--
default_error_handler_fn_def :: String -> String -> C.Unit
default_error_handler_fn_def drv ifn =
    C.FunctionDef C.Static C.Void (default_error_handler_fn_name drv ifn) params [
        C.Ex $ C.Call "DEBUG_ERR"
            [errvar, C.StringConstant $
             "asynchronous error in Flounder-generated " ++
             ifn ++ " " ++ drv ++ " binding (default handler)" ],
        C.Ex $ C.Call "abort" []
    ]
    where
        params = [ C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) "b",
                   C.Param (C.TypeName "errval_t") "err" ]

--
-- generate a generic control function that does nothing
--
generic_control_fn_def :: String -> String -> C.Unit
generic_control_fn_def drv ifn = 
    C.FunctionDef C.Static (C.TypeName "errval_t") (generic_control_fn_name drv ifn) params [
        C.SComment "no control flags are supported",
        C.Return $ C.Variable "SYS_ERR_OK"
    ]
    where
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
                  C.Param (C.TypeName "idc_control_t") "control"]

-- register a transmit continuation
register_txcont :: C.Expr -> [C.Stmt]
register_txcont cont_ex = [
    C.If (C.Binary C.NotEquals (cont_ex `C.FieldOf` "handler") (C.Variable "NULL"))
        [localvar (C.TypeName "errval_t") "_err" Nothing,
         C.Ex $ C.Assignment errvar $ C.Call "flounder_support_register"
            [bindvar `C.DerefField` "waitset",
             C.AddressOf $ bindvar `C.DerefField` "tx_cont_chanstate",
             cont_ex,
             C.Variable "false"],
         C.SComment "may fail if previous continuation hasn't fired yet",
         C.If (C.Call "err_is_fail" [errvar])
            [C.If (C.Binary C.Equals (C.Call "err_no" [errvar])
                                     (C.Variable "LIB_ERR_CHAN_ALREADY_REGISTERED"))
                [C.Return $ C.Variable "FLOUNDER_ERR_TX_BUSY"]
                [C.Ex $ C.Call "assert" [C.Unary C.Not $ C.StringConstant "shouldn't happen"],
                 C.Return $ errvar] ] []
         ] []
    ] where
        errvar = C.Variable "_err"

-- starting a send: just a debug hook
start_send :: String -> String -> String -> [MessageArgument] -> [C.Stmt]
start_send drvn ifn mn msgargs
    = [C.Ex $ C.Call "FL_DEBUG" [C.StringConstant $
                                 drvn ++ " TX " ++ ifn ++ "." ++ mn ++ "\n"]]

-- finished a send: clear msgnum, trigger pending waitsets/events
finished_send :: [C.Stmt]
finished_send = [
    C.Ex $ C.Assignment tx_msgnum_field (C.NumConstant 0)] ++
    [C.Ex $ C.Call "flounder_support_trigger_chan" [wsaddr ws]
    | ws <- ["tx_cont_chanstate", "register_chanstate"]]
    where
        tx_msgnum_field = C.DerefField bindvar "tx_msgnum"
        wsaddr ws = C.AddressOf $ bindvar `C.DerefField` ws

-- start receiving: allocate space for any static arrays in message
start_recv :: String -> String -> [TypeDef] -> String -> [MessageArgument] -> [C.Stmt]
start_recv drvn ifn typedefs mn msgargs
  = concat [
    [C.Ex $ C.Assignment (field fn)
          $ C.Call "malloc" [C.SizeOfT $ type_c_type ifn tr],
     C.Ex $ C.Call "assert" [C.Binary C.NotEquals (field fn) (C.Variable "NULL")]
    ] | Arg tr (Name fn) <- msgargs, is_array tr]
    
    where
      field fn = rx_union_elem mn fn
      is_array tr = case lookup_typeref typedefs tr of
        TArray _ _ _ -> True
        _ -> False

-- finished recv: debug, run handler and clean up
finished_recv :: String -> String -> [TypeDef] -> String -> [MessageArgument] -> [C.Stmt]
finished_recv drvn ifn typedefs mn msgargs
    = [C.Ex $ C.Call "FL_DEBUG" [C.StringConstant $
                                 drvn ++ " RX " ++ ifn ++ "." ++ mn ++ "\n"],
       C.Ex $ C.Call "assert" [C.Binary C.NotEquals handler (C.Variable "NULL")],
       C.Ex $ C.CallInd handler (bindvar:args),
       C.Ex $ C.Assignment rx_msgnum_field (C.NumConstant 0)]
    where
        rx_msgnum_field = C.DerefField bindvar "rx_msgnum"
        handler = C.DerefField bindvar "rx_vtbl" `C.FieldOf` mn
        args = concat [mkargs tr a | Arg tr a <- msgargs]
        mkargs tr (Name n) = case lookup_typeref typedefs tr of
          TArray _ _ _ -> [C.DerefPtr $ rx_union_elem mn n]
          _ -> [rx_union_elem mn n]
        mkargs _ (DynamicArray n l) = [rx_union_elem mn n, rx_union_elem mn l]

tx_arg_assignment :: String -> [TypeDef] -> String -> MessageArgument -> C.Stmt
tx_arg_assignment ifn typedefs mn (Arg tr v) = case v of
    Name an -> C.Ex $ C.Assignment (tx_union_elem mn an) (srcarg an)
    DynamicArray an len -> C.StmtList [
        C.Ex $ C.Assignment (tx_union_elem mn an) (C.Cast (C.Ptr typespec) (C.Variable an)),
        C.Ex $ C.Assignment (tx_union_elem mn len) (C.Variable len)]
    where
        typespec = type_c_type ifn tr
        srcarg an = 
          case lookup_typeref typedefs tr of
            -- XXX: I have no idea why GCC requires a cast for the array type
            TArray _ _ _ -> C.Cast (C.Ptr typespec) (C.Variable an)
            _             -> case typespec of
              -- we may need to cast away the const on a pointer
              C.Ptr _ -> C.Cast typespec (C.Variable an)
              _ -> C.Variable an


tx_union_elem :: String -> String -> C.Expr
tx_union_elem mn fn
   = bindvar `C.DerefField` "tx_union" `C.FieldOf` mn `C.FieldOf` fn

rx_union_elem :: String -> String -> C.Expr
rx_union_elem mn fn
   = bindvar `C.DerefField` "rx_union" `C.FieldOf` mn `C.FieldOf` fn

-- misc common bits of C
localvar = C.VarDecl C.NoScope C.NonConst
errvar = C.Variable "err"
bindvar = C.Variable intf_bind_var
report_user_err ex = C.Ex $ C.CallInd (C.DerefField bindvar "error_handler") [bindvar, ex]
report_user_tx_err ex = C.StmtList [
    report_user_err ex,
    C.Ex $ C.Assignment tx_msgnum_field (C.NumConstant 0),
    C.Ex $ C.Call "flounder_support_trigger_chan" [wsaddr "register_chanstate"],
    C.Ex $ C.Call "flounder_support_deregister_chan" [wsaddr "tx_cont_chanstate"]
    ] where
        tx_msgnum_field = C.DerefField bindvar "tx_msgnum"
        wsaddr ws = C.AddressOf $ bindvar `C.DerefField` ws
