{- 
   GHBackend: Flounder stub generator for generic header files
   
  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module GHBackend where

import Data.List
import Data.Char

import qualified CAbsSyntax as C
import Syntax
import qualified Backend
import BackendCommon

export_fn_name n = ifscope n "export" 
bind_fn_name n = ifscope n "bind"

------------------------------------------------------------------------
-- Language mapping: Create the generic header file for the interface
------------------------------------------------------------------------

compile :: String -> String -> Interface -> String
compile infile outfile interface = 
    unlines $ C.pp_unit $ intf_header_file infile interface

header_file :: String -> Interface -> [C.Unit] -> C.Unit
header_file infile interface@(Interface name _ _) body = 
    let sym = "__" ++ name ++ "_IF_H"
    in
      C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []

intf_header_file :: String -> Interface -> C.Unit
intf_header_file infile intf = 
    header_file infile intf (intf_header_body infile intf)

intf_header_body :: String -> Interface -> [C.Unit]
intf_header_body infile interface@(Interface name descr decls) = 
    let
        (types, messagedecls) = Backend.partitionTypesMessages decls
        messages = rpcs_to_msgs messagedecls
    in
      [ intf_preamble infile name descr,
        C.Blank,

        C.Include C.Standard "flounder/flounder.h",
        C.Blank,

        C.MultiComment [ "Concrete type definitions" ],
        C.UnitList $ define_types name types,
        C.Blank,

        C.MultiComment [ "Forward declaration of binding type" ],
        C.StructForwardDecl (intf_bind_type name),
        C.Blank,

        C.MultiComment [ "Contination (callback) and control function types" ],
        intf_bind_cont_fn name,
        can_send_fn_typedef name,
        register_send_fn_typedef name,
        change_waitset_fn_typedef name,
        control_fn_typedef name,
        error_handler_fn_typedef name,
        C.Blank,

        C.MultiComment [ "Enumeration for message numbers" ],
        msg_enums name messages,
        C.Blank,

        C.MultiComment [ "Message type signatures (transmit)" ],
        C.UnitList [ msg_signature TX name m | m <- messages ],
        C.Blank,

        C.MultiComment [ "Message type signatures (receive)" ],
        C.UnitList [ msg_signature RX name m | m <- messages ],
        C.Blank,

        C.MultiComment [ "Struct type for holding the args for each msg" ],
        C.UnitList [ msg_argstruct name types m | m <- messages ],
        C.Blank,

        C.MultiComment [ "Union type for all message arguments" ],
        intf_union name messages,
        C.Blank,

        C.MultiComment [ "VTable struct definition for the interface (transmit)" ],
        intf_vtbl name TX messages,
        C.Blank,

        C.MultiComment [ "VTable struct definition for the interface (receive)" ],
        intf_vtbl name RX messages,
        C.Blank,

        C.MultiComment [ "Incoming connect callback type" ],
        connect_callback_fn name,
        C.Blank,

        C.MultiComment [ "Export state struct" ],
        export_struct name,
        C.Blank,

        C.MultiComment [ "Export function" ],
        export_function name,
        C.Blank,
         
        C.MultiComment [ "The Binding structure" ],
        binding_struct name messages,
        C.Blank,

        C.MultiComment [ "Generic bind function" ],
        bind_function name,
        C.Blank,

        C.MultiComment [ "Send wrappers" ],
        C.UnitList [ tx_wrapper name m | m <- messages ],
        C.Blank,

        C.MultiComment [ "Backend-specific includes" ],
        C.UnitList $ backend_includes name,
         
        C.MultiComment [ "And we're done" ]
      ]

--
-- Generate an enumeration for each message type, to use as procedure numbers. 
--
msg_enums :: String -> [MessageDef] -> C.Unit
msg_enums ifname msgs
    = C.EnumDecl (msg_enum_name ifname)
        ([C.EnumItem (msg_enum_elem_name ifname "__dummy") (Just $ C.NumConstant 0)] ++
         [C.EnumItem (msg_enum_elem_name ifname (msg_name m)) (Just $ C.NumConstant i)
            | (m, i) <- zip msgs [1..]])

--
-- Generate type definitions for each message signature
--
msg_signature_generic :: Direction -> String -> [TypeDef] -> C.Param -> MessageDef -> C.Unit
msg_signature_generic dirn ifname typedefs firstparam m = case dirn of
    TX -> C.TypeDef (C.Function C.NoScope (C.TypeName "errval_t") params) name
    RX -> C.TypeDef (C.Function C.NoScope (C.TypeName "void") params) name
  where
    name = msg_sig_type ifname m dirn
    continuation = C.Param (C.Struct "event_closure") intf_cont_var
    -- need a continuation only for non-RPC TX
    opt_continuation = case dirn of
        TX -> case m of
            RPC _ _ _ -> []
            otherwise -> [ continuation ]
        RX -> []
    params = [ firstparam ] ++ opt_continuation ++ concat payload
    payload = case m of
        Message _ _ args _ -> [ msg_argdecl dirn ifname a | a <- args ]
        RPC s args _       -> [ rpc_argdecl2 ifname typedefs a | a <- args ]

msg_signature :: Direction -> String -> MessageDef -> C.Unit
msg_signature dir ifn = msg_signature_generic dir ifn [] (binding_param ifn)


--
-- Generate a struct to hold the arguments of a message while it's being sent.
-- 
msg_argstruct :: String -> [TypeDef] -> MessageDef -> C.Unit
msg_argstruct ifname typedefs m@(RPC n args _) = 
    C.StructDecl (msg_argstruct_name ifname n) 
         (concat [ rpc_argdecl ifname a | a <- args ])
msg_argstruct ifname typedefs m@(Message _ n [] _) = C.NoOp
msg_argstruct ifname typedefs m@(Message _ n args _) =
    let tn = msg_argstruct_name ifname n
    in
      C.StructDecl tn (concat [ msg_argstructdecl ifname typedefs a
                               | a <- args ])

--
-- Generate a union of all the above
-- 
intf_union :: String -> [MessageDef] -> C.Unit
intf_union ifn msgs = 
    C.UnionDecl (binding_arg_union_type ifn)
         ([ C.Param (C.Struct $ msg_argstruct_name ifn n) n
            | m@(Message _ n a _) <- msgs, 0 /= length a ]
          ++
          [ C.Param (C.Struct $ msg_argstruct_name ifn n) n
            | m@(RPC n a _) <- msgs, 0 /= length a ]
         )

--
-- Generate a struct defn for a vtable for the interface
--
intf_vtbl :: String -> Direction -> [MessageDef] -> C.Unit
intf_vtbl n d ml = 
    C.StructDecl (intf_vtbl_type n d) [ intf_vtbl_param n m d | m <- ml ]

intf_vtbl_param :: String -> MessageDef -> Direction ->  C.Param
intf_vtbl_param ifn m d = C.Param (C.Ptr $ C.TypeName $ msg_sig_type ifn m d) (msg_name m)

--
-- Generate the binding structure
--
binding_struct :: String -> [MessageDef] -> C.Unit
binding_struct n ml = C.StructDecl (intf_bind_type n) fields
  where
    fields = [
        C.ParamComment "Arbitrary user state pointer",
        C.Param (C.Ptr C.Void) "st",
        C.ParamBlank,

        C.ParamComment "Waitset used for receive handlers and send continuations",
        C.Param (C.Ptr $ C.Struct "waitset") "waitset",
        C.ParamBlank,

        C.ParamComment "Mutex for the use of user code.",
        C.ParamComment "Must be held before any operation where there is a possibility of",
        C.ParamComment "concurrent access to the same binding (eg. multiple threads, or",
        C.ParamComment "asynchronous event handlers that use the same binding object).",
        C.Param (C.Struct "event_mutex") "mutex",
        C.ParamBlank,

        C.ParamComment "returns true iff a message could currently be accepted by the binding",
        C.Param (C.Ptr $ C.TypeName $ can_send_fn_type n) "can_send",
        C.ParamBlank,

        C.ParamComment "register an event for when a message is likely to be able to be sent",
        C.Param (C.Ptr $ C.TypeName $ register_send_fn_type n) "register_send",
        C.ParamBlank,

        C.ParamComment "change the waitset used by a binding",
        C.Param (C.Ptr $ C.TypeName $ change_waitset_fn_type n) "change_waitset",
        C.ParamBlank,

        C.ParamComment "perform control operations",
        C.Param (C.Ptr $ C.TypeName $ control_fn_type n) "control",
        C.ParamBlank,

        C.ParamComment "error handler for any async errors associated with this binding",
        C.ParamComment "must be filled-in by user",
        C.Param (C.Ptr $ C.TypeName $ error_handler_fn_type n) "error_handler",
        C.ParamBlank,

        C.ParamComment "Message send functions (filled in by binding)",
        C.Param (C.Struct $ intf_vtbl_type n TX) "tx_vtbl",
        C.ParamBlank,

        C.ParamComment "Incoming message handlers (filled in by user)",
        C.Param (C.Struct $ intf_vtbl_type n RX) "rx_vtbl",
        C.ParamBlank,

        C.ParamComment "Private state belonging to the binding implementation",
        C.Param (C.Union $ binding_arg_union_type n) "tx_union",
        C.Param (C.Union $ binding_arg_union_type n) "rx_union",
        C.Param (C.Struct "waitset_chanstate") "register_chanstate",
        C.Param (C.Struct "waitset_chanstate") "tx_cont_chanstate",
        C.Param (C.Enum $ msg_enum_name n) "tx_msgnum",
        C.Param (C.Enum $ msg_enum_name n) "rx_msgnum",
        C.Param (C.TypeName "int") "tx_msg_fragment",
        C.Param (C.TypeName "int") "rx_msg_fragment",
        C.Param (C.TypeName "size_t") "tx_str_pos",
        C.Param (C.TypeName "size_t") "rx_str_pos",
        C.Param (C.TypeName "size_t") "tx_str_len",
        C.Param (C.TypeName "size_t") "rx_str_len",
        C.Param (C.Struct "event_queue_node") "event_qnode",
        C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) "bind_cont"]

--
-- Generate prototypes for export. 
--

connect_callback_fn :: String -> C.Unit
connect_callback_fn n = 
    C.TypeDef 
         (C.Function C.NoScope (C.TypeName "errval_t") params) 
         (connect_callback_name n)
    where
          params = [ C.Param (C.Ptr $ C.TypeName "void") "st",
                     C.Param (C.Ptr $ C.Struct $ intf_bind_type n) "binding" ]

export_struct :: String -> C.Unit
export_struct n = C.StructDecl (export_type n) fields
  where
    fields = [
        C.Param (C.Struct "idc_export") "common",
        C.Param (C.Ptr $ C.TypeName $ connect_callback_name n) "connect_cb",
        C.Param (C.Ptr $ C.Struct "waitset") "waitset",
        C.Param (C.Ptr $ C.Void) "st"]

export_function :: String -> C.Unit
export_function n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope (C.TypeName "errval_t") params) name Nothing
    where 
      name = export_fn_name n
      params = [ C.Param (C.Ptr $ C.TypeName "void") "st",
                 C.Param (C.Ptr $ C.TypeName "idc_export_callback_fn") "export_cb",
                 C.Param (C.Ptr $ C.TypeName $ connect_callback_name n) "connect_cb",
                 C.Param (C.Ptr $ C.Struct "waitset") "ws",
                 C.Param (C.TypeName "idc_export_flags_t") "flags"]

intf_bind_cont_fn :: String -> C.Unit
intf_bind_cont_fn n = 
    C.TypeDef 
         (C.Function C.NoScope (C.TypeName "void") params) 
         (intf_bind_cont_type n)
    where
      params = [ C.Param (C.Ptr $ C.TypeName "void") "st",
                 C.Param (C.TypeName "errval_t") "err",
                 binding_param n ]

can_send_fn_typedef :: String -> C.Unit
can_send_fn_typedef n = 
    C.TypeDef 
         (C.Function C.NoScope (C.TypeName "bool") params) 
         (can_send_fn_type n)
    where
      params = [ binding_param n ]

register_send_fn_typedef :: String -> C.Unit
register_send_fn_typedef n = 
    C.TypeDef 
         (C.Function C.NoScope (C.TypeName "errval_t") params) 
         (register_send_fn_type n)
    where
      params = [ binding_param n,
                 C.Param (C.Ptr $ C.Struct "waitset") "ws",
                 C.Param (C.Struct "event_closure") intf_cont_var ]

change_waitset_fn_typedef :: String -> C.Unit
change_waitset_fn_typedef n = 
    C.TypeDef 
         (C.Function C.NoScope (C.TypeName "errval_t") params) 
         (change_waitset_fn_type n)
    where
      params = [ binding_param n,
                 C.Param (C.Ptr $ C.Struct "waitset") "ws" ]

control_fn_typedef :: String -> C.Unit
control_fn_typedef n = 
    C.TypeDef 
         (C.Function C.NoScope (C.TypeName "errval_t") params) 
         (control_fn_type n)
    where
      params = [ binding_param n,
                  C.Param (C.TypeName "idc_control_t") "control" ]

error_handler_fn_typedef :: String -> C.Unit
error_handler_fn_typedef n = 
    C.TypeDef 
         (C.Function C.NoScope C.Void params) 
         (error_handler_fn_type n)
    where
      params = [ binding_param n,
                 C.Param (C.TypeName "errval_t") "err" ]

bind_function :: String -> C.Unit
bind_function n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope (C.TypeName "errval_t") params) name Nothing
    where 
      name = bind_fn_name n
      params = [ C.Param (C.TypeName "iref_t") "i",
                 C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) intf_cont_var,
                 C.Param (C.Ptr $ C.TypeName "void") "st",
                 C.Param (C.Ptr $ C.Struct "waitset") "waitset",
                 C.Param (C.TypeName "idc_bind_flags_t") "flags" ]

--
-- Generate send function inline wrappers for each message signature
--

tx_wrapper :: String -> MessageDef -> C.Unit
tx_wrapper ifn (Message _ mn args _)
    = C.StaticInline (C.TypeName "errval_t") (tx_wrapper_name ifn mn)
        ([ binding_param ifn, continuation ] ++ concat payload_params)
        [ C.Return $ C.CallInd (bindvar `C.DerefField` "tx_vtbl" `C.FieldOf` mn)
                ([bindvar, C.Variable intf_cont_var] ++ payload_args) ]
  where
    continuation = C.Param (C.Struct "event_closure") intf_cont_var
    payload_params = [ msg_argdecl TX ifn a | a <- args ]
    payload_args = map C.Variable $ concat $ map mkargs args
    mkargs (Arg _ (Name an)) = [an]
    mkargs (Arg _ (DynamicArray an al)) = [an, al]

--
-- Include the right files for different backends
--

flounder_backends = [ "lmp", "ump", "ump_ipi", "multihop" ]

backend_includes :: String -> [ C.Unit ] 
backend_includes n = 
    [ backend_include n b | b <- flounder_backends ]

backend_include n b = 
    C.IfDef ("CONFIG_FLOUNDER_BACKEND_" ++ (map toUpper b))
    [ C.Include C.Standard ("if/" ++ n ++ "_" ++ b ++ "_defs.h") ]
     []

-----------------------------------------------------------------
-- Code to generate concrete type definitions
-----------------------------------------------------------------

define_types :: String -> [TypeDef] -> [C.Unit]
define_types interfaceName types = 
    [ define_type interfaceName t | t <- types ]

define_type :: String -> TypeDef -> C.Unit
define_type ifname (TAliasT newType originType) =
    C.TypeDef (type_c_type ifname $ Builtin originType) (type_c_name1 ifname newType)

{-
A typedef'd alias:
\begin{verbatim}
typedef uint32 alias_type;
\end{verbatim}

Should compile to:
\begin{verbatim}
typedef uint32_t ifname_alias_type_t;
\end{verbatim}
-}

define_type ifname (TAlias newType originType) = 
    C.TypeDef (type_c_type ifname originType) (type_c_name1 ifname newType)

{-
For @TArray@, we have to map the type @name@ to an array of @length@
elements of type @typeElts@. In C, this surprisingly corresponds to
the, correct, following code:

\begin{verbatim} 
typedef typeElts name[length]
\end{verbatim}

So, we will compile:
\begin{verbatim}
typedef uint32 array_type[30];
\end{verbatim}

To the following type definition:
\begin{verbatim}
typedef uint32_t ifname_array_type_t[30]
\end{verbatim}
-}

define_type ifname (TArray typeElts name length) = 
    C.TypeDef 
         (C.Array length $ type_c_type ifname typeElts)
         (type_c_name1 ifname name)


{-
The following structure:
\begin{verbatim}
typedef struct {
    uint32 int_field;
    alias_type alias_field;
} struct_type;
\end{verbatim}

Should be compiled down to:
\begin{verbatim}
typedef struct _ifname_struct_type_t {
    uint32_t    int_field;
    ifname_alias_type_t    alias_field;
} ifname_struct_type_t;
\end{verbatim}
-}

define_type ifname (TStruct name fields) = 
    let struct_name = type_c_struct ifname name
        type_name = type_c_name1 ifname name
    in
      C.UnitList [ 
            (C.StructDecl struct_name 
                  [ C.Param (type_c_type ifname ft) fn
                        | TStructField ft fn <- fields ]),
            C.TypeDef (C.Struct struct_name) type_name ]

{-
This enumeration:
\begin{verbatim}
typedef enum {
    foo, bar, baz
} some_enum;
\end{verbatim}

Generates the following code:
\begin{verbatim}
enum ifname_some_enum_t {
    ifname_some_enum_t_foo = 1,
    ifname_some_enum_t_bar = 2,
    ifname_some_enum_t_baz = 3,
}
\end{verbatim}
-}

define_type ifname (TEnum name elements) = 
    C.EnumDecl (type_c_name1 ifname name) 
         [ C.EnumItem (type_c_enum ifname e) Nothing | e <- elements ]
