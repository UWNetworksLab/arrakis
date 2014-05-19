{- 
   LMP.hs: Flounder stub generator for local message passing.

  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2011, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module LMP where

import Data.Bits

import qualified CAbsSyntax as C
import qualified Backend
import GHBackend
import MsgFragments
import Syntax
import Arch
import BackendCommon

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

drvname = "lmp"

-- Name of the binding struct
lmp_bind_type :: String -> String
lmp_bind_type ifn = ifscope ifn "lmp_binding"

-- Name of the local variable used for the LMP-specific binding type
lmp_bind_var_name :: String
lmp_bind_var_name = "b"
lmp_bind_var = C.Variable lmp_bind_var_name

-- Name of the bind function
lmp_bind_fn_name n = ifscope n "lmp_bind"

-- Name of the bind continuation function
lmp_bind_cont_fn_name n = ifscope n "lmp_bind_continuation"

-- Name of the init function
lmp_init_fn_name n = ifscope n "lmp_init"

-- Name of the destroy function
lmp_destroy_fn_name n = ifscope n "lmp_destroy"

-- Name of the transmit function
tx_fn_name ifn mn = idscope ifn mn "lmp_send"

-- Name of the transmit handler
tx_handler_name ifn mn = idscope ifn mn "lmp_send_handler"

-- Name of the transmit vtable
lmp_vtbl_name ifn = ifscope ifn "lmp_tx_vtbl"

-- Name of the receive handler
rx_handler_name ifn = ifscope ifn "lmp_rx_handler"

-- Names of the control functions
change_waitset_fn_name ifn = ifscope ifn "lmp_change_waitset"
control_fn_name ifn = ifscope ifn "lmp_control"

------------------------------------------------------------------------
-- Language mapping: Create the header file for this interconnect driver
------------------------------------------------------------------------

header :: String -> String -> Interface -> String
header infile outfile intf = 
    unlines $ C.pp_unit $ header_file intf (lmp_header_body infile intf)
    where
        header_file :: Interface -> [C.Unit] -> C.Unit
        header_file interface@(Interface name _ _) body = 
            let sym = "__" ++ name ++ "_LMP_H"
            in C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []

lmp_header_body :: String -> Interface -> [C.Unit]
lmp_header_body infile interface@(Interface name descr decls) = [
    intf_preamble infile name descr,
    C.Blank,
    C.MultiComment [ "LMP interconnect driver" ],
    C.Blank,
    C.Include C.Standard "barrelfish/lmp_chan.h",
    C.Blank,
    lmp_binding_struct name,
    C.Blank,
    lmp_init_function_proto name,
    lmp_destroy_function_proto name,
    lmp_bind_function_proto name,
    lmp_connect_handler_proto name,
    lmp_rx_handler_proto name,
    C.Blank
    ]

lmp_binding_struct :: String -> C.Unit
lmp_binding_struct ifn = C.StructDecl (lmp_bind_type ifn) fields
  where
    fields = [
        C.Param (C.Struct $ intf_bind_type ifn) "b",
        C.Param (C.Struct "lmp_chan") "chan",
        C.Param (C.TypeName "lmp_send_flags_t") "flags"
        ]

lmp_init_function_proto :: String -> C.Unit
lmp_init_function_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope C.Void params) name Nothing
    where 
      name = lmp_init_fn_name n
      params = [C.Param (C.Ptr $ C.Struct (lmp_bind_type n)) "b",
                C.Param (C.Ptr $ C.Struct "waitset") "waitset"]

lmp_destroy_function_proto :: String -> C.Unit
lmp_destroy_function_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope C.Void params) name Nothing
    where 
      name = lmp_destroy_fn_name n
      params = [C.Param (C.Ptr $ C.Struct (lmp_bind_type n)) "b"]

lmp_bind_function_proto :: String -> C.Unit
lmp_bind_function_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope (C.TypeName "errval_t") params) name Nothing
    where 
      name = lmp_bind_fn_name n
      params = lmp_bind_params n

lmp_bind_params n = [ C.Param (C.Ptr $ C.Struct (lmp_bind_type n)) "b",
                 C.Param (C.TypeName "iref_t") "iref",
                 C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) intf_cont_var,
                 C.Param (C.Ptr $ C.TypeName "void") "st",
                 C.Param (C.Ptr $ C.Struct "waitset") "waitset",
                 C.Param (C.TypeName "idc_bind_flags_t") "flags",
                 C.Param (C.TypeName "size_t") "lmp_buflen" ]

lmp_rx_handler_proto ifn = C.GVarDecl C.Extern C.NonConst
    (C.Function C.NoScope C.Void [C.Param (C.Ptr C.Void) "arg"])
    (rx_handler_name ifn) Nothing

lmp_connect_handler_proto :: String -> C.Unit
lmp_connect_handler_proto ifn = C.GVarDecl C.Extern C.NonConst
    (C.Function C.NoScope (C.TypeName "errval_t") lmp_connect_handler_params)
    (drv_connect_handler_name drvname ifn) Nothing

lmp_connect_handler_params :: [C.Param]
lmp_connect_handler_params
    = [C.Param (C.Ptr $ C.Void) "st",
       C.Param (C.TypeName "size_t") "buflen_words",
       C.Param (C.Struct "capref") "endpoint",
       C.Param (C.Ptr $ C.Ptr $ C.Struct "lmp_chan") "retchan"]


------------------------------------------------------------------------
-- Language mapping: Create the stub (implementation) for this interconnect driver
------------------------------------------------------------------------

stub :: Arch -> String -> String -> Interface -> String
stub arch infile outfile intf = 
    unlines $ C.pp_unit $ lmp_stub_body arch infile intf

lmp_stub_body :: Arch -> String -> Interface -> C.Unit
lmp_stub_body arch infile intf@(Interface ifn descr decls) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,
    C.MultiComment [ "Generated Stub for LMP on " ++ archname arch ],
    C.Blank,

    C.Include C.Standard "string.h",
    C.Include C.Standard "barrelfish/barrelfish.h",
    C.Include C.Standard "flounder/flounder_support.h",
    C.Include C.Standard "flounder/flounder_support_lmp.h",
    C.Include C.Standard ("if/" ++ ifn ++ "_defs.h"),
    C.Blank,

    C.MultiComment [ "Send handler functions" ],
    C.UnitList [ tx_handler arch ifn m | m <- msg_specs ],
    C.Blank,

    C.MultiComment [ "Message sender functions" ],
    C.UnitList [ tx_fn ifn types m | m <- messages ],
    C.Blank,

    C.MultiComment [ "Send vtable" ],
    tx_vtbl ifn messages,
    
    C.MultiComment [ "Receive handler" ],
    rx_handler arch ifn types messages msg_specs,
    C.Blank,

    C.MultiComment [ "Control functions" ],
    can_send_fn_def drvname ifn,
    register_send_fn_def drvname ifn,
    default_error_handler_fn_def drvname ifn,
    change_waitset_fn_def ifn,
    control_fn_def ifn,

    C.MultiComment [ "Functions to initialise/destroy the binding state" ],
    lmp_init_fn ifn,
    lmp_destroy_fn ifn,
    C.Blank,

    C.MultiComment [ "Bind function" ],
    lmp_bind_cont_fn ifn,
    lmp_bind_fn ifn,
    C.Blank,

    C.MultiComment [ "Connect callback for export" ],
    lmp_connect_handler_fn ifn
    ]
    where
        (types, messagedecls) = Backend.partitionTypesMessages decls
        messages = rpcs_to_msgs messagedecls
        msg_specs = [build_lmp_msg_spec arch types m | m <- messages]

lmp_init_fn :: String -> C.Unit
lmp_init_fn ifn = C.FunctionDef C.NoScope C.Void (lmp_init_fn_name ifn) params [
    C.StmtList common_init,
    C.Ex $ C.Call "lmp_chan_init" [C.AddressOf $ C.DerefField lmp_bind_var "chan"],
    C.Ex $ C.Assignment (common_field "change_waitset") (C.Variable $ change_waitset_fn_name ifn),
    C.Ex $ C.Assignment (common_field "control") (C.Variable $ control_fn_name ifn),
    C.Ex $ C.Assignment
            (C.DerefField lmp_bind_var "flags")
            (C.Variable "LMP_SEND_FLAGS_DEFAULT") ]
    where
      params = [C.Param (C.Ptr $ C.Struct (lmp_bind_type ifn)) lmp_bind_var_name,
                C.Param (C.Ptr $ C.Struct "waitset") "waitset"]
      common_field f = lmp_bind_var `C.DerefField` "b" `C.FieldOf` f
      common_init = binding_struct_init drvname ifn
        (C.DerefField lmp_bind_var "b")
        (C.Variable "waitset")
        (C.Variable $ lmp_vtbl_name ifn)

lmp_destroy_fn :: String -> C.Unit
lmp_destroy_fn ifn = C.FunctionDef C.NoScope C.Void (lmp_destroy_fn_name ifn) params [
    C.StmtList common_destroy,
    C.Ex $ C.Call "lmp_chan_destroy" [C.AddressOf $ C.DerefField lmp_bind_var "chan"]]
    where
      params = [C.Param (C.Ptr $ C.Struct (lmp_bind_type ifn)) lmp_bind_var_name]
      common_destroy = binding_struct_destroy ifn (C.DerefField lmp_bind_var "b")

lmp_bind_fn :: String -> C.Unit
lmp_bind_fn ifn =
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (lmp_bind_fn_name ifn) params [
        localvar (C.TypeName "errval_t") "err" Nothing,
        C.Ex $ C.Call (lmp_init_fn_name ifn) [lmp_bind_var, C.Variable "waitset"],
        C.Ex $ C.Assignment (intf_bind_field "st") (C.Variable "st"),
        C.Ex $ C.Assignment (intf_bind_field "bind_cont") (C.Variable intf_cont_var),
        C.Ex $ C.Assignment errvar $ C.Call "lmp_chan_bind"
            [C.AddressOf $ lmp_bind_var `C.DerefField` "chan",
             C.StructConstant "lmp_bind_continuation"
                [("handler", C.Variable (lmp_bind_cont_fn_name ifn)),
                 ("st", lmp_bind_var)],
             C.AddressOf $ intf_bind_field "event_qnode",
             C.Variable "iref",
             C.Variable "lmp_buflen"],
        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.Call (lmp_destroy_fn_name ifn) [lmp_bind_var]] [],
        C.Return errvar
    ]
    where 
      params = lmp_bind_params ifn
      intf_bind_field = C.FieldOf (C.DerefField lmp_bind_var "b")

lmp_bind_cont_fn :: String -> C.Unit
lmp_bind_cont_fn ifn =
    C.FunctionDef C.Static C.Void (lmp_bind_cont_fn_name ifn) params [
        localvar (C.Ptr $ C.Struct $ lmp_bind_type ifn)
            lmp_bind_var_name (Just $ C.Variable "st"),
        C.SBlank,

        C.If (C.Call "err_is_ok" [errvar])
            [C.SComment "allocate a cap receive slot",
             C.Ex $ C.Assignment errvar $
                        C.Call "lmp_chan_alloc_recv_slot" [chanaddr],
             C.If (C.Call "err_is_fail" [errvar])
                [C.Ex $ C.Assignment errvar $
                        C.Call "err_push"
                                [errvar, C.Variable "LIB_ERR_LMP_ALLOC_RECV_SLOT"],
                 C.Goto "fail"] [],
             C.SBlank,

             C.SComment "register for receive",
             C.Ex $ C.Assignment errvar $ C.Call "lmp_chan_register_recv"
                [chanaddr, C.FieldOf intf_var "waitset",
                 C.StructConstant "event_closure"
                        [("handler", C.Variable $ rx_handler_name ifn),
                         ("arg", lmp_bind_var)]],
             C.If (C.Call "err_is_fail" [errvar])
                [C.Ex $ C.Assignment errvar $
                        C.Call "err_push"
                                [errvar, C.Variable "LIB_ERR_CHAN_REGISTER_RECV"],
                 C.Goto "fail"] []]
            [C.Label "fail",
             C.Ex $ C.Call (lmp_destroy_fn_name ifn) [lmp_bind_var]],
        C.SBlank,

        C.Ex $ C.CallInd (intf_var `C.FieldOf` "bind_cont")
            [intf_var `C.FieldOf` "st", errvar, C.AddressOf intf_var]
    ]
    where 
      params = [C.Param (C.Ptr C.Void) "st",
                C.Param (C.TypeName "errval_t") "err",
                C.Param (C.Ptr $ C.Struct "lmp_chan") "chan"]
      intf_var = C.DerefField lmp_bind_var "b"
      errvar = C.Variable "err"
      chanaddr = C.Variable "chan"

lmp_connect_handler_fn :: String -> C.Unit
lmp_connect_handler_fn ifn = C.FunctionDef C.NoScope (C.TypeName "errval_t")
    (drv_connect_handler_name "lmp" ifn) lmp_connect_handler_params [
    localvar (C.Ptr $ C.Struct $ export_type ifn) "e" $ Just $ C.Variable "st",
    localvar (C.TypeName "errval_t") "err" Nothing,
    C.SBlank,
    C.SComment "allocate storage for binding",
    localvar (C.Ptr $ C.Struct $ lmp_bind_type ifn) lmp_bind_var_name
        $ Just $ C.Call "malloc" [C.SizeOfT $ C.Struct $ lmp_bind_type ifn],
    C.If (C.Binary C.Equals lmp_bind_var (C.Variable "NULL"))
        [C.Return $ C.Variable "LIB_ERR_MALLOC_FAIL"] [],
    C.SBlank,
    
    localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
         intf_bind_var (Just $ C.AddressOf $ lmp_bind_var `C.DerefField` "b"),
    C.Ex $ C.Call (lmp_init_fn_name ifn) [lmp_bind_var,
                                          exportvar `C.DerefField` "waitset"],
    C.SBlank,

    C.SComment "run user's connect handler",
    C.Ex $ C.Assignment errvar $ C.CallInd (C.DerefField exportvar "connect_cb")
                       [C.DerefField exportvar "st", bindvar],
    C.If (C.Call "err_is_fail" [errvar])
        [C.SComment "connection refused",
         C.Ex $ C.Call (lmp_destroy_fn_name ifn) [lmp_bind_var],
         C.Return $ errvar] [],
    C.SBlank,

    C.SComment "accept the connection and setup the channel",
    C.SComment "FIXME: user policy needed to decide on the size of the message buffer?",
    C.Ex $ C.Assignment errvar $ C.Call "lmp_chan_accept"
                                [C.AddressOf $ C.DerefField lmp_bind_var "chan",
                                 C.Variable "buflen_words", C.Variable "endpoint"],
    C.If (C.Call "err_is_fail" [errvar])
        [C.Ex $ C.Assignment errvar $ C.Call "err_push"
                    [errvar, C.Variable "LIB_ERR_LMP_CHAN_ACCEPT"],
         report_user_err errvar,
         C.Return $ errvar] [],
    C.SBlank,

    C.SComment "allocate a cap receive slot",
    C.Ex $ C.Assignment errvar $
            C.Call "lmp_chan_alloc_recv_slot" [chanaddr],
    C.If (C.Call "err_is_fail" [errvar])
        [C.Ex $ C.Assignment errvar $ C.Call "err_push"
                        [errvar, C.Variable "LIB_ERR_LMP_ALLOC_RECV_SLOT"],
         report_user_err errvar,
         C.Return $ errvar] [],
    C.SBlank,
    
    C.SComment "register for receive",
    C.Ex $ C.Assignment errvar $ C.Call "lmp_chan_register_recv"
        [chanaddr, C.DerefField bindvar "waitset",
         C.StructConstant "event_closure"
            [("handler", C.Variable $ rx_handler_name ifn),
             ("arg", lmp_bind_var)]],
    C.If (C.Call "err_is_fail" [errvar])
        [C.Ex $ C.Assignment errvar $ C.Call "err_push"
                        [errvar, C.Variable "LIB_ERR_CHAN_REGISTER_RECV"],
         report_user_err errvar,
         C.Return $ errvar] [],
    C.SBlank,

    C.Ex $ C.Assignment (C.DerefPtr $ C.Variable "retchan") chanaddr,
    C.SBlank,
    C.Return $ C.Variable "SYS_ERR_OK"]
    where
        exportvar = C.Variable "e"
        chanaddr = C.AddressOf $ C.DerefField lmp_bind_var "chan"

change_waitset_fn_def :: String -> C.Unit
change_waitset_fn_def ifn = 
    C.FunctionDef C.Static (C.TypeName "errval_t") (change_waitset_fn_name ifn) params [
        localvar (C.Ptr $ C.Struct $ lmp_bind_type ifn)
            lmp_bind_var_name (Just $ C.Cast (C.Ptr C.Void) bindvar),
        C.SBlank,

        C.SComment "Migrate register and TX continuation notifications",
        C.Ex $ C.Call "flounder_support_migrate_notify" [register_chanstate, C.Variable "ws"],
        C.Ex $ C.Call "flounder_support_migrate_notify" [tx_cont_chanstate, C.Variable "ws"],
        C.SBlank,

        C.SComment "change waitset on binding",
        C.Ex $ C.Assignment
            (bindvar `C.DerefField` "waitset")
            (C.Variable "ws"),
        C.SBlank,

        C.SComment "Migrate send and receive notifications",
        C.Ex $ C.Call "lmp_chan_migrate_recv" [chanaddr, C.Variable "ws"],
        C.Ex $ C.Call "lmp_chan_migrate_send" [chanaddr, C.Variable "ws"],
        C.SBlank,

        C.Return $ C.Variable "SYS_ERR_OK"
    ]
    where
        register_chanstate = C.AddressOf $ C.DerefField bindvar "register_chanstate"
        tx_cont_chanstate = C.AddressOf $ C.DerefField bindvar "tx_cont_chanstate"
        chanaddr = C.AddressOf $ C.DerefField lmp_bind_var "chan"
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
                  C.Param (C.Ptr $ C.Struct "waitset") "ws"]

control_fn_def :: String -> C.Unit
control_fn_def ifn = 
    C.FunctionDef C.Static (C.TypeName "errval_t") (control_fn_name ifn) params [
        localvar (C.Ptr $ C.Struct $ lmp_bind_type ifn)
            lmp_bind_var_name (Just $ C.Cast (C.Ptr C.Void) $ C.Variable intf_bind_var),
        C.SBlank,

        C.Ex $ C.Assignment
            (C.DerefField lmp_bind_var "flags")
            (C.Call "idc_control_to_lmp_flags" [C.Variable "control", C.DerefField lmp_bind_var "flags"]),
        C.SBlank,

        C.Return $ C.Variable "SYS_ERR_OK"
    ]
    where
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
                  C.Param (C.TypeName "idc_control_t") "control"]

handler_preamble :: String -> C.Stmt
handler_preamble ifn = C.StmtList
    [C.SComment "Get the binding state from our argument pointer",
     localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
         intf_bind_var (Just $ C.Variable "arg"),
     localvar (C.Ptr $ C.Struct $ lmp_bind_type ifn)
         lmp_bind_var_name (Just $ C.Variable "arg"),
     localvar (C.TypeName "errval_t") "err" Nothing,
     C.SBlank]

tx_handler :: Arch -> String -> LMPMsgSpec -> C.Unit
tx_handler arch ifn (LMPMsgSpec mn msgfrags) =
    C.FunctionDef C.Static C.Void (tx_handler_name ifn mn) [C.Param (C.Ptr C.Void) "arg"] [
        handler_preamble ifn,
        C.SComment "Switch on current outgoing message fragment",
        C.Switch (C.DerefField bindvar "tx_msg_fragment") cases bad,
        C.SBlank,
        C.If (C.Call "lmp_err_is_transient" [errvar])
            -- transient errors
            [C.SComment "Construct retry closure and register it",
             localvar (C.Struct "event_closure") "retry_closure"
                    (Just $ C.StructConstant "event_closure" [
                            ("handler", C.Variable $ tx_handler_name ifn mn),
                            ("arg", C.Variable "arg")]),
             C.Ex $ C.Assignment errvar
                        (C.Call "lmp_chan_register_send" [
                            C.AddressOf $ C.DerefField lmp_bind_var "chan",
                            C.DerefField bindvar "waitset",
                            C.Variable "retry_closure"]),
             C.Ex $ C.Call "assert" [C.Call "err_is_ok" [errvar]]]
             -- permanent errors
            [C.SComment "Report error to user",
             report_user_tx_err errvar
            ]
        ]
    where
        cases = [let isLast = (i == length msgfrags - 1) in
                 C.Case (C.NumConstant $ toInteger i)
                 $ (tx_handler_case arch ifn mn frag isLast) ++ [gentest isLast]
                 | (frag, i) <- zip msgfrags [0 ..]]
        bad = [C.Ex $ C.Call "assert" [C.Unary C.Not $ C.StringConstant "invalid fragment"],
            C.Ex $ C.Assignment errvar (C.Variable "FLOUNDER_ERR_INVALID_STATE")]

        -- generate the if() that checks the result of sending
        gentest isLast = C.If (C.Call "err_is_ok" [errvar])
          (if isLast then -- if the last fragment succeeds, we're done
            finished_send ++ [C.ReturnVoid]
           else
            [C.Ex $ C.PostInc $ C.DerefField bindvar "tx_msg_fragment",
             C.SComment "fall through to next fragment"])
          -- else case is always the same
          [C.Break]
        tx_msgnum_field = C.DerefField bindvar "tx_msgnum"

tx_handler_case :: Arch -> String -> String -> LMPMsgFragment -> Bool -> [C.Stmt]
tx_handler_case arch ifn mn (LMPMsgFragment (MsgFragment words) cap) isLast =
    [C.Ex $ C.Assignment errvar (C.Call send_fn_name args)]
    where
        send_fn_name = "lmp_chan_send" ++ show (length words)
        args = [chan_arg, flag_arg, cap_arg] ++ (map (fragment_word_to_expr arch ifn mn) words)
        chan_arg = C.AddressOf $ C.DerefField lmp_bind_var "chan"
        flag_arg -- only set the sync flag on the last fragment
            | isLast = flag_var
            | otherwise = C.Binary C.BitwiseAnd flag_var $ C.Unary C.BitwiseNot (C.Variable "LMP_FLAG_SYNC")
        flag_var = C.DerefField lmp_bind_var "flags"
        cap_arg = case cap of
            Just (CapFieldTransfer _ af) -> argfield_expr TX mn af
            Nothing -> C.Variable "NULL_CAP"

tx_handler_case arch ifn mn (LMPMsgFragment (OverflowFragment _) (Just _)) _ =
    error "cannot send caps in same fragment as strings/buffers: NYI"

tx_handler_case arch ifn mn (LMPMsgFragment (OverflowFragment (StringFragment af)) Nothing) isLast =
    [C.Ex $ C.Assignment errvar (C.Call "flounder_stub_lmp_send_string" args)]
    where
        args = [chan_arg, flag_arg, string_arg, pos_arg, len_arg]
        chan_arg = C.AddressOf $ C.DerefField lmp_bind_var "chan"
        flag_arg -- only set the sync flag on the last fragment
            | isLast = flag_var
            | otherwise = C.Binary C.BitwiseAnd flag_var $ C.Unary C.BitwiseNot (C.Variable "LMP_FLAG_SYNC") 
        flag_var = C.DerefField lmp_bind_var "flags"
        string_arg = argfield_expr TX mn af
        pos_arg = C.AddressOf $ C.DerefField bindvar "tx_str_pos"
        len_arg = C.AddressOf $ C.DerefField bindvar "tx_str_len"

tx_handler_case arch ifn mn (LMPMsgFragment (OverflowFragment (BufferFragment _ afn afl)) Nothing) isLast =
    [C.Ex $ C.Assignment errvar (C.Call "flounder_stub_lmp_send_buf" args)]
    where
        args = [chan_arg, flag_arg, buf_arg, len_arg, pos_arg]
        chan_arg = C.AddressOf $ C.DerefField lmp_bind_var "chan"
        flag_arg -- only set the sync flag on the last fragment
            | isLast = flag_var
            | otherwise = C.Binary C.BitwiseAnd flag_var $ C.Unary C.BitwiseNot (C.Variable "LMP_FLAG_SYNC") 
        flag_var = C.DerefField lmp_bind_var "flags"
        buf_arg = argfield_expr TX mn afn
        len_arg = argfield_expr TX mn afl
        pos_arg = C.AddressOf $ C.DerefField bindvar "tx_str_pos"

tx_fn :: String -> [TypeDef] -> MessageDef -> C.Unit
tx_fn ifn typedefs msg@(Message _ n args _) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (tx_fn_name ifn n) params body
    where
        params = [binding_param ifn, cont_param] ++ (
                    concat [ msg_argdecl TX ifn a | a <- args ])
        cont_param = C.Param (C.Struct "event_closure") intf_cont_var
        body = [
            C.SComment "check that we can accept an outgoing message",
            C.If (C.Binary C.NotEquals tx_msgnum_field (C.NumConstant 0))
                [C.Return $ C.Variable "FLOUNDER_ERR_TX_BUSY"] [],
            C.SBlank,
            C.SComment "register send continuation",
            C.StmtList $ register_txcont (C.Variable intf_cont_var),
            C.SBlank,
            C.SComment "store message number and arguments",
            C.Ex $ C.Assignment tx_msgnum_field (C.Variable $ msg_enum_elem_name ifn n),
            C.Ex $ C.Assignment tx_msgfrag_field (C.NumConstant 0),
            C.StmtList [ tx_arg_assignment ifn typedefs n a | a <- args ],
            C.StmtList $ start_send drvname ifn n args,
            C.SBlank,
            C.SComment "try to send!",
            C.Ex $ C.Call (tx_handler_name ifn n) [C.Variable intf_bind_var],
            C.SBlank,
            C.Return $ C.Variable "SYS_ERR_OK"
            ]
        tx_msgnum_field = C.DerefField bindvar "tx_msgnum"
        tx_msgfrag_field = C.DerefField bindvar "tx_msg_fragment"

tx_vtbl :: String -> [MessageDef] -> C.Unit
tx_vtbl ifn ml =
    C.StructDef C.Static (intf_vtbl_type ifn TX) (lmp_vtbl_name ifn) fields
    where
        fields = [let mn = msg_name m in (mn, tx_fn_name ifn mn) | m <- ml]

rx_handler :: Arch -> String -> [TypeDef] -> [MessageDef] -> [LMPMsgSpec] -> C.Unit
rx_handler arch ifn typedefs msgdefs msgs =
    C.FunctionDef C.NoScope C.Void (rx_handler_name ifn) [C.Param (C.Ptr C.Void) "arg"] [
        handler_preamble ifn,
        localvar (C.Struct "lmp_recv_msg") "msg" (Just $ C.Variable "LMP_RECV_MSG_INIT"),
        localvar (C.Struct "capref") "cap" Nothing,

        -- declare closure for retry
        localvar (C.Struct "event_closure") "recv_closure"
            (Just $ C.StructConstant "event_closure" [
                ("handler", C.Variable $ rx_handler_name ifn),
                ("arg", C.Variable "arg")]),
        C.SBlank,

        C.DoWhile (C.Call "err_is_ok" [errvar]) [

        C.SComment "try to retrieve a message from the channel",
        C.Ex $ C.Assignment errvar
                $ C.Call "lmp_chan_recv" [chanaddr,
                        C.AddressOf $ C.Variable "msg",
                        C.AddressOf $ C.Variable "cap"],

        C.SComment "check if we succeeded",
        C.If (C.Call "err_is_fail" [errvar])
            -- if err_is_fail, check err_no
            [C.If (C.Binary C.Equals (C.Call "err_no" [errvar]) (C.Variable "LIB_ERR_NO_LMP_MSG"))
                [C.SComment "no message",
                 C.Break]
                [C.SComment "real error",
                 report_user_err $ C.Call "err_push" [errvar, C.Variable "LIB_ERR_LMP_CHAN_RECV"],
                 C.ReturnVoid]
            ]
            [],
        C.SBlank,

        C.SComment "allocate a new receive slot if needed",
        C.If (C.Unary C.Not $ C.Call "capref_is_null" [C.Variable "cap"])
            [C.Ex $ C.Assignment errvar $
                C.Call "lmp_chan_alloc_recv_slot" [chanaddr],
             C.If (C.Call "err_is_fail" [errvar])
                [report_user_err $
                    C.Call "err_push" [errvar, C.Variable "LIB_ERR_LMP_ALLOC_RECV_SLOT"]]
                []
            ] [],
        C.SBlank,
        
        C.SComment "is this the start of a new message?",
        C.If (C.Binary C.Equals rx_msgnum_field (C.NumConstant 0)) [
            C.SComment "check message length",
            C.If (C.Binary C.Equals msglen (C.NumConstant 0)) [
                report_user_err $ C.Variable "FLOUNDER_ERR_RX_EMPTY_MSG",
                C.Break] [],

            C.SComment "unmarshall message number from first word, set fragment to 0",
            C.Ex $ C.Assignment rx_msgnum_field $
                C.Binary C.BitwiseAnd (C.SubscriptOf msgwords $ C.NumConstant 0) msgnum_mask,
            C.Ex $ C.Assignment rx_msgfrag_field (C.NumConstant 0)
        ] [],
        C.SBlank,

        C.SComment "switch on message number and fragment number",
        C.Switch rx_msgnum_field msgnum_cases bad_msgnum
        ], -- end of the while(1) loop

        C.Label "out",
        C.SComment "re-register for another receive notification",
        C.Ex $ C.Assignment errvar $ C.Call "lmp_chan_register_recv"
            [chanaddr, C.DerefField bindvar "waitset", C.Variable "recv_closure"],
        C.Ex $ C.Call "assert" [C.Call "err_is_ok" [errvar]]
        ]
    where
        chanaddr = C.AddressOf $ C.DerefField lmp_bind_var "chan"
        msglen = C.Variable "msg" `C.FieldOf` "buf" `C.FieldOf` "msglen"
        msgwords = C.Variable "msg" `C.FieldOf` "words"
        msgnum_mask = C.HexConstant ((shift 1 msgnum_bits) - 1)
        msgnum_bits = bitsizeof_argfieldfrag arch MsgCode
        rx_msgnum_field = C.DerefField bindvar "rx_msgnum"
        rx_msgfrag_field = C.DerefField bindvar "rx_msg_fragment"

        msgnum_cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn) (msgnum_case msgdef msg)
                            | (msgdef, msg@(LMPMsgSpec mn _)) <- zip msgdefs msgs]

        msgnum_case msgdef@(Message _ _ msgargs _) (LMPMsgSpec mn frags) = [
            C.Switch rx_msgfrag_field
                [C.Case (C.NumConstant $ toInteger i) $
                    (if i == 0 then start_recv drvname ifn typedefs mn msgargs
                     else [])
                    ++ msgfrag_case msgdef (frags !! i) (i == length frags - 1)
                 | i <- [0 .. length frags - 1]]
                bad_msgfrag,
            C.Break]

        bad_msgnum = [report_user_err $ C.Variable "FLOUNDER_ERR_RX_INVALID_MSGNUM",
                      C.Goto "out"]

        bad_msgfrag = [report_user_err $ C.Variable "FLOUNDER_ERR_INVALID_STATE",
                      C.Goto "out"]

        msgfrag_case :: MessageDef -> LMPMsgFragment -> Bool -> [C.Stmt]
        msgfrag_case msg@(Message _ mn _ _) (LMPMsgFragment (MsgFragment wl) cap) isLast = [
            C.SComment "check length",
            -- XXX: LRPC always delivers a message of a fixed size
            C.If (if (length wl < lrpc_words arch)
                    then C.Binary C.GreaterThan msglen
                            (C.NumConstant $ toInteger $ lrpc_words arch)
                    else C.Binary C.NotEquals msglen
                            (C.NumConstant $ toInteger $ length wl)) [
                report_user_err $ C.Variable "FLOUNDER_ERR_RX_INVALID_LENGTH",
                C.Goto "out"] [],
            C.SBlank,

            C.StmtList $ concat [store_arg_frags arch ifn mn msgwords word 0 afl
                                 | (afl, word) <- zip wl [0..]],
            case cap of
                Just (CapFieldTransfer _ af) -> C.Ex $ C.Assignment (argfield_expr RX mn af) (C.Variable "cap")
                Nothing -> C.StmtList [],
            C.SBlank,

            msgfrag_case_prolog msg isLast,
            C.Break]

        msgfrag_case msg@(Message _ mn _ _) (LMPMsgFragment (OverflowFragment (StringFragment af)) _) isLast = [
            C.Ex $ C.Assignment errvar (C.Call "flounder_stub_lmp_recv_string" args),
            C.If (C.Call "err_is_ok" [errvar])
                [msgfrag_case_prolog msg isLast]
                -- error from string receive code, check if it's permanent
                [C.If (C.Binary C.NotEquals
                        (C.Call "err_no" [errvar])
                        (C.Variable "FLOUNDER_ERR_BUF_RECV_MORE"))
                    [report_user_err errvar] -- real error
                    [] -- will receive more next time
                ],
            C.Break]
            where
                args = [msg_arg, string_arg, pos_arg, len_arg]
                msg_arg = C.AddressOf $ C.Variable "msg"
                string_arg = C.AddressOf $ argfield_expr RX mn af
                pos_arg = C.AddressOf $ C.DerefField bindvar "rx_str_pos"
                len_arg = C.AddressOf $ C.DerefField bindvar "rx_str_len"

        msgfrag_case msg@(Message _ mn _ _) (LMPMsgFragment (OverflowFragment (BufferFragment _ afn afl)) _) isLast = [
            C.Ex $ C.Assignment errvar (C.Call "flounder_stub_lmp_recv_buf" args),
            C.If (C.Call "err_is_ok" [errvar])
                [msgfrag_case_prolog msg isLast]
                -- error from receive code, check if it's permanent
                [C.If (C.Binary C.NotEquals
                        (C.Call "err_no" [errvar])
                        (C.Variable "FLOUNDER_ERR_BUF_RECV_MORE"))
                    [report_user_err errvar] -- real error
                    [] -- will receive more next time
                ],
            C.Break]
            where
                args = [msg_arg, buf_arg, len_arg, pos_arg]
                msg_arg = C.AddressOf $ C.Variable "msg"
                buf_arg = C.Cast (C.Ptr $ C.Ptr C.Void) $ C.AddressOf $ argfield_expr RX mn afn
                len_arg = C.AddressOf $ argfield_expr RX mn afl
                pos_arg = C.AddressOf $ C.DerefField bindvar "rx_str_pos"

        msgfrag_case_prolog :: MessageDef -> Bool -> C.Stmt
        -- intermediate fragment
        msgfrag_case_prolog _ False
            = C.Ex $ C.PostInc $ C.DerefField bindvar "rx_msg_fragment"

        -- last fragment: call handler and zero message number
        msgfrag_case_prolog (Message _ mn msgargs _) True
            = C.StmtList $ finished_recv drvname ifn typedefs mn msgargs
