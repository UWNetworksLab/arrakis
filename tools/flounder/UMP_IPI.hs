{- 
  UMP_IPI.hs: Flounder stub generator for cross-core message passing using IPIs.

  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module UMP_IPI where

import CAbsSyntax as C
import qualified UMPCommon
import UMPCommon hiding (header, stub)
import BackendCommon

-- Name of the init function
init_fn_name n = ifscope n "ump_ipi_init"

uparams = template_params {
    ump_payload = 28, -- bytes
    ump_drv = "ump_ipi",

    ump_binding_extra_fields =
        [ C.Param (C.Struct "ipi_notify") "ipi_notify",
          C.Param (C.TypeName "bool") "no_notify"
        ],
    ump_extra_includes = ["arch/x86/barrelfish/ipi_notify.h"],

    ump_extra_protos = \ifn -> [init_fn_proto ifn],
    ump_extra_fns = \ifn -> [accept_alloc_notify_cont_fn ifn,
                             bind_alloc_notify_cont_fn ifn,
                             init_fn ifn],

    ump_register_recv = ump_ipi_register_recv,
    ump_deregister_recv = ump_ipi_deregister_recv,
    ump_accept_alloc_notify = Just accept_alloc_notify,
    ump_bind_alloc_notify = Just bind_alloc_notify,
    ump_store_notify_cap = store_notify_cap,
    ump_notify = do_notify,
    ump_binding_extra_fields_init = ump_ipi_binding_extra_fields_init,
    ump_connect_extra_fields_init = ump_ipi_connect_extra_fields_init
}

header = UMPCommon.header uparams
stub a = UMPCommon.stub (uparams { ump_arch = a })

bind_type ifn = UMPCommon.my_bind_type uparams ifn
bind_fn_name ifn = UMPCommon.bind_fn_name uparams ifn

accept_alloc_notify_cont_name ifn = ifscope ifn "ump_ipi_accept_alloc_notify_cont"
bind_alloc_notify_cont_name ifn = ifscope ifn "ump_ipi_bind_alloc_notify_cont"

ump_ipi_binding_extra_fields_init :: [C.Stmt]
ump_ipi_binding_extra_fields_init =
    [ C.Ex $ C.Assignment (my_bindvar `C.DerefField` "no_notify") $
           C.Ternary (C.Binary C.BitwiseAnd (C.Variable "flags") (C.Variable "IDC_BIND_FLAG_NO_NOTIFY")) (C.Variable "true") (C.Variable "false")
    ]

ump_ipi_connect_extra_fields_init :: [C.Stmt]
ump_ipi_connect_extra_fields_init =
    [ C.Ex $ C.Assignment (my_bindvar `C.DerefField` "no_notify") $
           C.Ternary (C.Binary C.BitwiseAnd ((C.DerefField exportvar "common") `C.FieldOf` "flags") (C.Variable "IDC_EXPORT_FLAG_NO_NOTIFY")) (C.Variable "true") (C.Variable "false")
    ]
    where
      exportvar = C.Variable "e"

-- generate the code to register for receive notification
ump_ipi_register_recv :: String -> [C.Stmt]
ump_ipi_register_recv ifn =
    [ C.If (C.Call "capref_is_null" [notifyvar `C.FieldOf` "my_notify_cap"])
      [ C.Ex $ C.Assignment errvar $ C.Call "ump_chan_register_recv"
        [C.AddressOf $ my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan",
         bindvar `C.DerefField` "waitset", C.StructConstant "event_closure"
         [("handler", C.Variable $ rx_handler_name uparams ifn), ("arg", bindvar)]]
      ]
      [ C.Ex $ C.Assignment errvar $ C.Call "ipi_notify_register"
        [notifyaddr, bindvar `C.DerefField` "waitset",
         C.StructConstant "event_closure"
         [("handler", C.Variable $ rx_handler_name uparams ifn), ("arg", bindvar)]]
      ]
    ]

ump_ipi_deregister_recv :: String -> [C.Stmt]
ump_ipi_deregister_recv ifn =
    [ C.If (C.Call "capref_is_null" [notifyvar `C.FieldOf` "my_notify_cap"])
      [C.Ex $ C.Assignment errvar $ C.Call "ump_chan_deregister_recv"
       [C.AddressOf $ my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan"]]
      [C.Ex $ C.Assignment errvar $ C.Call "ipi_notify_deregister" [notifyaddr]]
    ]

alloc_notify :: String -> [C.Stmt]
alloc_notify handler =
    [ C.If (my_bindvar `C.DerefField` "no_notify")
      [ C.Ex $ C.Assignment errvar $ C.Call "ipi_notify_init"
        [ notifyaddr, C.Variable "NULL_CAP", C.Variable "NULL_CAP",
          C.Variable "NULL_CAP", C.Variable "NULL" ],

        C.Ex $ C.Call handler [my_bindvar, errvar, C.Variable "NULL"],
        C.Ex $ (C.Assignment errvar (C.Variable "SYS_ERR_OK"))
      ]
      [ C.Ex $ C.Assignment errvar $ C.Call "ipi_notify_alloc"
        [notifyaddr, C.StructConstant "ipi_alloc_continuation"
         [("handler", C.Variable handler), ("st", my_bindvar)]]
      ]
    ]
    where
        chanvar = my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan"

accept_alloc_notify ifn = alloc_notify $ accept_alloc_notify_cont_name ifn

bind_alloc_notify ifn =
    [ C.If (my_bindvar `C.DerefField` "no_notify")
      [ C.Ex $ C.Assignment errvar $ C.Call "ipi_notify_init"
        [ notifyaddr, C.Variable "NULL_CAP", C.Variable "NULL_CAP",
          C.Variable "NULL_CAP", C.Variable "NULL" ],
        C.If (C.Call "err_is_ok" [errvar])
         [ C.Ex $ C.Assignment errvar $ C.Call "ump_chan_bind"
           [C.AddressOf $ chanvar,
            C.StructConstant "ump_bind_continuation"
            [("handler", C.Variable (bind_cont_fn_name uparams ifn)),
             ("st", my_bindvar)],
            C.AddressOf $ intf_bind_var `C.FieldOf` "event_qnode",
             my_bindvar `C.DerefField` "iref",
             chanvar `C.FieldOf` "monitor_binding",
             my_bindvar `C.DerefField` "inchanlen",
             my_bindvar `C.DerefField` "outchanlen",
             C.Variable "NULL_CAP" ] ] []
      ]
      [ C.Ex $ C.Assignment errvar $ C.Call "ipi_notify_alloc"
        [notifyaddr, C.StructConstant "ipi_alloc_continuation"
         [("handler", C.Variable handler), ("st", my_bindvar)]]
      ]
    ]
    where
      statevar = C.DerefField my_bindvar "ump_state"
      chanvar = my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan"
      handler = bind_alloc_notify_cont_name ifn
      intf_bind_var = C.DerefField my_bindvar "b"

accept_alloc_notify_cont_fn :: String -> C.Unit
accept_alloc_notify_cont_fn ifn = 
    C.FunctionDef C.Static C.Void (accept_alloc_notify_cont_name ifn) params [
        localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
                intf_bind_var (Just $ C.Variable "st"),
        localvar (C.Ptr $ C.Struct $ my_bind_type uparams ifn)
                my_bind_var_name (Just $ C.Variable "st"),
        C.SBlank,

        C.If (C.Call "err_is_fail" [errvar])
            [report_user_err errvar] [],
        C.SBlank,

        C.StmtList $ ump_ipi_register_recv ifn,
        C.SBlank,

        C.SComment "send back bind reply",
        C.Ex $ C.Call "ump_chan_send_bind_reply"
            [chanvar `C.FieldOf` "monitor_binding",
             C.AddressOf chanvar,
             errvar,
             chanvar `C.FieldOf` "monitor_id",
             notifyvar `C.FieldOf` "my_notify_cap"]
    ]
    where
        params = [C.Param (C.Ptr C.Void) "st",
                  C.Param (C.TypeName "errval_t") "err",
                  C.Param (C.Ptr $ C.Struct "ipi_notify") "notify"]
        chanvar = my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan"


bind_alloc_notify_cont_fn :: String -> C.Unit
bind_alloc_notify_cont_fn ifn = 
    C.FunctionDef C.Static C.Void (bind_alloc_notify_cont_name ifn) params [
        localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
                intf_bind_var (Just $ C.Variable "st"),
        localvar (C.Ptr $ C.Struct $ my_bind_type uparams ifn)
                my_bind_var_name (Just $ C.Variable "st"),
        C.SBlank,

        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.CallInd (bindvar `C.DerefField` "bind_cont")
                [bindvar `C.DerefField` "st", errvar, bindvar],
             C.Ex $ C.Call (destroy_fn_name uparams ifn) [my_bindvar],
             C.ReturnVoid] [],
        C.SBlank,

        C.Ex $ C.Assignment errvar $ C.Call "ump_chan_bind"
            [C.AddressOf $ chanvar,
             C.StructConstant "ump_bind_continuation"
                [("handler", C.Variable (bind_cont_fn_name uparams ifn)),
                 ("st", my_bindvar)],
             C.AddressOf $ bindvar `C.DerefField` "event_qnode",
             my_bindvar `C.DerefField` "iref",
             chanvar `C.FieldOf` "monitor_binding",
             my_bindvar `C.DerefField` "inchanlen",
             my_bindvar `C.DerefField` "outchanlen",
             notifyvar `C.FieldOf` "my_notify_cap"],
        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.CallInd (bindvar `C.DerefField` "bind_cont")
                [bindvar `C.DerefField` "st", errvar, bindvar],
             C.Ex $ C.Call (destroy_fn_name uparams ifn) [my_bindvar]] []
    ]
    where
        params = [C.Param (C.Ptr C.Void) "st",
                  C.Param (C.TypeName "errval_t") "err",
                  C.Param (C.Ptr $ C.Struct "ipi_notify") "notify"]
        chanvar = my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan"

store_notify_cap :: String -> C.Expr -> [C.Stmt]
store_notify_cap ifn capex
    = [C.Ex $ C.Call "ipi_notify_set" [notifyaddr, capex]]

do_notify :: [C.Stmt]
do_notify =
    [ C.If (C.Unary C.Not $ C.Call "capref_is_null" [notifyvar `C.FieldOf` "rmt_notify_cap"])
      [ C.Ex $ C.Assignment errvar $ C.Call "ipi_notify_raise" [notifyaddr],
        C.If (C.Call "err_is_fail" [errvar])
             [report_user_tx_err $
              C.Call "err_push" [errvar, C.Variable "LIB_ERR_IPI_NOTIFY"]] []] []
    ]

notifyvar = my_bindvar `C.DerefField` "ipi_notify"
notifyaddr = C.AddressOf $ notifyvar


init_fn_proto :: String -> C.Unit
init_fn_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope (C.TypeName "errval_t") (init_params n)) name Nothing
    where 
      name = init_fn_name n

init_params n = [
    C.Param (C.Ptr $ C.Struct (my_bind_type uparams n)) "b",
    C.Param (C.Ptr $ C.Struct "waitset") "waitset",
    C.Param (C.Volatile $ C.Ptr C.Void) "inbuf",
    C.Param (C.TypeName "size_t") "inbufsize",
    C.Param (C.Volatile $ C.Ptr C.Void) "outbuf",
    C.Param (C.TypeName "size_t") "outbufsize",
    C.Param (C.Struct "capref") "rmt_notify_cap",
    C.Param (C.Struct "capref") "my_notify_cap",
    C.Param (C.Struct "capref") "notify_ep_cap",
    C.Param (C.Ptr $ C.Struct "lmp_endpoint") "notify_ep"]

init_fn :: String -> C.Unit
init_fn ifn =
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (init_fn_name ifn) (init_params ifn)
       [localvar (C.TypeName "errval_t") "err" Nothing,
        localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
            intf_bind_var (Just $ C.AddressOf $ my_bindvar `C.DerefField` "b"),
        C.StmtList common_init,
        C.Ex $ C.Call "flounder_stub_ump_state_init" [C.AddressOf statevar, my_bindvar],

        C.Ex $ C.Assignment errvar $ C.Call "ump_chan_init"
            [C.AddressOf $ statevar `C.FieldOf` "chan",
            C.Variable "inbuf", C.Variable "inbufsize",
            C.Variable "outbuf", C.Variable "outbufsize"],
        C.If (C.Call "err_is_fail" [errvar])
            [C.Ex $ C.Call (destroy_fn_name uparams ifn) [my_bindvar],
             C.Return $
                C.Call "err_push" [errvar, C.Variable "LIB_ERR_UMP_CHAN_INIT"]]
            [],
        C.SBlank,

        C.Ex $ C.Call "ipi_notify_init"
            [C.AddressOf $ my_bindvar `C.DerefField` "ipi_notify",
             C.Variable "rmt_notify_cap", C.Variable "my_notify_cap",
             C.Variable "notify_ep_cap", C.Variable "notify_ep"],
        C.SBlank,

        C.Ex $ C.Assignment (common_field "change_waitset") (C.Variable $ change_waitset_fn_name uparams ifn),
        C.Ex $ C.Assignment (common_field "control") (C.Variable $ generic_control_fn_name (ump_drv uparams) ifn),

        C.StmtList $ register_recv uparams ifn,
        C.SBlank,

        C.Return errvar]
    where
        statevar = C.DerefField my_bindvar "ump_state"
        common_field f = my_bindvar `C.DerefField` "b" `C.FieldOf` f
        common_init = binding_struct_init (ump_drv uparams) ifn
                        (C.DerefField my_bindvar "b")
                        (C.Variable "waitset")
                        (C.Variable $ tx_vtbl_name uparams ifn)
