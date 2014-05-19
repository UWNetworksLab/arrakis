{- 
  UMP.hs: Flounder stub generator for cross-core shared memory message passing.

  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module UMP where

import CAbsSyntax as C
import qualified UMPCommon
import UMPCommon hiding (header, stub)
import BackendCommon

-- Name of the init function
init_fn_name p n = ump_ifscope p n "init"

params = template_params {
    ump_payload = 56, -- msg payload in bytes
    ump_drv = "ump",
    ump_arch = undefined,

    ump_extra_protos = \ifn -> [init_fn_proto params ifn],
    ump_extra_fns = \ifn -> [init_fn params ifn],

    ump_register_recv = ump_chan_register_recv,
    ump_deregister_recv = ump_chan_deregister_recv
}

header = UMPCommon.header params
stub a = UMPCommon.stub (params { ump_arch = a })

bind_type ifn = UMPCommon.my_bind_type params ifn
bind_fn_name ifn = UMPCommon.bind_fn_name params ifn

-- generate the code to register for receive notification
ump_chan_register_recv :: String -> [C.Stmt]
ump_chan_register_recv ifn = [
    C.Ex $ C.Assignment errvar $ C.Call "ump_chan_register_recv"
        [C.AddressOf $ my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan",
         bindvar `C.DerefField` "waitset",
         C.StructConstant "event_closure"
            [("handler", C.Variable $ rx_handler_name params ifn), ("arg", bindvar)]]
    ]

ump_chan_deregister_recv :: String -> [C.Stmt]
ump_chan_deregister_recv ifn = [
    C.Ex $ C.Assignment errvar $ C.Call "ump_chan_deregister_recv"
        [C.AddressOf $ my_bindvar `C.DerefField` "ump_state" `C.FieldOf` "chan"]]

init_fn_proto :: UMPParams -> String -> C.Unit
init_fn_proto p n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope (C.TypeName "errval_t") (init_params p n)) name Nothing
    where 
      name = init_fn_name p n

init_params p n = [
    C.Param (C.Ptr $ C.Struct (my_bind_type p n)) "b",
    C.Param (C.Ptr $ C.Struct "waitset") "waitset",
    C.Param (C.Volatile $ C.Ptr C.Void) "inbuf",
    C.Param (C.TypeName "size_t") "inbufsize",
    C.Param (C.Volatile $ C.Ptr C.Void) "outbuf",
    C.Param (C.TypeName "size_t") "outbufsize"]

init_fn :: UMPParams -> String -> C.Unit
init_fn p ifn =
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (init_fn_name p ifn) (init_params p ifn)
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
            [C.Ex $ C.Call (destroy_fn_name p ifn) [my_bindvar],
             C.Return $
                C.Call "err_push" [errvar, C.Variable "LIB_ERR_UMP_CHAN_INIT"]]
            [],
        C.SBlank,

        C.Ex $ C.Assignment (common_field "change_waitset") (C.Variable $ change_waitset_fn_name p ifn),
        C.Ex $ C.Assignment (common_field "control") (C.Variable $ generic_control_fn_name (ump_drv p) ifn),

        C.StmtList $ register_recv p ifn,
        C.SBlank,

        C.Return errvar]
    where
        statevar = C.DerefField my_bindvar "ump_state"
        common_field f = my_bindvar `C.DerefField` "b" `C.FieldOf` f
        common_init = binding_struct_init (ump_drv p) ifn
                        (C.DerefField my_bindvar "b")
                        (C.Variable "waitset")
                        (C.Variable $ tx_vtbl_name p ifn)
