{- 
  Multihop.hs: Flounder stub generator for multihop message passing.

  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module Multihop(stub, header, m_bind_type, m_bind_fn_name) where

import Data.Bits
import Data.Char

import qualified CAbsSyntax as C
import qualified Backend
import GHBackend
import MsgFragments
import Syntax
import Arch
import BackendCommon

------------------------------------------------------------------------
-- Multihop Interconnect Driver: Config
------------------------------------------------------------------------

-- the multihop driver uses the wordsize of this architecture
m_arch :: Arch
m_arch = case (parse_arch "x86_64") of
             Nothing -> error "Could not find architecture x86_64 (needed for multihop). Check Arch.hs file"
	     (Just a) -> a

-- type containing as many bytes as wordsize
wordsize_type = C.TypeName "uint64_t"

-- type used to send size of strings / buffers
m_size_type = C.TypeName "uint64_t"
m_size_type_bytes = 8

-- msg payload in bytes (currently not implemented)
msg_payload = 100000

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

-- name of the driver
drvname :: String
drvname = "multihop"

-- Name of the binding struct
m_bind_type :: String -> String
m_bind_type ifn = ifscope ifn "multihop_binding"

-- Name of the local variable used for the Multihop-specific binding type
multihop_bind_var_name :: String
multihop_bind_var_name = "mb"
multihop_bind_var = C.Variable multihop_bind_var_name

-- Name of the bind function
m_bind_fn_name :: String -> String
m_bind_fn_name ifn = ifscope ifn "multihop_bind"

-- Name of the bind continuation function
multihop_bind_cont_fn_name :: String -> String
multihop_bind_cont_fn_name ifn = ifscope ifn "multihop_bind_continuation"

-- Name of the init function
multihop_init_fn_name :: String -> String
multihop_init_fn_name ifn = ifscope ifn "multihop_init"

-- Name of the destroy function
multihop_destroy_fn_name :: String -> String
multihop_destroy_fn_name ifn = ifscope ifn "multihop_destroy"

-- Name of the transmit function for a message
tx_fn_name :: String -> String -> String
tx_fn_name ifn mn = idscope ifn mn "multihop_send"

-- Name of the transmit handler for a message
tx_handler_name :: String -> String -> String
tx_handler_name ifn mn = idscope ifn mn "multihop_send_handler"

-- Name of the caps send handler
cap_tx_handler_name :: String -> String
cap_tx_handler_name ifn = ifscope ifn "multihop_cap_send_handler"

-- Name of the transmit virtual table
multihop_vtbl_name :: String -> String
multihop_vtbl_name ifn = ifscope ifn "multihop_tx_vtbl"

-- Name of the receive handler
rx_handler_name :: String -> String
rx_handler_name ifn = ifscope ifn "multihop_rx_handler"

-- Name of the caps receive handlers
caps_rx_handler_name :: String -> String
caps_rx_handler_name ifn = ifscope ifn "multihop_caps_rx_handler"

-- Names of the control functions
change_waitset_fn_name, control_fn_name :: String -> String
change_waitset_fn_name ifn = ifscope ifn "multihop_change_waitset"
control_fn_name ifn = ifscope ifn "multihop_control"

------------------------------------------------------------------------
-- Language mapping: Create the header file for this interconnect driver
------------------------------------------------------------------------

-- create the header file
header :: String -> String -> Interface -> String
header infile outfile intf = 
    unlines $ C.pp_unit $ header_file intf (multihop_header_body infile intf)
    where
        header_file :: Interface -> [C.Unit] -> C.Unit
        header_file interface@(Interface name _ _) body = 
            let sym = "__" ++ name ++ "_MULTIHOP_H"
            in C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []

multihop_header_body :: String -> Interface -> [C.Unit]
multihop_header_body infile interface@(Interface name descr decls) = [
    intf_preamble infile name descr,
    C.Blank,
    C.MultiComment [ "Multihop interconnect driver" ],
    C.Blank,
    C.Include C.Standard "barrelfish/multihop_chan.h",
    C.Blank,
    multihop_binding_struct name,
    C.Blank,
    multihop_init_function_proto name,
    multihop_destroy_function_proto name,
    multihop_bind_function_proto name,
    multihop_connect_handler_proto name,
    multihop_rx_handler_proto name,
    multihop_caps_rx_handler_proto name,
    C.Blank
    ]

multihop_binding_struct :: String -> C.Unit
multihop_binding_struct ifn = C.StructDecl (m_bind_type ifn) fields
  where
    fields = [
        C.Param (C.Struct $ intf_bind_type ifn) "b",
        C.Param (C.Struct "multihop_chan") "chan",
        C.Param (C.Ptr C.Void) "message",
        C.Param (C.Struct "flounder_cap_state") "capst", 
        C.Param (C.TypeName "bool") "trigger_chan"]

multihop_init_function_proto :: String -> C.Unit
multihop_init_function_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope C.Void params) name Nothing
    where 
      name = multihop_init_fn_name n
      params = [C.Param (C.Ptr $ C.Struct (m_bind_type n)) "b",
                C.Param (C.Ptr $ C.Struct "waitset") "waitset"]

multihop_destroy_function_proto :: String -> C.Unit
multihop_destroy_function_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope C.Void params) name Nothing
    where 
      name = multihop_destroy_fn_name n
      params = [C.Param (C.Ptr $ C.Struct (m_bind_type n)) "b"]

multihop_bind_function_proto :: String -> C.Unit
multihop_bind_function_proto n = 
    C.GVarDecl C.Extern C.NonConst 
         (C.Function C.NoScope (C.TypeName "errval_t") params) name Nothing
    where 
      name = m_bind_fn_name n
      params = multihop_bind_params n

multihop_bind_params n = [ C.Param (C.Ptr $ C.Struct (m_bind_type n)) multihop_bind_var_name,
                 C.Param (C.TypeName "iref_t") "iref",
                 C.Param (C.Ptr $ C.TypeName $ intf_bind_cont_type n) intf_cont_var,
                 C.Param (C.Ptr $ C.TypeName "void") "st",
                 C.Param (C.Ptr $ C.Struct "waitset") "waitset",
                 C.Param (C.TypeName "idc_bind_flags_t") "flags" ]

multihop_rx_handler_proto ifn = C.GVarDecl C.Extern C.NonConst
    (C.Function C.NoScope C.Void multihop_rx_handler_params)
    (rx_handler_name ifn) Nothing

multihop_rx_handler_params :: [C.Param]
multihop_rx_handler_params = [C.Param (C.Ptr C.Void) "arg",
			      C.Param (C.Ptr (C.TypeName "uint8_t")) "message", C.Param (C.TypeName "size_t") "message_len"]

multihop_connect_handler_proto :: String -> C.Unit
multihop_connect_handler_proto ifn = C.GVarDecl C.Extern C.NonConst
    (C.Function C.NoScope (C.TypeName "errval_t") multihop_connect_handler_params)
    (drv_connect_handler_name drvname ifn) Nothing

multihop_connect_handler_params :: [C.Param]
multihop_connect_handler_params
    = [C.Param (C.Ptr $ C.Void) "st",
       C.Param (C.TypeName "multihop_vci_t") "vci"]

multihop_caps_rx_handler_proto :: String -> C.Unit
multihop_caps_rx_handler_proto ifn = C.GVarDecl C.Extern C.NonConst
    (C.Function C.NoScope C.Void multihop_caps_rx_handler_params)
    (caps_rx_handler_name ifn) Nothing

multihop_caps_rx_handler_params :: [C.Param]
multihop_caps_rx_handler_params = [C.Param (C.Ptr C.Void) "arg",
                                   C.Param (C.TypeName "errval_t") "success",
                                   C.Param (C.Struct "capref") "cap",
                                   C.Param (C.TypeName "uint32_t") "capid"]

------------------------------------------------------------------------
-- Language mapping: Create the stub (implementation) for this interconnect driver
------------------------------------------------------------------------

-- generate a C expression for constructing the given word of a message fragment
-- we need this version, because the version from MsgFragments.hs cast the argument
-- to uintptr_t, which is unacceptable for multi-hop messaging
m_fragment_word_to_expr :: Arch -> String -> String -> FragmentWord -> C.Expr
m_fragment_word_to_expr arch ifn mn frag = mkwordexpr 0 frag
    where
        mkwordexpr :: Int -> FragmentWord -> C.Expr
        mkwordexpr shift [af] = doshift shift (mkfieldexpr af)
        mkwordexpr shift (af:rest) = C.Binary C.BitwiseOr cur $ mkwordexpr rshift rest
            where
                cur = doshift shift (mkfieldexpr af)
                rshift = shift + bitsizeof_argfieldfrag arch af

        doshift :: Int -> C.Expr -> C.Expr
        doshift 0 ex = ex
        doshift n ex = C.Binary C.LeftShift
                        (C.Cast wordsize_type ex)
                        (C.NumConstant $ toInteger n)

        mkfieldexpr :: ArgFieldFragment -> C.Expr
        mkfieldexpr MsgCode = C.Variable $ msg_enum_elem_name ifn mn
        mkfieldexpr (ArgFieldFragment t af 0) = fieldaccessor t af
        mkfieldexpr (ArgFieldFragment t af off) =
            C.Binary C.RightShift (fieldaccessor t af) (C.NumConstant $ toInteger off)

        -- special-case bool types to ensure we only get the one-bit true/false value
        -- ticket #231
        fieldaccessor Bool af
          = C.Binary C.NotEquals (argfield_expr TX mn af) (C.Variable "false")
        fieldaccessor _ af = argfield_expr TX mn af

stub :: Arch -> String -> String -> Interface -> String
stub arch infile outfile intf = 
    unlines $ C.pp_unit $ multihop_stub_body arch infile intf

multihop_stub_body :: Arch -> String -> Interface -> C.Unit
multihop_stub_body arch infile intf@(Interface ifn descr decls) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,
    C.IfDef ("CONFIG_FLOUNDER_BACKEND_" ++ (map toUpper (drvname)))
    [
      C.MultiComment [ "Generated Stub for Multihop on " ++ archname arch ],
      C.Blank,

      C.Include C.Standard "string.h",
      C.Include C.Standard "barrelfish/barrelfish.h",
      C.Include C.Standard "flounder/flounder_support.h",
      C.Include C.Standard ("if/" ++ ifn ++ "_defs.h"),
      C.Blank,
 
      C.MultiComment [ "Capability sender function" ],
      (if (contains_caps) then 
         (tx_cap_handler arch ifn msg_specs) else C.Blank),

      C.MultiComment [ "Send handler functions" ],
      C.UnitList [ tx_handler arch ifn m | m <- msg_specs ],
      C.Blank,

      C.MultiComment [ "Cap receive handlers" ],
      caps_rx_handler arch ifn types messages msg_specs,
      C.Blank,

      C.MultiComment [ "Message sender functions" ],
      C.UnitList [ tx_fn ifn types m | m <- messages ],
      C.Blank,

      C.MultiComment [ "Send vtable" ],
      tx_vtbl ifn messages,
    
      C.MultiComment [ "Receive handler" ],
      rx_handler m_arch ifn types messages msg_specs,
      C.Blank,

      C.MultiComment [ "Control functions" ],
      m_can_send_fn_def drvname ifn,
      register_send_fn_def drvname ifn,
      default_error_handler_fn_def drvname ifn,
      change_waitset_fn_def ifn,
      control_fn_def ifn,

      C.MultiComment [ "Functions to initialise/destroy the binding state" ],
      multihop_init_fn ifn,
      multihop_destroy_fn ifn,
      C.Blank,

      C.MultiComment [ "Bind function" ],
      multihop_bind_cont_fn ifn,
      multihop_bind_fn ifn,
      C.Blank,

      C.MultiComment [ "Connect callback for export" ],
      multihop_connect_handler_fn ifn
    ] [] ]
    where
        (types, messagedecls) = Backend.partitionTypesMessages decls
        messages = rpcs_to_msgs messagedecls

	msg_specs :: [MsgSpec]
        msg_specs = map (build_msg_spec m_arch words_per_frag True types) messages
       
	-- number of words per transport level message
	words_per_frag = msg_payload `div` (wordsize arch `div` 8)

	contains_caps :: Bool
	contains_caps = not $ null $  concat [ caps | (MsgSpec _ frags caps) <- msg_specs]

-- initialize multi-hop channel
multihop_init_fn :: String -> C.Unit
multihop_init_fn ifn = C.FunctionDef C.NoScope C.Void (multihop_init_fn_name ifn) params [
  C.StmtList common_init,
  C.Ex $ C.Assignment (common_field "change_waitset") (C.Variable $ change_waitset_fn_name ifn),
  C.Ex $ C.Assignment (common_field "control") (C.Variable $ control_fn_name ifn),
  C.Ex $ C.Assignment (C.DerefField multihop_bind_var "trigger_chan") (C.Variable "false")
  ]
  where
      params = [C.Param (C.Ptr $ C.Struct (m_bind_type ifn)) multihop_bind_var_name,
                C.Param (C.Ptr $ C.Struct "waitset") "waitset"]
      common_field f = multihop_bind_var `C.DerefField` "b" `C.FieldOf` f
      common_init = binding_struct_init drvname ifn
        (C.DerefField multihop_bind_var "b")
        (C.Variable "waitset")
        (C.Variable $ multihop_vtbl_name ifn)

-- destroy the multi-hop channel
multihop_destroy_fn :: String -> C.Unit
multihop_destroy_fn ifn = C.FunctionDef C.NoScope C.Void (multihop_destroy_fn_name ifn) params [
    C.StmtList common_destroy,
    C.Ex $ C.Call "assert" [C.Variable "! \"NYI!\""] ]
    where
      params = [C.Param (C.Ptr $ C.Struct (m_bind_type ifn)) multihop_bind_var_name]
      common_destroy = binding_struct_destroy ifn (C.DerefField multihop_bind_var "b")

-- bind function
multihop_bind_fn :: String -> C.Unit
multihop_bind_fn ifn =
  C.FunctionDef C.NoScope (C.TypeName "errval_t") (m_bind_fn_name ifn) params 
  [
    localvar (C.TypeName "errval_t") "err" Nothing,
    C.Ex $ C.Call (multihop_init_fn_name ifn) [multihop_bind_var, C.Variable "waitset"],
    C.Ex $ C.Assignment (intf_bind_field "st") (C.Variable "st"),
    C.Ex $ C.Assignment (intf_bind_field "bind_cont") (C.Variable intf_cont_var),
    C.Ex $ C.Assignment errvar $ C.Call "multihop_chan_bind"
    [C.AddressOf $ multihop_bind_var `C.DerefField` "chan",
     C.StructConstant "multihop_bind_continuation"
     [("handler", C.Variable (multihop_bind_cont_fn_name ifn)),
      ("st", multihop_bind_var)],
     C.Variable "iref",
     C.Variable "waitset"],
    C.If (C.Call "err_is_fail" [errvar])
    [C.Ex $ C.Call (multihop_destroy_fn_name ifn) [multihop_bind_var]] [],
    C.Return errvar
  ]
    where 
      params = multihop_bind_params ifn
      intf_bind_field = C.FieldOf (C.DerefField multihop_bind_var "b")

-- bind continue function
multihop_bind_cont_fn :: String -> C.Unit
multihop_bind_cont_fn ifn =
    C.FunctionDef C.Static C.Void (multihop_bind_cont_fn_name ifn) params 
    [
      localvar (C.Ptr $ C.Struct $ m_bind_type ifn)
      multihop_bind_var_name (Just $ C.Variable "st"),
      C.SBlank,

      C.If (C.Call "err_is_ok" [errvar])
      [C.SComment "set receive handlers",
       C.Ex $ C.Call "multihop_chan_set_receive_handler" 
       [C.AddressOf $ C.DerefField multihop_bind_var "chan",
        C.StructConstant "multihop_receive_handler" 
        [("handler", C.Variable (rx_handler_name ifn)), 
         ("arg", C.Variable "st") ] ],
                
       C.Ex $ C.Call ("multihop_chan_set_caps_receive_handlers")
       [C.AddressOf $ C.DerefField multihop_bind_var "chan",
        C.StructConstant "monitor_cap_handlers"
        [("st", C.Variable "st"),
         ("cap_receive_handler", C.Variable (caps_rx_handler_name ifn))
        ]]]
      
      [ C.Ex $ C.Call (multihop_destroy_fn_name ifn) [multihop_bind_var]],
      C.SBlank,
      C.Ex $ C.CallInd (intf_var `C.FieldOf` "bind_cont")
      [intf_var `C.FieldOf` "st", errvar, C.AddressOf intf_var] 
    ]
      where 
        params = [C.Param (C.Ptr C.Void) "st",
                  C.Param (C.TypeName "errval_t") "err",
                  C.Param (C.Ptr $ C.Struct "multihop_chan") "chan"]
        intf_var = C.DerefField multihop_bind_var "b"
        errvar = C.Variable "err"
        chanaddr = C.Variable "chan"

multihop_connect_handler_fn :: String -> C.Unit
multihop_connect_handler_fn ifn = 
  C.FunctionDef C.NoScope (C.TypeName "errval_t")
    (drv_connect_handler_name "multihop" ifn) multihop_connect_handler_params 
  [
    localvar (C.Ptr $ C.Struct $ export_type ifn) "e" $ Just $ C.Variable "st",
    localvar (C.TypeName "errval_t") "err" Nothing,
    C.SBlank,

    C.SComment "allocate storage for binding",
    localvar (C.Ptr $ C.Struct $ m_bind_type ifn) multihop_bind_var_name
    $ Just $ C.Call "malloc" [C.SizeOfT $ C.Struct $ m_bind_type ifn],
    C.If (C.Binary C.Equals multihop_bind_var (C.Variable "NULL"))
    [C.Return $ C.Variable "LIB_ERR_MALLOC_FAIL"] [],
    C.SBlank,
    
    C.SComment "initialize binding",
    localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
    intf_bind_var (Just $ C.AddressOf $ multihop_bind_var `C.DerefField` "b"),
    C.Ex $ C.Call (multihop_init_fn_name ifn) [multihop_bind_var,
                                               exportvar `C.DerefField` "waitset"],
    C.Ex $ C.Assignment (C.FieldOf (C.DerefField  multihop_bind_var "chan") "vci")  (C.Variable "vci"),
    C.SBlank,

    C.SComment "run user's connect handler",
    C.Ex $ C.Assignment errvar $ C.CallInd (C.DerefField exportvar "connect_cb")
    [C.DerefField exportvar "st", bindvar],
    C.If (C.Call "err_is_fail" [errvar])
    [C.Return errvar ] [],
    C.SBlank,

    C.SComment "set receive handlers",	
    C.Ex $ C.Call ("multihop_chan_set_receive_handler") 
    [C.AddressOf $ C.DerefField multihop_bind_var "chan",
     C.StructConstant "multihop_receive_handler" 
     [("handler", C.Variable (rx_handler_name ifn)), 
      ("arg", multihop_bind_var) ] ],
                
                
    C.Ex $ C.Call ("multihop_chan_set_caps_receive_handlers")
    [C.AddressOf $ C.DerefField multihop_bind_var "chan",
     C.StructConstant "monitor_cap_handlers"
     [("st", multihop_bind_var),
      ("cap_receive_handler", C.Variable (caps_rx_handler_name ifn))
     ]],
    C.SBlank,
    
    C.SComment "send back bind reply",
    C.Ex $ C.Call ("multihop_chan_send_bind_reply")
    	 [C.AddressOf $ C.DerefField multihop_bind_var "chan", 
          C.Variable "SYS_ERR_OK" , 
          C.FieldOf (C.DerefField multihop_bind_var "chan") "vci", 
          C.FieldOf (C.DerefField multihop_bind_var "b") "waitset" ],
    
    C.SBlank,
    C.Return $ C.Variable "err"
  ]
    where
      exportvar = C.Variable "e"
      chanaddr = C.AddressOf $ C.DerefField multihop_bind_var "chan"

------------------------------------------------------------------------
-- Language mapping: Control functions
------------------------------------------------------------------------
m_can_send_fn_def :: String -> String -> C.Unit
m_can_send_fn_def drv ifn = 
    C.FunctionDef C.Static (C.TypeName "bool") (can_send_fn_name drv ifn) params [
       localvar (C.Ptr $ C.Struct $ m_bind_type ifn)
         multihop_bind_var_name (Just $ C.Cast (C.Ptr $ C.Struct $ m_bind_type ifn) (bindvar)),
        C.Return $ C.Binary C.And (C.Binary C.Equals (bindvar `C.DerefField` "tx_msgnum") (C.NumConstant 0))
        (C.Unary C.Not (C.Call "multihop_chan_is_window_full" [C.AddressOf $ C.DerefField multihop_bind_var "chan"]))
    ]
    where
        params = [ C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) "b" ]
        bindvar = C.Variable "b"


change_waitset_fn_def :: String -> C.Unit
change_waitset_fn_def ifn = 
    C.FunctionDef C.Static (C.TypeName "errval_t") (change_waitset_fn_name ifn) params [
        localvar (C.Ptr $ C.Struct $ m_bind_type ifn)
            multihop_bind_var_name (Just $ C.Cast (C.Ptr C.Void) bindvar),
        C.SBlank,

        C.SComment "change waitset on binding",
        C.Ex $ C.Assignment
            (bindvar `C.DerefField` "waitset")
            (C.Variable "ws"),
        C.SBlank,

        C.SComment "change waitset on multi-hop channel",
        C.Return $ C.Call "multihop_chan_change_waitset" [chanaddr, C.Variable "ws"]
    ]
    where
        chanaddr = C.AddressOf $ C.DerefField multihop_bind_var "chan"
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
                  C.Param (C.Ptr $ C.Struct "waitset") "ws"]

control_fn_def :: String -> C.Unit
control_fn_def ifn = 
    C.FunctionDef C.Static (C.TypeName "errval_t") (control_fn_name ifn) params [

      C.SComment "No control flags supported",
      C.Return $ C.Variable "SYS_ERR_OK"
      ]
    where
      params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
                C.Param (C.TypeName "idc_control_t") "control"]
   
------------------------------------------------------------------------
-- Language mapping: Send messages
------------------------------------------------------------------------
handler_preamble :: String -> C.Stmt
handler_preamble ifn = C.StmtList
    [C.SComment "Get the binding state from our argument pointer",
     localvar (C.Ptr $ C.Struct $ intf_bind_type ifn)
         intf_bind_var (Just $ C.Variable "arg"),
     localvar (C.Ptr $ C.Struct $ m_bind_type ifn)
         multihop_bind_var_name (Just $ C.Variable "arg"),
     C.SBlank
     ]

-- get all fixed size fragments
get_fixed_frags frags = [ x | x@(MsgFragment {}) <- frags ]
get_overflow_frags frags = [ x | x@(OverflowFragment {}) <- frags ]  

tx_handler :: Arch -> String -> MsgSpec -> C.Unit
tx_handler arch ifn (MsgSpec mn msgfrags caps) =

  C.FunctionDef C.Static C.Void (tx_handler_name ifn mn) [C.Param (C.Ptr C.Void) "arg"] 
  [
    handler_preamble ifn,
    localvar (C.TypeName "errval_t") "err" (Just $ C.Variable "SYS_ERR_OK"),
    localvar (C.Ptr $ wordsize_type) "msg" Nothing,
    localvar (C.TypeName "uint64_t") "msg_size" Nothing,
    
    -- we only need those variables of there are any dynamic arguments
    (if (not $ null (get_overflow_frags msgfrags)) then 
       C.StmtList 
       [localvar (C.Ptr $ C.TypeName "uint8_t") "msg2" Nothing,
        localvar (C.TypeName "uint64_t") "o_frag_size" Nothing]
     else C.SBlank),
    C.SBlank,

    C.SComment "Switch on current outgoing message fragment",
    C.Switch (C.DerefField bindvar "tx_msg_fragment") cases bad,
    C.SBlank,
    C.SBlank,

    C.SComment "Report error to user",
    report_user_tx_err errvar
  ]
    where
      
      -- case for unknown message fragment
      bad = [C.Ex $ C.Call "assert" [C.Unary C.Not $ C.StringConstant "invalid fragment"],
             C.Ex $ C.Assignment errvar (C.Variable "FLOUNDER_ERR_INVALID_STATE")]

      cases =
        [C.Case (C.NumConstant 0) (tx_frags arch ifn mn msgfrags)] 
        ++ [gen_last 1]
      
       -- generate the last case
      gen_last i
        | null caps = -- this message does not contain caps 
          C.Case (C.NumConstant $ toInteger $ i) 
          ([C.SComment "all fragments are sent",
            C.Ex $ C.Call "free" [C.DerefField multihop_bind_var "message"]]
           ++ [C.If (C.Call "multihop_chan_is_window_full" [C.AddressOf $ C.DerefField multihop_bind_var "chan"]) 
               [C.Ex $ C.Assignment (C.DerefField multihop_bind_var "trigger_chan") (C.Variable "true"),
                C.ReturnVoid]
               ([C.StmtList finished_send] ++ [C.ReturnVoid])])
        | otherwise = -- this message contains caps
            C.Case (C.NumConstant $ toInteger $ i)
            ([C.Ex $ C.Call "free" [C.DerefField multihop_bind_var "message"],
              C.SComment "send caps",
              C.Ex $ C.Call (cap_tx_handler_name ifn) [multihop_bind_var], C.ReturnVoid])       
      
      -- field for the message number
      tx_msgnum_field = C.DerefField bindvar "tx_msgnum"	

-- send an message (including all overflow fragemnts)
tx_frags :: Arch -> String -> String -> [MsgFragment] ->[C.Stmt]
tx_frags arch ifn mn frags  = 
  [
    C.SComment "Calculate size of message & allocate it",
    
    C.Ex $ C.Assignment (C.Variable "msg_size") 
    (C.NumConstant $ message_size (length words)) ,
    
    -- calculate size of strings
    C.StmtList
    [C.Ex $ C.Assignment (C.Variable "msg_size") 
     (C.Binary C.Plus (C.Binary C.Plus (C.Variable "msg_size") 
                       (C.Call "strlen" [argfield_expr TX mn argfield])) 
      (C.NumConstant (1 + m_size_type_bytes))) 
    | (OverflowFragment (StringFragment argfield)) <- frags ],
    
    -- calculate size of buffers
    C.StmtList
    [C.Ex $ C.Assignment (C.Variable "msg_size")
     (C.Binary C.Plus (C.Variable "msg_size") 
      (C.Binary C.Plus (argfield_expr TX mn l) (C.NumConstant m_size_type_bytes)))
    | (OverflowFragment (BufferFragment t d l)) <- frags ],
    
    -- make sure that the message size is not zero
    C.Ex $ C.Call "assert" [C.Binary C.NotEquals (C.Variable "msg_size") (C.NumConstant 0)],
    
    -- malloc message
    C.Ex $ C.Assignment (C.Variable "msg") (C.Call "malloc" [C.Variable "msg_size"]),
    C.SBlank,

    -- copy fixed size arguments
    C.SComment "copy words from fixed size fragments",
    C.SComment $ show words,
    C.StmtList
    [C.Ex $ C.Assignment (msgword n) (m_fragment_word_to_expr arch ifn mn (words !! n))
    | n <- [0 .. length words - 1]],
    (if (not $ null $ overflow_frags) then 
        C.Ex $ C.Assignment (C.Variable "msg2") 
        (C.Binary C.Plus (C.Cast (C.Ptr $ C.TypeName "uint8_t") (C.Variable "msg")) 
         (C.NumConstant $ message_size (length words)))
     else C.SBlank),
    C.SBlank,

    -- copy string arguments
    C.SComment "copy strings",
    C.StmtList $
    concat [ copy_string argfield  | (OverflowFragment (StringFragment argfield)) <- frags ],
    C.SBlank,

    --copy buffers
    C.SComment "copy buffers",
    C.StmtList $
    concat [ copy_buf b | (OverflowFragment b@(BufferFragment {})) <- frags],
    C.SBlank,
    
    -- send message
    C.SComment "try to send!",
    C.Ex $ C.PostInc $ C.DerefField bindvar "tx_msg_fragment",
    C.Ex $ C.Assignment (C.DerefField multihop_bind_var "message") (C.Variable "msg"),
    C.Ex $ C.Assignment (C.Variable "err") 
    (C.Call "multihop_send_message" [C.AddressOf $ C.DerefField multihop_bind_var "chan",  
                                     C.Call "MKCONT" [C.Variable $ (tx_handler_name ifn mn) , 
                                                      C.Variable intf_bind_var], 
                                     C.Variable "msg", 
                                     C.Variable "msg_size"]),
    
    -- make sure send was successful
    C.If (C.Binary C.Equals (C.Call "err_no" [errvar]) (C.Variable "FLOUNDER_ERR_TX_BUSY"))
    [C.Ex $ C.PostDec $ C.DerefField bindvar "tx_msg_fragment",
     C.Ex $ C.Assignment (C.Variable "err") 
     (C.Call "multihop_chan_register_send" [C.AddressOf $ C.DerefField multihop_bind_var "chan",  
                                            C.DerefField (C.Variable intf_bind_var) "waitset",
                                            C.Call "MKCONT" [C.Variable $ (tx_handler_name ifn mn) , 
                                                             C.Variable intf_bind_var]]),
      C.Ex $ C.Call "assert" [C.Call "err_is_ok" [errvar]]]
      [],
    C.If (C.Call "err_is_fail" [errvar]) [C.Break] [C.ReturnVoid]
  ]
    where
      
      words = concat [f | (MsgFragment f) <- frags]

      -- the fixed size fragments in this message
      fixed_frags = get_fixed_frags frags

      -- the overflow fragments in this message
      overflow_frags = get_overflow_frags frags

      msgword n = (C.Variable "msg") `C.SubscriptOf` (C.NumConstant $ toInteger n)

      message_size words = toInteger $ words * (wordsize m_arch `div` 8)
      
      -- copy a string
      copy_string :: ArgField -> [C.Stmt]  
      copy_string argfield = 
        [
          C.Ex $ C.Assignment (C.Variable "o_frag_size") 
          (C.Cast m_size_type (C.Call "strlen" [argfield_expr TX mn argfield])),
          C.Ex $ C.Call "memcpy" [C.Variable "msg2", 
                                  C.AddressOf $ C.Variable "o_frag_size", C.NumConstant m_size_type_bytes ],
          C.Ex $ C.Call "memcpy" [C.Binary C.Plus (C.Variable "msg2") 
                                  (C.NumConstant m_size_type_bytes), 
                                  argfield_expr TX mn argfield, 
                                  C.Variable "o_frag_size"],
          C.Ex $ C.Assignment (C.Variable "msg2") (C.Binary C.Plus 
                                                   (C.Binary C.Plus (C.Variable "msg2") 
                                                    (C.Variable "o_frag_size")) 
                                                   (C.NumConstant m_size_type_bytes))
        ]

      -- copy a buffer
      copy_buf :: OverflowFragment -> [C.Stmt]
      copy_buf (BufferFragment t d l) = 
          [
            C.Ex $ C.Assignment (C.Variable "o_frag_size") (C.Cast m_size_type (argfield_expr TX mn l)),
            C.Ex $ C.Call "memcpy" [C.Variable "msg2", 
                                    C.AddressOf $ C.Variable "o_frag_size", C.NumConstant m_size_type_bytes ],
            C.Ex $ C.Call "memcpy" [C.Binary C.Plus (C.Variable "msg2") 
                                    (C.NumConstant m_size_type_bytes), argfield_expr TX mn d, C.Variable "o_frag_size"],
            C.Ex $ C.Assignment (C.Variable "msg2") (C.Binary C.Plus 
                                                     (C.Binary C.Plus (C.Variable "msg2") 
                                                      (C.Variable "o_frag_size")) 
                                                     (C.NumConstant m_size_type_bytes))
          ]


tx_fn :: String -> [TypeDef] ->  MessageDef -> C.Unit
tx_fn ifn typedefs msg@(Message _ n args _) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (tx_fn_name ifn n) params body
    where
        params = [binding_param ifn, cont_param] ++ (
                    concat [ msg_argdecl TX ifn a | a <- args ])
        cont_param = C.Param (C.Struct "event_closure") intf_cont_var
        body = 
          [
            C.SComment "check that we can accept an outgoing message",
            C.If (C.Binary C.NotEquals tx_msgnum_field (C.NumConstant 0))
            [C.Return $ C.Variable "FLOUNDER_ERR_TX_BUSY"] [],
            C.SBlank,
            C.SComment "register send continuation",
            C.StmtList $ register_txcont (C.Variable intf_cont_var),
            C.SBlank,
            
            C.SComment "store message number and the arguments",
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

-- send virtual table
tx_vtbl :: String -> [MessageDef] -> C.Unit
tx_vtbl ifn ml =
    C.StructDef C.Static (intf_vtbl_type ifn TX) (multihop_vtbl_name ifn) fields
    where
        fields = [let mn = msg_name m in (mn, tx_fn_name ifn mn) | m <- ml]

-- send capabilities
tx_cap_handler :: Arch -> String -> [MsgSpec] -> C.Unit
tx_cap_handler arch ifn msgspecs = 
    C.FunctionDef C.Static C.Void (cap_tx_handler_name ifn) [C.Param (C.Ptr C.Void) "arg"] [
        handler_preamble ifn,
	localvar (C.TypeName "errval_t") "err" (Just (C.Variable "SYS_ERR_OK")),
        C.SBlank,

        C.SComment "Switch on current outgoing message",
        C.Switch (C.DerefField bindvar "tx_msgnum") cases
            [C.Ex $ C.Call "assert"
                    [C.Unary C.Not $ C.StringConstant "invalid message number"],
             report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")]
    ]
    where
        cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn)
                        (tx_cap_handler_case arch ifn mn (length frags) caps)
                 | MsgSpec mn frags caps <- msgspecs, caps /= []]


-- send caps of one message
tx_cap_handler_case :: Arch -> String -> String -> Int -> [CapFieldTransfer] -> [C.Stmt]
tx_cap_handler_case arch ifn mn nfrags caps = [
    C.SComment "Switch on current outgoing cap",
    C.Switch (capst `C.FieldOf` "tx_capnum") cases
            [C.Ex $ C.Call "assert"
                    [C.Unary C.Not $ C.StringConstant "invalid cap number"],
             report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")],
    C.Break]
    where
        capst = C.DerefField multihop_bind_var "capst"
        cases = [C.Case (C.NumConstant $ toInteger i) $ subcase cap i
                 | (cap, i) <- zip caps [0..]] ++ 
                [C.Case (C.NumConstant $ toInteger $ length caps) last_case]

        last_case = 
            -- if we've sent the last cap, and we've sent all the other fragments, we're done
          [C.If (C.Call "multihop_chan_is_window_full" [C.AddressOf $ C.DerefField multihop_bind_var "chan"]) 
               [C.Ex $ C.Assignment (C.DerefField multihop_bind_var "trigger_chan") (C.Variable "true"),
                C.Break]
               ([C.StmtList finished_send] ++ [C.Break])]
      
        tx_msgfrag_field = C.DerefField bindvar "tx_msg_fragment"

        subcase :: CapFieldTransfer -> Int -> [C.Stmt]
        subcase (CapFieldTransfer tm cap) ncap = [
            C.Ex $ C.Assignment errvar $ C.Call "multihop_send_capability"
                [C.AddressOf $ C.DerefField multihop_bind_var "chan", 
		C.Call "MKCONT" [C.Variable $ (cap_tx_handler_name ifn) , C.Variable intf_bind_var],
		C.AddressOf capst, argfield_expr TX mn cap],
            
            C.If (C.Binary C.Equals (C.Call "err_no" [errvar]) (C.Variable "FLOUNDER_ERR_TX_BUSY"))
            [C.Ex $ C.Assignment (C.Variable "err") 
             (C.Call "multihop_chan_register_send" [C.AddressOf $ C.DerefField multihop_bind_var "chan",  
                                                    C.DerefField (C.Variable intf_bind_var) "waitset",
                                                    C.Call "MKCONT" [C.Variable $ (cap_tx_handler_name ifn) , 
                                                                     C.Variable intf_bind_var]]),
             C.Ex $ C.Call "assert" [C.Call "err_is_ok" [errvar]]]
            [],
            
            C.If (C.Call "err_is_fail" [errvar])
                [report_user_tx_err errvar, C.Break] [],
            C.Break]

	tx_msgnum_field = C.DerefField bindvar "tx_msgnum"

------------------------------------------------------------------------
-- Language mapping: Receive messages
------------------------------------------------------------------------

rx_msgnum_field, rx_msgfrag_field, rx_capnum_field :: C.Expr
rx_msgnum_field = C.DerefField bindvar "rx_msgnum"
rx_msgfrag_field = C.DerefField bindvar "rx_msg_fragment"
rx_capnum_field = C.FieldOf (C.DerefField multihop_bind_var "capst") "rx_capnum"

rx_handler :: Arch -> String -> [TypeDef] -> [MessageDef] -> [MsgSpec] -> C.Unit
rx_handler arch ifn typedefs msgdefs msgs =
    C.FunctionDef C.NoScope C.Void (rx_handler_name ifn) multihop_rx_handler_params 
    [
      handler_preamble ifn,
      (if (contains_overflow_frags)
       then localvar m_size_type "o_frag_size" Nothing
       else C.SBlank),
      localvar (C.Ptr $ C.TypeName "uint8_t") "msg" Nothing,
      C.SBlank,
      
      C.SComment "if this a dummy message?",
      C.If (C.Binary C.Equals (C.Variable "message_len") (C.NumConstant 0))
      [C.If (C.DerefField multihop_bind_var "trigger_chan")
       [C.Ex $ C.Assignment (C.DerefField multihop_bind_var "trigger_chan") (C.Variable "false"),
        C.StmtList finished_send] [], C.ReturnVoid 
      ] [],
       
      C.SComment "is this the start of a new message?",
      C.If (C.Binary C.Equals rx_msgnum_field (C.NumConstant 0)) [
        C.SComment "unmarshall message number from first word, set fragment to 0",
        C.Ex $ C.Assignment rx_msgnum_field $
        C.Binary C.BitwiseAnd (C.SubscriptOf msgwords $ C.NumConstant 0) msgnum_mask,
        C.Ex $ C.Assignment rx_msgfrag_field (C.NumConstant 0),
        C.Ex $ C.Assignment rx_capnum_field (C.NumConstant 0)] 
      [C.Ex $ C.Call "assert" [C.Unary C.Not (C.Variable "\"should not happen\"") ]],
      C.SBlank,
      
      C.SComment "switch on message number",
      C.Switch rx_msgnum_field msgnum_cases bad_msgnum
    ]

    where
      
      contains_overflow_frags :: Bool
      contains_overflow_frags = not $ null $ concat $ map 
                                (\ ( MsgSpec _ frags _ ) -> [ f | f@(OverflowFragment _) <- frags]) msgs
      
      msgwords = C.Variable "message"
      msgnum_bits = bitsizeof_argfieldfrag arch MsgCode
      msgnum_mask = C.HexConstant ((shift 1 msgnum_bits) - 1)

      -- handle unknown msg
      bad_msgnum :: [C.Stmt]
      bad_msgnum = [report_user_err $ C.Variable "FLOUNDER_ERR_RX_INVALID_MSGNUM", C.ReturnVoid]

      -- generate code to receive the different message types
      msgnum_cases :: [C.Case]
      msgnum_cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn) 
		     (rx_handler_msg arch ifn typedefs msgdef msg)
                            | (msgdef, msg@(MsgSpec mn _ _)) <- zip msgdefs msgs]


-- generate code to receive one message type
rx_handler_msg :: Arch -> String -> [TypeDef] -> MessageDef -> MsgSpec -> [C.Stmt]
rx_handler_msg arch ifn typedefs msgdef (MsgSpec mn frags caps) =
    [
      C.SComment "store fixed size fragments",
      C.StmtList $ concat [store_arg_frags arch ifn mn msgExpr word 0 afl
                          | (afl, word) <- zip words [0..]],
      
      C.Ex $ C.Assignment (C.Variable "msg") (C.Binary C.Plus (C.Variable "message")
                                              (C.NumConstant $ message_size $ length words)),
      C.SBlank,

      C.SComment "receive strings",
      C.StmtList $
      concat [ receive_string s | (OverflowFragment s@(StringFragment {})) <- frags],
      C.SBlank,

      C.SComment "receive buffers",
      C.StmtList $
      concat [ receive_buf b | (OverflowFragment b@(BufferFragment {})) <- frags],
      C.SBlank,

      C.Ex $ C.Call "free" [C.Variable "message"],
      C.If (C.DerefField multihop_bind_var "trigger_chan")
      [C.Ex $ C.Assignment (C.DerefField multihop_bind_var "trigger_chan") (C.Variable "false"),
       C.StmtList finished_send] [],
      msgfrag_case_prolog ifn typedefs msgdef (null caps),
      C.Break
    ]
    
      where
        words = concat [f | (MsgFragment f) <- frags]

        msgExpr = C.Cast (C.Ptr $ wordsize_type) (C.Variable "message")
          
        -- calculate the size of the message from the number of words
        message_size :: Int -> Integer
        message_size words = toInteger $ words * (wordsize m_arch `div` 8)
                  
        -- handle invalide message fragment
	bad_msgfrag :: [C.Stmt]
        bad_msgfrag = [report_user_err $ C.Variable "FLOUNDER_ERR_INVALID_STATE", C.ReturnVoid]

        overflow_frags = get_overflow_frags frags
        fixed_frags = get_fixed_frags frags


        -- receive a string
	receive_string :: OverflowFragment -> [C.Stmt]
	receive_string (StringFragment argfield) = 
          [
            C.Ex $ C.Assignment (C.Variable "o_frag_size") (C.NumConstant 0),
            C.Ex $ C.Call "memcpy" [C.AddressOf $ C.Variable "o_frag_size", 
                                    C.Variable "msg",
                                    C.NumConstant m_size_type_bytes ],
            C.Ex $ C.Assignment (argfield_expr RX mn argfield)
            (C.Call "malloc" [C.Binary C.Plus (C.Variable "o_frag_size") (C.NumConstant 1)]),
            C.Ex $ C.Call "memcpy" [argfield_expr RX mn argfield, 
                                    C.Binary C.Plus (C.Variable "msg") 
                                    (C.NumConstant m_size_type_bytes),
                                    C.Variable "o_frag_size"],
            C.Ex $ C.Assignment (argfield_expr RX mn argfield `C.SubscriptOf` (C.Variable "o_frag_size")) (C.NumConstant 0),
            C.Ex $ C.Assignment (C.Variable "msg") (C.Binary C.Plus
                                                    (C.Binary C.Plus (C.Variable "msg") 
                                                     (C.Variable "o_frag_size"))
                                                    (C.NumConstant m_size_type_bytes))
          ]

	-- receive a buffer
	receive_buf :: OverflowFragment -> [C.Stmt]
	receive_buf (BufferFragment t d l) = 
          [
            C.Ex $ C.Assignment (C.Variable "o_frag_size") (C.NumConstant 0),
            C.Ex $ C.Call "memcpy" [C.AddressOf $ C.Variable "o_frag_size", 
                                    C.Variable "msg",
                                    C.NumConstant m_size_type_bytes ],
            C.Ex $ C.Assignment (argfield_expr RX mn d) 
            (C.Call "malloc" [C.Variable "o_frag_size"]),
            C.Ex $ C.Call "memcpy" [argfield_expr RX mn d, 
                                    C.Binary C.Plus (C.Variable "msg") 
                                    (C.NumConstant m_size_type_bytes),
                                    C.Variable "o_frag_size"],
            C.Ex $ C.Assignment (argfield_expr RX mn l) 
            (C.Cast (C.TypeName "size_t") (C.Variable "o_frag_size")), 
            C.Ex $ C.Assignment (C.Variable "msg") (C.Binary C.Plus
                                                    (C.Binary C.Plus (C.Variable "msg") 
                                                     (C.Variable "o_frag_size"))
                                                    (C.NumConstant m_size_type_bytes))
          ]

        msgfrag_case_prolog :: String -> [TypeDef] -> MessageDef -> Bool -> C.Stmt
        
        -- intermediate fragment
        msgfrag_case_prolog _ _ _ False
          = C.Ex $ C.PostInc $ C.DerefField bindvar "rx_msg_fragment"

        -- last fragment: call handler and zero message number
        msgfrag_case_prolog ifn typedefs (Message _ mn msgargs _) True
          = C.StmtList $ finished_recv drvname ifn typedefs mn msgargs
	  
-- receive caps
caps_rx_handler :: Arch -> String -> [TypeDef] -> [MessageDef] -> [MsgSpec] -> C.Unit
caps_rx_handler arch ifn typedefs msgdefs msgs =
    C.FunctionDef C.NoScope C.Void (caps_rx_handler_name ifn) multihop_caps_rx_handler_params 
    [
      handler_preamble ifn,
      C.SBlank,

      C.Ex $ C.Call "assert" [C.Binary C.Equals (C.Variable "capid") (capst `C.FieldOf` "rx_capnum")],
      C.SBlank, 
     
      C.SComment "Check if there's an associated error",
      C.SComment "FIXME: how should we report this to the user? at present we just deliver a NULL capref",
      C.If (C.Call "err_is_fail" [C.Variable "success"])
           [C.Ex $ C.Call "DEBUG_ERR" 
                 [C.Variable "success", C.StringConstant "could not send cap over multihop channel"]
           ] [],
      C.SBlank,

      C.SComment "Switch on current incoming message",
      C.Switch (C.DerefField bindvar "rx_msgnum") cases
      [C.Ex $ C.Call "assert"
       [C.Unary C.Not $ C.StringConstant "invalid message number"],
       report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")]
    ]
    where
      capst = C.DerefField multihop_bind_var "capst"
      cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn)
               (cap_rx_handler_case arch ifn typedefs mn msgdef (length frags) caps)
              | (MsgSpec mn frags caps, msgdef) <- zip msgs msgdefs, caps /= []]


-- receive the capabilities of one message
cap_rx_handler_case :: Arch -> String -> [TypeDef] -> String -> MessageDef -> Int -> [CapFieldTransfer] -> [C.Stmt]
cap_rx_handler_case arch ifn typedefs mn (Message _ _ msgargs _) nfrags caps = 
  [
    C.SComment "Switch on current incoming cap",
    C.Switch (C.PostInc $ capst `C.FieldOf` "rx_capnum") cases
    [C.Ex $ C.Call "assert"
     [C.Unary C.Not $ C.StringConstant "invalid cap number"],
     report_user_err (C.Variable "FLOUNDER_ERR_INVALID_STATE")],
    C.Break]
    where
      capst = C.DerefField multihop_bind_var "capst"
      
      cases = [C.Case (C.NumConstant $ toInteger i) $ subcase cap i
              | (cap, i) <- zip caps [0..]]

      subcase :: CapFieldTransfer -> Int -> [C.Stmt]
      subcase (CapFieldTransfer _ cap) ncap = [
        C.Ex $ C.Assignment (argfield_expr RX mn cap) (C.Variable "cap"),
        (if is_last then C.StmtList
           -- if this was the last cap, and we've received all the other fragments, we're done
           [(C.If (C.DerefField multihop_bind_var "trigger_chan")
             [C.Ex $ C.Assignment (C.DerefField multihop_bind_var "trigger_chan") (C.Variable "false"),
              C.StmtList finished_send] []),
            C.StmtList $ (finished_recv drvname ifn typedefs mn msgargs)]
         else C.StmtList []),
        C.Break]
        where
          rx_msgfrag_field = C.DerefField bindvar "rx_msg_fragment"
          
          is_last = (ncap + 1 == length caps)
