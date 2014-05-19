{-
   AHCI.hs: AHCI Backend implementation. Calls into libahci for disk access.

  Part of Flounder: a message passing IDL for Barrelfish

  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}

module AHCI where

import Data.Maybe
import Data.Either
import BackendCommon hiding (can_send_fn_def, register_send_fn_def)
import Syntax
import qualified Backend
import qualified GHBackend as GH
import qualified CAbsSyntax as C


-- handle printing of error values
ahci_err_fmt = C.NStr "PRIxERRV"
ahci_printf_error msg err = C.Ex $ C.Call "printf" [fmt, err]
    where fmt = C.StringCat [C.QStr (msg ++ ": 0x%"), ahci_err_fmt, C.QStr "\n"]

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

-- Name of the binding struct
ahci_bind_type :: String -> String
ahci_bind_type ifn = ifscope ifn "binding"

-- Name of command completed dispatcher
cc_rx_fn_name ifn mn = idscope ifn mn "completed"

ahci_intf_name ifn = "ahci_" ++ ifn

ahci_init_fn_name ifn = ifscope (ahci_intf_name ifn) "init"

-- Name of the transmit function
tx_fn_name ifn n = idscope ifn n "ahci_send"

ahci_vtbl_name ifn = ifscope ifn "ahci_tx_vtbl"

------------------------------------------------------------------------
-- Header
------------------------------------------------------------------------

header :: String -> String -> Interface -> String
header infile outfile interface@(Interface name _ _) =
    unlines $ C.pp_unit $ header_file
    where header_file = C.IfNDef sym ((C.Define sym [] "1") : body) []
          sym = "__" ++ name ++ "_AHCI_IF_H"
          body = ahci_header_file infile interface

ahci_header_file :: String -> Interface -> [C.Unit]
ahci_header_file infile interface@(Interface name descr decls) =
    let
        (types, messagedecls) = Backend.partitionTypesMessages decls
        rpcs = [ rpc | rpc@(RPC _ _ _) <- messagedecls ]
        rpc_msgs = concat $ map rpc_to_msgs rpcs
        rx_rpc_msgs = [ msg | msg@(Message MResponse _ _ _) <- rpc_msgs ]
        tx_rpc_msgs = [ msg | msg@(Message MCall _ _ _) <- rpc_msgs ]
        ahci_ifn = ahci_intf_name name
    in [
        intf_preamble infile ahci_ifn descr,
        C.Blank,

        C.Include C.Standard $ "ahci/ahci.h",
        C.Include C.Standard $ "if/" ++ name ++ "_defs.h",
        C.Blank,

        C.MultiComment [ "Forward declaration of binding type" ],
        C.StructForwardDecl (ahci_bind_type ahci_ifn),
        C.Blank,

        C.MultiComment [ "The binding structure" ],
        ahci_binding_struct name rpcs,
        C.Blank,

        C.MultiComment [ "Function to initialize an AHCI client" ],
        ahci_init_fn_proto name,

        C.Blank
        ]

ahci_binding_struct :: String -> [MessageDef] -> C.Unit
ahci_binding_struct ifn rpcs = C.StructDecl (intf_bind_type ahci_ifn) fields
    where
        ahci_ifn = ahci_intf_name ifn
        fields = [
            C.ParamComment "Binding supertype",
            C.Param (C.Struct $ intf_bind_type ifn) "b",
            C.ParamBlank,

            C.ParamComment "Binding to libahci",
            C.Param (C.Ptr $ C.Struct $ intf_bind_type "ahci") "b_lib",
            C.ParamBlank
            ]

ahci_init_fn_proto :: String -> C.Unit
ahci_init_fn_proto ifn =
    C.GVarDecl C.Extern C.NonConst
        (C.Function C.NoScope (C.TypeName "errval_t") params)
        (ahci_init_fn_name ifn) Nothing
        where
            params = [
                C.Param (C.Ptr $ C.Struct $ ahci_bind_type $ ahci_intf_name ifn) "binding",
                C.Param (C.Ptr $ C.Struct $ "waitset") "waitset",
                C.Param (C.Ptr $ C.Struct $ intf_bind_type "ahci") "ahci_binding"
                ]

---------------------------------------
-- Implementation
---------------------------------------

stub :: String -> String -> Interface -> String
stub infile outfile interface@(Interface name _ _) =
    unlines $ C.pp_unit $ C.UnitList $ ahci_stub_body infile interface

ahci_stub_body :: String -> Interface -> [C.Unit]
ahci_stub_body infile inf@(Interface ifn descr decls) =
    let
        (types, messagedecls) = Backend.partitionTypesMessages decls
        rpcs = [ rpc | rpc@(RPC _ _ _) <- messagedecls ]
        rpc_msgs = concat $ map rpc_to_msgs rpcs
        rx_rpc_msgs = [ msg | msg@(Message MResponse _ _ _) <- rpc_msgs ]
        tx_rpc_msgs = [ msg | msg@(Message MCall _ _ _) <- rpc_msgs ]
        ahci_ifn = ahci_intf_name ifn
    in [
        intf_preamble infile ifn descr,
        C.Blank,
        C.MultiComment [ "Generated Stub for AHCI" ],
        C.Blank,

        C.Include C.Standard "stdio.h",
        C.Include C.Standard "string.h",
        C.Include C.Standard "barrelfish/barrelfish.h",
        C.Include C.Standard "flounder/flounder_support.h",
        C.Include C.Standard "ahci/ahci_dma_pool.h",
        C.Include C.Standard "ahci/ahci_util.h",
        C.Include C.Standard "ahci/sata_fis.h",
        C.Include C.Standard ("if/" ++ ifn ++ "_ahci_defs.h"),
        C.Blank,

        C.MultiComment [ "Forward decleration of state struct" ],
        completed_rx_struct_decl,
        C.Blank,

        C.MultiComment [ "Command completed handler signature" ],
        completed_rx_typedef,
        C.Blank,

        C.MultiComment [ "Command dispatch and completion state struct" ],
        completed_rx_struct ifn,
        C.Blank,

        C.MultiComment [ "Debug printf" ],
        C.HashIf "defined(FLOUNDER_AHCI_DEBUG) || defined(FLOUNDER_DEBUG) || defined(GLOBAL_DEBUG)"
            [C.Define "AHCI_DEBUG" ["x..."] "printf(\"ahci_flounder: \" x)"]
            [C.Define "AHCI_DEBUG" ["x..."] "((void)0)"],
        C.Blank,

        C.MultiComment [ "Receiver functions for AHCI" ],
        ahci_command_completed_rx,
        C.UnitList [ cc_rx_fn ifn types msg | msg <- rpcs ],
        C.Blank,

        C.MultiComment [ "Command issue callback for freeing resources" ],
        issue_command_cb_fn,
        C.Blank,

        C.MultiComment [ "Message sender functions" ],
        C.UnitList [ tx_fn ifn types msg | msg <- rpcs ],
        C.Blank,

        C.MultiComment [ "Send vtable" ],
        tx_vtbl inf,
        C.Blank,

        C.MultiComment [ "Control functions" ],
        can_send_fn_def inf,
        register_send_fn_def inf,
        default_error_handler_fn_def "ahci" ifn,
        change_waitset_fn_def inf,

        C.MultiComment [ "Binding initialization function" ],
        ahci_init_fn inf,
        C.Blank
        ]

completed_rx_struct_n = "completed_rx_st"
completed_rx_struct_type = C.Struct completed_rx_struct_n
completed_rx_struct_decl :: C.Unit
completed_rx_struct_decl = C.StructForwardDecl completed_rx_struct_n
completed_rx_struct :: String -> C.Unit
completed_rx_struct ifn = C.StructDecl completed_rx_struct_n fields
    where
        fields = [
            C.ParamComment "Callback for handling message-specifics for command completion",
            C.Param (C.Ptr $ C.TypeName completed_rx_typedef_n) "completed_fn",
            C.ParamComment ("The " ++ ifn ++ " ahci binding"),
            C.Param (C.Ptr $ C.Struct $ intf_bind_type ahci_ifn) (ahci_ifn ++ "_binding"),
            C.ParamComment "The DMA region associated with this command, if any",
            C.Param (C.Ptr $ C.Struct "ahci_dma_region") "dma_region",
            C.ParamComment "Number of bytes in DMA region",
            C.Param (C.TypeName "size_t") "bytes",
            C.ParamBlank,
            C.ParamComment "Command fis",
            C.Param (C.Ptr C.Void) "fis",
            C.ParamComment "User's dispatch continuation",
            C.Param (C.Struct "event_closure") "dispatch_continuation"
            ]
        ahci_ifn = ahci_intf_name ifn

completed_rx_typedef_n = "completed_rx_fn_t"
completed_rx_typedef :: C.Unit
completed_rx_typedef = C.TypeDef (C.Function C.NoScope C.Void params) completed_rx_typedef_n
    where
        params = [
            binding_param "ahci",
            C.Param (C.Ptr completed_rx_struct_type) "completed_st"
            ]

ahci_command_completed_rx_name = "ahci_command_completed__rx"
ahci_command_completed_rx =
    C.FunctionDef C.Static (C.Void) ahci_command_completed_rx_name params body
    where
        params = [
            binding_param "ahci",
            C.Param (C.Ptr C.Void) "tag"
            ]
        body :: [C.Stmt]
        body = [
            localvar (C.Ptr completed_rx_struct_type) "st" $
                Just $ C.Cast (C.Ptr completed_rx_struct_type) $ C.Variable "tag",
            C.Ex $ C.CallInd (C.DerefField (C.Variable "st") "completed_fn") [
                bindvar,
                C.Variable "st"
                ],
            C.Ex $ C.Call "free" [C.Variable "st"]
            ]

issue_command_cb_fn_n = "issue_command_cb"
issue_command_cb_fn :: C.Unit
issue_command_cb_fn = C.FunctionDef C.Static C.Void issue_command_cb_fn_n params body
    where
        params = [C.Param (C.Ptr C.Void) "arg"]
        body = [
            localvar (C.Ptr completed_rx_struct_type) "st" Nothing,
            C.Ex $ C.Assignment (C.Variable "st") $ C.Cast (C.Ptr completed_rx_struct_type) $ C.Variable "arg",
            C.Ex $ C.Call "free" [st_field "fis"],
            C.Ex $ C.Assignment (st_field "fis") (C.Variable "NULL"),
            C.SComment "XXX: use waitset_chan_trigger_closure?",
            C.If (cont) [
                C.Ex $ C.CallInd cont [cont_arg]
                ] []
            ]
        st_field n = C.Variable "st" `C.DerefField` n
        cont = C.FieldOf (st_field "dispatch_continuation") "handler"
        cont_arg = C.FieldOf (st_field "dispatch_continuation") "arg"

can_send_fn_def :: Interface -> C.Unit
can_send_fn_def inf@(Interface ifn descr decls) = C.FunctionDef C.Static (C.TypeName "bool") name params body
    where
        name = (can_send_fn_name "ahci" ifn)
        params = [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var]
        body = [
            let bind_ptr_type = C.Ptr $ C.Struct $ ahci_bind_type $ ahci_intf_name ifn
                in localvar bind_ptr_type "b" $ Just $ C.Cast bind_ptr_type gen_bind_var,
            C.Return $ C.CallInd (lib_bind_var `C.DerefField` "can_send") [lib_bind_var]
            ]
        gen_bind_var = C.Variable intf_bind_var
        ahci_bind_var = C.Variable "b"
        lib_bind_var = ahci_bind_var `C.DerefField` "b_lib"

register_send_fn_def :: Interface -> C.Unit
register_send_fn_def inf@(Interface ifn descr decls) = C.FunctionDef C.Static (C.TypeName "errval_t") name params body
    where
        name = (register_send_fn_name "ahci" ifn)
        params = [
            C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
            C.Param (C.Ptr $ C.Struct "waitset") "waitset",
            C.Param (C.Struct "event_closure") intf_cont_var
            ]
        body = [
            let bind_ptr_type = C.Ptr $ C.Struct $ ahci_bind_type $ ahci_intf_name ifn
                in localvar bind_ptr_type "b" $ Just $ C.Cast bind_ptr_type gen_bind_var,
            C.Return $ C.CallInd (lib_bind_var `C.DerefField` "register_send") [
                lib_bind_var,
                C.Variable "waitset",
                C.Variable intf_cont_var
                ]
            ]
        gen_bind_var = C.Variable intf_bind_var
        ahci_bind_var = C.Variable "b"
        lib_bind_var = ahci_bind_var `C.DerefField` "b_lib"

change_waitset_fn_def :: Interface -> C.Unit
change_waitset_fn_def inf@(Interface ifn descr decls) = C.FunctionDef C.Static (C.TypeName "errval_t") name params body
    where
        name = ifscope (ahci_intf_name ifn) "change_waitset"
        params = [
            C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
            C.Param (C.Ptr $ C.Struct "waitset") "ws"
            ]
        body = [
            let bind_ptr_type = C.Ptr $ C.Struct $ ahci_bind_type $ ahci_intf_name ifn
                in localvar bind_ptr_type "b" $ Just $ C.Cast bind_ptr_type gen_bind_var,
            C.SBlank,

            C.SComment $ "change waitset on binding",
            C.Ex $ C.Assignment (gen_bind_var `C.DerefField` "waitset") $ C.Variable "ws",
            C.Ex $ C.CallInd (lib_bind_var `C.DerefField` "change_waitset") [lib_bind_var, C.Variable "ws"],
            C.SBlank,

            C.Return $ C.Variable "SYS_ERR_OK"
            ]
        gen_bind_var = C.Variable intf_bind_var
        ahci_bind_var = C.Variable "b"
        lib_bind_var = ahci_bind_var `C.DerefField` "b_lib"

rpc_arg :: [RPCArgument] -> String -> Maybe RPCArgument
rpc_arg rpcargs n = listToMaybe $ filter ((== n) . rpc_arg_var_name . rpc_arg_var) rpcargs
    where rpc_arg_var (RPCArgIn _ v) = v
          rpc_arg_var (RPCArgOut _ v) = v
          rpc_arg_var_name (Name n) = n
          rpc_arg_var_name (DynamicArray n _) = n

get_meta_arg :: String -> String -> [(String, [(String, MetaArgument)])] -> Maybe MetaArgument
get_meta_arg nspc n metaargs = (lookup nspc metaargs) >>= (lookup n)

has_meta_arg :: String -> String -> [(String, [(String, MetaArgument)])] -> Bool
has_meta_arg nspc n metaargs = isJust $ get_meta_arg nspc n metaargs

meta_arg :: String -> String -> [(String, [(String, MetaArgument)])] -> MetaArgument
meta_arg nspc n metaargs =
    case get_meta_arg nspc n metaargs of
        Just v  -> v
        Nothing -> error $ "missing meta-argument " ++ n

rpc_dma_arg_name :: MessageDef -> String
rpc_dma_arg_name rpc@(RPC _ rpcargs metaargs) = case meta_arg "ata" "dma_arg" metaargs of
    (BackendMsgArg n) -> if isJust $ rpc_arg rpcargs n then n else error ("invalid dma argument " ++ n)
    _                 -> error "dma_arg must refer to a message argument"

rpc_dma_direction rpc@(RPC _ rpcargs _) = case fromJust $ rpc_arg rpcargs $ rpc_dma_arg_name rpc of
    (RPCArgIn _ _) -> TX
    (RPCArgOut _ _) -> RX

rpc_dma_args :: [TypeDef] -> MessageDef -> Maybe (Either C.Expr C.Expr, C.Expr)
rpc_dma_args types rpc@(RPC name rpcargs metaargs) =
    if not $ has_meta_arg "ata" "dma_arg" metaargs
       then Nothing
       else Just $ case rpc_dma_direction rpc of
                     TX -> (Left dma_arg_var, dma_arg_in_length_var)
                     RX -> (Right dma_arg_var, dma_arg_out_length_var)
    where dma_arg_var = C.Variable $ rpc_dma_arg_name rpc
          dma_arg_in_length_var = take_dma_size $ catMaybes dma_in_size_sources
          dma_arg_out_length_var = take_dma_size $ catMaybes dma_out_size_sources
          dma_in_size_sources = [
              dma_dyn_arg_size,
              dma_arg_type_size,
              meta_arg_dma_size
              ]
          dma_out_size_sources = [
              dma_arg_type_size,
              meta_arg_dma_size
              ]
          dma_dyn_arg_size = case rpc_arg rpcargs $ rpc_dma_arg_name rpc of
              Just (RPCArgIn (Builtin UInt8) (DynamicArray _ l)) -> Just $ C.Variable l
              _                                                  -> Nothing
          dma_arg_type_size = case lookup_typeref types $ rpc_arg_type $ fromJust $ rpc_arg rpcargs $ rpc_dma_arg_name rpc of
              TArray (Builtin UInt8) _ length -> Just $ C.NumConstant length
              _                               -> Nothing
          meta_arg_dma_size = case get_meta_arg "ata" "dma_size" metaargs of
              Nothing                -> Nothing
              Just (BackendInt v)    -> Just $ C.NumConstant v
              Just (BackendMsgArg n) -> case rpc_arg rpcargs n of
                                            Nothing -> rpc_error $ "unkown dma size argument " ++ n
                                            Just (RPCArgIn _ _)  -> Just $ C.Variable n
                                            Just (RPCArgOut _ _) -> rpc_error "dma size arg must be input argument"
          rpc_arg_type (RPCArgIn t _) = t
          rpc_arg_type (RPCArgOut t _) = t
          take_dma_size xs = case xs of
              (x:[]) -> x
              []     -> rpc_error "unable to determine dma_size"
              _      -> rpc_error "dma_size is ambiguous"
          rpc_error msg = error (msg ++ " for RPC " ++ name)

cc_rx_fn :: String -> [TypeDef] -> MessageDef -> C.Unit
cc_rx_fn ifn types msg@(RPC name rpcargs metaargs) =
    C.FunctionDef C.Static C.Void (cc_rx_fn_name ifn name) params body
    where
        params = [
            binding_param "ahci",
            C.Param (C.Ptr completed_rx_struct_type) "completed_st"
            ]
        body = [
            localvar ahci_bind_type "b" $ Just $ st_var `C.DerefField` (ifscope ahci_ifn "binding"),
            C.SBlank,

            C.Ex $ C.Call "AHCI_DEBUG" [C.StringConstant "entering %s\n", C.Variable "__func__"],
            C.SBlank,

            case dma_dir_m of
                Just RX -> C.StmtList [
                    localvar (C.Ptr $ C.TypeName "uint8_t") dma_data_name $ Just $ C.Call "malloc" [dma_size],
                    C.Ex $ C.Call "ahci_dma_region_copy_out" [pr_region_var, C.Variable dma_data_name, C.NumConstant 0, dma_size],
                    C.SBlank
                    ]
                otherwise -> C.StmtList [],

            C.Ex $ C.CallInd (C.FieldOf vtbl $ rpc_resp_name name) $ [C.AddressOf gen_binding] ++ (concat $ map (output_arg_expr dma_dir_m) outargs),

            if has_dma
                then C.StmtList [
                    C.SBlank,
                    C.SComment "free dma region",
                    C.Ex $ C.Call "ahci_dma_region_free" [pr_region_var]
                    ]
                else C.StmtList []
            ]
        ahci_ifn = ahci_intf_name ifn
        ahci_bind_type = C.Ptr $ C.Struct $ intf_bind_type ahci_ifn

        st_var = C.Variable "completed_st"
        ahci_binding = C.Variable "b"
        gen_binding = ahci_binding `C.DerefField` "b"
        lib_binding = C.Variable intf_bind_var
        dma_data_name = "_data"

        (_, outargs) = partition_rpc_args rpcargs
        vtbl = gen_binding `C.FieldOf` "rx_vtbl"
        pr_region_var = C.Variable "completed_st" `C.DerefField` "dma_region"
        output_arg_expr :: Maybe Direction -> MessageArgument -> [C.Expr]
        output_arg_expr _ (Arg (TypeAlias "errval" _) (Name "status")) = [C.Variable "SYS_ERR_OK"]
        output_arg_expr (Just RX) (Arg (Builtin UInt8) (DynamicArray _ _)) = [C.Variable dma_data_name, dma_size]
        output_arg_expr _ arg = error ("unrecoginized output argument " ++ (show arg))

        dma_args = rpc_dma_args types msg
        has_dma = isJust dma_args
        dma_dir_m = if has_dma then Just dma_direction else Nothing
        -- following variables should only be used if has_dma == True
        --dma_size = snd $ fromJust dma_args
        dma_size = C.Variable "completed_st" `C.DerefField` "bytes"
        dma_direction = rpc_dma_direction msg
        dma_arg = head $ rights [fst $ fromJust dma_args]

tx_fn :: String -> [TypeDef] -> MessageDef -> C.Unit
tx_fn ifn types msg@(RPC name rpcargs metaargs) =
    C.FunctionDef C.Static (C.TypeName "errval_t") (tx_fn_name ifn $ rpc_call_name name) params body
    where
        ahci_ifn = ahci_intf_name ifn
        (txargs, _) = partition_rpc_args rpcargs
        params = [binding_param ifn, cont_param] ++ (concat $ map (msg_argdecl TX ifn) txargs)
        cont_param = C.Param (C.Struct "event_closure") intf_cont_var
        unused s = C.Ex $ C.Cast C.Void $ C.Variable s
        body = [
            localvar (C.TypeName "errval_t") "err" $ Just $ C.NumConstant 0,
            let bind_ptr_type = C.Ptr $ C.Struct $ ahci_bind_type ahci_ifn
                in localvar bind_ptr_type "b" $ Just $ C.Cast bind_ptr_type gen_bind_var,
            C.SBlank,
            C.Ex $ C.Call "AHCI_DEBUG" [C.StringConstant "entering %s\n", C.Variable "__func__"],
            C.SBlank,

            C.SComment "allocate state structure",
            localvar (C.Ptr completed_rx_struct_type) completed_st_var_n $ Just $ C.Call "calloc" [ C.NumConstant 1, C.SizeOfT completed_rx_struct_type ],
            C.If (C.Unary C.Not completed_st_var) [
                C.Ex $ C.Assignment errvar $ C.Variable "LIB_ERR_MALLOC_FAIL",
                C.Goto "cleanup"
                ] [],
            C.Ex $ C.Assignment (completed_st_var `C.DerefField` "completed_fn") $ C.Variable $ cc_rx_fn_name ifn name,
            C.Ex $ C.Assignment (completed_st_var `C.DerefField` (ahci_ifn ++ "_binding")) $ ahci_bind_var,
            C.Ex $ C.Assignment (completed_st_var `C.DerefField` "dispatch_continuation") $ C.Variable intf_cont_var,
            C.SBlank,

            C.SComment "determine sector size",
            localvar (C.TypeName "size_t") "sector_size" $ Just $ C.NumConstant 512,
            let identify = C.AddressOf $ libahci_bind_var `C.DerefField` "identify"
                in C.If (C.Call "ata_identify_plss_lls_rdf" [identify]) [
                       C.Ex $ C.Assignment (C.Variable "sector_size") $ C.Binary C.Times (C.NumConstant 2) (C.Call "ata_identify_wpls_rd" [identify])
                       ] [],
            C.SBlank,

            if has_dma
                then C.StmtList [
                    C.Ex $ C.Assignment dma_size_var dma_size,
                    C.SBlank,

                    C.SComment "determine sector count",
                    localvar (C.TypeName "size_t") "dma_count" Nothing,
                    let round_down_expr = C.Binary C.Divide dma_size_var $ C.Variable "sector_size"
                        round_up_expr = C.Call "CEIL_DIV" [dma_size_var, C.Variable "sector_size"]
                        assign var x = C.Ex $ C.Assignment var x
                    in meta_bool_arg_if "ata" "is_write" [
                        -- writes must be rounded down, everything else can be rounded up
                        assign (C.Variable "dma_count") round_down_expr,
                        C.SComment "recalculate read size to match rounded down sector count",
                        assign dma_size_var $ C.Binary C.Times dma_count_var $ C.Variable "sector_size"
                        ] [
                        assign (C.Variable "dma_count") round_up_expr
                        ],
                    C.SBlank,

                    C.SComment "determine size of DMA region, which must be a multiple of the sector count",
                    localvar (C.TypeName "size_t") "dma_region_size" $ Just $ C.Binary C.Times dma_count_var $ C.Variable "sector_size",
                    C.SBlank,

                    C.SComment "setup DMA region",
                    C.Ex $ C.Assignment errvar $ C.Call "ahci_dma_region_alloc" [ (C.Variable "dma_region_size"), C.AddressOf pr_region_var ],
                    C.If (C.Call "err_is_fail" [errvar]) [
                        ahci_printf_error "alloc_region failed" errvar,
                        C.Goto "cleanup"
                        ] [],
                    C.SBlank
                    ]
                else C.StmtList [],

            if has_dma && (dma_direction == TX)
                then C.StmtList [
                    C.SComment "copy in DMA data",
                    C.Ex $ C.Call "ahci_dma_region_copy_in" [
                            pr_region_var,
                            C.Cast (C.Ptr C.Void) dma_arg,
                            C.NumConstant 0,
                            dma_size_var
                        ],
                    C.SBlank
                    ]
                else C.StmtList [],

            C.SComment "setup FIS",
            localvar (C.TypeName "size_t") fis_size_var_n Nothing,
            C.Ex $ C.Assignment errvar $ C.Call "sata_alloc_h2d_register_fis" [(C.AddressOf fis_var), (C.AddressOf $ C.Variable fis_size_var_n)],
            C.If (C.Call "err_is_fail" [errvar]) [
                ahci_printf_error "sata_alloc_h2d_register_fis failed" errvar,
                C.Goto "cleanup"
                ] [],
            C.Ex $ C.Call "sata_set_command" [fis_var, meta_arg_expr_hex "ata" "command"],
            if has_dma
                then C.Ex $ C.Call "sata_set_count" [fis_var, C.Variable "dma_count"]
                else C.StmtList [],
            if has_meta_arg "ata" "lba" metaargs
                then C.Ex $ C.Call "sata_set_lba28" [fis_var, meta_arg_expr "ata" "lba"]
                else C.StmtList [],
            C.SBlank,

            C.SComment "issue command",
            C.Ex $ C.Assignment errvar $ C.Call "ahci_issue_command" [
                libahci_bind_var,
                C.Call "MKCLOSURE" [C.Variable issue_command_cb_fn_n, completed_st_var],
                completed_st_var,
                C.Cast (C.Ptr $ C.TypeName "uint8_t") fis_var,
                C.Variable fis_size_var_n,
                if has_meta_arg "ata" "is_write" metaargs then meta_arg_expr "ata" "is_write" else C.Variable "false",
                if has_dma then pr_region_var else C.Variable "NULL",
                if has_dma then dma_size_var else C.NumConstant 0
                ],
            C.If (C.Call "err_is_fail" [errvar]) [
                ahci_printf_error "ahci_issue_command failed"  errvar,
                C.Goto "cleanup"
                ] [],
            C.SBlank,

            C.Return $ C.Variable "SYS_ERR_OK",
            C.SBlank,

            C.Label "cleanup",
            C.SBlank,

            C.SComment "free memory",
            C.If (completed_st_var) [
                C.If (fis_var) [
                        C.Ex $ C.Call "free" [fis_var]
                    ] [],
                C.If (pr_region_var) [
                        C.Ex $ C.Call "ahci_dma_region_free" [pr_region_var]
                    ] [],
                C.Ex $ C.Call "free" [completed_st_var]
                ] [],
            C.SBlank,

            C.Return errvar
            ]

        dma_args = rpc_dma_args types msg
        has_dma = isJust dma_args
        -- following variables should only be used if has_dma == True
        dma_size = snd $ fromJust dma_args
        dma_direction = rpc_dma_direction msg
        dma_arg = head $ lefts [fst $ fromJust dma_args]

        completed_st_var_n = "completed_st"
        completed_st_var = C.Variable completed_st_var_n
        pr_region_var = completed_st_var `C.DerefField` "dma_region"
        dma_size_var = completed_st_var `C.DerefField` "bytes"
        dma_count_var = C.Variable "dma_count"
        fis_var = completed_st_var `C.DerefField` "fis"
        fis_size_var_n = "fis_size"

        gen_bind_var = C.Variable intf_bind_var
        ahci_bind_var = C.Variable "b"
        libahci_bind_var = ahci_bind_var `C.DerefField` "b_lib"

        meta_arg_expr_conv conv nspc n = case meta_arg nspc n metaargs of
            (BackendInt value)    -> conv value
            (BackendMsgArg ident) -> case rpc_arg rpcargs ident of
                Just (RPCArgIn _ _) -> C.Variable ident
                _                   -> error ("meta-argument " ++ n ++ " must refer to an input argument")
        meta_arg_expr = meta_arg_expr_conv C.NumConstant
        meta_arg_expr_hex = meta_arg_expr_conv C.HexConstant
        assign_fis n expr = C.Ex $ C.Assignment (C.FieldOf fis_var n) expr
        shift_right n expr = C.Binary C.RightShift expr (C.NumConstant n)
        bitwise_and n expr = C.Binary C.BitwiseAnd expr $ C.HexConstant n
        meta_bool_arg_if nspc n true_stmts false_stmts =
            if has_meta_arg nspc n metaargs
            then case meta_arg nspc n metaargs of
                (BackendInt value)    -> C.StmtList $ if value /= 0 then true_stmts else false_stmts
                (BackendMsgArg ident) -> C.If (meta_arg_expr nspc n) true_stmts false_stmts
            else C.StmtList false_stmts


tx_vtbl :: Interface -> C.Unit
tx_vtbl interface@(Interface ifn descr decls) =
    C.StructDef C.Static (intf_vtbl_type ifn TX) (ahci_vtbl_name ifn) fields
    where
        (types, messagedecls) = Backend.partitionTypesMessages decls
        fields = concat $ map assn_msg_handlers messagedecls
        assn_msg_handlers (Message _ mn _ _) = [(mn, "NULL")]
        assn_msg_handlers (RPC rpcn _ _) = [(rpc_call_name rpcn, tx_fn_name ifn $ rpc_call_name rpcn),
                                            (rpc_resp_name rpcn, "NULL")]

ahci_init_fn :: Interface -> C.Unit
ahci_init_fn intf@(Interface ifn descr decls) =
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (ahci_init_fn_name ifn) params body
    where
        params = [
            C.Param (C.Ptr $ C.Struct (intf_bind_type $ ahci_intf_name ifn)) "binding",
            C.Param (C.Ptr $ C.Struct "waitset") "waitset",
            C.Param (C.Ptr $ C.Struct (intf_bind_type "ahci")) "ahci_binding"
            ]
        body = [
            localvar (C.TypeName "errval_t") "err" $ Just $ C.Variable "SYS_ERR_OK",
            C.SBlank,

            C.StmtList $ binding_struct_init "ahci" ifn gen_binding (C.Variable "waitset") (C.Variable $ ahci_vtbl_name ifn),
            C.Ex $ C.Assignment (gen_binding `C.FieldOf` "change_waitset") $ C.Variable $ ifscope (ahci_intf_name ifn) "change_waitset",
            C.SBlank,

            C.Ex $ C.Assignment lib_binding (C.Variable "ahci_binding"),
            C.Ex $ C.CallInd (lib_binding `C.DerefField` "change_waitset") [lib_binding, C.Variable "waitset"],
            C.SBlank,

            C.Ex $ C.Assignment (lib_binding `C.DerefField` "rx_vtbl" `C.FieldOf` "command_completed") (C.Variable ahci_command_completed_rx_name),
            C.SBlank,

            C.SComment "initialize DMA buffer pool with 1M space",
            C.Ex $ C.Call "ahci_dma_pool_init" [ C.NumConstant (1024 * 1024) ],
            C.SBlank,

            C.Return $ errvar
            ]
        ahci_binding = C.Variable "binding"
        gen_binding = ahci_binding `C.DerefField` "b"
        lib_binding = ahci_binding `C.DerefField` "b_lib"
