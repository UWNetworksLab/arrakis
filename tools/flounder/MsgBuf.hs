{- 
  MsgBuf.hs: Flounder stub generator for marshalling into / out of in-memory
      message buffers

  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module MsgBuf where

import qualified CAbsSyntax as C
import qualified Backend
import BackendCommon hiding (errvar)
import Syntax

------------------------------------------------------------------------
-- Language mapping: C identifier names
------------------------------------------------------------------------

-- Name of the marshall functions
tx_fn_name ifn mn = idscope ifn mn "msgbuf_marshall"
rx_fn_name ifn = ifscope ifn "msgbuf_dispatch"

msgbuf_name = "_msg"
msgbuf_var = C.Variable msgbuf_name

errvar_name = "_err"
errvar = C.Variable errvar_name

------------------------------------------------------------------------
-- Language mapping: Create the header file for this interconnect driver
------------------------------------------------------------------------

header :: String -> String -> Interface -> String
header infile outfile intf = 
    unlines $ C.pp_unit $ header_file intf (header_body infile intf)
    where
        header_file :: Interface -> [C.Unit] -> C.Unit
        header_file interface@(Interface name _ _) body = 
            let sym = "__" ++ name ++ "_MSGBUF_STUB_H"
            in C.IfNDef sym ([ C.Define sym [] "1"] ++ body) []

header_body :: String -> Interface -> [C.Unit]
header_body infile interface@(Interface ifn descr decls) = [
    intf_preamble infile ifn descr,
    C.Blank,
    C.MultiComment [ "Generic message buffer marshalling header" ],
    C.Blank,
    C.Include C.Standard ("if/" ++ ifn ++ "_defs.h"),
    C.Blank,
    C.UnitList [ tx_fn_proto ifn m | m <- msgs ],
    C.Blank,
    rx_fn_proto ifn]
    where
        (types, msgs) = Backend.partitionTypesMessages decls

tx_fn_proto :: String -> MessageDef -> C.Unit
tx_fn_proto ifn msg = 
    C.GVarDecl C.NoScope C.NonConst
         (C.Function C.NoScope (C.TypeName "errval_t") (tx_fn_params ifn msg))
         (tx_fn_name ifn (msg_name msg)) Nothing

tx_fn_params :: String -> MessageDef -> [C.Param]
tx_fn_params ifn (Message _ _ args _)
    = [ C.Param (C.Ptr $ C.Struct "msgbuf") msgbuf_name ]
      ++ concat [ msg_argdecl TX ifn a | a <- args ]

rx_fn_proto :: String -> C.Unit
rx_fn_proto ifn = 
    C.GVarDecl C.NoScope C.NonConst 
         (C.Function C.NoScope (C.TypeName "errval_t") (rx_fn_params ifn))
         (rx_fn_name ifn) Nothing

rx_fn_params :: String -> [C.Param]
rx_fn_params ifn =
    [C.Param (C.Ptr $ C.Struct $ intf_bind_type ifn) intf_bind_var,
     C.Param (C.Ptr $ C.Struct "msgbuf") msgbuf_name]

------------------------------------------------------------------------
-- Language mapping: Create the stub (implementation) for this interconnect driver
------------------------------------------------------------------------

stub :: String -> String -> Interface -> String
stub infile outfile intf = unlines $ C.pp_unit $ stub_body infile intf

stub_body :: String -> Interface -> C.Unit
stub_body infile intf@(Interface ifn descr decls) = C.UnitList [
    intf_preamble infile ifn descr,
    C.Blank,
    C.MultiComment [ "Generic message buffer marshalling stubs" ],
    C.Blank,

    C.Include C.Standard "barrelfish/barrelfish.h",
    C.Include C.Standard "barrelfish/msgbuf.h",
    C.Include C.Standard ("if/" ++ ifn ++ "_msgbuf_defs.h"),
    C.Blank,

    C.MultiComment [ "Marshalling functions" ],
    C.UnitList [ tx_fn ifn m | m <- msgs ],
    C.Blank,

    C.MultiComment [ "Demarshall/dispatch function" ],
    rx_fn ifn msgs
    ]
    where
        (types, msgs) = Backend.partitionTypesMessages decls

tx_fn :: String -> MessageDef -> C.Unit
tx_fn ifn msg@(Message _ mn args _) =
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (tx_fn_name ifn mn) (tx_fn_params ifn msg)
    [
        localvar (C.TypeName "errval_t") errvar_name Nothing,
        C.SBlank,
        C.StmtList $ concat [ handle_marshall $ marshall_arg a | a <- msgnum_arg:args ],
        C.Return $ C.Variable "SYS_ERR_OK"
    ] where
        msgnum_arg = Arg (Builtin msg_code_type) (Name $ msg_enum_elem_name ifn mn)

        handle_marshall :: C.Expr -> [C.Stmt]
        handle_marshall marshall_expr =
            [C.Ex $ C.Assignment errvar marshall_expr,
             C.If (C.Call "err_is_fail" [errvar])
                [C.Return errvar] []]

        marshall_arg :: MessageArgument -> C.Expr
        marshall_arg (Arg (TypeAlias _ b) v) = marshall_arg (Arg (Builtin b) v)
        marshall_arg (Arg (Builtin b) (Name n))
            = C.Call ("msgbuf_marshall_" ++ (show b)) [msgbuf_var, C.Variable n]
        marshall_arg (Arg (Builtin b) (DynamicArray n l))
            | b `elem` [Int8, UInt8, Char]
                = C.Call "msgbuf_marshall_buffer" [msgbuf_var, C.Variable n, C.Variable l]
            | otherwise = error "dynamic arrays are NYI for MsgBuf backend"
        marshall_arg a = error $ "complex types are NYI for MsgBuf backend: " ++ show a

rx_fn :: String -> [MessageDef] -> C.Unit
rx_fn ifn msgs = 
    C.FunctionDef C.NoScope (C.TypeName "errval_t") (rx_fn_name ifn) (rx_fn_params ifn)
    [
        localvar (C.TypeName "errval_t") errvar_name Nothing,
        localvar (C.TypeName msg_code_ctype) "msgnum" Nothing,
        C.SBlank,
        C.SComment "unmarshall message code",
        C.Ex $ C.Assignment errvar (C.Call msg_code_unmarshall_func
                                        [msgbuf_var,
                                         C.AddressOf $ C.Variable "msgnum"]),
        C.If (C.Call "err_is_fail" [errvar]) [C.Return errvar] [],
        C.SBlank,
        C.Switch (C.Variable "msgnum") cases
            [C.Return $ C.Variable "FLOUNDER_ERR_RX_INVALID_MSGNUM"],
        C.SBlank,
        C.Return $ C.Variable "SYS_ERR_OK"
    ] where
        cases = [C.Case (C.Variable $ msg_enum_elem_name ifn mn) [handle_msg mn args]
                 | Message _ mn args _ <- msgs]

        handle_msg :: String -> [MessageArgument] -> C.Stmt
        handle_msg mn args = C.Block [
            localvar (C.Struct $ msg_argstruct_name ifn mn) "args" Nothing,
            C.SBlank,
            C.StmtList $ concat $ map (handle_unmarshall.unmarshall_arg) args,
            C.Ex $ C.Call "assert" [C.Binary C.NotEquals rx_handler (C.Variable "NULL")],
            C.Ex $ C.CallInd rx_handler rx_handler_args,
            C.Break
            ] where
                rx_handler = C.DerefField bindvar "rx_vtbl" `C.FieldOf` mn
                rx_handler_args = [bindvar] ++ (map arg_field $ concat $ map mkargs args)

                mkargs (Arg _ (Name n)) = [n]
                mkargs (Arg _ (DynamicArray n l)) = [n, l]

        handle_unmarshall :: C.Expr -> [C.Stmt]
        handle_unmarshall unmarshall_expr =
            [C.Ex $ C.Assignment errvar unmarshall_expr,
             C.If (C.Call "err_is_fail" [errvar])
                [C.Return errvar] []]

        unmarshall_arg :: MessageArgument -> C.Expr
        unmarshall_arg (Arg (TypeAlias _ b) v) = unmarshall_arg (Arg (Builtin b) v)
        unmarshall_arg (Arg (Builtin b) (Name n))
            = C.Call ("msgbuf_unmarshall_" ++ (show b))
                        [msgbuf_var, C.AddressOf $ arg_field n]
        unmarshall_arg (Arg (Builtin b) (DynamicArray n l))
            | b `elem` [Int8, UInt8, Char]
                = C.Call "msgbuf_unmarshall_buffer"
                    [msgbuf_var, C.AddressOf $ arg_field l,
                     C.Cast (C.Ptr $ C.Ptr C.Void) $ C.AddressOf $ arg_field n]
            | otherwise = error "dynamic arrays are NYI for MsgBuf backend"
        unmarshall_arg a = error $ "complex types are NYI for MsgBuf backend: " ++ show a

        arg_field n = (C.Variable "args") `C.FieldOf` n


msg_code_type = UInt16
msg_code_ctype = "uint16_t"
msg_code_unmarshall_func = "msgbuf_unmarshall_uint16"
