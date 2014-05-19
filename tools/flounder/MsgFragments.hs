{- 
  MsgFragments.hs: helper for backends that need to split up a message into
   multiple fragments.

  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module MsgFragments where

import Data.Bits
import Data.List
import Data.Ord

import qualified CAbsSyntax as C
import BackendCommon (Direction (..), intf_bind_var, msg_enum_elem_name,
                      tx_union_elem, rx_union_elem, type_c_type)
import Syntax
import Arch

-- an application level message is specified by one or more transport-level fragments
-- for UMP, we have a top-level list of non-cap fragments and separate list of caps
data MsgSpec = MsgSpec String [MsgFragment] [CapFieldTransfer]
    deriving (Show, Eq)

-- a message fragment defines the layout of a transport-level message
data MsgFragment = MsgFragment [FragmentWord] | OverflowFragment OverflowFragment
                 deriving (Show, Eq)

-- some fragments are "special" in that they can overflow and occupy an
-- arbitrary number of underlying transport messages, because their size is
-- only known at run time
data OverflowFragment = 
        -- for marshalling byte arrays: type, data pointer and length fields
        BufferFragment TypeBuiltin ArgField ArgField
        -- for marshalling strings: string pointer field
        | StringFragment ArgField
        deriving (Show, Eq)

-- LMP is a special case where caps can be sent in message fragments
data LMPMsgSpec = LMPMsgSpec String [LMPMsgFragment]
    deriving (Show, Eq)

data LMPMsgFragment = LMPMsgFragment MsgFragment (Maybe CapFieldTransfer)
                 deriving (Show, Eq)

type FragmentWord = [ArgFieldFragment]

-- an arg fragment refers to a (portion of a) primitive value which is part of
-- a (possibly larger) message argument, by type, qualified name and bit offset
data ArgFieldFragment = ArgFieldFragment TypeBuiltin ArgField Int
                      | MsgCode -- implicit argument, occurs once per message
                      deriving (Show, Eq)

-- an argument field names the lowest-level field of an argument
-- each entry in the list is a field name and (optional) array index
-- eg. foo[3].bar is [NamedField "foo", ArrayField 3, NamedField "bar"]
type ArgField = [ArgFieldElt]
data ArgFieldElt = NamedField String | ArrayField Integer
    deriving (Show, Eq)

-- modes of transfering a cap
data CapTransferMode = GiveAway | Copied
                  deriving (Show, Eq)

-- a capability is just identified by the name of its field
type CapField = ArgField 

-- a capability transfer is identified by the name of its field and the type 
-- of transfer requested
data CapFieldTransfer = CapFieldTransfer CapTransferMode ArgField
                  deriving (Show, Eq)

-- to generate the above, we use a slightly different intermediate
-- representation, which uses a list of fragments of individual fields
data FieldFragment = FieldFragment ArgFieldFragment
                   | CapField CapTransferMode ArgField
                   | OverflowField OverflowFragment
    deriving (Show, Eq)

-- builtin type used to transmit message code
msg_code_type :: TypeBuiltin
msg_code_type = UInt16

build_msg_spec :: Arch -> Int -> Bool -> [TypeDef] -> MessageDef -> MsgSpec
build_msg_spec arch words_per_frag contains_msgcode types (Message _ mn args _)
    -- ensure that we don't produce a completely empty message
    | (msg_frags ++ overflow_frags) == [] = MsgSpec mn [MsgFragment []] capfield_transfers
    | otherwise  = MsgSpec mn (msg_frags ++ overflow_frags) capfield_transfers
    where
        (frags, capfields, overfields)
            = partition_frags $ build_field_fragments arch types args
        field_frags = sort_field_fragments arch frags
        msg_frags = find_msg_fragments arch words_per_frag contains_msgcode field_frags
        overflow_frags = map OverflowFragment overfields
        capfield_transfers = map (\(CapField tm cf) -> (CapFieldTransfer tm cf)) capfields

-- build an LMP message spec by merging in the caps from a UMP spec
build_lmp_msg_spec :: Arch -> [TypeDef] -> MessageDef -> LMPMsgSpec
build_lmp_msg_spec arch types msgdef = LMPMsgSpec mn (merge_caps frags caps)
    where
        MsgSpec mn frags caps = build_msg_spec arch (lmp_words arch) True types msgdef

        -- XXX: ensure that we never put a cap together with an overflow fragment
        -- even though this could work at the transport-level, the current
        -- LMP code doesn't support it
        merge_caps :: [MsgFragment] -> [CapFieldTransfer] -> [LMPMsgFragment]
        merge_caps [] [] = []
        merge_caps (mf:restf) []
            = (LMPMsgFragment mf Nothing):(merge_caps restf [])
        merge_caps [] (c:restc)
            = (LMPMsgFragment (MsgFragment []) (Just c)):(merge_caps [] restc)
        merge_caps ((mf@(OverflowFragment _)):restf) caps
            = (LMPMsgFragment mf Nothing):(merge_caps restf caps)
        merge_caps (mf:restf) (c:restc)
            = (LMPMsgFragment mf (Just c)):(merge_caps restf restc)

-- partition a list of field fragments into (ordinary fields, caps, overflow buffers/strings)
partition_frags :: [FieldFragment] -> ([FieldFragment], [FieldFragment], [OverflowFragment])
partition_frags [] = ([], [], [])
partition_frags (h:t) = case h of
    f@(FieldFragment _) -> (f:restf, restc, resto)
    f@(CapField _ _)    -> (restf, f:restc, resto)
    OverflowField o     -> (restf, restc, o:resto)
    where
        (restf, restc, resto) = partition_frags t

find_msg_fragments :: Arch -> Int -> Bool -> [FieldFragment] -> [MsgFragment]
find_msg_fragments arch words_per_frag contains_msgcode frags
    = group_frags frags first_frag
    where
        -- does the first fragment need to contain the message code?
        first_frag
            | contains_msgcode = MsgFragment [[MsgCode]]
            | otherwise        = MsgFragment []

        group_frags :: [FieldFragment] -> MsgFragment -> [MsgFragment]
        group_frags [] (MsgFragment []) = [] -- empty fragment, drop it
        group_frags [] cur = [cur] -- terminated search
        group_frags ((FieldFragment f):rest) (MsgFragment [])
            = group_frags rest (MsgFragment [[f]])
        group_frags ((FieldFragment f):rest) cur@(MsgFragment wl)
            -- can we fit another fragment into the current word?
            | can_fit_word lastword f
                = group_frags rest (MsgFragment (restwords ++ [lastword ++ [f]]))
            -- can we fit another word onto the current message fragment?
            | (length wl) < words_per_frag
                = group_frags rest (MsgFragment (wl ++ [[f]]))
            | otherwise = cur:(group_frags rest (MsgFragment [[f]]))
            where
                lastword = last wl
                restwords = init wl
                bitsizeof = bitsizeof_argfieldfrag arch

                can_fit_word :: FragmentWord -> ArgFieldFragment -> Bool
                can_fit_word word frag =
                    (sum $ map bitsizeof word) + bitsizeof frag <= wordsize arch

-- sort the list of fragments by size, to optimise packing
sort_field_fragments :: Arch -> [FieldFragment] -> [FieldFragment]
sort_field_fragments ar = sortBy cmp
    where
        cmp (FieldFragment f1) (FieldFragment f2)
            = comparing (bitsizeof_argfieldfrag ar) f1 f2

build_field_fragments :: Arch -> [TypeDef] -> [MessageArgument] -> [FieldFragment]
build_field_fragments arch types args = concat $ map arg_fragments args
    where
        arg_fragments :: MessageArgument -> [FieldFragment]
        arg_fragments (Arg (TypeAlias _ b) v) = arg_fragments (Arg (Builtin b) v)
        arg_fragments (Arg (Builtin t) (DynamicArray n l))
            | t `elem` [UInt8, Int8, Char]
                = [OverflowField $ BufferFragment t [NamedField n] [NamedField l]]
            | otherwise = error "dynamic arrays of types other than char/int8/uint8 are not yet supported"
        arg_fragments (Arg (Builtin b) v) = fragment_builtin [NamedField (varname v)] b
        arg_fragments (Arg (TypeVar t) v) =
            fragment_typedef [NamedField (varname v)] (lookup_type_name types t)

        varname (Name n) = n
        varname (DynamicArray _ _)
            = error "dynamic arrays of types other than char/int8/uint8 are not yet supported"

        fragment_typedef :: ArgField -> TypeDef -> [FieldFragment]
        fragment_typedef f (TStruct _ fl) =
            concat [fragment_typeref ((NamedField fn):f) tr | TStructField tr fn <- fl]

        fragment_typedef f (TArray tr _ len) = concat [fragment_typeref i tr | i <- fields]
            where
                fields = [(ArrayField i):f | i <- [0..(len - 1)]]
        fragment_typedef f (TEnum _ _) = fragment_builtin f (enum_type arch)
        fragment_typedef f (TAlias _ _) = error "aliases unhandled here"
        fragment_typedef f (TAliasT _ b) = fragment_builtin f b

        fragment_typeref :: ArgField -> TypeRef -> [FieldFragment]
        fragment_typeref f (Builtin b) = fragment_builtin f b
        fragment_typeref f (TypeAlias _ b) = fragment_builtin f b
        fragment_typeref f (TypeVar tv) = fragment_typedef f (lookup_type_name types tv)

        fragment_builtin :: ArgField -> TypeBuiltin -> [FieldFragment]
        fragment_builtin f Cap = [CapField Copied f]
        fragment_builtin f GiveAwayCap = [CapField GiveAway f]
        fragment_builtin f String = [OverflowField $ StringFragment f]
        fragment_builtin f t =
            [FieldFragment (ArgFieldFragment t f off)
             | off <- [0, (wordsize arch) .. (bitsizeof_builtin arch t - 1)]]

bitsizeof_argfieldfrag :: Arch -> ArgFieldFragment -> Int
bitsizeof_argfieldfrag a (ArgFieldFragment t _ _)
    = min (wordsize a) (bitsizeof_builtin a t)
bitsizeof_argfieldfrag a MsgCode
    = bitsizeof_builtin a msg_code_type

bitsizeof_builtin :: Arch -> TypeBuiltin -> Int
bitsizeof_builtin _ UInt8 = 8
bitsizeof_builtin _ UInt16 = 16
bitsizeof_builtin _ UInt32 = 32
bitsizeof_builtin _ UInt64 = 64
bitsizeof_builtin _ Int8 = 8
bitsizeof_builtin _ Int16 = 16
bitsizeof_builtin _ Int32 = 32
bitsizeof_builtin _ Int64 = 64
bitsizeof_builtin a UIntPtr = ptrsize a
bitsizeof_builtin a IntPtr = ptrsize a
bitsizeof_builtin a Size = sizesize a
bitsizeof_builtin _ Bool = 1
bitsizeof_builtin _ IRef = 32 -- FIXME: move out of flounder
bitsizeof_builtin _ Char = 8
bitsizeof_builtin _ String = undefined
bitsizeof_builtin _ Cap = undefined
bitsizeof_builtin _ GiveAwayCap = undefined


-------------------------------------------------------
-- Utility function for working with arg fields
-- This generates a C expression to access a given field
-------------------------------------------------------

argfield_expr :: Direction -> String -> ArgField -> C.Expr
argfield_expr TX mn [NamedField n] = tx_union_elem mn n
argfield_expr RX mn [NamedField n] = rx_union_elem mn n
argfield_expr _ _ [ArrayField n] = error "invalid; top-level array"
argfield_expr dir mn ((NamedField n):rest)
    = C.FieldOf (argfield_expr dir mn rest) n
argfield_expr dir mn ((ArrayField i):rest)
    = C.SubscriptOf (C.DerefPtr $ argfield_expr dir mn rest) (C.NumConstant i)

-- generate a C expression for constructing the given word of a message fragment
fragment_word_to_expr :: Arch -> String -> String -> FragmentWord -> C.Expr
fragment_word_to_expr arch ifn mn frag = mkwordexpr 0 frag
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
                        (C.Cast (C.TypeName "uintptr_t") ex)
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


store_arg_frags :: Arch -> String -> String -> C.Expr -> Int -> Int -> [ArgFieldFragment] -> [C.Stmt]
store_arg_frags _ _ _ _ _ _ [] = []
store_arg_frags arch ifn mn msgdata_ex word bitoff (MsgCode:rest)
    = store_arg_frags arch ifn mn msgdata_ex word (bitoff + bitsizeof_argfieldfrag arch MsgCode) rest
store_arg_frags _ _ _ _ _ _ ((ArgFieldFragment String _ _):_)
    = error "strings are not handled here"
store_arg_frags arch ifn mn msgdata_ex word bitoff (aff@(ArgFieldFragment t af argoff):rest)
    = (C.Ex expr):(store_arg_frags arch ifn mn msgdata_ex word (bitoff + bitsize) rest)
    where
        bitsize = bitsizeof_argfieldfrag arch aff
        expr = C.Assignment (argfield_expr RX mn af) assval
        assval
            | argoff == 0 = mask msgval
            | otherwise = C.Binary C.BitwiseOr (argfield_expr RX mn af)
                (C.Binary C.LeftShift
                    (C.Cast (type_c_type ifn $ Builtin t) (mask msgval))
                    (C.NumConstant $ toInteger argoff))
        msgval
            | bitoff == 0 = msgword
            | otherwise = C.Binary C.RightShift msgword (C.NumConstant $ toInteger bitoff)
        msgword = C.SubscriptOf msgdata_ex $ C.NumConstant $ toInteger word
        mask ex
            | bitsize == (wordsize arch) = ex
            | otherwise = C.Binary C.BitwiseAnd ex (C.HexConstant maskval)
            where
                maskval = (shift 1 bitsize) - 1
