%include polycode.fmt

%if false
  Error: DSL for error definition
   
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%if false

> {-# LANGUAGE BangPatterns #-}

> module HamletAst where

> import Debug.Trace
> import Text.PrettyPrint.HughesPJ as Pprinter 
> import Data.List

%endif


> class Pretty a where
>     pretty :: a -> Doc

> data Capabilities = Capabilities { defines :: ![Define],
>                                    capabilities :: ![Capability] }
>                   deriving Show

> vcat' :: [Doc] -> Doc
> vcat' = foldr ($+$) empty 


> instance Pretty Capabilities where
>     pretty (Capabilities defs caps) =
>         text "Capabilities:" $+$
>         nest 4 ( text "Defines:" $+$
>                  nest 4 (vcat' $ map pretty defs) $+$
>                  text "Caps:" $+$
>                  nest 4 (vcat' $ map pretty caps))
                 

> data Define = Define !String !Int
>             deriving Show

> instance Pretty Define where
>     pretty (Define name val) = text name <+> char '=' <+> int val

> mkDefineList :: [Define] -> [(String, Int)]
> mkDefineList = map (\(Define s i) -> (s, i))

> data Capability = Capability { name :: !CapName,
>                                generalEquality :: !(Maybe Bool),
>                                from :: !(Maybe CapName),
>                                fromSelf :: Bool,
>                                multiRetype :: Bool,
>                                fields :: ![CapField],
>                                rangeExpr :: !(Maybe (AddressExpr, SizeExpr)),
>                                eqFields :: ![NameField] }
>                 deriving Show

> instance Pretty Capability where
>     pretty (Capability (CapName name)
>                        genEq 
>                        from
>                        fromSelf
>                        multiRetype
>                        fields
>                        rangeExpr
>                        eqFields) =
>        text name $+$
>        nest 4 (text "General Equality:" <+> text (show genEq) $+$
>                case from of
>                    Nothing -> text $ if fromSelf then "From self" else "From nothing"
>                    Just (CapName fromName) ->
>                        text "From:" <+> text fromName <> text (if fromSelf then " and self" else "")
>                $+$
>                text "Fields:" <> text (if null fields then " None" else "") $+$
>                text (if multiRetype then "Can be retyped multiple times." else "") $+$
>                nest 4 (vcat' (map pretty fields)) $+$
>                (case rangeExpr of
>                     Nothing -> text "Not addressable"
>                     Just (addressExpr, sizeExprE) ->
>                         (text "Address expr:" <+> pretty addressExpr $+$
>                          text "Size expr:" <+> (pretty sizeExprE)))
>                $+$
>                text "Equality fields:" <+> (text $ intercalate ", " $ map (\(NameField n) -> n) eqFields))

> data CapName = CapName !String
>              deriving (Show, Eq)

> data CapField = CapField !Type !NameField 
>               deriving Show
> instance Pretty CapField where
>     pretty (CapField typ (NameField name)) = 
>         text (show typ) <+> text name

> data NameField = NameField !String
>                deriving Show

> data Type = UInt8
>           | UInt16
>           | UInt32
>           | UInt64
>           | Int
>           | GenPAddr
>           | GenSize
>           | LPAddr
>           | GenVAddr
>           | LVAddr
>           | CAddr
>           | Pointer String
>           | CapRights
>           | CoreId
>             deriving Show

> instance Read Type where
>     readsPrec _ s 
>         | s == "uint8" = [(UInt8, "")]
>         | s == "uint16" = [(UInt16, "")]
>         | s == "uint32" = [(UInt32, "")]
>         | s == "uint64" = [(UInt64, "")]
>         | s == "int" = [(Int, "")]
>         | s == "genpaddr" = [(GenPAddr, "")]
>         | s == "gensize" = [(GenSize, "")]
>         | s == "lpaddr" = [(LPAddr, "")]
>         | s == "genvaddr" = [(GenVAddr, "")]
>         | s == "lvaddr" = [(LVAddr, "")]
>         | s == "caddr" = [(CAddr, "")]
>         | s == "caprights" = [(CapRights, "")]
>         | s == "coreid" = [(CoreId, "")]
>         | otherwise = [(Pointer s, "")]

> data AddressExpr = AddressExpr Expr | MemToPhysOp Expr
>                  deriving Show
> instance Pretty AddressExpr where
>     pretty (AddressExpr e) = pretty e
>     pretty (MemToPhysOp e) = text "mem_to_phys(" <> pretty e <> text ")"

> data SizeExpr = ZeroSize | SizeExpr Expr | SizeBitsExpr Expr
>               deriving Show
> instance Pretty SizeExpr where
>     pretty (ZeroSize) = text "0"
>     pretty (SizeExpr e) = pretty e
>     pretty (SizeBitsExpr e) = text "2^(" <> pretty e <> char ')'

> data Expr = AddExpr String String | NameExpr String
>           deriving Show
> instance Pretty Expr where
>     pretty (AddExpr l r) = text $ concat ["(", l, " + ", r, ")"]
>     pretty (NameExpr n) = text n

