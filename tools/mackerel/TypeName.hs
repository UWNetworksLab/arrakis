{- 
  Type name: qualified names for register types
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2011, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module TypeName where

import MackerelParser

{--------------------------------------------------------------------

--------------------------------------------------------------------}

-- Fully-qualified name of a type
data Name = Name String String
          deriving (Show, Eq)

fromParts :: String -> String -> Name
fromParts dev typename = Name dev typename
                   
fromRef :: AST -> String -> Name                   
fromRef (TypeRef tname (Just dname)) _ = Name dname tname
fromRef (TypeRef tname Nothing) dname  = Name dname tname

toString :: Name -> String
toString (Name d t) =  d ++ "." ++ t

devName :: Name -> String
devName (Name d _) = d

typeName :: Name -> String
typeName (Name _ t) = t


is_builtin_type :: Name -> Bool
is_builtin_type (Name _ "uint8") = True
is_builtin_type (Name _ "uint16") = True
is_builtin_type (Name _ "uint32") = True
is_builtin_type (Name _ "uint64") = True
is_builtin_type _ = False

null :: Name
null = Name "" ""