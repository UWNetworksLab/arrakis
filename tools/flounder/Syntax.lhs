%include polycode.fmt

%if false
  Flounder2: an even more simpler IDL for Barrelfish
   
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif


\section{The Abstract Syntax}

In this module, we define the combinators to embed the Flounder
language into Haskell. So, we will mix the design the Abstract-Syntax
Tree with the definition of some handy combinators.

> module Syntax where


\subsection{Interface Header}


First, we define the abstract syntax of our embedded language. At the
top-level is the \emph{interface} definition. It consists of a @name@
and, potentially, a @description@. It contains a list of
\emph{declarations}.

> data Interface = Interface String (Maybe String) [ Declaration ] 
>
> generalInterface :: Maybe String -> String -> [ Declaration ] -> Interface
> generalInterface description name declarations = 
>     Interface name description declarations

Which can be further refined into an anonymous interface, ie. with no
description:

> aInterface :: String -> [Declaration] -> Interface
> aInterface = generalInterface Nothing

And, the more common, a documented interface:

> interface :: String -> String -> [Declaration] -> Interface
> interface name description declarations = 
>     generalInterface (Just description) name declarations

Finally, various getters:

> interfaceName :: Interface -> String
> interfaceName (Interface name _ _) = name

\subsection{Declarations}

A declaration is either a \emph{type definition} or a \emph{message
definition}. In the next subsections, we define these terms in turn.

> data Declaration = Typedef TypeDef
>                  | Messagedef MessageDef


\subsubsection{Declaring types}


We can define new types out of existing ones thanks to five
constructs:
\begin{description}
        \item[Structure:] like C @struct@, it defines a name-indexed record
        \item[Array:] defines a static array of elements of a given type
        \item[Enumeration:] like C @enum@, defines a sum-type over some elements
        \item[Alias:] redefine the name of an already defined type
\end{description}

> data TypeDef = TStruct String [StructField]
>              | TArray TypeRef String Integer
>              | TEnum String [String] 
>              | TAlias String TypeRef
>              | TAliasT String TypeBuiltin

In this definition, we notice the presence of @TypeRef@: indeed, when
we construct a new type, it can use either built-in types, such as
@uint8_t@, or previously defined types, identified by their name.

> data TypeRef = Builtin TypeBuiltin
>              | TypeVar String
>              | TypeAlias String TypeBuiltin
>     deriving (Show)

The builtin types being:

> data TypeBuiltin = UInt8
>                  | UInt16
>                  | UInt32
>                  | UInt64
>                  | UIntPtr
>                  | Int8
>                  | Int16
>                  | Int32
>                  | Int64
>                  | IntPtr
>                  | Size
>                  | Char
>                  | Bool
>                  | String
>                  | IRef
>                  | Cap
>                  | GiveAwayCap
>                    deriving (Enum, Eq)

Which are shown with:

> instance Show TypeBuiltin where
>     show UInt8 = "uint8"
>     show UInt16 = "uint16"
>     show UInt32 = "uint32"
>     show UInt64 = "uint64"
>     show UIntPtr = "uintptr"
>     show Int8 = "int8"
>     show Int16 = "int16"
>     show Int32 = "int32"
>     show Int64 = "int64"
>     show IntPtr = "intptr"
>     show Size = "size"
>     show Bool = "bool"
>     show String = "string"
>     show Char = "char"
>     show IRef = "iref"
>     show Cap = "cap"
>     show GiveAwayCap = "give_away_cap"

> instance Read TypeBuiltin where
>     readsPrec _ = \s -> case s of 
>                                "uint8" -> [(UInt8, "")]
>                                "uint16" -> [(UInt16, "")]
>                                "uint32" -> [(UInt32, "")]
>                                "uint64" -> [(UInt64, "")]
>                                "uintptr" -> [(UIntPtr, "")]
>                                "int8" -> [(Int8, "")]
>                                "int16" -> [(Int16, "")]
>                                "int32" -> [(Int32, "")]
>                                "int64" -> [(Int64, "")]
>                                "intptr" -> [(IntPtr, "")]
>                                "int" -> [(Int32, "")] -- XXX: why? -AB
>                                "size" -> [(Size, "")]
>                                "bool" -> [(Bool, "")]
>                                "string" -> [(String, "")]
>                                "char" -> [(Char, "")]
>                                "iref" -> [(IRef, "")]
>                                "cap" -> [(Cap, "")]
>                                "give_away_cap" -> [(GiveAwayCap, "")]
>                                _ -> error  $ "Undefined builtin type " ++ s

Hence, we can define:

> isBuiltin :: TypeRef -> Bool
> isBuiltin (Builtin _) = True
> isBuiltin _ = False

And the usual combinators:

> uint8, uint16, uint32, uint64, uintptr :: TypeRef
> uint8 = Builtin UInt8
> uint16 = Builtin UInt16
> uint32 = Builtin UInt32
> uint64 = Builtin UInt64
> uintptr = Builtin UIntPtr

> int8, int16, int32, int64, intptr :: TypeRef
> int8 = Builtin Int8
> int16 = Builtin Int16
> int32 = Builtin Int32
> int64 = Builtin Int64
> intptr = Builtin IntPtr

> size, string, iref, cap, give_away_cap :: TypeRef
> size = Builtin Size
> string = Builtin String
> iref = Builtin IRef
> cap = Builtin Cap
> give_away_cap = Builtin GiveAwayCap

> var :: String -> TypeRef
> var typeRef = TypeVar typeRef

> als :: String -> TypeBuiltin -> TypeRef
> als typeRef origin = TypeAlias typeRef origin

Then, we can build a type definition out of these special cases with:

> typedef :: TypeDef -> Declaration
> typedef typeDefinition = Typedef typeDefinition



Here's a utility function to resolve a named type (which may be an alias) to
its canonical definition:

> lookup_type_name :: [TypeDef] -> String -> TypeDef
> lookup_type_name types name = case def of
>         -- FIXME: these types seem a bit confusing -AB
>         -- why are there so many ways to specify an alias of a builtin?
>         (TAlias _ (Builtin b)) -> TAliasT name b
>         (TAlias _ (TypeVar v)) -> lookup_type_name types v
>         (TAlias _ (TypeAlias _ b)) -> TAliasT name b
>         d -> d
>     where
>         -- I'm assuming there must be exactly one definition for the type name
>         def
>             | null defs = error $ "lookup_type_name: " ++ name ++ " not defined"
>             | null $ tail defs = head defs
>             | otherwise = error $ "lookup_type_name: " ++ name ++ " multiply defined"
>         defs = [t | t <- types, typedef_name t == name]
> 
>         typedef_name :: TypeDef -> String
>         typedef_name (TStruct n _) = n
>         typedef_name (TArray _ n _) = n
>         typedef_name (TEnum n _) = n
>         typedef_name (TAlias n _) = n
>         typedef_name (TAliasT n _) = n

As above, but for a TypeRef:

> lookup_typeref :: [TypeDef] -> TypeRef -> TypeDef
> lookup_typeref _ (Builtin b) = TAliasT (show b) b
> lookup_typeref _ (TypeAlias n b) = TAliasT n b
> lookup_typeref types (TypeVar v) = lookup_type_name types v



\paragraph{Structure}

So, a @struct@ is identified by its @name@, which, as in C, comes
after a list of @fields@.

> struct :: [StructField] -> String -> TypeDef
> struct fields name = TStruct name fields

The fields of a structure consist of a type @typeField@ associated
with a field @name@.

> data StructField = TStructField TypeRef String
>
> field, (.@@.) :: TypeRef -> String -> StructField
> field typeField name = TStructField typeField name
> (.@@.) = field

\paragraph{Array}

An array is identified by a @name@ and defined by the type of its
elements, @typeElts@, as well as its length.

> array :: TypeRef -> String -> Integer -> TypeDef
> array typeElts name length = TArray typeElts name length

\paragraph{Enumeration}

An enumeration is, as always, identified by a @name@. The content of
an enumeration is a list of tags, the @elements@.

> enum :: [String] -> String -> TypeDef
> enum elements name = TEnum name elements

\paragraph{Aliasing}

Finally, we can do type aliasing: we can give a @newName@ to a type,
which was previously known as @originalName@. Note that the names are
switched between the combinator and the data-type.

> alias :: TypeRef -> String -> TypeDef
> alias originalName newName = TAlias newName originalName



\subsubsection{Declaring a Message}


A @message@ is identified by a @name@ and is either a @Call@, a
@Response@, or it is a @Message@, in all generality. A message can
carry some arguments, which are described by a list of
@MessageArgument@, in @msgArgs@. Hence the following definition:

> data MessageDef = Message MessageType String [ MessageArgument ] [(String, [(String, MetaArgument)])]
>                 | RPC String [ RPCArgument ] [(String, [(String, MetaArgument)])]
>
> data MessageType = MMessage 
>                  | MCall
>                  | MResponse
>
> message, call, response :: String -> [ MessageArgument ] -> Declaration
> message name args = Messagedef $ Message MMessage name args []
> call name args = Messagedef $ Message MCall name args []
> response name args = Messagedef $ Message MResponse name args []

As for the arguments passed to a message, they are simply the type @typeArg@ and
the @identifier@ of the argument:

> data MessageArgument = Arg TypeRef Variable
>     deriving (Show)
>
> data Variable = Name String
>               | DynamicArray String String
>     deriving (Show)
>
> arg, (.@.) :: TypeRef -> String -> MessageArgument
> arg typeArg identifier = Arg typeArg (Name identifier)
> (.@.) = arg
>
> argDynamic, (.#.) :: TypeRef -> (String, String) -> MessageArgument
> argDynamic typeArg (identifier, length) = Arg typeArg (DynamicArray identifier length)
> (.#.) = argDynamic 

And we are done for message definitions.

Concerning RPC, the RPC arguments take into account whether a
parameter is used \emph{in} or \emph{out}.

> data RPCArgument = RPCArgIn TypeRef Variable
>                  | RPCArgOut TypeRef Variable

The meta-parameters allow passing additional information to individual
backends. The paramaters are a mapping from names to either an identifier,
which should match a message argument, or a value:

> data MetaArgument = BackendInt Integer
>                   | BackendMsgArg String
