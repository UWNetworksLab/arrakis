%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Constructs where

> import PureExpressions
> import Semantics

%endif


\section{Filet-o-Fish standard constructs}
\label{sec:fof_syntax_constructs}


The FoF language is defined by the syntax tree below. It gathers every
constructs defined in the |Constructs| directory as well as foreign
functions defined in the |Libc| and |Libbarrelfish| directories.


> data FoFConst a 

Foreign-call to libc Assert:

>     = Assert PureExpr a

Foreign-call to libc Printf:

>     | Printf String [PureExpr] a

Foreign-call to libarrelfish |has_descendants|:

>     | HasDescendants (Maybe String) PureExpr (PureExpr -> a)

Foreign-call to libarrelfish |mem_to_phys|:

>     | MemToPhys (Maybe String) PureExpr (PureExpr -> a)

Support for Union:

>     | NewUnion (Maybe String) AllocUnion String [(String,TypeExpr)] (String, Data) (Loc -> a)
>     | ReadUnion Loc String (Data -> a)
>     | WriteUnion Loc String Data a

Support for Typedef:

>     | Typedef TypeExpr a
>     | TypedefE String TypeExpr a

Support for Structures:

>     | NewStruct (Maybe String) AllocStruct String [(String,(TypeExpr,Data))] (Loc -> a)
>     | ReadStruct Loc String (Data -> a)
>     | WriteStruct Loc String Data a

Support for Strings:

>     | NewString (Maybe String) String (Loc -> a)

Support for Reference cells:

>      | NewRef (Maybe String) Data (Loc -> a)
>      | ReadRef Loc (Data -> a)
>      | WriteRef Loc Data a

Support for Functions:

>      | NewDef [FunAttr] String Function TypeExpr [(TypeExpr, Maybe String)] 
>               (PureExpr -> a)
>      | CallDef (Maybe String) PureExpr [PureExpr] 
>                (PureExpr -> a)
>      | Return PureExpr

Support for Enumerations:

>      | NewEnum (Maybe String) String Enumeration String (Loc -> a)

Support for Conditionals:

>      | If (FoFCode PureExpr)
>           (FoFCode PureExpr) 
>           (FoFCode PureExpr) a
>      | For (FoFCode PureExpr)  
>            (FoFCode PureExpr) 
>            (FoFCode PureExpr) 
>            (FoFCode PureExpr) a
>      | While (FoFCode PureExpr) 
>              (FoFCode PureExpr) a
>      | DoWhile (FoFCode PureExpr) 
>                (FoFCode PureExpr) a
>      | Switch PureExpr 
>               [(PureExpr, FoFCode PureExpr)] 
>               (FoFCode PureExpr) a
>      | Break
>      | Continue 

Support for Arrays:

>      | NewArray (Maybe String) AllocArray [Data] (Loc -> a)
>      | ReadArray Loc Index (Data -> a)
>      | WriteArray Loc Index Data a

The following type synonyms have been used above as a documentation
purpose. A |Data| represents a value used to initialize a
data-structure. A |Loc| represents a reference. An |Index| is a value
used to index an array.

> type Data = PureExpr
> type Loc = PureExpr
> type Index = PureExpr

\paragraph{Function attributes}

A function can be characterized by the following attributes, following
their C semantics:

> data FunAttr = Static
>              | Inline
>              deriving (Eq)

> instance Show FunAttr where
>     show Static = "static"
>     show Inline = "inline"

\paragraph{Enumeration}

When defining an enumeration, we use the following type synonym to
describe the list of pair name-value:

> type Enumeration = [(String, Int)]


\subsection{Functor instance}

A crucial specificity of |FoFConst| is that it defines a functor. This
functor is defined as follow.

> instance Functor FoFConst where
>     fmap f (Assert a b) = Assert a (f b)
>     fmap f (Printf a b c) = Printf a b (f c)
>     fmap f (HasDescendants a b c) = HasDescendants a b (f . c)
>     fmap f (MemToPhys a b c) = MemToPhys a b (f . c)
>     fmap f (NewUnion a b c d e g) = NewUnion a b c d e (f . g)
>     fmap f (ReadUnion a b c) = ReadUnion a b (f . c)
>     fmap f (WriteUnion a b c d) = WriteUnion a b c (f d)
>     fmap f (Typedef a c) = Typedef a (f c)
>     fmap f (TypedefE a b c) = TypedefE a b (f c)
>     fmap f (NewStruct a b c d e) = NewStruct a b c d (f . e)
>     fmap f (ReadStruct a b c) = ReadStruct a b (f . c)
>     fmap f (WriteStruct a b c d) = WriteStruct a b c (f d)
>     fmap f (NewString a b c) = NewString a b (f . c)
>     fmap f (NewRef a b c) = NewRef a b (f . c)
>     fmap f (ReadRef a b) = ReadRef a (f . b)
>     fmap f (WriteRef a b c) = WriteRef a b (f c)
>     fmap g (NewDef a b c d e f) = NewDef a b c d e (g . f)
>     fmap f (CallDef a b c d) = CallDef a b c (f . d)
>     fmap f (Return a) = Return a
>     fmap f (NewEnum a b c d e) = NewEnum a b c d (f . e)
>     fmap f (If a b c d) = If a b c (f d)
>     fmap f (For a b c d e) = For a b c d (f e)
>     fmap f (While a b c) = While a b (f c)
>     fmap f (DoWhile a b c) = DoWhile a b (f c)
>     fmap f (Switch a b c d) = Switch a b c (f d)
>     fmap f Break = Break
>     fmap f Continue = Continue
>     fmap f (NewArray a b c d) = NewArray a b c (f . d)
>     fmap f (ReadArray a b c) = ReadArray a b (f . c)
>     fmap f (WriteArray a b c d) = WriteArray a b c (f d)

Thanks to this functor structure, it makes sense to embed |FoFConst|
in a |Semantics|: the machinery we build in
Chapter~\ref{sec:semantics_machinery} will take care of transforming
this functor into a free monad. Hence the following type synonym.

> type FoFCode a = Semantics FoFConst a

