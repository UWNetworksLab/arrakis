%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Constructs.Typedef where

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF
> import IL.FoF.Compile

%endif

\section{Type Definition}

The |Typedef| construct provides a similar service than the C
@typedef@.

\subsection{Smart Constructors}

In particular, |Typedef| offers two combinators. The first one,
|alias| allows you to locally define a type alias.

> alias :: TypeExpr -> FoFCode PureExpr
> alias typedef = inject (Typedef typedef (return void))

The other one, |aliasE| allows you to mention an aliasing declared in
an external library, such as @<stdbool.h>@ that declares a @bool@ as
an integer.

> aliasE :: String ->
>           TypeExpr ->
>           FoFCode PureExpr
> aliasE incl typedef = inject (TypedefE incl typedef (return void))

\subsection{Compile Instantiation}

The compilation to FoF is straightforward:

> compileTypedef (Typedef (TTypedef typ aliasName) r) binding = 
>     let (cont, binding1) = r binding in
>     (FStatement (FTypedef typ aliasName) cont,
>      binding1)
>
> compileTypedef (TypedefE inclDirective typeDef@(TTypedef typ aliasName) r) binding =
>     let (cont, binding1) = r binding in
>     (FStatement (FTypedefE inclDirective typeDef) cont,
>      binding1)


\subsection{Run Instantiation}

These operations occurring at the type-level, the interpreter doesn't
pay any attention to them:

> runTypedef (Typedef _ r) heap = r heap
> runTypedef (TypedefE _ _ r) heap = r heap

