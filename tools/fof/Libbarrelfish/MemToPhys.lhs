%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Libbarrelfish.MemToPhys where

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF
> import IL.FoF.Compile
> import IL.FoF.Run

%endif

\section{Mem To Phys}

This construct embeds the libarrelfish function @mem_to_phys@ into FoF.


\subsection{Smart Constructors}

As for |HasDescendants|, both named and anonymous function are
provided. They are direct wrappers around the @mem_to_phys@ function.

> mem_to_phys :: PureExpr -> FoFCode PureExpr
> mem_to_phys cte = inject (MemToPhys Nothing cte return)
>
> mem_to_physN :: String -> PureExpr -> FoFCode PureExpr
> mem_to_physN name cte = inject (MemToPhys (Just name) cte return)

\subsection{Compile Instantiation}

Compiling is straightforward: just declare a foreign function.

> compileMemToPhys (MemToPhys mName arg r) binding = 
>     let (loc, binding1) = getFreshVar binding in
>     let name = case mName of
>                  Just x -> Provided x
>                  Nothing -> makeVarName Local loc in
>     let ref = CLRef Local uint64T name in
>     let (cont, binding2) = r ref binding1 in
>     (FStatement (FFFICall "mem_to_phys" [ref, arg]) cont,
>      binding2)


\subsection{Run Instantiation}

However, the semantics remains to be defined.

> runMemToPhys (MemToPhys _ a r) heap = error "MemToPhys: eval not implemented"
