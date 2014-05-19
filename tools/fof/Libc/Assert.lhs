%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Libc.Assert where

> import Text.PrettyPrint.HughesPJ as Pprinter

> import Semantics

> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF

%endif

\section{Assert}

The construct |Assert| embeds the C @assert@ function into FoF.

\subsection{Smart Constructors}

The use of |assert| is obvious, by its definition.

> assert :: PureExpr -> FoFCode PureExpr
> assert test = inject (Assert test (return Void))

\subsection{Compile Instantiation}

The compilation is a direct translation into a foreign function:

> compileAssert (Assert e r) binding = 
>     let (cont, binding1) = r binding in
>     (FStatement (FFFICall "assert" [e]) cont,
>      binding1)


\subsection{Run Instantiation}

As mentioned with |Printf|, we take here the easy option of ignoring
the run-time behaviour of an assertion. 

> runAssert (Assert a r) heap = r heap

Being able to capture the semantics of that operation would be helpful
when debugging a compiler. So, some efforts are worth being devoted
here.
