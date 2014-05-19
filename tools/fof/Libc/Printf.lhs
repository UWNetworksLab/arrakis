%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Libc.Printf where

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF


%endif

\section{Printf}

The |Printf| constructs is a simple foreign function wrapper around
the C library @printf@.

\subsection{Smart Constructors}

Provided with a format string and a list of parameters, the |printf|
Pcombinator emulates @printf@.

> printf :: String -> [PureExpr] -> FoFCode PureExpr
> printf format params = inject (Printf format params (return Void))

\subsection{Compile Instantiation}

Compilation is a natural foreign function call. Note the quoting of
|format|: we sacrify the semantics of the format string. We could
possibly apply some tricks to recover it, or to get it in a "nice"
format thanks to the |printf| combinator. However, for simplicity, we
drop its semantics for now.

> compilePrintf (Printf format params r) binding = 
>     let (cont, binding1) = r binding in
>     (FStatement (FFFICall "printf" ((quote format) : params)) cont, 
>      binding1)

\subsection{Run Instantiation}

For the reason mentioned above, it is a pain to recover the semantics
of the @printf@. Hence, we drop its side-effect when interpreting it. 

> runPrintf (Printf a b r) heap = r heap

An esthetically satisfying solution would be to store this (and
others) side-effecting operations in a stream, along with its
arguments. Hence, we could compare side-effecting programs by their
so-called \emph{trace}. By ignoring the effect of |printf| here, we
consider that side-effects have no semantic significance. This is kind
of lie when interpreting an imperative language.
