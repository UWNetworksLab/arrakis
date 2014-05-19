%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module IL.FoF.Run where

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

%endif


\section{Evaluator}
\label{sec:il_fof_run}


Just as for the compiler, described in the previous section, the
implementation of the Filet-o-Fish interpreter is automatically
derived from the one-step interpreters. Again, |foldSemantics| comes
to the rescue and computes the interpreter:

> run :: Semantics FoFConst PureExpr -> Heap -> (PureExpr, Heap)
> run = foldSemantics (,) runAlgebra


