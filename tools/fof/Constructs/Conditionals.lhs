%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Constructs.Conditionals where

> import Data.List

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions
> import Eval

> import IL.FoF.FoF
> import IL.FoF.Compile
> import IL.FoF.Run

%endif

\section{Conditionals}
\label{chap:conditionals}

The |Conditionals| constructs consist of all control-flow operators
defined in the C language, excepted the @goto@ statement and
fall-through switches.

\subsection{Smart Constructors}

We provide the DSL designer with all standard C control-flow
operators. Hence, we define the following combinators: |ifc|, |for|,
|while|, |doWhile|, |break|, and |continue|.

> ifc :: FoFCode PureExpr -> 
>        FoFCode PureExpr -> 
>        FoFCode PureExpr -> 
>        FoFCode PureExpr
> ifc cond ifTrue ifFalse = 
>     inject (If cond ifTrue ifFalse (return Void))
>
> for :: FoFCode PureExpr -> 
>        FoFCode PureExpr -> 
>        FoFCode PureExpr -> 
>        FoFCode PureExpr -> 
>        FoFCode PureExpr
> for init cond incr loop = 
>     inject (For init cond incr loop (return Void))
>
> while :: FoFCode PureExpr ->
>          FoFCode PureExpr -> 
>          FoFCode PureExpr 
> while cond loop = 
>     inject (While cond loop (return Void))
>
> doWhile :: FoFCode PureExpr -> 
>            FoFCode PureExpr -> 
>            FoFCode PureExpr
> doWhile loop cond = 
>     inject (DoWhile loop cond (return Void))
>
> break :: FoFCode PureExpr
> break = inject Break
>
> continue :: FoFCode PureExpr
> continue = inject Continue

The |switch| statement is slightly different from the C one: every
case is automatically terminated by a @break@ statement. Hence, it is
impossible to \emph{fall through} a case.

> switch :: PureExpr -> 
>           [(PureExpr, FoFCode PureExpr)] -> 
>           FoFCode PureExpr -> 
>           FoFCode PureExpr
> switch cond cases defaultCase = 
>     inject (Switch cond cases defaultCase (return Void))


\subsection{Compile Instantiation}

The compilation step is mostly standard. Note that we often have to
compile sub-blocks of code. Therefore, we need to carefully update the
relevant binding states, so as to ensure the freshness of generated
names while respecting the scope of locally defined variables.

> compileConditionals (If condi ifTrue ifFalse r) binding =
>     (FIf compCond compIfTrue compIfFalse cont,
>      binding2)
>         where (compCond, binding1) = compileSemtoFoF' condi binding 
>               (compIfTrue, binding1') = compileSemtoFoF' ifTrue binding1 
>               (compIfFalse, binding1'') = compileSemtoFoF' ifFalse 
>                                             (binding1' |-> binding1)
>               (cont, binding2) = r (binding1'' |-> binding) 
>
> compileConditionals (While condW loop r) binding =
>     (FWhile compCond compLoop cont,
>      binding3)
>         where (compCond, binding1) = compileSemtoFoF' condW binding 
>               (compLoop, binding2) = compileSemtoFoF' loop binding1 
>               (cont, binding3 ) = r (binding2 |-> binding) 
>     
>
> compileConditionals (DoWhile loop condD r) binding =
>     (FDoWhile compLoop compCond cont,
>      binding3)
>         where (compLoop, binding1) = compileSemtoFoF' loop binding 
>               (compCond, binding2) = compileSemtoFoF' condD 
>                                      (binding1 |-> binding)
>               (cont, binding3 ) = r (binding2 |-> binding)
>
> compileConditionals (For init test inc loop r) binding =
>     (FFor compInit compTest compInc compLoop cont,
>      binding5)
>         where (compInit, binding1) = compileSemtoFoF' init binding 
>               (compTest, binding2) = compileSemtoFoF' test binding1 
>               (compInc, binding3) = compileSemtoFoF' inc binding2 
>               (compLoop, binding4) = compileSemtoFoF' loop  
>                                      (binding1 |-> binding3)
>               (cont, binding5) = r (binding4 |-> binding) 
>
> compileConditionals (Switch test cases defaultC r) binding =
>     (FSwitch test compCases compDefault cont,
>      binding3)
>         where compileCase (compCodes, binding) (i, code) =
>                   ((i, compCode) : compCodes, 
>                    (binding1 |-> binding))
>                   where (compCode, binding1) = compileSemtoFoF' code binding
>               (compCases, binding1) = 
>                   foldl' compileCase ([], binding) cases 
>               (compDefault, binding2) =
>                   compileSemtoFoF' defaultC (binding1 |-> binding) 
>               (cont, binding3) = r (binding2 |-> binding)
>
> compileConditionals Break binding =
>     (FClosing $ FBreak, binding)
>
> compileConditionals Continue binding =
>     (FClosing $ FContinue, binding)



\subsection{Run Instantiation}

The implementation of the interpreter is straightforward. We start by
dispatching calls to construct-specific functions:

> runConditionals (If a b c r) heap  = 
>     r $ runIf a b c heap
> runConditionals (For a b c d r) heap = 
>     r $ runFor a b c d heap
> runConditionals (While a b r) heap = 
>     r $ runWhile a b heap
> runConditionals (DoWhile a b r) heap = 
>     r $ runDoWhile a b heap
> runConditionals (Switch a b c r) heap = 
>     r $ runSwitch a b c heap
> runConditionals Break heap = 
>     error "runAlgebra: Break not yet implemented"
> runConditionals Continue heap = 
>     error "runAlgebra: Continue not yet implemented"

Then, we implement the semantics of each of these constructs:

> runIf :: FoFCode PureExpr -> 
>          FoFCode PureExpr -> 
>          FoFCode PureExpr -> 
>          Heap -> Heap
> runIf test ifTrue ifFalse heap = 
>     let (vtest, heap1) = run test heap in
>     let CLInteger _ _ valVtest = symbEval vtest in
>     if (valVtest /= 0) then
>        let (_, heap2) = run ifTrue heap1 in
>        heap2
>     else
>        let (_, heap2) = run ifFalse heap1 in
>        heap2
>
> runFor :: FoFCode PureExpr -> 
>           FoFCode PureExpr ->
>           FoFCode PureExpr -> 
>           FoFCode PureExpr -> 
>           Heap -> Heap
> runFor init test incr loop heap =
>     let (_, heap1) = run init heap in
>     loopWhile heap1
>         where loopWhile heap =
>                   let (vtest, heap1) = run test heap in
>                   let CLInteger _ _ valVtest = symbEval vtest in
>                   if (valVtest /= 0) then
>                      let (_, heap2) = run loop heap1 in
>                      let (_, heap3) = run incr heap2 in
>                          loopWhile heap3
>                   else heap1
>
> runWhile :: FoFCode PureExpr -> 
>             FoFCode PureExpr -> 
>             Heap -> Heap
> runWhile test loop heap =
>     let (vtest, heap1) = run test heap in
>     let (CLInteger _ _ valVtest) = symbEval vtest in
>     if (valVtest /= 0) then
>        let (_, heap2) = run loop heap1 in
>        runWhile test loop heap2
>     else heap1
>
> runDoWhile :: FoFCode PureExpr ->  
>               FoFCode PureExpr -> 
>               Heap -> Heap
> runDoWhile loop test heap =
>     let (_, heap1) = run loop heap in
>     let (vtest, heap2) = run test heap1 in
>     let CLInteger _ _ valVtest = symbEval vtest in
>     if (valVtest /= 0) then
>         runDoWhile loop test heap2
>     else
>         heap2
>
> runSwitch :: PureExpr -> 
>              [(PureExpr, FoFCode PureExpr)] -> 
>              FoFCode PureExpr -> 
>              Heap -> Heap
> runSwitch test cases defaultCase heap =
>     let res = symbEval test in
>         case res `lookup` cases of
>           Just stmt -> let (_, heap1) = run stmt heap in
>                        heap1
>           Nothing -> let (_, heap1) = run defaultCase heap in
>                      heap1

