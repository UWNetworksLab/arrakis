%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Constructs.Functions where

> import Data.List
> import Data.Maybe

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF
> import IL.FoF.Compile
> import IL.FoF.Run


%endif

\section{Function Definition}

This module abstracts the function definition and manipulation
mechanisms found in C. This consists in a |def| constructor, to define
functions, a |call| and |callN| functions to call functions, as well
as a |returnc| combinator to return from a function call.

\subsection{Smart Constructors}

When defining a function, we provide a list of attributes, its name,
its body, its return type, and a list of arguments types:

> def :: [FunAttr] ->
>        String -> 
>        ([PureExpr] -> FoFCode PureExpr) ->
>        TypeExpr ->
>        [(TypeExpr, Maybe String)] ->
>        FoFCode PureExpr
> def attr name fun returnT argsT = 
>     inject (NewDef attr name (Fun fun) returnT argsT return)


Then, it is possible to call into a function, provided a list of
parameters. The result, if any, can be named by using the |callN|
construct.

Currently, both the interpreter and the compiler are extremely
optimistic about their inputs: in the future, we should add more
safety checks. For example, we should check that we are calling the
functions with the right arguments.

> call :: PureExpr -> [PureExpr] -> FoFCode PureExpr
> call funRef params = 
>     inject (CallDef Nothing funRef params return)
>
> callN :: String -> PureExpr -> [PureExpr] -> FoFCode PureExpr
> callN varName funRef params = 
>     inject (CallDef (Just varName) funRef params return)

Finally, it is possible to return from a function thanks to the usual
@return@. This should not be confused with the monadic |return| of
Haskell.

> returnc :: PureExpr -> FoFCode PureExpr
> returnc value = inject (Return value)

\subsection{Compile Instantiation}

Compiling functions is a little bit more tricky than usual. It
requires generating or handling arguments, as well as handling the
return value, if any. This corresponds to the following code.

> compileFunctions (NewDef attr nameF (Fun func) return args r)
>                  binding =  
>     (FNewDef attr nameF compBody return instanceArgs cont,
>      binding2)
>       where instanceArgs = instanciateArgs args 
>             (compBody, binding1) = compileSemtoFoF' (func instanceArgs) binding 
>             ref = CLRef Global (TFun nameF (Fun func) return args) (Provided nameF)
>             (cont, binding2) = r ref (binding1 |-> binding)
>             instanciateArgs :: [(TypeExpr, Maybe String)] -> [PureExpr]
>             instanciateArgs params = reverse $ foldl' instanciateArg [] $ 
>                                                zip [1..] params
>                 where instanciateArg l (idx, (typ, mName)) = (CLRef Param typ name) : l
>                           where name = case mName of
>                                        Just x -> Provided x
>                                        Nothing -> makeVarName Param idx

>
> compileFunctions (CallDef mName f@(CLRef _ (TFun nameF 
>                                                func 
>                                                returnT 
>                                                argsT) _)
>                                  args r) binding =
>     (FStatement (FCallDef name f args) cont,
>      binding2)
>         where (name, binding1) 
>                   = case returnT of
>                     TVoid -> (Nothing, binding)
>                     _ -> case mName of
>                          Just x -> (Just $ Provided x, binding)
>                          Nothing -> 
>                              (Just $ makeVarName Local loc,
>                               binding') 
>                              where (loc, binding') = getFreshVar binding 
>               (cont, binding2) 
>                   = case returnT of 
>                     TVoid -> r Void binding1
>                     _ -> r (CLRef Local 
>                                   returnT 
>                                   (fromJust name))
>                          binding1 

The translation of the |return| statement, on the other hand, is
trivial.

> compileFunctions (Return e) binding =
>     (FClosing $ FReturn e, binding)

\subsection{Run Instantiation}

As usual, we dispatch here:

> runFunctions (NewDef _ _ f _ _ r) heap = 
>     uncurry r $ runNewDef f heap
> runFunctions (CallDef _ a b r) heap = 
>     uncurry r $ runCallDef a b heap
> runFunctions (Return a) heap =
>     runReturn a heap -- OK??

And compute there:

> runReturn :: PureExpr -> Heap -> (PureExpr, Heap)
> runReturn e heap = (e,heap)
>
> runNewDef :: Function -> Heap -> (PureExpr, Heap)
> runNewDef function heap = 
>   (CLRef Global (TFun undefined function undefined undefined) undefined, heap)
>
> runCallDef :: PureExpr -> [PureExpr] -> Heap -> 
>                           (PureExpr, Heap)
> runCallDef (CLRef _ (TFun _ (Fun function) _ _) _) args heap =
>     let (result, heap1) = run (function args) heap in
>     (result, heap1)
