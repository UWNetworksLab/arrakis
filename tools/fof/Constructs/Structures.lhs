%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Constructs.Structures where

> import Data.Maybe

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF
> import IL.FoF.Compile

%endif

\section{Structures Definition}

The |Structure| construct allows you to mirror the @struct@ data-type
of C. It is composed by a |newStruct| combinator, to instantiate an
element of this type, a |readStruct| combinator, to read a field from
a structure, and a |writeStruct| combinator, to write into a field.

\subsection{Smart Constructors}

As often with instantiation operators, we can chose between statically
or dynamically allocating the value. Then, it is possible to chose
between an anonymous or a named value. All these choices are provided
by the following four combinators.

> newStaticStruct :: String -> 
>                    [(TypeExpr, String, Data)] -> 
>                    FoFCode Loc
> newStaticStruct name stt = 
>     inject (NewStruct Nothing StaticStruct name 
>                       (map (\(t,n,v) -> (n,(t,v))) stt) 
>                       return)
>
> newStaticStructN :: String ->
>                     String -> 
>                    [(TypeExpr, String, Data)] -> 
>                    FoFCode Loc
> newStaticStructN nameStr name stt = 
>     inject (NewStruct (Just nameStr) StaticStruct name 
>                       (map (\(t,n,v) -> (n,(t,v))) stt) 
>                       return)
>
> newStruct :: String -> 
>              [(TypeExpr, String, Data)] -> 
>              FoFCode Loc
> newStruct name stt = 
>     inject (NewStruct Nothing DynamicStruct name 
>                       (map (\(t,n,v) -> (n,(t,v))) stt) 
>                       return)
>
> newStructN :: String ->
>               String -> 
>              [(TypeExpr, String, Data)] -> 
>              FoFCode Loc
> newStructN nameStr name stt = 
>     inject (NewStruct (Just nameStr) DynamicStruct name 
>                       (map (\(t,n,v) -> (n,(t,v))) stt) 
>                       return)

Follow the read and write combinators:

> readStruct :: Loc -> String -> FoFCode Data
> readStruct l f = inject (ReadStruct l f return)
>
> writeStruct :: Loc -> String -> Data -> FoFCode ()
> writeStruct l f d = inject (WriteStruct l f d (return ()))

\subsection{Compile Instantiation}

Apart from type handling, the compilation naturally follows the
definition. As often, computing the |CLRef| is a magic voodoo, which
is far from being provably correct.

> compileStructures (NewStruct refName allocStruct name fields r) binding =
>     (FStatement newS cont,
>      binding2)
>         where (loc, binding1) = getFreshVar binding 
>               structName = case refName of
>                            Just x -> Provided x 
>                            Nothing -> makeVarName Dynamic loc 
>               fieldsTypeStr = [ (field, typ)
>                                 | (field,(typ,_)) <- fields] 
>               typeStr = TStruct DynamicStruct name fieldsTypeStr 
>               ret = CLRef Dynamic typeStr structName 
>               (cont, binding2) = r ret binding1 
>               newS = FNewStruct structName allocStruct name fields
>
> compileStructures (ReadStruct ref@(CLRef origin 
>                                          typ@(TStruct alloc name fields) 
>                                          xloc) 
>                               field r) binding =
>     (FStatement readS cont,
>      binding2)
>         where (loc, varName, binding1) = heritVarName binding xloc 
>               typeField = fromJust $ field `lookup` fields
>               ret = CLRef (allocToOrigin alloc) (readOf typeField) varName 
>               (cont, binding2) = r ret binding1
>               readS = FReadStruct varName ref field
>               allocToOrigin StaticStruct = Local
>               allocToOrigin DynamicStruct = Dynamic
>
> compileStructures (WriteStruct ref@(CLRef origin 
>                                           typ@(TStruct alloc name fields) 
>                                           xloc) 
>                                field 
>                                value r) binding =
>     (FStatement writeS  cont,
>      binding1)
>         where (cont, binding1) = r binding 
>               writeS = FWriteStruct ref field value


\subsection{Run Instantiation}

The interpreter follows with a dispatcher:

> runStructures (NewStruct _ a b c r) heap = 
>     uncurry r $ runNewStruct a b c heap
> runStructures (ReadStruct a b r) heap = 
>     uncurry r $ runReadStruct a b heap
> runStructures (WriteStruct a b c r) heap = 
>     r $ runWriteStruct a b c heap

And the per-construct implementation:

> runNewStruct :: AllocStruct ->
>                 String -> 
>                 [(String, (TypeExpr,Data))] -> 
>                 Heap -> (Loc, Heap)
> runNewStruct alloc name struct heap =
>     let structT = map (\(x1,(x2,_)) -> (x1,x2)) struct in
>     let structD = map (\(x1,(_,x2)) -> (x1,x2)) struct in
>     let loc = freshLoc heap in
>     let structs = strMap heap in
>     let varName = makeVarName Local loc in
>     let heap1 = heap { freshLoc = loc + 1 } in
>     let heap2 = heap1 { strMap = (varName, structD) : structs } in
>     (CLRef Local (TStruct alloc name structT) varName, heap2)
>
> runReadStruct :: Loc -> String -> Heap -> (Data, Heap)
> runReadStruct (CLRef _ _ location) field heap =
>     let structs = strMap heap in
>     let struct = fromJust $ location `lookup` structs in
>     let val = fromJust $ field `lookup` struct in
>     (val, heap)
>
> runWriteStruct :: Loc -> String -> Data -> Heap -> Heap
> runWriteStruct (CLRef _ _ location) field value heap =
>     let structs = strMap heap in
>     let struct = fromJust $ location `lookup` structs in
>     let struct1 = (field, value) : struct in
>     let structs1 = (location, struct1) : structs in
>     heap { strMap = structs1 }