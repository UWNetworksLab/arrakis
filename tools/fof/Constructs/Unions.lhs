%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Constructs.Unions where

> import Data.Maybe

> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import Semantics

> import IL.FoF.FoF
> import IL.FoF.Compile

%endif

\section{Unions Definition}

The |Union| constructs abstracts the @union@ data-type of C.

\subsection{Smart Constructors}

Hence, creating an union is available in four flavors, statically or
dynamically allocated, and anonymous or named.

> newStaticUnion :: String -> 
>                   [(TypeExpr, String)] -> 
>                   String ->
>                   Data ->
>                   FoFCode Loc
> newStaticUnion name fields field dat = 
>     inject (NewUnion Nothing StaticUnion name 
>                      (map (\(s1,s2) -> (s2,s1)) fields)
>                      (field, dat)
>                      return)
>
> newStaticUnionN :: String ->
>                    String -> 
>                   [(TypeExpr, String)] -> 
>                   String ->
>                   Data ->
>                   FoFCode Loc
> newStaticUnionN nameU name fields field dat = 
>     inject (NewUnion (Just nameU) StaticUnion name 
>                      (map (\(s1,s2) -> (s2,s1)) fields)
>                      (field, dat)
>                      return)
>
> newUnion ::  String -> 
>              [(TypeExpr, String)] -> 
>              String ->
>              Data ->
>              FoFCode Loc
> newUnion name fields field dat = 
>     inject (NewUnion Nothing DynamicUnion
>                      name 
>                      (map (\(s1,s2) -> (s2,s1)) fields)
>                      (field, dat)
>                      return)
>
> newUnionN :: String ->
>              String -> 
>              [(TypeExpr, String)] -> 
>              String ->
>              Data ->
>              FoFCode Loc
> newUnionN nameU name fields field dat = 
>     inject (NewUnion (Just nameU) DynamicUnion
>                      name 
>                      (map (\(s1,s2) -> (s2,s1)) fields)
>                      (field, dat)
>                      return)

Reading and writing follow the usual scheme:

> readUnion :: Loc -> String -> FoFCode Data
> readUnion l f = inject (ReadUnion l f return)

> writeUnion :: Loc -> String -> Data -> FoFCode ()
> writeUnion l f d = inject (WriteUnion l f d (return ()))

\subsection{Compile Instantiation}

As usual the difficulty of the compilation stands in not messing up
created and read types. Apart from that, it is a simple translation.

> compileUnions (NewUnion refName allocUnion nameU fields (initField, initData) r) binding = 
>     (FStatement newU cont, 
>      binding2)
>         where typeUnion = TUnion DynamicUnion nameU fields 
>               (loc, binding1) = getFreshVar binding 
>               name = case refName of
>                    Nothing -> makeVarName Dynamic loc 
>                    Just x -> Provided x 
>               ret = CLRef Dynamic typeUnion name 
>               (cont, binding2) = r ret binding1 
>               newU = FNewUnion name allocUnion nameU fields (initField, initData)
>
> compileUnions (ReadUnion ref@(CLRef _ typeU@(TUnion alloc
>                                                     nameU 
>                                                     fields) xloc) 
>                          field r) binding =
>     (FStatement readU cont,
>      binding2)
>         where (loc, name, binding1) = heritVarName binding xloc 
>               typeField = fromJust $ field `lookup` fields 
>               origin = allocToOrigin alloc
>               ret = CLRef origin (readOf typeField) name
>               (cont, binding2) = r ret binding1
>               readU = FReadUnion name ref field
>               allocToOrigin StaticUnion = Local
>               allocToOrigin DynamicUnion = Dynamic
>
> compileUnions (WriteUnion ref@(CLRef origin 
>                                  typ@(TUnion alloc _ fields) 
>                                  xloc) 
>                            field 
>                            value r) binding =
>     (FStatement writeU cont,
>      binding1)
>         where (cont, binding1) = r binding 
>               writeU = FWriteUnion ref field value


\subsection{Run Instantiation}

This part has not been implemented yet. Hence, the interpreter will
blow up in presence of unions. To get an idea of the desired
implementation, take a look at the reference cells interpreter. It
should be similarly easy.

> runUnions (NewUnion _ a b c d r) heap = error "runUnions: not yet implemented"
> runUnions (ReadUnion a b r) heap = error "runUnions: not yet implemented"
> runUnions (WriteUnion a b c r) heap = error "runUnions: not yet implemented"

