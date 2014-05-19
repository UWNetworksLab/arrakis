%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Constructs.Arrays where

> import Data.Maybe

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import Eval

> import IL.FoF.FoF
> import IL.FoF.Compile

%endif

\section{Arrays}

The |Array| construct, as well as the subsequent constructs, is
organized as follow. First, we define some smart constructors, which
are directly used by the DSL designer when implementing the
compiler. Then, we implement the one-step interpreter and compiler to
FoF.

|Array| offers an abstraction over C arrays, both statically defined
or statically allocated. Hence, it offers the possibility to create,
read from, and write into arrays.

\subsection{Smart Constructors}

We can create dynamic and static anonymous arrays using the following
combinators:

> newArray :: [Data] -> FoFCode Loc
> newArray value = inject (NewArray Nothing DynamicArray value return)

> newStaticArray :: [Data] -> FoFCode Loc
> newStaticArray value = inject (NewArray Nothing (StaticArray $ length value) value return)

Similarly, they can be named:

> newArrayN :: String -> [Data] -> FoFCode Loc
> newArrayN name value = inject (NewArray (Just name) DynamicArray value return)

> newStaticArrayN :: String -> [Data] -> FoFCode Loc
> newStaticArrayN name value = inject (NewArray (Just name) (StaticArray $ length value) value return)

Then, we can read the content of an array:

> readArray :: Loc -> Index -> FoFCode Data
> readArray l f = inject (ReadArray l f return)

As well as write some data in a cell:

> writeArray :: Loc -> Index -> Data -> FoFCode ()
> writeArray l f d = inject (WriteArray l f d (return ()))


\subsection{Run Instantiation}

The interpretation of an array operation is dispatched by the
following code.

> runArrays :: FoFConst (Heap -> (a, Heap)) -> (Heap -> (a, Heap))
> runArrays (NewArray a b c r) heap = uncurry r $ runNewArray b c heap
> runArrays (ReadArray a b r) heap = uncurry r $ runReadArray a b heap
> runArrays (WriteArray a b c r) heap = r $ runWriteArray a b c heap

Creating, reading, and writing to or from an array are trivially
implemented by the following code:

> runNewArray :: AllocArray -> [Data] -> Heap -> (Loc, Heap)
> runNewArray alloc initData heap = 
>     let loc = freshALoc heap in
>     let sizeInt = length initData in
>     let name = makeVarName Dynamic loc in
>     let ref = CLRef Dynamic (TArray alloc $ typeOf $ head initData) name in
>     let heap1 = heap { freshALoc = loc + 1,
>                        arrayMap = (name, initData) : (arrayMap heap) } in
>     (ref, heap1)
>
> runReadArray :: Loc -> Index -> Heap -> (Data, Heap)
> runReadArray (CLRef _ (TArray _ _) loc) index heap = 
>     let array = fromJust $ loc `lookup` (arrayMap heap) in
>     let (CLInteger _ _ indexInt) = symbEval index in
>     let val = array !! (fromInteger indexInt) in
>     (val, heap)
>
> runWriteArray :: Loc -> Index -> Data -> Heap -> Heap
> runWriteArray (CLRef _ (TArray _ _) loc) index dat heap = 
>     let array = fromJust $ loc `lookup` (arrayMap heap) in
>     let (CLInteger _ _ indexInt) = symbEval index in
>     let (arrayBegin, arrayEnd) = splitAt (fromInteger indexInt) array in
>     let array1 = arrayBegin ++ (dat : tail arrayEnd) in
>     let heap1 = heap { arrayMap = (loc, array1) : arrayMap heap } in
>     heap1

\subsection{Compile Instantiation}

Similarly, the compilation of array operations consists in
implementing the following function:

> compileArrays :: FoFConst (Binding -> (ILFoF, Binding)) ->
>                  (Binding -> (ILFoF, Binding))

The translation from the |FoFConst| terms to |FoF| terms is almost
automatic. The added value of this process consists in generating or
deriving names for the references.

> compileArrays (NewArray name allocArray dat r) binding =
>     let scopeVar 
>               = case allocArray of
>               DynamicArray -> Dynamic
>               StaticArray _ -> Global in
>     let (publicName, binding1)
>             = case name of 
>               Just x -> (Provided x, binding)
>               Nothing -> 
>                   let (loc, binding1) = getFreshVar binding in
>                   (makeVarName scopeVar loc, 
>                    binding1) in
>     let typeOfDat = typeOf $ head dat in
>     let ret = CLRef Dynamic (TArray allocArray typeOfDat) publicName in
>     let (cont, binding2) = r ret binding in
>     (FStatement (FNewArray publicName allocArray dat) cont, 
>      binding2)
>
> compileArrays (ReadArray ref@(CLRef origin (TArray arrayAlloc typ) xloc) index r) binding =
>     let (loc, name, binding1) = heritVarName binding xloc in
>     let ret = CLRef Dynamic (readOf typ) name in
>     let (cont, binding2) = r ret binding1 in
>     (FStatement (FReadArray name ref index) cont,
>      binding2)
>
> compileArrays (WriteArray ref@(CLRef origin 
>                                      (TArray arrayAlloc typ) 
>                                      xloc) 
>                           index dat r) binding =
>     let (cont, binding1) = r binding in
>     (FStatement (FWriteArray ref index dat) cont,
>      binding1)


