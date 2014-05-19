%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Constructs.Strings where

> import Data.Maybe

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF
> import IL.FoF.Compile

%endif

\section{Strings}

The |String| construct corresponds to static arrays of
characters. However, they are implemented here as a special case as
they are specially dealt with by the C compiler. 

\subsection{Smart Constructors}

We only provide string creation combinators: accessing a string can be
achieved thanks to |Arrays| combinators. As usual, we provide two
combinators: one to create an anonymous string, one to create a named
string.

> newString :: String -> FoFCode Loc
> newString value = inject (NewString Nothing value return)
>
> newStringN :: String -> String -> FoFCode Loc
> newStringN name value = inject (NewString (Just name) value return)

\subsection{Compile Instantiation}

The compilation is straightforward, on the model of static array
declaration.

> compileString (NewString name dat r) binding =
>     let (publicName, binding1)
>             = case name of
>                 Just x -> (Provided x, binding)
>                 Nothing -> 
>                     let (loc, binding1) = getFreshVar binding in
>                     (makeVarName Global loc,
>                      binding1) in
>     let ret = CLRef Global 
>                     (TArray (StaticArray $ length dat) TChar) 
>                     publicName in
>     let (cont, binding2) = r ret binding1 in
>     (FStatement (FNewString publicName dat) cont,
>      binding2)


\subsection{Run Instantiation}

Similarly, the interpreter is simple.

> runString (NewString a b r) heap = uncurry r $ runNewString b heap

> runNewString :: String -> Heap -> (Loc, Heap)
> runNewString string heap = 
>     let loc = freshALoc heap in
>     let size = length string in
>     let name = makeVarName Dynamic loc in
>     let ref = CLRef Dynamic (TArray (StaticArray size) TChar) name in
>     let heap1 = heap { freshALoc = loc + 1,
>                        arrayMap = (name, map cchar string) : (arrayMap heap) } in
>     (ref, heap1)
