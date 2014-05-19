%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt


%if false

> module Constructs.References where

> import Text.PrettyPrint.HughesPJ as Pprinter
> import Data.Maybe

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF
> import IL.FoF.Compile

%endif

\section{Reference Cells}
\label{chap:references}

The reference cell construct provides an abstraction to both variables
and C pointers. It composed by three combinators to create, read from,
and write to reference cells. It can be compared to OCaml references
or Haskell @IORef@.

\subsection{Smart Constructors}

A reference cell is created in an initialized state. The variant
|newRefN| allows the DSL designer to provide a name to the created
variable.

> newRef :: Data -> FoFCode Loc
> newRef d = inject (NewRef Nothing d return)
>
> newRefN :: String -> Data -> FoFCode Loc
> newRefN name d = inject (NewRef (Just name) d return)

Follow primitives to read from and write to these reference cells:

> readRef :: Loc -> FoFCode Data
> readRef l = inject (ReadRef l return)
>
> writeRef :: Loc -> Data -> FoFCode PureExpr
> writeRef l d = inject (WriteRef l d (return Void))

The current implementation lacks lots of sanity checks:
\begin{itemize}
        \item read and Write on CLRef,
        \item write from and to compatible types,
        \item do not write local pointers into param/global ones,
        \item \ldots
\end{itemize}


\subsection{Compile Instantiation}

The compilation is tricky when it comes to computing the pointer
type. I wouldn't be surprised if some bugs were lying there. This
concerns |newRef| and |readRef|, which effect on references is not
trivial. 

> compileReferences (NewRef refName ref r) binding =
>     (FStatement (FNewRef publicName ref) cont,
>      binding2)         
>         where (publicName, binding1)
>                   = case refName of
>                     Just x -> (Provided x, binding)
>                     Nothing -> 
>                         let (loc, binding1) = getFreshVar binding in
>                         (makeVarName Local loc, binding1) 
>               ret = CLRef Local (TPointer (typeOf ref) Avail) publicName 
>               (cont, binding2) = r ret binding1 
>
> compileReferences (ReadRef ref@(CLRef _ _ xloc) r) binding =
>     (FStatement (FReadRef name ref) cont, 
>      binding2)
>         where (loc, name, binding1) = heritVarName binding xloc 
>               ret = CLRef Local (unfoldPtrType ref) name 
>               (cont, binding2) = r ret binding1

|writeRef| is straightforward.

> compileReferences (WriteRef ref d r) binding =
>     (FStatement (FWriteRef ref d) cont,
>      binding1)
>         where (cont, binding1) = r binding 


\subsection{Run Instantiation}

On the other hand, the implementation of the interpreter is much
simpler. We start with the dispatcher:

> runReferences (NewRef _ d r) heap = uncurry r $ runNewRef d heap
> runReferences (ReadRef l r) heap = uncurry r $ runReadRef l heap
> runReferences (WriteRef l v r) heap = r $ runWriteRef l v heap

And the per-construct interpreters follow:

> runNewRef :: Data -> Heap -> (Loc, Heap)
> runNewRef value heap =
>     ( CLRef Local typeOfVal name, heap2 )
>         where typeOfVal = typeOf value 
>               loc = freshLoc heap
>               refs = refMap heap 
>               name = makeVarName Local loc 
>               heap1 = heap { freshLoc = loc + 1 } 
>               heap2 = heap1 { refMap = (name, value) : refs } 
>
> runReadRef :: Loc -> Heap -> (Data, Heap)
> runReadRef (CLRef _ _ location) heap =
>     let refs = refMap heap in
>     let val = fromJust $ location `lookup` refs in
>     (val, heap)
>
> runWriteRef :: Loc -> Data -> Heap -> Heap
> runWriteRef (CLRef _ _ location) value heap =
>     let refs = refMap heap in
>     let refs1 = (location, value) : refs in
>     heap { refMap = refs1 }