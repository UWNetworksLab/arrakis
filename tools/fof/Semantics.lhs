%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Semantics where

%endif

\section{Plumbing Machinery}
\label{sec:semantics_machinery}


The material presented in this chapter relies on some hairy concepts
from Category Theory. If you are curious about these things, Edward
Kmett wrote a nice blog post~\cite{kmett-free-monad} on the
subject. The first version of FoF, and in particular this file, relied
on Wouter Swierstra solution to the expression
problem~\cite{swierstra-expression}. However, the burden of this
approach on the type-system was unbearable for our users. 

Our motivation is to build a monad in which one can naturally write
sequential code, just as an imperative language. Each construct of the
language is defined in @Constructs@ by the |FoFConst|
data-type. Purposely, this data-type implements a functor. The code
below generically turn a functor |f| into a |Semantics f|
monad. Hence, in @Constructs@, we apply this machinery to make a monad
out of |FoFConst|.


\subsection{The Semantics Monad}


We build a monad |Semantics f| out of a function |f| thanks to the
following data-type:

> data Semantics f a = Pure a
>                    | Impure (f (Semantics f a))


First of all, we show that this defines a functor:

> instance Functor f => Functor (Semantics f) where
>     fmap f (Pure x) = Pure (f x)
>     fmap f (Impure t) = Impure (fmap (fmap f) t)

Then, we obtain the monad:

> instance Functor f => Monad (Semantics f) where
>     return = Pure
>     (Pure x) >>= f = f x
>     (Impure t) >>= f = Impure (fmap (>>= f) t)

Terms are embedded into the monad thanks the following function:

> inject :: f (Semantics f a) -> Semantics f a
> inject x = Impure x



\subsection{Folding the Free Monad}


Finally, once we have built the monad, we will need to manipulate its
content. For example, we will be willing to evaluate it, or to compile
it, etc. All these operations can be implemented by folding over the
monadic code, that is traversing the constructs in their definition
order and computing an output of type @b@. Note that we have to
distinguish |Pure| terms, which are simply values, from |Impure| ones,
which are the embedded constructs.

> foldSemantics :: Functor f => (a -> b) -> (f b -> b) -> Semantics f a -> b
> foldSemantics pure imp (Pure x) = pure x
> foldSemantics pure imp (Impure t) = imp $ fmap (foldSemantics pure imp) t


\subsection{Sequencing in the Free Monad}

Provided a list of monadic code, we are able to turn them into a
single monadic code returning a list of terms. This corresponds to the
|sequence| function in the IO monad:

> sequenceSem ms = foldr k (return []) ms
>     where k m m' = 
>               do
>                 x <- m
>                 xs <- m'
>                 return (x : xs)
