%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module IL.Paka.Optimizer where

> import Data.List
> import qualified Data.Map as Map

> import IL.Paka.Syntax
> import IL.Paka.Compile

%endif

\section{@IL.Paka@ Code Optimizer}
\label{sec:il_paka_optimizer}

The currently implemented optimizer is a naive redundant assignment
simplifier, which happens to do constant propagation at the same
time. It is naive in the several dimensions. An important one is that
it is entirely hard-coded, while we all know that optimization is
simply a matter of dataflow analysis. So, at some point, we should use
a more generic framework for that. It is also naive because it does
not try to reach a fix-point: it is single phase, while it is obvious
that more assignments could still be eliminated in subsequent
phases. Finally, it is naive because any case that was not easy to
deal with have been discarded: more redundant assignments could be
removed if the logic were more precise.

The purpose of that module is to show that ``it is possible to do
optimization''. It is a proof of concept. Now, it is Future Work
(Chapter~\ref{chap:future_work}) to get a clever optimization
framework. The ease I had in implementing that stuff convince me that
we are not far from this heaven.

So, if you want optimized Paka code, you will only get a slightly less
redundant code:

> optimizePaka :: PakaCode -> PakaCode
> optimizePaka = optimizeAssgmtElim

Because this analysis is intra-procedural, we go over each function
and apply an intra-procedural optimizer:

> optimizeAssgmtElim :: PakaCode -> PakaCode
> optimizeAssgmtElim code = code { functions = optFunc }
>     where funcs = functions code
>           optFunc = Map.mapMaybe (\(b,c,d,e,f,fun) -> Just (b,c,d,e,f, assgmtElim fun)) funcs

\subsection{Implementation}

This optimizer is quite easy to implement, assuming we have the right
tools at hand. That is, assuming that we are able to replace a
variable |x| by a variable |y| in a code |k| -- using 
|replace (Var x) (Var y) k| --, that we are able to say if a
variable |y| is either a constant or never used in a code |k| -- using 
|isUsed flatten y k| --, and that we are able to say if a variable |x|
is used without side-effects in a code |k| -- 
using |isUsed flattenS x k|.

The intra-procedural optimizer will turn an |ILPaka| into a better
|ILPaka|:

> assgmtElim :: ILPaka -> ILPaka

The interesting case is obviously the variable assignment: a value
|y| is assigned to a variable |x|. We remove that assignment and
replace |x| by |y| if and only if |y| is never used again and |x| is
not involved in some weird computation. Otherwise, we go ahead.

A small issue here is that we ask for |y| to be never used
again. That's quite restrictive. This results in being able to carry
only 4 assignment eliminations on today's Hamlet and Fugu inputs. This
is shame, compared to the numerous opportunities. To solve that issue,
we would have to extend or re-design |isUsed| to allow the definition
of more fine-grained predicates, such as ``is overwritten''.

> assgmtElim (PStatement a@(PAssign (Var x) _ [Var y]) k) =
>     if (not (isUsed flatten y k))
>        && (not (isUsed flattenS x k)) then
>         assgmtElim $ replace (Var x) (Var y) k
>     else
>         PStatement a $
>         assgmtElim k

All other assignments that do not fit this scheme, or the
instructions are skipped:

> assgmtElim (PStatement a k) =
>     PStatement a $
>     assgmtElim k

Finally, control-flow operators are simply iterated over:

> assgmtElim (PIf c t ifT ifF k) =
>     PIf 
>     (assgmtElim c)
>     t 
>     (assgmtElim ifT)
>     (assgmtElim ifF)
>     (assgmtElim k)
> assgmtElim (PWhile c t l k) =
>     PWhile
>     (assgmtElim c)
>     t
>     (assgmtElim l)
>     (assgmtElim k)
> assgmtElim (PDoWhile l c t k) =
>     PDoWhile 
>     (assgmtElim l)
>     (assgmtElim c)
>     t 
>     (assgmtElim k)
> assgmtElim (PSwitch t cases d k) =
>     PSwitch
>     t
>     (map (\(a,b) -> (a, assgmtElim b)) cases)
>     (assgmtElim d)
>     (assgmtElim k)
> assgmtElim x = x

\subsection{Code predication}

First, if I correctly remember my Software Testing lecture, a
\emph{use site} is a place where a variable is read. In opposition to
a \emph{def site} where a variable is written to. Well, then the
following is misleading.

|isUsed f x k| tells you that |x| has been found in a use or def site
of |k| in a situation where it played a role caught by |f|. To
simplify, |isUsed flatten| will catch any kind of use or def. |isUsed
flattenS| will catch a use or def in a |Complex| state.

As for the implementation, it is simply going over |ILPaka| terms and
doing the necessary on |PStatement|.

> isUsed :: (PakaVarName -> Maybe String) -> String -> ILPaka -> Bool
> isUsed p var PVoid = False
> isUsed p var (PClosing (PReturn k)) = Just var == (flatten $ pakaValName k)
> isUsed p var (PClosing _) = False
> isUsed p var (PStatement s k) = isUsedStmt s || isUsed p var k
>                               where isUsedStmt (PAssign t _ ls) =
>                                         Just var `elem` map flatten (t : ls)
>                                     isUsedStmt (PInstruction _ ls) =
>                                         Just var `elem` map flatten ls
> isUsed p var (PIf c t ifT ifF k) 
>     = (Just var == (flatten $ pakaValName t)) ||
>       (isUsed p var c || isUsed p var ifT 
>       || isUsed p var ifF || isUsed p var k)
> isUsed p var (PWhile c t l k) 
>     = (Just var == (flatten $ pakaValName t)) ||
>       isUsed p var c || isUsed p var l || isUsed p var k
> isUsed p var (PDoWhile l c t k)
>     = (Just var == (flatten $ pakaValName t)) ||
>       isUsed p var c || isUsed p var l || isUsed p var k
> isUsed p var (PSwitch t c d k) 
>     = (Just var == (flatten $ pakaValName t)) ||
>       foldl' (\a (_,b) -> a || isUsed p var b) False c 
>       || isUsed p var d || isUsed p var k

In light of the explanation above, the definition of |flatten| and
|flattenS| should be obvious. Aren't they?

> flatten :: PakaVarName -> Maybe String
> flatten (Var s) = Just $ s
> flatten (Ptr x) = flatten x
> flatten (Deref x) = flatten x
> flatten (Complex x) = flatten x
> flatten (K _) = Nothing 
>
> flattenS :: PakaVarName -> Maybe String
> flattenS (Var s) = Nothing
> flattenS (Ptr x) = Nothing
> flattenS (Deref x) = Nothing
> flattenS (Complex x) = flatten x
> flattenS (K _) = Nothing

\subsection{Code transformation}

As for |replace|, it is by now standard: go over the terms, hunt the
|dest|, and kill it with |source|. It is surgical striking, in its
full glory.

> replace :: PakaVarName -> PakaVarName -> ILPaka -> ILPaka
> replace dest source (PStatement (PAssign dst stmt srcs) k) = 
>     PStatement (PAssign dst stmt srcs') 
>     (replace dest source k)
>     where srcs' = replaceL dest source srcs
> replace dest source (PStatement (PInstruction stmt srcs) k) =
>     PStatement (PInstruction stmt srcs') 
>     (replace dest source k)
>     where srcs' = replaceL dest source srcs
> replace dest source (PIf c t ifT ifF k) =
>     PIf (replace dest source c) t
>         (replace dest source ifT) 
>         (replace dest source ifF)
>         (replace dest source k)
> replace dest source (PWhile c t l k) =
>     PWhile (replace dest source c)
>            t
>            (replace dest source l)
>            (replace dest source k)
> replace dest source (PDoWhile l c t k) =
>     PDoWhile (replace dest source l)
>              (replace dest source c)
>              t
>              (replace dest source k)
> replace dest source (PSwitch t cases d k) =
>     PSwitch t
>             (map (\(a,b) -> (a, replace dest source b)) cases)
>             (replace dest source d)
>             (replace dest source k)
> replace dest source x = x
>
> replaceL x y = map (\z -> if z == x then y else z)
