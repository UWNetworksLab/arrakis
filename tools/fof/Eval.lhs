%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Eval where

> import Data.Bits as B

> import PureExpressions

%endif

\section{Functional core interpreter}
\label{sec:semantics_core}


In this Section, we implement an expression evaluator. Given
any (correct) expression, it will compute the corresponding value. The
implementation is decomposed in several steps. In Section
\ref{sec:sub:eval_top}, we evaluate top-level expressions. Doing so,
we rely on case-specific evaluators. This includes unary operators
(Section \ref{sec:sub:eval_unary}), binary operators (Section
\ref{sec:sub:eval_binary}), the sizeof operation (Section
\ref{sec:sub:eval_sizeof}), the conditional operation (Section
\ref{sec:sub:eval_test}), and the cast operation (Section
\ref{sec:sub:eval_cast}).

Note that the following functions are \emph{partial}: not all
expressions can be successfully evaluated. Indeed, some operations are
simply meaningless. For example, computing the sum of a structure and
a float is illegal. Currently, we are simply ignore these errors and this might
result in run-time errors of the DSL compiler. Satisfactory solutions of
this problem exist, though. For example, we could implement a
type-checker that would ensure the absence of run-time errors. Another
approach would be improve our error handling code.


\subsubsection{Top-level Evaluation}
\label{sec:sub:eval_top}

The purpose of this section is implement the following function:

> symbEval :: PureExpr -> PureExpr

That reduces a given expression to a value. Hence, for values, this is
trivial:

> symbEval Void = Void
> symbEval x@(CLInteger _ _ _) = x
> symbEval x@(CLFloat _) = x
> symbEval x@(CLRef _ _ _) = x

Then, for inductive constructions, we rely on the specific functions
implemented in the following sections.

> symbEval (Unary op x) = 
>     symbEvalUnary op x'
>         where x' = symbEval x
>
> symbEval (Binary op x y) = 
>     symbEvalBinary op x' y'
>         where x' = symbEval x
>               y' = symbEval y
>
> symbEval (Sizeof typ) = symbEvalSizeof typ
>
> symbEval (Test x y z) = 
>     symbEvalTest x' y z
>         where x' = symbEval x
>
> symbEval (Cast t x) = 
>     symbEvalCast t x'
>         where x' = symbEval x


\subsubsection{Unary Operator Evaluation}
\label{sec:sub:eval_unary}

For unary operators, we need to implement the following function:

> symbEvalUnary :: UnaryOp -> PureExpr -> PureExpr

Hence the following code:

> symbEvalUnary Minus x =
>     case x of 
>       CLInteger Signed size x -> CLInteger Signed size (-x)
>       CLFloat x -> CLFloat (-x)
>       _ -> error "symbEvalUnary: minus on wrong type"
>
> symbEvalUnary Complement x = 
>     case x of
>       CLInteger sg sz x -> CLInteger sg sz (complement x)
>       _ -> error "symbEvalUnary: complement on wrong type"
>
> symbEvalUnary Negation x =
>     case x of 
>       CLInteger sg sz 0 -> CLInteger sg sz 1
>       CLInteger sg sz _ -> CLInteger sg sz 0
>       _ -> error "symbEvalUnary: negation on wrong type"


\subsubsection{Binary Operator Evaluation}
\label{sec:sub:eval_binary}

For binary operators, here is our goal:

> symbEvalBinary :: BinaryOp -> PureExpr -> PureExpr -> PureExpr

Achieved by the following, messy codes.

\paragraph{Arithmetic Operations\\}

> symbEvalBinary Plus (CLInteger sg si x) (CLInteger sg' si' y)
>     | sg == sg' && si == si' = CLInteger sg si (x + y)
>     | otherwise = error "symbEvalBinary: Plus undefined"
> symbEvalBinary Plus (CLInteger _ _ x) (CLFloat y) =
>     CLFloat ( (fromRational $ toRational x) + y )
> symbEvalBinary Plus (CLFloat x) (CLInteger _ _ y) = 
>     CLFloat ( x + (fromRational $ toRational y) )
> symbEvalBinary Plus (CLFloat x) (CLFloat y) = CLFloat ( x + y)
> symbEvalBinary Plus _ _ = error "symbEvalBinary: Plus undefined"

More checks should be added here. For examples, we should ensure that
the result of the subtraction of two unsigned numbers is still
positive, or make it wrap.

> symbEvalBinary Sub (CLInteger sg si x) (CLInteger sg' si' y)
>     | sg == sg' && si == si' = CLInteger sg si (x - y)
>     | otherwise = error "symbEvalBinary: Sub undefined"
> symbEvalBinary Sub (CLInteger _ _ x) (CLFloat y) = 
>     CLFloat ( (fromRational $ toRational x) - y )
> symbEvalBinary Sub (CLFloat x) (CLInteger _ _ y) = 
>     CLFloat ( x - (fromRational $ toRational y) )
> symbEvalBinary Sub (CLFloat x) (CLFloat y) = CLFloat ( x - y)
> symbEvalBinary Sub _ _ = error "symbEvalBinary: Sub undefined"
>
> symbEvalBinary Mul (CLInteger sg si x) (CLInteger sg' si' y) 
>     | sg == sg' && si == si' = CLInteger sg si (x * y)
>     | otherwise = error "symbEvalBinary: Mul undefined"
> symbEvalBinary Mul (CLInteger _ _ x) (CLFloat y) = 
>     CLFloat ( (fromRational $ toRational x) * y )
> symbEvalBinary Mul (CLFloat x) (CLInteger _ _ y) = 
>     CLFloat ( x * (fromRational $ toRational y) )
> symbEvalBinary Mul (CLFloat x) (CLFloat y) = CLFloat ( x * y )
> symbEvalBinary Mul _ _ = error "symbEvalBinary: Mul undefined"
>
> symbEvalBinary Div (CLInteger sg si x) (CLInteger sg' si' y)
>     | sg == sg' && si == si' = CLInteger sg si (x `div` y)
>     | otherwise = error "symbEvalBinary: Div undefined"
> symbEvalBinary Div (CLInteger _ _ x) (CLFloat y) = 
>     CLFloat ( (fromRational $ toRational x) / y )
> symbEvalBinary Div (CLFloat x) (CLInteger _ _ y) = 
>     CLFloat ( x / (fromRational $ toRational y) )
> symbEvalBinary Div (CLFloat x) (CLFloat y) = CLFloat (x / y)
> symbEvalBinary Div _ _ = error "symbEvalBinary: Div undefined"
>
> symbEvalBinary Mod (CLInteger sg si x) (CLInteger sg' si' y) 
>     | sg == sg' && si == si' = CLInteger sg si (x `mod` y)
>     | otherwise = error "symbEvalBinary: Mod undefined"
> symbEvalBinary Mod _ _ = error "symbEvalBinary: Mod undefined"


\paragraph{Boolean Operations\\}


> symbEvalBinary Shl (CLInteger sg si x) (CLInteger sg' si' y) 
>     | sg == sg' && si == si' = CLInteger sg si (shiftL x (fromInteger y))
>     | otherwise = error "symbEvalBinary: Shl undefined"
> symbEvalBinary Shl _ _ = error "symbEvalBinary: Shl undefined"
>
> symbEvalBinary Shr (CLInteger sg si x) (CLInteger sg' si' y) 
>     | sg == sg' && si == si' = CLInteger sg si (shiftR x (fromInteger y))
>     | otherwise = error "symbEvalBinary: Shr undefined"
> symbEvalBinary Shr _ _ = error "symbEvalBinary: Shr undefined"
>
> symbEvalBinary AndBit (CLInteger sg si x) (CLInteger sg' si' y) 
>     | sg == sg' && si == si' = CLInteger sg si (x B..|. y)
>     | otherwise = error "symbEvalBinary: And undefined"
> symbEvalBinary AndBit _ _ = error "symbEvalBinary: And undefined"
>
> symbEvalBinary OrBit (CLInteger sg si x) (CLInteger sg' si' y) 
>     | sg == sg' && si == si' = CLInteger sg si (x B..&. y)
>     | otherwise = error "symbEvalBinary: Or undefined"
> symbEvalBinary OrBit _ _  = error "symbEvalBinary: Or undefined"
>
> symbEvalBinary XorBit (CLInteger sg si x) (CLInteger sg' si' y) 
>     | sg == sg' && si == si' = CLInteger sg si (x `xor` y)
>     | otherwise = error "symbEvalBinary: Xor undefined"
> symbEvalBinary XorBit _ _ = error "symbEvalBinary: Xor undefined"


\paragraph{Comparison Operations\\}


> symbEvalBinary op (CLInteger sg si x) (CLInteger sg' si' y) 
>     | sg == sg' && si == si' = symbEvalComp op x y
>     | otherwise = error ("symbEvalBinary: " ++ show op ++ " undefined")
> symbEvalBinary op (CLInteger _ _ x) (CLFloat y) = 
>     symbEvalComp op (fromRational $ toRational x) y
> symbEvalBinary op (CLFloat x) (CLInteger _ _ y) = 
>     symbEvalComp op x (fromRational $ toRational y)
> symbEvalBinary op (CLFloat x) (CLFloat y) = symbEvalComp op x y

> symbEvalBinary Le _ _ = error "symbEvalBinary: Le undefined"
> symbEvalBinary Leq _ _ = error "symbEvalBinary: Leq undefined"
> symbEvalBinary Ge _ _ = error "symbEvalBinary: Leq undefined"
> symbEvalBinary Geq _ _ = error "symbEvalBinary: Leq undefined"
> symbEvalBinary Eq _ _ = error "symbEvalBinary: Leq undefined"
> symbEvalBinary Neq _ _ = error "symbEvalBinary: Leq undefined"

> symbEvalComp :: (Ord a, Num a) => BinaryOp -> a -> a -> PureExpr
> symbEvalComp op x y =
>     let cmp = case op of
>                 Le -> (<)
>                 Leq -> (<=)
>                 Ge -> (>)
>                 Geq -> (>=)
>                 Eq -> (==)
>                 Neq -> (/=) in
>         if cmp x y then 
>             CLInteger Unsigned TInt64 1 
>         else CLInteger Unsigned TInt64 0

\subsubsection{Sizeof Evaluation}
\label{sec:sub:eval_sizeof}

Our |sizeof| operator follows the corresponding C operation:

> symbEvalSizeof :: TypeExpr -> PureExpr
> symbEvalSizeof TVoid = CLInteger Unsigned TInt64 1
> symbEvalSizeof (TInt _ TInt8) = CLInteger Unsigned TInt64 1
> symbEvalSizeof (TInt _ TInt16) = CLInteger Unsigned TInt64 2
> symbEvalSizeof (TInt _ TInt32) = CLInteger Unsigned TInt64 4
> symbEvalSizeof (TInt _ TInt64) = CLInteger Unsigned TInt64 8
> symbEvalSizeof TFloat = CLInteger Unsigned TInt64 4
> symbEvalSizeof (TPointer _ _) = CLInteger Unsigned TInt64 8
> symbEvalSizeof (TCompPointer _) = CLInteger Unsigned TInt64 8
> symbEvalSizeof (TArray _ typ) = CLInteger Unsigned TInt64 8
> symbEvalSizeof (TStruct _ _ fields) = CLInteger Unsigned TInt64 8
> symbEvalSizeof (TUnion _ _ fields) = CLInteger Unsigned TInt64 8

%if false 

 symbEvalSizeof (TFunction _ _ _) = CLInteger Unsigned TInt64 1

%endif


\subsubsection{Conditionals Evaluation}
\label{sec:sub:eval_test}

The semantics of the conditional mimics a restricted version of the C
standard: True corresponds to everything which is not a float or
integer equal to zero. Hence, we evaluate the corresponding branch
accordingly.

> symbEvalTest :: PureExpr -> PureExpr -> PureExpr -> PureExpr
> symbEvalTest (CLInteger _ _ 0) _ y = symbEval y
> symbEvalTest (CLFloat 0) _ y = symbEval y
> symbEvalTest _ x _ = symbEval x


\subsubsection{Cast Evaluation}
\label{sec:sub:eval_cast}

Here is our stripped-down version of |cast|. It will probably deserve
some work in the future, as it is quite restrictive. Also, it should
ensure that the type modification are reflected on the data:
converting a signed, negative number to an unsigned form changes the
value of this number. This is currently unsupported.

> symbEvalCast :: TypeExpr -> PureExpr -> PureExpr
> symbEvalCast (TInt sg sz) (CLInteger sg' sz' x) 
>     | sg' < sg && sz' < sz = CLInteger sg sz x
>     | otherwise = error "symbEvalCast: illegal integer cast"
> symbEvalCast TFloat (CLInteger _ _ x) = 
>     CLFloat (fromRational $ toRational x)
> symbEvalCast TFloat vx@(CLFloat x) = vx
> symbEvalCast _ _ = 
>     error "symbEvalCast: Not yet implemented/undefined cast"
