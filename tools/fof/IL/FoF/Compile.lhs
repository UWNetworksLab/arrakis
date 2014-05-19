%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module IL.FoF.Compile where

> import Semantics
> import PureExpressions
> import {-# SOURCE #-} Expressions
> import Constructs

> import IL.FoF.FoF

%endif

\section{Translating @FoFCode@ to @IL.FoF@}
\label{sec:il_fof_compile}

\subsection{The compiler}

We already know how to translate individual statements of the
|FoFCode| language, by using the one step compiler |compileAlgebra|
defined in @./Expressions.lhs@ and provided a |Binding| capturing the
state of the compiler. The game is then to chain up these compilation
steps into a single one. Here, |foldSemantics| nicely comes to the
rescue and automatically build this compiler.

> compileSemtoFoF' :: FoFCode PureExpr -> Binding -> (ILFoF, Binding)
> compileSemtoFoF' = foldSemantics compilePure compileAlgebra

Where |compilePure| is used to compile pure expressions. Pure
expressions are, by definition, constants and returned as such. This
is used when generating tests for conditional expressions: the
computational part is generated above the test handler and only the
(pure) result is tested.

> compilePure :: PureExpr -> Binding -> (ILFoF, Binding)
> compilePure x binding = (FConstant x, binding)

For our convenience, we can define the following |compileSemToFoF|
function that takes a closed |FoFCode| and compiles it in the empty
environment: that's our compiler for self-contained expressions.

> compileSemtoFoF :: FoFCode PureExpr -> ILFoF
> compileSemtoFoF term = fst $ compileSemtoFoF' term emptyBinding
>     where emptyBinding = Binding { freshVar = 1,
>                                    defStructs = [],
>                                    defUnions = [],
>                                    defEnums = [] }


\subsection{The machinery}

\subsubsection{Manipulating the compiler environment}

We very often need to generate fresh names, while keeping the
freshness invariant of the compiler environment. The following
function just does that:

> getFreshVar :: Binding -> (Int, Binding)
> getFreshVar binding = (loc, binding1)
>     where loc = freshVar binding
>           binding1 = binding { freshVar = loc + 1 }

Note that a clever implementation would be something of type:

> better_getFreshVar :: Binding -> (Int -> Binding -> t) -> t
> better_getFreshVar binding f = undefined

Which enforces the fact that the function |f| is provided a
synchronized compiler state. This ensures that people don't
inadvertently mess up the compiler state. This remark holds for too
many functions below, I'm a bit sad about that.

In order to ensure the freshness of names across bindings, we define
the following |passFreshVar| function that builds a |stableBinding|
whose fresh variables are ensured not to clash with the one generated
using |upBinding|. Similarly, it carries the structures defined in
|upBinding|.

> passFreshVar :: Binding -> Binding -> Binding
> passFreshVar upBinding stableBinding =
>     stableBinding { freshVar = freshVar upBinding,
>                     defStructs = defStructs upBinding,
>                     defUnions = defUnions upBinding,
>                     defEnums = defEnums upBinding }
> (|->) = passFreshVar


From variable identifier and an origin, we can later make
|VarName|. In a craze of Hungarian naming, the origin dictactes the
name of variables.

> makeVarName :: Origin -> Int -> VarName
> makeVarName orig loc = Generated $ makeVarName' orig loc
>     where makeVarName' Local x = "fof_x" ++ show x
>           makeVarName' Param x = "fof_y" ++ show x
>           makeVarName' Dynamic x = "fof_d" ++ show x
>           makeVarName' Global x = "fof_g" ++ show x

The Hungarian fever can go further: when a variable is somehow related
to another |VarName|, the |heritVarName| makes it explicit at the name
level by deriving a fresh name from the previous one.

> heritVarName :: Binding -> VarName -> (Int, VarName, Binding)
> heritVarName binding name = (loc, Inherited loc name, binding1)
>     where (loc, binding1) = getFreshVar binding 
>     


\subsubsection{From Expressions to Types}

Let us be honest: the code which follows is tricky. Change something
there and the generated code will be wrong, if it is not already. I'm
looking at you |readOf| and |liftType|. They came to life during the
implementation of References and its painful compiler. After a
lot of work, I came to the conclusion (and proof) that they are
correct. The question is now: are they correct when mixed with complex
data-types, such as structs and arrays. The practician seems to say
``yes'', the theoretician remains proofless. 

The intrinsic difficulty is that a Reference abstracts both a C
variable and a C pointer. However, in C, both concepts are quite
distinct. Hence, the compiler needs to be clever to translate the
unified notion of Reference in two semantically different
objects. Hence that horrible machinery.

\paragraph{|typeOf|:}

Obviously, there exists a map going from each well-typed element of
|PureExpr| to an element of |TypeExpr|. Hence, this map assigns a
\emph{type} to a given, well-typed expression. As for ill-typed
expressions, we simply return an error message. 

Computing the type of base values as well as of unary operations is
straightforward:

> typeOf :: PureExpr -> TypeExpr
> typeOf (Void) = TVoid
> typeOf (CLInteger sign size _) = TInt sign size
> typeOf (CLFloat _) = TFloat
> typeOf (CLRef _ typ _) = typ
> typeOf (Unary _ x) = typeOf x

A binary operation is well-typed if and only if both sub-terms are
well-typed and of same type. The same goes for the branches of a
conditional expression:

> typeOf (Binary _ x y) = 
>     if (typeOfx == typeOfy) then
>        typeOfx
>     else error "typeOf: Binop on distinct types."
>
>     where typeOfx = typeOf x
>           typeOfy = typeOf y
>
> typeOf (Test _ t1 t2) =
>     if (typeOft1 == typeOft2) then
>        typeOft1
>     else error "typeOf: Test exits on distinct types"
>     
>     where typeOft1 = typeOf t1 
>           typeOft2 = typeOf t2 



By convention, the value returned by |sizeof| is an unsigned 64 bits
integer:

> typeOf (Sizeof _) = TInt Unsigned TInt64

Finally, the type of a casted expression is the assigned type. Note
that we do not judge of the legality of this cast here. This aspect is
handled by the dynamic semantics of FoF's meta-language.

> typeOf (Cast t _) = t

\paragraph{|readOf| and |unfoldPtrType|:}

When we \emph{read} the content of the reference cell, of type
|TPointer typeCell modeCell|, the type of the object read is either:
\begin{itemize}
\item A constant of type |typeCell|, or
\item A reference cell of type |typeCell|, in a |Read| mode
\end{itemize}

We can distinguish both cases thanks to |typeCell|. If |typeCell| is a
|TPointer| itself (first case, below), this means that we are dealing
with a reference cell. If |typeCell| is a base type (second case), this means that
this is a constant.

> readOf :: TypeExpr -> TypeExpr
> readOf (TPointer typ _) = TPointer typ Read
> readOf x = x
>
> unfoldPtrType :: PureExpr -> TypeExpr
> unfoldPtrType (CLRef _ (TPointer typ _) _) = readOf typ


\paragraph{|liftType|:}

Although our Reference Cell representation abstracts away the
distinction between variables and pointers, it has one drawback. A
variable is assigned a |TPointer| type, whereas, in C, we will be
working one |TPointer|-level below: our reference cell types
corresponds to the same C type but one pointer dereference. Hence, we
introduce the following lifting function:

> liftType :: TypeExpr -> TypeExpr
> liftType (TPointer x _) = x
> liftType x = x


\paragraph{|deref|:}

The |deref| is another operator dealing with the specify of reference
cells. In the compiler, we translate the high-level reference cell
operators by pointer manipulations and assignment. Therefore, when
manipulating a reference cell, we will not interested in its actual
content but its address. Hence the following function. Values will
manipulated just as usual, by value.

%if false

What about ... ? CLRef _ (TPointer _ _) _ : CLRef _ (TPointer _ _) _    ??

%endif

> deref :: PureExpr -> String
> deref (CLRef _ (TPointer _ _) _) = "&"
> deref _ = ""

