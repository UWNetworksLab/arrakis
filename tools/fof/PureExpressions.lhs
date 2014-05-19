%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module PureExpressions where

> import Data.Char

> import {-# SOURCE #-} Constructs

%endif

\section{Filet-o-Fish pure expressions}
\label{sec:fof_syntax_core}

The core of Filet-o-Fish is organized around the purely functional
core of C. It consists of C types as well as C expressions. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Types}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsubsection{Data-type Definitions}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The |TypeExpr| data-type encompasses the following types:
\begin{itemize}
        \item Void,
        \item Integers, of various signedness and size,
        \item Float,
        \item Named structures and unions,
        \item Named pointers, ie. a pointer recurring in a structure
             or union,
        \item Arrays, and
        \item Pointers
\end{itemize}

Note that a value of type |TInt| or |TFloat| is a \emph{constant},
like $2$, $3 / 7$, or \verb!sizeof(struct foo)!. In FoF meta-language,
a C variable is \emph{not} a value -- but a construct. So, the type of
the variable
$x$ defined by \verb!int32_t x = 4! is \emph{not} |TInt Signed TInt32|. 

> data TypeExpr = TVoid
>               | TInt Signedness Size
>               | TFloat
>               | TChar
>               | TStruct AllocStruct String TFieldList
>               | TUnion AllocUnion String TFieldList
>               | TCompPointer String 
>               | TEnum String [(String, Int)]
>               | TArray AllocArray TypeExpr
>               | TPointer TypeExpr Mode
>               | TTypedef TypeExpr String
>               | TFun String Function TypeExpr [(TypeExpr, Maybe String)]
>                 deriving (Eq, Show)

%%%%%%%%%%%%%%%%
\paragraph{Functions}
%%%%%%%%%%%%%%%%

A function is represented by an Haskell function, taking a list of
arguments and computing the body of the function. In the jargon, this
is called an \emph{higher-order abstract syntax}. So, the function
definition is represented by the following type:

> data Function = Fun ([PureExpr] -> FoFCode PureExpr)

Because |TypeExpr| is showable, |Function| has to be showable
too. While we could define a more complete |Show| instance for
|Function|, we will not do so here and simply return an opaque name.

> instance Show Function where
>     show _ = "<fun>"

Concerning equality, this becomes more tricky. We would have to define
what "equality" means and if that definition is decidable. Here, we
consider syntactic equality and although we could decide whether two
functions are syntactically equal or not, we will not do so for the
moment. We simply consider functions as always distinct.

> instance Eq Function where
>     _ == _ = False


%%%%%%%%%%%%%%%%
\paragraph{Composed data-types}
%%%%%%%%%%%%%%%%


Composed data-types have several allocation policies: they might be
declared dynamically, using malloc, or statically, on the stack. This
is reflected by the following definitions. We chose to use differents
definitions for each kind of data-type because they are likely to
evolve in future versions and diverge from this common scheme.

> data AllocStruct = StaticStruct
>                  | DynamicStruct
>                    deriving (Eq, Show)
> data AllocUnion = StaticUnion
>                 | DynamicUnion
>                    deriving (Eq, Show)
> data AllocArray = StaticArray Int
>                 | DynamicArray
>                    deriving (Eq, Show)

Both Structures and Unions rely on the |TFieldList|
synonym. Basically, the type of a Structure corresponds to its name
as well as the list of its field names and respective types.

> type TFieldList = [(String, TypeExpr)]


%%%%%%%%%%%%%%%%
\paragraph{Integers}
%%%%%%%%%%%%%%%%

Signedness and size of integers is defined as usual. An integer is
either signed or unsigned and its size may vary from 8 to 64
bits. Interestingly, we derive |Ord| on these data-types: |Ord|
provides us with a comparison function on the signedness and size. In
practice, we can check that a cast is a correct \emph{downcasting} by
enforcing that the sign and size we cast to is \emph{bigger} than the
original sign and size.

> data Signedness = Unsigned 
>                 | Signed
>                   deriving (Eq, Ord, Show)
>
> data Size = TInt8
>           | TInt16
>           | TInt32
>           | TInt64
>             deriving (Eq, Ord, Show)    


%%%%%%%%%%%%%%%%
\paragraph{Pointers}
%%%%%%%%%%%%%%%%

As we understand that the suspense is unbearable, we are going to
reveal you the type of $x$ defined above. Actually, the type of $x$ is
|TPointer (TInt Signed TInt32) Avail|. A pointer? Indeed, a variable
does actually \emph{points} to a location in memory. This choice
allows us to capture the notion of variables and pointers in a
single abstraction, called a \emph{reference cell}.

A reference cell can be in one of the following states: either
|Avail|able or |Read|. This
distinction makes sense during the compilation process, it can ignored
otherwise.

> data Mode = Avail
>           | Read
>             deriving (Eq, Show)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsubsection{Smart Constructors}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

In some circumstances, it is necessary to explicitly write the type of
an expression. However, explicitly combining the previously defined
types can be quite cumbersome. For example, we can naturally define
the base types as follow:

> voidT :: TypeExpr
> voidT = TVoid
>
> uint8T, uint16T, uint32T, uint64T :: TypeExpr 
> uint8T = TInt Unsigned TInt8
> uint16T = TInt Unsigned TInt16
> uint32T = TInt Unsigned TInt32
> uint64T = TInt Unsigned TInt64
>
> int8T, int16T, int32T, int64T :: TypeExpr
> int8T = TInt Signed TInt8
> int16T = TInt Signed TInt16
> int32T = TInt Signed TInt32
> int64T = TInt Signed TInt64
>
> floatT :: TypeExpr
> floatT = TFloat
>
> charT :: TypeExpr
> charT = TChar

> uintptrT :: TypeExpr
> uintptrT = TCompPointer "void"


And, similarly, we can build up composed types by applying them on
smaller types:

> arrayDT :: TypeExpr -> TypeExpr
> arrayDT typ = TArray DynamicArray typ 
>
> arrayST :: Int -> TypeExpr -> TypeExpr
> arrayST size typ = TArray (StaticArray size) typ 
>
> ptrT :: TypeExpr -> TypeExpr
> ptrT typ = TPointer typ Avail
>
> structDT, unionDT,
>  structST, unionST :: String -> TFieldList -> TypeExpr
> structDT name fields = TStruct DynamicStruct name fields
> unionDT name fields = TUnion DynamicUnion name fields
> structST name fields = TStruct StaticStruct name fields
> unionST name fields = TUnion StaticUnion name fields
>
> enumT :: String -> [(String, Int)] -> TypeExpr
> enumT name fields = TEnum name fields
>
> typedef :: TypeExpr -> String -> TypeExpr
> typedef typ name = TTypedef typ name

Finally, the named pointer -- which is actually a \emph{fix-point} --
takes as input the name of the structure or union it refers to.

> cptrT :: String -> TypeExpr
> cptrT id = TCompPointer id


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Pure Expressions}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

In a first step, we are going to define the expressions composing FoF
meta-language. As for types, this consists in a data-type, |PureExpr|,
capturing the syntax of expressions. Then, we also define some smart
constructors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsubsection{Data-type Definitions}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An expression is one of the following object:
\begin{itemize}
\item |void|, the only object populating the type |Void|,
\item an integer, of specific signedness and size,
\item a float,
\item a reference to an object in memory,
\item a unary operation, applied to an object,
\item a binary operation, applied on two objects,
\item the |sizeof| operator, applied to a type,
\item a conditional expression, testing an object against $0$,
      returning one of two objects, and
\item a |cast| operator, casting an object to a given type
\end{itemize}


> data PureExpr = Void
>               | CLInteger Signedness Size Integer
>               | CLFloat Float
>               | CLChar Char
>               | CLRef Origin TypeExpr VarName
>               | Unary UnaryOp PureExpr
>               | Binary BinaryOp PureExpr PureExpr
>               | Sizeof TypeExpr
>               | Test PureExpr PureExpr PureExpr
>               | Cast TypeExpr PureExpr
>               | Quote String
>                 deriving (Eq, Show)

%%%%%%%%%%%%%%%%
\paragraph{Variable names}
%%%%%%%%%%%%%%%%

A reference is identified by a name. A |Generated| name has been
forged by FoF. A |Provided| name has been defined by the compiler
designer. An |Inherited| name results from an operation performed on
another variable. We carefully track the origin of names for compilation purpose: for
example, if a variable name has been |Generated|, we should try to
eliminate it, to make the compiled code more readable.

> data VarName = Generated String
>              | Provided String
>              | Inherited Int VarName
>                deriving (Show, Eq)

A reference is also decorated by its \emph{origin}. This field is used by
the compiler to identify the scope of variables. Therefore, the
compiler can enforce some safety checks, such as verifying that the
address of a local variable is not assigned to a global one, for
example. Sadly, this information is not always precisely maintained
nor correctly used in the current implementation. More care and more
checks should be added in the future, to ensure the correctness of the
generated code.

> data Origin = Local
>             | Global
>             | Param
>             | Dynamic
>               deriving (Eq, Show)

%%%%%%%%%%%%%%%%
\paragraph{Unary operations}
%%%%%%%%%%%%%%%%

The unary operations are either the arithmetic \emph{minus} operation,
or the logic \emph{complement} operation, or the logic \emph{negation}
operation.

> data UnaryOp = Minus | Complement | Negation
>              deriving (Eq, Show)

%%%%%%%%%%%%%%%%
\paragraph{Binary operations}
%%%%%%%%%%%%%%%%

The binary operations are either arithmetic operators ($+$, $-$,
$\times$, $/$, and $\%$), Boolean operators ($<<$, $>>$, $\&$,
bitwise-or, and \^{}), or comparison operators ($<$, $<=$, $>$, $>=$,
$==$, and $!=$).

> data BinaryOp = Plus | Sub | Mul    | Div   | Mod
>               | Shl  | Shr | AndBit | OrBit | XorBit 
>               | Le   | Leq | Ge     | Geq   | Eq  | Neq
>                 deriving (Eq, Show)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsubsection{Smart Constructors}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

As usual, we define some constructors for the C programmer to feel at
home with FoF. Let us start with the constants first:

> void :: PureExpr
> void = Void
>
> int8, int16, int32, int64 :: Integer -> PureExpr
> int8 x = CLInteger Signed TInt8 x
> int16 x = CLInteger Signed TInt16 x
> int32 x = CLInteger Signed TInt32 x
> int64 x = CLInteger Signed TInt64 x
>
> uint8, uint16, uint32, uint64 :: Integer -> PureExpr
> uint8 x = CLInteger Unsigned TInt8 x
> uint16 x = CLInteger Unsigned TInt16 x
> uint32 x = CLInteger Unsigned TInt32 x
> uint64 x = CLInteger Unsigned TInt64 x
>
> charc :: Char -> PureExpr
> charc x = CLInteger Unsigned TInt8 (toInteger $ ord x)
>
> float :: Float -> PureExpr
> float x = CLFloat x
>
> cchar :: Char -> PureExpr
> cchar x = CLChar x

> opaque :: TypeExpr -> String -> PureExpr
> opaque t s = CLRef Local t (Provided s)

Then come the unary operators:

> minus, comp, neg :: PureExpr -> PureExpr
> minus = Unary Minus 
> comp = Unary Complement
> neg = Unary Negation 

And the binary operators. Note that they are defined
\emph{infix}. Therefore, it becomes possible to write the following
code:

> exampleInfix :: PureExpr
> exampleInfix = (uint8 1) .<. ((uint8 2) .+. (uint8 4))

Although not specified yet, we could have set up the left/right
associativity and precedence rules of these operators. This would
reduce the parenthesizing overhead. It is just a matter of doing it.

> (.+.), (.-.), (.*.), (./.), (.%.),
>          (.<<.), (.>>.), (.&.), (.|.), (.^.),
>          (.<.), (.<=.), (.>.), 
>          (.>=.), (.==.), (.!=.) :: PureExpr -> PureExpr -> PureExpr
> (.+.) = Binary Plus
> (.-.) = Binary Sub
> (.*.) = Binary Mul
> (./.) = Binary Div
> (.%.) = Binary Mod
> (.<<.) = Binary Shl
> (.>>.) = Binary Shr
> (.&.) = Binary AndBit
> (.|.) = Binary OrBit
> (.^.) = Binary XorBit
> (.<.) = Binary Le
> (.<=.) = Binary Leq
> (.>.) = Binary Ge
> (.>=.) = Binary Geq
> (.==.) = Binary Eq
> (.!=.) = Binary Neq

Finally, |sizeof|, conditionals, and |cast| have their straightforward
alter-ego in FoF:

> sizeof :: TypeExpr -> PureExpr
> sizeof t = Sizeof t
>
> test :: PureExpr -> PureExpr -> PureExpr -> PureExpr
> test c ift iff = Test c ift iff
>
> cast :: TypeExpr -> PureExpr -> PureExpr
> cast t e = Cast t e


When compiling foreign function calls, one might need to turn a
(Haskell) string into a FoF \emph{quote} object. This is achieved by
the following combinator. One must avoid using this operation as much
as possible: this quotation has no semantic meaning, therefore one
should use it only when we are really sure we are not interested in
the quoted semantic anymore.

> quote :: String -> PureExpr
> quote s = Quote s