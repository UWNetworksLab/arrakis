%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module IL.Paka.Syntax where

> import Text.PrettyPrint.HughesPJ
> import qualified Data.Map as Map

> import PureExpressions

%endif

\section{The Paka Intermediate Language}
\label{sec:il_paka_syntax}

The purpose of Paka is to ease the task of tracking down unnecessary
variable assignment in the to-be-generated C code. Therefore, its
syntax is extremely close to C and focused on intra-procedural
statements. This is reflected by the definition of |PakaCode|: the
structure of the C file is almost here, with includes, type
definitions and prototypes, function prototypes, and function
definitions, in this order.

Note that they are all defined by a |Map| or associative list from
|String| to something else. The |String| plays the role of an
identifier which should be compiled only once in the C
code. Typically, a type definition should appear only once, otherwise
the C compiler will complain. |Map| is used when the definition order
is not important, associative list is used when we want to keep it
(when a declaration might be defined in term of another
declaration defined earlier).

> data PakaCode
>     = PakaCode { includes     :: Map.Map String Doc,
>                  types        :: Map.Map String Doc,
>                  declarations :: [(String, Doc)],
>                  prototypes   :: Map.Map String Doc,
>                  globalVars   :: [(String, Doc)],
>                  functions    :: Map.Map String (Doc, Doc, String, Doc, PakaIntra, ILPaka) }
>
> emptyCode = PakaCode { includes = Map.empty,
>                        types = Map.empty,
>                        declarations = [],
>                        prototypes = Map.empty,
>                        globalVars = [],
>                        functions = Map.empty }

Each function is defined by a |PakaIntra| record, which stands for
\emph{intra-procedural}. In there, we find local variable definitions
and, potentially, a constant. This constant is used to carry the
result of a side-effecting test: the side-effecting is compiled before
the test-handler and the constant is tested instead.

> data PakaIntra
>     = PakaIntra { localVars :: Map.Map String Doc,
>                   expr      :: (Maybe PureExpr)}
>       deriving Show
>
> emptyIntra = PakaIntra { localVars = Map.empty, 
>                          expr = Nothing }

As part of the definition of functions, we find the body of the
function. This is presented as an |ILPaka| data-type. This is a
strip-down version of the |FoF| IL: we have kept most of the
control-flow structures (at the exception of the @for@ loop,
translated into @while@ loops) and statements. Because we are
describing intra-procedural code, we have removed the function
definition construct.

> data ILPaka
>     = PVoid
>     | PClosing PakaClosing
>     | PStatement PakaStatement ILPaka
>     | PIf ILPaka PureExpr ILPaka ILPaka ILPaka
>     | PWhile ILPaka PureExpr ILPaka ILPaka
>     | PDoWhile ILPaka ILPaka PureExpr ILPaka
>     | PSwitch PureExpr [(PureExpr, ILPaka)] ILPaka ILPaka

However, the major specificity of Paka is its definition of a
statement: a statement is either an assigment or an instruction. An
assignment |PAssign x t ys| is a term |t| in which the variable |x| is
assigned a value computed from the variables |ys|. On the other hand,
an instruction |PInstruction t ys| is a side-effecting operation |t|
making use of the variables |ys|.

In a nutshell, when chasing redundant assignments, we will track down
raw assignment |PAssign x t [y]|, remove the assignment, and replace
all use of |x| by |y|.

> data PakaStatement
>     = PAssign PakaVarName Term [PakaVarName]
>     | PInstruction Term [PakaVarName] 

A |Term| is an almost valid C statement, with holes in it. The holes
correspond to the variable names: provided with the list of variable
names, it computes a C statement.

Hence, when we have settled the input and output variables of a
|PAssign x t ys|, we obtain the corresponding C statement by applying
|t x:xs|. Similarly, we get the C code from an instruction
|PInstruction t ys| by computing |t ys|.

> type Term = [Doc] -> Doc

However, things are not that simple. First, we need more information
about the variable: are they raw C variables, or pointers, or
dereferenced from somewhere else? This information is vital to avoid
aliasing issues. 

Similarly, when a variable |y| is used in some operationally
non-trivial term |t|, we cannot simply replace |x| by |y|: we would
have to compute some sort of |t y| to be correct. Although it would be
doable, we do not support that at the moment and tag the variable name
as |Complex|, meaning ``non prone to simplification''.

Finally, constants are a gold opportunity we don't want to miss, hence
we explicitly carry the value instead of variable name. Therefore, we
are able to do some naive constant propagation for free.

> data PakaVarName
>     = Var String
>     | Ptr PakaVarName
>     | Deref PakaVarName
>     | Complex PakaVarName
>     | K PureExpr
>       deriving (Show, Eq)


> data PakaClosing
>     = PReturn PureExpr
>     | PBreak
>     | PContinue
>       deriving Show
