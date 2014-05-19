%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module IL.Paka.Builders where

> import Text.PrettyPrint.HughesPJ hiding (first)
> import qualified Data.Map as Map
> import Debug.Trace

> import PureExpressions

> import IL.Paka.Syntax

%endif

\section{Paka building blocks}

I'm particularly proud of the Paka code generation architecture. To
build a Paka term, we simply call some builders functions which are
chained up together with the |#| operator. These builders take care of
inserting the definitions in the right place in |PakaCode|,
|PakaIntra|, or sequentially extend the |ILPaka| code. Thanks to that
machinery, we don't have to explicitly build these data-structures, we
just call functions.

Hence, a builder is just putting a brick in the |PakaBuilding| wall:

> type PakaBuilding = (ILPaka -> ILPaka, PakaCode, PakaIntra)

That is, operations taking some arguments and extending a
|PakaBuilding| into a new one.


\subsection{Low-level machinery}

To give a feeling of ``sequential code'', the |#| operator is simply
an inversed composition operation:

> f # g = \x -> g (f x) 

Using |#|, we will compose our builders with a sequential feeling.

Because most, if not all, operations modify one element of the
|PakaBuilding| triple, we define the following combinators:

> first :: (a -> b) -> (a, c, d) -> (b, c, d)
> first f (a,b,c) = (f a, b, c)
>
> second :: (a -> b) -> (c, a, d) -> (c, b, d)
> second f (a,b,c) = (a, f b, c)
>
> third :: (a -> b) -> (c, d, a) -> (c, d, b)
> third f (a,b,c) = (a,b,f c)


\subsection{Building |PakaCode|}

We can add new C includes:

> include :: String -> PakaBuilding -> PakaBuilding
> include id = second $ include' id 
>     where include' id globalEnv 
>               = case id `Map.lookup` incls of
>                   Nothing -> globalEnv { includes = Map.insert id decl incls }
>                   Just _ -> globalEnv
>               where incls = includes globalEnv
>                     decl = text "#include" <+> text id 

We can declare new C types:

> declare :: String -> Doc -> Doc -> PakaBuilding -> PakaBuilding
> declare id typ decl = second $ declare' id typ decl
>     where declare' id typ decl globalEnv =
>               case id `Map.lookup` typs of
>                 Nothing -> globalEnv { declarations = (id,decl) : decls,
>                                        types = Map.insert id typ typs }
>                 Just _ -> globalEnv
>               where decls = declarations globalEnv
>                     typs = types globalEnv

We can declare global variables:

> globalVar :: String -> Doc -> PakaBuilding -> PakaBuilding
> globalVar id def = second $ globalVar' id def
>     where globalVar' id def globalEnv =
>               case id `lookup` vars of
>                 Nothing -> globalEnv { globalVars = (id,def) : vars  }
>                 Just _ -> globalEnv
>               where vars = globalVars globalEnv

We can add function prototypes:

> prototype :: String -> Doc -> PakaBuilding -> PakaBuilding
> prototype id proto = second $ prototype' id proto
>     where prototype' id proto globalEnv =
>               case id `Map.lookup` protos of
>                 Nothing -> globalEnv { prototypes = Map.insert id proto protos }
>                 Just _ -> globalEnv
>               where protos = prototypes globalEnv

And we can define new functions:

> function :: Doc -> Doc -> String -> Doc -> PakaIntra -> ILPaka -> PakaBuilding -> PakaBuilding
> function returnT attrs funName funArgs lEnv body = 
>     second $ function' returnT attrs funName funArgs lEnv body
>     where function' returnT attrs funName funArgs lEnv body gEnv =
>               case funName `Map.lookup` functions' of
>                 Nothing -> gEnv { functions = Map.insert funName (returnT, attrs, funName, funArgs, lEnv, body) functions' }
>                 Just _ -> gEnv
>               where functions' = functions gEnv


\subsection{Building |PakaIntra|}

As for global variables in the |PakaCode|, we can add local variables
in the |PakaIntra| environment:

> localVar :: String -> Doc -> PakaBuilding -> PakaBuilding
> localVar id def = third $ localVar' id def
>     where localVar' id def localEnv 
>               = case id `Map.lookup` vars of
>                   Nothing -> localEnv { localVars = Map.insert id def vars  }
>                   Just _ -> localEnv
>               where vars = localVars localEnv

And we can bring a constant in the |PakaIntra|:

> constant :: PureExpr -> PakaBuilding -> PakaBuilding
> constant e = third $ constant' e
>     where constant' e lEnv = lEnv { expr = Just e }

\subsection{Building |ILPaka|}

Obviously, the serious stuff happens in |ILPaka|, or more precisely
|ILPaka -> ILPaka|: this code is seriously continuation-passing. The
plan is that we want to build a |ILPaka| value. However, we note that,
for instance, to build a |PStatement| value, we need to know the
remaining code. But we don't know it yet, as we are compiling it! So,
we return a continuation that waits for that uncompiled chunk and plug
it in the right place. Continuation-passing style, yay!

As an example of that technique in action, take a look at |instr| and
|assgn| below. Apart from that CPS detail, they are computationally
trivial, bringing their arguments in the right place of the
constructor and returning by calling the continuation.

> instr :: Term -> [PakaVarName] -> PakaBuilding -> PakaBuilding
> instr instruction vars = first $ instr' instruction vars
>     where instr' instruction varNames k 
>               = \c ->
>                 k $ PStatement (PInstruction instruction varNames) c
>
> assgn :: PakaVarName -> Term -> [PakaVarName] -> PakaBuilding -> PakaBuilding
> assgn wVarName assgnmt rVarNames = first $ assgn' wVarName assgnmt rVarNames
>     where assgn' wVarName assgnmt rVarNames k
>               = \c ->
>                 k $ PStatement (PAssign wVarName assgnmt rVarNames) c

As you can expect, we need to stop ``continuating'' at some
point. This naturally fits with the role of closing terms:

> close :: PakaClosing -> PakaBuilding -> PakaBuilding
> close c = first $ close' c
>     where close' c = \k _ -> k (PClosing c)

Similarly, the control-flow operators closes all their branches and
only continue downward:

> pif :: ILPaka -> PureExpr -> ILPaka -> ILPaka -> PakaBuilding -> PakaBuilding
> pif cond test ifTrue ifFalse = first $ pif' cond test ifTrue ifFalse
>     where pif' cond test ifTrue ifFalse cont = \c ->
>               cont $ PIf cond test ifTrue ifFalse c
>
> pwhile :: ILPaka -> PureExpr -> ILPaka -> PakaBuilding -> PakaBuilding
> pwhile cond test loop = first $ pwhile' cond test loop
>     where pwhile' cond test loop cont = \c ->
>               cont $ PWhile cond test loop c
>
> pdoWhile :: ILPaka -> ILPaka -> PureExpr -> PakaBuilding -> PakaBuilding
> pdoWhile loop cond test = first $ pdoWhile' loop cond test
>     where pdoWhile' loop cond test cont = \c ->
>               cont $ PDoWhile loop cond test c
>
> pswitch :: PureExpr -> [(PureExpr,ILPaka)] -> ILPaka -> PakaBuilding -> PakaBuilding
> pswitch test cases defaultCase = first $ pswitch' test cases defaultCase
>     where pswitch' test cases defaultCase cont = \c ->
>               cont $ PSwitch test cases defaultCase c

