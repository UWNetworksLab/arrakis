%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module IL.Paka.Paka where

> import Debug.Trace

> import Text.PrettyPrint.HughesPJ as Pprinter 

> import Data.List
> import Data.Maybe
> import qualified Data.Map as Map

> import PureExpressions
> import Eval
> import Constructs

> import IL.Paka.Syntax
> import IL.Paka.Builders
> import IL.Paka.Compile

> import IL.FoF.FoF
> import IL.FoF.Compile

%endif


\section{Translating @IL.FoF@ to @IL.Paka@}
\label{sec:il_paka_paka}

To translate @IL.FoF@ code, we simply iterate over it and build the
corresponding @IL.Paka@ term. 

> compileFoFtoPaka :: ILFoF -> PakaCode
> compileFoFtoPaka code = ccode
>     where (_, ccode, _) = compileFoFtoPaka' code (id , emptyCode, emptyIntra)

The translation is often trivial, because both languages are very
similar in structure. The major novelty is that intra-procedural and
extra-procedural code are translated into different data-structures:
building an |ILPaka| term for the former, defining a |PakaCode| record
for the latter. At the same time, we carry a |PakaIntra| environment
during intra-procedural compilations. All these details are abstracted
away by the builders we have defined in the previous section and that
we abuse in this section.

At this stage, the compiler simply dispatches to construct-specific
compilers. Hence the following code:

> compileFoFtoPaka' :: ILFoF -> PakaBuilding -> PakaBuilding
> compileFoFtoPaka' (FStatement stmt k) = compileFoFtoPakaStmt stmt k 
> compileFoFtoPaka' t@(FIf _ _ _ _) = compileFoFtoPakaIf t
> compileFoFtoPaka' (FClosing c) = compileFoFtoPakaClosing c
> compileFoFtoPaka' t@(FNewDef _ _ _ _ _ _) = compileFoFtoPakaFunDef t
> compileFoFtoPaka' t@(FWhile _ _ _) = compileFoFtoPakaWhile t
> compileFoFtoPaka' t@(FDoWhile _ _ _) = compileFoFtoPakaDoWhile t
> compileFoFtoPaka' t@(FFor _ _ _ _ _) = compileFoFtoPakaFor t
> compileFoFtoPaka' t@(FSwitch _ _ _ _) = compileFoFtoPakaSwitch t
> compileFoFtoPaka' (FConstant e) = compileFoFtoPakaCst e

\subsection{Compiling Function definition}

The compilation of a function definition consists in building a
prototype, compiling the body of the function, building it, and
pursuing with the next definition.

> compileFoFtoPakaFunDef :: ILFoF -> PakaBuilding -> PakaBuilding
> compileFoFtoPakaFunDef (FNewDef funAttrs
>                                  funName
>                                  body 
>                                  returnT
>                                  args
>                                  k) (cont, gEnv, lEnv) =
>     prototype funName (attr <+> returnType <+> text funName <> parens functionArgs <> semi)
>     # function attr returnType funName functionArgs lEnv1 cbody
>     # compileFoFtoPaka' k
>     $ (cont, gEnv1, lEnv)
>         where returnType = toC returnT
>               attr = hsep (map (text . show) funAttrs)
>               functionArgs = buildFunctionArgs args
>               buildFunctionArgs params = hcat $ intersperse comma $ 
>                                          map buildFunctionArg params
>               buildFunctionArg x = toC (liftType $ typeOf x) <+> toC x
>               (cbody_, gEnv1, lEnv1) = compileFoFtoPaka' body (id, gEnv, emptyIntra)
>               cbody = cbody_ PVoid


\subsection{Compiling Constant}

This one is directly handled by the so-called builder:

> compileFoFtoPakaCst :: PureExpr -> PakaBuilding -> PakaBuilding
> compileFoFtoPakaCst = constant

\subsection{Compiling Closing statements}

As for closing statements, this is not much more difficult:

> compileFoFtoPakaClosing :: FClosing -> PakaBuilding -> PakaBuilding
> compileFoFtoPakaClosing (FReturn expr) = close $ PReturn expr
> compileFoFtoPakaClosing (FBreak) = close PBreak
> compileFoFtoPakaClosing (FContinue) = close PContinue


\subsection{Compiling control-flow operators}

The mechanics of control-flow operators does not vary much between
operators, so they are all here, together. 

Some points worth mentioning. First, sub-branches are compiled down
with |compileFoFtoPaka'|, as one would expect. Second, to get a
|ILPaka| value out of an |ILPaka -> ILPaka| continuation |k|, we call
|k pVoid|: @void@ is the ultimate closing statement, after all. Third,
an expression computing a tested value \emph{must} return a pure
expression, which we can grab |fromJust $ expr intraEnv|. This is an
invariant, if not respected |fromJust| will blow up. 

Finally, it's all fine and good to compile sub-branches privately
(inside |where| statements) but \emph{don't forget} to bring the
resulting global and local environments in the public setting. This
corresponds to the use of |second (const globalEnv)| and |third (const
localEnv)| in the public flow. Also, don't forget to thread these
environments in your private compilations, too. Someone should think
of a less error-prone solution.


> compileFoFtoPakaIf :: ILFoF -> PakaBuilding -> PakaBuilding
> compileFoFtoPakaIf (FIf cond 
>                          ifTrue 
>                          ifFalse 
>                          k) (cont, gEnv, lEnv) =
>     pif ccond test cifTrue cifFalse 
>     # second (const gEnv3)
>     # third (const lEnv3)
>     # compileFoFtoPaka' k 
>     $ (cont, gEnv3, lEnv3)
>         where (ccond_, gEnv1, lEnv1) = compileFoFtoPaka' cond (id, gEnv, lEnv) 
>               ccond = ccond_ PVoid
>               test = fromJust $ expr lEnv1
>               (cifTrue_, gEnv2, lEnv2) = compileFoFtoPaka' ifTrue (id, gEnv1, lEnv1)
>               cifTrue = cifTrue_ PVoid
>               (cifFalse_, gEnv3, lEnv3) = compileFoFtoPaka' ifFalse (id, gEnv2, lEnv2)
>               cifFalse = cifFalse_ PVoid
>
> compileFoFtoPakaWhile (FWhile cond
>                                loop 
>                                k) (cont, gEnv, lEnv) =
>     pwhile ccond test cloop
>     # second (const gEnv2)
>     # third (const lEnv2)
>     # compileFoFtoPaka' k 
>     $ (cont, gEnv2, lEnv2)
>         where (ccond_, gEnv1, lEnv1) = compileFoFtoPaka' cond (id, gEnv, lEnv) 
>               ccond = ccond_ PVoid
>               test = fromJust $ expr lEnv1
>               (cloop_, gEnv2, lEnv2) = compileFoFtoPaka' loop 
>                                        # compileFoFtoPaka' cond
>                                        $ (id, gEnv1, lEnv1)
>               cloop = cloop_ PVoid
>
> compileFoFtoPakaDoWhile (FDoWhile loop 
>                                    cond 
>                                    k) (cont, gEnv, lEnv) =
>     pdoWhile cloop ccond test 
>     # second (const gEnv2)
>     # third (const lEnv2)
>     # compileFoFtoPaka' k 
>     $ (cont, gEnv2, lEnv2)
>         where (ccond_, gEnv1, lEnv1) = compileFoFtoPaka' cond (id, gEnv, lEnv) 
>               ccond = ccond_ PVoid
>               test = fromJust $ expr lEnv1
>               (cloop_, gEnv2, lEnv2) = compileFoFtoPaka' loop 
>                                        # compileFoFtoPaka' cond 
>                                        $ (id, gEnv1, lEnv1)
>               cloop = cloop_ PVoid
>
> compileFoFtoPakaSwitch (FSwitch test 
>                                  cases 
>                                  defaultCase
>                                  k) (cont, gEnv, lEnv) =
>     pswitch test ccases cdefaultCase
>     # second (const gEnv2)
>     # third (const lEnv2)
>     # compileFoFtoPaka' k
>     $ (cont, gEnv, lEnv)
>         where (cdefaultCase_, gEnv1, lEnv1) = compileFoFtoPaka' defaultCase (id, gEnv, lEnv)
>               cdefaultCase = cdefaultCase_ PVoid
>               (codes, fcases) = unzip cases
>               (ccases_, gEnv2, lEnv2) = compileCases fcases gEnv1 lEnv1
>               ccases = zip codes ccases_
>               compileCases [] x y = ([], x, y)
>               compileCases (fcase : fcases) gEnv lEnv = 
>                   --   cfcase `deepSeq` codes `deepSeq`
>                   (cfcase : codes, gEnv2, lEnv2)
>                     where (fcase_, gEnv1, lEnv1) = compileFoFtoPaka' fcase (id, gEnv, lEnv) 
>                           cfcase = fcase_ PVoid
>                           (codes, gEnv2, lEnv2) = compileCases fcases gEnv1 lEnv1
>                 

For my personal convenience, @for@ loops are compiled into @while@
loops. If you're not happy with that, go ahead and implement
that. However, I have to warn you that dealing with computations
inside the indices is not a joy.

> compileFoFtoPakaFor (FFor init 
>                            test 
>                            inc 
>                            loop 
>                            k) (cont, gEnv, lEnv) =
>     pwhile ccond etest cloop
>     # second (const gEnv2)
>     # third (const lEnv2)
>     # compileFoFtoPaka' k 
>     $ (cont, gEnv2, lEnv2)
>         where (ccond_, gEnv1, lEnv1) = compileFoFtoPaka' init 
>                                        # compileFoFtoPaka' test
>                                        $ (id, gEnv, lEnv)
>               ccond = ccond_ PVoid
>               etest = fromJust $ expr lEnv1
>               (cloop_, gEnv2, lEnv2) = compileFoFtoPaka' loop 
>                                        # compileFoFtoPaka' inc
>                                        # compileFoFtoPaka' test
>                                        $ (id, gEnv1, lEnv1)
>               cloop = cloop_ PVoid


\subsection{Compiling statements}

The real stuff happens below: compiling these damned statements. And
there is a lot of them. That was for the bad news. The good news is
that, individually, these functions are quite easy to understand.

The careful reader will notice that |Term|s are not using all their
arguments. Honestly, I just wanted the basic Optimizer to be done, so
I dropped everything not necessary. So, you have the architecture, now
fill the holes if you want to do something clever. Therefore, when you
see a term defined with |(\[xs,_,xss] -> ...)|, this means that the
ignored variable is hard-coded in the term, and cannot be actually
replaced. This is ok with my simple optimizer, that would probably
need to be changed if you are to do something more clever.
  
\subsubsection{Compiling References}

As a starting, non frightening example, here is the code to compile
references. Honestly, it is self-explanatory, isn't it?

> compileFoFtoPakaStmt (FNewRef varName dat) k =
>     localVar mvarName (toC (typeOf dat) <+> toC varName <> semi)
>     # assgn pvarName (\[_,e] -> toC varName <+> char '=' <+> e <> semi)
>                      [ pakaVarName dat ]
>     # compileFoFtoPaka' k
>         where mvarName = mkPakaVarName varName
>               pvarName = Var $ mkPakaVarName varName
>
> compileFoFtoPakaStmt (FReadRef varName ref) k =
>     localVar mvarName (toC (unfoldPtrType ref) <+> toC varName <> semi)
>     # assgn pvarName (\[_,e] -> 
>                       toC varName <+> char '=' <+> e <> semi) 
>                      [ pakaValName ref ]
>     # compileFoFtoPaka' k
>         where mvarName = mkPakaVarName varName
>               pvarName = Var $ mkPakaVarName varName
>
> compileFoFtoPakaStmt (FWriteRef ref dat) k =
>     assgn (pakaValName ref)
>           (\[_,e] -> toC ref <+> char '=' <+> e <> semi)
>           [ pakaVarName dat ]
>     # compileFoFtoPaka' k

\subsubsection{Compiling Arrays}

Similarly, compiling arrays work the same way. There is minor nitpick
in the current implementation: it doesn't support dynamic array (that
is, malloc'ed arrays).

Actually, I suspect that if you are reading this file, it is because
your code is using a dynamic array and the compiler blew up when you
use it. Well, the code needs to be written. It is remotely similar to
static arrays, with the additional need to malloc memory and
initialize the data. If you are looking for a word to describe your
situation, I think that ``screwed'' is appropriate. Hint: a dynamic
array should be defined by a single initial element and an integer
variable specifying (at run time) the length of the array.

> compileFoFtoPakaStmt (FNewArray varName 
>                                  alloc@(StaticArray size) 
>                                  dat) k =
>     globalVar mvarName (toC typeOfDat <+> toC varName <> brackets Pprinter.empty
>                         <+> char '=' <+> braces (
>                             nest 4 $
>                                  fsep (punctuate comma
>                                        [ text (deref val) <> toC val
>                                        | val <- dat ])) <> 
>                             semi)
>     # compileFoFtoPaka' k
>         where mvarName = mkPakaVarName varName
>               typeOfDat = typeOf $ head dat        
>
> compileFoFtoPakaStmt (FReadArray varName
>                                   (CLRef origin 
>                                          (TArray (StaticArray size) typ) 
>                                          xloc) 
>                                   index) k =
>     localVar mvarName (toC typ <+> toC varName <> semi)
>     # (case symbEval index of
>            CLInteger _ _ x -> 
>                if x < (toInteger size) then
>                   assgn pvarName (\[_,_] -> 
>                                   toC varName <+> char '=' 
>                                   <+> toC xloc <> brackets (toC index) <> semi)
>                             [ Complex $ Var $ mkPakaVarName xloc ]
>                else
>                   instr (\_ ->
>                          text "assert" <> parens (text "! \"ReadArray: Out of bound\"") <> semi) 
>                         []
>            _ ->
>                assgn pvarName (\[_,_,e] ->
>                                text "if" <+> parens ( e
>                                                       <+> char '<' 
>                                                       <+> int size) <> lbrace
>                                $+$
>                                nest 4 (toC varName <+> char '=' 
>                                        <+> toC xloc <> brackets e <> semi) 
>                                $+$
>                                rbrace <+> text "else" <+> lbrace $+$
>                                       nest 4 (text "assert" <> parens (text "! \"ReadArray: Out of bound\"") <> semi
>                                               $+$ toC varName <+> char '=' <+> text "NULL" <> semi) 
>                                $+$
>                                rbrace) 
>                          [ Complex $ Var $ mkPakaVarName xloc, 
>                            pakaValName index ])
>     # compileFoFtoPaka' k
>         where mvarName = mkPakaVarName varName
>               pvarName = Var $ mkPakaVarName varName
>
> compileFoFtoPakaStmt (FWriteArray ref@(CLRef origin 
>                                        (TArray (StaticArray size) typ) 
>                                        xloc) 
>                                    index 
>                                    dat) k =
>     assgn pxloc (\[_,e,f] -> 
>                  text "if" <+> parens ( f <+> char '<' <+> int size) <> lbrace 
>                  $+$ nest 4 (toC xloc <> brackets f 
>                              <+> char '=' <+> e <> semi)
>                  $+$ rbrace <+> text "else" <+> lbrace 
>                  $+$ nest 4 (text "assert" <> parens (text "! \"Out of bound \"") <> semi)
>                  $+$ rbrace) [ pakaValName dat, pakaValName index ]
>     # compileFoFtoPaka' k
>         where pxloc = Var $ mkPakaVarName xloc
               

\subsubsection{Compiling Strings}

Building a new string is as simple as building a new static array:

> compileFoFtoPakaStmt (FNewString varName dat) k =
>     globalVar mvarName (toC TChar <+> toC varName <> text "[]"
>                        <+> char '='
>                        <+> doubleQuotes (text dat) <> semi)
>     # compileFoFtoPaka' k
>         where mvarName = mkPakaVarName varName


\subsubsection{Compiling Function call}

As for function call, there is no black magic either:

> compileFoFtoPakaStmt (FCallDef mVarName
>                                 (CLRef _ (TFun nameF 
>                                                func 
>                                                returnT 
>                                                argsT) _)
>                                 args) k =
>     case mVarName of
>       Nothing ->
>           instr (\_ ->
>                  text nameF 
>                  <> parens (hcat $ intersperse comma $ map toC args) <> semi)
>                 (map (Complex . pakaVarName) args)
>       Just varName ->
>           localVar (mkPakaVarName varName)
>                    (toC returnT <+> toC varName <> semi)
>           # assgn (Var $ mkPakaVarName varName)
>                   (\_ -> toC varName <+> char '=' 
>                    <+> text nameF 
>                    <> parens (hcat $ intersperse comma $ map toC args) <> semi)
>                   (map (Complex . pakaValName) args)
>     # compileFoFtoPaka' k


\subsubsection{Compiling Enumerations}

We can safely compile enumerations:

> compileFoFtoPakaStmt (FNewEnum varName 
>                                 nameEnum 
>                                 fields 
>                                 initVal) k = 
>     declareEnum nameEnum fields 
>     # compileFoFtoPaka' k
>       where mvarName = mkPakaVarName varName
>             pvarName = Var $ mkPakaVarName varName

\subsubsection{Compiling Union}

It is not a big deal to compile union operations either:

> compileFoFtoPakaStmt (FNewUnion name 
>                                  DynamicUnion
>                                  nameUnion 
>                                  fields 
>                                  (initField, initData)) k =
>     declareRecursive (TUnion DynamicUnion nameUnion fields)
>     # localVar (mkPakaVarName name) (text "union" <+> text nameUnion <> char '*' <+> toC name <> semi)
>     # assgn varName (\[_] ->
>                       toC name <+> char '=' <+> 
>                              parens (text  "union" <+> text nameUnion <> char '*')
>                                         <+> text "malloc" <> parens ( 
>                                             text "sizeof" <> parens (
>                                             text "union" <+> text nameUnion))
>                                         <> semi) []
>     # assgn varName (\[_,b] -> 
>                       toC name <> text "->" <> text initField 
>                              <+> char '=' <+> b <> semi) 
>             [pakaVarName initData]
>     # compileFoFtoPaka' k
>          where varName = Var $ mkPakaVarName name
>
> compileFoFtoPakaStmt (FNewUnion name StaticUnion nameUnion fields (initField, initData)) k =
>     declareRecursive (TUnion StaticUnion nameUnion fields)
>     # localVar (mkPakaVarName name) (text "union" <+> text nameUnion <+> toC name <> semi)
>     # assgn varName (\[_,e] ->
>                       toC name <> char '.' <> text initField 
>                                <+> char '=' <+> e <> semi) 
>             [pakaVarName initData]
>     # compileFoFtoPaka' k
>       where varName = Var $ mkPakaVarName name
>
> compileFoFtoPakaStmt (FReadUnion varName 
>                                   (CLRef _ typeU@(TUnion alloc
>                                                          nameU 
>                                                          fields) 
>                                            xloc) 
>                                   field) k =
>     declareRecursive typeU
>     # localVar mpVarName (toC typeField <+> toC varName <> semi)
>     # assgn pVarName (\[_,_] ->
>                        toC varName 
>                       <+> char '=' 
>                       <+> toC xloc <> ptrSigUnion alloc <> text field <> semi)
>             [ Complex $ Var $ mkPakaVarName xloc ]
>     # compileFoFtoPaka' k
>         where typeField = fromJust $ field `lookup` fields 
>               mpVarName = mkPakaVarName varName
>               pVarName = Var $ mkPakaVarName varName
>
> compileFoFtoPakaStmt (FWriteUnion (CLRef origin 
>                                           typeU@(TUnion alloc 
>                                                       nameU 
>                                                       fields) 
>                                           xloc) 
>                                    field 
>                                    value) k =
>     declareRecursive typeU
>     # assgn pxloc (\[_,e] ->
>                    toC xloc <> ptrSigUnion alloc <> text field
>                    <+> char '=' <+> e <> semi) 
>             [pakaVarName value]
>     # compileFoFtoPaka' k
>       where pxloc = Var $ mkPakaVarName xloc

\subsubsection{Compiling Structs}

Quite the same goes for structure operations:

> compileFoFtoPakaStmt (FNewStruct varName 
>                                   DynamicStruct 
>                                   nameStruct 
>                                   fields) k =
>     declareRecursive (TStruct DynamicStruct nameStruct fieldsTypeStr)
>     # localVar mVarName (text "struct" <+> text nameStruct <+> toC varName <> semi)
>     # (assgn pVarName (\[_] ->
>                       toC varName <+> char '=' 
>                       <+> parens (text "struct" <+> text nameStruct <+> char '*' )
>                       <+> text "malloc"
>                       <> parens ( text "sizeof" 
>                       <> parens ( text "struct" <+> text nameStruct)) 
>                       <> semi) [])
>      # foldl' (#) id [ assgn pVarName (\[_,e] ->
>                                       toC varName <> text "->" <> text field 
>                                       <+> char '='
>                                       <+> e <> semi) [ pakaVarName val ]
>                       | (field, (typ, val)) <- fields ]
>         where mVarName = mkPakaVarName varName
>               pVarName = Var $ mkPakaVarName varName
>               fieldsTypeStr = [ (field, typ)
>                               | (field,(typ,_)) <- fields] 
>
> compileFoFtoPakaStmt (FNewStruct varName 
>                                   StaticStruct 
>                                   nameStruct 
>                                   fields) k =
>     declareRecursive (TStruct StaticStruct nameStruct fieldsTypeStr)
>     # localVar mvarName (text "struct" <+> text nameStruct <+> toC varName 
>                               <+> char '='
>                               <+> braces ( nest 4 $
>                                       hcat (punctuate comma 
>                                             [ text (deref val) <> toC val 
>                                             | (_,(_,val)) <- fields ])) 
>                               <> semi) 
>     # compileFoFtoPaka' k
>         where mvarName = mkPakaVarName varName
>               fieldsTypeStr = [ (field, typ)
>                               | (field,(typ,_)) <- fields] 
> 
> compileFoFtoPakaStmt (FReadStruct varName 
>                                    ref@(CLRef origin 
>                                               typeS@(TStruct alloc 
>                                                            nameStruct 
>                                                            fields) 
>                                               xloc) 
>                                    field) k =
>     declareRecursive typeS
>     # localVar mvarName (toC typeField <+> toC varName <> semi)
>     # assgn pvarName (\[_,_] -> 
>                       toC varName <+> char '='
>                       <+> toC xloc <> ptrSigStruct alloc <> text field <> semi)
>                      [Complex $ Var $ mkPakaVarName xloc]
>     # compileFoFtoPaka' k
>         where typeField = fromJust $ field `lookup` fields
>               mvarName = mkPakaVarName varName
>               pvarName = Var $ mkPakaVarName varName
>
> compileFoFtoPakaStmt (FWriteStruct ref@(CLRef origin 
>                                                typeS@(TStruct alloc
>                                                             nameStruct 
>                                                             fields) 
>                                                xloc) 
>                                     field 
>                                     value) k =
>     declareRecursive typeS
>     # assgn pxloc (\[_,e] ->
>                     toC xloc <> ptrSigStruct alloc <> text field
>                             <+> char '=' <+> e <> semi) 
>             [ pakaVarName value ]
>     # compileFoFtoPaka' k
>       where pxloc = Var $ mkPakaVarName xloc


\subsubsection{Compiling Typedef}

And we can even get @typedef@s:

> compileFoFtoPakaStmt (FTypedef typ aliasName) k = 
>     declareRecursive typ
>     # declare aliasName Pprinter.empty
>                       (text "typedef" <+> toC typ <+> text aliasName <> semi)
>     # compileFoFtoPaka' k
>
> compileFoFtoPakaStmt (FTypedefE inclDirective 
>                                (TTypedef typ aliasName)) k =
>     include inclDirective 
>     # compileFoFtoPaka' k

\subsubsection{Compiling Foreign calls}

It is always the same story for foreign function calls. If
you have extended Filet-o-Fish with a new foreign-function, don't look
further: you should put your foreign call here!

So, as often, we have an inoffensive dispatcher. Don't touch it.

> compileFoFtoPakaStmt (FFFICall nameCall args) k = 
>     compileFFI nameCall args 
>     # compileFoFtoPaka' k

And the dispatched function, in which you should add your foreign code
generator. This is just like writing C code, so don't be shy.

> compileFFI nameCall params | nameCall == "printf" =
>   include "<stdio.h>" 
>   # instr (\_ -> text "printf" <> parens (hcat (punctuate comma (map toC params))) <> semi)
>           (map (Complex . pakaVarName) params)
>
> compileFFI nameCall [e] | nameCall == "assert" =
>   include "<assert.h>" 
>   # instr (\[e] -> text "assert" <> parens e <> semi) [pakaValName e]
>
> compileFFI nameCall [varName, param] | nameCall == "has_descendants" =
>   include "<mdb/mdb.h>"
>   # include "<capabilities.h>" 
>   # include "<stdbool.h>"
>   # localVar (show $ toC varName)
>              (text "bool" <+> toC varName <> semi)
>   # assgn (pakaValName $ varName)
>           (\[_,e] -> 
>            toC varName <+> char '=' 
>            <+> text "has_descendants"
>            <> parens e <> semi)
>           [pakaValName $ param]
>
>  -- XXX: |mem_to_phys| was renamed to |mem_to_local_phys|.
>  -- This is a temporary hack till we get around to producing
>  -- a whole list of translation functions here. -Akhi
>  -- XXX: moved include to hamlet file compilation so that user version of
>  -- |cap_predicates| can be built -Ross
>
> compileFFI nameCall [varName, param] | nameCall == "mem_to_phys" =
>   localVar (show $ toC varName)
>              (toC uint64T <+> toC varName <> semi)
>   # assgn (pakaValName $ varName)
>           (\[_,e] ->
>             toC varName <+> char '=' <+>
>             text "mem_to_local_phys" <> parens (toC param) <> semi)
>           [pakaValName $ param]



\subsubsection{Declaring types}

Above, we have dealt with the compilation of operations on complex
structures, such as @enum@s, @struct@s, and @union@s. When compiling a
code operating on such structure, we need to make sure that the
corresponding type is defined.

Hence, we provide an advanced builder to declare a @struct@ or an
@union@:

> declareStructUnion kind name fields =
>     declare name (text kind <+> text name <> semi)
>                  (text kind <+> text name <+> braces (
>                             nest 4 (vcat' [toC typ <+> text field <> semi  
>                                            -- special case for static array?
>                                            | (field, typ) <- fields ])) <> semi)

And similarly for declaring an @enum@, however without the forward declaration:

> declareEnum nameEnum fields =
>     declare nameEnum empty
>                      (text "enum" <+> text nameEnum <+> lbrace 
>                       $+$ nest 4 (vcat' $ punctuate comma 
>                                   ([ text name <+> char '=' <+> int val
>                                    | (name, val) <- fields ]))
>                       $+$ rbrace <> semi)

However, that does not solve the problem: a structure or an union
might be defined in term of other structures or unions. Hence, we need
to declare the dependencies before defining the concerned object. This
is handled by |declareRecursive|:

> declareRecursive = declareRecursive'
>     where declareRecursive' (TStruct _ name fields) (code, gEnv, lEnv) =
>               case name `Map.lookup` types gEnv of
>                 Just _ -> (code, gEnv, lEnv)
>                 Nothing -> 
>                     foldl' (#) id [ declareRecursive' typ | (_, typ) <- fields ]
>                     # declareStructUnion "struct" name fields
>                     $ (code, gEnv, lEnv)
>           declareRecursive' (TUnion _ name fields) (code, gEnv, lEnv) =
>               case name `Map.lookup` types gEnv of
>                 Just _ -> (code, gEnv, lEnv)
>                 Nothing -> 
>                     foldl' (#) id [ declareRecursive' typ | (_, typ) <- fields ]
>                     # declareStructUnion "union" name fields
>                     $ (code, gEnv, lEnv)
>           declareRecursive' (TEnum name fields) t =
>               declareEnum name fields $ t
>           declareRecursive' _ t = id t


These two functions have also been handy above, even though they are
not fundamentally clever. Depending on the allocation policy of the
data-structure, they choose to dereference and access it, or directly
access it.

> ptrSigUnion :: AllocUnion -> Doc
> ptrSigUnion DynamicUnion = text "->"
> ptrSigUnion StaticUnion = char '.'
>
> ptrSigStruct :: AllocStruct -> Doc
> ptrSigStruct DynamicStruct = text "->"
> ptrSigStruct StaticStruct = char '.'
