%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module IL.Paka.Compile where

> import Text.PrettyPrint.HughesPJ as Pprinter 
> import qualified Data.Map as Map
> import Data.List

> import PureExpressions

> import IL.Paka.Syntax

%endif

\section{Translating @IL.Paka@ to @C@}
\label{sec:il_paka_compile}

This file could as well be called @./IL/C/C.lhs@ but I felt guilty of
introducing yet another confusing IL. So, it is here but feel free to
move it around.

\subsection{Printing types and expressions}

Because we are good kids, we create a type-class called
|Compileable|. A data-type satisfying |Compileable| can be
pretty-printed to something vaguely looking like a bunch of C code.

> class Compileable a where
>     toC :: a -> Doc

Part of the |Compileable| class are FoF's types |TypeExpr| and FoF's
pure expressions |PureExpr|.

There is nothing but boiler plate code to get the job done for pure
expressions:

> instance Compileable PureExpr where
>     toC (Quote s) = doubleQuotes $ text s 
>     toC Void = empty
>     toC (CLInteger _ _ x) = integer x
>     toC (CLFloat x) = Pprinter.float x
>
>     toC (CLRef origin (TPointer _ Avail) loc) = toC loc
>     toC (CLRef origin (TPointer _ Read) loc) = char '*' <> toC loc
>     toC (CLRef origin _ loc) = toC loc
>
>     toC (Unary op x) = parens $ toC op <+> toC x 
>     toC (Binary op x y) = parens $ toC x <+> toC op <+> toC y
>     toC (Sizeof t) = text "sizeof" <> (parens $ toC t)
>     toC (Test t1 t2 t3) = parens $ 
>                           parens (toC t1) <+> char '?' <+> 
>                           parens (toC t2) <+> char ':' <+>
>                           parens (toC t3)
>     toC (Cast t e) = parens $ parens (toC t) <+> toC e
>
> instance Compileable UnaryOp where
>     toC Minus = char '-'
>     toC Complement = char '~'
>     toC Negation = char '!'     
>
> instance Compileable BinaryOp where
>     toC Plus = text "+"
>     toC Sub = text "-"
>     toC Mul = text "*"
>     toC Div = text "/"
>     toC Mod = text "%"
>     toC Shl = text "<<"
>     toC Shr = text ">>"
>     toC AndBit = text "&"
>     toC OrBit = text "|"
>     toC XorBit = text "^"
>     toC Le = text "<"
>     toC Leq = text "<="
>     toC Ge = text ">"
>     toC Geq = text ">="
>     toC Eq = text "=="
>     toC Neq = text "!="

And similarly for types:

> instance Compileable TypeExpr where
>     toC (TInt Signed TInt8) = text "int8_t"
>     toC (TInt Signed TInt16) = text "int16_t"
>     toC (TInt Signed TInt32) = text "int32_t"
>     toC (TInt Signed TInt64) = text "int64_t"
>     toC (TInt Unsigned TInt8) = text "uint8_t"
>     toC (TInt Unsigned TInt16) = text "uint16_t"
>     toC (TInt Unsigned TInt32) = text "uint32_t"
>     toC (TInt Unsigned TInt64) = text "uint64_t"
>     toC TFloat = text "float"
>     toC TVoid = text "void"
>     toC TChar = text "char"                
>     toC (TArray DynamicArray typ) = toC typ <> char '*'
>     toC (TArray (StaticArray size) typ) = toC typ <> char '*'
>     toC (TPointer x _) = toC x <> char '*'
>     toC (TStruct DynamicStruct name fields) = text "struct " <+> text name <+> char '*'
>     toC (TStruct StaticStruct name fields) = text "struct " <+> text name
>     toC (TUnion DynamicUnion name fields) = text "union " <+> text name <+> char '*'
>     toC (TUnion StaticUnion name fields) = text "union " <+> text name 
>     toC (TCompPointer name) = text "uintptr_t"
>     toC (TTypedef typ name) = text name
>     toC (TEnum name _) = text "enum" <+> text name                    

The picky reader will have noticed the absence of printer for function
types. This is hardly a problem at the moment because we do not
support function pointers, so we are not going to declare function
types anytime soon. Note that this argument might well be circular: if
we do not support function pointers, it is because it is a pain to
write their type, among other things (if I remember correctly). Oh
well.

Printing variable names is dead easy:

> instance Compileable VarName where
>     toC x = text $ mkPakaVarName x

\subsection{Names, everywhere}

I am not very proud of that section, and of the way I abused these
functions in @IL/Paka/Paka.lhs@. I beg your pardon for that. There
\emph{must} some abstraction to bust here but I was not able to catch
it.

Provided a @FoF@ |VarName|, we turn it into a string with a bit of
Hungarianism, but very little. Why this function is called
|mkPakaVarName| when it does not deal with |PakaVarName|? I have no
clue. 

> mkPakaVarName :: VarName -> String
> mkPakaVarName (Generated x) = "_" ++ x
> mkPakaVarName (Provided x) = x
> mkPakaVarName (Inherited y x) = mkPakaVarName x ++ "__" ++ show y

Then, we have to functions turning a |PureExpr| into a
|PakaVarName|. |PakaValName| provides you with the value described by
the |PureExpr|. On the other hand, |PakaVarName| works one level below
and gives you the value contained in the |PureExpr|. 

I have to admit that I am not myself convinced by this
explanation. Basically, I would have to look at the former code, the
|typeOf|, |deref|, |readOf| functions, and the new code. Then, I might
be able to make more sense of that. However, intrinsically, references
are a non-sense.

> pakaValName :: PureExpr -> PakaVarName
> pakaValName (CLRef origin (TPointer _ Avail) loc) = Var $! mkPakaVarName loc
> pakaValName (CLRef origin (TPointer _ Read) loc) = Ptr $! Var $ mkPakaVarName loc
> pakaValName (CLRef _ _ loc) = Var $! mkPakaVarName loc
> pakaValName x = K x
>
> pakaVarName :: PureExpr -> PakaVarName
> pakaVarName (CLRef origin (TPointer _ Avail) loc) = Deref (Var $ mkPakaVarName loc)
> pakaVarName (CLRef origin (TPointer _ Read) loc) = Var $ mkPakaVarName loc
> pakaVarName (CLRef _ _ loc) = Var $ mkPakaVarName loc
> pakaVarName x = K x

Finally, we need to be able to print these |PakaVarName| into meaning
C code. Here you go.

> instance Compileable PakaVarName where
>     toC (Deref x) = char '&' <> toC x
>     toC (Var x) = text x
>     toC (Ptr x) = char '*' <> toC x
>     toC (Complex _) = error "Cannot convert a Complex var name to C"
>     toC (K x) = toC x


\subsection{Generating C}

The following is a small addendum to the pretty-printer library. We
don't know why it is not defined there. 

> vcat' :: [Doc] -> Doc
> vcat' [] = empty 
> vcat' (x:xs) = l `seq` r `seq` r
>     where l = vcat' xs
>           r = x $+$ l

For once, I will do a bottom-up presentation. So, I will describe the
implementation of pretty-printers from |Paka| code to C.

The first step consists in printing closing terms:

> pprintClosing :: PakaClosing -> Doc
> pprintClosing (PReturn e) = text "return" <+> parens (toC e) <> semi
> pprintClosing PBreak = text "break"
> pprintClosing PContinue = text "continue"

Then, we print statements. As you remember, we need to build the final
code by applying the variables to the term:

> pprintStmt :: PakaStatement -> Doc
> pprintStmt (PAssign dst x srcs) = x (toC dst : map toC srcs)
> pprintStmt (PInstruction x srcs) = x (map toC srcs)

The next step consists in compiling intra-procedural code. This is
rather simple and quite directly follows from the |Paka| definitions:

> pprintPaka :: ILPaka -> Doc
> pprintPaka PVoid = empty
> pprintPaka (PClosing c) = pprintClosing c
> pprintPaka (PStatement stmt k) = 
>     pprintStmt stmt $+$ 
>     pprintPaka k
> pprintPaka (PIf cond test ifTrue ifFalse k) =
>     pprintPaka cond $+$ 
>     text "if" <+> parens (toC test) <> lbrace $+$
>         (nest 4 $! pprintPaka ifTrue) $+$
>     rbrace <+> text "else" <+> lbrace $+$
>         (nest 4 $! pprintPaka ifFalse) $+$
>     rbrace $+$ 
>     pprintPaka k
> pprintPaka (PWhile cond test loop k) =
>     pprintPaka cond $+$
>     text "while" <> parens (toC test) <> lbrace $+$
>         (nest 4 $! pprintPaka loop) $+$
>     rbrace $+$
>     pprintPaka k
> pprintPaka (PDoWhile loop cond test k) =
>     text "do" <+> lbrace $+$
>          (nest 4 $! pprintPaka loop) $+$
>     rbrace <+> text "while" <+> parens (toC test) <> semi $+$
>     pprintPaka k
> pprintPaka (PSwitch test cases defaultCase k) =
>     text "switch" <+> parens (toC test) <+> lbrace $+$
>          (nest 4 $ vcat' $ map compileCase cases) $+$
>          (nest 4 (text "default:" <+> lbrace $+$
>                        (nest 4 $! pprintPaka defaultCase) $+$
>                        rbrace)) $+$
>     rbrace $+$
>     pprintPaka k
>         where compileCase (i, code) =
>                   text "case" <+> toC i <> colon <+> lbrace $+$
>                        (nest 4 $! (pprintPaka code $+$
>                                   text "break" <> semi)) $+$
>                   rbrace

Finally, we can pretty-print a complete |PakaCode| by iterating other
each section, and, in each section, pretty-printing each element.

> pprint :: PakaCode -> Doc
> pprint code = 
>     text "/* Includes: */" $+$
>     space $+$
>     text "#include <stdint.h>" $+$
>     vcat' (extractM $ includes code) $+$
>     space $+$
>     (case Map.null $ types code of 
>        True -> empty
>        _ -> text "/* Type Declarations: */" $+$ 
>             space $+$
>             vcat' (extractM $ types code) $+$
>             vcat' (extractL $ declarations code) $+$
>             space) $+$
>     (case null $ globalVars code of
>        True -> empty
>        _ ->  text "/* Global Variables: */" $+$ 
>              space $+$
>              vcat' (map (\y -> text "static" <+> y) $
>                     extractL $ 
>                     globalVars code) $+$
>              space) $+$
>     (case Map.null $ prototypes code of
>        True -> empty
>        _ ->  text "/* Prototypes: */" $+$ 
>              space $+$
>              vcat' (extractM $ prototypes code) $+$
>              space) $+$
>     (case Map.null $ functions code of
>        True -> empty
>        _ ->  text "/* Function Definitions: */" $+$ 
>              space $+$
>              vcat' (map (\(returnT, attrs, name, args, lEnv, body) -> 
>                           returnT <+> attrs <+> text name <> parens args <+> lbrace $+$
>                           (nest 4 $ vcat' $ extractM $ localVars lEnv) $+$
>                           space $+$
>                           (nest 4 $ pprintPaka body) $+$
>                           rbrace $+$
>                           space)
>                    $ extractM
>                    $ functions code) $+$
>              space)
>     $+$ space 

We note the use of two extraction functions: these functions remove
the keys from the associative structure in use, and simply return the
content. When an order was maintained, ie. an associative list was
used, the definition order is carefully restored by reversing the
list.

> extractL :: Eq a => [(a, b)] -> [b]
> extractL = (map snd) .
>           reverse
>
> extractM :: Map.Map a b -> [b]
> extractM = Map.elems


Because we have worked very hard, we are rewarded by the right to
instantiate these |PakaCode| in the |Show|.

> instance Show PakaCode where
>     show = render . pprint
