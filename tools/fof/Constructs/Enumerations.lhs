%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Constructs.Enumerations where

> import Data.Maybe 

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF
> import IL.FoF.Compile

%endif

\section{Enumeration}

The |Enumeration| construct mirrors the @enum@ data-type of C. It
allows us to name a finite number of natural constants and manipulate
these names instead of numbers.

\subsection{Smart Constructors}

The |newEnum| combinator is used to create a member |value| belonging
to one of the |fields| of |nameEnum|.

> newEnum :: String ->
>            Enumeration ->
>            String ->
>            FoFCode PureExpr
> newEnum nameEnum fields value =
>     inject (NewEnum Nothing nameEnum fields value return)

Similarly, |newEnumN| creates a named member of an enumeration.

> newEnumN :: String ->
>            String ->
>            Enumeration ->
>            String ->
>            FoFCode PureExpr
> newEnumN name nameEnum fields value =
>     inject (NewEnum (Just name) name fields value return)

\subsection{Compile Instantiation}

A |NewEnum| is compiled as follow.

> compileEnumerations (NewEnum name enumName vals value r) binding = 
>     (FStatement (FNewEnum publicName enumName vals value) cont, 
>      binding3)
>         where (publicName, binding2) 
>                   = case name of
>                       Just x -> (Provided x, binding)
>                       Nothing -> (makeVarName Local loc,
>                                   binding1)
>                           where (loc, binding1) = getFreshVar binding 
>               ret = CLRef Global uint64T (Provided value) 
>               (cont, binding3) = r ret binding2

Note that |ret| is actually the name of the enumerated value: it is
treated as a constant and passed as such to the remaining code. A more
standard implementation would have been to create a variable
containing this constant value and pass the reference to the variable
to the subsequent code. However, when |switch|-ing over an enumerated
value, the case would match a variable instead of a constant, which is
refused by the C compiler.

Clearly, a clean solution to this implementation must be
found. However, the current solution, if not perfect, seems to be good
enough.

\subsection{Run Instantiation}

Running a |newEnum| simply consists in getting the associated value.

> runEnumerations (NewEnum _ _ enum name r) heap =
>     let ref = uint64 $ toInteger $ fromJust $ name `lookup` enum in
>     r ref heap 
