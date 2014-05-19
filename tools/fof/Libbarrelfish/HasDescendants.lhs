%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Libbarrelfish.HasDescendants where

> import Semantics
> import Constructs
> import PureExpressions
> import {-# SOURCE #-} Expressions

> import IL.FoF.FoF
> import IL.FoF.Compile
> import IL.FoF.Run

%endif

\section{Has Descendants}

The construct |HasDescendants| embeds the libarrelfish function
@has_descendants@ into FoF.

\subsection{Smart Constructors}

This function is provided in two flavors: an anonymous one, which
stores its result in an anonymous variable, and a named one, which
allows you to name the resulting variable.

> has_descendants :: PureExpr -> FoFCode PureExpr
> has_descendants cte = inject (HasDescendants Nothing cte return)
>
> has_descendantsN :: String -> PureExpr -> FoFCode PureExpr
> has_descendantsN name cte = inject (HasDescendants (Just name) cte return)

\subsection{Compile Instanciation}

This function is translated into a foreign function definition, as
usual:

> compileHasDescendants (HasDescendants mName arg r) binding =
>     let (loc, binding1) = getFreshVar binding in
>     let name = case mName of
>                  Nothing -> makeVarName Local loc
>                  Just x -> Provided x in
>     let ref = CLRef Local uint64T name in
>     let (cont, binding2) = r ref binding1 in
>     (FStatement (FFFICall "has_descendants" [ref, arg]) cont, 
>      binding2)


\subsection{Run Instantiation}

As for libc functions, we have not yet implemented the semantics of
that operation. A trace-based semantics would make sense, too.

> runHasDescendants (HasDescendants _ a r) heap = error "HasDescendants: eval not implemented"
