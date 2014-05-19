%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> {-# OPTIONS_GHC -fglasgow-exts #-}

%endif

%if false

> module Main where

> import Debug.Trace

> import Semantics
> import Expressions

> import Arrays
> import Conditionals
> import Enumerations
> import Functions
> import References
> import Strings
> import Structures
> import Typedef
> import Unions

> import Libc.Assert

> import Run
> import Compile

%endif



\section{Example}

%if false

> {-         

> test3 :: Semantics (Ref :+: Conditionals) PureExpr
> test3 = 
>     do 
>       ifc (do return (int32 1 .==. int32 0) :: Semantics Conditionals PureExpr)
>           (do 
>            x <- newRef $ int32 1
>            xv <- readRef x
>            writeRef x (xv .*. int32 2) :: Semantics (Ref :+: Conditionals) PureExpr)
>          (do return Void :: Semantics Conditionals PureExpr)

     

> test5 :: [PureExpr] -> Semantics (Bot :+: Ref :+: Def) PureExpr
> test5 (x : []) =
>     do
>       xv <- readRef x
>       xvv <- readRef xv
>       writeRef xv (xvv .+. int32 1)
>       returnc $ int32 0

> test7 :: [PureExpr] -> Semantics (Bot :+: Ref :+: Def) PureExpr
> test7 (x : []) = 
>     do
>       y <- newRef $ int32 0
>       xv <- readRef x
>       writeRef xv y
>       returnc $ int32 0


\subsection{References Tests}

\begin{verbatim}
int x1 = 0;
int x2 = x1;
int *x3 = &x1;
int *x4 = x3;
int **x5 = &x4;
int **x6 = x5;
int ***x7 = &x5;
int ***x8 = x7;
\end{verbatim}

> test100 :: Semantics Ref PureExpr
> test100 = do
>         a <- newRef $ int32 4
>         a1 <- readRef a
>         b <- newRef a
>         b1 <- readRef b
>         c <- newRef b
>         c1 <- readRef c
>         d <- newRef c
>         d1 <- readRef d
>         return d

\begin{verbatim}
int x1 = 0;
int* x2 = &x1;
int* x3 = x2;
int x4 = *x3;
int32_t* x5 = &*x3;
x1 = 1;
int x6 = 3
x2 = &x6
*x3 = 0
\end{verbatim}

> test101 :: Semantics Ref PureExpr
> test101 = do
>          x1 <- newRef $ int32 0
>          x2 <- newRef x1
>          x3 <- readRef x2
>          x4 <- readRef x3
>          x5 <- newRef x3
>          writeRef x1 $ int32 1
>          x6 <- newRef $ int32 3
>          writeRef x2 x6
>          writeRef x3 $ int32 0

\begin{verbatim}
int x1 = 0;
x1 = 1;
int *x2 = &x1;
x2 = &x1;
int **x3 = &x2;
x3 = &x2;
int ***x4 = &x3;
x4 = &x3;
int ***x5 = x4;
*x5 = &x2;
int **x6 = *x5;
*x6 = &x1;
int *x7 = *x6;
*x7 = 2;
int *x8 = &*x7;
x8 = &*x7;
int **x9 = &*x6;
x9 = &*x6;
\end{verbatim}

> test102 :: Semantics Ref PureExpr
> test102 = do
>          x1 <- newRef $ int32 0
>          writeRef x1 $ int32 1
>          x2 <- newRef x1
>          writeRef x2 x1
>          x3 <- newRef x2
>          writeRef x3 x2
>          x4 <- newRef x3
>          writeRef x4 x3
>          x5 <- readRef x4
>          writeRef x5 x2
>          x6 <- readRef x5
>          writeRef x6 x1
>          x7 <- readRef x6
>          writeRef x7 $ int32 2
>          x8 <- newRef x7
>          writeRef x8 x7
>          x9 <- newRef x6
>          writeRef x9 x6

\begin{verbatim}
int x1 = 0;
int *x2 = &x1;
int *x3 = &x1;
int *x4 = x2;
int *x5 = x3;
*x4 = 2
int *x6 = x3
*x5 = 3
int *x7 = x2;
\end{verbatim}

> test103 :: Semantics Ref PureExpr
> test103 = do
>          x1 <- newRef $ int32 0
>          x2 <- newRef x1
>          x3 <- newRef x1
>          x2' <- readRef x2
>          x3' <- readRef x3
>          writeRef x2' $ int32 2
>          x4 <- readRef x3
>          writeRef x3' $ int32 3
>          x5 <- readRef x2
>          return Void


\begin{verbatim}
int x1 = 0;
x1 = 1;
int x2 = x1;
// x2 == 1
\end{verbatim}

> test104 :: Semantics Ref PureExpr
> test104 = do
>          x1 <- newRef $ int32 0
>          writeRef x1 $ int32 1
>          y <- readRef x1
>          return Void

\begin{verbatim}
int x1 = 0;
int* x2 = &x1;
int* x3 = x2;
*x3 = 1;
int x4 = x1;
// x4 == 1
\end{verbatim}

> test105 :: Semantics Ref PureExpr
> test105 = do
>          x1 <- newRef $ int32 0
>          x2 <- newRef x1
>          x3 <- readRef x2
>          writeRef x3 $ int32 1
>          x4 <- readRef x1
>          return Void

\begin{verbatim}
int x1 = 0;
int* x2 = &x1;
int** x3 = &x2;
int** x4 = x3;
int* x5 = *x4;
*x5 = 1;
int x6 = x1;
\end{verbatim}

> test106 :: Semantics Ref PureExpr
> test106 = do
>          x1 <- newRef $ int32 0
>          x2 <- newRef x1
>          x3 <- newRef x2
>          x4 <- readRef x3
>          x5 <- readRef x4
>          writeRef x5 $ int32 1
>          x6 <- readRef x1
>          return Void

\begin{verbatim}
int32_t x1 = 0;
int32_t* x2 = &x1;
int32_t* x3 = x2;
int32_t x4 = *x3;
*x3 = ( x4 + 1 );
\end{verbatim}

> test107 :: Semantics Ref ()
> test107 = do
>           x1 <- newRef $ int32 0
>           x2 <- newRef x1
>           x3 <- readRef x2
>           x4 <- readRef x3
>           writeRef x3 (x4 .+. int32 1)
>           return ()

> test108 :: Semantics Ref ()
> test108 = do
>           x1 <- newRef $ int32 4
>           x2 <- newRef $ int32 5
>           x3 <- newRef x1
>           x4 <- readRef x3
>           writeRef x3 x2
>           writeRef x4 $ int32 1
>           x5 <- readRef x1
>           x6 <- readRef x2
>           return ()

> test109 :: Semantics Ref ()
> test109 = do
>           x1 <- newRef $ int32 4
>           x2 <- newRef $ int32 5
>           x3 <- newRef x1
>           x4 <- newRef x2
>           x5 <- newRef x3
>           x6 <- readRef x5
>           x7 <- readRef x6
>           writeRef x5 x4
>           writeRef x7 $ int32 1
>           return ()

> test110 :: Semantics Ref ()
> test110 = do
>           x1 <- newRef $ int32 4
>           x2 <- newRef x1
>           x3 <- readRef x2
>           x4 <- readRef x3
>           x5 <- newRef x4
>           return ()

> test111 :: Semantics Ref ()
> test111 = do
>           x1 <- newRef $ int32 0
>           x2 <- newRef x1
>           writeRef x1 $ int32 1
>           x3 <- newRef x1
>           x4 <- readRef x1
>           x5 <- newRef x4
>           writeRef x5 x1
>           writeRef x5 x4
>           x6 <- readRef x4
>           writeRef x4 $ int32 0
>           x7 <- newRef x3
>           x8 <- readRef x7
>           x9 <- newRef x8
>           x10 <- newRef x9
>           x11 <- readRef x10
>           x12 <- readRef x11
>           x13 <- readRef x12
>           writeRef x12 x1
>           writeRef x12 x4
>           writeRef x10 x9
>           writeRef x7 x8
>           writeRef x11 x5
>           writeRef x11 x8
>           return ()

\subsection{Structures Test}

\begin{verbatim}
struct { int f; } s1;
struct s1* d1 = (struct s1 *) malloc(sizeof(struct s1));
d1->f = 0;

struct { struct s1 *g } s2;
struct s2* d2 = (struct s2 *) malloc(sizeof(struct s2));
d2->g = d1;

struct s2* d3 = (struct s2 *) malloc(sizeof(struct s2));
d3->g = d1;

struct s1 *d4 = d2->g;

struct s1 *d5 = d3->g;

d4->f = 1

int d6 = d5->f

d5->f = 2

int d7 = d4->f
\end{verbatim}

> test200 :: Semantics Struct ()
> test200 = do
>          x1 <- newStruct "s1" [((TInt Signed TInt32), "f", int32 0)]
>          x2 <- newStruct "s2" [(TStruct "s1" [("f", (TInt Signed TInt32))], "g", x1)]
>          x3 <- newStruct "s2" [(TStruct "s1" [("f", (TInt Signed TInt32))], "g", x1)]
>          x4 <- readStruct x2 "g"
>          x5 <- readStruct x3 "g"
>          writeStruct x4 "f" (int32 1)
>          x6 <- readStruct x5 "f"
>          writeStruct x5 "f" (int32 2)
>          x7 <- readStruct x4 "f"
>          return ()


\begin{verbatim}
struct { int32_t f1; float f2;  } s1;
struct s1* d1 = (struct s1*) malloc(sizeof(struct s1));
d1->f1 = 0;
d1->f2 = 1.0;

struct { int32_t g1; struct s1 * g2;  } s2;
struct s2* d2 = (struct s2*) malloc(sizeof(struct s2));
d2->g1 = 0;
d2->g2 = d1;

int32_t d3 = d1->f1;

d1->f1 = 1;

float d4 = d1->f2;

d1->f2 = 4.0;

int32_t d5 = d2->g1;

d2->g1 = 2;

struct s1 * d6 = d2->g2;

d2->g2 = d1;

int32_t d7 = d6->f1;

d6->f1 = 1;

float d8 = d6->f2;

d6->f1 = 4.0;

int32_t x9 = 2;

struct { struct s2 * h1; int32_t* h2;  } s3;
struct s3* d10 = (struct s3*) malloc(sizeof(struct s3));
d10->h1 = d2;
d10->h2 = &x9;

int32_t* d11 = d10->h2;

*d11 = 4;

int size_x12 = 4;
float* d12 = ( float* ) malloc( (size_x12 + 1)  * sizeof( float ));
d12[0] = size_x12;
for (int i = 1; i <= size_x12; i++){
    d12[i] = 2.0;
}

struct { float* i1;  } s4;
struct s4* d13 = (struct s4*) malloc(sizeof(struct s4));
d13->i1 = d12;

float* d14 = d13->i1;

int index_x15 = 1;
int size_d14_x15 = d14[0];
if ( index_x15 < size_d14_x15 ){
    d14[ 1 + index_x15] = 12.0;
} else {
    assert(! "Out of bound " );
}
\end{verbatim}

> typeStruct1 :: TypeExpr
> typeStruct1 = TStruct "s1" [("f1", (TInt Signed TInt32)), ("f2", TFloat)]

> typeStruct2 :: TypeExpr
> typeStruct2 = TStruct "s2" [("g1", (TInt Signed TInt32)), ("g2", typeStruct1)]

> test201 :: Semantics (Ref :+: Struct :+: Array) ()
> test201 = do
>          x1 <- newStruct "s1" [((TInt Signed TInt32), "f1", int32 0),
>                                (TFloat, "f2", float 1)]
>          x2 <- newStruct "s2" [((TInt Signed TInt32), "g1", int32 0),
>                                (typeStruct1, "g2", x1)]
>          x3 <- readStruct x1 "f1"
>          writeStruct x1 "f1" $ int32 1
>          x4 <- readStruct x1 "f2"
>          writeStruct x1 "f2" $ float 4
>          x5 <- readStruct x2 "g1"
>          writeStruct x2 "g1" $ int32 2
>          x6 <- readStruct x2 "g2"
>          writeStruct x2 "g2" x1
>          x7 <- readStruct x6 "f1"
>          writeStruct x6 "f1" $ int32 1
>          x8 <- readStruct x6 "f2"
>          writeStruct x6 "f1" $ float 4
>          x9 <- newRef $ int32 2
>          x10 <- newStruct "s3" [(typeStruct2, "h1", x2),
>                                 (TPointer (TInt Signed TInt32) Avail, "h2", x9)]
>          x11 <- readStruct x10 "h2"
>          x12 <- writeRef x11 $ int32 4
>          x13 <- newArray (float 2) (int32 4)
>          x14 <- newStruct "s4" [(TArray TFloat, "i1", x13)]
>          x15 <- readStruct x14 "i1"
>          x16 <- writeArray x15 (int32 1) (float 12)
>          return ()


\begin{verbatim}
struct { int f; } s1;
struct s1* d1 = (struct s1 *) malloc(sizeof(struct s1))
d1->f = 0

struct { struct s1 *g; } s2;
struct s2* d2 = (struct s2 *) malloc(sizeof(struct s2))
d2->g = d1

struct s1* d3 = d2->g

struct s2* d4 = (struct s2 *) malloc(sizeof(struct s2))
d4->g = d3
\end{verbatim}

> typeSt1 :: TypeExpr
> typeSt1 = TStruct "s1" [("f", (TInt Signed TInt32))]

> test202 :: Semantics Struct ()
> test202 = do
>           x1 <- newStruct "s1" [((TInt Signed TInt32), "f", int32 0)]
>           x2 <- newStruct "s2" [(typeSt1, "g", x1)]
>           x3 <- readStruct x2 "g" 
>           x4 <- newStruct "s2" [(typeSt1, "g", x3)]
>           return ()

\begin{verbatim}
int32_t x1 = 0;

struct { int32_t* f;  } s1;
struct s1* d2 = (struct s1*) malloc(sizeof(struct s1));
d2->f = &x1;

int32_t* d3 = d2->f;

int32_t x4 = *d3;

*d3 = 3;
\end{verbatim}

> test203 :: Semantics (Ref :+: Struct) ()
> test203 = do
>           x1 <- newRef $ int32 0
>           x2 <- newStruct "s1" [(TPointer (TInt Signed TInt32) Avail,"f", x1)]
>           x3 <- readStruct x2 "f"
>           x4 <- readRef x3
>           writeRef x3 $ int32 3
>           return ()


\subsection{Arrays Tests}

\begin{verbatim}
int size_x1 = 1;
int32_t* d1 = ( int32_t* ) malloc( (size_x1 + 1)  * sizeof( int32_t ));
d1[0] = size_x1;
for (int i = 1; i <= size_x1; i++){
    d1[i] = 0;
}

int size_x2 = 1;
int32_t** d2 = ( int32_t** ) malloc( (size_x2 + 1)  * sizeof( int32_t* ));
d2[0] = size_x2;
for (int i = 1; i <= size_x2; i++){
    d2[i] = d1;
}

int size_x3 = 1;
int32_t** d3 = ( int32_t** ) malloc( (size_x3 + 1)  * sizeof( int32_t* ));
d3[0] = size_x3;
for (int i = 1; i <= size_x3; i++){
    d3[i] = d1;
}

int index_x4 = 0;
int size_d2_x4 = d2[0];
int32_t* d4;
if ( index_x4 < size_d2_x4 ){
    d4 = d2[ index_x4 + 1];
} else {
    assert(! "Out of bound");
    d4 = NULL;
}

int index_x5 = 0;
int size_d3_x5 = d3[0];
int32_t* d5;
if ( index_x5 < size_d3_x5 ){
    d5 = d3[ index_x5 + 1];
} else {
    assert(! "Out of bound");
    d5 = NULL;
}

int index_x6 = 0;
int size_d4_x6 = d4[0];
if ( index_x6 < size_d4_x6 ){
    d4[ 1 + index_x6] = 1;
} else {
    assert(! "Out of bound " );
}

int index_x7 = 0;
int size_d5_x7 = d5[0];
int32_t d7;
if ( index_x7 < size_d5_x7 ){
    d7 = d5[ index_x7 + 1];
} else {
    assert(! "Out of bound");
    d7 = NULL;
}

int index_x8 = 0;
int size_d5_x8 = d5[0];
if ( index_x8 < size_d5_x8 ){
    d5[ 1 + index_x8] = 2;
} else {
    assert(! "Out of bound " );
}

int index_x9 = 0;
int size_d4_x9 = d4[0];
int32_t d9;
if ( index_x9 < size_d4_x9 ){
    d9 = d4[ index_x9 + 1];
} else {
    assert(! "Out of bound");
    d9 = NULL;
}
\end{verbatim}

> test300 :: Semantics Array ()
> test300 = do
>          x1 <- newArray (int32 0) (int32 1)
>          x2 <- newArray x1 (int32 1)
>          x3 <- newArray x1 (int32 1)
>          x4 <- readArray x2 (int32 0)
>          x5 <- readArray x3 (int32 0)
>          writeArray x4 (int32 0) (int32 1) 
>          x6 <- readArray x5 (int32 0)
>          writeArray x5 (int32 0) (int32 2)
>          x7 <- readArray x4 (int32 0)
>          return ()

\begin{verbatim}
int size_x1 = 4;
float* d1 = ( float* ) malloc( (size_x1 + 1)  * sizeof( float ));
d1[0] = size_x1;
for (int i = 1; i <= size_x1; i++){
    d1[i] = 2.0;
}

int index_x2 = ( 3 - 2 );
int size_d1_x2 = d1[0];
float d2;
if ( index_x2 < size_d1_x2 ){
    d2 = d1[ index_x2 + 1];
} else {
    assert(! "Out of bound");
    d2 = NULL;
}

int index_x3 = 5;
int size_d1_x3 = d1[0];
float d3;
if ( index_x3 < size_d1_x3 ){
    d3 = d1[ index_x3 + 1];
} else {
    assert(! "Out of bound");
    d3 = NULL;
}

int index_x4 = 3;
int size_d1_x4 = d1[0];
if ( index_x4 < size_d1_x4 ){
    d1[ 1 + index_x4] = 19.0;
} else {
    assert(! "Out of bound " );
}


int size_x5 = 10;
float** d5 = ( float** ) malloc( (size_x5 + 1)  * sizeof( float* ));
d5[0] = size_x5;
for (int i = 1; i <= size_x5; i++){
    d5[i] = d1;
}

int index_x6 = 2;
int size_d5_x6 = d5[0];
if ( index_x6 < size_d5_x6 ){
    d5[ 1 + index_x6] = d1;
} else {
    assert(! "Out of bound " );
}

int index_x7 = 4;
int size_d5_x7 = d5[0];
float* d7;
if ( index_x7 < size_d5_x7 ){
    d7 = d5[ index_x7 + 1];
} else {
    assert(! "Out of bound");
    d7 = NULL;
}

int32_t x8 = 3;

int size_x9 = 4;
int32_t** d9 = ( int32_t** ) malloc( (size_x9 + 1)  * sizeof( int32_t* ));
d9[0] = size_x9;
for (int i = 1; i <= size_x9; i++){
    d9[i] = &x8;
}

int32_t* x10 = &x8;

int size_x11 = 3;
int32_t*** d11 = ( int32_t*** ) malloc( (size_x11 + 1)  * sizeof( int32_t** ));
d11[0] = size_x11;
for (int i = 1; i <= size_x11; i++){
    d11[i] = &x10;
}
\end{verbatim}

> test301 :: Semantics (Ref :+: Array) ()
> test301 = do
>          x1 <- newArray (float 2) (int32 4)
>          x10 <- readArray x1 (int32 3 .-. int32 2)
>          x15 <- readArray x1 (int32 5)
>          writeArray x1 (int32 3) (float 19)

>          x2 <- newArray x1 (int32 10)
>          writeArray x2 (int32 2) x1
>          x21 <- readArray x2 (int32 4)
>
>          x3 <- newRef (int32 3)
>          x4 <- newArray x3 (int32 4)
>
>          x5 <- newRef x3
>          x6 <- newArray x5 (int32 3)
>
>          return ()

\begin{verbatim}
struct { int32_t f1; float f2;  } s1;
struct s1* d1 = (struct s1*) malloc(sizeof(struct s1));
d1->f1 = 0;
d1->f2 = 1.0;

int size_x2 = 5;
struct s1 ** d2 = ( struct s1 ** ) malloc( (size_x2 + 1)  * sizeof( struct s1 * ));
d2[0] = size_x2;
for (int i = 1; i <= size_x2; i++){
    d2[i] = d1;
}

int index_x3 = 2;
int size_d2_x3 = d2[0];
struct s1 * d3;
if ( index_x3 < size_d2_x3 ){
    d3 = d2[ index_x3 + 1];
} else {
    assert(! "Out of bound");
    d3 = NULL;
}

int32_t d4 = d3->f1;

d3->f1 = 0;
\end{verbatim}

> test302 :: Semantics (Struct :+: Array) ()
> test302 = do
>          x1 <- newStruct "s1" [((TInt Signed TInt32), "f1", int32 0),
>                                (TFloat, "f2", float 1)]
>          x2 <- newArray x1 (int32 5)
>          x3 <- readArray x2 (int32 2)
>          x4 <- readStruct x3 "f1"
>          writeStruct x3 "f1" (int32 0)
>          return ()

\begin{verbatim}
int x1 = 0

int size_x2 = 4;
int32_t** d2 = ( int32_t** ) malloc( (size_x2 + 1)  * sizeof( int32_t* ));
d2[0] = size_x2;
for (int i = 1; i <= size_x2; i++){
    d2[i] = &x1;
}

int index_x3 = 1;
int size_d2_x3 = d2[0];
int32_t* d3;
if ( index_x3 < size_d2_x3 ){
    d3 = d2[ index_x3 + 1];
} else {
    assert(! "Out of bound");
    d3 = NULL;
}

int32_t x4 = *d3;

*d3 = 3;
\end{verbatim}

> test303 :: Semantics (Ref :+: Array) ()
> test303 = do
>           x1 <- newRef $ int32 0
>           x2 <- newArray x1 (int32 4)
>           x3 <- readArray x2 (int32 1)
>           x4 <- readRef x3
>           writeRef x3 $ int32 3
>           return ()

\begin{verbatim}
int32_t x1 = 0;

int32_t* x2 = &x1;

int size_x3 = 3;
int32_t*** d3 = ( int32_t*** ) malloc( (size_x3 + 1)  * sizeof( int32_t** ));
d3[0] = size_x3;
for (int i = 1; i <= size_x3; i++){
    d3[i] = &x2;
}

int index_x4 = 1;
int size_d3_x4 = d3[0];
int32_t** d4;
if ( index_x4 < size_d3_x4 ){
    d4 = d3[ index_x4 + 1];
} else {
    assert(! "Out of bound");
    d4 = NULL;
}

int32_t* x5 = *x4;

*x5 = 4;
\end{verbatim}

> test304 :: Semantics (Ref :+: Array) ()
> test304 = do
>           x1 <- newRef $ int32 0
>           x2 <- newRef x1
>           x3 <- newArray x2 (int32 3)
>           x4 <- readArray x3 (int32 1)
>           x5 <- readRef x4
>           writeRef x5 $ int32 4
>           return ()


\section{Functions}


> body400 :: [PureExpr] ->  Semantics Bot PureExpr
> body400 _ = do
>        return $ int32 2


> body401 :: [PureExpr] ->  Semantics (Bot :+: Def :+: Conditionals) PureExpr
> body401 (_ : y : []) = do
>           ifc (do return (y .==. int32 3) :: Semantics Bot PureExpr)
>               (do
>                returnc $ int32 2 :: Semantics Def PureExpr
>               )
>               (do
>                returnc $ int32 4 :: Semantics Def PureExpr
>               )
>           returnc $ int32 5



> unionTest :: TypeExpr
> unionTest = TUnion "test_t" [("field1", TInt Signed TInt32), ("field2", TFloat), ("field3", TInt Signed TInt32)]

> body402 :: [PureExpr] -> Semantics (Ref :+: Def :+: Assert :+: Conditionals) PureExpr
> body402 (x : []) = 
>      do
>       y <- newRef $ x
>       c <- newRef $ int32 1
>       while (do yv <- readRef y; return (yv .>. (int32 0)) :: Semantics Ref PureExpr) 
>           ((do
>             yv <- readRef y
>             writeRef y (yv .-. int32 1)
>             cv <- readRef c
>             writeRef c (cv .*. yv)) :: Semantics (Assert :+: Ref) PureExpr)
>       cv <- readRef c
>       returnc cv

> test403 :: Semantics (Bot :+: Def :+: Conditionals) PureExpr
> test403 =
>     do
>       f1 <- def [] "name" body401 int32T  [int32T, int32T]
>       x <- call f1 [int32 3, int32 1]
>       return Void

> test404 :: Semantics (Bot :+: Ref :+: Def) PureExpr
> test404 =
>     do
>       f1 <- def [] "bug" test7 voidT [ptrT (ptrT int32T)]
>       
>       x <- newRef $ int32 1
>       xp <- newRef x
>       
>       _ <- call f1 [xp]
>            
>       returnc x

> test405 :: Semantics (Bot :+: Ref :+: Def) PureExpr
> test405 =
>     do
>       f1 <- def [] "inc" test5 voidT [ptrT (ptrT int32T)]
>       
>       x <- newRef $ int32 4
>       y <- newRef $ x
>       _ <- call f1 [y]
>       _ <- call f1 [y]
>       yv <- readRef y
>       xv <- readRef yv
>       returnc xv

> test406 :: Semantics (Bot :+: Array :+: Def) PureExpr
> test406 =
>     do
>       f1 <- def [] "f1" body407 voidT [arrayT int32T]
>       x <- newArray (float 4) (int32 3)
>       _ <- call f1 [x]
>       return x

> body407 :: [PureExpr] -> Semantics (Bot :+: Array :+: Def) PureExpr
> body407 (x : [])= 
>     do
>       x1 <- readArray x (int32 1)
>       return Void

> test408 :: Semantics (Bot :+: Struct :+: Def) PureExpr
> test408 =
>     do
>       f1 <- def [] "f1" body409 voidT [structT "s1" [("f1",(int32T))]]
>       x <- newStruct "s1" [((TInt Signed TInt32), "f1", int32 4)]
>       _ <- call f1 [x]
>       return Void

> body409 :: [PureExpr] -> Semantics (Bot :+: Struct :+: Def) PureExpr
> body409 (x : [])= 
>     do
>       x1 <- readStruct x "f1"
>       return Void

> test410 :: Semantics (Bot :+: Ref :+: Assert :+: Conditionals :+: Def) PureExpr
> test410 = do
>           factorial <- def [] "factorial" body402 int32T [int32T]
>           x <- call factorial [int32 4]
>           return Void

\section{Static Arrays}

> test500 :: Semantics StaticArray PureExpr
> test500 = do
>          x1 <- newStaticArray [int32 1, int32 2, int32 3]
>          x2 <- newStaticArray [x1, x1, x1, x1]
>          x3 <- newStaticArray [x1, x1]
>          x4 <- readStaticArray x2 (int32 0)
>          x5 <- readStaticArray x3 (int32 1)
>          writeStaticArray x4 (int32 0) (int32 1) 
>          x6 <- readStaticArray x5 (int32 0)
>          writeStaticArray x5 (int32 0) (int32 2)
>          x7 <- readStaticArray x4 (int32 0)
>          return void

> test501 :: Semantics (Ref :+: StaticArray) PureExpr
> test501 = do
>          x1 <- newStaticArray [float 2, float 3, float 1]
>          x10 <- readStaticArray x1 (int32 3 .-. int32 2)
>          x15 <- readStaticArray x1 (int32 5)
>          writeStaticArray x1 (int32 3) (float 19)

>          x2 <- newStaticArray [x1, x1, x1]
>          writeStaticArray x2 (int32 2) x1
>          x21 <- readStaticArray x2 (int32 4)
>
>          x3 <- newRef (int32 3)
>          x4 <- newStaticArray [x3, x3]
>
>          x5 <- newRef x3
>          x6 <- newStaticArray [x5, x5, x5]
>
>          return void

> test502 :: Semantics (Struct :+: StaticArray) PureExpr
> test502 = do
>          x1 <- newStruct "s1" [((TInt Signed TInt32), "f1", int32 0),
>                                (TFloat, "f2", float 1)]
>          x2 <- newStaticArray [x1, x1]
>          x3 <- readStaticArray x2 (int32 2)
>          x4 <- readStruct x3 "f1"
>          writeStruct x3 "f1" (int32 0)
>          return void

> test503 :: Semantics (Ref :+: StaticArray) PureExpr
> test503 = do
>           x1 <- newRef $ int32 0
>           x2 <- newStaticArray [x1, x1]
>           x3 <- readStaticArray x2 (int32 1)
>           x4 <- readRef x3
>           writeRef x3 $ int32 3
>           return void

> test504 :: Semantics (Ref :+: StaticArray) PureExpr
> test504 = do
>           x1 <- newRef $ int32 0
>           x2 <- newRef x1
>           x3 <- newStaticArray [x2, x2, x2]
>           x4 <- readStaticArray x3 (int32 1)
>           x5 <- readRef x4
>           writeRef x5 $ int32 4
>           return void

> -}

%endif




> main :: IO ()
> main = do
>        putStrLn "Nothing done."

%if false 

> {- 

>        let (s, _) = compile (test500) emptyBinding in
>            putStrLn $ show s 

> --     let (v, h) = run (test6) emptyHeap in
> --        print $ show (symbEval v)

> -}

%endif