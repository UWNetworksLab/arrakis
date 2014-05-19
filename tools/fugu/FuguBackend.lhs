\documentclass{article}
\usepackage[english]{babel}
\usepackage{listings}
\usepackage{fullpage}
\usepackage{color}

%include polycode.fmt

%if false
  Error: DSL for error definition
   
  Copyright (c) 2009, 2011 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

\title{Filet-O-Fish Tutorial:\\
       The Fugu Error Definition Language}
\author{\Large{Pierre-Evariste \sc{Dagand}}}
\date{}

\begin{document}

\lstset{basicstyle=\ttfamily,
        columns=fullflexible,
        language=C}

\maketitle



\textcolor{red}{Warning: the comments below are out-of-sync with some
aspects of the code. Some will claim that this is a clear proof that
literate programming is useless. Others will note that in non-literate
code, I would not have written this disclaimer and the code would be a
complete mess. Anyway, feel free to improve the literate story: we
have variable-size blocks, error codes are not generated randomly
anymore, we use a threshold to differentiate success to failure
cases.}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{Introduction}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% <- Present the goal of the tutorial

In this tutorial, we aim at illustrating the usage of Filet-O-Fish
(FoF). Therefore, we will implement a small Domain-Specific Language
(DSL), using FoF in the back-end. However, we will also cover a
broader topic: how to design a new DSL taking advantage of FoF
particularities. In particular, this DSL will be an Error Definition
Language (EDL) called Fugu. Its functionalities are described in
Section~\ref{sec:edl}.

%%    -> Step-by-step, real life design

Hence, we will adopt a step-by-step approach. While covering the
various phases of development, we will try to devise a more
``principled'' approach, which could serve for future development.

%%    -> From C code to high-level language

One of our partis pris is that such work should start from mature C
code. FoF is meant as a safe meta-language, allowing the DSL designer
to, first, abstract over C and, then, manipulate these high-level
constructions in a powerful environment, such as Haskell or a theorem
prover.

%%    -> With a focus on the back-end 

For the sake of brevity, we will focus our presentation on the DSL
back-end. Hence, we will not write a parser for Fugu: the syntax of
this small language will be \emph{embedded} in Haskell, by the means
of some \emph{combinators}. We will take care of avoiding any
confusion between all those languages.

%% <- Outline
%%    -> Follow real development
%%    <- Intents of the Error Definition language
%%    <- Presentation of the C sample
%%    <- Designing the Syntax
%%    <- Backend

This tutorial is organized as follow. In a first Section, we specify
our requirements concerning Fugu. In a second Section, we consider
define a small class of errors and describe how we would like to
compile them down. From there, we carry a step-by-step implementation
of Fugu in Section~\ref{sec:backend}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{An Error Definition System}
\label{sec:edl}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% <- Replacing errno
%%      <- Overloaded meaning (several causes, one code)

The need for an Error Definition System arises from the deficiencies of
the traditional scheme, namely the \texttt{errno.h}, or similar,
file. In this file, developers typically aggregate the error codes of
all errors that could potentially occur in the system. In order to
scale, they quickly ``overload'' the meaning of these error codes:
instead of defining distinct error codes, they use a single code which
has multiple causes. For example, the {\sc Einval} error of FreeBSD
signals that ``some invalid argument was supplied: for example,
specifying an undefined signal to a signal function or a kill system
call''. In parallel, they also abuse the return values of
functions. For instance, a function returning a {\sc Null} pointer is
typically signalling an error. However, this does not provide much
information to the developer facing such case.

%%      <- Artificially limited namespace

Although this might have been historically relevant, there is no
incentive, today, to limit the error code space to 255. Using 16 bits
would allow us to define 65535 error codes. In such a huge space, we
would be freed from the burden of overloading error codes and
functions return values: when it is relevant, the developer should be
able to define new error codes. Hence, he could handle errors more
precisely, in the code as well as during the debugging process.

%%      <- Flat namespace

However, by imposing a flat name-space, the \texttt{errno.h} approach
reduces the benefit of defining more precise error codes. When an
error occurs, the developer is not able to relate it to a subsystem:
it is simply defined as one error among thousands of others. In a
sense, the error is more precise but still lacks some context. Being
able to give a context to an error would be a step forward, notably
during debugging.

%%      <- Absence of error-tracking

Finally, it is often the case that an error in one function must be
reported to several functions in the call-stack. Whereas the depth of
the call-stack is generally low, it is extremely inconvenient to
define case-specific error codes at each level (the number of cases
growing exponentially with the depth). Hence, we generally give up
accuracy and report a global, overloaded error code.

%% <- Design goals
%%      <- One class of errors per "component"

For these reasons, we provide the developer with the power of
defining classes of errors per ``component''. The notion of
\emph{component} is purposely kept fuzzy: it is up to the developer
to define its components, on a case-by-case basis. For example, it
could be a single function or a class of related functions.

%%      <- Call-trace

By using a 16 bits space, we are able get rid of the artificial
limitation of the state-space. But we can also take advantage of this
sub-word quantity in our 64 bits machines. Indeed, in one machine
word, we are able to handle 4 error codes. Hence, we can build the
call-trace by \emph{pushing} an error code in the previous one, and so
on along the call-path.

%%      <- Automated, human-readable error reporting

By relying on a tool to generate C code for us, we can also get rid of
a lot of boilerplate code. For example, when defining an error code,
we label it with a descriptive message as well as a short
acronym. Then, given an error, we should be able to report it to the
user, in a meaningful way.

%%      <- Sensibility to wild writes

Finally, in the event of wild write -- such as a buffer overflow -- on
the error code variable, we would like the error code to become
meaningless, instead of signaling an unrelated error. Therefore, we
should use random numbers to identify errors. Hence, the common
consequences of a wild write, such as overwriting with 0 or with a
character, could be detected more easily.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{C Sample Code}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% <- Why starting with a C sample


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Starting from C}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%     <- Ability for anyone (non-FoF user) to discuss 
%%        and modify the model
%%         <- Importance of user's feed-back

First of all, we would like to stress the importance of starting from
a mature C code. The first and foremost reason is the ability to
\emph{discuss} this draft with the final users of the DSL: whereas not
everyone is familiar with Haskell and FoF, C is the \emph{lingua
franca} for everyone. Thanks to this support, users are given the
power to influence the design of the DSL, without having to handle its
machinery. Moreover, this C file is an inexpensive support for testing
and debugging: we can quickly modify its behavior without dealing
with the compilation machinery.

%%     <- FoF should not be meant as a programming language per se
%%         <- Give a meaning

Although disturbing at first, FoF should \emph{not} be meant as a
programming language. First and foremost, FoF gives a meaning, the
semantics, to your DSL language. The fact that it can be compiled to C
comes as a bonus.

%%     <- C code serves as a weak specification of the intention
%%         <- Informal alternative semantics

Finally, a ``good'' DSL should be defined by an alternative semantics,
which would allow the DSL designer to check the correctness of the
compiler. Having a C sample of a particular instance can play this
role, for this test-case. Although it does not help in obtaining
formal guarantees, it is a first step toward a seemingly correct
compiler.


%% <- The Errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Our Test-Case}
\label{sec:example}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

In the following, we will define the error codes for two components:
``system'' and ``pci''. The ``\verb!system_err!'' class of errors will
contain the following cases of success:
\begin{description}
        \item[\verb!SYS_OK!] No error
        \item[\verb!SYS_ILIA!] Mafioso Pasta (private ETH joke, on SOSP lunches)
\end{description}

And the following cases of error:
\begin{description}
        \item[\verb!SYS_FAIL!] Failed
        \item[\verb!SYS_TEM!] Kernel Hacker
        \item[\verb!SYS_IPHUS!] Analphabet Greek
\end{description}

The ``\verb!pci_err!'' class of errors is more serious, with the
following cases of success:
\begin{description}
        \item[\verb!PCI_OK!] No PCI error
        \item[\verb!PCI_GET_CAP!] That is my cap
\end{description}

And this case of error:
\begin{description}
        \item[\verb!PCI_CAP_NOK!] Lost my cap
\end{description}

In the next Section, we review the C code \emph{we would like to
generate} from this or a similar description. Bear in mind that this
code is the result of a long and contradictory discussion with the
final users.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{The C Sample}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% <- Includes
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Includes}
%%%%%%%%%%%%%%%%%%%%


First of all, we will need, more or less, the following includes:

\begin{lstlisting}
 #include <stdlib.h>
 #include <stdio.h>
 #include <assert.h>
 #include <stdbool.h>
 #include <stdint.h>
 #include <stdarg.h>
\end{lstlisting}



%% <- Typedef
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Type Definitions}
%%%%%%%%%%%%%%%%%%%%

We define an error value as 64 bits machine word. This encompass both
the current error code and, potentially, 3 others stacked error codes.

\begin{lstlisting}
typedef uint64_t errval_t;
\end{lstlisting}

%% <- Enumeration
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Error codes}
%%%%%%%%%%%%%%%%%%%%

We define the error codes in an enumeration. Each code is identified
by a random number between 0 and 65535. The success codes are
identified by an odd number whereas failure codes are identified by an
even number. This will come handy when we define a global
\verb!err_is_ok! and \verb!err_is_fail! functions.

\begin{lstlisting}
enum err_code {
    SYS_OK = 11,
    SYS_FAIL = 84,
    SYS_ILIA = 93,
    SYS_TEM = 2,
    SYS_IPHUS = 34,
    PCI_OK = 41,
    PCI_CAP_NOK = 28,
    PCI_GET_CAP = 16
};
\end{lstlisting}


%% <- Labels
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Labels}
%%%%%%%%%%%%%%%%%%%%

Then, we statically define the previous error descriptions, acronyms,
and domains. This way, we will be able to provide a descriptive
notification to the user in case of error.

\begin{lstlisting}
static char *err_domains[] = {
    "System_err", "Pci_err", "Undefined"
};
static char *err_codes[] = {
    "SYS_OK", "SYS_FAIL", "SYS_ILIA",
    "SYS_TEM", "SYS_IPHUS", "PCI_OK",
    "PCI_CAP_NOK", "PCI_GET_CAP",
    "UNDEFINED"
};
static char *err_msgs[] = {
    "No error", "Failed",
    "Mafioso Pasta",
    "Kernel Hacker",
    "Analphabet Greek",
    "No PCI error",
    "Lost my cap",
    "That is my cap",
    "Undefined error",
};
\end{lstlisting}


%% <- Pop and push Error number
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Getting and Stacking Errors}
%%%%%%%%%%%%%%%%%%%%

Given an \verb!errval_t!, we will need to extract the latest error
code. This is done by accessing the lowest 16 bits.

\begin{lstlisting}
static inline enum err_code err_no(errval_t err){
    return (err & ((1 << 16) - 1));
}
\end{lstlisting}

Similarly, we can stack up an error code in an existing error value:
we simply have to shift-left the stack and write the current error
code in the lowest 16 bits.

\begin{lstlisting}
static inline errval_t err_push(errval_t err, enum err_code code){
    return (err << 16) || code;
}
\end{lstlisting}



%% <- Testing for success or failure
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Success and Failure}
%%%%%%%%%%%%%%%%%%%%

We will frequently need to test an error code for failure or
success. Hence, we provide two function which are both efficient and
apply to any error code.

\begin{lstlisting}
static inline bool err_is_ok(errval_t err){
    return err_no(err) % 2;
}
static inline bool err_is_fail(errval_t err){
    return 1 - (err_no(err) % 2);
}
\end{lstlisting}


%% <- Error-specific tests
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Error-specific Tests}
%%%%%%%%%%%%%%%%%%%%

Then, we might need to test some error codes against a specific
error. This is simply achieved by the following functions.

\begin{lstlisting}
static inline bool err_is_sys_fail(errval_t e){
    return err_no(e) == SYS_FAIL;
}
...
static inline bool err_is_pci_ok(errval_t e){
    return err_no(e) == SYS_OK;
}
...
\end{lstlisting}


%% <- Asserting success or failure
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Asserting Success and Failure}
%%%%%%%%%%%%%%%%%%%%

Similarly, during the debugging phase, we might need to assert the
correctness or incorrectness of a result. Hence, the following
assertions.

\begin{lstlisting}
static inline void err_assert_ok(errval_t err){
    assert(err_is_ok(err));
}
static inline void err_assert_fail(errval_t err){
    assert(err_is_fail(err));
}
\end{lstlisting}


%% <- Printers on errors
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Printing Errors}
%%%%%%%%%%%%%%%%%%%%



\begin{lstlisting}
static inline char *err_getstring(errval_t err) {

    switch (err_no(err)) {
    case SYS_OK: {
        return err_msgs[0]; 
        break;
    }
    ...
    case PCI_OK: {
        return err_msgs[5]; 
        break;
    }
    ...
    default: {
        return err_msgs[8];
        break;
    }
    }
    return NULL;
}

static inline char *err_getcode(errval_t err ) {

    switch (err_no(err)) {
    case SYS_OK: {
        return err_codes[0]; 
        break;
    }
    ...
    case PCI_OK: {
        return err_codes[5]; 
        break;
    }
    ...
    default: {
        return err_codes[8];
        break;
    }
    }
    return NULL;
}

static inline char *err_getdomain(errval_t err ) {

    switch (err_no(err)) {
    case SYS_OK: {
        return err_domains[0]; 
        break;
    }
    ...
    case PCI_OK: {
        return err_domains[1]; 
        break;
    }
    ...
    default: {
        return err_domains[2];
        break;
    }
    }
    return NULL;
}
\end{lstlisting}


%% <- Printing the call trace
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Printing the Call-trace}
%%%%%%%%%%%%%%%%%%%%

Finally, mostly during debugging, we would like to inspect the
call-trace leading to an error. This is achieved by the following
code. First, it verifies that the stack contains an error. Then, it
\emph{pop}s error codes from it and prints their domain, description,
and acronym.

\begin{lstlisting}
static inline void err_print_calltrace(errval_t err){
    
    if (err_is_fail(err)){
        
        enum err_code x;
        while( (x = err_no(err)) != 0 ){
            printf("Failure: (%12s) %20s [%s]\n",
                   err_getdomain(x),
                   err_getstring(x),
                   err_getcode(x));
            err = err >> 16;
        }       
    }
}
\end{lstlisting}


This concludes our draft implementation of an error management
system. Now, we are aware of our needs, what the user needs to specify
and what constructs should we abstract away. Let us start implementing
Fugu, now.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Fugu: the Error Definition Language}
\label{sec:backend}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%if false

> module FuguBackend where

> import System.IO
> import System.Random
> import Data.Char
> import Data.Maybe
> import Data.List
> import Data.Monoid
> import qualified Data.Bits as B

> import Debug.Trace

> import Semantics
> import Constructs
> import PureExpressions
> import Expressions

> import Constructs.Conditionals
> import Constructs.References
> import Constructs.Functions
> import Constructs.Structures
> import Constructs.Arrays
> import Constructs.Enumerations
> import Constructs.Typedef
> import Constructs.Strings

> import Libc.Assert
> import Libc.Printf

> import Compile

%endif

In the following Sections, we describe the implementation of Fugu. As
usual with me, you are reading a literate code: the Haskell compiler
processes this very same file. Therefore, you are reading the real
Fugu's code. In Section \ref{sec:syntax}, we define Fugu's AST and a
set of combinators, to be able to embed Fugu's language into
Haskell. In Section \ref{sec:fof_backend}, we implement the back-end
using Filet-O-Fish.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Fugu Embedded Syntax}
\label{sec:syntax}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%
\subsubsection{The Abstract-Syntax Tree}
%%%%%%%%%%%%%%%%%%%%

First, we define the Abstract-Syntax Tree (AST). An error definition
file is a list of error classes:

> type Errors = [ ErrorClass ]

Where an error class is identified by a string, its name, and a list
of error definitions:

> data ErrorClass = ErrorClass String [ ErrorField ]

An error definition is composed by a description and a short
acronym. We also specify an error status, either a failure case or a
success case.

> data ErrorField = ErrorField ErrorStatus String String
>
> data ErrorStatus = Failure
>                  | Success
>                  | DefaultSuccess
>                  deriving Eq



%%%%%%%%%%%%%%%%%%%%
\subsubsection{Enbedding Fugu in Haskell}
%%%%%%%%%%%%%%%%%%%%

Instead of designing a parser, possibly using Parsec, we chose to
embed the Fugu language inside Haskell. This means that we are going
to define some Haskell functions, operating on the AST. By combining
these functions, we will be able to build a complete AST that can be
processed by Fugu's back-end.

First of all, we can build an |ErrorField| out of the error acronym
and description, using one of these two combinators:

> success, failure :: String -> String -> ErrorField
> success acronym description = ErrorField Success acronym description
> failure acronym description = ErrorField Failure acronym description

Given a list of such fields and a global name for them, we can then
build an |ErrorClass|:

> errors :: String -> [ ErrorField ] -> ErrorClass
> errors className errors = ErrorClass className errors

And, finally, given a list of such classes, we get a valid error
definition ``file'':

> define :: [ErrorClass] -> Errors
> define errors = errors



%%%%%%%%%%%%%%%%%%%%
\subsubsection{Our Example in Fugu}
%%%%%%%%%%%%%%%%%%%%


If we translate our informal description of Section~\ref{sec:example},
this leads to the following code:

> testE = define [
>          errors "system_err" [
>                      success "SYS_ERR_OK" "No error",
>                      failure "SYS_ERR_FAIL" "Failed",
>                      success "SYS_ERR_ILIA" "Mafioso Pasta",
>                      failure "SYS_ERR_TEM" "Kernel Hacker",
>                      failure "SYS_ERR_IPHUS" "Analphabet Greek"
>                     ],
>          errors "pci_err" [
>                      success "PCI_ERR_OK" "No PCI error",
>                      failure "PCI_ERR_NOK" "Lost my cap",
>                      success "PCI_ERR_CAP" "That is my cap"
>                     ]
>          ]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Filet-O-Fish Back-end}
\label{sec:fof_backend}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

At this point, there is no way we can postpone the implementation of
the compiler back-end. So, \emph{Ave, Caesar, Morituri Te Salutant}.


%% <- Typedefs
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Defining Type Aliases}
%%%%%%%%%%%%%%%%%%%%


We start with some type aliasing. In its basic form, FoF only knows
\emph{primitive} types such as integers, floats, and user-defined
structures, unions, and arrays. Therefore, if we are to use the
standard |bool| or |char|, we have to inform FoF of their existence as
well as to which primitive type they match to.

> boolT :: TypeExpr
> boolT = typedef uint64T "bool"

Similarly, we can define our own aliases:

> err_codeT :: TypeExpr
> err_codeT = enumT "err_code" [] -- not right, [] is populated
> err_code :: Int -> PureExpr
> err_code = uint16 . toInteger
>
> errval_tT :: TypeExpr
> errval_tT = typedef uintptrT "errval_t"


%% <- Defining general functions
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Defining General Functions}
%%%%%%%%%%%%%%%%%%%%

Implementing the |err_no|, |err_push|, |err_is_ok|, and |err_is_fail|
consists in a straightforward translation from C to FoF. Let us give a
closer look at |err_no|, for instance. 

The body of |err_no| consists in doing some bit twiddling on the error
stack and returning this value:

> err_no_int :: Integer -> [PureExpr] -> FoFCode PureExpr
> err_no_int bitBlock (err : []) =
>     returnc $ cast err_codeT $ (err .&. ((uint64 1 .<<. uint64 bitBlock) .-. uint64 1))

We build a function out of the function body thanks to the |def|
operator. Along with the function body, we have to pass the return
type as well as the parameters types:

> err_noF :: Integer -> FoFCode PureExpr 
> err_noF bitBlock = def [Static, Inline] "err_no" (err_no_int bitBlock)
>                                         err_codeT  
>                                         [(errval_tT, Just "errval")]

And we are done: |err_noF| is corresponds to the C |err_no|
function. We will see later on how it can be called.

Similarly, we define |err_pushF|. This time, we declare the function
body in a @where@ clause. Note, however, that we have to provide the
type of |err_push_int| as it cannot be inferred by the Haskell
compiler.


> err_pushF :: Integer -> FoFCode PureExpr 
> err_pushF bitBlock = def [Static, Inline] "err_push" err_push_int errval_tT  [(errval_tT, Just "errval"),
>                                                                               (err_codeT, Just "errcode")]
>     where err_push_int (err : code : []) =
>               returnc ((err .<<. uint64 bitBlock) .|. 
>                        (cast errval_tT $ uint64 (((toInteger 1) `B.shiftL` (fromInteger bitBlock)) - 1) .&. code))

And the same goes for |err_is_okF| and |err_is_failF|:

> err_is_okF :: Integer -> Int -> FoFCode PureExpr
> err_is_okF bitBlock indexFailures = def [Static, Inline] "err_is_ok" err_is_ok_int boolT [(errval_tT, Just "errval")]
>     where err_is_ok_int (err : []) =
>               do
>               err_no <- err_noF bitBlock
>               err_no_e <- call err_no [err]
>               returnc (err_no_e .<. uint64 (toInteger indexFailures))
>
> err_is_failF :: Integer -> Int -> FoFCode PureExpr
> err_is_failF bitBlock indexFailures = def [Static, Inline] "err_is_fail" err_is_fail_int boolT [(errval_tT, Just "errval")]
>     where err_is_fail_int (err : []) =
>               do
>               err_no <- err_noF bitBlock
>               err_no_e <- call err_no [err]
>               returnc (err_no_e .>=. uint64 (toInteger indexFailures))

%% <- Defining parameterized functions
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Defining Parameterized Functions}
%%%%%%%%%%%%%%%%%%%%

An example is |err_get_array|, which is used to build the
|err_getdomain|, |err_getstring|, and |err_getcode| functions. Hence,
|err_get_array| takes as first argument the postfix of the |err_get*|
target. It is also provided a reference to the array that contains the
descriptive strings. And the last argument consists of a list of pairs
(error code, array index).

The body of the |err_get_array| is a small generalization over our C
examples: it calls |err_no| on the argument and switch over it. For
each error code, it reads in the provided array, at the corresponding
index. The default case simply consists in reading at latest position
in the array, where ``undefined'' or its variant is written.

> err_get_array :: Integer -> String -> PureExpr -> [(Int, PureExpr)] ->
>                  PureExpr -> FoFCode PureExpr
> err_get_array bitBlock name array codes defaultCode = 
>     def [] ("err_get" ++ name) err_getstring_int (ptrT charT) [(errval_tT, Just "errval")]
>     where err_getstring_int (err : []) =
>               do
>               err_no <- err_noF bitBlock
>               err_no_e <- call err_no [err]
>               switch err_no_e
>                          [ (uint64 $ toInteger code, (return_array array index))
>                            | (code, index) <- codes ]
>                          (return_array array defaultCode)
>               returnc (uint64 0)
>           return_array array index =
>               do
>               x <- readArray array index
>               returnc $ cast (ptrT charT) x

The last function is |err_print_calltrace|. Although longer, this
function does not involve technical aspects of FoF. We can observe the
use of |if| and |while| operators. As well as the definition and usage
of \emph{references}. We also make use of the |printf| operator.

> err_print_calltraceF :: Integer -> Int -> PureExpr -> PureExpr -> PureExpr -> FoFCode PureExpr
> err_print_calltraceF bitBlock indexFailures err_getdomain err_getstring err_getcode = 
>     def [] "err_print_calltrace" err_print_calltrace_int voidT [(errval_tT, Just "errval")]
>         where err_print_calltrace_int (err : []) =
>                   do
>                   err_no <- err_noF bitBlock
>                   err_is_fail <- err_is_failF bitBlock indexFailures
>                      
>                   err_ref <- newRef err
>
>                   ifc (do 
>                        is_fail_e <- call err_is_fail [err]
>                        return is_fail_e)
>                       (do 
>                        x <- newRef $ uint16 0
>                        iterations <- newRef $ uint16 0
>
>                        while (do
>                               err_ref_val <- readRef err_ref
>                               err_ref_no <- call err_no [err_ref_val]
>                               writeRef x err_ref_no
>                               return $ (err_ref_no .!=. (uint16 0)) .&. (iterations .<. ((sizeof(errval_tT) .*. (uint64 8)) ./. (uint64 bitBlock))))
>                              (do     
>                               err_ref_val <- readRef err_ref
>                               err_ref_dom <- call err_getdomain [err_ref_val]
>                               err_ref_str <- call err_getstring [err_ref_val]
>                               err_ref_code <- call err_getcode [err_ref_val]
>                               printf "Failure: (%15s) %20s [%10s]\\n" 
>                                      [err_ref_dom, err_ref_str, err_ref_code]
>                               writeRef iterations (iterations .+. (uint64 1))
>                               writeRef err_ref (err_ref_val .>>. (uint64 bitBlock))))
>                       (do return void)


%% <- Defining data-structures
%%%%%%%%%%%%%%%%%%%%
\subsubsection{Defining Data-Structures}
%%%%%%%%%%%%%%%%%%%%

The following codes involves more Haskell-specific data-processing of
the AST, and less FoF-specific manipulations. However, this
quasi-absence of FoF is also a good sign: FoF succeeds in being
non-intrusive while being able to provide strong guarantees on the
compiler's correctness.

%%%%%%%%
\paragraph{Compiling the Labels}
%%%%%%%%

The first step here is to extract the labels, ie. the error codes and
descriptions, from the Error Definition. Once extracted, we turn them
into FoF strings, thanks to |makeStrings|.

> err_strings :: Errors -> ([String], [String], [String])
> err_strings errors =
>     (domains ++ ["Undefined"], 
>      errcodes ++ ["UNDEFINED"], 
>      errdescs ++ ["Undefined"])
>     where domains = [ domain | ErrorClass domain _ <- errors ] 
>           (errcodes,errdescs) = unzip $ concat [ [ (code, descr)
>                                                    | ErrorField _ code descr  <- fields ] 
>                                                  | ErrorClass _ fields <- errors ]

Using |err_strings|, the following function builds the 3 static arrays
|err_domains|, |err_codes|, and |err_msgs|. Remember that
|err_strings| returns a triple of lists of strings. We instantiate
each of these strings as static arrays, hence obtaining a list of
arrays. Finally, we instantiate these list of arrays as static
arrays. Hence, we obtain 3 static arrays, each containing some static
arrays: the various strings. 

> newStringIndex :: String -> (Int, String) -> 
>                   FoFCode PureExpr
> newStringIndex name (i, x) = newStringN (name ++ show i) x                      

> err_arrays :: Errors -> FoFCode (PureExpr, PureExpr, PureExpr)
> err_arrays errors =
>     do
>       let (domains, errcodes, errdescs) = err_strings errors
>       errdomains_str <- sequence $ map (newStringIndex "errdomains_") $ zip [1..] domains
>       errcodes_str <- sequence $ map (newStringIndex "errcodes_") $ zip [1..] errcodes
>       errdescs_str <- sequence $ map (newStringIndex "errdescs_") $ zip [1..] errdescs
>       errdomains_arr <- newStaticArrayN "err_domains" errdomains_str
>       errcodes_arr <- newStaticArrayN "errcodes" errcodes_str
>       errdescs_arr <- newStaticArrayN "errdescs" errdescs_str
>       return (errdomains_arr, errcodes_arr, errdescs_arr)

At this point, we have compiled the labels.


%%%%%%%%
\paragraph{Compiling the Enumeration}
%%%%%%%%


Before compiling the enumeration, we need to compute two lists of
random, unique, odd and even numbers. This is achieved by the
following function, which implementation is hidden for the sake of
brevity. It takes a random number generator, the number $n$ of desired
integers, and returns a pair composed by $n$ even numbers and $n$
odd numbers.

> mkRandomUnique :: StdGen -> Int -> ([Int], [Int])

%if false 

> mkRandomUnique gen i = mkRandomUniqueInt gen i i [] []
>     where mkRandomUniqueInt gen 0 0 accEven accOdd = (accEven, accOdd)
>           mkRandomUniqueInt gen i j accEven accOdd = 
>               let (rand, gen') = next gen in
>               let rand' = rand `mod` 65536 in
>                   if rand' `mod` 2 == 0 then
>                      if rand' `elem` accEven then
>                         mkRandomUniqueInt gen' i j accEven accOdd
>                      else 
>                          if i == 0 then
>                             mkRandomUniqueInt gen' i j accEven accOdd
>                          else
>                             mkRandomUniqueInt gen' (i-1) j (rand' : accEven) accOdd
>                   else
>                   if rand' `elem` accOdd then
>                      mkRandomUniqueInt gen' i j accEven accOdd
>                   else
>                   if j == 0 then
>                      mkRandomUniqueInt gen' i j accEven accOdd
>                   else
>                   mkRandomUniqueInt gen' i (j-1) accEven (rand' : accOdd)

%endif

Using these lists of even and odd numbers, we assign an identifier to
each error and return this mapping as a list of triples, containing
the domain, the acronym, and the unique identifier. Depending whether
the error indicates a success or a failure, we pick the identifier,
respectively, among the odd or even numbers.

> mkEnum :: Errors -> (Int, Int, [(String, String, Int)])
> mkEnum errors = (succCode',
>                  succCode' + failCode,
>                  reverse $ codes)
>     where errorTypes = concat [ [ (typ, name,dom) 
>                                 | ErrorField typ name _ <- fields ]
>                               | ErrorClass dom fields <- errors ]
>           (codes,
>            succCode', -- note that |succCode'| is used below. Lazyness.
>            failCode) = foldl filterCode ([], 1, 0) errorTypes
>           filterCode (codes, succCode, failCode) 
>                      (typ, name, dom) 
>                      | typ == DefaultSuccess = 
>                          let s = (dom, name, 0) in
>                          (s : codes, succCode, failCode)
>                      | typ == Success = 
>                          let s = (dom, name, succCode) in
>                          (s : codes, succCode + 1, failCode)
>                      | typ == Failure = 
>                          let s = (dom, name, succCode' + failCode) in
>                          (s : codes, succCode, failCode + 1)



%%%%%%%%%%%%%%%%%%%%
\subsubsection{Putting Things Together}
%%%%%%%%%%%%%%%%%%%%


The back-end now consists in a straightforward recollection of the
various, previously developed pieces. Its type is the following:

> backendHeader :: StdGen -> Errors -> FoFCode PureExpr

Meaning that it takes a random number generation, an Error AST and it
generates a Semantics. We can notice which components of FoF are used
here: Enumerations, Assert, References, Functions, Static arrays,
Conditionals, Type definitions, and Printf.

The back-end executes the following actions:

> backendHeader gen errors =
>     do

Compile the type-definitions:

>     alias errval_tT
>
>     aliasE "<stdbool.h>" boolT

Compile the enumeration

>     newEnum "err_code" enumErrCodes ""

Compile the functions:

>     err_no <- err_noF bitBlock
>     err_push <- err_pushF bitBlock
>     err_is_ok <- err_is_okF bitBlock numberOfSuccess
>     err_is_fail <- err_is_failF bitBlock numberOfSuccess

And we are done.

>     return void

%if false

>         where errorCodes = concat [ [ code | ErrorField _ code _ <- fields ]
>                                      | ErrorClass _ fields <- errors ] 
>               noCodes = length errorCodes 
>               (evenNumbers, oddNumbers) = mkRandomUnique gen noCodes
>               (numberOfSuccess, numberOfCodes, labeledErrCodes) = mkEnum errors  
>               enumErrCodes = map (\(_,x1,x2) -> (x1,x2)) labeledErrCodes
>               idErrCodes = map (\(_,_,x) -> x) labeledErrCodes
>               mapIdDescription = zip idErrCodes (map uint64 [1..])
>               mapIdAcronym = mapIdDescription
>               mapIdDomain = zip idErrCodes (map domainIndex labeledErrCodes)
>               domains =  [ dom | ErrorClass dom _ <- errors ]
>               domainIndex (dom,_,_) = uint64 $ toInteger $ fromJust $ dom `elemIndex` [ dom | ErrorClass dom _ <- errors ]
>               noDomains = length domains
>               bitBlock = (floor $ logBase 2 $ (fromInteger . toInteger) numberOfCodes) + 1

%endif

> backendCode :: StdGen -> Errors -> FoFCode PureExpr

> backendCode  gen errors =
>     do

Compute the labels arrays:

>     (err_domains, 
>      err_codes, 
>      err_descs) <- err_arrays errors

Compile the array accessors:

>     err_getstring <- err_get_array bitBlock "string" err_descs mapIdDescription (err_code noCodes)
>     err_getcode <- err_get_array bitBlock "code" err_codes mapIdAcronym (err_code noCodes)
>     err_getdomain <- err_get_array bitBlock "domain" err_domains mapIdDomain (err_code noDomains)

Finally, compile the call-trace printer:

>     err_print_calltrace <- err_print_calltraceF bitBlock numberOfSuccess err_getdomain err_getstring err_getcode

And we are done.

>     return void

%if false

>         where errorCodes = concat [ [ code | ErrorField _ code _ <- fields ]
>                                      | ErrorClass _ fields <- errors ] 
>               noCodes = length errorCodes 
>               (evenNumbers, oddNumbers) = mkRandomUnique gen noCodes
>               (numberOfSuccess, numberOfCodes, labeledErrCodes) = mkEnum errors  
>               enumErrCodes = map (\(_,x1,x2) -> (x1,x2)) labeledErrCodes
>               idErrCodes = map (\(_,_,x) -> x) labeledErrCodes
>               mapIdDescription = zip idErrCodes (map uint64 [0..])
>               mapIdAcronym = mapIdDescription
>               mapIdDomain = zip idErrCodes (map domainIndex labeledErrCodes)
>               domains =  [ dom | ErrorClass dom _ <- errors ]
>               domainIndex (dom,_,_) = uint64 $ toInteger $ fromJust $ dom `elemIndex` [ dom | ErrorClass dom _ <- errors ]
>               noDomains = length domains
>               bitBlock = (floor $ logBase 2 $ (fromInteger . toInteger) numberOfCodes) + 1

%endif

%%%%%%%%%%%%%%%%
\subsubsection{Compiling the Test-Case}
%%%%%%%%%%%%%%%%


At this stage, we just have to open a file, call FoF's |compile|
function on the back-end applied to the test-case, and we are done!

%if false

> {-

%endif

> main :: IO ()
> main = do
>   let gen = mkStdGen 1
>   fileH <- openFile "test_error.h" WriteMode
>   hPutStrLn fileH $ show $ fst $ compile (backendHeader gen testE) emptyBinding
>   hClose fileH

%if false

> -}

%endif

Thanks for your attention!



\end{document}
