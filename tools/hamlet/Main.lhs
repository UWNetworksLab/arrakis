%if false
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

> {-# LANGUAGE BangPatterns #-}


> module Main where

> import System.Environment
> import System.Exit
> import System.Console.GetOpt
> import System.IO
> import System.FilePath.Posix

> import qualified Data.Map as Map

> import Debug.Trace

> import HamletBackend hiding (strict)
> import HamletAst hiding (vcat')
> import Parser

> import Expressions
> import Compile
> import PureExpressions
> import Constructs.Enumerations
> import IL.Paka.Paka
> import IL.Paka.Syntax
> import IL.Paka.Compile 

> main :: IO ()
> main =
>    do
>    argv <- System.Environment.getArgs
>    case argv of
>      [inputFile, filenameDefs, filenameCode, filenameUserCode] -> do
>              input <- parseCaps inputFile
>              case input of
>                Left err ->
>                    do
>                    hPutStrLn stderr "parse error at: "
>                    hPutStrLn stderr (show err) 
>                    exitWith (ExitFailure 1)
>                Right ast ->
>                    do  

>                    let compiledCode = (compile $! (backend $! ast))
>                    fileDefs <- openFile filenameDefs WriteMode
>                    hPutStrLn fileDefs "#ifndef CAPBITS_H"
>                    hPutStrLn fileDefs "#define CAPBITS_H"
>                    hPutStrLn fileDefs "#include <barrelfish_kpi/capabilities.h>"
>                    hPutStrLn fileDefs $! show $ vcat' $ extractM $ types compiledCode
>                    hPutStrLn fileDefs $! show $ vcat' $ extractL $ declarations compiledCode
>                    hPutStrLn fileDefs "#endif // CAPBITS_H"
>                    hClose fileDefs
>
>                    fileC <- openFile filenameCode WriteMode
>                    hPutStrLn fileC "#include <kernel.h>"
>                    hPutStrLn fileC "#include <capabilities.h>"
>                    hPutStrLn fileC "#include <cap_predicates.h>"
>                    hPutStrLn fileC "#include <offsets.h>"
>                    hPutStrLn fileC $! show $ compiledCode {declarations = [], 
>                                                            types = Map.empty,
>                                                            prototypes = Map.empty}
>                    hClose fileC
>
>                    let compiledCode = (compile $! (userbackend $! ast))
>                    fileC <- openFile filenameUserCode WriteMode
>                    hPutStrLn fileC "#include <barrelfish/barrelfish.h>"
>                    hPutStrLn fileC "#include <barrelfish_kpi/capbits.h>"
>                    hPutStrLn fileC "#include <barrelfish/cap_predicates.h>"
>                    hPutStrLn fileC $! show $ compiledCode {declarations = [], 
>                                                            types = Map.empty,
>                                                            prototypes = Map.empty}
>                    hClose fileC
>
>      _ -> do
>            hPutStrLn stderr "Usage: hamlet INPUT_CAPDEFS.hl OUTPUT_DEFS.h OUTPUT_CODE.c OUTPUT_USERCODE.c"
>            exitWith (ExitFailure 1)

