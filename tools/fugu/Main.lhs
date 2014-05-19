%include polycode.fmt

%if false
  Error: DSL for error definition
   
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

> module Main where

> import Text.PrettyPrint.HughesPJ as Pprinter 

> import System.Environment
> import System.Exit
> import System.Console.GetOpt
> import System.IO
> import System.Random
> import System.FilePath.Posix

> import Data.Char
> import qualified Data.Map as Map

> import Expressions
> import Compile
> import IL.Paka.Syntax
> import IL.Paka.Compile

> import FuguBackend
> import Parser


> main :: IO ()
> main = do
>        argv <- System.Environment.getArgs
>        case argv of
>          [ inF, hdrF, codeF ] -> do
>                         input <- Parser.parse inF
>                         case input of
>                           Left err -> do
>                                       hPutStrLn stderr "parse error at: "
>                                       hPutStrLn stderr (show err) 
>                                       exitWith (ExitFailure 1)
>                           Right ast -> do
>                                        let macro = map toUpper (takeBaseName hdrF) ++ "_BARRELFISH__"
>                                        let gen = mkStdGen 1 
>                                        let code = compile (backendCode gen ast)
>                                        let header_ = compile (backendHeader gen ast)
>                                        let header = header_ { prototypes = prototypes code 
>                                                                            `Map.union` 
>                                                                            prototypes header_ }
>                                        fileH <- openFile hdrF WriteMode
>                                        fileC <- openFile codeF WriteMode
>                                        let pre = "#ifndef " ++ macro ++ "\n" ++
>                                                  "#define " ++ macro ++ "\n"
>                                        let post = "\n#endif // " ++ macro
>                                        hPutStr fileH pre
>                                        hPutStr fileH $ show $ header
>                                        hPutStrLn fileH post
>                                        hClose fileH

>                                        hPutStr fileC "#include <errors/errno.h>\n"
>                                        hPutStr fileC "#include <assert.h>\n"
>                                        hPutStrLn fileC $ show $ code { prototypes= Map.empty,
>                                                                      functions = Map.filterWithKey (\n _ -> n /= "err_no" && 
>                                                                                                         n /= "err_is_fail") $ 
>                                                                                                         functions code }
>                                        hClose fileC
>          otherwise -> do
>                  hPutStrLn stderr "Usage: fugu input.fugu output.h output.c"
>                  exitWith (ExitFailure 1)
