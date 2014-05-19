%include polycode.fmt

%if false
  Trace Definitions: DSL for trace definitions (subsystems and events)
   
  Copyright (c) 2013 ETH Zurich.
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

> import Parser

> addInBetween :: String -> [String] -> [String]
> addInBetween _ [] = []
> addInBetween _ (x:[]) = [x]
> addInBetween inBetween (x:y:xs) = [x] ++ [inBetween] ++ addInBetween inBetween (y:xs)

> printEventJSON :: (String, (EventField, Integer)) -> String
> printEventJSON (subsystemName, (EventField name desc, number)) =
>	"\t\t" ++ show number ++ " : \"" ++ displayName ++ "\""
>	where
>		displayName = -- Use desc if it is not the empty string, else use the name used in the #define
>			if length desc == 0
>				then name
>				else desc

> printSubsysJSON :: (SubsystemClass, Integer) -> String
> printSubsysJSON (SubsystemClass name events, number) = 
>	show number ++ " : {\n\t" ++ subsysString ++ ",\n\t\"events\" : {\n" ++ eventStrings ++ "\n\t}\n}"
>	where
> 		subsysString = "\"name\" : \"" ++ name ++ "\""
>		eventStrings = concat (addInBetween ",\n" (map printEventJSON (zip (repeat name) (zip events [0..]))))

As the flounder message send / receive trace events are a bit a hack (they use the event to store
their own payload), they are not part of the pleco file. To still be able to decode them correctly
in Aquarium, we need to add the corresponding information here with the function "addFlounder".

> addFlounder :: [String]
> addFlounder  =
>	[",\n-5632 : {\n\t\"name\" : \"ump send\"\n\t},\n-5376 : {\n\t\"name\" : \"ump receive\"\n\t}"]

> printTraceFileJSON :: [SubsystemClass] -> String
> printTraceFileJSON subsystems =
>	concat ( ["{\n"] ++ (addInBetween ",\n" (map printSubsysJSON (zip subsystems [0..]))) ++ addFlounder ++ ["\n}"] )

> printEvent :: (String, (EventField, Integer)) -> String
> printEvent (subsystemName, (EventField name _, number)) =
>	"#define TRACE_EVENT_" ++ map toUpper subsystemName ++ "_" ++ map toUpper name ++ "\t" ++ show number ++ "\n"

> printSubsys :: (SubsystemClass, Integer) -> String
> printSubsys (SubsystemClass name events, number) = 
>	subsysString ++ eventStrings ++ "\n"
>	where
> 		subsysString = "#define TRACE_SUBSYS_" ++ map toUpper name ++ "\t" ++ show number ++  "\n"
>		eventStrings = concat (map printEvent (zip (repeat name) (zip events [0..])))

> printTraceFileC :: [SubsystemClass] -> String
> printTraceFileC subsystems = 
>	(concat (map printSubsys (zip subsystems [0..]))) ++ "\n\n#define TRACE_NUM_SUBSYSTEMS\t" ++ (show (length subsystems)) ++ "\n"


> main :: IO ()
> main = do
>        argv <- System.Environment.getArgs
>        case argv of
>          [ inF, hdrF, jsonF, codeF ] -> do
>                         input <- Parser.parse inF
>                         case input of
>                           Left err -> do
>                                       hPutStrLn stderr "parse error at: "
>                                       hPutStrLn stderr (show err) 
>                                       exitWith (ExitFailure 1)
>                           Right ast -> do
>                                        let macro = map toUpper (takeBaseName hdrF) ++ "_BARRELFISH__"
>                                        let header = printTraceFileC ast
>                                        fileH <- openFile hdrF WriteMode
>                                        fileC <- openFile codeF WriteMode
>                                        fileJ <- openFile jsonF WriteMode
>                                        let pre = "#ifndef " ++ macro ++ "\n" ++
>                                                  "#define " ++ macro ++ "\n\n"
>                                        let post = "\n#endif // " ++ macro
>                                        hPutStr fileH pre
>                                        hPutStr fileH header
>                                        hPutStrLn fileH post
>                                        hClose fileH
>                                        hPutStr fileJ (printTraceFileJSON ast)
>                                        hClose fileJ
>                                        hClose fileC

>          otherwise -> do
>                  hPutStrLn stderr "Usage: pleco input.pleco output.h output.json output.c"
>                  exitWith (ExitFailure 1)
