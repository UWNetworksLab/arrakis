{- 
   Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007-2011, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}

module Main where
 
import System.IO
import System.IO.Error
import System.Console.GetOpt
import System.FilePath
import System.Exit
import System.Environment
import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec as Parsec
import Text.Printf
import qualified MackerelParser
import qualified BitFieldDriver
import qualified ShiftDriver
import Checks
import Dev
import Control.Exception

--
-- Command line options and parsing code
--

-- Datatypes for carrying command options around
data Target = BitFieldDriver | ShiftDriver | NullDriver deriving (Eq, Show)

data Options = Options {
  opt_infilename :: Maybe String,
  opt_outfilename :: Maybe String,
  opt_includedirs :: [String],
  opt_target :: Target,
  opt_usage_error :: Bool,
  opt_verbosity :: Integer
  } deriving (Show,Eq)

defaultOptions :: Options
defaultOptions = Options { 
  opt_infilename = Nothing,
  opt_outfilename = Nothing,
  opt_includedirs = [],
  opt_target = ShiftDriver,
  opt_usage_error = False,
  opt_verbosity = 1 }

-- For driving System.GetOpt
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c'] ["input-file"] 
    (ReqArg (\f opts -> opts { opt_infilename = Just f } ) "file")
    "input file"
  , Option ['I'] ["include-dir"]
    (ReqArg (\ d opts -> opts { opt_includedirs = opt_includedirs opts ++ [d] }) "dir")
    "include directory (can be given multiple times)"
  , Option ['v'] ["verbose"]
    (NoArg (\ opts -> opts { opt_verbosity = opt_verbosity opts + 1 } ))
    "increase verbosity level"
  , Option ['o'] ["output"]
    (ReqArg (\ f opts -> opts { opt_outfilename = Just f }) "file")
     "output file name"
  , Option ['S'] ["shift-driver"]
    (NoArg (\ opts -> opts { opt_target = ShiftDriver } ))
     "use shift driver (default; preferred)"
  , Option ['n'] ["null-driver"]
    (NoArg (\ opts -> opts { opt_target = NullDriver } ))
     "use null output driver (don't generate any C)"
  , Option ['B'] ["bitfield-driver"]
    (NoArg (\ opts -> opts { opt_target = BitFieldDriver } ))
     "use bitfield driver (deprecrated: do not use)"
  ]

--
-- Set the correct default input and output files
--

defaultOutput :: Options -> Options
defaultOutput opts = 
  case opt_outfilename opts of
    (Just _) -> 
      opts
    Nothing  -> 
      opts { opt_outfilename = 
                Just (case opt_infilename opts of
                         (Just i) -> 
                           ((dropExtension $ takeFileName i) ++ "_dev.h")
                         Nothing -> "mackerel_output.h" 
                     )
           }

defaultInput :: Options -> [String] -> IO (Options)
defaultInput opts [f] = 
  if isNothing $ opt_infilename opts
  then return (defaultOutput (opts { opt_infilename = Just f }))
  else usageError []
defaultInput opts [] = 
  if (isJust $ opt_infilename opts)
  then return (defaultOutput opts)
  else usageError []
defaultInput _ _ = usageError []

compilerOpts :: [String] -> IO (Options)
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[])   -> defaultInput (foldl (flip id) defaultOptions o) n
    (_,_,errs) -> usageError errs

usageError :: [String] -> IO (Options)
usageError errs = 
  ioError (userError (concat errs ++ usageInfo usage options))
  where usage = "Usage: mackerel <options> <input file>" 

--
-- Processing source files
---

-- Null compilation 
nullCompiler :: String -> String -> Dev.Rec -> String
nullCompiler _ _ _ = ""

-- Perform run-time checks
run_checks :: String -> Dev.Rec -> IO String
run_checks input_fn dev =
    case (Checks.check_all input_fn dev) of
      Just errors ->
          do { (hPutStrLn stderr (unlines [ e ++ "\n"  | e <-errors]))
             ; System.Exit.exitWith (ExitFailure 1)
             }
      Nothing -> do { return "" }

-- Parsing the input file into an AST
parseFile :: String -> IO MackerelParser.DeviceFile
parseFile fname = do
    src <- readFile fname
    case (runParser MackerelParser.devfile () fname src) of
        Left err -> ioError $ userError ("Parse error at: " ++ (show err))
        Right x -> return x

-- Traverse the include path to find an import file
findImport :: [String] -> String -> IO MackerelParser.DeviceFile
findImport [] f = 
  ioError (userError $ printf "Can't find import '%s'" f)
findImport (d:t) f = 
  do 
    catch (parseFile (d </> f))
      (\e -> (if isDoesNotExistError e then findImport t f else ioError e))

-- Perform the transitive closure of all the imports

resolveImports :: [MackerelParser.DeviceFile] -> [String] 
                  -> IO [MackerelParser.DeviceFile]
resolveImports dfl path = 
  let allimports = nub $ concat [ il | (MackerelParser.DeviceFile _ il) <- dfl ]
      gotimports = [ n | (MackerelParser.DeviceFile (MackerelParser.Device n _ _ _ _) _) <- dfl ]
      required = allimports \\ gotimports
  in
   case required of
     [] -> return dfl
     (t:_) -> do { i <- (findImport path (t ++ ".dev"))
                 ; resolveImports (dfl ++ [i]) path
                 }

testentry :: IO ()
testentry = 
  let input_fn = "../../devices/xapic.dev"
      output_fn = "x2apic.dev.h"
      includedirs = ["../../devices"]
  in
   do { hPutStrLn stdout ("IN: " ++ input_fn)
      ; hPutStrLn stdout ("OUT: " ++ output_fn)
      ; df  <- parseFile input_fn
      ; dfl <- resolveImports [df] includedirs
      ; let dev = make_dev df (tail dfl) in
      do { _ <- run_checks input_fn dev
         ; outFileD <- openFile output_fn WriteMode
         ; hPutStr outFileD (ShiftDriver.compile input_fn output_fn dev)
         ; hClose outFileD 
         }
      }
  

-- Main entry point of Mackernel 
main :: IO ()
main = do { cli <- System.Environment.getArgs
          ; opts <- compilerOpts cli
          ; let input_fn = fromJust $ opt_infilename opts
                output_fn = fromJust $ opt_outfilename opts
            in 
             do { df  <- parseFile input_fn
                ; dfl <- resolveImports [df] (opt_includedirs opts)
                ; let dev = make_dev df (tail dfl) in
                do { _ <- run_checks input_fn dev
                   ; outFileD <- openFile output_fn WriteMode
                   ; hPutStr outFileD ((case (opt_target opts) of
                                           NullDriver -> nullCompiler 
                                           ShiftDriver -> ShiftDriver.compile 
                                           BitFieldDriver -> BitFieldDriver.compile)
                                       input_fn output_fn dev)
                   ; hClose outFileD          
                   }
                }
          }
