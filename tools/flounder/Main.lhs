%include polycode.fmt

%if false
  Flounder2: an even more simpler IDL for Barrelfish
   
  Copyright (c) 2009, 2010 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif



> module Main where

> import System.Environment
> import System.Exit
> import System.Console.GetOpt
> import System.IO
> import System.FilePath.Posix
> import Data.Maybe
> import Control.Monad

> import Text.ParserCombinators.Parsec as Parsec
> import qualified Parser
> import qualified Syntax            
> import qualified Arch
> import qualified Backend
> import qualified GHBackend
> import qualified GCBackend
> import qualified LMP
> import qualified UMP
> import qualified UMP_IPI
> import qualified Multihop
> import qualified Loopback
> import qualified RPCClient
> import qualified MsgBuf
> import qualified THCBackend
> import qualified THCStubsBackend
> import qualified AHCI

> data Target = GenericHeader
>            | GenericCode
>            | LMP_Header
>            | LMP_Stub
>            | UMP_Header
>            | UMP_Stub
>            | UMP_IPI_Header
>            | UMP_IPI_Stub
>  	     | Multihop_Stub
>            | Multihop_Header
>            | Loopback_Header
>            | Loopback_Stub
>            | RPCClient_Header
>            | RPCClient_Stub
>            | MsgBuf_Header
>            | MsgBuf_Stub
>            | THCHeader
>            | THCStubs
>            | AHCI_Header
>            | AHCI_Stub
>            deriving (Show)

> data Options = Options {
>     optTargets :: [Target],
>     optArch :: Maybe Arch.Arch,
>     optIncludes :: [String]
> }

> defaultOptions = Options { optTargets = [], optArch = Nothing, optIncludes = [] }

> generator :: Options -> Target -> String -> String -> Syntax.Interface -> String
> generator _ GenericHeader = GHBackend.compile
> generator _ GenericCode = GCBackend.compile
> generator _ LMP_Header = LMP.header
> generator opts LMP_Stub
>     | isNothing arch = error "no architecture specified for LMP stubs"
>     | otherwise = LMP.stub (fromJust arch)
>     where arch = optArch opts
> generator _ UMP_Header = UMP.header
> generator opts UMP_Stub
>     | isNothing arch = error "no architecture specified for UMP stubs"
>     | otherwise = UMP.stub (fromJust arch)
>     where arch = optArch opts
> generator _ UMP_IPI_Header = UMP_IPI.header
> generator opts UMP_IPI_Stub
>     | isNothing arch = error "no architecture specified for UMP_IPI stubs"
>     | otherwise = UMP_IPI.stub (fromJust arch)
>     where arch = optArch opts
> generator _ Multihop_Header = Multihop.header
> generator opts Multihop_Stub
>     | isNothing arch = error "no architecture specified for Multihop stubs"
>     | otherwise = Multihop.stub (fromJust arch)
>     where arch = optArch opts
> generator _ Loopback_Header = Loopback.header
> generator _ Loopback_Stub = Loopback.stub
> generator _ RPCClient_Header = RPCClient.header
> generator _ RPCClient_Stub = RPCClient.stub
> generator _ MsgBuf_Header = MsgBuf.header
> generator _ MsgBuf_Stub = MsgBuf.stub
> generator _ THCHeader = THCBackend.compile
> generator _ THCStubs = THCStubsBackend.compile
> generator _ AHCI_Header = AHCI.header
> generator _ AHCI_Stub = AHCI.stub

> addTarget :: Target -> Options -> IO Options
> addTarget t o = return o { optTargets = (optTargets o) ++ [t] }

> setArch :: String -> Options -> IO Options
> setArch s o = case optArch o of
>     Nothing -> if isJust arch then return o { optArch = arch }
>                else ioError $ userError $ "unknown architecture '" ++ s ++ "'"
>     Just _  -> ioError $ userError "multiple architectures are not supported"
>     where
>         arch = Arch.parse_arch s

> addInclude :: String -> Options -> IO Options
> addInclude s o = return o { optIncludes = (optIncludes o) ++ [s] }

> options :: [OptDescr (Options -> IO Options)]
> options = [ 
>             Option ['G'] ["generic-header"] (NoArg $ addTarget GenericHeader) "Create a generic header file",
>             Option [] ["generic-stub"] (NoArg $ addTarget GenericCode) "Create generic part of stub implemention",
>             Option ['a'] ["arch"] (ReqArg setArch "ARCH")           "Architecture for stubs",
>             Option ['i'] ["import"] (ReqArg addInclude "FILE")      "Include a given file before processing",
>             Option [] ["lmp-header"] (NoArg $ addTarget LMP_Header) "Create a header file for LMP",
>             Option [] ["lmp-stub"] (NoArg $ addTarget LMP_Stub)     "Create a stub file for LMP",
>             Option [] ["ump-header"] (NoArg $ addTarget UMP_Header) "Create a header file for UMP",
>             Option [] ["ump-stub"] (NoArg $ addTarget UMP_Stub)     "Create a stub file for UMP",
>             Option [] ["ump_ipi-header"] (NoArg $ addTarget UMP_IPI_Header) "Create a header file for UMP_IPI",
>             Option [] ["ump_ipi-stub"] (NoArg $ addTarget UMP_IPI_Stub)     "Create a stub file for UMP_IPI",
>             Option [] ["multihop-header"] (NoArg $ addTarget Multihop_Header) "Create a header file for Multihop",
>             Option [] ["multihop-stub"] (NoArg $ addTarget Multihop_Stub)     "Create a stub file for Multihop",
>             Option [] ["loopback-header"] (NoArg $ addTarget Loopback_Header) "Create a header file for loopback",
>             Option [] ["loopback-stub"] (NoArg $ addTarget Loopback_Stub) "Create a stub file for loopback",
>             Option [] ["rpcclient-header"] (NoArg $ addTarget RPCClient_Header) "Create a header file for RPC",
>             Option [] ["rpcclient-stub"] (NoArg $ addTarget RPCClient_Stub) "Create a stub file for RPC",
>             Option [] ["msgbuf-header"] (NoArg $ addTarget MsgBuf_Header) "Create a header file for message buffers",
>             Option [] ["msgbuf-stub"] (NoArg $ addTarget MsgBuf_Stub) "Create a stub file for message buffers",

>             Option ['T'] ["thc-header"] (NoArg $ addTarget THCHeader)             "Create a THC header file",
>             Option ['B'] ["thc-stubs"] (NoArg $ addTarget THCStubs)               "Create a THC stubs C file",
>             Option [] ["ahci-header"] (NoArg $ addTarget AHCI_Header) "Create a header file for AHCI",
>             Option [] ["ahci-stub"] (NoArg $ addTarget AHCI_Stub)     "Create a stub file for AHCI" ]

> compile :: Options -> Target -> Syntax.Interface -> String -> String -> Handle -> IO ()
> compile opts fl ast infile outfile outfiled =
>     hPutStr outfiled $ (generator opts fl) infile outfile ast

> parseFile :: (String -> IO (Either Parsec.ParseError a)) -> String -> IO a
> parseFile parsefn fname = do
>    input <- parsefn fname
>    case input of
>        Left err -> do
>            hPutStrLn stderr $ "Parse error at: " ++ (show err)
>            exitWith $ ExitFailure 1
>        Right x -> return x

> parseIncludes :: Options -> IO [(String, Syntax.Declaration)]
> parseIncludes opts
>     = foldM (\d -> parseFile $ Parser.parse_include d) [] (optIncludes opts)

> checkFilename :: Syntax.Interface -> String -> IO ()
> checkFilename interface fname = do
>                                 let Syntax.Interface ifacename _ _ = interface
>                                 if ifacename == takeBaseName fname then return () else ioError $ userError ("Interface name '" ++ ifacename ++ "' has to equal filename in " ++ fname)

> main :: IO ()
> main = do 
>        argv <- System.Environment.getArgs
>        case getOpt RequireOrder options argv of
>          (optf, [ inFile, outFile ], []) -> do
>              opts <- foldM (flip id) defaultOptions optf
>              includeDecls <- parseIncludes opts
>              ast <- parseFile (Parser.parse_intf includeDecls) inFile
>              outFileD <- openFile outFile WriteMode
>              checkFilename ast inFile
>              sequence_ $ map (\target
>                               -> compile opts target ast inFile outFile outFileD)
>                            (optTargets opts)
>              hClose outFileD
>          (_, _, errors) -> do
>              hPutStr stderr (concat errors ++ usageInfo usage options)
>              exitWith (ExitFailure 1)
>       where
>           usage = "Usage: flounder [OPTION...] input.if output"
