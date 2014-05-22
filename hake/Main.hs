{- 
  Hake: a meta build system for Barrelfish

  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}
  


module Main where

import System.Environment
import System.IO
import System.Directory
import System.Exit
import GHC hiding (Target)
import GHC.Paths ( libdir )
import DynFlags ( defaultFlushOut, defaultFatalMessager,
                  xopt_set,
                  ExtensionFlag (Opt_DeriveDataTypeable) )
import Data.Dynamic
import Data.Maybe
import Data.List
import Control.Monad
import Control.Parallel.Strategies

import RuleDefs
import HakeTypes
import qualified Path
import qualified Args
import qualified Config

--
-- Command line options and parsing code
--
data Opts = Opts { opt_makefilename :: String,
                   opt_installdir :: String,
                   opt_sourcedir :: String,
                   opt_bfsourcedir :: String,
                   opt_usage_error :: Bool,
                   opt_architectures :: [String],
                   opt_verbosity :: Integer
                 }
          deriving (Show,Eq)
                   
parse_arguments :: [String] -> Opts
parse_arguments [] =
  Opts { opt_makefilename = "Makefile",
         opt_installdir = Config.install_dir,
         opt_sourcedir = Config.source_dir,
         opt_bfsourcedir = Config.source_dir,
         opt_usage_error = False, 
         opt_architectures = [],
         opt_verbosity = 1 }
parse_arguments ("--install-dir" : (s : t)) =
  (parse_arguments t) { opt_installdir = s }
parse_arguments ("--source-dir" : s : t) =  
  (parse_arguments t) { opt_sourcedir = s }
parse_arguments ("--bfsource-dir" : s : t) =  
  (parse_arguments t) { opt_bfsourcedir = s }
parse_arguments ("--output-filename" : s : t) =
  (parse_arguments t) { opt_makefilename = s }
parse_arguments ("--quiet" : t ) = 
  (parse_arguments t) { opt_verbosity = 0 }
parse_arguments ("--verbose" : t ) = 
  (parse_arguments t) { opt_verbosity = 2 }
parse_arguments ("--architecture" : a : t ) = 
  let 
    o2 = parse_arguments t
    arches = (a : opt_architectures o2)
  in
    o2 { opt_architectures = arches }
parse_arguments _ = 
  (parse_arguments []) { opt_usage_error = True }

usage :: String
usage = unlines [ "Usage: hake <options>",
                  "   --source-dir <dir> (required)",
                  "   --bfsource-dir <dir> (defaults to source dir)",
                  "   --install-dir <dir> (defaults to source dir)",
                  "   --quiet",
                  "   --verbose"
                ]

--
-- Handy path operator
--
infix 4 ./.
root ./. path = Path.relToDir path root


--
-- Walk all over a directory tree and build a complete list of pathnames
--
listFilesR :: FilePath -> IO [FilePath]
listFilesR path = let
    isDODD :: String -> Bool
    isDODD f = not $ (isSuffixOf "/." f) 
                  || (isSuffixOf "/.." f) 
                  || (isSuffixOf "CMakeFiles" f)
                  || (isPrefixOf (path ++ "/.hg") f)
                  || (isPrefixOf (path ++ "/build") f)
                  || (isPrefixOf (path ++ "/.git") f)

    listDirs :: [FilePath] -> IO [FilePath]
    listDirs = filterM doesDirectoryExist 

    listFiles :: [FilePath] -> IO [FilePath]
    listFiles = filterM doesFileExist

    joinFN :: String -> String -> FilePath
    -- joinFN p1 p2 = joinPath [p1, p2]
    joinFN p1 p2 =  p1 ++ "/" ++ p2

    in do
        allfiles <- getDirectoryContents path
        no_dots <- filterM (return . isDODD) (map (joinFN path) allfiles)
        dirs <- listDirs no_dots
        subdirfiles <- (mapM listFilesR dirs >>= return . concat)
        files <- listFiles no_dots
        return $ files ++ subdirfiles

--
-- Return a list of pairs of (Hakefile name, contents)
-- 
readHakeFiles :: [FilePath] -> IO [ (String,String) ]
readHakeFiles [] = return []
readHakeFiles (h:hs) = do { r <- readFile h; 
                            rs <- readHakeFiles hs;
                            return ((h,r):rs)
                          }
--
-- Look for Hakefiles in a list of path names
--
hakeFiles :: [FilePath] -> [String]
hakeFiles f = [ fp | fp <- f, isSuffixOf "/Hakefile" fp ]

---
--- Functions to resolve relative path names in rules. 
---
--- First, the outer function: resolve path names in an HRule. The
--- third argument, 'root', is frequently the pathname of the Hakefile
--- relative to the source tree - since relative pathnames in
--- Hakefiles are interpreted relative to the Hakefile location.
---
resolveRelativePaths :: Opts -> HRule -> String -> HRule
resolveRelativePaths o (Rules hrules) root 
    = Rules [ resolveRelativePaths o r root | r <- hrules ]
resolveRelativePaths o (Rule tokens) root
    = Rule [ resolveRelativePath o t root | t <- tokens ]
resolveRelativePaths o (Include token) root
    = Include ( resolveRelativePath o token root )
resolveRelativePaths o (Error s) root 
    = (Error s)


--- Now resolve at the level of individual rule tokens.  At this
--- level, we need to take into account the tree (source, build, or
--- install).
resolveRelativePath :: Opts -> RuleToken -> String -> RuleToken
resolveRelativePath o (In t a f) root = 
    (In t a (resolveRelativePathName o t a f root))
resolveRelativePath o (Out a f) root = 
    (Out a (resolveRelativePathName o BuildTree a f root))
resolveRelativePath o (Dep t a f) root = 
    (Dep t a (resolveRelativePathName o t a f root))
resolveRelativePath o (NoDep t a f) root = 
    (NoDep t a (resolveRelativePathName o t a f root))
resolveRelativePath o (PreDep t a f) root = 
    (PreDep t a (resolveRelativePathName o t a f root))
resolveRelativePath o (Target a f) root = 
    (Target a (resolveRelativePathName o BuildTree a f root))
resolveRelativePath _ (Str s) _ = (Str s)
resolveRelativePath _ (NStr s) _ = (NStr s)
resolveRelativePath _ (ContStr b s1 s2) _ = (ContStr b s1 s2)
resolveRelativePath _ (ErrorMsg s) _ = (ErrorMsg s)
resolveRelativePath _ NL _ = NL

--- Now we get down to the nitty gritty.  We have a tree (source,
--- build, or install), an architecture (e.g. ARM), a pathname, and
--- the pathname of the Hakefile in which it occurs.

resolveRelativePathName :: Opts -> TreeRef -> String -> String -> String -> String
resolveRelativePathName o InstallTree "root" f root = 
    resolveRelativePathName' ((opt_installdir o)) "root" f root
resolveRelativePathName o _ "root" f root = 
    "." ./. f
resolveRelativePathName o SrcTree a f root =
    resolveRelativePathName' (opt_sourcedir o) a f root
resolveRelativePathName o BuildTree a f root =
    resolveRelativePathName' ("." ./. a) a f root
resolveRelativePathName o InstallTree a f root =
    resolveRelativePathName' ((opt_installdir o) ./. a) a f root

--- This is where the work is done: take 'root' (pathname relative to
--- us of the Hakefile) and resolve the filename we're interested in
--- relative to this.  This gives us a pathname relative to some root
--- of some architecture tree, then return this relative to the actual
--- tree we're interested in.  It's troubling that this takes more
--- bytes to explain than to code.
resolveRelativePathName' d a f root = 
    let af = Path.relToFile f root
        rf = Path.makeRel $ Path.relToDir af "/" 
    in Path.relToDir rf d

--
-- Generating a list of build directories
--
makeDirectories :: [(String, HRule)] -> String
makeDirectories r = 
    let alldirs = makeDirs1 (Rules [ rl | (f,rl) <- r ]) `using` parListChunk 200 rdeepseq
        marker d = d ./. ".marker"
    in unlines ([ "# Directories follow" ] ++
                [ "hake_dirs: " ++ (marker d) ++ "\n\n" ++
                  (marker d) ++ ": \n" ++
                  "\tmkdir -p " ++ d ++ "\n" ++
                  "\ttouch " ++ (marker d) ++ "\n"
                | d <- nub alldirs])

makeDirs1 :: HRule -> [String]
makeDirs1 (Rules hrules) = concat [ makeDirs1 r | r <- hrules]
makeDirs1 (Include tok) = 
    case tokDir tok of
      Nothing -> []
      Just d -> [d]
makeDirs1 (Rule toks) = [d | Just d <- [ tokDir t | t <- toks ]]
makeDirs1 (Error s) = []

tokDir :: RuleToken -> Maybe String
tokDir (In t a f) = tokDir1 f
tokDir (Out a f) =  tokDir1 f
tokDir (Dep t a f) =  tokDir1 f
tokDir (NoDep t a f) =  tokDir1 f
tokDir (PreDep t a f) =  tokDir1 f
tokDir (Target a f) =  tokDir1 f
tokDir (Str s) = Nothing
tokDir (NStr s) = Nothing
tokDir (ContStr b s1 s2) = Nothing
tokDir (ErrorMsg s) = Nothing
tokDir NL = Nothing

tokDir1 f 
    | (Path.dirname f) `Path.isBelow` "." = Just (Path.dirname f)
    | otherwise = Nothing

--
-- filter rules by the set of architectures in Config.architectures
--
filterRuleByArch :: HRule -> Maybe HRule
filterRuleByArch (Rule toks) = if allowedArchs (map frArch toks) then Just (Rule toks) else Nothing
filterRuleByArch (Include tok) = if allowedArchs [frArch tok] then Just (Include tok) else Nothing
filterRuleByArch (Rules rules) = Just (Rules (catMaybes $ map filterRuleByArch rules))
filterRuleByArch x = Just x

-- a rule is included if it has only "special" architectures and enabled architectures
allowedArchs :: [String] -> Bool
allowedArchs = all (\a -> a `elem` (Config.architectures ++ specialArchitectures))
    where specialArchitectures = ["", "src", "hake", "root", "tools", "docs"]

-- 
-- Functions to format rules as Makefile rules
--
makeMakefile :: [(String, HRule)] -> String
makeMakefile r = 
  unlines $ intersperse "" ([makeMakefileSection f rl | (f,rl) <- r] `using` parList rdeepseq)

makeMakefileSection :: String -> HRule -> String
makeMakefileSection fname rules = 
    "# From: " ++ fname ++ "\n\n" ++ makeMakeRules rules

makeMakeRules :: HRule -> String
makeMakeRules (Rules hrules)
    = unlines [ s | s <- [ makeMakeRules h | h <- hrules ], s /= "" ]
makeMakeRules (Include token) = unlines [
    "ifeq ($(MAKECMDGOALS),clean)",
    "else ifeq ($(MAKECMDGOALS),rehake)",
    "else ifeq ($(MAKECMDGOALS),Makefile)",
    "else",
    "include " ++ (formatToken token),
    "endif"]
makeMakeRules (Error s) = "$(error " ++ s ++ ")\n"
makeMakeRules (Rule tokens) = 
    let outs = nub [ f | (Out a f) <- tokens ] ++ [ f | (Target a f) <- tokens ]
        dirs = nub [ (Path.dirname f) ./. ".marker" | f <- outs ]
        deps = nub [ f | (In t a f) <- tokens ] ++ [ f | (Dep t a f) <- tokens ] 
        predeps = nub [ f | (PreDep t a f) <- tokens ] 
        spaceSep :: [ String ] -> String
        spaceSep sl = concat (intersperse " " sl)
        ruleBody = (concat[ formatToken t | t <- tokens, inRule t ])
    in if outs == [] then
      ("# hake: omitted rule with no output: " ++ ruleBody)
    else
      (spaceSep outs) ++ ": " 
      ++ 
      -- It turns out that if you add 'dirs' here, in an attempt to
      -- get Make to build the directories as well, it goes a bit
      -- pear-shaped: whenever the directory "changes" it goes out of
      -- date, so you end up rebuilding dependencies every time.
      (spaceSep (deps ++ dirs)) 
      ++ 
      (if (predeps == []) then "" else " | " ++ spaceSep (predeps))
      ++ "\n" 
      ++
      (if (ruleBody == "") then "" else "\t" ++ ruleBody ++ "\n")
      

preamble :: Opts -> [String] -> String
preamble opts args = 
    unlines ( [ "# This Makefile is generated by Hake.  Do not edit!",
                "# ",
                "# Hake was invoked with the following command line args:" ] ++
              [ "#        " ++ a | a <- args ] ++
              [ "# ",
                "SRCDIR=" ++ (opt_sourcedir opts),
                "HAKE_ARCHS=" ++ (concat $ intersperse " " Config.architectures),
                "include ./symbolic_targets.mk" ] )

stripSrcDir :: String -> String
stripSrcDir s = Path.removePrefix Config.source_dir s

hakeModule :: [String] -> [(String,String)] -> String
hakeModule allfiles hakefiles = 
    let unqual_imports = ["RuleDefs", "HakeTypes", "Path", "Args"]
        qual_imports = ["Config", "Data.List" ]
        relfiles = [ stripSrcDir f | f <- allfiles ]
        wrap1 n c = wrapLet "build a" 
                    ("(buildFunction a) allfiles " ++ (show n) ++ " a")
                    c
        wrap n c = "(" ++ (show n) ++ ", " 
                   ++ wrapLet "find fn arg" 
                          ("(fn allfiles " ++ (show n) ++ " arg)") 
                          ("Rules (" ++ (wrap1 n c) ++ ")")
                   ++ ")"
        flatten :: [String] -> String
        flatten s = foldl (++) "" (intersperse ",\n" s)
        addHeader (fn,fc) = (fn, "{-# LINE 1 \"" ++ fn ++ "\" #-}\n" ++ fc)
        files = flatten [ wrap (stripSrcDir fn) fc | (fn,fc) <- map addHeader hakefiles ]
    in
      unlines ( [ "module Hakefiles where {" ]
                ++
                [ "import " ++ i ++ ";" | i <- unqual_imports ]
                ++
                [ "import qualified " ++ i ++ ";" | i <- qual_imports ] 
                ++
                [ "allfiles = " ++ (show relfiles) ++ ";" ]
                ++ 
                [ "hf = [" ] 
              ) ++ files ++ "];\n}"

wrapLet :: String -> String -> String -> String
wrapLet var expr body = 
    "(let " ++ var ++ " = " ++ expr ++ " in\n" ++ body ++ ")"

evalHakeFiles :: Opts -> [String] -> [(String,String)] 
              -> IO [(String,HRule)]
evalHakeFiles o allfiles hakefiles = 
    let imports = [ "Hakefiles"]
        all_imports = ("Prelude":"HakeTypes":imports)
        moddirs = [ (opt_installdir o) ./. "hake", 
                    ".", 
                    (opt_bfsourcedir o) ./. "hake" ]
    in do 
      defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
         runGhc (Just libdir) $ do
           dflags <- getSessionDynFlags
	   let dflags1 = foldl xopt_set dflags [ Opt_DeriveDataTypeable ]
	   _ <- setSessionDynFlags dflags1{
		importPaths = moddirs,
                hiDir = Just "./hake",
                objectDir = Just "./hake"
	   }
           targets <- mapM (\m -> guessTarget m Nothing) imports
           setTargets targets
           load LoadAllTargets
           setContext [(IIDecl . simpleImportDecl) (mkModuleName m) | m <- (all_imports)]
           val <- dynCompileExpr "Hakefiles.hf :: [(String, HRule)]" 
           return (fromDyn val [("failed",Error "failed")])

--
-- Generate dependencies of the Makefile on all the Hakefiles
--
--resolveRelativePaths o (Rules hrules) root 
--- resolveRelativePath o (In t a f) root = 
makeHakeDeps :: Opts -> [ String ] -> String
makeHakeDeps o l = 
    let hake = resolveRelativePath o (In InstallTree "root" "/hake/hake") ""
        makefile = resolveRelativePath o (Out "root" (opt_makefilename o)) "/Hakefile"
        rule = Rule ( [ hake, 
                        Str "--source-dir", Str (opt_sourcedir o),
                        Str "--install-dir", Str (opt_installdir o),
                        Str "--output-filename", makefile
                      ] ++
                      [ Dep SrcTree "root" h | h <- l ]
                    )
    in
     (makeMakeRules rule)
     ++ ".DELETE_ON_ERROR:\n\n" -- this applies to all targets in the Makefile

makeHakeDeps1 :: Opts -> [ String ] -> String
makeHakeDeps1 _ l = 
    "Makefile: ./hake/hake " 
    ++ concat (intersperse " " l)
    ++ "\n\t./hake/hake Makefile\n"
    ++ ".DELETE_ON_ERROR:\n\n" -- this applies to all targets in the Makefile

-- check the configuration options, returning an error string if they're insane
configErrors :: Maybe String
configErrors
    | unknownArchs /= [] = Just ("unknown architecture(s) specified: "
                               ++ (concat $ intersperse ", " unknownArchs))
    | Config.architectures == [] = Just "no architectures defined"
    | Config.lazy_thc && not Config.use_fp = Just "Config.use_fp must be true to use Config.lazy_thc."
    | otherwise = Nothing
    where
    unknownArchs = Config.architectures \\ Args.allArchitectures


---
--- Convert a Hakefile name to one relative to the root of the source tree. 
---
strip_hfn :: Opts -> String -> String
strip_hfn opts f = Path.removePrefix (opt_sourcedir opts) f

main :: IO() 
main = do
    -- parse arguments; architectures default to config file
    args <- System.Environment.getArgs
    let o1 = parse_arguments args
        al = if opt_architectures o1 == [] 
             then Config.architectures 
             else opt_architectures o1
        opts = o1 { opt_architectures = al }
    if opt_usage_error opts then do
	hPutStrLn stderr usage
        exitWith $ ExitFailure 1
      else do

    -- sanity-check configuration settings
    -- this is currently known at compile time, but might not always be!
    if isJust configErrors then do
	hPutStrLn stderr $ "Error in configuration: " ++ (fromJust configErrors)
	exitWith $ ExitFailure 2    
      else do

    hPutStrLn stdout ("Source directory: " ++ opt_sourcedir opts)
    hPutStrLn stdout ("BF Source directory: " ++ opt_bfsourcedir opts)
    hPutStrLn stdout ("Install directory: " ++ opt_installdir opts)

    hPutStrLn stdout "Reading directory tree..."
    l <- listFilesR (opt_sourcedir opts)
    hPutStrLn stdout "Reading Hakefiles..."
    hfl <- readHakeFiles $ hakeFiles l
    hPutStrLn stdout "Writing HakeFile module..."
    modf <- openFile ("Hakefiles.hs") WriteMode
    hPutStrLn modf $ hakeModule l hfl
    hClose modf
    hPutStrLn stdout "Evaluating Hakefiles..."
    inrules <- evalHakeFiles opts l hfl
    hPutStrLn stdout "Done!"
    -- filter out rules for unsupported architectures and resolve relative paths
    let rules = 
          ([(f, resolveRelativePaths opts (fromJust (filterRuleByArch rl)) (strip_hfn opts f))
           | (f,rl) <- inrules, isJust (filterRuleByArch rl) ])
    hPutStrLn stdout $ "Generating " ++ (opt_makefilename opts) ++ " - this may take some time (and RAM)..." 
    makef <- openFile(opt_makefilename opts) WriteMode
    hPutStrLn makef $ preamble opts args
    -- let hfl2 = [ strip_hfn opts (fst h) | h <- hfl ]
    hPutStrLn makef $ makeHakeDeps opts $ map fst hfl
    hPutStrLn makef $ makeMakefile rules
    hPutStrLn makef $ makeDirectories rules
    hClose makef
    exitWith ExitSuccess
