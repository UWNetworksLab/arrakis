-------------------------------------------------------------------------
-- Copyright (c) 2007-2011, 2012 ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:-
-- ETH Zurich D-INFK CAB F.78, Universitaetstr 6, CH-8092 Zurich. 
-- Attn: Systems Group.
--
-- Basic Hake rule definitions and combinators
--
--------------------------------------------------------------------------

module RuleDefs where
import Data.List (intersect, isSuffixOf, union, (\\), nub, sortBy, elemIndex)
import Path
import qualified X86_64
import qualified X86_32
import qualified SCC
import qualified ARMv5
import qualified ARM11MP
import qualified XScale
import qualified ARMv7
import qualified ARMv7_M
import HakeTypes
import qualified Args
import qualified Config

import Debug.Trace
-- enable debug spew
-- should we move this to Config.hs? -AB
debugFlag = False

--
-- Is a token to be displayed in a rule?
--
inRule :: RuleToken -> Bool
inRule (Dep _ _ _) = False
inRule (PreDep _ _ _) = False
inRule (Target _ _) = False
inRule _ = True

--
-- Look for a set of files: this is called using the "find" combinator
--
withSuffix :: [String] -> String -> String -> [String]
withSuffix af tf arg =
    [ basename f | f <- af, f `isInSameDirAs` tf, isSuffixOf arg f ]
withSuffices :: [String] -> String -> [String] -> [String]
withSuffices af tf args =
    concat [ withSuffix af tf arg | arg <- args ]

--
-- Find files with a given suffix in a given dir
--
inDir :: [String] -> String -> String -> String -> [String]
inDir af tf dir suffix =
    -- Dummy is here so that we can find files in the same dir :-/
    let subdir = (if head dir == '/' then absdir else reldir) ./. "dummy"
        absdir = if head tf == '/' then dir else '.':dir
        reldir = (dirname tf) ./. dir
        files = withSuffix af subdir suffix
    in
        [ dir ./. f | f <- files ]

cInDir :: [String] -> String -> String -> [String]
cInDir af tf dir = inDir af tf dir ".c"
cxxInDir :: [String] -> String -> String -> [String]
cxxInDir af tf dir = inDir af tf dir ".cpp"
sInDir :: [String] -> String -> String -> [String]
sInDir af tf dir = inDir af tf dir ".S"

-------------------------------------------------------------------------
--
-- Architecture specific definitions
--
-------------------------------------------------------------------------

options :: String -> Options
options "x86_64" = X86_64.options
options "x86_32" = X86_32.options
options "scc" = SCC.options
options "armv5" = ARMv5.options
options "arm11mp" = ARM11MP.options
options "xscale" = XScale.options
options "armv7" = ARMv7.options
options "armv7-m" = ARMv7_M.options

kernelCFlags "x86_64" = X86_64.kernelCFlags
kernelCFlags "x86_32" = X86_32.kernelCFlags
kernelCFlags "scc" = SCC.kernelCFlags
kernelCFlags "armv5" = ARMv5.kernelCFlags
kernelCFlags "arm11mp" = ARM11MP.kernelCFlags
kernelCFlags "xscale" = XScale.kernelCFlags
kernelCFlags "armv7" = ARMv7.kernelCFlags
kernelCFlags "armv7-m" = ARMv7_M.kernelCFlags

kernelLdFlags "x86_64" = X86_64.kernelLdFlags
kernelLdFlags "x86_32" = X86_32.kernelLdFlags
kernelLdFlags "scc" = SCC.kernelLdFlags
kernelLdFlags "armv5" = ARMv5.kernelLdFlags
kernelLdFlags "arm11mp" = ARM11MP.kernelLdFlags
kernelLdFlags "xscale" = XScale.kernelLdFlags
kernelLdFlags "armv7" = ARMv7.kernelLdFlags
kernelLdFlags "armv7-m" = ARMv7_M.kernelLdFlags

archFamily :: String -> String
archFamily arch = optArchFamily (options arch)

-------------------------------------------------------------------------
--
-- Options for compiling the kernel, which is special
--
-------------------------------------------------------------------------

kernelIncludes arch = [ NoDep BuildTree arch f | f <- [
                    "/include",
                    "/include/dev" ]]
                 ++
                 [ NoDep SrcTree "src" f | f <- [
                    "/kernel/include/arch" ./. arch,
                    "/kernel/include/arch" ./. archFamily arch,
                    "/kernel/include",
                    "/include",
                    "/include/arch" ./. archFamily arch,
                    Config.libcInc,
                    "/include/c",
                    "/include/target" ./. archFamily arch]]

kernelOptions arch = Options {
            optArch = arch,
            optArchFamily = archFamily arch,
            optFlags = kernelCFlags arch,
            optCxxFlags = [],
            optDefines = (optDefines (options arch)) ++ [ Str "-DIN_KERNEL",
                Str ("-DCONFIG_SCHEDULER_" ++ (show Config.scheduler)),
                Str ("-DCONFIG_TIMESLICE=" ++ (show Config.timeslice)) ],
            optIncludes = kernelIncludes arch,
            optDependencies =
                [ Dep InstallTree arch "/include/errors/errno.h",
                  Dep InstallTree arch "/include/barrelfish_kpi/capbits.h",
                  Dep InstallTree arch "/include/asmoffsets.h",
                  Dep InstallTree arch "/include/trace_definitions/trace_defs.h" ],
            optLdFlags = kernelLdFlags arch,
            optLdCxxFlags = [],
            optLibs = [],
            optCxxLibs = [],
            optSuffix = [],
            optInterconnectDrivers = [],
            optFlounderBackends = [],
            extraFlags = [],
            extraDefines = [],
            extraIncludes = [],
            extraDependencies = [],
            extraLdFlags = []
          }


-------------------------------------------------------------------------
--
-- IMPORTANT: This section contains extraction of functions from the
-- relevant architecture module.  The names and types should be
-- exactly the same as in the architecture.hs file.  This section
-- should not contain any logic; ony architecture extraction.
--
--------------------------------------------------------------------------

--
-- First, the default C compiler for an architecture
--
cCompiler :: Options -> String -> String -> String -> [ RuleToken ]
cCompiler opts phase src obj
    | optArch opts == "x86_64"  = X86_64.cCompiler opts phase src obj
    | optArch opts == "x86_32"  = X86_32.cCompiler opts phase src obj
    | optArch opts == "scc"     = SCC.cCompiler opts phase src obj
    | optArch opts == "armv5"   = ARMv5.cCompiler opts phase src obj
    | optArch opts == "arm11mp" = ARM11MP.cCompiler opts phase src obj
    | optArch opts == "xscale" = XScale.cCompiler opts phase src obj
    | optArch opts == "armv7" = ARMv7.cCompiler opts phase src obj
    | optArch opts == "armv7-m" = ARMv7_M.cCompiler opts phase src obj
    | otherwise = [ ErrorMsg ("no C compiler for " ++ (optArch opts)) ]

cPreprocessor :: Options -> String -> String -> String -> [ RuleToken ]
cPreprocessor opts phase src obj
    | otherwise = [ ErrorMsg ("no C preprocessor for " ++ (optArch opts)) ]

--
-- C++ compiler, where supported
--
cxxCompiler :: Options -> String -> String -> String -> [ RuleToken ]
cxxCompiler opts phase src obj
    | optArch opts == "x86_64"  = X86_64.cxxCompiler opts phase src obj
    | otherwise = [ ErrorMsg ("no C++ compiler for " ++ (optArch opts)) ]


--
-- makeDepend step; note that obj can be whatever the intended output is
--
makeDepend :: Options -> String -> String -> String -> String -> [ RuleToken ]
makeDepend opts phase src obj depfile
    | optArch opts == "x86_64" =
        X86_64.makeDepend opts phase src obj depfile
    | optArch opts == "x86_32" =
        X86_32.makeDepend opts phase src obj depfile
    | optArch opts == "scc" =
        SCC.makeDepend opts phase src obj depfile
    | optArch opts == "armv5" =
        ARMv5.makeDepend opts phase src obj depfile
    | optArch opts == "arm11mp" =
        ARM11MP.makeDepend opts phase src obj depfile
    | optArch opts == "xscale" =
        XScale.makeDepend opts phase src obj depfile
    | optArch opts == "armv7" = 
        ARMv7.makeDepend opts phase src obj depfile
    | optArch opts == "armv7-m" = 
        ARMv7_M.makeDepend opts phase src obj depfile
    | otherwise = [ ErrorMsg ("no dependency generator for " ++ (optArch opts)) ]

makeCxxDepend :: Options -> String -> String -> String -> String -> [ RuleToken ]
makeCxxDepend opts phase src obj depfile
    | optArch opts == "x86_64" =
        X86_64.makeCxxDepend opts phase src obj depfile
    | otherwise = [ ErrorMsg ("no C++ dependency generator for " ++ (optArch opts)) ]

cToAssembler :: Options -> String -> String -> String -> String -> [ RuleToken ]
cToAssembler opts phase src afile objdepfile
    | optArch opts == "x86_64"  = X86_64.cToAssembler opts phase src afile objdepfile
    | optArch opts == "x86_32"  = X86_32.cToAssembler opts phase src afile objdepfile
    | optArch opts == "scc"     = SCC.cToAssembler opts phase src afile objdepfile
    | optArch opts == "armv5"   = ARMv5.cToAssembler opts phase src afile objdepfile
    | optArch opts == "arm11mp" = ARM11MP.cToAssembler opts phase src afile objdepfile
    | optArch opts == "xscale" = XScale.cToAssembler opts phase src afile objdepfile
    | optArch opts == "armv7" = ARMv7.cToAssembler opts phase src afile objdepfile
    | optArch opts == "armv7-m" = ARMv7_M.cToAssembler opts phase src afile objdepfile
    | otherwise = [ ErrorMsg ("no C compiler for " ++ (optArch opts)) ]

--
-- Assemble an assembly language file
--
assembler :: Options -> String -> String -> [ RuleToken ]
assembler opts src obj
    | optArch opts == "x86_64"  = X86_64.assembler opts src obj
    | optArch opts == "x86_32"  = X86_32.assembler opts src obj
    | optArch opts == "scc"     = SCC.assembler opts src obj
    | optArch opts == "armv5"   = ARMv5.assembler opts src obj
    | optArch opts == "arm11mp" = ARM11MP.assembler opts src obj
    | optArch opts == "xscale" = XScale.assembler opts src obj
    | optArch opts == "armv7" = ARMv7.assembler opts src obj
    | optArch opts == "armv7-m" = ARMv7_M.assembler opts src obj
    | otherwise = [ ErrorMsg ("no assembler for " ++ (optArch opts)) ]

archive :: Options -> [String] -> [String] -> String -> String -> [ RuleToken ]
archive opts objs libs name libname
    | optArch opts == "x86_64"  = X86_64.archive opts objs libs name libname
    | optArch opts == "x86_32"  = X86_32.archive opts objs libs name libname
    | optArch opts == "scc"     = SCC.archive opts objs libs name libname
    | optArch opts == "armv5"     = ARMv5.archive opts objs libs name libname
    | optArch opts == "arm11mp" = ARM11MP.archive opts objs libs name libname
    | optArch opts == "xscale" = XScale.archive opts objs libs name libname
    | optArch opts == "armv7" = ARMv7.archive opts objs libs name libname
    | optArch opts == "armv7-m" = ARMv7_M.archive opts objs libs name libname
    | otherwise = [ ErrorMsg ("Can't build a library for " ++ (optArch opts)) ]

linker :: Options -> [String] -> [String] -> String -> [RuleToken]
linker opts objs libs bin
    | optArch opts == "x86_64" = X86_64.linker opts objs libs bin
    | optArch opts == "x86_32" = X86_32.linker opts objs libs bin
    | optArch opts == "scc"    = SCC.linker opts objs libs bin
    | optArch opts == "armv5"  = ARMv5.linker opts objs libs bin
    | optArch opts == "arm11mp" = ARM11MP.linker opts objs libs bin
    | optArch opts == "xscale" = XScale.linker opts objs libs bin
    | optArch opts == "armv7" = ARMv7.linker opts objs libs bin
    | optArch opts == "armv7-m" = ARMv7_M.linker opts objs libs bin
    | otherwise = [ ErrorMsg ("Can't link executables for " ++ (optArch opts)) ]

cxxlinker :: Options -> [String] -> [String] -> String -> [RuleToken]
cxxlinker opts objs libs bin
    | optArch opts == "x86_64" = X86_64.cxxlinker opts objs libs bin
    | otherwise = [ ErrorMsg ("Can't link C++ executables for " ++ (optArch opts)) ]

--
-- The C compiler for compiling things on the host
--
nativeCCompiler :: String
nativeCCompiler = "$(CC)"

-------------------------------------------------------------------------
--
-- Functions to create useful filenames
--

dependFilePath :: String -> String
dependFilePath obj = obj ++ ".depend"

objectFilePath :: Options -> String -> String
objectFilePath opts src = (optSuffix opts) ./. ((removeSuffix src) ++ ".o")

generatedObjectFilePath :: Options -> String -> String
generatedObjectFilePath opts src = (removeSuffix src) ++ ".o"

preprocessedFilePath :: Options -> String -> String
preprocessedFilePath opts src = (optSuffix opts) ./. ((removeSuffix src) ++ ".i")

-- Standard convention is that human generated assembler is .S, machine generated is .s
assemblerFilePath :: Options -> String -> String
assemblerFilePath opts src = (optSuffix opts) ./. ((removeSuffix src) ++ ".s")


-------------------------------------------------------------------------
--
-- Functions with logic to start doing things
--

--
-- Create C file dependencies
--

-- Since this is where we know what the depfile is called it is here that we also
-- decide to include it.  This stops many different places below trying to
-- guess what the depfile is called
--
makeDependArchSub :: Options -> String -> String -> String -> String -> [ RuleToken ]
makeDependArchSub opts phase src objfile depfile =
   [ Str ("@echo Generating $@"), NL ] ++
     makeDepend opts phase src objfile depfile

makeDependArch :: Options -> String -> String -> String -> String -> HRule
makeDependArch opts phase src objfile depfile =
    Rules [ Rule (makeDependArchSub opts phase src objfile depfile),
            Include (Out (optArch opts) depfile)
          ]

-- Make depend for a standard object file
makeDependObj :: Options -> String -> String -> HRule
makeDependObj opts phase src =
    let objfile = (objectFilePath opts src)
    in
      makeDependArch opts phase src objfile (dependFilePath objfile)

-- Make depend for a C++ object file
makeDependCxxArchSub :: Options -> String -> String -> String -> String -> [ RuleToken ]
makeDependCxxArchSub opts phase src objfile depfile =
   [ Str ("@echo Generating $@"), NL ] ++
     makeCxxDepend opts phase src objfile depfile

makeDependCxxArch :: Options -> String -> String -> String -> String -> HRule
makeDependCxxArch opts phase src objfile depfile =
    Rules [ Rule (makeDependCxxArchSub opts phase src objfile depfile),
            Include (Out (optArch opts) depfile)
          ]

makeDependCxxObj :: Options -> String -> String -> HRule
makeDependCxxObj opts phase src =
    let objfile = (objectFilePath opts src)
    in
      makeDependCxxArch opts phase src objfile (dependFilePath objfile)

-- Make depend for an assembler output
makeDependAssembler :: Options -> String -> String -> HRule
makeDependAssembler opts phase src =
    let objfile = (assemblerFilePath opts src)
    in
      makeDependArch opts phase src objfile (dependFilePath objfile)

--
-- Compile a C program to assembler
--
makecToAssembler :: Options -> String -> String -> String -> [ RuleToken ]
makecToAssembler opts phase src obj =
    cToAssembler opts phase src (assemblerFilePath opts src) (dependFilePath obj)

--
-- Assemble an assembly language file
--
assemble :: Options -> String -> [ RuleToken ]
assemble opts src =
    assembler opts src (objectFilePath opts src)

--
-- Create a library from a set of object files
--
archiveLibrary :: Options -> String -> [String] -> [String] -> [ RuleToken ]
archiveLibrary opts name objs libs =
    archive opts objs libs name (libraryPath name)

--
-- Link an executable
--
linkExecutable :: Options -> [String] -> [String] -> String -> [RuleToken]
linkExecutable opts objs libs bin =
    linker opts objs libs (applicationPath bin)

--
-- Link a C++ executable
--
linkCxxExecutable :: Options -> [String] -> [String] -> String -> [RuleToken]
linkCxxExecutable opts objs libs bin =
    cxxlinker opts objs libs (applicationPath bin)

-------------------------------------------------------------------------





-------------------------------------------------------------------------
--
-- Hake macros (hacros?): each of these evaluates to HRule, i.e. a
-- list of templates for Makefile rules
--
-------------------------------------------------------------------------

--
-- Compile a C file for a particular architecture
-- We include cToAssembler to permit humans to type "make foo/bar.s"
--
compileCFile :: Options -> String -> HRule
compileCFile opts src =
    Rules [ Rule (cCompiler opts "src" src (objectFilePath opts src)),
            Rule (makecToAssembler opts "src" src (objectFilePath opts src)),
            makeDependObj opts "src" src
          ]

--
-- Compile a C++ file for a particular architecture
--
compileCxxFile :: Options -> String -> HRule
compileCxxFile opts src =
    Rules [ Rule (cxxCompiler opts "src" src (objectFilePath opts src)),
            makeDependCxxObj opts "src" src
          ]

--
-- Compile a C file for a particular architecture
--
compileGeneratedCFile :: Options -> String -> HRule
compileGeneratedCFile opts src =
    let o2 = opts { optSuffix = "" }
        arch = optArch o2
    in
      Rules [ Rule (cCompiler o2 arch src (objectFilePath o2 src) ),
              Rule (makecToAssembler o2 arch src (objectFilePath o2 src)),
              makeDependObj o2 arch src
            ]

compileCFiles :: Options -> [String] -> HRule
compileCFiles opts srcs = Rules [ compileCFile opts s | s <- srcs ]
compileCxxFiles :: Options -> [String] -> HRule
compileCxxFiles opts srcs = Rules [ compileCxxFile opts s | s <- srcs ]
compileGeneratedCFiles :: Options -> [String] -> HRule
compileGeneratedCFiles opts srcs =
    Rules [ compileGeneratedCFile opts s | s <- srcs ]

--
-- Add a set of C (or whatever) dependences on a *generated* file.
-- Somewhere else this file has to be defined as a target, of
-- course...
--
extraCDependencyForObj :: Options -> String -> String -> String -> [RuleToken]
extraCDependencyForObj opts file s obj =
    let arch = optArch opts
    in
      [ Target arch (dependFilePath obj),
        Target arch obj,
        Dep BuildTree arch file
      ]

extraCDependency :: Options -> String -> String -> HRule
extraCDependency opts file s = Rule (extraCDependencyForObj opts file s obj)
    where obj = objectFilePath opts s


extraCDependencies :: Options -> String -> [String] -> HRule
extraCDependencies opts file srcs =
    Rules [ extraCDependency opts file s | s <- srcs ]

extraGeneratedCDependency :: Options -> String -> String -> HRule
extraGeneratedCDependency opts file s =
    extraCDependency (opts { optSuffix = "" }) file s

--
-- Copy include files to the appropriate directory
--
includeFile :: Options -> String -> HRule
includeFile opts hdr =
    Rules [ (Rule [ Str "cp", In SrcTree "src" hdr, Out (optArch opts) hdr ]),
            (Rule [ PreDep BuildTree (optArch opts) hdr,
                    Target (optArch opts) "/include/errors/errno.h" ]
            )
          ]

--
-- Build a Mackerel header file from a definition.
--
mackerelProgLoc = In InstallTree "tools" "/bin/mackerel"
mackerelDevFileLoc d = In SrcTree "src" ("/devices" ./. (d ++ ".dev"))
mackerelDevHdrPath d = "/include/dev/" ./. (d ++ "_dev.h")

mackerel2 :: Options -> String -> HRule
mackerel2 opts dev = mackerel_generic opts dev "shift-driver"

mackerel :: Options -> String -> HRule
mackerel opts dev = mackerel_generic opts dev "bitfield-driver"

mackerel_generic :: Options -> String -> String -> HRule
mackerel_generic opts dev flag =
    let
        arch = optArch opts
    in
      Rule [ mackerelProgLoc,
             Str ("--" ++ flag),
             Str "-c", mackerelDevFileLoc dev,
             Str "-o", Out arch (mackerelDevHdrPath dev)
           ]

mackerelDependencies :: Options -> String -> [String] -> HRule
mackerelDependencies opts d srcs =
    extraCDependencies opts (mackerelDevHdrPath d) srcs

--
-- Basic Flounder definitions: where things are
--

flounderProgLoc = In InstallTree "tools" "/bin/flounder"
flounderIfFileLoc ifn = In SrcTree "src" ("/if" ./. (ifn ++ ".if"))

-- new-style stubs: path for generic header
flounderIfDefsPath ifn = "/include/if" ./. (ifn ++ "_defs.h")
-- new-style stubs: path for specific backend header
flounderIfDrvDefsPath ifn drv = "/include/if" ./. (ifn ++ "_" ++ drv ++ "_defs.h")

-- new-style stubs: generated C code (for all default enabled backends)
flounderBindingPath opts ifn =
    (optSuffix opts) ./. (ifn ++ "_flounder_bindings.c")
-- new-style stubs: generated C code (for extra backends enabled by the user)
flounderExtraBindingPath opts ifn =
    (optSuffix opts) ./. (ifn ++ "_flounder_extra_bindings.c")

flounderTHCHdrPath ifn = "/include/if" ./. (ifn ++ "_thc.h")
flounderTHCStubPath opts ifn =
    (optSuffix opts) ./. (ifn ++ "_thc.c")

applicationPath name = "/sbin" ./. name
libraryPath libname = "/lib" ./. ("lib" ++ libname ++ ".a")
kernelPath = "/sbin/cpu"

-- construct include arguments to flounder for common types
-- these are:
--  1. platform-specific types (if/platform/foo.if)
--  2. architecture-specific types (if/arch/foo.if)
--  3. generic types (if/types.if)
flounderIncludes :: Options -> [RuleToken]
flounderIncludes opts
    = concat [ [Str "-i", flounderIfFileLoc ifn]
               | ifn <- [ "platform" ./. (optArch opts), -- XXX: optPlatform
                          "arch" ./. (optArch opts),
                          "types" ] ]

flounderRule :: Options -> [RuleToken] -> HRule
flounderRule opts args
    = Rule $ [ flounderProgLoc ] ++ (flounderIncludes opts) ++ args

--
-- Build new-style Flounder header files from a definition
-- (generic header, plus one per backend)
--
flounderGenDefs :: Options -> String -> HRule
flounderGenDefs opts ifn =
    Rules $ flounderRule opts [
           Str "--generic-header", flounderIfFileLoc ifn,
           Out (optArch opts) (flounderIfDefsPath ifn)
         ] : [ flounderRule opts [
           Str $ "--" ++ drv ++ "-header", flounderIfFileLoc ifn,
           Out (optArch opts) (flounderIfDrvDefsPath ifn drv)]
           | drv <- Args.allFlounderBackends ]

--
-- Build a new Flounder binding file from a definition.
-- This builds the binding for all enabled backends
--
flounderBinding :: Options -> String -> [String] -> HRule
flounderBinding opts ifn =
    flounderBindingHelper opts ifn backends (flounderBindingPath opts ifn)
    where
        backends = "generic" : (optFlounderBackends opts)

-- as above, but for a specific set of user-specified backends
flounderExtraBinding :: Options -> String -> [String] -> [String] -> HRule
flounderExtraBinding opts ifn backends =
    flounderBindingHelper opts ifn backends (flounderExtraBindingPath opts ifn)

flounderBindingHelper :: Options -> String -> [String] -> String -> [String] -> HRule
flounderBindingHelper opts ifn backends cfile srcs = Rules $
    [ flounderRule opts $ args ++ [flounderIfFileLoc ifn, Out arch cfile ],
        compileGeneratedCFile opts cfile,
        flounderDefsDepend opts ifn allbackends srcs]
    ++ [extraGeneratedCDependency opts (flounderIfDrvDefsPath ifn d) cfile
        | d <- allbackends]
    where
        arch = optArch opts
        archfam = optArchFamily opts
        args = [Str "-a", Str archfam] ++ [Str $ "--" ++ d ++ "-stub" | d <- backends]
        allbackends = backends `union` optFlounderBackends opts \\ ["generic"]

--
-- Build a Flounder THC header file from a definition.
--
flounderTHCFile :: Options -> String -> HRule
flounderTHCFile opts ifn =
    flounderRule opts [
           Str "--thc-header", flounderIfFileLoc ifn,
           Out (optArch opts) (flounderTHCHdrPath ifn)
         ]

--
-- Build a Flounder THC stubs file from a definition.
--
flounderTHCStub :: Options -> String -> [String] -> HRule
flounderTHCStub opts ifn srcs =
    let cfile = flounderTHCStubPath opts ifn
        hfile = flounderTHCHdrPath ifn
        arch = optArch opts
    in
      Rules [ flounderRule opts [
                     Str "--thc-stubs", flounderIfFileLoc ifn,
                     Out arch cfile
                   ],
              compileGeneratedCFile opts cfile,
              extraCDependencies opts hfile srcs,
              extraGeneratedCDependency opts hfile cfile
            ]

--
-- Create a dependency on a Flounder header file for a set of files,
-- but don't actually build either stub (useful for libraries)
--
flounderDefsDepend :: Options -> String -> [String] -> [String] -> HRule
flounderDefsDepend opts ifn backends srcs = Rules $
    (extraCDependencies opts (flounderIfDefsPath ifn) srcs) :
    [extraCDependencies opts (flounderIfDrvDefsPath ifn drv) srcs
           | drv <- backends, drv /= "generic" ]

--
-- Emit all the Flounder-related rules/dependencies for a given target
--

flounderRules :: Options -> Args.Args -> [String] -> [HRule]
flounderRules opts args csrcs =
    ([ flounderBinding opts f csrcs | f <- Args.flounderBindings args ]
     ++
     [ flounderExtraBinding opts f backends csrcs
       | (f, backends) <- Args.flounderExtraBindings args ]
     ++
     [ flounderTHCStub opts f csrcs | f <- Args.flounderTHCStubs args ]
     ++
     -- Flounder extra defs (header files) also depend on the base
     -- Flounder headers for the same interface
     [ flounderDefsDepend opts f baseBackends csrcs | f <- allIf ]
     ++
     -- Extra defs only for non-base backends (those were already emitted above)
     [ flounderDefsDepend opts f (backends \\ baseBackends) csrcs
       | (f, backends) <- Args.flounderExtraDefs args ]
    )
    where
      -- base backends enabled by default
      baseBackends = optFlounderBackends opts

      -- all interfaces mentioned in flounderDefs or ExtraDefs
      allIf = nub $ Args.flounderDefs args ++ [f | (f,_) <- Args.flounderExtraDefs args]


--
-- Build a Fugu library
--
fuguFile :: Options -> String -> HRule
fuguFile opts file =
    let arch = optArch opts
        cfile = file ++ ".c"
        hfile = "/include/errors/" ++ file ++ ".h"
    in
      Rules [ Rule [In InstallTree "tools" "/bin/fugu",
                    In SrcTree "src" (file++".fugu"),
                    Out arch hfile,
                    Out arch cfile ],
              compileGeneratedCFile opts cfile
         ]

--
-- Build a Pleco library
--
plecoFile :: Options -> String -> HRule
plecoFile opts file =
    let arch = optArch opts
        cfile = file ++ ".c"
        hfile = "/include/trace_definitions/" ++ file ++ ".h"
        jsonfile = "/trace_definitions/" ++ file ++ ".json"
    in
      Rules [ Rule [In InstallTree "tools" "/bin/pleco",
                    In SrcTree "src" (file++".pleco"),
                    Out arch hfile,
                    Out arch jsonfile,
                    Out arch cfile ],
              compileGeneratedCFile opts cfile
         ]

--
-- Build a Hamlet file
--
hamletFile :: Options -> String -> HRule
hamletFile opts file =
    let arch = optArch opts
        hfile = "/include/barrelfish_kpi/capbits.h"
        cfile = "cap_predicates.c"
        usercfile = "user_cap_predicates.c"
        ofile = "user_cap_predicates.o"
        nfile = "cap_predicates"
        afile = "/lib/libcap_predicates.a"
    in
      Rules [ Rule [In InstallTree "tools" "/bin/hamlet",
                    In SrcTree "src" (file++".hl"),
                    Out arch hfile,
                    Out arch cfile,
                    Out arch usercfile ],
              compileGeneratedCFile opts usercfile,
              Rule (archive opts [ ofile ] [] nfile afile)
         ]

--
-- Link a set of object files and libraries together
--
link :: Options -> [String] -> [ String ] -> String -> HRule
link opts objs libs bin =
    Rule (linkExecutable opts objs libs bin)

--
-- Link a set of C++ object files and libraries together
--
linkCxx :: Options -> [String] -> [ String ] -> String -> HRule
linkCxx opts objs libs bin =
    Rule (linkCxxExecutable opts objs libs bin)

--
-- Link a CPU driver.  This is where it gets distinctly architecture-specific.
--
linkKernel :: Options -> String -> [String] -> [String] -> HRule
linkKernel opts name objs libs
    | optArch opts == "x86_64" = X86_64.linkKernel opts objs [libraryPath l | l <- libs ] ("/sbin" ./. name)
    | optArch opts == "x86_32" = X86_32.linkKernel opts objs [libraryPath l | l <- libs ] ("/sbin" ./. name)
    | optArch opts == "scc"    = SCC.linkKernel opts objs [libraryPath l | l <- libs ] ("/sbin" ./. name)
    | optArch opts == "armv5" = ARMv5.linkKernel opts objs [libraryPath l | l <- libs ] ("/sbin" ./. name)
    | optArch opts == "arm11mp" = ARM11MP.linkKernel opts objs [libraryPath l | l <- libs ] ("/sbin" ./. name)
    | optArch opts == "xscale" = XScale.linkKernel opts objs [libraryPath l | l <- libs ] ("/sbin" ./. name)
    | optArch opts == "armv7" = ARMv7.linkKernel opts objs [libraryPath l | l <- libs ] name
    | optArch opts == "armv7-m" = ARMv7_M.linkKernel opts objs [libraryPath l | l <- libs ] name
    | otherwise = Rule [ Str ("Error: Can't link kernel for '" ++ (optArch opts) ++ "'") ]

--
-- Copy a file from one place to another
--
copy :: Options -> String -> String -> HRule
copy opts src dest =
    Rule [ Str "cp", In BuildTree (optArch opts) src, Out (optArch opts) dest ]

--
-- Assemble a list of S files for a particular architecture
--
assembleSFile :: Options -> String -> HRule
assembleSFile opts src =
    Rules [ Rule (assemble opts src),
            makeDependObj opts "src" src
          ]

assembleSFiles :: Options -> [String] -> HRule
assembleSFiles opts srcs = Rules [ assembleSFile opts s | s <- srcs ]

--
-- Archive a bunch of objects into a library
--
staticLibrary :: Options -> String -> [String] -> [String] -> HRule
staticLibrary opts libpath objs libs =
    Rule (archiveLibrary opts libpath objs libs)

--
-- Compile a Haskell binary (for the host architecture)
--
compileHaskell prog main deps = compileHaskellWithLibs prog main deps []
compileHaskellWithLibs prog main deps dirs =
  let 
    tools_dir = (Dep InstallTree "tools" "/tools/.marker")
  in
    Rule ([ NStr "ghc -i",
            NoDep SrcTree "src" ".",
            Str "-odir ", NoDep BuildTree "tools" ".",
            Str "-hidir ", NoDep BuildTree "tools" ".",
            Str "-rtsopts=all",
            Str "--make ",
            In SrcTree "src" main,
            Str "-o ",
            Out "tools" ("/bin" ./. prog),
            Str "$(LDFLAGS)" ]
          ++ concat [[ NStr "-i", NoDep SrcTree "src" d] | d <- dirs]
          ++ [ (Dep SrcTree "src" dep) | dep <- deps ]
          ++ [ tools_dir ])

--
-- Compile (and link) a C binary (for the host architecture)
--
compileNativeC :: String -> [String] -> [String] -> [String] -> HRule
compileNativeC prog cfiles cflags ldflags =
    Rule ([ Str nativeCCompiler,
            Str "-o",
            Out "tools" ("/bin" ./. prog),
            Str "$(CFLAGS)",
            Str "$(LDFLAGS)" ]
          ++ [ (Str flag) | flag <- cflags ]
          ++ [ (Str flag) | flag <- ldflags ]
          ++ [ (In SrcTree "src" dep) | dep <- cfiles ])

--
-- Build a Technical Note
--
buildTechNote :: String -> String -> Bool -> Bool -> [String] -> HRule
buildTechNote input output bib glo figs =
    buildTechNoteWithDeps input output bib glo figs []
buildTechNoteWithDeps :: String -> String -> Bool -> Bool -> [String] -> [RuleToken] -> HRule
buildTechNoteWithDeps input output bib glo figs deps =
    let
        working_dir = NoDep BuildTree "tools" "/tmp/"
        style_files = [ "bfish-logo.pdf", "bftn.sty", "defs.bib", "barrelfish.bib" ]
    in
      Rule ( [ Dep SrcTree "src" (f ++ ".pdf") | f <- figs]
             ++
             [ Dep SrcTree "src" ("/doc/style" ./. f) | f <- style_files ]
             ++
             [ Str "mkdir", Str "-p", working_dir, NL ]
             ++
             deps
             ++
             [ In SrcTree "src" "/tools/run-pdflatex.sh",
               Str "--input-tex", In SrcTree "src" input,
               Str "--working-dir", working_dir,
               Str "--output-pdf", Out "docs" ("/" ++ output),
               Str "--texinput", NoDep SrcTree "src" "/doc/style",
               Str "--bibinput", NoDep SrcTree "src" "/doc/style"
             ]
             ++ (if bib then [ Str "--has-bib" ] else [])
             ++ (if glo then [ Str "--has-glo" ] else [])
           )

---------------------------------------------------------------------
--
-- Transformations on file names
--
----------------------------------------------------------------------

allObjectPaths :: Options -> Args.Args -> [String]
allObjectPaths opts args =
    [objectFilePath opts g
         | g <- (Args.cFiles args)++(Args.cxxFiles args)++(Args.assemblyFiles args)]
    ++
    [generatedObjectFilePath opts g
         | g <- [ flounderBindingPath opts f
                      | f <- (Args.flounderBindings args)]
                ++
                [ flounderExtraBindingPath opts f
                      | (f, _) <- (Args.flounderExtraBindings args)]
                ++
                [ flounderTHCStubPath opts f
                      | f <- (Args.flounderTHCStubs args)]
    ]

allLibraryPaths :: Args.Args -> [String]
allLibraryPaths args =
    [ libraryPath l | l <- Args.addLibraries args ]


---------------------------------------------------------------------
--
-- Very large-scale macros
--
----------------------------------------------------------------------

--
-- Build an application binary
--

application :: Args.Args
application = Args.defaultArgs { Args.buildFunction = applicationBuildFn }

applicationBuildFn :: [String] -> String -> Args.Args -> HRule
applicationBuildFn af tf args
    | debugFlag && trace (Args.showArgs (tf ++ " Application ") args) False
        = undefined
applicationBuildFn af tf args =
    Rules [ appBuildArch af tf args arch | arch <- Args.architectures args ]

appGetOptionsForArch arch args =
    (options arch) { extraIncludes =
                         [ NoDep SrcTree "src" a | a <- Args.addIncludes args],
                     optIncludes = (optIncludes $ options arch) \\
                         [ NoDep SrcTree "src" i | i <- Args.omitIncludes args ],
                     optFlags = (optFlags $ options arch) \\
                                [ Str f | f <- Args.omitCFlags args ],
                     optCxxFlags = (optCxxFlags $ options arch) \\
                                   [ Str f | f <- Args.omitCxxFlags args ],
                     optSuffix = "_for_app_" ++ Args.target args,
                     extraFlags = Args.addCFlags args ++ Args.addCxxFlags args,
                     extraLdFlags = [ Str f | f <- Args.addLinkFlags args ],
                     extraDependencies =
                         [Dep BuildTree arch s | s <- Args.addGeneratedDependencies args]
                   }

appBuildArch af tf args arch =
    let -- Fiddle the options
        opts = appGetOptionsForArch arch args
        csrcs = Args.cFiles args
        cxxsrcs = Args.cxxFiles args
        appname = Args.target args
        -- XXX: Not sure if this is correct. Currently assuming that if the app
        -- contains C++ files, we have to use the C++ linker.
        mylink = if cxxsrcs == [] then link else linkCxx
    in
      Rules ( flounderRules opts args csrcs
              ++
              [ mackerelDependencies opts m csrcs | m <- Args.mackerelDevices args ]
              ++
              [ compileCFiles opts csrcs,
                compileCxxFiles opts cxxsrcs,
                assembleSFiles opts (Args.assemblyFiles args),
                mylink opts (allObjectPaths opts args) (allLibraryPaths args) appname
              ]
            )

--
-- Build an Arrakis application binary
--

arrakisapplication :: Args.Args
arrakisapplication = Args.defaultArgs { Args.buildFunction = arrakisApplicationBuildFn }

arrakisApplicationBuildFn :: [String] -> String -> Args.Args -> HRule
arrakisApplicationBuildFn af tf args
    | debugFlag && trace (Args.showArgs (tf ++ " Arrakis Application ") args) False
        = undefined
arrakisApplicationBuildFn af tf args =
    Rules [ arrakisAppBuildArch af tf args arch | arch <- Args.architectures args ]

arrakisAppGetOptionsForArch arch args =
    (options arch) { extraIncludes =
                         [ NoDep SrcTree "src" a | a <- Args.addIncludes args],
                     optIncludes = (optIncludes $ options arch) \\
                         [ NoDep SrcTree "src" i | i <- Args.omitIncludes args ],
                     optFlags = ((optFlags $ options arch) ++ [ Str "-DARRAKIS" ]) \\
                                [ Str f | f <- Args.omitCFlags args ],
                     optCxxFlags = (optCxxFlags $ options arch) \\
                                   [ Str f | f <- Args.omitCxxFlags args ],
                     optSuffix = "_for_app_" ++ Args.target args,
                     optLibs = [ In InstallTree arch "/lib/libarrakis.a" ] ++
                               ((optLibs $ options arch) \\
                                [ In InstallTree arch "/lib/libbarrelfish.a" ]),
                     extraFlags = Args.addCFlags args ++ Args.addCxxFlags args,
                     extraLdFlags = [ Str f | f <- Args.addLinkFlags args ],
                     extraDependencies =
                         [Dep BuildTree arch s | s <- Args.addGeneratedDependencies args]
                   }

arrakisAppBuildArch af tf args arch =
    let -- Fiddle the options
        opts = arrakisAppGetOptionsForArch arch args
        csrcs = Args.cFiles args
        cxxsrcs = Args.cxxFiles args
        appname = Args.target args
        -- XXX: Not sure if this is correct. Currently assuming that if the app
        -- contains C++ files, we have to use the C++ linker.
        mylink = if cxxsrcs == [] then link else linkCxx
    in
      Rules ( flounderRules opts args csrcs
              ++
              [ mackerelDependencies opts m csrcs | m <- Args.mackerelDevices args ]
              ++
              [ compileCFiles opts csrcs,
                compileCxxFiles opts cxxsrcs,
                assembleSFiles opts (Args.assemblyFiles args),
                mylink opts (allObjectPaths opts args) (allLibraryPaths args) appname
              ]
            )

--
-- Build a static library
--

library :: Args.Args
library = Args.defaultArgs { Args.buildFunction = libraryBuildFn }

libraryBuildFn :: [String] -> String -> Args.Args -> HRule
libraryBuildFn af tf args | debugFlag && trace (Args.showArgs (tf ++ " Library ") args) False = undefined
libraryBuildFn af tf args =
    Rules [ libBuildArch af tf args arch | arch <- Args.architectures args ]

libGetOptionsForArch arch args =
    (options arch) { extraIncludes =
                         [ NoDep SrcTree "src" a | a <- Args.addIncludes args],
                     optIncludes = (optIncludes $ options arch) \\
                         [ NoDep SrcTree "src" i | i <- Args.omitIncludes args ],
                     optFlags = (optFlags $ options arch) \\
                                [ Str f | f <- Args.omitCFlags args ],
                     optCxxFlags = (optCxxFlags $ options arch) \\
                                   [ Str f | f <- Args.omitCxxFlags args ],
                     optSuffix = "_for_lib_" ++ Args.target args,
                     extraFlags = Args.addCFlags args ++ Args.addCxxFlags args,
                     extraDependencies =
                         [Dep BuildTree arch s | s <- Args.addGeneratedDependencies args]
                   }

libBuildArch af tf args arch =
    let -- Fiddle the options
        opts = libGetOptionsForArch arch args
        csrcs = Args.cFiles args
        cxxsrcs = Args.cxxFiles args
    in
      Rules ( flounderRules opts args csrcs
              ++
              [ mackerelDependencies opts m csrcs | m <- Args.mackerelDevices args ]
              ++
              [ compileCFiles opts csrcs,
                compileCxxFiles opts cxxsrcs,
                assembleSFiles opts (Args.assemblyFiles args),
                staticLibrary opts (Args.target args) (allObjectPaths opts args) (allLibraryPaths args)
              ]
            )

--
-- Library dependecies
--

-- The following code is under heavy construction, and also somewhat ugly
data LibDepTree = LibDep String | LibDeps [LibDepTree] deriving (Show,Eq)

-- manually add dependencies for now (it would be better if each library
-- defined each own dependencies locally, but that does not seem to be an
-- easy thing to do currently
libposixcompat_deps   = LibDeps [ LibDep "posixcompat",
                                  libvfs_deps_all, LibDep "term_server" ]
liblwip_deps          = LibDeps $ [ LibDep x | x <- deps ]
    where deps = ["lwip" ,"contmng" ,"net_if_raw" ,"timer" ,"hashtable"]
libnetQmng_deps       = LibDeps $ [ LibDep x | x <- deps ]
    where deps = ["net_queue_manager", "contmng" ,"procon" , "net_if_raw", "bfdmuxvm"]
libnfs_deps           = LibDeps $ [ LibDep "nfs" ]
libssh_deps           = LibDeps [ libposixcompat_deps, libopenbsdcompat_deps,
                                  LibDep "zlib", LibDep "crypto", LibDep "ssh" ]
libopenbsdcompat_deps = LibDeps [ libposixcompat_deps, LibDep "crypto",
                                  LibDep "openbsdcompat" ]

-- we need to make vfs more modular to make this actually useful
data VFSModules = VFS_RamFS | VFS_NFS | VFS_BlockdevFS | VFS_FAT
vfsdeps :: [VFSModules] -> [LibDepTree]
vfsdeps []                  = [LibDep "vfs"]
vfsdeps (VFS_RamFS:xs)      = [] ++ vfsdeps xs
vfsdeps (VFS_NFS:xs)        = [libnfs_deps] ++ vfsdeps xs
vfsdeps (VFS_BlockdevFS:xs) = [LibDep "ahci", LibDep "megaraid"] ++ vfsdeps xs
vfsdeps (VFS_FAT:xs)        = [] ++ vfsdeps xs

libvfs_deps_all        = LibDeps $ vfsdeps [VFS_NFS, VFS_RamFS, VFS_BlockdevFS,
                                            VFS_FAT]
libvfs_deps_nonfs      = LibDeps $ vfsdeps [VFS_RamFS, VFS_BlockdevFS, VFS_FAT]
libvfs_deps_nfs        = LibDeps $ vfsdeps [VFS_NFS]
libvfs_deps_ramfs      = LibDeps $ vfsdeps [VFS_RamFS]
libvfs_deps_blockdevfs = LibDeps $ vfsdeps [VFS_BlockdevFS]
libvfs_deps_fat        = LibDeps $ vfsdeps [VFS_FAT, VFS_BlockdevFS]

-- flatten the dependency tree
flat :: [LibDepTree] -> [LibDepTree]
flat [] = []
flat ((LibDep  l):xs) = [LibDep l] ++ flat xs
flat ((LibDeps t):xs) = flat t ++ flat xs

str2dep :: String -> LibDepTree
str2dep  str
    | str == "vfs"           = libvfs_deps_all
    | str == "vfs_nonfs"     = libvfs_deps_nonfs
    | str == "posixcompat"   = libposixcompat_deps
    | str == "lwip"          = liblwip_deps
    | str == "netQmng"       = libnetQmng_deps
    | str == "ssh"           = libssh_deps
    | str == "openbsdcompat" = libopenbsdcompat_deps
    | otherwise              = LibDep str

-- get library depdencies
--   we need a specific order for the .a, so we define a total order
libDeps :: [String] -> [String]
libDeps xs = [x | (LibDep x) <- (sortBy xcmp) . nub . flat $ map str2dep xs ]
    where xord = [ "ssh"
                  , "openbsdcompat"
                  , "crypto"
                  , "zlib"
                  , "posixcompat"
                  , "term_server"
                  , "vfs"
                  , "ahci"
                  , "megaraid"
                  , "nfs"
                  , "net_queue_manager"
                  , "bfdmuxvm"
                  , "lwip"
                  , "arranet"
                  , "e1000n"
                  , "e10k"
                  , "e10k_vf"
                  , "contmng"
                  , "procon"
                  , "net_if_raw"
                  , "vfsfd"
                  , "timer"
                  , "hashtable"]
          xcmp (LibDep a) (LibDep b) = compare (elemIndex a xord) (elemIndex b xord)


--
-- Build a CPU driver
--

cpuDriver :: Args.Args
cpuDriver = Args.defaultArgs { Args.buildFunction = cpuDriverBuildFn, 
                               Args.target = "cpu" }

-- CPU drivers are built differently
cpuDriverBuildFn :: [String] -> String -> Args.Args -> HRule
cpuDriverBuildFn af tf args = Rules []

