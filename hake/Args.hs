--------------------------------------------------------------------------
-- Copyright (c) 2007-2010, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
--
-- Arguments to major Hake targets
-- 
--------------------------------------------------------------------------

module Args where

import HakeTypes

data Args = Args { 
      buildFunction :: [String] -> String -> Args -> HRule,
      target :: String,
      cFiles :: [String],
      generatedCFiles :: [String],
      cxxFiles :: [String],
      generatedCxxFiles :: [String],
      assemblyFiles :: [String],
      flounderDefs :: [String],
      flounderBindings :: [String], -- built stubs for all enabled backends
      flounderExtraDefs :: [(String, [String])],
      flounderExtraBindings :: [(String, [String])], -- build stubs for specific backends
      flounderTHCDefs :: [String], -- TODO: this can probably be subsumed into the above?
      flounderTHCStubs :: [String], -- TODO: this can probably be subsumed into the above?
      mackerelDevices :: [String],
      addCFlags :: [String],
      addCxxFlags :: [String],
      omitCFlags :: [String],
      omitCxxFlags :: [String],
      addIncludes :: [String],
      omitIncludes :: [String],
      addLinkFlags :: [String],
      addLibraries :: [String],
      addGeneratedDependencies :: [String],
      architectures :: [String]
} 

defaultArgs = Args { 
      buildFunction = defaultBuildFn,
      target = "",
      cFiles = [],
      generatedCFiles = [],
      cxxFiles = [],
      generatedCxxFiles = [],
      assemblyFiles = [],
      flounderDefs = [],
      flounderBindings = [],
      flounderExtraDefs = [],
      flounderExtraBindings = [],
      flounderTHCDefs = [],
      flounderTHCStubs = [],
      mackerelDevices = [],
      addCFlags = [],
      addCxxFlags = [],
      omitCFlags = [],
      omitCxxFlags = [],
      addIncludes = [],
      omitIncludes = [],
      addLinkFlags = [],
      addLibraries = [],
      addGeneratedDependencies = [],
      architectures = allArchitectures
}

allArchitectures = [ "x86_64", "x86_32", "armv5", "arm11mp", "scc", "xscale", "armv7", "armv7-m" ]
allArchitectureFamilies = [ "x86_64", "x86_32", "arm", "scc" ]
-- architectures that currently support THC
thcArchitectures = ["x86_64", "x86_32", "scc"]

-- all known flounder backends that we might want to generate defs for
allFlounderBackends
    = [ "lmp", "ump", "ump_ipi", "loopback", "rpcclient", "msgbuf", "multihop", "ahci" ]

defaultBuildFn :: [String] -> String -> Args -> HRule
defaultBuildFn _ f _ = 
    Error ("Bad use of default Args in " ++ f)

showArgs :: String -> Args -> String
showArgs prefix a =
    prefix ++ "Args:" 
    ++ "\n  target:                " ++ (show $ target a)
    ++ "\n  cFiles:                " ++ (show $ cFiles a)
    ++ "\n  generatedCFiles:       " ++ (show $ generatedCFiles a)
    ++ "\n  cxxFiles:              " ++ (show $ cxxFiles a)
    ++ "\n  generatedCxxFiles      " ++ (show $ generatedCxxFiles a)
    ++ "\n  assemblyFiles:         " ++ (show $ assemblyFiles a)
    ++ "\n  flounderDefs:          " ++ (show $ flounderDefs a)
    ++ "\n  flounderBindings:      " ++ (show $ flounderBindings a)
    ++ "\n  flounderExtraDefs:     " ++ (show $ flounderExtraDefs a)
    ++ "\n  flounderExtraBindings: " ++ (show $ flounderExtraBindings a)
    ++ "\n  flounderTHCDefs:       " ++ (show $ flounderTHCDefs a)
    ++ "\n  flounderTHCStubs:      " ++ (show $ flounderTHCStubs a)
    ++ "\n  addCFlags:             " ++ (show $ addCFlags a)
    ++ "\n  addCxxFlags:           " ++ (show $ addCxxFlags a)
    ++ "\n  omitCFlags:            " ++ (show $ omitCFlags a)
    ++ "\n  omitCxxFlags:          " ++ (show $ omitCxxFlags a)
    ++ "\n  addIncludes:           " ++ (show $ addIncludes a)
    ++ "\n  omitIncludes:          " ++ (show $ omitIncludes a)
    ++ "\n  addLinkFlags:          " ++ (show $ addLinkFlags a)
    ++ "\n  addLibraries:          " ++ (show $ addLibraries a)
    ++ "\n  addDeps:               " ++ (show $ addGeneratedDependencies a)
    ++ "\n  architectures:         " ++ (show $ architectures a)
    ++ "\n"
