--------------------------------------------------------------------------
-- Copyright (c) 2007-2010, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
--
-- Default architecture-specific definitions for Barrelfish
-- 
--------------------------------------------------------------------------

module ArchDefaults where

import Data.List
import HakeTypes
import Path
import qualified Config

commonFlags = [ Str s | s <- [ "-fno-builtin",
                                "-nostdinc",
                                "-U__linux__",
                                "-Ulinux",
                                "-Wall",
                                "-Wshadow",
                                "-Wmissing-declarations",
                                "-Wmissing-field-initializers",
                                "-Wredundant-decls",
                                "-Werror",
                                "-imacros" ] ]
         ++ [ NoDep SrcTree "src" "/include/deputy/nodeputy.h" ]

commonCFlags = [ Str s | s <- [ "-std=c99",
                                "-U__STRICT_ANSI__", -- for newlib headers
                                "-Wstrict-prototypes",
                                "-Wold-style-definition",
                                "-Wmissing-prototypes" ] ]
                 ++ [ ContStr Config.use_fp "-fno-omit-frame-pointer" ""]

commonCxxFlags = [ Str s | s <- [ "-nostdinc++",
                                  "-std=c++0x",
                                  "-fno-exceptions",
                                  "-I" ] ]
                 ++ [ NoDep SrcTree "src" "/include/cxx" ]
                 ++ [ ContStr Config.use_fp "-fno-omit-frame-pointer" ""]

cFlags = [ Str s | s <- [ "-Wno-packed-bitfield-compat" ] ]
       ++ commonCFlags

cxxFlags = [ Str s | s <- [ "-Wno-packed-bitfield-compat" ] ]
       ++ commonCxxFlags

cDefines options = [ Str ("-D"++s) | s <- [ "BARRELFISH" ]]
                   ++ Config.defines
                   ++ Config.arch_defines options

cStdIncs arch archFamily =
    [ NoDep SrcTree "src" "/include",
      NoDep SrcTree "src" ("/include/arch" ./. archFamily),
      NoDep SrcTree "src" Config.libcInc,
      NoDep SrcTree "src" "/include/c",
      NoDep SrcTree "src" ("/include/target" ./. archFamily),
      NoDep SrcTree "src" Config.lwipxxxInc, -- XXX
      NoDep SrcTree "src" Config.lwipInc,
      NoDep InstallTree arch "/include",
      NoDep InstallTree arch "/include/dev",
      NoDep SrcTree "src" ".",
      NoDep BuildTree arch "." ]
                      
ldFlags arch = 
    [ Str Config.cOptFlags,
      In InstallTree arch "/lib/crt0.o",
      In InstallTree arch "/lib/crtbegin.o",
      Str "-fno-builtin",
      Str "-nostdlib" ]
          
ldCxxFlags arch = 
    [ Str Config.cOptFlags,
      In InstallTree arch "/lib/crt0.o",
      In InstallTree arch "/lib/crtbegin.o",
      Str "-fno-builtin",
      Str "-nostdlib" ]


-- Libraries that are linked to all applications.
stdLibs arch = 
    [ In InstallTree arch "/lib/libbarrelfish.a",
      In InstallTree arch "/lib/libterm_client.a",
      In InstallTree arch "/lib/liboctopus_parser.a", -- XXX: For NS client in libbarrelfish
      In InstallTree arch "/errors/errno.o",
      In InstallTree arch ("/lib/lib" ++ Config.libc ++ ".a"),
      --In InstallTree arch "/lib/libposixcompat.a",
      --In InstallTree arch "/lib/libvfs.a",
      --In InstallTree arch "/lib/libnfs.a",
      --In InstallTree arch "/lib/liblwip.a",
      --In InstallTree arch "/lib/libbarrelfish.a",
      --In InstallTree arch "/lib/libcontmng.a",
      --In InstallTree arch "/lib/libprocon.a",
      In InstallTree arch "/lib/crtend.o" ,
      In InstallTree arch "/lib/libcollections.a" ]

stdCxxLibs arch = 
    [ In InstallTree arch "/lib/libcxx.a",
      Str "./libsupc++.a" ]
    ++ stdLibs arch 

options arch archFamily = Options { 
            optArch = arch,
            optArchFamily = archFamily,
            optFlags = cFlags,
            optCxxFlags = cxxFlags,
            optDefines = [ Str "-DBARRELFISH" ] ++ Config.defines,
            optIncludes = cStdIncs arch archFamily,
            optDependencies = 
                [ PreDep InstallTree arch "/include/errors/errno.h",
                  PreDep InstallTree arch "/include/barrelfish_kpi/capbits.h",
                  PreDep InstallTree arch "/include/asmoffsets.h",
                  PreDep InstallTree arch "/include/trace_definitions/trace_defs.h" ],
            optLdFlags = ldFlags arch,
            optLdCxxFlags = ldCxxFlags arch,
            optLibs = stdLibs arch,
            optCxxLibs = stdCxxLibs arch,
            optInterconnectDrivers = ["lmp", "ump", "multihop"],
            optFlounderBackends = ["lmp", "ump", "multihop"],
            extraFlags = [],
            extraDefines = [],
            extraIncludes = [],
            extraDependencies = [],
            extraLdFlags = [],
            optSuffix = []
          }

------------------------------------------------------------------------
--
-- Now, commands to actually do something
--
------------------------------------------------------------------------

--
-- C compiler
--
cCompiler arch compiler opts phase src obj = 
    let incls = (optIncludes opts) ++ (extraIncludes opts)
        flags = (optFlags opts) 
                ++ (optDefines opts)
                ++ [ Str f | f <- extraFlags opts ]
                ++ [ Str f | f <- extraDefines opts ]
        deps = (optDependencies opts) ++ (extraDependencies opts)
    in
      [ Str compiler ] ++ flags ++ [ Str Config.cOptFlags ]
      ++ concat [ [ NStr "-I", i ] | i <- incls ] 
      ++ [ Str "-o", Out arch obj,
           Str "-c", In (if phase == "src" then SrcTree else BuildTree) phase src ]
      ++ deps
--
-- the C preprocessor, like C compiler but with -E
--
cPreprocessor arch compiler opts phase src obj = 
    let incls = (optIncludes opts) ++ (extraIncludes opts)
        flags = (optFlags opts) 
                ++ (optDefines opts)
                ++ [ Str f | f <- extraFlags opts ]
                ++ [ Str f | f <- extraDefines opts ]
        deps = (optDependencies opts) ++ (extraDependencies opts)
        cOptFlags = unwords ((words Config.cOptFlags) \\ ["-g"])
    in
      [ Str compiler ] ++ flags ++ [ Str cOptFlags ]
      ++ concat [ [ NStr "-I", i ] | i <- incls ] 
      ++ [ Str "-o", Out arch obj,
           Str "-E", In (if phase == "src" then SrcTree else BuildTree) phase src ]
      ++ deps

--
-- C++ compiler
--
cxxCompiler arch cxxcompiler opts phase src obj = 
    let incls = (optIncludes opts) ++ (extraIncludes opts)
        flags = (optCxxFlags opts) 
                ++ (optDefines opts)
                ++ [ Str f | f <- extraFlags opts ]
                ++ [ Str f | f <- extraDefines opts ]
        deps = (optDependencies opts) ++ (extraDependencies opts)
    in
      [ Str cxxcompiler ] ++ flags ++ [ Str Config.cOptFlags ]
      ++ concat [ [ NStr "-I", i ] | i <- incls ] 
      ++ [ Str "-o", Out arch obj,
           Str "-c", In (if phase == "src" then SrcTree else BuildTree) phase src ]
      ++ deps

--
-- Create C file dependencies
--
makeDepend arch compiler opts phase src obj depfile =
    let incls = (optIncludes opts) ++ (extraIncludes opts)
        flags = (optFlags opts) 
                ++ (optDefines opts)
                ++ [ Str f | f <- extraFlags opts ]
                ++ [ Str f | f <- extraDefines opts ]
    in
      [ Str ('@':compiler) ] ++ flags 
      ++ concat [ [ NStr "-I", i ] | i <- incls ] 
      ++ (optDependencies opts) ++ (extraDependencies opts)
      ++ [ Str "-M -MF", 
           Out arch depfile,
           Str "-MQ", NoDep BuildTree arch obj, 
           Str "-MQ", NoDep BuildTree arch depfile,
           Str "-c", In (if phase == "src" then SrcTree else BuildTree) phase src
         ] 

--
-- Create C++ file dependencies
--
makeCxxDepend arch cxxcompiler opts phase src obj depfile =
    let incls = (optIncludes opts) ++ (extraIncludes opts)
        flags = (optCxxFlags opts) 
                ++ (optDefines opts)
                ++ [ Str f | f <- extraFlags opts ]
                ++ [ Str f | f <- extraDefines opts ]
    in
      [ Str ('@':cxxcompiler) ] ++ flags 
      ++ concat [ [ NStr "-I", i ] | i <- incls ] 
      ++ (optDependencies opts) ++ (extraDependencies opts)
      ++ [ Str "-M -MF", 
           Out arch depfile,
           Str "-MQ", NoDep BuildTree arch obj, 
           Str "-MQ", NoDep BuildTree arch depfile,
           Str "-c", In (if phase == "src" then SrcTree else BuildTree) phase src
         ] 

--
-- Compile a C program to assembler
--
cToAssembler :: String -> String -> Options -> String -> String -> String -> String -> [ RuleToken ]
cToAssembler arch compiler  opts phase src afile objdepfile =
    let incls = (optIncludes opts) ++ (extraIncludes opts)
        flags = (optFlags opts) 
                ++ (optDefines opts)
                ++ [ Str f | f <- extraFlags opts ]
                ++ [ Str f | f <- extraDefines opts ]
        deps = [ Dep BuildTree arch objdepfile ] ++ (optDependencies opts) ++ (extraDependencies opts)
    in
      [ Str compiler ] ++ flags ++ [ Str Config.cOptFlags ]
      ++ concat [ [ NStr "-I", i ] | i <- incls ] 
      ++ [ Str "-o ", Out arch afile, 
           Str "-S ", In (if phase == "src" then SrcTree else BuildTree) phase src ]
      ++ deps

--
-- Assemble an assembly language file
--
assembler :: String -> String -> Options -> String -> String -> [ RuleToken ]
assembler arch compiler opts src obj = 
    let incls = (optIncludes opts) ++ (extraIncludes opts)
        flags = (optFlags opts) 
                ++ (optDefines opts)
                ++ [ Str f | f <- extraFlags opts ]
                ++ [ Str f | f <- extraDefines opts ]
        deps = (optDependencies opts) ++ (extraDependencies opts)
    in
      [ Str compiler ] ++ flags ++ [ Str Config.cOptFlags ]
      ++ concat [ [ NStr "-I", i ] | i <- incls ] 
      ++ [ Str "-o ", Out arch obj, Str "-c ", In SrcTree "src" src ]
      ++ deps

--
-- Create a library from a set of object files
--
archive :: String -> Options -> [String] -> [String] -> String -> String -> [ RuleToken ]
archive arch opts objs libs name libname =
    [ Str "rm -f ", Out arch libname ]
    ++ 
    [ NL, Str "ar cr ", Out arch libname ] 
    ++ 
    [ In BuildTree arch o | o <- objs ]
    ++
    if libs == [] then []
                  else (
      [ NL, Str ("rm -fr tmp-" ++ arch ++ name ++ "; mkdir tmp-" ++ arch ++ name) ]
      ++
      [ NL, Str ("cd tmp-" ++ arch ++ name ++ "; for i in ") ]
      ++
      [ In BuildTree arch a | a <- libs ]
      ++
      [ Str "; do ar x ../$$i; done" ]
      ++
      [ NL, Str "ar q ", Out arch libname, Str (" tmp-" ++ arch ++ name ++ "/*.o") ]
      ++
      [ NL, Str ("rm -fr tmp-" ++ arch ++ name) ]
    )
    ++
    [ NL, Str "ranlib ", Out arch libname ]

--
-- Link an executable
-- 
linker :: String -> String -> Options -> [String] -> [String] -> String -> [RuleToken]
linker arch compiler opts objs libs bin =
    [ Str compiler ] 
    ++ (optLdFlags opts) 
    ++ 
    (extraLdFlags opts) 
    ++ 
    [ Str "-o", Out arch bin ] 
    ++ 
    [ In BuildTree arch o | o <- objs ]
    ++
    [ In BuildTree arch l | l <- libs ]
    ++ 
    (optLibs opts)

--
-- Link an executable
-- 
cxxlinker :: String -> String -> Options -> [String] -> [String] -> String -> [RuleToken]
cxxlinker arch cxxcompiler opts objs libs bin =
    [ Str cxxcompiler ] 
    ++ (optLdCxxFlags opts) 
    ++ 
    (extraLdFlags opts) 
    ++ 
    [ Str "-o", Out arch bin ] 
    ++ 
    [ In BuildTree arch o | o <- objs ]
    ++
    [ In BuildTree arch l | l <- libs ]
    ++ 
    (optCxxLibs opts)
