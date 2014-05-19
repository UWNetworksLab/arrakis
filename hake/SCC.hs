--------------------------------------------------------------------------
-- Copyright (c) 2007-2011, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
--
-- Architectural definitions for Barrelfish on Single Chip Cloud (SCC).
-- 
--------------------------------------------------------------------------

module SCC where

import HakeTypes
import Path
import qualified Config
import qualified ArchDefaults
import qualified X86_32

-------------------------------------------------------------------------
--
-- Architecture specific definitions for SCC
--
-------------------------------------------------------------------------

arch = "scc"
archFamily = "x86_32"
compiler = "gcc"
cxxcompiler = "g++"

ourCommonFlags = [ Str "-m32",
                   Str "-mno-red-zone",
                   Str "-fPIE",
                   Str "-fno-stack-protector",
                   Str "-D__scc__",
                   Str "-D__x86__",
                   Str "-Wno-unused-but-set-variable",
                   Str "-march=pentium" ]


commonCFlags = ArchDefaults.commonCFlags
               ++ ArchDefaults.commonFlags
               ++ ourCommonFlags

commonCxxFlags = ArchDefaults.commonCxxFlags
                 ++ ArchDefaults.commonFlags
                 ++ ourCommonFlags

--
-- We inherit some flags here from X86_32, but we need to be careful.
-- If any specify arch-dependent files, such as libraries, they will
-- have dependencies on the wrong architecture.  Worse, AB's hake hack
-- to file non-arch rules from the Makefile will mean they will never
-- even appear in the generated Makefile, which is very difficult to
-- debug.
--
options = (ArchDefaults.options arch archFamily) { 
            optFlags = commonCFlags,
            optCxxFlags = commonCxxFlags,
            optDefines = ArchDefaults.cDefines options,
            optLdFlags = ArchDefaults.ldFlags arch ++ X86_32.ourLdFlags,
            optLdCxxFlags = ArchDefaults.ldFlags arch ++ X86_32.ourLdFlags,
            optLibs = (ArchDefaults.stdLibs arch) ++ [ Str "-lgcc" ],
            optInterconnectDrivers = ["lmp", "ump"], -- ump needed for ump_ipi
            optFlounderBackends = ["lmp", "ump_ipi"]
          }

--
-- The kernel is "different"
--

kernelCFlags = X86_32.kernelCFlags ++ [ Str "-march=pentium",
                                        Str "-D__scc__" ]
kernelLdFlags = X86_32.kernelLdFlags

--
-- Compilers
--
cCompiler = ArchDefaults.cCompiler arch compiler
cxxCompiler = ArchDefaults.cxxCompiler arch cxxcompiler
makeDepend = ArchDefaults.makeDepend arch compiler
makeCxxDepend  = ArchDefaults.makeCxxDepend arch cxxcompiler
cToAssembler = ArchDefaults.cToAssembler arch compiler
assembler = ArchDefaults.assembler arch compiler
archive = ArchDefaults.archive arch
linker = ArchDefaults.linker arch compiler
cxxlinker = ArchDefaults.cxxlinker arch cxxcompiler

--
-- Link the kernel (CPU Driver)
-- 
linkKernel :: Options -> [String] -> [String] -> String -> HRule
linkKernel opts objs libs kbin = 
    Rules [ Rule ([ Str compiler, Str Config.cOptFlags,
                    NStr "-T", In BuildTree arch "/kernel/linker.lds",
                    Str "-o", Out arch kbin 
                  ]
                  ++ (optLdFlags opts)
                  ++
                  [ In BuildTree arch o | o <- objs ]
                  ++
                  [ In BuildTree arch l | l <- libs ]
                  ++ 
                  [ Str "-lgcc" ]
                  ++
                  [ NL, NStr "/bin/echo -e '\\0002' | dd of=",
                    Out arch kbin, 
                    Str "bs=1 seek=16 count=1 conv=notrunc status=noxfer"
                  ]
                 ),
            Rule [ Str "cpp", 
                   NStr "-I", NoDep SrcTree "src" "/kernel/include/",
                   Str "-D__ASSEMBLER__", 
                   Str "-P", In SrcTree "src" "/kernel/arch/x86_32/linker.lds.in",
                   Out arch "/kernel/linker.lds"
                 ]
          ]
