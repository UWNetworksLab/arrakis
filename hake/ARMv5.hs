--------------------------------------------------------------------------
-- Copyright (c) 2007-2010, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
--
-- Architectural definitions for Barrelfish on ARMv5 ISA.
--
-- The build target is the integratorcp board on QEMU with the default
-- ARM926EJ-S cpu.
--
--------------------------------------------------------------------------

module ARMv5 where

import HakeTypes
import Path
import qualified Config
import qualified ArchDefaults

-------------------------------------------------------------------------
--
-- Architecture specific definitions for ARMv5
--
-------------------------------------------------------------------------

arch = "armv5"
archFamily = "arm"

compiler = "arm-none-linux-gnueabi-gcc"
objcopy  = "arm-none-linux-gnueabi-objcopy"
objdump  = "arm-none-linux-gnueabi-objdump"
ar       = "arm-none-linux-gnueabi-ar"
ranlib   = "arm-none-linux-gnueabi-ranlib"
cxxcompiler = "arm-none-linux-gnueabi-g++"

ourCommonFlags = [ Str "-fno-unwind-tables",
                   Str "-Wno-packed-bitfield-compat",
                   Str "-mcpu=arm926ej-s",
                   Str "-mapcs",
                   Str "-mabi=aapcs-linux",
                   Str "-msingle-pic-base",
                   Str "-mpic-register=r10",
                   Str "-DPIC_REGISTER=R10",
                   Str "-fpic",
                   Str "-ffixed-r9",
                   Str "-DTHREAD_REGISTER=R9",
                   Str "-D__ARM_ARCH_5__",
		   Str "-Wno-unused-but-set-variable",
		   Str "-Wno-format" ]

cFlags = ArchDefaults.commonCFlags 
         ++ ArchDefaults.commonFlags
         ++ ourCommonFlags

cxxFlags = ArchDefaults.commonCxxFlags
           ++ ArchDefaults.commonFlags
           ++ ourCommonFlags

cDefines = ArchDefaults.cDefines options

ourLdFlags = [ Str "-Wl,-section-start,.text=0x400000",
               Str "-Wl,-section-start,.data=0x600000" ]

ldFlags = ArchDefaults.ldFlags arch ++ ourLdFlags
ldCxxFlags = ArchDefaults.ldCxxFlags arch ++ ourLdFlags

stdLibs = ArchDefaults.stdLibs arch ++ [ Str "-lgcc" ]

options = (ArchDefaults.options arch archFamily) { 
            optFlags = cFlags,
            optCxxFlags = cxxFlags,
            optDefines = cDefines,
            optDependencies = 
                [ PreDep InstallTree arch "/include/errors/errno.h",
                  PreDep InstallTree arch "/include/barrelfish_kpi/capbits.h",
                  PreDep InstallTree arch "/include/asmoffsets.h", 
                  PreDep InstallTree arch "/include/romfs_size.h" ],
            optLdFlags = ldFlags,
            optLdCxxFlags = ldCxxFlags,
            optLibs = stdLibs,
            optInterconnectDrivers = ["lmp"],
            optFlounderBackends = ["lmp"]
          }

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
-- The kernel is "different"
--

kernelCFlags = [ Str s | s <- [ "-fno-builtin",
                                "-fno-unwind-tables",
                                "-nostdinc",
                                "-std=c99",
                                "-mcpu=arm926ej-s",
                                "-mapcs",
                                "-mabi=aapcs-linux",
                                "-fPIE",
                                "-U__linux__",
                                "-Wall",
                                "-Wshadow",
                                "-Wstrict-prototypes",
                                "-Wold-style-definition",
                                "-Wmissing-prototypes",
                                "-Wmissing-declarations",
                                "-Wmissing-field-initializers",
                                "-Wredundant-decls",
                                "-Werror",
                                "-imacros deputy/nodeputy.h",
                                "-fpie",
                                "-fno-stack-check",
                                "-ffreestanding",
                                "-fomit-frame-pointer",
                                "-mno-long-calls",
                                "-Wmissing-noreturn",
                                "-mno-apcs-stack-check",
                                "-mno-apcs-reentrant",
                                "-msingle-pic-base",
                                "-mpic-register=r10",
                                "-DPIC_REGISTER=R10",
                                "-ffixed-r9",
                                "-DTHREAD_REGISTER=R9",
                                "-D__ARM_ARCH_5__",
				"-Wno-unused-but-set-variable",
				"-Wno-format" ]]

kernelLdFlags = [ Str "-Wl,-N",
                  NStr "-Wl,-Map,", Out arch "kernel.map",
                  Str "-fno-builtin",
                  Str "-nostdlib",
                  Str "-Wl,--fatal-warnings"
                ]


--
-- Link the kernel (CPU Driver)
--
linkKernel :: Options -> [String] -> [String] -> String -> HRule
linkKernel opts objs libs kbin =
    let linkscript = "/kernel/linker.lds"
        kbootable  = kbin ++ ".bin"
    in
        Rules [ Rule ([ Str compiler, Str Config.cOptFlags,
                      NStr "-T", In BuildTree arch linkscript,
                      Str "-o", Out arch kbin
                    ]
                    ++ (optLdFlags opts)
                    ++
                    [ In BuildTree arch o | o <- objs ]
                    ++
                    [ In BuildTree arch l | l <- libs ]
                    ++
                    [ Str "-lgcc" ]
                   ),
              -- Edit ELF header so qemu-system-arm will treat it as a Linux kernel
              Rule [ In SrcTree "src" "/tools/arm-mkbootelf.sh",
                     Str objdump, In BuildTree arch kbin, Out arch (kbootable)],
              -- Generate kernel assembly dump
              Rule [ Str (objdump ++ " -d -M reg-names-raw"),
                    In BuildTree arch kbin, Str ">", Out arch (kbin ++ ".asm")],
              Rule [ Str "cpp",
                     NStr "-I", NoDep SrcTree "src" "/kernel/include/arch/armv5",
                     Str "-D__ASSEMBLER__",
                     Str "-P", In SrcTree "src" "/kernel/arch/armv5/linker.lds.in",
                     Out arch linkscript
                   ]
            ]
