/** \file
 * \brief ELF file format definitions
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*-
 * Copyright (c) 1998 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $FreeBSD: src/sys/sys/elf_common.h,v 1.23 2007/12/02 00:05:18 jb Exp $
 */

#ifndef ELF_H
#define ELF_H

#include <sys/cdefs.h>

__BEGIN_DECLS

/* Indexes into the e_ident array.  Keep synced with
   http://www.sco.com/developers/gabi/latest/ch4.eheader.html */
#define EI_MAG0         0       /* Magic number, byte 0. */
#define EI_MAG1         1       /* Magic number, byte 1. */
#define EI_MAG2         2       /* Magic number, byte 2. */
#define EI_MAG3         3       /* Magic number, byte 3. */
#define EI_CLASS        4       /* Class of machine. */
#define EI_DATA         5       /* Data format. */
#define EI_VERSION      6       /* ELF format version. */
#define EI_OSABI        7       /* Operating system / ABI identification */
#define EI_ABIVERSION   8       /* ABI version */
#define OLD_EI_BRAND    8       /* Start of architecture identification. */
#define EI_PAD          9       /* Start of padding (per SVR4 ABI). */
#define EI_NIDENT       16      /* Size of e_ident array. */

/* Values for the magic number bytes. */
#define ELFMAG0         0x7f
#define ELFMAG1         'E'
#define ELFMAG2         'L'
#define ELFMAG3         'F'
#define ELFMAG          "\177ELF"       /* magic string */
#define SELFMAG         4               /* magic string size */

/* Values for e_ident[EI_VERSION] and e_version. */
#define EV_NONE         0
#define EV_CURRENT      1

/* Values for e_ident[EI_CLASS]. */
#define ELFCLASSNONE    0       /* Unknown class. */
#define ELFCLASS32      1       /* 32-bit architecture. */
#define ELFCLASS64      2       /* 64-bit architecture. */

/* Values for e_ident[EI_DATA]. */
#define ELFDATANONE     0       /* Unknown data format. */
#define ELFDATA2LSB     1       /* 2's complement little-endian. */
#define ELFDATA2MSB     2       /* 2's complement big-endian. */

/* Values for e_ident[EI_OSABI]. */
#define ELFOSABI_NONE           0       /* UNIX System V ABI */
#define ELFOSABI_HPUX           1       /* HP-UX operating system */
#define ELFOSABI_NETBSD         2       /* NetBSD */
#define ELFOSABI_LINUX          3       /* GNU/Linux */
#define ELFOSABI_HURD           4       /* GNU/Hurd */
#define ELFOSABI_86OPEN         5       /* 86Open common IA32 ABI */
#define ELFOSABI_SOLARIS        6       /* Solaris */
#define ELFOSABI_AIX            7       /* AIX */
#define ELFOSABI_IRIX           8       /* IRIX */
#define ELFOSABI_FREEBSD        9       /* FreeBSD */
#define ELFOSABI_TRU64          10      /* TRU64 UNIX */
#define ELFOSABI_MODESTO        11      /* Novell Modesto */
#define ELFOSABI_OPENBSD        12      /* OpenBSD */
#define ELFOSABI_OPENVMS        13      /* Open VMS */
#define ELFOSABI_NSK            14      /* HP Non-Stop Kernel */
#define ELFOSABI_ARM            97      /* ARM */
#define ELFOSABI_STANDALONE     255     /* Standalone (embedded) application */

#define ELFOSABI_SYSV           ELFOSABI_NONE   /* symbol used in old spec */
#define ELFOSABI_MONTEREY       ELFOSABI_AIX    /* Monterey */

/* e_ident */
#define IS_ELF(ehdr)    ((ehdr).e_ident[EI_MAG0] == ELFMAG0 && \
                         (ehdr).e_ident[EI_MAG1] == ELFMAG1 && \
                         (ehdr).e_ident[EI_MAG2] == ELFMAG2 && \
                         (ehdr).e_ident[EI_MAG3] == ELFMAG3)

/* Values for e_type. */
#define ET_NONE         0       /* Unknown type. */
#define ET_REL          1       /* Relocatable. */
#define ET_EXEC         2       /* Executable. */
#define ET_DYN          3       /* Shared object. */
#define ET_CORE         4       /* Core file. */
#define ET_LOOS         0xfe00  /* First operating system specific. */
#define ET_HIOS         0xfeff  /* Last operating system-specific. */
#define ET_LOPROC       0xff00  /* First processor-specific. */
#define ET_HIPROC       0xffff  /* Last processor-specific. */

/* Values for e_machine. */
#define EM_NONE         0       /* Unknown machine. */
#define EM_M32          1       /* AT&T WE32100. */
#define EM_SPARC        2       /* Sun SPARC. */
#define EM_386          3       /* Intel i386. */
#define EM_68K          4       /* Motorola 68000. */
#define EM_88K          5       /* Motorola 88000. */
#define EM_860          7       /* Intel i860. */
#define EM_MIPS         8       /* MIPS R3000 Big-Endian only. */
#define EM_S370         9       /* IBM System/370. */
#define EM_MIPS_RS3_LE  10      /* MIPS R3000 Little-Endian. */
#define EM_PARISC       15      /* HP PA-RISC. */
#define EM_VPP500       17      /* Fujitsu VPP500. */
#define EM_SPARC32PLUS  18      /* SPARC v8plus. */
#define EM_960          19      /* Intel 80960. */
#define EM_PPC          20      /* PowerPC 32-bit. */
#define EM_PPC64        21      /* PowerPC 64-bit. */
#define EM_S390         22      /* IBM System/390. */
#define EM_V800         36      /* NEC V800. */
#define EM_FR20         37      /* Fujitsu FR20. */
#define EM_RH32         38      /* TRW RH-32. */
#define EM_RCE          39      /* Motorola RCE. */
#define EM_ARM          40      /* ARM. */
#define EM_SH           42      /* Hitachi SH. */
#define EM_SPARCV9      43      /* SPARC v9 64-bit. */
#define EM_TRICORE      44      /* Siemens TriCore embedded processor. */
#define EM_ARC          45      /* Argonaut RISC Core. */
#define EM_H8_300       46      /* Hitachi H8/300. */
#define EM_H8_300H      47      /* Hitachi H8/300H. */
#define EM_H8S          48      /* Hitachi H8S. */
#define EM_H8_500       49      /* Hitachi H8/500. */
#define EM_IA_64        50      /* Intel IA-64 Processor. */
#define EM_MIPS_X       51      /* Stanford MIPS-X. */
#define EM_COLDFIRE     52      /* Motorola ColdFire. */
#define EM_68HC12       53      /* Motorola M68HC12. */
#define EM_MMA          54      /* Fujitsu MMA. */
#define EM_PCP          55      /* Siemens PCP. */
#define EM_NCPU         56      /* Sony nCPU. */
#define EM_NDR1         57      /* Denso NDR1 microprocessor. */
#define EM_STARCORE     58      /* Motorola Star*Core processor. */
#define EM_ME16         59      /* Toyota ME16 processor. */
#define EM_ST100        60      /* STMicroelectronics ST100 processor. */
#define EM_TINYJ        61      /* Advanced Logic Corp. TinyJ processor. */
#define EM_X86_64       62      /* Advanced Micro Devices x86-64 */
#define EM_AMD64        EM_X86_64       /* Advanced Micro Devices x86-64 (compat) */

/* Non-standard or deprecated. */
#define EM_486          6       /* Intel i486. */
#define EM_MIPS_RS4_BE  10      /* MIPS R4000 Big-Endian */
#define EM_ALPHA_STD    41      /* Digital Alpha (standard value). */
#define EM_ALPHA        0x9026  /* Alpha (written in the absence of an ABI) */

/* Special section indexes. */
#define SHN_UNDEF            0          /* Undefined, missing, irrelevant. */
#define SHN_LORESERVE   0xff00          /* First of reserved range. */
#define SHN_LOPROC      0xff00          /* First processor-specific. */
#define SHN_HIPROC      0xff1f          /* Last processor-specific. */
#define SHN_LOOS        0xff20          /* First operating system-specific. */
#define SHN_HIOS        0xff3f          /* Last operating system-specific. */
#define SHN_ABS         0xfff1          /* Absolute values. */
#define SHN_COMMON      0xfff2          /* Common data. */
#define SHN_XINDEX      0xffff          /* Escape -- index stored elsewhere. */
#define SHN_HIRESERVE   0xffff          /* Last of reserved range. */

/* sh_type */
#define SHT_NULL                0       /* inactive */
#define SHT_PROGBITS            1       /* program defined information */
#define SHT_SYMTAB              2       /* symbol table section */
#define SHT_STRTAB              3       /* string table section */
#define SHT_RELA                4       /* relocation section with addends */
#define SHT_HASH                5       /* symbol hash table section */
#define SHT_DYNAMIC             6       /* dynamic section */
#define SHT_NOTE                7       /* note section */
#define SHT_NOBITS              8       /* no space section */
#define SHT_REL                 9       /* relocation section - no addends */
#define SHT_SHLIB               10      /* reserved - purpose unknown */
#define SHT_DYNSYM              11      /* dynamic symbol table section */
#define SHT_INIT_ARRAY          14      /* Initialization function pointers. */
#define SHT_FINI_ARRAY          15      /* Termination function pointers. */
#define SHT_PREINIT_ARRAY       16      /* Pre-initialization function ptrs. */
#define SHT_GROUP               17      /* Section group. */
#define SHT_SYMTAB_SHNDX        18      /* Section indexes (see SHN_XINDEX). */
#define SHT_LOOS                0x60000000      /* First of OS specific semantics */
#define SHT_HIOS                0x6fffffff      /* Last of OS specific semantics */
#define SHT_LOPROC              0x70000000      /* reserved range for processor */
#define SHT_AMD64_UNWIND        0x70000001      /* unwind information */
#define SHT_HIPROC              0x7fffffff      /* specific section header types */
#define SHT_LOUSER              0x80000000      /* reserved range for application */
#define SHT_HIUSER              0xffffffff      /* specific indexes */

/* Flags for sh_flags. */
#define SHF_WRITE               0x1     /* Section contains writable data. */
#define SHF_ALLOC               0x2     /* Section occupies memory. */
#define SHF_EXECINSTR           0x4     /* Section contains instructions. */
#define SHF_MERGE               0x10    /* Section may be merged. */
#define SHF_STRINGS             0x20    /* Section contains strings. */
#define SHF_INFO_LINK           0x40    /* sh_info holds section index. */
#define SHF_LINK_ORDER          0x80    /* Special ordering requirements. */
#define SHF_OS_NONCONFORMING    0x100   /* OS-specific processing required. */
#define SHF_GROUP               0x200   /* Member of section group. */
#define SHF_TLS                 0x400   /* Section contains TLS data. */
#define SHF_MASKOS      0x0ff00000      /* OS-specific semantics. */
#define SHF_MASKPROC    0xf0000000      /* Processor-specific semantics. */

/* Values for p_type. */
#define PT_NULL         0       /* Unused entry. */
#define PT_LOAD         1       /* Loadable segment. */
#define PT_DYNAMIC      2       /* Dynamic linking information segment. */
#define PT_INTERP       3       /* Pathname of interpreter. */
#define PT_NOTE         4       /* Auxiliary information. */
#define PT_SHLIB        5       /* Reserved (not used). */
#define PT_PHDR         6       /* Location of program header itself. */
#define PT_TLS          7       /* Thread local storage segment */
#define PT_LOOS         0x60000000      /* First OS-specific. */
#define PT_SUNW_UNWIND  0x6464e550      /* amd64 UNWIND program header */
#define PT_GNU_EH_FRAME 0x6474e550
#define PT_LOSUNW       0x6ffffffa
#define PT_SUNWBSS      0x6ffffffa      /* Sun Specific segment */
#define PT_SUNWSTACK    0x6ffffffb      /* describes the stack segment */
#define PT_SUNWDTRACE   0x6ffffffc      /* private */
#define PT_SUNWCAP      0x6ffffffd      /* hard/soft capabilities segment */
#define PT_HISUNW       0x6fffffff
#define PT_HIOS         0x6fffffff      /* Last OS-specific. */
#define PT_LOPROC       0x70000000      /* First processor-specific type. */
#define PT_HIPROC       0x7fffffff      /* Last processor-specific type. */

/* Values for p_flags. */
#define PF_X            0x1             /* Executable. */
#define PF_W            0x2             /* Writable. */
#define PF_R            0x4             /* Readable. */
#define PF_MASKOS       0x0ff00000      /* Operating system-specific. */
#define PF_MASKPROC     0xf0000000      /* Processor-specific. */

/* Extended program header index. */
#define PN_XNUM         0xffff

/* Values for d_tag. */
#define DT_NULL         0       /* Terminating entry. */
#define DT_NEEDED       1       /* String table offset of a needed shared
                                   library. */
#define DT_PLTRELSZ     2       /* Total size in bytes of PLT relocations. */
#define DT_PLTGOT       3       /* Processor-dependent address. */
#define DT_HASH         4       /* Address of symbol hash table. */
#define DT_STRTAB       5       /* Address of string table. */
#define DT_SYMTAB       6       /* Address of symbol table. */
#define DT_RELA         7       /* Address of ElfNN_Rela relocations. */
#define DT_RELASZ       8       /* Total size of ElfNN_Rela relocations. */
#define DT_RELAENT      9       /* Size of each ElfNN_Rela relocation entry. */
#define DT_STRSZ        10      /* Size of string table. */
#define DT_SYMENT       11      /* Size of each symbol table entry. */
#define DT_INIT         12      /* Address of initialization function. */
#define DT_FINI         13      /* Address of finalization function. */
#define DT_SONAME       14      /* String table offset of shared object
                                   name. */
#define DT_RPATH        15      /* String table offset of library path. [sup] */
#define DT_SYMBOLIC     16      /* Indicates "symbolic" linking. [sup] */
#define DT_REL          17      /* Address of ElfNN_Rel relocations. */
#define DT_RELSZ        18      /* Total size of ElfNN_Rel relocations. */
#define DT_RELENT       19      /* Size of each ElfNN_Rel relocation. */
#define DT_PLTREL       20      /* Type of relocation used for PLT. */
#define DT_DEBUG        21      /* Reserved (not used). */
#define DT_TEXTREL      22      /* Indicates there may be relocations in
                                   non-writable segments. [sup] */
#define DT_JMPREL       23      /* Address of PLT relocations. */
#define DT_BIND_NOW     24      /* [sup] */
#define DT_INIT_ARRAY   25      /* Address of the array of pointers to
                                   initialization functions */
#define DT_FINI_ARRAY   26      /* Address of the array of pointers to
                                   termination functions */
#define DT_INIT_ARRAYSZ 27      /* Size in bytes of the array of
                                   initialization functions. */
#define DT_FINI_ARRAYSZ 28      /* Size in bytes of the array of
                                   terminationfunctions. */
#define DT_RUNPATH      29      /* String table offset of a null-terminated
                                   library search path string. */
#define DT_FLAGS        30      /* Object specific flag values. */
#define DT_ENCODING     32      /* Values greater than or equal to DT_ENCODING
                                   and less than DT_LOOS follow the rules for
                                   the interpretation of the d_un union
                                   as follows: even == 'd_ptr', even == 'd_val'
                                   or none */
#define DT_PREINIT_ARRAY 32     /* Address of the array of pointers to
                                   pre-initialization functions. */
#define DT_PREINIT_ARRAYSZ 33   /* Size in bytes of the array of
                                   pre-initialization functions. */
#define DT_MAXPOSTAGS   34      /* number of positive tags */
#define DT_LOOS         0x6000000d      /* First OS-specific */
#define DT_SUNW_AUXILIARY       0x6000000d      /* symbol auxiliary name */
#define DT_SUNW_RTLDINF         0x6000000e      /* ld.so.1 info (private) */
#define DT_SUNW_FILTER          0x6000000f      /* symbol filter name */
#define DT_SUNW_CAP             0x60000010      /* hardware/software */
#define DT_HIOS         0x6ffff000      /* Last OS-specific */

/*
 * DT_* entries which fall between DT_VALRNGHI & DT_VALRNGLO use the
 * Dyn.d_un.d_val field of the Elf*_Dyn structure.
 */
#define DT_VALRNGLO     0x6ffffd00
#define DT_CHECKSUM     0x6ffffdf8      /* elf checksum */
#define DT_PLTPADSZ     0x6ffffdf9      /* pltpadding size */
#define DT_MOVEENT      0x6ffffdfa      /* move table entry size */
#define DT_MOVESZ       0x6ffffdfb      /* move table size */
#define DT_FEATURE_1    0x6ffffdfc      /* feature holder */
#define DT_POSFLAG_1    0x6ffffdfd      /* flags for DT_* entries, effecting */
                                        /*      the following DT_* entry. */
                                        /*      See DF_P1_* definitions */
#define DT_SYMINSZ      0x6ffffdfe      /* syminfo table size (in bytes) */
#define DT_SYMINENT     0x6ffffdff      /* syminfo entry size (in bytes) */
#define DT_VALRNGHI     0x6ffffdff

/*
 * DT_* entries which fall between DT_ADDRRNGHI & DT_ADDRRNGLO use the
 * Dyn.d_un.d_ptr field of the Elf*_Dyn structure.
 *
 * If any adjustment is made to the ELF object after it has been
 * built, these entries will need to be adjusted.
 */
#define DT_ADDRRNGLO    0x6ffffe00
#define DT_CONFIG       0x6ffffefa      /* configuration information */
#define DT_DEPAUDIT     0x6ffffefb      /* dependency auditing */
#define DT_AUDIT        0x6ffffefc      /* object auditing */
#define DT_PLTPAD       0x6ffffefd      /* pltpadding (sparcv9) */
#define DT_MOVETAB      0x6ffffefe      /* move table */
#define DT_SYMINFO      0x6ffffeff      /* syminfo table */
#define DT_ADDRRNGHI    0x6ffffeff

#define DT_VERSYM       0x6ffffff0      /* Address of versym section. */
#define DT_RELACOUNT    0x6ffffff9      /* number of RELATIVE relocations */
#define DT_RELCOUNT     0x6ffffffa      /* number of RELATIVE relocations */
#define DT_FLAGS_1      0x6ffffffb      /* state flags - see DF_1_* defs */
#define DT_VERDEF       0x6ffffffc      /* Address of verdef section. */
#define DT_VERDEFNUM    0x6ffffffd      /* Number of elems in verdef section */
#define DT_VERNEED      0x6ffffffe      /* Address of verneed section. */
#define DT_VERNEEDNUM   0x6fffffff      /* Number of elems in verneed section */

#define DT_LOPROC       0x70000000      /* First processor-specific type. */
#define DT_DEPRECATED_SPARC_REGISTER    0x7000001
#define DT_AUXILIARY    0x7ffffffd      /* shared library auxiliary name */
#define DT_USED         0x7ffffffe      /* ignored - same as needed */
#define DT_FILTER       0x7fffffff      /* shared library filter name */
#define DT_HIPROC       0x7fffffff      /* Last processor-specific type. */

/* Values for DT_FLAGS */
#define DF_ORIGIN       0x0001  /* Indicates that the object being loaded may
                                   make reference to the $ORIGIN substitution
                                   string */
#define DF_SYMBOLIC     0x0002  /* Indicates "symbolic" linking. */
#define DF_TEXTREL      0x0004  /* Indicates there may be relocations in
                                   non-writable segments. */
#define DF_BIND_NOW     0x0008  /* Indicates that the dynamic linker should
                                   process all relocations for the object
                                   containing this entry before transferring
                                   control to the program. */
#define DF_STATIC_TLS   0x0010  /* Indicates that the shared object or
                                   executable contains code using a static
                                   thread-local storage scheme. */

/* Values for n_type.  Used in core files. */
#define NT_PRSTATUS     1       /* Process status. */
#define NT_FPREGSET     2       /* Floating point registers. */
#define NT_PRPSINFO     3       /* Process state info. */

/* Symbol Binding - ELFNN_ST_BIND - st_info */
#define STB_LOCAL       0       /* Local symbol */
#define STB_GLOBAL      1       /* Global symbol */
#define STB_WEAK        2       /* like global - lower precedence */
#define STB_LOOS        10      /* Reserved range for operating system */
#define STB_HIOS        12      /*   specific semantics. */
#define STB_LOPROC      13      /* reserved range for processor */
#define STB_HIPROC      15      /*   specific semantics. */

/* Symbol type - ELFNN_ST_TYPE - st_info */
#define STT_NOTYPE      0       /* Unspecified type. */
#define STT_OBJECT      1       /* Data object. */
#define STT_FUNC        2       /* Function. */
#define STT_SECTION     3       /* Section. */
#define STT_FILE        4       /* Source file. */
#define STT_COMMON      5       /* Uninitialized common block. */
#define STT_TLS         6       /* TLS object. */
#define STT_NUM         7
#define STT_LOOS        10      /* Reserved range for operating system */
#define STT_HIOS        12      /*   specific semantics. */
#define STT_LOPROC      13      /* reserved range for processor */
#define STT_HIPROC      15      /*   specific semantics. */

/* Symbol visibility - ELFNN_ST_VISIBILITY - st_other */
#define STV_DEFAULT     0x0     /* Default visibility (see binding). */
#define STV_INTERNAL    0x1     /* Special meaning in relocatable objects. */
#define STV_HIDDEN      0x2     /* Not visible. */
#define STV_PROTECTED   0x3     /* Visible but not preemptible. */

/* Special symbol table indexes. */
#define STN_UNDEF       0       /* Undefined symbol index. */

/* Symbol versioning flags. */
#define VER_DEF_CURRENT 1
#define VER_DEF_IDX(x)  VER_NDX(x)

#define VER_FLG_BASE    0x01
#define VER_FLG_WEAK    0x02

#define VER_NEED_CURRENT        1
#define VER_NEED_WEAK   (1u << 15)
#define VER_NEED_HIDDEN VER_NDX_HIDDEN
#define VER_NEED_IDX(x) VER_NDX(x)

#define VER_NDX_LOCAL   0
#define VER_NDX_GLOBAL  1
#define VER_NDX_GIVEN   2

#define VER_NDX_HIDDEN  (1u << 15)
#define VER_NDX(x)      ((x) & ~(1u << 15))

#define CA_SUNW_NULL    0
#define CA_SUNW_HW_1    1               /* first hardware capabilities entry */
#define CA_SUNW_SF_1    2               /* first software capabilities entry */

/*
 * Syminfo flag values
 */
#define SYMINFO_FLG_DIRECT      0x0001  /* symbol ref has direct association */
                                        /*      to object containing defn. */
#define SYMINFO_FLG_PASSTHRU    0x0002  /* ignored - see SYMINFO_FLG_FILTER */
#define SYMINFO_FLG_COPY        0x0004  /* symbol is a copy-reloc */
#define SYMINFO_FLG_LAZYLOAD    0x0008  /* object containing defn should be */
                                        /*      lazily-loaded */
#define SYMINFO_FLG_DIRECTBIND  0x0010  /* ref should be bound directly to */
                                        /*      object containing defn. */
#define SYMINFO_FLG_NOEXTDIRECT 0x0020  /* don't let an external reference */
                                        /*      directly bind to this symbol */
#define SYMINFO_FLG_FILTER      0x0002  /* symbol ref is associated to a */
#define SYMINFO_FLG_AUXILIARY   0x0040  /*      standard or auxiliary filter */

/*
 * Syminfo.si_boundto values.
 */
#define SYMINFO_BT_SELF         0xffff  /* symbol bound to self */
#define SYMINFO_BT_PARENT       0xfffe  /* symbol bound to parent */
#define SYMINFO_BT_NONE         0xfffd  /* no special symbol binding */
#define SYMINFO_BT_EXTERN       0xfffc  /* symbol defined as external */
#define SYMINFO_BT_LOWRESERVE   0xff00  /* beginning of reserved entries */

/*
 * Syminfo version values.
 */
#define SYMINFO_NONE            0       /* Syminfo version */
#define SYMINFO_CURRENT         1
#define SYMINFO_NUM             2

#define ELF64_R_SYM(i)          ((i) >> 32)
#define ELF64_R_TYPE(i)         ((i) & 0xffffffffL)

#define ELF32_R_SYM(i)          ((i)>>8)
#define ELF32_R_TYPE(i)         ((uint8_t)(i))
#define ELF32_R_INFO(s,t)       (((s)<<8)+(uint8_t)(t))

/* AMD x86-64 relocations.  */
#define R_X86_64_NONE           0       /* No relocation. */
#define R_X86_64_64             1       /* Add 64 bit symbol value. */
#define R_X86_64_PC32           2       /* PC-relative 32 bit signed sym value. */
#define R_X86_64_GOT32          3       /* PC-relative 32 bit GOT offset. */
#define R_X86_64_PLT32          4       /* PC-relative 32 bit PLT offset. */
#define R_X86_64_COPY           5       /* Copy data from shared object. */
#define R_X86_64_GLOB_DAT       6       /* Set GOT entry to data address. */
#define R_X86_64_JMP_SLOT       7       /* Set GOT entry to code address. */
#define R_X86_64_RELATIVE       8       /* Add load address of shared object. */
#define R_X86_64_GOTPCREL       9       /* Add 32 bit signed pcrel offset to GOT. */
#define R_X86_64_32             10      /* Add 32 bit zero extended symbol value */
#define R_X86_64_32S            11      /* Add 32 bit sign extended symbol value */
#define R_X86_64_16             12      /* Add 16 bit zero extended symbol value */
#define R_X86_64_PC16           13      /* Add 16 bit signed extended pc relative symbol value */
#define R_X86_64_8              14      /* Add 8 bit zero extended symbol value */
#define R_X86_64_PC8            15      /* Add 8 bit signed extended pc relative symbol value */
#define R_X86_64_DTPMOD64       16      /* ID of module containing symbol */
#define R_X86_64_DTPOFF64       17      /* Offset in TLS block */
#define R_X86_64_TPOFF64        18      /* Offset in static TLS block */
#define R_X86_64_TLSGD          19      /* PC relative offset to GD GOT entry */
#define R_X86_64_TLSLD          20      /* PC relative offset to LD GOT entry */
#define R_X86_64_DTPOFF32       21      /* Offset in TLS block */
#define R_X86_64_GOTTPOFF       22      /* PC relative offset to IE GOT entry */
#define R_X86_64_TPOFF32        23      /* Offset in static TLS block */

/* i386 relocations.  */
#define R_386_NONE              0       /* No relocation. */
#define R_386_32                1       /* Add symbol value. */
#define R_386_PC32              2       /* Add PC-relative symbol value. */
#define R_386_GOT32             3       /* Add PC-relative GOT offset. */
#define R_386_PLT32             4       /* Add PC-relative PLT offset. */
#define R_386_COPY              5       /* Copy data from shared object. */
#define R_386_GLOB_DAT          6       /* Set GOT entry to data address. */
#define R_386_JMP_SLOT          7       /* Set GOT entry to code address. */
#define R_386_RELATIVE          8       /* Add load address of shared object. */
#define R_386_GOTOFF            9       /* Add GOT-relative symbol address. */
#define R_386_GOTPC             10      /* Add PC-relative GOT table address. */
#define R_386_TLS_TPOFF         14      /* Negative offset in static TLS block */
#define R_386_TLS_IE            15      /* Absolute address of GOT for -ve static TLS */
#define R_386_TLS_GOTIE         16      /* GOT entry for negative static TLS block */
#define R_386_TLS_LE            17      /* Negative offset relative to static TLS */
#define R_386_TLS_GD            18      /* 32 bit offset to GOT (index,off) pair */
#define R_386_TLS_LDM           19      /* 32 bit offset to GOT (index,zero) pair */
#define R_386_TLS_GD_32         24      /* 32 bit offset to GOT (index,off) pair */
#define R_386_TLS_GD_PUSH       25      /* pushl instruction for Sun ABI GD sequence */
#define R_386_TLS_GD_CALL       26      /* call instruction for Sun ABI GD sequence */
#define R_386_TLS_GD_POP        27      /* popl instruction for Sun ABI GD sequence */
#define R_386_TLS_LDM_32        28      /* 32 bit offset to GOT (index,zero) pair */
#define R_386_TLS_LDM_PUSH      29      /* pushl instruction for Sun ABI LD sequence */
#define R_386_TLS_LDM_CALL      30      /* call instruction for Sun ABI LD sequence */
#define R_386_TLS_LDM_POP       31      /* popl instruction for Sun ABI LD sequence */
#define R_386_TLS_LDO_32        32      /* 32 bit offset from start of TLS block */
#define R_386_TLS_IE_32         33      /* 32 bit offset to GOT static TLS offset entry */
#define R_386_TLS_LE_32         34      /* 32 bit offset within static TLS block */
#define R_386_TLS_DTPMOD32      35      /* GOT entry containing TLS index */
#define R_386_TLS_DTPOFF32      36      /* GOT entry containing TLS offset */
#define R_386_TLS_TPOFF32       37      /* GOT entry of -ve static TLS offset */

/* ARM relocations. A LOT MISSING! */
#define R_ARM_NONE				0
#define R_ARM_ABS32				2
#define R_ARM_RELATIVE			23

/**
 * \brief ELF64 file header.
 */
struct Elf64_Ehdr {
    uint8_t        e_ident[EI_NIDENT];
    uint16_t       e_type;
    uint16_t       e_machine;
    uint32_t       e_version;
    uint64_t       e_entry;
    uint64_t       e_phoff;
    uint64_t       e_shoff;
    uint32_t       e_flags;
    uint16_t       e_ehsize;
    uint16_t       e_phentsize;
    uint16_t       e_phnum;
    uint16_t       e_shentsize;
    uint16_t       e_shnum;
    uint16_t       e_shstrndx;
} __attribute__ ((packed));

/**
 * \brief ELF64 program header.
 */
struct Elf64_Phdr {
    uint32_t        p_type;
    uint32_t        p_flags;
    uint64_t        p_offset;
    uint64_t        p_vaddr;
    uint64_t        p_paddr;
    uint64_t        p_filesz;
    uint64_t        p_memsz;
    uint64_t        p_align;
} __attribute__ ((packed));

/**
 * \brief ELF64 section header.
 */
struct Elf64_Shdr {
    uint32_t    sh_name;
    uint32_t    sh_type;
    uint64_t    sh_flags;
    uint64_t    sh_addr;
    uint64_t    sh_offset;
    uint64_t    sh_size;
    uint32_t    sh_link;
    uint32_t    sh_info;
    uint64_t    sh_addralign;
    uint64_t    sh_entsize;
} __attribute__ ((packed));

/**
 * \brief ELF64 relocation entry.
 */
struct Elf64_Rela {
    uint64_t    r_offset;
    uint64_t    r_info;
    int64_t     r_addend;
} __attribute__ ((packed));

/**
 * \brief ELF64 symbol table entry.
 */
struct Elf64_Sym {
    uint32_t        st_name;
    uint8_t         st_info;
    uint8_t         st_other;
    uint16_t        st_shndx;
    uint64_t        st_value;
    uint64_t        st_size;
} __attribute__ ((packed));

/**
 * \brief ELF64 Dynamic section entry
 */
struct Elf64_Dyn {
    int64_t         d_tag;
    union {
        uint64_t    d_val;
        uint64_t    d_ptr;
    } d_un;
} __attribute__ ((packed));

/**
 * \brief ELF32 file header.
 */
struct Elf32_Ehdr {
    uint8_t  e_ident[EI_NIDENT];        // 00
    uint16_t e_type;                    // 16
    uint16_t e_machine;                 // 18
    uint32_t e_version;                 // 20
    uint32_t e_entry;                   // 24
    uint32_t e_phoff;                   // 28
    uint32_t e_shoff;                   // 32
    uint32_t e_flags;                   // 36
    uint16_t e_ehsize;                  // 40
    uint16_t e_phentsize;               // 42
    uint16_t e_phnum;                   // 44
    uint16_t e_shentsize;               // 46
    uint16_t e_shnum;                   // 48
    uint16_t e_shstrndx;                // 50
};

/**
 * \brief ELF32 program header.
 */
struct  Elf32_Phdr {
    uint32_t p_type;                    // 0
    uint32_t p_offset;                  // 4
    uint32_t p_vaddr;                   // 8
    uint32_t p_paddr;                   // 12
    uint32_t p_filesz;                  // 16
    uint32_t p_memsz;                   // 20
    uint32_t p_flags;                   // 24
    uint32_t p_align;                   // 28
};

/**
 * \brief ELF32 section header.
 */
struct Elf32_Shdr {
    uint32_t sh_name;                   // 0
    uint32_t sh_type;                   // 4
    uint32_t sh_flags;                  // 8
    uint32_t sh_addr;                   // 12
    uint32_t sh_offset;                 // 16
    uint32_t sh_size;                   // 20
    uint32_t sh_link;                   // 24
    uint32_t sh_info;                   // 28
    uint32_t sh_addralign;              // 32
    uint32_t sh_entsize;                // 36
};

/**
 * \brief ELF32 relocation entry.
 */
struct Elf32_Rel {
    uint32_t r_offset;
    uint32_t r_info;
};

/**
 * \brief ELF32 relocation entry with addend.
 */
struct Elf32_Rela {
    uint32_t r_offset;
    uint32_t r_info;
    int32_t  r_addend;
};

/**
 * \brief ELF32 symbol table entry.
 */
struct Elf32_Sym {
    uint32_t st_name;
    uint32_t st_value;
    uint32_t st_size;
    uint8_t  st_info;
    uint8_t  st_other;
    uint16_t st_shndx;
};


struct Elf32_Dyn {
    int32_t          d_tag;
    union {
        uint32_t     d_val;
        uint32_t     d_ptr;
    } d_un;
} __attribute__ ((packed));

struct Elf64_Shdr *
elf64_find_section_header_type(struct Elf64_Shdr *shdr,
                               uint32_t entries, uint32_t type);
struct Elf32_Shdr *
elf32_find_section_header_type(struct Elf32_Shdr *shdr,
                               uint32_t entries, uint32_t type);

struct Elf64_Shdr *
elf64_find_section_header_name(genvaddr_t elf_base, size_t elf_bytes,
                               const char* section_name);
struct Elf32_Shdr *
elf32_find_section_header_name(genvaddr_t elf_base, size_t elf_bytes,
                               const char* section_name);

void elf64_relocate(genvaddr_t dst, genvaddr_t src,
                    struct Elf64_Rela * SAFE NONNULL rela, size_t size,
                    struct Elf64_Sym * SAFE NONNULL symtab, size_t symsize,
                    genvaddr_t start, void *vbase);
void elf32_relocate(genvaddr_t dst, genvaddr_t src,
                    struct Elf32_Rel * SAFE NONNULL rela, size_t size,
                    struct Elf32_Sym * SAFE NONNULL symtab, size_t symsize,
                    genvaddr_t start, void *vbase);

typedef errval_t (*elf_allocator_fn)(void *state, genvaddr_t base,
                                     size_t size, uint32_t flags, void **ret);

errval_t elf64_load(uint16_t em_machine, elf_allocator_fn allocate_func,
                    void *state, lvaddr_t base,
                    size_t size, genvaddr_t *retentry,
                    genvaddr_t *ret_tlsbase, size_t *ret_tlsinitlen,
                    size_t *ret_tlstotallen);
errval_t elf32_load(uint16_t em_machine, elf_allocator_fn allocate_func,
                    void *state, lvaddr_t base,
                    size_t size, genvaddr_t *retentry,
                    genvaddr_t *ret_tlsbase, size_t *ret_tlsinitlen,
                    size_t *ret_tlstotallen);
errval_t elf_load(uint16_t em_machine, elf_allocator_fn allocate_func,
                  void *state, lvaddr_t base,
                  size_t size, genvaddr_t *retentry);

errval_t elf_load_tls(uint16_t em_machine, elf_allocator_fn allocate_func,
                      void *state, lvaddr_t base,
                      size_t size, genvaddr_t *retentry,
                      genvaddr_t *ret_tlsbase, size_t *ret_tlsinitlen,
                      size_t *ret_tlstotallen);

size_t  elf_virtual_size(lvaddr_t base);

genvaddr_t elf_virtual_base32(struct Elf32_Ehdr *ehead);
genvaddr_t elf_virtual_base64(struct Elf64_Ehdr *ehead);
genvaddr_t elf_virtual_base(lvaddr_t base);

__END_DECLS

#endif // ELF_H
