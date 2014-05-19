/* 
 * M5 boot image tool.  
 *
 * Molly: a fish that starts with an "M" and has 5 letters in it.
 *
 * This file processes a menu.lst to generate C definitions for the 
 * multiboot structures.  It refers to additional object files
 * which contain the contents of the kernel and modules as blobs of
 * data.  (Generated using objcopy via build_data_files.sh)
 *
 * The resulting files are then linked against molly_boot.S, 
 * molly_init.c, and an ELF loader.  This results in a single kernel
 * image that M5 can boot.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include "elf.h"

#define MAX_MODULES 256

static char *kernel_symbol_prefix;
static char *kernel_cmd_line;
static char *module_symbol_prefix[MAX_MODULES];
static char *module_cmd_line[MAX_MODULES];

static char *get_symbol_name_prefix(char *original) {
  char *prefix = "_binary__";
  char *r = malloc(strlen(prefix) + strlen(original) + 1);
  sprintf(r, "%s%s", prefix, original);
  for (int i = 0; i < strlen(r); i ++) {
    if (r[i] == '/') {
      r[i] = '_';
    }
  }
  return r;
}

int main(int argc, char *argv[])
{
  int got_kernel = 0;
  int n_modules = 0;
  int n_mmaps = 0;
  
  if(argc < 3) {
    printf("Usage: %s <menu.lst> <output.c>\n", argv[0]);
    return 0;
  }

  FILE *f = fopen(argv[1], "r");
  assert((f != NULL) && "Could not open input file");
  
  FILE *o = fopen(argv[2], "w");
  assert((o != NULL) && "Could not open output file");
  
  // Output initial part of file: include basic types etc.
  fprintf(o, "#include <assert.h>\n");
  fprintf(o, "#include <stdio.h>\n");
  fprintf(o, "#include <stdint.h>\n");
  fprintf(o, "#include <stddef.h>\n");
  fprintf(o, "#include <string.h>\n");
  fprintf(o, "#include <barrelfish_kpi/types.h>\n");
  fprintf(o, "#include <errors/errno.h>\n");
  fprintf(o, "#include <elf/elf.h>\n");
  fprintf(o, "#include \"../kernel/include/multiboot.h\"\n");

  // Process menu.lst, generating definitions
  char cmd[1024], args[1024], image[1024];
  while(!feof(f)) {
    char line[1024];
    
    cmd[0] = args[0] = image[0] = line[0] = '\0';
    
    fgets(line, 1024, f);
    sscanf(line, "%s %s %[^\n]", cmd, image, args);
    
    if(!strcmp(cmd, "kernel")) {
      kernel_symbol_prefix = get_symbol_name_prefix(image);
      kernel_cmd_line = malloc(strlen(line) + 1);
      sprintf(kernel_cmd_line, "%s %s", image, args);
      fprintf(o, "extern char %s_start;\n", kernel_symbol_prefix);
      fprintf(o, "extern char %s_end;\n", kernel_symbol_prefix);
      got_kernel = 1;
    } else if(!strcmp(cmd, "module")) {
      assert(n_modules < MAX_MODULES);
      module_symbol_prefix[n_modules] = get_symbol_name_prefix(image);
      module_cmd_line[n_modules] = malloc(strlen(line) + 1);
      sprintf(module_cmd_line[n_modules], "%s %s", image, args);
      fprintf(o, "extern char %s_start;\n", module_symbol_prefix[n_modules]);
      fprintf(o, "extern char %s_end;\n", module_symbol_prefix[n_modules]);
      n_modules ++;
    } else if(!strcmp(cmd, "mmap")) {
      uint64_t base, len;
      int type;
      sscanf(args, "%" SCNi64 " %" SCNi64 " %i",
             &base, &len, &type);
      printf("Inserting MMAP %d: [0x%" PRIx64 ", 0x%" PRIx64 "], type %d\n",
             n_mmaps, base, len, type);
      fprintf(o, "static uint64_t mbi_mmap%d[] = {0x%llx, 0x%llx, %d};\n", 
              n_mmaps, base, len, type);        
      n_mmaps ++;
    } else {
      bool iscmd = false;
      for(int i = 0; i < strlen(cmd); i++) {
        if(cmd[i] == '#') {
          break;
        }
        if(!isspace(cmd[i])) {
          iscmd = true;
          break;
        }
      }
      if(iscmd) {
        printf("Ignoring command '%s'\n", cmd);
      }
    }
  }

  // Generate multiboot-structure initialization code
  fprintf(o, "static struct multiboot_modinfo mbi_mods[%d];\n", n_modules + 1);
  fprintf(o, "static struct multiboot_mmap mbi_mmaps[%d];\n", n_mmaps);
  fprintf(o, "static struct multiboot_info mbi;\n\n");
  fprintf(o, "struct multiboot_info *molly_get_mbi(void) {\n");

  // Flags:
  fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_CMDLINE;\n");
  fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MODS;\n");
  fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS;\n");
  fprintf(o, "  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MMAP;\n");

  // Kernel command line:
  fprintf(o, "  mbi.cmdline = (uint32_t)(uint64_t) \"%s\";\n", kernel_cmd_line);

  // Modules:
  fprintf(o, "  mbi.mods_count = %d;\n", n_modules + 1);
  fprintf(o, "  mbi.mods_addr = (uint32_t)(uint64_t) mbi_mods;\n");
  fprintf(o, "  mbi_mods[0].mod_start = (uint32_t)(uint64_t) &%s_start;\n",
	  kernel_symbol_prefix);
  fprintf(o, "  mbi_mods[0].mod_end = (uint32_t)(uint64_t) &%s_end;\n",
	  kernel_symbol_prefix);
  fprintf(o, "  mbi_mods[0].string = (uint32_t)(uint64_t) \"%s\";\n",
	  kernel_cmd_line);

  for (int i = 0; i < n_modules; i ++) {
    fprintf(o, "  mbi_mods[%d].mod_start = (uint32_t)(uint64_t) &%s_start;\n",
	    i+1, module_symbol_prefix[i]);
    fprintf(o, "  mbi_mods[%d].mod_end = (uint32_t)(uint64_t) &%s_end;\n", 
	    i+1, module_symbol_prefix[i]);
    fprintf(o, "  mbi_mods[%d].string = (uint32_t)(uint64_t) \"%s\";\n", 
	    i+1, module_cmd_line[i]);
  }

  // MMAPS:
  fprintf(o, "  mbi.mmap_length = sizeof(mbi_mmaps);\n", n_mmaps);
  fprintf(o, "  mbi.mmap_addr = (uint32_t)(uint64_t) mbi_mmaps;\n");
  for (int i = 0; i < n_mmaps; i ++) {
    fprintf(o, "  mbi_mmaps[%d].size = sizeof(struct multiboot_mmap);\n", i);
    fprintf(o, "  mbi_mmaps[%d].base_addr = mbi_mmap%d[0];\n", i, i);
    fprintf(o, "  mbi_mmaps[%d].length = mbi_mmap%d[1];\n", i, i);
    fprintf(o, "  mbi_mmaps[%d].type = (int)mbi_mmap%d[2];\n", i, i);
  }
  fprintf(o, "  return &mbi;\n");
  fprintf(o, "}\n\n");
  
  fclose(f);
  fclose(o);
  
  return 0;
}
