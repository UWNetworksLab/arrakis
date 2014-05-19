/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, ECRC.
 * 
 * END LICENSE BLOCK */

#include "memman.h"

void
panic(what, where)
char *what, *where;
{
	printf("Panic: %s in %s\n", what, where);
	exit(-1);
}

struct heap_descriptor h1, h2;

main()
{
	char *addr = (char *) 0x2180000;
	char *p1, *p2;

char		*shared_mem_init(int create_flag,
			char* mapfile, char* start,
			int size, int increment,
			void (*panic_fct)(),
			struct heap_descriptor *hd);
	if ((int) shared_mem_init(1, "map1", 0, 0, 0, panic, &h1) == -1)
	{
		printf("shared_mem_init 1 failed");
		exit(-1);
	}

	if ((int) shared_mem_init(1, "map2", 0, 0, 0, panic, &h2) == -1)
	{
		printf("shared_mem_init 2 failed");
		exit(-1);
	}

	p1 = (char *) alloc_size(&h1, 50);
	p2 = (char *) alloc_size(&h2, 50);
	free_size(&h1, (void *) p1, 50);
	free_size(&h2, (void *) p2, 50);
	exit(0);
}
