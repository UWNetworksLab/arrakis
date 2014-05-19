/** \file
 * \brief Header for generic GDB stub code.
 */

/*
 * Copyright (c) 2007, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

void gdb_handle_exception(int vector, uintptr_t *save_area)
    __attribute__((noreturn));
void gdb_stub_entry(int signal_code, char * OPT NTS init_message)
    __attribute__((noreturn));

int gdb_arch_get_register(int regnum, uintptr_t * NONNULL SAFE value);
int gdb_arch_set_register(int regnum, uintptr_t value);
int gdb_arch_write_byte(uint8_t * NONNULL SAFE addr, uint8_t val);
int gdb_arch_read_byte(uint8_t * NONNULL SAFE addr, uint8_t * NONNULL SAFE val);
void gdb_arch_single_step(lvaddr_t addr);
void gdb_arch_continue(lvaddr_t addr);
