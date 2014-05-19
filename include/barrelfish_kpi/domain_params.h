/**
 * \file
 * \brief Initial parameters passed to a domain.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SPAWN_DOMAIN_PARAMS_H
#define SPAWN_DOMAIN_PARAMS_H

struct spawn_domain_params {
    int argc;           ///< Number of arguments
    const char *argv[MAX_CMDLINE_ARGS + 1]; ///< Command-line arguments; +1 for NULL terminator
    char *envp[MAX_ENVIRON_VARS + 1]; ///< Environment strings; +1 for NULL terminator
    void *vspace_buf;   ///< Serialised vspace data
    size_t vspace_buf_len; ///< Length of serialised vspace data
    void *tls_init_base;        ///< Address of initialised TLS data block
    size_t tls_init_len;        ///< Length of initialised TLS data block
    size_t tls_total_len;       ///< Total (initialised + BSS) TLS data length
};

#endif // SPAWN_DOMAIN_PARAMS_H
