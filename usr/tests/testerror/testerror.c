/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>

static errval_t test0(void)
{
    return SYS_ERR_LMP_NO_TARGET;
}

static errval_t test1(void)
{
    errval_t err = test0();
    return err_push(err, SYS_ERR_LMP_TARGET_DISABLED);
}
    
static errval_t test2(void)
{
    errval_t err = test1();
    return err_push(err, SYS_ERR_LMP_BUF_OVERFLOW);
}

static errval_t test3(void)
{
    errval_t err = test2();
    return err_push(err, SYS_ERR_LRPC_NOT_ENDPOINT);
}

static errval_t test4(void)
{
    errval_t err = test3();
    return err_push(err, SYS_ERR_GUARD_SIZE_OVERFLOW);
}


int main(void)
{

    errval_t err = test4();
    if (! err_is_ok(err)){
        err_print_calltrace(err);
    }
    assert(err_is_ok(err));
    

}
