/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */
#include "i2c.h"
#include "twl6030.h"

static uint8_t _ti_twl6030_id1_read_8(void *d, size_t off);
static void _ti_twl6030_id1_write_8(void *d, size_t off, uint8_t regval);
#define ti_twl6030_id1_read_8(dev, off) _ti_twl6030_id1_read_8(dev, off)
#define ti_twl6030_id1_write_8(dev, off, regval) _ti_twl6030_id1_write_8(dev, off, regval)
#include <dev/ti_twl6030_dev.h>

// I2C Host controller id
#define I2C_HC 0
// I2C slave address for id1 reads and writes is 0x48
#define ID1_I2C_ADDR 0x48

#define PBS (8*1024)
static char PRBUF[PBS];
#define PRBUFL PRBUF, (PBS-1)

#include "mmchs_debug.h"
#if defined(TWL_SERIVCE_DEBUG) || defined(MMCHS_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define TWL_DEBUG(x...) printf(x)
#else
#define TWL_DEBUG(x...) ((void)0)
#endif

static ti_twl6030_t twl;

static inline uint8_t _ti_twl6030_id1_read_8(void *d, size_t off)
{
    errval_t err;

    struct i2c_msg msg[2];

    uint8_t reg = off & 0xff;
    uint8_t result = 0;

    TWL_DEBUG("id1_read_8(reg=0x%"PRIx8")\n", reg);

    /* set register to read from */
    msg[0].slave = ID1_I2C_ADDR;
    msg[0].flags = I2C_WR | I2C_NOSTOP;
    msg[0].length = 1;
    msg[0].buf = &reg;

    /* read data back */
    msg[1].slave = ID1_I2C_ADDR;
    msg[1].flags = I2C_RD;
    msg[1].length = 1;
    msg[1].buf = &result;

    err = ti_i2c_transfer(I2C_HC, msg, 2);

    if (err_is_fail(err)) {
        TWL_DEBUG("ti_i2c_transfer: %"PRIuERRV"\n", err);
        return 0;
    }

    return result;
}

static inline void _ti_twl6030_id1_write_8(void *d, size_t off, uint8_t regval)
{
    struct i2c_msg msg;
    errval_t err;

    uint8_t addr = off & 0xff;
    uint8_t msgbuf[2];

    msgbuf[0] = addr;
    msgbuf[1] = regval;

    msg.slave = ID1_I2C_ADDR;
    msg.flags = I2C_WR;
    msg.length = 2;
    msg.buf = msgbuf;

    err = ti_i2c_transfer(I2C_HC, &msg, 1);

    if (err_is_fail(err)) {
        TWL_DEBUG("ti_i2c_transfer failed in mackerel: %"PRIuERRV"\n", err);
    }

    return;
}

void ti_twl6030_init(void)
{
    TWL_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);
    ti_i2c_init(I2C_HC);
}

void ti_twl6030_vmmc_pr(void)
{
    ti_twl6030_pr(PRBUFL, &twl);
    TWL_DEBUG("%s\n", PRBUF);
}

static ti_twl6030_vsel_t millis_to_vsel(int millis)
{
    switch (millis) {
    case 0:
        return ti_twl6030_v0v0;
    case 1000:
        return ti_twl6030_v1v0;
    case 1100:
        return ti_twl6030_v1v1;
    case 1200:
        return ti_twl6030_v1v2;
    case 1300:
        return ti_twl6030_v1v3;
    case 1400:
        return ti_twl6030_v1v4;
    case 1500:
        return ti_twl6030_v1v5;
    case 1600:
        return ti_twl6030_v1v6;
    case 1700:
        return ti_twl6030_v1v7;
    case 1800:
        return ti_twl6030_v1v8;
    case 1900:
        return ti_twl6030_v1v9;
    case 2000:
        return ti_twl6030_v2v0;
    case 2100:
        return ti_twl6030_v2v1;
    case 2200:
        return ti_twl6030_v2v2;
    case 2300:
        return ti_twl6030_v2v3;
    case 2400:
        return ti_twl6030_v2v4;
    case 2500:
        return ti_twl6030_v2v5;
    case 2600:
        return ti_twl6030_v2v6;
    case 2700:
        return ti_twl6030_v2v7;
    case 2750:
        return ti_twl6030_v2v75;
    case 2800:
        return ti_twl6030_v2v8;
    case 2900:
        return ti_twl6030_v2v9;
    case 3000:
        return ti_twl6030_v3v0;
    case 3100:
        return ti_twl6030_v3v1;
    case 3200:
        return ti_twl6030_v3v2;
    case 3300:
        return ti_twl6030_v3v3;
    default:
        TWL_DEBUG("voltage (%d) not available, returning 0.0V\n", millis);
        return ti_twl6030_v0v0;
    }
}


void ti_twl6030_vmmc_off(void)
{
    // turn off
    ti_twl6030_cfg_state_w_t st = ti_twl6030_cfg_state_w_default;
    st = ti_twl6030_cfg_state_w_grp_app_insert(st, 0x1);
    st = ti_twl6030_cfg_state_w_grp_con_insert(st, 0x1);
    st = ti_twl6030_cfg_state_w_grp_mod_insert(st, 0x1);
    st = ti_twl6030_cfg_state_w_state_insert(st, 0x0);

    ti_twl6030_vmmc_cfg_state_w_rawwr(&twl, st);
}

void ti_twl6030_vmmc_on(void)
{
    // turn on
    ti_twl6030_cfg_state_w_t st = ti_twl6030_cfg_state_w_default;
    st = ti_twl6030_cfg_state_w_grp_app_insert(st, 0x1);
    st = ti_twl6030_cfg_state_w_grp_con_insert(st, 0x1);
    st = ti_twl6030_cfg_state_w_grp_mod_insert(st, 0x1);
    st = ti_twl6030_cfg_state_w_state_insert(st, 0x1);
    ti_twl6030_vmmc_cfg_state_w_wr(&twl, st);
}

static volatile uint32_t dummy = 0;
static void wait_msec(long msec)
{
    int i = 0, sum = 0;
    long end = (1200000 * msec / 8);

    // Cannot use volatile variables in loop
    while (++i < end)  {
        sum += i + end;
    }

    dummy += sum;
}


errval_t ti_twl6030_set_vmmc_vsel(int millis)
{
    TWL_DEBUG("ti_twl6030_vmmc_vsel\n");
    //ti_twl6030_mmcctrl_vmmc_auto_off_wrf(&twl, 0x0);
    ti_twl6030_mmcctrl_sw_fc_wrf(&twl, 0x1);

    ti_twl6030_vmmc_off();
    wait_msec(10);

    ti_twl6030_vsel_t vsel = millis_to_vsel(millis);
    ti_twl6030_vmmc_cfg_voltage_vsel_wrf(&twl, vsel);

    ti_twl6030_vmmc_on();
    //ti_twl6030_mmcctrl_vmmc_auto_off_wrf(&twl, 0x0);

    ti_twl6030_vmmc_pr();


    return SYS_ERR_OK;
}
