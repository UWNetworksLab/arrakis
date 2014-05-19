/*
 * Copyright (c) 2008, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __E1000_DESC_H__
#define __E1000_DESC_H__

typedef union {
    uint16_t   vlan;
    struct {
        uint16_t vlan :12;
        uint16_t cfi  :1;
        uint16_t pri  :3;
    } __attribute__((packed)) bits;
} __attribute__((packed)) vlan_tag_t;


union rx_desc {
    uint64_t raw[2] __attribute__((packed));
    struct {
        uint64_t buffer_address;
        struct {
            uint16_t   length;
            uint16_t   checksum;        /* reserved on: 82544GC/EI */
            struct {
                unsigned int   dd      :1;
                unsigned int   eop     :1;
                unsigned int   ixsm    :1;
                unsigned int   vp      :1;
                unsigned int   udpcs   :1;  /* reserved on: 8254x */
                unsigned int   tcpcs   :1;
                unsigned int   ipcs    :1;
                unsigned int   pif     :1;
            } __attribute__ ((packed)) status;

            union {
                uint8_t errors;
                struct {
                    uint8_t ce   :1;
                    uint8_t seq  :1;        /* reserved on: 82541xx, 82547GI/EI, and 82540EP/EM only. */
                    uint8_t res0 :1;        /* reserved on: 8254x */
                    uint8_t cxe  :1;        /* 82544GC/EI only */
                    uint8_t tcpe :1;
                    uint8_t ipe  :1;
                    uint8_t rxe  :1;
                } __attribute__ ((packed)) bits;
            } __attribute__ ((packed)) errors;

            vlan_tag_t vlan;

        } __attribute__ ((packed)) info;

    } __attribute__ ((packed)) rx_read_format;

} __attribute__ ((packed));


struct tx_desc {
    uint64_t buffer_address;
    union {
        uint64_t raw;
        struct {
            uint16_t data_len;
            uint8_t cso;
            union {
                uint8_t raw;
                struct {
                    uint8_t eop  :1;
                    uint8_t ifcs :1;
                    uint8_t ic   :1;
                    uint8_t rs   :1;
                    uint8_t rsv  :1;
                    uint8_t dext :1;
                    uint8_t vle  :1;
                    uint8_t ide  :1;
                } __attribute__ ((packed)) d;
            } __attribute__ ((packed)) cmd;
            union {
                uint8_t raw;
                struct {
                    uint8_t dd  :1;
                    uint8_t ec  :1;
                    uint8_t lc  :1;
                    uint8_t res :5;
                } __attribute__ ((packed)) d;
            } __attribute__ ((packed)) stat_rsv;
            uint8_t  css;
            uint16_t special;
        } __attribute__ ((packed)) legacy;

        struct {
            uint64_t data_len :20;
            uint64_t dtype    :4;
            union {
                uint8_t raw;
                struct {
                    uint8_t eop  :1;
                    uint8_t ifcs :1;
                    uint8_t tse  :1;
                    uint8_t rs   :1;
                    uint8_t rsv  :1;
                    uint8_t dext :1;
                    uint8_t vle  :1;
                    uint8_t ide  :1;
                } __attribute__ ((packed)) d;
            } __attribute__ ((packed)) dcmd;

            union {
                uint8_t raw;
                struct {
                    uint8_t dd  :1;
                    uint8_t res :7;
                } __attribute__ ((packed)) d;
            } __attribute__ ((packed)) stat_rsv;

            union {
                uint8_t  raw;
                struct {
                    uint8_t ixsm : 1;
                    uint8_t txsm : 1;
                    uint8_t  res : 6;
                } __attribute__ ((packed)) d;
            } __attribute__ ((packed)) popts;

            vlan_tag_t vlan;

        } __attribute__ ((packed)) extended_tcpip;
    } __attribute__ ((packed)) ctrl;
} __attribute__ ((packed));

/*
 * TCP/IP Context Descriptor Layout
 *
 * Provides access to enhanced checksum offload facility
 * available in the Ethernet controllerfor TCP and UDP packets.
 */
union context_desc {
    uint64_t raw;
    struct {
        uint8_t ipcss;
        uint8_t ipcso;
        uint16_t ipcse;
        uint8_t tucss;
        uint8_t tucso;
        uint16_t tucse;
        struct {
            uint32_t paylen :20;
            uint32_t dtype  :4;
        } __attribute__ ((packed)) pd;

        union {
            uint8_t raw;
            struct {
                uint8_t tcp  :1;
                uint8_t ip   :1;
                uint8_t tse  :1;
                uint8_t rs   :1;
                uint8_t rsv  :1;
                uint8_t dext :1;
                uint8_t snap :1;
                uint8_t ide  :1;
            } __attribute__ ((packed)) d;
        } __attribute__ ((packed)) tucmd;

        union {
            uint8_t raw;
            struct {
                uint8_t dd  :1;
                uint8_t res :7;
            } __attribute__ ((packed)) d;
        } __attribute__ ((packed)) stat_rsv;

        uint8_t hdrlen;
        uint16_t mss;

    } __attribute__ ((packed)) d;
} __attribute__ ((packed));


#endif
