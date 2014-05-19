/**
 * \file
 * \brief Physical memory map for TI OMAP 44xx-series SoCs. 
 * 
 * This is derived from:
 *
 * OMAP4430 Multimedia Device Silicon Revision 2.x Technical Reference
 * Manual Version O 
 * OMAP4460 Multimedia Device Silicon Revision 1.x Technical Reference
 * Manual Version Q
 * 
 * Section numbers refer to the OMAP4460 TRM.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef OMAP44XX_MAP_H
#define OMAP44XX_MAP_H

/* 
 * 2.2.1 L3_EMU Memory Space Mapping 
 */
#define OMAP44XX_MAP_L3_EMU_MIPI_STM_0                  0x54000000
#define OMAP44XX_MAP_L3_EMU_MIPI_STM_0_SIZE             0x100000
#define OMAP44XX_MAP_L3_EMU_MIPI_STM_1                  0x54100000
#define OMAP44XX_MAP_L3_EMU_MIPI_STM_1_SIZE             0x40000
#define OMAP44XX_MAP_L3_EMU_A9_CPU0_DEBUG_PMU           0x54140000
#define OMAP44XX_MAP_L3_EMU_A9_CPU0_DEBUG_PMU_SIZE      0x2000
#define OMAP44XX_MAP_L3_EMU_A9_CPU1_DEBUG_PMU           0x54142000
#define OMAP44XX_MAP_L3_EMU_A9_CPU1_DEBUG_PMU_SIZE      0x2000

#define OMAP44XX_MAP_L3_EMU_CTI0                        0x54148000
#define OMAP44XX_MAP_L3_EMU_CTI0_SIZE                   0x1000
#define OMAP44XX_MAP_L3_EMU_CTI1                        0x54149000
#define OMAP44XX_MAP_L3_EMU_CTI1_SIZE                   0x1000

#define OMAP44XX_MAP_L3_EMU_PTM0                        0x5414C000
#define OMAP44XX_MAP_L3_EMU_PTM0_SIZE                   0x1000
#define OMAP44XX_MAP_L3_EMU_PTM1                        0x5414D000
#define OMAP44XX_MAP_L3_EMU_PTM1_SIZE                   0x1000

#define OMAP44XX_MAP_L3_EMU_TRACE_FUNNEL                0x54158000
#define OMAP44XX_MAP_L3_EMU_TRACE_FUNNEL_SIZE           0x1000
#define OMAP44XX_MAP_L3_EMU_DAP_PC                      0x54159000
#define OMAP44XX_MAP_L3_EMU_DAP_PC_SIZE                 0x1000

#define OMAP44XX_MAP_L3_EMU_APB                         0x5415F000
#define OMAP44XX_MAP_L3_EMU_APB_SIZE                    0x1000
#define OMAP44XX_MAP_L3_EMU_DRM                         0x54160000
#define OMAP44XX_MAP_L3_EMU_DRM_SIZE                    0x1000
#define OMAP44XX_MAP_L3_EMU_MIPI_STM                    0x54161000
#define OMAP44XX_MAP_L3_EMU_MIPI_STM_SIZE               0x1000
#define OMAP44XX_MAP_L3_EMU_ETB                         0x54162000
#define OMAP44XX_MAP_L3_EMU_ETB_SIZE                    0x1000
#define OMAP44XX_MAP_L3_EMU_CS_TPIU                     0x54163000
#define OMAP44XX_MAP_L3_EMU_CS_TPIU_SIZE                0x1000
#define OMAP44XX_MAP_L3_EMU_CS_TF0                      0x54164000
#define OMAP44XX_MAP_L3_EMU_CS_TF0_SIZE                 0x1000

/*
 * 2.3.1 L4_CFG Memory Space Mapping
 */
#define OMAP44XX_MAP_L4_CFG_AP                          0x4A000000
#define OMAP44XX_MAP_L4_CFG_AP_SIZE                     0x800
#define OMAP44XX_MAP_L4_CFG_LA                          0x4A000800
#define OMAP44XX_MAP_L4_CFG_LA_SIZE                     0x800
#define OMAP44XX_MAP_L4_CFG_IP0                         0x4A001000
#define OMAP44XX_MAP_L4_CFG_IP0_SIZE                    0x1000
#define OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE        0x4A002000
#define OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE_SIZE   0x1000
#define OMAP44XX_MAP_L4_CFG_CM1                         0x4A004000
#define OMAP44XX_MAP_L4_CFG_CM1_SIZE                    0x1000

#define OMAP44XX_MAP_L4_CFG_CM2                         0x4A008000
#define OMAP44XX_MAP_L4_CFG_CM2_SIZE                    0x2000

#define OMAP44XX_MAP_L4_CFG_SDMA                        0x4A056000
#define OMAP44XX_MAP_L4_CFG_SDMA_SIZE                   0x1000
#define OMAP44XX_MAP_L4_CFG_HSI_TOP                     0x4A058000
#define OMAP44XX_MAP_L4_CFG_HSI_TOP_SIZE                0x1000
#define OMAP44XX_MAP_L4_CFG_HSI_DMA                     0x4A059000
#define OMAP44XX_MAP_L4_CFG_HSI_DMA_SIZE                0x1000
#define OMAP44XX_MAP_L4_CFG_HSI_PORT1                   0x4A05A000
#define OMAP44XX_MAP_L4_CFG_HSI_PORT1_SIZE              0x1000
#define OMAP44XX_MAP_L4_CFG_HSI_PORT2                   0x4A05B000
#define OMAP44XX_MAP_L4_CFG_HSI_PORT2_SIZE              0x1000

#define OMAP44XX_MAP_L4_CFG_SAR_ROM                     0x4A05E000
#define OMAP44XX_MAP_L4_CFG_SAR_ROM_SIZE                0x2000

#define OMAP44XX_MAP_L4_CFG_HSUSBTLL                    0x4A062000
#define OMAP44XX_MAP_L4_CFG_HSUSBTLL_SIZE               0x1000

#define OMAP44XX_MAP_L4_CFG_HSUSBHOST                   0x4A064000
#define OMAP44XX_MAP_L4_CFG_HSUSBHOST_SIZE              0x1000
#define OMAP44XX_MAP_L4_CFG_DSP_SUBSYSTEN               0x4A066000
#define OMAP44XX_MAP_L4_CFG_DSP_SUBSYSTEN_SIZE          0x1000

#define OMAP44XX_MAP_L4_CFG_FSUSB                       0x4A0A9000
#define OMAP44XX_MAP_L4_CFG_FSUSB_SIZE                  0x1000
#define OMAP44XX_MAP_L4_CFG_HSUSBOTG                    0x4A0AB000
#define OMAP44XX_MAP_L4_CFG_HSUSBOTG_SIZE               0x1000
#define OMAP44XX_MAP_L4_CFG_USBPHY                      0x4A0AD000
#define OMAP44XX_MAP_L4_CFG_USBPHY_SIZE                 0x1000

#define OMAP44XX_MAP_L4_CFG_SR_MPU                      0x4A0D9000
#define OMAP44XX_MAP_L4_CFG_SR_MPU_SIZE                 0x1000
#define OMAP44XX_MAP_L4_CFG_SR_IVA                      0x4A0DB000
#define OMAP44XX_MAP_L4_CFG_SR_IVA_SIZE                 0x1000
#define OMAP44XX_MAP_L4_CFG_SR_CORE                     0x4A0DD000
#define OMAP44XX_MAP_L4_CFG_SR_CORE_SIZE                0x1000

#define OMAP44XX_MAP_L4_CFG_MAILBOX                     0x4A0F4000
#define OMAP44XX_MAP_L4_CFG_MAILBOX_SIZE                0x1000
#define OMAP44XX_MAP_L4_CFG_SPINLOCK                    0x4A0F6000
#define OMAP44XX_MAP_L4_CFG_SPINLOCK_SIZE               0x1000

#define OMAP44XX_MAP_L4_CFG_SYSCTRL_PADCONF_CORE        0x4A100000
#define OMAP44XX_MAP_L4_CFG_SYSCTRL_PADCONF_CORE_SIZE   0x1000
#define OMAP44XX_MAP_L4_CFG_OCP_WP                      0x4A102000
#define OMAP44XX_MAP_L4_CFG_OCP_WP_SIZE                 0x1000

#define OMAP44XX_MAP_L4_CFG_FACE_DETECT                 0x4A10A000
#define OMAP44XX_MAP_L4_CFG_FACE_DETECT_SIZE            0x1000

#define OMAP44XX_MAP_L4_CFG_C2C_INIT_FIREWALL           0x4A204000
#define OMAP44XX_MAP_L4_CFG_C2C_INIT_FIREWALL_SIZE      0x1000
#define OMAP44XX_MAP_L4_CFG_C2C_TARGET_FIREWALL         0x4A206000
#define OMAP44XX_MAP_L4_CFG_C2C_TARGET_FIREWALL_SIZE    0x1000

#define OMAP44XX_MAP_L4_CFG_MA_FIREWALL                 0x4A20A000
#define OMAP44XX_MAP_L4_CFG_MA_FIREWALL_SIZE            0x1000
#define OMAP44XX_MAP_L4_CFG_EMIF_FIREWALL               0x4A20C000
#define OMAP44XX_MAP_L4_CFG_EMIF_FIREWALL_SIZE          0x1000

#define OMAP44XX_MAP_L4_CFG_GPMC_FIREWALL               0x4A210000
#define OMAP44XX_MAP_L4_CFG_GPMC_FIREWALL_SIZE          0x1000
#define OMAP44XX_MAP_L4_CFG_OCMC_RAM_FIREWALL           0x4A212000
#define OMAP44XX_MAP_L4_CFG_OCMC_RAM_FIREWALL_SIZE      0x1000
#define OMAP44XX_MAP_L4_CFG_GFX_T_FIREWALL              0x4A214000
#define OMAP44XX_MAP_L4_CFG_GFX_T_FIREWALL_SIZE         0x1000
#define OMAP44XX_MAP_L4_CFG_ISS_T_FIREWALL              0x4A216000
#define OMAP44XX_MAP_L4_CFG_ISS_T_FIREWALL_SIZE         0x1000
#define OMAP44XX_MAP_L4_CFG_M3_T_FIREWALL               0x4A218000
#define OMAP44XX_MAP_L4_CFG_M3_T_FIREWALL_SIZE          0x1000

#define OMAP44XX_MAP_L4_CFG_DSS_T_FIREWALL              0x4A21C000
#define OMAP44XX_MAP_L4_CFG_DSS_T_FIREWALL_SIZE         0x1000
#define OMAP44XX_MAP_L4_CFG_SL2_T_FIREWALL              0x4A21E000
#define OMAP44XX_MAP_L4_CFG_SL2_T_FIREWALL_SIZE         0x1000
#define OMAP44XX_MAP_L4_CFG_IVAHD_CFG_T_FIREWALL        0x4A220000
#define OMAP44XX_MAP_L4_CFG_IVAHD_CFG_T_FIREWALL_SIZE   0x1000

#define OMAP44XX_MAP_L4_CFG_L4_EMU_FIREWALL             0x4A226000
#define OMAP44XX_MAP_L4_CFG_L4_EMU_FIREWALL_SIZE        0x1000
#define OMAP44XX_MAP_L4_CFG_L4_ABE_FIREWALL             0x4A228000
#define OMAP44XX_MAP_L4_CFG_L4_ABE_FIREWALL_SIZE        0x1000

#define OMAP44XX_MAP_L4_CFG_L4_WKUP                     0x4A300000
#define OMAP44XX_MAP_L4_CFG_L4_WKUP_SIZE                0x40000

/*
 * 2.3.2 L4_WKUP Memory Space Mapping
 */
#define OMAP44XX_MAP_L4_WKUP_AP                         0x4A300000
#define OMAP44XX_MAP_L4_WKUP_AP_SIZE                    0x800
#define OMAP44XX_MAP_L4_WKUP_LA                         0x4A300800
#define OMAP44XX_MAP_L4_WKUP_LA_SIZE                    0x800
#define OMAP44XX_MAP_L4_WKUP_IP0                        0x4A301000
#define OMAP44XX_MAP_L4_WKUP_IP0_SIZE                   0x1000

#define OMAP44XX_MAP_L4_WKUP_32KTIMER                   0x4A304000
#define OMAP44XX_MAP_L4_WKUP_32KTIMER_SIZE              0x1000
#define OMAP44XX_MAP_L4_WKUP_PRM                        0x4A306000
#define OMAP44XX_MAP_L4_WKUP_PRM_SIZE                   0x2000

#define OMAP44XX_MAP_L4_WKUP_SRCM                       0x4A30A000
#define OMAP44XX_MAP_L4_WKUP_SRCM_SIZE                  0x1000
#define OMAP44XX_MAP_L4_WKUP_SYSCTRL_GENERAL_WKUP       0x4A30C000
#define OMAP44XX_MAP_L4_WKUP_SYSCTRL_GENERAL_WKUP_SIZE  0x1000

#define OMAP44XX_MAP_L4_WKUP_GPIO1                      0x4A310000
#define OMAP44XX_MAP_L4_WKUP_GPIO1_SIZE                 0x1000

#define OMAP44XX_MAP_L4_WKUP_WDTIMER2                   0x4A314000 
#define OMAP44XX_MAP_L4_WKUP_WDTIMER2_SIZE              0x1000

#define OMAP44XX_MAP_L4_WKUP_GPTIMER1                   0x4A318000
#define OMAP44XX_MAP_L4_WKUP_GPTIMER1_SIZE              0x1000

#define OMAP44XX_MAP_L4_WKUP_KEYBOARD                   0x4A31C000
#define OMAP44XX_MAP_L4_WKUP_KEYBOARD_SIZE              0x1000

#define OMAP44XX_MAP_L4_WKUP_SYSCTRL_PADCONF_WKUP       0x4A31E000
#define OMAP44XX_MAP_L4_WKUP_SYSCTRL_PADCONF_WKUP_SIZE  0x1000

#define OMAP44XX_MAP_L4_WKUP_SAR_RAM1                   0x4A326000
#define OMAP44XX_MAP_L4_WKUP_SAR_RAM1_SIZE              0x1000
#define OMAP44XX_MAP_L4_WKUP_SAR_RAM2                   0x4A328000
#define OMAP44XX_MAP_L4_WKUP_SAR_RAM2_SIZE              0x800
#define OMAP44XX_MAP_L4_WKUP_SAR_RAM3                   0x4A329000
#define OMAP44XX_MAP_L4_WKUP_SAR_RAM3_SIZE              0x400
#define OMAP44XX_MAP_L4_WKUP_SAR_RAM4                   0x4A32A000
#define OMAP44XX_MAP_L4_WKUP_SAR_RAM4_SIZE              0x1000

/*
 * 2.3.3 L4_PER Memory Space Mapping
 */
#define OMAP44XX_MAP_L4_PER_AP                          0x48000000
#define OMAP44XX_MAP_L4_PER_AP_SIZE                     0x800
#define OMAP44XX_MAP_L4_PER_LA                          0x48000800
#define OMAP44XX_MAP_L4_PER_LA_SIZE                     0x800
#define OMAP44XX_MAP_L4_PER_IP0                         0x48001000
#define OMAP44XX_MAP_L4_PER_IP0_SIZE                    0x400
#define OMAP44XX_MAP_L4_PER_IP1                         0x48001400
#define OMAP44XX_MAP_L4_PER_IP1_SIZE                    0x400
#define OMAP44XX_MAP_L4_PER_IP2                         0x48001800
#define OMAP44XX_MAP_L4_PER_IP2_SIZE                    0x400
#define OMAP44XX_MAP_L4_PER_IP3                         0x48001C00
#define OMAP44XX_MAP_L4_PER_IP3_SIZE                    0x400

#define OMAP44XX_MAP_L4_PER_UART3                       0x48020000
#define OMAP44XX_MAP_L4_PER_UART3_SIZE                  0x1000

#define OMAP44XX_MAP_L4_PER_GPTIMER2                    0x48030000
#define OMAP44XX_MAP_L4_PER_GPTIMER2_SIZE               0x1000
#define OMAP44XX_MAP_L4_PER_GPTIMER3                    0x48034000
#define OMAP44XX_MAP_L4_PER_GPTIMER3_SIZE               0x1000
#define OMAP44XX_MAP_L4_PER_GPTIMER4                    0x48036000
#define OMAP44XX_MAP_L4_PER_GPTIMER4_SIZE               0x1000

#define OMAP44XX_MAP_L4_PER_GPTIMER9                    0x4803E000
#define OMAP44XX_MAP_L4_PER_GPTIMER9_SIZE               0x1000
#define OMAP44XX_MAP_L4_PER_DISPLAY                     0x48040000
#define OMAP44XX_MAP_L4_PER_DISPLAY_SIZE                0x10000

#define OMAP44XX_MAP_L4_PER_GPIO2                       0x48055000
#define OMAP44XX_MAP_L4_PER_GPIO2_SIZE                  0x1000
#define OMAP44XX_MAP_L4_PER_GPIO3                       0x48057000
#define OMAP44XX_MAP_L4_PER_GPIO3_SIZE                  0x1000
#define OMAP44XX_MAP_L4_PER_GPIO4                       0x48059000
#define OMAP44XX_MAP_L4_PER_GPIO4_SIZE                  0x1000
#define OMAP44XX_MAP_L4_PER_GPIO5                       0x4805B000
#define OMAP44XX_MAP_L4_PER_GPIO5_SIZE                  0x1000
#define OMAP44XX_MAP_L4_PER_GPIO6                       0x4805D000
#define OMAP44XX_MAP_L4_PER_GPIO6_SIZE                  0x1000

#define OMAP44XX_MAP_L4_PER_I2C3                        0x48060000
#define OMAP44XX_MAP_L4_PER_I2C3_SIZE                   0x1000

#define OMAP44XX_MAP_L4_PER_UART1                       0x4806A000
#define OMAP44XX_MAP_L4_PER_UART1_SIZE                  0x1000
#define OMAP44XX_MAP_L4_PER_UART2                       0x4806C000
#define OMAP44XX_MAP_L4_PER_UART2_SIZE                  0x1000
#define OMAP44XX_MAP_L4_PER_UART4                       0x4806E000
#define OMAP44XX_MAP_L4_PER_UART4_SIZE                  0x1000
#define OMAP44XX_MAP_L4_PER_I2C1                        0x48070000
#define OMAP44XX_MAP_L4_PER_I2C1_SIZE                   0x1000
#define OMAP44XX_MAP_L4_PER_I2C2                        0x48072000
#define OMAP44XX_MAP_L4_PER_I2C2_SIZE                   0x1000

#define OMAP44XX_MAP_L4_PER_SLIMBUS2                    0x48076000
#define OMAP44XX_MAP_L4_PER_SLIMBUS2_SIZE               0x1000
#define OMAP44XX_MAP_L4_PER_ELM                         0x48078000
#define OMAP44XX_MAP_L4_PER_ELM_SIZE                    0x1000

#define OMAP44XX_MAP_L4_PER_GPTIMER10                   0x48086000
#define OMAP44XX_MAP_L4_PER_GPTIMER10_SIZE              0x1000
#define OMAP44XX_MAP_L4_PER_GPTIMER11                   0x48088000
#define OMAP44XX_MAP_L4_PER_GPTIMER11_SIZE              0x1000

#define OMAP44XX_MAP_L4_PER_MCBSP4                      0x48096000
#define OMAP44XX_MAP_L4_PER_MCBSP4_SIZE                 0x1000
#define OMAP44XX_MAP_L4_PER_MCSPI1                      0x48098000
#define OMAP44XX_MAP_L4_PER_MCSPI1_SIZE                 0x1000
#define OMAP44XX_MAP_L4_PER_MCSPI2                      0x4809A000
#define OMAP44XX_MAP_L4_PER_MCSPI2_SIZE                 0x1000
#define OMAP44XX_MAP_L4_PER_HSMMC1                      0x4809C000
#define OMAP44XX_MAP_L4_PER_HSMMC1_SIZE                 0x1000

#define OMAP44XX_MAP_L4_PER_MMC_SD3                     0x480AD000
#define OMAP44XX_MAP_L4_PER_MMC_SD3_SIZE                0x1000

#define OMAP44XX_MAP_L4_PER_HDQ                         0x480B2000
#define OMAP44XX_MAP_L4_PER_HDQ_SIZE                    0x1000

#define OMAP44XX_MAP_L4_PER_HSMMC2                      0x480B4000
#define OMAP44XX_MAP_L4_PER_HSMMC2_SIZE                 0x1000

#define OMAP44XX_MAP_L4_PER_MCSPI3                      0x480B8000
#define OMAP44XX_MAP_L4_PER_MCSPI3_SIZE                 0x1000
#define OMAP44XX_MAP_L4_PER_MCSPI4                      0x480BA000
#define OMAP44XX_MAP_L4_PER_MCSPI4_SIZE                 0x1000

#define OMAP44XX_MAP_L4_PER_MMC_SD4                     0x480D1000
#define OMAP44XX_MAP_L4_PER_MMC_SD4_SIZE                0x1000

#define OMAP44XX_MAP_L4_PER_MMC_SD5                     0x480D2000
#define OMAP44XX_MAP_L4_PER_MMC_SD5_SIZE                0x1000

#define OMAP44XX_MAP_L4_PER_I2C4                        0x48350000
#define OMAP44XX_MAP_L4_PER_I2C4_SIZE                   0x1000

/*
 * 2.3.4 ABE L4 Memory Space Mapping
 */
#define OMAP44XX_MAP_L4_ABE_L3_ABE                      0x40100000
#define OMAP44XX_MAP_L4_ABE_L3_ABE_SIZE                 0x4000

#define OMAP44XX_MAP_L4_ABE_MCBSP1                      0x40122000
#define OMAP44XX_MAP_L4_ABE_MCBSP1_SIZE                 0x1000
#define OMAP44XX_MAP_L4_ABE_MCBSP2                      0x40124000
#define OMAP44XX_MAP_L4_ABE_MCBSP2_SIZE                 0x1000
#define OMAP44XX_MAP_L4_ABE_MCBSP3                      0x40126000
#define OMAP44XX_MAP_L4_ABE_MCBSP3_SIZE                 0x1000
#define OMAP44XX_MAP_L4_ABE_MCASP                       0x40128000
#define OMAP44XX_MAP_L4_ABE_MCASP_SIZE                  0x1000
#define OMAP44XX_MAP_L4_ABE_SLIMBUS1                    0x4012C000
#define OMAP44XX_MAP_L4_ABE_SLIMBUS1_SIZE               0x1000
#define OMAP44XX_MAP_L4_ABE_DMIC                        0x4012E000
#define OMAP44XX_MAP_L4_ABE_DMIC_SIZE                   0x1000
#define OMAP44XX_MAP_L4_ABE_WDTIMER3                    0x40130000
#define OMAP44XX_MAP_L4_ABE_WDTIMER3_SIZE               0x1000
#define OMAP44XX_MAP_L4_ABE_MCPDM                       0x40132000
#define OMAP44XX_MAP_L4_ABE_MCPDM_SIZE                  0x1000

#define OMAP44XX_MAP_L4_ABE_GPTIMER5                    0x40138000
#define OMAP44XX_MAP_L4_ABE_GPTIMER5_SIZE               0x1000
#define OMAP44XX_MAP_L4_ABE_GPTIMER6                    0x4013A000
#define OMAP44XX_MAP_L4_ABE_GPTIMER6_SIZE               0x1000
#define OMAP44XX_MAP_L4_ABE_GPTIMER7                    0x4013C000
#define OMAP44XX_MAP_L4_ABE_GPTIMER7_SIZE               0x1000
#define OMAP44XX_MAP_L4_ABE_GPTIMER8                    0x4013E000
#define OMAP44XX_MAP_L4_ABE_GPTIMER8_SIZE               0x1000

#define OMAP44XX_MAP_L4_ABE_DMEM                        0x40180000
#define OMAP44XX_MAP_L4_ABE_DMEM_SIZE                   0x10000

#define OMAP44XX_MAP_L4_ABE_CMEM                        0x401A0000
#define OMAP44XX_MAP_L4_ABE_CMEM_SIZE                   0x10000

#define OMAP44XX_MAP_L4_ABE_SMEM                        0x401C0000
#define OMAP44XX_MAP_L4_ABE_SMEM_SIZE                   0x10000

#define OMAP44XX_MAP_L4_ABE_AESS                        0x401F1000
#define OMAP44XX_MAP_L4_ABE_AESS_SIZE                   0x1000

/*
 * ABE L3 Memory Space Mapping
 */
#define OMAP44XX_MAP_L3_ABE_L3_ABE                      0x49000000
#define OMAP44XX_MAP_L3_ABE_L3_ABE_SIZE                 0x4000

#define OMAP44XX_MAP_L3_ABE_MCBSP1                      0x49022000
#define OMAP44XX_MAP_L3_ABE_MCBSP1_SIZE                 0x1000
#define OMAP44XX_MAP_L3_ABE_MCBSP2                      0x49024000
#define OMAP44XX_MAP_L3_ABE_MCBSP2_SIZE                 0x1000
#define OMAP44XX_MAP_L3_ABE_MCBSP3                      0x49026000
#define OMAP44XX_MAP_L3_ABE_MCBSP3_SIZE                 0x1000
#define OMAP44XX_MAP_L3_ABE_MCASP                       0x49028000
#define OMAP44XX_MAP_L3_ABE_MCASP_SIZE                  0x1000
#define OMAP44XX_MAP_L3_ABE_SLIMBUS1                    0x4902C000
#define OMAP44XX_MAP_L3_ABE_SLIMBUS1_SIZE               0x1000
#define OMAP44XX_MAP_L3_ABE_DMIC                        0x4902E000
#define OMAP44XX_MAP_L3_ABE_DMIC_SIZE                   0x1000
#define OMAP44XX_MAP_L3_ABE_WDTIMER3                    0x49030000
#define OMAP44XX_MAP_L3_ABE_WDTIMER3_SIZE               0x1000
#define OMAP44XX_MAP_L3_ABE_MCPDM                       0x49032000
#define OMAP44XX_MAP_L3_ABE_MCPDM_SIZE                  0x1000

#define OMAP44XX_MAP_L3_ABE_GPTIMER5                    0x49038000
#define OMAP44XX_MAP_L3_ABE_GPTIMER5_SIZE               0x1000
#define OMAP44XX_MAP_L3_ABE_GPTIMER6                    0x4903A000
#define OMAP44XX_MAP_L3_ABE_GPTIMER6_SIZE               0x1000
#define OMAP44XX_MAP_L3_ABE_GPTIMER7                    0x4903C000
#define OMAP44XX_MAP_L3_ABE_GPTIMER7_SIZE               0x1000
#define OMAP44XX_MAP_L3_ABE_GPTIMER8                    0x4903E000
#define OMAP44XX_MAP_L3_ABE_GPTIMER8_SIZE               0x1000

#define OMAP44XX_MAP_L3_ABE_DMEM                        0x49080000
#define OMAP44XX_MAP_L3_ABE_DMEM_SIZE                   0x10000

#define OMAP44XX_MAP_L3_ABE_CMEM                        0x490A0000
#define OMAP44XX_MAP_L3_ABE_CMEM_SIZE                   0x10000

#define OMAP44XX_MAP_L3_ABE_SMEM                        0x490C0000
#define OMAP44XX_MAP_L3_ABE_SMEM_SIZE                   0x10000

#define OMAP44XX_MAP_L3_ABE_AESS                        0x490F1000
#define OMAP44XX_MAP_L3_ABE_AESS_SIZE                   0x1000

/*
 * 2.4 Dual Cortex-M3 Subsystem Memory Space Mapping
 */
#define OMAP44XX_MAP_M3_L2MMU                           0x55082000
#define OMAP44XX_MAP_M3_L2MMU_SIZE                      0x1000
/*
 * 2.5 DSP Subsystem Memory Space Mapping
 */

/* 
 * 2.6.1 L3 Interconnect View of the Display Memory Spac
 */
#define OMAP44XX_MAP_L3_DISPLAY_REGISTERS               0x58000000
#define OMAP44XX_MAP_L3_DISPLAY_REGISTERS_SIZE          0x1000
#define OMAP44XX_MAP_L3_DISPLAY_DISPC                   0x58001000
#define OMAP44XX_MAP_L3_DISPLAY_DISPC_SIZE              0x1000
#define OMAP44XX_MAP_L3_DISPLAY_RFBI                    0x58002000
#define OMAP44XX_MAP_L3_DISPLAY_RFBI_SIZE               0x1000
#define OMAP44XX_MAP_L3_DISPLAY_VENC                    0x58003000
#define OMAP44XX_MAP_L3_DISPLAY_VENC_SIZE               0x1000
#define OMAP44XX_MAP_L3_DISPLAY_DSI1                    0x58004000
#define OMAP44XX_MAP_L3_DISPLAY_DSI1_SIZE               0x1000
#define OMAP44XX_MAP_L3_DISPLAY_DSI2                    0x58005000
#define OMAP44XX_MAP_L3_DISPLAY_DSI2_SIZE               0x1000
#define OMAP44XX_MAP_L3_DISPLAY_HDMI                    0x58006000
#define OMAP44XX_MAP_L3_DISPLAY_HDMI_SIZE               0x1000
#define OMAP44XX_MAP_L3_DISPLAY_HDCP                    0x58007000
#define OMAP44XX_MAP_L3_DISPLAY_HDCP_SIZE               0x1000

/* 
 * 2.6.2 L4 Interconnect View of the Display Memory Spac
 */
#define OMAP44XX_MAP_L4_DISPLAY_REGISTERS               0x48040000
#define OMAP44XX_MAP_L4_DISPLAY_REGISTERS_SIZE          0x1000
#define OMAP44XX_MAP_L4_DISPLAY_DISPC                   0x48041000
#define OMAP44XX_MAP_L4_DISPLAY_DISPC_SIZE              0x1000
#define OMAP44XX_MAP_L4_DISPLAY_RFBI                    0x48042000
#define OMAP44XX_MAP_L4_DISPLAY_RFBI_SIZE               0x1000
#define OMAP44XX_MAP_L4_DISPLAY_VENC                    0x48043000
#define OMAP44XX_MAP_L4_DISPLAY_VENC_SIZE               0x1000
#define OMAP44XX_MAP_L4_DISPLAY_DSI1                    0x48044000
#define OMAP44XX_MAP_L4_DISPLAY_DSI1_SIZE               0x1000
#define OMAP44XX_MAP_L4_DISPLAY_DSI2                    0x48045000
#define OMAP44XX_MAP_L4_DISPLAY_DSI2_SIZE               0x1000
#define OMAP44XX_MAP_L4_DISPLAY_HDMI                    0x48046000
#define OMAP44XX_MAP_L4_DISPLAY_HDMI_SIZE               0x1000
#define OMAP44XX_MAP_L4_DISPLAY_HDCP                    0x48047000
#define OMAP44XX_MAP_L4_DISPLAY_HDCP_SIZE               0x1000

/*
 * Others from Table 2.1
 */
#define OMAP44XX_MAP_SDRAM                              0x80000000

/*
 * 15.3 External memory interface
 */
#define OMAP44XX_MAP_EMIF1                              0x4c000000
#define OMAP44XX_MAP_EMIF2                              0x4d000000
#define OMAP44XX_MAP_EMIF_SIZE                          0x1000000

#endif  // OMAP44XX_MAP_H
