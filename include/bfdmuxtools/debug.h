/**
 * \file
 * \brief Debug makro definitions
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __DEBUG_H__
#define __DEBUG_H__

// Debug


/**
 * \brief Debug level
 * \arg 0: No messages will be printed on stdout.
 * \arg 1: Only error messages will be printed.
 * \arg 2: Error and information messages will be printed.
 * \arg 3: Information, errors and packet data as ascii will be printed.
 * \arg 4: Information, errors and packet data as ascii and hex will be printed.
 */
#define DEBUG_LEVEL	2

/**
 * \brief Set the current function name for well-arranged debug messages.
 */
#define PDEBUG_FNAME(x)		char* __DEBUG__CURRENT_FUNCTION_NAME = x; int __DEBUG__OMIT = 0; if (__DEBUG__OMIT) {}; // avoids
                                                                                                                                                                                                                                        // compilers 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 'unused 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // variable' 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // warning 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // in 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // functions 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // only 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // using 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // error 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // debug 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // 
                                                                                                                                                                                                                                        // messages

/**
 * \brief When calling this makro, all debug messages for the caller function will be omitted.
 */
#define PDEBUG_OMIT			__DEBUG__OMIT = 1;

/**
 * \brief This prints a debug header e.g: libbfdmux.c:regster_app: *** Hello world debug message ***
 */
#define PDEBUG_HEADER(x)	if ((DEBUG_LEVEL >= 2) && (!__DEBUG__OMIT)) { \
								printf("> %s: *** ", __DEBUG__CURRENT_FUNCTION_NAME); \
								printf x; \
								printf(" ***\n"); \
							};
/**
 * \brief This prints a debug footer line e.g: libbfdmux.c:regiser_app ### Foo bar footer ###
 */
#define PDEBUG_FOOTER(x)	if ((DEBUG_LEVEL >= 2) && (!__DEBUG__OMIT)) { \
								printf("> %s: ### ", __DEBUG__CURRENT_FUNCTION_NAME); \
								printf x; \
								printf(" ###\n"); \
							};
/**
 * \brief This makro is used to print error messages.
 */
#define PDEBUG_ERROR(x)		if (DEBUG_LEVEL >= 1) { \
								printf("E %s: ", __DEBUG__CURRENT_FUNCTION_NAME); \
								printf x; \
								printf("\n"); \
							};

/**
 * \brief This makro is used to print additional information
 */
#define PDEBUG_INFO(x)		if ((DEBUG_LEVEL >= 2) && (!__DEBUG__OMIT)) { \
								printf("  %s: ", __DEBUG__CURRENT_FUNCTION_NAME); \
								printf x; \
								printf("\n"); \
							};

/**
 * \brief This makro is used to dump a memory segment to the screen as hex and
 *
 * (if the debug level allows it) as characters.
 */
#define PDEBUG_RAW(arr,cnt)	if ((DEBUG_LEVEL >= 4) && (!__DEBUG__OMIT)) { \
								int __debug_i; __debug_i = 0; \
								char __debug_c; __debug_c = ' '; \
								printf("  %s: ", __DEBUG__CURRENT_FUNCTION_NAME); \
								printf("Address: %p, size: %u Bytes\n", arr, (unsigned) (cnt)); \
								printf("  %s: ", __DEBUG__CURRENT_FUNCTION_NAME); \
								for(__debug_i = 0; __debug_i < (cnt); __debug_i++) { \
									printf("%02x ",*( (uint8_t*) (arr) + __debug_i )); \
									if (!((__debug_i+1)%20)) printf("\n  %s: ", __DEBUG__CURRENT_FUNCTION_NAME); \
								}; \
							}; \
							if ((DEBUG_LEVEL >= 3) && (!__DEBUG__OMIT)) { \
								int __debug_j; __debug_j = 0; \
								char __debug_d; __debug_d = ' '; \
								printf("\n  %s: ", __DEBUG__CURRENT_FUNCTION_NAME); \
								for(__debug_j = 0; __debug_j < (cnt); __debug_j++) { \
									__debug_d = *((uint8_t*) (arr) + __debug_j ); \
									if (__debug_d < 0x20 || __debug_d > 0x7e) __debug_d = '*'; \
									printf("%c",__debug_d); \
									if (!((__debug_j+1)%60)) printf("\n  %s: ", __DEBUG__CURRENT_FUNCTION_NAME); \
								}; \
								printf("\n"); \
								if (FLUSH_AND_SYNC) { \
									fflush(stdin); \
								} \
							};

#endif
