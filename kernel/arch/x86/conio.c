/**
 * \file
 * \brief Text-mode video console output driver.
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <string.h>
#include <x86.h>
#include <arch/x86/conio.h>

#define COLS        80                  ///< Number of columns on screen
#define LINES       25                 ///< Number of lines on screen
#define CHAR_ATTR   7                   ///< Attribute of plain text characters
#define VIDEO_LEN   (COLS * LINES * 2)  ///< Size of video memory
#define TAB_WIDTH   8                   ///< Tab-stop width

// Cursor X/Y position and attribute
static int xpos = 0, ypos = 0, attribute = CHAR_ATTR;
// Pointer to video memory
static uint8_t * COUNT(VIDEO_LEN) video
     = (uint8_t * COUNT(VIDEO_LEN)) TC(VIDEO_MEM);
static int use_conio = 1;

/**
 * \brief Clear the screen and reset cursor.
 */
void conio_cls(void)
{
    if (use_conio && CPU_IS_M5_SIMULATOR) {
        use_conio = 0;
    } else {
        memset(video, 0, LINES * COLS * 2);
        xpos = ypos = 0;
    }
}

/**
 * \brief Relocate video memory to new location.
 *
 * This function relocates the start of the video memory to the
 * virtual address 'newaddr'.
 *
 * \param newaddr       Address of start of new video memory.
 */
void conio_relocate_vidmem(lvaddr_t newaddr)
{
    video = (uint8_t *)newaddr;
}

/**
 * \brief Feed the screen page by one line.
 */
static void page_feed(void)
{
    memmove(video, video + COLS * 2, (LINES - 1) * COLS * 2);
    memset(video + (LINES - 1) * COLS * 2, 0, COLS * 2);
}

/**
 * \brief Put the character 'c' on the screen.
 *
 * \param c     Character to print on the screen.
 */
void conio_putchar(char c)
{
    if (use_conio) {
        uint8_t *vidptr;

        // Handle carriage return character
        if (c == '\r') {
        carriage:
          xpos = 0;
          return;
        }
        
        // Handle newline character
        if (c == '\n') {
        newline:
          if (ypos < LINES - 1) {
            ypos++;
          } else {
            page_feed();
          }
          goto carriage;
        }
        
        // Handle backspace character
        if (c == '\b') {
          xpos--;
          return;
        }
        
        // Handle tab-stop character
        if (c == '\t') {
          while(xpos % TAB_WIDTH != 0)
            xpos++;
          
          return;
        }
        
        vidptr = video + (xpos + ypos * COLS) * 2;
        *vidptr = c & 0xff;
        *(vidptr + 1) = attribute;
        
        xpos++;
        if (xpos >= COLS) {
          goto newline;
        }
    }
}
