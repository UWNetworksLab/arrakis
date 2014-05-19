/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "vmkitmon.h"
#include "console.h"
#include <stdlib.h>
#include <barrelfish/terminal.h>

struct console *
console_new (void)
{
    struct console *ret = calloc(1, sizeof(struct console));
    return ret;
}

static int
handle_set_cursor_position (struct console *c, struct guest *g)
{
    // only support mode 0 for now
    if (guest_get_bh(g) != 0) {
        return HANDLER_ERR_UNHANDELED;
    }

    c->cursor_pos_x = guest_get_dl(g);
    c->cursor_pos_y = guest_get_dh(g);

    return HANDLER_ERR_OK;
}

static int
handle_write_char_with_attr (struct console *c, struct guest *g)
{
    // only support mode 0 for now
    if (guest_get_bh(g) != 0) {
        return HANDLER_ERR_UNHANDELED;
    }
    // we only support "normal" text output atm
    if (guest_get_bl(g) != 7) {
        return HANDLER_ERR_UNHANDELED;
    }

    // FIXME: Here we completely ignore the postition of the cursor atm since we
    //        do not have proper terminal support in BF.
    //        We also ignore multiple char writes, they make no sense atm.
    int r;
    char chr = guest_get_al(g);

    r = terminal_write(&chr, 1);
    assert(r == 1);

    return HANDLER_ERR_OK;
}

static int
handle_teletype_output (struct console *c, struct guest *g)
{
    // only support mode 0 for now
    if (guest_get_bh(g) != 0) {
        return HANDLER_ERR_UNHANDELED;
    }

    // we do not yet have a real understanding of the terminal, assume it is
    // 80x25 and we simply insert new lines at the end of the "screen"
    int r;
    char chr = guest_get_al(g);

    // treat CR and LF as column clearing chars
    if (chr == '\r' || chr == '\n') {
        c->cursor_pos_x = 0;
    }
    // insert a CR if we are passed the last column of the screen
    else if (c->cursor_pos_x > 79) {
        r = terminal_write("\n", 1);
        assert(r == 1);
        c->cursor_pos_x = 0;
    }
    // in all other cases just increase the column
    else {
        c->cursor_pos_x++;
    }

    r = terminal_write(&chr, 1);
    assert(r == 1);

    return HANDLER_ERR_OK;
}

static int
handle_get_cursor_pos_and_size (struct console *c, struct guest *g)
{
    // only support mode 0 for now
    if (guest_get_bh(g) != 0) {
        return HANDLER_ERR_UNHANDELED;
    }

    // set ax to 0
    guest_set_ax(g, 0);
    // we do not support scan-lines
    guest_set_cx(g, 0);

    // set the position
    guest_set_dl(g, c->cursor_pos_x);
    guest_set_dh(g, c->cursor_pos_y);

    return HANDLER_ERR_OK;
}

static int
handle_set_text_cursor_shape (struct console *c, struct guest *g)
{
    // we only handle primitive cursors
    if (guest_get_cx(g) != 0) {
        printf("console: Unsupported cursor requested\n");
        return HANDLER_ERR_FATAL;
    }

    return HANDLER_ERR_OK;
}

static int
handle_get_current_video_mode (struct console *c, struct guest *g)
{
    // FIXME: for the terminal to be more flexible this should not be hardcoded
    guest_set_ah(g, 80),
    guest_set_al(g, 0x6); // VGA, 80x25, 8x8 box, res 640x200, 2 colors
    guest_set_bh(g, 0);

    return HANDLER_ERR_OK;
}

static int
handle_get_ega_info (struct console *c, struct guest *g)
{
    // FIXME: only partially implemented, linux wants BX to be 0x10 to believe
    //        it is confronted with a CGA card.
    guest_set_bx(g, 0x10);

    return HANDLER_ERR_OK;
}

static int
handle_get_svga_info (struct console *c, struct guest *g)
{
    // we do not support vesa yet
    guest_set_ax(g, 0);

    return HANDLER_ERR_OK;
}

static int
handle_set_video_mode (struct console *c, struct guest *g)
{
    // FIXME: Ignored for now.

    return HANDLER_ERR_OK;
}

int
console_handle_int10 (struct console *c, struct guest *g)
{
    // VESA SuperVGA BIOS (VBE) - GET SuperVGA INFORMATION
    if (guest_get_ax(g) == 0x4f00) {
        return handle_get_svga_info(c, g);
    }
    // VIDEO - SET VIDEO MODE
    if (guest_get_ah(g) == 0x0) {
        return handle_set_video_mode(c, g);
    }
    // VIDEO - SET TEXT-MODE CURSOR SHAPE
    if (guest_get_ah(g) == 0x1) {
        return handle_set_text_cursor_shape(c, g);
    }
    // VIDEO - SET CURSOR POSITION
    else if (guest_get_ah(g) == 0x2) {
        return handle_set_cursor_position(c, g);
    }
    // VIDEO - GET CURSOR POSITION AND SIZE
    else if (guest_get_ah(g) == 0x3) {
        return handle_get_cursor_pos_and_size(c, g);
    }
    // VIDEO - WRITE CHARACTER AND ATTRIBUTE AT CURSOR POSITION
    else if (guest_get_ah(g) == 0x9) {
        return handle_write_char_with_attr(c, g);
    }
    // VIDEO - TELETYPE OUTPUT
    else if (guest_get_ah(g) == 0xe) {
        return handle_teletype_output(c, g);
    }
    // VIDEO - GET CURRENT VIDEO MODEL_CGA
    else if (guest_get_ah(g) == 0xf) {
        return handle_get_current_video_mode(c, g);
    }
    // VIDEO - ALTERNATE FUNCTION SELECT (PS, EGA, VGA, MCGA) - GET EGA INFO
    else if (guest_get_ah(g) == 0x12 && guest_get_bl(g) == 0x10) {
        return handle_get_ega_info(c, g);
    }

    return HANDLER_ERR_UNHANDELED;
}
