/**
 * \file
 * \brief Simple test program to test terminal input.
 **/

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/terminal.h>

#define SIZE   128

static void test_getchar(void)
{
    printf("--- TEST GETCHAR ---\n");
    printf("Please enter a character: ");
    fflush(stdout);

    int c = getchar();
    printf("\n");
    fflush(stdout);

    if (c == EOF) {
        fprintf(stderr, "Received EOF.\n");
    } else {
        printf("Received character %c (hex %#x).\n", c, c);
    }
    printf("\n");
    fflush(stdout);
}

static void test_gets(void)
{
    printf("--- TEST GETS ---\n");
    printf("Please enter a string of maximum length %d: ", SIZE);
    fflush(stdout);

    char buffer[SIZE];
    char *ret = NULL;
    ret = gets(buffer);
    printf("\n");
    fflush(stdout);

    if (ret == NULL) {
        fprintf(stderr, "Error during gets().\n");
    } else {
        printf("Received the following string: %s\n", buffer);
    }
    printf("\n");
    fflush(stdout);
}

static void test_scanf(void)
{
    printf("--- TEST SCANF ---\n");
    printf("Please enter an integer followed by a string of maximum length %d: ",
           SIZE);
    fflush(stdout);

    int i = 0;
    char buffer[SIZE];
    int num = scanf("%i %s", &i, buffer);
    printf("\n");
    fflush(stdout);

    if (num == EOF) {
        fprintf(stderr, "Error during scanf().\n");
    } else if (num != 2) {
        fprintf(stderr, "Could not read an interger and a string.\n");
    } else {
        printf("Received the number: %i\n", i);
        printf("Received the string: %s\n", buffer);
    }
    printf("\n");
    fflush(stdout);
}

int main(int argc, char **argv)
{
    printf("--- TERMINAL INPUT TESTS ---\n");

    test_getchar();
    test_gets();
    test_scanf();

    return EXIT_SUCCESS;
}
