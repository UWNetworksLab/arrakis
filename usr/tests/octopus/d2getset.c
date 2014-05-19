/**
 * \file
 * \brief Tests for octopus get/set/del API
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <skb/skb.h>
#include <octopus/octopus.h>

#include "common.h"

#define STR(a) #a
#define R(var, re)  static char var##_[] = STR(re);\
const char * var = ( var##_[ sizeof(var##_) - 2] = '\0',  (var##_ + 1) );

static void get_names(void)
{
    errval_t err = SYS_ERR_OK;
    char** names = NULL;
    size_t size = 0;

    err = oct_get_names(&names, &size, "r'^object.*'");
    ASSERT_ERR_OK(err);
    assert(size == 4);
    ASSERT_STRING(names[0], "object3");
    ASSERT_STRING(names[1], "object4");
    ASSERT_STRING(names[2], "object5");
    ASSERT_STRING(names[3], "object6");
    oct_free_names(names, size);

    err = oct_get_names(&names, &size, "_ { weight: _ }");
    ASSERT_ERR_OK(err);
    assert(size == 2);
    //ASSERT_STRING(names[0], "object2");
    ASSERT_STRING(names[0], "object3");
    ASSERT_STRING(names[1], "object4");
    oct_free_names(names, size);

    err = oct_get_names(&names, &size, "_ { attr: _, weight: %d }", 20);
    ASSERT_ERR_OK(err);
    assert(size == 1);
    ASSERT_STRING(names[0], "object4");
    oct_free_names(names, size);

    names = NULL;
    size = 0;
    err = oct_get_names(&names, &size, "asdfasd", 20);
    assert(names == NULL);
    assert(size == 0);
    ASSERT_ERR(err, OCT_ERR_NO_RECORD);

    err = oct_get_names(&names, &size, "}+_df}");
    assert(names == NULL);
    assert(size == 0);
    ASSERT_ERR(err, OCT_ERR_PARSER_FAIL);

    printf("get_names() done!\n");
}

static void get_records(void)
{
    errval_t err = SYS_ERR_OK;
    char* data = NULL;

    err = oct_get(&data, "recordDoesNotExist");
    ASSERT_ERR(err, OCT_ERR_NO_RECORD);
    assert(data == NULL);

    err = oct_get(&data, "parser { error, m, }");
    ASSERT_ERR(err, OCT_ERR_PARSER_FAIL);
    assert(data == NULL);


    err = oct_get(&data, "object1");
    ASSERT_ERR_OK(err);
    ASSERT_STRING(data, "object1 { weight: 20 }");
    free(data);

    /*err = oct_get(&data, "object2 { weight: 25 }");
    ASSERT_ERR_OK(err);
    ASSERT_STRING(data, "object2 { weight: 25 }");
    free(data);*/

    err = oct_get(&data, "object4");
    ASSERT_ERR_OK(err);
    ASSERT_STRING(data, "object4 { attr: 'Somestring', fl: 12.0, weight: 20 }");
    free(data);

    err = oct_get(&data, "_ { weight >= 10, fl > 11.0 }");
    ASSERT_ERR_OK(err);
    ASSERT_STRING(data, "object4 { attr: 'Somestring', fl: 12.0, weight: 20 }");
    free(data);

    err = oct_del("object4");
    ASSERT_ERR_OK(err);

    err = oct_get(&data, "object4");
    ASSERT_ERR(err, OCT_ERR_NO_RECORD);
    //free(data); TODO??

    err = oct_set("object4 { attr: 'Somestring', fl: 12.0, weight: 20 }");
    ASSERT_ERR_OK(err);

    err = oct_get(&data, "object4");
    ASSERT_ERR_OK(err);
    ASSERT_STRING(data, "object4 { attr: 'Somestring', fl: 12.0, weight: 20 }");
    free(data);

    err = oct_del("object1");
    ASSERT_ERR_OK(err);

    err = oct_get(&data, "object1");
    //printf("data: %s\n", data);
    ASSERT_ERR(err, OCT_ERR_NO_RECORD);
    // TODO free(data);?

    /*err = oct_get(&data, "object2 { weight: 25 }");
    ASSERT_ERR_OK(err);
    ASSERT_STRING(data, "object2 { weight: 25 }");
    free(data);*/

    err = oct_get(&data, "_ { pattern1: r'^12.*ab$' }");
    ASSERT_ERR_OK(err);
    ASSERT_STRING(data,
            "object5 { pattern1: '123abab', pattern2: " \
            "'StringToTestRegexMatching', pattern3: '10-10-2010' }");
    //printf("data: %s\n", data);
    free(data);

    // Test long regex
    R(re, "_ { pattern3: r'^(((((((0?[13578])|(1[02]))[\.\-/]?((0?[1-9])|([12]\d)|(3[01])))|(((0?[469])|(11))[\.\-/]?((0?[1-9])|([12]\d)|(30)))|((0?2)[\.\-/]?((0?[1-9])|(1\d)|(2[0-8]))))[\.\-/]?(((19)|(20))?([\d][\d]))))|((0?2)[\.\-/]?(29)[\.\-/]?(((19)|(20))?(([02468][048])|([13579][26])))))$' }");
    err = oct_get(&data, re);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(data,
            "object5 { pattern1: '123abab', pattern2: " \
            "'StringToTestRegexMatching', pattern3: '10-10-2010' }");
    //printf("data: %s\n", data);
    free(data);


    // TODO implement oct_del with constraints, attributes!

    printf("get_records() done!\n");
}

static void exist_records(void)
{
    errval_t err = oct_exists("././12");
    ASSERT_ERR(err, OCT_ERR_PARSER_FAIL);

    err = oct_exists("recordDoesNotExist");
    ASSERT_ERR(err, OCT_ERR_NO_RECORD);

    err = oct_exists("object3 { fl > 10, weight: _ }");
    ASSERT_ERR_OK(err);
}

static void set_records(void)
{
    errval_t err = oct_set("q{weqw1,.");
    ASSERT_ERR(err, OCT_ERR_PARSER_FAIL);

    err = oct_set("object1 { weight: %d }", 20);
    ASSERT_ERR_OK(err);

    // TODO: Do we want this?
    /*
    err = oct_set("object2 { weight: %lu, weight: %lu }", 20, 25);
    ASSERT_ERR_OK(err);*/

    char* str = "A text string.";
    err = oct_set("object3 { attr: '%s', weight: 9, fl: 12.0 }",
            str);
    ASSERT_ERR_OK(err);

    err = oct_set(
            "object4 { attr: 'Somestring', weight: 20, fl: %f }", 12.0);
    ASSERT_ERR_OK(err);

    char* pattern1 = "123abab";
    char* pattern2 = "StringToTestRegexMatching";
    char* pattern3 = "10-10-2010";
    err = oct_set(
            "object5 { pattern1: '%s', pattern2: '%s', pattern3: '%s' }",
            pattern1, pattern2, pattern3);
    ASSERT_ERR_OK(err);

    pattern1 = "123ababc";
    pattern3 = "21-00-2900";
    err = oct_set(
            "object6 { pattern1: '%s', pattern2: '%s', pattern3: '%s' }",
            pattern1, pattern2, pattern3);
    ASSERT_ERR_OK(err);


    printf("set_records() done!\n");
}

static void regex_name(void)
{
    errval_t err = SYS_ERR_OK;
    char* rec = NULL;

    err = oct_set("obj123 { attr: 12 }");
    ASSERT_ERR_OK(err);

    err = oct_set("obj1 { attr: 12 }");
    ASSERT_ERR_OK(err);

    err = oct_set("obj134 { attr: 12 }");
    ASSERT_ERR_OK(err);

    err = oct_get(&rec, "r'obj.$' { attr: 12 }");
    ASSERT_ERR_OK(err);

    err = oct_set("r'obj.$' { attr: 12 }");
    ASSERT_ERR(err, OCT_ERR_NO_RECORD_NAME);

    char** names = NULL;
    size_t len = 0;
    err = oct_get_names(&names, &len, "r'^obj.$'");
    ASSERT_ERR_OK(err);
    assert(len == 1);
    ASSERT_STRING("obj1", names[0]);
    oct_free_names(names, len);

    err = oct_del("r'obj.$' { attr: 12}");
    ASSERT_ERR(err, OCT_ERR_NO_RECORD_NAME);

    printf("regex_name() done!\n");
}

int main(int argc, char *argv[])
{
    oct_init();

    // Run Tests
    set_records();
    exist_records();
    get_records();
    get_names();
    regex_name();

    printf("d2getset SUCCESS!\n");
    return EXIT_SUCCESS;
}
