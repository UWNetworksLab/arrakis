#include <check.h>
#include <stdio.h>
#include "test_libs_c.h"
#include <string.h>

START_TEST(fopen_test)
{
	fail_unless(fopen("foo", "r") == NULL, "Any opened file should return NULL");
}
END_TEST

void
fs_test_init(Suite *suite)
{
	TCase *tc;
	tc = tcase_create("fopen"); 
	tcase_add_test(tc, fopen_test);
	suite_add_tcase(suite, tc);
}
