#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include "test_libs_c.h"
#include <string.h>
#include <setjmp.h>

START_TEST(strcmp_equal)
{
	static char foobar[] = "Foo, bar";
	fail_unless(strcmp("Hello World", "Hello World") == 0, "Two equal inline strings");
	fail_unless(strcmp("", "") == 0, "Emptry strings equal");
	fail_unless(strcmp(foobar, foobar) == 0, "The string equal itself");
	fail_unless(strcmp(foobar, "Foo, bar") == 0, "Out of line string equal inline string");
}
END_TEST

START_TEST(strcmp_nequal)
{
	static char foobar[] = "Foo, bar";
	static char Hello[] = "Hello";
	static char Helloo[] = "Helloo";
	static char Hell[] = "Hell";
	static char dot[] = ".";
	static char empty[] = "";

	fail_unless(strcmp(Hello, Helloo) != 0, "Two different out of line string");
	fail_unless(strcmp(dot, empty) != 0, "Empty string and non-empty string");
	fail_unless(strcmp(empty, dot) != 0, "Non-empty string and empty string");
	fail_unless(strcmp(foobar, "Foo, ba") != 0, "Static string with different inline string");
	fail_unless(strcmp(Hello, Hell) != 0, "String with shorter string");
}
END_TEST

START_TEST(strcmp_different)
{
	char a[] = "a";
	char b[] = "b";
	char AA[] = "AA";
	char A[] = "A";

	fail_unless(strcmp(a, b)  < 0, "strcmp(\"a\", \"b\")");
	fail_unless(strcmp(b, a)  > 0, "strcmp(\"b\", \"a\")");
	fail_unless(strcmp(AA, A) > 0, "strcmp(\"AA\", \"A\")");
	fail_unless(strcmp(A, AA) < 0, "strcmp(\"A\", \"AA\")");
}
END_TEST
 
START_TEST(strncmp_equal)
{
	int i;
	char Foo_Bar[] = "Foo, bar";
	char Hello_World[] = "Hello World";
	fail_unless(strncmp(Hello_World, Hello_World, 5) == 0, "Start of two string equal");
	for (i = 0; i < 4; i++)
		fail_unless(strncmp("", "", i) == 0, 
			    "Empty strings equal (Length given as %d)", i);

	fail_unless(strncmp(Foo_Bar, Foo_Bar, strlen(Foo_Bar)) == 0, "The string equal itself");
	fail_unless(strncmp(Foo_Bar, "Foo, bar", strlen(Foo_Bar)) == 0, "Out of line string equal inline string");
}
END_TEST

START_TEST(strncmp_partially_equal)
{
	char Hello[] = "Hello";
	char HellO[] = "HellO";
	char Hell[] = "Hell";
	fail_unless(strncmp(Hello, HellO, 4) == 0, "strncmp(\"Hello\", \"HellO\", 4) == 0");
	fail_unless(strncmp(HellO, Hello, 4) == 0, "strncmp(\"HellO\", \"Hello\", 4) == 0");
	fail_unless(strncmp(Hell, Hello, 4) == 0, "strncmp(\"Hell\", \"Hello\", 4) == 0");
	fail_unless(strncmp(Hello, Hell, 4) == 0, "strncmp(\"Hello\", \"Hell\", 4) == 0");
}
END_TEST

START_TEST(strncmp_partially_different)
{
	char Hello[] = "Hello";
	char HellO[] = "HellO";
	char Hell[] = "Hell";
	fail_unless(strncmp(Hello, HellO, 5) != 0, "strncmp(\"Hello\", \"HellO\", 5) != 0");
	fail_unless(strncmp(HellO, Hello, 5) != 0, "strncmp(\"HellO\", \"Hello\", 5) != 0");
	fail_unless(strncmp(Hell, Hello, 5) != 0, "strncmp(\"Hell\", \"Hello\", 5) != 0");
	fail_unless(strncmp(Hello, Hell, 5) != 0, "strncmp(\"Hello\", \"Hell\", 5) != 0");
}
END_TEST

//extern char *__strstr(const char *s, const char *substring);

START_TEST(strstr_exists)
{
	char Foo_bar[] = "Foo bar";
	char bar[] = "bar";
	char Foo[] = "Foo";
	char oo_b[] = "oo b";
	char Hell[] = "Hell";
	char Hello[] = "Hello";

	fail_unless(strstr(Foo_bar, bar) != NULL, "strstr(\"Foo bar\", \"bar\") != NULL");
	fail_unless(strstr(Foo_bar, Foo) != NULL, "strstr(\"Foo bar\", \"Foo\") != NULL");
	fail_unless(strstr(Foo_bar, oo_b) != NULL, "strstr(\"Foo bar\", \"oo b\") != NULL");
	fail_unless(strstr(Hello, Hell) != NULL, "strstr(\"Foo bar\", \"Hell\") != NULL");
}
END_TEST

START_TEST(strstr_empty)
{
	static char foobar[] = "Foo, bar";
	fail_unless(strstr(foobar, "") == foobar, "Second string empty");
}
END_TEST

START_TEST(strstr_nexists)
{
	fail_unless(strstr("Foo bar", "baz") == NULL, "strstr(\"Foo bar\", \"baz\") == NULL");
	fail_unless(strstr("Foo bar", "quz") == NULL, "strstr(\"Foo bar\", \"quz\") == NULL");
}
END_TEST


START_TEST(sprintf_test)
{
	char dest[100];

	fail_unless(sprintf(dest, "foo") == 3, "incorrect size returned");
	fail_unless(strcmp(dest, "foo") == 0, "printf no formatting");

	fail_unless(sprintf(dest, "%p", NULL) == 5, "incorrect size returned");
	fail_unless(strcmp(dest, "(nil)") == 0, "printf NULL pointer");
}
END_TEST


START_TEST(format_left_justify)
{
	//char dest[100];

	//sprintf(dest,"X: %-5d",0);
	//fail_unless(strcmp(dest, "X: 0"), "Failed to left justify");
}
END_TEST

START_TEST(snprintf_test)
{
	char dest[100];

	memset(dest, '%', 100); /* Make sure the buffer isn't zeroed */

	fail_unless(snprintf(dest, 2, "foo") == 3, "incorrect size returned");
	fail_unless(strcmp(dest, "f") == 0, "printf no formatting");
}
END_TEST


START_TEST(stdout_fputs_test)
{
	char *foo = "Test\n";
	int c;
	printf("IGNORE OUTPUT\n");
	c = fputs(foo, stdout);
	fail_unless(c != 0, "fputs variable");
	c = fputs("Test2\n", stdout);
	fail_unless(c != 0, "fputs constant");
}
END_TEST

START_TEST(ungetc_test)
{
	FILE *tmp = tmpfile();
	fail_unless(ungetc('x', tmp) == 'x', "Unget failed");
	fail_unless(getc(tmp) == 'x', "Getting after unget failed");
}
END_TEST

START_TEST(ungetc_multiple_test)
{
	FILE *tmp = tmpfile();
	fail_unless(ungetc('x', tmp) == 'x', "unget failed");
	fail_unless(ungetc('y', tmp) == 'y', "unget (2) failed");
	fail_unless(getc(tmp) == 'y', "getc after unget failed");
	fail_unless(getc(tmp) == 'x', "getc (2) after unget failed");
}
END_TEST

START_TEST(memcmp_equal)
{
	int i;
	char s[4][10];

	for ( i = 0; i < 10; i++) {
		s[0][i] = s[1][i] = i;
		if(i < 9) {
			s[2][i] = s[3][i] = i;
		} else {
			s[2][i] = 0;
			s[3][i] = 1;
		}
	}

	fail_unless(memcmp(s[0], s[1], 10) == 0, 
		    "equal, when two objects have the same content");
	fail_unless(memcmp(s[2], s[3], 9) == 0, 
		    "equal, when only compare the parts containing the "
		    "same content of the two objects");
}
END_TEST

START_TEST(memcmp_unequal)
{
	int i;
	char s[4][10];

	for (i = 0; i < 10; i++) {
		s[0][i] = i;
		s[1][i] = i + 1;

		if(i < 9) {
			s[2][i] = s[3][i] = i;
		} else {
			s[2][i] = 0;
			s[3][i] = 1;
		}
	}
	   
	fail_unless(memcmp(s[0], s[1], 10) != 0,
		    "unequal, when two memory have completed "
		    "different content");

	fail_unless(memcmp(s[2], s[3], 10) != 0, 
		    "unequal, when compare the two objects with "
		    "partly equal content from the start to the end");
}
END_TEST

START_TEST(memcpy_nooverlap)
{
	int i;
	char s1[10], s2[10];
	
	for ( i = 0; i < 10; i++) {
		s2[i] = i;
	}
	
	fail_unless(memcpy(s1, s2, 10) == s1, "check memcpy return value");  
	fail_unless(memcmp(s1, s2, 10) == 0, "check memcpy performed correctly");  
}
END_TEST

START_TEST(memmove_noorwithoverlap)
{
	int i;
	char s1[10], s2[20], s4[20];
	char *s3 = &s2[5];

	for (i = 0; i < 10; i++) {
		s2[i] = s4[i] = i;
	}

	fail_unless(memmove(s1, s2, 10) == s1, "memmove non overlap, return value");  
	fail_unless(memcmp(s1, s2, 10) == 0, "memmove non overlap, data correct");  

	fail_unless(memmove(s3, s2, 10) == s3, "moving between twooverlap objects"); 
	fail_unless(memcmp(s3, s4, 10) == 0, "moving between twooverlap objects"); 
}
END_TEST

START_TEST(strcpy_test)
{
	char s1[10];
	char s2[] = "bigger";
   
	fail_unless(strcpy(s1, s2) == s1, "copying from small to big, return value");  
	fail_unless(strcmp(s1, s2) == 0, "copying from small to big, data correct");  
}
END_TEST


START_TEST(strncpy_oversized_destination)
{
	char s1[] = "thebiggest";
	char s2[] = "bigger";
	char s4[] = "\0\0\0\0"; /*four null characters*/

	fail_unless(strncpy(s1, s2, 10) == s1, "copying size is bigger than the source but smaller than the destination, return value");
	fail_unless(strcmp(s1, s2) == 0, "copying size is bigger than the source but smaller than the destination, data correct");
	fail_unless(strncmp(&s1[6], s4, 4) == 0, "copying size is bigger than the source but smaller than the destinnation, nul padding");
}
END_TEST


START_TEST(memchr_findorno)
{
	char s1[10], s2[10];
	int i;
	
	for (i = 0; i < 10; i++) {
	      	s1[i] = s2[i] = i;
		if (i == 9)
			s1[i] = 'a';
	}
	
	fail_unless(memchr(s1, 'a', 10) == (s1 + 9), "find a character in a memory with this character");
	fail_unless(memchr(s2, 'a', 10) == NULL, "find a character in a memory without this character");
	fail_unless(memchr(s1, 'a', 8) == NULL, "find a character in the range without this character ");
}
END_TEST

START_TEST(strchr_findorno)
{
	char s1[] = "sheep";
	char s2[] = "ship";
	
	fail_unless(strchr(s1, 'e') == (s1 + 2), "find a non null character in a string with this character");
	fail_unless(strchr(s2, 'e') == NULL, "find a non null character in a string without this character");
	fail_unless(strchr(s2, '\0') == (s2 + strlen(s2)), "find a null character in a string");
}
END_TEST

START_TEST(strcat_stickatend)
{
	char s1[20] = "ineed";
	char s2[] = "bread";
	
	fail_unless(strcat(s1, s2) == s1, "stick s2 on the end of s1, return value");
	fail_unless(strcmp(s1, "ineedbread") == 0, "stick s2 on the end of s1, data correct");
}
END_TEST

START_TEST(strncat_stickatend)
{
	static char s1[40] = "ineed";
	static char s2[] = "bread";
	static char s3[10] = "br ea";

	s3[2] = '\0';

	fail_unless(strncat(s1, s2, strlen(s2)) == s1, "n is equal to the length of s2, return value");
	fail_unless(strcmp(s1, "ineedbread") == 0, "n is equal to the length of s2, data correct");

	fail_unless(strncat(s1, s2, 10) == s1, "n is greater to the length of s2, return value");
	fail_unless(strcmp(s1, "ineedbreadbread") == 0, "n is greater to the length of s2, data correct");

	fail_unless(strncat(s1, s3, 10) == s1, "s2 is a region with a hole, return value");
	fail_unless(strcmp(s1, "ineedbreadbreadbr") == 0, "s2 is a region with a hole, data correct");
}
END_TEST

START_TEST(strcspn_inornot)
{
	char s1[] = "ineedbread";
	char s2[] = "bread";
	char s3[] = "cut"; 
	
	fail_unless(strcspn(s1, s2) == 2, "there are members of s2 in s1");
	fail_unless(strcspn(s3, s2) == 3, "there is no member of s2 in s3");
}
END_TEST

START_TEST(strpbrk_inornot)
{
	char s1[] = "ineedbread";
	char s2[] = "bread";
	char s3[] = "cut"; 
	
	fail_unless(strpbrk(s1, s2) == (s1 + 2), "there are members of s2 in s1");
	fail_unless(strpbrk(s3, s2) == NULL, "there is no member of s2 in s3");
}
END_TEST

START_TEST(strrchr_inornot)
{
	char s1[] = "ineedbread";
	
	fail_unless(strrchr(s1, 'e') == (s1 + 7), "there is non-null character 'e' in s1");
	fail_unless(strrchr(s1, 'c') == NULL, "there is no non-null character 'c' in s3");
	/* 
	   Guess what, the follow doesn't actually end up testing strrchr, because gcc
	   wonderfully optomises calls to strrchar(x, '\0') to actually call strchr(x, '\0')
	*/
	fail_unless(strrchr(s1, '\0') == (s1 + strlen(s1)), "the searched is null character");
}
END_TEST

START_TEST(strspn_inornot)
{
           char s1[] = "ididbread";
	   char s2[] = "ineed";
	
	   fail_unless(strspn(s1, s2) == 4, "there are members of s2 in s1");
	   fail_unless(strspn(s2, s2) == 5, "there is no member of s2 in s3");
}
END_TEST

START_TEST(strtok_split)
{
           char test1[] = "want a break!";
	   char test2[] = "continues";
	   char test3[] = "--";

	   char delims[] = " .,;:!-";
	
	   fail_unless((strtok(test1, delims) == test1	      \
			&& strtok(NULL, delims) == test1 + 5 \
			&& strtok(NULL, delims) == test1 + 7 \
			&& strtok(NULL, delims) == NULL), 
		       "there are members of delims within test1");
	   fail_unless(strtok(test2, delims) == test2 && \
		       strtok(NULL, delims) == NULL, 
		       "there is no member of delims in test2");
	   fail_unless(strtok(test3, delims) == NULL, 
		       "there is only member of delims in test3");
}
END_TEST

START_TEST(memset_simple_test)
{
	char buf[10];
	unsigned int i;
	
	for(i = 0; i< 10; i++) 
		buf[i] = 0;
	
	fail_unless(memset(buf, 'I', 9) == buf, "memset: error on simple test");
	fail_unless(strspn(buf, "I") == 9, "memset: bad data");
	
}
END_TEST

START_TEST(memset_wordsize_alignment)
{
	char buf1[17], *buf2;
	unsigned int i, count, align;
	
	for(i = 0; i< 17; i++) 
		buf1[i] = 0;
	
	#if UINTPTR_MAX == UINT32_MAX
		align = 4;
	#elif UINTPTR_MAX == UINT64_MAX
		align = 8; 
	#endif
	for(i = 1; i < align; i++) {
	
		buf2 = buf1 + i;
		count = align - i;
		
		fail_unless(memset(buf1, 'I', 16) == buf1, "memset: error on aligned case");
		fail_unless(strspn(buf1, "I") == 16, "memset: bad data or memory corruption by writing to wrong address");
		
		fail_unless(memset(buf2, 'A', count) == buf2, "memset: error on unaligned start & aligned end");
		fail_unless(strspn(buf2, "A") == count, "memset: bad data or memory corruption by writing to wrong address");
		
		fail_unless(memset(buf2, 'B', 8) == buf2, "memset: error on unaligned start & unaligned end");
		fail_unless(strspn(buf2, "B") == 8, "memset: bad data or memory corruption by writing to wrong address");
		
		fail_unless(memset(buf1, 'C', 4) == buf1, "memset: error on aligned start & unaligned end");
		fail_unless(strspn(buf1, "C") == 4, "memset: bad data or memory corruption by writing to wrong address");
	}
}
END_TEST

#define PAGESIZE 4096
#define PAGEBITS 12
START_TEST(memset_pagesize_alignment)
{
	char *buf1, *buf2;
	unsigned int i;
	
	buf1 = (char *)malloc(PAGESIZE+16);
	fail_unless(buf1 != 0, "not enough memory on malloc");
	
	for (i = 0; i < PAGESIZE+16; i++)
		buf1[i] = 0;
	/*find the page boundary and make buf2 points to a few bytes before this boundary */
	buf2 = (char *)(((((uintptr_t)buf1 + PAGESIZE) >> PAGEBITS) << PAGEBITS) - 8);		

	fail_unless(memset(buf2, 'I', 16) == buf2, "memset: error on setting a region cross page boundary");
	fail_unless(strspn(buf2, "I") == 16, "memset: bad data or memory corruption by writing to wrong address");
	
	free(buf1);
}
END_TEST

START_TEST(strlen_length)
{
	char s1[] = "aaaaa";
		   
	fail_unless(strlen(s1) == 5, "returned the length of a string");
}
END_TEST

/* setjmp_simple globals and helper functions */
static jmp_buf env;
static int     expect = 0;

static void
jump(int val)
{
        //printf("jump(%d)\n", val);
        longjmp(env, val);
}

START_TEST(setjmp_simple)
{
        int val;

        val = setjmp(env);
        //printf("%d <- setjmp()\n", val);
        fail_unless(val == expect, "setjmp returned wrong value");

        /* FIXME: need a more systematic way of testing */
        if (val == 0) {
                expect = 1;
                jump(0);
        } else if (val == 1) {
                jump(expect = 5);
        } else if (val == 5) {
                jump(expect = 12);
        } else if (val == 12) {
                jump(expect = 13);
        }
}
END_TEST

START_TEST(malloc_simple)
{
	void *mem = malloc(213);

	free(mem);
}
END_TEST

START_TEST(free_simple)
{
	/* check we can free NULL */
	free(NULL);
}
END_TEST

START_TEST(calloc_simple)
{
	char *mem = calloc(128, 1);
	int i;

	/* check that calloc returns zeroed memory */
	for (i = 0; i < 10; i++)
		fail_unless(mem[i] == 0, "calloc didn't zero memory");

	free(mem);
}
END_TEST

START_TEST(strtoul_hex)
{
       char *str = "0x06040000&0xffff0000";
       char *next;
       unsigned long val;
       
       val = strtoul(str, &next, 0);
       fail_unless(val == 0x06040000, "Value not calculated correctly. Guess base.");

       val = strtoul(str, &next, 0x10);
       fail_unless(val == 0x06040000, "Value not calculated correctly. Explicit base.");
}
END_TEST

START_TEST(strtoul_upper_hex)
{
	char *str = "0xaBcDeF";
	char *next;
	unsigned long val;
	
	val = strtoul(str, &next, 0);
	fail_unless(val == 0xabcdef, "Value not calculated correctly.");
}
END_TEST

START_TEST(strtoul_decimal)
{
       char *str = "6040000&37";
       char *next;
       unsigned long val;
       
       val = strtoul(str, &next, 0);
       fail_unless(val == 6040000, "Value not calculated correctly");

       val = strtoul(str, &next, 10);
       fail_unless(val == 6040000, "Value not calculated correctly");
}
END_TEST


Suite *
make_test_libs_c_suite(void)
{
	Suite *suite;
	TCase *tc;

	suite = suite_create("libc tests");

	tc = tcase_create("strcmp"); 
	tcase_add_test(tc, strcmp_equal);
	tcase_add_test(tc, strcmp_nequal);
	tcase_add_test(tc, strcmp_different);
	suite_add_tcase(suite, tc);

	tc = tcase_create("strncmp"); 
	tcase_add_test(tc, strncmp_equal);
	tcase_add_test(tc, strncmp_partially_equal);
	tcase_add_test(tc, strncmp_partially_different);
	suite_add_tcase(suite, tc);

	tc = tcase_create("strstr"); 
	tcase_add_test(tc, strstr_exists);
	tcase_add_test(tc, strstr_empty);
	tcase_add_test(tc, strstr_nexists);
	suite_add_tcase(suite, tc);
	
	tc = tcase_create("memcmp"); 
	tcase_add_test(tc, memcmp_equal);
	tcase_add_test(tc, memcmp_unequal);
	suite_add_tcase(suite, tc);

	tc = tcase_create("memcpy"); 
	tcase_add_test(tc, memcpy_nooverlap);
	suite_add_tcase(suite, tc);
	
	tc = tcase_create("memmove"); 
	tcase_add_test(tc, memmove_noorwithoverlap);
	suite_add_tcase(suite, tc);
	
	tc = tcase_create("strcpy"); 
	tcase_add_test(tc, strcpy_test);
	suite_add_tcase(suite, tc);
	
	tc = tcase_create("strncpy"); 
	tcase_add_test(tc, strncpy_oversized_destination);
	suite_add_tcase(suite, tc);

	tc = tcase_create("sprintf"); 
	tcase_add_test(tc, sprintf_test);
	suite_add_tcase(suite, tc);

	tc = tcase_create("snprintf"); 
	tcase_add_test(tc, snprintf_test);
	suite_add_tcase(suite, tc);

	tc = tcase_create("format");
	tcase_add_test(tc, format_left_justify);
	suite_add_tcase(suite, tc);

	tc = tcase_create("stdout"); 
	tcase_add_test(tc, stdout_fputs_test);
	suite_add_tcase(suite, tc);

	tc = tcase_create("stdio"); 
	tcase_add_test(tc, ungetc_test);
	tcase_add_test(tc, ungetc_multiple_test);
	suite_add_tcase(suite, tc);
	
	tc = tcase_create("memchr"); 
	tcase_add_test(tc, memchr_findorno);
	suite_add_tcase(suite, tc);
	
	tc = tcase_create("strchr"); 
	tcase_add_test(tc, strchr_findorno);
	suite_add_tcase(suite, tc);
	
	tc = tcase_create("strcat");
        tcase_add_test(tc, strcat_stickatend);
        suite_add_tcase(suite, tc);
	
	tc = tcase_create("strncat");
        tcase_add_test(tc, strncat_stickatend);
        suite_add_tcase(suite, tc);
	
	tc = tcase_create("strcspn");
	tcase_add_test(tc, strcspn_inornot);
        suite_add_tcase(suite, tc);
	
	tc = tcase_create("strpbrk");
	tcase_add_test(tc, strpbrk_inornot);
        suite_add_tcase(suite, tc);
	
	tc = tcase_create("strrchr");
	tcase_add_test(tc, strrchr_inornot);
        suite_add_tcase(suite, tc);
	
	tc = tcase_create("strspn");
	tcase_add_test(tc, strspn_inornot);
        suite_add_tcase(suite, tc);
	
	tc = tcase_create("strtok");
	tcase_add_test(tc, strtok_split);
        suite_add_tcase(suite, tc);
	
	tc = tcase_create("memset");
	tcase_add_test(tc, memset_simple_test);
	tcase_add_test(tc, memset_wordsize_alignment);
	tcase_add_test(tc, memset_pagesize_alignment);
        suite_add_tcase(suite, tc);
	
	tc = tcase_create("strlen");
	tcase_add_test(tc, strlen_length);
        suite_add_tcase(suite, tc);
	
	tc = tcase_create("setjmp");
	tcase_add_test(tc, setjmp_simple);
        suite_add_tcase(suite, tc);

	tc = tcase_create("malloc");
	tcase_add_test(tc, malloc_simple);
	tcase_add_test(tc, free_simple);
	tcase_add_test(tc, calloc_simple);
	suite_add_tcase(suite, tc);

	tc = tcase_create("strto");
	tcase_add_test(tc, strtoul_decimal);
	tcase_add_test(tc, strtoul_hex);
	tcase_add_test(tc, strtoul_upper_hex);
	suite_add_tcase(suite, tc);

	fs_test_init(suite);

	return suite;
}
