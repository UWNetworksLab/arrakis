/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
	SccsId = "%W%        %E%"
*/

/*LINTLIBRARY*/
#include "gmp.h"

void mp_set_memory_functions(a,r,f) void *(*a)(),*(*r)(),(*f)(); {}

/**************** Integer (i.e. Z) routines.  ****************/

void mpz_init(x) MP_INT *x; {}
void mpz_set(x,y) MP_INT *x; MP_INT *y; {}
void mpz_set_ui(x,y) MP_INT *x; unsigned long int y; {}
void mpz_set_si(x,y) MP_INT *x; long int y; {}
int mpz_set_str(x,y,z) MP_INT *x; char *y; int z; {return 0;}
void mpz_init_set(x,y) MP_INT *x; MP_INT *y; {}
void mpz_init_set_ui(x,y) MP_INT *x; unsigned long int y; {}
void mpz_init_set_si(x,y) MP_INT *x; long int y; {}
int mpz_init_set_str(x,y,z) MP_INT *x; char *y; int z; {return 0;}
unsigned long int mpz_get_ui(x) MP_INT *x; {return 0;}
long int mpz_get_si(x) MP_INT *x; {return 0;}
char * mpz_get_str(x,y,z) char *x; int y; MP_INT *z; {return 0;}
void mpz_clear(x) MP_INT *x; {}
void * _mpz_realloc(x,y) MP_INT *x; mp_size y; {return 0;}
void mpz_add(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
void mpz_add_ui(x,y,z) MP_INT *x; MP_INT *y; unsigned long int z; {}
void mpz_sub(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
void mpz_sub_ui(x,y,z) MP_INT *x; MP_INT *y; unsigned long int z; {}
void mpz_mul(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
void mpz_mul_ui(x,y,z) MP_INT *x; MP_INT *y; unsigned long int z; {}
void mpz_div(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
void mpz_div_ui(x,y,z) MP_INT *x; MP_INT *y; unsigned long int z; {}
void mpz_mod(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
void mpz_mod_ui(x,y,z) MP_INT *x; MP_INT *y; unsigned long int z; {}
void mpz_divmod(x,y,z,d) MP_INT *x, *y, *z, *d; {}
void mpz_divmod_ui(x,y,z,d) MP_INT *x, *y, *z; unsigned long int d; {}
void mpz_mdiv(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
void mpz_mdiv_ui(x,y,z) MP_INT *x; MP_INT *y; unsigned long int z; {}
void mpz_mmod(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
unsigned long int mpz_mmod_ui(x,y,z) MP_INT *x, *y; unsigned long int z;
	{return 0;}
void mpz_mdivmod(x,y,z,d) MP_INT *x, *y, *z, *d; {}
unsigned long int mpz_mdivmod_ui(x,y,z,d)
	MP_INT *x, *y, *z; unsigned long int d;
	{return 0;}
void mpz_sqrt(x,y) MP_INT *x; MP_INT *y; {}
void mpz_sqrtrem(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
int mpz_perfect_square_p(x) MP_INT *x; {return 0;}
int mpz_probab_prime_p(x,y) MP_INT *x; int y; {return 0;}
void mpz_powm(x,y,z,m) MP_INT *x; MP_INT *y; MP_INT *z; MP_INT *m; {}
void mpz_powm_ui(x,y,z,m) MP_INT *x,*y; unsigned long int z; MP_INT *m; {}
void mpz_pow_ui(x,y,z) MP_INT *x; MP_INT *y; unsigned long int z; {}
void mpz_fac_ui(x,y) MP_INT *x; unsigned long int y; {}
void mpz_gcd(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
void mpz_gcdext(x,y,z,a,b) MP_INT *x,*y,*z,*a,*b; {}
void mpz_neg(x,y) MP_INT *x; MP_INT *y; {}
void mpz_abs(x,y) MP_INT *x; MP_INT *y; {}
int mpz_cmp(x,y) MP_INT *x; MP_INT *y; {return 0;}
int mpz_cmp_ui(x,y) MP_INT *x; unsigned long int y; {return 0;}
int mpz_cmp_si(x,y) MP_INT *x; long int y; {return 0;}
void mpz_mul_2exp(x,y,z) MP_INT *x; MP_INT *y; unsigned long int z; {}
void mpz_div_2exp(x,y,z) MP_INT *x; MP_INT *y; unsigned long int z; {}
void mpz_mod_2exp(x,y,z) MP_INT *x; MP_INT *y; unsigned long int z; {}
void mpz_and(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
void mpz_ior(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
void mpz_xor(x,y,z) MP_INT *x; MP_INT *y; MP_INT *z; {}
void mpz_com(x,y) MP_INT *x; MP_INT *y; {}

#ifdef __STDC__
#if defined (FILE) || defined (_STDIO_H) || defined (__STDIO_H__)
void mpz_inp_raw(x,y) MP_INT *x; FILE *y; {}
void mpz_inp_str(x,y,z) MP_INT *x; FILE *y; int z; {}
void mpz_out_raw(x,y) FILE *x; MP_INT *y; {}
void mpz_out_str(x,y,z) FILE *x; int y; MP_INT *z; {}
#endif
#else
void mpz_inp_raw(x,y) MP_INT *x; int *y; {}
void mpz_inp_str(x,y,z) MP_INT *x; int *y; int z; {}
void mpz_out_raw(x,y) int *x; MP_INT *y; {}
void mpz_out_str(x,y,z) int *x; int y; MP_INT *z; {}
#endif

void mpz_array_init(x,y,z) MP_INT x[]; size_t y; mp_size z; {}
void mpz_random(x,y) MP_INT *x; mp_size y; {}
void mpz_random2(x,y) MP_INT *x; mp_size y; {}
size_t mpz_size(x) MP_INT *x; {return 0;}
size_t mpz_sizeinbase(x,y) MP_INT *x; int y; {return 0;}

/**************** Rational(x,y) i.e. Q) routines.  ****************/

void mpq_init(x) MP_RAT *x; {}
void mpq_clear(x) MP_RAT *x; {}
void mpq_set(x,y) MP_RAT *x; MP_RAT *y; {}
void mpq_set_ui(x,y,z) MP_RAT *x; unsigned long int y; unsigned long int z; {}
void mpq_set_si(x,y,z) MP_RAT *x; long int y; unsigned long int z; {}
void mpq_add(x,y,z) MP_RAT *x; MP_RAT *y; MP_RAT *z; {}
void mpq_sub(x,y,z) MP_RAT *x; MP_RAT *y; MP_RAT *z; {}
void mpq_mul(x,y,z) MP_RAT *x; MP_RAT *y; MP_RAT *z; {}
void mpq_div(x,y,z) MP_RAT *x; MP_RAT *y; MP_RAT *z; {}
void mpq_neg(x,y) MP_RAT *x; MP_RAT *y; {}
int mpq_cmp(x,y) MP_RAT *x; MP_RAT *y; {return 0;}
void mpq_inv(x,y) MP_RAT *x; MP_RAT *y; {}
void mpq_set_num(x,y) MP_RAT *x; MP_INT *y; {}
void mpq_set_den(x,y) MP_RAT *x; MP_INT *y; {}
void mpq_get_num(x,y) MP_INT *x; MP_RAT *y; {}
void mpq_get_den(x,y) MP_INT *x; MP_RAT *y; {}

/************ Low level positive-integer(x,y) i.e. N) routines.  ************/

mp_limb mpn_add(a,b,c,d,e) mp_ptr a; mp_srcptr b; mp_size c; mp_srcptr d; mp_size e; {return 0;}
mp_size mpn_sub(a,b,c,d,e) mp_ptr a; mp_srcptr b; mp_size c; mp_srcptr d; mp_size e; {return 0;}
mp_size mpn_mul(a,b,c,d,e) mp_ptr a; mp_srcptr b; mp_size c; mp_srcptr d; mp_size e; {return 0;}
mp_size mpn_div(a,b,c,d,e) mp_ptr a; mp_ptr b; mp_size c; mp_srcptr d; mp_size e; {return 0;}
mp_limb mpn_divmod_1(a,b,c,d,e) mp_ptr a; mp_srcptr b; mp_size c; mp_limb e; {return 0;}
mp_limb mpn_mod_1(a,b,c) mp_srcptr a; mp_size b; mp_limb c; {return 0;}
mp_limb mpn_lshift(a,b,c,d) mp_ptr a; mp_srcptr b; mp_size c; unsigned int d; {return 0;}
mp_size mpn_rshift(a,b,c,d) mp_ptr a; mp_srcptr b; mp_size c; unsigned int d; {return 0;}
mp_size mpn_rshiftci(a,b,c,d,e) mp_ptr a; mp_srcptr b; mp_size c;unsigned int d; mp_limb e; {return 0;}
mp_size mpn_sqrt(a,b,c,d) mp_ptr a; mp_ptr b; mp_srcptr c; mp_size d; {return 0;}
int mpn_cmp(a,b,c) mp_srcptr a; mp_srcptr b; mp_size c; {return 0;}
