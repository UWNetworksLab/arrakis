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
 * Copyright (C) 1998-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe SAMPLE CODE
 *
 * $Id: eg_c_test.c,v 1.1 2008/06/30 17:43:54 jschimpf Exp $
 *
 * AUTHOR:		Joachim Schimpf
 *
 * DESCRIPTION:
 *	Test all features of the embedding interface.
 */

#include	"eclipse.h"

long	longs[] = {1,2,3,4,5};
double	doubles[] = {1.1,2.2,3.3,4.4,5.5};
char	chars[] = "chars";
pword	pwords[5];


static void
echo_stream(int stream)
{
    char buf[1024];
    int n;

    while ((n = ec_queue_read(stream, buf, 1023)) > 0)
    {
	buf[n] = 0;
	printf("%s", buf);
    }
}

static int
ec_resume_flush(void)
{
    int res;
    long arg;
    for(;;)
    {
    	res = ec_resume_long(&arg);
	switch (res)
	{
	case PFLUSHIO:
	    echo_stream((int) arg);
	    break;
	default:
	    return res;
	}
    }
}

static int
ec_handle_events_flush(void)
{
    int res;
    long arg;
    for(;;)
    {
    	res = ec_handle_events(&arg);
	switch (res)
	{
	case PFLUSHIO:
	    echo_stream((int) arg);
	    break;
	default:
	    return res;
	}
    }
}


#ifdef TEST_ASYNC

#define ec_resume ec_resume_special

int
ec_resume_special(void)
{
    int res = ec_resume_async();
    if (res != PSUCCEED)
	printf("PROBLEM: ec_resume_async() = %d\n", res);
    do
	res = ec_resume_status();
    while (res == PRUNNING);
    printf("ec_resume() = %d\n", res);
    return res;
}

#endif


main(int argc, char **argv)
{
    char 	*s;
    dident	a;
    long	n;
    double	d;
    ec_ref	Vars,X;
    ec_refs	YZ;
    pword 	pw1,pw2,pw3,pw4;
    
    ec_set_option_int(EC_OPTION_ARGC, argc);
    ec_set_option_ptr(EC_OPTION_ARGV, argv);
#define TEST_MEMORY_IO
#ifdef TEST_MEMORY_IO
    ec_set_option_int(EC_OPTION_IO, MEMORY_IO);
#endif

    if (ec_init())
    	goto _problem_;


    /*----------------------------------------*/
    printf("Testing ec_exec_string()\n");
    /*----------------------------------------*/
    Vars = ec_ref_create(ec_nil());
    ec_exec_string("X=hello,atom_string(X,Y),Z is string_length(Y)+1",Vars);
    if (PSUCCEED == ec_var_lookup(Vars,"X",&pw1) &&
    	PSUCCEED == ec_get_atom(pw1,&a) &&
        PSUCCEED == ec_var_lookup(Vars,"Y",&pw1) &&
    	PSUCCEED == ec_get_string(pw1,&s) &&
        PSUCCEED == ec_var_lookup(Vars,"Z",&pw1) &&
    	PSUCCEED == ec_get_long(pw1,&n)
	)
    {
	printf("Answer was X=%s, Y=\"%s\", Z=%d\n",DidName(a),s,n);
    }
    else
    	goto _problem_;
    ec_ref_destroy(Vars);


    /*----------------------------------------*/
    printf("Testing ec_post_goal()\n");
    /*----------------------------------------*/
    YZ = ec_refs_create(2, ec_long(5));	/* try create-destroy sequence */
    ec_refs_destroy(YZ);

    X = ec_ref_create_newvar();
    YZ = ec_refs_create_newvars(2);

    ec_post_goal(ec_term(ec_.d.unify,
   			 ec_ref_get(X),
			ec_atom(ec_did("hello",0))));
    ec_post_goal(ec_term(ec_did("atom_string",2),
    			ec_ref_get(X),
    			ec_refs_get(YZ,0)));
    ec_post_goal(ec_term(ec_did("is",2),
    			ec_refs_get(YZ,1),
			ec_term(ec_.d.plus,
			    ec_term(ec_did("string_length",1), ec_refs_get(YZ,0)),
			    ec_long(1))));
    ec_post_string("writeln(done)");

    if (PSUCCEED == ec_resume_flush() &&
    	PSUCCEED == ec_get_atom(ec_ref_get(X),&a) &&
    	PSUCCEED == ec_get_string(ec_refs_get(YZ,0),&s) &&
    	PSUCCEED == ec_get_long(ec_refs_get(YZ,1),&n)
	)
    {
	printf("Answer was X=%s, Y=\"%s\", Z=%d\n", DidName(a),s,n);
    }
    else
    	goto _problem_;
    ec_ref_destroy(X);


    /*----------------------------------------*/
    printf("Testing constructors\n");
    /*----------------------------------------*/
    X = ec_ref_create_newvar();
    pwords[0] = ec_long(0);
    pwords[1] = ec_long(1);
    pwords[2] = ec_long(2);
    pwords[3] = ec_long(3);
    pwords[4] = ec_long(4);
    ec_post_goal(ec_term(ec_.d.unify, ec_ref_get(X),
    	ec_list(ec_listofdouble(5,doubles),
    	ec_list(ec_listoflong(5,longs),
    	ec_list(ec_listofchar(5,chars),
    	ec_list(ec_string("hello"),
	ec_list(ec_atom(ec_did("world",0)),
	ec_list(ec_long(123456),
	ec_list(ec_double(3.14),
	ec_list(ec_term_array(ec_did("five",5),pwords),
	ec_list(ec_listofrefs(YZ),
	ec_list(ec_newvar(),
	ec_list(ec_arrayofdouble(5,doubles),
	ec_list(ec_matrixofdouble(1,5,doubles),
	ec_list(ec_matrixofdouble(2,2,doubles),
    	ec_nil())))))))))))))));
    if (PSUCCEED != ec_resume())
    	goto _problem_;

    ec_post_goal(ec_atom(ec_did("garbage_collect",0)));
    if (PSUCCEED != ec_resume())
    	goto _problem_;
    ec_post_goal(ec_term(ec_did("writeln",1), ec_ref_get(X)));
    if (PSUCCEED != ec_resume_flush())
    	goto _problem_;

    /*----------------------------------------*/
    printf("Testing assignment\n");
    /*----------------------------------------*/
    ec_refs_set(YZ, 1, ec_long(77));
    if (!(ec_get_long(ec_refs_get(YZ,1),&n) == PSUCCEED && n == 77))
    	goto _problem_;

    /*----------------------------------------*/
    printf("Testing checking and decomposition\n");
    /*----------------------------------------*/
    pw1 = ec_ref_get(X);
    if (ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
    	ec_get_list(pw2,&pw3,&pw4) == PSUCCEED &&
    	ec_get_double(pw3,&d) == PSUCCEED
    )
	printf("d = %f\n", d);
    else
    	goto _problem_;

    if (ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
    	ec_get_list(pw2,&pw3,&pw4) == PSUCCEED &&
    	ec_get_long(pw3,&n) == PSUCCEED
    )
	printf("n = %d\n", n);
    else
    	goto _problem_;

    if (ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
    	ec_get_list(pw2,&pw3,&pw4) == PSUCCEED &&
    	ec_get_long(pw3,&n) == PSUCCEED
    )
	printf("n = %d\n", n);
    else
    	goto _problem_;

    if (ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
    	ec_get_string(pw2,&s) == PSUCCEED &&
    	ec_get_string_length(pw2,&s,&n) == PSUCCEED
    )
	printf("s = %s, len = %d\n", s, n);
    else
    	goto _problem_;

    if (ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
    	ec_get_atom(pw2,&a) == PSUCCEED
    )
	printf("a = %s\n", DidName(a));
    else
    	goto _problem_;

    if (ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
        ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
        ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
        ec_get_functor(pw2,&a) == PSUCCEED &&
        ec_get_arg(3,pw2,&pw3) == PSUCCEED &&
    	ec_get_long(pw3,&n) == PSUCCEED
    )
	printf("functor = %s/%d, n = %d\n", DidName(a), ec_arity(pw2), n);
    else
    	goto _problem_;

    if (ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
        ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
        ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
        ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
        ec_get_list(pw1,&pw2,&pw1) == PSUCCEED &&
        ec_get_nil(pw1) == PSUCCEED
    )
	printf("end\n", n);
    else
    	goto _problem_;

    ec_post_event(ec_atom(ec_did("hello",0)));
    if (ec_handle_events_flush() != PSUCCEED)
    	goto _problem_;

    ec_refs_destroy(X);
    ec_refs_destroy(YZ);
    ec_cleanup();
    return 0;

_problem_:
    printf("PROBLEM!!!\n");
    return -1;
}

