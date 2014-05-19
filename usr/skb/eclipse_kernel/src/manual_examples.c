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
 * External predicate examples from the SEPIA user manual
 */


#include "external.h"


int
p_succ_ints(value val1, type tag1,
	value val2, type tag2)
{

        Check_Integer(tag1);
        Check_Integer(tag2);

        Succeed_If(val1.nint + 1 == val2.nint);
}


int
p_print_array(value va, type ta,
	value vm, type tm)
{
        int        size = 1;  /* number of array elements */
        pword      *p;
        uword     *dim;
        dident     wdid;
        int        arity;

        Error_If_Ref(ta);
        if (IsAtom(ta))                        /* a global variable */
                wdid = va.did;
        else                                   /* an array */
        {
                Get_Functor_Did(va, ta, wdid);
                Get_Visible_Array_Header(wdid, vm, tm, p);
                if (!IsStructure(p->tag)) /* error if not prolog */
                {
                        Error(TYPE_ERROR);
                }
                dim = (uword *) p->val.ptr;
                dim++;                        /* skip the did */
                for (arity = DidArity(wdid); arity; arity--)
                        size *= *dim++;        /* compute the size */
        }
        Get_Visible_Array_Address(wdid, vm, tm, p);
        for (; size; size--, p++)
        {
                Write(p->val, p->tag, Current_Output);
                Fprintf(Current_Output, " ");
        }
        Succeed;
}        


int
p_sincos(value val_arg, type tag_arg,
	value val_sin, type tag_sin,
	value val_cos, type tag_cos)
{
        extern void sincos();       /* from the math library */
        double s, c;
        Prepare_Requests;

        Error_If_Ref(tag_arg);
        Check_Output_Float(tag_sin);
        Check_Output_Float(tag_cos);

        if (IsDouble(tag_arg))
            sincos(Dbl(val_arg), &s, &c);
        else if (IsInteger(tag_arg))
            sincos((double) val_arg.nint, &s, &c);
        else
        {
            Error(TYPE_ERROR);
        }
        Request_Unify_Float(val_sin, tag_sin, s);
        Request_Unify_Float(val_cos, tag_cos, c);
        Return_Unify;
}


int
p_transform(value val1, type tag1,
	value val2, type tag2)
{
        pword     *p = Gbl_Tg;
        dident    did1;    /* the DID of the structure */
        int       arity;      /* its arity */
        int       i;

        /* the first argument must be a structure */
        Check_Structure(tag1);
        /* the second argument must be a structure or a variable */
        Check_Output_Structure(tag2);
        /* val1 points to the functor */
        did1 = val1.ptr->val.did;
        arity = DidArity(did1);
        /* reserve space for the functor and (arity + 1) args */
        Gbl_Tg += arity + 2;
        /* insert the functor - the same name and higher arity */
        p[0].tag.kernel = TDICT;
        p[0].val.did = Did(DidName(did1), arity + 1);
        /* copy the arguments */
        for (i = 1; i <= arity; i++)
        {
                p[i].tag.all = val1.ptr[i].tag.all;
                p[i].val.all = val1.ptr[i].val.all;
                /* on some machines use p[i] = val1.ptr[i] */
        }
        /* now create the free variable in the last argument;
         * it is a self-reference
         */
        p[arity + 1].tag.kernel = TREF;
        p[arity + 1].val.ptr = p + (arity + 1);
        /* and unify with the second argument */
        Return_Unify_Structure(val2, tag2, p);
}


int
p_get_env(value v0, type t0,
	value v1, type t1)
{
        extern char *getenv();
        char *name;
        value v;

        Get_Name(v0,t0,name)
        name = getenv(name);
        if(name == (char *) 0)
        {
                Fail;
        }
        Cstring_To_Prolog(name, v);
        Return_Unify_String(v1, t1, v.ptr)
}


int
p_member(value velt, type telt,
	value vlist, type tlist)
{
        pword *p;

        /* we require a list or nil */
        Check_List(tlist);
        /* if the list is empty, we fail */
        if(IsNil(tlist))
        {
                Fail;
        }
        /* the tail of the list */
        p = vlist.ptr + 1;
        /* must be dereferenced! */
        Dereference(p);
        /*
        * on backtracking we will get the tail of the list
        * instead of the list itself
        */
        Remember(2, p->val, p->tag);
        /*
        * and we behave as the unification
        * of the element and the head
        */
        Return_Unify_Pw(velt, telt,
                vlist.ptr->val, vlist.ptr->tag);
}


int
p_p2(value v1, type t1, value v2, type t2)
{
        char        *result;
        value       new_v2;
        type        new_t2;

        /* first check the arguments */
        Check_Integer(t2);
        Check_Output_Atom(t1);
        /* take note of new resatisfaction */
        new_v2.nint = v2.nint + 1;
        new_t2.kernel = TINT;
        Remember(2, new_v2, new_t2);
        /* get the string that corresponds to the value of v2 */
        switch(v2.nint)
        {
                case 1:
                        result = "a";
                        break;
                case 2:
                        result = "b";
                        break;
                case 3:
                        result = "c";
                        break;
                default:
                        Fail;
        }
        Return_Unify_Atom(v1, t1, Did(result, 0));
}


int
p_diff_vars(value v1, type t1, value v2, type t2)
{
        if (IsRef(t1) && IsRef(t2) && v1.ptr != v2.ptr)
        {
                Mark_Suspending_Variable(v1.ptr);
                Mark_Suspending_Variable(v2.ptr);
                Succeed;
        }
        else
                Fail;
}


int
p_atomd(value v1, type t1)
{
        if (IsRef(t1))
        {
                Mark_Suspending_Variable(v1.ptr);
                Delay;
        }
        else
                Succeed_If(IsAtom(t1));
}

