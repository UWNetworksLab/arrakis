/**
 * \file
 * \brief Argument processing for distributed services
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich. 
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ARGS_H__
#define __ARGS_H__

/**
 * \struct args
 * \brief generic distributed service arguments passed to a dispatcher
 */
struct dist_args {
    char *path;			/*< pathname of the service to start */
    coreid_t *cores;		/*< list of cores to run on */
    int cores_len;		/*< lenght of list */
    //    coreid_t *exclude;		/*< list core to explicitly *not* run on */
    //    int exclude_len;		/*< length of list */
    int num_cores;		/*< total number of cores to use */
    //    bool all_cores;		/*< run on all available cores */
    bool master;		/*< this dispatcher is the master */
};

/**
 * \brief Process and return the generic distributed service command-line 
 *        arguments.
 * 
 * The generic distributed service command-line arguments are processed, and
 * appropriate values set in the return struct.  In particular, this procedire
 * creates and returns a list of cores to start the service on based on the
 * arguments and possibly querying the SKB. 
 * 
 * After processing argc and argv are modified so that argv points to all 
 * un-recognised arguments. In this way, service-specific arguments can still 
 * be processed separately.
 * 
 * @param argc a pointer to argc as passed to main(). argc will be modified.
 * @param argv argv as passed to main(). argv will be modified.
 * @return A struct containing the values of the processed arguments.  The list 
 *         elements of the struct are malloced (and so must be freed by the 
 *         caller), but the path element is not.
 */
struct dist_args process_dist_args(int *argc, char **argv[]);

/**
 * \brief Convert an array of coreids into a string representation of the list.
 *
 *	  The string looks like: "1,2,3,".  This function mallocs the memory 
 *        for the string. The calelr is therefore responsible for freeing it.
 *
 * @param list the list to convert. 
 * @param l_len the number of elements in the list.
 * @return The string representation of the list. This function mallocs the 
 *         memory for the string. The caller is therefore responsible for 
 *         freeing it.
 */
char *list_to_string(coreid_t list[], size_t l_len);

#endif

