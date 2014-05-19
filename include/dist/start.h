/**
 * \file
 * \brief Startup code for distributed services
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich. 
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __START_H__
#define __START_H__

/**
 * \brief Main function that starts work for master and worker dispatchers.
 *
 * Depending on the arguments passed in d_args->master this function starts 
 * a dispatcher running as a master or a worker.  As a master it spwans 
 * worker dispatchers on the given cores (d_args->cores) and then calls
 * the externally defined run_master() function.  As a worker it calls the
 * externally defined run_worker() function.
 *
 * @param d_args the generic distributed service arguments passed to this 
          dispatcher.
 * @param m_args the service-specific arguments passed to this dispatcher.
 * @param name the name of this service.
 * @return an exit value (main() should return this value).
 */
int dist_main(struct dist_args *d_args, void *m_args, char *name);



/**
 * \brief Externally defined function to convert service-specific arguments. 
 *
 * This function must return an array of command line arguments, representing 
 * those in m_args, that can be passed to a worker dispatcher when it is 
 * spawned by a master.
 *
 * @param m_args the service-specific arguments passed to this dispatcher.
 * @param res returns a list of command line arguments (like argv).  This 
 *        is malloced by this function, the caller must free it.
 * @param res_len returns the number of elements in the res list
 * @return success or error code.
 */
errval_t worker_args(void *m_args, char **res[], int *res_len);

/**
 * \brief Externally defined function providing service specific master 
 *        functionality.
 *
 * This function is run after all the workers have been succesfully spawned.
 * In most cases this function won't do much, since the master's main task 
 * is to spawn workers and then coordinate their startup.
 *
 * @param core core ID of this dispatcher.
 * @param cores a list of the cores that the workers run on.
 * @param cores_len the length of the list.
 * @param m_args the service-specific arguments passed to this dispatcher.
 * @param name the name of this service.
 * @return success or error code.
 */
errval_t run_master(coreid_t core, coreid_t *cores, int cores_len,  
                    void *m_args, char *name); 

/**
 * \brief Externally defined function providing service-specific worker 
 *        functionality.
 *
 * This function is run by every worker dispatcher when it is started.
 * This is the main service-specific entry point.  In most cases this 
 * function never returns but starts an infinite dispatch loop.
 *
 * @param core core ID of this dispatcher.
 * @param cores a list of the cores that the workers run on.
 * @param cores_len the length of the list.
 * @param m_args the service-specific arguments passed to this dispatcher.
 * @param name the name of this service.
 * @return success or error code.
 */
errval_t run_worker(coreid_t core, coreid_t *cores, int cores_len, 
                    void *m_args, char *name); 


#endif
