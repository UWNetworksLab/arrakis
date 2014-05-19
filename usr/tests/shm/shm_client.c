/*
 * shm-client - client program to demonstrate shared memory.
 */
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>

#define SHMSZ     27

int main(int argc, char *argv[])
{
    int shmid;
    key_t key;
    char *shm, *s;

    /*
     * We need to get the segment named
     * "5678", created by the server.
     */
    key = 5678;

    /*
     * Locate the segment.
     */
    if ((shmid = shmget(key, SHMSZ, 0666)) < 0) {
         debug_printf("client: error in shmid\n");
        return 1;
    }

    debug_printf("client: shmget performed correctly id: %d\n", shmid);

    /*
     * Now we attach the segment to our data space.
     */
    if ((shm = shmat(shmid, NULL, 0)) == (char *) -1) {
        debug_printf("client: error in shmat\n");
        return 1;
    }

    debug_printf("client: shmat performed correctly shm: %p\n", shm);

    /*
     * Now read what the server put in the memory.
     */
    for (s = shm; *s != 0; s++)
        putchar(*s);
    putchar('\n');

    debug_printf("client: data read, writing *\n");

    /*
     * Finally, change the first character of the
     * segment to '*', indicating we have read
     * the segment.
     */
    *shm = '*';

    debug_printf("client: exiting\n");

    return 0;
}
