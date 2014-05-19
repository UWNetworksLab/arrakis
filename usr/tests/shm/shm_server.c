#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>

#define SHMSZ     27

int main(int argc, char *argv[])
{
    char c;
    int shmid;
    key_t key;
    char *shm, *s;

    /*
     * We'll name our shared memory segment
     * "5678".
     */
    key = 5678;

    /*
     * Create the segment.
     */
    if ((shmid = shmget(key, SHMSZ, IPC_CREAT | 0666)) < 0) {
        debug_printf("server: error in shmid\n");
        return 1;
    }

    debug_printf("server: shmget performed correctly id: %d\n", shmid);

    /*
     * Now we attach the segment to our data space.
     */
    if ((shm = shmat(shmid, NULL, 0)) == (char *) -1) {
        debug_printf("server: error in shmat\n");
        return 1;
    }

    debug_printf("server: shmat performed correctly shm: %p\n", shm);

    /*
     * Now put some things into the memory for the
     * other process to read.
     */
    s = shm;

    for (c = 'a'; c <= 'z'; c++)
        *s++ = c;
    *s = 0;

    debug_printf("server: written some data, waiting for *\n");
    /*
     * Finally, we wait until the other process
     * changes the first character of our memory
     * to '*', indicating that it has read what
     * we put there.
     */
    while (*shm != '*') thread_yield();
        //sleep(1);

    debug_printf("server: got it, exiting\n");

    return 0;
}
