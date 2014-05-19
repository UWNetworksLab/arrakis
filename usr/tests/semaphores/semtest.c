#include <stdio.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <semaphore.h>

int main(int argc, char *argv[])
{
    sem_t sem;
    debug_printf("semtest start");
    if(argc == 2) {
        // Initializer
        sem_init(&sem, 1, 0);
        assert(sem.id == 1);

        for(int i = 0; i < 10; i++) {
            errval_t err;
            char *args[5];
            args[0] = "program";
            args[1] = NULL;
            err = spawn_program(disp_get_core_id(), "/x86_64/sbin/semtest",
                                args, NULL, SPAWN_NEW_DOMAIN, NULL);
            assert(err_is_ok(err));
        }
    } else {
        sem.id = 1;
        sem.pshared = 1;
    }

    // Worker
    for(;;) {
        if(disp_get_domain_id() % 1 == 0) {
            printf("%d: post\n", disp_get_domain_id());
            sem_post(&sem);
        }
        printf("%d: trywait\n", disp_get_domain_id());
        if(sem_trywait(&sem) == -1) {
            printf("%d: would block\n", disp_get_domain_id());
            printf("%d: wait\n", disp_get_domain_id());
            sem_wait(&sem);
        } else {
            printf("%d: works\n", disp_get_domain_id());
        }

        if(disp_get_domain_id() % 1 == 1) {
            printf("%d: post\n", disp_get_domain_id());
            sem_post(&sem);
        }
    }

    return 0;
}
