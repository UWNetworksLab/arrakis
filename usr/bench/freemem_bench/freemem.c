#include <stdio.h>
#include <barrelfish/barrelfish.h>

static int freemem(void)
{
	struct mem_rpc_client *mc = get_mem_client();
	assert(mc != NULL);
	errval_t err;
	genpaddr_t available, total;

	err = ram_available(&available, &total);
	if(err_is_fail(err)) {
		DEBUG_ERR(err, "available");
		return EXIT_FAILURE;
	}

	printf("Free memory: %"PRIu32" MB (%" PRIu32 " bytes)\n",((uint32_t)available)/1024/1024, (uint32_t)available);
	printf("Total memory: %"PRIu32" MB (%" PRIu32 " bytes)\n",((uint32_t)total)/1024/1024, (uint32_t)total);

	return EXIT_SUCCESS;
}

int main(int argc, char *argv[])
{
  freemem();
  printf("freemem done!\n");
  return EXIT_SUCCESS;
}
