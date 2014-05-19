#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <assert.h>
#include <vfs/vfs.h>

static char testmsg[20] = "This is a test mes.";

int main(int argc, char *argv[])
{
  int sv[2];
  char rcv[20];

  vfs_init();

  int r = socketpair(AF_UNIX, SOCK_STREAM, 0, sv);
  assert(r == 0);
  
  ssize_t r2 = write(sv[0], testmsg, 20);
  assert(r2 == 20);

  ssize_t r3 = read(sv[1], rcv, 20);
  assert(r3 == 20);

  printf("Received: '%s'\n", rcv);

  int pipes[2];
  
  r = pipe(pipes);
  assert(r == 0);

  r2 = write(pipes[1], testmsg, 20);
  assert(r2 == 20);

  r3 = read(pipes[0], rcv, 20);
  assert(r3 == 20);

  printf("Received from pipe: '%s'\n", rcv);
  
  return 0;
}
