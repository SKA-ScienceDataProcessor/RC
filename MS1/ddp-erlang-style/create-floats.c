#include <stdio.h> 
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>

int main(int argc, char **arg)
{
  int fd, count; 
  
  double d = 1.0;
  
  if (argc != 3) { 
    printf("usage: %s path count -- write count floats to file \"path\"\n", arg[0]);
    exit(1);
  }

  count = atoi(arg[2]);
  assert(printf("Writing %i floats (all 1.0) to %s\n", count, arg[1]) > 0);
  fd = open(arg[1], O_CREAT | O_RDWR | O_TRUNC, 0664);
  assert (fd > 0);
  for (int i = 0; i < count ; i++)
    assert(write(fd, &d, sizeof (d)) == sizeof(d));
  return 0;
}
