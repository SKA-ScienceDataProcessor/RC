#include <stdio.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
/* XXX error handling: should we return an ssize_t
   XXX types: buffer size is a ssize_t, not int
*/

void read_data(double *buf, int n, int o, char *p)
{
  int fd;

  fd = open(p, O_RDONLY);
  assert(fd > 0);
  /*  printf("reading %lu bytes at offset %d\n", n* sizeof(double), o); */
  assert( pread(fd, buf, n * sizeof(double), o) == n * sizeof(double) );
  close(fd);
  return;
}
