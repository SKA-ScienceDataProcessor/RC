#include <stdio.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/mman.h>
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


/* Return pointer to data read through mmap.
 */
double* read_data_mmap(int n, int o, char *p)
{
  int fd;

  int real_size = n * sizeof(double);

  double *mapping;

  fd = open(p, O_RDONLY | __O_DIRECT);
  assert(fd > 0);
  mapping = (double*)mmap(NULL, real_size, PROT_READ, MAP_ANONYMOUS, -1, 0);
  assert(mapping != NULL);
  assert( pread(fd, (void*)mapping, real_size, o) == real_size);
  close(fd);
  return mapping;
}
