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

  int page_size = sysconf(_SC_PAGE_SIZE);

  int offset_within_page = (o % page_size);

  int index_within_page = offset_within_page / sizeof(double);

  int page_gran_offset = o - offset_within_page;

  int real_size = n * sizeof(double);

  double *mapping;

  fd = open(p, O_RDONLY | __O_DIRECT);
  assert(fd > 0);
  mapping = (double*)mmap(NULL, real_size, PROT_READ, MAP_PRIVATE, fd, page_gran_offset);
  assert(mapping != NULL);
  close(fd);
  return &mapping[index_within_page];
}
