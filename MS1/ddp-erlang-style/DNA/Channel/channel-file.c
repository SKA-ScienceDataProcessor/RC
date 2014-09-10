#include <stdio.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/mman.h>
#include <unistd.h>

#ifndef	__O_DIRECT
#warning "__O_DIRECT is undefined, defining as 0."
#define	__O_DIRECT	0
#endif

#ifndef	MAP_ANONYMOUS
#warning "defining MAP_ANONYMOUS as MAP_ANON."
#define	MAP_ANONYMOUS	MAP_ANON
#endif

/* XXX error handling: should we return an ssize_t
   XXX types: buffer size is a ssize_t, not int
*/

void read_data(double *buf, long n, long o, char *p)
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
double* read_data_mmap(long n, long o, char *p)
{
  int fd;
  size_t real_size = n * sizeof(double);
  ssize_t read_size;
  double *mapping;

  fd = open(p, O_RDONLY | __O_DIRECT);
  assert(fd > 0);
  mapping = (double*)mmap(NULL, real_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  assert(mapping != NULL);
  read_size= pread(fd, (void*)mapping, real_size, o);
//  printf("mapping %p, n %ld, o %ld, real_size %ld, read_size %ld.\n", mapping, n, o, real_size, read_size);
  if (real_size != read_size) {
    perror("failure context");
    printf("failure context: mapping %p, n %ld, o %ld, real_size %ld, read_size %ld.\n", mapping, n, o, real_size, read_size);
  }
  assert(real_size == read_size);
  close(fd);

  return mapping;
}
