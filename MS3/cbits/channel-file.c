
#define _GNU_SOURCE 1

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/stat.h>

#ifndef	O_DIRECT
#warning "O_DIRECT is undefined, defining as 0."
#define	O_DIRECT	0
#endif

#ifndef	MAP_ANONYMOUS
#warning "defining MAP_ANONYMOUS as MAP_ANON."
#define	MAP_ANONYMOUS	MAP_ANON
#endif

/* FIXME: error handling: should we return an ssize_t
   FIXME: types: buffer size is a ssize_t, not int
*/

void read_data(double *buf, long n, long o, char *p)
{
    int fd;
    fd = open(p, O_RDONLY);
    assert(fd > 0);

    const size_t n_bytes = n * sizeof(double);
    const size_t offset  = o * sizeof(double);
    ssize_t n_read;
    n_read = pread(fd, buf, n_bytes, offset);
    assert( n_read == n_bytes );
    close(fd);
}


/* Return pointer to data read through mmap.
 */
double* read_data_mmap(long n, long o, char *p, char *nodeid)
{
  int fd;
  size_t real_size = n * sizeof(double);
  ssize_t read_size;
  double *mapping;

  fd = open(p, O_RDONLY | O_DIRECT);
  assert(fd > 0);
  mapping = (double*)mmap(NULL, real_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  assert(mapping != NULL);
  read_size= pread(fd, (void*)mapping, real_size, o);
  printf("[%s] read: %ld floats. Bytes  %ld at %ld, mapping %p\n", nodeid, n, real_size, o, mapping);
  if (real_size != read_size) {
    printf("[%s]: error: mapping %p, n %ld, o %ld, real_size %ld, read_size %ld.\n", nodeid, mapping, n, o, real_size, read_size);
  }
  assert(real_size == read_size);
  close(fd);

  return mapping;
}

/* Unmap data for vector. We pass data length as pointer because of
 * constraints of ForeignPtr API
 */
void munmap_data(long* n, double* ptr) {
  // FIXME: we don't check return code for munmap
  munmap(ptr, sizeof(double) * (*n));
  free(n);
}
