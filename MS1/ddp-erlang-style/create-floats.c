#include <stdio.h> 
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>



long roundUpDiv(long num, long den)
{
  assert (den != 0);
  return (num + den -1) / den;
}

// cC = number of chunks, typically number of compute processes
// cN = chunk number
// iC = item count, how many objects
long chunkSize(long cC, long iC, long cN)
{
  int r;

  if (cN < 1 || cN > cC)
    r =0;
  else if (cN > iC/roundUpDiv(iC, cC) )
    r = iC - (cN-1) * roundUpDiv(iC,cC);
  else
    r = roundUpDiv(iC,cC);
  printf("cC: %d, iC: %d, cN: %d, result: %d\n", cC, iC, cN, r);
  return r;
}

long chunkOffset(long cC, long iC, long cN)
{
  int r;
  if (cN > cC || cN <1)
    r = -1;
  else
    r = (cN-1) * roundUpDiv(iC, cC);
  printf("cC: %d, iC: %d, cN: %d, result: %d\n", cC, iC, cN, r);
  return r;
}

#ifdef  LOCAL_TEST
#define BUF_SIZE        16384
double buffer [BUF_SIZE];
#endif

int main(int argc, char **arg)
{
  int fd;
  off_t size, offset;
  long iC, cN, cC, count;
  long len, current_len;

  double d = 1.0;

  if (argc != 5) {
    printf("usage: %s path itemcount processcount chunkno -- write floats to file \"path\" \n", arg[0]);
    exit(1);
  }

  iC = atol(arg[2]);  // item count  = number of floats
  cC = atol(arg[3]);  // chunk count = number of compute procs
  cN = atol(arg[4]);  // chunk number

  if (cN == 0) {
    fd = open(arg[1], O_CREAT | O_RDWR, 0664);
    assert (fd > 0);
    ftruncate(fd, iC * sizeof(d));
#ifdef  LOCAL_TEST   // for local testing.
    len = iC;
    offset = 0;
    for (int i = 0; i < BUF_SIZE; i++) {
      buffer[i] = d;
    }
    while (len > 0) {
      current_len = len > BUF_SIZE ? BUF_SIZE : len;
      size_t size_to_write = current_len * sizeof(buffer[0]);
      ssize_t written = pwrite(fd, buffer, size_to_write, offset);
      assert (size_to_write == written);
      offset += size_to_write;
      len -= current_len;
    }
#endif
    return 0;
  }

  count = chunkSize(cC, iC, cN);
  size = sizeof(d) * count;
  offset = sizeof(d) * chunkOffset(cC, iC, cN);

  assert(offset >= 0 && count <= iC);
  assert(size >= 0);

  printf("Writing %ld floats of %ld (all 1.0) after %ld floats to %s.\n", count, iC , chunkOffset(cC, iC, cN), arg[1]);
  fd = open(arg[1], O_CREAT | O_RDWR, 0664);
  assert (fd > 0);
/*
  for (int i = 0; i < count ; i++) {
    assert(pwrite(fd, &d, sizeof (d), offset) == sizeof(d));
    offset += sizeof(d);
  }
*/
  return 0;
}

