#include <stdio.h> 
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>

// Offsets and lengths, expressed in bytes, should be divisible by 4096 (due to O_DIRECT requirements).
// When expressed as number of doubles, they should be divisible by the following constant.
// The value below evaluates to 512, or 2^9.
// This effectively makes feasible item counts to something like multiplies of 512, 2560, 12800, 64000, etc.
// This also means that for big number of chunks some counts will be zero.
// Program should be ready for this.
#define ITEM_COUNT_DIVISOR (4096/sizeof(double))

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
  if (cN < 1 || cN > cC)
    return 0;
  else if (cN > iC/roundUpDiv(iC, cC) )
    return iC - (cN-1) * roundUpDiv(iC,cC);
  else
    return roundUpDiv(iC,cC);
}

long chunkSizeDivisibleByItemCount(long cC, long iC, long cN)
{
  return ITEM_COUNT_DIVISOR * chunkSize(cC, iC/ITEM_COUNT_DIVISOR, cN);
}

long chunkOffset(long cC, long iC, long cN)
{
  if (cN > cC || cN <1)
    return -1;
  else
    return (cN-1) * roundUpDiv(iC, cC);
}

long chunkOffsetDivisibleByItemCount(long cC, long iC, long cN)
{
  return ITEM_COUNT_DIVISOR * chunkOffset(cC, iC/ITEM_COUNT_DIVISOR, cN);
}

#define BUF_SIZE        16384
double buffer [BUF_SIZE];

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

  assert ((iC % ITEM_COUNT_DIVISOR) == 0);

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

  count = chunkSizeDivisibleByItemCount(cC, iC, cN);
  size = sizeof(d) * count;
  offset = sizeof(d) * chunkOffsetDivisibleByItemCount(cC, iC, cN);

  assert(offset >= 0 && count <= iC);
  assert(size >= 0);

  printf("Writing %ld floats of %ld (all 1.0) after %ld floats to %s.\n", count, iC , chunkOffset(cC, iC, cN), arg[1]);
  fd = open(arg[1], O_CREAT | O_RDWR, 0664);
  assert (fd > 0);
  for (int i = 0; i < BUF_SIZE; i++) {
    buffer[i] = d;
  }
  len = count;
  while (len > 0) {
    current_len = len > BUF_SIZE ? BUF_SIZE : len;
    size_t size_to_write = current_len * sizeof(buffer[0]);
    ssize_t written = pwrite(fd, buffer, size_to_write, offset);
    assert (size_to_write == written);
    offset += size_to_write;
    len -= current_len;
  }
/*
  for (int i = 0; i < count ; i++) {
    assert(pwrite(fd, &d, sizeof (d), offset) == sizeof(d));
    offset += sizeof(d);
  }
*/
  return 0;
}

