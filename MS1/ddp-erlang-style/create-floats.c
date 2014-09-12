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
#define ITEM_COUNT_DIVISOR      (4096/sizeof(double))

long roundUpDiv(long num, long den)
{
  assert (den != 0);
  return (num + den -1) / den;
}

/* we should be careful here because for large amounts of data (terabytes, iC) and
 * massive parallelism (tens and hundreds of thousands of chunks/threads, cC) we can get integer overflow.
 * we have to compute here an offset that is (iC*cN+cC-1)/cC.
 * (iC*cN+cC-1)/cC ===
 * (iC*cN)/cC + (((iC*cN)%cC)+cC-1)/cC
 * assume iC=iCDivcC*cC + iCModcC, where iCDivcC = iC/cC, iCModcC = iC % cC
 * then:
 * (iC*cN)/cC ===
 * ((iCDivcC*cC+iCModcC)*cN)/cC ===
 * iCDivcC*cN + iCModcC*cN/cC
 * we don't get integer overflow here because cN<= cC and first summand is less than iC (fits into long);
 * second summand also fits into word as iCModcC is less than cC.
 * (((iC*cN)%cC)+cC-1)/cC ===
 * ((((iCDivcC*cC+iCModcC)*cN)%cC)+cC-1)/cC ===
 * (((iCDivcC*cC*cN%cC+(iCModcC*cN)%cC)+cC-1)/cC ===
 * (((iCModcC*cN)%cC)+cC-1)/cC
 *
 * warning: this code may overflow if/when cC*cC overflows. So it is good for 64-bit longs for
 * cC up to about 2.0e9 (2^31-1)
 */
long chunkOffsetCompute(long cC, long iC, long cN)
{
//  if (cN > cC || cN <1)
//    return -1;
//  if (cN == cC)
//    return iC;
  long iCDivcC = iC / cC;
  long iCModcC = iC % cC;
  long sum1 = iCDivcC*cN + (iCModcC*cN)/cC;
  long sum2 = ((iCModcC*cN) % cC + cC - 1) / cC;
  return sum1+sum2;
//  return (cN-1) * roundUpDiv(iC, cC);
}

long chunkOffset(long cC, long iC, long cN)
{
  return chunkOffsetCompute(cC, iC, cN-1);
}

// cC = number of chunks, typically number of compute processes
// cN = chunk number
// iC = item count, how many objects
long chunkSize(long cC, long iC, long cN)
{
#if 1
  long nextOfs = chunkOffset(cC, iC, cN+1);
  long thisOfs = chunkOffset(cC, iC, cN);
  if (nextOfs < 0)
    return 0;
  assert(thisOfs >= 0);
printf("nextOfs %ld, thisOfs %ld\n",nextOfs, thisOfs);
  return nextOfs - thisOfs;
#else
  if (cN < 1 || cN > cC)
    return 0;
  else if (cN > iC/roundUpDiv(iC, cC) )
    return iC - (cN-1) * roundUpDiv(iC,cC);
  else
    return roundUpDiv(iC,cC);
#endif
}

long chunkSizeDivisibleByItemCount(long cC, long iC, long cN)
{
  return ITEM_COUNT_DIVISOR * chunkSize(cC, iC/ITEM_COUNT_DIVISOR, cN);
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

  printf("Chunk count %ld, chunk index %ld, item count %ld.\n", cC, cN, iC);
  assert ((iC % ITEM_COUNT_DIVISOR) == 0);

  for (int i = 0; i < BUF_SIZE; i++) {
    buffer[i] = d;
  }

  if (cN == 0) {
    fd = open(arg[1], O_CREAT | O_RDWR, 0664);
    assert (fd > 0);
    ftruncate(fd, iC * sizeof(d));
    sync(fd);
    close(fd);
    return 0;
  }

  printf("Attempting to write %ld floats of %ld (all 1.0) after %ld floats to %s.\n", chunkSizeDivisibleByItemCount(cC, iC, cN), iC , chunkOffsetDivisibleByItemCount(cC, iC, cN), arg[1]);
  count = chunkSizeDivisibleByItemCount(cC, iC, cN);
  size = sizeof(d) * count;
  offset = sizeof(d) * chunkOffsetDivisibleByItemCount(cC, iC, cN);

  assert(offset >= 0 && count <= iC);
  assert(size >= 0);

  fd = open(arg[1], O_CREAT | O_RDWR, 0664);
  assert (fd > 0);
  len = count;
  while (len > 0) {
    current_len = len > BUF_SIZE ? BUF_SIZE : len;
    size_t size_to_write = current_len * sizeof(buffer[0]);
    ssize_t written = pwrite(fd, buffer, size_to_write, offset);
    assert (size_to_write == written);
    offset += size_to_write;
    len -= current_len;
  }
  sync(fd);
  close(fd);
  return 0;
}

