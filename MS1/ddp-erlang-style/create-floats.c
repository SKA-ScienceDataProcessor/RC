#include <stdio.h> 
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>



int roundUpDiv(int num, int den)
{
  assert (den != 0);
  return (num + den -1) / den;
}

// cC = number of chunks, typically number of compute processes
// cN = chunk number
// iC = item count, how many objects
int chunkSize(int cC, int iC, int cN)
{
  if (cN < 1 || cN > cC)
    return 0;
  else if (cN > iC/roundUpDiv(iC, cC) )
    return iC - (cN-1) * roundUpDiv(iC,cC);
  else
    return roundUpDiv(iC,cC);
}

int chunkOffset(int cC, int iC, int cN)
{
  if (cN > cC || cN <1)
    return -1;
  else
    return (cN-1) * roundUpDiv(iC, cC);
}

int main(int argc, char **arg)
{
  int fd;
  off_t size, offset;
  int iC, cN, cC, count;

  double d = 1.0;

  if (argc != 5) {
    printf("usage: %s path itemcount processcount chunkno -- write floats to file \"path\" \n", arg[0]);
    exit(1);
  }

  iC = atoi(arg[2]);  // item count  = number of floats
  cC = atoi(arg[3]);  // chunk count = number of compute procs
  cN = atoi(arg[4]);  // chunk number

  if (cN == 0) {
    fd = open(arg[1], O_CREAT | O_RDWR, 0664);
    assert (fd > 0);
    ftruncate(fd, iC * sizeof(d));
    return 0;
  }

  count = chunkSize(cC, iC, cN);
  size = sizeof(d) * count;
  offset = sizeof(d) * chunkOffset(cC, iC, cN);

  assert(offset >= 0 && count <= iC);
  assert(size >= 0);

  assert(printf("Writing %d floats of %d (all 1.0) after %d floats to %s.\n", count, iC , chunkOffset(cC, iC, cN), arg[1]) > 0);
  fd = open(arg[1], O_CREAT | O_RDWR, 0664);
  assert (fd > 0);
  for (int i = 0; i < count ; i++) {
    assert(pwrite(fd, &d, sizeof (d), offset) == sizeof(d));
    offset += sizeof(d);
  }
  return 0;
}

