#include <stdio.h> 
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>

// presently there is no alignment for DIRECT_IO
// However, direct IO is not available at HPCS

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
  long r;

  if (cN < 1 || cN > cC)
    r =0;
  else if (cN > iC/roundUpDiv(iC, cC) )
    r = iC - (cN-1) * roundUpDiv(iC,cC);
  else
    r = roundUpDiv(iC,cC);
  //    printf("cC: %ld, iC: %ld, cN: %ld, result: %ld\n", cC, iC, cN, r);
  return r;
}

long chunkOffset(long cC, long iC, long cN)
{
  long r;
  if (cN > cC || cN <1)
    r = -1;
  else
    r = (cN-1) * roundUpDiv(iC, cC);
  //  printf("cC: %d, iC: %d, cN: %d, result: %d\n", cC, iC, cN, r);
  return r;
}

int main(int argc, char **arg)
{
  int fd;
  off_t size, offset;
  long iC, cN, cC, count, i;
  char *nodeid;

  double d = 1.0;

  if (argc != 5 && argc != 6) {
    printf("usage: %s path itemcount processcount chunkno [nodeid] -- write floats to file \"path\" \n", arg[0]);
    exit(1);
  }

  iC = atol(arg[2]);  // item count  = total number of floats
  cC = atol(arg[3]);  // chunk count = number of compute procs
  cN = atol(arg[4]);  // chunk number 
  if  (arg[5])
    nodeid = arg[5];
  else 
    nodeid = "<no nodeid>";
     
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

  printf("[%s] Chunk %ld/%ld: %ld/%ld floats at %ld floats.  bytes: %lld at %lld. file: \"%s\".\n", 
	 nodeid, cN, cC, count, iC, chunkOffset(cC, iC, cN), size, offset, arg[1]);
  fd = open(arg[1], O_CREAT | O_RDWR, 0660);
  assert (fd > 0);
  for (i=0; i<count; i++) {
    assert( sizeof(d) == pwrite(fd, &d, sizeof(d), offset));
    offset += sizeof(d);
  }
  
  return 0;
}

