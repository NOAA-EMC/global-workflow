#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>


int main(int argc, char **argv) {
     
     int handle,rc;

     if(argc < 2) {
          printf("Usage : %s <file to fsync> \n",argv[0]);
          exit(1);
     }

    handle = open(argv[1], O_WRONLY, 0666);
    if (handle == -1)
    {
      fprintf(stderr, "Could not open file '%s' to fsync it, errno=%d\n",
              argv[1], errno);
      exit(1);
    }
    rc = fsync(handle);
    if (rc == -1)
    {
      fprintf(stderr, "Error %d from fsync\n", errno);
      exit(1);
    }
    rc = close(handle);
    if (rc == -1)
    {
      fprintf(stderr, "Could not close file '%s' after fsync, errno=%d\n",
              argv[1], errno);
      exit(1);
    }
}
