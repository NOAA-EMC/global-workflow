#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

void c_run_command(int *retval,char *cmd) {
  *retval=system(cmd);
}

void cwaitfor(int *status, int *minage, int *minsize, int *maxwait,
              int *sleeptime, char *filename) {
  struct stat s;
  time_t now=time(NULL);
  int maxwaitv=*maxwait;
  int size,age;
  fprintf(stderr,"%s: cwaitfor with minage=%d minsize=%d maxwait=%d=%d sleeptime=%d\n",
          filename,*minage,*minsize,*maxwait,maxwaitv,*sleeptime);
  while(maxwaitv<0 || time(NULL)-now<maxwaitv) {
    if(!stat(filename,&s)) {
      size=s.st_size;
      age=time(NULL)-s.st_mtime;
      if(size>=*minsize && age>*minage) {
        fprintf(stderr,"%s: ready.\n",filename);
        *status=0;
        return;
      } else {
        fprintf(stderr,"%s: Not ready yet.  Size=%d, age=%d, min size=%d, min age=%d\n",
               filename,size,age,*minsize,*minage);
      }
    } else {
      fprintf(stderr,"%s: cannot stat: %d\n",filename,errno);
    }
    fprintf(stderr,"%s: sleep %d\n",filename,*sleeptime);
    sleep(*sleeptime);
  }

  *status=1;
  return;
}

void c_run_command_(int*r,char*c) { c_run_command(r,c); }
void c_run_command__(int*r,char*c) { c_run_command(r,c); }
void C_RUN_COMMAND(int*r,char*c) { c_run_command(r,c); }
void C_RUN_COMMAND_(int*r,char*c) { c_run_command(r,c); }
void C_RUN_COMMAND__(int*r,char*c) { c_run_command(r,c); }

void cwaitfor_(int*a,int*b,int*c,int*d,int*e,char*f) {
  cwaitfor(a,b,c,d,e,f);
}

void cwaitfor__(int*a,int*b,int*c,int*d,int*e,char*f) {
  cwaitfor(a,b,c,d,e,f);
}

void CWAITFOR(int*a,int*b,int*c,int*d,int*e,char*f) {
  cwaitfor(a,b,c,d,e,f);
}

void CWAITFOR_(int*a,int*b,int*c,int*d,int*e,char*f) {
  cwaitfor(a,b,c,d,e,f);
}

void CWAITFOR__(int*a,int*b,int*c,int*d,int*e,char*f) {
  cwaitfor(a,b,c,d,e,f);
}
