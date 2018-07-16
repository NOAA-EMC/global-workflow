#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

void printrusage() {
  struct rusage usage;
  if(!getrusage(RUSAGE_SELF, &usage)) {
    fprintf(stderr,"getrusage: Usage only for this process and its threads:\n");
    fprintf(stderr,"getrusage: user time used (seconds):     %f\n",usage.ru_utime.tv_sec+usage.ru_utime.tv_usec/1e6);
    fprintf(stderr,"getrusage: system time used (seconds):   %f\n",usage.ru_stime.tv_sec+usage.ru_stime.tv_usec/1e6);
    fprintf(stderr,"getrusage: max resident set size:        %10ld\n",usage.ru_maxrss);
    fprintf(stderr,"getrusage: max shared memory size:       %10ld\n",usage.ru_ixrss);
    fprintf(stderr,"getrusage: max unshared data size:       %10ld\n",usage.ru_idrss);
    fprintf(stderr,"getrusage: max unshared stack size:      %10ld\n",usage.ru_isrss);
    fprintf(stderr,"getrusage: minor page faults:            %10ld\n",usage.ru_minflt);
    fprintf(stderr,"getrusage: major page faults:            %10ld\n",usage.ru_majflt);
    fprintf(stderr,"getrusage: swaps:                        %10ld\n",usage.ru_nswap);
    fprintf(stderr,"getrusage: block input ops:              %10ld\n",usage.ru_inblock);
    fprintf(stderr,"getrusage: block output ops:             %10ld\n",usage.ru_oublock);
    fprintf(stderr,"getrusage: messages sent:                %10ld\n",usage.ru_msgsnd);
    fprintf(stderr,"getrusage: messages received:            %10ld\n",usage.ru_msgrcv);
    fprintf(stderr,"getrusage: signals received:             %10ld\n",usage.ru_nsignals);
    fprintf(stderr,"getrusage: voluntary context switches:   %10ld\n",usage.ru_nvcsw);
    fprintf(stderr,"getrusage: involuntary context switches: %10ld\n",usage.ru_nivcsw);
  } else
    fprintf(stderr,"getrusage failed: %s\n",strerror(errno));
}

void printrusage_()  { printrusage(); }
void printrusage__() { printrusage(); }
void PRINTRUSAGE()   { printrusage(); }
void PRINTRUSAGE_()  { printrusage(); }
void PRINTRUSAGE__() { printrusage(); }

