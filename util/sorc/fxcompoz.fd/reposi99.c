#include <stdio.h>
/* ... #include <basedefs.h>  ... */
/*   long long function reposi99(luni,byte_offset,from_indx)
     ... to reposition the pointer within the file opened by open99... */
/*   ... The long long that is returned is a return code   */
/*   ...     return code = 0   if normal return
                         = 1   if bad value given for luni
                         = 2   if bad value given for from_indx

     ... from_indx is to tell me the base position from which to
             to apply the byte_offset;
     ... acceptable values for from_indx are 0,1, or 2:
             from_indx=0   from beginning of file
                      =1   from current position in file
                      =2   from end of file                      
                                                           ... */
#ifdef UNDERSCORE
long long reposi99_(luni,byte_offset,from_indx)
#else
long long reposi99(luni,byte_offset,from_indx)
#endif
long long        *luni ;
long long        *byte_offset;
long long        *from_indx;
{
/*     COMMON FILE PARAMETERS    */
    extern FILE *ftxx[100] ;


    FILE  *file_pointer ;
    long long    isekrtn;
    long long    offset;
    long long    indicator;
    long long    iretn;
    long long    iunit;
    long long    maxuni = 99;

/*  . . .   S T A R T   . . .  */

     offset = *byte_offset;
     indicator = *from_indx;
     iunit = *luni ;
/*      printf("\n reposi99 input unit value %ld\n", iunit);  */    
     if(iunit <= 0 || iunit > maxuni)  
     {
       fprintf(stderr,"\n reposi99:failed on given invalid luni\n");
       iretn = 1 ;
     }
     else if(indicator < 0 || indicator > 2)
     {
       fprintf(stderr,"\n reposi99:failed on");
       fprintf(stderr," given invalid from_indx=%lld.\n",indicator);
       iretn = 2 ;
     }
     else
     {
       file_pointer = ftxx[iunit] ;

/*     ... how to test for bad result of seek? I won't test    */  
       isekrtn = fseek(file_pointer,offset,indicator);
       iretn = 0;       /* ... normal retn  = 0  */
     }
/*     printf("\n reposi99 return value %ld\n", iretn); */
     return (iretn);
}
