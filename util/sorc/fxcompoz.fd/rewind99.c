#include <stdio.h>
/* ... #include <basedefs.h>  ... */
/*   long long function rewind99(luni)
     ... to rewind the file opened by open99     ... */
/*   ... The long long that is returned is a return code   */
/*   ...     return code = 0   if normal return
                         = 1   if bad value given for luni
                                                          ... */
#ifdef UNDERSCORE
long long rewind99_(luni)
#else
long long rewind99(luni)
#endif
long long        *luni ;
{
/*     COMMON FILE PARAMETERS    */
    extern FILE *ftxx[100] ;


    FILE  *file_pointer ;
    long long    irewrtn;
    long long    iretn;
    long long    iunit;
    long long    maxuni = 99;

/*  . . .   S T A R T   . . .  */

     iunit = *luni ;
/*     printf("\n rewinf99 input unit value %ld\n", iunit); */
     if(iunit <= 0 || iunit > maxuni)
       
     {
       fprintf(stderr,"\n rewind99:failed on given invalid luni\n");
       iretn = 1 ;
     }
     else
     {
       file_pointer = ftxx[iunit] ;
/*     ... how to test for good rewind?  I will not test    */  
       irewrtn = fseek(file_pointer,0L,0);
       iretn = 0;       /* ... normal retn  = 0  */
     }
/*     printf("\n rewind99 return value %ld\n", iretn); */
     return (iretn);
}
