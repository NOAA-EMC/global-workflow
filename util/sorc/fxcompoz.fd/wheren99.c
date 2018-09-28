#include <stdio.h>
/* ... #include <basedefs.h> ... removed fr CRAY version      */
/*   long long function wheren99(luni,byte_offset)
     ... to locate where_am_I in the file opened by open99... */
/*   ... The long long that is returned is a return code   */
/*   ...     return code = 0   if normal return
                         = 1   if bad value given for luni

                                                           ... */
#ifdef UNDERSCORE
long long wheren99_(luni,byte_offset)
#else
long long wheren99(luni,byte_offset)
#endif
long long        *luni ;
long long        *byte_offset;       /* for resulting ptr in bytes  */
{
/*     COMMON FILE PARAMETERS    */
    extern FILE *ftxx[100] ;


    FILE  *file_pointer ;
    long long    ibyt_ptr;
    long long    iretn;
    long long    iunit;
    long long    maxuni = 99;

/*  . . .   S T A R T   . . .  */

     ibyt_ptr = 0 ;
     iunit = *luni ;
/*     printf("\n wheren99 input unit value %ld\n", iunit); */
     if(iunit <= 0 || iunit > maxuni)  
     {
       fprintf(stderr,"\n wheren99:failed on given invalid luni\n");
       iretn = 1 ;
     }
     else
     {
       file_pointer = ftxx[iunit] ;

/*     ... how to test for bad result of ftell?   I won't test    */  
       ibyt_ptr = ftell(file_pointer);
       iretn = 0;       /* ... normal retn  = 0  */
     }
     *byte_offset = ibyt_ptr ;
      printf("\n wheren99 return value %ld\n", iretn);
     return (iretn);
}
