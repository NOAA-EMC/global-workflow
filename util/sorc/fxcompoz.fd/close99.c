/* long long function close99.c                          		 */
/*   ... I*4 function close99(luni)   ...  */
/*   ... to close the file which was opened by open99    */
/*     ... a FORTRAN callable function      			 */
/*   ... The long long returned by the function is a return code
            return code = 0     for normal return
                        = 1     for failure due to bad value for luni */
#include  <stdio.h>
/* ... #include  <basedefs.h> ... */
/*     COMMON FILE PARAMETERS    */
/* ...     FILE *ftxx[100] ;  ...*/
/*   ==========================================================	*/
#ifdef UNDERSCORE
long long close99_(luni)
#else
long long close99(luni)
#endif
long long  *luni;
{
/*     COMMON FILE PARAMETERS    */
     extern FILE *ftxx[100] ;
     
     FILE  *file_pointer ;
     long long   icloretn;
     long long   iunit;
     long long   maxuni = 99;

     iunit = *luni ;
/*         printf("\n close99 input unit value %ld\n", iunit); */
     if(iunit <= 0 || iunit > maxuni)  
     {
       fprintf(stderr,"\n close99:failed on given invalid luni\n");
       icloretn = 1;
     }
     else
     {
       file_pointer = ftxx[iunit] ;
 
       fclose(file_pointer);
       printf("\n close99:closed file %ld\n",iunit);
       icloretn = 0; 
     }
/*     printf("\n close99 return value %ld\n", icloretn); */
     return(icloretn);
}
