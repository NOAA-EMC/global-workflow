/* long function open99.c                          		 */
/*  ... to open stream input file				 */
/*             ... for reading a word at a time          */
/*    a FORTRAN callable function    */
/*    The long which is returned is the return code       	 */
/*        return code = 0  for normal successful open
                      = 1  for failure due to out-of-bounds luni
                      =-1  for failure from fopen()        	 */

/* #include  <fortran.h > */
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>

/* ... #include  <basedefs.h>   ... */
/*     COMMON FILE PARAMETERS    */
     FILE *ftxx[100] ;
/*   ==========================================================	*/
#ifdef UNDERSCORE
long long open99_(luni, cfinam, ciomode)
#else
long long open99(luni, cfinam, ciomode)
#endif
long long  *luni;
char *cfinam;
char *ciomode;
{
/*     COMMON FILE PARAMETERS    */
     extern FILE *ftxx[100] ;
     
     FILE  *file_pointer ;
     FILE  *fopen();
     long long   iopnretn;
     long long   iunit;
     long long   maxuni = 99;

     long 	namelen,
		modelen;

     char	*nstring,
		*mstring;


     namelen = strlen(cfinam);
     nstring = malloc(namelen+1);
     strncpy(nstring, cfinam, namelen);
     nstring[namelen] = '\0';

     modelen = strlen(ciomode);
     mstring = malloc(modelen+1);
     strncpy(mstring, ciomode, modelen);
     mstring[modelen] = '\0';


     iunit = *luni ;
/*     printf("\n open99 input unit value %ld\n", iunit); */
     if(iunit <= 0 || iunit > maxuni)  
     {
       fprintf(stderr,"\n open99:failed on given invalid luni\n");
       iopnretn = 1;
     }
     else
     {

       file_pointer = fopen(nstring,mstring);
/* ...  file_pointer = fopen("/usr/home/shimomur/afsfcplt.dat","r");  */
       ftxx[iunit] = file_pointer ;
       if (file_pointer == NULL)
       {   
perror("fopen() failed");
         fprintf(stderr,"\n open99:failed to open file %ld\n", iunit);
         iopnretn = -1; 
       }
       else
       {
         printf("\n open99:successfully opened file %ld\n", iunit);
         iopnretn = 0; 
       }
     }
/*   printf("\n open return value %ld\n", iopnretn); */
  return(iopnretn);
}
