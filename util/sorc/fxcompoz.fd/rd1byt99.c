/*  long long function rd1byt99.c                          5-Jun-1996/dss */
/* ... mods for CRAY4 system upgrade version 9.0.2 of 3-Jun-1996     */
/* ... #include <basedefs.h>  ... removed from CRAY version */
/*   long long function rd1byt99(luni,numbyts,c1array,nbytsred)
     ... to read one or more words from file opened by open99     ... */
/*   ... The long long that is returned is a return code   */
/*   ...     return code = 0   if normal read
                         = 1   if bad value given for luni
                         = -1  if EOF
                         = -2  if ERR            ... */
/* #include    <fortran.h>*/
#include    <stdio.h>
#ifdef UNDERSCORE
long long rd1byt99_(luni,numbyts,c1array,nbytsred)
#else
long long rd1byt99(luni,numbyts,c1array,nbytsred)
#endif
long long        *luni ;
long long        *numbyts;
char        *c1array;      		/* ... char c1array[]; ... */
long long        *nbytsred;
{
/*     COMMON FILE PARAMETERS    */
    extern FILE *ftxx[100] ;


    FILE  *file_pointer ;
    long long         irdretn;
/* ...    char*  ptr;   	... */
    static long long    n_chars = 1;
    long long    n_items;
    long long    iretn;
    long long    i;
    long long   iunit;
    long long   maxuni = 99;

/*  . . .   S T A R T   . . .  */

     iunit = *luni ;
/*      printf("\n rd1byt99 input unit value %ld\n", iunit); */
     if(iunit <= 0 || iunit > maxuni)  
     {
       fprintf(stderr,"\n rd1byt99:failed on given invalid luni\n");
       *nbytsred = 0 ;
       iretn = 1 ;
     }
     else
     {
       file_pointer = ftxx[iunit] ;
       n_items = *numbyts;
/* ...       ptr = (char*)c1array;                        ... */
/*     ... how to test for end-of-file on input? irdretn=0    */
  
       irdretn = fread(c1array,n_chars,n_items,file_pointer);
/* ...       irdretn = fread(ptr,n_chars,n_items,file_pointer); ... */

       if(irdretn != 0)  {   /* ... then, good read ...  */
         iretn = 0;       /* ... normal retn  = 0  */
       }                 /*  ... endif on good read ... */
       else
       {                     /* ... else, bad fread ... */
         if(feof(file_pointer))  
         {                         /* ... then this is EOF ...*/
           iretn = -1;       /* ... End Of File retn = -1    ... */
         }
         else
         {
           perror("fread() failed");
           iretn = -2;
         }
       }
       *nbytsred = irdretn;
     }
/*      printf("\n rd1byt99 return value %ld\n",iretn ); */   
     return (iretn);
}
