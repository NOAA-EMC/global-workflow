/*module documentation block                                         */
/*               .      .    .                                       */
/* module:  obsmod                                                   */
/* prgmmr:  middlecoff       org: gsd/esrl         date: 2006-04-27  */
/*                                                                   */
/* abstract: This module contains contains routines used to open,    */
/*           read from, and close data files in a manner similar     */
/*           to fortran direct access (DA) reads                     */
/*                                                                   */
/*    The Fortran standard does not                                  */
/*      - specify what the status return code should be for a DA     */
/*        when read past the EOF                                     */
/*      - provide a way to detect end-of-file for a DA file          */
/*      - apply the concept end-of-file to direct-access files       */
/*                                                                   */
/*    Consequently, the standard does not specify what the contents  */
/*    of the buffer will be for locations past the EOF.              */
/*                                                                   */
/*    The routines in this file provide a more portable way to       */
/*    read a file in a "direct access" like manner.                  */
/*                                                                   */
/* program history log:                                              */
/*   2006-04-27 middlecoff                                           */
/*                                                                   */
/*                                                                   */
/* Subroutines Included:                                             */
/*  sub openfileread - ope                                           */
/*  sub closfile                                                     */
/*  sub getbytes     -                                               */
/*                                                                   */
/* Variable Definitions:                                             */
/*                                                                   */
/*                                                                   */
/*                                                                   */
/*end documentation block                                            */


#include <stdlib.h>
#include <stdio.h>

#define MAXLUN 1000
static int lunTableInit=0;
static FILE *lunTable[MAXLUN];
static int lunUsed[MAXLUN];

void initLunTable ()
  {
  int i;
  if (lunTableInit == 0)
    {
    for (i=0; i < MAXLUN; i++)
      lunUsed[i] = 0;
    lunTableInit = 1;
    }
  }

void
#if defined(funder)
  openfileread_
#elif defined(f2under)
  openfileread__
#elif defined(fcaps)
  OPENFILEREAD
#else
  openfileread
#endif
            (FortranInt *lun, FortranInt *istat, FortranByte *fname)
            /* Assume the name is null terminated in the Fortran code */
 {
 int cLun;
 initLunTable();
 cLun = (int) *lun;
 /* Check lun range */
 if ((cLun < 0) || (cLun >= MAXLUN))
   {
   *istat = -1;
   return;
   }

 /* Close if open */
 if (lunUsed[cLun] != 0)
   fclose (lunTable[cLun]);

 /* Open the file */
 lunTable[cLun] = fopen (fname, "r");
 if (lunTable[cLun] == NULL)
   {
   *istat = -1;
   return;
   }
 lunUsed[cLun] = 1;
 *istat = 0;
 return;
 }


/* This routine emulates IBM Fortran direct access read */
void
#if defined(funder)
  getbytes_
#elif defined(f2under)
  getbytes__
#elif defined(fcaps)
  GETBYTES
#else
  getbytes
#endif
           (FortranInt *lun, void *buff, FortranLlong *recno, FortranLlong *recl, FortranInt *istat)
 {
 int cLun;
 size_t cRecl;
 size_t xfer;
 char *cbuff;
 int i;
 long offset;
 initLunTable();
 cRecl = (size_t) *recl;
 cLun = (int) *lun;
 cbuff = (char *) buff;
 /* Check lun range */
 if ((cLun < 0) || (cLun >= MAXLUN))
   {
   *istat = -1;
   return;
   }

 /* Is it open? */
 if (lunUsed[cLun] == 0)
   {
   *istat = -2;
   return;
   }

 /* move to the right record */
 offset = ((long) *recno-1)*cRecl;
 if(fseek(lunTable[cLun], offset, 0) !=0) 
   {*istat = -3;return; }

 /* Get the data */
 xfer = fread (buff, (size_t) 1, cRecl, lunTable[cLun]);
 if (xfer < cRecl)
   {
   *istat = 1;
   /* Short read, pad with zeros */
   for (i=xfer; i < cRecl; i++)
     cbuff[i] = '\0';
   }
 else
   *istat = 0;

 return;
 }


void
#if defined(funder)
   closefile_
#elif defined(f2under)
   closefile__
#elif defined(fcaps)
   CLOSEFILE
#else
   closefile
#endif
             (FortranInt *lun, FortranInt *istat)
 {
 int cLun;

 initLunTable();

 cLun = (int) *lun;
 /* Check lun range */
 if ((cLun < 0) || (cLun >= MAXLUN))
   {
   *istat = -1;
   return;
   }

 /* Is it open? */
 if (lunUsed[cLun] == 0)
   {
   *istat = -1;
   return;
   }

 /* Close the file */
 fclose (lunTable[cLun]);
 lunUsed[cLun] = 0;

 *istat = 0;
 return;
 
}
