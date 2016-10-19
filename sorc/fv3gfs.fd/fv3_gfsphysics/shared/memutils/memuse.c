 /**********************************************************************
 *                   GNU General Public License                        *
 * This file is a part of fvGFS.                                       *
 *                                                                     *
 * fvGFS is free software; you can redistribute it and/or modify it    *
 * and are expected to follow the terms of the GNU General Public      *
 * License as published by the Free Software Foundation; either        *
 * version 2 of the License, or (at your option) any later version.    *
 *                                                                     *
 * fvGFS is distributed in the hope that it will be useful, but        *
 * WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
 * General Public License for more details.                            *
 *                                                                     *
 * For the full text of the GNU General Public License,                *
 * write to: Free Software Foundation, Inc.,                           *
 *           675 Mass Ave, Cambridge, MA 02139, USA.                   *
 * or see:   http://www.gnu.org/licenses/gpl.html                      *
 **********************************************************************/
#if defined(__sgi) || defined(__aix) || defined(__SX)
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>
#include <sys/resource.h>

#define RUSAGE_SELF      0         /* calling process */
#define RUSAGE_CHILDREN  -1        /* terminated child processes */

#ifdef __aix
int memuse(void)
#else
int memuse_(void)
#endif
{
 struct rusage my_rusage;
 int iret;

 my_rusage.ru_maxrss = 0;
 iret = getrusage(RUSAGE_SELF,&my_rusage);
 iret = (int) my_rusage.ru_maxrss;
 return iret;
 /* printf("Max memuse in KB is %d \n",iret); */
}
#endif
