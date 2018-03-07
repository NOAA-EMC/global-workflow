#include <stdio.h>
#include <stddef.h>
#include "wgrib2.h"

/* wesley ebisuzaki v1.3
 *
 * write ieee file -- big endian format
 *
 * input float *array		data to be written
 *	 int n			size of array
 *	 int header		1 for f77 style header 0 for none
 *				(header is 4 byte header
 *	 FILE *output		output file
 *
 * v1.2 7/97 buffered, faster
 * v1.3 2/99 fixed (typo) error in wrtieee_header found by
 *     Bob Farquhar
 * v1.4 3/2008 w. ebisuzaki added little-endian output
 * v1.5 11/2013 w. ebisuzaki remove h4[] to cleanup not ititialized warning
 *                use OpenMP
 */

/* BSIZ MUST be a multiple of 4 */

#define BSIZ (4*1024*4)


extern int ieee_little_endian;

int wrtieee(float *array, unsigned int n, int header, FILE *output) {

	unsigned int l;
	unsigned int nbuf;
	unsigned char buff[BSIZ];
	int i, j, lim1, lim2;

	nbuf = 0;
	l = n * 4;
	if (header) {
		buff[nbuf  ] = (l >> 24) & 255;
		buff[nbuf+1] = (l >> 16) & 255;
		buff[nbuf+2] = (l >>  8) & 255;
		buff[nbuf+3] = l         & 255;
		nbuf += 4;
	}

	i = 0;

	while (i < n) {
{
	    if (nbuf >= BSIZ) {
		if (ieee_little_endian) swap_buffer(buff, BSIZ);
		fwrite(buff, 1, BSIZ, output);
		nbuf = 0;
	    }
	    lim1 = (n - i); 
	    lim2 = (BSIZ - nbuf) >> 2; 
	    lim1 = lim1 < lim2 ? lim1 : lim2;

	    i += lim1;
	    nbuf += lim1*4;
}

	    for (j = 0; j < lim1; j++) {
		flt2ieee(array[i - lim1 + j], buff + nbuf - lim1*4 + j*4 );
	    }

	}

/* old code
	for (i = 0; i < n; i++) {
		if (nbuf >= BSIZ) {
		    if (ieee_little_endian) swap_buffer(buff, BSIZ);
		    fwrite(buff, 1, BSIZ, output);
		    nbuf = 0;
		}
		flt2ieee(array[i], buff + nbuf);
		nbuf += 4;
	}
*/
	if (nbuf >= BSIZ) {
	    if (ieee_little_endian) swap_buffer(buff, BSIZ);
	    fwrite(buff, 1, BSIZ, output);
	    nbuf = 0;
	}

	if (header) {
		if (nbuf == BSIZ) {
		    if (ieee_little_endian) swap_buffer(buff, BSIZ);
		    fwrite(buff, 1, BSIZ, output);
		    nbuf = 0;
		}
		buff[nbuf  ] = (l >> 24) & 255;
		buff[nbuf+1] = (l >> 16) & 255;
		buff[nbuf+2] = (l >>  8) & 255;
		buff[nbuf+3] = l         & 255;
		nbuf += 4;
	}
	if (nbuf) {
	    if (ieee_little_endian) swap_buffer(buff, nbuf);
	    fwrite(buff, 1, nbuf, output);
	}

	return 0;
}
