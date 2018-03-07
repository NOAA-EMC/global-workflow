#include <stdio.h>
#include <stddef.h>
#include "wgrib2.h"

/*
 * rdieee_file: reads a big/little endian file with optional header
 *
 * 10/2008 Public domain Wesley Ebisuzaki
 */

#define BSIZ (1024*4)

extern int ieee_little_endian;

int rdieee_file(float *array, int n, int header, FILE *input) {

    int i, j;
    unsigned int l;
    unsigned char buff[BSIZ];
    unsigned char h4[4],t4[4];
    float *p;

    n = n*4;

    if (header) {
	if (fread(h4,1,4,input) != 4) fatal_error("rdieee: header read","");
	if (ieee_little_endian) 
	    l = (h4[3] << 24) | (h4[2] << 16) | (h4[1] << 8) | h4[0];
	else
	    l = (h4[0] << 24) | (h4[1] << 16) | (h4[2] << 8) | h4[3];
	
	if (l != n) fatal_error_ii("rdieee: bad header=%u expecting %d",l,n);
    }

    p = array;
    while (n > 0) {
	j =  n < BSIZ ? n : BSIZ;
	if (fread(buff,1,j,input) != j) fatal_error("rdieee: data read","");
	if (ieee_little_endian) swap_buffer(buff, j);
	for (i = 0; i < j; i += 4) {
	    *p++ = ieee2flt(buff + i);
	}
	n = n - j;
    }

    if (header) {
	if (fread(t4,1,4,input) != 4) fatal_error("rdieee: trailer read","");
	if (h4[0] != t4[0] || h4[1] != t4[1] || h4[2] != t4[2] || h4[3] != t4[3])
	   fatal_error("rdieee: bad trailer","");
    }

    return 0;
}
