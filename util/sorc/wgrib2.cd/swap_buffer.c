#include <stdio.h>
#include "wgrib2.h"

/*
 * does a byte swap of 4-byte integers/ieee
 *
 * n should be a multiple of 4
 *
 * 3/2008 Public Domain Wesley Ebisuzaki
 *
 */

int swap_buffer(unsigned char *buffer, int n) {
    unsigned char *p;
    unsigned char i, j;

    p = buffer;
    while (n > 0) {
	i = p[0];
	j = p[1];
        p[0] = p[3];
        p[1] = p[2];
        p[3] = i;
        p[2] = j;
	n -= 4;
	p += 4;
    }
    return 0;
}


