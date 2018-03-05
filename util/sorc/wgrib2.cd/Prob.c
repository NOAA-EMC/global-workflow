#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Prob.c
 *
 *  Some routines for probablilty forecasts
 *
 * 11/2008: Public Domain: Wesley Ebisuzaki
 * 12/2008: fixed return code
 * 1/2008: ncep bug fix
 *
 */

#define LOWER_LIMIT scaled2flt(INT1(p[1]), int4(p+2) )
#define UPPER_LIMIT scaled2flt(INT1(p[6]), int4(p+7) )

extern int fix_ncep_2_flag;


/*
 * HEADER:100:fix_ncep_2:setup:0:ncep bug fix 2, probability observation < -ve number
 */

int f_fix_ncep_2(ARG0) {
    fix_ncep_2_flag = 1;
    return 0;
}

int fix_ncep_2(unsigned char **sec) {
    unsigned char *p;
    int i;

    p = code_table_4_9_location(sec);
    if (p == NULL) return 0;

    if ((p[2] & 0x0c) == 0x0c) {	// fix bug
	i = int4_comp(p+2);
	int_char(i, p+2);
    }
    if ((p[7] & 0x0c) == 0x0c) {	// fix bug
	i = int4_comp(p+7);
	int_char(i, p+7);
    }
    return 0;
}

/*
 * HEADER:100:prob:inv:0:probability information
 */
int f_prob(ARG0) {
    unsigned char *p;

    if (mode == -1) {
	return 0;
    }
    else if (mode >= 0) {
	p = code_table_4_9_location(sec);
	if (p == NULL) return 0;
	sprintf(inv_out,"probtype=%d ",*p);
	switch (*p) {

	case 0: sprintf(inv_out,"prob <%g", LOWER_LIMIT); break;
	case 1: sprintf(inv_out,"prob >%g", UPPER_LIMIT); break;
	
	case 2: if (LOWER_LIMIT == UPPER_LIMIT) {
		sprintf(inv_out,"prob =%g", LOWER_LIMIT); break;
		}
		sprintf(inv_out,"prob >=%g <%g", LOWER_LIMIT, UPPER_LIMIT); break;
	case 3: sprintf(inv_out,"prob >%g", LOWER_LIMIT);
		break;
	case 4: sprintf(inv_out,"prob <%g", UPPER_LIMIT);
		break;
	}
	if (mode == 99) {
	    inv_out += strlen(inv_out);
            sprintf(inv_out, " LOWER LIMIT scale=%d, val= 0x%.2x 0x%.2x 0x%.2x 0x%.2x",
		INT1(p[1]), p[2], p[3], p[4], p[5]);
	}
    }
    return 0;
}

