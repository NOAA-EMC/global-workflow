#include <stdio.h>
#include <ctype.h>
#include "wgrib2.h"

/*
 * rd_inventory:  public domain 2005 w. ebisuzaki
 *  moved from wgrib2.c to rd_inventory.c 10/2014
 *
 * update: 10/2014 reads from input rather than stdin
 *
 * reads inventory and pulls out the address of the record
 * and submessage number
 */

int rd_inventory(int *rec_num, int *submsg, long int *pos, FILE *input) {

    long int tmp;
    int c, i;

    /* format: [digits].[submessage]:[pos]: or digits]:[pos]: */

    i = 0;

    c=getc(input);
    while (c == ' ') c = getc(input);
    if (c == EOF) return 1;
    if (!isdigit((unsigned char) c)) {
	fatal_error_i("bad inventory on line: %d",*rec_num);
    }

    /* record number */
    while (isdigit((unsigned char) c) ) {
	i = 10*i + c - '0';
        c=getc(input);
    }

    *rec_num = i;

    if (c == '.') {
        i = 0;
	while (isdigit((unsigned char) (c = getc(input)) ) ) {
	    i = 10*i + c - '0';
	}
	*submsg = i;
    }
    else {
        *submsg = 1;
    }

    if (c != ':') fatal_error_i("bad inventory on line: %d",*rec_num);

    tmp = 0;
    while (isdigit((unsigned char) (c = getc(input))) ) {
        tmp = 10*tmp + c - '0';
    }

    /* if (c != ':') fatal_error_i("bad inventory on line: %d",*rec_num); */

    /* read the rest of the line */
    while (c != EOF && c != '\n') {
	c = getc(input);
    }
    *pos = tmp;
    return 0;
}
