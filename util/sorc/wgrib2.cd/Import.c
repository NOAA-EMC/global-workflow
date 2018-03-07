#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Import.c
 *
 *  Some routines to read data into data buffer
 *
 * 10/2008: Public Domain: Wesley Ebisuzaki
  *
 */

extern int decode;
extern int nx, ny, use_scale;
extern int ieee_little_endian, header;

/*
 * HEADER:100:import_text:misc:1:read text file (X) for data
 */
int f_import_text(ARG1) {
    int ix, iy;
    unsigned int i;
    float t;

    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1, "r")) == NULL) 
            fatal_error("import_text: Could not open %s", arg1);
        decode = 1; 
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
	if (header) {
	    if (fscanf((FILE *) *local, "%d %d", &ix, &iy) != 2) {
                fatal_error("import_text: Could not read nx, ny in file %s", arg1);
            }
	    if (nx != ix) {
                fatal_error_i("import_text: nx=%d is wrong",ix);
	    }
	    if (ny != iy) {
                fatal_error_i("import_text: ny=%d is wrong",iy);
	    }
	}
	for (i = 0; i < ndata; i++) {
	    if (fscanf((FILE *) *local, "%f", &t) != 1)
                fatal_error_i("import_text: Could not read data. location=%u",i+1);
	    data[i] = t;
        }
        use_scale = 0;
    }
    return 0;
}

/*
 * HEADER:100:import_ieee:misc:1:read ieee file (X) for data
 */
int f_import_ieee(ARG1) {

    int i;

    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1, "rb")) == NULL)
            fatal_error("import_ieee: Could not open %s", arg1);
        decode = 1;
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
	i = rdieee_file(data, ndata, header, (FILE *) *local);
        if (i) fatal_error_i("import_ieee, error %d", i);
        use_scale = 0;
    }
    return 0;
}

/*
 * HEADER:100:import_bin:misc:1:read binary file (X) for data
 */

int f_import_bin(ARG1) {
    unsigned int i, j;
    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1,"rb")) == NULL) {
            fatal_error("Could not open %s", arg1);
        }
        decode = 1;
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
        if (header) {
            if (fread((void *) &i, sizeof(int), 1, (FILE *) *local) != 1)
                fatal_error("set_bin: read error header","");
            if (i != sizeof(float) * ndata)
                fatal_error_ii("import_bin: header record size wrong, %u expected %u", i, sizeof(float) * ndata);
        }
        j = fread(data, sizeof(float), ndata, (FILE *) *local);
        if (j != ndata) fatal_error_ii("import_bin: read error read %u words, expected %u", j, ndata);
        if (header) {
            if (fread((void *) &i, sizeof(int), 1, (FILE *) *local) != 1)
                fatal_error("set_bin: read error trailer","");
            if (i != sizeof(float) * ndata)
                fatal_error_i("import_bin: trailer record size wrong, %u", i);
        }
        use_scale = 0;                  // new field, unknown scaling
    }
    return 0;
}
