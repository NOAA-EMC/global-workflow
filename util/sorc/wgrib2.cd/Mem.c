#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"
/*
 * Grid2mem.c
 * 2014: Public Domain, Wesley Ebisuzaki
 * -grid2mem &data &n nmax
 *   writes grid to a memory address .. used for callable wgrib2
 *
 * will have problems if size_t is greater the unsigned long
 */

#ifdef CALLABLE_WGRIB2

extern int decode;
extern char *last_inv_out;

/*
 * HEADER:100:grid2mem:output:3:write grid to memory X=address of data, Y=address of #points Z=max # points
 */
int f_grid2mem(ARG3) {

    struct local_struct {
        float *grid;
        long int *n;
        long int nmax;
    };
    struct local_struct *save;
    size_t nn;

    if (mode == -1) {
	decode = 1;
	if (sizeof(size_t) > sizeof(unsigned long)) 
		fatal_error("grid2mem: sizeof(size_t) > sizeof(unsigned long)","");
        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct));

	save->grid = (float *) strtoul(arg1,NULL,10);
	save->n = (long int *) strtoul(arg2,NULL,10);
	*(save->n) = 0;
	save->nmax = strtol(arg3,NULL,10);
    }
    else if (mode == -2) {
        free(*local);
    }
    else if (mode >= 0) {
        save = *local;
	if (ndata > save->nmax) {
	    *(save->n) = -save->nmax;
	    nn = (size_t) save->nmax;
	}
	else {
	    nn = *(save->n) = ndata;
	}
	nn *= sizeof(float);
	memcpy((void *) save->grid, (const void *) &(data[0]), nn);
    }
    return 0;
}

/*
 * HEADER:100:last2mem:inv_output:2:write last inv item to memory X=address of data Y=max # characters
 */
int f_last2mem(ARG2) {
    struct local_struct {
        char *string;
        long int nmax;
    };
    struct local_struct *save;

    if (mode == -1) {
        if (sizeof(size_t) > sizeof(unsigned long))
                fatal_error("last2mem: sizeof(size_t) > sizeof(unsigned long)","");
        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct));

        save->string = (char *) strtoul(arg1,NULL,10);
        save->nmax = strtol(arg2,NULL,10);
    }
    else if (mode == -2) {
	free(*local);
    }
    else if (mode >= 0) {
	save = *local;
	strncpy(save->string, last_inv_out, save->nmax);
	save->string[save->nmax-1] = '\0';
    }
    return 0;
}

/*
 * HEADER:100:import_mem:misc:2:read memory for data, X=address of data, Y=number of grid points
 */
int f_import_mem(ARG2) {
    long int size;
    float *grid;
    if (mode == -1) {
        decode = 1;
    }
    else if (mode >= 0) {
	grid = (float *) strtoul(arg1,NULL,10);
	size = strtol(arg2,NULL,10);
	if (size != ndata)  fatal_error_ii(
            "import_mem: record sizes wrong, %u expected %u", size, (long) ndata);
	memcpy(data, grid, ndata * sizeof(float));
    }
    return 0;
}



#else
int f_grid2mem(ARG3) {
    if (mode == -1) fatal_error("grib2mem: only used when calling wgrib2 subroutine","");
    return 1;
}
int f_last2mem(ARG2) {
    if (mode == -1) fatal_error("txt2mem: only used when calling wgrib2 subroutine","");
    return 1;
}
int f_import_mem(ARG2) {
    if (mode == -1) fatal_error("import_mem: only used when calling wgrib2 subroutine","");
    return 1;
}
#endif
