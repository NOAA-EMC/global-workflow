#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Summary.c
 *
 * options that produce output at the end of the job (mode == 2)
 * they can provide a summary of the operations
 *
 * 10/2008 Public Domain Wesley Ebisuzaki
 * 1/2011 chagned new_GDS by GDS_chagne_no WNE
 *
 * count: writes to stdout the number of records processed
 * grid_changes: checks to see that only 1 grid type was processed
 *
 */



extern enum input_type input;
extern int header, dump_rec, dump_submsg;

extern int file_append;
extern const char *item_deliminator;
extern FILE *inv_file;
extern const char *nl;

extern int GDS_change_no;

/*
 * HEADER:100:count:misc:0:prints number of fields 
 */
int f_count(ARG0) {
    struct local_struct {
        int count;
    };
    struct local_struct *save;

    if (mode == -1) {
        *local = save = (struct local_struct *)malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("f_count memory allocation ","");
	save->count = 0;
    }
    else if (mode >= 0) {
        save = (struct local_struct *) *local;
	save->count += 1;
    }
    else if (mode == -2) {
        save = (struct local_struct *) *local;
	sprintf(inv_out,"number of records: %d", save->count);
	free(save);
    }
    return 0;
}

/*
 * HEADER:100:grid_changes:misc:0:prints number of grid changes
 */
int f_grid_changes(ARG0) {
    if (mode == -2) {
	switch(GDS_change_no) {
	case 0: sprintf(inv_out,"Warning: no grib2 records");
		break;
	case 1: sprintf(inv_out,"Good: only one grid");
		break;
	default: sprintf(inv_out,"Warning: muliple grids, %d changes", GDS_change_no);
	}
    }
    return 0;
}
