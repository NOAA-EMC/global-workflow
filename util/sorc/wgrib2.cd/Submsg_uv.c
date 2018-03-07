/*
 * Submsg_uv
 *
 * Put vector fields into same submessage .. for multiprocessing
 *  based on NCEP_uv.c
 *  
 * to alter the U/V list, use -new_grid_vectors
 *
 * U and V must be adjacent
 *
 * 1/2015 Public Domain Wesley Ebisuzaki
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

extern int file_append;
extern const char **vectors;

/*
   is_u:
   returns name of v field or NULL
 */

static const char *is_u(const char *uname) {
    int i;

    i = 0;
    while (vectors[i] != NULL) {
	if (strcmp(vectors[i], uname) == 0) {
	    return vectors[i + 1];
	}
	i += 2;
    }
    return NULL;
}

/*
 * HEADER:111:submsg_uv:output:1:combine vector fields into one message
 */

int f_submsg_uv(ARG1) {

    struct local_struct {
        unsigned char *sec[9];
	const char *vname;    
        FILE *output;
    };

    struct local_struct *save;
    int i, j, is_v;
    unsigned long int size;
    char name[NAMELEN];
    unsigned char s[8];

    if (mode == -1) {		// initialization

        // allocate static structure

        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct) );
        if (save == NULL) fatal_error("submsg_uv: memory allocation","");

        if ((save->output = ffopen(arg1, file_append ? "ab" : "wb")) == NULL) {
            fatal_error("submsg_uv: could not open file %s", arg1);
        }
	save->vname = NULL;
        init_sec(save->sec);
	return 0;
    }

    save = (struct local_struct *) *local;

    if (mode == -2)  {		// cleanup
	if (save->vname != NULL) {		// write out cached field
	   i = wrt_sec(save->sec[0], save->sec[1], save->sec[2], save->sec[3], 
		save->sec[4], save->sec[5], save->sec[6], save->sec[7], save->output);
	   if (i) fatal_error_i("submsg_uv: last record problem %i",i);
           free_sec(save->sec);
	}
	ffclose(save->output);
        free(save);
	return 0;
    }

    if (mode >= 0 )  {					// processing
        i = getName(sec, mode, NULL, name, NULL, NULL);

	/* see if name == expected vname */
	is_v = 0;
	if (save->vname != NULL && strcmp(name, save->vname) == 0) {
	    is_v = 1;
            if (same_sec0(sec,save->sec) == 0) is_v = 0;
            if (same_sec1(sec,save->sec) == 0) is_v = 0;
            if (same_sec2(sec,save->sec) == 0) is_v = 0;
            if (same_sec3(sec,save->sec) == 0) is_v = 0;

	    i = GB2_ParmNum(sec);
	    j = GB2_ParmCat(sec);
	    GB2_ParmNum(sec) = GB2_ParmNum(save->sec);
	    GB2_ParmCat(sec) = GB2_ParmCat(save->sec);
            if (same_sec4(sec,save->sec) == 0) is_v = 0;
	    GB2_ParmNum(sec) = i;
	    GB2_ParmCat(sec) = j;
	}

	/* finished tests for U/V sequence */

	/* is U/V sequence */
	if (is_v) {
	    size = (unsigned long int) GB2_Sec0_size + GB2_Sec8_size +
	    (sec[1] ? uint4(sec[1]) : 0) +
	    (sec[2] ? uint4(sec[2]) : 0) +
	    (sec[3] ? uint4(sec[3]) : 0) +
	    (sec[4] ? uint4(sec[4]) : 0) +
	    (sec[5] ? uint4(sec[5]) : 0) +
	    (sec[6] ? uint4(sec[6]) : 0) +
	    (sec[7] ? uint4(sec[7]) : 0) +
	    (save->sec[4] ? uint4(save->sec[4]) : 0) +
	    (save->sec[5] ? uint4(save->sec[5]) : 0) +
	    (save->sec[6] ? uint4(save->sec[6]) : 0) +
	    (save->sec[7] ? uint4(save->sec[7]) : 0);

	    fwrite((void *) sec[0], sizeof(char), 8, save->output);
            uint8_char(size, s);
            fwrite((void *) s, sizeof(char), 8, save->output);

            if (sec[1]) {
                i = uint4(sec[1]);
                if (fwrite((void *)sec[1], sizeof(char), i, save->output) != i) return 1;
            }
            if (sec[2]) {
                i = uint4(sec[2]);
                if (fwrite((void *)sec[2], sizeof(char), i, save->output) != i) return 1;
            }
            if (sec[3]) {
                i = uint4(sec[3]);
                if (fwrite((void *)sec[3], sizeof(char), i, save->output) != i) return 1;
            }
            if (save->sec[4]) {
                i = uint4(save->sec[4]);
                if (fwrite((void *)save->sec[4], sizeof(char), i, save->output) != i) return 1;
            }
            if (save->sec[5]) {
                i = uint4(save->sec[5]);
                if (fwrite((void *)save->sec[5],sizeof(char), i, save->output) != i) return 1;
            }
            if (save->sec[6]) {
                i = uint4(save->sec[6]);
                if (fwrite((void *)save->sec[6],sizeof(char), i, save->output) != i) return 1;
            }
            if (save->sec[7]) {
                i = uint4(save->sec[7]);
                if (fwrite((void *)save->sec[7],sizeof(char), i, save->output) != i) return 1;
            }
            if (sec[4]) {
                i = uint4(sec[4]);
                if (fwrite((void *)sec[4], sizeof(char), i, save->output) != i) return 1;
            }
            if (sec[5]) {
                i = uint4(sec[5]);
                if (fwrite((void *)sec[5], sizeof(char), i, save->output) != i) return 1;
            }
            if (sec[6]) {
                i = uint4(sec[6]);
                if (fwrite((void *)sec[6], sizeof(char), i, save->output) != i) return 1;
            }
            if (sec[7]) {
                i = uint4(sec[7]);
                if (fwrite((void *)sec[7], sizeof(char), i, save->output) != i) return 1;
            }

            s[0] = s[1] = s[2] = s[3] = 55; /* s = "7777" */
            if (fwrite((void *) s, sizeof(char), 4, save->output) != 4)
	           fatal_error("submsg_uv: write record problem","");

	    save->vname = NULL;
            free_sec(save->sec);
            return 0;
	}

	/* has U but not V, write U */    

	if (save->vname != NULL) {
	   i = wrt_sec(save->sec[0], save->sec[1], save->sec[2], save->sec[3], 
		save->sec[4], save->sec[5], save->sec[6], save->sec[7], save->output);
	   if (i) fatal_error_i("submsg_uv: write record problem %i",i);
	   free_sec(save->sec);
	   save->vname = NULL;
	}

	/* check to see if new field is a U */

	save->vname = is_u(name);

	/* if U, cache it */
	if (save->vname != NULL) {
	    copy_sec(sec,save->sec);
	    return 0;
	}

	/* not U, write it out */
	i = wrt_sec(sec[0], sec[1], sec[2], sec[3], sec[4], sec[5], sec[6], sec[7], save->output);
	if (i) fatal_error_i("submsg_uv: write problem %i",i);
	return 0;
    }
    return 0;
}
