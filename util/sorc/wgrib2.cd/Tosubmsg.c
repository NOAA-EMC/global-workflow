/*
 * Tosubmsg.c
 *   Write submessages
 *
 * June 2009: R.N. Bokhorst, reinoud.bokhorst@bmtargoss.com, Public Domain
 * June 2009: some changes Wesley Ebisuzaki
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

// #define DEBUG
#define GB2_Sec_i_size(i)  (uint4(sec[i]+0))

struct submsg {
    long start_pos;
    unsigned long saved_space, written_count, written_bytes;
    unsigned char *last_sec[9];
    FILE *output;
};

extern int   file_append;
extern int   flush_mode;


/* mode functions */
static int init_tosubmsg(ARG1, struct submsg *save);
static int write_tosubmsg(ARG1, struct submsg *save);
static int cleanup_tosubmsg(ARG1, struct submsg *save);

/* check for same section .. only for sections 1..7 */
static int same_sec(unsigned char *seca, unsigned char *secb) {
    unsigned int a, b, i;
    if (seca == NULL && secb == NULL) return 1;
    if (seca == NULL || secb == NULL) return 0;
    a = uint4(seca);
    b = uint4(secb);
    if (a != b) return 0;
    for (i = 0; i < a; i++) {
	if (seca[i] != secb[i]) return 0;
    }
    return 1;
}

/*
 * HEADER:100:tosubmsg:output:1:convert GRIB message to submessage and write to file X
 */
int f_tosubmsg(ARG1) {
    struct submsg *save;

    if (flush_mode)
      fatal_error("Tosubmsg: requires random access I/O, incompatible with -flush" ,"");

    if (mode == -1) {
        *local = save = (struct submsg *) malloc( sizeof(struct submsg));
        if (save == NULL) fatal_error("memory allocation tosubmsg","");
        init_tosubmsg(call_ARG1(inv_out,local,arg1), save);
    }
    else if (mode >= 0) {
        save = (struct submsg *) *local;
        write_tosubmsg(call_ARG1(inv_out,local,arg1), save);
    }
    else if (mode == -2) {
        save = (struct submsg *) *local;
        cleanup_tosubmsg(call_ARG1(inv_out,local,arg1), save);
    }
    return 0;
}


/*
 * init_tosubmsg: Initialisation (mode = -1)
 */
int init_tosubmsg(ARG1, struct submsg *save) {

    /* Open output file */
    /* Note that we have to use rb+ instead of ab in append mode, otherwise we cannot
       overwrite section 0 somewhere in the middle of the file */

    if ((save->output  = ffopen(arg1, file_append ? "rb+" : "wb")) == NULL) {
        fatal_error("Tosubmsg: Could not open %s", arg1);
    }
    if (file_append) {
        /* rb+ mode positions file at the beginning */
        fseek(save->output, 0L, SEEK_END);
    }

    /* save start position in file for rewriting section 0 */
    save->start_pos = ftell(save->output);
    save->saved_space = 0;
    save->written_count = 0;
    save->written_bytes = 0;
    init_sec(save->last_sec);

    return 0;
}


/*
 * write_tosubmsg: process each (sub)messages (mode = 0,1,2)
 */
int write_tosubmsg(ARG1, struct submsg *save) {
    int i, ok;
    static unsigned char sec6_repeat[] = {0,0,0,6,6,254};

    if (save->written_count == 0L) {

	/* first message */

	/* copy all sections */
        copy_sec(sec, save->last_sec);

	/* write sections 0..7 */

        fwrite((void *) sec[0], sizeof(char), GB2_Sec0_size, save->output);
        save->written_bytes = (unsigned long int) GB2_Sec0_size;

        for (i = 1; i <= 7; i++) {
            if (sec[i]) {
                fwrite((void *) sec[i], sizeof(char), GB2_Sec_i_size(i), save->output);
                save->written_bytes += GB2_Sec_i_size(i);
            }
	}
        save->written_count++;
	return 0;
    }

    /* can only merge if sec0 and sec1 are the same */

    ok = 1;
    // check discipline
    if (sec[0][6] != save->last_sec[0][6]) ok = 0;
    // check grib number
    if (sec[0][7] != save->last_sec[0][7]) ok = 0;

    if (same_sec(sec[1],save->last_sec[1]) == 0) ok = 0;

    if (ok == 0) {
	fprintf(stderr,"tosubmsg: only handle one discipline at a time, record not saved\n");
	return 0;
    }

    ok = 1;  	// ok to not to write
    for (i = 2; i <= 7; i++) {
	if (i == 4) ok = 0;	// can only skip sections 2 or 3
	if (ok == 1) {
            if (same_sec(sec[i],save->last_sec[i]) == 0) ok = 0;
	}
	if (ok == 0) {
	    if (i == 6 && GB2_Sec6_size(sec) > 6 && same_sec(sec[i],save->last_sec[i])) {
		// if same bitmap as before .. use special code
                fwrite(sec6_repeat, sizeof(char), 6, save->output);
                save->written_bytes += 6;
    		save->saved_space += GB2_Sec6_size(sec) - 6;
                if (mode == 99) fprintf(stdout, ":Bitmap indicator set to 254");
	    }
	    else {
		if (sec[i] != NULL) {
                    fwrite((void *) sec[i], sizeof(char), GB2_Sec_i_size(i), save->output);
                    save->written_bytes += GB2_Sec_i_size(i);
		}
	    }
	}
	else {
	    // save space by not writing out a duplicate section
            if (sec[i]) save->saved_space += GB2_Sec_i_size(i);
	}
    }

    // refresh the sections
    // this could be optimized
    free_sec(save->last_sec);
    copy_sec(sec, save->last_sec);

    save->written_count++;

    return 0;
}


/*
 * cleanup_tosubmsg: cleaning up (mode = -2)
 */
static int cleanup_tosubmsg(ARG1, struct submsg *save) {

    unsigned char s[GB2_Sec8_size];

    if (save->written_count > 0) {
        /* Write section 8 */
        s[0] = s[1] = s[2] = s[3] = 55; /* 7777 */
        fwrite((void *) s, sizeof(char), GB2_Sec8_size, save->output);
        save->written_bytes += (unsigned long int) GB2_Sec8_size;

        /* Rewrite section 0 with correct total size */
        fseek(save->output, save->start_pos, SEEK_SET);
        uint8_char(save->written_bytes, save->last_sec[0]+8);
        fwrite(save->last_sec[0], sizeof(char), GB2_Sec0_size, save->output);
    }


    fprintf(stderr, "\nSubmessage statistics:\n"
                  "- # submessages written  : %ld\n"
                  "- Kbytes saved           : %ld\n"
                  "- Kbytes written         : %ld\n"
                        ,save->written_count, save->saved_space/1024, save->written_bytes/1024);

    ffclose(save->output);
    free_sec(save->last_sec);
    return 0;
}
