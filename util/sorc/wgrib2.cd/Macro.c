#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"


/* 3/2008 Public Domain Wesley Ebisuzaki
 * 3/2008 Manfred Schwarb added -V
 * 3/2012 Wesley Ebisuzaki: added use_ext_name (extended name)
 */

extern const char *item_deliminator;
extern int file_append, decode;
extern int use_ext_name;
extern int ieee_little_endian;

/*
 * HEADER:100:s:inv:0:simple inventory
 */

/*
 * this is a simple macro .. see how easy it is!
 * would be more complicated if functions used static variables
 * minor complication if need to set decode or latlon flags
 */

int f_s(ARG0) {

    if (mode >= 0) {
	f_t(call_ARG0(inv_out,NULL));
	strcat(inv_out,item_deliminator);
	inv_out += strlen(inv_out);

        if (use_ext_name == 0) f_var(call_ARG0(inv_out,NULL));
	else f_ext_name(call_ARG0(inv_out,NULL));

	strcat(inv_out,item_deliminator);
	inv_out += strlen(inv_out);

	f_lev(call_ARG0(inv_out, NULL));
	strcat(inv_out,item_deliminator);
	inv_out += strlen(inv_out);

	f_ftime(call_ARG0(inv_out,NULL));
	strcat(inv_out,item_deliminator);
	inv_out += strlen(inv_out);

        if (use_ext_name == 0) f_misc(call_ARG0(inv_out,NULL));
    }
    return 0;
}

/*
 * HEADER:100:S:inv:0:simple inventory with minutes and seconds (subject to change)
 */

int f_S(ARG0) {

    if (mode >= 0) {
        f_T(call_ARG0(inv_out,NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        if (use_ext_name == 0) f_var(call_ARG0(inv_out,NULL));
	else f_ext_name(call_ARG0(inv_out,NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        f_lev(call_ARG0(inv_out, NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        f_ftime(call_ARG0(inv_out,NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        if (use_ext_name == 0) f_misc(call_ARG0(inv_out,NULL));
    }
    return 0;
}


/*
 * HEADER:100:s_out:inv_output:1:simple inventory written to X
 */

int f_s_out(ARG1) {

    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1,file_append ? "a" : "w")) == NULL)
                fatal_error("Could not open %s", arg1);
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
	inv_out[0] = 0;
	f_s(call_ARG0(inv_out,NULL));
	fprintf((FILE *) *local, "%s\n", inv_out);
	inv_out[0] = 0;
    }
    return 0;
}

/*
 * HEADER:100:inv_f77:inv_output:3:match inventory written to Z with character*(Y) and options X
 */

int f_inv_f77(ARG3) {
    int clen, len, i;
    char blanks[100];
    unsigned char header[4];
    struct local_struct {
        int charlen;
        enum {bin, ieee} header_type;
        FILE *output;
    };
    struct local_struct *save;

    if (mode == -1) {
        *local = save = (struct local_struct *) malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("s_f77: memory allocation","");
        if ((save->output = ffopen(arg3, file_append ? "ab" : "wb")) == NULL) {
            fatal_error("s_inv_f77: could not open file %s", arg3);
	}
	save->charlen = clen = atoi(arg2);
	if (clen <= 0 || clen > 400) fatal_error_i("s_f77: len (%d) is bad or too large", clen);
	if (strcmp(arg1,"bin") == 0) save->header_type = bin;
	else if (strcmp(arg1,"ieee") == 0) save->header_type = ieee;
	else fatal_error("s_inv_f77: undefined type %s", arg1);
	return 0;
    }

    save = *local;
    clen = save->charlen;
    if (mode == -2) {                   // cleanup
	ffclose(save->output);
	if (save) free(save);
    }
    else if (mode >= 0) {
        inv_out[0] = 0;

        f_match_inv(call_ARG0(inv_out,NULL));
	len = strlen(inv_out);

	/* write header */
	if (save->header_type == bin) {
            fwrite((void *) &clen, sizeof(int), 1, save->output);
	}
	else {
	    if (ieee_little_endian) {
		header[0] = clen & 255;
		header[1] = (clen >> 8) & 255;
		header[2] = (clen >> 16) & 255;
		header[3] = (clen >> 24) & 255;
	    }
	    else {
		header[3] = clen & 255;
		header[2] = (clen >> 8) & 255;
		header[1] = (clen >> 16) & 255;
		header[0] = (clen >> 24) & 255;
	    }
            fwrite((void *) header, 1, 4, save->output);
	}
	if (len >= clen) {
	    fwrite((void *) inv_out, sizeof(unsigned char), clen, save->output);
	}
	else {
	    fwrite((void *) inv_out, sizeof(unsigned char), len, save->output);
	    for (i = 0; i < 100; i++) blanks[i] = ' ';
	    while (len < clen) {
		if (clen - len >= 100) {
                    fwrite((void *) blanks, sizeof(unsigned char), 100, save->output);
		    len +=100;
		}
		else {
                    fwrite((void *) blanks, sizeof(unsigned char), clen-len, save->output);
		    len = clen;
		}
	    }
	}
	if (save->header_type == bin) {
            fwrite((void *) &clen, sizeof(int), 1, save->output);
	}
	else {
            fwrite((void *) header, 1, 4, save->output);
	}
        inv_out[0] = 0;
    }
    return 0;
}

/*
 * HEADER:100:verf:inv:0:simple inventory using verification time
 */
int f_verf(ARG0) {

    if (mode >= 0) {
        f_vt(call_ARG0(inv_out,NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        if (use_ext_name == 0) f_var(call_ARG0(inv_out,NULL));
	else f_ext_name(call_ARG0(inv_out,NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        f_lev(call_ARG0(inv_out, NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        f_ftime(call_ARG0(inv_out,NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        if (use_ext_name == 0) f_misc(call_ARG0(inv_out,NULL));
    }
    return 0;
}

/*
 * Manfred Schwarb
 */

/*
 * HEADER:100:V:inv:0:diagnostic output
 */

int f_V(ARG0) {
    int oldmode;
    if (mode == -1) decode = 1;
    if (mode >= 0) {
        f_vt(call_ARG0(inv_out,NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        f_lev(call_ARG0(inv_out, NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        f_ftime(call_ARG0(inv_out,NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);

        oldmode=mode;
        mode=1;

        if (use_ext_name == 0) f_var(call_ARG0(inv_out,NULL));
	else f_ext_name(call_ARG0(inv_out,NULL));
        strcat(inv_out,item_deliminator);
        inv_out += strlen(inv_out);
        mode=oldmode;

        if (use_ext_name == 0) f_misc(call_ARG0(inv_out,NULL));
        strcat(inv_out,"\n    ");
        inv_out += strlen(inv_out);

        f_stats(call_ARG0(inv_out,NULL));
        strcat(inv_out,"\n    ");
        inv_out += strlen(inv_out);

        f_grid(call_ARG0(inv_out,NULL));
        strcat(inv_out,"\n");
    }
    return 0;
}
