#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"
/*
 * File.c
 *   routines that write output to files
 *
 * 2006: Public Domain, Wesley Ebisuzaki
 * 1/2007: cleanup M. Schwarb
 * 1/2008: lat and lon changed from float to double
 * 1/2008: W. Ebisuzaki, use filename "-" to write to stdout (-text, -spread)
 * 2/2008: W. Ebisuzaki, use filename "-" to write to stdout (-text, -spread) revised
 * 2/2008: W. Ebisuzaki, use filename "-" to write to stdout (-text, -spread) revised
 * 2/2008: W. Ebisuzaki remove decode flags for grib, GRIB
 * 6/2011: W. Ebisuzaki WxText enabled -spread, added flush_mode to -spread
 */

extern int header;
extern int file_append;
extern int decode, latlon;
extern int flush_mode;
extern int nx, ny;
extern double *lat, *lon;
extern int WxText, WxNum;

/* parameters for text mode */
const char *text_format;
int text_column;
extern const char *nl;

/*
 * HEADER:100:bin:output:1:write binary data to X
 */

int f_bin(ARG1) {
    unsigned int i, j;
    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1, file_append ? "ab" : "wb")) == NULL) {
	    fatal_error("Could not open %s", arg1);
	}
        decode = 1;
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
        i = ndata * sizeof(float);
        if (header) fwrite((void *) &i, sizeof(int),1, (FILE *) *local);
        j = fwrite((void *) data, sizeof(float), ndata, (FILE *) *local);
	if (j != ndata) fatal_error_i("bin: error writing grid point written=%d", j);
        if (header) fwrite((void *) &i, sizeof(int),1, (FILE *) *local);
        if (flush_mode) fflush((FILE *) *local);
    }
    return 0;
}


/*
 * HEADER:100:ieee:output:1:write (default:big-endian) IEEE data to X
 */

int f_ieee(ARG1) {

    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1, file_append ? "ab" : "wb")) == NULL) {
	    fatal_error("Could not open %s", arg1);
	}
        decode = 1;
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
	wrtieee(data, ndata, header, (FILE *) *local);
        if (flush_mode) fflush((FILE *) *local);
    }
    return 0;
}

/*
 * HEADER:100:text_fmt:misc:1:format for text output (C)
 */
int f_text_fmt(ARG1) {
    if (mode >= -1) text_format = arg1;
    return 0;
}

/*
 * HEADER:100:text_col:misc:1:number of columns on text output
 */
int f_text_col(ARG1) {
    if (mode >= -1) {
        text_column = atoi(arg1);
        if (text_column < 1) text_column = 1;
    }
    return 0;
}

/*
 * HEADER:100:text:output:1:write text data into X
 */
int f_text(ARG1) {
    unsigned int i;

    if (mode == -1) {
	if ((*local = (void *) ffopen(arg1,file_append ? "a" : "w")) == NULL) 
	        fatal_error("Could not open %s", arg1);
        decode = 1;
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
        if (header == 1) {
	    fprintf((FILE *) *local,"%d %d\n", nx, ny);
	}
	for (i = 0; i < ndata; i++) {
	    fprintf((FILE *) *local, text_format, data[i]);
            fprintf((FILE *) *local, ((i+1) % text_column) ? " " : "\n");
        }
        if (flush_mode) fflush((FILE *) *local);
    }
    return 0;
}

/*
 * HEADER:100:spread:output:1:write text - spread sheet format into X (WxText enabled)
 */
int f_spread(ARG1) {
    unsigned int i;

    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1,file_append ? "a" : "w")) == NULL)
	        fatal_error("Could not open %s", arg1);
        WxText = latlon = decode = 1;
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
	if (lat == NULL || lon == NULL || data == NULL) {
	    fprintf(stderr,"no code to determine lat-lon information, no spread sheet output\n");
	    return 0;
	}
	set_mode(0);
	f_var(call_ARG0(inv_out,NULL));
	fprintf((FILE *) *local,"lon,lat,%s",inv_out);
	f_lev(call_ARG0(inv_out,NULL));
	fprintf((FILE *) *local," %s", inv_out);
	f_t(call_ARG0(inv_out,NULL));
	fprintf((FILE *) *local," %s", inv_out);
	f_ftime(call_ARG0(inv_out,NULL));
	fprintf((FILE *) *local," %s\n", inv_out);

	if (WxNum > 0) {
	    for (i = 0; i < ndata; i++) {
	        if(!UNDEFINED_VAL(data[i])) 
	            fprintf((FILE *) *local,"%lf,%lf,\"%s\"\n",lon[i],lat[i],WxLabel(data[i]));
	    }
	}
	else {
	    for (i = 0; i < ndata; i++) {
	        if(!UNDEFINED_VAL(data[i])) 
	            fprintf((FILE *) *local,"%lf,%lf,%g\n",lon[i],lat[i],data[i]);
	    }
	}
        if (flush_mode) fflush((FILE *) *local);
	inv_out[0] = 0;
	set_mode(mode);
    }

    return 0;
}


/*
 * HEADER:100:GRIB:output:1:writes entire GRIB record (all submessages)
 */

int f_GRIB(ARG1) {
    long unsigned int size;

    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1,file_append ? "ab" : "wb")) == NULL) {
	    fatal_error("Could not open %s", arg1);
	}
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
        /* figure out size of grib file */
        size = uint8(sec[0]+8);
        /* write entire record to out */
        fwrite((void *) sec[0], sizeof(char), size, (FILE *) *local);
        if (flush_mode) fflush((FILE *) *local);
    }
    return 0;
}


/*
 * HEADER:100:grib:output:1:writes GRIB record (one submessage) to X
 */

int f_grib(ARG1) {
    unsigned long int size;
    int i;
    unsigned char s[8];

    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1, file_append ? "ab" : "wb")) == NULL) {
	    fatal_error("Could not open %s", arg1);
	}
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
        /* figure out size of grib file */
        size = (unsigned long int) GB2_Sec0_size + GB2_Sec1_size(sec) + GB2_Sec2_size(sec) + 
          + GB2_Sec3_size(sec) + GB2_Sec4_size(sec) + GB2_Sec5_size(sec) + GB2_Sec6_size(sec) 
          + GB2_Sec7_size(sec) + GB2_Sec8_size;

        /* section 0 */
        fwrite((void *) sec[0], sizeof(char), 8, (FILE *) *local);
        uint8_char(size, s);
        fwrite((void *) s, sizeof(char), 8, (FILE *) *local);
        for (i = 1; i <= 7; i++) {
            if (sec[i]) {
                size = uint4(sec[i]);
                fwrite((void *) sec[i], sizeof(char), size, (FILE *) *local);
            }
        }
        s[0] = s[1] = s[2] = s[3] = 55; /* s = "7777" */
        fwrite((void *) s, sizeof(char), 4, (FILE *) *local);
//      fwrite((void *) "7777", sizeof(char), 4, (FILE *) *local);

        if (flush_mode) fflush((FILE *) *local);
    }
    return 0;
}

/*
 * HEADER:100:persistent_file:setup:1:makes file persistent if already opened, X=filename CW2
 *   only useful when wgrib is called as a subroutine, no error if failure
 */

int f_persistent_file(ARG1) {
    if (mode == -1) {
	mk_file_persistent(arg1);
    }
    return 0;
}

/*
 * HEADER:100:close_persistent_file:setup:1:close persistent file, X=filename CW2
 *   only useful when wgrib is called as a subroutine, no error if failure
 */

int f_close_persistent_file(ARG1) {
    if (mode == -1) {
	mk_file_transient(arg1);
    }
    return 0;
}

/*
 * HEADER:100:rewind_file:setup:1:rewinds file if already opened, X=filename, CW2
 *   only useful when wgrib is called as a subroutine, no error if failure
 */
int f_rewind_file(ARG1) {
    if (mode == -1) {
	rewind_file(arg1);
    }
    return 0;
}

