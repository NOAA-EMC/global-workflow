#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"


#ifdef USE_JASPER
#include "grib2.h"
#include <jasper/jasper.h>
#endif
/*
 * Grib_out
 *
 * routines to encode data into grib2
 *
 * 12/2007 Public Domain by Wesley Ebisuzaki
 *
 */

extern int decode, nx, ny, scan;
extern int file_append, flush_mode;
extern enum output_order_type output_order;
extern int use_scale, dec_scale, bin_scale, max_bits, wanted_bits;
extern int save_translation;
extern enum output_grib_type grib_type;
extern int use_bitmap;

/*
 * HEADER:100:set_grib_type:misc:1:set grib type = jpeg, simple, ieee, complex(1|2|3), same
 */

int f_set_grib_type(ARG1) {
    int pack;
    if (strcmp(arg1,"jpeg") == 0) grib_type = jpeg;
    else if (strcmp(arg1,"j") == 0) grib_type = jpeg;
    else if (strcmp(arg1,"ieee") == 0) grib_type = ieee_packing;
    else if (strcmp(arg1,"i") == 0) grib_type = ieee_packing;
    else if (strcmp(arg1,"simple") == 0) grib_type = simple;
    else if (strcmp(arg1,"s") == 0) grib_type = simple;
    else if (strcmp(arg1,"complex1") == 0) grib_type = complex1;
    else if (strcmp(arg1,"c1") == 0) grib_type = complex1;
    else if (strcmp(arg1,"complex2") == 0) grib_type = complex2;
    else if (strcmp(arg1,"c2") == 0) grib_type = complex2;
    else if (strcmp(arg1,"complex3") == 0) grib_type = complex3;
    else if (strcmp(arg1,"c3") == 0) grib_type = complex3;
    else if (strcmp(arg1,"same") == 0) {
	if (mode >= 0) {
            pack = code_table_5_0(sec);
	    if (pack == 0) grib_type = simple;
	    else if (pack == 2) grib_type = complex1;
	    else if (pack == 3) {
	        if (code_table_5_6(sec) == 1) grib_type = complex2;
	        else grib_type = complex3;
	    }
	    else if (pack == 4) grib_type = ieee_packing;
	    else if (pack == 40) grib_type = jpeg;
	    // cannot duplicate output grib type
	    else grib_type = complex1;
	}
	else grib_type = complex1;
    }
    else fatal_error("set_grib_type: bad type %s", arg1);
    return 0;
}

/*
 * HEADER:100:set_bitmap:misc:1:use bitmap when creating complex packed files X=1/0
 */
int f_set_bitmap(ARG1) {
    if (mode >= -1) {
	use_bitmap = atoi(arg1);
    }
    return 0;
}
 
/*
 * HEADER:100:grib_out:output:1:writes decoded/modified data in grib-2 format to file X
 */

int f_grib_out(ARG1) {

    float *data_tmp;

    if (mode == -1) {
        save_translation = decode = 1;
	*local = file_append ? (void *) ffopen(arg1, "ab") : (void *) ffopen(arg1, "wb");
        if (*local == NULL) fatal_error("Could not open %s", arg1);
    }
    else if (mode == -2) {
	ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
	if ((data_tmp = (float *) malloc(ndata * sizeof(float))) == NULL)
	 	fatal_error("memory allocation - data_tmp","");
        undo_output_order(data, data_tmp, ndata);

        grib_wrt(sec, data_tmp, ndata, nx, ny, use_scale, dec_scale, 
		bin_scale, wanted_bits, max_bits, grib_type, (FILE *) *local);
        if (flush_mode) fflush((FILE *) *local);
	free(data_tmp);
    }
    return 0;
}

/*
 * grib_wrt - writes out grib file
 *
 * under some conditions, data will be changed
 */

int grib_wrt(unsigned char **sec, float *data, unsigned int ndata, int nx, int ny, int use_scale, int dec_scale, 
	int bin_scale, int wanted_bits, int max_bits, enum output_grib_type grib_type, FILE *out) {

    if (grib_type == simple) simple_grib_out(sec, data, ndata, use_scale, dec_scale, bin_scale, wanted_bits, max_bits, out); 
    else if (grib_type == ieee_packing) ieee_grib_out(sec, data, ndata, out);
    else if (grib_type == jpeg) jpeg2000_grib_out(sec, data, ndata, nx, ny, use_scale, dec_scale, bin_scale, wanted_bits, max_bits, out);
    else if (grib_type == complex1) complex_grib_out(sec, data, ndata, use_scale, dec_scale, bin_scale, wanted_bits, 
	max_bits, 1, use_bitmap, out); 
    else if (grib_type == complex2) complex_grib_out(sec, data, ndata, use_scale, dec_scale, bin_scale, wanted_bits, 
	max_bits, 2, use_bitmap, out); 
    else if (grib_type == complex3) complex_grib_out(sec, data, ndata, use_scale, dec_scale, bin_scale, wanted_bits, 
	max_bits, 3, use_bitmap, out); 

    return 0;
}

/*
 * HEADER:100:set_grib_max_bits:misc:1:sets scaling so number of bits does not exceed N in (new) grib output
 */

int f_set_grib_max_bits(ARG1) {
    int i;

    i = atoi(arg1);
    if (mode >= -1) {
	if (i < 0) fatal_error_i("set_grib_max_bits: %d is less than zero", i);
	if (i > 25) fatal_error_i("set_grib_max_bits: %d > 25", i);
    }
    if (mode >= -1) max_bits = i;
    return 0;
}

/*
 * based on mk_BMS (from gribw)
 *
 * public domain 12/2007 Wesley Ebisuzaki
 *
 * note: data[] is changed (undefined values are eliminated)
 */

unsigned char *mk_bms(float *data, unsigned int *ndata) {

    int bms_size, c, imask;
    unsigned char *bms, *cbits;
    unsigned int nn, i, start;

    nn = *ndata;
    bms_size = 6 + (nn+7) / 8;
    bms = (unsigned char *) malloc(bms_size);

    uint_char(bms_size, bms);		// length of section 6
    bms[4] = 6;				// section 6
    bms[5] = 0;				// has bitmap

    cbits = bms + 6;
    c = start = 0;
    imask = 128;
    for (i = 0; i < nn; i++) {
	if (data[i] < UNDEFINED_LOW || data[i] > UNDEFINED_HIGH) {
	    c += imask;
	    data[start++] = data[i];
	}
	if ((imask >>= 1) == 0) {
	    *cbits++ = c;
	    c = 0;
	    imask = 128;
	}
    }
    if (imask != 128) *cbits = c;
    *ndata = start;
    if (nn == start) {
	free (bms);
	bms = (unsigned char *) malloc(6);
	uint_char(6, bms);		// length of section 6
	bms[4] = 6;			// section 5
	bms[5] = 255;			// no bitmap
    }
    return bms;
}


/*
 * HEADER:100:set_bin_prec:misc:1:X set number of binary bits for grib_out packing
 */

int f_set_bin_prec(ARG1) {
    int i;

    struct local_struct {
        int nbits;
    };
    struct local_struct *save;

    if (mode == -1) {

        i = atoi(arg1);
	if (i < 0) i = 0;
	if (i > max_bits) i = max_bits;

        *local = save = (struct local_struct *)malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("set_bit_prec  memory allocation ","");
        save->nbits = wanted_bits = i;
	use_scale = 0;
    }
    else if (mode < 0) {
	free(*local);
    }
    else if (mode >= 0) {
	save = (struct local_struct *) *local;
	wanted_bits = save->nbits;
	use_scale = 0;
    }
    return 0;
}

/*
 * HEADER:100:set_scaling:misc:2:set decimal scaling=X binary scaling=Y for grib_out packing
 *
 * if arg1 == same .. use grib file definition
 */

int f_set_scaling(ARG2) {

    struct local_struct {
        int dec, bin;
    };
    struct local_struct *save;

    if (mode == -1) {
        *local = save = (struct local_struct *)malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("set_scaling: memory allocation ","");

        save->dec = atoi(arg1);
        save->bin = atoi(arg2);
        use_scale = 1;
    }
    else if (mode >= 0) {
        save = (struct local_struct *) *local;
       	use_scale = 1;
        dec_scale = save->dec;
        bin_scale = save->bin;
    }
    return 0;
}


/*
 * grib: convert linear list of ints to a bitstream
 *
 * in public domain : 2007 Wesley Ebisuzaki
 */


static unsigned int mask[] = {0,1,3,7,15,31,63,127,255};

void flist2bitstream(float *list, unsigned char *bitstream, unsigned int ndata, int nbits) 
{

    int cbits, jbits;
    unsigned int j, c;

    if (nbits == 0) {
	return;
    }
    if (nbits < 0) fatal_error_i( "flist2bitstream nbits < 0!  nbits = %d", nbits);

    cbits = 8;
    c = 0;
    while (ndata-- > 0) {
	/* note float -> unsigned int .. truncate */
        j = (unsigned int) (*list++ + 0.5);
	jbits = nbits;
	while (cbits <= jbits) {
	    if (cbits == 8) {
	        jbits -= 8;
	        *bitstream++ = (j >> jbits) & 255;
	    }
	    else {
	        jbits -= cbits;
	        *bitstream++ = (c << cbits) + ((j >> jbits) & mask[cbits]);
		cbits = 8;
	        c = 0;
	    }
	}
	/* now jbits < cbits */
	if (jbits) {
	    c = (c << jbits) + (j & mask[jbits]);
	    cbits -= jbits;
	}
    }
    if (cbits != 8) *bitstream++ = c << cbits;
}

int set_order(unsigned char **sec, enum output_order_type order) {
    int flag_3_4;

    if (order == raw) return 0;
    flag_3_4 = flag_table_3_4(sec) & 15;
    if (order == wesn) return set_flag_table_3_4(sec, flag_3_4 | (4 << 4));
    if (order == wens) return set_flag_table_3_4(sec, flag_3_4);
    return 1;
}

/*
 * write sections as a grib message
 */

int wrt_sec(unsigned char *sec0, unsigned char *sec1, unsigned char *sec2, 
   unsigned char *sec3, unsigned char *sec4, unsigned char *sec5, 
   unsigned char *sec6, unsigned char *sec7, FILE *out) {

    unsigned long int size;
    unsigned char s[8];
    unsigned int i;

    size = (unsigned long int) GB2_Sec0_size + GB2_Sec8_size +
         (sec1 ? uint4(sec1) : 0) +
         (sec2 ? uint4(sec2) : 0) +
         (sec3 ? uint4(sec3) : 0) +
         (sec4 ? uint4(sec4) : 0) +
         (sec5 ? uint4(sec5) : 0) +
         (sec6 ? uint4(sec6) : 0) +
         (sec7 ? uint4(sec7) : 0);

    fwrite((void *) sec0, sizeof(char), 8, out);
    uint8_char(size, s);
    fwrite((void *) s, sizeof(char), 8, out);

    if (sec1) {
	i = uint4(sec1);
	if (fwrite((void *)sec1, sizeof(char), i, out) != i) return 1;
    }
    if (sec2) {
	i = uint4(sec2);
	if (fwrite((void *)sec2, sizeof(char), i, out) != i) return 1;
    }
    if (sec3) {
	i = uint4(sec3);
	if (fwrite((void *)sec3, sizeof(char), i, out) != i) return 1;
    }
    if (sec4) {
	i = uint4(sec4);
	if (fwrite((void *)sec4, sizeof(char), i, out) != i) return 1;
    }
    if (sec5) {
	i = uint4(sec5);
	if (fwrite((void *)sec5, sizeof(char), i, out) != i) return 1;
    }
    if (sec6) {
	i = uint4(sec6);
	if (fwrite((void *)sec6, sizeof(char), i, out) != i) return 1;
    }
    if (sec7) {
	i = uint4(sec7);
	if (fwrite((void *)sec7, sizeof(char), i, out) != i) return 1;
    }
    s[0] = s[1] = s[2] = s[3] = 55; /* s = "7777" */
    if (fwrite((void *) s, sizeof(char), 4, out) != 4) return 1;

    return 0;
}
