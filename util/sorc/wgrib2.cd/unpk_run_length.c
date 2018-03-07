#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "grb2.h"
#include "wgrib2.h"

// #define DEBUG

// unpk_run_length (8/2009) is in the public domain,  Wesley Ebisuzaki
//
// this routine unpacks a grib2 data section that is in the run length
//    packing method
//
// 8/2009 preliminary version, based on readrdr
// 9/2009 fixed typo in bitmap section
// 12/2014: David Binderman fixed line: while (vals[i] > mv && i < nvals) {

int unpk_run_length(unsigned char **sec, float *data, unsigned int ndata) {

    int i, k, pack, decimal_scale, n_bits;
    int mv, mvl;
    unsigned int j, ncheck, npts;

    double *levels, dec_factor;
    int size_compressed_data, nvals, *vals;
    int v, n, factor, range;
    int bitmap_flag;
    unsigned char *mask_pointer;
    const unsigned int mask[] = {128,64,32,16,8,4,2,1};

    pack = code_table_5_0(sec);
    if (pack != 200) return 0;

    npts = GB2_Sec3_npts(sec);
    n_bits = (int) sec[5][11];
    mv = (int) uint2(sec[5]+12);
    mvl = (int) uint2(sec[5]+14);
    decimal_scale = (int) sec[5][16];
    if (decimal_scale > 127) {		// convert signed negative values
	decimal_scale = - (decimal_scale - 128);
    }
    dec_factor = Int_Power(10.0, -decimal_scale);

#ifdef DEBUG
    printf(" packing=%d n_bits=%d mv=%d mvl=%d decimal_scale=%d\n", pack, n_bits, mv, 
		mvl, decimal_scale);
#endif

    size_compressed_data = GB2_Sec7_size(sec)-5;
    nvals = ( size_compressed_data * 8) / n_bits;
#ifdef DEBUG
    printf(" size_compressed data %d npnts %d\n", size_compressed_data, nvals);
#endif

    levels = (double *) malloc(mvl * sizeof(double));
    vals = (int *) malloc(nvals * sizeof(int));

    for (i = 0; i < mvl; i++) {
	levels[i] = int2(sec[5] + 17 + i*2)*dec_factor;
    }

#ifdef DEBUG
    for (i = 0; i < mvl; i++) {
	printf(" lvls[%d] = %lf ", i, levels[i]);
	if (i % 4 == 0) printf("\n");
	if (i % 4 == 0) printf("\n");
    }
    printf("\n");
#endif

    rd_bitstream(sec[7]+5, 0, vals, n_bits, nvals);

    ncheck = i = 0;
    range = (1 << n_bits) - 1 - mv;
    if (range <= 0) fatal_error("unpk_running_length: range error","");

    j = 0;
    mask_pointer = sec[6] + 6;
    bitmap_flag = code_table_6_0(sec);
    if (bitmap_flag == 254) bitmap_flag = 0;
    if (bitmap_flag != 0 && bitmap_flag != 255) 
	fatal_error("unpk_running_length: unsupported bitmap","");
    while (i < nvals) {
	if (vals[i] > mv) fatal_error_i("test rlp val=%d",(int) i);
	v = vals[i++];
	n = 1;
	factor = 1;
	// 12/2014 while (vals[i] > mv && i < nvals) {
	while (i < nvals && vals[i] > mv) {
	    n += factor * (vals[i]-mv-1);
	    factor = factor * range;
	    i++;
	}
	ncheck += n;
        if (ncheck > npts) fatal_error_i("unpk_run_length: ncheck > npts (%u),",npts);

	if (bitmap_flag != 0) {
	    for (k = 0; k < n; k++) data[j++] = levels[v];
	}
	else {
	    for (k = 0; k < n; k++) {
		while (mask_pointer[j >> 3] & mask[j & 7]) {
	            data[j++] = UNDEFINED;
		}
		data[j++] = levels[v];
	    }
	}
    }
    if (j != ndata) fatal_error("unpk_run_length: bitmap problem","");
    free(levels);
    free(vals);
    return 0;
}
