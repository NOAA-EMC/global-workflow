#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Public Domain 2009: Wesley Ebisuzaki
 */
extern char *nl;

/*
 * HEADER:400:spectral_bands:inv:0:spectral bands for satellite, pdt=4.31
 */
int f_spectral_bands(ARG0) {
    int pdtsize, pdt, nb, i;
    int code1, code2, instrument, scale_factor, scaled_val;
    float value;
    
    if (mode >= 0) {
        pdt = GB2_ProdDefTemplateNo(sec);
	if (pdt != 31) return 0;
	nb = sec[4][13];
	sprintf(inv_out,"%snumber of spectral bands=%d", nl, nb);
	inv_out += strlen(inv_out);

	// check size

	pdtsize =  prod_def_temp_size(sec);
	i = 14 + 11*nb;
	if (pdtsize != i) fatal_error_i("spectral bands: section 4 is wrong size %d",pdtsize);

	// print out spectral info
	for (i = 0; i < nb; i++) {
	    code1 = int2(sec[4]+14+11*i);
	    code2 = int2(sec[4]+16+11*i);
	    instrument = (int) sec[4][18+11*i];
	    scale_factor = int1(sec[4]+19+11*i);
	    scaled_val = int4(sec[4]+20+11*i);
	    value = scaled2flt(scale_factor, scaled_val);
	    sprintf(inv_out,"%sband %d %d instrument %d central wave no %lf (m-1)",
		nl, code1, code2, instrument, value);
	}
    }
    return 0;
}
