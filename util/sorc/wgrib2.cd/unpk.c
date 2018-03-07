/* unpack_grib 
 * 3/2008 public domain Wesley Ebisuzaki
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stddef.h>

#include "wgrib2.h"
#include "grb2.h"

#ifdef USE_PNG
   #include <png.h>
   #include "grib2.h"
   int dec_png(unsigned char *,g2int *,g2int *,char *);
#endif
#ifdef USE_JASPER
   #include <jasper/jasper.h>
#endif

/*
 * unpack grib -- only some formats (code table 5.0) are supported
 *
 * supported: 0 (simple), 4 (ieee), 40 (jpeg), 41(png)
 *
 * input:  sec[]
 *         float data[npnts]
 *
 */

int unpk_grib(unsigned char **sec, float *data) {

    int i, ndata, k, packing, bitmap_flag, nbits;
    unsigned char *mask_pointer, mask;
    unsigned char *ieee, *p, *c;
    int j;
    float reference, tmp;
    double bin_scale, dec_scale, b;

#ifdef USE_PNG
    g2int width, height;
#endif

#ifdef USE_JASPER
    jas_image_t *image;
    char *opts;
    jas_stream_t *jpcstream;
    jas_image_cmpt_t *pcmpt;
    jas_matrix_t *jas_data;
#endif

    packing = code_table_5_0(sec);
    ndata = (int) GB2_Sec3_npts(sec);
    bitmap_flag = code_table_6_0(sec);

    if (bitmap_flag != 0 && bitmap_flag != 254 && bitmap_flag != 255)
         fatal_error("unknown bitmap", "");

    if (packing == 4) {			// ieee
        if (sec[5][11] != 1) fatal_error_i("unpk ieee grib file precision %d not supported", 
		(int) sec[5][11]);

        // ieee depacking -- simple no bitmap etc testing
        if (bitmap_flag == 255) {
            for (i = 0; i < ndata; i++) {
                data[i] = ieee2flt_nan(sec[7]+5+i*4);
//                data[i] = ieee2flt(sec[7]+5+i*4);
            }
	    return 0;
        }
        if (bitmap_flag == 0 || bitmap_flag == 254) {
	    mask_pointer = sec[6] + 6;
	    ieee = sec[7]+5;
	    mask = 0;
	    for (i = 0; i < ndata; i++) {
		if ((i & 7) == 0) mask = *mask_pointer++;
		if (mask & 128) {
		    data[i] = ieee2flt_nan(ieee);
//		    data[i] = ieee2flt(ieee);
		     ieee += 4;
		}
		else {
		    data[i] = UNDEFINED;
		}
		mask <<= 1;
	    }
	    return 0;
	}
        fatal_error("unknown bitmap", "");
    }
    else if (packing == 0 || packing == 61) {			// simple grib1 packing  61 -- log preprocessing

	p = sec[5];
	reference = ieee2flt(p+11);
	bin_scale = Int_Power(2.0, int2(p+15));
	dec_scale = Int_Power(10.0, -int2(p+17));
	nbits = p[19];
	b = 0.0;
        if (packing == 61) b = ieee2flt(p+20);

	if (bitmap_flag != 0 && bitmap_flag != 254 && bitmap_flag != 255)
            fatal_error("unknown bitmap", "");

        if (nbits == 0) {
            tmp = reference*dec_scale;
	    if (packing == 61) tmp = exp(tmp) - b;		// remove log prescaling
            if (bitmap_flag == 255) {
                for (i = 0; i < ndata; i++) {
                    data[i] = tmp;
                }
                return 0;
            }
            if (bitmap_flag == 0 || bitmap_flag == 254) {
                mask_pointer = sec[6] + 6;
		mask = 0;
                for (i = 0; i < ndata; i++) {
		    if ((i & 7) == 0) mask = *mask_pointer++;
                    data[i] = (mask & 128) ?  tmp : UNDEFINED;
                    mask <<= 1;
                }
                return 0;
            }
        }

	mask_pointer = (bitmap_flag == 255) ? NULL : sec[6] + 6;

	unpk_0(data, sec[7]+5, mask_pointer, nbits, ndata, reference, 
		bin_scale,dec_scale);

	if (packing == 61) {		// remove log prescaling
// #pragma omp parallel for private(i) schedule(static)
            for (i = 0; i < ndata; i++) {
                if (DEFINED_VAL(data[i])) data[i] = exp(data[i]) - b;
            }
	}
	return 0;
    }

    else if (packing == 2 || packing == 3) {		// complex
	return unpk_complex(sec, data, ndata);
    }
    else if (packing == 200) {				// run length
	return unpk_run_length(sec, data, ndata);
    }
#ifdef USE_JASPER
    else if (packing == 40 ||  packing == 40000) {		// jpeg2000
	p = sec[5];
	reference = ieee2flt(p+11);
	bin_scale = Int_Power(2.0, int2(p+15));
	dec_scale = Int_Power(10.0, -int2(p+17));
	nbits = p[19];
	
	if (nbits == 0) {
	    tmp = reference*dec_scale;
            if (bitmap_flag == 255) {
		for (i = 0; i < ndata; i++) {
		    data[i] = tmp;
		}
		return 0;
	    }
            if (bitmap_flag == 0 || bitmap_flag == 254) {
                mask_pointer = sec[6] + 6;
                ieee = sec[7]+5;
                mask = 0;
                for (i = 0; i < ndata; i++) {
                    if ((i & 7) == 0) mask = *mask_pointer++;
                    data[i] = (mask & 128) ? tmp : UNDEFINED;
                    mask <<= 1;
                }
                return 0;
	    }
            fatal_error("unknown bitmap", "");
        }

	// decode jpeg2000

        image = NULL;
	opts = NULL;
        jpcstream=jas_stream_memopen((char *) sec[7]+5, (int) GB2_Sec7_size(sec)-5);
	image = jpc_decode(jpcstream, opts);
	if (image == NULL) fatal_error("jpeg2000 decoding", "");
	pcmpt = image->cmpts_[0];
        if (image->numcmpts_ != 1 ) 
		fatal_error("unpk: Found color image.  Grayscale expected","");

        jas_data=jas_matrix_create(jas_image_height(image), jas_image_width(image));
        jas_image_readcmpt(image,0,0,0,jas_image_width(image), jas_image_height(image),jas_data);

	// transfer data

	k = ndata - pcmpt->height_ * pcmpt->width_;

// #pragma omp parallel for private(i,j)
        for (i=0;i<pcmpt->height_;i++) {
            for (j=0;j<pcmpt->width_;j++) {
//		data[k++] = (((jas_data->rows_[i][j])*bin_scale)+reference)*dec_scale;
		data[k+j+i*pcmpt->width_] = (((jas_data->rows_[i][j])*bin_scale)+reference)*dec_scale;
	    }
	}

        if (bitmap_flag == 0 || bitmap_flag == 254) {
	    k = ndata - pcmpt->height_ * pcmpt->width_;
            mask_pointer = sec[6] + 6;
            mask = 0;
            for (i = 0; i < ndata; i++) {
                 if ((i & 7) == 0) mask = *mask_pointer++;
                 data[i] = (mask & 128) ? data[k++] : UNDEFINED;
                 mask <<= 1;
            }
        }
	else if (bitmap_flag != 255) {
            fatal_error_i("unknown bitmap: %d", bitmap_flag);
	}
	jas_matrix_destroy(jas_data);
	jas_stream_close(jpcstream);
	jas_image_destroy(image);
	return 0;
    }
#endif
#ifdef USE_PNG
    else if (packing == 41) {		// png
	p = sec[5];
	reference = ieee2flt(p+11);
	bin_scale = Int_Power(2.0, int2(p+15));
	dec_scale = Int_Power(10.0, -int2(p+17));
	nbits = p[19];

        if (nbits == 0) {
            tmp = reference*dec_scale;
            if (bitmap_flag == 255) {
                for (i = 0; i < ndata; i++) {
                    data[i] = tmp;
                }
                return 0;
            }
            if (bitmap_flag == 0 || bitmap_flag == 254) {
                mask_pointer = sec[6] + 6;
                ieee = sec[7]+5;
		mask = 0;
                for (i = 0; i < ndata; i++) {
                    if ((i & 7) == 0) mask = *mask_pointer++;
                    data[i] = (mask & 128) ?  tmp : UNDEFINED;
                    mask <<= 1;
                }
                return 0;
            }
            fatal_error("unknown bitmap", "");
        }


	if ((c = (unsigned char *) malloc(ndata*4*sizeof(char))) == NULL)
            fatal_error("unpk: allocation error", "");

	i = (int) dec_png(sec[7]+5, &width, &height, (char *) c);
	if (i) fatal_error_i("unpk: png decode error %d",i);
	mask_pointer = (bitmap_flag == 255) ? NULL : sec[6] + 6;

//	check sizes

	if (mask_pointer == NULL) {
	    if (ndata != width*height) 
    		fatal_error_i("png size mismatch w*h=%d", width*height);
	}
	else {
	    if (ndata != width*height + missing_points(mask_pointer, GB2_Sec3_npts(sec)) )
                fatal_error("png size mismatch", "");
	}

	unpk_0(data, c, mask_pointer, nbits, ndata, reference, 
		bin_scale,dec_scale);
	free(c);
	return 0;
    }
#endif

    fatal_error_i("packing type %d not supported", packing);
    return 1;
}
