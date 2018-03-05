#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Public Domain 2010: Wesley Ebisuzaki
 */

/*
 * HEADER:400:spectral:inv:0:show spectral bands
 */
int f_spectral(ARG0) {
    int nb;
    if (mode >= 0 && GB2_ProdDefTemplateNo(sec)) {
	nb = (int) sec[4][13];
        sprintf(inv_out,"num spectral bands=%d", nb);
	inv_out += strlen(inv_out);
    }
    return 0;
}
