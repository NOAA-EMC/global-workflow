#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/* NCEP dlat/dlon are only good to milli-degrees because
   they are converted from grib1 */

#define ERROR (0.001 * nx)

/*
 * HEADER:-1:cyclic:inv:0:is grid cyclic? (not for for mercator and thinned grids)
 */

extern enum output_order_type output_order;
int f_cyclic(ARG0) {
    if (mode >= 0) {
	sprintf(inv_out,cyclic(sec) ? "cyclic" : "not cyclic");
    }
    return 0;
}

/*
 * cyclic: return 0/1 if cyclic in longitude
 *
 * v1.1 add gaussian (not thinned)
 * v1.2 dx has to be defined, added mercator
 */

int cyclic(unsigned char **sec) {
    int grid_template, nx, ny, res, scan, flag_3_3, no_dx, basic_ang, sub_ang;
    unsigned int npnts;
    unsigned char *gds;
    double dlon, units, lon1, lon2;

    get_nxny(sec, &nx, &ny, &npnts, &res, &scan);
    if (nx <= 1 || ny <= 0) return 0;

    grid_template = code_table_3_1(sec);
    gds = sec[3];

    flag_3_3 = flag_table_3_3(sec);
    no_dx =  0;
    if (flag_3_3 != -1) {
        if ((flag_3_3 & 0x20) == 0) no_dx = 1;
    }
    if (no_dx) return 0;

    if (grid_template == 0) {

        basic_ang = GDS_LatLon_basic_ang(gds);
        sub_ang = GDS_LatLon_sub_ang(gds);
        units = basic_ang == 0 ?  0.000001 : (double) basic_ang / (double) sub_ang;

	/* dlon has to be defined */
        dlon = units * GDS_LatLon_dlon(gds);
	return (fabs(nx*dlon-360.0) < ERROR);
    }
    if (grid_template == 10) {
	if (output_order != wesn) return 0;		// only works with we:sn order
	lon1 = GDS_Mercator_lon1(gds);
	lon2 = GDS_Mercator_lon2(gds);
	if (lon2 < lon1) lon2 += 360.0;
	dlon = (lon2-lon1)*nx/(nx-1.0);
        return (fabs(dlon-360.0) < ERROR);
    }

    if (grid_template == 40) {

        basic_ang = GDS_Gaussian_basic_ang(gds);
        sub_ang = GDS_Gaussian_sub_ang(gds);
        units = basic_ang == 0 ?  0.000001 : (double) basic_ang / (double) sub_ang;

	/* dlon has to be defined */
        dlon = units * GDS_Gaussian_dlon(gds);
        return (fabs(nx*dlon-360.0) < ERROR);
    }


    return 0;
}
