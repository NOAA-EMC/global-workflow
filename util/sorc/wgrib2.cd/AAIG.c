#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * AAIG.c - converts lat-lon file to ArcInfo ASCII grid file
 *   each grid is written into its own file
 *
 * 7/2008 v0.9: Public Domain: Wesley Ebisuzaki
 * 10/2010 v0.99: bug fix H. Peifer
 */


extern int decode, latlon;
extern double *lat, *lon;
extern int nx, ny;
extern enum output_order_type output_order, output_order_wanted;

/*
 * HEADER:100:AAIG:output:0:writes Ascii ArcInfo Grid file, lat-lon grid only (alpha)
 */

int f_AAIG(ARG0) {

    int grid_template;
    double cellsize;
    char *save_inv_out, level[STRING_SIZE], file[STRING_SIZE], name[STRING_SIZE];
    int year0, month0, day0, hour0, minute0, second0;
    int year, month, day, hour, minute, second, i, j;
    FILE *out;

    if (mode == -1) {
        decode = latlon = 1;
        return 0;
    }
    if (mode < 0) return 0;

    if (lat == NULL || lon == NULL) {
        fprintf(stderr,"f_AAIG does nothing, no lat-lon information\n");
        return 0;
    }
    if (output_order != wesn) {
        fprintf(stderr,"f_AAIG does nothing, not in we:sn order\n");
        return 0;
    }

    grid_template = code_table_3_1(sec);
    if (grid_template != 0) {
        fprintf(stderr,"f_AAIG does nothing, not lat-lon grid\n");
        return 0;
    }
    if (nx == -1 || ny == -1) {
        fprintf(stderr,"f_AAIG does nothing, found thinned lat-lon grid\n");
        return 0;
    }
    cellsize = -1.0;
    if (nx > 1) {
	cellsize = lon[1] - lon[0];
	if ( fabs(lat[nx]-lat[0] - cellsize) > 0.0001*cellsize) {
            fprintf(stderr,"f_AAIG does nothing, dlon != dlat\n");
            return 0;
	}
    }
    else {
	cellsize = lat[nx] - lat[0];
    }

    *name = *level = 0;

    if (getExtName(sec, mode, NULL, name, NULL, NULL,".","_") != 0) {
        fatal_error("f_AAIG does nothing, no name","");
        return 0;
    }

    f_lev(call_ARG0(level,NULL));

    save_inv_out = level;
    while (*save_inv_out) {
        if (*save_inv_out == ' ') *save_inv_out = '_';
	save_inv_out++;
    }

    reftime(sec, &year0, &month0, &day0, &hour0, &minute0, &second0);

    if (verftime(sec, &year, &month, &day, &hour, &minute, &second) != 0) {
        fprintf(stderr,"f_AAIG no verf time\n");
	year = year0;
	month = month0;
	day = day0;
	hour = hour0;
    }
    if (year == year0 && month == month0 && day == day0 && hour == hour0) {
        sprintf(file,"%s.%s.%4.4d%2.2d%2.2d%2.2d.asc",name,level,year,month,day,hour);
    } 
    else {
        sprintf(file,"%s.%s.%4.4d%2.2d%2.2d%2.2d_%4.4d%2.2d%2.2d%2.2d.asc",
		name,level,year0,month0,day0,hour0,year,month,day,hour);
    }

    if ((out = ffopen(file,"w")) == NULL) {
        fprintf(stderr,"f_AAIG could not open raster file %s\n",file);
        return 0;
    }

    fprintf(stderr, "raster file: %s\n", file);

    fprintf(out,"ncols %d\n", nx);
    fprintf(out,"nrows %d\n", ny);
    fprintf(out,"xllcenter %lf\n", lon[0] > 180.0 ? lon[0]-360.0 : lon[0]);
    fprintf(out,"yllcenter %lf\n", lat[0]);
    fprintf(out,"cellsize %lf\n", cellsize);
    fprintf(out,"NODATA_VALUE 9.999e20\n");
    for (j = ny-1; j >= 0; j--) {
	for (i = 0; i < nx; i++) {
	    fprintf(out,"%f\n", data[i+j*nx]);
	}
    }
    fclose(out);
    return 0;
}
