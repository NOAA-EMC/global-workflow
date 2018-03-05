#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"
/*
 * Lola.c
 *  creates a LOngitude LAtitude grid
 *
 * 10/2006 Public Domain,  Wesley Ebisuzaki
 * 1/2008 lat and lon changed from float to double
 * 1/2011 replaced new_GDS by GDS_change_no, WNE
 */
extern int decode, flush_mode;
extern int file_append;
extern enum output_order_type output_order;

extern double *lat, *lon;
extern int latlon;

extern int GDS_change_no;

extern int use_scale, dec_scale, bin_scale, wanted_bits, max_bits;
extern enum output_grib_type grib_type;

extern int header;
extern char *text_format;
extern int text_column;

/*
 * HEADER:100:lola:output:4:lon-lat grid values X=lon0:nlon:dlon Y=lat0:nlat:dlat Z=file A=[bin|text|spread|grib]
 */

int f_lola(ARG4) {

    int nx, ny, j, k;
    int i, nxny;
    double latitude, longitude;
    double x0,dx, y0,dy;
    unsigned char *new_sec[8];
    float *tmp, t;

    struct local_struct {
        int nlat, nlon;
        double lat0, lon0, dlat, dlon;
        FILE *out;
        int *iptr;
	int last_GDS_change_no;
    };
    struct local_struct *save;
    char open_mode[4];

    /* initialization phase */

    if (mode == -1) {
        decode = latlon = 1;
        if (sscanf(arg1,"%lf:%d:%lf", &x0, &nx, &dx) != 3) {
            fatal_error("lola parsing longitudes lon0:nx:dlon  %s", arg1);
        }
        if (sscanf(arg2,"%lf:%d:%lf", &y0,&ny,&dy) != 3) {
            fatal_error("lola parsing latitudes lat0:nx:dlat  %s", arg2);
        }

        if (strcmp(arg4,"spread") != 0 
                && strcmp(arg4,"text") != 0 
                && strcmp(arg4,"grib") != 0
                && strcmp(arg4,"bin") != 0) fatal_error("lola bad write mode %s", arg4);

	if (strcmp(arg4,"grib") == 0 && (dx <= 0 || dy <= 0))
		fatal_error("lola parsing, for grib dlat > 0 and dlon > 0","");

        strcpy(open_mode, file_append ? "a" : "w");
        if (strcmp(arg4,"bin") == 0 || strcmp(arg4,"grib") == 0) strcat(open_mode,"b");

        nxny = nx*ny;
        *local = save = (struct local_struct *)malloc( sizeof(struct local_struct));
        if (save == NULL) fatal_error("lola memory allocation ","");
	if (x0 < 0.0) x0 += 360.0;
	if (x0 >= 360.0) x0 -= 360.0;
	if (x0 < 0.0 || x0 >= 360.0) fatal_error("-lola: bad initial longitude","");
	if (nx <= 0) fatal_error_i("-lola: bad nlon %d", nx);
        save->nlon = nx;
        save->lon0 = x0;
        save->dlon = dx;

	if (y0 < -90.0 || y0 > 90.0) fatal_error("-lola: bad initial latitude","");
	if (ny <= 0) fatal_error_i("-lola: bad nlat %d", ny);
        save->nlat = ny;
        save->lat0 = y0;
        save->dlat = dy;
        save->iptr = (int *) malloc(nx*ny * sizeof(int));
	if (save->iptr == NULL) fatal_error("-lola: memory allocation","");
        if ((save->out = ffopen(arg3,open_mode)) == NULL) 
              fatal_error("lola could not open file %s", arg3);
	save->last_GDS_change_no = 0;
        return 0;
    }

    save = (struct local_struct *) *local;

    /* cleanup phase */

    if (mode == -2) {
	ffclose(save->out);
	return 0;
    }

    /* processing phase */

    nx = save->nlon;
    ny = save->nlat;
    nxny = nx*ny;

    if (save->last_GDS_change_no != GDS_change_no) {
	save->last_GDS_change_no = GDS_change_no;
        // if (output_order != wesn) fatal_error("lola only works in we:sn order","");
        if (lat == NULL || lon == NULL || data == NULL) fatal_error("lola: no val","");

        /* find the nearest points for the grid */
        closest_init(sec);
#pragma omp parallel for private(i,j,k,latitude,longitude)
        for (j = 0; j < ny; j++) {
            k = j*nx;
            latitude = save->lat0 + j*save->dlat;
            for (i = 0; i < nx; i++) {
               longitude = save->lon0 + i*save->dlon;
               save->iptr[k++] = closest(sec, latitude, longitude);
            }
        }
    }

    if (strcmp(arg4,"spread") == 0) {
        k = 0;
        fprintf(save->out, "longitude, latitude, value,\n");
        for (j = 0; j < ny; j++) {
            latitude = save->lat0 + j*save->dlat;
            for (i = 0; i < nx; i++) {
                t = save->iptr[k] >= 0 ? data[save->iptr[k]] : UNDEFINED;
		if (DEFINED_VAL(t)) {
                    longitude = save->lon0 + i*save->dlon;
		    if (longitude >= 360.0) longitude -= 360.0;
                    fprintf(save->out, "%.6lf, %.6lf, %g,\n",longitude,latitude,t);
		}
		k++;
            }
        }
    }

    else if (strcmp(arg4,"bin") == 0) {
        i = nxny * sizeof(float);
        if (header) fwrite((void *) &i, sizeof(int), 1, save->out);
        for (i = 0; i < nxny; i++) {
            t = save->iptr[i] >= 0 ? data[save->iptr[i]] : UNDEFINED;
            fwrite(&t, sizeof(float), 1, save->out);
	}
        if (header) fwrite((void *) &i, sizeof(int), 1, save->out);
    }

    else if (strcmp(arg4,"text") == 0) {
        /* text output */
        if (header == 1) {
            fprintf(save->out ,"%d %d\n", nx, ny);
        }
        for (i = 0; i < nxny; i++) {
            t = save->iptr[i] >= 0 ? data[save->iptr[i]] : UNDEFINED;
            fprintf(save->out, text_format, t);
            fprintf(save->out, ((i+1) % text_column) ? " " : "\n");
        }
    }

    else if (strcmp(arg4,"grib") == 0) {
	/* make new_sec[] with new grid definition */
	for (i = 0; i < 8; i++) new_sec[i] = sec[i];
	new_sec[3] = sec3_lola(nx, save->lon0, save->dlon, ny, save->lat0, save->dlat, sec);

	/* get grid values */
	tmp = (float *) malloc(nx*ny * sizeof (float));
	if (tmp == NULL) fatal_error("-lola: memory allocation","");
        for (i = 0; i < nxny; i++) 
            tmp[i] = save->iptr[i] >= 0 ? data[save->iptr[i]] : UNDEFINED;

        grib_wrt(new_sec, tmp, nx*ny, nx, ny, use_scale, dec_scale, bin_scale, 
		wanted_bits, max_bits, grib_type, save->out);

	free(tmp);
    }

    if (flush_mode) fflush(save->out);
    return 0;
}

