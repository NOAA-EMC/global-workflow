#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * Some routines that involve Sec3 - Grid definition section
 *
 * Public Domain - Wesley Ebisuzaki
 * mod: 1/2007 M. Schwarb - cleanup
 * mod: 8/2007 Boi Vuong - bug fix - mercator variable dimension had wrong location
 */

extern int nx, ny, res, scan;
extern unsigned int npnts;
extern enum output_order_type output_order;
extern char *nl;

int n_variable_dim = 0;
int *variable_dim = NULL, *raw_variable_dim = NULL;


/*
 * HEADER:200:Sec3:inv:0:contents of section 3 (Grid Definition Section)
 */

int f_Sec3(ARG0) {

    unsigned char *p;
    if (mode >= 0) {

        p = sec[3];

        if (p[4] != 3) {
            fatal_error("Sec3 was expected and not found", "");
        }
        sprintf(inv_out,
           "Sec3 len=%u src gdef=%d npts=%d Grid Def Template=3.%d opt arg=%u" ,
            GB2_Sec3_size(sec), GB2_Sec3_gdef(sec), GB2_Sec3_npts(sec),
            code_table_3_1(sec), p[10] );
    }
    return 0;
}

/*
 * figures out nx and ny
 *   res = resolution and component flags table 3.3
 *   scan = scan mode table 3.4
 *
 * Use this code rather than .h files for nx, ny, res and scan
 */

int get_nxny(unsigned char **sec, int *nx, int *ny, unsigned int *npnts, int *res, int *scan) {
    int grid_template, n_var_dim, i, j, n_octets;
    unsigned int npoints, n;
    unsigned char *gds, *p;

    grid_template = code_table_3_1(sec);
    *res = flag_table_3_3(sec);
    *scan = flag_table_3_4(sec);

    gds = sec[3];

    switch (grid_template) {
        case 0:
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 10:
        case 12:
        case 20:
        case 30:
        case 31:
        case 40:
        case 41:
        case 42:
        case 43:
        case 44:
        case 90:
        case 110:
        case 140:
        case 204:
                *nx = uint4_missing(gds+30); *ny = uint4_missing(gds+34); break;
        case 51:
        case 52:
        case 53:
        case 130:
        case 50: *nx = GB2_Sec3_npts(sec); *ny = 1; break;        // should calculate for from parameters

        case 120: *nx = uint4_missing(gds+14);			// nx = bin along radials, ny = num radials
 		  *ny = uint4_missing(gds+18);
		  break;
	case 32768:
	case 32769:
	    if (GB2_Center(sec) == NCEP) {
		*nx = uint4_missing(gds+30);
		*ny = uint4_missing(gds+34);
		break;
	    }
            *nx = *ny = -1; break;
        default:
                *nx = *ny = -1; break;
    }

    n_var_dim = 0;
    if (*nx == -1) n_var_dim = *ny;
    if (*ny == -1) n_var_dim = *nx;
    if (*nx == -1 && *ny == -1) n_var_dim = 0;

    p = NULL;
    if (n_var_dim) {
        switch (grid_template) {
           case 0: p = gds + 72; break;
           case 1: p = gds + 84; break;
           case 2: p = gds + 84; break;
           case 3: p = gds + 96; break;
           case 10: p = gds + 72; break;
           case 40: p = gds + 72; break;
           case 41: p = gds + 84; break;
           case 42: p = gds + 84; break;
           case 43: p = gds + 96; break;
	   case 32768: if (GB2_Center(sec) == NCEP) p = gds + 72;
			else p = NULL;
			break;
	   case 32769: if (GB2_Center(sec) == NCEP) p = gds + 80;
			else p = NULL;
			break;
           default: p = NULL; break;
        }
    }

    /* calculate number of grid points, check with GDS */
    npoints = 0;
    if (n_var_dim) {

        if (n_variable_dim != n_var_dim) {
            if (variable_dim) free(variable_dim);
            if (raw_variable_dim) free(raw_variable_dim);
fprintf(stderr,">>> malloc variable_dim\n");
            variable_dim = (int *) malloc(n_var_dim * sizeof(int));
            raw_variable_dim = (int *) malloc(n_var_dim * sizeof(int));

            if (variable_dim == NULL || raw_variable_dim == NULL) 
		fatal_error("ran out of memory","");
            n_variable_dim = n_var_dim;
        }
        n_octets = (int) gds[10];        /* number of octets per integer */
        for (i = 0; i < n_var_dim; i++) {
            for (n = j = 0; j < n_octets; j++) {
                n = (n << 8) + (int) *p++;
            }
            raw_variable_dim[i] = variable_dim[i] = (int) n;
            npoints += n;
        }

        /* convert variable_dim to SN order if needed */
        if (*nx == -1 && GDS_Scan_y(*scan) == 0 && output_order == wesn) {
            for (i = 0; i < *ny; i++) {
                variable_dim[i] = raw_variable_dim[*ny-1-i];
            }
        }
        /* convert variable_dim to NS order if needed */
        else if (*nx == -1 && GDS_Scan_y(*scan) != 0 && output_order == wens) {
            for (i = 0; i < *ny; i++) {
                variable_dim[i] = raw_variable_dim[*ny-1-i];
            }
        }
    }
    else if (*nx > 0 && *ny > 0) npoints = (unsigned) *nx * *ny;
    *npnts = GB2_Sec3_npts(sec);

//    if ((*nx != -1 || *ny != -1) && GB2_Sec3_npts(sec) != npoints) {
    if ((*nx != -1 || *ny != -1) && GB2_Sec3_npts(sec) != npoints && GDS_Scan_staggered_storage(*scan) == 0) {
        fprintf(stderr,"two values for number of points %u (GDS) %u (calculated)\n",
                      GB2_Sec3_npts(sec), npoints);
    }

/*
    for (i = 0; i < n_var_dim; i++) {
      printf("%d ", variable_dim[i]);
    }
*/

    return 0;
}

/*
 * HEADER:200:nxny:inv:0:nx and ny of grid
 */
int f_nxny(ARG0) {
    if (mode >= 0) sprintf(inv_out,"(%d x %d)",nx,ny);
    return 0;
}


/*
 * HEADER:200:scan:inv:0:scan order of grid
 */
const char *scan_order[] = {
        "WE:NS", "WE|EW:NS",
        "NS:WE", "NS(W|E)",

        "WE:SN", "WE|EW:SN",
        "SN:WE", "SN(W|E)",

        "EW:NS", "EW|WE:NS",
        "NS:EW", "NS(E|W)",

        "EW:SN", "EW|WE:SN",
        "SN:EW", "SN(E|W)",
 };

int f_scan(ARG0) {
    if (mode >= 0) {
        if (scan == -1) sprintf(inv_out,"scan=? ????");
        else sprintf(inv_out,"scan=%d input=%s output=%s",scan >>4, scan_order[scan >> 4], output_order_name());
    }
    return 0;
}

/*
 * HEADER:200:nlons:inv:0:number of longitudes for each latitude
 */

int f_nlons(ARG0) {

    int j;

    if (mode < 0) return 0;

    sprintf(inv_out,"nlon (S/N)=");
    inv_out += strlen(inv_out);

    if (n_variable_dim == 0) {
        for (j = 0; j < ny; j++) {
            sprintf(inv_out,"%d ", nx);
            inv_out += strlen(inv_out);
        }
    }
    else {
        for (j = 0; j < ny; j++) {
            sprintf(inv_out, "%d ",variable_dim[j]);
            inv_out += strlen(inv_out);
        }
    }
    if (ny) inv_out[-1] = 0;
    return 0;
}


/*
 * HEADER:200:grid:inv:0:grid definition
 */

/*
 * some really needs to add some meat to this routine.
 */

int f_grid(ARG0) {
    int grid_template;
    int basic_ang, sub_ang;
    double units, tmp;
    double lat1, lon1, lat2, lon2, dlat, dlon;
    unsigned char *gds, *p;
    int i, j, val, n, noct, offset, flag_3_3, no_dx, no_dy;

    if (mode >= 0) {

        if (GB2_Sec3_gdef(sec) != 0) {
            sprintf(inv_out,"no grid template");
            return 0;
        }

        gds = sec[3];
        grid_template = code_table_3_1(sec);
	no_dx = no_dy = 0;

	if (grid_template != 20 && grid_template != 30 &&
		grid_template != 31) {
	    flag_3_3 = flag_table_3_3(sec);
	    if (flag_3_3 != -1) {
	        if ((flag_3_3 & 0x20) == 0) no_dx = 1;
	        if ((flag_3_3 & 0x10) == 0) no_dy = 1;
	    }
	}

        sprintf(inv_out,"grid_template=%d:", grid_template);
        inv_out += strlen(inv_out);

        if (res >= 0) {
            sprintf(inv_out, res & 8 ? "winds(grid):" : "winds(N/S):");
            inv_out += strlen(inv_out);
	}

        switch(grid_template) {
            case 0:
            case 1:
            case 2:
            case 3:
                sprintf(inv_out,"%s", nl);
                inv_out += strlen(inv_out);
                if (nx == -1 || ny == -1) {
                    i = code_table_3_11(sec);
                    if (i == 1) sprintf(inv_out,"thinned global ");
                    else if (i == 2) sprintf(inv_out,"thinned regional ");
                    else fatal_error_i("code table 3.11 =%d is not right", i);
                    inv_out += strlen(inv_out);
                }
                if (grid_template == 0) sprintf(inv_out,"lat-lon grid:");
                if (grid_template == 1) sprintf(inv_out,"rotated lat-lon grid:");
                if (grid_template == 2) sprintf(inv_out,"stretched lat-lon grid:");
                if (grid_template == 3) sprintf(inv_out,"stretched/rotated lat-lon grid:");
                inv_out += strlen(inv_out);

                basic_ang = GDS_LatLon_basic_ang(gds);
                sub_ang = GDS_LatLon_sub_ang(gds);
                units = basic_ang == 0 ?  0.000001 : (float) basic_ang / (float) sub_ang;

                sprintf(inv_out,"(%d x %d)",nx,ny);
                inv_out += strlen(inv_out);

                sprintf(inv_out," units %g input %s output %s res %d%s", units, scan_order[scan>>4],output_order_name(), res,nl);
                inv_out += strlen(inv_out);

                lat1 = units * GDS_LatLon_lat1(gds);
                lon1 = units * GDS_LatLon_lon1(gds);
                lat2 = units * GDS_LatLon_lat2(gds);
                lon2 = units * GDS_LatLon_lon2(gds);

                if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0)
                    fprintf(stderr,"BAD GDS:lon1=%lf lon2=%lf should be 0..360\n",lon1,lon2);

                dlon = units * GDS_LatLon_dlon(gds);
                dlat = units * GDS_LatLon_dlat(gds);
		if (no_dx) dlon = 0.0;
		if (no_dy) dlat = 0.0;

//vsm_frm                if (ny == -1) sprintf(inv_out, "lat %g to %g with variable spacing%s", lat1, lat2, nl);
                if (ny == -1) sprintf(inv_out, "lat %lf to %lf with variable spacing%s", lat1, lat2, nl);
                else sprintf(inv_out, "lat %lf to %lf by %lf%s", lat1, lat2, dlat, nl);
                inv_out += strlen(inv_out);
//vsm_frm                if (nx == -1) sprintf(inv_out, "lon %g to %g with variable spacing", lon1, lon2);
//vsm_frm                else sprintf(inv_out, "lon %g to %g by %g", lon1, lon2, dlon);
                if (nx == -1) sprintf(inv_out, "lon %lf to %lf with variable spacing", lon1, lon2);
                else sprintf(inv_out, "lon %lf to %lf by %lf", lon1, lon2, dlon);
                inv_out += strlen(inv_out);
                sprintf(inv_out, " #points=%u", npnts);
                inv_out += strlen(inv_out);

                if (grid_template == 1) {
                    sprintf(inv_out, "%ssouth pole lat=%lf lon=%lf angle of rot=%lf",
                        nl,units*int4(gds+72), units*int4(gds+76),units*int4(gds+80));
                }
                if (grid_template == 2) {
                    sprintf(inv_out, "%sstretch lat=%lf lon=%lf factor=%lf",
                        nl,units*int4(gds+72), units*int4(gds+76),int4(gds+80)*1e-6);
                }
                if (grid_template == 3) {
                    sprintf(inv_out, "%ssouth pole lat=%lf lon=%lf angle of rot=%lf",
                        nl,units*int4(gds+72), units*int4(gds+76),units*int4(gds+80));
//vsm +1 line:
                    inv_out += strlen(inv_out);                        
                    sprintf(inv_out, ", stretch lat=%lf lon=%lf factor=%lf",
                        units*int4(gds+84), units*int4(gds+88), int4(gds+92)*1e-6);
                }
                inv_out += strlen(inv_out);

                if (mode > 0) {
                    sprintf(inv_out,"%sbasic_ang=%d sub_angle=%d units=%lf",nl,basic_ang,sub_ang,units);
                    inv_out += strlen(inv_out);
                    sprintf(inv_out,"%sunscaled lat=%d to %d lon=%u to %u",nl, GDS_Gaussian_lat1(gds),
                        GDS_Gaussian_lat2(gds), GDS_Gaussian_lon1(gds), GDS_Gaussian_lon2(gds));
                    inv_out += strlen(inv_out);
                }
                // print out the variable grid spacing
                if (nx == -1) sprintf(inv_out,"%s#grid points by latitude:",nl);
                if (ny == -1) sprintf(inv_out,"%s#grid points by longitude:",nl);
                inv_out += strlen(inv_out);
                if (nx == -1 || ny == -1) {
                    offset = 0;
                    switch(grid_template) {
                        case 0: offset = 72; break;
                        case 1: offset = 84; break;
                        case 2: offset = 84; break;
                        case 3: offset = 96; break;
                        default: fatal_error_i("Fix code to handle offset grid template %d", grid_template);
                    }

                    noct = sec[3][10];
                    n = nx > ny ? nx : ny;
                    for (i = 0; i < n; i++) {
                        p = &(sec[3][offset+i*noct]);
                        val = 0;
                        for (j = 0; j < noct; j++) {
                            val = 256*val + *p++;
                        }
                        if ((i + 7) % 20 == 0) {
                            sprintf(inv_out,"%s",nl);
                            inv_out += strlen(inv_out);
                        }
                        sprintf(inv_out," %d",val);
                        inv_out += strlen(inv_out);
                    }
                }
                inv_out += strlen(inv_out);
                if (GDS_Scan_staggered(scan)) sprintf(inv_out,"%sstagger %d offset(even_x:%s odd_x:%s y:%s) storage %s",
                   nl, scan & 15,
                   scan & 8 ? "dx/2" : "0" , scan & 4 ? "dx/2" : "0",
                   scan & 2 ? "dy/2" : "0" , scan & 1 ? "trim" : "nxny");
                break;
            case 4:
            case 5:
                sprintf(inv_out,"%sVaraible Resolution %sLatitude/Longitude grid: (%d x %d) input %s output %s res %d%s",
                         nl, grid_template == 5 ? "Rotated " : "", nx, ny, scan_order[scan>>4], output_order_name(), res, nl);

                basic_ang = GDS_LatLon_basic_ang(gds);
                sub_ang = GDS_LatLon_sub_ang(gds);
                units = basic_ang == 0 ?  0.000001 : (float) basic_ang / (float) sub_ang;

                // don't print too many coordinates
                j = (npnts > 300 ? 300 : npnts);

                sprintf(inv_out,"%slat=", grid_template == 4 ? "" : "rotated ");
                inv_out += strlen(inv_out);
	        offset = grid_template == 4 ? 48 + 4*npnts : 60 + 4*npnts;
                for (i = 0; i < j; i++) {
                    sprintf(inv_out,"%lf ",units*int4(gds+30+i*8));
                    inv_out += strlen(inv_out);
                }
                if (j < npnts) {
                    sprintf(inv_out,"... (list truncated)");
                    inv_out += strlen(inv_out);
		}

                sprintf(inv_out,"%slon=", grid_template == 4 ? "" : "rotated ");
	        offset = grid_template == 4 ? 48 : 60;
                for (i = 0; i < j; i++) {
                    sprintf(inv_out,"%lf ",units*int4(gds+30+i*8));
                    inv_out += strlen(inv_out);
                }
                if (j < npnts) {
                    sprintf(inv_out,"... (list truncated)");
                    inv_out += strlen(inv_out);
		}
                break;

            case 10: sprintf(inv_out,"%sMercator grid: (%d x %d) LatD %lf input %s output %s res %d%s",nl,
                        nx, ny, GDS_Mercator_latD(gds), scan_order[scan>>4],output_order_name(),res,nl);
                inv_out += strlen(inv_out);
                lon1 = GDS_Mercator_lon1(gds);
                lon2 = GDS_Mercator_lon2(gds);
		dlat = GDS_Mercator_dy(gds);
		dlon = GDS_Mercator_dx(gds);
		if (no_dx) dlon = 0.0;
		if (no_dy) dlat = 0.0;

                sprintf(inv_out, "lat %lf to %lf by %lf m%slon %lf to %lf by %lf m%sorientation %lf",
                        GDS_Mercator_lat1(gds), GDS_Mercator_lat2(gds), dlat, nl,
                        lon1, lon2, dlon, nl, GDS_Mercator_ori_angle(gds));

                if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0)
                      fprintf(stderr,"BAD GDS:lon1=%lf lon2=%lf should be 0..360\n",lon1,lon2);
                inv_out += strlen(inv_out);
                if (GDS_Scan_staggered(scan)) sprintf(inv_out,"%sstagger %d offset(even_x:%s odd_x:%s y:%s) storage %s",
                   nl, scan & 15,
                   scan & 8 ? "dx/2" : "0" , scan & 4 ? "dx/2" : "0",
                   scan & 2 ? "dy/2" : "0" , scan & 1 ? "trim" : "nxny");
                break;
	    case 12: sprintf(inv_out,"%sTransverse Mercator grid:%s", nl, nl);
                break;
            case 20: sprintf(inv_out,"%spolar stereographic grid: (%d x %d) input %s output %s res %d%s",nl,
                        nx, ny, scan_order[scan>>4],output_order_name(), res,nl);
                inv_out += strlen(inv_out);
                sprintf(inv_out,"%s pole ", flag_table_3_5(sec) & 128 ? "South" : "North");
                inv_out += strlen(inv_out);
                dlon = GDS_Polar_dx(gds);
		dlat = GDS_Polar_dy(gds);
		if (no_dx) dlon = 0.0;
		if (no_dy) dlat = 0.0;

                sprintf(inv_out,"lat1 %lf lon1 %lf latD %lf lonV %lf dx %lf m dy %lf m",
                    GDS_Polar_lat1(gds), GDS_Polar_lon1(gds),
                    GDS_Polar_lad(gds), GDS_Polar_lov(gds),
                    dlon, dlat);
                inv_out += strlen(inv_out);

                break;
            case 30:
                sprintf(inv_out,"%sLambert Conformal: (%d x %d) input %s output %s res %d%s",nl,
                        nx, ny, scan_order[scan>>4],output_order_name(), res,nl);
                inv_out += strlen(inv_out);
                dlon = GDS_Lambert_dx(gds);
		dlat = GDS_Lambert_dy(gds);
		if (no_dx) dlon = 0.0;
		if (no_dy) dlat = 0.0;

                sprintf(inv_out,"Lat1 %lf Lon1 %lf LoV %lf%sLatD %lf "
                    "Latin1 %lf Latin2 %lf%sLatSP %lf LonSP %lf%s"
                    "%s (%d x %d) Dx %lf m Dy %lf m mode %d",
                    GDS_Lambert_La1(gds), GDS_Lambert_Lo1(gds),
                    GDS_Lambert_Lov(gds), nl, GDS_Lambert_LatD(gds),
                    GDS_Lambert_Latin1(gds), GDS_Lambert_Latin2(gds), nl,
                    GDS_Lambert_LatSP(gds), GDS_Lambert_LonSP(gds), nl,
                    GDS_Lambert_NP(gds) ? "North Pole": "South Pole",
                    nx, ny, dlon, dlat, res);
                inv_out += strlen(inv_out);
                if (GDS_Scan_staggered(scan)) sprintf(inv_out,"%sstagger %d offset(even_x:%s odd_x:%s y:%s) storage %s",
                   nl, scan & 15,
                   scan & 8 ? "dx/2" : "0" , scan & 4 ? "dx/2" : "0",
                   scan & 2 ? "dy/2" : "0" , scan & 1 ? "trim" : "nxny");

                break;

            case 31:
                sprintf(inv_out,"%sAlbers equal area projection: (%d x %d) input %s output %s res %d%s",nl,
                        nx, ny, scan_order[scan>>4],output_order_name(), res,nl);
                inv_out += strlen(inv_out);
                dlon = GDS_Lambert_dx(gds);
                dlat = GDS_Lambert_dy(gds);
                if (no_dx) dlon = 0.0;
                if (no_dy) dlat = 0.0;

                sprintf(inv_out,"Lat1 %lf Lon1 %lf LoV %lf%sLatD %lf "
                    "Latin1 %lf Latin2 %lf%sLatSP %lf LonSP %lf%s"
                    "%s (%d x %d) Dx %lf m Dy %lf m mode %d",
                    GDS_Lambert_La1(gds), GDS_Lambert_Lo1(gds),
                    GDS_Lambert_Lov(gds), nl, GDS_Lambert_LatD(gds),
                    GDS_Lambert_Latin1(gds), GDS_Lambert_Latin2(gds), nl,
                    GDS_Lambert_LatSP(gds), GDS_Lambert_LonSP(gds), nl,
                    GDS_Lambert_NP(gds) ? "North Pole": "South Pole",
                    nx, ny, dlon, dlat, res);
                inv_out += strlen(inv_out);
                if (GDS_Scan_staggered(scan)) sprintf(inv_out,"%sstagger %d offset(even_x:%s odd_x:%s y:%s) storage %s",
                   nl, scan & 15,
                   scan & 8 ? "dx/2" : "0" , scan & 4 ? "dx/2" : "0",
                   scan & 2 ? "dy/2" : "0" , scan & 1 ? "trim" : "nxny");
                break;
            case 40: 
            case 41:
            case 42:
            case 43:
                basic_ang = GDS_Gaussian_basic_ang(gds);
                sub_ang = GDS_Gaussian_sub_ang(gds);
                units = basic_ang == 0 ?  0.000001 : (float) basic_ang / (float) sub_ang;

                lat1 = units * GDS_Gaussian_lat1(gds);
                lon1 = units * GDS_Gaussian_lon1(gds);
                lat2 = units * GDS_Gaussian_lat2(gds);
                lon2 = units * GDS_Gaussian_lon2(gds);

                if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0)
                    fprintf(stderr,"BAD GDS:lon1=%lf lon2=%lf should be 0..360\n",lon1,lon2);

                sprintf(inv_out,"%s",nl);
                inv_out += strlen(inv_out);
                if (nx == -1 || ny == -1) {
                    i = code_table_3_11(sec);
                    if (i == 1) sprintf(inv_out,"thinned global ");
                    else if (i == 2) sprintf(inv_out,"thinned regional ");
                    else fatal_error_i("code table 3.11 =%d is not right", i);
                    inv_out += strlen(inv_out);
                }

                if (grid_template == 40) sprintf(inv_out,"Gaussian grid:");
                if (grid_template == 41) sprintf(inv_out,"Rotated Gaussian grid:");
                if (grid_template == 42) sprintf(inv_out,"Stretched Gaussian grid:");
                if (grid_template == 43) sprintf(inv_out,"%sStretched-Rotated Gaussian grid:",nl);
                inv_out += strlen(inv_out);

                sprintf(inv_out," (%d x %d) units %g input %s output %s%s", nx, ny, units, scan_order[scan>>4],output_order_name(), nl);
                inv_out += strlen(inv_out);

                sprintf(inv_out,"number of latitudes between pole-equator=%u #points=%u%s",
                    GDS_Gaussian_nlat(gds),npnts,nl);
                inv_out += strlen(inv_out);

                sprintf(inv_out, "lat %lf to %lf%slon %lf to %lf ",
                     lat1, lat2,nl,lon1, lon2);
                inv_out += strlen(inv_out);
		if (ny > 0 && grid_template == 40) sprintf(inv_out,"by %lf", GDS_Gaussian_dlon(gds)*units );
                inv_out += strlen(inv_out);

                if (grid_template == 41) {
                    sprintf(inv_out, "%ssouth pole lat=%lf lon=%lf angle of rot=%lf",
                        nl,units*int4(gds+72),units*int4(gds+76),units*int4(gds+80));
                }
                if (grid_template == 42) {
                    sprintf(inv_out, "%sstretch lat=%lf lon=%lf factor=%lf",
                        nl,units*int4(gds+72), units*int4(gds+76),int4(gds+80)*1e-6);
                }
                if (grid_template == 43) {
                      sprintf(inv_out, "%ssouth pole lat=%lf lon=%lf angle of rot=%lf",
                        nl,units*int4(gds+72),units*int4(gds+76),units*int4(gds+80));
                      sprintf(inv_out, ", stretch lat=%lf lon=%lf factor=%lf",
                        units*int4(gds+84), units*int4(gds+88),int4(gds+92)*1e-6);
                }
                inv_out += strlen(inv_out);

                if (mode > 0) {
                    sprintf(inv_out,"%sbasic_ang=%d sub_angle=%d units=%lf",nl,basic_ang,sub_ang,units);
                    inv_out += strlen(inv_out);
                    sprintf(inv_out,"%sunscaled lat=%d to %d lon=%u to %u",nl, GDS_Gaussian_lat1(gds),
                        GDS_Gaussian_lat2(gds), GDS_Gaussian_lon1(gds), GDS_Gaussian_lon2(gds));
                    inv_out += strlen(inv_out);
                }
                // print out the variable grid spacing
                if (nx == -1) sprintf(inv_out,"%s#grid points by latitude:",nl);
                if (ny == -1) sprintf(inv_out,"%s#grid points by longitude:",nl);
                inv_out += strlen(inv_out);
                if (nx == -1 || ny == -1) {
                    offset = 0;
                    switch(grid_template) {
                        case 40: offset = 72; break;
                        case 41: offset = 84; break;
                        case 42: offset = 84; break;
                        case 43: offset = 96; break;
                        default: fatal_error_i("f_grid code needs to be updated offset for grid template %d",
                            grid_template);
                    }
                    noct = sec[3][10];
                    n = nx > ny ? nx : ny;
                    for (i = 0; i < n; i++) {
                        p = &(sec[3][offset+i*noct]);
                        val = 0;
                        for (j = 0; j < noct; j++) {
                                val = 256*val + *p++;
                        }
                        if ((i + 7) % 20 == 0) {
                            sprintf(inv_out,"%s",nl);
                            inv_out += strlen(inv_out);
                        }
                        sprintf(inv_out," %d",val);
                        inv_out += strlen(inv_out);
                    }
                }
                break;

            case 50: sprintf(inv_out,"Spherical harmonic j=%d k=%d l=%d, code_table_3.6=%d code_table_3.7=%d",
                      GDS_Harmonic_j(gds), GDS_Harmonic_k(gds), GDS_Harmonic_m(gds),
                      GDS_Harmonic_code_3_6(gds), GDS_Harmonic_code_3_7(gds));
                  break;
            case 51: sprintf(inv_out,"Rotated Spherical harmonic j=%d k=%d l=%d, code_table_3.6=%d code_table_3.7=%d%s",
                      GDS_Harmonic_j(gds), GDS_Harmonic_k(gds), GDS_Harmonic_m(gds),
                      GDS_Harmonic_code_3_6(gds), GDS_Harmonic_code_3_7(gds),nl);
                  inv_out += strlen(inv_out);
                  sprintf(inv_out,"South pole of proj lat=%lf lon=%lf rotation angle=%lf",
                      ieee2flt(gds+28), ieee2flt(gds+32),ieee2flt(gds+36));
                  break;
            case 52: sprintf(inv_out,"Stretched Spherical harmonic j=%d k=%d l=%d, code_table_3.6=%d code_table_3.7=%d%s",
                      GDS_Harmonic_j(gds), GDS_Harmonic_k(gds), GDS_Harmonic_m(gds),
                      GDS_Harmonic_code_3_6(gds), GDS_Harmonic_code_3_7(gds),nl);
                  inv_out += strlen(inv_out);
                  sprintf(inv_out,"pole of stretching lat=%lf lon=%lf stretching=%lf",
                        ieee2flt(gds+28), ieee2flt(gds+32),ieee2flt(gds+36));
                  break;
            case 53: sprintf(inv_out,"Rotated=Stretched Spherical harmonic j=%d k=%d l=%d, code_table_3.6=%d code_table_3.7=%d%s",
                      GDS_Harmonic_j(gds), GDS_Harmonic_k(gds), GDS_Harmonic_m(gds),
                      GDS_Harmonic_code_3_6(gds), GDS_Harmonic_code_3_7(gds),nl);
                  inv_out += strlen(inv_out);
                  sprintf(inv_out,"South pole of proj lat=%lf lon=%lf rotation angle=%lf%s",
                      ieee2flt(gds+28), ieee2flt(gds+32),ieee2flt(gds+36),nl);
                  inv_out += strlen(inv_out);
                  sprintf(inv_out,"pole of stretching lat=%lf lon=%lf stretching=%lf",
                      ieee2flt(gds+40), ieee2flt(gds+44),ieee2flt(gds+48));
                  break;

            case 90: 
		sprintf(inv_out,"%sSpace view perspective or orographic grid (%d x %d)",nl,nx,ny);
                inv_out += strlen(inv_out);
\
     		basic_ang = GDS_LatLon_basic_ang(gds);
                sub_ang = GDS_LatLon_sub_ang(gds);
                units = basic_ang == 0 ?  0.000001 : (float) basic_ang / (float) sub_ang;

                sprintf(inv_out," units %g input %s output %s res %d%s", units, 
			scan_order[scan>>4],output_order_name(), res,nl);
                inv_out += strlen(inv_out);

		sprintf(inv_out,"sub-sat point: lat %lf lon %lf ix=%lf iy=%lf%s", 
		GDS_Space_lap(gds), GDS_Space_lop(gds), GDS_Space_xp(gds), GDS_Space_yp(gds), nl);
                inv_out += strlen(inv_out);

		sprintf(inv_out,"diameter of earth dx=%d dy=%d grid cells ori_angle %lf%s", 
		GDS_Space_dx(gds),GDS_Space_dy(gds),GDS_Space_ori(gds),nl);
                inv_out += strlen(inv_out);
		tmp = GDS_Space_altitude(gds);
		if (tmp > 0) 
		   sprintf(inv_out,"sat. altitude=%lf (equatorial radii) grid_origin Xo=%d Yo=%d", 
			tmp,GDS_Space_x0(gds), GDS_Space_y0(gds));
		else
		    sprintf(inv_out,"sat. altitude=infinity (equatorial radii) grid_origin Xo=%d Yo=%d", 
			GDS_Space_x0(gds), GDS_Space_y0(gds));

                  break;
            case 100: sprintf(inv_out,"Triangular grid based on icosahedron");
                  break;
            case 110: sprintf(inv_out,"Equatorial azimuthal equidistant projection");
                  break;
            case 120:
 		  sprintf(inv_out,"%sAzimuth-range projection: (%u bins x %u radials)%scenter Lat1 %lf Lon1 %lf ",
                        nl, nx, ny, nl, GDS_AzRan_lat1(gds), GDS_AzRan_lon1(gds));
                  inv_out += strlen(inv_out);
                  sprintf(inv_out,"%sDx %.3lf m (bin spacing along radial)  Dstart %.3lf m", 
			nl, GDS_AzRan_dx(gds), GDS_AzRan_dstart(gds));
                  inv_out += strlen(inv_out);
		  for (i = 0; i < ny; i++) {
		      sprintf(inv_out,"%sradial %d: direction %.1lf degrees, width %.2lf degrees ",
	   		nl, i+1, int2(gds+39+i*4)*0.1, int2(gds+39+i*4)*0.01);
                      inv_out += strlen(inv_out);
                  }
                  break;
            case 130:
                  sprintf(inv_out,"winds(N/S):%sIrregular Grid:(%d x %d) units 1e-06 input raw output raw res N/A%s",nl,nx,ny,nl);
                  inv_out += strlen(inv_out);

		  // don't print too many coordinates
		  j = (nx > 300 ? 300 : nx);
		  sprintf(inv_out,"lat=");
                  inv_out += strlen(inv_out);
		  for (i = 0; i < j; i++) {
		      sprintf(inv_out,"%lf ",1e-6*int4(gds+30+i*8));
		      inv_out += strlen(inv_out);
		  }
	          if (j < nx) {
		      sprintf(inv_out,"... (list truncated)");
		      inv_out += strlen(inv_out);
		  }

		  sprintf(inv_out,"%slon=",nl);
                  inv_out += strlen(inv_out);
		  for (i = 0; i < j; i++) {
		      sprintf(inv_out,"%lf ",1e-6*uint4(gds+34+i*8));
		      inv_out += strlen(inv_out);
		  }
	          if (j < nx) {
		      sprintf(inv_out,"... (list truncated)");
		      inv_out += strlen(inv_out);
		  }
                  break;
            case 204:
                  sprintf(inv_out,"Curvilinear Orthogonal grid: see lat lon fields in this grib file");
                  break;
            case 1000: sprintf(inv_out,"Cross-section, points equal spaced on horizontal%s",nl);
		  inv_out += strlen(inv_out);
                  basic_ang = GDS_CrossSec_basic_ang(gds);
                  sub_ang = GDS_CrossSec_sub_ang(gds);
                  units = basic_ang == 0 ?  0.000001 : (float) basic_ang / (float) sub_ang;
                  lat1 = units * GDS_CrossSec_lat1(gds);
                  lon1 = units * GDS_CrossSec_lon1(gds);
                  lat2 = units * GDS_CrossSec_lat2(gds);
                  lon2 = units * GDS_CrossSec_lon2(gds);
		  sprintf(inv_out, "lon %g to %g%s", lon1, lon2,nl);
		  inv_out += strlen(inv_out);
		  sprintf(inv_out, "lat %g to %g", lat1, lat2);
		  inv_out += strlen(inv_out);

                  break;
            case 1100: sprintf(inv_out,"Hovmoller diagram, equal spaced on horizontal");
                  break;
            case 1200: sprintf(inv_out,"Time section grid");
                  break;
	    case 32768:
		  if (GB2_Center(sec) == NCEP) {
			/* for a rotated grid, where is the rotation angle? */
		      sprintf(inv_out,"%sRotated Lat/Lon (Arakawa Staggered E-grid)",nl);
		      inv_out += strlen(inv_out);

                      basic_ang = GDS_LatLon_basic_ang(gds);
                      sub_ang = GDS_LatLon_sub_ang(gds);
                      units = basic_ang == 0 ?  0.000001 : (float) basic_ang / (float) sub_ang;

                      sprintf(inv_out,"(%d x %d)",nx,ny);
                      inv_out += strlen(inv_out);

                      sprintf(inv_out," units %g input %s output %s res %d%s", units, scan_order[scan>>4],
				output_order_name(), res,nl);
                      inv_out += strlen(inv_out);

                      lat1 = units * GDS_LatLon_lat1(gds);
                      lon1 = units * GDS_LatLon_lon1(gds);
                      lat2 = units * GDS_LatLon_lat2(gds);
                      lon2 = units * GDS_LatLon_lon2(gds);

                      if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0)
                          fprintf(stderr,"BAD GDS:lon1=%lf lon2=%lf should be 0..360\n",lon1,lon2);

                      dlon = units * GDS_LatLon_dlon(gds);
                      dlat = units * GDS_LatLon_dlat(gds);
                      if (no_dx) dlon = 0.0;
                      if (no_dy) dlat = 0.0;

                      if (ny == -1) sprintf(inv_out, "lat0 %lf lat-center %lf with variable spacing%s", lat1, lat2, nl);
                      else sprintf(inv_out, "lat0 %lf lat-center %lf dlat %lf%s", lat1, lat2, dlat, nl);
                      inv_out += strlen(inv_out);

                      if (nx == -1) sprintf(inv_out, "lon0 %lf lon-center %lf with variable spacing", lon1, lon2);
                      else sprintf(inv_out, "lon0 %lf lon-center %lf dlon %lf", lon1, lon2, dlon);
                      inv_out += strlen(inv_out);
                      sprintf(inv_out, " #points=%u", npnts);
                      inv_out += strlen(inv_out);

                      // print out the variable grid spacing
                      if (nx == -1) sprintf(inv_out,"%s#grid points by latitude:",nl);
                      if (ny == -1) sprintf(inv_out,"%s#grid points by longitude:",nl);
                      inv_out += strlen(inv_out);
                      if (nx == -1 || ny == -1) {
		          offset = 72;
                          noct = sec[3][10];
                          n = nx > ny ? nx : ny;
                          for (i = 0; i < n; i++) {
                              p = &(sec[3][offset+i*noct]);
                              val = 0;
                              for (j = 0; j < noct; j++) {
                                   val = 256*val + *p++;
                              }
                              if ((i + 7) % 20 == 0) {
                                  sprintf(inv_out,"%s",nl);
                                  inv_out += strlen(inv_out);
                              }
                              sprintf(inv_out," %d",val);
                              inv_out += strlen(inv_out);
                          }
		      }
	          }
		  else {
            	      sprintf(inv_out,"no other grid info");
		  }
                  break;
	    case 32769:
		  if (GB2_Center(sec) == NCEP) {
		      sprintf(inv_out,"%sI am not an Arakawa E-grid. %sI am rotated but have no rotation angle.%s",nl,nl,nl);
                      inv_out += strlen(inv_out);
		      sprintf(inv_out,"I am staggered. What am I?%s",nl);
                      inv_out += strlen(inv_out);

                      basic_ang = GDS_LatLon_basic_ang(gds);
                      sub_ang = GDS_LatLon_sub_ang(gds);
                      units = basic_ang == 0 ?  0.000001 : (float) basic_ang / (float) sub_ang;

                      sprintf(inv_out,"(%d x %d)",nx,ny);
                      inv_out += strlen(inv_out);

                      sprintf(inv_out," units %g input %s output %s res %d%s", units, scan_order[scan>>4],
                                output_order_name(), res,nl);
                      inv_out += strlen(inv_out);

                      lat1 = units * GDS_LatLon_lat1(gds);
                      lon1 = units * GDS_LatLon_lon1(gds);
                      lat2 = units * GDS_LatLon_lat2(gds);
                      lon2 = units * GDS_LatLon_lon2(gds);

                      if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0)
                          fprintf(stderr,"BAD GDS:lon1=%lf lon2=%lf should be 0..360\n",lon1,lon2);

                      if (lon1 < 0.0 || lon2 < 0.0 || lon1 > 360.0 || lon2 > 360.0)
                          fprintf(stderr,"BAD GDS:lon1=%lf lon2=%lf should be 0..360\n",lon1,lon2);

                      dlon = units * GDS_LatLon_dlon(gds);
                      dlat = units * GDS_LatLon_dlat(gds);
                      if (no_dx) dlon = 0.0;
                      if (no_dy) dlat = 0.0;

                      if (ny == -1) sprintf(inv_out, "lat0 %lf lat-center %lf with variable spacing%s", lat1, lat2, nl);
                      else sprintf(inv_out, "lat0 %lf lat-center %lf dlat %lf%s", lat1, lat2, dlat, nl);
                      inv_out += strlen(inv_out);

                      if (nx == -1) sprintf(inv_out, "lon0 %lf lon-center %lf with variable spacing", lon1, lon2);
                      else sprintf(inv_out, "lon0 %lf lon-center %lf dlon %lf", lon1, lon2, dlon);
                      inv_out += strlen(inv_out);
                      sprintf(inv_out, " #points=%u", npnts);
                      inv_out += strlen(inv_out);

		      if (nx == -1) sprintf(inv_out,"%s#grid points by latitude:",nl);
                      if (ny == -1) sprintf(inv_out,"%s#grid points by longitude:",nl);
                      inv_out += strlen(inv_out);
                      if (nx == -1 || ny == -1) {
                          offset = 72;
                          noct = sec[3][10];
                          n = nx > ny ? nx : ny;
                          for (i = 0; i < n; i++) {
                              p = &(sec[3][offset+i*noct]);
                              val = 0;
                              for (j = 0; j < noct; j++) {
                                   val = 256*val + *p++;
                              }
                              if ((i + 7) % 20 == 0) {
                                  sprintf(inv_out,"%s",nl);
                                  inv_out += strlen(inv_out);
                              }
                              sprintf(inv_out," %d",val);
                              inv_out += strlen(inv_out);
                          }
                      }
                      break;
		  }
        break;
            default: sprintf(inv_out,"no other grid info");
                  break;
        
        }
    }
    return 0;
}
