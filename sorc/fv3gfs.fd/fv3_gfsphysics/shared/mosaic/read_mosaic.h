 /**********************************************************************
 *                   GNU General Public License                        *
 * This file is a part of fvGFS.                                       *
 *                                                                     *
 * fvGFS is free software; you can redistribute it and/or modify it    *
 * and are expected to follow the terms of the GNU General Public      *
 * License as published by the Free Software Foundation; either        *
 * version 2 of the License, or (at your option) any later version.    *
 *                                                                     *
 * fvGFS is distributed in the hope that it will be useful, but        *
 * WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
 * General Public License for more details.                            *
 *                                                                     *
 * For the full text of the GNU General Public License,                *
 * write to: Free Software Foundation, Inc.,                           *
 *           675 Mass Ave, Cambridge, MA 02139, USA.                   *
 * or see:   http://www.gnu.org/licenses/gpl.html                      *
 **********************************************************************/
#ifndef READ_MOSAIC_H_
#define READ_MOSAIC_H_

int read_mosaic_xgrid_size( const char *xgrid_file );
#ifdef OVERLOAD_R4
void read_mosaic_xgrid_order1(const char *xgrid_file, int *i1, int *j1, int *i2, int *j2, float *area );
void read_mosaic_xgrid_order1_region(const char *xgrid_file, int *i1, int *j1, int *i2, int *j2, float *area, int *isc, int *iec );
void read_mosaic_xgrid_order2(const char *xgrid_file, int *i1, int *j1, int *i2, int *j2, 
                              float *area, float *di, float *dj );
float get_global_area(void);
#else
void read_mosaic_xgrid_order1(const char *xgrid_file, int *i1, int *j1, int *i2, int *j2, double *area );
void read_mosaic_xgrid_order1_region(const char *xgrid_file, int *i1, int *j1, int *i2, int *j2, double *area, int *isc, int *iec );
void read_mosaic_xgrid_order2(const char *xgrid_file, int *i1, int *j1, int *i2, int *j2, 
                              double *area, double *di, double *dj );
double get_global_area(void);
#endif
int read_mosaic_ntiles(const char *mosaic_file);
int read_mosaic_ncontacts(const char *mosaic_file);
void read_mosaic_grid_sizes(const char *mosaic_file, int *nx, int *ny);
void read_mosaic_contact(const char *mosaic_file, int *tile1, int *tile2, int *istart1, int *iend1,
			 int *jstart1, int *jend1, int *istart2, int *iend2, int *jstart2, int *jend2);
void read_mosaic_grid_data(const char *mosaic_file, const char *name, int nx, int ny,
                           double *data, int level, int ioff, int joff);

#endif
