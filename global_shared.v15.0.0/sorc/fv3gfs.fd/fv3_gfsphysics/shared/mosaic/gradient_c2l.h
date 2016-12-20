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
#ifndef GRADIENT_H_
#define GRADIENT_H_
void grad_c2l(const int *nlon, const int *nlat, const double *pin, const double *dx, const double *dy, const double *area,
	      const double *edge_w, const double *edge_e, const double *edge_s, const double *edge_n,
	      const double *en_n, const double *en_e, const double *vlon, const double *vlat,
	      double *grad_x, double *grad_y, const int *on_west_edge, const int *on_east_edge,
	      const int *on_south_edge, const int *on_north_edge);
void calc_c2l_grid_info(int *nx_pt, int *ny_pt, const double *xt, const double *yt, const double *xc, const double *yc,
		        double *dx, double *dy, double *area, double *edge_w, double *edge_e, double *edge_s,
		        double *edge_n, double *en_n, double *en_e, double *vlon, double *vlat,
                        int *on_west_edge, int *on_east_edge, int *on_south_edge, int *on_north_edge);
void get_edge(int nx, int ny, const double *lont, const double *latt,
	      const double *lonc, const double *latc, double *edge_w, double *edge_e, double *edge_s, double *edge_n,
              int on_west_edge, int on_east_edge, int on_south_edge, int on_north_edge );
void mid_pt_sphere(const double *p1, const double *p2, double *pm);
void mid_pt3_cart(const double *p1, const double *p2, double *e);
#endif
