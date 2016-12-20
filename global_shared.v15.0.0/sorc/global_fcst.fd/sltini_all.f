      subroutine sltini_n_uvw(n_ggp,lon_dim_h,uu,vv,lons_lat)
      use machine            , only : kind_evod
      use resol_def          , only : levs
      use layout_lag         , only : lats_dim_h
      use      pmgrid        , only : plev,pi
      implicit none

      integer,intent(in) :: n_ggp,lon_dim_h,lons_lat
      real(kind=kind_evod),dimension(lon_dim_h,levs,lats_dim_h),
     &                                    intent(inout) :: uu,vv

      integer i,j,ii,jj,k,lonh
      real(kind=kind_evod) singg1_mal(2:lons_lat+1),   ! sin(lamda_at_last_gg_lat)
     &                     cosgg1_mal(2:lons_lat+1)    ! cos(lamda_at_last_gg_lat)
      real(kind=kind_evod) zavec1,zavec2,zaves1,zaves2 ! accumulators for wavenumber 1

      real(kind=kind_evod) lam_0,dgg1_mal,gg1_mal,twoblons
!     real(kind=kind_evod) lam_0,pi,dgg1_mal,gg1_mal,twoblons

      lonh     = lons_lat/2
      lam_0    = 0.0
!     pi       = 4.*atan(1.)
      dgg1_mal = (pi+pi) / float(lons_lat)
      twoblons = 1.0 / float(lonh)

      do i = 2,lons_lat+1
        gg1_mal       = float(i-2)*dgg1_mal + lam_0
        singg1_mal(i) = sin( gg1_mal )
        cosgg1_mal(i) = cos( gg1_mal )
      end do
      do k = 1,plev
        zavec1 = 0.0
        zavec2 = 0.0
        zaves1 = 0.0
        zaves2 = 0.0
        do i = 2,lons_lat+1
          zavec1 = zavec1 + vv(i,k,n_ggp)*cosgg1_mal(i)
          zaves1 = zaves1 + vv(i,k,n_ggp)*singg1_mal(i)
          zavec2 = zavec2 + uu(i,k,n_ggp)*cosgg1_mal(i)
          zaves2 = zaves2 + uu(i,k,n_ggp)*singg1_mal(i)
        end do
        zavec1 = zavec1 * twoblons
        zaves1 = zaves1 * twoblons
        zavec2 = zavec2 * twoblons
        zaves2 = zaves2 * twoblons


        do i = 2,lons_lat+1
          vv(i,k,n_ggp+1) = zavec1*cosgg1_mal(i) + zaves1*singg1_mal(i)
          uu(i,k,n_ggp+1) = zavec2*cosgg1_mal(i) + zaves2*singg1_mal(i)
        end do

        do i = 2,1+lonh
          ii = lonh + i
          vv( i,k,n_ggp+2) = -vv(ii,k,n_ggp)
          vv(ii,k,n_ggp+2) = -vv( i,k,n_ggp)
          uu( i,k,n_ggp+2) = -uu(ii,k,n_ggp)
          uu(ii,k,n_ggp+2) = -uu( i,k,n_ggp)
        end do
      end do !plev loop

      do jj=1,2
         j = n_ggp+jj
         do k=1,plev
            uu(         1,k,j) = uu(1+lons_lat,k,j)
            uu(lons_lat+2,k,j) = uu(2,         k,j)
            uu(lons_lat+3,k,j) = uu(3,         k,j)

            vv(         1,k,j) = vv(1+lons_lat,k,j)
            vv(lons_lat+2,k,j) = vv(2,         k,j)
            vv(lons_lat+3,k,j) = vv(3,         k,j)

         end do
      end do
      return
      end
      subroutine sltini_nh_scalar(n_ggp,lon_dim_h,q3,lons_lat)

      use machine            , only : kind_evod
      use resol_def          , only : levs
      use layout_lag         , only : lats_dim_h
      use      pmgrid        , only : plev
      implicit none

      integer,intent(in) :: n_ggp,lon_dim_h,lons_lat
      real(kind=kind_evod),dimension(lon_dim_h,levs,lats_dim_h),
     &     intent(inout) :: q3

      integer i,j,jj,k,lonh
      real(kind=kind_evod) zave                   ! accumulator for zonal averaging

      lonh = lons_lat/2
      do k=1,plev
         zave = 0.0
         do i=2,lons_lat+1
            zave = zave + q3(i,k ,n_ggp)
         end do
         zave = zave / float(lons_lat)
         do i=2,lons_lat+1
            q3(i,k,n_ggp+1) = zave
         end do
      end do

      do k=1,plev
         do i=2,1+lonh
            q3(     i,k,n_ggp+2) = q3(lonh+i,k,n_ggp)
            q3(lonh+i,k,n_ggp+2) = q3(     i,k,n_ggp)
         end do
      end do

      do jj=1,2
         j = n_ggp+jj
         do k=1,plev
            q3(         1,k ,j) = q3(1+lons_lat,k ,j)
            q3(lons_lat+2,k ,j) = q3(2,         k ,j)
            q3(lons_lat+3,k ,j) = q3(3,         k ,j)
         end do
      end do

      return
      end
      subroutine sltini_s_uvw(s_ggp,lon_dim_h,uu,vv,lons_lat)

      use machine            , only : kind_evod
      use resol_def          , only : levs
      use layout_lag         , only : lats_dim_h
      use      pmgrid        , only : plev,pi

      implicit none
      integer,intent(in) :: s_ggp,lon_dim_h,lons_lat

      real(kind=kind_evod),dimension(lon_dim_h,levs,lats_dim_h),
     &     intent(inout) :: uu,vv

      integer i,j,ii,jj,k,lonh
      real(kind=kind_evod) singg1_mal(2:lons_lat+1),   ! sin(lamda_at_last_gg_lat)
     &                     cosgg1_mal(2:lons_lat+1)    ! cos(lamda_at_last_gg_lat)
      real(kind=kind_evod) zavec1,zavec2,zaves1,zaves2 ! accumulators for wavenumber 1

      real(kind=kind_evod) lam_0,dgg1_mal,gg1_mal,twoblons
!     real(kind=kind_evod) lam_0,pi,dgg1_mal,gg1_mal,twoblons

      lonh     = lons_lat/2
      lam_0    = 0.0
!     pi       = 4.*atan(1.)
      dgg1_mal = (pi+pi) / float(lons_lat)
      twoblons = 1.0 / float(lonh)

      do i = 2,lons_lat+1
         gg1_mal       = float(i-2)*dgg1_mal + lam_0
         singg1_mal(i) = sin( gg1_mal )
         cosgg1_mal(i) = cos( gg1_mal )
      end do
      do k = 1,plev
         zavec1 = 0.0
         zaves1 = 0.0
         zavec2 = 0.0
         zaves2 = 0.0
         do i = 2,lons_lat+1
            zavec1 = zavec1 + vv(i,k,s_ggp  )*cosgg1_mal(i)
            zaves1 = zaves1 + vv(i,k,s_ggp  )*singg1_mal(i)
            zavec2 = zavec2 + uu(i,k,s_ggp  )*cosgg1_mal(i)
            zaves2 = zaves2 + uu(i,k,s_ggp  )*singg1_mal(i)
         end do
         zavec1 = zavec1 * twoblons
         zaves1 = zaves1 * twoblons
         zavec2 = zavec2 * twoblons
         zaves2 = zaves2 * twoblons

         do i = 2,lons_lat+1
           vv(i,k,s_ggp-1) = zavec1*cosgg1_mal(i) + zaves1*singg1_mal(i)
           uu(i,k,s_ggp-1) = zavec2*cosgg1_mal(i) + zaves2*singg1_mal(i)
         end do

         do i = 2,1+lonh
            ii = lonh + i
            vv( i,k,s_ggp-2) = -vv(ii,k,s_ggp)     
            vv(ii,k,s_ggp-2) = -vv( i,k,s_ggp)     
            uu( i,k,s_ggp-2) = -uu(ii,k,s_ggp)     
            uu(ii,k,s_ggp-2) = -uu( i,k,s_ggp)     
         end do
      end do

      do jj=1,2
         j = s_ggp-jj
         do k=1,plev
            uu(         1,k,j) = uu(1+lons_lat,k,j)
            uu(lons_lat+2,k,j) = uu(2,         k,j)
            uu(lons_lat+3,k,j) = uu(3,         k,j)
            vv(         1,k,j) = vv(1+lons_lat,k,j)
            vv(lons_lat+2,k,j) = vv(2,         k,j)
            vv(lons_lat+3,k,j) = vv(3,         k,j)
            end do
         end do
      return
      end
      subroutine sltini_sh_scalar(s_ggp,lon_dim_h,q3,lons_lat)

      use machine            , only : kind_evod
      use resol_def          , only : levs
      use layout_lag         , only : lats_dim_h
      use      pmgrid        , only : plev
      implicit none

      integer,intent(in) :: s_ggp,lon_dim_h,lons_lat
      real(kind=kind_evod),dimension(lon_dim_h,levs,lats_dim_h),
     &     intent(inout) :: q3

      integer i,j,jj,k,lonh
      real(kind=kind_evod) zave                   ! accumulator for zonal averaging

      lonh = lons_lat/2
      do k=1,plev
         zave = 0.0
         do i = 2,lons_lat+1
            zave = zave + q3(i,k,s_ggp  )
         end do
         zave = zave / float(lons_lat)
         do i=2,lons_lat+1
            q3(i,k,s_ggp-1) = zave
         end do
      end do
      do k=1,plev
         do i=2,1+lonh
            q3(     i,k,s_ggp-2) = q3(lonh+i,k,s_ggp)       
            q3(lonh+i,k,s_ggp-2) = q3(     i,k,s_ggp)
         end do
      end do

      do jj=1,2
         j = s_ggp-jj
         do k=1,plev
            q3(         1,k,j) = q3(1+lons_lat,k ,j)
            q3(lons_lat+2,k,j) = q3(2,         k ,j)
            q3(lons_lat+3,k,j) = q3(3,         k ,j)
         end do
      end do
      return
      end
