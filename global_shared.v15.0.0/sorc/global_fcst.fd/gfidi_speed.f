      subroutine gfidi_speed(lon_dim_a,lon_dim_h,lons_lat,lat,
     &                       vg,rcl,v_max)
      use machine , only : kind_phys
      use resol_def
 
      implicit none

      integer lon_dim_a,lon_dim_h,lons_lat
      integer j,k,lat

      real v_max(levs),vg(lon_dim_h,levs),ek,rcl

!     order is bottom to top
 
      v_max = 0.
      do  k=1,levs
        do j=1,lons_lat
          ek = vg(j,k) * vg(j,k) * rcl
          if (ek > v_max(k))  v_max(k) = ek
        enddo
      enddo

      return
      end
