      program test_lons_grid

      use reduce_lons_grid_module, only : reduce_grid

!     parameter(numreduce=1, jcapi= 62, latg= 94)
!     parameter(numreduce=1, jcapi=126, latg=190)
!     parameter(numreduce=2, jcapi=180, latg=270)
!     parameter(numreduce=2, jcapi=254, latg=384)
!     parameter(numreduce=2, jcapi=382, latg=576)
!     parameter(numreduce=2, jcapi=446, latg=672)
!     parameter(numreduce=2, jcapi=510, latg=766)
!     parameter(numreduce=2, jcapi=1020, latg=1532)
      parameter(numreduce=2, jcapi=1278, latg=1920)
!     parameter(numreduce=2, jcapi=2000, latg=3000)

      integer lons_lat(latg),i

      call reduce_grid (numreduce,jcapi,latg,lons_lat)

      write(*,*) latg/2
      write(*,100) (lons_lat(i),i=1,latg/2)
 100  format(10(i6,','))

      stop
      end
