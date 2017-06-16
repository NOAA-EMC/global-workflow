      program test_lons_grid
!
! this program is to use reduce_lons_grid_module to get gaussian latitudes
! and reduced grid number for each Gaussian latitudes. The reduced grid
! number determination is based on Juang (2004) and some documentation in
! the related module.
! Juang, H-M., 2004: A reduced spectral transform for the NCEP seasonal
! forecast global spectral atmospheric model. Monthly Weather Review, 2004
! 1019-1035.
!
! to run this program, we need input file contains resolution of wave, 
! latitudinal grid number, longitudinal grid number (Gaussian grid number),
! and digital number of reduced accuracy, the default is 4.
! where
!      jcap  is wave number, must be even
!      lonf  is FFT-able latitudinal grid number 
!            for linear grid lonf >= jcap*2 + 1
!            for quadratic grid  lonf >= jcap*3 + 1
!      latg  is lonf/2 and must be even number
!      numreduce is fixed with 4 as default, the smaller the less accurate
!
! author: Henry Juang Feb 14, 2013
!

      use reduce_lons_grid_module, only : reduce_grid

! example for the input
!     jcap  lonf  latg numreduce
!      574  1152   576   4 
!      878  1760   880   4
!     1148  2304  1152   4
!     2000  4032  2016   4
!

      real, allocatable :: gslat(:)
      integer, allocatable :: lons_lat(:)
      integer i, jcap, lonf, latg, numreduce

      print *,' Enter jcap lonf latg numreduce'
      read(*,*) jcap, lonf, latg, numreduce
      print *,' Read in jcap=',jcap,' lonf=',lonf,' latg=',latg
      print *,' Use fixed numreduce=',numreduce

      allocate(gslat(latg))
      allocate(lons_lat(latg))

      call reduce_grid (numreduce,jcap,lonf,latg,gslat,lons_lat)

      write(10,100) latg/2
 100  format(i6)
      write(10,150) (gslat(i),i=1,latg/2)
 150  format(5(g13.6))
      write(10,155) jcap,lonf,latg,numreduce
 155  format(' jcap=',i6,' lonf=',i6,' latg=',i6,' numreduce=',i2)

      write(20,200) latg/2
 200  format(i6)
      write(20,250) (lons_lat(i),i=1,latg/2)
 250  format(10(i6))
      write(20,255) jcap,lonf,latg,numreduce
 255  format(' jcap=',i6,' lonf=',i6,' latg=',i6,' numreduce=',i2)

      stop
      end
