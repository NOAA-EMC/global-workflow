!!!!!  ==========================================================  !!!!!
!!!!!             module "module_iounitdef description             !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!     this module defines fortran unit numbers for input/output data   !
!     files for the ncep gfs model.                                    !
!                                                                      !
!      name      type    description                         unit no.  !
!     ---------------------------------------------------------------  !
!                                                                      !
!     nico2cn - input,  monthly/yearly 2-d co2 data   (shared)  102    !
!     nio3clm - input,  ozone climatology distribution          48     !
!     niradsf - input,  radiation surface data files  (shared)  102    !
!     nicltun - input,  cloud tuning table                      43     !
!     niaercm - input,  aerosols climatology          (shared)  102    !
!                                                                      !
!     noflxf  - output, flux file for post process              63     !
!hchuang code change [+1l]
!     nog3df  - output, 3-d  file for gfs-gocart specific       69     !
!                                                                      !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!

!========================================!
      module module_iounitdef            !
!........................................!
!
      implicit   none
!
      public

!  --- ...  input units

      integer, parameter :: nico2cn = 102
      integer, parameter :: nicltun = 43
      integer, parameter :: nio3clm = 48
      integer, parameter :: niaercm = 102
      integer, parameter :: niradsf = 102

!  --- ... output units

      integer, parameter :: noflxf  = 63
!hchuang code change [+1l]
      integer, parameter :: nog3df  = 69

!
!........................................!
      end module module_iounitdef        !
!========================================!
