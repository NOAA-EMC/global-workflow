!!!!!  ==============================================================  !!!!!
!!!!!             lw-rrtm3 radiation package description               !!!!!
!!!!!  ==============================================================  !!!!!
!                                                                          !
!   this package includes ncep's modifications of the rrtm-lw radiation    !
!   code from aer inc.                                                     !
!                                                                          !
!    the rrtm3 package includes these parts:                               !
!                                                                          !
!       'radlw_rrtm3_param.f'                                              !
!       'radlw_rrtm3_datatb.f'                                             !
!       'radlw_rrtm3_main.f'                                               !
!                                                                          !
!    the 'radlw_rrtm3_param.f' contains:                                   !
!                                                                          !
!       'module_radlw_parameters'  -- band parameters set up               !
!                                                                          !
!    the 'radlw_rrtm3_datatb.f' contains:                                  !
!                                                                          !
!       'module_radlw_avplank'     -- plank flux data                      !
!       'module_radlw_ref'         -- reference temperature and pressure   !
!       'module_radlw_cldprlw'     -- cloud property coefficients          !
!       'module_radlw_kgbnn'       -- absorption coeffients for 16         !
!                                     bands, where nn = 01-16              !
!                                                                          !
!    the 'radlw_rrtm3_main.f' contains:                                    !
!                                                                          !
!       'module_radlw_main'        -- main lw radiation transfer           !
!                                                                          !
!    in the main module 'module_radlw_main' there are only two             !
!    externally callable subroutines:                                      !
!                                                                          !
!       'lwrad'     -- main rrtm3 lw radiation routine                     !
!       'rlwinit'   -- to initialize rrtm3 lw radiation                    !
!                                                                          !
!    all the lw radiation subprograms become contained subprograms         !
!    in module 'module_radlw_rrtm' and many of them are not directly       !
!    accessable from places outside the module.                            !
!                                                                          !
!    compilation sequence is:                                              !
!                                                                          !
!       'radlw_rrtm3_param.f'                                              !
!       'radlw_rrtm3_datatb.f'                                             !
!       'radlw_rrtm3_main.f'                                               !
!                                                                          !
!    and all should be put in front of routines that use lw modules        !
!                                                                          !
!    ncep modifications history log:                                       !
!                                                                          !
!       see list in program "radlw_rrtm3_main.f"                           !
!                                                                          !
!!!!!  ==============================================================  !!!!!
!!!!!                         end descriptions                         !!!!!
!!!!!  ==============================================================  !!!!!


!========================================!
      module module_radlw_parameters     !
!........................................!

      use physpara,                only : kind_phys

      implicit none
!
      public
!
!  ---  define type construct for radiation fluxes at toa
!
      type :: topflw_type
        real (kind=kind_phys) :: upfxc         ! total sky upward flux at toa
        real (kind=kind_phys) :: upfx0         ! clear sky upward flux at toa
      end type
!
!  ---  define type construct for radiation fluxes at surface
!
      type :: sfcflw_type
        real (kind=kind_phys) :: upfxc         ! total sky upward flux at sfc
        real (kind=kind_phys) :: upfx0         ! clear sky upward flux at sfc
        real (kind=kind_phys) :: dnfxc         ! total sky downward flux at sfc
        real (kind=kind_phys) :: dnfx0         ! clear sky downward flux at sfc
      end type
!
!  ---  define type construct for optional radiation flux profiles
!
      type :: proflw_type
        real (kind=kind_phys) :: upfxc         ! level up flux for total sky
        real (kind=kind_phys) :: dnfxc         ! level dn flux for total sky
        real (kind=kind_phys) :: upfx0         ! level up flux for clear sky
        real (kind=kind_phys) :: dnfx0         ! level dn flux for clear sky
      end type
!
!  ---  parameter constants for lw band structures
!
      integer, parameter :: nbands = 16         ! num of total spectral bands
      integer, parameter :: ngptlw = 140        ! num of total g-points
      integer, parameter :: ntbl   = 10000      ! lookup table dimension
      integer, parameter :: maxgas = 7          ! max num of absorbing gases
      integer, parameter :: maxxsec= 4          ! num of halocarbon gases
      integer, parameter :: nrates = 6          ! num of ref rates of binary species
      integer, parameter :: nplnk  = 181        ! dim for plank function table

      integer, parameter :: nbdlw  = nbands

!  ---  number of g-point in each band
      integer  :: ng01, ng02, ng03, ng04, ng05, ng06, ng07, ng08,       &
     &            ng09, ng10, ng11, ng12, ng13, ng14, ng15, ng16
      parameter (ng01=10, ng02=12, ng03=16, ng04=14, ng05=16, ng06=08,  &
     &           ng07=12, ng08=08, ng09=12, ng10=06, ng11=08, ng12=08,  &
     &           ng13=04, ng14=02, ng15=02, ng16=02)

!  ---  begining index of each band
      integer  :: ns01, ns02, ns03, ns04, ns05, ns06, ns07, ns08,       &
     &            ns09, ns10, ns11, ns12, ns13, ns14, ns15, ns16
      parameter (ns01=00, ns02=10, ns03=22, ns04=38, ns05=52, ns06=68,  &
     &           ns07=76, ns08=88, ns09=96, ns10=108, ns11=114,         &
     &           ns12=122, ns13=130, ns14=134, ns15=136, ns16=138)

!  ---  band indices for each g-point
      integer, dimension(ngptlw) :: ngb
      data ngb(:) / 10*1, 12*2, 16*3, 14*4, 16*5,  8*6, 12*7,  8*8,     & ! band  1- 8
     &              12*9, 6*10, 8*11, 8*12, 4*13, 2*14, 2*15, 2*16 /      ! band  9-16

!  ---  band spectrum structures (wavenumber in cm**-1)
      real (kind=kind_phys) :: wvnlw1(nbands), wvnlw2(nbands)
      data wvnlw1  /                                                    &
     &         10.,  351.,  501.,  631.,  701.,  821.,  981., 1081.,    &
     &       1181., 1391., 1481., 1801., 2081., 2251., 2381., 2601. /
      data wvnlw2  /                                                    &
     &        350.,  500.,  630.,  700.,  820.,  980., 1080., 1180.,    &
     &       1390., 1480., 1800., 2080., 2250., 2380., 2600., 3250. /

      real (kind=kind_phys) :: delwave(nbands)
      data delwave / 340., 150., 130.,  70., 120., 160., 100., 100.,    &
     &               210.,  90., 320., 280., 170., 130., 220., 650. /

!........................................!
      end module module_radlw_parameters !
!========================================!
