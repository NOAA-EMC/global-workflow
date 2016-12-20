!!!!!  ==============================================================  !!!!!
!!!!!              sw-rrtm3 radiation package description              !!!!!
!!!!!  ==============================================================  !!!!!
!                                                                          !
!   this package includes ncep's modifications of the rrtm-sw radiation    !
!   code from aer inc.                                                     !
!                                                                          !
!   the sw-rrtm3 package includes these parts:                             !
!                                                                          !
!      'radsw_rrtm3_param.f'                                               !
!      'radsw_rrtm3_datatb.f'                                              !
!      'radsw_rrtm3_main.f'                                                !
!                                                                          !
!   the 'radsw_rrtm3_param.f' contains:                                    !
!                                                                          !
!      'module_radsw_parameters'  -- band parameters set up                !
!                                                                          !
!   the 'radsw_rrtm3_datatb.f' contains:                                   !
!                                                                          !
!      'module_radsw_ref'         -- reference temperature and pressure    !
!      'module_radsw_cldprtb'     -- cloud property coefficients table     !
!      'module_radsw_sflux'       -- spectral distribution of solar flux   !
!      'module_radsw_kgbnn'       -- absorption coeffients for 14          !
!                                    bands, where nn = 16-29               !
!                                                                          !
!   the 'radsw_rrtm3_main.f' contains:                                     !
!                                                                          !
!      'module_radsw_main'        -- main sw radiation transfer            !
!                                                                          !
!   in the main module 'module_radsw_main' there are only two              !
!   externally callable subroutines:                                       !
!                                                                          !
!      'swrad'      -- main rrtm3 sw radiation routine                     !
!      'rswinit'    -- initialization routine                              !
!                                                                          !
!   all the sw radiation subprograms become contained subprograms          !
!   in module 'module_radsw_main' and many of them are not directly        !
!   accessable from places outside the module.                             !
!                                                                          !
!   compilation sequence is:                                               !
!                                                                          !
!      'radsw_rrtm3_param.f'                                               !
!      'radsw_rrtm3_datatb.f'                                              !
!      'radsw_rrtm3_main.f'                                                !
!                                                                          !
!   and all should be put in front of routines that use sw modules         !
!                                                                          !
!   ncep modifications history log:                                        !
!                                                                          !
!       see list in program "radsw_rrtm3_main.f"                           !
!                                                                          !
!!!!!  ==============================================================  !!!!!
!!!!!                         end descriptions                         !!!!!
!!!!!  ==============================================================  !!!!!


!========================================!
      module module_radsw_parameters     !
!........................................!

      use physpara,                only : kind_phys

      implicit   none
!
      public
!
!  ---  define type construct for radiation fluxes at toa
!
      type :: topfsw_type
        real (kind=kind_phys) :: upfxc         ! total sky upward flux at toa
        real (kind=kind_phys) :: dnfxc         ! total sky downward flux at toa
        real (kind=kind_phys) :: upfx0         ! clear sky upward flux at toa
      end type
!
!  ---  define type construct for radiation fluxes at surface
!
      type :: sfcfsw_type
        real (kind=kind_phys) :: upfxc         ! total sky upward flux at sfc
        real (kind=kind_phys) :: dnfxc         ! total sky downward flux at sfc
        real (kind=kind_phys) :: upfx0         ! clear sky upward flux at sfc
        real (kind=kind_phys) :: dnfx0         ! clear sky downward flux at sfc
      end type
!
!  ---  define type construct for optional radiation flux profiles
!
      type :: profsw_type
        real (kind=kind_phys) :: upfxc         ! total sky level upward flux
        real (kind=kind_phys) :: dnfxc         ! total sky level downward flux
        real (kind=kind_phys) :: upfx0         ! clear sky level upward flux
        real (kind=kind_phys) :: dnfx0         ! clear sky level downward flux
      end type
!
!  ---  define type construct for optional component downward fluxes at surface
!
      type :: cmpfsw_type
        real (kind=kind_phys) :: uvbfc         ! total sky downward uv-b flux at sfc
        real (kind=kind_phys) :: uvbf0         ! clear sky downward uv-b flux at sfc

        real (kind=kind_phys) :: nirbm         ! sfc downward nir direct beam flux
        real (kind=kind_phys) :: nirdf         ! sfc downward nir diffused flux
        real (kind=kind_phys) :: visbm         ! sfc downward uv+vis direct beam flx
        real (kind=kind_phys) :: visdf         ! sfc downward uv+vis diffused flux
      end type
!
!  ---  parameter constants for sw band structures
!
      integer, parameter :: nblow  = 16            ! band range lower limit
      integer, parameter :: nbhgh  = 29            ! band range upper limit
      integer, parameter :: nbands = nbhgh-nblow+1 ! num of spectral bands
      integer, parameter :: ngptsw = 112           ! total num of g-point in all bands
      integer, parameter :: ngmax  = 16            ! max num of g-point in one band
      integer, parameter :: maxgas = 7             ! max num of absorbing gases
      integer, parameter :: ntbmx  = 10000         ! indx upper lim of trans table

      integer, parameter :: nswstr = 1
      integer, parameter :: nbdsw  = nbands

!  ---  number of g-point in each band
      integer  :: ng16, ng17, ng18, ng19, ng20, ng21, ng22,             &
     &            ng23, ng24, ng25, ng26, ng27, ng28, ng29
      parameter ( ng16=06, ng17=12, ng18=08, ng19=08, ng20=10,          &
     &            ng21=10, ng22=02, ng23=10, ng24=08, ng25=06,          &
     &            ng26=06, ng27=08, ng28=06, ng29=12)

      integer, dimension(nblow:nbhgh) :: ng
      data  ng / ng16, ng17, ng18, ng19, ng20, ng21, ng22,              &
     &           ng23, ng24, ng25, ng26, ng27, ng28, ng29  /

!  ---  starting index of each band
      integer  :: ns16, ns17, ns18, ns19, ns20, ns21, ns22,             &
     &            ns23, ns24, ns25, ns26, ns27, ns28, ns29
      parameter ( ns16=00,         ns17=ns16+ng16,  ns18=ns17+ng17,     &
     &            ns19=ns18+ng18,  ns20=ns19+ng19,  ns21=ns20+ng20,     &
     &            ns22=ns21+ng21,  ns23=ns22+ng22,  ns24=ns23+ng23,     &
     &            ns25=ns24+ng24,  ns26=ns25+ng25,  ns27=ns26+ng26,     &
     &            ns28=ns27+ng27,  ns29=ns28+ng28  )

      integer, dimension(nblow:nbhgh) :: ngs
      data  ngs / ns16, ns17, ns18, ns19, ns20, ns21, ns22,             &
     &            ns23, ns24, ns25, ns26, ns27, ns28, ns29  /

!  ---  band index for each g-point
      integer, dimension(ngptsw)      :: ngb
      data ngb(:) / 16,16,16,16,16,16,                                  & ! band 16
     &              17,17,17,17,17,17,17,17,17,17,17,17,                & ! band 17
     &              18,18,18,18,18,18,18,18,                            & ! band 18
     &              19,19,19,19,19,19,19,19,                            & ! band 19
     &              20,20,20,20,20,20,20,20,20,20,                      & ! band 20
     &              21,21,21,21,21,21,21,21,21,21,                      & ! band 21
     &              22,22,                                              & ! band 22
     &              23,23,23,23,23,23,23,23,23,23,                      & ! band 23
     &              24,24,24,24,24,24,24,24,                            & ! band 24
     &              25,25,25,25,25,25,                                  & ! band 25
     &              26,26,26,26,26,26,                                  & ! band 26
     &              27,27,27,27,27,27,27,27,                            & ! band 27
     &              28,28,28,28,28,28,                                  & ! band 28
     &              29,29,29,29,29,29,29,29,29,29,29,29 /                 ! band 29

!  ---  band wavenumber intervals
      real (kind=kind_phys), dimension(nbands):: wvnum1, wvnum2
      data wvnum1(:)    /                                               &
     &         2600.0, 3251.0, 4001.0, 4651.0, 5151.0, 6151.0, 7701.0,  &
     &         8051.0,12851.0,16001.0,22651.0,29001.0,38001.0,  820.0 /
      data wvnum2(:)    /                                               &
     &         3250.0, 4000.0, 4650.0, 5150.0, 6150.0, 7700.0, 8050.0,  &
     &        12850.0,16000.0,22650.0,29000.0,38000.0,50000.0, 2600.0 /

!
!........................................!
      end module module_radsw_parameters !
!========================================!
