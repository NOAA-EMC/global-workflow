!!!!!  ==========================================================  !!!!!
!!!!!                    module physpara description               !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!     this module defines commonly used control variables/parameters   !
!     in physics related programs.                                     !
!                                                                      !
!     section 1 contains control variables defined in the form of      !
!     parameter. they are pre-determined choices and not adjustable    !
!     during model's run-time.                                         !
!                                                                      !
!     section 2 contains control variables defined as module variables.!
!     they are more flexible to be changed during run-time by either   !
!     through input namelist, or through model environment condition.  !
!     they are preassigned here as the default values.                 !
!                                                                      !
!!!!!  ==========================================================  !!!!!

!========================================!
      module physpara                    !
!........................................!
!
!     implicit   none

!  --- ...  define kind parameters here

!   ** if already exist, use the module containing kind definitions
      use machine

!   ** otherwise, define kind parameter here
!     implicit   none
!     integer, public, parameter :: kind_io4 = 4
!     integer, public, parameter :: kind_io8 = 8
!     integer, public, parameter :: kind_phys= selected_real_kind(13,60) ! the '60' maps to 64-bit real
!      .....

!     implicit   none
!
      public

!==================================================================================
!  section - 1 -
!     control flags are pre-set as run-time non-adjuztable parameters.
!==================================================================================

! ............................................. !
!  -1.1- control flags for sw radiation         !
! ............................................. !
      integer,parameter :: iswrate = 2  ! sw heating rate unit control flag
                                        ! =1:k/day; =2:k/second.
      integer,parameter :: iswrgas = 1  ! sw rare gases effect control flag (ch4,n2o,o2,...)
                                        ! =0:no; =1:yes.
      integer,parameter :: iswcliq = 1  ! sw optical property for liquid clouds
                                        ! =0:input cld opt depth, ignoring iswcice setting
                                        ! =1:input cwp,rew, use hu and stamnes(1993) method
                                        ! =2:not defined yet
      integer,parameter :: iswcice = 3  ! sw optical property for ice clouds (only iswcliq>0)
                                        ! =0:not defined yet
                                        ! =1:input cip,rei, use ebert and curry (1992) method
                                        ! =2:input cip,rei, use streamer v3.0 (2001) method
                                        ! =3:input cip,rei, use fu's method (1996) method
      integer,parameter :: iswmode = 2  ! sw control flag for 2-stream transfer scheme
                                        ! =1:delta-eddington    (joseph et al., 1976)
                                        ! =2:pifm               (zdunkowski et al., 1980)
                                        ! =3:discrete ordinates (liou, 1973)


! ............................................. !
!  -1.2- control flags for lw radiation         !
! ............................................. !
      integer,parameter :: ilwrate = 2  ! lw heating rate unit (1:k/day; 2:k/second)
                                        ! =1:k/day; =2:k/second.
      integer,parameter :: ilwrgas = 1  ! lw rare gases effect control flag (ch4,n2o,o2,cfcs...)
                                        ! =0:no; =1:yes.
      integer,parameter :: ilwcliq = 1  ! lw optical property for liquid clouds
                                        ! =0:input cld opt depth, ignoring ilwcice setting
                                        ! =1:input cwp,rew, use hu and stamnes(1993) method
                                        ! =2:not defined yet
      integer,parameter :: ilwcice = 3  ! lw optical property for ice clouds (only ilwcliq>0)
                                        ! =0:not defined yet
                                        ! =1:input cip,rei, use ebert and curry (1992) method
                                        ! =2:input cip,rei, use streamer (1996) method
                                        ! =3:input cip,rei, use fu's method (1998) method

! ............................................. !
!  -1.3- control flag for lw aerosol property   !
! ............................................. !
      logical,parameter :: lalw1bd =.false. ! =t: use 1 broad-band lw aeros properties
                                            ! =f: use multi bands aeros properites




!==================================================================================
!  section - 2 -
!     values of control flags might be re-set in initialization subroutines
!       (may be adjusted at run time based on namelist input or run condition)
!==================================================================================

! ............................................. !
!  -2.1- for module radiation_astronomy         !
! ............................................. !
      integer, save :: isolar  = 0      ! solar constant scheme control flag

      character, save :: solar_file*26  ! external solar constant data table
!     data solar_file   / 'solarconstantdata.txt     ' /
      data solar_file   / 'solarconstant_noaa_a0.txt ' /

! ............................................. !
!  -2.2- for module radiation_aerosols          !
! ............................................. !
      integer, save :: iaermdl = 0      ! aerosol model scheme control flag
      integer, save :: iaerflg = 0      ! aerosol effect control flag

      logical, save :: lalwflg = .true. ! lw aerosols effect control flag
      logical, save :: laswflg = .true. ! sw aerosols effect control flag
      logical, save :: lavoflg = .true. ! stratospheric volcanic effect flag

      character, save :: aeros_file*26  ! external aerosols data file
!     data aeros_file   / 'climaeropac_global.txt    ' /
      data aeros_file   / 'aerosol.dat               ' /

! ............................................. !
!  -2.3- for module radiation_gases             !
! ............................................. !
      integer, save :: ico2flg = 0      ! co2 data source control flag
      integer, save :: ictmflg = 0      ! external data time/date control flag
      integer, save :: ioznflg = 1      ! ozone data source control flag

      character, save :: co2dat_file*26 ! external co2 2d monthly obsv data table
      character, save :: co2gbl_file*26 ! external co2 global annual mean data tb
      character, save :: co2usr_file*26 ! external co2 user defined data table
      character, save :: co2cyc_file*26 ! external co2 clim monthly cycle data tb
      data co2dat_file   / 'co2historicaldata_2004.txt' /   !year is run-time selected
      data co2gbl_file   / 'co2historicaldata_glob.txt' /
      data co2usr_file   / 'co2userdata.txt           ' /
      data co2cyc_file   / 'co2monthlycyc.txt         ' /

! ............................................. !
!  -2.4- for module radiation_clouds            !
! ............................................. !
      integer, save :: icldflg = 1      ! cloud optical property scheme control flag
      integer, save :: icmphys = 1      ! cloud microphysics scheme control flag
      integer, save :: iovrsw  = 1      ! cloud overlapping control flag for sw
      integer, save :: iovrlw  = 1      ! cloud overlapping control flag for lw

      logical, save :: lcrick  =.false. ! eliminating crick control flag
      logical, save :: lcnorm  =.false. ! in-cld condensate control flag
      logical, save :: lnoprec =.false. ! precip effect on radiation flag (ferrier microphysics)

! ............................................. !
!  -2.5- for module radiation_surface           !
! ............................................. !
      integer, save :: ialbflg = 0      ! surface albedo scheme control flag
      integer, save :: iemsflg = 0      ! surface emissivity scheme control flag

      character, save :: semis_file*26  ! external sfc emissivity data table
      data semis_file   / 'sfc_emissivity_idx.txt    ' /

! ............................................. !
!  -2.6- general purpose                        !
! ............................................. !
      integer, save :: ivflip  = 1      ! vertical profile indexing flag
      integer, save :: isubcsw = 0      ! sub-column cloud approx flag in sw radiation
      integer, save :: isubclw = 0      ! sub-column cloud approx flag in lw radiation
      integer, save :: ipsd0   = 0      ! initial permutation seed for mcica radiation

!
!........................................!
      end module physpara                !
!========================================!
