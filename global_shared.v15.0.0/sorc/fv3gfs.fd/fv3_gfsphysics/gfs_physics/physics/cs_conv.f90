module cs_conv
!---------------------------------------------------------------------------------
! Purpose:
!
! Interface for Chikira-Sugiyama convection scheme 
!
! Author: Minoru Chikira
! History:
!     Modified for GFS by Don Dazlich 26 June 2014
!  Apr 10 2015 : S. Moorthi - check for allocatable arrays and fix argument for cbmfx
!
!---------------------------------------------------------------------------------
!DD adapt to GFS
  use machine ,   only : r8 => kind_phys
  use physcons,   only : cp => con_cp, con_fvirt, grav => con_g,           &
     &                   rair => con_rd, rvap => con_rv,                   &
     &                   cliq => con_cliq, cvap => con_cvap,               &
     &                   el => con_hvap, emelt => con_hfus, t0c => con_t0c
  use funcphys, only : fpvs ! this is saturation vapor pressure in funcphys.f

!DD  use abortutils,    only: endrun
!DD  use cam_logfile,   only: iulog
  
  implicit none

!DD  include 'mpif.h'

  private                ! Make default type private to the module

! Tuning parameters set from namelist
!
   real(r8), save, public :: CLMD = 0.6,    & ! entrainment efficiency
                             PA=0.15,       & ! factor for buoyancy to affect updraft velocity
                             CPRES = 0.55,  & ! pressure factor for momentum transport
                             ALP0 = 8.0e7     ! alpha parameter in prognostic closure
!DD next two parameters control partitioning of water between detrainment
!DD   and precipitation. Decrease for more precip
!M REAL(r8), public, save ::  PRECZ0 = 1.5e3_r8   ! default = 1.5e3
!M REAL(r8), public, save ::  PRECZ0 = 1.5e3_r8   ! default = 1.5e3
   REAL(r8), public, save ::  PRECZ0 = 1.0e3_r8   ! default = 1.5e3
   REAL(r8), public, save ::  PRECZH = 3.e3_r8    ! default = 4.e3
!
! Private data
!
  integer , parameter, public :: nctp  = 20            ! number of cloud types
! integer , parameter, public :: nctp  = 14            ! number of cloud types
  real(r8), parameter         :: unset_r8 = -999._r8   ! missing value
  real(r8), parameter         :: gravi = 1.0 / grav    ! inverse of gravity
!
! Physical constants (temporarily following MIROC)
!
!DD  real(r8), parameter :: &
!DD    GRAV  = 9.8_r8   ,   & ! gravity
!DD    CP    = 1004.6_r8,   & ! specific heat of air
!DD    EL    = 2.5e6_r8,    & ! latent heat of condensation
!DD    EMELT = 3.4e5_r8,    & ! latent heat of fusion
!DD    RAIR  = 287.04_r8,   & ! gas constant of air
!DD    RVAP  = 461._r8,     & ! gas constant of vapor
!DD    TMELT = 273.15_r8,   & ! melting point of water
!DD    ES0   = 611._r8,     & ! saturation e at 0 deg C (Pa)
!DD    TQICE = 273.15_r8      ! T threshold for ice QSAT

  integer :: iulog ! unit to write debugging and diagnostic output
                   !DD Note - see if I can find corresponding variable in a GFS module
!
!DD  real(r8), save :: EPSV , EPSVT
  real(r8), parameter :: EPSV = RAIR / RVAP,  epsvm1 = epsv - 1.0_r8,   &
                         EPSVT = 1._r8 / EPSV - 1._r8
!
! Shared variables
!
  integer, parameter :: ITL = 3         ! index of liquid water
  integer, parameter :: ITI = 2         ! index of ice water

!DD  integer, save :: ICHNK        ! chunk identifier

!   [INTERNAL PARM]   !DD moved to module scope and allocatable
  LOGICAL,   SAVE, ALLOCATABLE, DIMENSION(:) :: OTSPT1   ! tracer transport by updraft, downdraft on/off
                                                         ! should not include subgrid PDF and turbulence
   LOGICAL,  SAVE, ALLOCATABLE, DIMENSION(:) :: OTSPT2   ! tracer transport by subsidence on/off
                                                         ! should include subgrid PDF and turbulence
   INTEGER,  SAVE, ALLOCATABLE, DIMENSION(:) :: IMFXR 
!  REAL(r8), SAVE, ALLOCATABLE, DIMENSION(:) :: FSCAV    !DD    split declaration and initialization
!  REAL(r8), SAVE, ALLOCATABLE, DIMENSION(:) :: FSWTR    !DD    split declaration and initialization
!
!
!DD  integer, save :: irank, ierror   ! to obtain RANK
!

!  PUBLIC: interfaces
!
!DD  public csconv_readnl ! read csconv_nl namelist
!DD  public cs_convi      ! CS scheme initialization

   public cs_convr        ! CS scheme main driver
  
   contains

!---------------------------------------------------------------------------------
! use GFS functions
   function FQSAT( T, P )   ! calculate saturation water vapor 

   implicit none
  
   real(r8) :: FQSAT           ! saturation water vapor
   real(r8), intent(in) :: T   ! temperature [K]
   real(r8), intent(in) :: P   ! pressure [Pa]
   real(r8), parameter  :: one_m10=1.0d-10
  
!DD  FQSAT = EPSV * ES0 / P &
!DD        * EXP( (EL+EMELT/2._r8*(1._r8-SIGN(1._r8,T-TQICE))) &
!DD               /RVAP *( 1._r8/TMELT - 1._r8/T )           )

   FQSAT = fpvs(T)             !DD this is saturation vapor pressure
!  FQSAT = EPSV * FQSAT / P    !DD This is saturation mixing ratio
   FQSAT = EPSV * FQSAT / (max(p+epsvm1*fqsat,ONE_M10))  !DD&Moo This is saturation mixing ratio

   end function FQSAT
!---------------------------------------------------------------------------------
!  following GFS
   function FDQSAT( T, QS )   ! calculate d(qs)/dT

   implicit none
  
   real(r8) :: FDQSAT           ! d(QSAT)/d(T)
   real(r8), intent(in) :: T    ! temperature [K]
   real(r8), intent(in) :: QS   ! saturation water vapor [kg/kg]
   real(r8)             :: wrk
  
   real(r8), parameter :: fact1=(cvap-cliq)/rvap,fact2=el/rvap-fact1*t0c
  
!DD  FDQSAT = (EL+EMELT/2._r8*(1._r8-SIGN(1._r8,T-TMELT))) &
!DD         * QS / ( RVAP * T*T )

            wrk      = 1.0 / t
            FDQSAT   = qs * wrk * (fact1 + fact2*wrk)
!           FDQSAT   = qs * (fact1 / t + fact2 / (t**2))


   end function FDQSAT
!---------------------------------------------------------------------------------
!DDsubroutine csconv_readnl(nlfile)
!DD
!DD   use namelist_utils, only: find_group_name
!DD   use units,          only: getunit, freeunit
!DD   use mpishorthand
!DD
!DD   implicit none
!DD
!DD   character(len=*), intent(in) :: nlfile   ! filepath for file containing namelist input
!DD
!DD!
!DD! Local variables
!DD!
!DD   integer :: unitn, ierr
!DD   character(len=*), parameter :: subname = 'csconv_readnl'
!DD!
!DD   real(r8) :: &
!DD     csconv_clmd  = unset_r8,   &
!DD     csconv_pa    = unset_r8,   &
!DD     csconv_cpres = unset_r8,   &
!DD     csconv_alp0  = unset_r8
!DD
!DD   namelist /csconv_nl/ csconv_clmd, csconv_pa, csconv_cpres, csconv_alp0
!DD
!DD   if ( mrank == 0 ) then
!DD      unitn = getunit()
!DD      open( unitn, file=trim(nlfile), status='old' )
!DD      call find_group_name( unitn, 'csconv_nl', status=ierr )
!DD      if ( ierr == 0 ) then
!DD         read( unitn, csconv_nl, iostat=ierr)
!DD         if ( ierr /= 0 ) then
!DD            call endrun( subname // ':: ERROR reading namelist' )
!DD         end if
!DD      end if
!DD      close( unitn )
!DD      call freeunit( unitn )
!DD
!DD      CLMD  = csconv_clmd
!DD      PA    = csconv_pa
!DD      CPRES = csconv_cpres
!DD      ALP0  = csconv_alp0
!DD   end if
!DD
!DD#ifdef SPMD
!DD   ! Broadcast namelist variables
!DD   call mpibcast( CLMD , 1, mpir8, 0, mpicom)
!DD   call mpibcast( PA   , 1, mpir8, 0, mpicom)
!DD   call mpibcast( CPRES, 1, mpir8, 0, mpicom) 
!DD   call mpibcast( ALP0 , 1, mpir8, 0, mpicom) 
!DD#endif
!DD
!DDend subroutine csconv_readnl
!---------------------------------------------------------------------------------
!DDsubroutine cs_convi(limcnv_in, no_deep_pbl_in)
!DD
!DD   use constituents, only: cnst_get_ind
!DD
!DD   implicit none
!DD
!DD   integer, intent(in)           :: limcnv_in       ! top interface level limit for convection (not supported yet)
!DD   logical, intent(in), optional :: no_deep_pbl_in  ! no_deep_pbl = .true. eliminates CS convection entirely within PBL (not supported yet)
!DD   
!DD   EPSV  = RAIR / RVAP
!DD   EPSVT = 1._r8 / EPSV - 1._r8
!DD   
!DD   call cnst_get_ind('CLDLIQ', ITL)
!DD   call cnst_get_ind('CLDICE', ITI)
!DD
!DD   if ( CLMD  == unset_r8 ) &
!DD      call endrun( 'cs_convi: csconv_clmd must be set in the namelist.' )
!DD   if ( PA    == unset_r8 ) &
!DD      call endrun( 'cs_convi: csconv_pa must be set in the namelist.' )
!DD   if ( CPRES == unset_r8 ) &
!DD      call endrun( 'cs_convi: csconv_cpres must be set in the namelist.' )
!DD   if ( ALP0  == unset_r8 ) &
!DD      call endrun( 'cs_convi: csconv_alp0 must be set in the namelist.' )
!DD
!DD   if ( mrank == 0 ) then
!DD      write(iulog,*) 'tuning parameters cs_convi: CLMD' , CLMD
!DD      write(iulog,*) 'tuning parameters cs_convi: PA'   , PA
!DD      write(iulog,*) 'tuning parameters cs_convi: CPRES', CPRES
!DD      write(iulog,*) 'tuning parameters cs_convi: ALP0' , ALP0
!DD   end if
!DD
!DDend subroutine cs_convi
!---------------------------------------------------------------------------------
   subroutine cs_convr(IM      ,IJSDIM  ,KMAX  , NTR   ,        & !DD dimensions
                       t       ,q       ,prec  , clw   ,        &
                       zm      ,zi      ,                       &
                       pap     ,paph    ,                       &
                       delta   ,delti   ,                       &
                       ud_mf   ,dd_mf   ,dt_mf   ,              &
                       u       ,v       , fscav, fswtr,         &
                       cbmfx, mype, wcbmaxm)
!---------------------------------------------------------------------------------
! Purpose:
!
! Main driver for Chikira-Sugiyama convective scheme 
!
! Author: Minoru Chikira
!
!---------------------------------------------------------------------------------
!DD   use ppgrid,        only: pcols, pver, pverp
!DD   use cam_history,   only: outfld
!DD   use constituents,  only: pcnst

   implicit none
!
! input arguments
!
   INTEGER, INTENT(IN) :: IM,IJSDIM, KMAX, NTR, mype             !! DD, for GFS, pass in


   real(r8), intent(inout) :: t(IM,KMAX)          ! temperature at mid-layer (K)
   real(r8), intent(inout) :: q(IM,KMAX)          ! water vapor array including moisture (kg/kg)
   real(r8), intent(inout) :: clw(IM,KMAX,ntr-1)  ! tracer array including cloud condensate (kg/kg)
   real(r8), intent(in)    :: pap(IM,KMAX)        ! pressure at mid-layer (Pa)
   real(r8), intent(in)    :: paph(IM,KMAX+1)     ! pressure at boundaries (Pa)
   real(r8), intent(in)    :: zm(IM,KMAX)         ! geopotential at mid-layer (m)
   real(r8), intent(in)    :: zi(IM,KMAX+1)       ! geopotential at boundaries (m)
   real(r8), intent(in)    :: fscav(ntr), fswtr(ntr), wcbmaxm(ijsdim)
! added for cs_convr
   real(r8), intent(inout) :: u(IM,KMAX)          ! zonal wind at mid-layer (m/s)
   real(r8), intent(inout) :: v(IM,KMAX)          ! meridional wind at mid-layer (m/s)
   
   real(r8), intent(in)    :: DELTA               ! physics time step
   real(r8), intent(in)    :: DELTI               ! dynamics time step (model time increment in seconds)
!
! modified arguments
!
   real(r8), intent(inout) :: CBMFX(IM,nctp)      ! cloud base mass flux (kg/m2/s)
!
! output arguments
!
   real(r8), intent(out)   :: ud_mf(IJSDIM,KMAX)  ! updraft mass flux (kg/m2/s)
   real(r8), intent(out)   :: dd_mf(IJSDIM,KMAX)  ! downdraft mass flux (kg/m2/s)
   real(r8), intent(out)   :: dt_mf(IJSDIM,KMAX)  ! detrainment mass flux (kg/m2/s)
   
   real(r8), intent(out)   :: prec(IJSDIM)        ! precipitation at surface (including snowfall) (kg/m2/s)
!
! output arguments of CS_CUMLUS
!
   real(r8) GTT(IJSDIM,KMAX)                      ! temperature tendency [K/s]
   real(r8) GTQ(IJSDIM,KMAX,NTR)                  ! tracer tendency [kg/kg/s]
   real(r8) GTU(IJSDIM,KMAX)                      ! zonal velocity tendency [m/s2]
   real(r8) GTV(IJSDIM,KMAX)                      ! meridional velocity tendency [m/s2]
   real(r8) CMDET(IJSDIM,KMAX)                    ! detrainment mass flux [kg/m2/s]
   real(r8) GTLDET(IJSDIM,KMAX)                   ! cloud liquid tendency by detrainment [1/s]
   real(r8) GTIDET(IJSDIM,KMAX)                   ! cloud ice tendency by detrainment [1/s]
   real(r8) GTPRP(IJSDIM,KMAX+1)                  ! precipitation (including snowfall) flux at interfaces [kg/m2/s]
   real(r8) GSNWP(IJSDIM,KMAX+1)                  ! snowfall flux at interfaces [kg/m2/s]
   real(r8) GMFX0(IJSDIM,KMAX+1)                  ! updraft mass flux [kg/m2/s]
   real(r8) GMFX1(IJSDIM,KMAX+1)                  ! downdraft mass flux [kg/m2/s]
   integer  KT(IJSDIM,nctp)                       ! cloud top index for each cloud type

!DD removed as output arguments
   real(r8) :: jctop(IJSDIM)           ! o row of top-of-deep-convection indices passed out.
   real(r8) :: jcbot(IJSDIM)           ! o row of base of cloud indices passed out.
   real(r8) :: dlf(IJSDIM,KMAX)        ! scattered version of the detraining cld h2o tend (kg/kg/s)
   real(r8) :: pflx(IJSDIM,KMAX+1)     ! scattered precip flux at each level
   real(r8) :: cme(IJSDIM,KMAX)        ! condensation - evaporation
   real(r8) :: cape(IJSDIM)            ! convective available potential energy (J/kg)
   real(r8) :: rliq(IJSDIM)            ! reserved liquid (not yet in cldliq) for energy integrals (m/s)
   real(r8) :: flxprec(IJSDIM,KMAX+1)  ! precipitation flux (including snowfall) at interfaces (kg/m2/s)
   real(r8) :: flxsnow(IJSDIM,KMAX+1)  ! snowfall flux at interfaces (kg/m2/s)
   real(r8) :: snow(IJSDIM)            ! snowfall at surface (kg/m2/s)

!
! input arguments of CS_CUMLUS
!
   real(r8) GDT(IJSDIM,KMAX)                       ! temperature [K]
   real(r8) GDQ(IJSDIM,KMAX,NTR)                   ! tracers including moisture [kg/kg]
   real(r8) GDU(IJSDIM,KMAX)                       ! zonal wind [m/s]
   real(r8) GDV(IJSDIM,KMAX)                       ! meridional wind [m/s]
   real(r8) GDTM(IJSDIM,KMAX+1)                    ! temperature at boundaries of layers [K]
   real(r8) GDP(IJSDIM,KMAX)                       ! pressure [Pa]
   real(r8) GDPM(IJSDIM,KMAX+1)                    ! pressure at boundaries of layers [Pa]
   real(r8) GDZ(IJSDIM,KMAX)                       ! altitude [m]
   real(r8) GDZM(IJSDIM,KMAX+1)                    ! altitude at boundaries of layers [m]
   integer ISTS, IENS
!
! local variables
!
!DD   real(r8) :: zs(IJSDIM)                       ! surface height [m]
!DD   real(r8) :: ftem(IJSDIM,KMAX)
   integer KTMAX(IJSDIM)                           ! max of KT
   real(r8)    :: ftintm, wrk, wrk1
   integer i, k, n
!  integer i, k, n, iunit

!DD borrowed from RAS to go form total condensate to ice/water separately
      real(kind=r8), parameter  :: zero=0.0, one=1.0
      real(kind=r8) tf, tcr, tcrf, tcl
!     parameter (tf=130.16, tcr=160.16, tcrf=1.0/(tcr-tf),tcl=2.0)
!     parameter (tf=230.16, tcr=260.16, tcrf=1.0/(tcr-tf))
      parameter (tf=233.16, tcr=263.16, tcrf=1.0/(tcr-tf),tcl=2.0)
      real(r8) :: tem

!DD   call mpi_comm_rank( mpi_comm_world, irank, ierror )
!
! convert CAM input variables to MIROC counterparts
!
 !DD  ICHNK = lchnk
   ISTS = 1
   IENS = IJSDIM

   do k = 1, KMAX
      do i = 1, IJSDIM
         GDT (i,k)   = t(i,k)
         GDU (i,k)   = u(i,k)
         GDV (i,k)   = v(i,k)
         GDZ (i,k)   = zm(i,k) * gravi
         GDP (i,k)   = pap(i,k)
         GDQ (i,k,1) = q(i,k)
      end do
   end do
   do k = 1, KMAX+1
      do i = 1, IJSDIM
         GDZM(i,k) = zi(i,k) * gravi
         GDPM(i,k) = paph(i,k)
      end do
   end do

!DD following adapted from ras
   if (clw(1,1,2) <= -999.0) then  ! input ice/water are together
     do k=1,kmax
       do i = 1, IJSDIM
         tem = clw(i,k,1) * MAX(ZERO, MIN(ONE, (TCR-t(i,k))*TCRF))
         clw(i,k,2) = clw(i,k,1) - tem
         clw(i,k,1) = tem
       enddo
     enddo
   endif
!DD end ras adaptation

   do n = 2, NTR
      do k = 1, KMAX
         do i = 1, IJSDIM
            GDQ(i,k,n) = clw(i,k,n-1)
         end do
      end do
   end do
!***************************************************************************************
!  iunit = 400 + mype
!  write(iunit,*)kmax,'kmax',delta,'delta',im,'im',ijsdim,'ijsdim',iens,'iens',ists,'ists' !DDdebug
!  write(iunit,*),i  !DDdebug
!  do i = 1, 1                  !DDdebug
!    write(iunit,*)'gdt'        !DDdebug
!    write(iunit,*)gdt(I,:)     !DDdebug
!    write(iunit,*)'gdu'        !DDdebug
!    write(iunit,*)gdu(I,:)     !DDdebug
!    write(iunit,*)'gdv'        !DDdebug
!    write(iunit,*)gdv(I,:)     !DDdebug
!    do k = 1,ntr  !DDdebug
!      write(iunit,*)'gdq',k    !DDdebug
!      write(iunit,*)gdq(I,:,k) !DDdebug
!    enddo  !DDdebug
!    write(iunit,*)'gdz'        !DDdebug
!    write(iunit,*)gdz(I,:)     !DDdebug
!    write(iunit,*)'gdp'        !DDdebug
!    write(iunit,*)gdp(I,:)     !DDdebug
!    write(iunit,*)'gdzm'       !DDdebug
!    write(iunit,*)gdzm(I,:)    !DDdebug
!    write(iunit,*)'gdpm'       !DDdebug
!    write(iunit,*)gdpm(I,:)    !DDdebug
!    write(iunit,*)'cbmfx'      !DDdebug
!    write(iunit,*)cbmfx(I,:)   !DDdebug
!  enddo  !DDdebug
!***************************************************************************************
!
! calculate temperature at interfaces
!
!  call TINTP( IJSDIM, KMAX  ,     & !DD dimensions
!              GDTM,               & ! output
!              GDT, GDP, GDPM,     & ! input
!              ISTS, IENS      )     ! active array size
   DO K = 2, KMAX
     DO I = ISTS, IENS
       wrk  = 1.0d0 / GDP(I,K)
       wrk1 = 1.0d0 / LOG(GDP(I,K-1)*wrk)
       FTINTM    = wrk1 * LOG(GDPM(I,K)*wrk)
       GDTM(I,K) = FTINTM *GDT(I,K-1) + (1.0-FTINTM)*GDT(I,K)
      ENDDO
   ENDDO

   DO I = ISTS, IENS
     GDTM(I,KMAX+1) = GDT(I,KMAX)
     GDTM(I,1     ) = GDT(I,1)        ! Is this a good approximation ? - Moorthi
   ENDDO
!
! call main routine
!
!***************************************************************************************
   call CS_CUMLUS (im    , IJSDIM, KMAX  , NTR   ,   &  !DD dimensions
                   GTT   , GTQ   , GTU   , GTV   ,   & ! output
                   CMDET , GTLDET, GTIDET,           & ! output
                   GTPRP , GSNWP , GMFX0 ,           & ! output
                   GMFX1 , cape  , KT    ,           & ! output
                   CBMFX ,                           & ! modified
                   GDT   , GDQ   , GDU   , GDV   ,   & ! input
                   GDTM  ,                           & ! input
                   GDP   , GDPM  , GDZ   , GDZM  ,   & ! input
                   DELTA , DELTI , ISTS  , IENS, mype,&! input
                   fscav, fswtr, wcbmaxm)              ! input
!
! convert MIROC output variables to CAM counterparts
!DD detrainment has to be added in for GFS
!
!DD   do n = 2, NTR
     do k = 1, KMAX
        do i = 1, IJSDIM
!DD           clw(i,k,n-1) = GDQ(i,k,n) + GTQ(i,k,n) * delta
           clw(i,k,1) = GDQ(i,k,2) + (gtq(i,k,2) + gtidet(i,k)) * delta
           clw(i,k,2) = GDQ(i,k,3) + (gtq(i,k,3) + gtldet(i,k)) * delta
        end do
     end do
!DD   end do
!
   do k = 1, KMAX
     do i = 1, IJSDIM
!DD    heat(i,KMAX-k+1) = CP*GTT(i,k) - EMELT*GTIDET(i,k)
       q(i,k)        = GDQ(i,k,1)  + GTQ(i,k,1) * delta
       t(i,k)        = GDT(i,k)    + GTT(i,k) * delta
       u(i,k)        = GDU(i,k)    + GTU(i,k) * delta
       v(i,k)        = GDV(i,k)    + GTV(i,k) * delta
!DD    dlf (i,k)     = GTLDET(i,k) + GTIDET(i,k)
!DD    rliq(i)       = ( GTLDET(i,k)+GTIDET(i,k) )*( GDPM(i,k+1)-GDPM(i,k) )/GRAV
!
        flxprec(i,k) = GTPRP(i,k)
        flxsnow(i,k) = GSNWP(i,k)
! Set the mass fluxes.
        ud_mf  (i,k) = GMFX0(i,k)
        dd_mf  (i,k) = GMFX1(i,k)
        dt_mf  (i,k) = CMDET(i,k)
     end do
   end do

!****************************************************************************
!  do i=1,1                     !DDdebug 
!    write(iunit,*)'gtt'        !DDdebug
!    write(iunit,*)gtt(I,:)     !DDdebug
!    do k = 1,ntr               !DDdebug
!      write(iunit,*)'gtq',k    !DDdebug
!      write(iunit,*)gtq(I,:,k) !DDdebug
!    enddo                      !DDdebug
!    write(iunit,*)'gtu'        !DDdebug
!    write(iunit,*)gtu(I,:)     !DDdebug
!    write(iunit,*)'gtv'        !DDdebug
!    write(iunit,*)gtv(I,:)     !DDdebug
!    write(iunit,*)'gtprp'      !DDdebug
!    write(iunit,*)gtprp(I,:)   !DDdebug
!    write(iunit,*)'gsnwp'      !DDdebug
!    write(iunit,*)gsnwp(I,:)   !DDdebug
!    write(iunit,*)'gmfx0'      !DDdebug
!    write(iunit,*)gmfx0(I,:)   !DDdebug
!    write(iunit,*)'gmfx1'      !DDdebug
!    write(iunit,*)gmfx1(I,:)   !DDdebug
!    write(iunit,*)'cmdet'      !DDdebug
!    write(iunit,*)cmdet(I,:)   !DDdebug
!    write(iunit,*)'cbmfx'      !DDdebug
!    write(iunit,*)cbmfx(I,:)   !DDdebug
!    write(iunit,*)'kt'         !DDdebug
!    write(iunit,*)kt(I,:)      !DDdebug
!    write(iunit,*)'cape'       !DDdebug
!    write(iunit,*)cape(I)      !DDdebug
!    write(iunit,*)'gtldet'     !DDdebug
!    write(iunit,*)gtldet(I,:)  !DDdebug
!    write(iunit,*)'gtidet'     !DDdebug
!    write(iunit,*)gtldet(I,:)  !DDdebug
!  enddo                        !DDdebug
!****************************************************************************
!
   KTMAX = 1
   do n = 1, nctp
     do i = 1, IJSDIM
        KTMAX(i) = max( KTMAX(i), KT(i,n) )
     end do
   end do
!
   do i = 1, IJSDIM
     jctop(i) = KTMAX(i)
     prec(i) = GTPRP(i,1)
     snow(i) = GSNWP(i,1)
!    rliq(i) = rliq(i)/1000._r8      ! kg/m2/s => m/s
   end do
  
   cme   = 0._r8   ! temporarily set to be zero
   pflx  = 0._r8   ! temporarily set to be zero
   jcbot = 1       ! set to be the lowest layer

!DD  call outfld('CAPE'    , cape         , IJSDIM, lchnk)
!DD  ftem(:ncol,:KMAX) = heat(:ncol,:KMAX)/CP
!DD  call outfld('CSDT'    , ftem         , IJSDIM, lchnk)
!DD  call outfld('CSDQ'    , qtnd(1,1,1)  , IJSDIM, lchnk)
!DD  call outfld('CSDLIQ'  , qtnd(1,1,ITL), IJSDIM, lchnk)
!DD  call outfld('CSDICE'  , qtnd(1,1,ITI), IJSDIM, lchnk)
!DD  call outfld('CSMTU'   , utnd         , IJSDIM, lchnk)
!DD  call outfld('CSMTV'   , vtnd         , IJSDIM, lchnk)
!DD  call outfld('CSFLXPRC', flxprec      , IJSDIM, lchnk)
!DD  call outfld('CSFLXSNW', flxsnow      , IJSDIM, lchnk)
!DD  call outfld('CSMU'    , mcon         , IJSDIM, lchnk)
!DD  call outfld('PRECCDCS', prec         , IJSDIM, lchnk)

   end subroutine cs_convr


!************************************************************************
!* Original source code in MIROC5
!*
!* PACKAGE PCUMC  !!  physics: cumulus parameterization with
!*                             state-dependent entrainment rate
!*                             developed by Minoru Chikira
!* [Note]
!* -This routine works as the prognostic Arakawa-Schubert scheme
!*  if OPT_ASMODE is specified.
!* -Specify OPT_NS02 to use entrainment rate of Neggers et al. (2002)
!* -Specify OPT_CUMBGT to check water and energy budget.
!* -Specify OPT_CUMCHK to check range of output values.
!*
!*   [HIS] 08/09/19(chikira)   MIROC4.1
!*         08/10/30(hiro)      CMT modified
!*         08/11/11(chikira)   Neggers et al. (2002)
!*         08/12/3 (chikira)   downdraft detrainment modified
!*         08/12/3 (chikira)   COSP output
!*         09/02/24(chikira)   fix convective inhibition
!*         09/04/16(hiro)      CMIP5 output (cbasep,ctopp)
!*         09/09/03(yokohata)  COSP
!*         10/11/19(toshi)     small bug fix
!*         14/02/07(chikira)   CUMDWN bug fix, CMT modified
!************************************************************************
   SUBROUTINE CS_CUMLUS                                    & !cumulus main routine
                        (im    , IJSDIM, KMAX  , NTR   ,   & !DD dimensions
                         GTT   , GTQ   , GTU   , GTV   ,   & ! output
                         CMDET , GTLDET, GTIDET,           & ! output
                         GTPRP , GSNWP , GMFX0 ,           & ! output
                         GMFX1 , CAPE  , KT    ,           & ! output
!                        CUMCLW, CUMFRC,
!*COSP
!                        QLIQC , QICEC , GPRCPF, GSNWPF,
!
!                        GTCFRC, FLIQC ,
!#ifdef OPT_CHASER
!     O                  RFXC  , SFXC  , LEVCUM, LNFRC , REVC  , ! <<CHEM>>
!#endif
                         CBMFX ,                           & ! modified
                         GDT   , GDQ   , GDU   , GDV   ,   & ! input
                         GDTM  ,                           & ! input
                         GDP   , GDPM  , GDZ   , GDZM  ,   & ! input
!                        GDCFRC,
                         DELTA , DELTI , ISTS  , IENS, mype,&! input
                         fscav, fswtr, wcbmaxm)              ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!DD      use constituents, only: NTR => pcnst
!
   IMPLICIT NONE
      
   INTEGER, INTENT(IN) :: im, IJSDIM, KMAX, NTR, mype   !! DD, for GFS, pass in
!
! [OUTPUT]
   REAL(r8), INTENT(OUT) :: GTT   ( IJSDIM, KMAX      ) !! heating rate
   REAL(r8), INTENT(OUT) :: GTQ   ( IJSDIM, KMAX, NTR ) !! change in q
   REAL(r8), INTENT(OUT) :: GTU   ( IJSDIM, KMAX      ) !! tendency of u
   REAL(r8), INTENT(OUT) :: GTV   ( IJSDIM, KMAX      ) !! tendency of v
   REAL(r8), INTENT(OUT) :: CMDET ( IJSDIM, KMAX      ) !! detrainment mass flux
   REAL(r8), INTENT(OUT) :: GTLDET( IJSDIM, KMAX      ) !! cloud liquid tendency by detrainment
   REAL(r8), INTENT(OUT) :: GTIDET( IJSDIM, KMAX      ) !! cloud ice tendency by detrainment
   REAL(r8), INTENT(OUT) :: GTPRP ( IJSDIM, KMAX+1    ) !! rain+snow flux
   REAL(r8), INTENT(OUT) :: GSNWP ( IJSDIM, KMAX+1    ) !! snowfall flux
   REAL(r8), INTENT(OUT) :: GMFX0 ( IJSDIM, KMAX+1    ) !! updraft mass flux
   REAL(r8), INTENT(OUT) :: GMFX1 ( IJSDIM, KMAX+1    ) !! downdraft mass flux
   REAL(r8), INTENT(OUT) :: CAPE  ( IJSDIM            )
   INTEGER , INTENT(OUT) :: KT    ( IJSDIM, NCTP      ) !! cloud top
!
!  [MODIFIED]
   REAL(r8), INTENT(INOUT) :: CBMFX ( IM, NCTP        ) !! cloud base mass flux
!
!  [INPUT]
   REAL(r8), INTENT(IN) :: GDT   ( IJSDIM, KMAX      ) !! temperature T
   REAL(r8), INTENT(IN) :: GDQ   ( IJSDIM, KMAX, NTR ) !! humidity, tracer
   REAL(r8), INTENT(IN) :: GDU   ( IJSDIM, KMAX      ) !! westerly u
   REAL(r8), INTENT(IN) :: GDV   ( IJSDIM, KMAX      ) !! southern wind v
   REAL(r8), INTENT(IN) :: GDTM  ( IJSDIM, KMAX+1    ) !! temperature T
   REAL(r8), INTENT(IN) :: GDP   ( IJSDIM, KMAX      ) !! pressure P
   REAL(r8), INTENT(IN) :: GDPM  ( IJSDIM, KMAX+1    ) !! pressure (half lev)
   REAL(r8), INTENT(IN) :: GDZ   ( IJSDIM, KMAX      ) !! altitude
   REAL(r8), INTENT(IN) :: GDZM  ( IJSDIM, KMAX+1    ) !! altitude
   REAL(r8), INTENT(IN) :: DELTA                       !! delta(t) (dynamics)
   REAL(r8), INTENT(IN) :: DELTI                       !! delta(t) (internal variable)
   INTEGER, INTENT(IN)  :: ISTS, IENS   !! array range

   real(r8), intent(in) :: fscav(ntr), fswtr(ntr), wcbmaxm(ijsdim)
!
!  [INTERNAL WORK]
   REAL(r8)     GPRCC ( IJSDIM, NTR       ) !! rainfall
   REAL(r8)     GSNWC ( IJSDIM            ) !! snowfall
   REAL(r8)     CUMCLW( IJSDIM, KMAX      ) !! cloud water in cumulus
   REAL(r8)     CUMFRC( IJSDIM            ) !! cumulus cloud fraction
!COSP
   REAL(r8)     QLIQC ( IJSDIM, KMAX   )    !! cumulus cloud liquid water [kg/kg]
   REAL(r8)     QICEC ( IJSDIM, KMAX   )    !! cumulus cloud ice [kg/kg]
   REAL(r8)     GPRCPF( IJSDIM, KMAX   )    !! rainfall flux at full level
   REAL(r8)     GSNWPF( IJSDIM, KMAX   )    !! snowfall flux at full level
!
   REAL(r8)     GTCFRC( IJSDIM, KMAX      ) !! change in cloud fraction
   REAL(r8)     FLIQC ( IJSDIM, KMAX      ) !! liquid ratio in cumulus
!
!#ifdef OPT_CHASER
!      REAL(r8)     RFXC  ( IJSDIM, KMAX+1    ) !! precipi. flx [kg/m2/s]
!      REAL(r8)     SFXC  ( IJSDIM, KMAX+1    ) !! ice/snow flx [kg/m2/s]
!      INTEGER      LEVCUM( IJSDIM, KMAX      ) !! flag for cum. cloud top
!      REAL(r8)     LNFRC ( IJSDIM, KMAX      ) !! areal rates of clouds
!      REAL(r8)     REVC  ( IJSDIM, KMAX      ) !! evaporation rates
!#endif
!
   REAL(r8)     GDCFRC( IJSDIM, KMAX      ) !! cloud fraction
!
   REAL(r8)     GDQI  ( IJSDIM, KMAX )      !! cloud ice
   REAL(r8)     GTQI  ( IJSDIM, KMAX )      !! tendency of cloud ice
   REAL(r8)     GTQL  ( IJSDIM, KMAX )      !! tendency of cloud liquid
!
   REAL(r8)     GDW   ( IJSDIM, KMAX )      !! total water
   REAL(r8)     DELP  ( IJSDIM, KMAX )
   REAL(r8)     GDQS  ( IJSDIM, KMAX )      !! saturate moisture
   REAL(r8)     FDQS  ( IJSDIM, KMAX )
   REAL(r8)     GAM   ( IJSDIM, KMAX )
   REAL(r8)     GDS   ( IJSDIM, KMAX )      !! dry static energy
   REAL(r8)     GDH   ( IJSDIM, KMAX )      !! moist static energy
   REAL(r8)     GDHS  ( IJSDIM, KMAX )      !! saturate MSE
!
   REAL(r8)     GCYM  ( IJSDIM, KMAX )      !! norm. mass flux (half lev)
   REAL(r8)     GCHB  ( IJSDIM )            !! cloud base MSE-Li*Qi
   REAL(r8)     GCWB  ( IJSDIM )            !! cloud base total water
   REAL(r8)     GCUB  ( IJSDIM )            !! cloud base U
   REAL(r8)     GCVB  ( IJSDIM )            !! cloud base V
   REAL(r8)     GCIB  ( IJSDIM )            !! cloud base ice
   REAL(r8)     ELAM  ( IJSDIM, KMAX, NCTP )   !! entrainment (rate*massflux)
   REAL(r8)     GCYT  ( IJSDIM, NCTP )      !! norm. mass flux @top
   REAL(r8)     GCHT  ( IJSDIM, NCTP )      !! cloud top MSE
   REAL(r8)     GCQT  ( IJSDIM, NCTP )      !! cloud top q
   REAL(r8)     GCUT  ( IJSDIM, NCTP )      !! cloud top U
   REAL(r8)     GCVT  ( IJSDIM, NCTP )      !! cloud top V
   REAL(r8)     GCLT  ( IJSDIM, NCTP )      !! cloud top cloud water
   REAL(r8)     GCIT  ( IJSDIM, NCTP )      !! cloud top cloud ice
   REAL(r8)     GTPRT ( IJSDIM, NCTP )      !! precipitation/M
   REAL(r8)     GCLZ  ( IJSDIM, KMAX )      !! cloud liquid for each CTP
   REAL(r8)     GCIZ  ( IJSDIM, KMAX )      !! cloud ice for each CTP

   REAL(r8)     ACWF  ( IJSDIM, NCTP )      !! cloud work function
   REAL(r8)     GPRCIZ( IJSDIM, KMAX+1 )    !! precipitation
   REAL(r8)     GSNWIZ( IJSDIM, KMAX+1 )    !! snowfall
   REAL(r8)     GTPRC0( IJSDIM       )      !! precip. before evap.

   REAL(r8)     GMFLX ( IJSDIM, KMAX+1 )    !! mass flux (updraft+downdraft)
   REAL(r8)     QLIQ  ( IJSDIM, KMAX   )    !! total cloud liquid
   REAL(r8)     QICE  ( IJSDIM, KMAX   )    !! total cloud ice
   REAL(r8)     GPRCI ( IJSDIM, KMAX   )    !! rainfall generation
   REAL(r8)     GSNWI ( IJSDIM, KMAX   )    !! snowfall generation

   REAL(r8)     GPRCP ( IJSDIM, KMAX+1 )    !! rainfall flux
!
   REAL(r8)     GTEVP ( IJSDIM, KMAX   )    !! evaporation+sublimation
   REAL(r8)     GMDD  ( IJSDIM, KMAX+1 )    !! downdraft mass flux

   REAL(r8)     CUMHGT( IJSDIM, NCTP   )    !! cloud top height
   REAL(r8)     CTOPP ( IJSDIM         )    !! cloud top pressure

   REAL(r8)     GDZTR ( IJSDIM         )   !! tropopause height
   REAL(r8)     FLIQOU( IJSDIM, KMAX   )   !! liquid ratio in cumulus
!#ifdef OPT_CHASER
!      REAL(r8)     TOPFLX( IJSDIM, NCTP   )    !! flux at each cloud top
!#endif
   INTEGER    KB    ( IJSDIM )
   INTEGER    KSTRT ( IJSDIM ) !! tropopause level
   REAL(r8)   GAMX
   REAL(r8)   CIN   ( IJSDIM )
   INTEGER    JBUOY ( IJSDIM )
   REAL(r8)   DELZ, BUOY, DELWC, DELER
!M REAL(r8)   WCB   ( NCTP )                !! updraft velocity**2 @base
!M SAVE       WCB
   REAL(r8)   WCBX (IJSDIM)
   REAL(r8)   ERMR  ( NCTP )                !! entrainment rate (ASMODE)
   SAVE       ERMR
   INTEGER    KTMX  ( NCTP )                !! max of cloud top
   INTEGER    KTMXT                         !! max of cloud top
   REAL(r8)   TIMED
   REAL(r8)   GDCLDX, GDMU2X, GDMU3X
!
   LOGICAL    OOUT1, OOUT2
   INTEGER    KBMX, I, K, CTP, ierr, n, kp1

   REAL(r8)     HBGT ( IJSDIM )     !! imbalance in column heat
   REAL(r8)     WBGT ( IJSDIM )     !! imbalance in column water
!
!  [INTERNAL PARM]
   REAL(r8) :: WCBMIN = 0._r8       !! min. of updraft velocity at cloud base
!M REAL(r8) :: WCBMAX = 1.4_r8      !! max. of updraft velocity at cloud base
!M wcbas commented by Moorthi since it is not used
!M REAL(r8) :: WCBAS  = 2._r8       !! updraft velocity**2 at cloud base (ASMODE)
!M REAL(r8) :: ERAMIN = 1.e-5_r8    !! min. of entrainment rate
                                    !! used only in OPT_ASMODE
!M REAL(r8) :: ERAMAX = 2.e-3_r8    !! max. of entrainment rate
                                    !! used only in OPT_ASMODE

   LOGICAL  :: OINICB = .false.     !! set 0.d0 to CBMFX

   REAL(r8) :: VARMIN = 1.e-13_r8   !! minimum of PDF variance
   REAL(r8) :: VARMAX = 5.e-7_r8    !! maximum of PDF variance
   REAL(r8) :: SKWMAX = 0.566_r8    !! maximum of PDF skewness

   REAL(r8) :: PSTRMX = 400.e2_r8   !! max P of tropopause
   REAL(r8) :: PSTRMN = 50.e2_r8    !! min P of tropopause
   REAL(r8) :: GCRSTR = 1.e-4_r8    !! crit. dT/dz tropopause

        ! 0: mass fixer is not applied
        !    tracers which may become negative values
        !    e.g. subgrid-PDFs
        ! 1: mass fixer is applied, total mass may change through cumulus scheme
        !    e.g. moisture, liquid cloud, ice cloud, aerosols
        ! 2: mass fixer is applied, total mass never change through cumulus scheme
        !    e.g. CO2
   real(kind=r8), parameter  :: zero=0.0, one=1.0
   real(kind=r8)             :: tem
!
   LOGICAL, SAVE :: OFIRST = .TRUE.   !! called first time?
!
!  [ONCE]
   IF ( OFIRST ) THEN
!DD  IF ( mrank == 0 ) &
!DD  WRITE ( iulog,* ) ' @@@ CHIKIRA-SUGIYAMA CUMULUS SCHEME'

     OFIRST = .FALSE.
     if (.not. allocated(OTSPT1)) ALLOCATE (OTSPT1(NTR))  !DD made module scope and allocatable
     if (.not. allocated(OTSPT2)) ALLOCATE (OTSPT2(NTR))  !DD made module scope and allocatable
     if (.not. allocated(IMFXR))  ALLOCATE (IMFXR(NTR))   !DD made module scope and allocatable

!    if (.not. allocated(FSCAV))  ALLOCATE (FSCAV(NTR))   !DD made module scope and allocatable
!    if (.not. allocated(FSWTR))  ALLOCATE (FSWTR(NTR))   !DD made module scope and allocatable
!    fscav = 0._r8
!    fswtr = 0._r8
!    write(0,*)' NTR in cs_conv=',ntr,' mype=',mype
!    do n=1,ntr
!      FSCAV(n) = 0._r8       !DD    split declaration and initialization
!      FSWTR(n) = 0._r8       !DD    split declaration and initialization
!    enddo

     IF ( OINICB ) THEN
!DD    IF ( mype == 0 ) WRITE ( iulog,*)' ### PCUMC: OINICB=T - DEFAULT USED: CBMFX', 0.D0
       CBMFX = 0.D0
     END IF
!
!DDdebug         IF ( NCTP < 1 ) &
!DDdebug            CALL endrun( 'CS_CUMLUS: NCTP must be positive.' )

!    DELWC  = ( WCBMAX-WCBMIN ) / DBLE( NCTP )
!    DO CTP = 1, NCTP
!       DELWC  = ( WCBMAXm(i)-WCBMIN ) / DBLE( NCTP )
!       WCB ( CTP ) = ( CTP*DELWC )**2
!    END DO

!DD#ifdef OPT_ASMODE
!DD         IF ( NCTP >= 2 ) THEN
!DD            IF ( ERAMIN <= 0._r8 ) &
!DD               CALL endrun( 'CS_CUMLUS: ERAMIN must be positive.' )
!DD            DELER  = LOG10( ERAMAX / ERAMIN ) / DBLE( NCTP-1 )
!DD            DO CTP = 1, NCTP
!DD               ERMR( CTP ) = ERAMAX*( 10.D0**( -DELER*( CTP-1 ) ) )
!DD            END DO
!DD         ELSE
!DD            ERMR( 1 ) = ERAMIN
!DD         END IF
!DD         IF ( mrank == 0 ) &
!DD            WRITE( iulog,* ) &
!DD              ' ### PCUMC: ERMR =', ( ERMR( CTP ), CTP=1, NCTP )
!DD#else

     ERMR = 0._r8

!DD         IF ( mype == 0 ) WRITE( iulog,* )' ### PCUMC: WCB =', ( WCB( CTP ), CTP=1, NCTP)
!DD#endif

     OTSPT1      = .false.
     OTSPT2      = .true.
     OTSPT2(1)   = .false.
     OTSPT2(ITL) = .false.
     OTSPT2(ITI) = .false.

     IMFXR( :  ) = 0
     IMFXR( 1  ) = 1
     IMFXR( ITL) = 1
     IMFXR( ITI) = 1
   END IF
!
   kp1 = kmax + 1
   do k=1,kmax
     do i=1,ijsdim
       gtt(i,k)     = zero
       gtu(i,k)     = zero
       gtv(i,k)     = zero
       gtq(i,k,:)   = zero
       gtqi(i,k)    = zero
       gtql(i,k)    = zero
       gmflx(i,k)   = zero
       gmfx0(i,k)   = zero
       gprci(i,k)   = zero
       gsnwi(i,k)   = zero
       qliq(i,k)    = zero
       qice(i,k)    = zero
       gtcfrc(i,k)  = zero
       cumclw(i,k)  = zero
       fliqc(i,k)   = zero
       fliqou(i,k)  = zero
       gprcpf(i,k)  = zero
       gsnwpf(i,k)  = zero
     enddo
   enddo
   do i=1,ijsdim
     gprcc(i,:)   = zero
     gmflx(i,kp1) = zero
     gmfx0(i,kp1) = zero
     gtprc0(i)    = zero
     hbgt(i)      = zero
     wbgt(i)      = zero
     gdztr(i)     = zero
     kstrt(i)     = kmax
   enddo

!#ifdef OPT_CHASER
!      TOPFLX = 0.D0
!      LNFRC  = 0.D0
!      REVC   = 0.D0
!      LEVCUM = 0
!#endif
   do k=1,kmax
     do i=1,ijsdim
       GDQI(i,k) = GDQ( i,k,ITI )
       GDW(i,k)  = GDQ( i,k,1 ) + GDQ( i,k,ITL ) + GDQI( i,k )
     enddo
   enddo
!  GDW  = GDQ( :,:,1 ) + GDQ( :,:,ITL ) + GDQI( :,: )
!
   DO K = 1, KMAX
     DO I = ISTS, IENS
       DELP ( I,K ) = GDPM( I,K ) - GDPM( I,K+1 )
       GDQS ( I,K ) = FQSAT( GDT( I,K ), GDP( I,K ) )
       FDQS ( I,K ) = FDQSAT( GDT( I,K ), GDQS( I,K ) )
       GAM  ( I,K ) = EL/CP*FDQS( I,K )
       GDS  ( I,K ) = CP*GDT( I,K ) + GRAV*GDZ( I,K )
       GDH  ( I,K ) = GDS( I,K ) + EL*GDQ( I,K,1 )
       GDHS ( I,K ) = GDS( I,K ) + EL*GDQS( I,K )
     END DO
   END DO
!
!        < tropopause >
!
   DO K = 1, KMAX
     DO I = ISTS, IENS
       GAMX = ( GDTM( I,K+1 )-GDTM( I,K ) ) / ( GDZM( I,K+1 )-GDZM( I,K ) )
       IF ( (GDP(I,K) < PSTRMX .AND. GAMX > GCRSTR) .OR. GDP(I,K) < PSTRMN) THEN
          KSTRT( I ) = MIN( K, KSTRT(I) )
       END IF
     END DO
   END DO
   DO I = ISTS, IENS
     K = KSTRT( I )
     GDZTR( I ) = GDZM( I,K )
   END DO
!
   CALL CUMBAS                                   & !! Cloud Base properties
              ( IJSDIM, KMAX  ,                  & !DD dimensions
               KB    , GCYM  , KBMX  ,           & ! output
               GCHB  , GCWB  , GCUB  , GCVB  ,   & ! output
               GCIB  ,                           & ! output
               GDH   , GDW   , GDHS  , GDQS  ,   & ! input
               GDQI  , GDU   , GDV   , GDZM  ,   & ! input
               GDPM  , FDQS  , GAM   ,           & ! input
               ISTS  , IENS                    )   ! input
!
   DO CTP = 1, NCTP
!
!DD#ifdef OPT_ASMODE
!DD         WCBX = WCBAS
!DD#else
!M    WCBX = WCB( CTP )
!DD#endif

      tem = ctp / DBLE( NCTP )
      do i=1,ijsdim
        DELWC  = tem *  (WCBMAXm(i) - WCBMIN)
        WCBX (I) = DELWC * DELWC
      enddo

      CALL CUMUP                                                     & !! In-cloud Properties
               ( IJSDIM, KMAX  , NTR   ,                             & !DD dimensions
                 ACWF(1,CTP) , ELAM(1,1,CTP),                        & ! output
                 GCLZ        , GCIZ        , GPRCIZ      , GSNWIZ,   & ! output
                 GCYT(1,CTP) , GCHT(1,CTP) , GCQT (1,CTP),           & ! output
                 GCLT(1,CTP) , GCIT(1,CTP) , GTPRT(1,CTP),           & ! output
                 GCUT(1,CTP) , GCVT(1,CTP) ,                         & ! output
                 KT  (1,CTP) , KTMX(CTP)   ,                         & ! output
                 GCYM  ,                                             & ! modified
                 GCHB  , GCWB  , GCUB  , GCVB  ,                     & ! input
                 GCIB  ,                                             & ! input
                 GDU   , GDV   , GDH   , GDW   ,                     & ! input
                 GDHS  , GDQS  , GDT   , GDTM  ,                     & ! input
                 GDQ   , GDQI  , GDZ   , GDZM  ,                     & ! input
                 GDPM  , FDQS  , GAM   , GDZTR ,                     & ! input
                 CPRES , WCBX  , ERMR(CTP),                          & ! input
                 KB    , CTP   , ISTS  , IENS   )                      ! input
!
      CALL CUMBMX                                                    & !! Cloud Base Mass Flux
               ( IJSDIM, KMAX  ,                                     & !DD dimensions
                 CBMFX(1,CTP),                                       & ! modified
                 ACWF (1,CTP), GCYT(1,CTP), GDZM     ,               & ! input
                 GDW         , GDQS       , DELP     ,               & ! input
                 KT   (1,CTP), KTMX(CTP)  , KB       ,               & ! input
                 DELTI       ,ISTS        , IENS       )
!
      CALL CUMFLX                                                    & !! Cloud Mass Flux & Precip.
               ( IJSDIM, KMAX  ,                                     & !DD dimensions
                 GMFX0 , GPRCI , GSNWI ,                             & ! output
                 QLIQ  , QICE  , GTPRC0,                             & ! output
!#ifdef OPT_CHASER
!     M          TOPFLX(1,CTP),                   ! <<CHEM>>
!#endif
                 CBMFX(1,CTP), GCYM        , GPRCIZ     , GSNWIZ ,   & ! input
                 GTPRT(1,CTP), GCLZ        , GCIZ       ,            & ! input
                 KB          , KT   (1,CTP), KTMX (CTP) ,            & ! input
                 ISTS        , IENS                               )    ! input
   END DO
!
   do k=1,kmax+1
     do i=ists,iens
       GMFLX( I,k ) = GMFX0( I,k )
     enddo
   enddo
   KTMXT = 3
   DO CTP = 1, NCTP
     IF ( KTMX( CTP ) > KTMXT ) KTMXT = KTMX( CTP )
   END DO
   DO K = 1, KTMXT
     DO I = ISTS, IENS
       CUMCLW( I,K ) = QLIQ( I,K ) + QICE( I,K )
       IF ( CUMCLW( I,K ) > zero ) THEN
            FLIQC( I,K )  = QLIQ( I,K ) / CUMCLW( I,K )
            FLIQOU( I,K ) = FLIQC( I,K )
       END IF
     END DO
   END DO
!
   CALL CUMCLD                                          & !! Cumulus Cloudiness
             ( IJSDIM, KMAX  ,                          & !DD dimensions
               CUMCLW, QLIQ  , QICE  , FLIQC  ,         & ! modified
               CUMFRC,                                  & ! output
!#ifdef OPT_CHASER
!     M        LEVCUM, LNFRC ,           ! <<CHEM>>
!     I        TOPFLX,                   ! <<CHEM>>
!#endif
               GMFLX , KTMXT , ISTS  , IENS    )    ! input
!
   CALL CUMDET                                           & !! Cloud Detrainment Heating
             ( im    , IJSDIM, KMAX  , NTR   ,           & !DD dimensions
               CMDET , GTLDET, GTIDET,                   & ! output
               GTT   , GTQ   , GTCFRC, GTU   , GTV   ,   & ! modified
               GTQI  ,                                   & ! modified
               GDH   , GDQ   , GDCFRC, GDU   , GDV   ,   & ! input
               CBMFX , GCYT  , DELP  , GCHT  , GCQT  ,   & ! input
               GCLT  , GCIT  , GCUT  , GCVT  , GDQI  ,   & ! input
               KT    , ISTS  , IENS                    )   ! input
!
   CALL CUMDWN                                           & !! Melt & Freeze & Evaporation
             ( IJSDIM, KMAX  , NTR   ,                   & !DD dimensions
               GTT   , GTQ   , GTU   , GTV   ,           & ! modified
               GTQI  , GMFLX ,                           & ! modified
               GPRCP , GSNWP , GTEVP , GMDD  ,           & ! output
!#ifdef OPT_CHASER
!     O        REVC  ,                   ! <<CHEM>>
!#endif
               GPRCI , GSNWI ,                           & ! input
               GDH   , GDW   , GDQ   , GDQI  ,           & ! input
               GDQS  , GDS   , GDHS  , GDT   ,           & ! input
               GDU   , GDV   , GDZ   ,                   & ! input
               GDZM  , GCYM  , FDQS  , DELP  ,           & ! input
               KB    , KTMXT , ISTS  , IENS    )           ! input
!
   do i=ISTS,IENS
      GPRCC( I,1 ) = GPRCP( I,1 )
      GSNWC( I   ) = GSNWP( I,1 )
   enddo
   do k=1,kmax+1
     do i=ISTS,IENS
       GTPRP( I,k ) = GPRCP( I,k ) + GSNWP( I,k )
     enddo
   enddo
!
   CALL CUMSBH                                           & !! Cloud Subsidence Heating
             ( IJSDIM, KMAX  , NTR   ,                   & !DD dimensions
               GTT   , GTQ   , GTQI  ,                   & ! modified
               GTU   , GTV   ,                           & ! modified
               GDH   , GDQ   , GDQI  ,                   & ! input
               GDU   , GDV   ,                           & ! input
               DELP  , GMFLX , GMFX0 ,                   & ! input
               KTMXT , CPRES , ISTS  , IENS )   ! input
!
   CALL CUMUPR                                           & !! Tracer Updraft
             ( im    , IJSDIM, KMAX  , NTR   ,           & !DD dimensions
               GTQ   , GPRCC ,                           & ! modified
               GDQ   , CBMFX , ELAM  , GDZ   , GDZM  ,   & ! input
               GCYM  , GCYT  , GCQT  , GCLT  , GCIT  ,   & ! input
               GTPRT , GTEVP , GTPRC0,                   & ! input
               KB    , KBMX  , KT    , KTMX  , KTMXT ,   & ! input
               DELP  , OTSPT1, ISTS  , IENS,             & ! input
               fscav, fswtr)
!
   CALL CUMDNR   & !! Tracer Downdraft
             ( IJSDIM, KMAX  , NTR   ,                   & !DD dimensions
               GTQ   ,                                   & ! modified
               GDQ   , GMDD  , DELP  ,                   & ! input
               KTMXT , OTSPT1, ISTS  , IENS )              ! input
!
   CALL CUMSBR                                           & !! Tracer Subsidence
             ( IJSDIM, KMAX  , NTR   ,                   & !DD dimensions
               GTQ   ,                                   & ! modified
               GDQ   , DELP  ,                           & ! input
               GMFLX , KTMXT , OTSPT2,                   & ! input
               ISTS  , IENS            )                   ! input
!
   do k=1,kmax
     do i=ISTS,IENS
       GTQ( I,k,ITI ) = GTQI( I,k )
     enddo
   enddo
!
   CALL CUMFXR                                           & !! Tracer mass fixer without detrainment
             ( IJSDIM, KMAX  , NTR   ,                   & !DD dimensions
               GTQ   ,                                   & ! modified
               GDQ   , DELP  , DELTA , KTMXT , IMFXR,    & ! input
               ISTS  , IENS                            )   ! input
!
   do k=1,kmax
     do i=ISTS,IENS
       GTQL( I,k ) = GTQ( I,k,ITL ) + GTLDET( I,k ) + GTIDET( I,k )
     enddo
   enddo
!
   CALL CUMFXR1                                                 & !! Tracer mass fixer with detrainment
              ( IJSDIM, KMAX  ,                                 & !DD dimensions
                GTQL        ,                                   & ! modified
                GDQ(1,1,ITL), DELP, DELTA, KTMXT, IMFXR(ITL),   & ! input
                ISTS        , IENS                            )   ! input
!
   DO K = 1, KMAX
     DO I = ISTS, IENS
       GTLDET( I,k ) = GTQL( I,k ) - GTQ( I,k,ITL ) - GTIDET( I,k )

! tendencies of subgrid PDF (turned off)
!      GDCLDX = GDCFRC( I,K ) + GTCFRC( I,K )*DELTA
!      GDCLDX = MIN( MAX( GDCLDX, 0.D0 ), 1.D0 )
!      GTCFRC( I,K ) = ( GDCLDX - GDCFRC( I,K ) )/DELTA
!
!      GDMU2X = GDQ( I,K,IMU2 ) + GTQ( I,K,IMU2 )*DELTA
!      GDMU2X = MIN( MAX( GDMU2X,VARMIN ),VARMAX )
!      GDMU3X = GDQ( I,K,IMU3 ) + GTQ( I,K,IMU3 )*DELTA
!      GDMU3X = MIN( MAX( GDMU3X,-SKWMAX ),SKWMAX )
!      GTQ( I,K,IMU2 ) = ( GDMU2X - GDQ( I,K,IMU2 ))/DELTA
!      GTQ( I,K,IMU3 ) = ( GDMU3X - GDQ( I,K,IMU3 ))/DELTA
!
       tem = DELP(I,K)*GRAVI
       HBGT( I ) = HBGT( I ) + ( CP*GTT(I,K) + EL*GTQ(I,K,1)                         &
                               - EMELT*(GTQ(I,K,ITI)+GTIDET(I,K)) ) * tem
       WBGT( I ) = WBGT( I ) + ( GTQ(I,K,1) + GTQ(I,K,ITL)                           &
                             +   GTQ(I,K,ITI) + GTLDET(I,K) + GTIDET(I,K) ) * tem
     END DO
   END DO
!
   DO I = ISTS, IENS
     HBGT( I ) = HBGT( I ) - EMELT*GSNWC( I )
     WBGT( I ) = WBGT( I ) + GPRCC( I,1 ) + GSNWC( I )
    CTOPP( I ) = 1.D6
   END DO
!
   DO CTP = 1, NCTP
     DO I = ISTS, IENS
       IF ( KT( I,CTP ) .GT. KB( I ) ) THEN
         CUMHGT ( I,CTP ) = GDZ( I,KT( I,CTP ) )
         CTOPP( I ) = MIN( CTOPP( I ),GDP( I,KT( I,CTP ) ))
       ELSE
         CUMHGT ( I,CTP ) = -999.D0
       END IF
     END DO
   END DO
   DO I = ISTS, IENS
     IF( CTOPP( I ) >= 1.D6 ) THEN
      CTOPP( I ) = -999.D0
    END IF
   END DO
!
   DO K = 1, KMAX
     DO I = ISTS, IENS
       GPRCPF( I,K ) = 0.5D0*( GPRCP( I,K )+GPRCP( I,K+1 ) )
       GSNWPF( I,K ) = 0.5D0*( GSNWP( I,K )+GSNWP( I,K+1 ) )
     END DO
   END DO
!COSP
!necessary?
   DO K = 1, KMAX
     DO I = ISTS, IENS
       QLIQC( I,K ) = QLIQ( I,K )
       QICEC( I,K ) = QICE( I,K )
     END DO
   END DO
!
!DD      CALL OUTFLD_CS('CSDLDET' , GTLDET       , IJSDIM, KMAX  , ICHNK ) 
!DD      CALL OUTFLD_CS('CSDIDET' , GTIDET       , IJSDIM, KMAX  , ICHNK ) 
!DD      CALL OUTFLD_CS('CSES'    , GDS          , IJSDIM, KMAX  , ICHNK ) 
!DD      CALL OUTFLD_CS('CSEH'    , GDH          , IJSDIM, KMAX  , ICHNK ) 
!DD      CALL OUTFLD_CS('CSEHS'   , GDHS         , IJSDIM, KMAX  , ICHNK ) 
!DD      CALL OUTFLD_CS('CSEQS'   , GDQS         , IJSDIM, KMAX  , ICHNK ) 
!DD      CALL OUTFLD_CS('CSCBMF01', CBMFX(1,NCTP), IJSDIM, 1     , ICHNK ) 
!DD      CALL OUTFLD_CS('CSCWF01' , ACWF(1,NCTP) , IJSDIM, 1     , ICHNK ) 
!DD      CALL OUTFLD_CS('CSHBGT'  , HBGT         , IJSDIM, 1     , ICHNK )
!DD      CALL OUTFLD_CS('CSWBGT'  , WBGT         , IJSDIM, 1     , ICHNK )
!
!      CALL HISTNN( GCYT, 'GCYT', 'norm mass-flux at top',
!     &             'kg/m**2/s', 'A', HCLAS, NCTP )
!      CALL HISTNN( ACWF,  'CWF',   'cloud work function',
!     &             'J/kg'     , 'A', HCLAS, NCTP )
!      CALL HISTNN( CBMFX, 'CBMFX', 'cloud-base mass flux',
!     &             'kg/m**2/s', 'A', HCLAS, NCTP )
!      CALL HISTNN( CUMHGT, 'CUMHGT', 'cloud top height',
!     &             'm'        , 'A', HCLAS, NCTP )
!      CALL HISTIN( CTOPP, 'CTOPP', 'cloud top pressure',
!     &                                      'Pa'       , 'ASFC', HCLAS )
!      CALL HISTIN( GMFLX, 'CMFLX', 'cloud mass flux',
!     &                                      'kg/m**2/s', 'AMLV', HCLAS )
!      CALL HISTIN( GMDD,  'CDFLX', 'downdraft mass flux',
!     &                                      'kg/m**2/s', 'AMLV', HCLAS )
!      CALL HISTIN( GPRCP, 'FRANC', 'cumulus rain flux',
!     &                                      'kg/m**2/s', 'AMLV', HCLAS )
!      CALL HISTIN( GPRCPF, 'FRANCF', 'cumulus rain flux',
!     &                                      'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTIN( GSNWP, 'FSNWC', 'cumulus snow flux',
!     &                                      'kg/m**2/s', 'AMLV', HCLAS )
!      CALL HISTIN( GSNWPF, 'FSNWCF', 'cumulus snow flux',
!     &                                      'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTIN( GPRCP, 'FPRCC', 'cumulus rain+snow flux',
!     &                                      'kg/m**2/s', 'AMLV', HCLAS )
!      CALL HISTAD( GSNWP, 'FPRCC', 1.D0 )
!      CALL HISTIN( GTEVP, 'EPRCC', 'cumulus rain+snow evap',
!     &                                      'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTIN( GDS,   'GDS',   'dry static energy',
!     &                                      'm**2/s**2', 'ALEV', HCLAS )
!      CALL HISTIN( GDH,   'GDH',   'moist static energy',
!     &                                      'm**2/s**2', 'ALEV', HCLAS )
!      CALL HISTIN( GDHS,  'GDHS',  'saturate moist static energy',
!     &                                      'm**2/s**2', 'ALEV', HCLAS )
!      CALL HISTRG( OOUT1, 'CAPE', 'conv. available potential energy',
!     &                                      'm**2/s**2', 'ASFC', HCLAS )
!      CALL HISTRG( OOUT2, 'CIN',  'conv. inhibition',
!     &                                      'm**2/s**2', 'ASFC', HCLAS )
!      CALL HISTIN( QLIQ,  'QLIQC', 'cumulus cloud liquid water',
!     &                                      'kg/kg',     'ALEV', HCLAS )
!      CALL HISTIN( QICE,  'QICEC', 'cumulus cloud ice',
!     &                                      'kg/kg',     'ALEV', HCLAS )
!      CALL HISTIN( FLIQOU, 'FLIQC', 'cumulus cloud liquid fraction',
!     &                                      '     ',     'ALEV', HCLAS )
!      CALL HISTIN( HBGT, 'DHBGTC', 'inbalance in column heat',
!     &                                    'J/m**2/s','ASFC', HCLAS )
!      CALL HISTIN( WBGT, 'DWBGTC', 'inbalance in column water',
!     &                                    'kg/m**2/s','ASFC', HCLAS )
!      CALL HISTIN( GDZTR, 'ZTROP', 'tropopause height',
!     &                                    'm', 'ASFC', HCLAS )
!
!  IF ( OOUT1 .OR. OOUT2 ) THEN
     CAPE  = 0.D0
     CIN   = 0.D0
     JBUOY = 0
     DO K = 2, KMAX
       DO I = ISTS, IENS
         IF ( K >= KB( I ) ) THEN
            BUOY = (GDH(I,1)-GDHS(I,K)) / ((one+EL/CP*FDQS(I,K)) * CP*GDT(I,K))
         ELSE
            BUOY = ( GDS( I,1 )-GDS( I,K ) ) / ( CP*GDT( I,K ) )
         END IF
            DELZ = GDZM( I,K+1 )-GDZM( I,K )
         IF ( BUOY > zero.AND.  JBUOY( I ) /=  0   ) THEN
            CAPE( I ) = CAPE( I ) + BUOY*GRAV*DELZ
              JBUOY( I ) = 2
         ELSE IF ( BUOY < zero .AND. JBUOY( I ) /= 2 ) THEN
            CIN( I ) = CIN( I ) - BUOY*GRAV*DELZ
            JBUOY( I ) = 1
         END IF
       END DO
     END DO
     DO I = ISTS, IENS
       IF ( JBUOY(I) /= 2 ) CIN( I ) = -999.D0
     END DO
!         CALL HISTAX( CAPE, 'CAPE', 1.D0, .FALSE. )
!         CALL HISTAX( CIN,  'CIN',  1.D0, .FALSE. )
!      END IF
!
!DD#ifdef OPT_CUMCHK
!DD      CALL CUMCHK   & !! check range of output values
!DD         ( IJSDIM, KMAX  , NTR   ,           & !DD dimensions
!DD           GTT   , GTQ   , GTU   , GTV   ,   & ! input
!DD           GTCFRC, GPRCC , GSNWC , CUMCLW,   & ! input
!DD           CUMFRC, FLIQC , GTPRP ,           & ! input
!DD           ISTS  , IENS                    )   ! input
!DD#endif

!DD provide GFS with a separate downdraft mass flux
     DO K = 1, KMAX+1
        DO I = ISTS, IENS
           GMFX1( I,K ) = GMFX0( I,K ) - GMFLX( I,K )
        END DO
     END DO
!
      END SUBROUTINE CS_CUMLUS
!***********************************************************************
      SUBROUTINE CUMBAS   & !! cloud base
               ( IJSDIM, KMAX  ,                   & !DD dimensions
                 KB    , GCYM  , KBMX  ,           & ! output
                 GCHB  , GCWB  , GCUB  , GCVB  ,   & ! output
                 GCIB  ,                           & ! output
                 GDH   , GDW   , GDHS  , GDQS  ,   & ! input
                 GDQI  , GDU   , GDV   , GDZM  ,   & ! input
                 GDPM  , FDQS  , GAM   ,           & ! input
                 ISTS  , IENS                    )   ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: IJSDIM, KMAX             !! DD, for GFS, pass in
!
!   [OUTPUT]
      INTEGER    KB    ( IJSDIM )         !! cloud base
      REAL(r8)   GCYM  ( IJSDIM, KMAX )   !! norm. mass flux (half lev)
      INTEGER    KBMX
      REAL(r8)   GCHB  ( IJSDIM )         !! cloud base MSE
      REAL(r8)   GCWB  ( IJSDIM )         !! cloud base total water
      REAL(r8)   GCUB  ( IJSDIM )         !! cloud base U
      REAL(r8)   GCVB  ( IJSDIM )         !! cloud base V
      REAL(r8)   GCIB  ( IJSDIM )         !! cloud base ice
!
!   [INPUT]
      REAL(r8)   GDH   ( IJSDIM, KMAX )        !! moist static energy
      REAL(r8)   GDW   ( IJSDIM, KMAX )        !! total water
      REAL(r8)   GDHS  ( IJSDIM, KMAX )        !! saturate MSE
      REAL(r8)   GDQS  ( IJSDIM, KMAX )        !! saturate humidity
      REAL(r8)   GDQI  ( IJSDIM, KMAX )        !! cloud ice
      REAL(r8)   GDU   ( IJSDIM, KMAX )        !! u-velocity
      REAL(r8)   GDV   ( IJSDIM, KMAX )        !! v-velocity
      REAL(r8)   GDZM  ( IJSDIM, KMAX+1 )      !! Altitude (half lev)
      REAL(r8)   GDPM  ( IJSDIM, KMAX+1 )      !! pressure (half lev)
      REAL(r8)   FDQS  ( IJSDIM, KMAX )
      REAL(r8)   GAM   ( IJSDIM, KMAX )
      INTEGER    ISTS, IENS
!
!   [INTERNAL WORK]
      REAL(r8)   CBASE ( IJSDIM )            !! cloud base height
      REAL(r8)   CBASEP( IJSDIM )            !! cloud base pressure
      REAL(r8)   DELZ, QSL, GAMX
      INTEGER    I, K
!
!   [INTERNAL PARM]
      INTEGER :: KMAXM1
      INTEGER :: KLCLB                       !! LCL base level
      INTEGER :: KCB                         !! fix cloud bottom
      INTEGER :: KBMAX                       !! cloud base max
      INTEGER :: KBOFS                       !! cloud base offset

      KMAXM1 = KMAX-1
      KLCLB = 1                   !! LCL base level
      KCB   = 0                   !! fix cloud bottom
      KBMAX = KMAXM1              !! cloud base max
      KBOFS = 0                   !! cloud base offset
!
      GCYM( ISTS:IENS,: ) = 0.D0
!
      IF ( KCB > 0 ) THEN
         DO I = ISTS, IENS
            KB( I ) = KCB
         END DO
      ELSE
         DO I = ISTS, IENS
            KB( I ) = KBMAX
         END DO
         DO K = KBMAX-1, KLCLB+1, -1
            DO I = ISTS, IENS
               GAMX = FDQS( I,K )/( 1.D0+GAM( I,K ) )/CP
               QSL = GDQS( I,K ) &
                   + GAMX*( GDH( I,KLCLB )-GDHS( I,K ) )
               IF ( GDW( I,KLCLB ) .GE. QSL ) THEN
                  KB( I ) = K + KBOFS
               END IF
            END DO
         END DO
      END IF
!
      KBMX = 1
      DO I = ISTS, IENS
         KBMX = MAX( KBMX, KB( I ) )
         CBASE ( I ) = GDZM( I,KB( I ) )-GDZM( I,1 )
         CBASEP( I ) = GDPM( I,KB( I ) )
      END DO
!
      DO K = 1, KBMX
         DO I = ISTS, IENS
            IF ( K <= KB( I ) ) THEN
               GCYM( I,K ) = ( GDZM( I,K      ) - GDZM( I,1 ) ) &
                           / ( GDZM( I,KB(I) ) - GDZM( I,1 ) )
               GCYM( I,K ) = SQRT( GCYM( I,K ) )
            END IF
         END DO
      END DO
!
      DO I = ISTS, IENS
        GCHB( I ) = 0.D0
        GCWB( I ) = 0.D0
        GCUB( I ) = 0.D0
        GCVB( I ) = 0.D0
        GCIB( I ) = 0.D0
      END DO
!
      DO K = 1, KBMX
         DO I = ISTS, IENS
            IF ( K < KB( I ) ) THEN
               DELZ      = GCYM( I,K+1 ) - GCYM( I,K )
               GCHB( I ) = GCHB( I ) + DELZ * GDH ( I,K )
               GCWB( I ) = GCWB( I ) + DELZ * GDW ( I,K )
               GCUB( I ) = GCUB( I ) + DELZ * GDU ( I,K )
               GCVB( I ) = GCVB( I ) + DELZ * GDV ( I,K )
               GCIB( I ) = GCIB( I ) + DELZ * GDQI( I,K )
            END IF
         END DO
      END DO
!
!      CALL HISTIN( CBASE , 'CBASE' , 'cloud base height',
!     &             'm'  , 'ASFC' , HCLAS )
!      CALL HISTIN( CBASEP, 'CBASEP', 'cloud base pressure',
!     &             'Pa' , 'ASFC' , HCLAS )
!
      END SUBROUTINE CUMBAS
!***********************************************************************
      SUBROUTINE CUMUP   & !! in-cloud properties
               ( IJSDIM, KMAX  , NTR   ,           & !DD dimensions
                 ACWF  , ELAM  ,                   & ! output
                 GCLZ  , GCIZ  , GPRCIZ, GSNWIZ,   & ! output
                 GCYT  , GCHT  , GCQT  ,           & ! output
                 GCLT  , GCIT  , GTPRT ,           & ! output
                 GCUT  , GCVT  ,                   & ! output
                 KT    , KTMX  ,                   & ! output
                 GCYM  ,                           & ! modified
                 GCHB  , GCWB  , GCUB  , GCVB  ,   & ! input
                 GCIB  ,                           & ! input
                 GDU   , GDV   , GDH   , GDW   ,   & ! input
                 GDHS  , GDQS  , GDT   , GDTM  ,   & ! input
                 GDQ   , GDQI  , GDZ   , GDZM  ,   & ! input
                 GDPM  , FDQS  , GAM   , GDZTR ,   & ! input
                 CPRES , WCB   , ERMR  ,           & ! input
                 KB    , CTP   , ISTS  , IENS    )   ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!DD      use constituents, only: NTR => pcnst
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJSDIM, KMAX, NTR             !! DD, for GFS, pass in
!
!   [OUTPUT]
      REAL(r8)   ACWF  ( IJSDIM         )   !! cloud work function
      REAL(r8)   ELAM  ( IJSDIM, KMAX   )   !! entrainment (rate*massflux)
      REAL(r8)   GCLZ  ( IJSDIM, KMAX   )   !! cloud liquid water*eta
      REAL(r8)   GCIZ  ( IJSDIM, KMAX   )   !! cloud ice*eta
      REAL(r8)   GPRCIZ( IJSDIM, KMAX   )   !! rain generation*eta
      REAL(r8)   GSNWIZ( IJSDIM, KMAX   )   !! snow generation*eta
      REAL(r8)   GCYT  ( IJSDIM         )   !! norm. mass flux @top
      REAL(r8)   GCHT  ( IJSDIM         )   !! cloud top MSE*eta
      REAL(r8)   GCQT  ( IJSDIM         )   !! cloud top moisture*eta
      REAL(r8)   GCLT  ( IJSDIM         )   !! cloud top liquid water*eta
      REAL(r8)   GCIT  ( IJSDIM         )   !! cloud top ice*eta
      REAL(r8)   GTPRT ( IJSDIM         )   !! cloud top (rain+snow)*eta
      REAL(r8)   GCUT  ( IJSDIM         )   !! cloud top u*eta
      REAL(r8)   GCVT  ( IJSDIM         )   !! cloud top v*eta
      INTEGER    KT    ( IJSDIM         )   !! cloud top
      INTEGER    KTMX                       !! max of cloud top
!
!   [MODIFIED]
      REAL(r8)   GCYM  ( IJSDIM, KMAX   )   !! norm. mass flux
!
!   [INPUT]
      REAL(r8)   GCHB  ( IJSDIM         )   !! MSE at cloud base
      REAL(r8)   GCWB  ( IJSDIM         )   !! total water @cloud base
      REAL(r8)   GCUB  ( IJSDIM         )   !! U at cloud base
      REAL(r8)   GCVB  ( IJSDIM         )   !! V at cloud base
      REAL(r8)   GCIB  ( IJSDIM         )   !! cloud ice at cloud base
      REAL(r8)   GDU   ( IJSDIM, KMAX   )   !! U
      REAL(r8)   GDV   ( IJSDIM, KMAX   )   !! V
      REAL(r8)   GDH   ( IJSDIM, KMAX   )   !! moist static energy
      REAL(r8)   GDW   ( IJSDIM, KMAX   )   !! total water
      REAL(r8)   GDHS  ( IJSDIM, KMAX   )   !! saturation MSE
      REAL(r8)   GDQS  ( IJSDIM, KMAX   )   !! saturation q
      REAL(r8)   GDT   ( IJSDIM, KMAX   )   !! T
      REAL(r8)   GDTM  ( IJSDIM, KMAX+1 )   !! T (half lev)
      REAL(r8)   GDQ   ( IJSDIM, KMAX, NTR )   !! q
      REAL(r8)   GDQI  ( IJSDIM, KMAX   )   !! cloud ice
      REAL(r8)   GDZ   ( IJSDIM, KMAX   )   !! z
      REAL(r8)   GDZM  ( IJSDIM, KMAX+1 )   !! z (half lev)
      REAL(r8)   GDPM  ( IJSDIM, KMAX+1 )   !! p (half lev)
      REAL(r8)   FDQS  ( IJSDIM, KMAX   )
      REAL(r8)   GAM   ( IJSDIM, KMAX   )
      REAL(r8)   GDZTR ( IJSDIM         )   !! tropopause height
      REAL(r8)   CPRES                      !! pres. fac. for cum. fric.
      REAL(r8)   WCB(ijsdim)                !! updraft velocity**2 @base
      REAL(r8)   ERMR                       !! entrainment rate (ASMODE)
      INTEGER    KB    ( IJSDIM         )
      INTEGER    CTP
      INTEGER    ISTS, IENS, kp1
!
!   [INTERNAL WORK]
      REAL(r8)     GCHMZ ( IJSDIM, KMAX+1 )   !! cloud h *eta (half lev)
      REAL(r8)     GCWMZ ( IJSDIM, KMAX+1 )   !! cloud Qt*eta (half lev)
      REAL(r8)     GCUMZ ( IJSDIM, KMAX+1 )   !! cloud U *eta (half lev)
      REAL(r8)     GCVMZ ( IJSDIM, KMAX+1 )   !! cloud V *eta (half lev)
      REAL(r8)     GCIMZ ( IJSDIM, KMAX+1 )   !! cloud Qi*eta (half lev)
      REAL(r8)     GTPRMZ( IJSDIM, KMAX+1 )   !! rain+snow *eta (half lev)
!
      REAL(r8)     BUOY  ( IJSDIM, KMAX   )   !! buoyancy
      REAL(r8)     BUOYM ( IJSDIM, KMAX+1 )   !! buoyancy (half lev)
      REAL(r8)     WCM   ( IJSDIM, KMAX+1 )   !! updraft velocity**2 (half lev)
      REAL(r8)     WCV   ( IJSDIM, KMAX+1 )   !! updraft velocity (half lev)
      REAL(r8)     GCY   ( IJSDIM, KMAX   )   !! norm. mass flux
      REAL(r8)     ELAR  ( IJSDIM, KMAX   )   !! entrainment rate
!
      REAL(r8)     GCHM  ( IJSDIM, KMAX+1 )   !! cloud MSE (half lev)
      REAL(r8)     GCWM  ( IJSDIM, KMAX+1 )   !! cloud Qt  (half lev)
      REAL(r8)     GCTM  ( IJSDIM, KMAX+1 )   !! cloud T (half lev)
      REAL(r8)     GCQM  ( IJSDIM, KMAX+1 )   !! cloud q (half lev)
      REAL(r8)     GCLM  ( IJSDIM, KMAX+1 )   !! cloud liquid ( half lev)
      REAL(r8)     GCIM  ( IJSDIM, KMAX+1 )   !! cloud ice (half lev)
      REAL(r8)     GCUM  ( IJSDIM, KMAX+1 )   !! cloud U (half lev)
      REAL(r8)     GCVM  ( IJSDIM, KMAX+1 )   !! cloud V (half lev)
!
      REAL(r8)     WCM_  ( IJSDIM         )
      REAL(r8)     ELARM1( IJSDIM         )
      REAL(r8)     GDZMKB( IJSDIM         )
      REAL(r8)     GDQSM, GDHSM, GDQM, GDCM, FDQSM, GCCM
      REAL(r8)     DELZ, ELADZ, DCTM , CPGM, DELC, FICE, ELARM2
      REAL(r8)     GCQMZ, GCCMZ, PRECR, GTPRIZ, DELZL
      REAL(r8)     GCWT, GCCT, DCT, WCVX
      REAL(r8)     PRCZH, wrk
      INTEGER      K, I, kk
      CHARACTER    CTNUM*2
!
!DD#ifdef OPT_CUMBGT
!DD      REAL(r8)     HBGT  ( IJSDIM )           !! heat budget
!DD      REAL(r8)     WBGT  ( IJSDIM )           !! water budget
!DD      REAL(r8)     PBGT  ( IJSDIM )           !! precipitation budget
!DD      REAL(r8)     MBGT  ( IJSDIM )           !! mass budget
!DD      REAL(r8)     GTPRX ( IJSDIM )           !! (rain+snow)*eta at top
!DD      REAL(r8)     GSNWT ( IJSDIM )           !! cloud top snow*eta
!DD      REAL(r8)     HBMX, WBMX, PBMX, MBMX
!DD      SAVE       HBMX, WBMX, PBMX, MBMX
!DD#endif
!
!   [INTERNAL PARAM]

      REAL(r8), SAVE :: CLMP
!DD      REAL(r8) ::  PRECZ0 = 1.5e3_r8    ! move to module scope for tuning
!DD      REAL(r8) ::  PRECZH = 4.e3_r8    ! move to module scope for tuning
      REAL(r8) ::  ZTREF  = 1._r8
      REAL(r8) ::  PB     = 1._r8
      REAL(r8) ::  TAUZ   = 1.e4_r8
      REAL(r8) ::  ELMD   = 2.4e-3     !! for Neggers and Siebesma (2002)
      REAL(r8) ::  ELAMIN = 0._r8      !! min. of entrainment rate
      REAL(r8) ::  ELAMAX = 4.e-3      !! max. of entrainment rate
      REAL(r8) ::  WCCRT  = 0._r8
      REAL(r8) ::  TSICE  = 268.15_r8  !! compatible with macrop_driver
      REAL(r8) ::  TWICE  = 238.15_r8  !! compatible with macrop_driver
      REAL(r8) ::  EPSln  = 1.e-10

      LOGICAL, SAVE :: OFIRST = .TRUE.
!
!   [INTERNAL FUNC]
      REAL(r8)     FPREC   !! precipitation ratio in condensate
      REAL(r8)     FRICE   !! ice ratio in cloud water
      REAL(r8)     Z       !! altitude
      REAL(r8)     ZH      !! scale height
      REAL(r8)     T       !! temperature
!
      FPREC( Z,ZH ) = MIN( MAX(1.D0-EXP(-(Z-PRECZ0)/ZH), 0.D0), 1.D0)
      FRICE( T ) = MIN( MAX( (TSICE-T)/(TSICE-TWICE), 0.D0 ), 1.D0 )
!
! Note: iteration is not made to diagnose cloud ice for simplicity
!
      IF ( OFIRST ) THEN
         CLMP = 2.D0*(1.D0-CLMD)*PA
         OFIRST = .FALSE.
      END IF

      kp1 = kmax + 1
      do i=ists,iens
        ACWF  (I) = 0.D0
        GCYT  (I) = 0.D0
        GCHT  (I) = 0.D0
        GCQT  (I) = 0.D0
        GCLT  (I) = 0.D0
        GCIT  (I) = 0.D0
        GTPRT (I) = 0.D0
        GCUT  (I) = 0.D0
        GCVT  (I) = 0.D0

        GCHMZ (I,kp1) = 0.D0
        GCWMZ (I,kp1) = 0.D0
        GCUMZ (I,kp1) = 0.D0
        GCVMZ (I,kp1) = 0.D0
        GTPRMZ(I,kp1) = 0.D0
!
        BUOYM (I,kp1) = unset_r8
        WCM   (I,kp1) = unset_r8
        WCV   (I,kp1) = unset_r8
!
        GCHM  (I,kp1) = unset_r8
        GCWM  (I,kp1) = unset_r8
        GCTM  (I,kp1) = unset_r8
        GCQM  (I,kp1) = unset_r8
        GCLM  (I,kp1) = unset_r8
        GCIM  (I,kp1) = unset_r8
        GCUM  (I,kp1) = unset_r8
        GCVM  (I,kp1) = unset_r8
      enddo
      do k=1,kmax
        do i=ists,iens
          ELAM  ( I,k) = unset_r8
          GCLZ  ( I,k ) = 0.D0
          GCIZ  ( I,k ) = 0.D0
          GPRCIZ( I,k ) = 0.D0
          GSNWIZ( I,k ) = 0.D0
!
          GCHMZ ( I,k ) = 0.D0
          GCWMZ ( I,k ) = 0.D0
          GCIMZ ( I,k ) = 0.D0
          GCUMZ ( I,k ) = 0.D0
          GCVMZ ( I,k ) = 0.D0
          GTPRMZ( I,k ) = 0.D0
!
          BUOY  ( I,k ) = unset_r8
          BUOYM ( I,k ) = unset_r8
          WCM   ( I,k ) = unset_r8
          WCV   ( I,k ) = unset_r8
          GCY   ( I,k ) = unset_r8
          ELAR  ( I,k ) = unset_r8
!
          GCHM  ( I,k ) = unset_r8
          GCWM  ( I,k ) = unset_r8
          GCTM  ( I,k ) = unset_r8
          GCQM  ( I,k ) = unset_r8
          GCLM  ( I,k ) = unset_r8
          GCIM  ( I,k ) = unset_r8
          GCUM  ( I,k ) = unset_r8
          GCVM  ( I,k ) = unset_r8
        enddo
      enddo

!#ifdef SYS_SX
      DO K = 1, KMAX
         DO I = ISTS, IENS
            IF ( K > KB( I ) ) THEN
               GCYM( I,K ) = 0.D0
            END IF
         END DO
      END DO
!#else
!      DO I = ISTS, IENS
!         GCYM( I,KB(I)+1:KMAX ) = 0.D0
!      END DO
!#endif
      DO I = ISTS, IENS
         GDZMKB( I ) = GDZM( I,KB(I) )
      END DO
!
!     < cloud base properties >
!
      DO I = ISTS, IENS
         K = KB( I )
         GCHM( I,K ) = GCHB( I )
         GCWM( I,K ) = GCWB( I )
         WCM ( I,K ) = WCB(i)
         GCUM( I,K ) = GCUB( I )
         GCVM( I,K ) = GCVB( I )
!
         GDQSM = FQSAT( GDTM( I,K ), GDPM( I,K ) )
         GDHSM = CP*GDTM( I,K ) + GRAV*GDZMKB( I ) + EL*GDQSM
         FDQSM = FDQSAT( GDTM( I,K ), GDQSM )
!
         DCTM  = ( GCHM( I,K ) - GDHSM )/( CP+EL*FDQSM )
         GCTM( I,K )  = GDT( I,K ) + DCTM
         GCQM( I,K )  = GDQSM + FDQSM*DCTM
         GCQM( I,K )  = MIN( GCQM( I,K ), GCWM( I,K ) )
         GCCM          = MAX( GCWM( I,K )-GCQM( I,K ), 0.D0 )
!
         GCIM( I,K ) = FRICE( GCTM( I,K ) )*GCCM
         GCLM( I,K ) = MAX( GCCM-GCIM( I,K ), 0.D0 )
         GCHM( I,K ) = GCHM( I,K )+EMELT*( GCIM( I,K )-GCIB( I ) )
         DCTM        = ( GCHM( I,K ) - GDHSM )/( CP+EL*FDQSM )
         GCTM( I,K ) = GDT( I,K ) + DCTM
!
         GDQM  = 0.5D0*( GDQ( I,K,1 )     + GDQ( I,K-1,1 ) )
         GDCM  = 0.5D0*( GDQ( I,K,ITL )   + GDQI( I,K )                &
                       + GDQ( I,K-1,ITL ) + GDQI( I,K-1 ) )
!
         BUOYM( I,K ) = ( DCTM/GDTM( I,K ) + EPSVT*( GCQM(I,K)-GDQM )-GCCM+GDCM )*GRAV
!
!DD#ifdef OPT_ASMODE
!DD         ELARM1( I ) = ERMR
!DD#elif defined OPT_NS02
!DD         ELARM1( I ) = ELMD/SQRT( WCM( I,K ) )
!DD#else
         ELARM1( I ) = CLMD*PA*BUOYM( I,K )/WCM( I,K )
!DD#endif
         ELARM1( I ) = MIN( MAX( ELARM1( I ), ELAMIN ), ELAMAX )
!
         GCHMZ ( I,K ) = GCHM( I,K )
         GCWMZ ( I,K ) = GCWM( I,K )
         GCUMZ ( I,K ) = GCUM( I,K )
         GCVMZ ( I,K ) = GCVM( I,K )
         GCIMZ ( I,K ) = GCIM( I,K )
         WCM_( I )  = WCM( I,K )
      END DO
!
!     < in-cloud properties >
!
      DO K = 3, KMAX
         DO I = ISTS, IENS
            IF ( K > KB( I ) .AND. WCM_( I ) > WCCRT ) THEN
               WCV( I,K-1 ) = SQRT( MAX( WCM_( I ), 0.D0 ) )
               DELZ  = GDZM( I,K ) - GDZM( I,K-1 )
               GCYM( I,K ) = EXP( ELARM1( I )*DELZ )*GCYM( I,K-1 )
               ELADZ = GCYM( I,K ) - GCYM( I,K-1 )
!
               GCHMZ( I,K ) = GCHMZ( I,K-1 ) + GDH( I,K-1 )*ELADZ
               GCWMZ( I,K ) = GCWMZ( I,K-1 ) + GDW( I,K-1 )*ELADZ
!
               GDQSM = FQSAT( GDTM( I,K ), GDPM( I,K ) )
               GDHSM = CP*GDTM( I,K )+GRAV*GDZM( I,K )+EL*GDQSM
               FDQSM = FDQSAT( GDTM( I,K ), GDQSM )
               CPGM  = CP + EL*FDQSM
               PRCZH = PRECZH * MIN( GDZTR( I ) / ZTREF, 1.D0 )
               PRECR = FPREC( GDZM( I,K )-GDZMKB( I ),PRCZH )
!
               wrk   = 1.0 / GCYM( I,K )
               DCTM  = ( GCHMZ( I,K )*wrk - GDHSM )/CPGM
               GCQMZ = ( GDQSM+FDQSM*DCTM )*GCYM( I,K )
               GCQMZ = MIN( GCQMZ, GCWMZ( I,K ) )
               GTPRMZ( I,K ) = PRECR*( GCWMZ( I,K )-GCQMZ )
               GTPRMZ( I,K ) = MAX( GTPRMZ(I,K), GTPRMZ(I,K-1) )
               GCCMZ = GCWMZ( I,K )-GCQMZ-GTPRMZ( I,K )
               DELC  = MIN( GCCMZ, 0.D0 )
               GCCMZ = GCCMZ - DELC
               GCQMZ = GCQMZ + DELC
!
               FICE  = FRICE( GDTM( I,K )+DCTM )
               GCIMZ( I,K ) = FICE*GCCMZ
               GSNWIZ( I,K-1 ) = FICE*( GTPRMZ(I,K)-GTPRMZ(I,K-1) )
               GCHMZ( I,K ) = GCHMZ( I,K ) &
                 + EMELT*( GCIMZ( I,K   ) + GSNWIZ( I,K-1 ) &
                         - GCIMZ( I,K-1 ) - GDQI( I,K-1 )*ELADZ )
               DCTM  = ( GCHMZ( I,K )*wrk - GDHSM )/CPGM
!
               GDQM  = 0.5D0*( GDQ( I,K,1 ) + GDQ( I,K-1,1 ) )
               GDCM  = 0.5D0*( GDQ( I,K,ITL )+GDQI( I,K ) &
                             + GDQ( I,K-1,ITL )+GDQI( I,K-1 ) )
               GCQM( I,K )  = GCQMZ*wrk
               GCCM         = GCCMZ*wrk
!
               BUOYM( I,K ) = ( DCTM/GDTM( I,K ) &
                 + EPSVT*( GCQM(I,K)-GDQM )-GCCM+GDCM )*GRAV
               BUOY( I,K-1 ) = 0.5D0*( BUOYM( I,K )+BUOYM( I,K-1 ) )
!
!DD#ifdef OPT_ASMODE
!DD               WCM( I,K ) &
!DD                 = ( WCM_( I ) + 2.D0*PA*DELZ*BUOY( I,K-1 ) ) &
!DD                 / ( 1.D0 + 2.D0*PB*DELZ*ERMR )
!DD#elif OPT_NS02
!DD               WCM( I,K ) = WCM_( I ) &
!DD                 + 2.D0*DELZ*( PA*BUOYM( I,K-1 )-ELMD*WCV( I,K-1 ) )
!DD               WCM( I,K ) = MAX( WCM( I,K ), 0.D0 )
!DD               WCVX = SQRT( 0.5D0*( WCM( I,K )+WCM_( I ) ) )
!DD               WCM( I,K ) = WCM_( I )  + 2.D0*DELZ*( PA*BUOY( I,K-1 )-ELMD*WCVX )
!DD#else
               IF ( BUOY( I,K-1 ) > 0.D0 ) THEN
                  WCM ( I,K ) = ( WCM_( I ) + CLMP*DELZ*BUOY( I,K-1 ) ) / ( 1.D0 + DELZ/TAUZ )
               ELSE
                  WCM ( I,K ) = ( WCM_( I ) + 2.D0*PA*DELZ*BUOY( I,K-1 ) ) &
                              / ( 1.D0 + DELZ/TAUZ + 2.D0*DELZ*ELAMIN )
               END IF
!DD#endif
!
!DD#ifdef OPT_ASMODE
!DD               ELARM2 = ERMR
!DD#elif OPT_NS02
!DD               ELARM2 = ELMD/SQRT( MAX( WCM( I,K ), EPSln ) )
!DD#else
               ELARM2 = CLMD*PA*BUOYM( I,K )/MAX( WCM( I,K ), EPSln )
!DD#endif
               ELARM2        = MIN( MAX( ELARM2, ELAMIN ), ELAMAX )
               ELAR( I,K-1 ) = 0.5D0*( ELARM1( I ) + ELARM2 )
               GCYM( I,K )   = EXP( ELAR(I,K-1)*DELZ )*GCYM( I,K-1 )
               ELADZ         = GCYM( I,K ) - GCYM( I,K-1 )
               ELAM( I,K-1 ) = ELADZ/DELZ
!
               GCHMZ( I,K )  = GCHMZ( I,K-1 ) + GDH( I,K-1 )*ELADZ
               GCWMZ( I,K )  = GCWMZ( I,K-1 ) + GDW( I,K-1 )*ELADZ
               GCUMZ( I,K )  = GCUMZ( I,K-1 ) + GDU( I,K-1 )*ELADZ
               GCVMZ( I,K )  = GCVMZ( I,K-1 ) + GDV( I,K-1 )*ELADZ
!
               wrk           = 1.0 / GCYM( I,K )
               DCTM         = ( GCHMZ( I,K )*wrk - GDHSM )/CPGM
               GCQMZ        = ( GDQSM+FDQSM*DCTM )*GCYM( I,K )
               GCQMZ        = MIN( GCQMZ, GCWMZ( I,K ) )
               GTPRMZ( I,K ) = PRECR*( GCWMZ( I,K )-GCQMZ )
               GTPRMZ( I,K ) = MAX( GTPRMZ(I,K), GTPRMZ(I,K-1) )
               GCCMZ = GCWMZ( I,K )-GCQMZ-GTPRMZ( I,K )
               DELC  = MIN( GCCMZ, 0.D0 )
               GCCMZ = GCCMZ - DELC
               GCQMZ = GCQMZ + DELC
               GCCM         = GCCMZ*wrk
               GCQM( I,K )  = GCQMZ*wrk
!
               FICE  = FRICE( GDTM( I,K )+DCTM )
               GCIMZ( I,K ) = FICE*GCCMZ
               GCIM( I,K ) = GCIMZ( I,K )*wrk
               GCLM( I,K ) = MAX( GCCM-GCIM( I,K ), 0.D0 )
               GTPRIZ = GTPRMZ( I,K ) - GTPRMZ( I,K-1 )
               GSNWIZ( I,K-1 ) = FICE*GTPRIZ
               GPRCIZ( I,K-1 ) = ( 1.D0-FICE )*GTPRIZ
               GCHMZ( I,K ) = GCHMZ( I,K ) + EMELT*( GCIMZ( I,K ) + GSNWIZ( I,K-1 ) &
                                           - GCIMZ( I,K-1 ) - GDQI( I,K-1 )*ELADZ )
               GCHM( I,K ) = GCHMZ( I,K )*wrk
               DCTM  = ( GCHM( I,K )-GDHSM )/CPGM
               GCTM( I,K ) = GDTM( I,K ) + DCTM
!
               GCWM( I,K ) = GCWMZ( I,K )*wrk
               GCUM( I,K ) = GCUMZ( I,K )*wrk
               GCVM( I,K ) = GCVMZ( I,K )*wrk
               DELZL = GDZ( I,K-1 )-GDZM( I,K-1 )
               GCY ( I,K-1 ) = GCYM(I,K-1)*EXP( ELAR(I,K-1)*DELZL )
               GCLZ( I,K-1 ) = 0.5D0*( GCLM( I,K ) + GCLM( I,K-1 ) )  * GCY( I,K-1 )
               GCIZ( I,K-1 ) = 0.5D0*( GCIM( I,K ) + GCIM( I,K-1 ) ) * GCY( I,K-1 )
               IF ( BUOY( I,K-1 ) > 0.D0 ) THEN
                  ACWF( I ) = ACWF( I ) + BUOY( I,K-1 )*GCY( I,K-1 )*DELZ
               END IF
!
               ELARM1( I ) = ELARM2
               WCM_( I ) = WCM( I,K )
            END IF
         END DO
      END DO
!
!     < find cloud top >
!
      DO I = ISTS, IENS
        KT( I ) = -1
      enddo
      DO K = KMAX, 2, -1
         DO I = ISTS, IENS
            IF ( K > KB( I ) .AND. KT( I )  == -1                            &
                 .AND. BUOYM( I,K ) > 0.D0 .AND. WCM  ( I,K ) > WCCRT ) THEN
               KT( I ) = K
            END IF
         END DO
      END DO
!
      KTMX = 2
      DO I = ISTS, IENS
         IF ( KT( I ) > KTMX ) KTMX = KT( I )
      END DO
!
      DO I = ISTS, IENS
        kk = kt(i)
        IF ( KK > 0 ) THEN
          do k=kk+1,kmax
            GCYM  ( I,K) = 0.D0
          enddo
          do k=kk,kmax
            GCLZ  (I,K) = 0.D0
            GCIZ  (I,K) = 0.D0
            GPRCIZ(I,K) = 0.D0
            GSNWIZ(I,K) = 0.D0
          enddo
        ELSE
          do k=kb(i)+1,kmax
            GCYM  (I,K) = 0.D0
          enddo
          do k=1,kmax
            GCLZ  (I,k) = 0.D0
            GCIZ  (I,k) = 0.D0
            GPRCIZ(I,k) = 0.D0
            GSNWIZ(I,k) = 0.D0
          enddo
        END IF
      END DO
!
!     < cloud top properties >
!
      DO I = ISTS, IENS
         IF ( KT( I ) .GT. 0 ) THEN
            K = KT( I )
            GCYT( I ) = GCY( I,K )
            ELADZ = GCYT( I ) - GCYM( I,K )
            ELAM( I,K ) = ELADZ/( GDZ( I,K )-GDZM( I,K ) )
!
            GCHT( I ) = GCHMZ( I,K ) + GDH( I,K )*ELADZ
            GCWT      = GCWMZ( I,K ) + GDW( I,K )*ELADZ
            GCUT( I ) = GCUMZ( I,K ) + GDU( I,K )*ELADZ
            GCVT( I ) = GCVMZ( I,K ) + GDV( I,K )*ELADZ
!
            DCT  = ( GCHT( I )/GCYT( I ) - GDHS( I,K ) ) &
                 / ( CP*( 1.D0 + GAM( I,K ) ) )
            GCQT( I )   = ( GDQS( I,K ) + FDQS( I,K )*DCT ) * GCYT( I )
            GCQT( I )   = MIN( GCQT( I ), GCWT )
            PRCZH       = PRECZH * MIN( GDZTR( I ) / ZTREF, 1.D0 )
            GTPRT( I )  = FPREC( GDZ( I,K )-GDZMKB( I ), PRCZH ) * ( GCWT-GCQT( I ) )
            GTPRT( I )  = MAX( GTPRT( I ), GTPRMZ( I,K ) )
            GCCT        = GCWT-GCQT( I )-GTPRT( I )
            DELC        = MIN( GCCT, 0.D0 )
            GCCT        = GCCT - DELC
            GCQT( I )   = GCQT( I ) + DELC
!
            FICE = FRICE( GDT( I,K )+DCT )
            GCIT( I )   = FICE*GCCT
            GCLT( I )   = ( 1.D0-FICE )*GCCT
            GTPRIZ      = GTPRT( I ) - GTPRMZ( I,K )
            GPRCIZ(I,K) = ( 1.D0-FICE )*GTPRIZ
            GSNWIZ(I,K) = FICE*GTPRIZ
            GCHT( I )   = GCHT(I)                                                        &
                        + EMELT * (GCIT(I) + GSNWIZ(I,K) - GCIMZ(I,K) - GDQI(I,K)*ELADZ)
!
            GCUT( I )   = GCUT( I )*( 1.D0-CPRES ) + GCY( I,K )*GDU( I,K )*CPRES
            GCVT( I )   = GCVT( I )*( 1.D0-CPRES ) + GCY( I,K )*GDV( I,K )*CPRES
            GCLZ( I,K ) = GCLT( I )
            GCIZ( I,K ) = GCIT( I )
         END IF
      END DO
!
!DD#ifdef OPT_CUMBGT   /* budget check */
!DD      HBGT ( ISTS:IENS ) = 0.D0
!DD      WBGT ( ISTS:IENS ) = 0.D0
!DD      PBGT ( ISTS:IENS ) = 0.D0
!DD      MBGT ( ISTS:IENS ) = 0.D0
!DD      GTPRX( ISTS:IENS ) = 0.D0
!DD      GSNWT( ISTS:IENS ) = 0.D0
!DD!
!DD      IF ( CTP .EQ. 1 ) THEN
!DD         HBMX = 0.D0
!DD         WBMX = 0.D0
!DD         PBMX = 0.D0
!DD         MBMX = 0.D0
!DD      END IF
!DD!
!DD      DO K = 2, KMAX
!DD         DO I = ISTS, IENS
!DD            IF ( K .GE. KB( I ) .AND. K .LT. KT( I ) ) THEN
!DD               ELADZ = GCYM( I,K+1 ) - GCYM( I,K )
!DD               DELZ  = GDZM( I,K+1 ) - GDZM( I,K )
!DD               HBGT( I ) = HBGT( I ) + ( GDH( I,K )-EMELT*GDQI( I,K ) )*ELADZ
!DD               WBGT( I ) = WBGT( I ) + GDW( I,K )*ELADZ
!DD               MBGT( I ) = MBGT( I ) + ELAM( I,K )*DELZ
!DD               GTPRX( I ) = GTPRX( I ) + GPRCIZ( I,K ) + GSNWIZ( I,K )
!DD               GSNWT( I ) = GSNWT( I ) + GSNWIZ( I,K )
!DD            END IF
!DD         END DO
!DD      END DO
!DD!
!DD      DO I = ISTS, IENS
!DD         IF ( KT( I ) .GT. KB( I ) ) THEN
!DD            ELADZ = GCYT( I ) - GCYM( I,KT(I) )
!DD            DELZ  = GDZ( I,KT(I) )-GDZM( I,KT(I) )
!DD            GTPRX( I ) = GTPRX( I ) + GPRCIZ( I,KT(I) ) + GSNWIZ( I,KT(I) )
!DD            GSNWT( I ) = GSNWT( I ) + GSNWIZ( I,KT(I) )
!DD            HBGT( I )  = HBGT( I )  + GCHB( I ) - EMELT*GCIB( I )        &
!DD                       + ( GDH( I,KT(I) )-EMELT*GDQI( I,KT(I) ) ) *ELADZ &
!DD                       - ( GCHT(I)-EMELT*( GCIT(I)+GSNWT(I) ) )
!DD            WBGT( I ) = WBGT( I ) &
!DD                       + GCWB( I ) + GDW( I,KT(I) )*ELADZ &
!DD                       - GCQT( I ) - GCLT( I ) - GCIT( I ) &
!DD                       - GTPRT( I )
!DD            MBGT( I ) = MBGT( I ) + 1.D0 + ELAM( I,KT(I) )*DELZ &
!DD                       - GCYT( I )
!DD            PBGT( I ) = GTPRT( I ) - GTPRX( I )
!DD!
!DD            IF ( ABS( HBGT(I) ) .GT. ABS( HBMX ) ) HBMX = HBGT(I)
!DD            IF ( ABS( WBGT(I) ) .GT. ABS( WBMX ) ) WBMX = WBGT(I)
!DD            IF ( ABS( PBGT(I) ) .GT. ABS( PBMX ) ) PBMX = PBGT(I)
!DD            IF ( ABS( MBGT(I) ) .GT. ABS( MBMX ) ) MBMX = MBGT(I)
!DD         END IF
!DD      END DO
!DD!
!DD      IF ( CTP .EQ. NCTP ) THEN
!DD         WRITE( iulog,* ) &
!DD            '### CUMUP(rank=',irank,'): energy imbalance =', HBMX
!DD         WRITE( iulog,* ) &
!DD            '### CUMUP(rank=',irank,'): water imbalance =', WBMX
!DD         WRITE( iulog,* ) &
!DD            '### CUMUP(rank=',irank,'): precipitation imbalance =', PBMX
!DD         WRITE( iulog,* ) &
!DD            '### CUMUP(rank=',irank,'): mass imbalance =', MBMX
!DD      END IF
!DD#endif
!
!DD      CALL OUTFLD_CS('CSCH01', GCHM, IJSDIM, KMAX, ICHNK )
!
!      WRITE( CTNUM, '(I2.2)' ) CTP
!
!      CALL HISTIN( BUOY, 'BUOY'//CTNUM, 'cumulus buoyancy '//CTNUM,
!     &             'm/s**2', 'ALEV', HCLAS )
!      CALL HISTIN( ELAR, 'ELAR'//CTNUM, 'entrainment rate '//CTNUM,
!     &             '1/m', 'ALEV', HCLAS )
!      CALL HISTIN( GCHM, 'CUMH'//CTNUM,
!     &             'cum. moist static energy '//CTNUM,
!     &             'm**2/s**2', 'AMLV', HCLAS )
!      CALL HISTIN( GCWM, 'CUMW'//CTNUM, 'cumulus total water '//CTNUM,
!     &             'kg/kg', 'AMLV', HCLAS )
!      CALL HISTIN( WCV, 'WC'//CTNUM, 'updraft velocity '//CTNUM,
!     &             'm/s', 'AMLV', HCLAS )
!      CALL HISTIN( GCTM, 'CUMT'//CTNUM, 'cumulus temperature '//CTNUM,
!     &             'K', 'AMLV', HCLAS )
!      CALL HISTIN( GCQM, 'CUMQ'//CTNUM, 'cumulus water vapor '//CTNUM,
!     &             'kg/kg', 'AMLV', HCLAS )
!      CALL HISTIN( GCLM, 'CUML'//CTNUM, 'cumulus liquid '//CTNUM,
!     &             'kg/kg', 'AMLV', HCLAS )
!      CALL HISTIN( GCIM, 'CUMI'//CTNUM, 'cumulus ice '//CTNUM,
!     &             'kg/kg', 'AMLV', HCLAS )
!      CALL HISTIN( GCLM, 'CUMC'//CTNUM, 'cumulus cloud water '//CTNUM,
!     &             'kg/kg', 'AMLV', HCLAS )
!      CALL HISTAD( GCIM, 'CUMC'//CTNUM, 1.D0 )
!      CALL HISTIN( GCUM, 'CUMU'//CTNUM, 'cumulus U '//CTNUM,
!     &             'm/s', 'AMLV', HCLAS )
!      CALL HISTIN( GCVM, 'CUMV'//CTNUM, 'cumulus V '//CTNUM,
!     &             'm/s', 'AMLV', HCLAS )
!      CALL HISTIN( GPRCIZ, 'CUMRI'//CTNUM,
!     &             'cum. rain generation '//CTNUM,
!     &             'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTIN( GSNWIZ, 'CUMSI'//CTNUM,
!     &             'cum. snow generation '//CTNUM,
!     &             'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTIN( GPRCIZ, 'CUMPI'//CTNUM,
!     &             'cum. rain+snow generation '//CTNUM,
!     &             'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTAD( GSNWIZ, 'CUMPI'//CTNUM, 1.D0 )
!      CALL HISTIN( GCYM, 'CUMY'//CTNUM, 'norm. mass flux '//CTNUM,
!     &             '', 'AMLV', HCLAS )
!
      END SUBROUTINE CUMUP
!***********************************************************************
      SUBROUTINE CUMBMX   & !! cloud base mass flux
               ( IJSDIM, KMAX  ,           & !DD dimensions
                 CBMFX ,                   & ! modified
                 ACWF  , GCYT  , GDZM  ,   & ! input
                 GDW   , GDQS  , DELP  ,   & ! input
                 KT    , KTMX  , KB    ,   & ! input
                 DELT  , ISTS  , IENS    )   ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!
      IMPLICIT NONE
      
      INTEGER, INTENT(IN) :: IJSDIM, KMAX             !! DD, for GFS, pass in
!
!   [MODIFY]
      REAL(r8)     CBMFX ( IJSDIM )            !! cloud base mass flux
!
!   [INPUT]
      REAL(r8)     ACWF  ( IJSDIM )            !! cloud work function
      REAL(r8)     GCYT  ( IJSDIM )            !! norm mass flux @top
      REAL(r8)     GDZM  ( IJSDIM, KMAX+1 )    !! height
      REAL(r8)     GDW   ( IJSDIM, KMAX   )    !! total water
      REAL(r8)     GDQS  ( IJSDIM, KMAX   )    !! saturate humidity
      REAL(r8)     DELP  ( IJSDIM, KMAX   )    !! delt pressure
      INTEGER      KT    ( IJSDIM )            !! cloud top
      INTEGER      KTMX                        !! max. of cloud top
      INTEGER      KB    ( IJSDIM )            !! cloud base
      REAL(r8)     DELT                        !! time step
      INTEGER      ISTS, IENS
!
!   [INTERNAL WORK]
      REAL(r8)     QX    ( IJSDIM )
      REAL(r8)     QSX   ( IJSDIM )
      REAL(r8)     RHM   ( IJSDIM )
      INTEGER      I, K
      REAL(r8)     ALP
      REAL(r8)     FMAX1, wrk
!
!   [INTERNAL PARAM]
      REAL(r8) :: FMAX   = 1.5e-2_r8          !! maximum flux
      REAL(r8) :: RHMCRT = 0._r8              !! critical val. of RH@ all could
!     REAL(r8) :: RHMCRT = 0.5_r8             !! critical val. of RH@ all could
      REAL(r8) :: ALP1   = 0._r8
      REAL(r8) :: TAUD   = 1.e3_r8
      REAL(r8) :: ZFMAX  = 3.5e3_r8
      REAL(r8) :: ZDFMAX = 5.e2_r8
!     REAL(r8) :: FMAXP  = 2._r8
      REAL(r8) :: EPSln  = 1.e-10_r8
!
      do i=ists,iens
        qx(i)  = 0.0d0
        qsx(i) = 0.0d0
      enddo
!
      DO K = 1, KTMX
         DO I = ISTS, IENS
            IF ( K >= KB( I ) .AND. K <= KT( I ) ) THEN
               QX ( I ) = QX ( I ) + GDW ( I,K ) * DELP( I,K )
               QSX( I ) = QSX( I ) + GDQS( I,K ) * DELP( I,K )
            END IF
         END DO
      END DO
      DO I = ISTS, IENS
         RHM(I) = min(1.0d0, max(0.0d0, QX(I)/MAX(QSX(I),EPSln)))
      END DO
!
      wrk = 1.0d0 + delt/(taud+taud)
      DO I = ISTS, IENS
         IF ( KT(I) > KB(I) .AND. RHM(I) >= RHMCRT ) THEN
            ALP      = ALP0 + ALP1*( GDZM( I,KT(I) )-GDZM( I,KB(I) ) )
            FMAX1    = (1.D0 - TANH( (GDZM(I,1)-ZFMAX)/ZDFMAX) ) * 0.5D0
!           FMAX1    = FMAX * FMAX1**FMAXP
            FMAX1    = FMAX * FMAX1*FMAX1
!           CBMFX(I) = CBMFX(I) + MAX(ACWF(I), 0.D0)/(ALP+ALP)*DELT
!           CBMFX(I) = CBMFX(I) / ( 1.D0 + DELT/(TAUD+TAUD) )
            CBMFX(I) = (CBMFX(I) + MAX(ACWF(I), 0.D0)/(ALP+ALP)*DELT) * wrk
            CBMFX(I) = MIN(max(CBMFX(I), 0.D0), FMAX1/GCYT(I))
         ELSE
            CBMFX(I) = 0.D0
         END IF
      END DO
!
      END SUBROUTINE CUMBMX
!***********************************************************************
      SUBROUTINE CUMFLX                                   & !! cloud mass flux
                      ( IJSDIM, KMAX  ,                   & !DD dimensions
                        GMFLX , GPRCI , GSNWI ,           & ! output
                        QLIQ  , QICE  , GTPRC0,           & ! output
!#ifdef OPT_CHASER
!     M                 TOPFLX,                         ! <<CHEM>>
!#endif
                        CBMFX , GCYM  , GPRCIZ, GSNWIZ,   & ! input
                        GTPRT , GCLZ  , GCIZ  ,           & ! input
                        KB    , KT    , KTMX  ,           & ! input
                        ISTS  , IENS                    )   ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJSDIM, KMAX            !! DD, for GFS, pass in
!
!   [OUTPUT]
      REAL(r8)     GMFLX ( IJSDIM, KMAX+1 )     !! mass flux
      REAL(r8)     GPRCI ( IJSDIM, KMAX   )     !! rainfall generation
      REAL(r8)     GSNWI ( IJSDIM, KMAX   )     !! snowfall generation
      REAL(r8)     QLIQ  ( IJSDIM, KMAX   )     !! cloud liquid
      REAL(r8)     QICE  ( IJSDIM, KMAX   )     !! cloud ice
      REAL(r8)     GTPRC0( IJSDIM         )     !! precip. before evap.
!
!   [MODIFY]
!#ifdef OPT_CHASER
!      REAL(r8)     TOPFLX( IJSDIM         )     !! mass flux at cloud top
!#endif
!
!   [INPUT]
      REAL(r8)     CBMFX ( IJSDIM         )     !! cloud base mass flux
      REAL(r8)     GCYM  ( IJSDIM, KMAX   )     !! normalized mass flux
      REAL(r8)     GPRCIZ( IJSDIM, KMAX   )     !! precipitation/M
      REAL(r8)     GSNWIZ( IJSDIM, KMAX   )     !! snowfall/M
      REAL(r8)     GTPRT ( IJSDIM         )     !! rain+snow @top
      REAL(r8)     GCLZ  ( IJSDIM, KMAX   )     !! cloud liquid/M
      REAL(r8)     GCIZ  ( IJSDIM, KMAX   )     !! cloud ice/M
      INTEGER      KB    ( IJSDIM         )     !! cloud base
      INTEGER      KT    ( IJSDIM         )     !! cloud top
      INTEGER      KTMX                         !! max of cloud top
      INTEGER      ISTS, IENS
!
!   [INTERNAL WORK]
      INTEGER    I, K
!
!M    DO K = 1, KTMX
!M       DO I = ISTS, IENS
!M          GMFLX( I,K ) = GMFLX( I,K ) + GCYM( I,K )*CBMFX( I )
!M       END DO
!M    END DO
!
!#ifdef OPT_CHASER
!      DO I = ISTS, IENS
!         IF ( KT( I ) .GT. KB( I ) ) THEN
!           TOPFLX( I ) = GCYM( I,KT(I) )*CBMFX( I )
!         ELSE
!           TOPFLX( I ) = 0.D0
!         END IF
!      END DO
!#endif
!
      DO K = 1, KTMX
         DO I = ISTS, IENS
            GMFLX( I,K ) = GMFLX( I,K ) + GCYM( I,K )   * CBMFX( I )
            GPRCI( I,K ) = GPRCI( I,K ) + GPRCIZ( I,K ) * CBMFX( I )
            GSNWI( I,K ) = GSNWI( I,K ) + GSNWIZ( I,K ) * CBMFX( I )
            QLIQ( I,K )  = QLIQ ( I,K ) + GCLZ( I,K )   * CBMFX( I )
            QICE( I,K )  = QICE ( I,K ) + GCIZ( I,K )   * CBMFX( I )
         END DO
      END DO
!
      DO I = ISTS, IENS
         GTPRC0( I ) = GTPRC0( I ) + GTPRT( I )*CBMFX( I )
      END DO
!
!M    DO K = 1, KTMX
!M       DO I = ISTS, IENS
!M          QLIQ( I,K ) = QLIQ( I,K ) + GCLZ( I,K )*CBMFX( I )
!M          QICE( I,K ) = QICE( I,K ) + GCIZ( I,K )*CBMFX( I )
!M       END DO
!M    END DO
!
      END SUBROUTINE CUMFLX
!***********************************************************************
      SUBROUTINE CUMDET   & !! detrainment
               ( im    , IJSDIM, KMAX  , NTR   ,           & !DD dimensions
                 CMDET , GTLDET, GTIDET,                   & ! output
                 GTT   , GTQ   , GTCFRC, GTU   , GTV   ,   & ! modified
                 GTQI  ,                                   & ! modified
                 GDH   , GDQ   , GDCFRC, GDU   , GDV   ,   & ! input
                 CBMFX , GCYT  , DELP  , GCHT  , GCQT  ,   & ! input
                 GCLT  , GCIT  , GCUT  , GCVT  , GDQI  ,   & ! input
                 KT    , ISTS  , IENS                    )   ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!DD      use constituents, only: NTR => pcnst
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: im, IJSDIM, KMAX, NTR             !! DD, for GFS, pass in
!
!   [OUTPUT]
      REAL(r8)     CMDET ( IJSDIM, KMAX )   !! detrainment mass flux
      REAL(r8)     GTLDET( IJSDIM, KMAX )   !! cloud liquid tendency by detrainment
      REAL(r8)     GTIDET( IJSDIM, KMAX )   !! cloud ice tendency by detrainment
!
!   [MODIFY]
      REAL(r8)     GTT   ( IJSDIM, KMAX )   !! temperature tendency
      REAL(r8)     GTQ   ( IJSDIM, KMAX, NTR )   !! moisture tendency
      REAL(r8)     GTCFRC( IJSDIM, KMAX )   !! cloud fraction tendency
      REAL(r8)     GTU   ( IJSDIM, KMAX )   !! u tendency
      REAL(r8)     GTV   ( IJSDIM, KMAX )   !! v tendency
      REAL(r8)     GTQI  ( IJSDIM, KMAX )   !! cloud ice tendency
!
!   [INPUT]
      REAL(r8)     GDH   ( IJSDIM, KMAX )   !! moist static energy
      REAL(r8)     GDQ   ( IJSDIM, KMAX, NTR ) !! humidity qv
      REAL(r8)     GDCFRC( IJSDIM, KMAX )   !! cloud fraction
      REAL(r8)     GDU   ( IJSDIM, KMAX )
      REAL(r8)     GDV   ( IJSDIM, KMAX )
      REAL(r8)     DELP  ( IJSDIM, KMAX )
      REAL(r8)     CBMFX ( IM,     NCTP )   !! cloud base mass flux
      REAL(r8)     GCYT  ( IJSDIM, NCTP )   !! detraining mass flux
      REAL(r8)     GCHT  ( IJSDIM, NCTP )   !! detraining MSE
      REAL(r8)     GCQT  ( IJSDIM, NCTP )   !! detraining qv
      REAL(r8)     GCLT  ( IJSDIM, NCTP )   !! detraining ql
      REAL(r8)     GCIT  ( IJSDIM, NCTP )   !! detraining qi
      REAL(r8)     GCUT  ( IJSDIM, NCTP )   !! detraining u
      REAL(r8)     GCVT  ( IJSDIM, NCTP )   !! detraining v
      REAL(r8)     GDQI  ( IJSDIM, KMAX )   !! cloud ice
      INTEGER      KT    ( IJSDIM, NCTP )   !! cloud top
      INTEGER      ISTS, IENS
!
!   [INTERNAL WORK]
      REAL(r8)     GTHCI, GTQVCI, GTQLCI, GTQICI
!M    REAL(r8)     GTCCI
!M    REAL(r8)     GTUCI, GTVCI
      REAL(r8)     GTXCI
      INTEGER      I, K, CTP, kk
!
!
!PARALLEL_FORBID

      do k=1,kmax
        DO I = ISTS, IENS
          CMDET ( I,k ) = 0.D0
          GTLDET( I,k ) = 0.D0
          GTIDET( I,k ) = 0.D0
        enddo
      enddo

!PARALLEL_FORBID
      DO CTP = 1, NCTP
         DO I = ISTS, IENS
            K = KT( I,CTP )
            IF ( K > 0 ) THEN
               GTXCI = GRAV/DELP( I,K )*CBMFX( I,CTP )
               GTHCI = GTXCI * ( GCHT(I,CTP) - GCYT(I,CTP)*GDH(I,K) )
               GTQVCI = GTXCI * ( GCQT(I,CTP) - GCYT(I,CTP)*GDQ(I,K,1) )
               GTQLCI = GTXCI * ( GCLT(I,CTP) - GCYT( I,CTP)*GDQ(I,K,ITL) )
               GTQICI = GTXCI * ( GCIT(I,CTP) - GCYT( I,CTP)*GDQI(I,K) )
!M             GTCCI  = GTXCI * ( GCYT(I,CTP) - GCYT(I,CTP)*GDCFRC(I,K) )
!M             GTUCI  = GTXCI * ( GCUT(I,CTP) - GCYT(I,CTP)*GDU(I,K) )
!M             GTVCI  = GTXCI * ( GCVT(I,CTP) - GCYT(I,CTP)*GDV(I,K) )
!
               GTQ(I,K,1)   = GTQ(I,K,1) + GTQVCI
               GTT(I,K  )   = GTT(I,K) + (GTHCI - EL*GTQVCI)/CP
! ql tendency by detrainment is treated by stratiform scheme
!              GTQ(I,K,ITL) = GTQ(I,K,ITL) + GTQLCI
               GTLDET(I,K)  = GTLDET(I,K) + GTQLCI
! qi tendency by detrainment is treated by stratiform scheme
!              GTQI  (I,K)  = GTQI(I,K)   + GTQICI
               GTIDET(I,K)  = GTIDET(I,K) + GTQICI
!M             GTCFRC(I,K)  = GTCFRC(I,K) + GTCCI
!M             GTU(I,K)     = GTU(I,K)    + GTUCI
!M             GTV(I,K)     = GTV(I,K)    + GTVCI

               GTCFRC(I,K)  = GTCFRC(I,K) + GTXCI * (GCYT(I,CTP) - GCYT(I,CTP)*GDCFRC(I,K))
               GTU(I,K)     = GTU(I,K)    + GTXCI * (GCUT(I,CTP) - GCYT(I,CTP)*GDU(I,K))
               GTV(I,K)     = GTV(I,K)    + GTXCI * (GCVT(I,CTP) - GCYT(I,CTP)*GDV(I,K))
!
               CMDET(I,K )  = CMDET(I,K) + GCYT( I,CTP )*CBMFX( I,CTP )
            END IF
         END DO
      END DO
!
!      CALL HISTIN( CMDET, 'CMDET', 'detrainment',
!     &             'kg/m**2/s', 'ALEV', HCLAS )
!
      END SUBROUTINE CUMDET
!***********************************************************************
      SUBROUTINE CUMSBH   & !! adiabat. descent
               ( IJSDIM, KMAX  , NTR   ,           & !DD dimensions
                 GTT   , GTQ   , GTQI  ,        & ! modified
                 GTU   , GTV   ,                & ! modified
                 GDH   , GDQ   , GDQI  ,        & ! input
                 GDU   , GDV   ,                & ! input
                 DELP  , GMFLX , GMFX0 ,        & ! input
                 KTMX  , CPRES , ISTS  , IENS )   ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!DD      use constituents, only: NTR => pcnst
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJSDIM, KMAX, NTR             !! DD, for GFS, pass in
!
!   [MODIFY]
      REAL(r8)     GTT   ( IJSDIM, KMAX )   !! Temperature tendency
      REAL(r8)     GTQ   ( IJSDIM, KMAX, NTR )   !! Moisture etc tendency
      REAL(r8)     GTQI  ( IJSDIM, KMAX )
      REAL(r8)     GTU   ( IJSDIM, KMAX )   !! u tendency
      REAL(r8)     GTV   ( IJSDIM, KMAX )   !! v tendency
!
!   [INPUT]
      REAL(r8)     GDH   ( IJSDIM, KMAX )
      REAL(r8)     GDQ   ( IJSDIM, KMAX, NTR )   !! humidity etc
      REAL(r8)     GDQI  ( IJSDIM, KMAX )
      REAL(r8)     GDU   ( IJSDIM, KMAX )
      REAL(r8)     GDV   ( IJSDIM, KMAX )
      REAL(r8)     DELP  ( IJSDIM, KMAX )
      REAL(r8)     GMFLX ( IJSDIM, KMAX+1 )   !! mass flux (updraft+downdraft)
      REAL(r8)     GMFX0 ( IJSDIM, KMAX+1 )   !! mass flux (updraft only)
      INTEGER      KTMX
      REAL(r8)     CPRES                    !! pressure factor for cumulus friction
      INTEGER      ISTS, IENS
!
!   [INTERNAL WORK]
      INTEGER      I, K, KM, KP
      REAL(r8)     SBH0, SBQ0, SBL0, SBI0, SBC0, SBS0
      REAL(r8)     SBH1, SBQ1, SBL1, SBI1, SBC1, SBS1, FX1
      REAL(r8)     SBU0, SBV0, SBU1, SBV1
      REAL(r8)     GTHCI, GTQVCI, GTQLCI, GTQICI
      REAL(r8)     GTM2CI, GTM3CI
!M    REAL(r8)     GTUCI, GTVCI, wrk, wrk1
      REAL(r8)     wrk, wrk1
      REAL(r8)     FX    ( ISTS:IENS )

      REAL(r8) :: GTLSBH( IJSDIM, KMAX )
      REAL(r8) :: GTISBH( IJSDIM, KMAX )
!
!
      FX = 0.D0
      GTLSBH = 0._r8
      GTISBH = 0._r8
!
      DO K = KTMX, 1, -1
         KM = MAX( K-1, 1    )
         KP = MIN( K+1, KMAX )
         DO I = ISTS, IENS
            SBH0 = GMFLX(I,K+1) * (GDH(I,KP)-GDH(I,K))
            SBQ0 = GMFLX(I,K+1) * (GDQ(I,KP,1)-GDQ(I,K,1))
            SBL0 = GMFLX(I,K+1) * (GDQ(I,KP,ITL )-GDQ(I,K,ITL))
            SBI0 = GMFLX(I,K+1) * (GDQI(I,KP)-GDQI(I,K))
            SBU0 = GMFLX(I,K+1) * (GDU(I,KP)-GDU(I,K))           &
                 - GMFX0(I,K+1) * (GDU(I,KP)-GDU(I,K))*CPRES
            SBV0 = GMFLX(I,K+1) * (GDV(I,KP)-GDV(I,K))           &
                 - GMFX0(I,K+1) * (GDV(I,KP)-GDV(I,K))*CPRES
!
            SBH1 = GMFLX(I,K) * (GDH(I,K)-GDH(I,KM))
            SBQ1 = GMFLX(I,K) * (GDQ(I,K,1)-GDQ(I,KM,1))
            SBL1 = GMFLX(I,K) * (GDQ(I,K,ITL)-GDQ(I,KM,ITL))
            SBI1 = GMFLX(I,K) * (GDQI(I,K)-GDQI(I,KM))
            SBU1 = GMFLX(I,K) * (GDU(I,K)-GDU(I,KM))            &
                 - GMFX0(I,K) * (GDU(I,K)-GDU(I,KM))*CPRES
            SBV1 = GMFLX(I,K) * (GDV(I,K)-GDV(I,KM))            &
                 - GMFX0(I,K) * (GDV(I,K)-GDV(I,KM))*CPRES
!
!#ifndef SYS_SX   /* original */
            IF ( GMFLX( I,K) > GMFLX( I,K+1) ) THEN
               FX1 = 0.5D0
            ELSE
               FX1 = 0.D0
            END IF
!#else            /* optimized for NEC SX series */
!            FX1 = 0.25D0 - SIGN(0.25D0,GMFLX(I,K+1)-GMFLX(I,K)) !! 0.5 or 0.
!#endif
!
            wrk    = GRAV / DELP(I,K)
            wrk1   = 1.D0 - FX(I)
            GTHCI  = wrk * (wrk1*SBH0 + FX1 *SBH1)
            GTQVCI = wrk * (wrk1*SBQ0 + FX1 *SBQ1)
            GTQLCI = wrk * (wrk1*SBL0 + FX1 *SBL1)
            GTQICI = wrk * (wrk1*SBI0 + FX1 *SBI1)
!M          GTUCI  = wrk * (wrk1*SBU0 + FX1 *SBU1)
!M          GTVCI  = wrk * (wrk1*SBV0 + FX1 *SBV1)
!
            GTT (I,K    ) = GTT(I,K)     +( GTHCI-EL*GTQVCI )/CP
            GTQ (I,K,1  ) = GTQ(I,K,1)   + GTQVCI
            GTQ (I,K,ITL) = GTQ(I,K,ITL) + GTQLCI
            GTQI(I,K)     = GTQI(I,K)    + GTQICI
!M          GTU (I,K)     = GTU(I,K)     + GTUCI
!M          GTV (I,K)     = GTV(I,K)     + GTVCI
            GTU (I,K)     = GTU(I,K)     + wrk * (wrk1*SBU0 + FX1*SBU1)
            GTV (I,K)     = GTV(I,K)     + wrk * (wrk1*SBV0 + FX1*SBV1)

            GTLSBH(I,K)   = GTQLCI
            GTISBH(I,K)   = GTQICI
!
!            SBC0 = GMFLX(I,K+1) * (GDQ(I,KP,IMU2)-GDQ(I,K,IMU2))
!            SBS0 = GMFLX(I,K+1) * (GDQ(I,KP,IMU3)-GDQ(I,K,IMU3))
!            SBC1 = GMFLX(I,K  ) * (GDQ(I,K,IMU2)-GDQ(I,KM,IMU2))
!            SBS1 = GMFLX(I,K  ) * (GDQ(I,K,IMU3)-GDQ(I,KM,IMU3))
!            GTM2CI = GRAV/DELP( I,K )
!     &             *( ( 1.D0-FX( I ) )*SBC0 + FX1 *SBC1 )
!            GTM3CI = GRAV/DELP( I,K )
!     &             *( ( 1.D0-FX( I ) )*SBS0 + FX1 *SBS1 )
!            GTQ( I,K,IMU2 ) = GTQ( I,K,IMU2 ) + GTM2CI
!            GTQ( I,K,IMU3 ) = GTQ( I,K,IMU3 ) + GTM3CI
!
            FX ( I )   = FX1
         enddo
      enddo
!
!DD      CALL OUTFLD_CS('CSDLSB', GTLSBH, IJSDIM, KMAX, ICHNK )
!DD      CALL OUTFLD_CS('CSDISB', GTISBH, IJSDIM, KMAX, ICHNK )
!
      END SUBROUTINE CUMSBH
!***********************************************************************
      SUBROUTINE CUMDWN   & !! Freeze & Melt & Evaporation
               ( IJSDIM, KMAX  , NTR   ,           & !DD dimensions
                 GTT   , GTQ   , GTU   , GTV   ,   & ! modified
                 GTQI  , GMFLX ,                   & ! modified
                 GPRCP , GSNWP , GTEVP , GMDD  ,   & ! output
!#ifdef OPT_CHASER
!     O           REVC  ,                   ! <<CHEM>>
!#endif
                 GPRCI , GSNWI ,                   & ! input
                 GDH   , GDW   , GDQ   , GDQI  ,   & ! input
                 GDQS  , GDS   , GDHS  , GDT   ,   & ! input
                 GDU   , GDV   , GDZ   ,           & ! input
                 GDZM  , GCYM  , FDQS  , DELP  ,   & ! input
                 KB    , KTMX  , ISTS  , IENS    )   ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!DD      use constituents, only: NTR => pcnst
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJSDIM, KMAX, NTR             !! DD, for GFS, pass in
!
!   [MODIFY]
      REAL(r8)     GTT   ( IJSDIM, KMAX )   !! Temperature tendency
      REAL(r8)     GTQ   ( IJSDIM, KMAX, NTR )  !! Moisture etc tendency
      REAL(r8)     GTU   ( IJSDIM, KMAX )   !! u tendency
      REAL(r8)     GTV   ( IJSDIM, KMAX )   !! v tendency
      REAL(r8)     GTQI  ( IJSDIM, KMAX )   !! cloud ice tendency
      REAL(r8)     GMFLX ( IJSDIM, KMAX+1 ) !! mass flux
!
!   [OUTPUT]
      REAL(r8)     GPRCP ( IJSDIM, KMAX+1 ) !! rainfall flux
      REAL(r8)     GSNWP ( IJSDIM, KMAX+1 ) !! snowfall flux
      REAL(r8)     GTEVP ( IJSDIM, KMAX   ) !! evaporation+sublimation
      REAL(r8)     GMDD  ( IJSDIM, KMAX+1 ) !! downdraft mass flux
!#ifdef OPT_CHASER
!      REAL(r8)     REVC  ( IJSDIM, KMAX )   !! evapo. rate <<CHEM>>
!#endif
!
!   [INPUT]
      REAL(r8)     GPRCI ( IJSDIM, KMAX )   !! rainfall generation
      REAL(r8)     GSNWI ( IJSDIM, KMAX )   !! snowfall generation
      REAL(r8)     GDH   ( IJSDIM, KMAX )   !! moist static energy
      REAL(r8)     GDW   ( IJSDIM, KMAX )   !! total water
      REAL(r8)     GDQ   ( IJSDIM, KMAX, NTR )   !! humidity etc
      REAL(r8)     GDQI  ( IJSDIM, KMAX )   !! cloud ice
      REAL(r8)     GDQS  ( IJSDIM, KMAX )   !! saturate humidity
      REAL(r8)     GDS   ( IJSDIM, KMAX )   !! dry static energy
      REAL(r8)     GDHS  ( IJSDIM, KMAX ) !! saturate moist static energy
      REAL(r8)     GDT   ( IJSDIM, KMAX )   !! air temperature T
      REAL(r8)     GDU   ( IJSDIM, KMAX )   !! u-velocity
      REAL(r8)     GDV   ( IJSDIM, KMAX )   !! v-velocity
      REAL(r8)     GDZ   ( IJSDIM, KMAX )   !! altitude
      REAL(r8)     GDZM  ( IJSDIM, KMAX+1 ) !! altitude (half lev)
      REAL(r8)     GCYM  ( IJSDIM, KMAX )   !! norm. mass flux
      REAL(r8)     FDQS  ( IJSDIM, KMAX )
      REAL(r8)     DELP  ( IJSDIM, KMAX )
      INTEGER      KB    ( IJSDIM )
      INTEGER      KTMX
      INTEGER      ISTS, IENS
!
!   [INTERNAL WORK]
! Note: Some variables have 3-dimensions for the purpose of budget check.
      REAL(r8)     EVAPD ( IJSDIM, KMAX )      !! evap. in downdraft
      REAL(r8)     SUBLD ( IJSDIM, KMAX )      !! subl. in downdraft
      REAL(r8)     EVAPE ( IJSDIM, KMAX )      !! evap. in environment
      REAL(r8)     SUBLE ( IJSDIM, KMAX )      !! subl. in environment
      REAL(r8)     EVAPX ( IJSDIM, KMAX )      !! evap. env. to DD
      REAL(r8)     SUBLX ( IJSDIM, KMAX )      !! subl. env. to DD
      REAL(r8)     GMDDE ( IJSDIM, KMAX )      !! downdraft entrainment
      REAL(r8)     SNMLT ( IJSDIM, KMAX )      !! melt - freeze
      REAL(r8)     GCHDD ( IJSDIM, KMAX )      !! MSE detrainment
      REAL(r8)     GCWDD ( IJSDIM, KMAX )      !! water detrainment
      REAL(r8)     GTTEV ( IJSDIM, KMAX )      !! T tendency by evaporation
      REAL(r8)     GTQEV ( IJSDIM, KMAX )      !! q tendency by evaporation
      REAL(r8)     GCHD  ( ISTS:IENS )         !! downdraft MSE
      REAL(r8)     GCWD  ( ISTS:IENS )         !! downdraft q
      REAL(r8)     GCUD  ( ISTS:IENS )         !! downdraft u
      REAL(r8)     GCVD  ( ISTS:IENS )         !! downdraft v
      REAL(r8)     FSNOW ( ISTS:IENS )
      REAL(r8)     GMDDD ( ISTS:IENS )
      INTEGER      I, K
      REAL(r8)     FMELT( IJSDIM,KMAX )
      REAL(r8)     FEVP ( IJSDIM,KMAX )
      REAL(r8)     GDTW
      REAL(r8)     GCHX, GCTX, GCQSX, GTPRP, EVSU, GTEVE, LVIC
      REAL(r8)     DQW, DTW, GDQW, DZ, GCSD, FDET, GDHI
      REAL(r8)     GMDDX, GMDDMX
      REAL(r8)     GCHDX, GCWDX
      REAL(r8)     GCUDD, GCVDD
      REAL(r8)     GTHCI, GTQVCI, GTQLCI, GTQICI
!M    REAL(r8)     GTHCI, GTQVCI, GTQLCI, GTQICI, GTUCI, GTVCI
      real(r8)     wrk
!DD#ifdef OPT_CUMBGT
!DD      REAL(r8)     WBGT  ( ISTS:IENS )         !! water budget
!DD      REAL(r8)     HBGT  ( ISTS:IENS )         !! energy budget
!DD      REAL(r8)     DDWBGT( ISTS:IENS )         !! downdraft water budget
!DD      REAL(r8)     DDHBGT( ISTS:IENS )         !! downdraft energy budget
!DD      REAL(r8)     WMX, HMX, DDWMX, DDHMX
!DD#endif
!
!   [INTERNAL PARM]
      REAL(r8) :: TWSNOW = 273.15_r8   !! wet-bulb temp. rain/snow
      REAL(r8) :: FTMLT  = 4._r8       !! temp. factor for melt
      REAL(r8) :: GMFLXC = 5.e-2_r8    !! critical mass flux
      REAL(r8) :: VTERMS = 2._r8       !! terminal velocity of snowflake
      REAL(r8) :: MELTAU = 10._r8      !! melting timescale
!
      REAL(r8) :: EVAPR  = 0.3_r8      !! evaporation factor
      REAL(r8) :: REVPDD = 1._r8       !! max rate of DD to evapolation
      REAL(r8) :: RDDR   = 5.e-4_r8    !! DD rate (T0 R0 W0)^-1
      REAL(r8) :: RDDMX  = 0.5_r8      !! norm. flux of downdraft
      REAL(r8) :: VTERM  = 5._r8       !! term. vel. of precip.
      REAL(r8) :: EVATAU = 2._r8    !! evaporation/sublimation timescale
      REAL(r8) :: ZDMIN  = 5.e2_r8     !! min altitude of downdraft detrainment
!
! Note: It is assumed that condensate evaporates in downdraft air.
!
      GPRCP( ISTS:IENS,: ) = 0.D0
      GSNWP( ISTS:IENS,: ) = 0.D0
      GMDD ( ISTS:IENS,: ) = 0.D0
      GTEVP( ISTS:IENS,: ) = 0.D0
      EVAPD( ISTS:IENS,: ) = 0.D0
      SUBLD( ISTS:IENS,: ) = 0.D0
      EVAPE( ISTS:IENS,: ) = 0.D0
      SUBLE( ISTS:IENS,: ) = 0.D0
      EVAPX( ISTS:IENS,: ) = 0.D0
      SUBLX( ISTS:IENS,: ) = 0.D0
      GMDDE( ISTS:IENS,: ) = 0.D0
      SNMLT( ISTS:IENS,: ) = 0.D0
      GCHDD( ISTS:IENS,: ) = 0.D0
      GCWDD( ISTS:IENS,: ) = 0.D0
      GTTEV( ISTS:IENS,: ) = 0.D0
      GTQEV( ISTS:IENS,: ) = 0.D0
      GCHD ( ISTS:IENS ) = 0.D0
      GCWD ( ISTS:IENS ) = 0.D0
      GCUD ( ISTS:IENS ) = 0.D0
      GCVD ( ISTS:IENS ) = 0.D0
      FMELT( ISTS:IENS,: ) = 0.D0
      FEVP ( ISTS:IENS,: ) = 0.D0
!#ifdef OPT_CHASER
!      REVC ( ISTS:IENS,: )  = 0.D0
!#endif
!
      DO K = KTMX, 1, -1   ! loop A
         DO I = ISTS, IENS
            DZ   = GDZM( I,K+1 ) - GDZM( I,K )
            FEVP( I,K ) = ( 1.D0 - TANH( EVATAU*VTERM/DZ ) )
         END DO
!
!     < precipitation melt & freeze >
!
         DO I = ISTS, IENS
            GTPRP = GPRCP( I,K+1 ) + GSNWP( I,K+1 )
            IF ( GTPRP > 0.D0 ) THEN
               FSNOW( I ) = GSNWP(I,K+1) / GTPRP
            ELSE
               FSNOW( I ) = 0.D0
            END IF
            LVIC  = ( EL+EMELT*FSNOW( I ) )/CP
            GDTW  = GDT( I,K ) - LVIC*( GDQS( I,K ) - GDQ( I,K,1 ) ) &
                               / ( 1.D0 + LVIC*FDQS( I,K ) )
            IF ( GDTW  < TWSNOW ) THEN
               GSNWP( I,K ) = GSNWP( I,K+1 ) + GPRCI( I,K ) + GSNWI( I,K )
               GTTEV( I,K ) = EMELT/CP*GPRCI( I,K ) * GRAV/DELP( I,K )
               SNMLT( I,K ) = -GPRCI( I,K )
            ELSE
               DZ   = GDZM( I,K+1 ) - GDZM( I,K )
               FMELT( I,K ) = ( 1.D0 + FTMLT*( GDTW - TWSNOW ) ) &
                            * ( 1.D0 - TANH( GMFLX( I,K+1 )/GMFLXC ) ) &
                            * ( 1.D0 - TANH( VTERMS*MELTAU/DZ ) )
               FMELT( I,K ) = MAX( MIN( FMELT(I,K), 1.D0 ), 0.D0 )
               SNMLT( I,K ) = GSNWP( I,K+1 )*FMELT( I,K )
               GSNWP( I,K ) = GSNWP( I,K+1 )+GSNWI( I,K ) - SNMLT( I,K )
               GPRCP( I,K ) = GPRCP( I,K+1 )+GPRCI( I,K ) + SNMLT( I,K )
               GTTEV( I,K ) = -EMELT/CP*SNMLT( I,K ) * GRAV/DELP( I,K )
            END IF
         END DO
!
!     < downdraft >
!
         DO I = ISTS, IENS   ! loop B
            wrk = grav / delp(i,k)
            IF ( GMDD( I,K+1 ) > 0.D0 ) THEN
               GCHX  = GCHD( I )/GMDD( I,K+1 )
               GCTX  = GDT( I,K )  + ( GCHX-GDHS(I,K) )/( CP+EL*FDQS(I,K) )
               GCQSX = GDQS( I,K ) + FDQS( I,K )*( GCTX-GDT( I,K ) )
               GCQSX = GCQSX*GMDD( I,K+1 )
               EVSU  = MAX( GCQSX - GCWD( I ),0.D0 ) * FEVP( I,K )
               GTPRP = GPRCP( I,K ) + GSNWP( I,K )
               IF ( GTPRP > 0.D0 ) THEN
                  FSNOW( I ) = GSNWP( I,K )/GTPRP
               ELSE
                  FSNOW( I ) = 0.D0
               END IF
               EVAPD( I,K ) = EVSU*( 1.D0-FSNOW( I ) )
               EVAPD( I,K ) = MIN( EVAPD( I,K ), GPRCP( I,K ) )
               SUBLD( I,K ) = EVSU*FSNOW( I )
               SUBLD( I,K ) = MIN( SUBLD( I,K ), GSNWP( I,K ) )
               GPRCP( I,K ) = GPRCP( I,K ) - EVAPD( I,K )
               GSNWP( I,K ) = GSNWP( I,K ) - SUBLD( I,K )
               GCWD( I )    = GCWD( I ) + EVAPD( I,K ) + SUBLD( I,K )
               GCHD( I )    = GCHD( I ) - EMELT*SUBLD( I,K )
            END IF

            GMDD( I,K ) = GMDD( I,K+1 )
!
            LVIC = ( EL + EMELT*FSNOW( I ) )/CP
            DQW  = ( GDQS( I,K ) - GDW( I,K ) ) / ( 1.D0 + LVIC*FDQS(I,K) )
            DQW  = MAX( DQW, 0.D0 )
            DTW  = LVIC*DQW
            GDQW = GDW( I,K ) + DQW*FEVP( I,K )
            DZ   = GDZM( I,K+1 ) - GDZM( I,K )
!
            EVSU = EVAPR/VTERM*DQW*DZ * FEVP( I,K )
            EVAPE( I,K ) = EVSU*GPRCP( I,K )
            EVAPE( I,K ) = MIN( EVAPE( I,K ), GPRCP( I,K ) )
            SUBLE( I,K ) = EVSU*GSNWP( I,K )
            SUBLE( I,K ) = MIN( SUBLE( I,K ), GSNWP( I,K ) )
            GTEVP( I,K ) = EVAPD( I,K ) + SUBLD( I,K ) &
                          + EVAPE( I,K ) + SUBLE( I,K )
!
!#ifdef OPT_CHASER
!            GTPRP = GPRCP( I,K+1 )+GSNWP( I,K+1 )
!            IF ( GTPRP > 0.D0 ) THEN
!               REVC ( I,K )  = GTEVP( I,K )/GTPRP
!     $                          *wrk                    ! <<CHEM>>
!            END IF
!#endif
!
            GTPRP = GPRCP( I,K ) + GSNWP( I,K )
            GPRCP( I,K ) = GPRCP( I,K ) - EVAPE( I,K )
            GSNWP( I,K ) = GSNWP( I,K ) - SUBLE( I,K )
!
            GMDDD( I ) = 0.D0
            IF ( GDZ( I,K )-GDZM( I,1 ) > ZDMIN ) THEN
               GTEVE  = EVAPE( I,K )+SUBLE( I,K )
               GMDDMX = REVPDD*GTEVE/MAX( DQW, 1.D-10 )
               GMDDE( I,K ) = RDDR*( DTW*GTPRP*DELP( I,K ) )
               GMDDE( I,K ) = MAX( MIN( GMDDE(I,K), GMDDMX ), 0.D0 )
               GMDDX  = GMDD( I,K+1 ) + GMDDE( I,K )
               EVSU   = GMDDE( I,K )*DQW*FEVP( I,K )
               IF ( GTEVE > 0.D0 ) THEN
                  FSNOW( I ) = SUBLE( I,K )/GTEVE
               ELSE
                  FSNOW( I ) = 0.D0
               END IF
               EVAPX( I,K ) = ( 1.D0-FSNOW( I ) )*EVSU
               SUBLX( I,K ) = FSNOW( I )*EVSU
!
               IF ( GMDDX > 0.D0 ) THEN
                  GDHI  = GDH( I,K ) - EMELT*GDQI( I,K )
                  GCHDX = GCHD( I ) + GDHI*GMDDE(I,K) - EMELT*SUBLX( I,K )
                  GCWDX = GCWD( I ) + GDQW*GMDDE(I,K)
                  GCSD  = ( GCHDX - EL*GCWDX )/GMDDX
                  IF ( GCSD < GDS( I,K ) ) THEN
                     GCHD( I   ) = GCHDX
                     GCWD( I   ) = GCWDX
                     GCUD( I   ) = GCUD( I ) + GDU( I,K )*GMDDE( I,K )
                     GCVD( I   ) = GCVD( I ) + GDV( I,K )*GMDDE( I,K )
                     GMDD( I,K ) = GMDDX
                     EVAPE( I,K ) = EVAPE( I,K ) - EVAPX( I,K )
                     SUBLE( I,K ) = SUBLE( I,K ) - SUBLX( I,K )
                     EVAPD( I,K ) = EVAPD( I,K ) + EVAPX( I,K )
                     SUBLD( I,K ) = SUBLD( I,K ) + SUBLX( I,K )
                     GMDDD( I )  = 0.D0
                  ELSE
                     GMDDE( I,K ) = 0.D0
                     GMDDD( I   ) = GMDD( I,K+1 )
                  END IF
               END IF
            ELSE
               GMDDD( I ) = ( GDZM( I,K+1 )-GDZM( I,K ) )               &
                          / ( GDZM( I,K+1 )-GDZM( I,1 ) ) * GMDD( I,K+1 )
            END IF
!
            GMDDD( I ) = MAX( GMDDD(I), GMDD(I,K)-RDDMX*GMFLX(I,K) )
!
            IF ( GMDDD( I ) > 0.D0 ) THEN
               FDET = GMDDD( I )/GMDD( I,K )
               GCHDD( I,K ) = FDET*GCHD( I )
               GCWDD( I,K ) = FDET*GCWD( I )
               GCUDD = FDET*GCUD( I )
               GCVDD = FDET*GCVD( I )
!
               GTHCI  =  wrk * (GCHDD(I,K) - GMDDD(I)*GDH(I,K))
               GTQVCI =  wrk * (GCWDD(I,K) - GMDDD(I)*GDQ(I,K,1))
               GTQLCI = -wrk * GMDDD(I)*GDQ(I,K,ITL)
               GTQICI = -wrk * GMDDD(I )*GDQI(I,K)
!M             GTUCI  =  wrk * (GCUDD - GMDDD(I)*GDU(I,K))
!M             GTVCI  =  wrk * (GCVDD - GMDDD(I)*GDV(I,K))
!
               GTT (I,K)     = GTT(I,K)     + (GTHCI - EL*GTQVCI)/CP
               GTQ (I,K,1)   = GTQ(I,K,1)   + GTQVCI
               GTQ (I,K,ITL) = GTQ(I,K,ITL) + GTQLCI
               GTQI(I,K)     = GTQI(I,K)    + GTQICI
!M             GTU (I,K) = GTU(I,K) + GTUCI
!M             GTV (I,K) = GTV(I,K) + GTVCI
               GTU (I,K) = GTU(I,K) + wrk * (GCUDD - GMDDD(I)*GDU(I,K))
               GTV (I,K) = GTV(I,K) + wrk * (GCVDD - GMDDD(I)*GDV(I,K))
!
               GCHD(I  ) = GCHD(I)   - GCHDD(I,K)
               GCWD(I  ) = GCWD(I)   - GCWDD(I,K)
               GCUD(I  ) = GCUD(I)   - GCUDD
               GCVD(I  ) = GCVD(I)   - GCVDD
               GMDD(I,K) = GMDD(I,K) - GMDDD( I )
            END IF
         END DO   ! loop B
!
      END DO   ! loop A
!
      DO K = 1, KTMX
         DO I = ISTS, IENS
            wrk = GRAV/DELP( I,K )
            GTTEV( I,K ) = GTTEV( I,K )                              &
                         - ( EL*EVAPE(I,K)+( EL+EMELT )*SUBLE(I,K) ) &
                         * wrk * (1.0d0/cp)
            GTT( I,K )   = GTT( I,K ) + GTTEV( I,K )
!
            GTQEV( I,K ) = GTQEV( I,K ) + ( EVAPE(I,K)+SUBLE(I,K) ) * wrk
            GTQ( I,K,1 ) = GTQ( I,K,1 ) + GTQEV( I,K )
!
            GMFLX( I,K ) = GMFLX( I,K ) - GMDD( I,K )
         END DO
      END DO
!
!      CALL HISTIN( FMELT, 'FMELT', 'melting rate in cumdown',
!     &             '   ', 'ALEV', HCLAS )
!      CALL HISTIN( FEVP, 'FEVP', 'evap/subl factor',
!     &             '   ', 'ALEV', HCLAS )
!      CALL HISTIN( EVAPD, 'EVAPD', 'cum. rain evap. into DD',
!     &             'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTIN( SUBLD, 'SUBLD', 'cum. snow subl. into DD',
!     &             'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTIN( EVAPE, 'EVAPE', 'cum. rain evap. into env.',
!     &             'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTIN( SUBLE, 'SUBLE', 'cum. snow subl. into env.',
!     &             'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTIN( EVAPX, 'EVAPX', 'cum. rain evap. from env. into DD.',
!     &             'kg/m**2/s', 'ALEV', HCLAS )
!      CALL HISTIN( SUBLX, 'SUBLX', 'cum. snow subl. from env. into DD.',
!     &             'kg/m**2/s', 'ALEV', HCLAS )
!
!DD#ifdef OPT_CUMBGT   /* budget check */
!DD      WBGT( ISTS:IENS ) = 0.D0
!DD      HBGT( ISTS:IENS ) = 0.D0
!DD      DDWBGT( ISTS:IENS ) = 0.D0
!DD      DDHBGT( ISTS:IENS ) = 0.D0
!DD      WMX = 0.D0
!DD      HMX = 0.D0
!DD      DDWMX = 0.D0
!DD      DDHMX = 0.D0
!DD      DO K = 1, KMAX
!DD         DO IJ = ISTS, IENS
!DD            WBGT( IJ ) = WBGT( IJ ) + GPRCI( IJ,K ) + GSNWI( IJ,K ) &
!DD                       - EVAPD( IJ,K ) - SUBLD( IJ,K ) &
!DD                       - GTQEV( IJ,K )*DELP( IJ,K )/GRAV
!DD            HBGT( IJ ) = HBGT( IJ ) &
!DD                       + EL*EVAPE( IJ,K ) + EMELT*SNMLT( IJ,K ) &
!DD                       + ( EL+EMELT )*SUBLE( IJ,K ) &
!DD                       + CP*GTTEV( IJ,K )*DELP( IJ,K )/GRAV
!DD            DDWBGT( IJ ) = DDWBGT( IJ ) &
!DD                         + EVAPD( IJ,K ) + SUBLD( IJ,K ) &
!DD                         + GDW( IJ,K )*GMDDE( IJ,K ) &
!DD                         - GCWDD( IJ,K )
!DD            DDHBGT( IJ ) = DDHBGT( IJ ) &
!DD                         + ( GDH(IJ,K)-EMELT*GDQI(IJ,K) )*GMDDE(IJ,K) &
!DD                         - EMELT*SUBLD( IJ,K ) &
!DD                         - GCHDD( IJ,K )
!DD         END DO
!DD      END DO
!DD      DO IJ = ISTS, IENS
!DD         WBGT( IJ ) = WBGT( IJ ) - GPRCP( IJ,1 ) - GSNWP( IJ,1 )
!DD         IF ( ABS( WBGT(IJ) ) .GT. ABS( WMX ) ) WMX   = WBGT( IJ )
!DD         IF ( ABS( HBGT(IJ) ) .GT. ABS( HMX ) ) HMX   = HBGT( IJ )
!DD         IF ( ABS(DDWBGT(IJ)) .GT. ABS(DDWMX) ) DDWMX = DDWBGT(IJ)
!DD         IF ( ABS(DDHBGT(IJ)) .GT. ABS(DDHMX) ) DDHMX = DDHBGT(IJ)
!DD      END DO
!DD!
!DD      WRITE( iulog,* ) &
!DD         '### CUMDWN(rank=',irank,'): water imbalance =', WMX
!DD      WRITE( iulog,* ) &
!DD         '### CUMDWN(rank=',irank,'): energy imbalance =', HMX
!DD      WRITE( iulog,* ) &
!DD         '### CUMDWN(rank=',irank,'): downdraft water imbalance =', DDWMX
!DD      WRITE( iulog,* ) &
!DD         '### CUMDWN(rank=',irank,'): downdraft energy imbalance =', DDHMX
!DD#endif
!
      END SUBROUTINE CUMDWN
!***********************************************************************
      SUBROUTINE CUMCLD   & !! cloudiness
               ( IJSDIM, KMAX  ,                    & !DD dimensions
                 CUMCLW, QLIQ  , QICE  , FLIQC  ,   & ! modified
                 CUMFRC,                            & ! output
!#ifdef OPT_CHASER
!     M           LEVCUM, LNFRC ,           ! <<CHEM>>
!     I           TOPFLX,                   ! <<CHEM>>
!#endif
                 GMFLX , KTMX  , ISTS, IENS   )       ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJSDIM, KMAX             !! DD, for GFS, pass in
!
!   [OUTPUT]
      REAL(r8)     CUMFRC( IJSDIM )            !! cumulus cloud fraction
!
!   [MODIFY]
      REAL(r8)     CUMCLW( IJSDIM, KMAX   )    !! cloud water in cumulus
      REAL(r8)     QLIQ  ( IJSDIM, KMAX   )    !! cloud liquid
      REAL(r8)     QICE  ( IJSDIM, KMAX   )    !! cloud ice
      REAL(r8)     FLIQC ( IJSDIM, KMAX   )    !! liquid ratio in cumulus
!#ifdef OPT_CHASER
!      INTEGER      LEVCUM( IJSDIM, KMAX )      !!
!      REAL(r8)     LNFRC ( IJSDIM, KMAX )      !! fraction of each cloud
!#endif
!
!   [INPUT]
      REAL(r8)     GMFLX ( IJSDIM, KMAX+1 ) !! cumulus mass flux
      INTEGER      KTMX
!#ifdef OPT_CHASER
!      REAL(r8)     TOPFLX( IJSDIM, NCTP   ) !! mass flux at each cloud top
!#endif
      INTEGER      ISTS, IENS
!
!   [WORK]
      INTEGER      I, K
      REAL(r8)     CUMF, QC, wrk
!#ifdef OPT_CHASER
!      REAL(r8)     SNFRC( ISTS:IENS   )
!      REAL(r8)     LNF
!#endif
      LOGICAL, SAVE :: OFIRST = .TRUE.
!
!   [INTERNAL PARAM]
      REAL(r8) :: FACLW  = 0.1_r8     !! Mc->CLW
      REAL(r8) :: CMFMIN = 2.e-3_r8   !! Mc->cloudiness
      REAL(r8) :: CMFMAX = 3.e-1_r8   !! Mc->cloudiness
      REAL(r8) :: CLMIN  = 1.e-3_r8   !! cloudiness Min.
      REAL(r8) :: CLMAX  = 0.1_r8     !! cloudiness Max.
      REAL(r8), SAVE :: FACLF
!
      IF ( OFIRST ) THEN
         FACLF = (CLMAX-CLMIN)/LOG(CMFMAX/CMFMIN)
         OFIRST = .FALSE.
      END IF
!
      CUMFRC( ISTS:IENS ) = 0.D0
      DO K = 1, KTMX
         DO I = ISTS, IENS
            CUMFRC( I ) = MAX( CUMFRC( I ), GMFLX( I,K ) )
         END DO
      END DO
      DO I = ISTS, IENS
         IF ( CUMFRC( I ) > 0.D0 ) THEN
            CUMF        = LOG( MAX( CUMFRC( I ),CMFMIN )/CMFMIN )
            CUMFRC( I ) = MIN( FACLF*CUMF+CLMIN, CLMAX )
         END IF
      END DO
!
      DO K = 1, KTMX
         DO I = ISTS, IENS
            IF ( GMFLX(I,K) > 0.D0 ) THEN
               wrk = FACLW / GMFLX( I,K) * CUMFRC(I)
               QLIQ  (I,K) = wrk * QLIQ(I,K)
               QICE  (I,K) = wrk * QICE(I,K)
               CUMCLW(I,K) = wrk * CUMCLW( I,K )
               QC = QLIQ( I,K ) + QICE( I,K )
               IF ( QC > 0.D0 ) THEN
                  FLIQC( I,K ) = QLIQ( I,K ) / QC
               END IF
            END IF
         END DO
      END DO
!
!#ifdef OPT_CHASER
! =  <<CHEM>> ========================================================
!
!      DO 3100 K  = 1    , KTMX
!      DO 3100 I = ISTS , IENS
!         IF ( TOPFLX( I,K ) .GT. CMFMIN*1.D-2 ) THEN
!            LEVCUM( I,K ) = 1
!         END IF
!
!         IF ( TOPFLX( I,K ) .GT. 0. ) THEN
!            LNF           = LOG( MAX(TOPFLX( I,K ),CMFMIN)/CMFMIN )
!            LNFRC( I,K ) = MIN( FACLF*LNF+CLMIN, CLMAX )
!         END IF
! 3100 CONTINUE
!
!      DO I = ISTS, IENS
!         SNFRC( I ) = 0.D0
!      END DO
!
!      DO K  = 1    , KTMX
!      DO I = ISTS , IENS
!         SNFRC( I ) = SNFRC( I ) + LNFRC( I,K )
!      END DO
!      END DO
!
!      DO K  = 1    , KTMX
!      DO I = ISTS , IENS
!         IF ( SNFRC( I ) .GT. 0.D0 ) THEN
!            LNFRC( I,K ) = LNFRC( I,K )*CUMFRC( I )/ SNFRC( I )
!         END IF
!      END DO
!      END DO
!#endif /* OPT_CHASER */
!
      END SUBROUTINE CUMCLD
!***********************************************************************
      SUBROUTINE CUMUPR   & !! Tracer Updraft
               ( im    , IJSDIM, KMAX  , NTR   ,           & !DD dimensions
                 GTR   , GPRCC ,                           & ! modified
                 GDR   , CBMFX , ELAM  , GDZ   , GDZM  ,   & ! input
                 GCYM  , GCYT  , GCQT  , GCLT  , GCIT  ,   & ! input
                 GTPRT , GTEVP , GTPRC0,                   & ! input
                 KB    , KBMX  , KT    , KTMX  , KTMXT ,   & ! input
                 DELP  , OTSPT , ISTS  , IENS,             & ! input
                 fscav, fswtr)
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!DD      use constituents, only: NTR => pcnst
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: im, IJSDIM, KMAX, NTR             !! DD, for GFS, pass in
!
!   [MODIFY]
      REAL(r8)     GTR   ( IJSDIM, KMAX, NTR )
      REAL(r8)     GPRCC ( IJSDIM, NTR  )
!
!   [INPUT]
      REAL(r8)     GDR   ( IJSDIM, KMAX, NTR )
      REAL(r8)     CBMFX ( IM,     NCTP   )
      REAL(r8)     ELAM  ( IJSDIM, KMAX, NCTP )
      REAL(r8)     GDZ   ( IJSDIM, KMAX   )
      REAL(r8)     GDZM  ( IJSDIM, KMAX+1 )
      REAL(r8)     GCYM  ( IJSDIM, KMAX   )
      REAL(r8)     GCYT  ( IJSDIM, NCTP   )
      REAL(r8)     GCQT  ( IJSDIM, NCTP   )
      REAL(r8)     GCLT  ( IJSDIM, NCTP   )
      REAL(r8)     GCIT  ( IJSDIM, NCTP   )
      REAL(r8)     GTPRT ( IJSDIM, NCTP   )
      REAL(r8)     GTEVP ( IJSDIM, KMAX   )
      REAL(r8)     GTPRC0( IJSDIM         )   !! precip. before evap.
      real(r8)     fscav(ntr), fswtr(ntr)
      INTEGER      KB    ( IJSDIM )
      INTEGER      KBMX
      INTEGER      KT    ( IJSDIM, NCTP   )
      INTEGER      KTMX  ( NCTP           )
      INTEGER      KTMXT
      REAL(r8)     DELP  ( IJSDIM, KMAX   )
      LOGICAL      OTSPT ( NTR )              !! transport with this routine?
      INTEGER      ISTS, IENS
!
!   [INTERNAL WORK]
      INTEGER      I, K, LT, TP, CTP
      REAL(r8)     GCRTD
      REAL(r8)     SCAV
      REAL(r8)     GCWT, GPRCR
      REAL(r8)     GCRB  ( ISTS:IENS )
      REAL(r8)     GCRT  ( ISTS:IENS )
      REAL(r8)     DR    ( ISTS:IENS )
      REAL(r8)     DGCB  ( ISTS:IENS, KMAX )
      REAL(r8)     DZ    ( ISTS:IENS, KMAX )
      REAL(r8)     DZT   ( ISTS:IENS, NCTP )
      REAL(r8)     RGCWT ( ISTS:IENS, NCTP )
      REAL(r8)     RDZM  ( ISTS:IENS, KMAX )
      REAL(r8)     EVPF  ( ISTS:IENS, KMAX )
      REAL(r8)     MASK1 ( ISTS:IENS, NCTP )
      REAL(r8)     MASK2 ( ISTS:IENS, NCTP )
!
!
      DO K = 1, KBMX
         DO I = ISTS, IENS
            DGCB( I,K ) = GCYM( I,K+1 ) - GCYM( I,K )
         END DO
      END DO
      DO K = 1, KTMXT
         DO I = ISTS, IENS
            DZ  ( I,K ) = GDZM( I,K+1 ) - GDZM( I,K )
            RDZM( I,K ) = GRAV / DELP( I,K )
            EVPF( I,K ) = 0.D0
            IF ( GTPRC0( I ) > 0.D0 ) THEN
               EVPF( I,K ) = GTEVP( I,K ) / GTPRC0( I )
            END IF
         END DO
      END DO
      DO CTP = 1, NCTP
         DO I = ISTS, IENS
            K = KT( I, CTP )
!
            GCWT = GCQT( I,CTP ) + GCLT( I,CTP ) + GCIT( I,CTP )
            RGCWT( I,CTP ) = 0.D0
            IF ( GCWT .GT. 0.D0 ) THEN
               RGCWT( I,CTP ) = 1.D0 / GCWT
            END IF
!
            MASK1( I,CTP ) = 0.D0
            DZT  ( I,CTP ) = 0.D0
            IF ( K > KB( I ) ) THEN
               MASK1( I,CTP ) = 1.D0
               DZT  ( I,CTP ) = GDZ( I,K ) - GDZM( I,K )
            END IF
            MASK2( I,CTP ) = 0.D0
            IF ( CBMFX( I,CTP ) > 0.D0 ) then
               MASK2( I,CTP ) = 1.D0
            END IF
         END DO
      END DO
!
      DO LT = 1, NTR   ! outermost loop
!
         IF ( OTSPT( LT ) ) THEN
            GCRB = 0.D0
            DO K = 1, KBMX
               DO I = ISTS, IENS
                  IF ( K .LT. KB( I ) ) THEN
                     GCRB( I ) = GCRB( I ) &
                                + DGCB( I,K ) * GDR( I,K,LT )
                  END IF
               END DO
            END DO
!
            DO CTP = 1, NCTP
               DR = 0.D0
               DO K = 2, KTMX( CTP )
                  DO I = ISTS, IENS
                     IF ( K .GE. KB( I     ) .AND. &
                          K .LT. KT( I,CTP ) ) THEN
                        DR( I ) = DR( I ) &
                                 + DZ( I,K ) * ELAM( I,K,CTP ) &
                                              * GDR ( I,K,LT  )
                     END IF
                  END DO
               END DO
!
               DO I = ISTS, IENS
                  K = MAX( KT( I,CTP ), 1 )
                  DR( I ) = DR( I ) &
                           + DZT( I,CTP ) * ELAM( I,K,CTP ) &
                                           * GDR ( I,K,LT  ) &
                                           * MASK1( I,CTP )
                  GCRT( I ) = ( GCRB( I ) + DR( I ) ) * MASK1( I,CTP )
!
                  SCAV = FSCAV( LT ) * GTPRT( I,CTP ) + FSWTR( LT ) &
                       * GTPRT( I,CTP ) * RGCWT( I,CTP )
                  SCAV  = MIN( SCAV, 1.D0 )
                  GCRTD = GCRT( I ) * ( 1.D0 - SCAV )
                  GPRCR = SCAV * GCRT( I ) * CBMFX( I,CTP )

                  GTR(I,K,LT) = GTR(I,K,LT) + RDZM(I,K) * CBMFX(I,CTP) &
                              * (GCRTD - GCYT(I,CTP) * GDR(I,K,LT)) * MASK2(I,CTP)
                  GPRCC(I,LT) = GPRCC(I,LT) + GPRCR * MASK2(I,CTP)
               END DO
            END DO
!
            DO K = KTMXT, 1, -1
               DO I = ISTS, IENS
                  GTR(I,K,LT) = GTR(I,K,LT) + RDZM(I,K) * GPRCC(I,LT) * EVPF(I,K)
                  GPRCC(I,LT) = GPRCC(I,LT) * (1.D0 - EVPF(I,K))
               END DO
            END DO
!
         END IF
!
      END DO   ! outermost loop
!
      END SUBROUTINE CUMUPR
!***********************************************************************
      SUBROUTINE CUMDNR                                 & !! Tracer Downdraft
                      ( IJSDIM, KMAX  , NTR   ,         & !DD dimensions
                        GTR   ,                         & ! modified
                        GDR   , GMDD  , DELP  ,         & ! input
                        KTMX  , OTSPT , ISTS  , IENS )    ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!DD      use constituents, only: NTR => pcnst
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJSDIM, KMAX, NTR             !! DD, for GFS, pass in
!
!   [MODIFY]
      REAL(r8)     GTR   ( IJSDIM, KMAX, NTR )   !! Temperature tendency
!
!   [INPUT]
      REAL(r8)     GDR   ( IJSDIM, KMAX, NTR )
      REAL(r8)     GMDD  ( IJSDIM, KMAX+1 )      !! downdraft mass flux
      REAL(r8)     DELP  ( IJSDIM, KMAX   )
      INTEGER      KTMX
      LOGICAL      OTSPT ( NTR )
      INTEGER      ISTS, IENS
!
!   [INTERNAL WORK]
      REAL(r8)     GCRD  ( ISTS:IENS )           !! downdraft q
      REAL(r8)     GMDDE, GMDDD, GCRDD
      INTEGER      I, K, LT
!
!
      DO LT = 1, NTR
!
         IF ( OTSPT( LT ) ) THEN
            GCRD = 0.D0
            DO K = KTMX, 1, -1
               DO I = ISTS, IENS
                  GMDDE = GMDD(I,K) - GMDD(I,K+1)
                  IF ( GMDDE >= 0.D0 ) THEN
                     GCRD(I) = GCRD(I) + GDR(I,K,LT)*GMDDE
                  ELSE IF ( GMDD(I,K+1) > 0.D0 ) THEN
                     GMDDD = - GMDDE
                     GCRDD = GMDDD/GMDD(I,K+1) * GCRD(I)
                     GTR(I,K,LT) = GTR(I,K,LT) + GRAV/DELP( I,K ) &
                                               * (GCRDD - GMDDD*GDR( I,K,LT ))
                     GCRD(I)     = GCRD( I ) - GCRDD
                  END IF
               END DO
            END DO
!
         END IF
!
      END DO
!
      END SUBROUTINE CUMDNR
!***********************************************************************
      SUBROUTINE CUMSBR                                      & !! Tracer Subsidence
                      ( IJSDIM, KMAX  , NTR   ,              & !DD dimensions
                        GTR   ,                              & ! modified
                        GDR   , DELP  ,                      & ! input
                        GMFLX , KTMX  , OTSPT , ISTS, IENS )   ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!DD      use constituents, only: NTR => pcnst
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJSDIM, KMAX, NTR             !! DD, for GFS, pass in
!
!   [MODIFY]
      REAL(r8)     GTR   ( IJSDIM, KMAX, NTR )   !! tracer tendency
!
!   [INPUT]
      REAL(r8)     GDR   ( IJSDIM, KMAX, NTR )   !! tracer
      REAL(r8)     DELP  ( IJSDIM, KMAX   )
      REAL(r8)     GMFLX ( IJSDIM, KMAX+1 )      !! mass flux
      INTEGER      KTMX
      LOGICAL      OTSPT ( NTR )                 !! tracer transport on/off
      INTEGER      ISTS, IENS
!
!   [INTERNAL WORK]
      INTEGER      I, K, KM, KP, LT
      REAL(r8)     SBR0, SBR1, FX1
      REAL(r8)     FX    ( ISTS:IENS )
!
      DO LT = 1, NTR
         IF ( OTSPT( LT ) ) THEN
            DO I = ISTS, IENS
              FX(I) = 0.D0
            enddo
            DO K = KTMX, 1, -1
               KM = MAX( K-1, 1    )
               KP = MIN( K+1, KMAX )
               DO I = ISTS, IENS
                  SBR0 = GMFLX( I,K+1 ) * (GDR( I,KP,LT )-GDR( I,K ,LT ))
                  SBR1 = GMFLX( I,K   ) * (GDR( I,K ,LT )-GDR( I,KM,LT ))
!
                  IF ( GMFLX( I,K ) > GMFLX( I,K+1 ) ) THEN
                     FX1 = 0.5D0
                  ELSE
                     FX1 = 0.D0
                  END IF
!
                  GTR(I,K,LT) = GTR(I,K,LT) + GRAV/DELP(I,K)            &
                                * ((1.D0-FX( I))*SBR0 + FX1*SBR1 )
!
                  FX( I ) = FX1
               END DO
            END DO
!
         END IF
      END DO
!
      END SUBROUTINE CUMSBR
!*********************************************************************
      SUBROUTINE CUMFXR                                           & !! Tracer mass fixer
                      ( IJSDIM, KMAX  , NTR   ,                   & !DD dimensions
                        GTR   ,                                   & ! modified
                        GDR   , DELP  , DELTA , KTMX  , IMFXR ,   & ! input
                        ISTS  , IENS                            )   ! input
!
!DD   use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!DD   use constituents, only: NTR => pcnst
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJSDIM, KMAX, NTR             !! DD, for GFS, pass in
!
!   [MODIFY]
      REAL(r8)     GTR   ( IJSDIM, KMAX, NTR )   !! tracer tendency
!
!   [INPUT]
      REAL(r8)     GDR   ( IJSDIM, KMAX, NTR )   !! tracer
      REAL(r8)     DELP  ( IJSDIM, KMAX      )
      REAL(r8)     DELTA                         !! time step
      INTEGER      KTMX
      INTEGER      IMFXR ( NTR )
        ! 0: mass fixer is not applied
        !    tracers which may become negative values
        !    e.g. subgrid-PDFs
        ! 1: mass fixer is applied, total mass may change through cumulus scheme
        !    e.g. moisture, liquid cloud, ice cloud, aerosols
        ! 2: mass fixer is applied, total mass never change through cumulus scheme
        !    e.g. CO2
      INTEGER      ISTS, IENS
!
!   [INTERNAL WORK]
      REAL(r8)     GDR1
      REAL(r8)     GDR2  ( ISTS:IENS, KMAX )
      REAL(r8)     TOT0  ( ISTS:IENS )
      REAL(r8)     TOT1  ( ISTS:IENS )
      REAL(r8)     TRAT  ( ISTS:IENS )
      REAL(r8)     FWAT
      INTEGER      I, K, LT
!DD#ifdef OPT_CUMBGT
!DD      REAL(r8)     GTWB  ( ISTS:IENS )           !! Qt tendency (before)
!DD      REAL(r8)     GTWA  ( ISTS:IENS )           !! Qt tendency (after)
!DD      REAL(r8)     WBGT, WBMX
!DD#endif
!
! Attention: tracers are forced to be positive unless IMFXR=0.
!
!DD#ifdef OPT_CUMBGT
!DD      GTWB( ISTS:IENS ) = 0.D0
!DD      DO K = 1, KMAX
!DD         DO I = ISTS, IENS
!DD            GTWB( I ) = GTWB( I ) &
!DD                       + ( GTR( I,K,1 )+GTR( I,K,ITL ) &
!DD                         + GTR( I,K,ITI ) )*DELP( I,K )/GRAV
!DD         END DO
!DD      END DO
!DD#endif
!
      DO LT = 1, NTR
         SELECT CASE ( IMFXR( LT ) )
            CASE (0)
               CYCLE
            CASE (1)
               FWAT = 1.D0
            CASE (2)
               FWAT = 0.D0
            CASE DEFAULT
               EXIT
         END SELECT
!
         DO I = ISTS, IENS
           TOT0( I ) = 0.D0
           TOT1( I ) = 0.D0
         enddo
!
         DO K = KTMX, 1, -1
            DO I = ISTS, IENS
               IF ( GTR( I,K,LT ) /= 0.D0 ) THEN
                  GDR1        = GDR( I,K,LT ) + DELTA*GTR( I,K,LT )
                  GDR2( I,K ) = MAX( GDR1, 0.D0 )
                  GDR1        = GDR1 * FWAT + GDR( I,K,LT )*( 1.D0 - FWAT )
                  TOT0( I )   = TOT0( I ) + GDR1 *(DELP( I,K )*GRAVI)
                  TOT1( I )   = TOT1( I ) + GDR2( I,K )*(DELP( I,K )*GRAVI)
               END IF
            END DO
         END DO
!
         DO I = ISTS, IENS
            IF ( TOT1( I ) > 0.D0 ) THEN
               TRAT( I ) = MAX( TOT0( I ), 0.D0 )/TOT1( I )
            ELSE
               TRAT( I ) = 1.D0
            END IF
         END DO
!
         DO K = KTMX, 1, -1
            DO I = ISTS, IENS
               IF ( GTR( I,K,LT ) /= 0.D0 ) THEN
                  GDR2( I,K    ) = GDR2( I,K )*TRAT( I )
                  GTR ( I,K,LT ) = (GDR2( I,K )-GDR( I,K,LT )) / DELTA
               END IF
            END DO
         END DO
!
      END DO   ! LT-loop
!
!DD#ifdef OPT_CUMBGT   /* budget check */
!DD      GTWA( ISTS:IENS ) = 0.D0
!DD      WBMX = 0.D0
!DD      DO K = 1, KMAX
!DD         DO I = ISTS, IENS
!DD            GTWA( I ) = GTWA( I )  + ( GTR( I,K,1 )+GTR( I,K,ITL )        &
!DD                                     + GTR( I,K,ITI ) )*DELP( I,K )/GRAV
!DD         END DO
!DD      END DO
!DD      DO I = ISTS, IENS
!DD         WBGT = GTWA( I )-GTWB( I )
!DD         IF ( ABS( WBGT ) .GT. ABS( WBMX ) ) WBMX = WBGT
!DD      END DO
!DD      WRITE( iulog,* ) &
!DD         '### CUMFXR(rank=',irank,'): water imbalance =', WBMX
!DD#endif
!
      END SUBROUTINE CUMFXR
!*********************************************************************
      SUBROUTINE CUMFXR1   & !! Tracer mass fixer
               ( IJSDIM, KMAX  ,                           & !DD dimensions
                 GTR   ,                                   & ! modified
                 GDR   , DELP  , DELTA , KTMX  , IMFXR ,   & ! input
                 ISTS  , IENS                            )   ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJSDIM, KMAX           !! DD, for GFS, pass in
!
!   [MODIFY]
      REAL(r8)     GTR   ( IJSDIM, KMAX )   !! tracer tendency
!
!   [INPUT]
      REAL(r8)     GDR   ( IJSDIM, KMAX )   !! tracer
      REAL(r8)     DELP  ( IJSDIM, KMAX )
      REAL(r8)     DELTA                         !! time step
      INTEGER      KTMX
      INTEGER      IMFXR
        ! 0: mass fixer is not applied
        !    tracers which may become negative values
        !    e.g. subgrid-PDFs
        ! 1: mass fixer is applied, total mass may change through cumulus scheme
        !    e.g. moisture, liquid cloud, ice cloud, aerosols
        ! 2: mass fixer is applied, total mass never change through cumulus scheme
        !    e.g. CO2
      INTEGER      ISTS, IENS
!
!   [INTERNAL WORK]
      REAL(r8)     GDR1
      REAL(r8)     GDR2  ( ISTS:IENS, KMAX )
      REAL(r8)     TOT0  ( ISTS:IENS )
      REAL(r8)     TOT1  ( ISTS:IENS )
      REAL(r8)     TRAT  ( ISTS:IENS )
      REAL(r8)     FWAT
      INTEGER      I, K
!
! Attention: tracers are forced to be positive unless IMFXR=0.
!
      SELECT CASE ( IMFXR )
         CASE (0)
            RETURN
         CASE (1)
            FWAT = 1.D0
         CASE (2)
            FWAT = 0.D0
         CASE DEFAULT
            RETURN
      END SELECT
!
      DO I = ISTS, IENS
        TOT0( I ) = 0.D0
        TOT1( I ) = 0.D0
      enddo
!
      DO K = KTMX, 1, -1
         DO I = ISTS, IENS
            IF ( GTR( I,K ) /= 0.D0 ) THEN
               GDR1        = GDR( I,K ) + DELTA*GTR( I,K )
               GDR2( I,K ) = MAX( GDR1, 0.D0 )
               GDR1        = GDR1*FWAT + GDR( I,K )*( 1.D0 - FWAT )
               TOT0( I )   = TOT0( I ) + GDR1 *(DELP( I,K )/GRAV)
               TOT1( I )   = TOT1( I ) + GDR2( I,K )*(DELP( I,K )/GRAV)
            END IF
         END DO
      END DO
!
      DO I = ISTS, IENS
         IF ( TOT1( I ) > 0.D0 ) THEN
            TRAT( I ) = MAX( TOT0( I ), 0.D0 )/TOT1( I )
         ELSE
            TRAT( I ) = 1.D0
         END IF
      END DO
!
      DO K = KTMX, 1, -1
         DO I = ISTS, IENS
            IF ( GTR( I,K ) /= 0.D0 ) THEN
               GDR2( I,K ) = GDR2( I,K )*TRAT( I )
               GTR ( I,K ) = ( GDR2( I,K )-GDR( I,K ) ) / DELTA
            END IF
         END DO
      END DO
!
      END SUBROUTINE CUMFXR1
!*********************************************************************
      SUBROUTINE CUMCHK                                   & !! check range of output values
                      ( IJSDIM, KMAX  , NTR   ,           & !DD dimensions
                        GTT   , GTQ   , GTU   , GTV   ,   & ! input
                        GTCFRC, GPRCC , GSNWC , CUMCLW,   & ! input
                        CUMFRC, FLIQC , GTPRP ,           & ! input
                        ISTS  , IENS                    )   ! input
!
!DD      use ppgrid      , only: IJSDIM => pcols, KMAX => pver
!DD      use constituents, only: NTR => pcnst
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJSDIM, KMAX, NTR             !! DD, for GFS, pass in
!
!   [INPUT]
      REAL(r8)     GTT   ( IJSDIM, KMAX      ) !! heating rate
      REAL(r8)     GTQ   ( IJSDIM, KMAX, NTR ) !! change in q
      REAL(r8)     GTU   ( IJSDIM, KMAX      ) !! tendency of u
      REAL(r8)     GTV   ( IJSDIM, KMAX      ) !! tendency of v
      REAL(r8)     GPRCC ( IJSDIM, NTR       ) !! rainfall
      REAL(r8)     GSNWC ( IJSDIM            ) !! snowfall
      REAL(r8)     CUMCLW( IJSDIM, KMAX      ) !! cloud water in cumulus
      REAL(r8)     CUMFRC( IJSDIM            ) !! cumulus cloud fraction
      REAL(r8)     GTCFRC( IJSDIM, KMAX      ) !! change in cloud fraction
      REAL(r8)     FLIQC ( IJSDIM, KMAX      ) !! liquid ratio in cumulus
      REAL(r8)     GTPRP ( IJSDIM, KMAX+1    ) !! rain+snow flux
!
      INTEGER    ISTS, IENS
!
!   [INTERNAL WORK]
      INTEGER    I, K
!
!   [INTERNAL PARM]
      REAL(r8) :: GTTMAX  = 1.e-2_r8
      REAL(r8) :: GTQVMAX = 1.e-4_r8
      REAL(r8) :: GTQLMAX = 1.e-5_r8
      REAL(r8) :: GTUMAX  = 1.e-2_r8
      REAL(r8) :: GTVMAX  = 1.e-2_r8
      REAL(r8) :: GTCFMAX = 1.e-3_r8
      REAL(r8) :: PRCCMAX = 1.e-2_r8
      REAL(r8) :: SNWCMAX = 1.e-2_r8
      REAL(r8) :: CLWMAX  = 1.e-3_r8
      REAL(r8) :: TPRPMAX = 1.e-2_r8
      REAL(r8) :: GTQIMAX = 1.e-5_r8
      REAL(r8) :: GTM2MAX = 1._r8
      REAL(r8) :: GTM3MAX = 1._r8
!
      DO K = 1, KMAX
         DO I = ISTS, IENS
            IF ( ABS( GTT( I,K ) ) > GTTMAX ) THEN
               WRITE(iulog,*) '### CUMCHK: GTT(',I,',',K,')=',GTT( I,K )
            END IF
            IF ( ABS( GTQ( I,K,1 ) ) > GTQVMAX ) THEN
               WRITE(iulog,*) '### CUMCHK: GTQ(',I,',',K,',1 )=', &
                          GTQ( I,K,1 )
            END IF
            IF ( ABS( GTQ( I,K,ITL ) ) > GTQLMAX ) THEN
               WRITE(iulog,*) '### CUMCHK: GTQ(',I,',',K,',ITL )=', &
                          GTQ( I,K,ITL )
            END IF
            IF ( ABS( GTU( I,K ) ) > GTUMAX ) THEN
               WRITE(iulog,*) '### CUMCHK: GTU(',I,',',K,')=',GTU( I,K )
            END IF
            IF ( ABS( GTV( I,K ) ) .GT. GTVMAX ) THEN
               WRITE(iulog,*) '### CUMCHK: GTV(',I,',',K,')=',GTV( I,K )
            END IF
            IF ( ABS( GTCFRC( I,K ) ) > GTCFMAX ) THEN
               WRITE(iulog,*) '### CUMCHK: GTCFRC(',I,',',K,')=', &
                          GTCFRC( I,K )
            END IF
            IF ( CUMCLW( I,K ) > CLWMAX .OR. CUMCLW( I,K ) < 0.D0  ) THEN
               WRITE(iulog,*) '### CUMCHK: CUMCLW(',I,',',K,')=', &
                          CUMCLW( I,K )
            END IF
            IF ( FLIQC( I,K ) > 1.D0 .OR.  FLIQC( I,K ) < 0.D0 ) THEN
               WRITE(iulog,*) '### CUMCHK: FLIQC(',I,',',K,')=', &
                          FLIQC( I,K )
            END IF
            IF ( GTPRP( I,K ) > TPRPMAX .OR.  GTPRP( I,K ) < 0.D0 ) THEN
               WRITE(iulog,*) '### CUMCHK: GTPRP(',I,',',K,')=', GTPRP( I,K )
            END IF
            IF ( ABS( GTQ( I,K,ITI ) ) > GTQIMAX ) THEN
               WRITE(iulog,*) '### CUMCHK: GTQ(',I,',',K,',ITI )=', GTQ(I,K,ITI)
            END IF
!            IF ( ABS( GTQ( I,K,IMU2 ) ) > GTM2MAX ) THEN
!               WRITE(iulog,*) '### CUMCHK: GTQ(',I,',',K,',IMU2 )=', GTQ(I,K,IMU2)
!            END IF
!            IF ( ABS( GTQ( I,K,IMU3 ) ) > GTM3MAX ) THEN
!               WRITE(iulog,*) '### CUMCHK: GTQ(',I,',',K,',IMU3 )=', GTQ( I,K,IMU3 )
!            END IF
         END DO
      END DO
!
      DO I = ISTS, IENS
        IF ( GPRCC(I,1) > PRCCMAX .OR. GPRCC(I,1) < 0.D0 ) THEN
            WRITE(iulog,*) '### CUMCHK: GPRCC(',I,')=',GPRCC(I,1)
        END IF
        IF ( GSNWC(I) > SNWCMAX .OR. GSNWC(I) < 0.D0     ) THEN
           WRITE(iulog,*) '### CUMCHK: GSNWC(',I,')=',GSNWC(I)
        END IF
        IF ( CUMFRC(I) > 1.D0 .OR.  CUMFRC(I) < 0.D0      ) THEN
           WRITE(iulog,*) '### CUMCHK: CUMFRC(',I,')=',CUMFRC(I)
        END IF
      END DO
!
      END SUBROUTINE CUMCHK
!***********************************************************************
      SUBROUTINE TINTP                          & !! vertical interpolation of temperature
                     ( IJSDIM, KMAX  ,          & !DD dimensions
                       GDTM  ,                  & ! output
                       GDT   , GDP   , GDPM ,   & ! input
                       ISTS  , IENS           )   ! input

!DD      use ppgrid, only: IJSDIM => pcols, KMAX => pver
!
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: IJSDIM, KMAX       !! DD, for GFS, pass in
!*
!*   [OUTPUT]
      REAL(r8)     GDTM  ( IJSDIM, KMAX+1 )     !! temperature (half lev)
!*
!*   [INPUT]
      REAL(r8)     GDT   ( IJSDIM, KMAX )       !! temperature (full lev)
      REAL(r8)     GDP   ( IJSDIM, KMAX )       !! pressure (full lev)
      REAL(r8)     GDPM  ( IJSDIM, KMAX+1 )     !! pressure (half lev)
      INTEGER      ISTS, IENS                   !! range of active grids
!*
!*   [INTERNAL WORK]
!     REAL(r8)     FTINT ( KMAX )               !! intrp. coef.
!     REAL(r8)     FTINTM( KMAX )               !! intrp. coef.
      real (r8)  :: wrk, wrk1, ftintm

      INTEGER    I, K
!*
!*          < interp. temp. >
!*
      DO K = 2, KMAX
        DO I = ISTS, IENS
          wrk  = 1.0d0 / GDP(I,K)
          wrk1 = 1.0d0 / LOG(GDP(I,K-1)*wrk)
          FTINTM      = wrk1 * LOG(GDPM(I,K)*wrk)
          GDTM( I,K ) = FTINTM *GDT(I,K-1) + (1.0-FTINTM)*GDT(I,K)
!         FTINTM( K ) = wrk1 * LOG(GDPM(I,K)*wrk)
!         FTINT ( K ) = wrk1 * LOG(GDP(I,K-1)/GDPM(I,K))
!         GDTM( I,K ) = FTINTM(K)*GDT(I,K-1) + FTINT(K)*GDT(I,K)
         END DO
      END DO

      DO I = ISTS, IENS
        GDTM( I,KMAX+1 ) = GDT( I,KMAX )
        GDTM( I,1      ) = GDT( I,1 )
      END DO

      RETURN
      END SUBROUTINE TINTP
!***********************************************************************
!DD      subroutine outfld_cs & !! outfld for MIROC variables
!DD               ( name, var_cs, idim, kdim, lchnk )
!DD
!DD      use cam_history,   only: outfld
!DD
!DD      implicit none
!DD
!DD      character(len=*), intent(in) :: name
!DD      integer, intent(in) :: idim, kdim, lchnk
!DD      real(r8), intent(in) :: var_cs(idim,kdim)
!DD
!DD      real(r8) :: var_cam(idim,kdim)
!DD      integer :: i, k
!DD
!DD      do k = 1, kdim
!DD         do i = 1, idim
!DD            var_cam(i,kdim-k+1) = var_cs(i,k)
!DD         end do
!DD      end do
!DD
!DD      call outfld( name, var_cam, idim, lchnk )
!DD
!DD      end subroutine outfld_cs
!***********************************************************************

end module cs_conv
