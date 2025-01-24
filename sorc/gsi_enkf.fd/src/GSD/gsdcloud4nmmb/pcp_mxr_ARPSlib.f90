
SUBROUTINE pcp_mxr (nx,ny,nz,t_3d,p_3d ,ref_3d                          &
           ,cldpcp_type_3d                                              &
           ,qr_3d,qs_3d,qg_3d,istatus )

!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  pcp_mxr calculates hydrometeor mixing ratios based on Kessler radar reflectivity equations
!
!   PRGMMR:                  ORG:                DATE:              
!
! ABSTRACT: 
!  This subroutine calculate precipitation based on Kessler radar reflectivity equations
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nx          - no. of lons on subdomain (buffer points on ends)
!     ny          - no. of lats on subdomain (buffer points on ends)
!     nz          - no. of levels
!     t_3d        - 3D background temperature (K)
!     p_3d        - 3D background pressure  (hPa)
!     ref_3d      - 3D reflectivity in analysis grid (dBZ)
!     cldpcp_type_3d - 3D precipitation type
!
!   output argument list:
!     qr_3d       - rain mixing ratio (g/kg)
!     qs_3d       - snow mixing ratio (g/kg)
!     qg_3d       - graupel/hail mixing ratio (g/kg)
!     istatus     -
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!   Old documents from CAPS
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Perform 3D precipitation mixing ratio (in g/kg) analysis using
!  radar reflectivity data. For rain water, using Kessler (1969)
!  formula:
!            qr(g/kg) = a*(rho*arg)**b                  (1)
!
!  Here arg = Z (mm**6/m**3), and dBZ = 10log10 (arg).
!  Coeffcients a=17300.0, and b=7/4.
!  rho represents the air density.
!
!  For snow and graupel/hail, using Rogers and Yau (1989) formula:
!
!            qs(g/kg) = c*(rho*arg)**d                  (2)
!
!  where, c=38000.0,  d=2.2
!
!
!-----------------------------------------------------------------------
!
!  AUTHOR: (Jian Zhang)
!  06/13/96
!
!  MODIFICATION HISTORY:
!  07/30/97 (J. Zhang)
!           Added precipitation type in the argument list so that
!           mixing ratios of different precip. types can be computed.
!  09/04/97 (J. Zhang)
!            Changed the radar echo thresholds for inserting precip.
!            from radar reflectivities.
!
!-----------------------------------------------------------------------
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
! 

!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind, r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  integer(i_kind),intent(in) :: nx,ny,nz       ! Model grid size
!
  REAL(r_kind),  intent(in) :: ref_3d(nx,ny,nz)! radar reflectivity (dBZ)
  real(r_single),intent(in) :: t_3d(nx,ny,nz)  ! Temperature (deg. Kelvin)
  real(r_single),intent(in) :: p_3d(nx,ny,nz)  ! Pressure (Pascal)

  integer(i_kind),intent(in):: cldpcp_type_3d(nx,ny,nz) ! cloud/precip type field
!
!  OUTPUT:
  INTEGER(i_kind),intent(out) :: istatus
!
  REAL(r_single),intent(out) :: qr_3d(nx,ny,nz)! rain mixing ratio in (g/kg)
  REAL(r_single),intent(out) :: qs_3d(nx,ny,nz)! snow/sleet/frz-rain mixing ratio
                            ! in (g/kg)
  REAL(r_single),intent(out) :: qg_3d(nx,ny,nz)! graupel/hail mixing ratio in (g/kg)
!
!  LOCAL:
  REAL(r_kind) :: a,b,c,d                  ! Coef. for Z-qr relation.
  PARAMETER (a=17300.0_r_kind, b=7.0/4.0_r_kind)
  PARAMETER (c=38000.0_r_kind, d=2.2_r_kind)
  REAL(r_kind) :: rair                     ! Gas constant (J/deg/kg)
  PARAMETER (rair = 287.04_r_kind)
  REAL(r_kind) :: thresh_ref
  PARAMETER (thresh_ref = 0.0_r_kind)
  INTEGER(i_kind) :: pcptype
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  INTEGER(i_kind) :: i,j,k, iarg
  REAL(r_kind)    :: arg,rhobar,br,dr
  PARAMETER (br=1.0_r_kind/b, dr=1.0_r_kind/d)
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
!-----------------------------------------------------------------------
!
  istatus=0
!
!-----------------------------------------------------------------------
!
!  Compute the precip mixing ratio in g/kg from radar reflectivity
!  factor following Kessler (1969) or Rogers and Yau (1989).
!
!-----------------------------------------------------------------------
!
  DO k = 1,nz-1
    DO j = 2,ny-1
      DO i = 2,nx-1
        IF (ref_3d(i,j,k) >= thresh_ref) THEN    ! valid radar refl.
          rhobar = p_3d(i,j,k)/rair/t_3d(i,j,k)
          arg = 10.0_r_kind**(0.1_r_kind*ref_3d(i,j,k))
          iarg = cldpcp_type_3d(i,j,k)
          pcptype = iarg/16              ! precip. type

          IF (pcptype == 0) THEN       ! no precip
            PRINT*,'+++ NOTE: radar echo though no precip. +++'
          ELSE IF (pcptype == 1.OR.pcptype == 3) THEN   ! rain or Z R
            qr_3d(i,j,k) = (arg/a)**br/rhobar
          ELSE IF (pcptype == 2) THEN                   ! snow
            qs_3d(i,j,k) = (arg/c)**dr/rhobar
          ELSE IF (pcptype == 4.OR.pcptype == 5) THEN   ! hail or sleet
            qg_3d(i,j,k) = (arg/c)**dr/rhobar
          ELSE                                          ! unknown
            PRINT*,'+++ NOTE: unknown precip type. +++'
          END IF
        ELSE
          qr_3d(i,j,k) = 0._r_kind
          qs_3d(i,j,k) = 0._r_kind
          qg_3d(i,j,k) = 0._r_kind
        END IF
      END DO ! k
    END DO ! i
  END DO ! j
!
!-----------------------------------------------------------------------
!
  istatus = 1
!
  RETURN
END SUBROUTINE pcp_mxr

!
SUBROUTINE pcp_mxr_ferrier_new (nx,ny,nz,t_3d,p_3d ,ref_3d                  &
           ,cldpcp_type_3d,q_3d                                              &
           ,qr_3d,qs_3d,qg_3d,istatus )
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  pcp_mxr calculate hydrometeor type based on ferrier radar reflectivity equations
!              from Carley's setup_dbz.f90 and old Hu's pcp_mxr_ferrier
!
!   PRGMMR:   Shun Liu               ORG:  EMC/NCEP              DATE:              
!
! ABSTRACT: 
!  This subroutine calculate precipitation based on ferrier radar reflectivity equations
!
! PROGRAM HISTORY LOG:
!    2014-12-01  Shun Liu create for new NMMB ferrier 
!
!
!   input argument list:
!     nx          - no. of lons on subdomain (buffer points on ends)
!     ny          - no. of lats on subdomain (buffer points on ends)
!     nz          - no. of levels
!     t_3d        - 3D background temperature (K)
!     p_3d        - 3D background pressure  (hPa)
!     ref_3d      - 3D reflectivity in analysis grid (dBZ)
!     cldpcp_type_3d - 3D precipitation type
!
!   output argument list:
!     qr_3d       - rain mixing ratio (g/kg)
!     qs_3d       - snow mixing ratio (g/kg)
!     istatus     -
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!   Old document from CAPS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Perform 3D precipitation mixing ratio (in g/kg) analysis using
!  radar reflectivity data. For rain water, using Ferrier et al (1995)
!  formulation:
!
!
!     For rain water:
!
!          18
!        10   * 720                              1.75
!  Zer = --------------------------- * (rho * qr)
!          1.75      0.75       1.75
!        pi     * N0r     * rhor
!
!
!     For dry snow (t <= 0 C):
!
!
!                  18     
!        0.224 * 10   * 720       
!                                                          2
!  Zes = ------------------------------------- * (rho * qs)   
!          2        2       
!        pi   * rhol   * N0s     
!                  
!    n(0)r -> intercept parameter for rain 8x10^-6 (m^-4)
!    rho_l -> density of liquid water 1000 (kg/m^3)
!    rho -> air density (kg/m^3)
!    qr -> rain mixing ratio (kg/kg)
!    qli -> precipitation ice mixing ratio (kg/kg)
!    N_li -> precipitation ice number concentration 5x10^3 (m^-3)
!   
!
!    Plugging in the constants yields the following form:
!
!    Zer  = Cr * (rho*qr)^1.75
!    Zeli  = Cli * (rho*qli)^2
!   
!    where:
!           Cr  = 3.6308 * 10^9
!           Cli = 3.268 * 10^9
!
!    Which yields the forward model:
!
!    Z = 10*log10(Zer+Zes)
!
!
!  Here Zx (mm**6/m**3, x=r,s,h), and dBZ = 10log10 (Zx).
!  rho represents the air density, rhor,rhos,rhoh are the density of
!  rain, snow and hail respectively. Other variables are all constants
!  for this scheme, see below.
!
!    Zer  = Cr * (rho*qr)^1.75
!    Zeli = Cli * (rho*qli)^2
!   
!    where:
!           Cr  = 3.6308 * 10^9
!           Cli = 3.268 * 10^9

!    (Zer)^(1/1.75)=(rho*qr)
!    (Zer/Cr)^(1/1.75)=rho*qr
!    [(Zer/Cr)^(1/1.75)]/rho=qr

!    [(Zeli/Cli)^(1/2)]/rho=qs

!
!-----------------------------------------------------------------------
!
!  AUTHOR: (Shun Liu)
!  01/20/2015
!
!  MODIFICATION HISTORY:
!
!-----------------------------------------------------------------------
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
! 


!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind, r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER(i_kind),intent(in) :: nx,ny,nz        ! Model grid size
!
  REAL(r_kind),   intent(in) :: ref_3d(nx,ny,nz)! radar reflectivity (dBZ)
  REAL(r_single), intent(in) :: t_3d(nx,ny,nz)  ! Temperature (deg. Kelvin)
  REAL(r_single), intent(in) :: p_3d(nx,ny,nz)  ! Pressure (Pascal)
  REAL(r_single), intent(in) :: q_3d(nx,ny,nz) ! mixing ratio in (g/g)

  INTEGER(i_kind),intent(in) :: cldpcp_type_3d(nx,ny,nz) ! cloud/precip type field
!
!  OUTPUT:
  INTEGER(i_kind),intent(out):: istatus
!
  REAL(r_single),intent(out) :: qr_3d(nx,ny,nz) ! rain mixing ratio in (g/kg)
  REAL(r_single),intent(out) :: qs_3d(nx,ny,nz) ! snow/sleet/frz-rain mixing ratio
                                                ! in (g/kg)
  REAL(r_single),intent(out) :: qg_3d(nx,ny,nz) ! graupel/hail mixing ratio 
                                                ! in (g/kg)
!



  REAL(r_kind), PARAMETER :: rd=287.0_r_kind   ! Gas constant for dry air  (m**2/(s**2*K))
  REAL(r_kind), PARAMETER :: thresh_ref = 0.0_r_kind

  REAL(r_kind), PARAMETER :: ze_qr_const=3.6308*1.0e9
  REAL(r_kind), PARAMETER :: ze_qs_const=3.268*1.0e9
  REAL(r_kind) :: ze_d_qrcon,ze_d_qscon

!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  INTEGER(i_kind) :: i,j,k, iarg
  INTEGER(i_kind) :: pcptype
  REAL(r_kind) :: zkconst,zerf,zesnegf,zesposf,zehf,rfract
  REAL(r_kind) :: ze,zer,zeh,zes,rho,tc

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
!-----------------------------------------------------------------------
!
!  Intiailize constant factors in the Ze terms for rain, snow and graupel/hail,
!  respectively, in Ferrier.
!
!  These are the inverse of those presented in the reflec_ferrier function.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Compute the precip mixing ratio in g/kg from radar reflectivity
!  factor following Ferrier et al (1995).
!
!-----------------------------------------------------------------------
!

! qr_3d = -999._r_kind
! qs_3d = -999._r_kind
  qg_3d = -999._r_kind

  DO k = 2,nz-1
    DO j = 2,ny-1
      DO i = 2,nx-1
        IF (ref_3d(i,j,k) >= thresh_ref) THEN    ! valid radar refl.
          rho = p_3d(i,j,k)/(rd*t_3d(i,j,k))*(1.0+0.608*(q_3d(i,j,k)/1.0+q_3d(i,j,k)))
          ze = 10.0_r_kind**(0.1_r_kind*ref_3d(i,j,k))
          tc = t_3d(i,j,k) - 273.15_r_kind
          IF (tc >= 0.0_r_kind) THEN
            ze_d_qrcon=ze/ze_qr_const
            qr_3d(i,j,k) = (ze_d_qrcon)**(1/1.75) !/ rho 
          else
            ze_d_qscon=ze/ze_qs_const
            qs_3d(i,j,k) = (ze_d_qscon)**(0.5)  !/ rho
          ENDIF
        END IF
      END DO ! k
    END DO ! i
  END DO ! j

! qr_3d=qr_3d*1000.0  !kg/kg to g/kg
! qs_3d=qs_3d*1000.0  !kg/kg to g/kg

!  PRINT*,'Finish Ferrier ...'
!
!-----------------------------------------------------------------------
!
  istatus = 1
!
  RETURN
END SUBROUTINE pcp_mxr_ferrier_new

!
SUBROUTINE pcp_mxr_ferrier (nx,ny,nz,t_3d,p_3d ,ref_3d                  &
           ,cldpcp_type_3d                                              &
           ,qr_3d,qs_3d,qg_3d,istatus,mype )
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  pcp_mxr calculate hydrometeor type based on ferrier radar reflectivity equations
!
!   PRGMMR:                  ORG:                DATE:              
!
! ABSTRACT: 
!  This subroutine calculate precipitation based on ferrier radar reflectivity equations
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nx          - no. of lons on subdomain (buffer points on ends)
!     ny          - no. of lats on subdomain (buffer points on ends)
!     nz          - no. of levels
!     t_3d        - 3D background temperature (K)
!     p_3d        - 3D background pressure  (hPa)
!     ref_3d      - 3D reflectivity in analysis grid (dBZ)
!     cldpcp_type_3d - 3D precipitation type
!
!   output argument list:
!     qr_3d       - rain mixing ratio (g/kg)
!     qs_3d       - snow mixing ratio (g/kg)
!     qg_3d       - graupel/hail mixing ratio (g/kg)
!     istatus     -
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!   Old document from CAPS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Perform 3D precipitation mixing ratio (in g/kg) analysis using
!  radar reflectivity data. For rain water, using Ferrier et al (1995)
!  formulation:
!
!
!     For rain water:
!
!          18
!        10   * 720                              1.75
!  Zer = --------------------------- * (rho * qr)
!          1.75      0.75       1.75
!        pi     * N0r     * rhor
!
!
!     For dry snow (t <= 0 C):
!
!
!          18           2                     0.25
!        10  * 720 * |K|                * rhos
!                       ice                                    1.75
!  Zes = ----------------------------------------- * (rho * qs)     t <= 0 C
!          1.75         2          0.75       2
!        pi        * |K|      * N0s     * rhoi
!                     water
!
!
!     For wet snow (t >= 0 C):
!
!
!              18
!            10   * 720                                 1.75
!  Zes =     ----------------------------   * (rho * qs)            t >  0 C
!              1.75      0.75       1.75
!            pi     * N0s     * rhos
!
!
!     For hail water:
!
!
!          /   18                       \ 0.95
!         /  10   * 720                  \              1.6625
!  Zeh = |   ---------------------------- | * (rho * qg)
!         \    1.75      0.75       1.75 /
!          \ pi     * N0h     * rhoh    /
!
!  Here Zx (mm**6/m**3, x=r,s,h), and dBZ = 10log10 (Zx).
!  rho represents the air density, rhor,rhos,rhoh are the density of
!  rain, snow and hail respectively. Other variables are all constants
!  for this scheme, see below.
!
!
!-----------------------------------------------------------------------
!
!  AUTHOR: (Donghai Wang and Eric Kemp)
!  07/20/2000
!
!  MODIFICATION HISTORY:
!
!  11/09/2000 Keith Brewster
!  Moved some parameters with real-valued exponentiation to be
!  computed at runtime due to compiler complaint.
!
!  04/07/2003 Keith Brewster
!  Restructured code to make more tractable.and consistent with 
!  the reflec_ferrier subroutine.
!
!-----------------------------------------------------------------------
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
! 


!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind, r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER(i_kind),intent(in) :: nx,ny,nz        ! Model grid size
!
  REAL(r_kind),   intent(inout) :: ref_3d(nx,ny,nz)! radar reflectivity (dBZ)
  REAL(r_single), intent(in) :: t_3d(nx,ny,nz)  ! Temperature (deg. Kelvin)
  REAL(r_single), intent(in) :: p_3d(nx,ny,nz)  ! Pressure (Pascal)

  INTEGER(i_kind),intent(in) :: cldpcp_type_3d(nx,ny,nz) ! cloud/precip type field
  INTEGER(i_kind),intent(in) :: mype
!
!  OUTPUT:
  INTEGER(i_kind),intent(out):: istatus
!
  REAL(r_single),intent(out) :: qr_3d(nx,ny,nz) ! rain mixing ratio in (g/kg)
  REAL(r_single),intent(out) :: qs_3d(nx,ny,nz) ! snow/sleet/frz-rain mixing ratio
                                                ! in (g/kg)
  REAL(r_single),intent(out) :: qg_3d(nx,ny,nz) ! graupel/hail mixing ratio 
                                                ! in (g/kg)
!

  REAL(r_kind),PARAMETER :: ki2 = 0.176_r_kind ! Dielectric factor for ice if other
                                               !   than melted drop diameters are used.
  REAL(r_kind),PARAMETER :: kw2=0.93_r_kind ! Dielectric factor for water.

  REAL(r_kind),PARAMETER :: m3todBZ=1.0E+18_r_kind ! Conversion factor from m**3 to
                                                   !   mm**6 m**-3.
  REAL(r_kind),PARAMETER :: Zefact=720.0_r_kind ! Multiplier for Ze components.
  REAL(r_kind),PARAMETER :: lg10div=0.10_r_kind ! Log10 multiplier (1/10)

  REAL(r_kind),PARAMETER :: pi=3.1415926_r_kind! Pi.
  REAL(r_kind),PARAMETER :: N0r=8.0E+06_r_kind ! Intercept parameter in 1/(m^4) for rain.
  REAL(r_kind),PARAMETER :: N0s=3.0E+06_r_kind ! Intercept parameter in 1/(m^4) for snow.
  REAL(r_kind),PARAMETER :: N0h=4.0E+04_r_kind ! Intercept parameter in 1/(m^4) for graupel/hail.

  REAL(r_kind),PARAMETER :: N0xpowf=3.0/7.0_r_kind ! Power to which N0r,N0s & N0h are
                                                   !   raised.
  REAL(r_kind),PARAMETER :: K2powf=4.0/7.0_r_kind  ! Power to which K-squared 
                                                   !  of ice, water are raised
  REAL(r_kind),PARAMETER :: zkpowf=4.0/7.0_r_kind  ! Power to which Zk is raised
  REAL(r_kind),PARAMETER :: zepowf=4.0/7.0_r_kind  ! Power to which Ze is raised
  REAL(r_kind),PARAMETER :: zehpowf=(4.0/7.0)*1.0526_r_kind  ! Power to which Zeh is raised

  REAL(r_kind),PARAMETER :: rhoi=917._r_kind  ! Density of ice (kg m**-3)
  REAL(r_kind),PARAMETER :: rhor=1000._r_kind ! Density of rain (kg m**-3)
  REAL(r_kind),PARAMETER :: rhos=100._r_kind  ! Density of snow (kg m**-3)
  REAL(r_kind),PARAMETER :: rhoh=913._r_kind  ! Density of graupel/hail (kg m**-3)

  REAL(r_kind),PARAMETER :: rhoipowf=8.0/7.0_r_kind  ! Power to which rhoi is raised.
  REAL(r_kind),PARAMETER :: rhospowf=1.0/7.0_r_kind  ! Power to which rhos is raised.

  REAL(r_kind), PARAMETER :: rd=287.0_r_kind   ! Gas constant for dry air  (m**2/(s**2*K))
  REAL(r_kind), PARAMETER :: thresh_ref = 0.0_r_kind
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  INTEGER(i_kind) :: i,j,k, iarg
  INTEGER(i_kind) :: pcptype
  REAL(r_kind) :: zkconst,zerf,zesnegf,zesposf,zehf,rfract
  REAL(r_kind) :: ze,zer,zeh,zes,rho,tc

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
!-----------------------------------------------------------------------
!
!  Intiailize constant factors in the Ze terms for rain, snow and graupel/hail,
!  respectively, in Ferrier.
!
!  These are the inverse of those presented in the reflec_ferrier function.
!
!-----------------------------------------------------------------------
!
  istatus=0

  zkconst = (Zefact*m3todBZ) ** zkpowf

  zerf=1000._r_kind*(pi * (N0r**N0xpowf) * rhor )/zkconst

  zesnegf=1000._r_kind*(pi*(kw2**k2powf)*(N0s**N0xpowf)*(rhoi**rhoipowf)) /    &
          ( zkconst * (ki2**k2powf) * (rhos**rhospowf) )
          
  zesposf=1000._r_kind*( pi * (N0s**N0xpowf) * rhos) / zkconst
 
  zehf=1000._r_kind*( pi * (N0h**N0xpowf) * rhoh) / zkconst

!-----------------------------------------------------------------------
!
!  Compute the precip mixing ratio in g/kg from radar reflectivity
!  factor following Ferrier et al (1995).
!
!-----------------------------------------------------------------------
!
!mhu if(mype==51 ) then
!mhu write(*,*) 'c=',mype,zesnegf,zepowf,rd
!mhu ref_3d(10,10,:)=10.0
!mhu ref_3d(11,11,:)=20.0
!mhu ref_3d(12,12,:)=30.0
!mhu ref_3d(13,13,:)=40.0
!mhu ref_3d(14,14,:)=50.0
!mhu endif

  DO k = 2,nz-1
    DO j = 2,ny-1
      DO i = 2,nx-1
        IF (ref_3d(i,j,k) >= thresh_ref) THEN    ! valid radar refl.
          rho = p_3d(i,j,k)/(rd*t_3d(i,j,k))
          ze = 10.0_r_kind**(0.1_r_kind*ref_3d(i,j,k))
          iarg = cldpcp_type_3d(i,j,k)
          pcptype = iarg/16              ! precip. type
          tc = t_3d(i,j,k) - 273.15_r_kind
!mhu temporal fix
          IF (tc <= 0.0_r_kind) THEN
            qs_3d(i,j,k) = zesnegf * (ze**zepowf) / rho
            qr_3d(i,j,k) = 0.0_r_kind
          ELSE IF (tc < 5.0_r_kind) THEN             !wet snow
             rfract=0.20_r_kind*tc
             zer=rfract*ze
             zes=(1.-rfract)*ze
!             qs_3d(i,j,k) = zesposf * (zes**zepowf) / rho
!             qr_3d(i,j,k) = zerf * (zer**zepowf) / rho
             qs_3d(i,j,k) = zesnegf * (zes**zepowf) / rho
             qr_3d(i,j,k) = zerf * (zer**zepowf) / rho
          else
            qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
            qs_3d(i,j,k) = 0.0_r_kind
          ENDIF
          cycle
!mhu
          IF (pcptype == 1) THEN                       ! rain
            qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
          ELSE IF (pcptype == 2) THEN                  ! snow
            IF (tc <= 0.0_r_kind) THEN                 !dry snow
              qs_3d(i,j,k) = zesnegf * (ze**zepowf) / rho
            ELSE IF (tc < 5.0_r_kind) THEN             !wet snow
              rfract=0.20_r_kind*tc
              zer=rfract*ze
              zes=(1.-rfract)*ze
              qs_3d(i,j,k) = zesposf * (zes**zepowf) / rho
              qr_3d(i,j,k) = zerf * (zer**zepowf) / rho
            ELSE
              qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
            END IF
          ELSE IF (pcptype == 3) THEN                  ! ZR
            qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
          ELSE IF (pcptype == 4) THEN                  ! sleet
            IF (tc <= 0.0_r_kind) THEN                 ! graupel/hail category
              qg_3d(i,j,k) = zehf * (ze**zehpowf) / rho
            ELSE IF( tc < 10._r_kind ) THEN
              rfract=0.10_r_kind*tc
              zer=rfract*ze
              zeh=(1.-rfract)*ze
              qr_3d(i,j,k) = zerf * (zer**zepowf) / rho
              qg_3d(i,j,k) = zehf * (zeh**zehpowf) / rho
            ELSE
              qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
            END IF
          ELSE IF (pcptype == 5) THEN                   ! graupel/hail
            qg_3d(i,j,k) = zehf * (ze**zehpowf) / rho
          ELSE                                          ! unknown
            IF (tc <= 0.0_r_kind) THEN                  !dry snow
              qs_3d(i,j,k) = zesnegf * (ze**zepowf) / rho
            ELSE IF ( tc < 5.0_r_kind ) THEN            !wet snow
              rfract=0.20_r_kind*tc
              zer=rfract*ze
              zes=(1.-rfract)*ze
              qs_3d(i,j,k) = zesposf * (zes**zepowf) / rho
              qr_3d(i,j,k) = zerf * (zer**zepowf) / rho
            ELSE ! rain
              qr_3d(i,j,k) = zerf * (ze**zepowf) / rho
            END IF
          END IF
        ELSE
          qr_3d(i,j,k) = -999._r_kind
          qs_3d(i,j,k) = -999._r_kind
          qg_3d(i,j,k) = -999._r_kind
        END IF
      END DO ! k
    END DO ! i
  END DO ! j
!  PRINT*,'Finish Ferrier ...'
!
!-----------------------------------------------------------------------
!
  istatus = 1
!
  RETURN
END SUBROUTINE pcp_mxr_ferrier
