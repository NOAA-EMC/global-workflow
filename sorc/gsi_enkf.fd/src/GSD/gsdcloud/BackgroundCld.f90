SUBROUTINE  BackgroundCldgfs(mype,lon2,lat2,nsig,tbk,pbk,psbk,q,hbk)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  BackgroundCld  Ingest gfs background fields for cloud analysis
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-27
!
! ABSTRACT: 
!  This subroutine reads in background hydrometeor fields for cloud analysis
!
! PROGRAM HISTORY LOG:
!    2009-01-02  Hu  Add NCO document block
!    2010-04-26  Hu  delete the module gridmod and guess_grids.
!                    transfer information subroutine dummy variables
!
!
!   input argument list:
!     mype         - processor ID
!     lon2        - no. of lons on subdomain (buffer points on ends)
!     lat2        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of vertical levels
!     tbk         - 3D background potential temperature (K)
!     psbk        - 2D background surface pressure (hPa)
!     q           - 3D moisture (water vapor mixing ratio kg/kg)
!     pbk         - 3D background pressure  (hPa)
!
!   output argument list:
!     hbk         - 3D height above the ground (not the sea level)
!!!!     z_lcl       - lifting condensation level
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use gsd_kinds, only: r_single,i_kind,r_kind
  use constants, only: rd_over_cp, h1000
  use constants, only: rd, grav, half, rad2deg

  implicit none

  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(in):: lon2
  integer(i_kind),intent(in):: lat2
  integer(i_kind),intent(in):: nsig

! background
!
! read in from WRF
!
  real(r_single),intent(inout) :: tbk(lon2,lat2,nsig)     ! temperature
  real(r_single),intent(in)    :: psbk(lon2,lat2)         ! surface pressure
  real(r_single),intent(inout) :: q(lon2,lat2,nsig)       ! moisture
  real(r_single),intent(in)    :: pbk(lon2,lat2,nsig)     ! pressure  hPa
!
! derived fields
!
  real(r_single),intent(out) :: hbk(lon2,lat2,nsig)! height
!
!  misc.
!
  INTEGER :: i,j,k

  REAL(r_single) :: rdog, h, dz
  REAL(r_single) :: height(nsig+1)
  
!
!================================================================
!
  do k=1,nsig
    do j=1,lat2
      do i=1,lon2
         q(i,j,k) = q(i,j,k)/(1.0_r_kind-q(i,j,k))   ! water vapor mixing ratio (kg/kg)
      enddo
    enddo
  enddo

!
!   Compute geopotential height above the ground at midpoint of each layer
!
  rdog = rd/grav
  do j=1,lat2
    do i=1,lon2
      k  = 1
      h  = rdog * tbk(i,j,k)
      dz = h * log(psbk(i,j)/pbk(i,j,k))
      height(k) = dz
      
      do k=2,nsig
        h  = rdog * half * (tbk(i,j,k-1)+tbk(i,j,k))
        dz = h * log(pbk(i,j,k-1)/pbk(i,j,k))
        height(k) = height(k-1) + dz
      end do
    
      do k=1,nsig
       hbk(i,j,k)=height(k)
      end do
    end do
  end do

  do k=1,nsig
    do j=1,lat2
      do i=1,lon2
         tbk(i,j,k)=tbk(i,j,k)*(h1000/pbk(i,j,k))**rd_over_cp
      enddo
    enddo
  enddo

END SUBROUTINE BackgroundCldgfs

SUBROUTINE  BackgroundCld(mype,lon2,lat2,nsig,tbk,pbk,psbk,q,hbk, &
             zh,pt_ll,eta1_ll,aeta1_ll,eta2_ll,aeta2_ll,regional,wrf_mass_regional)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  BackgroundCld  Ingest background fields for cloud analysis
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-27
!
! ABSTRACT: 
!  This subroutine reads in background hydrometeor fields for cloud analysis
!
! PROGRAM HISTORY LOG:
!    2009-01-02  Hu  Add NCO document block
!    2010-04-26  Hu  delete the module gridmod and guess_grids.
!                    transfer information subroutine dummy variables
!   2017-03-23   Hu   - add code to use hybrid vertical coodinate in WRF MASS
!                      core
!
!
!   input argument list:
!     mype         - processor ID
!     lon2        - no. of lons on subdomain (buffer points on ends)
!     lat2        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of vertical levels
!     tbk         - 3D background potential temperature (K)
!     psbk        - 2D background surface pressure (hPa)
!     q           - 3D moisture (water vapor mixing ratio kg/kg)
!     zh          - terrain
!     pt_ll       -  vertical coordinate 
!     eta1_ll     -  vertical coordinate 
!     aeta1_ll    -  vertical coordinate 
!     regional    -  if regional
!     wrf_mass_regional - if mass core
!
!   output argument list:
!     pbk         - 3D background pressure  (hPa)
!     hbk         - 3D height above the ground (not the sea level)
!!!!     z_lcl       - lifting condensation level
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use gsd_kinds, only: r_single,i_kind,r_kind
  use constants, only: rd_over_cp, h1000
  use constants, only: rd, grav, half, rad2deg

  implicit none

  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(in):: lon2
  integer(i_kind),intent(in):: lat2
  integer(i_kind),intent(in):: nsig

  real(r_kind), intent(in) :: pt_ll
  real(r_kind), intent(in) :: eta1_ll(nsig+1)  !
  real(r_kind), intent(in) :: aeta1_ll(nsig)   !
  real(r_kind), intent(in) :: eta2_ll(nsig+1)  !
  real(r_kind), intent(in) :: aeta2_ll(nsig)   !
  logical,      intent(in) :: regional         ! .t. for regional background/analysis
  logical,      intent(in) :: wrf_mass_regional   ! 


! background
!
! read in from WRF
!
  real(r_single),intent(inout) :: tbk(lon2,lat2,nsig)     ! temperature
  real(r_single),intent(inout) :: psbk(lon2,lat2)         ! surface pressure
  real(r_single),intent(in)    :: zh(lon2,lat2)           ! terrain elevation
  real(r_single),intent(inout) :: q(lon2,lat2,nsig)       ! moisture
!
! derived fields
!
  real(r_single),intent(out) :: hbk(lon2,lat2,nsig)! height
  real(r_single),intent(out) :: pbk(lon2,lat2,nsig)! pressure  hPa
!  real(r_single),intent(out) :: z_lcl(lon2,lat2)   ! lifting condensation level
!
!  misc.
!
  INTEGER :: i,j,k

  REAL(r_single) :: rdog, h, dz
  REAL(r_single) :: height(nsig+1)
  real(r_single) :: q_integral(lon2,lat2),q_integralc4h(lon2,lat2)   
  real(r_single) :: deltasigma, deltasigmac4h,psfc_this
  
!
!================================================================
!
  q_integral=1
  q_integralc4h=0.0
  do k=1,nsig
    deltasigma=eta1_ll(k)-eta1_ll(k+1)
    deltasigmac4h=eta2_ll(k)-eta2_ll(k+1)
    do j=1,lat2
      do i=1,lon2
         q(i,j,k) = q(i,j,k)/(1.0_r_kind-q(i,j,k))   ! water vapor mixing ratio (kg/kg)
         q_integral(i,j)=q_integral(i,j)+deltasigma*q(i,j,k)
         q_integralc4h(i,j)=q_integralc4h(i,j)+deltasigmac4h*q(i,j,k)
      enddo
    enddo
  enddo
  do j=1,lat2
    do i=1,lon2
       psfc_this=pt_ll+(psbk(i,j)-pt_ll)/q_integral(i,j)
       psbk(i,j)= psfc_this
    enddo
  enddo

!
!  assign CAPE as 0, this part needs more work
!
!  gsfc(:,:,3)=0.0  ! CAPE, we need but not included in wrf_inout
!  1: land use;   2: sfc soil T;    3: CAPE
!
! get land use and convert latitude and longitude back to degree
!  xland=gsfc(:,:,1)
!  soil_tbk=gsfc(:,:,2)
!
! get virtual potential temperature (thv)
!
!  thv=0.0
!  do k=1,nsig
!    do j=1,nlat
!      do i=1,nlon
!         rl=qr(i,j,k)+qs(i,j,k)+qg(i,j,k)+qc(i,j,k)+qi(i,j,k)
!         thv(i,j,k)=tbk(i,j,k)*(1.0+0.61*q(i,j,k)-rl)
!      ENDDO
!    ENDDO
!  ENDDO
!!
!
! now get pressure (pbk) and height (hbk) at each grid point
!
  if(regional .and. wrf_mass_regional ) then

    do k=1,nsig
      do j=1,lat2
        do i=1,lon2
           pbk(i,j,k)=aeta1_ll(k)*(psbk(i,j)-pt_ll)+pt_ll + aeta2_ll(k)
        end do
      end do
    end do

!   Compute geopotential height at midpoint of each layer
    rdog = rd/grav
    do j=1,lat2
      do i=1,lon2
        k  = 1
        h  = rdog * tbk(i,j,k)
        dz = h * log(psbk(i,j)/pbk(i,j,k))
        height(k) = zh(i,j) + dz
      
        do k=2,nsig
          h  = rdog * half * (tbk(i,j,k-1)+tbk(i,j,k))
          dz = h * log(pbk(i,j,k-1)/pbk(i,j,k))
          height(k) = height(k-1) + dz
        end do
      
        do k=1,nsig
          hbk(i,j,k)=height(k) - zh(i,j)
        end do
      end do
    end do
  else
    write(6,*) ' Only wrf mass grid is done for cloud analysis '
    write(6,*) ' You are choosing grid that is not recoginzed by cloud analysis'
    call stop2(114)
  endif

  do k=1,nsig
    do j=1,lat2
      do i=1,lon2
         tbk(i,j,k)=tbk(i,j,k)*(h1000/pbk(i,j,k))**rd_over_cp
      enddo
    enddo
  enddo

END SUBROUTINE BackgroundCld
