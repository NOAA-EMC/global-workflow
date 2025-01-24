SUBROUTINE PrecipMxR_radar(mype,nlat,nlon,nsig,   &
                 t_bk,p_bk,ref_mos_3d,  &
                 cldpcp_type_3d,q_bk,qr_cld,qnr_3d,qs_cld,qg_cld,cldqropt)  
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  PrecipMxR_radar  find cloud liquid water content
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-20
!
! ABSTRACT: 
!  This is the driver to call subroutines that calculate liquid water content based on
!         radar reflectivity and hydrometeor type diagnosed from radar
!         and background 3-D temperature fields
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!
!     t_bk        - 3D background potential temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     ref_mos_3d  - 3D reflectivity in analysis grid (dBZ)
!     cldpcp_type_3d - 3D hydrometeor type
!     cldqropt    -  scheme used to retrieve 
!                  mixing ratios for hydrometeors related to precipitation (qr, qs, qg)
!                        1=Kessler 2=Lin 3=Thompson
!
!   output argument list:
!     qr_cld      - rain mixing ratio (g/kg)
!     qnr_3d      - rain number concentration
!     qs_cld      - snow mixing ratio (g/kg)
!     qg_cld      - graupel mixing ratio (g/kg)
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

  use constants, only: rd_over_cp, h1000
  use kinds, only: r_single,i_kind,r_kind

  implicit none
  integer(i_kind),intent(in):: nlat,nlon,nsig
  integer(i_kind),intent(in):: mype
!
!  background
!
  real(r_single),intent(in) :: t_bk(nlon,nlat,nsig)   ! potential temperature
  real(r_single),intent(in) :: p_bk(nlon,nlat,nsig)   ! height
  real(r_single),intent(in) :: q_bk(nlon,nlat,nsig)   ! mixing ratio
!
  real(r_kind),intent(in)  :: ref_mos_3d(nlon,nlat,nsig)
!
!  Variables for cloud analysis
!
  integer(i_kind),intent(in) :: cldpcp_type_3d(nlon,nlat,nsig)
!
! hydrometeors
!
  REAL(r_single),intent(out) :: qr_cld(nlon,nlat,nsig)  ! rain
  REAL(r_single),intent(out) :: qnr_3d(nlon,nlat,nsig)  ! rain number concentration(/kg)
  REAL(r_single),intent(out) :: qs_cld(nlon,nlat,nsig)  ! snow
  REAL(r_single),intent(out) :: qg_cld(nlon,nlat,nsig)  ! graupel

!-----------------------------------------------------------
!
! temp.
!

  REAL(r_single) :: t_3d(nlon,nlat,nsig)  
  REAL(r_single) :: p_3d(nlon,nlat,nsig)  
  REAL(r_kind) ::  qs_max

  INTEGER(i_kind) :: cldqropt
  INTEGER(i_kind) :: istatus_pcp
  INTEGER(i_kind) :: i,j,k
  INTEGER(i_kind) :: k_qs_max
  REAL(r_kind) ::    threshold_t_1st
  
!
!====================================================================
!  Begin
!
!  cldqropt = 2

  DO j = 2,nlat-1
    DO i = 2,nlon-1
      DO k = 1,nsig            
        t_3d(i,j,k) = t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp
        p_3d(i,j,k) = p_bk(i,j,k)*100.0_r_single
      END DO
    END DO
  END DO

!-----------------------------------------------------------------------
!
!  Calculate 3D precipitation hydrometeor mixing ratios 
!    from radar reflectivity in g/kg.
!  Note that qr_cld, qs_cld, and qg_cld are diagnosed
!  qr, qs and qg in g/kg, respectively.
!
!-----------------------------------------------------------------------
!
  IF (cldqropt == 1) THEN
!
! Kessler's scheme
!
      if(mype==0) then
        WRITE(6,'(a)') 'PrecipMxR_radar: Computing Precip mixing ratio.'
        WRITE(6,'(a)')                                                  &
          ' Using Kessler radar reflectivity equations...'
      endif
      CALL pcp_mxr (nlon,nlat,nsig,t_3d,p_3d,ref_mos_3d,                &
                    cldpcp_type_3d,                                     &
                    qr_cld,qs_cld,qg_cld,                               &
                    istatus_pcp)

  ELSE IF (cldqropt == 2) THEN
!
! Ferrier's scheme
!
      if(mype==0) then
        WRITE(6,'(a)') 'PrecipMxR_radar: Computing Precip mixing ratio.'
        WRITE(6,'(a)')                                                  &
          ' Using Ferrier radar reflectivity equations...'
      endif
      CALL pcp_mxr_ferrier_new (nlon,nlat,nsig,t_3d,p_3d,ref_mos_3d,        &
                            cldpcp_type_3d,q_bk,                         &
                            qr_cld,qs_cld,qg_cld,                       &
                            istatus_pcp)

  ELSE IF (cldqropt == 3) THEN
!
! Thompson's  scheme
!
      if(mype==0) then
        WRITE(6,'(a)') ' PrecipMxR_radar: Computing Precip mixing ratio.'
        WRITE(6,'(a)')                                                  &
          ' Using Thompson RUC radar reflectivity equations...'
      endif
!      call pcp_mxr_thompsonRUC(qr_cld,qs_cld,qg_cld,       &
!                               p_3d,t_3d,                  &
!                               ref_mos_3d,nlon,nlat,nsig,cldpcp_type_3d)
      call hydro_mxr_thompson (nlon,nlat,nsig, t_3d, p_3d, ref_mos_3d,   &
                              qr_cld,qnr_3d,qs_cld, istatus_pcp,mype)

  END IF   !cldqropt=1 or 2  or 3

END SUBROUTINE PrecipMxR_radar

