SUBROUTINE TempAdjust(mype,nlat,nlon,nsig,cldptopt, t_bk, p_bk,w_bk,q_bk, &
                    qc,qi,ctmp_bk) 

!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  TempAdjust  temperature adjustment
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-26
!
! ABSTRACT: 
!  This subroutine adjusts the perturbation potential temperature field to account
!  for the latent heating release.
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
!     cldptopt    - schemes of adjustment
!                       3=latent heat, 4,5,6 = adiabat profile
!     t_bk        - 3D background potential temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     w_bk        - 3D background vertical velocity 
!     q_bk        - 3D moisture (water vapor mixing ratio)
!     qc          - 3D cloud water mixing ratio (kg/kg)
!     qi          - 3D cloud ice mixing ratio (kg/kg)
!     ctmp_bk     - 3D cloud temperature
!
!   output argument list:
!     t_bk        - 3D background potential temperature (K)
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

  use constants, only: cp,rd_over_cp, h1000, hvap
  use kinds, only: r_single,i_kind

  implicit none
  integer(i_kind),intent(in):: nlat,nlon,nsig
  integer(i_kind),intent(in):: mype

!
!  background
!
  real(r_single),intent(inout) :: t_bk(nlon,nlat,nsig)   ! temperature
  real(r_single),intent(in) :: p_bk(nlon,nlat,nsig)   ! pressure
  real(r_single),intent(in) :: w_bk(nlon,nlat,nsig)   ! terrain
  real(r_single),intent(in) :: q_bk(nlon,nlat,nsig)   ! moisture - water vapor mixing ratio
!
!  real(r_single) :: t_bk_check(nlon,nlat,nsig)   ! temperature
!
!
! cloud water and cloud ice mixing ratios
!
  real (r_single),intent(in) :: qc(nlon,nlat,nsig)
  real (r_single),intent(in) :: qi(nlon,nlat,nsig)
  real (r_single),intent(in) :: ctmp_bk(nlon,nlat,nsig)
!
! constant
  REAL :: p0
!
!
! temp.
!
  INTEGER :: i,j,k
  INTEGER(i_kind),intent(in) :: cldptopt
  REAL :: frac_qc_2_lh, max_lh_2_pt
  REAL :: max_pt_adj
  REAL :: p0inv,arg,ptdiff
  REAL :: ppi,wratio,ptcld
!
!
!-----------------------------------------------------------
!
!  t_bk_check=0.0

  p0=h1000
!
  wratio=1.0
!  cldptopt=3
  frac_qc_2_lh =1.0
  max_lh_2_pt=20.0
!
  IF (cldptopt == 3) THEN
if(mype==0) then
    WRITE(6,'(a)')'TempAdjust: Adjusting t_bk to account for latent heating.'
    WRITE(6,'(a,f10.4,a,f10.4)')                                        &
         'TempAdjust: frac of qc:',frac_qc_2_lh,' adj_lim:',max_lh_2_pt
endif
    p0inv=1./p0
    max_pt_adj = 0.0
    DO k=2,nsig
      DO j=2,nlat-1
        DO i=2,nlon-1
          arg=max(0.0,qc(i,j,k)) + max(0.0,qi(i,j,k))
          if( arg > 0.0 ) then
            ppi = (p_bk(i,j,k)*p0inv) ** rd_over_cp
            arg = hvap*frac_qc_2_lh*arg*0.001/(cp*ppi)
            max_pt_adj = MAX(max_pt_adj,arg)
            t_bk(i,j,k) = t_bk(i,j,k) + MIN(arg,max_lh_2_pt)
          endif
        END DO
      END DO
    END DO
  if(mype==0)  PRINT*,'max_adj=',max_pt_adj
  ELSE IF (cldptopt == 4) THEN
if(mype==0) then
    WRITE(6,'(a)')'TempAdjust: Adjusting t_bk to account for latent heating in w.'
    PRINT*,'frac of qc:',frac_qc_2_lh,' adj_lim:',max_lh_2_pt
endif
    max_pt_adj = 0.0
    DO k=2,nsig
      DO j=2,nlat-1
        DO i=2,nlon-1
          IF(w_bk(i,j,k) > 0. .and. ctmp_bk(i,j,k) > 0.0) THEN
            wratio=1.0
            ptcld=ctmp_bk(i,j,k)*(p0/p_bk(i,j,k))**rd_over_cp
            ptdiff=ptcld-t_bk(i,j,k)
            IF(ptdiff > 0.) THEN
              arg = frac_qc_2_lh*wratio*ptdiff
              t_bk(i,j,k) = t_bk(i,j,k) + MIN(arg,max_lh_2_pt)
              max_pt_adj = MAX(max_pt_adj,arg)
            END IF
          END IF
        END DO
      END DO
    END DO
  if(mype==0)  PRINT*,'max_adj=',max_pt_adj
  ELSE IF (cldptopt == 5) THEN
if(mype==0) then
    WRITE(6,'(a)')'TempAdjust: Adjusting t_bk to moist-adiab cloud temp for w>-0.2'
    PRINT*,'frac of qc:',frac_qc_2_lh,' adj_lim:',max_lh_2_pt
endif
    max_pt_adj = 0.0
    DO k=2,nsig
      DO j=2,nlat-1
        DO i=2,nlon-1
          IF( ctmp_bk(i,j,k) > 0.0) THEN
            wratio=min(max(0.,(5.0*(w_bk(i,j,k)+0.2))),1.0)
            ptcld=ctmp_bk(i,j,k)*(p0/p_bk(i,j,k))**rd_over_cp
            ptdiff=ptcld-t_bk(i,j,k)
            IF(ptdiff > 0.) THEN
              arg = frac_qc_2_lh*wratio*ptdiff
              t_bk(i,j,k) = t_bk(i,j,k) + MIN(arg,max_lh_2_pt)
              max_pt_adj = MAX(max_pt_adj,arg)
            END IF
          ENDIF
        END DO
      END DO
    END DO
  if(mype==0)  PRINT*,'max_adj=',max_pt_adj

  ELSE IF (cldptopt == 6) THEN
if(mype==0) then
    WRITE(6,'(a)')'TempAdjust: Adjusting t_bk to moist-adiab cloud temp for w>0.0'
    PRINT*,'frac of qc:',frac_qc_2_lh,' adj_lim:',max_lh_2_pt
endif
    max_pt_adj = 0.0
    DO k=2,nsig
      DO j=2,nlat-1
        DO i=2,nlon-1
          IF(w_bk(i,j,k) > 0. .and. ctmp_bk(i,j,k)>0.0 ) THEN
            ptcld=ctmp_bk(i,j,k)*(p0/p_bk(i,j,k))**rd_over_cp
            ptdiff=ptcld-t_bk(i,j,k)
            IF(ptdiff > 0.) THEN
              arg = frac_qc_2_lh*ptdiff
              t_bk(i,j,k) = t_bk(i,j,k) + MIN(arg,max_lh_2_pt)
!              t_bk_check(i,j,k) = MIN(arg,max_lh_2_pt)
              max_pt_adj = MAX(max_pt_adj,arg)
            END IF
          END IF
        END DO
      END DO
    END DO
  if(mype==0)  PRINT*,'max_adj=',max_pt_adj

  END IF   ! cldptopt=3?

!  t_bk = t_bk_check

END SUBROUTINE TempAdjust
