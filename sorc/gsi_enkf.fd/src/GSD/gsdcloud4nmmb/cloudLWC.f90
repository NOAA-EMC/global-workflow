SUBROUTINE cloudLWC_stratiform(mype,nlat,nlon,nsig,q_bk,t_bk,p_bk, &
                 cld_cover_3d,cld_type_3d,wthr_type,cloudlayers_i,  &
                 cldwater_3d,cldice_3d)
!
!  find cloud liquid water content
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudLWC_stratiform  find cloud liquid water content
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-20
!
! ABSTRACT: 
!  This subroutine calculate liquid water content for stratiform cloud
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     q_bk        - 3D moisture
!     t_bk        - 3D background potential temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     cld_cover_3d- 3D cloud cover
!     cld_type_3d - 3D cloud type
!     wthr_type   - 3D weather type
!     cloudlayers_i - 3D cloud layer index
!
!   output argument list:
!     cldwater_3d - 3D cloud water mixing ratio (g/kg)
!     cldice_3d   - 3D cloud ice mixing ratio  (g/kg)
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
  use kinds, only: r_single,i_kind, r_kind

  implicit none

  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(in):: nlat,nlon,nsig
!
!  background
!
  real(r_single),intent(in)    :: t_bk(nlon,nlat,nsig)   ! potential temperature
  real(r_single),intent(inout) :: q_bk(nlon,nlat,nsig)   ! mixing ratio (kg/kg)
  real(r_single),intent(in)    :: p_bk(nlon,nlat,nsig)   ! pressure
!
!
!  Variables for cloud analysis
!
  real (r_single),intent(inout) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(in) :: cld_type_3d(nlon,nlat,nsig)
  integer(i_kind),intent(in) :: wthr_type(nlon,nlat)
!
!  cloud layers
!
  integer(i_kind),intent(in) :: cloudlayers_i(nlon,nlat,21)  ! 5 =different layers
!                                      1= the number of layers
!                                      2,4,... bottom
!                                      3,5,... top
!
! cloud water and cloud ice
!
  real (r_single),intent(out) :: cldwater_3d(nlon,nlat,nsig)
  real (r_single),intent(out) :: cldice_3d(nlon,nlat,nsig)
  real (r_single) :: cloudtmp_3d(nlon,nlat,nsig)
!-----------------------------------------------------------
!
! temp.
!
  INTEGER(i_kind) :: i,j,k,ilvl,nlvl
  INTEGER(i_kind) :: kb,kt,k1
  real(r_single) :: p_pa_1d(nsig), thv(nsig)
  real(r_single) :: cld_base_m, cld_top_m
  real(r_single) :: cld_base_qc_m, cld_top_qc_m
  real(r_single) :: cloudqvis(nlon,nlat,nsig)
  real(r_single) :: rh(nlon,nlat,nsig)

! --- Key parameters
!     Rh_clear_p        = 0.80          RH to use when clearing cloud
!     Cloud_q_qvis_rat_p= 0.10          Ratio of cloud water to water/ice

  real(r_single)    Cloud_q_qvis_rat_p, cloud_q_qvis_ratio
  real(r_single)    auto_conver
  real(r_single)    cloud_def_p
  real(r_single)    rh_cld3_p
  real(r_single)    rh_clear_p
  data  Cloud_q_qvis_rat_p/ 0.05_r_single/
  data  auto_conver       /0.0002_r_single/
  data  cloud_def_p       /0.000001_r_single/
  data  rh_cld3_p         /0.98_r_single/    ! mhu, do we need to adjust this number to 0.94, WPP has PBL top set as 0.95
  data  rh_clear_p        /0.8_r_single/

  real(r_kind) ::  es0_p
  parameter (es0_p=6.1121_r_kind)     ! saturation vapor pressure (mb)
  real(r_kind) SVP1,SVP2,SVP3
  data SVP1,SVP2,SVP3/es0_p,17.67_r_kind,29.65_r_kind/

  real(r_kind) :: temp_qvis1, temp_qvis2
  data temp_qvis1, temp_qvis2 /268.15_r_kind, 263.15_r_kind/

  REAL(r_kind) stab, stab_threshold
  LOGICAL :: l_prt
  INTEGER(i_kind) :: iflag_slwc
  INTEGER(i_kind) :: kp3,km3

  REAL(r_kind) :: q, Temp, tv, evs, qvs1, eis, qvi1, watwgt, qavail
!
!====================================================================
!  Begin
!
  cldwater_3d=-99999.9_r_kind
  cldice_3d=-99999.9_r_kind
  cloudtmp_3d=-99999.9_r_kind
!-----------------------------------------------------------------------
!
!  Find Cloud Layers and Computing Output Field(s)
!  The procedure works column by column.
!
!-----------------------------------------------------------------------
!
  rh=0.0
  DO j = 2,nlat-1
    DO i = 2,nlon-1
!
      DO k = 2,nsig-1
        p_pa_1d(k) = p_bk(i,j,k)*100.0_r_single
        q = q_bk(i,j,k)/(1._r_single+q_bk(i,j,k))     !  Q = water vapor specific humidity
                                                      !  q_bk = water vapor mixing ratio
        tv = t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp
! now, tmperature from GSI s potential temperature
        Temp = tv   ! temperature
! evs, eis in mb
        evs = svp1*exp(SVP2*(Temp-273.15_r_kind)/(Temp-SVP3))
        qvs1 = 0.62198_r_kind*evs*100._r_kind/(p_pa_1d(k)-100._r_kind*evs)   ! qvs1 is mixing ratio kg/kg, so no need next line
!      qvs1 = qvs1/(1.0-qvs1)
        eis = svp1 *exp(22.514_r_kind - 6.15e3_r_kind/Temp)
        qvi1 = 0.62198_r_kind*eis*100._r_kind/(p_pa_1d(k)-100._r_kind*eis)   ! qvi1 is mixing ratio kg/kg, so no need next line
!      qvi1 = qvi1/(1.0-qvi1)
!      watwgt = max(0.,min(1.,(Temp-233.15)/(263.15-233.15)))
! ph - 2/7/2012 - use ice mixing ratio only for temp < 263.15
        watwgt = max(0._r_kind,min(1._r_kind,(Temp-temp_qvis2)/&
                                     (temp_qvis1-temp_qvis2)))
        cloudtmp_3d(i,j,k)= Temp
        cloudqvis(i,j,k)= (watwgt*qvs1 + (1._r_kind-watwgt)*qvi1)
!      qvis(i,j,k)= (watwgt*qvs1 + (1.-watwgt)*qvi1)
        rh(i,j,k) = q_bk(i,j,k)/cloudqvis(i,j,k)
      enddo
    enddo   ! i
  enddo     ! j

  stab_threshold = 3._r_kind/10000._r_kind
  DO j = 2,nlat-1
    DO i = 2,nlon-1
      DO k = 1,nsig
        p_pa_1d(k) = p_bk(i,j,k)*100.0_r_kind
        thv(k)     = t_bk(i,j,k)*(1.0_r_kind + 0.6078_r_kind*q_bk(i,j,k))
      ENDDO
      nlvl=cloudlayers_i(i,j,1)
      if(nlvl > 0 ) then
        DO ilvl = 1, nlvl          ! loop through cloud layers
          kb=cloudlayers_i(i,j,2*ilvl)
          kt=cloudlayers_i(i,j,2*ilvl+1)
          DO k = kb,kt

! -- change these to +/- 3 vertical levels
            kp3 = min(nsig,k+5)
            km3 = max(1     ,k)
            stab = (thv(kp3)-thv(km3))/(p_pa_1d(km3)-p_pa_1d(kp3))

! -- stability check.  Use 2K/100 mb above 600 mb and
!       3K/100mb below (nearer sfc)
            if ((stab<stab_threshold .and. p_pa_1d(k)/100._r_kind>600._r_kind)   &
                     .or. stab<0.66_r_kind*stab_threshold )  then
!               write(*,'(a,3i4,f8.3)') 'skip building cloud in stable layer',i,j,k,stab*10000.0
               cld_cover_3d(i,j,k)=-99999.0
            elseif(rh(i,j,k) < 0.40 .and. ((cloudqvis(i,j,k)-q_bk(i,j,k)) > 0.003_r_kind)) then
!               write(*,'(a,3i4,2f6.2)') 'skip building cloud in too-dry layer',i,j,k,& 
!                            rh(i,j,k),(cloudqvis(i,j,k)-q_bk(i,j,k))*1000.0
               cld_cover_3d(i,j,k)=-99999.0_r_single
            else
!dk * we need to avoid adding cloud if sat_ctp is lower than 650mb
! ph - 2/7/2012 - use a temperature-dependent cloud_q_qvis_ratio 
!                 and with 0.1 smaller condensate mixing ratio building also for temp < 263.15
               Temp = cloudtmp_3d(i,j,k)
               watwgt = max(0._r_kind,min(1._r_kind,(Temp-temp_qvis2)/&
                                     (temp_qvis1-temp_qvis2)))
               cloud_q_qvis_ratio = watwgt*cloud_q_qvis_rat_p  &
                                    + (1.0-watwgt)*0.1*cloud_q_qvis_rat_p
               qavail = min(0.5_r_single*auto_conver,cloud_q_qvis_ratio*cloudqvis(i,j,k))

!    -------------------------------------------------------------------
!   - set cloud water mixing ratio  - no more than 0.1 g/kg,
!      which is the current autoconversion mixing ratio set in exmoisg
!      according to John Brown - 14 May 99
!    -------------------------------------------------------------------
               cldwater_3d(i,j,k) = watwgt*qavail*1000.0_r_kind   ! g/kg
!   - set ice mixing ratio
               cldice_3d(i,j,k)= (1.-watwgt)*qavail*1000.0_r_kind   ! g/kg
!              end if
            end if
          enddo   ! k
        enddo   ! ilvl
      endif   !  nlvl > 1
    enddo  ! i
  enddo    ! j

END SUBROUTINE cloudLWC_stratiform

SUBROUTINE cloudLWC_Cumulus(nlat,nlon,nsig,h_bk,t_bk,p_bk,                         &
                 cld_cover_3d,cld_type_3d,wthr_type,cloudlayers_i,  &
                 cldwater_3d,cldice_3d,cloudtmp_3d)
!
!  find cloud liquid water content
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudLWC_Cumulus  find cloud liquid water content for cumulus cloud
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-20
!
! ABSTRACT: 
!  This subroutine calculates liquid water content for cumulus cloud
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     h_bk        - 3D height
!     t_bk        - 3D background potentional temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     cld_cover_3d- 3D cloud cover
!     cld_type_3d - 3D cloud type
!     wthr_type   - 3D weather type
!     cloudlayers_i - 3D cloud layer index
!
!   output argument list:
!     cldwater_3d - 3D cloud water mixing ratio (g/kg)
!     cldice_3d   - 3D cloud ice mixing ratio  (g/kg)
!     cloudtmp_3d - 3D cloud temperature
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
  integer(i_kind),intent(in) :: nlat,nlon,nsig
!
!  surface observation
!
!
!  background
!
  real(r_single),intent(in) :: t_bk(nlon,nlat,nsig)   ! temperature
  real(r_single),intent(in) :: h_bk(nlon,nlat,nsig)   ! height
  real(r_single),intent(in) :: p_bk(nlon,nlat,nsig)   ! pressure
!
!
!  Variables for cloud analysis
!
  real (r_single),intent(in) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(in) :: cld_type_3d(nlon,nlat,nsig)
  integer(i_kind),intent(in) :: wthr_type(nlon,nlat)
!
!  cloud layers
!
  integer(i_kind),intent(in) :: cloudlayers_i(nlon,nlat,21)  ! 5 =different layers
!                                      1= the number of layers
!                                      2,4,... bottom
!                                      3,5,... top
!
! cloud water and cloud ice
!
  real (r_single),intent(out) :: cldwater_3d(nlon,nlat,nsig)
  real (r_single),intent(out) :: cldice_3d(nlon,nlat,nsig)
  real (r_single),intent(out) :: cloudtmp_3d(nlon,nlat,nsig)
!-----------------------------------------------------------
!
! temp.
!
  INTEGER(i_kind) :: i,j,k,ilvl,nlvl
  INTEGER(i_kind) :: kb,kt,k1
  real (r_single) :: zs_1d(nsig)
  real (r_single) :: cv_1d(nsig)
  real (r_single) :: t_1d(nsig)
  real (r_single) :: p_pa_1d(nsig)
  real (r_single) :: p_mb_1d(nsig)
  real (r_single) :: cld_base_m, cld_top_m
  real (r_single) :: cld_base_qc_m, cld_top_qc_m

  real (r_single) :: slwc_1d(nsig)
  real (r_single) :: cice_1d(nsig)
  real (r_single) :: ctmp_1d(nsig)

  LOGICAL :: l_prt
  INTEGER(i_kind) :: iflag_slwc
!
!====================================================================
!  Begin
!
  l_prt =.false.
  iflag_slwc = 11
  cldwater_3d=-99999.9_r_single
  cldice_3d  =-99999.9_r_single
  cloudtmp_3d=-99999.9_r_single
!-----------------------------------------------------------------------
!
!  Find Cloud Layers and Computing Output Field(s)
!  The procedure works column by column.
!
!-----------------------------------------------------------------------
!
  DO j = 2,nlat-1
    DO i = 2,nlon-1
!
      DO k = 1,nsig                      ! Initialize
        t_1d(k) = t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp
        zs_1d(k) = h_bk(i,j,k)
        p_pa_1d(k) = p_bk(i,j,k)*100.0_r_single
        p_mb_1d(k) = p_bk(i,j,k)
      END DO
!-----------------------------------------------------------------------
      nlvl=cloudlayers_i(i,j,1)
      if(nlvl > 0 ) then
        DO ilvl = 1, nlvl          ! loop through cloud layers

          kb=cloudlayers_i(i,j,2*ilvl)
          kt=cloudlayers_i(i,j,2*ilvl+1)

          cld_base_m = 0.5_r_single * (zs_1d(kb-1) + zs_1d(kb))
          cld_top_m = 0.5_r_single * (zs_1d(kt) + zs_1d(kt+1))
!
          IF(iflag_slwc /= 0) THEN
             IF(iflag_slwc < 10) THEN ! simple adiabatc scheme
                CALL get_slwc1d (nsig,cld_base_m,cld_top_m,kb,kt      &
                         ,zs_1d,t_1d,p_pa_1d,iflag_slwc,slwc_1d)

             ELSE ! iflag_slwc > 10, new Smith-Feddes scheme
                DO k1 = 1,nsig ! Initialize
                   slwc_1d(k1) = 0.0_r_single
                   cice_1d(k1) = 0.0_r_single
                   ctmp_1d(k1) = t_bk(i,j,k1)
                END DO
!
!-----------------------------------------------------------------------
!
!  QC the data going into SMF
!
!-----------------------------------------------------------------------
!
                IF(cld_top_m > zs_1d(nsig-1) - 110._r_single) THEN
                   cld_top_qc_m = zs_1d(nsig-1) - 110._r_single
                   cld_base_qc_m =                                   &
                            MIN(cld_base_m,cld_top_qc_m - 110._r_single)
                ELSE ! normal case
                   cld_top_qc_m = cld_top_m
                   cld_base_qc_m = cld_base_m
                END IF
!
                CALL get_sfm_1d(nsig,cld_base_qc_m,cld_top_qc_m       &
                              ,zs_1d,p_mb_1d,t_1d                &
                              ,slwc_1d,cice_1d,ctmp_1d,l_prt)
!
             END IF ! iflag_slwc < 10
          END IF ! iflag_slwc .ne. 0
!
          DO k1 = kb,kt ! Loop through the cloud layer
            IF(iflag_slwc /= 0) THEN
              IF(slwc_1d(k1) > 0._r_single) cldwater_3d(i,j,k1)=slwc_1d(k1)
              IF(cice_1d(k1) > 0._r_single) cldice_3d(i,j,k1)=cice_1d(k1)
              cloudtmp_3d(i,j,k1)=ctmp_1d(k1)
            END IF ! iflag_slwc .ne. 0
          END DO ! k1

        enddo   ! ilvl
      endif   ! nlvl > 0

    ENDDO  ! i
  ENDDO  ! j

END SUBROUTINE cloudLWC_Cumulus
