module gsdcloudlib_pseudoq_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    gsdcloudlib_pseudoq_mod      contains cloud analysis subroutines
! for generating pseudo moisture
!   prgmmr: Ming Hu          org: GSD                date: 2019-05-29
!
! abstract: contains routines for generating pseudo moisture
!
! program history log:
!   2005-01-22  Hu
!
! subroutines included:
!   sub create_balance_vars      - create arrays for balance vars
!   sub destroy_balance_vars     - remove arrays for balance vars
!
! Variable Definitions:
!
! attributes:
!   language: f90
!   machine:  JET
!
!$$$ end documentation block

  implicit none

! set default to private
  private
! set subroutines to public
  public :: cloudCover_Surface_col
  public :: cloudLWC_pseudo

contains

SUBROUTINE cloudCover_Surface_col(mype,nsig, &
                        i_cloud_q_innovation,&
                        cld_bld_hgt,h_bk,zh,  &
                        NVARCLD_P,ocld,Oelvtn,&
                        wthr_type,pcp_type_obs,     &
                        vis2qc,cld_cover_obs)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudCover_Surface_col  cloud cover analysis for a column using surface observation
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-30
!
! ABSTRACT: 
!  This subroutine determines cloud fractional cover using surface observations
!    For each vertical column 
!    Code based on RUC assimilation code (hybfront/hybcloud.f)
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!    2017-19  Ladwig adaptation for columns 
!
!
!   input argument list:
!     mype        - processor ID
!     nsig        - no. of levels
!     i_cloud_q_innovation  - flag to control building/clearing/both 
!     cld_bld_hgt - Height below which cloud building is done
!
!     h_bk        - 3D background height  (m)
!     zh          - terrain (m)
!
!     NVARCLD_P   -  first dimension of OCLD
!     OCLD        -  cloud amount, cloud height, visibility
!     OWX         -  weather observation
!     Oelvtn      -  observation elevation
!
!   output argument list:
!     cld_cover_3d- 3D cloud cover
!     cld_type_3d - 3D cloud type
!     wthr_type   - 3D weather type
!     pcp_type_3d - 3D weather precipitation type
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
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

  use kinds, only: r_single,i_kind,r_kind

  implicit none

  integer(i_kind),intent(in) :: mype
  integer(i_kind),intent(in) :: nsig
  integer(i_kind),intent(in) :: i_cloud_q_innovation
  real(r_kind),   intent(in) :: cld_bld_hgt
!
!  surface observation
!
  INTEGER(i_kind),intent(in) :: NVARCLD_P
  INTEGER(i_kind),intent(in) :: OCLD(NVARCLD_P)  ! cloud amount, cloud height, visibility
  real(r_single), intent(in) :: Oelvtn ! elevation
  integer(i_kind), intent(inout) :: wthr_type
!
!  background
!
  real(r_kind),intent(in) :: zh         ! terrain
  real(r_kind),intent(in) :: h_bk(nsig)  ! height
!
!  Variables for cloud analysis
!
  integer(i_kind),intent(inout) :: pcp_type_obs(nsig)
  real (r_single),intent(inout) :: vis2qc
  real (r_single),intent(inout) :: cld_cover_obs(nsig)
!
!  local
!
  real (r_single) :: cloud_zthick_p
  data  cloud_zthick_p    /30._r_kind/
!
  INTEGER(i_kind) :: k
  INTEGER(i_kind) :: ic
  integer(i_kind) :: firstcloud,cl_base_broken_k,obused
  integer(i_kind) :: kcld,kclr
  real(r_single)  ::    underlim
  REAL(r_kind) :: zdiff
  REAL(r_kind) :: zlev_clr,cloud_dz,cl_base_ista,betav


!====================================================================
!  Begin
!
!  set constant names consistent with original RUC code
!
   vis2qc=-9999.0_r_single
   zlev_clr = 3650._r_kind
   firstcloud = 0
   obused =0
   kcld=-9
   kclr=99
!
!*****************************************************************
!  analysis of surface/METAR cloud observations
! *****************************************************************

!       Consider clear condition case
!       -----------------------------
   if (ocld(1)==0) then

        !QC, make sure clear ob has missing for the rest of the layers
        do ic=1,6
           if(real(abs(ocld(6+ic)),r_kind) < 55555.0_r_kind) then
              write(6,*) 'cloudCover_Surface: Observed cloud above the clear level !!!'
              write(6,*) 'cloudCover_Surface: some thing is wrong in surface cloud observation !'
              write(6,*) 'cloudCover_Surface: check the station no.', 'at process ', mype
              write(6,*) ic
              write(6,*) (ocld(k),k=1,12)
              call stop2(114)
           endif
        enddo

        ! cloud clearing obs
        if(i_cloud_q_innovation==20 .or. i_cloud_q_innovation==22) then
           do k=3,nsig,5
              if (h_bk(k) < zlev_clr) then
                 cld_cover_obs(k)=0.0_r_single
              endif
           enddo
        endif

! -- Now consider non-clear obs
!    --------------------------
   else
                
        ! legacy - increase zthick by 1.5x factor for ceiling < 900 m (~3000 ft - MVFR)
        cloud_dz = cloud_zthick_p
        cl_base_broken_k = -9

        do ic = 1,6
           if (obused == 0) then
              if (ocld(ic)>0 .and. ocld(ic)<50) then
                 if(ocld(ic) == 4) then
                    if(wthr_type > 10 .and. wthr_type < 20) cloud_dz = 1000._r_kind  
                                            ! precipitation + highest level
                    if(wthr_type == 1) cloud_dz = 10000._r_kind  ! thunderstorm
                 endif

                 ! convert cloud base observation from AGL to ASL
                 cl_base_ista = real(ocld(6+ic),r_kind) + Oelvtn - zh
                 if(zh < 1.0_r_kind .and. Oelvtn > 20.0_r_kind &
                    .and. real(ocld(6+ic),r_kind) < 250.0_r_kind) then
                    cycle   ! limit the use of METAR station over oceas for low cloud base
                 endif
                 
                 firstcloud = 0
                 underlim = 10._r_kind   !

                 do k=1,nsig
                    if (firstcloud==0) then
                       zdiff = cl_base_ista - h_bk(k)
!     Must be within cloud_dz meters (300 or 1000 currently)
!    -------------------------------------------------------------------
!  -- Bring in the clouds if model level is within 10m under cloud level.
                       if(k==1)  underlim=(h_bk(k+1)-h_bk(k))*0.5_r_kind
                       if(k==2)  underlim=10.0_r_kind    ! 100 feet
                       if(k==3)  underlim=20.0_r_kind    ! 300 feet
                       if(k==4)  underlim=15.0_r_kind    ! 500 feet
                       if(k==5)  underlim=33.0_r_kind    ! 1000 feet
                       if (k>=6 .and. k <= 7) underlim = (h_bk(k+1)-h_bk(k))*0.6_r_kind
                       if(k==8)  underlim=95.0_r_kind    ! 3000 feet
                       if(k>=9 .and. k<nsig-1) underlim=(h_bk(k+1)-h_bk(k))*0.8_r_kind
                       if (zdiff<underlim) then
                          !build cloud
                          if(i_cloud_q_innovation==20 .or. i_cloud_q_innovation==21) then
                          !double check logic for following if statement
                          if((cl_base_ista >= 1.0_r_kind .and. (firstcloud==0 .or. abs(zdiff)<cloud_dz)) .or. &
                             (cl_base_ista < 1.0_r_kind  .and. (abs(zdiff)<cloud_dz)) ) then
                             !limit cloud building to below a specified height 
                             if (h_bk(k) < cld_bld_hgt) then 
                               if(ocld(ic) == 1 ) then
                                  pcp_type_obs(k)=0
                               elseif (ocld(ic) == 2 ) then
                               elseif (ocld(ic) == 3 ) then
                                  cld_cover_obs(k)=max(cld_cover_obs(k),0.7_r_single)
                                  if(cl_base_broken_k < 0 ) cl_base_broken_k=k
                                  obused = 1
                               elseif (ocld(ic) == 4 ) then
                                  cld_cover_obs(k)=max(cld_cover_obs(k),1.01_r_single)
                                  obused = 1
                                  if(cl_base_broken_k < 0 ) cl_base_broken_k=k
                                  if(wthr_type == 1) then
                                     pcp_type_obs(k)=1
                                  endif
                                  if(wthr_type > 10 .and. wthr_type < 20)  then
                                     pcp_type_obs(k)=1
                                  endif
                               else
                                  write(6,*) 'cloudCover_Surface: wrong cloud coverage observation!'
                                  call stop2(114)
                               endif !ocld values
                             endif ! below cld_bld_hgt
                             kcld=k
                             firstcloud = firstcloud + 1
                          endif  ! zdiff < cloud_dz
                          endif ! i_cloud_q_innovation=20or21
                       endif  ! zdiff<underlim
                    endif ! firstcloud
                 enddo  ! end K loop
              endif  ! end if ocld valid

              ! after cloud base is found, clear ~half way below
              if(i_cloud_q_innovation==20 .or. i_cloud_q_innovation==22) then
                 kclr=kcld/2
                 if(kclr>= 3) cld_cover_obs(kclr)=0.0_r_single
              endif

           endif  ! obused
        enddo      ! end IC loop
     endif      ! end if cloudy ob  

! -- Use visibility for low-level cloud whether
     if (wthr_type < 30   .and. wthr_type > 20 .and. &
         ocld(13)  < 5000 .and. ocld(13) > 1 ) then
         betav = 3.912_r_kind / (real(ocld(13),r_kind) / 1000._r_kind)
         vis2qc = ( (betav/144.7_r_kind) ** 1.14_r_kind) / 1000._r_kind
     endif  ! cloud or clear

END SUBROUTINE cloudCover_Surface_col


SUBROUTINE cloudLWC_pseudo(nsig,q_bk,t_bk,p_bk, &
                 cld_cover_obs,  &
                 cldwater_obs,cldice_obs)
!
!  find cloud liquid water content
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudLWC_pseudo  find cloud liquid water content
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-20
!
! ABSTRACT: 
!  This subroutine calculate liquid water content for stratiform cloud
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!    2017-19 Ladwig Adapt for pseudo obs
!
!
!   input argument list:
!     mype        - processor ID
!     nsig        - no. of levels
!     q_bk        - 3D moisture
!     t_bk        - 3D background potential temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     cld_cover_obs- vertical column of cloud cover
!     cloudlayers_i - 3D cloud layer index
!
!   output argument list:
!     cldwater_obs - vertical column cloud water mixing ratio (g/kg)
!     cldice_obs   - vertical column cloud ice mixing ratio  (g/kg)
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

  use constants, only: rd_over_cp
  use kinds, only: r_single,i_kind, r_kind

  implicit none

  integer(i_kind),intent(in):: nsig
!
!  background
!
  real(r_kind),intent(in)    :: t_bk(nsig)   ! potential temperature
  real(r_kind),intent(inout) :: q_bk(nsig)   ! mixing ratio (kg/kg)
  real(r_kind),intent(in)    :: p_bk(nsig)   ! pressure
!
!
!  Variables for cloud analysis
!
  real (r_single),intent(inout) :: cld_cover_obs(nsig)
!
! cloud water and cloud ice
!
  real (r_single),intent(out) :: cldwater_obs(nsig)
  real (r_single),intent(out) :: cldice_obs(nsig)
  real (r_single) :: cloudtmp_obs(nsig)
!-----------------------------------------------------------
!
! temp.
!
  INTEGER(i_kind) :: k
  real(r_single) :: p_pa_1d(nsig), thv(nsig)
  real(r_single) :: cloudqvis(nsig)
  real(r_single) :: rh(nsig)

! --- Key parameters
!     Rh_clear_p        = 0.80          RH to use when clearing cloud
!     Cloud_q_qvis_rat_p= 0.10          Ratio of cloud water to water/ice

  real(r_single)    Cloud_q_qvis_rat_p, cloud_q_qvis_ratio
  real(r_single)    auto_conver
  data  Cloud_q_qvis_rat_p/ 0.05_r_single/
  data  auto_conver       /0.0002_r_single/

  real(r_kind) ::  es0_p
  parameter (es0_p=6.1121_r_kind)     ! saturation vapor pressure (mb)
  real(r_kind) SVP1,SVP2,SVP3
  data SVP1,SVP2,SVP3/es0_p,17.67_r_kind,29.65_r_kind/

  real(r_kind) :: temp_qvis1, temp_qvis2
  data temp_qvis1, temp_qvis2 /268.15_r_kind, 263.15_r_kind/

  REAL(r_kind) stab, stab_threshold
  INTEGER(i_kind) :: kp3,km3

  REAL(r_kind) :: Temp, evs, qvs1, eis, qvi1, watwgt, qavail
!
!====================================================================
!  Begin
!
  cldwater_obs=-99999.9_r_kind
  cldice_obs=-99999.9_r_kind
  cloudtmp_obs=-99999.9_r_kind
  rh=0.0_r_single
  stab_threshold = 3._r_kind/10000._r_kind
!-----------------------------------------------------------------------
!
!  Find Cloud Layers and Computing Output Field(s)
!  The procedure works column by column.
!
!-----------------------------------------------------------------------
!
      !VIRTUAL POTENTIAL TEMP
  do k = 1,nsig
     thv(k)     = (t_bk(k)*(100._r_single/p_bk(k))**rd_over_cp)*(1.0_r_single + 0.6078_r_single*q_bk(k))
     !p_pa1d is pressure in pascal
     p_pa_1d(k) = p_bk(k)*1000.0_r_single
  enddo


  do k = 2,nsig-1
        
     if (cld_cover_obs(k) <= -0.001_r_kind) then
         cycle
     elseif (cld_cover_obs(k) > -0.001_r_kind .and. cld_cover_obs(k) < 0.001_r_kind) then
         cldwater_obs(k) = 0.0_r_kind   
         cldice_obs(k)= 0.0_r_kind 
     ! non-var analysis also clears for partial cloud, changes will be considered for this case 
     elseif (cld_cover_obs(k) > 0.001_r_kind .and. cld_cover_obs(k) < 0.6_r_kind ) then
         cldwater_obs(k) = 0.0_r_kind   
         cldice_obs(k)= 0.0_r_kind    
     elseif (cld_cover_obs(k) >= 0.6_r_kind .and. cld_cover_obs(k) < 1.5_r_kind) then
        !t_bk is sensible temp
        Temp=t_bk(k)
    
        ! evs, eis in mb
        evs = svp1*exp(SVP2*(Temp-273.15_r_kind)/(Temp-SVP3))
        qvs1 = 0.62198_r_kind*evs*100._r_kind/(p_pa_1d(k)-100._r_kind*evs)   ! qvs1 is mixing ratio kg/kg, so no need next line
        eis = svp1 *exp(22.514_r_kind - 6.15e3_r_kind/Temp)
        qvi1 = 0.62198_r_kind*eis*100._r_kind/(p_pa_1d(k)-100._r_kind*eis)   ! qvi1 is mixing ratio kg/kg, so no need next line
        ! ph - 2/7/2012 - use ice mixing ratio only for temp < 263.15
        watwgt = max(0._r_kind,min(1._r_kind,(Temp-temp_qvis2)/&
                                     (temp_qvis1-temp_qvis2)))
        cloudtmp_obs(k)= Temp
        cloudqvis(k)= (watwgt*qvs1 + (1._r_kind-watwgt)*qvi1)
        rh(k) = q_bk(k)/cloudqvis(k)

        ! STABILITY CHECK, used in RUC keep it for now
        ! -- change these to +/- 3 vertical levels
        kp3 = min(nsig,k+5)
        km3 = max(1     ,k)
        stab = (thv(kp3)-thv(km3))/(p_pa_1d(km3)-p_pa_1d(kp3))

        ! -- stability check.  Use 2K/100 mb above 600 mb and
        ! 3K/100mb below (nearer sfc)
        if ((stab<stab_threshold .and. p_pa_1d(k)/100._r_kind>600._r_kind)   &
                     .or. stab<0.66_r_kind*stab_threshold )  then
               cld_cover_obs(k)=-9999.0_r_single
        elseif(rh(k) < 0.40_r_single .and. ((cloudqvis(k)-q_bk(k)) > 0.003_r_kind)) then
               cld_cover_obs(k)=-9999.0_r_single
        else
        !dk * we need to avoid adding cloud if sat_ctp is lower than 650mb
        ! ph - 2/7/2012 - use a temperature-dependent cloud_q_qvis_ratio 
        !      and with 0.1 smaller condensate mixing ratio building also for temp < 263.15
               Temp = cloudtmp_obs(k)
               watwgt = max(0._r_kind,min(1._r_kind,(Temp-temp_qvis2)/&
                                     (temp_qvis1-temp_qvis2)))
               cloud_q_qvis_ratio = watwgt*cloud_q_qvis_rat_p  &
                                    + (1.0_r_single-watwgt)*0.1_r_single*cloud_q_qvis_rat_p
               qavail = min(0.5_r_single*auto_conver,cloud_q_qvis_ratio*cloudqvis(k))

               !-------------------------------------------------------------------
               ! - set cloud water mixing ratio  - no more than 0.1 g/kg,
               !   which is the current autoconversion mixing ratio set in exmoisg
               !   according to John Brown - 14 May 99
               !-------------------------------------------------------------------
               cldwater_obs(k) = watwgt*qavail*1000.0_r_kind   ! g/kg
               !   - set ice mixing ratio
               cldice_obs(k)= (1.-watwgt)*qavail*1000.0_r_kind   ! g/kg
        end if
     else
        write(*,*) 'WARNING, cld_cover_obs outside of known ranges.', cld_cover_obs(k)
     endif
  enddo   ! k

END SUBROUTINE cloudLWC_pseudo

end module gsdcloudlib_pseudoq_mod
