SUBROUTINE cloudCover_Surface(mype,nlat,nlon,nsig,thunderRadius,&
                        cld_bld_hgt,t_bk,p_bk,q,h_bk,zh,  &
                        mxst_p,NVARCLD_P,numsao,OI,OJ,OCLD,OWX,Oelvtn,Odist,&
                        cld_cover_3d,cld_type_3d,wthr_type,pcp_type_3d,     &
                        watericemax, kwatericemax,vis2qc)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudCover_Surface  cloud cover analysis using surface observation
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-30
!
! ABSTRACT: 
!  This subroutine determines 3D cloud fractional cover using surface observations
!    Code based on RUC assimilation code (hybfront/hybcloud.f)
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
!     thunderRadius -
!     cld_bld_hgt - Height below which cloud building is done 
!
!     t_bk        - 3D background potentional temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     q           - 3D moisture (water vapor mixing ratio)
!     h_bk        - 3D background height  (m)
!     zh          - terrain (m)
!
!     mxst_p      -  maximum observation number
!     NVARCLD_P   -  first dimension of OLCD
!     numsao      -  observation number
!     OI          -  observation x location
!     OJ          -  observation y location
!     OLCD        -  cloud amount, cloud height, visibility
!     OWX         -  weather observation
!     Oelvtn      -  observation elevation
!     Odist       -  distance from the nearest station
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

  use gsd_kinds, only: r_single,i_kind,r_kind

  implicit none

  integer(i_kind),intent(in) :: mype
  integer(i_kind),intent(in) :: nlat,nlon,nsig
  real(r_single), intent(in) :: thunderRadius
  real(r_kind),   intent(in) :: cld_bld_hgt
!
!  surface observation
!
  INTEGER(i_kind),intent(in) :: mxst_p,NVARCLD_P

!  PARAMETER (LSTAID_P=9)

  INTEGER(i_kind),intent(in) :: numsao
  real(r_single), intent(in) :: OI(mxst_p)  ! x location
  real(r_single), intent(in) :: OJ(mxst_p)  ! y location
  INTEGER(i_kind),intent(in) :: OCLD(NVARCLD_P,mxst_p)  ! cloud amount, cloud height,
                                               ! visibility
  CHARACTER*10,   intent(in) :: OWX(mxst_p)    ! weather
  real(r_single), intent(in) :: Oelvtn(mxst_p) ! elevation
  real(r_single), intent(in) :: Odist(mxst_p)  ! distance from the nearest station

!
!  background
!
  real(r_single),intent(in) :: t_bk(nlon,nlat,nsig)  ! temperature
  real(r_single),intent(in) :: p_bk(nlon,nlat,nsig)  ! pressure
  real(r_single),intent(in) :: zh(nlon,nlat)         ! terrain
  real(r_single),intent(in) :: q(nlon,nlat,nsig)     ! moisture, water vapor mixing ratio (kg/kg)
  real(r_single),intent(in) :: h_bk(nlon,nlat,nsig)  ! height
!
  REAL(r_single),intent(in)   :: watericemax(mxst_p)  ! max of background total liquid water in station
  INTEGER(i_kind),intent(in):: kwatericemax(nlon,nlat)  ! lowest level of background total liquid water in grid
!
!  Variables for cloud analysis
!
  real (r_single),intent(inout) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: cld_type_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: wthr_type(nlon,nlat)
  integer(i_kind),intent(inout) :: pcp_type_3d(nlon,nlat,nsig)
  real (r_single),intent(inout) :: vis2qc(nlon,nlat)
!
!  local
!
  real (r_single) :: cloud_zthick_p
  data  cloud_zthick_p    /300._r_kind/
!
  REAL (r_kind)   :: spval_p
  PARAMETER ( spval_p    =  99999.0_r_kind )

  INTEGER(i_kind) :: i,j,k
  INTEGER(i_kind) :: i1,j1,ic
  INTEGER(i_kind) :: nx_p, ny_p, nztn_p
  INTEGER(i_kind) :: ista
  INTEGER(i_kind) :: ich !, iob,job 
  
  REAL(r_kind) :: min_dist !, dist
  REAL(r_kind) :: zdiff
  REAL(r_kind) :: zlev_clr,cloud_dz,cl_base_ista,betav
!
!
!
  real(r_single):: tbk_k(nlon,nlat,nsig)
  real(r_single):: cv_bk(nlon,nlat,nsig)
  real(r_single):: z_lcl(nlon,nlat)
  REAL(r_kind)  :: cf_model_base,t_model_base, ht_base
  REAL(r_kind)  :: t_dry_adiabat,t_inversion_strength

  LOGICAL :: l_cf,l_inversion
  LOGICAL :: if_cloud_exist

  integer(i_kind) :: firstcloud,cl_base_broken_k
  real(r_single)  ::    underlim
  integer(i_kind) :: npts_near_clr


!====================================================================
!  Begin
!
!  set constant names consistent with original RUC code
!
   nx_p=nlon
   ny_p=nlat
   nztn_p=nsig

   vis2qc=-9999.0_r_kind
   npts_near_clr=0
   zlev_clr = 3650.
!
!
!*****************************************************************
!  analysis of surface/METAR cloud observations
! *****************************************************************

   loopstation: DO ista=1,numsao
     i1 = int(oi(ista)+0.0001_r_kind) 
     j1 = int(oj(ista)+0.0001_r_kind)
     min_dist =  Odist(ista)

!mh - grid point has the closest cloud station

! -- find out if any precip is present
     do ich=1,1
       if ( owx(ista)(ich:ich+1)=='SH'  ) wthr_type(i1,j1)=16
       if ( owx(ista)(ich:ich+1)=='TH' .and. &
                min_dist < thunderRadius) wthr_type(i1,j1)=1
       if ( owx(ista)(ich:ich+1)=='RA'  ) wthr_type(i1,j1)=11
       if ( owx(ista)(ich:ich+1)=='SN'  ) wthr_type(i1,j1)=12
       if ( owx(ista)(ich:ich+1)=='PL'  ) wthr_type(i1,j1)=13
       if ( owx(ista)(ich:ich+1)=='DZ'  ) wthr_type(i1,j1)=14
       if ( owx(ista)(ich:ich+1)=='UP'  ) wthr_type(i1,j1)=15
       if ( owx(ista)(ich:ich+1)=='BR'  ) wthr_type(i1,j1)=21
       if ( owx(ista)(ich:ich+1)=='FG'  ) wthr_type(i1,j1)=22
     enddo

!       Consider clear condition case
!       -----------------------------
     if (ocld(1,ista)==0) then

        do ic=1,6
           if(float(abs(ocld(6+ic,ista))) < 55555) then
              write(6,*) 'cloudCover_Surface: Observed cloud above the clear level !!!'
              write(6,*) 'cloudCover_Surface: some thing is wrong in surface cloud observation !'
              write(6,*) 'cloudCover_Surface: check the station no.', ista, 'at process ', mype
              write(6,*) ic,OI(ista),OJ(ista)
              write(6,*) (ocld(k,ista),k=1,12)
              cycle loopstation
           endif
        enddo
! clean the whole column up to ceilometer height (12 kft) if ob is CLR
!    h_bk is AGL, not ASL (per Ming Hu's notes below
!
!            zlev_clr = Oelvtn(ista)+3650.
! Upcoming mods commented out below for this commit - 4/3/2010
! PH: added in column cleaning up to ceilometer height if ob is CLR
!  move this check out of this if block. Because it will be used later.
!        zlev_clr = 3650.

        do k=1,nztn_p
           if (h_bk(i1,j1,k) < zlev_clr) then
              cld_cover_3d(i1,j1,k)=0.0_r_kind
              pcp_type_3d(i1,j1,k)=0
           endif
        end do

        wthr_type(i1,j1)=0

! -- Now consider non-clear obs
!    --------------------------
     else
           
!      increase zthick by 1.5x factor for ceiling < 900 m (~3000 ft - MVFR)
        cloud_dz = cloud_zthick_p
        cl_base_broken_k = -9
! ????? check with Stan    O(h_p)         if (Oelvtn(ista).lt.900.) cloud_dz = cloud_zthick_p * 2

        do ic = 1,6
           if (ocld(ic,ista)>0 .and. ocld(ic,ista)<50) then
!             if  ( csza(i,j)>=0.10 .and. sat_ctp(i1,j1)>1010.0 &
!                 .and. sat_ctp(i1,j1)<1050.)  go to 1850
!
! New tweak - 11/07/2009
!   If there was cloud in background over station but if there
!     was partial cloudiness within volume and this is one of the
!     clear columns within the polygonal area for this METAR,
!     then leave it that way and skip.
!            if (watericemax(iob,job).gt.0. .and.
!     1          kwatericemax(iob,job).gt.0 .and.
!     1          kwatericemax(iob,job).le.12) then
!              npts_cld_match = npts_cld_match + 1
!              dzbase = cl_base_ista - g3(iob,job,kwatericemax(iob,job),h_p)
!              sum_dzbase = sum_dzbase + dzbase
!              sum_dzbase_abs = sum_dzbase_abs + abs(dzbase)
!            end if

! mhu, Aug. 28, 2013: comment out patial cloudiness. It causes the degradation
! in 3000' ceiling 1-h forecast.
!              if(watericemax(ista) > 0._r_single .and. kwatericemax(i1,j1)==-1) then
!                 !PH 2/28/2013: ensure cloud building at 4 neighboring
!                 !gridpoints (Odist < 1), regardless of background 
!                 if(Odist(ista) >= 1.0_r_kind) then
!                    npts_near_clr = npts_near_clr + 1
!                    cycle   ! skip cloud build at point (i,j) because
!                            ! background is clear
!                 endif
!              endif

              if(ocld(ic,ista) == 4) then
                 if(wthr_type(i1,j1) > 10 .and. wthr_type(i1,j1) < 20) cloud_dz = 1000._r_kind  
                                         ! precipitation + highest level
                 if(wthr_type(i1,j1) == 1) cloud_dz = 10000._r_kind  ! thunderstorm
              endif

! --- calculate cloud ceiling level, not exactly, FEW SCT are also considered now
!                   iob = int(oi(ista)-idw+0.5)
!                   job = int(oj(ista)-ids+0.5)
!                   cl_base_ista = (float(ocld(6+ic,ista))+zh(iob,job))
!                   cl_base_ista = (float(ocld(6+ic,ista))+Oelvtn(ista))
! the h_bk is AGL. So observation cloud base should be AGL too, delete Oelvtn(ista)
! cover cloud base observation from AGL to ASL
              cl_base_ista = float(ocld(6+ic,ista)) + Oelvtn(ista) - zh(i1,j1)
              if(zh(i1,j1) < 1.0_r_kind .and. Oelvtn(ista) > 20.0_r_kind &
                 .and. float(ocld(6+ic,ista)) < 250.0_r_kind) then
                 cycle   ! limit the use of METAR station over oceas for low cloud base
              endif
              
              firstcloud = 0
              underlim = 10._r_kind   !

              do k=1,nztn_p
                 zdiff = cl_base_ista - h_bk(i1,j1,k)
!     Must be within cloud_dz meters (300 or 1000 currently)
!    -------------------------------------------------------------------
!  -- Bring in the clouds if model level is within 10m under cloud level.
                 if(k==1)  underlim=(h_bk(i1,j1,k+1)-h_bk(i1,j1,k))*0.5_r_kind
                 if(k==2)  underlim=10.0_r_kind    ! 100 feet
                 if(k==3)  underlim=20.0_r_kind    ! 300 feet
                 if(k==4)  underlim=15.0_r_kind    ! 500 feet
                 if(k==5)  underlim=33.0_r_kind    ! 1000 feet
                 if (k>=6 .and. k <= 7) underlim = (h_bk(i1,j1,k+1)-h_bk(i1,j1,k))*0.6_r_kind
                 if(k==8)  underlim=95.0_r_kind    ! 3000 feet
                 if(k>=9 .and. k<nztn_p-1) underlim=(h_bk(i1,j1,k+1)-h_bk(i1,j1,k))*0.8_r_kind
                 if (zdiff<underlim) then
                    if((cl_base_ista >= 1.0 .and. (firstcloud==0 .or. abs(zdiff)<cloud_dz)) .or. &
                       (cl_base_ista < 1.0 .and. (abs(zdiff)<cloud_dz)) ) then
                       if (h_bk(i1,j1,k) < cld_bld_hgt) then !limit cloud building to below a specified height 
                       if(ocld(ic,ista) == 1 ) then
                          cld_cover_3d(i1,j1,k)=max(cld_cover_3d(i1,j1,k),0.1_r_single)
                          pcp_type_3d(i1,j1,k)=0
                       elseif (ocld(ic,ista) == 2 ) then
                          cld_cover_3d(i1,j1,k)=max(cld_cover_3d(i1,j1,k),0.3_r_single)
                       elseif (ocld(ic,ista) == 3 ) then
                          cld_cover_3d(i1,j1,k)=max(cld_cover_3d(i1,j1,k),0.7_r_single)
                          if(cl_base_broken_k < 0 ) cl_base_broken_k=k
                       elseif (ocld(ic,ista) == 4 ) then
                          cld_cover_3d(i1,j1,k)=max(cld_cover_3d(i1,j1,k),1.01_r_single)
                          if(cl_base_broken_k < 0 ) cl_base_broken_k=k
                          if(wthr_type(i1,j1) == 1) then
!                            cld_type_3d(i1,j1,k)=10
                             pcp_type_3d(i1,j1,k)=1
                          endif
                          if(wthr_type(i1,j1) > 10 .and. wthr_type(i1,j1) < 20)  then
!                             cld_type_3d(i1,j1,k)=5 
                             pcp_type_3d(i1,j1,k)=1
                           endif
                       else
                           write(6,*) 'cloudCover_Surface: wrong cloud coverage observation!'
                           cycle loopstation
                       endif
                       endif
                       firstcloud = firstcloud + 1
                    end if  ! zdiff < cloud_dz
                 else
!  ---- Clear up to cloud base of first cloud level
                    if (ic==1) cld_cover_3d(i1,j1,k)=0
                    if (ocld(ic,ista) == 1) pcp_type_3d(i1,j1,k)=0
                    if (ocld(ic,ista) == 3 .or. ocld(ic,ista) == 4) then
                       if( (wthr_type(i1,j1) > 10 .and. wthr_type(i1,j1) < 20)  &
                                                   .or. wthr_type(i1,j1) == 1 )  then 
                          pcp_type_3d(i1,j1,k)=1
                       endif
                    endif
                 end if  ! underlim
              end do  ! end K loop
!  ----clean cloud above stratusphere
              do k=1,nztn_p
                 if( h_bk(i1,j1,k) > 18000 ) cld_cover_3d(i1,j1,k)=0
              enddo
!
           end if     ! end if ocld > 0
        end do      ! end IC loop
!
!  clean up to broken (3) or if cloud cover less than 2, clean to cloud top
!
            if(cl_base_broken_k > 0 .and. cl_base_broken_k < nztn_p) then
                do k=1, cl_base_broken_k
                  if( cld_cover_3d(i1,j1,k) < -0.001_r_kind )   cld_cover_3d(i1,j1,k)=0
                enddo
            else
                if(ocld(1,ista) == 1 .or. ocld(1,ista) == 2 ) then
                   do k=1, nztn_p
                     if (h_bk(i1,j1,k) < zlev_clr) then
                        if( cld_cover_3d(i1,j1,k) < -0.001_r_kind )  cld_cover_3d(i1,j1,k)=0
                     endif
                   enddo
                endif
            endif

     end if      ! end if cloudy ob   ocld(1,ista) > 0

! -- Use visibility for low-level cloud whether
     if (wthr_type(i1,j1) < 30 .and. wthr_type(i1,j1) > 20 .and. &
         ocld(13,ista)  < 5000 .and. ocld(13,ista) > 1 .and.     &
         min_dist < 20.0_r_single) then
           cld_type_3d(i1,j1,1) = 2
           cld_type_3d(i1,j1,2) = 2
           betav = 3.912_r_kind / (float(ocld(13,ista)) / 1000._r_kind)
           vis2qc(i1,j1) = ( (betav/144.7_r_kind) ** 1.14_r_kind) / 1000._r_kind
     endif  ! cloud or clear

   ENDDO  loopstation ! ista


!   Determine if the layer is dry or it has inversion.
!  (in either case, the cloud will be cleared out)
!
   IF(.false.) THEN     ! Set inversion strength flag
     call BckgrndCC(nlon,nlat,nsig,    &
                 t_bk,p_bk,q,h_bk,zh,  &
                 cv_bk,tbk_k,z_lcl)    ! out

     DO j = 2,nlat-1
       DO i = 2,nlon-1

         if_cloud_exist=.false.
         do k=nsig-1,2,-1
           if(cld_cover_3d(i,j,k) > 0.01_r_kind) then
             cf_model_base = cv_bk(i,j,k)
             t_model_base = tbk_k(i,j,k)
             ht_base=h_bk(i,j,k)
             if_cloud_exist=.true.
           endif
         enddo
!
! note, do we need to consider cloud base from background
         if(if_cloud_exist) then
           do k=2, nsig-1
             if(cld_cover_3d(i,j,k) > 0.01_r_kind) then
               l_cf=.false.
               l_inversion=.false.
               t_dry_adiabat = tbk_k(i,j,2) -.0098_r_kind * (h_bk(i,j,k) - h_bk(i,j,2))
               t_inversion_strength = tbk_k(i,j,k) - t_dry_adiabat
 
               IF( (tbk_k(i,j,k) > t_model_base)  .and.  &
                   (tbk_k(i,j,k) > 283.15_r_kind) .and.  &   !   temp check
                   (t_inversion_strength > 4._r_kind) ) then ! delta theta chk
                       l_inversion = .true.           ! Inversion exists
               endif
               IF( (cv_bk(i,j,k) < cf_model_base - 0.3_r_kind) .and. &
                   (h_bk(i,j,k) - ht_base >= 500._r_kind) ) THEN
                     l_cf = .true.           ! Dry layer exists
               ENDIF 
               if(l_inversion) then
                  cld_cover_3d(i,j,k) =0.0_r_kind
               endif
             endif ! in cloud
           enddo  ! k
         endif !   if_cloud_exist = true

       ENDDO   ! i
     ENDDO   ! j

   END IF     ! .true. for dry-inversion check.

END SUBROUTINE cloudCover_Surface

