SUBROUTINE cloudCover_NESDIS(mype,regional_time,nlat,nlon,nsig,&
                        xlong,xlat,t_bk,p_bk,h_bk,xland, &
                        soil_tbk,sat_ctp,sat_tem,w_frac,&
                        l_cld_bld,cld_bld_hgt,build_cloud_frac_p,clear_cloud_frac_p,nlev_cld, &
                        cld_cover_3d,cld_type_3d,wthr_type,Osfc_station_map)
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudCover_NESDIS  cloud cover analysis using NESDIS cloud products
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-10
!
! ABSTRACT: 
!  This subroutine determines cloud_cover (fractional) field using NESDIS cloud products
!  Based on RUC assimilation code - (Benjamin, Weygandt, Kim, Brown)
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     regional_time - analysis time
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     xlong       - 2D longitude in each grid
!     xlat        - 2D latitude in each grid
!
!     t_bk        - 3D background potential temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     h_bk        - 3D background height  
!     xland       - surface type (water, land)
!     soil_tbk    - background soil temperature
!     sat_ctp     - GOES cloud top pressure in analysis grid
!     sat_tem     - GOES cloud top temperature in analysis grid
!     w_frac      - GOES cloud coverage in analysis grid
!     l_cld_bld   - logical for turning on GOES cloud building
!     cld_bld_hgt - Height below which cloud building is done
!     build_cloud_frac_p - Threshold above which we build clouds
!     clear_cloud_frac_p - Threshold below which we clear clouds
!
!   output argument list:
!     nlev_cld    - cloud status
!     cld_cover_3d- 3D cloud cover (fractional cloud)
!     cld_type_3d - 3D cloud type
!     wthr_type   - 3D weather type
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
  use constants, only: deg2rad, rad2deg, pi
  use gsd_kinds, only: r_single,i_kind,r_kind
  
  implicit none

  integer(i_kind),intent(in) :: mype
  integer(i_kind),intent(in) :: regional_time(6)
  integer(i_kind),intent(in) :: nlat,nlon,nsig
!
!  background
!
  real(r_single),intent(in)    :: xlong(nlon,nlat)       ! longitude
  real(r_single),intent(in)    :: xlat(nlon,nlat)        ! latitude
  real(r_single),intent(in)    :: t_bk(nlon,nlat,nsig)   ! potentional temperature
  real(r_single),intent(inout) :: p_bk(nlon,nlat,nsig)   ! pressure
  real(r_single),intent(in)    :: h_bk(nlon,nlat,nsig)   ! height
  real(r_single),intent(in)    :: xland(nlon,nlat)       ! surface
  real(r_single),intent(in)    :: soil_tbk(nlon,nlat)    ! soil tmperature
!  real(r_single),intent(in)    :: q_bk(nlon,nlat,nsig)   ! moisture, water vapor mixing ratio (kg/kg)
!
! Observation
!
  real(r_single),intent(inout) :: sat_ctp(nlon,nlat)
  real(r_single),intent(inout) :: sat_tem(nlon,nlat)
  real(r_single),intent(inout) :: w_frac(nlon,nlat)
  integer(i_kind),intent(out)  :: nlev_cld(nlon,nlat)
  integer(i_kind),intent(in)  :: Osfc_station_map(nlon,nlat)
!
! Turn on cloud building and height limit
  logical,      intent(in)     :: l_cld_bld
  real(r_kind), intent(in)     :: cld_bld_hgt
  real(r_kind), intent(in)     :: build_cloud_frac_p
  real(r_kind), intent(in)     :: clear_cloud_frac_p
!
!  Variables for cloud analysis
!
  real (r_single),intent(inout) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: cld_type_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: wthr_type(nlon,nlat)
!
!-------------------------------------------------------------------------
! --- Key parameters
!     Min_cloud_lev_p   = 3             Lowest model level to check for cloud
!     Sat_cloud_pthick_p=  50.          Depth (mb) of new sat-sensed cloud layer
!     Cloud_up_p        = 10            Pressure thickness for
!                                         Upward extrapolation of cloud
!                                        (if model level is within cloud_up_p
!                                        mb of sat cloud level)
!     min_cloud_p_p     = 960.          Max pressure at which NESDIS cloud
!                                         info is considered reliable
!                                         (i.e., not reliable at low levels)

!     zen_limit         = 0.20          Solar zenith angle - lower limit
!                                         at which sun is considered
!                                         high enough to trust the
!                                         GOES cloud data

  integer(i_kind) ::    min_cloud_lev_p
  real(r_kind)    ::    sat_cloud_pthick_p
  real(r_kind)    ::    cloud_up_p
  real(r_kind)    ::    min_cloud_p_p
  real(r_kind)    ::    co2_preslim_p
  real(r_kind)    ::    zen_limit
  real(r_kind)    ::    dt_remap_pcld_limit_p

! --- Key parameters
  data  Min_cloud_lev_p    / 1_i_kind  /        !  w/ sfc cld assim
!  data  Min_cloud_lev_p    / 3_i_kind  /        !  w/ sfc cld assim
  data  Sat_cloud_pthick_p / 30._r_kind/
!  data  Sat_cloud_pthick_p / 50._r_kind/
  data  cloud_up_p         / 0._r_kind /
  data  min_cloud_p_p      / 1080._r_kind/      ! w/ sfc cld assim
  data  co2_preslim_p      / 620._r_kind/
! -- change to 82 deg per Patrick Minnis - 4 Nov 09
  data  zen_limit          / 0.14_r_kind/
! data  zen_limit          / 0.20_r_kind /
  data  dt_remap_pcld_limit_p / 3.5_r_kind /
!-----------------------------------------------------------
!
! temp.
!
  INTEGER(i_kind)  ::   null_p
  REAL(r_kind)     ::   spval_p
  PARAMETER ( null_p     = -1       )
  PARAMETER ( spval_p    =  99999.0 )

  INTEGER(i_kind)  :: i,j,k,k1,i1,j1,jp1,jm1,ip1,im1
  INTEGER(i_kind)  :: gmt,nday,iyear,imonth,iday
  REAL(r_kind)     :: declin
  real(r_kind)     :: hrang,xxlat
  real(r_single)   :: csza(nlon,nlat)
 
  INTEGER(i_kind)  :: ndof_tot, npts_clear, npts_build, npts_bel650
  INTEGER(i_kind)  :: npts_warm_cld_flag, npts_tskin_flag, npts_stab_flag, npts_ptly_cloudy
  real (r_single)  :: tbk_k(nlon,nlat,nsig)

  INTEGER(i_kind)  :: npts_ctp_change, npts_ctp_delete, npts_ctp_nobuddy
  INTEGER(i_kind)  :: npts_clr_nobuddy,npts_ctp_marine_remap
  real (r_single)  :: dctp, dctpabs

  real(r_single)     :: tsmin

  INTEGER(i_kind)  :: kisotherm, ibuddy, ktempmin
  real(r_kind)     :: tempmin,dth2dp2, stab, stab_threshold

  real(r_kind)     :: firstcloud, pdiff,pdiffabove

  INTEGER(i_kind)  :: k_closest, cld_warm_strat(nlon,nlat)
  REAL(r_kind)     :: tdiff

!
!====================================================================
!  Begin
!
!   calculation solar declination
!
  iyear=regional_time(1)
  imonth=regional_time(2)
  iday=regional_time(3)
  call getdays(nday,iyear,imonth,iday)
  declin=deg2rad*23.45_r_kind*sin(2.0_r_kind*pi*(284+nday)/365.0_r_kind)

  cld_warm_strat=-1
!
!   from mb to Pa
!
   do k = 1,nsig
     do j = 1,nlat
       do i = 1,nlon
!          qw=q_bk(i,j,k)/(1. + q_bk(i,j,k))   ! convert to specific humidity
          tbk_k(i,j,k)=t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp  ! convert to temperature(K) 
          p_bk(i,j,k)  = p_bk(i,j,k)*100._r_kind
       end do
     end do
   end do

   if( p_bk(nlon/2,nlat/2,2) < 5000.0_r_kind ) then
     write(6,*) 'cloudCover_NESDIS: pressure unit check failed', p_bk(nlon/2,nlat/2,2) 
     call stop2(114)
   endif
   if( tbk_k(nlon/2,nlat/2,nsig-2) > 300._r_kind) then
     write(6,*) 'cloudCover_NESDIS: temperature unit check failed', &
                tbk_k(nlon/2,nlat/2,nsig-2) 
     call stop2(114)
   endif

!
!  csza = fraction of solar constant (cos of zenith angle)
   gmt = regional_time(4)   ! UTC 
   do j=2,nlat-1
     do i=2,nlon-1
       hrang= (15._r_kind*gmt + xlong(i,j) - 180._r_kind )*deg2rad
       xxlat=xlat(i,j)*deg2rad
       csza(i,j)=sin(xxlat)*sin(declin)                &
                +cos(xxlat)*cos(declin)*cos(hrang)
     end do
   end do

!
!  start checking the data
!
   ndof_tot = 0   !counting total number of grids of sat info
   npts_clear = 0
   npts_build = 0
   npts_bel650 = 0
   npts_tskin_flag = 0
   npts_stab_flag = 0
   npts_ptly_cloudy = 0

   do j=2,nlat-1
     do i=2,nlon-1
       jp1 = min(j+1,nlat)
       jm1 = max(j-1,1   )
       ip1 = min(i+1,nlon)
       im1 = max(i-1,1   )
       tsmin = soil_tbk(i,j)
!  ---  Determine min skin temp in 3x3 around grid point.
!       This is to detect nearby presence of coastline.
       do j1 = jm1,jp1
         do i1 = im1,ip1
            tsmin = min(tsmin,soil_tbk(i1,j1) )
         end do
       end do

       if ( w_frac(i,j) > -1._r_kind                  &
            .and. (sat_tem(i,j)-soil_tbk(i,j)) > 4._r_kind    &
            .and. soil_tbk(i,j) < 263._r_kind         &
            .and. sat_ctp(i,j) > co2_preslim_p        &
            .and. sat_ctp(i,j) < 1010._r_kind         &
            .and. abs(xland(i,j))>0.0001_r_single     &
            .and. p_bk(i,j,1)/100. >=850._r_kind ) then
!              w_frac(i,j) = -99999._r_kind
!              sat_tem(i,j) =  99999._r_kind
!              sat_ctp(i,j) =      0._r_kind
!              nlev_cld(i,j) = -999
               npts_warm_cld_flag = npts_warm_cld_flag + 1
               cld_warm_strat(i,j)=5
       end if
!  PH changed condition to match RUC:   Tcld-Tskin(bkg) < 4, > -2
       if (      w_frac(i,j) > -1._r_kind             &
           .and. (sat_tem(i,j)-tsmin) > -2._r_kind    &
           .and. (sat_tem(i,j)-tsmin) <= 4._r_kind    &
           .and. sat_ctp(i,j) > co2_preslim_p         &
           .and. sat_ctp(i,j) < 1010._r_kind          &
           .and. abs(xland(i,j)) > 0.0001_r_single    &
           .and. p_bk(i,j,1)/100._r_kind>= 950._r_kind ) then
              w_frac(i,j)  = -99999._r_kind
              sat_tem(i,j) =  99999._r_kind
              sat_ctp(i,j) =      0._r_kind
              nlev_cld(i,j)= -999
              npts_tskin_flag = npts_tskin_flag + 1
              cld_warm_strat(i,j)=4
       end if
       if (w_frac(i,j)<=clear_cloud_frac_p  .and.      &
           w_frac(i,j)>-1._r_kind)        then
              sat_ctp(i,j) = 1013.0_r_kind
              npts_clear   = npts_clear + 1
              cld_warm_strat(i,j)=0
       end if
       if (w_frac(i,j) > clear_cloud_frac_p.and.      &
           w_frac(i,j) < build_cloud_frac_p) then
!              w_frac(i,j) = -99999._r_kind
               sat_tem(i,j)=  99999._r_kind
! mhu: this can cause problem: a miss line between cloud and clean, set it to clean
! PH: for CLAVR data, just set sat_ctp = 0.
               sat_ctp(i,j) =     0._r_kind
               nlev_cld(i,j)= -999
               npts_ptly_cloudy = npts_ptly_cloudy + 1
               cld_warm_strat(i,j)=1
       end if
       if (w_frac(i,j) >= build_cloud_frac_p.and.      &
           sat_ctp(i,j) < 1050) then
               npts_build = npts_build + 1
               cld_warm_strat(i,j)=2
       end if
       if (sat_ctp(i,j)>co2_preslim_p .and. sat_ctp(i,j)<1010._r_kind)  &
             npts_bel650 = npts_bel650 + 1

! -- nlev_cld = 1 if cloud info is present
! -- nlev_cld = 0 if no cloud info is at this grid point

       if(nlev_cld(i,j) >= 1) ndof_tot = ndof_tot + 1
     end do   ! i
   end do   ! j
!
   if(mype==0) then
   write(6,*) 'cloudCover_NESDIS: TOTAL NUMBER OF GRID pts w/ GOES CLOUD data =',ndof_tot
   write(6,*) 'cloudCover_NESDIS: CLEAR NUMBER OF GRID pts w/ GOES CLOUD data =',npts_clear
   write(6,*) 'cloudCover_NESDIS: BUILD NUMBER OF GRID pts w/ GOES CLOUD data =',npts_build 
   write(6,*) 'cloudCover_NESDIS: PTCLDY NUMBER OF GRID pts w/ GOES CLOUD data =',npts_ptly_cloudy
   write(6,*) 'cloudCover_NESDIS: > 650mb - no OF GRID pts w/ GOES CLOUD data =',npts_bel650
   write(6,*) 'cloudCover_NESDIS: Flag CTP - skin temp too close to TB, no=',npts_tskin_flag
   write(6,*) 'cloudCover_NESDIS: Clear -> cloud frac < clear frac'
   write(6,*) 'cloudCover_NESDIS: Build -> cloud frac > build frac'
   endif

!
!!
!
   npts_ctp_change = 0
   npts_ctp_delete = 0
   npts_ctp_nobuddy = 0
   npts_clr_nobuddy = 0
   npts_ctp_marine_remap = 0
   dctp = 0.
   dctpabs = 0.

! - stability threshold for building cloud - 3K / 100 mb (10000 Pa)

   stab_threshold = 3._r_kind/10000._r_kind
   do j=2,nlat-1
     do i=2,nlon-1

! -- GOES indicates clouds in the lower troposphere
       if (sat_ctp(i,j) < 1010._r_kind .and. sat_ctp(i,j) > co2_preslim_p) then

          tdiff = 999.
          k_closest = -1
          do k=3,nsig-1
!      Attempt remapping if within 75 hPa (arbitrary)
             if ((sat_ctp(i,j)-p_bk(i,j,k)/100._r_kind)< 75._r_kind) then
               if (abs(sat_tem(i,j)-tbk_k(i,j,k)) < tdiff) then
                 k_closest = k
                 tdiff = abs(sat_tem(i,j)-tbk_k(i,j,k))
               end if
             end if
          end do    ! k loop

          if (k_closest <= 0 .and. abs(xland(i,j)) > 0.0001_r_single) then
             npts_ctp_delete = npts_ctp_delete + 1
             write (6,*) i,j,sat_tem(i,j),tdiff,k_closest,xland(i,j)
             go to 111
          end if

          k = k_closest

          if( abs(xland(i,j)) >0.0001_r_single ) then
! PH: dt_limit was hardwired to 1.5K, changed it to 3.5K to match RUC
             if ((tdiff < dt_remap_pcld_limit_p) .or.       &
                 (cld_warm_strat(i,j) == 5 .and. tdiff < 4._r_kind )) then
                  dctpabs = dctpabs + abs(sat_ctp(i,j)-p_bk(i,j,k)/100._r_kind)
                  dctp    = dctp+ (sat_ctp(i,j)-p_bk(i,j,k)/100._r_kind)
                  k1      = k

1115              continue

! --- This stability check only for reassigining CTP using RUC bkg profile.
!      There is a later general check also.
                  stab = (t_bk(i,j,k1+1)-t_bk(i,j,k1))    &
                        /(p_bk(i,j,k1)-p_bk(i,j,k1+1))
                  if (stab <  stab_threshold) then
                     k1 = k1 + 1
                     if ((p_bk(i,j,k)-p_bk(i,j,k1)) > 5000._r_kind) then
                       w_frac(i,j)   = -99999._r_kind
                       sat_tem(i,j)  =  99999._r_kind
                       sat_ctp(i,j)  =  99999._r_kind
                       nlev_cld(i,j) = -999
                       npts_stab_flag= npts_stab_flag + 1
                       go to 111
                     end if
                     go to 1115
                  end if

                  sat_ctp(i,j) = p_bk(i,j,k)/100._r_kind
                  npts_ctp_change = npts_ctp_change + 1
                  go to 111
             else
                  npts_ctp_delete = npts_ctp_delete + 1
!                  write (6,*) i,j,sat_tem(i,j),tdiff
                  go to 111
             end if

          else ! xland==0:  over water

! --- Remap marine cloud to min temp level below 880 mb
!       if no matching RUC temp already found

             if (sat_ctp(i,j)>880._r_kind)then
                tempmin = -500._r_kind

! --- Look thru lowest 15 levels for lowest temp for
!        level to put marine cloud at.
!  ---  Start at level 3
                kisotherm = 20
                ktempmin = 20
                do k=min_cloud_lev_p+2,15
                  if (p_bk(i,j,k)/100._r_kind .lt. 880._r_kind) go to 1101
                    dth2dp2 = t_bk(i,j,k+1)+t_bk(i,j,k-1)-2._r_kind*t_bk(i,j,k)
                    if (kisotherm==0 .and.                         &
                       tbk_k(i,j,k) < tbk_k(i,j,k+1))  kisotherm = k
                    if (dth2dp2>tempmin) then
                       ktempmin = k
                       tempmin = max(dth2dp2,tempmin)
                    end if
                end do
1101            continue
                ktempmin = min(ktempmin,kisotherm)
                sat_ctp(i,j) = p_bk(i,j,ktempmin)/100._r_kind
                npts_ctp_marine_remap = npts_ctp_marine_remap + 1
             end if   !  sat_ctp(i,j)>880._r_kind
          endif    !   xland == 0
       end if
111    continue
     enddo  ! i
   enddo  ! j

   if(mype==0) then
   write(6,*) 'cloudCover_NESDIS: Flag CTP - unstable w/i 50mb of CTP, no=', npts_stab_flag
   write(6,*) 'cloudCover_NESDIS: Flag CTP - can''t remap CTP,         no=', npts_ctp_delete
   write(6,*) 'cloudCover_NESDIS: Flag CTP -remap marine cloud,        no=', npts_ctp_marine_remap
   endif

   if (npts_ctp_change > 0) then
   if(mype==0)  write (6,1121) npts_ctp_change, dctp/float(npts_ctp_change),  &
                dctpabs/float(npts_ctp_change)
1121    format (/'No. of pts w/ cloud-top pres change = ',i6          &
              /'Mean cloud-top pres change (old-new)= ',f8.1          &
              /'Mean abs cloud-top pres change      = ',f8.1/)
   end if
!
! --- Make sure that any cloud point has another cloud point nearby.
!       Otherwise, get rid of it.
   do j=2,nlat-1
     do i=2,nlon-1
        if (sat_ctp(i,j)< 1010._r_kind .and. sat_ctp(i,j)>50._r_kind) then
          ibuddy = 0
          do j1=j-1,j+1
            do i1=i-1,i+1
              if (sat_ctp(i1,j1)<1010._r_kind .and. sat_ctp(i1,j1)>50._r_kind)  ibuddy = 1
            end do
          end do
          if (ibuddy==0) then
               w_frac(i,j)   = -99999._r_kind
               sat_tem(i,j)  =  99999._r_kind
               sat_ctp(i,j)  =  99999._r_kind
               nlev_cld(i,j) = -999
               npts_ctp_nobuddy = npts_ctp_nobuddy + 1
          end if
        end if
        if (sat_ctp(i,j)>1010._r_kind .and. sat_ctp(i,j) <1100._r_kind) then
            ibuddy = 0
            do j1=j-1,j+1
              do i1=i-1,i+1
                if (sat_ctp(i1,j1) > 1010._r_kind .and. sat_ctp(i1,j1) <1100._r_kind) ibuddy = 1
              end do
            end do
            if (ibuddy == 0) then
               w_frac(i,j)   = -99999._r_kind
               sat_tem(i,j)  =  99999._r_kind
               sat_ctp(i,j)  =  99999._r_kind
               nlev_cld(i,j) = -999
               npts_clr_nobuddy = npts_clr_nobuddy + 1
            end if
        end if
     enddo
   enddo

   if(mype==0) then
    write(6,*) 'cloudCover_NESDIS: Flag CTP - no contiguous points also w/ cloud, no=',  &
           npts_ctp_nobuddy

    write(6,*) 'cloudCover_NESDIS: Flag CTP - no contiguous points also w/ clear, no=', &
           npts_clr_nobuddy
    endif

!
!     *****************************************************************
!     *****************************************************************
!        Start to adjust to GOES cloud top pressure
!     *****************************************************************
!     *****************************************************************
               
!     --- clear where GOES shows clear down to the surface
!            or down to the GOES cloud top level
               
! =============================================
! - clear down to surface in fully clear column (according to GOES)
! =============================================
!    Only trust 'clear' indication under following conditions
!        - over ocean
!        - or over land only if p<620 mb overnight
!        - or at any level in daytime (zenith angle
!                      greater than zen_limit threshold)
!
!  mhu  Nov. 26, 2014: Add a metar station map: Osfc_station_map
!       when Osfc_station_map=1, it is a grid point around a metar station
!       Then the satellite clean step will skip this metar station point.
! =============================================
    do j=2,nlat-1
      do i=2,nlon-1
        if (sat_ctp(i,j) >=1010.0_r_kind .and. sat_ctp(i,j) <1050._r_kind) then !clear
          do k=1,nsig
             if ((csza(i,j)<zen_limit                            &
                .and. p_bk(i,j,k)/100._r_kind<co2_preslim_p)     &
                 .or. abs(xland(i,j)) < 0.0001_r_single          &
                 .or. csza(i,j)>=zen_limit) then
                    if(Osfc_station_map(i,j) == 1 .and.          &
                       cld_cover_3d(i,j,k) > 0.0001_r_kind) then
                    else
                       cld_cover_3d(i,j,k) = 0.0_r_single
                       wthr_type(i,j) = 0
                    endif
!
!mhu Nov 15 2014: don't let metar build cloud if
!          - over land
!          - during night
!          - lower than co2_preslim_p
!          - clear from satellite
             else  ! mhu Dec 2016: turn off this night low cloud check
                    if(Osfc_station_map(i,j) == 1 .and. &
                       cld_cover_3d(i,j,k) >0.0001_r_kind) then  
                    else
                       cld_cover_3d(i,j,k) = 0.0_r_single 
                       wthr_type(i,j) = 0
                    endif
!mhu             elseif( (csza(i,j)<zen_limit .and.                      &
!mhu                     p_bk(i,j,k)/100._r_kind>=co2_preslim_p) .and.    &   
!mhu                     abs(xland(i,j)-0._r_single) > 0.0001_r_single .and. &
!mhu                     cld_cover_3d(i,j,k) >0.0001_r_kind)  then
!mhu                    if(Osfc_station_map(i,j) == 1) then  
!mhu                    else
!mhu                       cld_cover_3d(i,j,k) = - 77777.0_r_single     ! set to unknown
!mhu                    endif
             end if
          end do
!mhu: use 1060hps cloud top pressure to clean above the low cloud top
        elseif (abs(sat_ctp(i,j)-1060.0_r_kind) < 1.0_r_kind) then !clear since the low cloud top
          do k=1,nsig
                       cld_cover_3d(i,j,k) = 0.0_r_single
                       wthr_type(i,j) = 0
!mhu mhu Dec 2016: turn off this night low cloud check
!mhu             if (csza(i,j)<zen_limit                             &
!mhu                .and. p_bk(i,j,k)/100._r_kind<co2_preslim_p      &
!mhu                 .or. abs(xland(i,j)) < 0.0001_r_single          &
!mhu                 .or. csza(i,j)>=zen_limit) then
!mhu                   if( abs(cld_cover_3d(i,j,k)) > 2.0_r_single ) then
!mhu                       cld_cover_3d(i,j,k) = 0.0_r_single
!mhu                       wthr_type(i,j) = 0
!mhu                  endif
!mhu             end if
          end do 
        end if
      enddo
    enddo
! =============================================
! - clearing above cloud top
! =============================================

    do  j=2,nlat-1
      do  i=2,nlon-1
        do k=1,nsig-1
           if (sat_ctp(i,j)<1010._r_kind .and.          &
               sat_ctp(i,j)>p_bk(i,j,k)/100._r_kind) then
               if(sat_ctp(i,j) >= 800.0_r_kind .and. Osfc_station_map(i,j) == 1) then
                  cld_cover_3d(i,j,k+1) =                  &
                       max(0.0_r_single, cld_cover_3d(i,j,k+1))
               else
                  cld_cover_3d(i,j,k+1) = 0.0_r_single
               endif
           endif

! - return to previous (but experimental) version - 12 Oct 04
!mhu          if (csza(i,j) < zen_limit                           &
!mhu              .and. p_bk(i,j,k)/100._r_kind<co2_preslim_p     &
!mhu               .or. abs(xland(i,j)) < 0.0001_r_single         &
!mhu               .or. csza(i,j)>=zen_limit) then
! --- since we set GOES to nearest RUC level, only clear at least
!       1 RUC level above cloud top
!mhu                 if (sat_ctp(i,j)<1010._r_kind .and.          &
!mhu                     sat_ctp(i,j)>p_bk(i,j,k)/100._r_kind) then
!
!  mhu, some low cloud top press (> 800 hpa) over clean the cloud that observed by METAR
! so add these check to keep cloud base correct
!
!mhu                   if(sat_ctp(i,j) >= 800.0_r_kind ) then
!mhu                     cld_cover_3d(i,j,k+1) =                  &
!mhu                          max(0.0_r_single, cld_cover_3d(i,j,k+1))
!mhu                   else
!mhu                     cld_cover_3d(i,j,k+1) = 0.0_r_single
!mhu                   endif
!mhu                 endif
!mhu          end if
        end do
      enddo
    enddo

!    print *, 'h_bk max: ', maxval(h_bk(:,:,1)), ' min: ', minval(h_bk(:,:,1))

! =============================================
! - start building where GOES indicates so
! =============================================
    do j=2,nlat-1
      do i=2,nlon-1

        if ((w_frac(i,j)>= build_cloud_frac_p) .and. &
            (w_frac(i,j)< 99999._r_kind) )then   !Dongsoo added

! --- Cloud info below MIN_CLOUD_P not reliable
          firstcloud = 0
! - pdiff (diff between sat cloud top and model sfc pres) in mb
          do k=nsig-1,min_cloud_lev_p,-1
            pdiff = (sat_ctp(i,j)-p_bk(i,j,k)/100._r_kind)
! --- set closest RUC level w/ cloud
            if (pdiff<=0. .and. firstcloud==0) then
              pdiffabove = sat_ctp(i,j)-p_bk(i,j,k+1)/100._r_kind
              if (abs(pdiffabove)<abs(pdiff)) then
!    sgb - 2/7/2012 - remove this condition
!    Allow cloud building below CO2_preslim and at night and over land
!                if (p_bk(i,j,k)/100._r_kind<co2_preslim_p    &
!                    .or. abs(xland(i,j)) < 0.0001_r_single   &
!                    .or. csza(i,j)==zen_limit) then
                   if (l_cld_bld .and. h_bk(i,j,k+1) < cld_bld_hgt) then
                      cld_cover_3d(i,j,k+1)=1.0_r_single
                   else
                      cld_cover_3d(i,j,k+1)=-99998.0_r_single
                   end if
                   firstcloud = 1
                   
!               end if
              else
!
!  mhu, some low cloud top press (> 800 hpa) over clean the cloud that observed by METAR
! so add these check to keep cloud base correct
!
                 if(sat_ctp(i,j) >= 800.0_r_kind ) then
                    cld_cover_3d(i,j,k+1) = max(0.0_r_single, cld_cover_3d(i,j,k+1))
                 else
                    cld_cover_3d(i,j,k+1) = 0.0_r_single
                 endif
                 firstcloud = 1
              end if
            end if

!     no cloud above cloud top

!
! --- Add 50mb thick (at least 1 level) of cloud where GOES
!         indicates cloud top
            if (abs(xland(i,j)) > 0.0001_r_single) then
              if (sat_ctp(i,j)< min_cloud_p_p .and.   &
                  pdiff<=cloud_up_p  ) then
                if (firstcloud==0.or. firstcloud==1   &
                    .and.pdiff >= -1.*sat_cloud_pthick_p) then
!    sgb - 2/7/2012 - remove this condition
!    Allow cloud building below CO2_preslim and at night and over land
!                    if (p_bk(i,j,k)/100._r_kind<co2_preslim_p ) then
                       if (l_cld_bld .and. h_bk(i,j,k+1) < cld_bld_hgt) then
                          cld_cover_3d(i,j,k)=1.0_r_single
                       else
                          cld_cover_3d(i,j,k)=-99998.0_r_single
                       end if
                       firstcloud = 1

!                    end if
                end if
              end if
            else
              if ( pdiff<=cloud_up_p ) then
                 if (firstcloud==0.or. firstcloud==1    &
                     .and.pdiff >= -1.*sat_cloud_pthick_p) then
! xland ==0                   if (p_bk(i,j,k)/100..lt.co2_preslim_p) then
                    if (l_cld_bld .and. h_bk(i,j,k+1) < cld_bld_hgt) then
                       cld_cover_3d(i,j,k)=1.0_r_single
                    else
                       cld_cover_3d(i,j,k)=-99998.0_r_single
                    end if
                    firstcloud = 1
                 end if
              end if
            end if

          end do
        end if
      enddo  ! j
    enddo

! go from pa  to mb
   do k = 1,nsig
     do j = 2,nlat-1
       do i = 2,nlon-1
          p_bk(i,j,k)  = p_bk(i,j,k)/100._r_kind
       end do
     end do
   end do
!
END SUBROUTINE cloudCover_NESDIS

