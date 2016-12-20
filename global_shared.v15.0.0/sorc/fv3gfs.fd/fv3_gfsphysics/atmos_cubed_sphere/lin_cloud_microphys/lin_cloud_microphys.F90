!***********************************************************************
!*                   GNU General Public License                        *
!* This file is a part of fvGFS.                                       *
!*                                                                     *
!* fvGFS is free software; you can redistribute it and/or modify it    *
!* and are expected to follow the terms of the GNU General Public      *
!* License as published by the Free Software Foundation; either        *
!* version 2 of the License, or (at your option) any later version.    *
!*                                                                     *
!* fvGFS is distributed in the hope that it will be useful, but        *
!* WITHOUT ANY WARRANTY; without even the implied warranty of          *
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
!* General Public License for more details.                            *
!*                                                                     *
!* For the full text of the GNU General Public License,                *
!* write to: Free Software Foundation, Inc.,                           *
!*           675 Mass Ave, Cambridge, MA 02139, USA.                   *
!* or see:   http://www.gnu.org/licenses/gpl.html                      *
!***********************************************************************
!
! Cloud micro-physics package for GFDL global cloud resolving model
! The algorithms are originally based on Lin et al 1983. Many key
! elements have been changed/improved based on several other publications
! Developer: Shian-Jiann Lin
!
module lin_cld_microphys_mod
 use mpp_mod,           only: stdlog, mpp_pe, mpp_root_pe, mpp_clock_id, &
                              mpp_clock_begin, mpp_clock_end, CLOCK_ROUTINE, &
                              input_nml_file, mpp_max
 use diag_manager_mod,  only: register_diag_field, send_data
 use time_manager_mod,  only: time_type, get_time
 use constants_mod,     only: grav, rdgas, rvgas, cp_air, cp_vapor, hlv, hlf, kappa, pi=>pi_8
 use fms_mod,           only: write_version_number, open_namelist_file, &
                              check_nml_error, file_exist, close_file,  &
                              error_mesg, FATAL

 implicit none
 private

 public  lin_cld_microphys_driver, lin_cld_microphys_init, lin_cld_microphys_end, sg_conv, wqs1, wqs2, qs_blend
 public  qsmith_init, qsmith, es2_table1d, es3_table1d, esw_table1d, g_sum, wqsat_moist, wqsat2_moist, sat_adj2
 public  setup_con, wet_bulb
 real             :: missing_value = -1.e10
 logical          :: module_is_initialized = .false.
 logical          :: qsmith_tables_initialized = .false.
 character(len=17) :: mod_name = 'lin_cld_microphys'

!==== fms constants ====================
!!! real, parameter :: latv  = hlv             ! = 2.500e6
!!!  real, parameter:: cv_air = 717.56   ! Satoh value
!!! real, parameter :: lati  = hlf             ! = 3.34e5
!!! real, parameter :: lats  = latv+lati       ! = 2.834E6
! rdgas = 287.04;   rvgas = 461.50
! cp_air =rdgas * 7./2. = 1006.64           ! heat capacity at constant pressure (j/kg/k)
! The following two are from Emanuel's book "Atmospheric Convection"
!!! real, parameter :: c_liq = 4190.           ! heat capacity of water at 0C
                                            !
 real, parameter :: eps   = rdgas/rvgas     ! = 0.621971831
 real, parameter :: zvir  = rvgas/rdgas-1.  ! = 0.607789855
 real, parameter :: table_ice  = 273.16  ! freezing point for qs table
 real, parameter :: cv_air =  cp_air - rdgas ! = rdgas * (7/2-1) = 2.5*rdgas=717.68

   real, parameter:: e00 = 610.71  ! saturation vapor pressure at T0
   real, parameter:: c_liq = 4190.       ! heat capacity of water at 0C
   real, parameter:: c_ice = 2106.           ! heat capacity of ice at 0C: c=c_ice+7.3*(T-Tice) 
   real, parameter:: cp_vap = cp_vapor   ! 1846.
!  real, parameter:: cv_vap = 1410.0     ! Emanuel value
! For consistency, cv_vap derived FMS constants:
   real, parameter:: cv_vap = cp_vap - rvgas  ! 1384.5
   real, parameter:: dc_vap = cp_vap - c_liq     ! = -2344.    isobaric heating/cooling
   real, parameter:: dc_ice  =  c_liq - c_ice     ! =  2084

! Values at 0 Deg C
   real, parameter:: hlv0 = 2.501e6   ! Emanuel Appendix-2
   real, parameter:: hlf0 = 3.337e5   ! Emanuel
   real, parameter:: t_ice = 273.16
   real, parameter:: Lv0 =  hlv0 - dc_vap*t_ice   ! = 3.141264e6
   real, parameter:: li00 = hlf0 - dc_ice *t_ice   ! = -2.355446e5

   real, parameter:: d2ice   = cp_vap - c_ice
   real, parameter:: Li2 = hlv0+hlf0 - d2ice*t_ice
!==== fms constants ====================

 real, parameter :: qrmin  = 1.e-9
 real, parameter :: qvmin  = 1.e-22      ! min value for water vapor (treated as zero)
 real, parameter :: qcmin  = 1.e-12      ! min value for cloud condensates
 real, parameter :: sfcrho = 1.20        ! surface air density
 real, parameter :: vmin   = 1.e-3       ! minimum fall speed for rain/graupel
 real, parameter :: rhor   = 1.0e3  ! LFO83
 real, parameter :: dz_min = 1.e-2
 real :: cracs, csacr, cgacr, cgacs, acco(3,4), csacw,          &
         craci, csaci, cgacw, cgaci, cracw, cssub(5), cgsub(5), &
         crevp(5), cgfr(2), csmlt(5), cgmlt(5)
 real :: es0, ces0
 real :: pie, rgrav, fac_rc
 real :: lcp, icp, tcp
 real :: lv00, c_air, c_vap

 logical :: de_ice = .true.     !
 logical :: sedi_transport = .false.     !
 logical :: do_sedi_w = .false.
 logical :: do_sedi_heat = .false.      !
 logical :: prog_ccn = .false.     ! do prognostic CCN (Yi Ming's method)
 logical :: do_qa   = .false.      ! do inline cloud fraction
 logical :: rad_snow =.false.
 logical :: rad_graupel =.false.
 logical :: rad_rain =.false.
 logical :: fix_negative =.false.
 logical :: do_setup=.true.
 logical :: master
 logical :: p_nonhydro = .false.

 real, allocatable:: table(:), table2(:), table3(:), tablew(:), des(:), des2(:), des3(:), desw(:)
 logical :: tables_are_initialized = .false.

 integer:: id_rh, id_vtr, id_vts,  id_vtg, id_vti, id_rain, id_snow, id_graupel, &
           id_ice, id_prec, id_cond, id_var, id_droplets, id_dbz, id_maxdbz, id_basedbz
 real:: lati, latv, lats

 real, parameter :: dt_fr = 8.       ! homogeneous freezing of all cloud water at t_wfr - dt_fr
                                     ! minimum temperature water can exist (Moore & Molinero Nov. 2011, Nature)
                                     ! dt_fr can be considered as the error bar
 integer :: lin_cld_mp_clock   ! clock for timing of driver routine

 real :: t_snow_melt = 12.      ! snow melt tempearture scale factor
 real :: t_grau_melt = 15.      ! graupel melt tempearture scale factor
 real :: p_min = 100.    ! minimum pressure (Pascal) for MP to operate

! The defaults are good for 25-50 km simulation
! For cloud-resolving: 1-5 km
!                       qi0_crt = 0.8E-4
!                       qs0_crt = 0.6E-3
!                       c_psaci = 0.1
!                       c_pgacs = 0.1
!----------------------
! namelist  parameters:
!----------------------
 real :: cld_min = 0.05
 real :: tice  = 273.16  ! set tice = 165. to trun off ice-phase phys (Kessler emulator)

 real :: qc_crt  = 5.0e-8  ! minimum condensate mixing ratio to allow partial cloudiness
 real :: t_min   = 180.  ! Min temp to freeze-dry all water vapor
 real :: t_sub   = 184.  ! Min temp for sublimation of cloud ice
 real :: mp_time = 150.  ! maximum micro-physics time step (sec)

 real :: rh_inc = 0.10   ! rh increment for complete evap of ql and qi
 real :: rh_inr = 0.25
 real :: rh_ins = 0.25   ! rh increment for sublimation of snow

 real :: tau_r  = 120.    ! rain freezing time scale during fast_sat
! The following 3 time scales are for melting during terminal falls
 real :: tau_s  = 120.    ! snow melt
 real :: tau_g  = 180.    ! graupel melt
 real :: tau_mlt = 10.    ! ice melting time-scale

! Fast MP:
 real :: tau_i2s = 150.   ! 
! cloud water
 real :: tau_v2l = 600.   ! vapor --> cloud water (condensation)  time scale
 real :: tau_l2v = 600.   ! cloud water --> vapor (evaporation)  time scale
! Graupel
 real :: tau_g2v = 900.   ! Grapuel sublimation time scale
 real :: tau_v2g =21600. ! Grapuel deposition -- make it a slow process

 real :: dw_land  = 0.20  ! base value for subgrid deviation/variability over land
 real :: dw_ocean = 0.15  ! base value for ocean
 real :: ccn_o = 100.
 real :: ccn_l = 250.
 real :: rthresh = 10.0e-6     ! critical cloud drop radius (micro m)

!-------------------------------------------------------------
! WRF/WSM6 scheme: qi_gen = 4.92e-11 * (1.e3*exp(0.1*tmp))**1.33
!  optimized:      qi_gen = 4.92e-11 * exp( 1.33*log(1.e3*exp(0.1*tmp)) )
! qi_gen ~ 4.808e-7 at 0 C; 1.818e-6 at -10 C, 9.82679e-5 at -40C
! the following value is constructed such that qc_crt = 0 at zero C and @ -10C matches
! WRF/WSM6 ice initiation scheme; qi_crt = qi_gen*min(qi_lim, 0.1*tmp) / den
 real :: qi_gen  = 1.818E-6
 real :: qi_lim  = 1.
 real :: ql_mlt  = 4.0e-3    ! max value of cloud water allowed from melted cloud ice
 real :: ql_gen  = 1.0e-3    ! max ql generation during remapping step if fast_sat_adj = .T.
 real :: sat_adj0 = 0.99     ! adjustment factor (0: no, 1: full) during fast_sat_adj

! Cloud condensate upper bounds: "safety valves" for ql & qi
 real :: ql0_max = 2.0e-3    ! max ql value (converted to rain)
 real :: qi0_max = 3.0e-6    ! max qi value (by other sources)

 real :: qi0_crt = 1.0e-4    ! ice  --> snow autocon threshold (was 1.E-4)
                             ! qi0_crt is highly dependent on horizontal resolution
 real :: qr0_crt = 1.0e-4    ! rain --> snow or graupel/hail threshold
                             ! LFO used *mixing ratio* = 1.E-4 (hail in LFO)
 real :: c_paut  = 0.55     ! autoconversion ql --> qr  (use 0.5 to reduce autoconversion)
 real :: c_psaut = 1.0e-3   ! autoconversion rate: cloud_ice -> snow
 real :: c_psaci = 0.01     ! accretion: cloud ice --> snow (was 0.1 in Zetac)
 real :: c_piacr = 5.0      ! accretion: rain --> ice:
 real :: c_cracw = 0.9      ! rain accretion efficiency

! Decreasing  clin to reduce csacw (so as to reduce cloud water ---> snow)
 real:: alin = 842.0
 real:: clin = 4.8      ! 4.8 --> 6. (to ehance ql--> qs)

!-----------------
! Graupel control:
!-----------------
 real :: qs0_crt = 2.0e-3   ! snow --> graupel density threshold (0.6e-3 in Purdue Lin scheme)
 real :: c_pgacs = 2.0e-3   ! snow --> graupel "accretion" eff. (was 0.1 in Zetac)

! fall velocity tuning constants:
 real :: den_ref = sfcrho   ! Reference (surface) density for fall speed
                            ! Larger value produce larger fall speed
 real :: vr_fac = 1.
 real :: vs_fac = 1.
 real :: vg_fac = 1.
 real :: vi_fac = 1.

 logical :: fast_sat_adj  = .false.
 logical :: z_slope_liq  = .true.          !  use linear mono slope for autocconversions
 logical :: z_slope_ice  = .false.          !  use linear mono slope for autocconversions
 logical :: use_deng_mace = .true.       ! Helmfield-Donner ice speed
 logical :: do_subgrid_z = .false.       ! 2X resolution sub-grid saturation/cloud scheme
 logical :: use_ccn      = .false.
 logical :: use_ppm      = .true.
 logical :: ppm_rain_fall  = .true.
 logical :: mono_prof = .true.          ! perform terminal fall with mono ppm scheme
 logical :: mp_debug = .false.
 logical :: mp_print = .false.

 real:: global_area = -1.

 real:: tice0, t_wfr
 real:: p_crt   = 100.E2   !
 integer:: k_moist = 100

 namelist /lin_cld_microphys_nml/mp_time, t_min, t_sub, tau_s, tau_g, dw_land, dw_ocean,  &
                      vr_fac, vs_fac, vg_fac, vi_fac, ql_mlt, do_qa, fix_negative, &
                      qs0_crt, qi_gen, ql0_max, qi0_max, qi0_crt, qr0_crt, fast_sat_adj, &
                      rh_inc, rh_ins, rh_inr, use_deng_mace, use_ccn, do_subgrid_z,  &
                      rthresh, ccn_l, ccn_o, qc_crt, tau_g2v, tau_v2g, sat_adj0,    &
                      c_piacr, tau_mlt, tau_v2l, tau_l2v, tau_i2s, qi_lim, ql_gen,  &
                      c_paut, c_psaut, c_psaci, c_pgacs, z_slope_liq, z_slope_ice, prog_ccn,  &
                      c_cracw, alin, clin, p_crt, tice, k_moist, rad_snow, rad_graupel, rad_rain,   &
                      cld_min, use_ppm, ppm_rain_fall, mono_prof, do_sedi_heat,sedi_transport,   &
                      do_sedi_w,  de_ice, mp_debug, mp_print

!---- version number -----
 character(len=128) :: version = '$Id: lin_cloud_microphys.F90,v 21.0.2.1 2014/12/18 21:14:54 Lucas.Harris Exp $'
 character(len=128) :: tagname = '$Name:  $'

 contains


  subroutine lin_cld_microphys_driver(qv, ql, qr, qi, qs, qg, qa, qn,                &
                               qv_dt, ql_dt, qr_dt, qi_dt, qs_dt, qg_dt, qa_dt,      &
                               pt_dt, pt, w, uin, vin, udt, vdt, dz, delp, area, dt_in, &
                               land,  rain, snow, ice, graupel,                      &
                               hydrostatic, phys_hydrostatic,                        &
                               iis,iie, jjs,jje, kks,kke, ktop, kbot, time)
! kks == 1; kke == kbot == npz
  type(time_type), intent(in):: time
  logical,         intent(in):: hydrostatic, phys_hydrostatic
  integer,         intent(in):: iis,iie, jjs,jje  ! physics window
  integer,         intent(in):: kks,kke           ! vertical dimension
  integer,         intent(in):: ktop, kbot        ! vertical compute domain
  real,            intent(in):: dt_in

  real, intent(in   ), dimension(:,:)  :: area
  real, intent(in   ), dimension(:,:)  :: land  !land fraction
  real, intent(out  ), dimension(:,:)  :: rain, snow, ice, graupel
  real, intent(in   ), dimension(:,:,:):: delp, dz, uin, vin
  real, intent(in   ), dimension(:,:,:):: pt, qv, ql, qr, qg, qa, qn 
  real, intent(inout), dimension(:,:,:):: qi, qs
  real, intent(inout), dimension(:,:,:):: pt_dt,  qa_dt, udt, vdt, w
  real, intent(inout), dimension(:,:,:):: qv_dt, ql_dt, qr_dt, qi_dt,  &
                                          qs_dt, qg_dt


! local:
  logical used
  real    :: mpdt, rdt, dts, convt, tot_prec
  integer :: i,j,k
  integer :: is,ie, js,je  ! physics window
  integer :: ks,ke         ! vertical dimension
  integer :: seconds, days, ntimes

  real, dimension(iie-iis+1,jje-jjs+1)           :: prec1, prec_mp, cond, w_var, rh0, maxdbz
  real, dimension(iie-iis+1,jje-jjs+1,kke-kks+1) :: vt_r, vt_s, vt_g, vt_i, qn2, dbz
  real, dimension(size(pt,1), size(pt,3)) :: m2_rain, m2_sol

  real :: allmax

  is = 1
  js = 1
  ks = 1
  ie = iie-iis+1
  je = jje-jjs+1
  ke = kke-kks+1

  call mpp_clock_begin (lin_cld_mp_clock)

  if ( phys_hydrostatic .or. hydrostatic ) then
        lv00 = Lv0
      c_air = cp_air
      c_vap = cp_vapor
      p_nonhydro = .false.
  else
        lv00 = Lv0
      c_air = cv_air
      c_vap = cv_vap
      p_nonhydro = .true.
  endif
  if ( hydrostatic ) do_sedi_w = .false.
  latv = hlv
  lati = hlf
  lats = latv + lati
  lcp = latv / cp_air
  icp = lati / cp_air
  tcp = (latv+lati) / cp_air

! tendency zero out for am moist processes should be done outside the driver
     mpdt = min(dt_in, mp_time)
      rdt = 1. / dt_in
   ntimes = nint( dt_in/mpdt )
! small time step:
      dts = dt_in / real(ntimes)

  call get_time (time, seconds, days)


  do j=js, je
     do i=is, ie
        graupel(i,j) = 0.
           rain(i,j) = 0.
           snow(i,j) = 0.
            ice(i,j) = 0.
           cond(i,j) = 0.
     enddo
  enddo
  do j=js,je
     call mpdrv( hydrostatic, uin, vin, w, delp, pt, qv, ql, qr, qi, qs, qg, qa, qn, dz,  &
                 is, ie, js, je, ks, ke, ktop, kbot, j, dt_in,  &
                 ntimes, rain(:,j), snow(:,j), graupel(:,j), &
                 ice(:,j), m2_rain, m2_sol, cond(:,j), area(:,j), land(:,j),  &
                 udt, vdt, pt_dt, qv_dt, ql_dt, qr_dt, qi_dt,    &
                 qs_dt, qg_dt, qa_dt, &
                 w_var, vt_r, vt_s, vt_g, vt_i, qn2 )

  enddo

! no clouds allowed above ktop
   if ( ks < ktop ) then
      do k=ks, ktop
        if ( do_qa ) then
           do j=js,je
              do i=is,ie
                 qa_dt(i,j,k) = 0.
              enddo
           enddo
        else
           do j=js,je
              do i=is,ie
                 qa_dt(i,j,k) = -qa(i,j,k) * rdt
              enddo
           enddo
        endif
      enddo
   endif

   if ( id_vtr> 0 ) then
     used=send_data(id_vtr, vt_r, time, is_in=iis, js_in=jjs)
   endif

   if ( id_vts> 0 ) then
     used=send_data(id_vts, vt_s, time, is_in=iis, js_in=jjs)
   endif

   if ( id_vtg> 0 ) then
     used=send_data(id_vtg, vt_g, time, is_in=iis, js_in=jjs)
   endif

   if ( id_vti> 0 ) then
     used=send_data(id_vti, vt_i, time, is_in=iis, js_in=jjs)
   endif

   if ( id_droplets> 0 ) then
     used=send_data(id_droplets, qn2, time, is_in=iis, js_in=jjs)
   endif

   if ( id_var> 0 ) then
     used=send_data(id_var, w_var, time, is_in=iis, js_in=jjs)
   endif

! Convert to mm/day
   convt = 86400.*rdt*rgrav
   do j=js,je
      do i=is,ie
            rain(i,j) =    rain(i,j) * convt
            snow(i,j) =    snow(i,j) * convt
             ice(i,j) =     ice(i,j) * convt
         graupel(i,j) = graupel(i,j) * convt
         prec_mp(i,j) =    rain(i,j) + snow(i,j) + ice(i,j) + graupel(i,j)
      enddo
   enddo

   if ( id_cond>0 ) then
        do j=js,je
           do i=is,ie
              cond(i,j) = cond(i,j)*rgrav
           enddo
        enddo
        used=send_data(id_cond, cond, time, is_in=iis, js_in=jjs)
   endif

   if ( id_snow>0 ) then
!       used=send_data(id_snow,    snow,    time, iis, jjs)
        used=send_data(id_snow, snow, time, is_in=iis, js_in=jjs)
        if ( mp_print .and. seconds==0 ) then
             tot_prec = g_sum(snow, is, ie, js, je, area, 1)
             if(master) write(*,*) 'mean snow=', tot_prec
        endif
   endif

   if ( id_graupel>0 ) then
!       used=send_data(id_graupel, graupel, time, iis, jjs)
        used=send_data(id_graupel, graupel, time, is_in=iis, js_in=jjs)
        if ( mp_print .and. seconds==0 ) then
             tot_prec = g_sum(graupel, is, ie, js, je, area, 1)
             if(master) write(*,*) 'mean graupel=', tot_prec
        endif
   endif

   if ( id_ice>0 ) then
!       used=send_data(id_ice, ice, time, iis, jjs)
        used=send_data(id_ice, ice, time, is_in=iis, js_in=jjs)
        if ( mp_print .and. seconds==0 ) then
             tot_prec = g_sum(ice, is, ie, js, je, area, 1)
             if(master) write(*,*) 'mean ice_mp=', tot_prec
        endif
   endif

   if ( id_rain>0 ) then
!       used=send_data(id_rain,    rain,    time, iis, jjs)
        used=send_data(id_rain, rain, time, is_in=iis, js_in=jjs)
        if ( mp_print .and. seconds==0 ) then
             tot_prec = g_sum(rain, is, ie, js, je, area, 1)
             if(master) write(*,*) 'mean rain=', tot_prec
        endif
   endif

   if ( id_rh>0 ) then !Not used?
!       used=send_data(id_rh,  rh0,   time, iis, jjs)
        used=send_data(id_rh, rh0, time, is_in=iis, js_in=jjs)
   endif


   if ( id_prec>0 ) then
!       used=send_data(id_prec, prec_mp, time, iis, jjs)
        used=send_data(id_prec, prec_mp, time, is_in=iis, js_in=jjs)
   endif

   if ( id_dbz>0 .or. id_maxdbz>0 .or. id_basedbz>0) then
      call dbzcalc(qv, qr, qs, qg, pt, delp, dz, &
           dbz, maxdbz, allmax, is, ie, js, je, ks, ke, .true., .true., .true., .true.)
      if (id_dbz > 0) then
         used=send_data(id_dbz, dbz, time, is_in=iis, js_in=jjs) 
      endif
      if (id_maxdbz > 0) then
         used=send_data(id_maxdbz, maxdbz, time, is_in=iis, js_in=jjs)
      endif
      if (id_basedbz > 0) then
         !interpolate to 1km dbz
         !Reusing maxdbz
         do j=js,je
         do i=is,ie
            maxdbz(i,j) = dbz(i,j,ke)
         enddo
         enddo
         used=send_data(id_basedbz, maxdbz, time, is_in=iis, js_in=jjs)
      endif

      if (mp_print .and. seconds == 0) then
         call mpp_max(allmax)
         if (master) write(*,*) 'max reflectivity = ', allmax, ' dBZ'
      endif
      
   endif

!----------------------------------------------------------------------------

   if ( mp_print ) then
        prec1(:,:) = prec1(:,:) + prec_mp(:,:)
        if ( seconds==0 ) then
             prec1(:,:) = prec1(:,:)*dt_in/86400.
             tot_prec = g_sum(prec1, is, ie, js, je, area, 1)
             if(master) write(*,*) 'Daily prec_mp=', tot_prec
             prec1(:,:) = 0.
        endif
   endif
!----------------------------------------------------------------------------


   call mpp_clock_end (lin_cld_mp_clock)

 end subroutine lin_cld_microphys_driver



 subroutine mpdrv( hydrostatic, uin, vin, w, delp, pt, qv, ql, qr, qi, qs, qg, qa, qn, dz,     &
                   is, ie, js, je, ks, ke, ktop, kbot, j, dt_in, ntimes,  &
                   rain, snow, graupel, ice, m2_rain, m2_sol, &
                   cond, area1, land, u_dt, v_dt, pt_dt, qv_dt, ql_dt, qr_dt, qi_dt, &
                   qs_dt, qg_dt, qa_dt, &
                   w_var, vt_r, vt_s, vt_g, vt_i, qn2 )

!-------------------------------------------------------------------
!  lin et al., 1983, jam, 1065-1092, and
!  rutledge and hobbs, 1984, jas, 2949-2972
!-------------------------------------------------------------------
! terminal fall is handled lagrangianly by conservative fv algorithm
!
! pt: temperature (k)
! 6 water species:
! 1) qv: water vapor (kg/kg)
! 2) ql: cloud water (kg/kg)
! 3) qr: rain        (kg/kg)
! 4) qi: cloud ice   (kg/kg)
! 5) qs: snow        (kg/kg)
! 6) qg: graupel     (kg/kg)

  logical,         intent(in):: hydrostatic
  integer,         intent(in):: j, is,ie, js,je, ks,ke
  integer,         intent(in):: ntimes, ktop, kbot
  real,            intent(in):: dt_in

  real, intent(in), dimension(is:):: area1, land
  real, intent(in), dimension(is:,js:,ks:):: uin, vin, delp, pt, qv, ql, qr, qg, qa, qn, dz
  real, intent(inout), dimension(is:,js:,ks:):: qi, qs
  real, intent(inout), dimension(is:,js:,ks:):: u_dt, v_dt, w, pt_dt,  qa_dt,  &
                                            qv_dt, ql_dt, qr_dt, qi_dt, qs_dt, qg_dt
  real, intent(inout), dimension(is:):: rain, snow, ice, graupel, cond
  real, intent(out), dimension(is:,js:)       :: w_var
  real, intent(out), dimension(is:,js:,ks:) :: vt_r, vt_s, vt_g, vt_i, qn2
  real, intent(out), dimension(is:,ks:):: m2_rain,  m2_sol

!----------
! local var
!----------
  real, dimension(ktop:kbot):: qvz, qlz, qrz, qiz, qsz, qgz, qaz, &
                               vtiz, vtsz, vtgz, vtrz, dp0, &
                               dp1, qv0, ql0, qr0, qi0, qs0, qg0, qa0, t0, den, &
                               den0, tz, p1, dz0, dz1, denfac
  real, dimension(ktop:kbot):: ccn, c_praut, m1_rain, m1_sol, m1, u0, v0, u1, v1, w1
  real :: cpaut,  rh_adj, rh_rain
  real :: r1, s1, i1, g1, rdt, ccn0
  real :: dt_rain, dts, fac_sno, fac_gra
  real :: s_leng, t_land, t_ocean, h_var
  real :: cvm, tmp, omq
  real:: dqi, qio, qin
  integer :: i,k,n

       dts = dt_in / real(ntimes)
   dt_rain = dts * 0.5

   fac_sno = 1. - exp( -0.5*dts/tau_s )
   fac_gra = 1. - exp( -0.5*dts/tau_g )

   rdt = 1. / dt_in

! SJL: 20150210
   cpaut = c_paut*0.104*grav/1.717e-5

   do 2000 i=is, ie
      do k=ktop, kbot
         qiz(k) = qi(i,j,k)
         qsz(k) = qs(i,j,k)
      enddo
      if ( de_ice ) then
! This is to prevent excessive build-up of cloud ice from external sources
        do k=ktop, kbot
           qio = qiz(k) - dt_in*qi_dt(i,j,k)   ! original qi before phys
           qin = max(qio, qi0_max)             ! Adjusted value
           if ( qiz(k) > qin ) then
                qsz(k) = qsz(k) + qiz(k) - qin
                qiz(k) = qin
                dqi = (qin - qio)*rdt  ! modified qi tendency
                qs_dt(i,j,k) = qs_dt(i,j,k) + qi_dt(i,j,k) - dqi
                qi_dt(i,j,k) = dqi
                qi(i,j,k) = qiz(k)
                qs(i,j,k) = qsz(k)
           endif
        enddo
      endif

   do k=ktop, kbot
       t0(k) = pt(i,j,k)
       tz(k) = t0(k)
      dp1(k) = delp(i,j,k)
      dp0(k) = dp1(k)       ! moist air mass * grav
!-- Moist mixing ratios ------------
      qvz(k) = qv(i,j,k)
      qlz(k) = ql(i,j,k)
      qrz(k) = qr(i,j,k)
      qgz(k) = qg(i,j,k)
!-- dp1: Dry air_mass
      dp1(k) = dp1(k)*(1. -(qvz(k)+qlz(k)+qrz(k)+qiz(k)+qsz(k)+qgz(k)))
!-- Convert to dry mixing ratios
         omq = dp0(k) / dp1(k)
      qvz(k) = qvz(k)*omq
      qlz(k) = qlz(k)*omq
      qrz(k) = qrz(k)*omq
      qiz(k) = qiz(k)*omq
      qsz(k) = qsz(k)*omq
      qgz(k) = qgz(k)*omq
!-----------------------------------
      qa0(k) = qa(i,j,k)
      qaz(k) = 0.
      dz0(k) = dz(i,j,k)
!--------------------------
     den0(k) = -dp1(k)/(grav*dz0(k))  ! density of dry air
       p1(k) = den0(k)*rdgas*t0(k)    ! dry air pressure
!------------------------------
! Save a copy of old value for computing tendencies
      qv0(k) = qvz(k)
      ql0(k) = qlz(k)
      qr0(k) = qrz(k)
      qi0(k) = qiz(k)
      qs0(k) = qsz(k)
      qg0(k) = qgz(k)
! *** For sedi_momentum ***
       m1(k) = 0.
       u0(k) = uin(i,j,k)
       v0(k) = vin(i,j,k)
       u1(k) = u0(k)
       v1(k) = v0(k)
! *** For sedi_momentum ***
   enddo

   if ( do_sedi_w ) then
     do k=ktop, kbot
        w1(k) = w(i,j,k)
     enddo
   endif
! Based on Klein Eq. 15

 if ( prog_ccn ) then
   do k=ktop, kbot
!convert #/cc to #/m^3
      ccn(k) = qn(i,j,k) * 1.e6
      c_praut(k) = cpaut * (ccn(k)*rhor)**(-1./3.)
   enddo
   use_ccn = .false.
 else
   ccn0 = (ccn_l*land(i) + ccn_o*(1.-land(i))) * 1.e6
   if ( use_ccn ) then
! *** CCN is formulted as CCN = CCN_surface * (den/den_surface) ***
        ccn0 = ccn0 * rdgas*tz(kbot)/p1(kbot)
   endif
   tmp = cpaut * (ccn0*rhor)**(-1./3.)
   do k=ktop, kbot
      c_praut(k) = tmp
          ccn(k) = ccn0
   enddo
  endif

!--------------------------------------------------------
! Total water subgrid deviation in horizontal direction
!--------------------------------------------------------
!       default area dependent form: use dx ~ 100 km as the base
   s_leng  = sqrt(sqrt(area1(i)/1.E10))
   t_land  = dw_land  * s_leng
   t_ocean = dw_ocean * s_leng
   h_var = t_land*land(i) + t_ocean*(1.-land(i))
   h_var = min(0.20, max(0.01, h_var))            ! cap:
   if ( id_var>0 ) w_var(i,j) = h_var

   rh_adj  = 1. - h_var - rh_inc
   rh_rain = max(0.35, rh_adj - rh_inr)

!-------------------------
! * fix all negatives
!-------------------------

 if ( fix_negative )  &
 call neg_adj(ktop, kbot, p1, tz, dp1, qvz, qlz, qrz, qiz, qsz, qgz)

 m2_rain(:,:) = 0.
 m2_sol(:,:) = 0.
 do 1000 n=1,ntimes

   if ( p_nonhydro ) then
   do k=ktop, kbot
          dz1(k) = dz0(k)
          den(k) = den0(k)    ! dry air density remains the same
          denfac(k) = sqrt(sfcrho/den(k))
       enddo
   else
       do k=ktop, kbot
          dz1(k) = dz0(k)*tz(k)/t0(k)    ! hydrostatic balance
         den(k) = den0(k)*dz0(k)/dz1(k)
      denfac(k) = sqrt(sfcrho/den(k))
   enddo
   endif

!-------------------------------------------
! Time-split warm rain processes: first pass
!-------------------------------------------
   call warm_rain(dt_rain, ktop, kbot, p1, dp1, dz1, tz, qvz, qlz, qrz, qiz, qsz, qgz, p1, den, denfac, ccn,  &
                  c_praut, rh_rain, vtrz, r1, m1_rain, w1, h_var)
   rain(i) = rain(i) + r1
   do k=ktop, kbot
      m2_rain(i,k) = m2_rain(i,k) + m1_rain(k)
      m1(k) = m1(k) + m1_rain(k)
   enddo

!------------------------------------------------
! * sedimentation of cloud ice, snow, and graupel
!------------------------------------------------
   call fall_speed(ktop, kbot, den, qsz, qiz, qgz, qlz, tz, vtsz, vtiz, vtgz)

   call terminal_fall ( dts, ktop, kbot, tz, qvz, qlz, qrz, qgz, qsz, qiz, p1, &
                        dz1, dp1, den, vtgz, vtsz, vtiz, fac_sno, fac_gra,    &
                        r1, g1, s1, i1, m1_sol, w1 )

      rain(i) = rain(i)    + r1  ! from melted snow & ice that reached the ground
      snow(i) = snow(i)    + s1
   graupel(i) = graupel(i) + g1
       ice(i) = ice(i)     + i1
   if ( do_sedi_heat )  &
   call sedi_heat(ktop, kbot, dp1, m1_sol, dz1, tz, qvz, qlz, qrz, qiz, qsz, qgz, c_ice)

!-------------------------------------------
! Time-split warm rain processes: 2nd pass
!-------------------------------------------
   call warm_rain(dt_rain, ktop, kbot, p1, dp1, dz1, tz, qvz, qlz, qrz, qiz, qsz, qgz, p1, den, denfac, ccn,  &
                  c_praut, rh_rain, vtrz, r1, m1_rain, w1, h_var)
   rain(i) = rain(i) + r1
   do k=ktop, kbot
      m2_rain(i,k) = m2_rain(i,k) + m1_rain(k)
       m2_sol(i,k) = m2_sol(i,k) + m1_sol(k)
      m1(k) = m1(k) + m1_rain(k) + m1_sol(k)
   enddo

!-------------------------
! * ice-phase microphysics
!-------------------------

   call icloud( ktop, kbot, tz, p1, qvz, qlz, qrz, qiz, qsz, qgz,  &
                dp1, den, denfac, vtsz, vtgz, vtrz, qaz, rh_adj, rh_rain, dts, fac_sno, fac_gra, h_var )

1000  continue  ! sub-cycle
   if ( sedi_transport ) then
! Note: dp1 is dry mass; dp0 is the old moist (total) mass
       do k = ktop+1, kbot
          u1(k) = (dp0(k)*u1(k) + m1(k-1)*u1(k-1))/(dp0(k)+m1(k-1))
          v1(k) = (dp0(k)*v1(k) + m1(k-1)*v1(k-1))/(dp0(k)+m1(k-1))
          u_dt(i,j,k) = u_dt(i,j,k) + (u1(k)-u0(k))*rdt
          v_dt(i,j,k) = v_dt(i,j,k) + (v1(k)-v0(k))*rdt
       enddo
   endif
       if ( do_sedi_w ) then
        do k = ktop, kbot
           w(i,j,k) = w1(k)
       enddo
   endif

   do k = ktop, kbot
! update moist air mass (actually hydrostatic pressure)
!-- Convert to dry mixing ratios
         omq = dp1(k) / dp0(k)
      qv_dt(i,j,k) = qv_dt(i,j,k) + rdt*(qvz(k)-qv0(k))*omq
      ql_dt(i,j,k) = ql_dt(i,j,k) + rdt*(qlz(k)-ql0(k))*omq
      qr_dt(i,j,k) = qr_dt(i,j,k) + rdt*(qrz(k)-qr0(k))*omq
      qi_dt(i,j,k) = qi_dt(i,j,k) + rdt*(qiz(k)-qi0(k))*omq
      qs_dt(i,j,k) = qs_dt(i,j,k) + rdt*(qsz(k)-qs0(k))*omq
      qg_dt(i,j,k) = qg_dt(i,j,k) + rdt*(qgz(k)-qg0(k))*omq
      cvm = c_air + qvz(k)*c_vap + (qrz(k)+qlz(k))*c_liq + (qiz(k)+qsz(k)+qgz(k))*c_ice
           pt_dt(i,j,k) = pt_dt(i,j,k) + rdt*(tz(k)- t0(k))*cvm/cp_air
   enddo

   do k = ktop, kbot
      if ( do_qa ) then
           qa_dt(i,j,k) = 0.
      else
           qa_dt(i,j,k) = qa_dt(i,j,k) + rdt*( qaz(k)/real(ntimes)-qa0(k))
      endif
   enddo

!-----------------
! fms diagnostics:
!-----------------

   if ( id_cond>0 ) then
     do k=ktop,kbot                   ! total condensate
        cond(i) = cond(i) + dp1(k)*(qlz(k)+qrz(k)+qsz(k)+qiz(k)+qgz(k))
     enddo
   endif

   if ( id_vtr> 0 ) then
        do k=ktop, kbot
           vt_r(i,j,k) = vtrz(k)
        enddo
   endif
   if ( id_vts> 0 ) then
        do k=ktop, kbot
           vt_s(i,j,k) = vtsz(k)
        enddo
   endif
   if ( id_vtg> 0 ) then
        do k=ktop, kbot
           vt_g(i,j,k) = vtgz(k)
        enddo
   endif
   if ( id_vts> 0 ) then
        do k=ktop, kbot
           vt_i(i,j,k) = vtiz(k)
        enddo
   endif

   if ( id_droplets> 0 ) then
        do k=ktop, kbot
           qn2(i,j,k) = ccn(k)
        enddo
   endif

2000  continue

 end subroutine mpdrv



 subroutine sedi_heat(ktop, kbot, dm, m1, dz, tz, qv, ql, qr, qi, qs, qg, cw)
 integer, intent(in):: ktop, kbot
! Input q fields are dry mixing ratios, and dm is dry air mass
 real, intent(in   ), dimension(ktop:kbot):: dm, m1, dz, qv, ql, qr, qi, qs, qg
 real, intent(inout), dimension(ktop:kbot):: tz
 real, intent(in):: cw
!
 real,dimension(ktop:kbot):: dgz, cvm, cvn
 real:: tmp
 integer:: k


   do k=ktop, kbot
      dgz(k) = -0.5*grav*dz(k)    ! > 0
      cvn(k) = dm(k)*(cv_air + qv(k)*cv_vap + (qr(k)+ql(k))*c_liq + (qi(k)+qs(k)+qg(k))*c_ice)
   enddo

! SJL July 2014
! Assumption: the KE in the falling condensates is negligible compared to the potential energy
! that was unaccounted for. Local thermal equilibrium is assumed, and the loss in PE is transformed
! into internal energy (to heat the whole grid box)
! Backward time-implicit upwind transport scheme:
! dm here is dry air mass

   k = ktop
     tmp = cvn(k) + m1(k)*cw
   tz(k) = (tmp*tz(k) + m1(k)*dgz(k) ) / tmp

   do k=ktop+1, kbot
      tz(k) = ( (cvn(k)+cw*(m1(k)-m1(k-1)))*tz(k) + m1(k-1)*cw*tz(k-1) + dgz(k)*(m1(k-1)+m1(k)) )    &
            / (  cvn(k)+cw*m1(k) )
   enddo

 end subroutine sedi_heat


 subroutine warm_rain( dt, ktop, kbot, p1, dp, dz, tz, qv, ql, qr, qi, qs, qg, pm, &
                       den, denfac, ccn, c_praut, rh_rain, vtr, r1, m1_rain, w1, h_var)

 integer, intent(in):: ktop, kbot
 real,    intent(in):: dt                    ! time step (s)
 real,    intent(in), dimension(ktop:kbot):: dp, p1, dz, pm, den, denfac, ccn, c_praut
 real,    intent(in):: rh_rain, h_var
 real, intent(inout), dimension(ktop:kbot):: tz, qv, ql, qr, qi, qs, qg, vtr
 real, intent(out):: r1
 real, intent(inout), dimension(ktop:kbot):: m1_rain, w1

! local:
 real, parameter:: so3 = 7./3.
 real, dimension(ktop:kbot):: dl, dm
 real, dimension(ktop:kbot+1):: ze, zt
 real:: sink, dq, qc0, qc
 real:: rho0, qden
 real:: zs = 0.
 real:: dt5
 integer k
!-----------------------------------------------------------------------
! fall velocity constants:
!-----------------------------------------------------------------------
 real, parameter :: vconr = 2503.23638966667
 real, parameter :: normr = 25132741228.7183
 real, parameter :: thr=1.e-10
 logical no_fall

!---------------------
! warm-rain processes:
!---------------------

  dt5 = 0.5*dt

!------------------------
! Terminal speed of rain:
!------------------------

  m1_rain(:) = 0.
  call check_column(ktop, kbot, qr, no_fall)
  if ( no_fall ) then
       vtr(:) = vmin
       r1 = 0.
       go to 999   ! jump to auto-conversion
  endif

  if ( den_ref < 0. ) then
       rho0 = -den_ref*den(kbot)
  else
       rho0 = den_ref   ! default=1.2
  endif

  do k=ktop, kbot
     qden = qr(k)*den(k)
     if ( qr(k) < thr ) then
         vtr(k) = vmin
     else
         vtr(k) = max(vmin, vr_fac*vconr*sqrt(min(10., rho0/den(k)))*exp(0.2*log(qden/normr)))
     endif
  enddo

  ze(kbot+1) = zs
  do k=kbot, ktop, -1
     ze(k) = ze(k+1) - dz(k)  ! dz<0
  enddo
  zt(ktop) = ze(ktop)


 do k=ktop+1,kbot
    zt(k) = ze(k) - dt5*(vtr(k-1)+vtr(k))
 enddo
 zt(kbot+1) = zs - dt*vtr(kbot)

 do k=ktop,kbot
    if( zt(k+1)>=zt(k) ) zt(k+1) = zt(k) - dz_min
 enddo

! Evap_acc of rain for 1/2 time step
  if ( .not. fast_sat_adj )  &
  call revap_racc( ktop, kbot, dt5, tz, qv, ql, qr, qi, qs, qg, den, denfac, rh_rain, h_var )

  if ( do_sedi_w ) then
      do k=ktop, kbot
         dm(k) = dp(k)*(1.+qv(k)+ql(k)+qr(k)+qi(k)+qs(k)+qg(k))
      enddo
  endif

  if ( ppm_rain_fall ) then
       call lagrangian_fall_ppm(ktop, kbot, zs, ze, zt, dp, qr, r1, m1_rain, mono_prof)
  else
       call lagrangian_fall_pcm(ktop, kbot, zs, ze, zt, dp, qr, r1, m1_rain)
  endif

  if ( do_sedi_w ) then
      w1(ktop) = (dm(ktop)*w1(ktop)+m1_rain(ktop)*vtr(ktop)) /(dm(ktop)-m1_rain(ktop))
      do k=ktop+1, kbot
         w1(k) = (dm(k)*w1(k)-m1_rain(k-1)*vtr(k-1)+m1_rain(k)*vtr(k))   &
               / (dm(k)      +m1_rain(k-1)         -m1_rain(k))
      enddo
  endif
  if ( do_sedi_heat )  &
  call sedi_heat(ktop, kbot, dp, m1_rain, dz, tz, qv, ql, qr, qi, qs, qg, c_liq)
! Finish the remaing 1/2 time step
  call revap_racc( ktop, kbot, dt5, tz, qv, ql, qr, qi, qs, qg, den, denfac, rh_rain, h_var )

999  continue

!-------------------
! * auto-conversion
!-------------------
! Assuming linear subgrid vertical distribution of cloud water
! following Lin et al. 1994, MWR

  call linear_prof( kbot-ktop+1, p1(ktop), ql(ktop), dl(ktop), z_slope_liq, h_var )



  do k=ktop,kbot
    qc0 = fac_rc*ccn(k)
    if ( tz(k) > t_wfr + dt_fr ) then
      dl(k) = min(max(1.e-5, dl(k)), 0.5*ql(k))

!--------------------------------------------------------------------
!  As in Klein's GFDL AM2 stratiform scheme (with subgrid variations)
!--------------------------------------------------------------------
      if ( use_ccn ) then
!  CCN is formulted as CCN = CCN_surface * (den/den_surface)
           qc = qc0
      else
           qc = qc0/den(k)
      endif

      dq = 0.5*(ql(k) + dl(k) - qc)    ! dq = dl  if qc==q_minus = ql - dl
                                       ! dq = 0   if qc==q_plus  = ql + dl
      if ( dq > 0. ) then    ! q_plus > qc
! Revised continuous form: linearly decays (with subgrid dl) to zero at qc == ql + dl
            sink = min(1.,dq/dl(k)) * dt*c_praut(k)*den(k)*exp(so3*log(ql(k)))
           ql(k) = ql(k) - sink
           qr(k) = qr(k) + sink
      endif
    endif
  enddo


 end subroutine warm_rain

 subroutine revap_racc( ktop, kbot, dt, tz, qv, ql, qr, qi, qs, qg, den, denfac, rh_rain, h_var)
 integer, intent(in):: ktop, kbot
 real,    intent(in):: dt                 ! time step (s)
 real,    intent(in), dimension(ktop:kbot):: den, denfac
 real,    intent(in)                      :: rh_rain, h_var
 real, intent(inout), dimension(ktop:kbot):: tz, qv, qr, ql, qi, qs, qg
! local:
 real:: dqv, qsat, dqsdt, evap, t2, qden, q_plus, q_minus, sink
 real:: qpz, dq, dqh, tin, lcpk
 integer k

  do k=ktop,kbot
     if ( tz(k) > t_wfr .and. qr(k) > qrmin ) then

      lcpk = (lv00+dc_vap*tz(k)) / (c_air + qv(k)*c_vap + (qr(k)+ql(k))*c_liq + &
                (qi(k)+qs(k)+qg(k))*c_ice)

             tin = tz(k) - lcpk*ql(k) ! presence of clouds suppresses the rain evap
             qpz = qv(k) + ql(k)
            qsat = wqs2(tin, den(k), dqsdt)
             dqh = max(ql(k), h_var*max(qpz, qcmin))
             dqv = qsat - qv(k)   ! use this to prevent super-sat the gird box
         q_minus = qpz - dqh
         q_plus  = qpz + dqh

! qsat must be > q_minus to activate evaporation
! qsat must be < q_plus  to activate accretion

!-------------------
! * Rain evaporation
!-------------------
         if ( dqv>qvmin .and. qsat > q_minus ) then
              if ( qsat > q_plus ) then
                   dq = qsat - qpz
              else
! q_minus < qsat < q_plus
! dq == dqh if qsat == q_minus
                  dq = 0.25*(q_minus-qsat)**2 / dqh
              endif
              qden = qr(k)*den(k)
               t2 = tin*tin
              evap =  crevp(1)*t2*dq*(crevp(2)*sqrt(qden)+crevp(3)*exp(0.725*log(qden)))   &
                   / (crevp(4)*t2 + crevp(5)*qsat*den(k))
              evap = min( qr(k), dt*evap, dqv/(1.+lcpk*dqsdt) )
! Alternative Minimum Evap in dry environmental air
              sink = min( qr(k), dim(rh_rain*qsat, qv(k))/(1.+lcpk*dqsdt) )
              evap = max( evap, sink )
             qr(k) = qr(k) - evap
             qv(k) = qv(k) + evap
             tz(k) = tz(k) - evap*lcpk
         endif

         if ( qr(k)>qrmin .and. ql(k)>1.E-8  .and.  qsat<q_plus ) then
!-------------------
! * Accretion: pracc
!-------------------
               sink = dt*denfac(k)*cracw * exp(0.95*log(qr(k)*den(k)))
               sink = sink/(1.+sink)*ql(k)
              ql(k) = ql(k) - sink
              qr(k) = qr(k) + sink
         endif

     endif   ! warm-rain
  enddo

 end subroutine revap_racc


 subroutine linear_prof(km, p1,  q, dm, z_var, h_var)
! Used for cloud ice and cloud water autoconversion
! qi --> ql  & ql --> qr
! Edges: qE == qbar +/- dm
 integer, intent(in):: km
 real, intent(in ):: p1(km),  q(km), h_var
 real, intent(out):: dm(km)
 logical, intent(in):: z_var
!
 real:: dq(km)
 integer:: k

 if ( z_var ) then
    do k=2,km
       dq(k) = 0.5*(q(k) - q(k-1))
    enddo
    dm(1) = 0.
    do k=2, km-1
! Use twice the strength of the  positive definiteness limiter (Lin et al 1994)
       dm(k) = 0.5*min(abs(dq(k) + dq(k+1)), 0.5*q(k))
       if ( dq(k)*dq(k+1) <= 0. ) then
            if ( dq(k) > 0. ) then   ! Local max
                 dm(k) = min( dm(k), dq(k), -dq(k+1) )
            else
                 dm(k) = 0.
            endif
       endif
    enddo
    dm(km) = 0.
! impose a presumed background horizontal variability that is proportional to the value itself
    do k=1, km
       dm(k) = max( dm(k), qvmin, h_var*q(k) )
    enddo
 else
    do k=1, km
       dm(k) = max( qvmin, h_var*q(k) )
    enddo
 endif

 end subroutine linear_prof


 subroutine icloud(ktop, kbot, tzk, p1, qvk, qlk, qrk, qik, qsk, qgk, dp1, &
                   den, denfac, vts, vtg, vtr, qak, rh_adj, rh_rain, dts, fac_sno, fac_gra, h_var)

!----------------------------------------------------
! Bulk cloud micro-physics; processes splitting
! with some un-split sub-grouping
! Time implicit (when possible) accretion and autoconversion
! Author: Shian-Jiann Lin, GFDL
!-------------------------------------------------------

 integer, intent(in) :: ktop, kbot
 real, intent(in),    dimension(ktop:kbot):: p1, dp1, den, denfac, vts, vtg, vtr
 real, intent(inout), dimension(ktop:kbot):: tzk, qvk, qlk, qrk, qik, qsk, qgk, qak
 real, intent(in) :: rh_adj, rh_rain, dts, fac_sno, fac_gra, h_var
! local:
 real, parameter:: rhos = 0.1e3    ! snow density (1/10 of water)
 real, dimension(2*(kbot-ktop-1)):: p2, den2, tz2, qv2, ql2, qr2, qi2, qs2, qg2, qa2
 real, dimension(ktop:kbot) :: lcpk, icpk, tcpk, di
!!! real:: rh_sno, rh_gra
 real:: rdts, fac_g2v, fac_v2g, fac_i2s
 real:: tz, qv, ql, qr, qi, qs, qg, melt
 real:: pracw, pracs, psacw, pgacw, pgmlt,   &
        psmlt, prevp, psacr, pgacr, pgfr,  pgacs,   &
        pgaut, pgaci, praci, psaut, psaci, piacr, pgsub
 real:: tc, tsq, dqs0, qden, qim, qsm, pssub
 real:: factor, sink
 real:: tmp1, qsw, qsi, dqsdt, dq
 real:: dtmp, qc, q_plus, q_minus, cvm
 integer:: km, kn
 integer:: i, j, k, k1

! rh_sno = max(0.3, rh_adj - rh_ins)
! rh_gra = rh_sno - 0.05

 fac_i2s = 1. - exp( -dts/tau_i2s )
 fac_g2v = 1. - exp( -dts/tau_g2v )
 fac_v2g = 1. - exp( -dts/tau_v2g )

 rdts = 1./dts

 do k=ktop,kbot
    cvm = c_air + qvk(k)*c_vap + (qlk(k)+qrk(k))*c_liq + (qik(k)+qsk(k)+qgk(k))*c_ice
    lcpk(k) = (lv00+dc_vap*tzk(k)) / cvm
    icpk(k) = (li00+dc_ice*tzk(k)) / cvm
   tcpk(k) = lcpk(k) + icpk(k)
 enddo

! Sources of cloud ice: pihom, cold rain, and the sat_adj
! (initiation plus deposition)

! Sources of snow: cold rain, auto conversion + accretion (from cloud ice)
! sat_adj (deposition; requires pre-existing snow); initial snow comes from auto conversion

 do k=ktop, kbot
!--------------------------------------
! * pimlt: instant melting of cloud ice
!--------------------------------------
    if( tzk(k) > tice .and. qik(k) > qcmin ) then
        melt = min( qik(k), (tzk(k)-tice)/icpk(k) )
        tmp1 = min( melt, dim(ql_mlt, qlk(k)) )   ! max ql amount
         qlk(k) = qlk(k) + tmp1
         qrk(k) = qrk(k) + melt - tmp1
         qik(k) = qik(k) - melt
         tzk(k) = tzk(k) - melt*icpk(k)
    endif
 enddo

 call linear_prof( kbot-ktop+1, p1(ktop), qik(ktop), di(ktop), z_slope_ice, h_var )

 do 3000 k=ktop, kbot

   if( tzk(k) < t_min .or. p1(k) < p_min ) goto 3000

   tz = tzk(k)
   qv = qvk(k)
   ql = qlk(k)
   qi = qik(k)
   qr = qrk(k)
   qs = qsk(k)
   qg = qgk(k)

!--------------------------------------
! *** Split-micro_physics_processes ***
!--------------------------------------

   pgacr = 0.
   pgacw = 0.

   tc = tz-tice
if ( tc > 0. ) then

!-----------------------------
!* Melting of snow and graupel
!-----------------------------
     dqs0 = ces0/p1(k) - qv

! The following is needed after the terminal fall
     if ( qs>qvmin ) then
! Melting of snow into rain ( half time step )
          factor = min( 1., tc/t_snow_melt )
            sink = min( fac_sno*qs, factor*tc/icpk(k) )
          qs = qs - sink
          qr = qr + sink
          tz = tz - sink*icpk(k)    ! cooling due to snow melting
          tc = tz-tice
     endif

     if( qs>qcmin ) then

! * accretion: cloud water --> snow
! only rate is used (for snow melt) since tc > 0.
        if( ql>qrmin ) then
            factor = denfac(k)*csacw*exp(0.8125*log(qs*den(k)))
             psacw = factor/(1.+dts*factor)*ql     ! rate
        else
             psacw = 0.
        endif

        if ( qr>qrmin ) then
! * accretion: melted snow --> rain:
             psacr = min(acr3d(vts(k), vtr(k), qr, qs, csacr, acco(1,2), den(k)), qr*rdts)
! * accretion: snow --> rain
             pracs = acr3d(vtr(k), vts(k), qs, qr, cracs, acco(1,1), den(k))
        else
             psacr = 0.
             pracs = 0.
        endif

! Total snow sink:
! * Snow melt (due to rain accretion): snow --> rain
        psmlt = max(0., smlt(tc, dqs0, qs*den(k), psacw, psacr, csmlt, den(k), denfac(k)))
         sink = min(qs, dts*(psmlt+pracs), tc/icpk(k))

        qs = qs - sink
        qr = qr + sink
        tz = tz - sink*icpk(k)    ! cooling due to snow melting
        tc = tz-tice
     endif

     if ( qg>qcmin .and. tc>0. ) then

! *Temperature-dependent* Melting of graupel into rain ( half time step )
          factor = min( 1., tc/t_grau_melt )   ! smoother !!
            sink = min( fac_gra*qg, factor*tc/icpk(k) )
          qg = qg - sink
          qr = qr + sink
          tz = tz - sink*icpk(k)    ! cooling due to melting
          tc = tz-tice

         if ( qr>qrmin ) then
! * accretion: rain --> graupel
              pgacr = min(acr3d(vtg(k), vtr(k), qr, qg, cgacr, acco(1,3), den(k)), rdts*qr)
         endif

         qden = qg*den(k)
         if( ql>qrmin ) then
! * accretion: cloud water --> graupel
!            factor = cgacw/sqrt(den(k))*(qg*den(k))**0.875
             factor = cgacw*qden/sqrt(den(k)*sqrt(sqrt(qden)))
              pgacw = factor/(1.+dts*factor) * ql  ! rate
         endif

! * melting: graupel --> rain
         pgmlt = dts*gmlt(tc, dqs0, qden, pgacw, pgacr, cgmlt, den(k))
         pgmlt = min( max(0., pgmlt), qg, tc/icpk(k) )
            qg = qg - pgmlt
            qr = qr + pgmlt
            tz = tz - pgmlt*icpk(k)

     endif   ! graupel existed

elseif( tc < 0.0 ) then

!------------------
! Cloud ice proc:
!------------------
  if ( qi>1.E-8 ) then

!----------------------------------------
! * accretion (pacr): cloud ice --> snow
!----------------------------------------
     if ( qs>1.E-8 )  then
! The following is originally from the "Lin Micro-physics" in Zetac
! SJL added (following Lin Eq. 23) the temperature dependency
! To reduce accretion, use Esi = exp(0.05*tc) as in Hong et al 2004
! To increase ice/reduce snow: exp(0.025*tc)
          factor = dts*denfac(k)*csaci*exp(0.05*tc + 0.8125*log(qs*den(k)))
          psaci = factor/(1.+factor) * qi
     else
          psaci = 0.
     endif

!-------------------------------------
! * autoconversion: cloud ice --> snow
!-------------------------------------
! Similar to LFO 1983: Eq. 21 solved implicitly
! Threshold from WSM6 scheme, Hong et al 2004, Eq(13) : qi0_crt ~0.8E-4
    qim = qi0_crt / den(k)

! Assuming linear subgrid vertical distribution of cloud ice
! The mismatch computation following Lin et al. 1994, MWR
    di(k) = max( di(k), qrmin )
    q_plus = qi + di(k)
    if ( q_plus > (qim+qrmin) ) then
         if ( qim > (qi - di(k)) ) then
              dq = 0.25*(q_plus-qim)**2 / di(k)
         else
              dq = qi - qim
         endif
!        factor = dts*c_psaut*exp(0.025*tc)
!        psaut  = factor/(1.+factor) * dq
! Since mv2.10
         psaut  = fac_i2s * dq
    else
         psaut = 0.
    endif

    sink = min( qi, psaci+psaut )
      qi = qi - sink
      qs = qs + sink

!-----------------------------------
! * accretion: cloud ice --> graupel
!-----------------------------------
    if ( qg>qrmin .and. qi>1.E-7 ) then
!        factor = dts*cgaci/sqrt(den(k))*(qg*den(k))**0.875
!----------------------------------------------------------------------------
! SJL added exp(0.025*tc) efficiency factor
         factor = dts*cgaci/sqrt(den(k))*exp(0.025*tc + 0.875*log(qg*den(k)))
!----------------------------------------------------------------------------
          pgaci = factor/(1.+factor)*qi
             qi = qi - pgaci
             qg = qg + pgaci
    endif

  endif  ! cloud ice existed

!----------------------------------
! * sublimation/deposition of snow:
!----------------------------------
  if ( qs>qrmin ) then
           qsi = iqs2(tz, den(k), dqsdt)
          qden = qs*den(k)
          tmp1 = exp(0.65625*log(qden))
           tsq = tz**2
            dq = (qsi-qv)/(1.+tcpk(k)*dqsdt)
         pssub =  cssub(1)*tsq*(cssub(2)*sqrt(qden) + cssub(3)*tmp1*sqrt(denfac(k)))  &
               / (cssub(4)*tsq+cssub(5)*qsi*den(k))
         pssub = (qsi-qv)*dts*pssub
         if ( pssub > 0. ) then     ! qs --> qv,  sublimation
              pssub = min(pssub*min(1.,dim(tz,t_sub)*0.2), qs)
! Enforce minimum sublimation:
!              sink = min( qs, dim(rh_sno*qsi, qv)/(1.+tcpk(k)*dqsdt) )
!             pssub = max( pssub, sink )
         else
              if ( tz > tice ) then
                   pssub = 0.
              else
                   pssub = max( pssub, 0.05*dq, (tz-tice)/tcpk(k) )
              endif
         endif
         qs = qs - pssub
         qv = qv + pssub
         tz = tz - pssub*tcpk(k)
  endif

!----------------
! Cold-Rain proc:
!----------------
! rain to ice, snow, graupel processes:

  tc = tz-tice

  if ( qr>qrmin .and. tc < 0. ) then

#ifndef NOT_USE_PRACI
! * accretion: accretion of cloud ice by rain to produce snow or graupel
! (LFO: produces snow or graupel; cloud ice sink.. via psacr & pgfr)
! ice --> snow OR graupel (due to falling rain)
! No change to qr and  tz
         if ( qi > 1.E-5 ) then
            factor = dts*denfac(k)*craci*exp(0.95*log(qr*den(k)))
             praci = factor/(1.+factor)*qi
             if ( qr > qr0_crt ) then
                  qg = qg + praci
             else
                  qs = qs + praci
             endif
             qi = qi - praci
         endif
#endif

! *sink* terms to qr: psacr + piacr + pgfr
! source terms to qs: psacr
! source terms to qi: piacr
! source terms to qg: pgfr

! * accretion of rain by snow
      if ( qs > 1.E-8 ) then   ! if snow exists
           psacr = dts*acr3d(vts(k), vtr(k), qr, qs, csacr, acco(1,2), den(k))
      else
           psacr = 0.
      endif

! The following added by SJL (missing from Zetac)
! * piacr: accretion of rain by cloud ice [simplified from lfo 26]
! The value of c_piacr needs to be near order(1) to have significant effect
!-------------------------------------------------------------------
! rain --> ice
      if ( qi > qrmin ) then
         factor = dts*denfac(k)*qi * c_piacr
          piacr = factor/(1.+factor)*qr
      else
          piacr = 0.
      endif

!-------------------------------------------------------------------
! * rain freezing --> graupel
!-----------------------------------------------------------------------------------
     pgfr = dts*cgfr(1)/den(k)*(exp(-cgfr(2)*tc)-1.)*exp( 1.75*log(qr*den(k)) )

!--- Total sink to qr
       sink = psacr + piacr + pgfr
     factor = min( sink, qr, -tc/icpk(k) ) / max( sink, qrmin )

      psacr = factor * psacr
      piacr = factor * piacr
      pgfr  = factor * pgfr

      sink = psacr + piacr + pgfr
        tz = tz + sink*icpk(k)
        qr = qr - sink
        qs = qs + psacr
        qi = qi + piacr
        qg = qg + pgfr
        tc = tz - tice
  endif  ! qr existed

!------------------
! Cloud water sink:
!------------------
  if( ql>qcmin ) then

! * pihom * homogeneous Freezing of cloud water into cloud ice:
! This is the 1st occurance of liquid water freezing in the split MP process
! done here to prevent excessive snow production
      dtmp = t_wfr - tz
      if( dtmp > 0. ) then
        factor = min( 1., dtmp/dt_fr )
          sink = min( ql*factor, dtmp/icpk(k) )
            ql = ql - sink
            qi = qi + sink
            tz = tz + sink*icpk(k)
            tc = tz - tice
      endif

! * super-cooled cloud water --> Snow
   if ( .not. fast_sat_adj ) then
     if( qs>1.E-8 .and. ql>1.E-8 .and. tc<-0.01 ) then
! The following originally from Zetac: PSACW
        factor = dts*denfac(k)*csacw*exp(0.8125*log(qs*den(k)))
        psacw = min( factor/(1.+factor)*ql, -tc/icpk(k) )
        qs = qs + psacw
        ql = ql - psacw
        tz = tz + psacw*icpk(k)
     endif
     endif

  endif  ! (significant) cloud water existed

!--------------------------
! Graupel production terms:
!--------------------------

  if( qs > qrmin ) then
! * accretion: snow --> graupel
      if ( qg > qrmin ) then
           sink = dts*acr3d(vtg(k), vts(k), qs, qg, cgacs, acco(1,4), den(k))
      else
           sink = 0.
      endif

      qsm = qs0_crt / den(k)
      if ( qs > qsm ) then
! * Autoconversion Snow --> graupel
           factor = dts*1.e-3*exp(0.09*(tz-tice))
             sink = sink + factor/(1.+factor)*(qs-qsm)
      endif
      sink = min( qs, sink )
        qs = qs - sink
        qg = qg + sink

  endif   ! snow existed

  if ( qg>qrmin .and. tz < tice0 ) then

! * accretion: cloud water --> graupel
     if( ql>1.E-8 ) then
!        factor = dts*cgacw/sqrt(den(k))*(qg*den(k))**0.875
           qden = qg*den(k)
         factor = dts*cgacw*qden/sqrt(den(k)*sqrt(sqrt(qden)))
          pgacw = factor/(1.+factor)*ql
     else
          pgacw = 0.
     endif

! * accretion: rain --> graupel
     if ( qr>qrmin ) then
          pgacr = min(dts*acr3d(vtg(k), vtr(k), qr, qg, cgacr, acco(1,3), den(k)), qr)
     else
          pgacr = 0.
     endif

       sink = pgacr + pgacw
     factor = min( sink, dim(tice,tz)/icpk(k) ) / max( sink, qrmin )
      pgacr = factor * pgacr
      pgacw = factor * pgacw

     sink = pgacr + pgacw
       tz = tz + sink*icpk(k)
       qg = qg + sink
       qr = qr - pgacr
       ql = ql - pgacw

!------------------------------------------------------------
! * Simplified 2-way grapuel sublimation-deposition mechanism
!------------------------------------------------------------
         qsi = iqs2(tz, den(k), dqsdt)
          dq = qv - qsi
        pgsub = (qv/qsi-1.) * qg
       if ( pgsub > 0. ) then        ! deposition
            pgsub = min( fac_v2g*pgsub, 0.01*dq, dim(tice,tz)/tcpk(k) )
       else                          ! submilation
            pgsub = max( fac_g2v*pgsub, dq/(1.+tcpk(k)*dqsdt) )*min(1.,dim(tz,t_sub)*0.1)
! Minimum sublimation to maintain rh > rh_gra
!            sink = min( qg, dim(rh_gra*qsi, qv)/(1.+tcpk(k)*dqsdt) )
!           pgsub = min( pgsub, -sink)
       endif
       qg = qg + pgsub
       qv = qv - pgsub
       tz = tz + pgsub*tcpk(k)
  endif    ! graupel existed

endif   ! end ice-physics

!!!#ifdef RAIN_RH_MIN
! Minimum Evap of rain in dry environmental air
 if( qr>qcmin) then
      qsw = wqs2(tz, den(k), dqsdt)
     sink = min(qr, dim(rh_rain*qsw, qv)/(1.+lcpk(k)*dqsdt))
       qv = qv + sink
       qr = qr - sink
       tz = tz - sink*lcpk(k)
 endif
!!!#endif

     tzk(k) = tz
     qvk(k) = qv
     qlk(k) = ql
     qik(k) = qi
     qrk(k) = qr
     qsk(k) = qs
     qgk(k) = qg

3000 continue   ! k-loop

 if ( do_subgrid_z ) then

! Except top 2 and bottom 2 layers (4 layers total), using subgrid PPM distribution
! to perform saturation adjustment at 2X the vertical resolution

   kn = kbot - ktop + 1
   km = 2*(kbot-ktop-1)

   p2(1) =  p1(ktop  )
   p2(2) =  p1(ktop+1)
   do k=3,km-3,2
           k1 = ktop+1 + k/2
      p2(k  ) = p1(k1) - 0.25*dp1(k1)
      p2(k+1) = p1(k1) + 0.25*dp1(k1)
   enddo

   if ( mp_debug ) then
     if (k1 /= (kbot-2))  then
         write(*,*) 'FATAL: k1=', k1
         call error_mesg ('LIN_CLD_MICROPHYS:', 'DO_MAP2_SAT', FATAL)
     endif
   endif

   p2(km-1) = p1(kbot-1)
   p2(km  ) = p1(kbot)

   call remap2(ktop, kbot, kn, km, dp1, tzk, tz2, 1)
   call remap2(ktop, kbot, kn, km, dp1, qvk, qv2, 1)
   call remap2(ktop, kbot, kn, km, dp1, qlk, ql2, 1)
   call remap2(ktop, kbot, kn, km, dp1, qik, qi2, 1)

if( rad_snow .or. rad_rain ) then
   call remap2(ktop, kbot, kn, km, dp1, qrk, qr2, 1)
   call remap2(ktop, kbot, kn, km, dp1, qsk, qs2, 1)
else
   qr2(:) = 1.E30
   qs2(:) = 1.E30
endif

   do k=1,km
      den2(k) = p2(k)/(rdgas*tz2(k))
       qa2(k) = 0.
   enddo

   qg2(:) = 0.  ! need to fix this hack
   call subgrid_z_proc(1, km, p2, den2, dts, rh_adj, tz2, qv2, ql2, qr2, qi2, qs2, qg2, qa2, h_var)

! Remap back to original larger volumes:
   qak(ktop  ) = qak(ktop  ) + qa2(1)
   qak(ktop+1) = qak(ktop+1) + qa2(2)

   tzk(ktop  ) = tz2(1)
   tzk(ktop+1) = tz2(2)

   qvk(ktop  ) = qv2(1)
   qvk(ktop+1) = qv2(2)

   qlk(ktop  ) = ql2(1)
   qlk(ktop+1) = ql2(2)

   qik(ktop  ) = qi2(1)
   qik(ktop+1) = qi2(2)

if( rad_snow .or. rad_rain ) then
   qrk(ktop  ) = qr2(1)
   qrk(ktop+1) = qr2(2)

   qsk(ktop  ) = qs2(1)
   qsk(ktop+1) = qs2(2)
endif

   do k=3,km-3,2
          k1  = ktop+1 + k/2
      qak(k1) = qak(k1) + max(qa2(k), qa2(k+1))  ! Maximum only
! Subgrid overlap schemes: max and random parts weighted by subgrid horizontal deviation
!-------------------------------------------------------------------------------------
! Random cloud fraction = 1 - (1-a1)*(1-a2) = a1 + a2 - a1*a2
! RAND_CLOUD
!     qak(k1) = qak(k1) + (1.-h_var)*max(qa2(k), qa2(k+1))     &  ! Maximum fraction
!                       + h_var*(qa2(k)+qa2(k+1)-qa2(k)*qa2(k+1)) ! Random  fraction
!-------------------------------------------------------------------------------------
      tzk(k1) = 0.5*(tz2(k) + tz2(k+1))
      qvk(k1) = 0.5*(qv2(k) + qv2(k+1))
      qlk(k1) = 0.5*(ql2(k) + ql2(k+1))
      qik(k1) = 0.5*(qi2(k) + qi2(k+1))
   enddo

   qak(kbot-1) = qak(kbot-1) + qa2(km-1)
   qak(kbot  ) = qak(kbot  ) + qa2(km  )

   tzk(kbot-1) = tz2(km-1)
   tzk(kbot  ) = tz2(km  )

   qvk(kbot-1) = qv2(km-1)
   qvk(kbot  ) = qv2(km  )

   qlk(kbot-1) = ql2(km-1)
   qlk(kbot  ) = ql2(km  )

   qik(kbot-1) = qi2(km-1)
   qik(kbot  ) = qi2(km  )

 else
   call subgrid_z_proc(ktop, kbot, p1, den, dts, rh_adj, tzk, qvk, qlk, qrk, qik, qsk, qgk, qak, h_var)
 endif

 end subroutine icloud


 subroutine remap2(ktop, kbot, kn, km, dp, q1, q2, id)
 integer, intent(in):: ktop, kbot, kn, km , id
! constant distribution if id ==0
 real, intent(in), dimension(ktop:kbot):: q1, dp
 real, intent(out):: q2(km)
! local
 real:: a4(4,ktop:kbot)
 real:: tmp
 integer:: k, k1

  q2(1) = q1(ktop  )
  q2(2) = q1(ktop+1)

  if ( id==1 ) then

      do k=ktop,kbot
         a4(1,k) = q1(k)
      enddo
      call cs_profile( a4(1,ktop), dp(ktop), kn, mono_prof )  ! non-monotonic

      do k=3,km-3,2
              k1 = ktop+1 + k/2
         q2(k  ) = min( 2.*q1(k1), max( qvmin, a4(1,k1) + 0.25*(a4(2,k1)-a4(3,k1)) ) )
         q2(k+1) = 2.*q1(k1) - q2(k)
      enddo

  else
      do k=3,km-3,2
              k1 = ktop+1 + k/2
         q2(k  ) = q1(k1)
         q2(k+1) = q1(k1)
      enddo
  endif

  q2(km-1) = q1(kbot-1)
  q2(km  ) = q1(kbot)

 end subroutine remap2



 subroutine subgrid_z_proc(ktop, kbot, p1, den, dts, rh_adj, tz, qv, ql, qr, qi, qs, qg, qa, h_var)

! Temperature sentive high vertical resolution processes:

 integer, intent(in):: ktop, kbot
 real, intent(in),    dimension(ktop:kbot):: p1, den
 real, intent(in)                         :: dts, rh_adj, h_var
 real, intent(inout), dimension(ktop:kbot):: tz, qv, ql, qr, qi, qs, qg, qa
! local:
 real, dimension(ktop:kbot):: lcpk, icpk, tcpk, tcp3
 real:: fac_v2l, fac_l2v
 real:: pidep, qi_crt
! qstar over water may be accurate only down to -80 C with ~10% uncertainty
! must not be too large to allow PSC
 real:: rh, clouds,  rqi, tin, qsw, qsi, qpz, qstar
 real:: dqsdt, dwsdt, dq, dq0, cond0, factor, tmp, cvm
 real:: q_plus, q_minus, dt_pisub
 real:: evap, sink, tc, pisub, iwt, q_adj, dtmp, lat2
 integer :: k

 fac_v2l = 1. - exp( -0.5*dts/tau_v2l )        !
 fac_l2v = 1. - exp( -0.5*dts/tau_l2v )        !

 lat2 = lats * lats

 do k=ktop, kbot
! Moist heat capacity
    cvm = c_air + qv(k)*c_vap + (ql(k)+qr(k))*c_liq + (qi(k)+qs(k)+qg(k))*c_ice
    lcpk(k) = (lv00+dc_vap*tz(k)) / cvm
    icpk(k) = (li00+dc_ice*tz(k)) / cvm
      tcpk(k) = lcpk(k) + icpk(k)
   tcp3(k) = lcpk(k) + icpk(k)*min(1., dim(tice,tz(k))/(tice-t_wfr))
 enddo

 do 4000 k=ktop,kbot

   if ( p1(k) < p_min ) goto 4000

   if ( tz(k) < t_min ) then
        sink = dim(qv(k), 1.e-7)
        qv(k) = qv(k) - sink
        qi(k) = qi(k) + sink
        tz(k) = tz(k) + sink*tcpk(k)
        if ( .not.do_qa ) qa(k) = qa(k) + 1.  ! Air fully saturated; 100 % cloud cover
        goto 4000
    endif

! Instant evaporation/sublimation of all clouds if RH<rh_adj --> cloud free
      iwt = qi(k)
   clouds = ql(k) + iwt

   tin = tz(k) - ( lcpk(k)*clouds + icpk(k)*iwt )  ! minimum  temperature
   if ( tin>t_sub+6. ) then
   qpz = qv(k) + clouds
       rh = qpz / iqs1(tin, den(k))

    if ( rh < rh_adj ) then  ! qpz / rh_adj < qs
         tz(k) = tin
         qv(k) = qpz
         ql(k) = 0.
         qi(k) = 0.
         goto 4000            ! cloud free
    endif
    endif

! cloud water <--> vapor adjustment:
   qsw = wqs2(tz(k), den(k), dwsdt)
   dq0 = qsw - qv(k)
   if ( dq0 > 0. ) then
        evap =  min( ql(k), fac_l2v*dq0/(1.+tcp3(k)*dwsdt) )
   else   ! condensate all excess vapor into cloud water
          ! this should take care of most of the super-sat due to phys.
        evap = dq0/(1.+tcp3(k)*dwsdt)
   endif
   qv(k) = qv(k) + evap
   ql(k) = ql(k) - evap
   tz(k) = tz(k) - evap*lcpk(k)

! Enforce complete freezing below -48 C
   dtmp = t_wfr - tz(k)   ! [-40,-48]
   if( dtmp>0.  .and. ql(k) > qcmin ) then
       sink = min( ql(k),  ql(k)*dtmp*0.125, dtmp/icpk(k) )
      ql(k) = ql(k) - sink
      qi(k) = qi(k) + sink
      tz(k) = tz(k) + sink*icpk(k)
   endif

  if ( fast_sat_adj ) then
      dt_pisub = 0.5*dts
  else
      dt_pisub = dts
      tc = tice - tz(k)
      if( ql(k) > qcmin .and. tc> 0.1 ) then
! Bigg mechanism
         sink = dts*3.3333e-10*(exp(0.66*tc)-1.)*den(k)*ql(k)*ql(k)
         sink = min(ql(k), tc/icpk(k), sink)
         ql(k) = ql(k) - sink
         qi(k) = qi(k) + sink
         tz(k) = tz(k) + sink*icpk(k)
      endif ! significant ql existed
  endif

   cvm = c_air + qv(k)*c_vap + (ql(k)+qr(k))*c_liq + (qi(k)+qs(k)+qg(k))*c_ice
   lcpk(k) = (lv00+dc_vap*tz(k)) / cvm
   icpk(k) = (li00+dc_ice*tz(k)) / cvm
     tcpk(k) = lcpk(k) + icpk(k)
!------------------------------------------
! * pidep: sublimation/deposition of ice:
!------------------------------------------
    if ( tz(k) < tice ) then
         qsi = iqs2(tz(k), den(k), dqsdt)
          dq = qv(k) - qsi
        sink = dq/(1.+tcpk(k)*dqsdt)

        if ( qi(k) > qrmin ) then
! Eq 9, Hong et al. 2004, MWR
! For A and B, see Dudhia 1989: page 3103 Eq (B7) and (B8)
             pidep = dt_pisub*dq*349138.78*exp(0.875*log(qi(k)*den(k)))     &
               / (qsi*den(k)*lat2/(0.0243*rvgas*tz(k)**2) + 4.42478e4)
        else
             pidep = 0.
        endif

        if ( dq > 0. ) then   ! vapor -> ice
             tmp = tice - tz(k)
             qi_crt = qi_gen*min(qi_lim, 0.1*tmp) / den(k)
             sink = min(sink, max(qi_crt-qi(k),pidep), tmp/tcpk(k))
        else  ! ice --> vapor
             pidep = pidep * min(1., dim(tz(k),t_sub)*0.2)
             sink = max(pidep, sink, -qi(k))
        endif
        qv(k) = qv(k) - sink
        qi(k) = qi(k) + sink
        tz(k) = tz(k) + sink*tcpk(k)
    endif

   if ( do_qa ) goto 4000

   if ( rad_snow ) then
        iwt = qi(k) + qs(k)
   else
        iwt = qi(k)
   endif
   if ( rad_rain ) then
        clouds = ql(k) + qr(k) + iwt
   else
        clouds = ql(k) + iwt
   endif

   qpz = qv(k) + clouds     ! qpz is conserved
   tin = tz(k) - ( lcpk(k)*clouds + icpk(k)*iwt )  ! minimum  temperature

!--------------------
! * determine qstar
!--------------------
! Using the "liquid-frozen water temperature": tin
   if( tin <= t_wfr ) then
       qstar = iqs1(tin, den(k))
   elseif ( tin >= tice ) then
       qstar = wqs1(tin, den(k))
   else
! mixed phase:
       qsi = iqs1(tin, den(k))
       qsw = wqs1(tin, den(k))
       if( clouds > 3.E-6 ) then
           rqi = iwt / clouds
       else
! Mostly liquid water clouds at initial cloud development stage
           rqi = (tice-tin)/(tice-t_wfr)
       endif
       qstar = rqi*qsi + (1.-rqi)*qsw
   endif

!-------------------------
! * cloud fraction
!-------------------------
! Assuming subgrid linear distribution in horizontal; this is effectively a smoother for the
! binary cloud scheme

   if ( qpz > qrmin ) then
! Partial cloudiness by PDF:
            dq = max(qcmin, h_var*qpz)
       q_plus  = qpz + dq        ! cloud free if qstar > q_plus
       q_minus = qpz - dq
       if ( qstar < q_minus ) then
            qa(k) = qa(k) + 1.       ! Air fully saturated; 100 % cloud cover
       elseif ( qstar<q_plus .and. clouds>qc_crt ) then
            qa(k) = qa(k) + (q_plus-qstar)/(dq+dq)       ! partial cloud cover
       endif
   endif

4000 continue

 end subroutine subgrid_z_proc


 subroutine sat_adj2(mdt, is, ie, js, je, ng, hydrostatic, consv_te, &
                     te0, qv, ql, qi, qr, qs, qg, qa, area, dpeln, delz, pt, dp,  &
                     q_con, &
#ifdef MOIST_CAPPA
					 cappa, &
#endif
					 dtdt, out_dt, last_step)
! This is designed for 6-class micro-physics schemes
! input pt is T_vir
 real, intent(in):: mdt ! remapping time step
 integer, intent(in):: is, ie, js, je, ng
 logical, intent(in):: hydrostatic, last_step, consv_te, out_dt
 real, intent(in), dimension(is-ng:ie+ng,js-ng:je+ng):: dp, area, delz

 real, intent(in):: dpeln(is:ie,js:je)           ! ln(pe)
 real, intent(inout), dimension(is-ng:ie+ng,js-ng:je+ng):: pt, qv, ql, qi, qr, qs, qg, qa
 real, intent(inout), dimension(is-ng:,js-ng:):: q_con
#ifdef MOIST_CAPPA
 real, intent(inout), dimension(is-ng:,js-ng:):: cappa
#endif
 real, intent(inout)::dtdt(is:ie,js:je)
 real, intent(out):: te0(is:ie,js:je)
! Local:
 real, dimension(is:ie):: cpm, t0, pt1, icp2, lcp2, tcp2, tcp3, den, q_liq, q_sol, hvar, src, p1
 real:: sink, qsw, rh, fac_v2l, fac_l2v, cond0
 real:: tc, qsi, dqsdt, dq, dq0, pidep, qi_crt, tmp, dtmp, lat2
 real:: condensates, qpz, tin, qstar, rqi, q_plus, q_minus
 real:: sdt, rh_ql, adj_fac, fac_s, fac_r, fac_i2s
 real:: factor, psacw, qim
 integer i,j

 sdt = 0.5 * mdt

 fac_i2s = 1. - exp( -mdt/tau_i2s )        !
 fac_v2l = 1. - exp( -sdt/tau_v2l )
 fac_l2v = 1. - exp( -sdt/tau_l2v )        !
 fac_l2v = min(sat_adj0, 0.5, fac_l2v)
 fac_r = 1. - exp( -sdt/tau_r )
 fac_s = 1. - exp( -sdt/tau_s )

  latv = hlv
  lati = hlf
  lats = latv + lati
  lat2 = lats * lats

 do j=js, je

    do i=is, ie
#ifdef USE_COND
       q_con(i,j) = ql(i,j) + qr(i,j) + qi(i,j) + qs(i,j) + qg(i,j)
       pt1(i) = pt(i,j) / ((1.+zvir*qv(i,j))*(1-q_con(i,j)))
#else
       pt1(i) = pt(i,j) / (1.+zvir*qv(i,j))
#endif
        t0(i) = pt1(i)
       q_liq(i) = ql(i,j) + qr(i,j)
       q_sol(i) = qi(i,j) + qs(i,j) + qg(i,j)
        hvar(i) = min(0.2, max(0.01, dw_ocean*sqrt(sqrt(area(i,j)/1.E10))) )
    enddo
! Perform terminal fall & momentum transport???

    if ( hydrostatic ) then
        do i=is, ie
! Need to fix the following for consistency:
           den(i) = dp(i,j)/(dpeln(i,j)*rdgas*pt(i,j))
           cpm(i) = (1.-(qv(i,j)+q_liq(i)+q_sol(i)))*cp_air + qv(i,j)*cp_vap +    &
                     q_liq(i)*c_liq + q_sol(i)*c_ice
           lcp2(i) = (Lv0 +dc_vap*t0(i)) / cpm(i)
           icp2(i) = (li00+dc_ice*t0(i)) / cpm(i)
        enddo
    else
        do i=is, ie
! Moist mixing ratios:
           den(i) = -dp(i,j)/(grav*delz(i,j))
           cpm(i) = (1.-(qv(i,j)+q_liq(i)+q_sol(i)))*cv_air + qv(i,j)*cv_vap +    &
                     q_liq(i)*c_liq + q_sol(i)*c_ice
           lcp2(i) = (Lv0+dc_vap*t0(i)) / cpm(i)
           icp2(i) = (li00+dc_ice*t0(i)) / cpm(i)
        enddo
    endif

    do i=is, ie
       tcp2(i) = lcp2(i) + icp2(i)
! Compute special heat capacity for qv --> ql (dqsdt term)
       tcp3(i) = lcp2(i) + icp2(i)*min(1., dim(tice,t0(i))/40.)
       src(i) = 0.
    enddo
    if ( consv_te ) then 
       if ( hydrostatic ) then
         do i=is, ie
            te0(i,j) = -cp_air*dp(i,j)*t0(i)*(1.+zvir*qv(i,j))
         enddo
       else
         do i=is, ie
#if defined(USE_COND) && defined(MOIST_CAPPA)
            te0(i,j) = -cpm(i)*dp(i,j)*t0(i)
#else
            te0(i,j) = -cv_air*dp(i,j)*t0(i)
#endif
         enddo
       endif
    endif

!********************
! Fast moist physics:
!********************
    do i=is, ie
       if( qi(i,j) < 0. ) then
! Fix negative cloud ice with snow
           qs(i,j) = qs(i,j) + qi(i,j)
           qi(i,j) = 0.
       elseif ( qi(i,j)>qcmin .and. pt1(i) > tice ) then
! Melting of cloud ice into cloud water ********
! Cooling via melting of cloud ice
           sink = min( qi(i,j), (pt1(i)-tice)/icp2(i) )
! Maximum amount of melted ice converted to ql
            tmp = min( sink, dim(ql_mlt, ql(i,j)) )   ! max ql amount
           ql(i,j) = ql(i,j) + tmp
           qr(i,j) = qr(i,j) + sink - tmp
           qi(i,j) = qi(i,j) - sink
            pt1(i) =  pt1(i) - sink*icp2(i)
       endif
! -----------------------
! Melting of snow --> qr
! -----------------------
       dtmp = pt1(i) - (tice + 1.)
       if ( qs(i,j)>qcmin .and. dtmp>0. ) then
            tmp = min( 1., fac_s*(dtmp*0.2)**2 ) * qs(i,j)
           sink = min( tmp, dtmp/icp2(i) )
           qr(i,j) = qr(i,j) + sink
           qs(i,j) = qs(i,j) - sink
            pt1(i) =  pt1(i) - sink*icp2(i)
       endif
    enddo

    do i=is, ie
       if( qs(i,j) < 0. ) then
! Fix negative snow with graupel
           qg(i,j) = qg(i,j) + qs(i,j)
           qs(i,j) = 0.
       elseif( qg(i,j) < 0. ) then
! Fix negative graupel with available snow
           tmp = min( -qg(i,j), max(0., qs(i,j)) )
           qg(i,j) = qg(i,j) + tmp
           qs(i,j) = qs(i,j) - tmp
       endif
    enddo
! At this point cloud ice & snow are positive definite

    do i=is, ie
       if( ql(i,j) < 0. ) then
! Fix negative cloud water if rain exists
           tmp = min( -ql(i,j), max(0., qr(i,j)) )
           ql(i,j) = ql(i,j) + tmp
           qr(i,j) = qr(i,j) - tmp
       elseif( qr(i,j) < 0. ) then
! Fix negative rain water with available cloud water
           tmp = min( -qr(i,j), max(0., ql(i,j)) )
           ql(i,j) = ql(i,j) - tmp
           qr(i,j) = qr(i,j) + tmp
       endif
    enddo

! Enforce complete freezing below -48 C
! (do this before ql has a chance to evap)
    do i=is, ie
       dtmp = tice - 48. - pt1(i)
       if( ql(i,j)>0. .and. dtmp > 0. ) then
           sink = min( ql(i,j),  dtmp/icp2(i) )
           ql(i,j) = ql(i,j) - sink
           qi(i,j) = qi(i,j) + sink
            pt1(i) =  pt1(i) + sink*icp2(i)
       endif
    enddo
 if ( last_step ) then
! Enforce upper (no super_sat) & lower (critical RH) bounds
      do i=is, ie
         qsw = wqs2(pt1(i), den(i), dqsdt)
         dq0 = qv(i,j) - qsw
         if ( dq0 > 0. ) then ! remove super-saturation
! Prevent super saturation over water:
             src(i) = dq0/(1.+tcp3(i)*dqsdt)
         else
! Evaporation of ql
            src(i) = -min( ql(i,j), -fac_l2v*dq0/(1.+tcp3(i)*dqsdt) )
         endif
      enddo
      adj_fac = 1.
 else
      adj_fac = sat_adj0
      do i=is, ie
         qsw = wqs2(pt1(i), den(i), dqsdt)
         dq0 = qv(i,j) - qsw
         if ( dq0 > 0. ) then ! whole grid-box saturated
               tmp = dq0/(1.+tcp3(i)*dqsdt)
            src(i) = min(adj_fac*tmp, max(ql_gen-ql(i,j), fac_v2l*tmp))

         else
! Evaporation of ql
            src(i) = -min( ql(i,j), -fac_l2v*dq0/(1.+tcp3(i)*dqsdt) )
         endif
      enddo
 endif

    do i=is, ie
       qv(i,j) = qv(i,j) - src(i)
       ql(i,j) = ql(i,j) + src(i)
        pt1(i) =  pt1(i) + src(i)*lcp2(i)
    enddo

! *********** freezing of cloud water ********
! Enforce complete freezing below -48 C
    do i=is, ie
       dtmp = t_wfr - pt1(i)   ! [-40,-48]
       if( ql(i,j)>0. .and. dtmp > 0. ) then
           sink = min( ql(i,j),  ql(i,j)*dtmp*0.125, dtmp/icp2(i) )
           ql(i,j) = ql(i,j) - sink
           qi(i,j) = qi(i,j) + sink
            pt1(i) =  pt1(i) + sink*icp2(i)
       endif
    enddo
    do i=is, ie
       tc = tice0 - pt1(i)
       if( ql(i,j)>qcmin .and. tc > 0. ) then
! Bigg mechanism
           sink = mdt*3.3333e-10*(exp(0.66*tc)-1.)*den(i)*ql(i,j)*ql(i,j)
           sink = min(ql(i,j), tc/icp2(i), sink)
           ql(i,j) = ql(i,j) - sink
           qi(i,j) = qi(i,j) + sink
            pt1(i) =  pt1(i) + sink*icp2(i)
       endif
    enddo
! *********** freezing of rain water qr-->qg ********
    do i=is, ie
       dtmp = (tice - 1.) - pt1(i)
       if( qr(i,j)>qcmin .and. dtmp > 0. ) then
            tmp = min( 1., fac_r*(dtmp*0.025)**2 ) * qr(i,j)
           sink = min( tmp,  dtmp/icp2(i) )
           qr(i,j) = qr(i,j) - sink
           qg(i,j) = qg(i,j) + sink
            pt1(i) =  pt1(i) + sink*icp2(i)
       endif
    enddo

! Update after freezing & before ice-phase adjustment
    do i=is, ie
       q_liq(i) = max(0., ql(i,j) + qr(i,j))
       q_sol(i) = max(0., qi(i,j) + qs(i,j) + qg(i,j))
    enddo

! Enforce upper bounds on ql (can be regarded as autoconversion)
    do i=is, ie
       if ( ql(i,j) > ql0_max ) then
            qr(i,j) = q_liq(i) - ql0_max
            ql(i,j) = ql0_max
       endif
    enddo

    if ( hydrostatic ) then
        do i=is, ie
           cpm(i) = (1.-(qv(i,j)+q_liq(i)+q_sol(i)))*cp_air + qv(i,j)*cp_vap +    &
                     q_liq(i)*c_liq + q_sol(i)*c_ice
           lcp2(i) = (Lv0+ dc_vap*pt1(i)) / cpm(i)
           icp2(i) = (li00+dc_ice*pt1(i)) / cpm(i)
        enddo
    else
        do i=is, ie
           cpm(i) = (1.-(qv(i,j)+q_liq(i)+q_sol(i)))*cv_air + qv(i,j)*cv_vap +    &
                     q_liq(i)*c_liq + q_sol(i)*c_ice
           lcp2(i) = (Lv0+dc_vap*pt1(i)) / cpm(i)
           icp2(i) = (li00+dc_ice*pt1(i)) / cpm(i)
        enddo
    endif

    do i=is, ie
       tcp2(i) = lcp2(i) + icp2(i)
       src(i) = 0.
    enddo
 
! Ice-phase
!------------------------------------------
! * pidep: sublimation/deposition of ice:
!------------------------------------------
    do i=is, ie
       p1(i) = dp(i,j)/dpeln(i,j)
       if ( p1(i) > p_min ) then
       if ( pt1(i) < t_min ) then  ! Too cold to be accurate; freeze qv as a fix
            src(i) = dim(qv(i,j), 1.e-7 )
       elseif ( pt1(i) < tice0 ) then
          qsi = iqs2(pt1(i), den(i), dqsdt)
           dq = qv(i,j) - qsi
         sink = adj_fac*dq/(1.+tcp2(i)*dqsdt)
         if ( qi(i,j) > qrmin ) then
! Eq 9, Hong et al. 2004, MWR; For A and B, see Dudhia 1989: page 3103 Eq (B7)-(B8)
              pidep = sdt*dq*349138.78*exp(0.875*log(qi(i,j)*den(i)))  &
               / (qsi*den(i)*lat2/(0.0243*rvgas*pt1(i)**2) + 4.42478e4)
         else
              pidep = 0.
         endif
         if ( dq > 0. ) then   ! vapor -> ice
             tmp = tice - pt1(i)
             qi_crt = qi_gen*min(qi_lim, 0.1*tmp) / den(i)
             src(i) = min(sink, max(qi_crt-qi(i,j),pidep), tmp/tcp2(i))
         else
             pidep = pidep * min(1., dim(pt1(i),t_sub)*0.2)
             src(i) = max(pidep, sink, -qi(i,j))
         endif
         endif
       endif
    enddo
    do i=is, ie
       qv(i,j) = qv(i,j) - src(i)
       qi(i,j) = qi(i,j) + src(i)
        pt1(i) =  pt1(i) + src(i)*tcp2(i)
    enddo

! Enforce upper bounds on qi (autoconversion to qs)
    do i=is, ie
       qim = qi0_crt / den(i)
       if ( qi(i,j) > qim ) then
            sink = fac_i2s*(qi(i,j)-qim)
            qi(i,j) = qi(i,j) - sink
            qs(i,j) = qs(i,j) + sink
       endif
    enddo

!--- TESTING the ql-->qs process
    do i=is, ie
       tc = pt1(i) - tice 
       if( tc<-1.0 .and. qs(i,j)>1.E-7 .and. ql(i,j)>1.E-7 ) then
         factor = mdt*sqrt(sfcrho/den(i))*csacw*exp(0.8125*log(qs(i,j)*den(i)))
         psacw = min( factor/(1.+factor)*ql(i,j), -tc/icp2(i) )
         qs(i,j) = qs(i,j) + psacw
         ql(i,j) = ql(i,j) - psacw
         pt1(i) = pt1(i) + psacw*icp2(i)
        endif
    enddo
!--- TESTING
    do i=is, ie
       if( qg(i,j) < 0. ) then
! Fix negative graupel with available ice
           tmp = min( -qg(i,j), max(0., qi(i,j)) )
           qg(i,j) = qg(i,j) + tmp
           qi(i,j) = qi(i,j) - tmp
       endif
    enddo
! At this point cloud ice & snow are positive definite

! rain re-evap
    call revap_rac1(hydrostatic, is, ie, sdt, pt1, qv(is,j), ql(is,j), qr(is,j), qi(is,j), qs(is,j), qg(is,j), den, hvar)

! Virtual temp updated !!!
    do i=is, ie
#ifdef USE_COND
       q_liq(i) = ql(i,j) + qr(i,j)
       q_sol(i) = qi(i,j) + qs(i,j) + qg(i,j)
       q_con(i,j) = q_liq(i) + q_sol(i)
       pt(i,j) = pt1(i)*(1.+zvir*qv(i,j))*(1.-q_con(i,j))
#ifdef MOIST_CAPPA
       cpm(i) = (1.-(qv(i,j)+q_con(i,j)))*cv_air + qv(i,j)*cv_vap + q_liq(i)*c_liq + q_sol(i)*c_ice
       cappa(i,j) = rdgas/(rdgas + cpm(i)/(1.+zvir*qv(i,j)))
#endif
#else
       pt(i,j) = pt1(i)*(1.+zvir*qv(i,j))
#endif
    enddo

    if ( out_dt ) then
         do i=is, ie
            dtdt(i,j) = dtdt(i,j) + pt1(i) - t0(i)
         enddo
    endif

    if ( consv_te ) then 
       if ( hydrostatic ) then
         do i=is, ie
            te0(i,j) = te0(i,j) + cp_air*dp(i,j)*pt1(i)*(1.+zvir*qv(i,j))
         enddo
       else
         do i=is, ie
#if defined(USE_COND) && defined(MOIST_CAPPA)
            te0(i,j) = te0(i,j) + cpm(i)*dp(i,j)*pt1(i)
#else
            te0(i,j) = te0(i,j) + cv_air*dp(i,j)*pt1(i)
#endif
         enddo
       endif
    endif

if ( do_qa .and. last_step ) then

    do i=is, ie
       qa(i,j) = 0.
    enddo

    if ( rad_snow ) then
      if ( rad_graupel ) then
       do i=is, ie
          q_sol(i) = qi(i,j) + qs(i,j) + qg(i,j)
       enddo
      else
       do i=is, ie
          q_sol(i) = qi(i,j) + qs(i,j)
       enddo
      endif
    else
       do i=is, ie
          q_sol(i) = qi(i,j)
       enddo
    endif

    do i=is, ie
    if ( p1(i) > p_min ) then
       if ( rad_rain) then
            condensates = q_sol(i) + ql(i,j) + qr(i,j)
       else
            condensates = q_sol(i) + ql(i,j)
       endif
       qpz = qv(i,j) + condensates
! Using the "liquid-frozen water temperature": tin
       tin = pt1(i) - ( lcp2(i)*condensates + icp2(i)*q_sol(i) )  ! minimum  temperature
       if( tin <= t_wfr ) then
           qstar = iqs1(tin, den(i))
       elseif ( tin > tice ) then
           qstar = wqs1(tin, den(i))
       else
! mixed phase:
           qsi = iqs1(tin, den(i))
           qsw = wqs1(tin, den(i))
           if( condensates > 3.E-6 ) then
               rqi = q_sol(i) / condensates
           else
! Mostly liquid water clouds at initial cloud development stage
               rqi = (tice-tin)/(tice-t_wfr)
           endif
           qstar = rqi*qsi + (1.-rqi)*qsw
       endif

! Partial cloudiness by PDF:
! Assuming subgrid linear distribution in horizontal; this is effectively a smoother for the
! binary cloud scheme;  qa=0.5 if qstar==qpz
 
      rh = qpz / qstar
      if ( rh > 0.75 .and. qpz > qrmin ) then
                dq = hvar(i)*qpz
           q_plus  = qpz + dq
           q_minus = qpz - dq
           if ( qstar < q_minus ) then
                qa(i,j) = 1.
           else
             if ( qstar<q_plus ) then
                qa(i,j) = (q_plus-qstar)/(dq+dq)       ! partial cloud cover
             endif
! Impose minimum cloudiness if substantial condensates exist
             if ( condensates > 1.0e-6 ) qa(i,j) = max(cld_min, qa(i,j))
           endif
           endif
      endif
   enddo
endif

 enddo

 end subroutine sat_adj2


 subroutine revap_rac1(hydrostatic, is, ie, dt, tz, qv, ql, qr, qi, qs, qg, den, hvar)
 logical, intent(in):: hydrostatic
 integer, intent(in):: is, ie
 real, intent(in):: dt       ! time step (s)
 real, intent(in),    dimension(is:ie):: den, hvar, qi, qs, qg
 real, intent(inout), dimension(is:ie):: tz, qv, qr, ql
! local:
 real, dimension(is:ie):: lcp2, denfac
 real:: dqv, qsat, dqsdt, evap, qden, q_plus, q_minus, sink
 real:: tin, t2, qpz, dq, dqh, q_liq, q_sol
 integer i

  if ( hydrostatic ) then
  do i=is, ie
     q_liq = ql(i) + qr(i)
     q_sol = qi(i) + qs(i) + qg(i)
     lcp2(i) = (Lv0+dc_vap*tz(i))/((1.-(qv(i)+q_liq+q_sol))*cp_air+qv(i)*cp_vap+q_liq*c_liq+q_sol*c_ice)
!    denfac(i) = sqrt(sfcrho/den(i))
  enddo
  else
  do i=is, ie
     q_liq = ql(i) + qr(i)
     q_sol = qi(i) + qs(i) + qg(i)
     lcp2(i) = (Lv0+dc_vap*tz(i))/((1.-(qv(i)+q_liq+q_sol))*cv_air+qv(i)*cv_vap+q_liq*c_liq+q_sol*c_ice)
!    denfac(i) = sqrt(sfcrho/den(i))
  enddo
  endif

  do i=is, ie
     if ( qr(i) > qrmin .and. tz(i) > t_wfr ) then
          qpz = qv(i) + ql(i)
          tin = tz(i) - lcp2(i)*ql(i) ! presence of clouds suppresses the rain evap
         qsat = wqs2(tin, den(i), dqsdt)
          dqh = max(ql(i), hvar(i)*max(qpz, qcmin))
          dqv = qsat - qv(i)
         q_minus = qpz - dqh
         q_plus  = qpz + dqh

! qsat must be > q_minus to activate evaporation
! qsat must be < q_plus  to activate accretion

!-------------------
! * Rain evaporation
!-------------------
         if ( dqv > qvmin  .and. qsat > q_minus ) then
              if ( qsat > q_plus ) then
                   dq = qsat - qpz
              else
! q_minus < qsat < q_plus
! dq == dqh if qsat == q_minus
                  dq = 0.25*(q_minus-qsat)**2 / dqh
              endif
              qden = qr(i)*den(i)
              t2 = tin * tin
              evap =  crevp(1)*t2*dq*(crevp(2)*sqrt(qden)+crevp(3)*exp(0.725*log(qden)))   &
                   / (crevp(4)*t2  +  crevp(5)*qsat*den(i))
              evap = min( qr(i), dt*evap, dqv/(1.+lcp2(i)*dqsdt) )
             qr(i) = qr(i) - evap
             qv(i) = qv(i) + evap
             tz(i) = tz(i) - evap*lcp2(i)
         endif

         if ( qr(i)>qrmin .and. ql(i)>1.E-8 .and. qsat<q_plus ) then
!-------------------
! * Accretion: pracc
!-------------------
               denfac(i) = sqrt(sfcrho/den(i))
               sink = dt*denfac(i)*cracw*exp(0.95*log(qr(i)*den(i)))
               sink = sink/(1.+sink)*ql(i)
              ql(i) = ql(i) - sink
              qr(i) = qr(i) + sink
         endif
     endif
  enddo

 end subroutine revap_rac1

 subroutine terminal_fall(dtm, ktop, kbot, tz, qv, ql, qr, qg, qs, qi, pm, dz, dp,  &
                          den, vtg, vts, vti, fac_sno, fac_gra, r1, g1, s1, i1, m1_sol, w1)

! lagrangian control-volume method:

 real,    intent(in):: dtm                    ! time step (s)
 integer, intent(in):: ktop, kbot
 real,    intent(in):: fac_sno, fac_gra
 real,    intent(in), dimension(ktop:kbot):: vtg, vts, vti, pm, den, dp, dz
 real,    intent(inout), dimension(ktop:kbot):: qv, ql, qr, qg, qs, qi, tz, m1_sol, w1
 real,    intent(out):: r1, g1, s1, i1
! local:
 real, dimension(ktop:kbot+1):: ze, zt
 real:: qsat, dqsdt, dt5, melt, evap, dtime
 real:: factor, frac
 real:: tmp1, precip, tc, sink
 real, dimension(ktop:kbot):: lcpk, icpk
 real, dimension(ktop:kbot):: m1, dm
 real:: zs = 0.
 real:: cvm
 integer k, k0, m
 logical no_fall

 
  do k=ktop,kbot
     m1_sol(k) = 0.
     cvm = c_air + qv(k)*c_vap + (qr(k)+ql(k))*c_liq + (qi(k)+qs(k)+qg(k))*c_ice
     lcpk(k) = (lv00+dc_vap*tz(k)) / cvm
     icpk(k) = (li00+dc_ice*tz(k)) / cvm
  enddo

  dt5 = 0.5*dtm


! Melting of cloud_ice and snow (before fall):

! find significant melting level
  k0 = kbot
  do k=ktop, kbot-1
     if ( tz(k) > tice ) then
          k0 = k
          go to 11
     endif
  enddo
11  continue

 do k=k0, kbot
!------
! * ice
!------
    tc = tz(k) - tice
    if( qi(k) > qcmin .and. tc>0. ) then
        melt = min( qi(k), tc/icpk(k) )
        tmp1 = min( melt, dim(ql_mlt, ql(k)) )
        ql(k) = ql(k) + tmp1
        qr(k) = qr(k) + melt - tmp1
        qi(k) = qi(k) - melt
        tz(k) = tz(k) - melt*icpk(k)
           tc = tz(k) - tice
    endif
!------
! Snow
!------
!!!#ifdef MORE_SNOW_MLT
    if ( qs(k)>qvmin .and. tc>0. ) then
         factor = min( 1., tc/t_snow_melt)
           sink = min(fac_sno*qs(k), factor*tc/icpk(k))
         qs(k) = qs(k) - sink
         qr(k) = qr(k) + sink
         tz(k) = tz(k) - sink*icpk(k)    ! cooling due to snow melting
            tc = tz(k) - tice
    endif

!------
! Graupel
!------
    if ( qg(k)>qcmin .and. tc>0.01 ) then
         factor = min( 1., tc/t_grau_melt )
           sink = min( fac_gra*qg(k), factor*tc/icpk(k) )
          qg(k) = qg(k) - sink
          qr(k) = qr(k) + sink
          tz(k) = tz(k) - sink*icpk(k)
    endif
!!!#endif
 enddo

  if ( dtm < 60. ) k0 = kbot
  k0 = kbot

!-----
! ice:
!-----

  ze(kbot+1) = zs
  do k=kbot, ktop, -1
     ze(k) = ze(k+1) - dz(k)  ! dz<0
  enddo

  zt(ktop) = ze(ktop)

  call check_column(ktop, kbot, qi, no_fall)

  if ( vi_fac < 1.e-5 .or. no_fall ) then
     i1 = 0.
  else

  do k=ktop+1,kbot
     zt(k) = ze(k) - dt5*(vti(k-1)+vti(k))
  enddo
  zt(kbot+1) = zs - dtm*vti(kbot)

  do k=ktop,kbot
     if( zt(k+1)>=zt(k) ) zt(k+1) = zt(k) - dz_min
  enddo

  if ( k0 < kbot ) then
  do k=kbot-1,k0,-1
     if ( qi(k) > qrmin ) then
          do m=k+1, kbot
             if ( zt(k+1)>=ze(m) ) exit
             if ( zt(k)<ze(m+1) .and. tz(m)>tice ) then
                  dtime = min( 1.0, (ze(m)-ze(m+1))/(max(vmin,vti(k))*tau_mlt) )
                   melt = min( qi(k)*dp(k)/dp(m), dtime*(tz(m)-tice)/icpk(m) )
                   tmp1 = min( melt, dim(ql_mlt, ql(m)) )
                  ql(m) = ql(m) + tmp1
                  qr(m) = qr(m) - tmp1 + melt
                  tz(m) = tz(m) - melt*icpk(m)
                  qi(k) = qi(k) - melt*dp(m)/dp(k)
             endif
          enddo
     endif
  enddo
  endif

  if ( do_sedi_w ) then
      do k=ktop, kbot
         dm(k) = dp(k)*(1.+qv(k)+ql(k)+qr(k)+qi(k)+qs(k)+qg(k))
      enddo
  endif
  if ( use_ppm ) then
       call lagrangian_fall_ppm(ktop, kbot, zs, ze, zt, dp, qi, i1, m1_sol, mono_prof)
  else
       call lagrangian_fall_pcm(ktop, kbot, zs, ze, zt, dp, qi, i1, m1_sol)
  endif

  if ( do_sedi_w ) then
     w1(ktop) = (dm(ktop)*w1(ktop)+m1_sol(ktop)*vti(ktop)) /(dm(ktop)-m1_sol(ktop))
     do k=ktop+1, kbot
        w1(k) = (dm(k)*w1(k)-m1_sol(k-1)*vti(k-1)+m1_sol(k)*vti(k))   &
              / (dm(k)      +m1_sol(k-1)         -m1_sol(k))
     enddo
  endif

  endif

!--------------------------------------------
! melting of falling snow (qs) into rain(qr)
!--------------------------------------------
  r1 = 0.

  call check_column(ktop, kbot, qs, no_fall)

  if ( no_fall ) then
       s1 = 0.
  else

  do k=ktop+1,kbot
     zt(k) = ze(k) - dt5*(vts(k-1)+vts(k))
  enddo
  zt(kbot+1) = zs - dtm*vts(kbot)

  do k=ktop,kbot
     if( zt(k+1)>=zt(k) ) zt(k+1) = zt(k) - dz_min
  enddo

  if ( k0 < kbot ) then
  do k=kbot-1,k0,-1
     if ( qs(k) > qrmin ) then
          do m=k+1, kbot
             if ( zt(k+1)>=ze(m) ) exit
                  dtime = min( dtm, (ze(m)-ze(m+1))/(vmin+vts(k)) )
             if ( zt(k)<ze(m+1) .and. tz(m)>tice ) then
                  dtime = min(1., dtime/tau_s)
                   melt = min(qs(k)*dp(k)/dp(m), dtime*(tz(m)-tice)/icpk(m))
                  tz(m) = tz(m) - melt*icpk(m)
                  qs(k) = qs(k) - melt*dp(m)/dp(k)
                  if ( zt(k)<zs ) then
                       r1 = r1 + melt*dp(m)   ! precip as rain
                  else
!                      qr source here will fall next time step (therefore, can evap)
                       qr(m) = qr(m) + melt
                  endif
             endif
             if ( qs(k) < qrmin ) exit
          enddo
     endif
  enddo
  endif

  if ( do_sedi_w ) then
      do k=ktop, kbot
         dm(k) = dp(k)*(1.+qv(k)+ql(k)+qr(k)+qi(k)+qs(k)+qg(k))
      enddo
  endif
  if ( use_ppm ) then
       call lagrangian_fall_ppm(ktop, kbot, zs, ze, zt, dp, qs, s1, m1, mono_prof)
  else
       call lagrangian_fall_pcm(ktop, kbot, zs, ze, zt, dp, qs, s1, m1)
  endif
  do k=ktop,kbot
     m1_sol(k) = m1_sol(k) + m1(k)
  enddo
  if ( do_sedi_w ) then
     w1(ktop) = (dm(ktop)*w1(ktop)+m1(ktop)*vts(ktop)) /(dm(ktop)-m1(ktop))
     do k=ktop+1, kbot
        w1(k) = (dm(k)*w1(k) - m1(k-1)*vts(k-1) + m1(k)*vts(k))   &
              / (dm(k)       + m1(k-1)          - m1(k))
     enddo
  endif
  endif

!----------------------------------------------
! melting of falling graupel (qg) into rain(qr)
!----------------------------------------------

  call check_column(ktop, kbot, qg, no_fall)

  if ( no_fall ) then
       g1 = 0.
  else
  do k=ktop+1,kbot
     zt(k) = ze(k) - dt5*(vtg(k-1)+vtg(k))
  enddo
  zt(kbot+1) = zs - dtm*vtg(kbot)

  do k=ktop,kbot
     if( zt(k+1)>=zt(k) ) zt(k+1) = zt(k) - dz_min
  enddo

  if ( k0 < kbot ) then
  do k=kbot-1,k0,-1
     if ( qg(k) > qrmin ) then
          do m=k+1, kbot
             if ( zt(k+1)>=ze(m) ) exit
             dtime = min( dtm, (ze(m)-ze(m+1))/vtg(k) )
             if ( zt(k)<ze(m+1) .and. tz(m)>tice ) then
                  dtime = min(1., dtime/tau_g)
                   melt = min(qg(k)*dp(k)/dp(m), dtime*(tz(m)-tice)/icpk(m))
                  tz(m) = tz(m) - melt*icpk(m)
                  qg(k) = qg(k) -  melt*dp(m)/dp(k)
                  if ( zt(k)<zs ) then
                       r1 = r1 + melt*dp(m)
                  else
                       qr(m) = qr(m) + melt
                  endif
             endif
             if ( qg(k) < qrmin ) exit
           enddo
     endif
  enddo
  endif

  if ( do_sedi_w ) then
      do k=ktop, kbot
         dm(k) = dp(k)*(1.+qv(k)+ql(k)+qr(k)+qi(k)+qs(k)+qg(k))
      enddo
  endif
  if ( use_ppm ) then
       call lagrangian_fall_ppm(ktop, kbot, zs, ze, zt, dp, qg, g1, m1, mono_prof)
  else
       call lagrangian_fall_pcm(ktop, kbot, zs, ze, zt, dp, qg, g1, m1)
  endif
  do k=ktop,kbot
     m1_sol(k) = m1_sol(k) + m1(k)
  enddo
  if ( do_sedi_w ) then
     w1(ktop) = (dm(ktop)*w1(ktop)+m1(ktop)*vtg(ktop)) /(dm(ktop)-m1(ktop))
     do k=ktop+1, kbot
        w1(k) = (dm(k)*w1(k) - m1(k-1)*vtg(k-1) + m1(k)*vtg(k))   &
              / (dm(k)       + m1(k-1)          - m1(k))
     enddo
  endif
  endif


 end subroutine terminal_fall


 subroutine check_column(ktop, kbot, q, no_fall)
 integer, intent(in):: ktop, kbot
 real,    intent(in):: q(ktop:kbot)
 logical, intent(out):: no_fall
! local:
 integer k

 no_fall = .true.
 do k=ktop, kbot
    if ( q(k) > qrmin ) then
         no_fall = .false.
         exit
    endif
 enddo

 end subroutine check_column


 subroutine lagrangian_fall_pcm(ktop, kbot, zs, ze, zt, dp, q, precip, m1)
 real,    intent(in):: zs
 integer, intent(in):: ktop, kbot
 real,    intent(in), dimension(ktop:kbot+1):: ze, zt
 real,    intent(in), dimension(ktop:kbot):: dp
 real,    intent(inout), dimension(ktop:kbot):: q, m1
 real,    intent(out):: precip
! local:
 real, dimension(ktop:kbot):: qm
 integer k, k0, n, m

! density:
  do k=ktop,kbot
     q(k) = q(k)*dp(k) / (zt(k)-zt(k+1))
     qm(k) = 0.
  enddo

   k0 = ktop
   do k=ktop,kbot
      do n=k0,kbot
      if(ze(k) <= zt(n) .and. ze(k) >= zt(n+1)) then
         if(ze(k+1) >= zt(n+1)) then
!                          entire new grid is within the original grid
            qm(k) = q(n)*(ze(k)-ze(k+1))
            k0 = n
            goto 555
         else
            qm(k) = q(n)*(ze(k)-zt(n+1))    ! fractional area
            do m=n+1,kbot
!                                        locate the bottom edge: ze(k+1)
               if(ze(k+1) < zt(m+1) ) then
                  qm(k) = qm(k) + q(m)
               else
                  qm(k) = qm(k) + q(m)*(zt(m)-ze(k+1))
                  k0 = m
                  goto 555
               endif
            enddo
            goto 555
         endif
      endif
      enddo
555 continue
   enddo

  m1(ktop) = q(ktop) - qm(ktop)
  do k=ktop+1,kbot
     m1(k) = m1(k-1) + q(k) - qm(k)
  enddo

#ifdef DO_ DIRECT_PRECIP
     precip = 0.
! direct algorithm (prevent small negatives)
     do k=ktop,kbot
        if ( zt(k+1) < zs ) then
             precip = q(k)*(zs-zt(k+1))
             if ( (k+1) > kbot ) goto 777
                  do m=k+1,kbot
                     precip = precip + q(m)
                  enddo
             goto 777
        endif
     enddo
777  continue
#else
  precip = m1(kbot)
#endif

! Convert back to *dry* mixing ratio:
! dp must be dry air_mass (because moist air mass will be changed due to terminal fall).
   do k=ktop,kbot
     q(k) = qm(k)/dp(k)
   enddo

 end subroutine lagrangian_fall_pcm



 subroutine lagrangian_fall_ppm(ktop, kbot, zs, ze, zt, dp, q, precip, m1, mono)
 integer, intent(in):: ktop, kbot
 real,    intent(in):: zs
 logical, intent(in):: mono
 real,    intent(in), dimension(ktop:kbot+1):: ze, zt
 real,    intent(in), dimension(ktop:kbot):: dp
! m1: flux
 real,    intent(inout), dimension(ktop:kbot):: q, m1
 real,    intent(out):: precip
! local:
 real, dimension(ktop:kbot):: qm, dz
 real a4(4,ktop:kbot)
 real pl, pr, delz, esl
 integer k, k0, n, m
 real, parameter:: r3 = 1./3., r23 = 2./3.

! density:
  do k=ktop,kbot
      dz(k) = zt(k) - zt(k+1)      ! note: dz is positive
       q(k) = q(k) * dp(k)
     a4(1,k) = q(k) / dz(k)
     qm(k) = 0.
  enddo

! Construct vertical profile with zt as coordinate

   call cs_profile(a4(1,ktop), dz(ktop), kbot-ktop+1, mono)

   k0 = ktop
   do k=ktop,kbot
      do n=k0,kbot
      if(ze(k) <= zt(n) .and. ze(k) >= zt(n+1)) then
         pl = (zt(n)-ze(k)) / dz(n)
         if( zt(n+1) <= ze(k+1) ) then
!                          entire new grid is within the original grid
                pr = (zt(n)-ze(k+1)) / dz(n)
            qm(k) = a4(2,n) + 0.5*(a4(4,n)+a4(3,n)-a4(2,n))*(pr+pl) -  &
                     a4(4,n)*r3*(pr*(pr+pl)+pl**2)
            qm(k) = qm(k)*(ze(k)-ze(k+1))
            k0 = n
            goto 555
         else
            qm(k) = (ze(k)-zt(n+1)) * (a4(2,n)+0.5*(a4(4,n)+   &
                      a4(3,n)-a4(2,n))*(1.+pl) - a4(4,n)*( r3*(1.+pl*(1.+pl))) )
            if ( n<kbot ) then
               do m=n+1,kbot
!                                        locate the bottom edge: ze(k+1)
                  if( ze(k+1) < zt(m+1) ) then
                     qm(k) = qm(k) + q(m)
                  else
                     delz = zt(m) - ze(k+1)
                      esl = delz / dz(m)
                     qm(k) = qm(k) + delz*( a4(2,m) + 0.5*esl*        &
                             (a4(3,m)-a4(2,m)+a4(4,m)*(1.-r23*esl)) )
                     k0 = m
                     goto 555
                  endif
               enddo
            endif
            goto 555
         endif
      endif
      enddo
555 continue
   enddo

  m1(ktop) = q(ktop) - qm(ktop)
  do k=ktop+1,kbot
     m1(k) = m1(k-1) + q(k) - qm(k)
  enddo
  precip = m1(kbot)

! Convert back to *dry* mixing ratio:
! dp must be dry air_mass (because moist air mass will be changed due to terminal fall).
   do k=ktop,kbot
     q(k) = qm(k)/dp(k)
   enddo

 end subroutine lagrangian_fall_ppm


 subroutine cs_profile(a4, del, km, do_mono)
 integer, intent(in):: km      ! vertical dimension
 real   , intent(in):: del(km)
 logical, intent(in):: do_mono
 real , intent(inout):: a4(4,km)
!-----------------------------------------------------------------------
 real, parameter:: qp_min = 1.e-6
 real  gam(km)
 real  q(km+1)
 real   d4, bet, a_bot, grat, pmp, lac
 real   pmp_1, lac_1, pmp_2, lac_2
 real  da1, da2, a6da
 integer k
 logical extm(km)

     grat = del(2) / del(1)   ! grid ratio
      bet = grat*(grat+0.5)
     q(1) = (2.*grat*(grat+1.)*a4(1,1)+a4(1,2)) / bet
   gam(1) = ( 1. + grat*(grat+1.5) ) / bet

  do k=2,km
      d4 = del(k-1) / del(k)
     bet =  2. + 2.*d4 - gam(k-1)
     q(k) = (3.*(a4(1,k-1)+d4*a4(1,k))-q(k-1))/bet
     gam(k) = d4 / bet
  enddo

       a_bot = 1. + d4*(d4+1.5)
     q(km+1) = (2.*d4*(d4+1.)*a4(1,km)+a4(1,km-1)-a_bot*q(km))  &
             / ( d4*(d4+0.5) - a_bot*gam(km) )

  do k=km,1,-1
     q(k) = q(k) - gam(k)*q(k+1)
  enddo

!------------------
! Apply constraints
!------------------
  do k=2,km
     gam(k) = a4(1,k) - a4(1,k-1)
  enddo

! Apply large-scale constraints to ALL fields if not local max/min

! Top:
  q(1) = max( q(1), 0. )
  q(2) = min( q(2), max(a4(1,1), a4(1,2)) )
  q(2) = max( q(2), min(a4(1,1), a4(1,2)), 0. )

! Interior:
  do k=3,km-1
     if ( gam(k-1)*gam(k+1)>0. ) then
          q(k) = min( q(k), max(a4(1,k-1),a4(1,k)) )
          q(k) = max( q(k), min(a4(1,k-1),a4(1,k)) )
     else
          if ( gam(k-1) > 0. ) then
! There exists a local max
               q(k) = max( q(k), min(a4(1,k-1),a4(1,k)) )
          else
! There exists a local min
               q(k) = min( q(k), max(a4(1,k-1),a4(1,k)) )
               q(k) = max( q(k), 0.0 )
          endif
     endif
  enddo

  q(km  ) = min( q(km), max(a4(1,km-1), a4(1,km)) )
  q(km  ) = max( q(km), min(a4(1,km-1), a4(1,km)), 0. )
! q(km+1) = max( q(km+1), 0.)

!-----------------------------------------------------------
! f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
!-----------------------------------------------------------
  do k=1,km-1
     a4(2,k) = q(k  )
     a4(3,k) = q(k+1)
  enddo

  do k=2,km-1
     if ( gam(k)*gam(k+1) > 0.0 ) then
          extm(k) = .false.
     else
          extm(k) = .true.
     endif
  enddo

  if ( do_mono ) then
     do k=3,km-2
        if ( extm(k) ) then
! positive definite constraint ONLY if true local extrema
           if ( a4(1,k)<qp_min .or. extm(k-1) .or. extm(k+1) ) then
               a4(2,k) = a4(1,k)
               a4(3,k) = a4(1,k)
           endif
        else
           a4(4,k) = 6.*a4(1,k) - 3.*(a4(2,k)+a4(3,k))
           if( abs(a4(4,k)) > abs(a4(2,k)-a4(3,k)) ) then
! Check within the smooth region if subgrid profile is non-monotonic
                pmp_1 = a4(1,k) - 2.0*gam(k+1)
                lac_1 = pmp_1   + 1.5*gam(k+2)
              a4(2,k) = min( max(a4(2,k), min(a4(1,k), pmp_1, lac_1)),  &
                                          max(a4(1,k), pmp_1, lac_1) )
                pmp_2 = a4(1,k) + 2.0*gam(k)
                lac_2 = pmp_2   - 1.5*gam(k-1)
              a4(3,k) = min( max(a4(3,k), min(a4(1,k), pmp_2, lac_2)),  &
                                          max(a4(1,k), pmp_2, lac_2) )
           endif
        endif
     enddo
  else
     do k=3,km-2
        if ( extm(k) .and. (extm(k-1).or.extm(k+1).or.a4(1,k)<qp_min) ) then
             a4(2,k) = a4(1,k)
             a4(3,k) = a4(1,k)
        endif
     enddo
  endif

  do k=1,km-1
     a4(4,k) = 6.*a4(1,k) - 3.*(a4(2,k)+a4(3,k))
  enddo

  k = km-1
  if( extm(k) ) then
      a4(2,k) = a4(1,k)
      a4(3,k) = a4(1,k)
      a4(4,k) = 0.
  else
      da1  = a4(3,k) - a4(2,k)
      da2  = da1**2
      a6da = a4(4,k)*da1
      if(a6da < -da2) then
         a4(4,k) = 3.*(a4(2,k)-a4(1,k))
         a4(3,k) = a4(2,k) - a4(4,k)
      elseif(a6da > da2) then
         a4(4,k) = 3.*(a4(3,k)-a4(1,k))
         a4(2,k) = a4(3,k) - a4(4,k)
      endif
  endif

  call cs_limiters(km-1, a4)

! Bottom layer:
  a4(2,km) = a4(1,km)
  a4(3,km) = a4(1,km)
  a4(4,km) = 0.

 end subroutine cs_profile



 subroutine cs_limiters(km, a4)
 integer, intent(in) :: km
 real, intent(inout) :: a4(4,km)   ! PPM array
! !LOCAL VARIABLES:
 real, parameter:: r12 = 1./12.
 integer k

! Positive definite constraint

 do k=1,km
 if( abs(a4(3,k)-a4(2,k)) < -a4(4,k) ) then
     if( (a4(1,k)+0.25*(a4(3,k)-a4(2,k))**2/a4(4,k)+a4(4,k)*r12) < 0. ) then
         if( a4(1,k)<a4(3,k) .and. a4(1,k)<a4(2,k) ) then
             a4(3,k) = a4(1,k)
             a4(2,k) = a4(1,k)
             a4(4,k) = 0.
         elseif( a4(3,k) > a4(2,k) ) then
             a4(4,k) = 3.*(a4(2,k)-a4(1,k))
             a4(3,k) = a4(2,k) - a4(4,k)
         else
             a4(4,k) = 3.*(a4(3,k)-a4(1,k))
             a4(2,k) = a4(3,k) - a4(4,k)
         endif
     endif
 endif
 enddo

 end subroutine cs_limiters



 subroutine fall_speed(ktop, kbot, den, qs, qi, qg, ql, tk, vts, vti, vtg)
 integer, intent(in)                     :: ktop, kbot
 real, intent(in ), dimension(ktop:kbot) :: den, qs, qi, qg, ql, tk
 real, intent(out), dimension(ktop:kbot) :: vts, vti, vtg
! fall velocity constants:
 real, parameter :: thi = 1.0e-9   ! cloud ice threshold for terminal fall
 real, parameter :: thg = 1.0e-9
 real, parameter :: ths = 1.0e-9
 real, parameter :: vf_min = 1.0E-5
 real, parameter :: vs_max = 7.        ! max fall speed for snow
!-----------------------------------------------------------------------
! marshall-palmer constants
!-----------------------------------------------------------------------
 real :: vcons = 6.6280504, vcong = 87.2382675, vconi = 3.29
 real :: norms = 942477796.076938, &
         normg =  5026548245.74367
 real, dimension(ktop:kbot) :: ri, qden, tc, rhof
 real :: aa = -4.14122e-5, bb = -0.00538922, cc = -0.0516344, dd = 0.00216078, ee = 1.9714

 real :: rho0, vconf
 integer:: k
!-----------------------------------------------------------------------
! marshall-palmer formula
!-----------------------------------------------------------------------

! try the local air density -- for global model; the true value could be
! much smaller than sfcrho over high mountains

  if ( den_ref < 0. ) then
       rho0 = -den_ref*den(kbot)
  else
       rho0 = den_ref   ! default=1.2
  endif

  do k=ktop, kbot
     rhof(k) = sqrt( min(100., rho0/den(k)) )
  enddo

   do k=ktop, kbot
! snow:
      if ( qs(k) < ths ) then
           vts(k) = vf_min
      else
           vts(k) = max(vf_min, vcons*rhof(k)*exp(0.0625*log(qs(k)*den(k)/norms)))
           vts(k) = min(vs_max, vs_fac*vts(k) )
      endif

! graupel:
      if ( qg(k) < thg ) then
           vtg(k) = vf_min
      else
           vtg(k) = max(vf_min, max(vmin, vg_fac*vcong*rhof(k)*sqrt(sqrt(sqrt(qg(k)*den(k)/normg)))))
      endif
   enddo

! ice:
   if ( use_deng_mace ) then
! ice use Deng and Mace (2008, GRL), which gives smaller fall speed than HD90 formula
       do k=ktop, kbot
          if ( qi(k) < thi ) then
               vti(k) = vf_min
          else
           qden(k) = log10( 1000.*qi(k)*den(k) )   !--- used in DM formula, in g/m^-3
             tc(k) = tk(k) - tice
            vti(k) = qden(k)*( tc(k)*(aa*tc(k) + bb) + cc ) + dd*tc(k) + ee
            vti(k) = max( vf_min, vi_fac*0.01*10.**vti(k) )
          endif
       enddo
   else
! HD90 ice speed:
       vconf = vi_fac*vconi
       do k=ktop, kbot
          if ( qi(k) < thi ) then
               vti(k) = vf_min
          else
! vti = vconi*rhof*(qi*den)**0.16
               vti(k) = max( vf_min, vconf*rhof(k)*exp(0.16*log(qi(k)*den(k))) )
          endif
       enddo
   endif

 end subroutine fall_speed


 subroutine setupm

 real :: gcon, cd, scm3, pisq, act(8), acc(3)
 real :: vdifu, tcond
 real :: visk
 real :: ch2o, hltf
 real ::  hlts, hltc, ri50

 real :: gam263, gam275, gam290,                                &
         gam325, gam350, gam380,                                &
         gam425, gam450, gam480,                                &
         gam625, gam680

 data  gam263/1.456943/,   gam275/1.608355/,  gam290/1.827363/  &
       gam325/2.54925/,    gam350/3.323363/,  gam380/4.694155/  &
       gam425/8.285063/,   gam450/11.631769/, gam480/17.837789/ &
       gam625/184.860962/, gam680/496.604067/
!
!     physical constants (mks)
!
 real :: rnzr, rnzs, rnzg, rhos, rhog
 data rnzr /8.0e6/  ! lin83
 data rnzs /3.0e6/  ! lin83
 data rnzg /4.0e6/  ! rh84
 data rhos /0.1e3/  ! lin83    (snow density; 1/10 of water)
 data rhog /0.4e3/  ! rh84     (graupel density)
 data acc/5.0,2.0,0.5/

 real den_rc
 integer :: k, i

      pie = 4.*atan(1.0)

! S. Klein's formular (EQ 16) from AM2
      fac_rc = (4./3.)*pie*rhor*rthresh**3

   if ( prog_ccn ) then
      if(master) write(*,*) 'prog_ccn option is .T.'
   else
      den_rc = fac_rc * ccn_o*1.e6
      if(master) write(*,*) 'MP: rthresh=', rthresh, 'vi_fac=', vi_fac
      if(master) write(*,*) 'MP: for ccn_o=', ccn_o, 'ql_rc=', den_rc
      den_rc = fac_rc * ccn_l*1.e6
      if(master) write(*,*) 'MP: for ccn_l=', ccn_l, 'ql_rc=', den_rc
   endif

      vdifu=2.11e-5
      tcond=2.36e-2

      visk=1.259e-5
      hlts=2.8336e6
      hltc=2.5e6
      hltf=3.336e5

      ch2o=4.1855e3
      ri50=1.e-4

      pisq = pie*pie
      scm3 = (visk/vdifu)**(1./3.)
!
      cracs = pisq*rnzr*rnzs*rhos
      csacr = pisq*rnzr*rnzs*rhor
      cgacr = pisq*rnzr*rnzg*rhor
      cgacs = pisq*rnzg*rnzs*rhos
      cgacs = cgacs*c_pgacs
!
!     act:  1-2:racs(s-r); 3-4:sacr(r-s);
!           5-6:gacr(r-g); 7-8:gacs(s-g)
!
      act(1) = pie * rnzs * rhos
      act(2) = pie * rnzr * rhor
      act(6) = pie * rnzg * rhog
      act(3) = act(2)
      act(4) = act(1)
      act(5) = act(2)
      act(7) = act(1)
      act(8) = act(6)

      do i=1,3
         do k=1,4
            acco(i,k) = acc(i)/(act(2*k-1)**((7-i)*0.25)*act(2*k)**(i*0.25))
         enddo
      enddo
!
      gcon  = 40.74 * sqrt( sfcrho )   ! 44.628
!
      csacw = pie*rnzs*clin*gam325/(4.*act(1)**0.8125)
! Decreasing  csacw to reduce cloud water ---> snow

      craci = pie*rnzr*alin*gam380/(4.*act(2)**0.95)
      csaci = csacw * c_psaci
!
      cgacw = pie*rnzg*gam350*gcon/(4.*act(6)**0.875)
!     cgaci = cgacw*0.1
! SJL, May 28, 2012
      cgaci = cgacw*0.05
!
      cracw = craci            ! cracw= 3.27206196043822
      cracw = c_cracw * cracw
!
!     subl and revp:  five constants for three separate processes
!
      cssub(1) = 2.*pie*vdifu*tcond*rvgas*rnzs
      cgsub(1) = 2.*pie*vdifu*tcond*rvgas*rnzg
      crevp(1) = 2.*pie*vdifu*tcond*rvgas*rnzr
      cssub(2) = 0.78/sqrt(act(1))
      cgsub(2) = 0.78/sqrt(act(6))
      crevp(2) = 0.78/sqrt(act(2))
      cssub(3) = 0.31*scm3*gam263*sqrt(clin/visk)/act(1)**0.65625
      cgsub(3) = 0.31*scm3*gam275*sqrt(gcon/visk)/act(6)**0.6875
      crevp(3) = 0.31*scm3*gam290*sqrt(alin/visk)/act(2)**0.725
      cssub(4) = tcond*rvgas
      cssub(5) = hlts**2*vdifu
      cgsub(4) = cssub(4)
      crevp(4) = cssub(4)
      cgsub(5) = cssub(5)
      crevp(5) = hltc**2*vdifu
!
      cgfr(1) = 20.e2*pisq*rnzr*rhor/act(2)**1.75
      cgfr(2) = 0.66
!
!sk ********************************************************************
!sk   smlt:  five constants ( lin et al. 1983 )
      csmlt(1) = 2.*pie*tcond*rnzs/hltf
      csmlt(2) = 2.*pie*vdifu*rnzs*hltc/hltf
      csmlt(3) = cssub(2)
      csmlt(4) = cssub(3)
      csmlt(5) = ch2o/hltf
!sk ********************************************************************
!     gmlt:  five constants
      cgmlt(1) = 2.*pie*tcond*rnzg/hltf
      cgmlt(2) = 2.*pie*vdifu*rnzg*hltc/hltf
      cgmlt(3) = cgsub(2)
      cgmlt(4) = cgsub(3)
      cgmlt(5) = ch2o/hltf
!sk ********************************************************************
      es0 = 6.107799961e2   ! ~6.1 mb
      ces0 = eps*es0

 end subroutine setupm


 subroutine lin_cld_microphys_init(id, jd, kd, axes, time)

    integer,         intent(in) :: id, jd, kd
    integer,         intent(in) :: axes(4)
    type(time_type), intent(in) :: time

    integer   :: unit, io, ierr, k, logunit
    logical   :: flag
    real :: tmp, q1, q2

    master = (mpp_pe().eq.mpp_root_pe())

#ifdef INTERNAL_FILE_NML
    read( input_nml_file, nml = lin_cld_microphys_nml, iostat = io )
    ierr = check_nml_error(io,'lin_cloud_microphys_nml')
#else
    if( file_exist( 'input.nml' ) ) then
       unit = open_namelist_file ()
       io = 1
       do while ( io .ne. 0 )
          read( unit, nml = lin_cld_microphys_nml, iostat = io, end = 10 )
          ierr = check_nml_error(io,'lin_cloud_microphys_nml')
       end do
10     call close_file ( unit )
    end if
#endif
    call write_version_number (version, tagname)
    logunit = stdlog()

    if ( do_setup ) then
      call setup_con
      call setupm
      do_setup = .false.
    endif

    tice0 = tice - 0.1
    t_wfr = tice - 40. ! supercooled water can exist down to -48 C, which is the "absolute"

    if (master) write( logunit, nml = lin_cld_microphys_nml )

    id_vtr = register_diag_field ( mod_name, 'vt_r', axes(1:3), time,        &
         'rain fall speed', 'm/sec', missing_value=missing_value )
    id_vts = register_diag_field ( mod_name, 'vt_s', axes(1:3), time,        &
         'snow fall speed', 'm/sec', missing_value=missing_value )
    id_vtg = register_diag_field ( mod_name, 'vt_g', axes(1:3), time,        &
         'graupel fall speed', 'm/sec', missing_value=missing_value )
    id_vti = register_diag_field ( mod_name, 'vt_i', axes(1:3), time,        &
         'ice fall speed', 'm/sec', missing_value=missing_value )
    id_droplets = register_diag_field ( mod_name, 'droplets', axes(1:3), time,        &
         'droplet number concentration', '#/m3', missing_value=missing_value )
    id_rh = register_diag_field ( mod_name, 'rh_lin', axes(1:2), time,        &
         'relative humidity', 'n/a', missing_value=missing_value )

    id_rain = register_diag_field ( mod_name, 'rain_lin', axes(1:2), time,        &
         'rain_lin', 'mm/day', missing_value=missing_value )
    id_snow = register_diag_field ( mod_name, 'snow_lin', axes(1:2), time,        &
         'snow_lin', 'mm/day', missing_value=missing_value )
    id_graupel = register_diag_field ( mod_name, 'graupel_lin', axes(1:2), time,  &
         'graupel_lin', 'mm/day', missing_value=missing_value )
    id_ice = register_diag_field ( mod_name, 'ice_lin', axes(1:2), time,        &
         'ice_lin', 'mm/day', missing_value=missing_value )
    id_prec = register_diag_field ( mod_name, 'prec_lin', axes(1:2), time,     &
         'prec_lin', 'mm/day', missing_value=missing_value )
!   if ( master ) write(*,*) 'prec_lin diagnostics initialized.', id_prec

    id_cond = register_diag_field ( mod_name, 'cond_lin', axes(1:2), time,     &
         'total condensate', 'kg/m**2', missing_value=missing_value )

    id_var = register_diag_field ( mod_name, 'var_lin', axes(1:2), time,     &
         'subgrid variance', 'n/a',  missing_value=missing_value )

    id_dbz = register_diag_field ( mod_name, 'reflectivity', axes(1:3), time, &
         'Stoelinga simulated reflectivity', 'dBz', missing_value=missing_value)

    id_maxdbz = register_diag_field ( mod_name, 'max_reflectivity', axes(1:2), time, &
         'Stoelinga simulated maximum (composite) reflectivity', 'dBz', missing_value=missing_value)

    id_basedbz = register_diag_field ( mod_name, 'base_reflectivity', axes(1:2), time, &
         'Stoelinga simulated base reflectivity', 'dBz', missing_value=missing_value)
!   call qsmith_init

! TESTING the water vapor tables
   if ( mp_debug .and. master ) then
        write(*,*) 'TESTING water vapor tables in lin_cld_microphys'
        tmp = tice - 90.
   do k=1,25
      q1 = wqsat_moist(tmp, 0., 1.E5)
      q2 = qs1d_m(tmp, 0., 1.E5)
      write(*,*) NINT(tmp-tice), q1, q2, 'dq=', q1-q2
      tmp = tmp + 5.
   enddo
   endif

   if ( master ) write(*,*) 'lin_cld_micrphys diagnostics initialized.'

   lin_cld_mp_clock = mpp_clock_id('Lin_cld_microphys', grain=CLOCK_ROUTINE)

   module_is_initialized = .true.

 end subroutine lin_cld_microphys_init



 subroutine lin_cld_microphys_end

   deallocate ( table  )
   deallocate ( table2 )
   deallocate ( table3 )
   deallocate ( tablew )
   deallocate ( des )
   deallocate ( des2 )
   deallocate ( des3 )
   deallocate ( desw )

   tables_are_initialized = .false.

 end subroutine lin_cld_microphys_end



 subroutine setup_con

  master = (mpp_pe().eq.mpp_root_pe())
  rgrav = 1./ grav

  if ( .not. qsmith_tables_initialized ) call qsmith_init
  qsmith_tables_initialized = .true.

 end subroutine setup_con



 real function acr3d(v1, v2, q1, q2, c, cac, rho)
 real, intent(in) :: v1, v2, c, rho
 real, intent(in) :: q1, q2    ! mixing ratio!!!
 real, intent(in) :: cac(3)
 real :: t1, s1, s2
!integer :: k
! real:: a
!     a=0.0
!     do k=1,3
!        a = a + cac(k)*( (q1*rho)**((7-k)*0.25) * (q2*rho)**(k*0.25) )
!     enddo
!     acr3d = c * abs(v1-v2) * a/rho
!----------
! Optimized
!----------
      t1 = sqrt(q1*rho)
      s1 = sqrt(q2*rho)
      s2 = sqrt(s1)       ! s1 = s2**2
      acr3d = c*abs(v1-v2)*q1*s2*(cac(1)*t1 + cac(2)*sqrt(t1)*s2 + cac(3)*s1)

 end function acr3d




 real function smlt(tc, dqs, qsrho,psacw,psacr,c,rho, rhofac)
 real, intent(in):: tc,dqs,qsrho,psacw,psacr,c(5),rho, rhofac

 smlt = (c(1)*tc/rho-c(2)*dqs) * (c(3)*sqrt(qsrho)+ &
         c(4)*qsrho**0.65625*sqrt(rhofac)) + c(5)*tc*(psacw+psacr)

 end function smlt


 real function gmlt(tc, dqs,qgrho,pgacw,pgacr,c, rho)
 real, intent(in)::  tc,dqs,qgrho,pgacw,pgacr,c(5),rho

!     note:  pgacw and pgacr must be calc before gmlt is called
!
 gmlt = (c(1)*tc/rho-c(2)*dqs) * (c(3)*sqrt(qgrho)+ &
         c(4)*qgrho**0.6875/rho**0.25) + c(5)*tc*(pgacw+pgacr)
 end function gmlt


 subroutine qsmith_init
  integer, parameter:: length=2621
  integer i

  if( .not. tables_are_initialized ) then

    master = (mpp_pe().eq.mpp_root_pe())
    if (master) print*, ' lin MP: initializing qs tables'
!!! DEBUG CODE
!    print*, mpp_pe(), allocated(table), allocated(table2), allocated(table3), allocated(tablew), allocated(des), allocated(des2), allocated(des3), allocated(desw)
!!! END DEBUG CODE

!                            generate es table (dt = 0.1 deg. c)
       allocate ( table( length) )
       allocate ( table2(length) )
       allocate ( table3(length) )
       allocate ( tablew(length) )
       allocate (   des (length) )
       allocate (   des2(length) )
       allocate (   des3(length) )
       allocate (   desw(length) )

       call qs_table (length )
       call qs_table2(length )
       call qs_table3(length )
       call qs_tablew(length )

       do i=1,length-1
           des(i) = max(0.,  table(i+1) -  table(i))
          des2(i) = max(0., table2(i+1) - table2(i))
          des3(i) = max(0., table3(i+1) - table3(i))
          desw(i) = max(0., tablew(i+1) - tablew(i))
       enddo
        des(length) =  des(length-1)
       des2(length) = des2(length-1)
       des3(length) = des3(length-1)
       desw(length) = desw(length-1)

       tables_are_initialized = .true.
  endif

 end subroutine qsmith_init

 real function wqs1(ta, den)
! Pure water phase; universal dry/moist formular using air density
! Input "den" can be either dry or moist air density
  real, intent(in):: ta, den
! local:
  real es, ap1
  real, parameter:: tmin=table_ice - 160.
  integer it

       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
        es = tablew(it) + (ap1-it)*desw(it)
      wqs1 = es / (rvgas*ta*den)

 end function wqs1

 real function wqs2(ta, den, dqdt)
! Pure water phase; universal dry/moist formular using air density
! Input "den" can be either dry or moist air density
  real, intent(in):: ta, den
  real, intent(out):: dqdt
! local:
  real es, ap1
  real, parameter:: tmin=table_ice - 160.
  integer it

  if (.not. tables_are_initialized) call qsmith_init

       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
        es = tablew(it) + (ap1-it)*desw(it)
      wqs2 = es / (rvgas*ta*den)
        it = ap1 - 0.5
! Finite diff, del_T = 0.1:
      dqdt = 10.*(desw(it) + (ap1-it)*(desw(it+1)-desw(it))) / (rvgas*ta*den)

 end function wqs2

 real function wet_bulb(q, t, den)
! Liquid phase only
 real, intent(in):: t, q, den
 real:: qs, tp, dqdt

  wet_bulb = t
  qs = wqs2(wet_bulb, den, dqdt)
  tp = 0.5*(qs-q)/(1.+lcp*dqdt)*lcp
  wet_bulb = wet_bulb - tp 
! tp is negative if super-saturated
  if ( tp > 0.01 ) then
       qs = wqs2(wet_bulb, den, dqdt)
       tp = (qs-q)/(1.+lcp*dqdt)*lcp
       wet_bulb = wet_bulb - tp 
  endif

 end function wet_bulb
 real function iqs1(ta, den)
! water-ice phase; universal dry/moist formular using air density
! Input "den" can be either dry or moist air density
  real, intent(in):: ta, den
! local:
  real es, ap1
  real, parameter:: tmin=table_ice - 160.
  integer it

       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
        es = table2(it) + (ap1-it)*des2(it)
      iqs1 = es / (rvgas*ta*den)

 end function iqs1

 real function iqs2(ta, den, dqdt)
! water-ice phase; universal dry/moist formular using air density
! Input "den" can be either dry or moist air density
  real, intent(in):: ta, den
  real, intent(out):: dqdt
! local:
  real es, ap1
  real, parameter:: tmin=table_ice - 160.
  integer it

       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
        es = table2(it) + (ap1-it)*des2(it)
      iqs2 = es / (rvgas*ta*den)
        it = ap1 - 0.5
      dqdt = 10.*(des2(it) + (ap1-it)*(des2(it+1)-des2(it))) / (rvgas*ta*den)

 end function iqs2

 real function qs1d_moist(ta, qv, pa, dqdt)
! 2-phase tabel
  real, intent(in):: ta, pa, qv
  real, intent(out):: dqdt
! local:
  real es, ap1
  real, parameter:: tmin=table_ice - 160.
  real, parameter:: eps10 = 10.*eps
  integer it

       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
        es = table2(it) + (ap1-it)*des2(it)
      qs1d_moist = eps*es*(1.+zvir*qv)/pa
        it = ap1 - 0.5
      dqdt = eps10*(des2(it) + (ap1-it)*(des2(it+1)-des2(it)))*(1.+zvir*qv)/pa

 end function qs1d_moist

 real function wqsat2_moist(ta, qv, pa, dqdt)
! Pure water phase
  real, intent(in):: ta, pa, qv
  real, intent(out):: dqdt
! local:
  real es, ap1
  real, parameter:: tmin=table_ice - 160.
  real, parameter:: eps10 = 10.*eps
  integer it

       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
        es = tablew(it) + (ap1-it)*desw(it)
     wqsat2_moist = eps*es*(1.+zvir*qv)/pa
     dqdt = eps10*(desw(it) + (ap1-it)*(desw(it+1)-desw(it)))*(1.+zvir*qv)/pa

 end function wqsat2_moist

 real function wqsat_moist(ta, qv, pa)
! Pure water phase
  real, intent(in):: ta, pa, qv
! local:
  real es, ap1
  real, parameter:: tmin=table_ice - 160.
  integer it

       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
        es = tablew(it) + (ap1-it)*desw(it)
     wqsat_moist = eps*es*(1.+zvir*qv)/pa

 end function wqsat_moist

 real function qs1d_m(ta, qv, pa)
! 2-phase tabel
  real, intent(in):: ta, pa, qv
! local:
  real es, ap1
  real, parameter:: tmin=table_ice - 160.
  real, parameter:: eps10 = 10.*eps
  integer it

       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
        es = table2(it) + (ap1-it)*des2(it)
      qs1d_m = eps*es*(1.+zvir*qv)/pa

 end function qs1d_m

 real function d_sat(ta)
! Computes the difference in saturation vapor *density* between water and ice
  real, intent(in):: ta
  real, parameter:: tmin=table_ice - 160.
  real es_w, es_i, ap1
  integer it

       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
! over Water:
       es_w = tablew(it) + (ap1-it)*desw(it)
! over Ice:
       es_i = table2(it) + (ap1-it)*des2(it)
      d_sat = dim(es_w, es_i)/(rvgas*ta)  ! Take positive difference

 end function d_sat


 real function esw_table(ta)
! pure water phase table
  real, intent(in):: ta
  real, parameter:: tmin=table_ice - 160.
  real  ap1
  integer it
       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
      esw_table = tablew(it) + (ap1-it)*desw(it)
 end function esw_table


 real function es2_table(ta)
! two-phase table
  real, intent(in):: ta
  real, parameter:: tmin=table_ice - 160.
  real  ap1
  integer it
       ap1 = 10.*dim(ta, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
      es2_table = table2(it) + (ap1-it)*des2(it)
 end function es2_table


 subroutine esw_table1d(ta, es, n)
  integer, intent(in):: n
! For waterphase only
  real, intent(in)::  ta(n)
  real, intent(out):: es(n)
  real, parameter:: tmin=table_ice - 160.
  real  ap1
  integer i, it

  do i=1, n
       ap1 = 10.*dim(ta(i), tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
     es(i) = tablew(it) + (ap1-it)*desw(it)
  enddo
 end subroutine esw_table1d



 subroutine es2_table1d(ta, es, n)
  integer, intent(in):: n
! two-phase table with -2C as the transition point for ice-water phase
! For sea ice model
  real, intent(in)::  ta(n)
  real, intent(out):: es(n)
  real, parameter:: tmin=table_ice - 160.
  real  ap1
  integer i, it

  do i=1, n
       ap1 = 10.*dim(ta(i), tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
     es(i) = table2(it) + (ap1-it)*des2(it)
  enddo
 end subroutine es2_table1d


 subroutine es3_table1d(ta, es, n)
  integer, intent(in):: n
! two-phase table with -2C as the transition point for ice-water phase
  real, intent(in)::  ta(n)
  real, intent(out):: es(n)
  real, parameter:: tmin=table_ice - 160.
  real  ap1
  integer i, it

  do i=1, n
       ap1 = 10.*dim(ta(i), tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
     es(i) = table3(it) + (ap1-it)*des3(it)
  enddo
 end subroutine es3_table1d



 subroutine qs_tablew(n)
! Over water
      integer, intent(in):: n
      real:: delt=0.1
      real esbasw, tbasw, esbasi, tbasi, tmin, tem, aa, b, c, d, e
      integer i

! constants
      esbasw = 1013246.0
       tbasw =     373.16
      esbasi =    6107.1
       tbasi =     273.16
        tmin = tbasi - 160.

     do i=1,n
        tem = tmin+delt*real(i-1)
!  compute es over water
#ifdef SMITHSONIAN_TABLE
!  see smithsonian meteorological tables page 350.
        aa  = -7.90298*(tbasw/tem-1.)
        b   =  5.02808*alog10(tbasw/tem)
        c   = -1.3816e-07*(10**((1.-tem/tbasw)*11.344)-1.)
        d   =  8.1328e-03*(10**((tbasw/tem-1.)*(-3.49149))-1.)
        e   = alog10(esbasw)
        tablew(i) = 0.1 * 10**(aa+b+c+d+e)
#else
        tablew(i) = e00*exp((dc_vap*log(tem/t_ice)+Lv0*(tem-t_ice)/(tem*t_ice))/rvgas)
#endif
     enddo

 end subroutine qs_tablew


 subroutine qs_table2(n)
! 2-phase table
  integer, intent(in):: n
  real:: delt=0.1
  real esbasw, tbasw, esbasi, tbasi, tmin, tem, aa, b, c, d, e
  integer :: i0, i1
  real :: tem0, tem1
  integer i

! constants
      esbasw = 1013246.0
       tbasw =     373.16
      esbasi =    6107.1
       tbasi =     273.16
      tmin = tbasi - 160.

     do i=1,n
        tem = tmin+delt*real(i-1)
        if ( i<= 1600 ) then
!  compute es over ice between -160c and 0 c.
#ifdef SMITHSONIAN_TABLE
!  see smithsonian meteorological tables page 350.
              aa  = -9.09718 *(tbasi/tem-1.)
              b   = -3.56654 *alog10(tbasi/tem)
              c   =  0.876793*(1.-tem/tbasi)
              e   = alog10(esbasi)
             table2(i) = 0.1 * 10**(aa+b+c+e)
#else
             table2(i) = e00*exp((d2ice*log(tem/t_ice)+Li2*(tem-t_ice)/(tem*t_ice))/rvgas)
#endif
        else
!  compute es over water between 0c and 102c.
#ifdef SMITHSONIAN_TABLE
!  see smithsonian meteorological tables page 350.
             aa  = -7.90298*(tbasw/tem-1.)
             b   =  5.02808*alog10(tbasw/tem)
             c   = -1.3816e-07*(10**((1.-tem/tbasw)*11.344)-1.)
             d   =  8.1328e-03*(10**((tbasw/tem-1.)*(-3.49149))-1.)
             e   = alog10(esbasw)
             table2(i) = 0.1 * 10**(aa+b+c+d+e)
#else
             table2(i) = e00*exp((dc_vap*log(tem/t_ice)+Lv0*(tem-t_ice)/(tem*t_ice))/rvgas)
#endif
        endif
     enddo

!----------
! smoother
!----------
      i0 = 1600;  i1 = 1601
      tem0 = 0.25*(table2(i0-1) + 2.*table(i0) + table2(i0+1))
      tem1 = 0.25*(table2(i1-1) + 2.*table(i1) + table2(i1+1))
      table2(i0) = tem0
      table2(i1) = tem1

 end subroutine qs_table2



 subroutine qs_table3(n)
! 2-phase table with "-2 C" as the transition point
  integer, intent(in):: n
  real:: delt=0.1
  real esbasw, tbasw, esbasi, tbasi, tmin, tem, aa, b, c, d, e
  integer :: i0, i1
  real :: tem0, tem1
  integer i

! constants
      esbasw = 1013246.0
       tbasw =     373.16
      esbasi =    6107.1
       tbasi =     273.16
      tmin = tbasi - 160.

     do i=1,n
        tem = tmin+delt*real(i-1)
!       if ( i<= 1600 ) then
        if ( i<= 1580 ) then  ! to -2 C
!  compute es over ice between -160c and 0 c.
!  see smithsonian meteorological tables page 350.
              aa  = -9.09718 *(tbasi/tem-1.)
              b   = -3.56654 *alog10(tbasi/tem)
              c   =  0.876793*(1.-tem/tbasi)
              e   = alog10(esbasi)
             table3(i) = 0.1 * 10**(aa+b+c+e)
        else
!  compute es over water between -2c and 102c.
!  see smithsonian meteorological tables page 350.
             aa  = -7.90298*(tbasw/tem-1.)
             b   =  5.02808*alog10(tbasw/tem)
             c   = -1.3816e-07*(10**((1.-tem/tbasw)*11.344)-1.)
             d   =  8.1328e-03*(10**((tbasw/tem-1.)*(-3.49149))-1.)
             e   = alog10(esbasw)
             table3(i) = 0.1 * 10**(aa+b+c+d+e)
        endif
     enddo

!----------
! smoother
!----------
      i0 = 1580
      tem0 = 0.25*(table3(i0-1) + 2.*table(i0) + table3(i0+1))
      i1 = 1581
      tem1 = 0.25*(table3(i1-1) + 2.*table(i1) + table3(i1+1))
      table3(i0) = tem0
      table3(i1) = tem1

 end subroutine qs_table3


 real function qs_blend(t, p, q)
! Note: this routine is based on "moist" mixing ratio
! Blended mixed phase table
  real, intent(in):: t, p, q
  real es, ap1
  real, parameter:: tmin=table_ice - 160.
  integer it

       ap1 = 10.*dim(t, tmin) + 1.
       ap1 = min(2621., ap1)
        it = ap1
        es = table(it) + (ap1-it)*des(it)
      qs_blend = eps*es*(1.+zvir*q)/p

 end function qs_blend

 subroutine qs_table(n)
      integer, intent(in):: n
      real esupc(200)
      real:: delt=0.1
      real esbasw, tbasw, esbasi, tbasi, tmin, tem, aa, b, c, d, e, esh20
      real wice, wh2o
      integer i

! constants
      esbasw = 1013246.0
       tbasw =     373.16
      esbasi =    6107.1
       tbasi =     273.16

!  compute es over ice between -160c and 0 c.
      tmin = tbasi - 160.
!  see smithsonian meteorological tables page 350.
      do i=1,1600
         tem = tmin+delt*real(i-1)
#ifdef SMITHSONIAN_TABLE
         aa  = -9.09718 *(tbasi/tem-1.)
         b   = -3.56654 *alog10(tbasi/tem)
         c   =  0.876793*(1.-tem/tbasi)
         e   = alog10(esbasi)
         table(i)= 0.1 * 10**(aa+b+c+e)
#else
         table(i) = e00*exp((d2ice*log(tem/t_ice)+Li2*(tem-t_ice)/(tem*t_ice))/rvgas)
#endif
      enddo

!  compute es over water between -20c and 102c.
!  see smithsonian meteorological tables page 350.
      do  i=1,1221
          tem = 253.16+delt*real(i-1)
#ifdef SMITHSONIAN_TABLE
          aa  = -7.90298*(tbasw/tem-1.)
          b   =  5.02808*alog10(tbasw/tem)
          c   = -1.3816e-07*(10**((1.-tem/tbasw)*11.344)-1.)
          d   =  8.1328e-03*(10**((tbasw/tem-1.)*(-3.49149))-1.)
          e   = alog10(esbasw)
          esh20 = 0.1 * 10**(aa+b+c+d+e)
#else
          esh20 = e00*exp((dc_vap*log(tem/t_ice)+Lv0*(tem-t_ice)/(tem*t_ice))/rvgas)
#endif
          if (i <= 200) then
              esupc(i) = esh20
          else
              table(i+1400) = esh20
          endif
      enddo

!  derive blended es over ice and supercooled water between -20c and 0c
      do i=1,200
         tem  = 253.16+delt*real(i-1)
         wice = 0.05*(273.16-tem)
         wh2o = 0.05*(tem-253.16)
         table(i+1400) = wice*table(i+1400)+wh2o*esupc(i)
      enddo

 end subroutine qs_table


 subroutine qsmith(im, km, ks, t, p, q, qs, dqdt)
! input t in deg k; p (pa) : moist pressure
  integer, intent(in):: im, km, ks
  real, intent(in),dimension(im,km):: t, p, q
  real, intent(out),dimension(im,km):: qs
  real, intent(out), optional:: dqdt(im,km)
! local:
  real, parameter:: eps10 = 10.*eps
  real es(im,km)
  real ap1
  real, parameter:: tmin=table_ice - 160.
  integer i, k, it

  if( .not. tables_are_initialized ) then
       call  qsmith_init
  endif

      do k=ks,km
         do i=1,im
            ap1 = 10.*dim(t(i,k), tmin) + 1.
            ap1 = min(2621., ap1)
            it = ap1
            es(i,k) = table(it) + (ap1-it)*des(it)
            qs(i,k) = eps*es(i,k)*(1.+zvir*q(i,k))/p(i,k)
         enddo
      enddo

      if ( present(dqdt) ) then
      do k=ks,km
           do i=1,im
              ap1 = 10.*dim(t(i,k), tmin) + 1.
              ap1 = min(2621., ap1) - 0.5
              it  = ap1
              dqdt(i,k) = eps10*(des(it)+(ap1-it)*(des(it+1)-des(it)))*(1.+zvir*q(i,k))/p(i,k)
           enddo
      enddo
      endif

 end subroutine qsmith


 subroutine neg_adj(ktop, kbot, p1, pt, dp, qv, ql, qr, qi, qs, qg)
! 1d version:
! this is designed for 6-class micro-physics schemes
 integer, intent(in):: ktop, kbot
 real, intent(in):: dp(ktop:kbot), p1(ktop:kbot)
 real, intent(inout), dimension(ktop:kbot)::    &
                                pt, qv, ql, qr, qi, qs, qg
! local:
 real lcpk(ktop:kbot), icpk(ktop:kbot)
 real dq, tmp1, cvm
 integer k

 do k=ktop,kbot
    cvm = c_air + qv(k)*c_vap + (qr(k)+ql(k))*c_liq + (qi(k)+qs(k)+qg(k))*c_ice
    lcpk(k) = (lv00+dc_vap*pt(k)) / cvm
    icpk(k) = (li00+dc_ice*pt(k)) / cvm
 enddo

 do k=ktop, kbot
!-----------
! ice-phase:
!-----------
! if ice<0 borrow from snow
          if( qi(k) < 0. ) then
              qs(k) = qs(k) + qi(k)
              qi(k) = 0.
          endif
! if snow<0 borrow from graupel
          if( qs(k) < 0. ) then
              qg(k) = qg(k) + qs(k)
              qs(k) = 0.
          endif
! if graupel < 0 then borrow from rain
          if ( qg(k) < 0. ) then
               qr(k) = qr(k) + qg(k)
               pt(k) = pt(k) - qg(k)*icpk(k)   ! heating
               qg(k) = 0.
          endif

! liquid phase:
! fix negative rain by borrowing from cloud water
          if ( qr(k) < 0. ) then
               ql(k) = ql(k) + qr(k)
               qr(k) = 0.
          endif
! fix negative cloud water with vapor
          if ( ql(k) < 0. ) then
               qv(k) = qv(k) + ql(k)
               pt(k) = pt(k) - ql(k)*lcpk(k)
               ql(k) = 0.
          endif
 enddo

!-----------------------------------
! fix water vapor; borrow from below
!-----------------------------------
 do k=ktop,kbot-1
    if( qv(k) < 0. ) then
        qv(k+1) = qv(k+1) + qv(k)*dp(k)/dp(k+1)
        qv(k  ) = 0.
    endif
 enddo

! bottom layer; borrow from above
 if( qv(kbot) < 0. .and. qv(kbot-1)>0.) then
             dq = min(-qv(kbot)*dp(kbot), qv(kbot-1)*dp(kbot-1))
     qv(kbot-1) = qv(kbot-1) - dq/dp(kbot-1)
     qv(kbot  ) = qv(kbot  ) + dq/dp(kbot  )
 endif
! if qv is still < 0

 end subroutine neg_adj


 subroutine sg_conv(is, ie, js, je, isd, ied, jsd, jed,               &
                    km, nq, dt, tau,             &
                    delp, phalf, pm, zfull, zhalf, ta, qa, ua, va, w, &
                    u_dt, v_dt, t_dt, q_dt, nqv, nql, nqi, nqr, nqs, nqg, &
                    hydrostatic, phys_hydrostatic)
! Non-precipitating sub-grid scale convective adjustment-mixing
!-------------------------------------------
      logical, intent(in):: hydrostatic, phys_hydrostatic
      integer, intent(in):: is, ie, js, je, km, nq
      integer, intent(in):: isd, ied, jsd, jed
      integer, intent(in):: tau            ! Relaxation time scale
      integer, intent(in):: nqv, nql, nqi  ! vapor, liquid, ice
      integer, intent(in):: nqr, nqs, nqg  !
      real, intent(in):: dt             ! model time step
      real, intent(in):: phalf(is:ie,js:je,km+1)
      real, intent(in):: pm(is:ie,js:je,km)
      real, intent(in):: zfull(is:ie,js:je,km)
      real, intent(in):: zhalf(is:ie,js:je,km+1)
      real, intent(in):: delp(isd:ied,jsd:jed,km)      ! Delta p at each model level
! Output:
      real, intent(inout), dimension(is:ie,js:je,km)::  ta, ua, va
      real, intent(inout):: qa(is:ie,js:je,km,nq)   ! Specific humidity & tracers
      real, intent(inout):: w(isd:ied,jsd:jed,km)
      real, intent(inout):: u_dt(isd:ied,jsd:jed,km)   ! updated u-wind field
      real, intent(inout):: v_dt(isd:ied,jsd:jed,km)   !         v-wind
      real, intent(inout):: t_dt(is:ie,js:je,km)   !         temperature
      real, intent(inout):: q_dt(is:ie,js:je,km,nq) !
!---------------------------Local variables-----------------------------
      real, dimension(is:ie,km):: tvm, u0, v0, w0, t0, gz, hd, pkz, qcon
      real, dimension(is:ie,km+1):: pk, peln
      real q0(is:ie,km,nq)
      real gzh(is:ie)
      real pbot, ri, pt1, pt2, lf, ratio
      real rdt, dh, dh0, dhs, dq, tv, h0, mc, mx,  fra, rk, rz, rcp
      real qsw
      integer:: mcond
      integer kcond
      integer i, j, k, n, m, iq, kk
      real, parameter:: ustar2 = 1.E-6
      real, parameter:: dh_min = 1.E-4

      if ( nqv /= 1 ) then
           call error_mesg ('sg_conv', 'Tracer indexing error', FATAL)
      endif

    rz = rvgas - rdgas          ! rz = zvir * rdgas
    rk = cp_air/rdgas + 1.
   rcp = 1./cp_air

    m = 3
    rdt = 1. / dt
    fra = dt/real(tau)

!------------
! Compute gz: center
!------------
  mcond = 1
  do 1000 j=js,je       ! this main loop can be OpneMPed in j

    do k=mcond,km+1
       do i=is,ie
          peln(i,k) = log(phalf(i,j,k))
            pk(i,k) = exp(kappa*peln(i,k))
       enddo
    enddo

    do k=mcond,km
       do i=is,ie
          u0(i,k) = ua(i,j,k)
          v0(i,k) = va(i,j,k)
          t0(i,k) = ta(i,j,k)
         pkz(i,k) = (pk(i,k+1)-pk(i,k))/(kappa*(peln(i,k+1)-peln(i,k)))
       enddo
    enddo

    if ( .not.hydrostatic ) then
       do k=mcond,km
          do i=is,ie
             w0(i,k) = w(i,j,k)
          enddo
       enddo
    endif

    do iq=1,nq
       do k=mcond,km
          do i=is,ie
             q0(i,k,iq) = qa(i,j,k,iq)
          enddo
       enddo
    enddo


!-----------------
! K-H instability:
!-----------------
   kcond = mcond

   do n=1,m
      ratio = real(n)/real(m)

    if( phys_hydrostatic ) then
       do i=is,ie
          gzh(i) = 0.
       enddo
       do k=km, mcond,-1
          do i=is,ie
           tvm(i,k) = t0(i,k)*(1.+zvir*q0(i,k,nqv))
                tv  = rdgas*tvm(i,k)
            gz(i,k) = gzh(i) + tv*(1.-phalf(i,j,k)/pm(i,j,k))
            hd(i,k) = cp_air*tvm(i,k)+gz(i,k)+0.5*(u0(i,k)**2+v0(i,k)**2)
             gzh(i) = gzh(i) + tv*(peln(i,k+1)-peln(i,k))
          enddo
       enddo
       do i=is,ie
          gzh(i) = 0.
       enddo
    else
       do k=mcond,km
          do i=is,ie
             gz(i,k) = grav*zfull(i,j,k)
             hd(i,k) = cp_air*t0(i,k)+gz(i,k)+0.5*(u0(i,k)**2+v0(i,k)**2+w0(i,k)**2)
          enddo
       enddo
    endif

! PBL: ---------------------------------------
#ifdef USE_PBL
      do 123 i=is,ie
         do k=km-1,km/2,-1
! Richardson number = g*delz * theta / ( del_theta * (del_u**2 + del_v**2) )
            pt1 = t0(i,k)/pkz(i,k)*(1.+zvir*q0(i,k,nqv)-q0(i,k,nql)-q0(i,k,nqi))
            pt2 = t0(i,km)/pkz(i,km)*(1.+zvir*q0(i,km,nqv)-q0(i,km,nql)-q0(i,km,nqi))
            ri = (gz(i,k)-gz(i,km))*(pt1-pt2)/( 0.5*(pt1+pt2)*        &
                ((u0(i,k)-u0(i,km))**2+(v0(i,k)-v0(i,km))**2+ustar2) )
            if ( ri < 1. ) then
! Mix the lowest two layers completely
                 mc = delp(i,j,km-1)*delp(i,j,km)/(delp(i,j,km-1)+delp(i,j,km))
                 do iq=1,nq
                              h0 = mc*(q0(i,km,iq)-q0(i,km-1,iq))
                    q0(i,km-1,iq) = q0(i,km-1,iq) + h0/delp(i,j,km-1)
                    q0(i,km  ,iq) = q0(i,km  ,iq) - h0/delp(i,j,km  )
                 enddo
! u:
                        h0 = mc*(u0(i,km)-u0(i,km-1))
                 u0(i,km-1) = u0(i,km-1) + h0/delp(i,j,km-1)
                 u0(i,km  ) = u0(i,km  ) - h0/delp(i,j,km  )
! v:
                        h0 = mc*(v0(i,km)-v0(i,km-1))
                 v0(i,km-1) = v0(i,km-1) + h0/delp(i,j,km-1)
                 v0(i,km  ) = v0(i,km  ) - h0/delp(i,j,km  )
! h:
                          h0 = mc*(hd(i,km)-hd(i,km-1))
                   hd(i,km-1) = hd(i,km-1) + h0/delp(i,j,km-1)
                   hd(i,km  ) = hd(i,km  ) - h0/delp(i,j,km  )
                if ( .not.hydrostatic ) then
                           h0 = mc*(w0(i,km)-w0(i,km-1))
                    w0(i,km-1) = w0(i,km-1) + h0/delp(i,j,km-1)
                    w0(i,km  ) = w0(i,km  ) - h0/delp(i,j,km  )
                endif
              goto 123
            endif
         enddo
123   continue
#endif
! PBL: ---------------------------------------

      do k=km,kcond+1,-1
         do i=is,ie
            qcon(i,k-1) = q0(i,k-1,nql) + q0(i,k-1,nqi) + q0(i,k-1,nqs) + q0(i,k-1,nqr) + q0(i,k-1,nqg)
            qcon(i,k) = q0(i,k,nql) + q0(i,k,nqi) + q0(i,k,nqs) + q0(i,k,nqr) + q0(i,k,nqg)
! Richardson number = g*delz * theta / ( del_theta * (del_u**2 + del_v**2) )
            pt1 = t0(i,k-1)/pkz(i,k-1)*(1.+zvir*q0(i,k-1,nqv)-qcon(i,k-1))
            pt2 = t0(i,k  )/pkz(i,k  )*(1.+zvir*q0(i,k  ,nqv)-qcon(i,k))
            ri = (gz(i,k-1)-gz(i,k))*(pt1-pt2)/( 0.5*(pt1+pt2)*        &
                ((u0(i,k-1)-u0(i,k))**2+(v0(i,k-1)-v0(i,k))**2+ustar2) )
! Dry convective mixing for K-H instability & CAT (Clear Air Turbulence):
            if ( ri < 1. ) then
! Compute equivalent mass flux: mc
                 mc = ratio * (1.-max(0.0, ri)) ** 2
                 mc = mc*delp(i,j,k-1)*delp(i,j,k)/(delp(i,j,k-1)+delp(i,j,k))
                 do iq=1,nq
                              h0 = mc*(q0(i,k,iq)-q0(i,k-1,iq))
                    q0(i,k-1,iq) = q0(i,k-1,iq) + h0/delp(i,j,k-1)
                    q0(i,k  ,iq) = q0(i,k  ,iq) - h0/delp(i,j,k  )
                 enddo
! u:
                        h0 = mc*(u0(i,k)-u0(i,k-1))
                 u0(i,k-1) = u0(i,k-1) + h0/delp(i,j,k-1)
                 u0(i,k  ) = u0(i,k  ) - h0/delp(i,j,k  )
! v:
                        h0 = mc*(v0(i,k)-v0(i,k-1))
                 v0(i,k-1) = v0(i,k-1) + h0/delp(i,j,k-1)
                 v0(i,k  ) = v0(i,k  ) - h0/delp(i,j,k  )
! h:
                          h0 = mc*(hd(i,k)-hd(i,k-1))
                   hd(i,k-1) = hd(i,k-1) + h0/delp(i,j,k-1)
                   hd(i,k  ) = hd(i,k  ) - h0/delp(i,j,k  )
                if ( .not.hydrostatic ) then
                           h0 = mc*(w0(i,k)-w0(i,k-1))
                    w0(i,k-1) = w0(i,k-1) + h0/delp(i,j,k-1)
                    w0(i,k  ) = w0(i,k  ) - h0/delp(i,j,k  )
                endif
            endif
         enddo
!--------------
! Retrive Temp:
!--------------
      if ( phys_hydrostatic ) then
         kk = k
         do i=is,ie
            t0(i,kk) = (hd(i,kk)-gzh(i)-0.5*(u0(i,kk)**2+v0(i,kk)**2))  &
                     / ( rk - phalf(i,j,kk)/pm(i,j,kk) )
              gzh(i) = gzh(i) + t0(i,kk)*(peln(i,kk+1)-peln(i,kk))
            t0(i,kk) = t0(i,kk) / ( rdgas + rz*q0(i,kk,nqv) )
         enddo
         kk = k-1
         do i=is,ie
            t0(i,kk) = (hd(i,kk)-gzh(i)-0.5*(u0(i,kk)**2+v0(i,kk)**2))  &
                     / ((rk-phalf(i,j,kk)/pm(i,j,kk))*(rdgas+rz*q0(i,kk,nqv)))
         enddo
      else
! Non-hydrostatic under constant volume heating/cooling
         do kk=k-1,k
            do i=is,ie
               t0(i,kk) = rcp*(hd(i,kk)-gz(i,kk)-0.5*(u0(i,kk)**2+v0(i,kk)**2+w0(i,kk)**2))
            enddo
         enddo
      endif
      enddo
   enddo       ! n-loop


!-------------------------
! Moist adjustment/mixing:
!-------------------------
  m = 4

 if( km>k_moist+1 ) then
   do n=1,m

    ratio = real(n)/real(m)

    if ( phys_hydrostatic ) then
       do i=is,ie
          gzh(i) = 0.
       enddo
    endif

!   do k=km,max(kcond,k_moist)+1,-1
    do k=km, kcond+1, -1
       do i=is,ie
          if ( phalf(i,j,k) > p_crt ) then
              qsw = wqsat_moist(t0(i,k-1), q0(i,k-1,nqv), pm(i,j,k-1))

              dh0 = hd(i,k) - hd(i,k-1) - lati*(q0(i,k,nqi)-q0(i,k-1,nqi))
              dhs = dh0 + latv*(q0(i,k,nqv)-qsw        )
              dh  = dh0 + latv*(q0(i,k,nqv)-q0(i,k-1,nqv))

              if ( dhs>0.0 .and. dh>dh_min .and. q0(i,k,nqv)>q0(i,k-1,nqv) ) then
                   mc = ratio*min(1.0, dhs/dh)*    &
                        delp(i,j,k-1)*delp(i,j,k)/(delp(i,j,k-1)+delp(i,j,k))
                          h0 = mc*(hd(i,k) - hd(i,k-1))
                   hd(i,k-1) = hd(i,k-1) + h0/delp(i,j,k-1)
                   hd(i,k  ) = hd(i,k  ) - h0/delp(i,j,k  )
! Perform local mixing of all advected tracers:
                   do iq=1,nq
                                h0 = mc*(q0(i,k,iq)-q0(i,k-1,iq))
                      q0(i,k-1,iq) = q0(i,k-1,iq) + h0/delp(i,j,k-1)
                      q0(i,k  ,iq) = q0(i,k  ,iq) - h0/delp(i,j,k  )
                   enddo
! u:
                          h0 = mc*(u0(i,k)-u0(i,k-1))
                   u0(i,k-1) = u0(i,k-1) + h0/delp(i,j,k-1)
                   u0(i,k  ) = u0(i,k  ) - h0/delp(i,j,k  )
! v:
                          h0 = mc*(v0(i,k)-v0(i,k-1))
                   v0(i,k-1) = v0(i,k-1) + h0/delp(i,j,k-1)
                   v0(i,k  ) = v0(i,k  ) - h0/delp(i,j,k  )
! *** Non-hydrostatic:
                  if ( .not.hydrostatic ) then
                          h0 = mc*(w0(i,k)-w0(i,k-1))
                   w0(i,k-1) = w0(i,k-1) + h0/delp(i,j,k-1)
                   w0(i,k  ) = w0(i,k  ) - h0/delp(i,j,k  )
                  endif
              endif  ! dh check
            endif    ! p_crt check
         enddo
!--------------
! Retrive Temp:
!--------------
       if ( phys_hydrostatic ) then
         kk = k
         do i=is,ie
            t0(i,kk) = (hd(i,kk)-gzh(i)-0.5*(u0(i,kk)**2+v0(i,kk)**2))  &
                     / ( rk - phalf(i,j,kk)/pm(i,j,kk) )
              gzh(i) = gzh(i) + t0(i,kk)*(peln(i,kk+1)-peln(i,kk))
            t0(i,kk) = t0(i,kk) / ( rdgas + rz*q0(i,kk,nqv) )
         enddo
         kk = k-1
         do i=is,ie
            t0(i,kk) = (hd(i,kk)-gzh(i)-0.5*(u0(i,kk)**2+v0(i,kk)**2))  &
                     / ((rk-phalf(i,j,kk)/pm(i,j,kk))*(rdgas+rz*q0(i,kk,nqv)))
         enddo
       else
! Non-hydrostatic under constant volume heating/cooling
         do kk=k-1,k
            do i=is,ie
               t0(i,kk) = rcp*(hd(i,kk)-gz(i,kk)-0.5*(u0(i,kk)**2+v0(i,kk)**2+w0(i,kk)**2))
            enddo
         enddo
       endif
      enddo
   enddo       ! n-loop
 endif      ! k_moist check

   if ( fra < 1. ) then
      do k=mcond,km
         do i=is,ie
            t0(i,k) = ta(i,j,k) + (t0(i,k) - ta(i,j,k))*fra
            u0(i,k) = ua(i,j,k) + (u0(i,k) - ua(i,j,k))*fra
            v0(i,k) = va(i,j,k) + (v0(i,k) - va(i,j,k))*fra
         enddo
      enddo

      if ( .not.hydrostatic ) then
      do k=mcond,km
         do i=is,ie
            w0(i,k) = w(i,j,k) + (w0(i,k) - w(i,j,k))*fra
         enddo
      enddo
      endif

      do iq=1,nq
         do k=mcond,km
            do i=is,ie
               q0(i,k,iq) = qa(i,j,k,iq) + (q0(i,k,iq) - qa(i,j,k,iq))*fra
            enddo
         enddo
      enddo
   endif

!---
   do k=mcond, km
      do i=is, ie
         u_dt(i,j,k) = u_dt(i,j,k) + rdt*(u0(i,k) - ua(i,j,k))
         v_dt(i,j,k) = v_dt(i,j,k) + rdt*(v0(i,k) - va(i,j,k))
         t_dt(i,j,k) = t_dt(i,j,k) + rdt*(t0(i,k) - ta(i,j,k))
! Updates:
!          ua(i,j,k) = u0(i,k)
!          va(i,j,k) = v0(i,k)
           ta(i,j,k) = t0(i,k)
      enddo
   enddo

   do iq=1,nq
      do k=mcond, km
         do i=is, ie
            q_dt(i,j,k,iq) = q_dt(i,j,k,iq) + rdt*(q0(i,k,iq)-qa(i,j,k,iq))
! q updated
              qa(i,j,k,iq) = q0(i,k,iq)
         enddo
      enddo
   enddo

   if ( .not. hydrostatic ) then
      do k=mcond,km
         do i=is,ie
            w(i,j,k) = w0(i,k)   ! w updated
         enddo
      enddo
   endif

1000 continue

 end subroutine sg_conv

 subroutine dbzcalc(qv, qr, qs, qg, pt, delp, dz, &
      dbz, maxdbz, allmax, is, ie, js, je, ks, ke, &
      in0r, in0s, in0g, iliqskin)

   !Code from Mark Stoelinga's dbzcalc.f from the RIP package. 
   !Currently just using values taken directly from that code, which is
   ! consistent for the MM5 Reisner-2 microphysics. From that file:

!     This routine computes equivalent reflectivity factor (in dBZ) at
!     each model grid point.  In calculating Ze, the RIP algorithm makes
!     assumptions consistent with those made in an early version
!     (ca. 1996) of the bulk mixed-phase microphysical scheme in the MM5
!     model (i.e., the scheme known as "Resiner-2").  For each species:
!
!     1. Particles are assumed to be spheres of constant density.  The
!     densities of rain drops, snow particles, and graupel particles are
!     taken to be rho_r = rho_l = 1000 kg m^-3, rho_s = 100 kg m^-3, and
!     rho_g = 400 kg m^-3, respectively. (l refers to the density of
!     liquid water.)
!
!     2. The size distribution (in terms of the actual diameter of the
!     particles, rather than the melted diameter or the equivalent solid
!     ice sphere diameter) is assumed to follow an exponential
!     distribution of the form N(D) = N_0 * exp( lambda*D ).
!
!     3. If in0X=0, the intercept parameter is assumed constant (as in
!     early Reisner-2), with values of 8x10^6, 2x10^7, and 4x10^6 m^-4,
!     for rain, snow, and graupel, respectively.  Various choices of
!     in0X are available (or can be added).  Currently, in0X=1 gives the
!     variable intercept for each species that is consistent with
!     Thompson, Rasmussen, and Manning (2004, Monthly Weather Review,
!     Vol. 132, No. 2, pp. 519-542.)
!
!     4. If iliqskin=1, frozen particles that are at a temperature above
!     freezing are assumed to scatter as a liquid particle.
!
!     More information on the derivation of simulated reflectivity in RIP
!     can be found in Stoelinga (2005, unpublished write-up).  Contact
!     Mark Stoelinga (stoeling@atmos.washington.edu) for a copy.  

   integer, intent(IN) :: is, ie, js, je, ks, ke
   real,    intent(IN),  dimension(is:, js:, ks:) :: qv, qr, qs, qg, pt, delp, dz
   real,    intent(OUT), dimension(is:, js:, ks:) :: dbz
   real,    intent(OUT), dimension(is:, js:)      :: maxdbz
   logical, intent(IN) :: in0r, in0s, in0g, iliqskin
   real,    intent(OUT) :: allmax

   !Parameters for constant intercepts (in0[rsg] = .false.)
   real, parameter :: rn0_r = 8.e6 ! m^-4
   real, parameter :: rn0_s = 2.e7 ! m^-4
   real, parameter :: rn0_g = 4.e6 ! m^-4

   !Constants for variable intercepts
   real, parameter :: r1=1.e-15
   real, parameter :: ron=8.e6
   real, parameter :: ron2=1.e10
   real, parameter :: son=2.e7
   real, parameter :: gon=5.e7
   real, parameter :: ron_min = 8.e6
   real, parameter :: ron_qr0 = 0.00010
   real, parameter :: ron_delqr0 = 0.25*ron_qr0
   real, parameter :: ron_const1r = (ron2-ron_min)*0.5
   real, parameter :: ron_const2r = (ron2+ron_min)*0.5

   !Other constants
   real, parameter :: gamma_seven = 720.
   real, parameter :: rho_r = rhor ! 1000. kg m^-3
   real, parameter :: rho_s = 100.   ! kg m^-3
   real, parameter :: rho_g = 400.   ! kg m^-3
   real, parameter :: alpha = 0.224
   real, parameter :: factor_r = gamma_seven * 1.e18 * (1./(pi*rho_r))**1.75
   real, parameter :: factor_s = gamma_seven * 1.e18 * (1./(pi*rho_s))**1.75 &
        * (rho_s/rho_r)**2 * alpha
   real, parameter :: factor_g = gamma_seven * 1.e18 * (1./(pi*rho_g))**1.75 &
        * (rho_g/rho_r)**2 * alpha

   integer :: i,j,k
   real :: factorb_s, factorb_g, rhoair
   real :: temp_c, pres, sonv, gonv, ronv, z_e
   real :: q1, q2, q3

   maxdbz = 0.
   allmax = 0.

   do k=ks, ke
   do j=js, je
   do i=is, ie
      
      rhoair = -delp(i,j,k)/(grav*dz(i,j,k)) ! air density
      
      !      Adjust factor for brightband, where snow or graupel particle
      !      scatters like liquid water (alpha=1.0) because it is assumed to
      !      have a liquid skin.
      
      !lmh: celkel in dbzcalc.f presumably freezing temperature
      if (iliqskin .and. pt(i,j,k) .gt. tice) then
         factorb_s=factor_s/alpha
         factorb_g=factor_g/alpha
      else
         factorb_s=factor_s
         factorb_g=factor_g
      endif

      !Calculate variable intercept parameters if necessary
      !  using definitions from Thompson et al
      if (in0s) then
         temp_c = min(-0.001, pt(i,j,k) - tice)
         sonv = min(2.0e8, 2.0e6*exp(-0.12*temp_c))
      else
         sonv = rn0_s
      end if

      q1 = max(0., qg(i,j,k))
      q2 = max(0., qr(i,j,k))
      q3 = max(0., qs(i,j,k))

      if (in0g) then
         gonv = gon
         if ( qg(i,j,k) > r1) then
            gonv = 2.38 * (pi * rho_g / (rhoair*q1))**0.92
            gonv = max(1.e4, min(gonv,gon))
         end if
      else
         gonv = rn0_g
      end if

      if (in0r) then
         ronv = ron2
         if (qr(i,j,k) > r1 ) then
            ronv = ron_const1r * tanh((ron_qr0-q2)/ron_delqr0) + ron_const2r
         end if
      else
         ronv = rn0_r
      end if

      !Total equivalent reflectivity: mm^6 m^-3
      z_e =   factor_r  * (rhoair*q2)**1.75 / ronv**.75    & ! rain
            + factorb_s * (rhoair*q3)**1.75 / sonv**.75    & ! snow
            + factorb_g * (rhoair*q1)**1.75 / gonv**.75      ! graupel
      
      z_e = max(z_e,0.01)
      dbz(i,j,k) = 10. * log10(z_e)

      maxdbz(i,j) = max(dbz(i,j,k), maxdbz(i,j))
      allmax      = max(dbz(i,j,k), allmax)

   enddo
   enddo
   enddo

 end subroutine dbzcalc

 real function g_sum(p, ifirst, ilast, jfirst, jlast, area, mode)
!-------------------------
! Quick local sum algorithm
!-------------------------
 use mpp_mod,           only: mpp_sum
 integer, intent(IN) :: ifirst, ilast
 integer, intent(IN) :: jfirst, jlast
 integer, intent(IN) :: mode  ! if ==1 divided by area
 real, intent(IN) :: p(ifirst:ilast,jfirst:jlast)      ! field to be summed
 real, intent(IN) :: area(ifirst:ilast,jfirst:jlast)
 integer :: i,j
 real gsum

   if( global_area < 0. ) then
       global_area = 0.
       do j=jfirst,jlast
          do i=ifirst,ilast
             global_area = global_area + area(i,j)
          enddo
       enddo
       call mpp_sum(global_area)
   end if

   gsum = 0.
   do j=jfirst,jlast
      do i=ifirst,ilast
         gsum = gsum + p(i,j)*area(i,j)
      enddo
   enddo
   call mpp_sum(gsum)

   if ( mode==1 ) then
        g_sum = gsum / global_area
   else
        g_sum = gsum
   endif

 end function g_sum

 subroutine interpolate_z(is, ie, js, je, km, zl, hght, a3, a2)

 integer,  intent(in):: is, ie, js, je, km
 real, intent(in):: hght(is:ie,js:je,km+1)  ! hght(k) > hght(k+1)
 real, intent(in):: a3(is:ie,js:je,km)
 real, intent(in):: zl
 real, intent(out):: a2(is:ie,js:je)
! local:
 real zm(km)
 integer i,j,k


!$OMP parallel do default(none) shared(is,ie,js,je,km,hght,zl,a2,a3) private(zm)
 do j=js,je
    do 1000 i=is,ie
       do k=1,km
          zm(k) = 0.5*(hght(i,j,k)+hght(i,j,k+1))
       enddo
       if( zl >= zm(1) ) then
           a2(i,j) = a3(i,j,1)
       elseif ( zl <= zm(km) ) then
           a2(i,j) = a3(i,j,km)
       else 
           do k=1,km-1
              if( zl <= zm(k) .and. zl >= zm(k+1) ) then
                  a2(i,j) = a3(i,j,k) + (a3(i,j,k+1)-a3(i,j,k))*(zm(k)-zl)/(zm(k)-zm(k+1))
                  go to 1000
              endif
           enddo
       endif
1000   continue
 enddo

 end subroutine interpolate_z

end module lin_cld_microphys_mod
