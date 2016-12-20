module lag_traj
!$$$ module documentation block
!           .      .    .                                       .
! module:   lag_traj
!   prgmmr: meunier          org:                     date: 2009-03-11
!
! abstract:  This module contain, the trajectory model used to forecast
!            the position of superpressure balloon when used in data
!            assimilation. 
!            - A model based on a Runge-Kutta (4th order)
!            algorithm is fully implemented (non-linear, linear and 
!            adjoint).
!            - A model based on a Runge-Kutta (2nd order)
!            algorithm is fully implemented (non-linear, linear and 
!            adjoint).
!
! module history log:
!   2009-03-11  meunier
!   2011-08-01  lueken  - replaced F90 with f90 (no machine logic),
!                         and remove double &
!
! Subroutines Included: (public)
!   - lag_initlparam : Initialisation subroutine that must be called before
!                      using the models
!
!   - lag_rk2iter_nl : Non-linear model (retrieve also the parameters needed
!                      by the tangent linear and adjoint versions
!   - lag_rk2iter_tl : Tangent linear model
!   - lag_rk2iter_ad : Adjoint model
!
! Functions Included: (public)
!
! variable definitions:
!
! Warnings:  The wind field need to be comformed to those used in lag_fields
!
! attributes:
!   language: f90
!   machine:  
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero,one,two,rearth,pi

  ! Interpolation module  
  use lag_interp, only: lag_int3d_nl,lag_int3d_tl,lag_int3d_ad

  implicit none

  private

  public:: lag_poledist,lag_trajfail

  public:: lag_stepduration,lag_iteduration
  public:: lag_rk4itenpara_r,lag_rk4itenpara_i
  public:: lag_rk2itenpara_r,lag_rk2itenpara_i

  public:: lag_initlparam
  public:: lag_d_haversin
  public:: lag_rk2iter_nl,lag_rk2iter_tl,lag_rk2iter_ad

  ! Number of parameters needed by the tangent linear model, for one step
  ! of the Runge-Kutta (4th order) trajectory model
  integer(i_kind),parameter::lag_rk4stepnpara_r=57  ! real numbers
  integer(i_kind),parameter::lag_rk4stepnpara_i=32  ! integer numbers
  ! Number of parameters needed by the tangent linear model, for one step
  ! of the Runge-Kutta (2nd order) trajectory model
  integer(i_kind),parameter::lag_rk2stepnpara_r=29  ! real numbers
  integer(i_kind),parameter::lag_rk2stepnpara_i=16  ! integer numbers

  real(r_kind)::lag_stepduration= 900.0_r_kind  ! Duration in sec. of 1 step

  real(r_kind)::lag_iteduration   ! Duration in sec. of 1 iteration (composed
                                  ! of several steps)

  ! Value used if the balloon is to close from the pole
  real(r_kind),parameter::lag_trajfail= -99999._r_kind
  ! Distance to the pole in latitude from which the trajectory is not computed
  ! anymore. (in radians).
  real(r_kind),parameter::lag_poledist= 8.72664626e-3_r_kind !(=0.5deg)


  ! Number of parameters needed by the tangent linear model, for one iteration
  ! of the Runge-Kutta (4th order) trajectory model
  integer(i_kind)::lag_rk4itenpara_r,lag_rk4itenpara_i
  ! Number of parameters needed by the tangent linear model, for one iteration
  ! of the Runge-Kutta (2nd order) trajectory model
  integer(i_kind)::lag_rk2itenpara_r,lag_rk2itenpara_i

  ! Number of steps needed to achieve one iteration
  integer(i_kind)::lag_nstepiter

  ! Array indexes for the storage of the NL RK4 model parameters
  integer(i_kind),parameter::irk4_loc_0=1
  integer(i_kind),parameter::irk4_loc_half_p =9
  integer(i_kind),parameter::irk4_loc_half_pp=17
  integer(i_kind),parameter::irk4_loc_1_p=25

  integer(i_kind),parameter::irk4_wei_0=1
  integer(i_kind),parameter::irk4_wei_half_p =9
  integer(i_kind),parameter::irk4_wei_half_pp=17
  integer(i_kind),parameter::irk4_wei_1_p=25

  integer(i_kind),parameter::irk4_ldu_0=33
  integer(i_kind),parameter::irk4_ldu_half_p =35
  integer(i_kind),parameter::irk4_ldu_half_pp=37
  integer(i_kind),parameter::irk4_ldu_1_p=39
  integer(i_kind),parameter::irk4_ldv_0=41
  integer(i_kind),parameter::irk4_ldv_half_p =43
  integer(i_kind),parameter::irk4_ldv_half_pp=45
  integer(i_kind),parameter::irk4_ldv_1_p=47

  integer(i_kind),parameter::irk4_tstep=49

  integer(i_kind),parameter::irk4_lon_0=50
  integer(i_kind),parameter::irk4_lon_half_p =52
  integer(i_kind),parameter::irk4_lon_half_pp=54
  integer(i_kind),parameter::irk4_lon_1_p=56

  ! Array indexes for the storage of the NL RK2 model parameters
  integer(i_kind),parameter::irk2_loc_0=1
  integer(i_kind),parameter::irk2_loc_1_p=9

  integer(i_kind),parameter::irk2_wei_0=1
  integer(i_kind),parameter::irk2_wei_1_p=9

  integer(i_kind),parameter::irk2_ldu_0=17
  integer(i_kind),parameter::irk2_ldu_1_p=19
  integer(i_kind),parameter::irk2_ldv_0=21
  integer(i_kind),parameter::irk2_ldv_1_p=23

  integer(i_kind),parameter::irk2_tstep=25

  integer(i_kind),parameter::irk2_lon_0=26
  integer(i_kind),parameter::irk2_lon_1_p=28


  contains


  ! ------------------------------------------------------------------------
  ! Calculate derived global variables (required before using the models
  ! for iterations)
  ! ------------------------------------------------------------------------
  subroutine lag_initlparam
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_initlparam
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    lag_nstepiter=ceiling(lag_iteduration/lag_stepduration)

    lag_rk4itenpara_r=lag_nstepiter*lag_rk4stepnpara_r
    lag_rk4itenpara_i=lag_nstepiter*lag_rk4stepnpara_i+1
    lag_rk2itenpara_r=lag_nstepiter*lag_rk2stepnpara_r
    lag_rk2itenpara_i=lag_nstepiter*lag_rk2stepnpara_i+1

  end subroutine lag_initlparam
  ! ------------------------------------------------------------------------
  ! Utility subroutine to add a lattitude/longitude deplacement to a given point
  ! given in lon/lat coordinates (check the criteria one latitude and correct
  ! the longitude to remain between 0 and 2pi)
  ! ------------------------------------------------------------------------
  subroutine add_position(lon,lat,dlon,dlat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    add_position
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lon,lat
!    dlon,dlat
!
!   output argument list:
!    lon,lat
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),intent(inout) :: lon,lat
    real(r_kind),intent(in   ) :: dlon,dlat

    ! Check for a previous failure if m2lon
    if (dlon==lag_trajfail .or. lon==lag_trajfail .or. lat==lag_trajfail) then
       lat=lag_trajfail; lon=lag_trajfail
       return
    end if

    ! Pole crossing problem to handle first :
    if (abs(lat+dlat)< pi/two-lag_poledist) then
       lat=lat+dlat
       lon=lon+dlon
    else  ! Too close of the pole
       lat=lag_trajfail
       lon=lag_trajfail
       return
    end if

    ! Because we use radians : make longitude remain between 0 and 2pi
    do while (lon<zero) 
       lon=lon+two*pi
    end do
    do while (lon>=two*pi)
       lon=lon-two*pi
    end do

  end subroutine add_position

  ! ------------------------------------------------------------------------
  ! Convert a delta_y (meters) in a delta_latitude (radians) - linear operator
  ! ------------------------------------------------------------------------
  function m2lat_tl(rv_dy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m2lat_tl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    rv_dy
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    ! distance in meters along a latitude line
    real(r_kind),intent(in   ) :: rv_dy
    ! equivalent distance in radians
    real(r_kind)::m2lat_tl          

    m2lat_tl=rv_dy/rearth
  end function m2lat_tl

  ! ------------------------------------------------------------------------
  ! Convert a delta_x (meters) in a delta_longitude (radians)
  ! ------------------------------------------------------------------------
  function m2lon_nl(rv_orig_lat,rv_dx,lspec_r)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m2lon_nl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    rv_orig_lat
!    rv_dx
!
!   output argument list:
!    lspec_r
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    ! latitude of the starting point
    real(r_kind)                      ,intent(in   ) :: rv_orig_lat
    ! distance in meters along a longitude line
    real(r_kind)                      ,intent(in   ) :: rv_dx
    ! parameters needed for the TL and adjoint (optional)
    real(r_kind),dimension(2),optional,intent(  out) :: lspec_r
    ! equivalent distance in radians
    real(r_kind)::m2lon_nl

    ! Check distance to pole
    if (abs(rv_orig_lat-pi/two)<lag_poledist .or. &
        abs(rv_orig_lat+pi/two)<lag_poledist) then
       m2lon_nl=lag_trajfail
       if (present(lspec_r)) lspec_r=zero
    end if

    ! Ok...
    m2lon_nl=rv_dx/(rearth*cos(rv_orig_lat))
    
    if (present(lspec_r)) then
       lspec_r(1)=one/(rearth*cos(rv_orig_lat))
       lspec_r(2)=(rv_dx/rearth)*&
          sin(rv_orig_lat)/(cos(rv_orig_lat)**2)
    end if
  end function m2lon_nl

  ! ------------------------------------------------------------------------
  ! Tangent linear version
  ! ------------------------------------------------------------------------
  function m2lon_tl(lspec_r,rv_dorig_lat,rv_dx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m2lon_tl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lspec_r
!    rv_dorig_lat
!    rv_dx
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),dimension(2),intent(in   ) :: lspec_r
    real(r_kind)             ,intent(in   ) :: rv_dorig_lat
    real(r_kind)             ,intent(in   ) :: rv_dx
    real(r_kind)::m2lon_tl

    m2lon_tl=lspec_r(1)*rv_dx + lspec_r(2)*rv_dorig_lat
  end function m2lon_tl

  ! ------------------------------------------------------------------------
  ! Adjoint version
  ! ------------------------------------------------------------------------
  subroutine m2lon_ad(lspec_r,ad_m2lon_l,ad_rv_dorig_lat,ad_rv_dx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m2lon_ad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lspec_r
!    ad_m2lon_l
!    ad_rv_dorig_lat
!    ad_rv_dx
!
!   output argument list:
!    ad_rv_dorig_lat
!    ad_rv_dx
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),dimension(2),intent(in   ) :: lspec_r
    real(r_kind)             ,intent(in   ) :: ad_m2lon_l
    real(r_kind)             ,intent(inout) :: ad_rv_dorig_lat
    real(r_kind)             ,intent(inout) :: ad_rv_dx

    ad_rv_dorig_lat =ad_rv_dorig_lat+lspec_r(2)*ad_m2lon_l
    ad_rv_dx        =ad_rv_dx       +lspec_r(1)*ad_m2lon_l
  end subroutine m2lon_ad


  ! ------------------------------------------------------------------------
  ! Haversine function
  ! ------------------------------------------------------------------------
  function lag_haversin(rv_theta)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_haversin
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    rv_theta
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),intent(in   ) :: rv_theta
    real(r_kind)::lag_haversin
    lag_haversin=sin(rv_theta/2)**2
  end function lag_haversin
  ! ------------------------------------------------------------------------
  ! Inverse-haversine function
  ! ------------------------------------------------------------------------
  function lag_haversin_inv(rv_z)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_haversin_inv
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    rv_z
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),intent(in   )::rv_z
    real(r_kind)::lag_haversin_inv
    lag_haversin_inv=two*asin(sqrt(rv_z))
  end function lag_haversin_inv

  ! ------------------------------------------------------------------------
  ! Haversine Formula : Calculate the distance between 2 lat/lon points
  ! ------------------------------------------------------------------------
  function lag_d_haversin(rv_lon1,rv_lat1,rv_lon2,rv_lat2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_d_haversin
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    rv_lon1,rv_lat1,rv_lon2,rv_lat2
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),intent(in   ) :: rv_lon1,rv_lat1,rv_lon2,rv_lat2
    real(r_kind)::lag_d_haversin

    ! Detect missing values
    if(rv_lon1==lag_trajfail .or. rv_lat1==lag_trajfail .or. &
       rv_lon2==lag_trajfail .or. rv_lat2==lag_trajfail) then
       lag_d_haversin=lag_trajfail
       return
    end if

    lag_d_haversin=lag_haversin_inv( &
       lag_haversin(rv_lat1-rv_lat2) + &
       cos(rv_lat1)*cos(rv_lat2)*lag_haversin(rv_lon1-rv_lon2) &
       )*rearth
  end function lag_d_haversin

  ! ------------------------------------------------------------------------
  ! Give the distance in meters between two lattitude circles
  ! ------------------------------------------------------------------------
  function lag_d_lat(rv_lat1,rv_lat2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_d_lat
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    rv_lat1,rv_lat2
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),intent(in   ) :: rv_lat1,rv_lat2
    real(r_kind)::lag_d_lat

    ! Detect missing values
    if(rv_lat1==lag_trajfail .or. rv_lat2==lag_trajfail) then
       lag_d_lat=lag_trajfail
       return
    end if

    lag_d_lat=(rv_lat2-rv_lat1)*rearth
  end function lag_d_lat

  ! ------------------------------------------------------------------------
  ! Give the distance in meters between two longitude circles, given the
  ! latitude of the first point
  ! ------------------------------------------------------------------------
  function lag_d_lon(rv_lon1,rv_lon2,rv_lat1)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_d_lon
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    rv_lon1,rv_lon2,rv_lat1
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),intent(in   ) :: rv_lon1,rv_lon2,rv_lat1
    real(r_kind)::lag_d_lon

    real(r_kind)::angle_diff

    ! Detect missing values
    if(rv_lon1==lag_trajfail .or. rv_lon2==lag_trajfail .or. &
       rv_lat1==lag_trajfail) then
       lag_d_lon=lag_trajfail
       return
    end if

    angle_diff=rv_lon2-rv_lon1
    if (angle_diff> pi) angle_diff=angle_diff-two*pi
    if (angle_diff<-pi) angle_diff=angle_diff+two*pi

    lag_d_lon=angle_diff*rearth*cos(rv_lat1)
  end function lag_d_lon


  ! ------------------------------------------------------------------------
  ! RK4 MODEL IMPLEMENTATION FOR ONE TIME STEP -----------------------------
  ! ------------------------------------------------------------------------

  
  ! ------------------------------------------------------------------------
  ! Implementation of the Runge-Kutta algorithm : Non linear
  ! ------------------------------------------------------------------------
  subroutine lag_rk4_nl(lon,lat,p,ufield,vfield,tstep,lspec_i,lspec_r)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_rk4_nl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lon,lat,p
!    ufield,vfield
!    tstep
!
!   output argument list:
!    lon,lat,p
!    lspec_i
!    lspec_r
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    ! longitude, latitude, pressure of the balloon
    real(r_kind)                                          ,intent(inout) :: lon,lat,p
    ! components of the wind fields (2nd dimension for vertical level)
    real(r_kind)   ,dimension(:,:)                        ,intent(in   ) :: ufield,vfield
    ! Time step (seconds)
    real(r_kind)                                          ,intent(in   ) :: tstep
    ! Parameters for the TL and Adjoint (optional)
    integer(i_kind),dimension(lag_rk4stepnpara_i),optional,intent(  out) :: lspec_i
    real(r_kind)   ,dimension(lag_rk4stepnpara_r),optional,intent(  out) :: lspec_r

    logical::lv_spec

    ! Calculated positions
    real(r_kind),dimension(3)::pos_half_p,pos_half_pp,pos_1_p,pos_1_pp
    ! Interpolated winds
    real(r_kind)::rv_intu_0,rv_intu_half_p,rv_intu_half_pp,rv_intu_1_p
    real(r_kind)::rv_intv_0,rv_intv_half_p,rv_intv_half_pp,rv_intv_1_p
    ! Temporary
    integer(i_kind),dimension(8) ::tmp_ispec_int
    real(r_kind)   ,dimension(10)::tmp_rspec_int
    real(r_kind)   ,dimension(2) ::tmp_rspec_lon
    real(r_kind)::dlon_tmp,dlat_tmp

    lv_spec=present(lspec_i) .and. present(lspec_r)

    ! Detect missing values
    if(lon==lag_trajfail .or. lat==lag_trajfail) then
       if (lv_spec) then
          lspec_i=0; lspec_r=zero
       end if
       lon=lag_trajfail; lat=lag_trajfail; return
    end if

    ! 1st step of the 4th order scheme : calculate the approximate
    ! of the half-timestep position, using an explicit scheme
    if (lv_spec) then
       rv_intu_0=lag_int3d_nl(ufield,lon,lat,p,tmp_ispec_int,tmp_rspec_int)
       lspec_i(irk4_loc_0:(irk4_loc_0+7))=tmp_ispec_int
       lspec_r(irk4_wei_0:(irk4_wei_0+7))=tmp_rspec_int(1:8)
       lspec_r(irk4_ldu_0:(irk4_ldu_0+1))=tmp_rspec_int(9:10)
       rv_intv_0=lag_int3d_nl(vfield,lon,lat,p,tmp_ispec_int,tmp_rspec_int)
       lspec_r(irk4_ldv_0:(irk4_ldv_0+1))=tmp_rspec_int(9:10)
       dlat_tmp=m2lat_tl(tstep/two*rv_intv_0)
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep/two*rv_intu_0,tmp_rspec_lon)
       lspec_r(irk4_lon_0:(irk4_lon_0+1))=tmp_rspec_lon
    else
       rv_intu_0=lag_int3d_nl(ufield,lon,lat,p)
       rv_intv_0=lag_int3d_nl(vfield,lon,lat,p)
       dlat_tmp=m2lat_tl(tstep/two*rv_intv_0)
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep/two*rv_intu_0)
    end if
    pos_half_p(1)=lon; pos_half_p(2)=lat
    pos_half_p(3)=p
    call add_position(pos_half_p(1),pos_half_p(2),dlon_tmp,dlat_tmp)

    if (pos_half_p(1)==lag_trajfail) then
       if (lv_spec) then
          lspec_i=0; lspec_r=zero
       end if
       lon=lag_trajfail; lat=lag_trajfail; return
    end if

    ! 2nd step : calculate a second approximate of the half-timestep
    ! position, using an implicit scheme (with guest given by the position
    ! previously calculated)
    if (lv_spec) then
       rv_intu_half_p=lag_int3d_nl(ufield,&
          pos_half_p(1),pos_half_p(2),pos_half_p(3),&
          tmp_ispec_int,tmp_rspec_int)
       lspec_i(irk4_loc_half_p:(irk4_loc_half_p+7))=tmp_ispec_int
       lspec_r(irk4_wei_half_p:(irk4_wei_half_p+7))=tmp_rspec_int(1:8)
       lspec_r(irk4_ldu_half_p:(irk4_ldu_half_p+1))=tmp_rspec_int(9:10)
       rv_intv_half_p=lag_int3d_nl(vfield,&
          pos_half_p(1),pos_half_p(2),pos_half_p(3),&
          tmp_ispec_int,tmp_rspec_int)
       lspec_r(irk4_ldv_half_p:(irk4_ldv_half_p+1))=tmp_rspec_int(9:10)
       dlat_tmp=m2lat_tl(tstep/two*rv_intv_half_p)
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep/two*rv_intu_half_p,tmp_rspec_lon)
       lspec_r(irk4_lon_half_p:(irk4_lon_half_p+1))=tmp_rspec_lon
    else
       rv_intu_half_p=lag_int3d_nl(ufield,&
          pos_half_p(1),pos_half_p(2),pos_half_p(3))
       rv_intv_half_p=lag_int3d_nl(vfield,&
          pos_half_p(1),pos_half_p(2),pos_half_p(3))
       dlat_tmp=m2lat_tl(tstep/two*rv_intv_half_p)
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep/two*rv_intu_half_p)
    end if
    pos_half_pp(1)=lon; pos_half_pp(2)=lat
    pos_half_pp(3)=p
    call add_position(pos_half_pp(1),pos_half_pp(2),dlon_tmp,dlat_tmp)

    if (pos_half_pp(1)==lag_trajfail) then
       if (lv_spec) then
          lspec_i=0; lspec_r=zero
       end if
       lon=lag_trajfail; lat=lag_trajfail; return
    end if

    ! 3rd step : calculate the first estimate of the final position, using
    ! a midpoint rule predictor
    if (lv_spec) then
       rv_intu_half_pp=lag_int3d_nl(ufield,&
          pos_half_pp(1),pos_half_pp(2),pos_half_pp(3),&
          tmp_ispec_int,tmp_rspec_int)
       lspec_i(irk4_loc_half_pp:(irk4_loc_half_pp+7))=tmp_ispec_int
       lspec_r(irk4_wei_half_pp:(irk4_wei_half_pp+7))=tmp_rspec_int(1:8)
       lspec_r(irk4_ldu_half_pp:(irk4_ldu_half_pp+1))=tmp_rspec_int(9:10)
       rv_intv_half_pp=lag_int3d_nl(vfield,&
          pos_half_pp(1),pos_half_pp(2),pos_half_pp(3),&
          tmp_ispec_int,tmp_rspec_int)
       lspec_r(irk4_ldv_half_pp:(irk4_ldv_half_pp+1))=tmp_rspec_int(9:10)
       dlat_tmp=m2lat_tl(tstep*rv_intv_half_pp)
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep*rv_intu_half_pp,tmp_rspec_lon)
       lspec_r(irk4_lon_half_pp:(irk4_lon_half_pp+1))=tmp_rspec_lon
    else
       rv_intu_half_pp=lag_int3d_nl(ufield,&
          pos_half_pp(1),pos_half_pp(2),pos_half_pp(3))
       rv_intv_half_pp=lag_int3d_nl(vfield,&
          pos_half_pp(1),pos_half_pp(2),pos_half_pp(3))
       dlat_tmp=m2lat_tl(tstep*rv_intv_half_pp)
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep*rv_intu_half_pp)
    end if
    pos_1_p(1)=lon; pos_1_p(2)=lat
    pos_1_p(3)=p
    call add_position(pos_1_p(1),pos_1_p(2),dlon_tmp,dlat_tmp)

    if (pos_1_p(1)==lag_trajfail) then
       if (lv_spec) then
          lspec_i=0; lspec_r=zero
       end if
       lon=lag_trajfail; lat=lag_trajfail; return
    end if

    ! Final step : calculate the final position  of the balloon using a
    ! Simpson's rules corrector
    if (lv_spec) then
       rv_intu_1_p=lag_int3d_nl(ufield,&
          pos_1_p(1),pos_1_p(2),pos_1_p(3),&
          tmp_ispec_int,tmp_rspec_int)
       lspec_i(irk4_loc_1_p:(irk4_loc_1_p+7))=tmp_ispec_int
       lspec_r(irk4_wei_1_p:(irk4_wei_1_p+7))=tmp_rspec_int(1:8)
       lspec_r(irk4_ldu_1_p:(irk4_ldu_1_p+1))=tmp_rspec_int(9:10)
       rv_intv_1_p=lag_int3d_nl(vfield,&
          pos_1_p(1),pos_1_p(2),pos_1_p(3),&
          tmp_ispec_int,tmp_rspec_int)
       lspec_r(irk4_ldv_1_p:(irk4_ldv_1_p+1))=tmp_rspec_int(9:10)
       dlat_tmp=m2lat_tl(tstep/6_r_kind*( &
          rv_intv_0 + &
          rv_intv_half_p *two + rv_intv_half_pp *two +&
          rv_intv_1_p))
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep/6_r_kind*( &
          rv_intu_0 + &
          rv_intu_half_p *two + rv_intu_half_pp *two +&
          rv_intu_1_p) , tmp_rspec_lon)
       lspec_r(irk4_lon_1_p:(irk4_lon_1_p+1))=tmp_rspec_lon
    else
       rv_intu_1_p=lag_int3d_nl(ufield,&
          pos_1_p(1),pos_1_p(2),pos_1_p(3))
       rv_intv_1_p=lag_int3d_nl(vfield,&
          pos_1_p(1),pos_1_p(2),pos_1_p(3))
       dlat_tmp=m2lat_tl(tstep/6_r_kind*( &
          rv_intv_0 + &
          rv_intv_half_p *two + rv_intv_half_pp *two +&
          rv_intv_1_p))
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep/6_r_kind*( &
          rv_intu_0 + &
          rv_intu_half_p *two + rv_intu_half_pp *two +&
          rv_intu_1_p))
    end if
    pos_1_pp(1)=lon; pos_1_pp(2)=lat
    pos_1_pp(3)=p
    call add_position(pos_1_pp(1),pos_1_pp(2),dlon_tmp,dlat_tmp)

    if (pos_1_pp(1)==lag_trajfail) then
       if (lv_spec) then
          lspec_i=0; lspec_r=zero
       end if
       lon=lag_trajfail; lat=lag_trajfail; return
    end if

    !Save the time step
    if (lv_spec) lspec_r(irk4_tstep)=tstep

    ! return values
    lon=pos_1_pp(1)
    lat=pos_1_pp(2)
    p  =pos_1_pp(3)

  end subroutine lag_rk4_nl


  ! ------------------------------------------------------------------------
  ! Implementation of the Runge-Kutta algorithm-> TLM
  ! ------------------------------------------------------------------------
  subroutine lag_rk4_tl(lspec_i,lspec_r,lon,lat,p,ufield,vfield)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_rk4_tl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lspec_i
!    lspec_r
!    lon,lat,p
!    ufield,vfield
!
!   output argument list:
!    lon,lat,p
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    integer(i_kind),dimension(:)  ,intent(in   ) :: lspec_i
    real(r_kind)   ,dimension(:)  ,intent(in   ) :: lspec_r
    real(r_kind)                  ,intent(inout) :: lon,lat,p
    real(r_kind)   ,dimension(:,:),intent(in   ) :: ufield,vfield

    ! Calculated positions
    real(r_kind),dimension(3)::pos_half_p,pos_half_pp,pos_1_p,pos_1_pp
    ! Interpolated winds
    real(r_kind)::rv_intu_0,rv_intu_half_p,rv_intu_half_pp,rv_intu_1_p
    real(r_kind)::rv_intv_0,rv_intv_half_p,rv_intv_half_pp,rv_intv_1_p
    ! Temporary
    integer(i_kind),dimension(8) ::tmp_ispec_int
    real(r_kind)   ,dimension(10)::tmp_rspec_int

    ! failure check
    if (lspec_r(irk4_tstep)==zero) then
       lon=zero; lat=zero; p=zero; return
    end if

    ! 1st step of the 4th order scheme : calculate the approximate
    ! of the half-timestep position, using an explicit scheme
    tmp_ispec_int      =lspec_i(irk4_loc_0:(irk4_loc_0+7))
    tmp_rspec_int(1:8) =lspec_r(irk4_wei_0:(irk4_wei_0+7))
    tmp_rspec_int(9:10)=lspec_r(irk4_ldu_0:(irk4_ldu_0+1))
    rv_intu_0=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       lon,lat,ufield)
    tmp_rspec_int(9:10)=lspec_r(irk4_ldv_0:(irk4_ldv_0+1))
    rv_intv_0=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       lon,lat,vfield)
    pos_half_p(2)=lat+m2lat_tl(lspec_r(irk4_tstep)/two*rv_intv_0)
    pos_half_p(1)=lon+m2lon_tl(lspec_r(irk4_lon_0:(irk4_lon_0+1)),&
       pos_half_p(2),lspec_r(irk4_tstep)/two*rv_intu_0)
    pos_half_p(3)=p

    ! 2nd step : calculate a second approximate of the half-timestep
    ! position, using an implicit scheme (with guest given by the position
    ! previously calculated)
    tmp_ispec_int      =lspec_i(irk4_loc_half_p:(irk4_loc_half_p+7))
    tmp_rspec_int(1:8) =lspec_r(irk4_wei_half_p:(irk4_wei_half_p+7))
    tmp_rspec_int(9:10)=lspec_r(irk4_ldu_half_p:(irk4_ldu_half_p+1))
    rv_intu_half_p=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       pos_half_p(1),pos_half_p(2),ufield)
    tmp_rspec_int(9:10)=lspec_r(irk4_ldv_half_p:(irk4_ldv_half_p+1))
    rv_intv_half_p=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       pos_half_p(1),pos_half_p(2),vfield)
    pos_half_pp(2)=lat+m2lat_tl(lspec_r(irk4_tstep)/two*rv_intv_half_p)
    pos_half_pp(1)=lon+m2lon_tl(lspec_r(irk4_lon_half_p:(irk4_lon_half_p+1)),&
       pos_half_pp(2),lspec_r(irk4_tstep)/two*rv_intu_half_p)
    pos_half_pp(3)=p

    ! 3rd step : calculate the first estimate of the final position, using
    ! a midpoint rule predictor
    tmp_ispec_int      =lspec_i(irk4_loc_half_pp:(irk4_loc_half_pp+7))
    tmp_rspec_int(1:8) =lspec_r(irk4_wei_half_pp:(irk4_wei_half_pp+7))
    tmp_rspec_int(9:10)=lspec_r(irk4_ldu_half_pp:(irk4_ldu_half_pp+1))
    rv_intu_half_pp=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       pos_half_pp(1),pos_half_pp(2),ufield)
    tmp_rspec_int(9:10)=lspec_r(irk4_ldv_half_pp:(irk4_ldv_half_pp+1))
    rv_intv_half_pp=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       pos_half_pp(1),pos_half_pp(2),vfield)
    pos_1_p(2)=lat+m2lat_tl(lspec_r(irk4_tstep)*rv_intv_half_pp)
    pos_1_p(1)=lon+m2lon_tl(lspec_r(irk4_lon_half_pp:(irk4_lon_half_pp+1)),&
       pos_1_p(2),lspec_r(irk4_tstep)*rv_intu_half_pp)
    pos_1_p(3)=p

    ! Final step : calculate the final position  of the balloon using a
    ! Simpson's rules corretor
    tmp_ispec_int      =lspec_i(irk4_loc_1_p:(irk4_loc_1_p+7))
    tmp_rspec_int(1:8) =lspec_r(irk4_wei_1_p:(irk4_wei_1_p+7))
    tmp_rspec_int(9:10)=lspec_r(irk4_ldu_1_p:(irk4_ldu_1_p+1))
    rv_intu_1_p=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       pos_1_p(1),pos_1_p(2),ufield)
    tmp_rspec_int(9:10)=lspec_r(irk4_ldv_1_p:(irk4_ldv_1_p+1))
    rv_intv_1_p=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       pos_1_p(1),pos_1_p(2),vfield)
    pos_1_pp(2)=lat+m2lat_tl(lspec_r(irk4_tstep)/6_r_kind*(&
       rv_intv_0 + &
       rv_intv_half_p *two + rv_intv_half_pp *two +&
       rv_intv_1_p))
    pos_1_pp(1)=lon+m2lon_tl(lspec_r(irk4_lon_1_p:(irk4_lon_1_p+1)),&
       pos_1_pp(2),lspec_r(irk4_tstep)/6_r_kind*(&
       rv_intu_0 + &
       rv_intu_half_p *two + rv_intu_half_pp *two +&
       rv_intu_1_p))
    pos_1_pp(3)=p

    ! return values
    lon=pos_1_pp(1)
    lat=pos_1_pp(2)
    p  =pos_1_pp(3)

  end subroutine lag_rk4_tl


  ! ------------------------------------------------------------------------
  ! Implementation of the Runge-Kutta algorithm-> ADJOINT
  ! ------------------------------------------------------------------------
  subroutine lag_rk4_ad(lspec_i,lspec_r,&
    ad_lon,ad_lat,ad_p,ad_ufield,ad_vfield)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_rk4_ad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lspec_i
!    lspec_r
!    ad_lon,ad_lat,ad_p
!    ad_ufield,ad_vfield
!
!   output argument list:
!    ad_lon,ad_lat,ad_p
!    ad_ufield,ad_vfield
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use constants, only: three
    implicit none

    integer(i_kind),dimension(:)  ,intent(in   ) :: lspec_i
    real(r_kind)   ,dimension(:)  ,intent(in   ) :: lspec_r
    real(r_kind)                  ,intent(inout) :: ad_lon,ad_lat,ad_p
    real(r_kind)   ,dimension(:,:),intent(inout) :: ad_ufield,ad_vfield

    ! Calculated positions
    real(r_kind),dimension(3)::ad_pos_half_p,ad_pos_half_pp,ad_pos_1_p,ad_pos_1_pp
    ! Interpolated winds
    real(r_kind)::ad_rv_intu_0,ad_rv_intu_half_p,ad_rv_intu_half_pp,ad_rv_intu_1_p
    real(r_kind)::ad_rv_intv_0,ad_rv_intv_half_p,ad_rv_intv_half_pp,ad_rv_intv_1_p
    ! Temporary
    integer(i_kind),dimension(8) ::tmp_ispec_int
    real(r_kind)   ,dimension(10)::tmp_rspec_int
    real(r_kind)::rv_tmp

    ! failure check
    if (lspec_r(irk4_tstep)==zero) then
       return
    end if
    
    ! zeroing local variables
    ad_pos_half_p=zero; ad_pos_half_pp=zero
    ad_pos_1_p=zero; ad_pos_1_pp=zero
    ad_rv_intu_0=zero;
    ad_rv_intu_half_p=zero;ad_rv_intu_half_pp=zero
    ad_rv_intu_1_p=zero
    ad_rv_intv_0=zero;
    ad_rv_intv_half_p=zero;ad_rv_intv_half_pp=zero
    ad_rv_intv_1_p=zero

    ! initialising tmp values
    ad_pos_1_pp(1)=ad_lon
    ad_pos_1_pp(2)=ad_lat
    ad_pos_1_pp(3)=ad_p
    ad_lon=zero; ad_lat=zero; ad_p=zero;

    ! Final step : calculate the final position  of the balloon using a
    ! Simpson's rules corretor
    ad_p=ad_p+ad_pos_1_pp(3)

    ad_lon=ad_lon+ad_pos_1_pp(1)
    rv_tmp=zero
    call m2lon_ad(lspec_r(irk4_lon_1_p:(irk4_lon_1_p+1)),ad_pos_1_pp(1),&
       ad_pos_1_pp(2),rv_tmp)
    ad_rv_intu_0      =ad_rv_intu_0+lspec_r(irk4_tstep)/6_r_kind*rv_tmp
    ad_rv_intu_half_p =ad_rv_intu_half_p+lspec_r(irk4_tstep)/three*rv_tmp
    ad_rv_intu_half_pp=ad_rv_intu_half_pp+lspec_r(irk4_tstep)/three*rv_tmp
    ad_rv_intu_1_p    =ad_rv_intu_1_p+lspec_r(irk4_tstep)/6_r_kind*rv_tmp
    
    ad_lat=ad_lat+ad_pos_1_pp(2)
    ad_rv_intv_0      =ad_rv_intv_0+&
       m2lat_tl(lspec_r(irk4_tstep)/6_r_kind*ad_pos_1_pp(2))
    ad_rv_intv_half_p =ad_rv_intv_half_p+&
       m2lat_tl(lspec_r(irk4_tstep)/three*ad_pos_1_pp(2))
    ad_rv_intv_half_pp=ad_rv_intv_half_pp+&
       m2lat_tl(lspec_r(irk4_tstep)/three*ad_pos_1_pp(2))
    ad_rv_intv_1_p    =ad_rv_intv_1_p+&
       m2lat_tl(lspec_r(irk4_tstep)/6_r_kind*ad_pos_1_pp(2))
    
    tmp_ispec_int      =lspec_i(irk4_loc_1_p:(irk4_loc_1_p+7))
    tmp_rspec_int(1:8) =lspec_r(irk4_wei_1_p:(irk4_wei_1_p+7))
   
    tmp_rspec_int(9:10)=lspec_r(irk4_ldv_1_p:(irk4_ldv_1_p+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intv_1_p,ad_pos_1_p(1),ad_pos_1_p(2),ad_vfield)
      
    tmp_rspec_int(9:10)=lspec_r(irk4_ldu_1_p:(irk4_ldu_1_p+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intu_1_p,ad_pos_1_p(1),ad_pos_1_p(2),ad_ufield)

    ! 3rd step : calculate the first estimate of the final position, using
    ! a midpoint rule predictor
    ad_p=ad_p+ad_pos_1_p(3)
    
    ad_lon=ad_lon+ad_pos_1_p(1)
    rv_tmp=zero
    call m2lon_ad(lspec_r(irk4_lon_half_pp:(irk4_lon_half_pp+1)),&
       ad_pos_1_p(1),ad_pos_1_p(2),rv_tmp)
    ad_rv_intu_half_pp=ad_rv_intu_half_pp+lspec_r(irk4_tstep)*rv_tmp
    
    ad_lat=ad_lat+ad_pos_1_p(2)
    ad_rv_intv_half_pp=ad_rv_intv_half_pp+&
       m2lat_tl(lspec_r(irk4_tstep)*ad_pos_1_p(2))
    
    tmp_ispec_int      =lspec_i(irk4_loc_half_pp:(irk4_loc_half_pp+7))
    tmp_rspec_int(1:8) =lspec_r(irk4_wei_half_pp:(irk4_wei_half_pp+7))
   
    tmp_rspec_int(9:10)=lspec_r(irk4_ldv_half_pp:(irk4_ldv_half_pp+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intv_half_pp,ad_pos_half_pp(1),ad_pos_half_pp(2),ad_vfield)
      
    tmp_rspec_int(9:10)=lspec_r(irk4_ldu_half_pp:(irk4_ldu_half_pp+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intu_half_pp,ad_pos_half_pp(1),ad_pos_half_pp(2),ad_ufield)

    ! 2nd step : calculate a second approximate of the half-timestep
    ! position, using an implicit scheme (with guest given by the position
    ! previously calculated)
    ad_p=ad_p+ad_pos_half_pp(3)
    
    ad_lon=ad_lon+ad_pos_half_pp(1)
    rv_tmp=zero
    call m2lon_ad(lspec_r(irk4_lon_half_p:(irk4_lon_half_p+1)),&
       ad_pos_half_pp(1),ad_pos_half_pp(2),rv_tmp)
    ad_rv_intu_half_p=ad_rv_intu_half_p+lspec_r(irk4_tstep)/two*rv_tmp
    
    ad_lat=ad_lat+ad_pos_half_pp(2)
    ad_rv_intv_half_p=ad_rv_intv_half_p+&
       m2lat_tl(lspec_r(irk4_tstep)/two*ad_pos_half_pp(2))
    
    tmp_ispec_int      =lspec_i(irk4_loc_half_p:(irk4_loc_half_p+7))
    tmp_rspec_int(1:8) =lspec_r(irk4_wei_half_p:(irk4_wei_half_p+7))
   
    tmp_rspec_int(9:10)=lspec_r(irk4_ldv_half_p:(irk4_ldv_half_p+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intv_half_p,ad_pos_half_p(1),ad_pos_half_p(2),ad_vfield)
      
    tmp_rspec_int(9:10)=lspec_r(irk4_ldu_half_p:(irk4_ldu_half_p+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intu_half_p,ad_pos_half_p(1),ad_pos_half_p(2),ad_ufield)
    
    ! 1st step of the 4th order scheme : calculate the approximate
    ! of the half-timestep position, using an explicit scheme
    ad_p=ad_p+ad_pos_half_p(3)
    
    ad_lon=ad_lon+ad_pos_half_p(1)
    rv_tmp=zero
    call m2lon_ad(lspec_r(irk4_lon_0:(irk4_lon_0+1)),ad_pos_half_p(1),&
       ad_pos_half_p(2),rv_tmp)
    ad_rv_intu_0=ad_rv_intu_0+lspec_r(irk4_tstep)/two*rv_tmp
    
    ad_lat=ad_lat+ad_pos_half_p(2)
    ad_rv_intv_0=ad_rv_intv_0+m2lat_tl(lspec_r(irk4_tstep)/two*ad_pos_half_p(2))
    
    tmp_ispec_int      =lspec_i(irk4_loc_0:(irk4_loc_0+7))
    tmp_rspec_int(1:8) =lspec_r(irk4_wei_0:(irk4_wei_0+7))
   
    tmp_rspec_int(9:10)=lspec_r(irk4_ldv_0:(irk4_ldv_0+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intv_0,ad_lon,ad_lat,ad_vfield)
      
    tmp_rspec_int(9:10)=lspec_r(irk4_ldu_0:(irk4_ldu_0+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intu_0,ad_lon,ad_lat,ad_ufield)

  end subroutine lag_rk4_ad

  ! ------------------------------------------------------------------------
  ! RK2 MODEL IMPLEMENTATION FOR ONE TIME STEP -----------------------------
  ! ------------------------------------------------------------------------

  
  ! ------------------------------------------------------------------------
  ! Implementation of the Runge-Kutta algorithm (2nd order) : Non linear
  ! (Heun's Method)
  ! ------------------------------------------------------------------------
  subroutine lag_rk2_nl(lon,lat,p,ufield,vfield,tstep,lspec_i,lspec_r)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_rk2_nl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lon,lat,p
!    ufield,vfield
!    tstep
!
!   output argument list:
!    lon,lat,p
!    lspec_i
!    lspec_r
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    ! longitude, latitude, pressure of the balloon
    real(r_kind)                                          ,intent(inout) :: lon,lat,p
    ! components of the wind fields (2nd dimension for vertical level)
    real(r_kind)   ,dimension(:,:)                        ,intent(in   ) :: ufield,vfield
    ! Time step (seconds)
    real(r_kind)                                          ,intent(in   ) :: tstep
    ! Parameters for the TL and Adjoint (optional)
    integer(i_kind),dimension(lag_rk2stepnpara_i),optional,intent(  out) :: lspec_i
    real(r_kind)   ,dimension(lag_rk2stepnpara_r),optional,intent(  out) :: lspec_r

    logical::lv_spec

    ! Calculated positions
    real(r_kind),dimension(3)::pos_1_p,pos_1_pp
    ! Interpolated winds
    real(r_kind)::rv_intu_0,rv_intu_1_p
    real(r_kind)::rv_intv_0,rv_intv_1_p

    ! Temporary
    integer(i_kind),dimension(8) ::tmp_ispec_int
    real(r_kind)   ,dimension(10)::tmp_rspec_int
    real(r_kind)   ,dimension(2) ::tmp_rspec_lon
    real(r_kind)::dlon_tmp,dlat_tmp

    lv_spec=present(lspec_i) .and. present(lspec_r)

    ! Detect missing values
    if(lon==lag_trajfail .or. lat==lag_trajfail) then
       if (lv_spec) then
          lspec_i=0; lspec_r=zero
       end if
       lon=lag_trajfail; lat=lag_trajfail; return
    end if

    ! 1st step of the 2nd order scheme : calculate the approximate
    ! of the final position position, using an explicit scheme
    if (lv_spec) then
       rv_intu_0=lag_int3d_nl(ufield,lon,lat,p,tmp_ispec_int,tmp_rspec_int)
       lspec_i(irk2_loc_0:(irk2_loc_0+7))=tmp_ispec_int
       lspec_r(irk2_wei_0:(irk2_wei_0+7))=tmp_rspec_int(1:8)
       lspec_r(irk2_ldu_0:(irk2_ldu_0+1))=tmp_rspec_int(9:10)
       rv_intv_0=lag_int3d_nl(vfield,lon,lat,p,tmp_ispec_int,tmp_rspec_int)
       lspec_r(irk2_ldv_0:(irk2_ldv_0+1))=tmp_rspec_int(9:10)
       dlat_tmp=m2lat_tl(tstep*rv_intv_0)
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep*rv_intu_0,tmp_rspec_lon)
       lspec_r(irk2_lon_0:(irk2_lon_0+1))=tmp_rspec_lon
    else
       rv_intu_0=lag_int3d_nl(ufield,lon,lat,p)
       rv_intv_0=lag_int3d_nl(vfield,lon,lat,p)
       dlat_tmp=m2lat_tl(tstep*rv_intv_0)
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep*rv_intu_0)
    end if
    pos_1_p(1)=lon; pos_1_p(2)=lat
    pos_1_p(3)=p
    call add_position(pos_1_p(1),pos_1_p(2),dlon_tmp,dlat_tmp)

    if (pos_1_p(1)==lag_trajfail) then
       if (lv_spec) then
          lspec_i=0; lspec_r=zero
       end if
       lon=lag_trajfail; lat=lag_trajfail; return
    end if

    ! 2nd step : calculate the first estimate of the final position, using
    ! a heun method
    if (lv_spec) then
       rv_intu_1_p=lag_int3d_nl(ufield,&
          pos_1_p(1),pos_1_p(2),pos_1_p(3),&
          tmp_ispec_int,tmp_rspec_int)
       lspec_i(irk2_loc_1_p:(irk2_loc_1_p+7))=tmp_ispec_int
       lspec_r(irk2_wei_1_p:(irk2_wei_1_p+7))=tmp_rspec_int(1:8)
       lspec_r(irk2_ldu_1_p:(irk2_ldu_1_p+1))=tmp_rspec_int(9:10)
       rv_intv_1_p=lag_int3d_nl(vfield,&
          pos_1_p(1),pos_1_p(2),pos_1_p(3),&
          tmp_ispec_int,tmp_rspec_int)
       lspec_r(irk2_ldv_1_p:(irk2_ldv_1_p+1))=tmp_rspec_int(9:10)
       dlat_tmp=m2lat_tl(tstep/two*(rv_intv_0+rv_intv_1_p))
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep/two*(rv_intu_0+rv_intu_1_p),&
          tmp_rspec_lon)
       lspec_r(irk2_lon_1_p:(irk2_lon_1_p+1))=tmp_rspec_lon
    else
       rv_intu_1_p=lag_int3d_nl(ufield,&
          pos_1_p(1),pos_1_p(2),pos_1_p(3))
       rv_intv_1_p=lag_int3d_nl(vfield,&
          pos_1_p(1),pos_1_p(2),pos_1_p(3))
       dlat_tmp=m2lat_tl(tstep/two*(rv_intv_0+rv_intv_1_p))
       dlon_tmp=m2lon_nl(lat+dlat_tmp,tstep/two*(rv_intu_0+rv_intu_1_p))
    end if
    pos_1_pp(1)=lon; pos_1_pp(2)=lat
    pos_1_pp(3)=p
    call add_position(pos_1_pp(1),pos_1_pp(2),dlon_tmp,dlat_tmp)

    if (pos_1_pp(1)==lag_trajfail) then
       if (lv_spec) then
          lspec_i=0; lspec_r=zero
       end if
       lon=lag_trajfail; lat=lag_trajfail; return
    end if

    !Save the time step
    if (lv_spec) lspec_r(irk2_tstep)=tstep

    ! return values
    lon=pos_1_pp(1)
    lat=pos_1_pp(2)
    p  =pos_1_pp(3)

  end subroutine lag_rk2_nl

  
  ! ------------------------------------------------------------------------
  ! Implementation of the Runge-Kutta algorithm (2nd order) : Tangent linear
  ! (Heun's Method)
  ! ------------------------------------------------------------------------
  subroutine lag_rk2_tl(lspec_i,lspec_r,lon,lat,p,ufield,vfield)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_rk2_tl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lspec_i
!    lspec_r
!    lon,lat,p
!    ufield,vfield
!
!   output argument list:
!    lon,lat,p
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    ! Parameters for the TL and Adjoint
    integer(i_kind),dimension(lag_rk2stepnpara_i),intent(in   ) :: lspec_i
    real(r_kind)   ,dimension(lag_rk2stepnpara_r),intent(in   ) :: lspec_r
    ! longitude, latitude, pressure of the balloon
    real(r_kind)                                 ,intent(inout) :: lon,lat,p
    ! components of the wind fields (2nd dimension for vertical level)
    real(r_kind)   ,dimension(:,:)               ,intent(in   ) :: ufield,vfield

    ! Calculated positions
    real(r_kind),dimension(3)::pos_1_p,pos_1_pp
    ! Interpolated winds
    real(r_kind)::rv_intu_0,rv_intu_1_p
    real(r_kind)::rv_intv_0,rv_intv_1_p

    ! Temporary
    integer(i_kind),dimension(8) ::tmp_ispec_int
    real(r_kind)   ,dimension(10)::tmp_rspec_int

    ! failure check
    if (lspec_r(irk2_tstep)==zero) then
       lon=zero; lat=zero; p=zero; return
    end if

    ! 1st step of the 2nd order scheme : calculate the approximate
    ! of the final position position, using an explicit scheme
    tmp_ispec_int      =lspec_i(irk2_loc_0:(irk2_loc_0+7))
    tmp_rspec_int(1:8) =lspec_r(irk2_wei_0:(irk2_wei_0+7))
    tmp_rspec_int(9:10)=lspec_r(irk2_ldu_0:(irk2_ldu_0+1))
    rv_intu_0=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       lon,lat,ufield)
    tmp_rspec_int(9:10)=lspec_r(irk2_ldv_0:(irk2_ldv_0+1))
    rv_intv_0=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       lon,lat,vfield)
    pos_1_p(2)=lat+m2lat_tl(lspec_r(irk2_tstep)*rv_intv_0)
    pos_1_p(1)=lon+m2lon_tl(lspec_r(irk2_lon_0:(irk2_lon_0+1)),&
       pos_1_p(2),lspec_r(irk2_tstep)*rv_intu_0)
    pos_1_p(3)=p
    
    ! 2nd step : calculate the first estimate of the final position, using
    ! a heun method
    tmp_ispec_int      =lspec_i(irk2_loc_1_p:(irk2_loc_1_p+7))
    tmp_rspec_int(1:8) =lspec_r(irk2_wei_1_p:(irk2_wei_1_p+7))
    tmp_rspec_int(9:10)=lspec_r(irk2_ldu_1_p:(irk2_ldu_1_p+1))
    rv_intu_1_p=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       pos_1_p(1),pos_1_p(2),ufield)
    tmp_rspec_int(9:10)=lspec_r(irk2_ldv_1_p:(irk2_ldv_1_p+1))
    rv_intv_1_p=lag_int3d_tl(tmp_ispec_int,tmp_rspec_int,&
       pos_1_p(1),pos_1_p(2),vfield)
    pos_1_pp(2)=lat+m2lat_tl(lspec_r(irk2_tstep)/two*&
       (rv_intv_0+rv_intv_1_p))
    pos_1_pp(1)=lon+m2lon_tl(lspec_r(irk2_lon_1_p:(irk2_lon_1_p+1)),&
       pos_1_pp(2),lspec_r(irk2_tstep)/two*(rv_intu_0+rv_intu_1_p))
    pos_1_pp(3)=p

    ! return values
    lon=pos_1_pp(1)
    lat=pos_1_pp(2)
    p  =pos_1_pp(3)

  end subroutine lag_rk2_tl

  
  ! ------------------------------------------------------------------------
  ! Implementation of the Runge-Kutta algorithm (2nd order) : Adjoint
  ! (Heun's Method)
  ! ------------------------------------------------------------------------
  subroutine lag_rk2_ad(lspec_i,lspec_r,ad_lon,ad_lat,ad_p,&
    ad_ufield,ad_vfield)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_rk2_ad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lspec_i
!    lspec_r
!    ad_lon,ad_lat,ad_p
!    ad_ufield,ad_vfield
!
!   output argument list:
!    ad_lon,ad_lat,ad_p
!    ad_ufield,ad_vfield
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    ! Parameters for the TL and Adjoint
    integer(i_kind),dimension(lag_rk2stepnpara_i),intent(in   ) :: lspec_i
    real(r_kind)   ,dimension(lag_rk2stepnpara_r),intent(in   ) :: lspec_r
    ! longitude, latitude, pressure of the balloon
    real(r_kind)                                 ,intent(inout) :: ad_lon,ad_lat,ad_p
    ! components of the wind fields (2nd dimension for vertical level)
    real(r_kind)   ,dimension(:,:)               ,intent(inout) :: ad_ufield,ad_vfield

    ! Calculated positions
    real(r_kind),dimension(3)::ad_pos_1_p,ad_pos_1_pp
    ! Interpolated winds
    real(r_kind)::ad_rv_intu_0,ad_rv_intu_1_p
    real(r_kind)::ad_rv_intv_0,ad_rv_intv_1_p

    ! Temporary
    integer(i_kind),dimension(8) ::tmp_ispec_int
    real(r_kind)   ,dimension(10)::tmp_rspec_int
    real(r_kind)::rv_tmp

    ! failure check
    if (lspec_r(irk2_tstep)==zero) then
       return
    end if

    ! zeroing local variables
    ad_pos_1_p  =zero; ad_pos_1_pp   =zero;
    ad_rv_intu_0=zero; ad_rv_intu_1_p=zero;
    ad_rv_intv_0=zero; ad_rv_intv_1_p=zero;

    ! initialising tmp values
    ad_pos_1_pp(1)=ad_lon
    ad_pos_1_pp(2)=ad_lat
    ad_pos_1_pp(3)=ad_p
    ad_lon=zero; ad_lat=zero; ad_p=zero;

    ! 2nd step : calculate the first estimate of the final position, using
    ! a heun method
    ad_p=ad_p+ad_pos_1_pp(3)

    ad_lon=ad_lon+ad_pos_1_pp(1)
    rv_tmp=zero
    call m2lon_ad(lspec_r(irk2_lon_1_p:(irk2_lon_1_p+1)),&
       ad_pos_1_pp(1),ad_pos_1_pp(2),rv_tmp)
    ad_rv_intu_0  =ad_rv_intu_0  +lspec_r(irk2_tstep)/two*rv_tmp
    ad_rv_intu_1_p=ad_rv_intu_1_p+lspec_r(irk2_tstep)/two*rv_tmp

    ad_lat=ad_lat+ad_pos_1_pp(2)
    ad_rv_intv_0  =ad_rv_intv_0  +&
       m2lat_tl(lspec_r(irk2_tstep)/two*ad_pos_1_pp(2))
    ad_rv_intv_1_p=ad_rv_intv_1_p+&
       m2lat_tl(lspec_r(irk2_tstep)/two*ad_pos_1_pp(2))

    tmp_ispec_int      =lspec_i(irk2_loc_1_p:(irk2_loc_1_p+7))
    tmp_rspec_int(1:8) =lspec_r(irk2_wei_1_p:(irk2_wei_1_p+7))

    tmp_rspec_int(9:10)=lspec_r(irk2_ldv_1_p:(irk2_ldv_1_p+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intv_1_p,ad_pos_1_p(1),ad_pos_1_p(2),ad_vfield)

    tmp_rspec_int(9:10)=lspec_r(irk2_ldu_1_p:(irk2_ldu_1_p+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intu_1_p,ad_pos_1_p(1),ad_pos_1_p(2),ad_ufield)

    ! 1st step of the 2nd order scheme : calculate the approximate
    ! of the final position position, using an explicit scheme
    ad_p=ad_p+ad_pos_1_p(3)

    ad_lon=ad_lon+ad_pos_1_p(1)
    rv_tmp=zero
    call m2lon_ad(lspec_r(irk2_lon_0:(irk2_lon_0+1)),&
       ad_pos_1_p(1),ad_pos_1_p(2),rv_tmp)
    ad_rv_intu_0  =ad_rv_intu_0  +lspec_r(irk2_tstep)*rv_tmp

    ad_lat=ad_lat+ad_pos_1_p(2)
    ad_rv_intv_0  =ad_rv_intv_0  +&
       m2lat_tl(lspec_r(irk2_tstep)*ad_pos_1_p(2))

    tmp_ispec_int      =lspec_i(irk2_loc_0:(irk2_loc_0+7))
    tmp_rspec_int(1:8) =lspec_r(irk2_wei_0:(irk2_wei_0+7))

    tmp_rspec_int(9:10)=lspec_r(irk2_ldv_0:(irk2_ldv_0+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intv_0,ad_lon,ad_lat,ad_vfield)

    tmp_rspec_int(9:10)=lspec_r(irk2_ldu_0:(irk2_ldu_0+1))
    call lag_int3d_ad(tmp_ispec_int,tmp_rspec_int,&
       ad_rv_intu_0,ad_lon,ad_lat,ad_ufield)

  end subroutine lag_rk2_ad


  ! --------------------------------------------------------------------------
  ! RK2 MODEL IMPLEMENTATION FOR ONE ITERATION (discomposed in sev. time steps)
  ! --------------------------------------------------------------------------


  ! ------------------------------------------------------------------------
  ! Implementation for the RK2 non linear model
  ! ------------------------------------------------------------------------
  subroutine lag_rk2iter_nl(lon,lat,p,ufield,vfield,tstep,lspec_i,lspec_r)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_rk2iter_nl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lon,lat,p
!    ufield,vfield
!    tstep
!
!   output argument list:
!    lon,lat,p
!    lspec_i
!    lspec_r
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    ! longitude, latitude, pressure of the balloon
    real(r_kind)                         ,intent(inout) :: lon,lat,p
    ! components of the wind fields (2nd dimension for vertical level)
    real(r_kind)   ,dimension(:,:)       ,intent(in   ) :: ufield,vfield
    ! Time step (seconds)
    real(r_kind)                         ,intent(in   ) :: tstep
    ! Parameters for the TL and Adjoint (optional)
    integer(i_kind),dimension(:),optional,intent(  out) :: lspec_i
    real(r_kind)   ,dimension(:),optional,intent(  out) :: lspec_r

    logical::lv_spec
   
    ! Time step for each step of the algo 
    real(r_kind),dimension(:),allocatable::tstep_iter
    ! Total number of steps
    integer(i_kind)::nsteps
    
    integer(i_kind),dimension(:),allocatable::tmp_ispec
    real(r_kind),dimension(:),allocatable::tmp_rspec
    real(r_kind)::tstep_tmp
    integer(i_kind)::i,tmp_begin

    lv_spec=present(lspec_i) .and. present(lspec_r)

    ! determine the time step for each step
    allocate(tstep_iter(lag_nstepiter))
    tstep_iter=zero
    nsteps=0
    tstep_tmp=tstep
    do i=1,lag_nstepiter
       if (tstep_tmp>zero) then
          nsteps=nsteps+1
          if (tstep_tmp/lag_stepduration>=one) then
             tstep_iter(i)=lag_stepduration
             tstep_tmp=tstep_tmp-lag_stepduration
          else
             tstep_iter(i)=tstep_tmp
             tstep_tmp=zero
          end if
       else
          tstep_iter(i)=zero
       end if
    end do
    
    ! save the number of time steps and intialise parameters
    if (lv_spec) then
       lspec_i=0
       lspec_r=zero
       lspec_i(1)=nsteps
    end if
    
    ! run each iteration
    allocate(tmp_ispec(lag_rk2stepnpara_i))
    allocate(tmp_rspec(lag_rk2stepnpara_r))
    do i=1,nsteps
       if (lv_spec) then
          call lag_rk2_nl(lon,lat,p,ufield,vfield,tstep_iter(i),&
             tmp_ispec,tmp_rspec)
          tmp_begin=2+(i-1)*lag_rk2stepnpara_i
          lspec_i(tmp_begin:(tmp_begin+lag_rk2stepnpara_i-1))=tmp_ispec
          tmp_begin=1+(i-1)*lag_rk2stepnpara_r
          lspec_r(tmp_begin:(tmp_begin+lag_rk2stepnpara_r-1))=tmp_rspec
       else
          call lag_rk2_nl(lon,lat,p,ufield,vfield,tstep_iter(i))
       end if
       ! Failure check
       if (lon==lag_trajfail .or. lat==lag_trajfail) then
          lon=lag_trajfail; lat=lag_trajfail
          if (lv_spec) then 
             lspec_i=0; lspec_r=zero
          end if
          return
       end if
    end do

    !cleaning
    deallocate(tstep_iter,tmp_ispec,tmp_rspec)

  end subroutine lag_rk2iter_nl


  ! ------------------------------------------------------------------------
  ! Implementation for the RK2 tangent-linear model
  ! ------------------------------------------------------------------------
  subroutine lag_rk2iter_tl(lspec_i,lspec_r,lon,lat,p,ufield,vfield)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_rk2iter_tl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lspec_i
!    lspec_r
!    lon,lat,p
!    ufield,vfield
!
!   output argument list:
!    lon,lat,p
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    integer(i_kind),dimension(:)  ,intent(in   ) :: lspec_i
    real(r_kind)   ,dimension(:)  ,intent(in   ) :: lspec_r
    real(r_kind)                  ,intent(inout) :: lon,lat,p
    real(r_kind)   ,dimension(:,:),intent(in   ) :: ufield,vfield

    integer(i_kind)::i,tmp_begin_i,tmp_begin_r
    
    ! Failure check
    if (lspec_i(1)==0) then
       lon=zero; lat=zero; p=zero;
       return
    end if
    
    ! run each iteration
    do i=1,lspec_i(1)
       tmp_begin_i=2+(i-1)*lag_rk2stepnpara_i
       tmp_begin_r=1+(i-1)*lag_rk2stepnpara_r
       call lag_rk2_tl(&
          lspec_i(tmp_begin_i:(tmp_begin_i+lag_rk2stepnpara_i-1)),&
          lspec_r(tmp_begin_r:(tmp_begin_r+lag_rk2stepnpara_r-1)),&
          lon,lat,p,ufield,vfield)
    end do

  end subroutine lag_rk2iter_tl


  ! ------------------------------------------------------------------------
  ! Implementation for the RK2 adjoint model
  ! ------------------------------------------------------------------------
  subroutine lag_rk2iter_ad(lspec_i,lspec_r,ad_lon,ad_lat,ad_p,ad_ufield,ad_vfield)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_rk2iter_ad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lspec_i
!    lspec_r
!    ad_lon,ad_lat,ad_p
!    ad_ufield,ad_vfield
!
!   output argument list:
!    ad_lon,ad_lat,ad_p
!    ad_ufield,ad_vfield
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    integer(i_kind),dimension(:)  ,intent(in   ) :: lspec_i
    real(r_kind)   ,dimension(:)  ,intent(in   ) :: lspec_r
    real(r_kind)                  ,intent(inout) :: ad_lon,ad_lat,ad_p
    real(r_kind)   ,dimension(:,:),intent(inout) :: ad_ufield,ad_vfield

    integer(i_kind)::i,tmp_begin_i,tmp_begin_r
    
    ! Failure check
    if (lspec_i(1)==0) then
       return
    end if
    
    ! run each iteration
    do i=lspec_i(1),1,-1
       tmp_begin_i=2+(i-1)*lag_rk2stepnpara_i
       tmp_begin_r=1+(i-1)*lag_rk2stepnpara_r
       call lag_rk2_ad(&
          lspec_i(tmp_begin_i:(tmp_begin_i+lag_rk2stepnpara_i-1)),&
          lspec_r(tmp_begin_r:(tmp_begin_r+lag_rk2stepnpara_r-1)),&
          ad_lon,ad_lat,ad_p,ad_ufield,ad_vfield)
    end do

  end subroutine lag_rk2iter_ad


end module lag_traj
