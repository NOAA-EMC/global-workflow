!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!
! Module:      Icing
!
! Author:      Yali Mao
!
! Date:        January 2011
!
! Notes:       int_d%gdcp_liq_cld_frac is MISSING_DATA_VALUE
!              for GOES before run_icing_algo() is called
!
!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

module Icing

  use Kinds

  IMPLICIT NONE

!**********************************************************************
! * private and public declaration
! *

!  private
  ! public methods
!  public run_icing

  real, parameter :: SLD_SPECIAL_VALUE = -0.1 ! for finalizing icing output

  real, parameter :: COS_MAX_DAY_ANGLE   = 0.339  ! Cosine(70deg). For daytime, -70deg < sun_angle < +70deg.
  real, parameter :: COS_MIN_NIGHT_ANGLE = 0.035 ! Cosine(88deg). For nighttime, abs(sun_angle) > +88deg.

  integer, parameter :: MAX_CLOUD_LAYERS = 8
  type :: input_data_t
     ! (the alloctable array is vertical levels 0:nz-1)

     ! model
     real, allocatable :: rh(:) 
     real, allocatable :: temps(:)
     real, allocatable :: hgt(:)
     real, allocatable :: vv(:)    ! pvv
     real, allocatable :: slw(:)   ! Supercooled Liquid Water
     real, allocatable :: liqc(:)  ! Liquid Condensate, only for icing severity
     real, allocatable :: icec(:)  ! Ice Condensate, only for icing severity
     real, allocatable :: thetaE(:)! Equivalent Potential Temperature, only for icing severity
     real, allocatable :: twp(:)   ! Total Water Path, only for icing severity

     ! metar
     real :: cloud_cover!                                            0
     real :: cloud_base ! first from cloud_base_hgt, then modified.  MISSING
     real :: zl         ! distance to freezing drizzle               MISSING
     real :: zr         ! distance to freezing rain                  MISSING
     real :: ip         ! distance to ice pellets                    MISSING
     real :: rn         ! distance to rain                           MISSING
     real :: sno        ! distance to snow                           MISSING
     real :: dzl        ! distance to drizzle                        MISSING

     ! lightning
     real :: thdr ! lght
     real :: nstrikes ! nstrikes

     ! radar
     real :: vipge1
     real :: r25
     real :: r75

     ! satellite
     real :: vis75   ! derived
     real :: c2mc475 ! derived
     real :: sunz50  ! derived
     real :: solar_angle_cos ! centered grid's solar angle's cosine

     ! pirep
     real, allocatable :: pirep_interest(:)
     real, allocatable :: pirep_weight(:)

  end type input_data_t

  type :: interp_data_t   !  Processed (as opposed to raw) data
     ! (the alloctable array is vertical levels 0:nz-1)

     integer :: topoK     ! Topography K level (e.g., approx 6 for Denver, 0 or 1 for San Francisco).
     integer :: warmnoseK ! Warm nose K level.
     integer :: cloud_baseK ! Cloud base K level.

     integer :: prcp   ! Precipitation flag.
     integer :: z_prcp ! Freezing precipitation flag.
     integer :: allsno ! All snow flag.

     integer :: tops(0:MAX_CLOUD_LAYERS-1)        ! K level or altitude for each cloud layer top.
     integer :: bases(0:MAX_CLOUD_LAYERS-1)       ! K level or altitude for each cloud layer base.
     integer :: moist_bases(0:MAX_CLOUD_LAYERS-1) ! K level or altitude for each cloud layer base.
     integer :: cnt ! Number of cloud layers.

     ! It is used in Layers and Icing, simply assign MISSING as its value
     real, allocatable :: gdcp_liq_cld_frac(:) ! Liquid fraction of cloudy GDCP pixels at each vertical level.

     integer, allocatable :: layer_num(:) ! Which cloud layer a vertical level is within.
     real, allocatable :: cbz(:)  ! The actual base of the cloudy layer
     real, allocatable :: ctt(:)  ! Cloud-top temperature for each vertical level 
                                  ! (all levels within or below a particular cloud are assigned the same value).
     real, allocatable :: dz_cloudbase(:)! Thickness (one cloud top to next cloud top) for each vertical level 
                                         ! (redundant values within or below the same cloud).
     real, allocatable :: dq(:)

     real, allocatable :: icing(:)
     real, allocatable :: sld(:)
     real, allocatable :: ice_intensity(:)
     real, allocatable :: scenario(:)
     real, allocatable :: ice_prob(:)
  end type interp_data_t 

contains

!**********************************************************************
! * function:    mapSev2Cat()
! *
! * Description: map severity values to severity categories
! * 
! * Returns:     category value
! *
  ! 0 = none (0, 0.08) 
  ! 4 = trace [0.08, 0.21]
  ! 1 = light (0.21, 0.37]
  ! 2 = moderate (0.37, 0.67]
  ! 3 (no value yet, July 2015)
  ! 5 = heavy (0.67, 1]
  !http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_table4-207.shtml
!*
  real function mapSev2Cat(data)
    real, intent(in) :: data

    if (data < 0.08) then
       mapSev2Cat = 0.0
    elseif (data <= 0.21) then
       mapSev2Cat = 4.
    else if(data <= 0.37) then
       mapSev2Cat = 1.0
    else if(data <= 0.67) then
       mapSev2Cat = 2.0
    else
       mapSev2Cat = 5.0
    endif

    return
  end function mapSev2Cat


!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine run_icing(nz, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: nz
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d

    integer :: k
    real :: vis_adj

    ! array initialization
    int_d%sld(:) = 0.0
    int_d%icing(:) = 0.0
    int_d%ice_prob(:) = 0.0

    if_thdr: if(id%thdr > 0.1 .and. id%thdr < 25.0) then
    ! run the algo if there is thunder within 150km of the grid point
 
       do k = nz-1, 0, -1
          call m_icing_A(k, id, int_d)
       end do

    else if(int_d%prcp == 0) then
    ! No Precip at the grid point - loop from top to bottom
       do k = nz-1, 0, -1
          call m_icing_B(k, id, int_d)
       end do

    else if(int_d%prcp == 1) then
    ! Precip  is occuring at the surface

       ! use cloud icing eqn. for the layers above the 
       ! precipitating layer
       do k = nz-1, int_d%tops(int_d%cnt - 1)+1, -1
          call m_icing_B(k, id, int_d)
       end do

       ! Precipitating cloud
       do k = int_d%tops(int_d%cnt - 1), 0, -1

          if(int_d%allsno == 1) then
	     ! Snow is only precip type 
             call m_icing_C(k, id, int_d)

          else	  
             ! Non-snow Precip occuring 

             ! use basic icing eqn for precip above the 
             ! warmnoseK (=0 if no nose) 
             if(k >= int_d%warmnoseK) then
                call m_icing_D(k, id, int_d)
             else
             ! use below warm nose eqn. 
                call m_icing_E(k, id, int_d)
             end if

             ! SLD without a warmnose
             if(int_d%warmnoseK == int_d%topoK) then
                call m_icing_F(k, id, int_d)
             end if

          end if ! if(int_d%allsno == 1)

       end do ! do k = int_d%tops(int_d%cnt - 1), 0, -1

    end if if_thdr

    ! add the pirep map and clean up the sld stuff 
    do k = nz-1, 0, -1
       call m_threshold(k, int_d)

       call m_pirep_inclusion(k, id, int_d)

       ! get_ice_type(k, id, int_d) -- useless

       ! Damp the icing potential using the same visible 
       ! satellite dampers as for intensity
       !
       ! If the visible data are bad then sunz50 will be set to 0 (in sat_cld) 
       ! and no damping will occur.
       !
       vis_adj = 0.75 * int_d%icing(k) * g12_vis_damping_map(id%vis75) * &
                 solar_angle_map(id%sunz50)
       int_d%icing(k) = int_d%icing(k) - vis_adj

       ! If it is terminator or nighttime (as determined by the solar angle)
       ! then scale the icing values using the derived equations.
       if (id%solar_angle_cos > MISSING+1 .and. id%solar_angle_cos <= COS_MAX_DAY_ANGLE) then
          int_d%icing(k) = m_night_adjust_ice(int_d%icing(k))
       end if

       ! Create the icing probability field
       int_d%ice_prob(k) = int_d%icing(k) !* 0.85

    end do ! k loop

    return
  end subroutine run_icing

!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_icing_A(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d
!write(*,*) "icing A k=", k
    if(int_d%ctt(k) < 300.0) then
       int_d%icing(k) = icing_th_temp_map(id%temps(k))
       int_d%sld(k) = int_d%icing(k)
    end if

    return
  end subroutine m_icing_A

!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_icing_B(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d

!write(*,*) "icing B k=", k
    if (int_d%ctt(k) < 300.0) then
       int_d%icing(k) = icing_temp_map(id%temps(k)) * &
                        icing_cloud_top_temp_map(int_d%ctt(k), int_d%gdcp_liq_cld_frac(k)) * &
                        icing_rel_hum_map(id%rh(k))
    end if
  
    return
  end subroutine m_icing_B


!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_icing_C(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d
!write(*,*) "icing C k=", k
    int_d%icing(k) = (0.9 - 0.6*icing_radar_map(id%r75)) * &
                     icing_temp_map(id%temps(k)) * &
                     icing_cloud_top_temp_map(int_d%ctt(k), int_d%gdcp_liq_cld_frac(k)) * &
                     icing_rel_hum_map(id%rh(k))
    return
  end subroutine m_icing_C

!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_icing_D(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d

    real :: tmap, cttmap, rhmap

!write(*,*) "icing D k=", k
    tmap = icing_temp_map(id%temps(k))
    cttmap = icing_cloud_top_temp_map(int_d%ctt(k), int_d%gdcp_liq_cld_frac(k))
    rhmap = icing_rel_hum_map(id%rh(k))
  
    int_d%icing(k) = tmap * cttmap * rhmap

    !  SLD ALGO - above an existing warmnose 
    if(cttmap >= 1.0 .and. int_d%warmnoseK > 2) then
       int_d%sld(k) = tmap - 0.2*icing_radar_map(id%r75)
    end if

    return
  end subroutine m_icing_D


!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_icing_E(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d

    real :: tmap, tmapWN, r75map_ice
!write(*,*) "icing E k=", k

    tmap = icing_temp_map(id%temps(k))
    tmapWN = icing_warm_nose_temp_map(id%temps(k), tmap)
    r75map_ice = icing_radar_map(id%r75)

    int_d%icing(k) = tmapWN + 0.2*r75map_ice*tmapWN
    int_d%sld(k) = int_d%icing(k)

    return
  end subroutine m_icing_E

!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_icing_F(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d

    real :: tmap, cttmap, r75map_ice
!write(*,*) "icing F k=", k
    tmap = icing_temp_map(id%temps(k))
    cttmap = icing_cloud_top_temp_map(int_d%ctt(k), int_d%gdcp_liq_cld_frac(k))
    r75map_ice = icing_radar_map(id%r75)

    if(int_d%z_prcp == 1) then
    ! freezing precip at the grid point
       if(cttmap >= 0.4) then
          int_d%sld(k) = (tmap + 0.2*r75map_ice) * ((cttmap-0.4)/0.6) + 0.25*tmap
       else  
          int_d%sld(k) = 0.25*tmap
       end if

    else if(int_d%ctt(k) >= 261.) then
       int_d%sld(k) = sqrt((int_d%ctt(k)-261.0)/14.0)*tmap
    end if

    return
  end subroutine m_icing_F


!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine  m_threshold(k, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(interp_data_t), intent(inout) :: int_d

    if ((int_d%icing(k) >= 0.01) .and. (int_d%sld(k) < 0.01))  int_d%sld(k) = -9.9

    if (int_d%sld(k) > 1.000)    int_d%sld(k) = 1.0
  
    if (int_d%icing(k) < int_d%sld(k))    int_d%icing(k) = int_d%sld(k)
  
    if (int_d%icing(k) > 1.0)  int_d%icing(k) = 1.0
      
    if (int_d%icing(k) <  0.0) int_d%icing(k) = 0.0

    ! Don't put out any icing information below the topography
    if (k < int_d%topoK) then
       int_d%icing(k) = 0.0
       int_d%sld(k) = 0.0
    end if

    return
  end subroutine m_threshold


!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_pirep_inclusion(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d

    real :: pirep_map, p3_map, vvmap

    if(int_d%icing(k) > 0.0005) then

       ! Use P1 and P3 for the PIREP interest
       if ((id%pirep_interest(k) < 0.0) .or. &
           (id%pirep_weight(k) < 0.0)) then
          pirep_map = 0.0
       else
          if (id%pirep_weight(k) > 1.0) then
             p3_map = 1.0
          else
             p3_map = id%pirep_weight(k)
          end if

          pirep_map = id%pirep_interest(k) * p3_map
       end if

       vvmap = vv_map(id%vv(k))

       call icing_repmapN(int_d%icing(k), pirep_map, id%slw(k), vvmap) 
      
    end if !  if(int_d%icing(k) > 0.0005) 

    return
  end subroutine m_pirep_inclusion

!**********************************************************************
! * function: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  real function  m_night_adjust_ice(ice)
    IMPLICIT NONE
    real, intent(in) :: ice

    if (ice >= 0.99) then
       m_night_adjust_ice = 10.0*ice - 9.0
    else if (ice >= 0.92) then
       m_night_adjust_ice = 1.43*ice - 0.52
    else if (ice >= 0.82) then
       m_night_adjust_ice = ice - 0.12
    else if (ice >= 0.73) then
       m_night_adjust_ice = 1.11*ice - 0.21
    else if (ice >= 0.63) then
       m_night_adjust_ice = ice - 0.13
    else if (ice >= 0.51) then
       m_night_adjust_ice = 0.83*ice - 0.025
    else if (ice >= 0.38) then
       m_night_adjust_ice = 0.77*ice + 0.007
    else if (ice >= 0.25) then
       m_night_adjust_ice = 0.77*ice + 0.007
    else if (ice >= 0.13) then
       m_night_adjust_ice = 0.83*ice - 0.008
    else if (ice >= 0.0) then
       m_night_adjust_ice = 0.77*ice
    end if

    return
  end function m_night_adjust_ice



!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine run_intensity(nz, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: nz
    type(input_data_t), intent(inout) :: id ! id stands for input data
    type(interp_data_t), intent(inout) :: int_d

    integer :: k
    real :: ave_den_vv, temperatue_adjust, ctt_adjust, reflect_adjust, visible_adjust

    ! zero some arrays, size=nz
    int_d%ice_intensity(:) = 0.0
    int_d%scenario(:) = 0.0

    ! calculate average VV in dendretic layer
    ave_den_vv = m_get_ave_vv(nz, id, int_d)
  
    ! modify surface precipitation types
    call m_update_surface_obs(id, int_d)

    ! Calculate the layer Q and the total water path for all 
    ! of the points in the column
    call  m_get_layerQ(id, int_d)

    ! Go through the column
    do k = nz-1, 0, -1
    
       ! Only calculate intensity if the icing potential is non-zero
       if (int_d%icing(k) < 0.01) cycle

       ! Find the scenario and calculate intensity
       ! Keep track of the scenario for debugging purposes
       !   0 = no icing          9 = convection
       !   Precipitation Scenarios
       !   1 = non-precipitating 
       !   2 = below warmnose (no snow) 
       !   3 = above warmnose (no snow)
       !   4 = all snow           
       !   5 = cold rain or R75>=5       
       !   6 = warm precip (CTT >= -12)
       !   7 = cold DZ,FZDZ,FZRA,PE (CTT < -12)
       !
       !   !!!!  NOT DOING THE LAYERS ANYMORE  !!!!
       !   Layers: yx - where y is the number of layers and x is one of 
       !   the above precip. scenarios
       !

       if_cnt: if(m_is_convection(id)) then
          call m_convection_scenario(k, id, int_d, .true.)
       else if(int_d%cnt == 1) then

          if(m_is_classic_precip_blwwmn(k, id, int_d)) then
             call m_classic_precip_blwwmn_scenario(k, id, int_d, ave_den_vv)
          else if(m_is_classic_precip_abvwmn(k, id, int_d)) then
             call m_classic_precip_abvwmn_scenario(k, id, int_d, .true.)
          else if(m_is_classic_precip(k, id, int_d)) then
             call m_classic_precip_scenario(k, id, int_d, .true.)
          else if(m_is_no_precip(k, id, int_d)) then
             call m_no_precip_scenario(k, id, int_d, .true.)
          else if(m_is_snow(k, id, int_d)) then
             call m_snow_scenario(k, id, int_d, .true.)
          else if(m_is_cold_rain(k, id, int_d)) then
             call m_cold_rain_scenario(k, id, int_d, .true.)
          else if(m_is_warm_precip(k, id, int_d)) then
             call m_warm_precip_scenario(k, id, int_d, .true.)
          else if(m_is_drizzle(k, id, int_d)) then
             call m_drizzle_scenario(k, id, int_d, .true.)
          end if

       else if(int_d%cnt > 1) then
          if_layer_num: if(int_d%layer_num(k) == 1) then ! top layer
             call m_no_precip_scenario(k, id, int_d, .true.)
          else if (int_d%layer_num(k) == int_d%cnt) then ! bottom layer
             if(m_is_classic_precip_blwwmn(k, id, int_d)) then
                call m_classic_precip_blwwmn_scenario(k, id, int_d, ave_den_vv)
             else if(m_is_classic_precip_abvwmn(k, id, int_d)) then
                call m_classic_precip_abvwmn_scenario(k, id, int_d, .false.)
             else if(m_is_classic_precip(k, id, int_d)) then
                call m_classic_precip_scenario(k, id, int_d, .false.)
             else if(m_is_no_precip(k, id, int_d)) then
                call m_no_precip_scenario(k, id, int_d, .false.)
             else if (m_is_snow(k, id, int_d)) then
                call m_snow_scenario(k, id, int_d, .false.)
             else if (m_is_cold_rain(k, id, int_d)) then
                call m_cold_rain_scenario(k, id, int_d, .false.)
             else if (m_is_warm_precip(k, id, int_d)) then
                call m_warm_precip_scenario(k, id, int_d, .false.)
             else if (m_is_drizzle(k, id, int_d)) then
                call m_drizzle_scenario(k, id, int_d, .false.)
             end if
          else 
             call m_no_precip_scenario(k, id, int_d, .false.) ! intermediate layers
          end if if_layer_num
       end if if_cnt

! GMC -- WHY DO THIS IF DONE AGAIN LATER 
!     ! Make sure the ice intensity has a minimum value
!     if (int_d%ice_intensity(k) < 0.01)
!       int_d%ice_intensity(k) = 0.01

       ! Now that the max intensity has been calculated, we want to 
       ! dampen it based on T, CTT, and radar. Each adjustment is 
       ! calculated by multiplying (weight)*(initial intensity)*(1 - damp_map).
       ! The CTT and radar adjustments are also multiplied by a map based on 
       ! the height below cloud top and above cloud base, respectively

       ! Temperature damping - 
       !   Can dampen by a max of 0.5, only where the scenario is not 
       !   convection or below the warm nose.
       !
       temperatue_adjust = 0.0
       if(int_d%scenario(k) /= 2.0 .and. int_d%scenario(k) /= 9.0) then
          temperatue_adjust = 0.5 * (1.0 - temp_map(id%temps(k)))
       end if

       ! CTT damping - 
       !    Can dampen by a max of 0.2. For all clouds and its interest 
       !    increases the closer to cloud top. Only where the scenario 
       !    is not convection or below the warm nose and where it is in a 
       !    cloud layer.
       !
       ctt_adjust = 0.0
       if(int_d%scenario(k) /= 2.0 .and. int_d%scenario(k) /= 9.0 .and. &
          int_d%layer_num(k) > 0) then
          ctt_adjust = 0.2*(1.0 - ctt_map(int_d%ctt(k))) *& 
                       deltaz_ctz_map(id%hgt(int_d%tops(int_d%layer_num(k)-1)), id%hgt(k))
       end if

       ! Reflectivity damping - 
       !    Can dampen by a max of 0.15. Only in the lowest cloud layer 
       !    and interest increases the closer to cloud base we are. Interest 
       !    will be the max below cloud base as well. Maps differ for different 
       !    scenarios
       !
       reflect_adjust = 0.0
       if_low_layers: if(int_d%layer_num(k) == int_d%cnt .or. id%hgt(k) <= id%cloud_base .or.& 
          k <= int_d%cloud_baseK) then

          if((int_d%scenario(k) == 3.0) .or. (int_d%scenario(k) == 5.0)) then
             ! Above Warmnose or Cold Rain
             reflect_adjust = 0.15*coldrain_r25_map(id%r25)& 
                              * deltaz_cbz_rad_map(id%hgt(k), int_d%cbz(k))
          else if(int_d%scenario(k) == 4.0) then
             ! All Snow
             reflect_adjust = 0.15*allsnow_r25_map(id%r25) &
                              * deltaz_cbz_rad_map(id%hgt(k), int_d%cbz(k))
          else if ((int_d%scenario(k) == 6.0) .or. (int_d%scenario(k) == 7.0)) then
             ! Warm Precip or Cold Non-snow/rain precip
             reflect_adjust = 0.15*warmprecip_r25_map(id%r25)&
                              * deltaz_cbz_rad_map(id%hgt(k), int_d%cbz(k))
          end if
       end if if_low_layers

       int_d%ice_intensity(k) = int_d%ice_intensity(k) * &
                                (1.0 - (temperatue_adjust + ctt_adjust + reflect_adjust))

       ! Visible damping - 
       !    Can dampen by a max of 0.75 of ice_intensity
       !
       ! If the visible data are bad then sunz50 will be set to 0 (in sat_cld) 
       ! and no damping will occur.
       !
       visible_adjust = 0.75*g12_vis_damping_map(id%vis75) * &
                       solar_angle_map(id%sunz50)

       ! Use this adjustment to get the new intensity
       int_d%ice_intensity(k) = int_d%ice_intensity(k)* (1. - visible_adjust)


       ! Make sure the new ice intensity has a minimum value
       if (int_d%ice_intensity(k) < 0.01)  int_d%ice_intensity(k) = 0.01

       ! cap the value because some people don't know how to design algorithms
       if (int_d%ice_intensity(k) > 1.0) int_d%ice_intensity(k) = 1.0

       ! If it is terminator or nighttime (as determined by the solar angle)
       ! then scale the severity values using the derived equations.
       if (id%solar_angle_cos > MISSING+1 .and. id%solar_angle_cos <= COS_MAX_DAY_ANGLE) then
          int_d%ice_intensity(k) = m_night_adjust_sev(int_d%ice_intensity(k))
       end if

       ! severity category calculations have been moved to pressure2flight,
       ! because the categories cannot be interpolated
    
    end do  ! end  k loop

    return
  end subroutine run_intensity


!**********************************************************************
! * subroutine: 
! *
! * Description: calculates dq for all cloudy points
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_get_layerQ(id, int_d)
    IMPLICIT NONE
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d

    integer :: k, n, m, kk
    real :: Q_top, Q_base, Q_layer, totalQ, P, delta_TE, sum_rh
    real :: mean_rh, te_map_val, rh_map_val, adj_Q
    integer :: base_k, num_layers
    logical :: found

    ! Initialize the dq
    int_d%dq(:) = 0.0

    !  loop through each cloud layer
    loop_n: do n = 0, int_d%cnt-1
    loop_k: do k = int_d%tops(n), int_d%moist_bases(n)+1, -1
    
       ! Initialize
       Q_top = 0.0 
       Q_base = 0.0 
       Q_layer = 0.0 
       !      float Q_sum = 0.0 
       totalQ = 0.0 

       ! get base_k value (first layer that 4K colder than at k)
       found = .false.
       do m = k, int_d%moist_bases(n), -1
          if (id%thetaE(k) > id%thetaE(m) + 4.0) then
             base_k = m+1
             found = .true.
             exit
          end if
       end do ! m loop
  
       if (.not. found)  base_k = int_d%moist_bases(n)


       ! calculate the delta thetaE and deltaQ and delta RH for the layer
       ! pressure levels are 25hPa apart
       loop_m: do m = k, base_k+1, -1 ! m > base_k
          P = 1000.0 - (25.0*m)
          Q_top = m_mixing_ratio((id%temps(m)-273.15), (1000.0 - (25.0*m)) )
          Q_base = m_mixing_ratio((id%temps(m-1)-273.15), (1000.0 - (25.0*(m-1))) )
          Q_layer = Q_base - Q_top
	
          ! convert Q from g per kg to g m**3
          Q_layer = (Q_layer * (P * 100.0) )/(GAS_CONSTANT * id%temps(m))

          if (Q_layer < 0.0) Q_layer = 0.0

          delta_TE = id%thetaE(k) - id%thetaE(m-1)

          ! get the mean RH in the layer
          num_layers = 0
          sum_rh = 0.0
          do kk = k, m-1, -1
             num_layers = num_layers + 1
             sum_rh = sum_rh + id%rh(kk)
          end do ! kk loop: rh calculator
    
          mean_rh = 0.0
          if(num_layers > 0)   mean_rh = sum_rh/num_layers
      
          te_map_val = dq_dte_map(delta_TE) 
          rh_map_val = rh_map(mean_rh) 
          adj_Q = Q_layer * te_map_val * rh_map_val

          totalQ = adj_Q + totalQ
	
       end do loop_m

       int_d%dq(k) = totalQ

    end do loop_k
    end do loop_n

    return
  end subroutine m_get_layerQ

!**********************************************************************
! * function: 
! *
! * Description: calculates mixing ratio given t in C and p in mb
! * 
! * Returns:     mixing ratio
! *
! * Notes: 
! *
  real function m_mixing_ratio(t, p)
    IMPLICIT NONE
    real, intent(in) :: t, p

    real :: pr_vapr, corr, e
  
    pr_vapr = 6.112 * exp( (17.67*t)/(t+243.5))
    corr = 1.001 + ( (p - 100.0) /900.0) * 0.0034
    e = pr_vapr * corr

    m_mixing_ratio = 0.62197 * (e/(p-e)) * 1000.0

    return
  end function m_mixing_ratio



!**********************************************************************
! * function: 
! *
! * Description: Calculate the average VV in the dendritic layer 
! * 		 (-13 to -16) if it's in the lowest cloud
! * 
! * Returns:     average VV in pa/s
! *
! * Notes: 
! *
  real function m_get_ave_vv(nz, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: nz
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(in) :: int_d

    real :: totvv
    integer :: vvcount, k

    totvv = 0.0
    vvcount = 0
    do k = 0, int_d%tops(0)
       if (id%temps(k) <= 260.15 .and. id%temps(k) >= 257.15) then
          totvv = totvv + id%vv(k)
          vvcount = vvcount + 1
       end if
    end do ! k loop
    if (vvcount == 0) then
       m_get_ave_vv = 0.0
    else 
       m_get_ave_vv = totvv/vvcount
    end if

    return
  end function m_get_ave_vv


!**********************************************************************
! * subroutine: 
! *
! * Description: modifies preciptication types
! * 
! * Returns:     none
! *
! * Notes:       Some grid points won't have a precip type but will have 
! *		 enough radar echo for CIP to call it precipitating
! *
! *		 Determine the precip. type based on the temperature of 
! *		 the sfc and column and set the distance to something 
! *		 that won't matter in the severity calculation.
! *
  subroutine  m_update_surface_obs(id, int_d)
    IMPLICIT NONE
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d

    if ((id%zl < 0.0) .and. (id%zr < 0.0) .and. (id%ip < 0.0) .and. &
        (id%rn < 0.0) .and. (id%sno < 0.0) .and. (id%dzl < 0.0) .and. &
        (int_d%prcp == 1)) then

       ! RA
       if (id%temps(int_d%topoK) > 273.15) then
          id%rn = 125.0
          ! FZRA
       else if (int_d%warmnoseK > int_d%topoK) then
          id%zr = 125.0
          ! SN
       else
          id%sno = 125.0
          int_d%allsno = 1
       end if

    end if

    return
  end subroutine m_update_surface_obs


!**********************************************************************
! * function: 
! *
! * Description: applies scenario test for convection
! * 
! * Returns:
! *
! * Notes: 
! *
  logical function m_is_convection(id)
    IMPLICIT NONE
    type(input_data_t), intent(in) :: id

    if((id%thdr >= 0.0) .and. (id%thdr <= 25.0)) then
       m_is_convection = .true.
    else 
       m_is_convection = .false.
    end if

    return
  end function m_is_convection

!**********************************************************************
! * subroutine: 
! *
! * Description: calcultes severity for convection scenario
! * 
! * Returns:     none
! *
! * Notes: 
! *
  subroutine m_convection_scenario(k, id, int_d, use_sat)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(inout) :: id ! id stands for input data
    type(interp_data_t), intent(inout) :: int_d
    logical, intent(in) :: use_sat

    real :: sa_map, satcombo, lghtdistmap, rdiffmap, nstrikesmap, thmap_sev

    sa_map = 0.0
    satcombo = 0.0
    if(use_sat) then
       sa_map = solar_angle_map(id%sunz50)
       satcombo = sat_combo(id%vis75, id%c2mc475, id%sunz50)
    end if

    lghtdistmap = lightningdist_map(id%thdr)
    rdiffmap = rdiff_map(id%r75, id%r25)
    nstrikesmap = nstrikes_map(id%nstrikes)
    thmap_sev = conv_temp_map(id%temps(k)) ** 0.5

    int_d%ice_intensity(k) = & 
         (3.5*lghtdistmap + 3.0*satcombo + 2.0*rdiffmap + &
          5.0*nstrikesmap + 3.0*thmap_sev) / (3.5 + 3.0*sa_map + 2.0 + 5.0 + 3.0)
    int_d%scenario(k) = 9.0

    return
  end subroutine m_convection_scenario


!**********************************************************************
! * function: 
! *
! * Description: applies scenario test for classical precipitation 
! *		 (not snow) below the warm nose
! * 
! * Returns:
! *
! * Notes: 
! *
  logical function m_is_classic_precip_blwwmn(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(in) :: int_d

    if((id%r75 >= 5.0 .or. int_d%prcp == 1) .and. &
       int_d%allsno == 0 .and. int_d%warmnoseK > int_d%topoK .and. &
       k <= int_d%warmnoseK .and. &
       int_d%tops(int_d%cnt-1) > int_d%warmnoseK .and. &
       int_d%ctt(int_d%tops(int_d%cnt-1)) < 261.15) then
       m_is_classic_precip_blwwmn = .true.
    else
       m_is_classic_precip_blwwmn = .false.
    end if

    return
  end function m_is_classic_precip_blwwmn

!**********************************************************************
! * subroutine: 
! *
! * Description: calculates severity for classical precipitation (not snow) 
! *		 below the warm nose scenario
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_classic_precip_blwwmn_scenario(k, id, int_d, ave_den_vv)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(inout) :: int_d
    real, intent(in) :: ave_den_vv

    real :: bwn_tmap, fzra_r75map, fzra_dzmap, bwncttmap
    real :: mmap, p1map, p3map, snvvmap

    bwn_tmap = bwn_temp_map(id%temps(k))
    fzra_r75map = fzra_r75_map(id%r75)
    fzra_dzmap = deltaz_fzra_map(int_d%dz_cloudbase(k))
    bwncttmap = belowwn_ctt_map(int_d%ctt(k))
    mmap = moisture_map(id%rh(k), id%liqc(k), id%icec(k))
    p1map = pirep_interest_map(id%pirep_interest(k))
    p3map = pirep_weight_map(id%pirep_weight(k))
    snvvmap = vv_snlayer_map(ave_den_vv)

    int_d%ice_intensity(k) = &
         (3.0*bwn_tmap + 4.0*fzra_r75map + 3.0*fzra_dzmap + 3.0*bwncttmap + &
         3.5*mmap + 5.0*p1map*p3map + 2.5*snvvmap + 2.0*int_d%icing(k)) &
         / (3.0 + 4.0 + 3.0 + 3.0 + 3.5 + 5.0*p3map + 2.5 + 2.0)
    int_d%scenario(k) = 2.0

    return
  end subroutine m_classic_precip_blwwmn_scenario


!**********************************************************************
! * function: 
! *
! * Description: applies scenario test for classical precipitation 
! *		 (not snow) below the warm nose
! * 
! * Returns:
! *
! * Notes: 
! *
  logical function m_is_classic_precip_abvwmn(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(in) :: int_d

    if((id%r75 >= 5.0 .or. int_d%prcp == 1) .and. int_d%allsno == 0 .and. &
       int_d%warmnoseK > int_d%topoK .and. k > int_d%warmnoseK .and. &
       int_d%tops(int_d%cnt-1) > int_d%warmnoseK .and. &
       int_d%ctt(int_d%tops(int_d%cnt-1)) < 261.15) then
       m_is_classic_precip_abvwmn = .true.
    else
       m_is_classic_precip_abvwmn = .false.
    end if

    return
  end function m_is_classic_precip_abvwmn

!**********************************************************************
! * subroutine: 
! *
! * Description: calculates severity for classical precipitation (not snow) 
! *		 below the warm nose scenario
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_classic_precip_abvwmn_scenario(k, id, int_d, use_sat)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(inout) :: int_d
    logical, intent(in) :: use_sat

    real :: sa_map, satcombo, twppmap, vvmap_sev, mmap, p1map, p3map

    sa_map = 0.0
    satcombo = 0.0
    if(use_sat) then
       sa_map = solar_angle_map(id%sunz50)
       satcombo = sat_combo(id%vis75, id%c2mc475, id%sunz50)
    end if

    twppmap = twp_pcp_map(id%twp(k))
    vvmap_sev = vv_map(id%vv(k))
    mmap = moisture_map(id%rh(k), id%liqc(k), id%icec(k))
    p1map = pirep_interest_map(id%pirep_interest(k))
    p3map = pirep_weight_map(id%pirep_weight(k))

    int_d%ice_intensity(k) = &
         (4.0*satcombo + 3.0*twppmap + 3.5*vvmap_sev + 4.0*mmap + &
          5.0*p1map*p3map + 2.0*int_d%icing(k)) / &
         (4.0*sa_map + 3.0 + 3.5 + 4.0 + 5.0*p3map + 2.0)
    int_d%scenario(k) = 3.0

    return
  end subroutine m_classic_precip_abvwmn_scenario

!**********************************************************************
! * function: 
! *
! * Description: applies scenario test for classical precipitation 
! * 
! * Returns:
! *
! * Notes: 
! *
  logical function m_is_classic_precip(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(in) :: int_d

    if((id%r75 >= 5.0 .or. int_d%prcp == 1) .and. int_d%allsno == 0 .and. &
       int_d%warmnoseK > int_d%topoK .and. &
       (int_d%tops(int_d%cnt-1) <= int_d%warmnoseK .or. &
        int_d%ctt(int_d%tops(int_d%cnt-1)) >= 261.15)) then
       m_is_classic_precip = .true.
    else
       m_is_classic_precip = .false.
    end if

    return
  end function m_is_classic_precip

!**********************************************************************
! * subroutine: 
! *
! * Description: calculates severity for classical precipitation scenario.
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_classic_precip_scenario(k, id, int_d, use_sat)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(inout) :: int_d
    logical, intent(in) :: use_sat

    real :: sa_map, satcombo, twpwpmap, wp_dzmap, vvmap_sev, mmap
    real :: p1map, p3map, dqmap, sld

    sa_map = 0.0
    satcombo = 0.0
    if(use_sat) then
       sa_map = solar_angle_map(id%sunz50)
       satcombo = sat_combo(id%vis75, id%c2mc475, id%sunz50)
    end if

    sld = 0.0
    if (int_d%sld(k) > 0.0)   sld = int_d%sld(k)

    twpwpmap = twp_warmpcp_map(id%twp(k))
    wp_dzmap = deltaz_warmrain_map(int_d%dz_cloudbase(k))
    vvmap_sev = vv_map(id%vv(k))
    mmap = moisture_map(id%rh(k), id%liqc(k), id%icec(k))
    p1map = pirep_interest_map(id%pirep_interest(k))
    p3map = pirep_weight_map(id%pirep_weight(k))
    dqmap = dq_map(int_d%dq(k))

    int_d%ice_intensity(k) = &
         (4.0*satcombo + 3.0*twpwpmap + 3.0*wp_dzmap + 3.0*vvmap_sev + &
          3.5*mmap + 4.0*sld +  5.0*p1map*p3map + 2.0*dqmap) / &
         (4.0*sa_map + 3.0 + 3.0 + 3.0 + 3.5 + 4.0 + 5.0*p3map + 2.0)
    int_d%scenario(k) = 6.0

    return
  end subroutine m_classic_precip_scenario

!**********************************************************************
! * function: 
! *
! * Description: applies scenario test for no precipitation
! * 
! * Returns:
! *
! * Notes: 
! *
  logical function m_is_no_precip(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(in) :: int_d

    if(int_d%prcp == 0 .and. id%r75 <= 0.0) then
       m_is_no_precip = .true.
    else
       m_is_no_precip = .false.
    end if

    return
  end function m_is_no_precip


!**********************************************************************
! * subroutine: 
! *
! * Description: calculates severity for no precipitation scenario
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_no_precip_scenario(k, id, int_d, use_sat)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(inout) :: int_d
    logical, intent(in) :: use_sat

    real :: sa_map, satcombo, dqmap, np_dzmap, vvmap_sev
    real :: mmap, p1map, p3map

    sa_map = 0.0
    satcombo = 0.0
    if(use_sat) then
       sa_map = solar_angle_map(id%sunz50)
       satcombo = sat_combo(id%vis75, id%c2mc475, id%sunz50)
    end if

    dqmap = dq_map(int_d%dq(k))
    np_dzmap = deltaz_noprecip_map(int_d%dz_cloudbase(k))
    vvmap_sev = vv_map(id%vv(k))
    mmap = moisture_map(id%rh(k), id%liqc(k), id%icec(k))
    p1map = pirep_interest_map(id%pirep_interest(k))
    p3map = pirep_weight_map(id%pirep_weight(k))

    int_d%ice_intensity(k) = &
         (5.0*satcombo + 3.0*dqmap + 3.5*np_dzmap + 4.0*vvmap_sev + & 
           4.0*mmap + 3.0*int_d%icing(k) + 5.0*p1map*p3map) / &
         (5.0*sa_map + 3.0 + 3.5 + 4.0 + 4.0 + 3.0 + 5.0*p3map)
    int_d%scenario(k) = 1.0

    return
  end subroutine m_no_precip_scenario


!**********************************************************************
! * function: 
! *
! * Description: applies scenario test for snow
! * 
! * Returns:
! *
! * Notes: 
! *
  logical function m_is_snow(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(in) :: int_d

    if(int_d%allsno == 1) then
       m_is_snow = .true.
    else
       m_is_snow = .false.
    end if

    return
  end function m_is_snow


!**********************************************************************
! * subroutine: 
! *
! * Description: calculates severity for snow scenario
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_snow_scenario(k, id, int_d, use_sat)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(inout) :: int_d
    logical, intent(in) :: use_sat

    real :: sa_map, satcombo, twppmap, as_dzmap, vvmap_sev
    real :: mmap, p1map, p3map

    sa_map = 0.0
    satcombo = 0.0
    if(use_sat) then
       sa_map = solar_angle_map(id%sunz50)
       satcombo = sat_combo(id%vis75, id%c2mc475, id%sunz50)
    end if

    twppmap = twp_pcp_map(id%twp(k))
    as_dzmap = deltaz_allsnow_map(int_d%dz_cloudbase(k))
    vvmap_sev = vv_map(id%vv(k))
    mmap = moisture_map(id%rh(k), id%liqc(k), id%icec(k))
    p1map = pirep_interest_map(id%pirep_interest(k))
    p3map = pirep_weight_map(id%pirep_weight(k))
  
    int_d%ice_intensity(k) = &
         (4.0*satcombo + 3.0*twppmap + 3.5*as_dzmap + 3.5*vvmap_sev + &
          4.0*mmap + 3.0*int_d%icing(k) + 5.0*p1map*p3map) / &
         (4.0*sa_map + 3.0 + 3.5 + 3.5 + 4.0 + 3.0 + 5.0*p3map)
    int_d%scenario(k) = 4.0

    return
  end subroutine m_snow_scenario


!**********************************************************************
! * function: 
! *
! * Description: applies scenario test cold rain 
! * 
! * Returns:
! *
! * Notes: 
! *
  logical function m_is_cold_rain(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(in) :: int_d

    if((id%rn >= 0.0 .or. id%r75 >= 5.0 .or. int_d%prcp == 1) .and. & 
       int_d%ctt(k) < 261.15) then
       m_is_cold_rain = .true.
    else
       m_is_cold_rain = .false.
    end if

    return
  end function m_is_cold_rain


!**********************************************************************
! * subroutine: 
! *
! * Description: calculates severity for cold rain scenariio
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_cold_rain_scenario(k, id, int_d, use_sat)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(inout) :: int_d
    logical, intent(in) :: use_sat

    real :: sa_map, satcombo, twppmap, cr_dzmap, vvmap_sev
    real :: mmap, p1map, p3map

    sa_map = 0.0
    satcombo = 0.0
    if(use_sat) then
       sa_map = solar_angle_map(id%sunz50)
       satcombo = sat_combo(id%vis75, id%c2mc475, id%sunz50)
    end if

    twppmap = twp_pcp_map(id%twp(k))
    cr_dzmap = deltaz_coldrain_map(int_d%dz_cloudbase(k))
    vvmap_sev = vv_map(id%vv(k))
    mmap = moisture_map(id%rh(k), id%liqc(k), id%icec(k))
    p1map = pirep_interest_map(id%pirep_interest(k))
    p3map = pirep_weight_map(id%pirep_weight(k))

    int_d%ice_intensity(k) = &
         (4.0*satcombo + 3.0*twppmap + 3.5*cr_dzmap + 3.5*vvmap_sev + &
          4.0*mmap + 2.0*int_d%icing(k) + 5.0*p1map*p3map) / &
         (4.0*sa_map + 3.0 + 3.5 + 3.5 + 4.0 + 2.0 + 5.0*p3map)
    int_d%scenario(k) = 5.0

    return
  end subroutine m_cold_rain_scenario


!**********************************************************************
! * function: 
! *
! * Description: applies  scenario test for warm precipitation
! * 
! * Returns:
! *
! * Notes: 
! *
  logical function m_is_warm_precip(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(in) :: int_d

    if((id%rn >= 0.0 .or. id%zl >= 0.0 .or. id%zr >= 0.0 .or. &
        id%ip >= 0.0 .or. id%dzl >= 0.0 .or. & 
        (id%r75 >= 5.0 .and. int_d%prcp == 0)) .and.  &
       int_d%ctt(k) >= 261.15) then
       m_is_warm_precip = .true.
    else
       m_is_warm_precip = .false.
    end if

    return
  end function m_is_warm_precip


!**********************************************************************
! * subroutine: 
! *
! * Description: calculates severity for warm precipitation scenario
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_warm_precip_scenario(k, id, int_d, use_sat)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(inout) :: int_d
    logical, intent(in) :: use_sat

    real :: sa_map, satcombo, twpwpmap, wp_dzmap, vvmap_sev
    real :: mmap, p1map, p3map, dqmap, sld

    sld = 0.0
    if (int_d%sld(k) > 0.0)  sld = int_d%sld(k)

    sa_map = 0.0
    satcombo = 0.0
    if(use_sat) then
       sa_map = solar_angle_map(id%sunz50)
       satcombo = sat_combo(id%vis75, id%c2mc475, id%sunz50)
    end if

    twpwpmap = twp_warmpcp_map(id%twp(k))
    wp_dzmap = deltaz_warmrain_map(int_d%dz_cloudbase(k))
    vvmap_sev = vv_map(id%vv(k))
    mmap = moisture_map(id%rh(k), id%liqc(k), id%icec(k))
    p1map = pirep_interest_map(id%pirep_interest(k))
    p3map = pirep_weight_map(id%pirep_weight(k))
    dqmap = dq_map(int_d%dq(k))

    int_d%ice_intensity(k) = &
         (4.0*satcombo + 3.0*twpwpmap + 3.0*wp_dzmap + 3.0*vvmap_sev +  &
          3.5*mmap + 4.0*sld +  5.0*p1map*p3map + 2.0*dqmap) /&
         (4.0*sa_map + 3.0 + 3.0 + 3.0 + 3.5 + 4.0 + 5.0*p3map + 2.0)
    int_d%scenario(k) = 6.0

    return
  end subroutine m_warm_precip_scenario


!**********************************************************************
! * function: 
! *
! * Description: applies scenario test for drizzle, freezing drizzle,
! *		 freezing rain and ice pellets
! * 
! * Returns:
! *
! * Notes: 
! *
  logical function m_is_drizzle(k, id, int_d)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(in) :: int_d

    if((id%dzl >= 0.0 .or. id%zl >= 0.0 .or. id%zr >= 0.0 .or. & 
        id%ip >= 0.0) .and. int_d%ctt(k) < 261.15) then
       m_is_drizzle = .true.
    else
       m_is_drizzle = .false.
    end if

    return
  end function m_is_drizzle

!**********************************************************************
! * subroutine: 
! *
! * Description: calculates severity for drizzle, freezing drizzle,
!		freezing rain and ice pellets scenario 
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine m_drizzle_scenario(k, id, int_d, use_sat)
    IMPLICIT NONE
    integer, intent(in) :: k
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(inout) :: int_d
    logical, intent(in) :: use_sat

    real :: sa_map, satcombo, twppmap, cr_dzmap, vvmap_sev
    real :: mmap, p1map, p3map

    sa_map = 0.0
    satcombo = 0.0
    if(use_sat) then
       sa_map = solar_angle_map(id%sunz50)
       satcombo = sat_combo(id%vis75, id%c2mc475, id%sunz50)
    end if

    twppmap = twp_pcp_map(id%twp(k))
    cr_dzmap = deltaz_coldrain_map(int_d%dz_cloudbase(k))
    vvmap_sev = vv_map(id%vv(k))
    mmap = moisture_map(id%rh(k), id%liqc(k), id%icec(k))
    p1map = pirep_interest_map(id%pirep_interest(k))
    p3map = pirep_weight_map(id%pirep_weight(k))

    int_d%ice_intensity(k) = & 
         (4.0*satcombo + 3.0*twppmap + 3.5*cr_dzmap + 3.5*vvmap_sev + &
          4.0*mmap + 2.0*int_d%icing(k) + 5.0*p1map*p3map) / &
         (4.0*sa_map + 3.0 + 3.5 + 3.5 + 4.0 + 2.0 + 5.0*p3map)
    int_d%scenario(k) = 7.0

    return
  end subroutine m_drizzle_scenario


!**********************************************************************
! * function: m_night_adjust_sev
! *
! * Description: 
! * 
! * Returns:
! *
! * Notes: 
! *
  real function m_night_adjust_sev(sev)
    IMPLICIT NONE
    real :: sev
    if (sev >= 0.78) then
       m_night_adjust_sev = 1.36*sev - 0.36
    else if (sev >= 0.47) then
       m_night_adjust_sev = 1.05*sev - 0.115
    else if (sev >= 0.26) then
       m_night_adjust_sev = 0.95*sev - 0.072
    else if (sev >= 0.0) then
       m_night_adjust_sev = 0.67 * sev
    end if

    return
  end function m_night_adjust_sev

!********************Interest maps
!***********************************************************

!**********************************************************************
! *
  real function ctt_map(ctt)
    IMPLICIT NONE
    real, intent(in) :: ctt

    real :: cttmap
  
    if((ctt >= 261.15) .and. (ctt <= 280.15)) then
       cttmap = 1.0
    else if((ctt > 223.15) .and. (ctt < 261.15)) then
       cttmap = 0.2 + 0.8 * ((ctt - 223.15)/38.0)**2.0
    else if(ctt <= 223.15) then
       cttmap = 0.2
    else
       cttmap = 0.0
    end if
  
    ctt_map = cttmap
    return
  end function ctt_map

!**********************************************************************
! *
  real function g12_vis_map(vis)
    real, intent(in) :: vis

    real :: vismap

    if (vis <= 30.0) then
       vismap = 0.0
    else if (vis >= 80.0) then
       vismap = 1.0
    else
       vismap = (vis - 30.0) / 50.0
    end if

    g12_vis_map = vismap
    return
  end function g12_vis_map

!**********************************************************************
! *
  real function g10_vis_map(vis)
    IMPLICIT NONE
    real, intent(in) :: vis

    ! The g10 endpoints are 0.85 of the g12 ones
    real :: vismap

    if (vis <= 25.0) then
       vismap = 0.0
    else if (vis >= 68.0) then
       vismap = 1.0
    else 
       vismap = (vis - 25.0) / 43.0
    end if

    g10_vis_map = vismap
    return
  end function g10_vis_map

!**********************************************************************
! *
  real function g12_vis_damping_map(vis)
    IMPLICIT NONE
    real, intent(in) :: vis

    real :: vismap

    if (vis <= 30.0) then
       vismap = 1.0
    else if ((vis > 30.0) .and. (vis < 65.0)) then
       vismap = 1.0 - ((vis - 30.0) / 35.0)
    else
       vismap = 0.0
    end if

    g12_vis_damping_map = vismap  
    return
  end function g12_vis_damping_map

!**********************************************************************
! *
  real function g10_vis_damping_map(vis)
    IMPLICIT NONE
    real, intent(in) :: vis

    real :: vismap

    if (vis <= 25.0) then
       vismap = 1.0
    else if ((vis > 25.0) .and. (vis < 55.0)) then
       vismap = 1.0 - ((vis - 25.0) / 30.0)
    else
       vismap = 0.0
    end if
  
    g10_vis_damping_map = vismap
    return
  end function g10_vis_damping_map

!**********************************************************************
! *
  real function c2mc4_map(c2mc4)
    IMPLICIT NONE
    real, intent(in) :: c2mc4

    real :: c2mc4map

    if (c2mc4 < MISSING + 10.) then
       c2mc4map = 0.0
    else if (c2mc4 <= 13.0) then
       c2mc4map = -0.4
    else if ((c2mc4 > 13.0) .and. (c2mc4 < 20.0)) then
       c2mc4map = -0.4 + (1.4 * ((c2mc4-13.0)/7.0))
    else
       c2mc4map = 1.0
    end if

    c2mc4_map = c2mc4map
    return 
  end function c2mc4_map

!**********************************************************************
! *
  real function solar_angle_map(sunz)
    IMPLICIT NONE
    real, intent(in) :: sunz

    real :: sol_ang_map

    if(abs(sunz) > 1.0) then
       solar_angle_map = 0.0
       return
    end if

    if (sunz < 0.258) then ! acos(0.258) = 75.05 degrees
    ! Sun is too low
       sol_ang_map = 0.0
    else if (sunz >= 0.342) then ! acos(0.342) = 70.0 degrees
    ! Sun is optimal
       sol_ang_map = 1.0
    else
    ! Sun is transitional
       sol_ang_map = (sunz - 0.258) / 0.084
    end if

    solar_angle_map = sol_ang_map
    return 
  end function solar_angle_map

!**********************************************************************
! *
  real function vv_map(vv)
    IMPLICIT NONE
    real, intent(in) :: vv

    real :: vvmap

    if (vv > 0.0 ) then
       vvmap = 0.0
    else if (vv < -0.5) then
       vvmap = 1.0
    else
       vvmap = -1.0 * (vv/0.5)
    end if

    vv_map = vvmap
    return
  end function vv_map

!**********************************************************************
! *
  real function vv_snlayer_map(vv)
    IMPLICIT NONE
    real, intent(in) :: vv

    vv_snlayer_map = vv_map(vv)
    return
  end function vv_snlayer_map

!**********************************************************************
! *
  real function fzra_r75_map(r75)
    IMPLICIT NONE
    real, intent(in) :: r75

    real :: r75map

    if (r75 >= 35.0) then
       r75map = 1.0
    else if ((r75 < 35.0) .and. (r75 >= 15.0)) then
       r75map = (r75 - 15.0) / 20.0
    else
       r75map = 0.0
    end if

    fzra_r75_map = r75map
    return
  end function fzra_r75_map

!**********************************************************************
! *
  real function warmprecip_r25_map(r25)
    IMPLICIT NONE
    real, intent(in) :: r25

    real :: r25map
  
    ! New map, for using r25 in the damping for WP
    if ((r25 >= 0.0) .and. (r25 <= 10.0)) then
       r25map = 0.0
    else if ((r25 > 10.0) .and. (r25 <= 20.0)) then
       r25map = 0.5 * ((r25 - 10.0) / 10.0)
    else if (r25 < 0.0) then
       r25map = 0.0
    else
       r25map = 0.5
    end if

    warmprecip_r25_map = r25map
    return 
  end function warmprecip_r25_map

!**********************************************************************
! *
  real function allsnow_r25_map(r25)
    IMPLICIT NONE
    real, intent(in) :: r25

    real :: r25map

    if ((r25 >= 0.0) .and. (r25 <= 5.0)) then
       r25map = 0.0
    else if (r25 >= 25.0) then
       r25map = 1.0
    else if (r25 < 0.0) then
       r25map = 0.0
    else
       r25map = (r25 - 5.0) / 20.0
    end if

    allsnow_r25_map = r25map
    return 
  end function allsnow_r25_map

!**********************************************************************
! *
  real function coldrain_r25_map(r25)
    IMPLICIT NONE
    real, intent(in) :: r25

    real :: r25map

    if ((r25 >= 0.0) .and. (r25 <= 10.0)) then
       r25map = 0.0
    else if (r25 >= 30.0) then
       r25map = 1.0
    else if (r25 < 0.0) then
       r25map = 0.0
    else
       r25map = (r25 - 10.0) / 20.0
    end if

    coldrain_r25_map = r25map
    return 
  end function coldrain_r25_map

!**********************************************************************
! *
  real function rdiff_map(r75, r25)
    IMPLICIT NONE
    real, intent(in) :: r75
    real, intent(in) :: r25

    real :: rdiff, rdiffmap

    if ((r25 <= 0.0) .or. (r75 <= 0.0)) then
       rdiff = r75
    else
       rdiff = abs(r75 - r25)
    end if

    if ((rdiff > 0.0) .and. (rdiff <= 20.0)) then
       rdiffmap = rdiff / 20.0
    else if (rdiff <= 0.0) then
       rdiffmap = 0.0
    else
       rdiffmap = 1.0
    end if

    rdiff_map = rdiffmap
    return 
  end function rdiff_map

!**********************************************************************
! *
  real function lightningdist_map(dist)
    IMPLICIT NONE
    real, intent(in) :: dist

    real :: distmap

    if ( (dist <= 15.0) .and. (dist >= 0.0) ) then
       distmap = 1.0
    else if (dist <= 25.0) then
       distmap = 0.5 + (25.0 - dist)/20.0
    else
       distmap = 0.0
    end if

    lightningdist_map = distmap
    return 
  end function lightningdist_map

!**********************************************************************
! *
  real function cond_map(cond)
    IMPLICIT NONE
    real, intent(in) :: cond

    real :: condmap

    if( cond < 0.004) then
       condmap = 0.0
    else if (cond > 0.2) then
       condmap = 1.0
    else
       condmap = (5.0 * cond)
    end if

    cond_map = condmap    
    return
  end function cond_map

!**********************************************************************
! *
  real function rh_map(rh)
    IMPLICIT NONE
    real, intent(in) :: rh

    real :: rhmap

    if (rh <= 0.7) then
       rhmap = 0.0
    else if (rh >= 1.0) then
       rhmap = 1.0
    else
       rhmap = (rh - 0.7) / 0.3
    end if

    rh_map = rhmap
    return 
  end function rh_map

!**********************************************************************
! *
  real function twp_pcp_map(twp)
    IMPLICIT NONE
    real, intent(in) :: twp

    real :: twpmap

    if (twp <= 0.0) then
       twpmap = 0.0
    else if (twp >= 1000.0) then
       twpmap = 1.0
    else
       twpmap = twp / 1000.0
    end if

    twp_pcp_map = twpmap
    return 
  end function twp_pcp_map

!**********************************************************************
! *
  real function twp_warmpcp_map(twp)
    IMPLICIT NONE
    real, intent(in) :: twp

    real :: twpmap

    if (twp <= 0.0) then
       twpmap = 0.0
    else if (twp >= 500.0) then
       twpmap = 1.0
    else
       twpmap = twp / 500.0
    end if

    twp_warmpcp_map = twpmap
    return
  end function twp_warmpcp_map

!**********************************************************************
! *
  real function dq_map(dq)
    IMPLICIT NONE
    real, intent(in) :: dq

    real :: dqmap

    if (dq >= 1.0) then
       dqmap = 1.0
    else if (dq <= 0.0) then
       dqmap = 0.0
    else
       dqmap = dq
    end if

    dq_map = dqmap
    return 
  end function dq_map

!**********************************************************************
! *
  real function deltaz_fzra_map(dz)
    IMPLICIT NONE
    real, intent(in) :: dz

    real :: dzmap

    if (dz > 100.0) then
       dzmap = 1.0
    else
       dzmap = 0.0
    end if

    deltaz_fzra_map = dzmap
    return 
  end function deltaz_fzra_map

!**********************************************************************
! *
  real function deltaz_noprecip_map(dz)
    IMPLICIT NONE
    real, intent(in) :: dz

    real :: dzmap
  
    if ((dz >= 0.0) .and. (dz <= 6000.0)) then
       dzmap = dz / 6000.0
    else if (dz > 6000.0) then
       dzmap = 1.0
    else
       dzmap = 0.0
    end if

    deltaz_noprecip_map = dzmap
    return
  end function deltaz_noprecip_map

!**********************************************************************
! *
  real function deltaz_allsnow_map(dz)
    IMPLICIT NONE
    real, intent(in) :: dz

    real :: dzmap
  
    if (dz < 3000.0) then
       dzmap = 0.0
    else if ((dz >= 3000.0) .and. (dz <= 9000.0)) then
       dzmap = (dz - 3000.0) / 6000.0
    else
       dzmap = 1.0
    end if

    deltaz_allsnow_map =dzmap
    return
  end function deltaz_allsnow_map

!**********************************************************************
! *
  real function deltaz_coldrain_map(dz)
    IMPLICIT NONE
    real, intent(in) :: dz

    real :: dzmap

    if (dz < 5000.0) then
       dzmap = 0.0
    else if ((dz >= 5000.0) .and. (dz <= 14000.0)) then
       dzmap = (dz - 5000.0) / 9000.0
    else
       dzmap = 1.0
    end if

    deltaz_coldrain_map = dzmap
    return
  end function deltaz_coldrain_map

!**********************************************************************
! *
  real function deltaz_warmrain_map(dz)
    IMPLICIT NONE
    real, intent(in) :: dz

    deltaz_warmrain_map =  deltaz_noprecip_map(dz)
    return
  end function deltaz_warmrain_map

!**********************************************************************
! *
  real function deltaz_ctz_map(ctz, z)
    IMPLICIT NONE
    real, intent(in) :: ctz
    real, intent(in) :: z

    real :: dz,dzmap

    dz = ctz - z

    if ((dz >= 0.0) .and. (dz <= 2000.0)) then
       dzmap = 1.0
    else if ((dz < 0.0) .or. (dz >= 10000.0)) then
       dzmap = 0.0
    else
       dzmap = (10000.0 - dz) / 8000.0
    end if

    deltaz_ctz_map = dzmap
    return
  end function deltaz_ctz_map

!**********************************************************************
! *
  real function deltaz_cbz_rad_map(z, cbz)
    IMPLICIT NONE
    real, intent(in) :: z
    real, intent(in) :: cbz

    real :: dz,dzmap

    dz = z - cbz

    if (dz <= 1000.0) then
       dzmap = 1.0
    else if (dz >= 5000.0) then
       dzmap = 0.0
    else
       dzmap = (5000.0 - dz) / 4000.0
    end if

    deltaz_cbz_rad_map = dzmap
    return
  end function deltaz_cbz_rad_map

!**********************************************************************
! *
  real function belowwn_ctt_map(ctt)
    IMPLICIT NONE
    real, intent(in) :: ctt

    belowwn_ctt_map = 1.0 - ctt_map(ctt)
    return
  end function belowwn_ctt_map

!**********************************************************************
! *
  real function temp_map(temp)
    IMPLICIT NONE
    real, intent(in) :: temp

    real :: tmap

    if ((temp >= 261.15) .and. (temp <= 268.15)) then
       tmap = 1.0
    else if ((temp > 268.15) .and. (temp <= 273.15)) then
       tmap = (273.15 - temp) / 5.0
    else if ((temp <= 248.15) .or. (temp > 273.15)) then
       tmap = 0.0
    else 
       tmap = ((temp - 248.15)/13.0)**0.5
    end if

    temp_map = tmap
    return
  end function temp_map

!**********************************************************************
! *
  real function bwn_temp_map(temp)
    IMPLICIT NONE
    real, intent(in) :: temp

    real :: tmap
  
    if(temp >= 273.15) then
       tmap = 0.0
    else if(temp <= 269.15) then
       tmap = 1.0
    else
       tmap = (273.15 - temp) / 4.0
    end if

    bwn_temp_map = tmap
    return
  end function bwn_temp_map

!**********************************************************************
! *
  real function conv_temp_map(temp)
    IMPLICIT NONE
    real, intent(in) :: temp

    real :: ctmap
  
    if((temp >= 243.15) .and. (temp <= 265.15)) then
       ctmap=(temp-243.15)/22.0
    else if((temp > 265.15) .and. (temp <= 269.15)) then
       ctmap=1.0
    else if((temp > 269.15) .and. (temp <= 273.15)) then
       ctmap = ((273.15 - temp)/4.0)**0.50
    else 
       ctmap=0.0
    end if

    conv_temp_map = ctmap
    return
  end function conv_temp_map


!**********************************************************************
! *
  real function nstrikes_map(nstrikes)
    IMPLICIT NONE
    real, intent(in) :: nstrikes

    real :: nstrikesmap

    if ((nstrikes > 0.0) .and. (nstrikes <= 10.0)) then
       nstrikesmap = (nstrikes / 10.0) ** 0.5
    else if (nstrikes > 10.0) then
       nstrikesmap = 1.0
    else
       nstrikesmap = 0.0
    end if

    nstrikes_map = nstrikesmap
    return 
  end function nstrikes_map


!**********************************************************************
! *
  real function sat_combo(vis, c2mc4, sunz)
    IMPLICIT NONE
    real, intent(in) :: vis
    real, intent(in) :: c2mc4
    real, intent(in) :: sunz

    sat_combo = (g12_vis_map(vis) * c2mc4_map(c2mc4) * solar_angle_map(sunz))
    return 
  end function sat_combo

!**********************************************************************
! *
  real function moisture_map(rh, liqc, icec)
    IMPLICIT NONE
    real, intent(in) :: rh
    real, intent(in) :: liqc
    real, intent(in) :: icec

    real :: temp

    temp = 0.5 * rh_map(rh)
    moisture_map = temp + ((0.6 * (cond_map(liqc) * (1.0 - temp))) + (0.4 * (cond_map(icec) * (1.0 - temp))))

    return
  end function moisture_map

!**********************************************************************
! *
  real function pirep_interest_map(p1)
    IMPLICIT NONE
    real, intent(in) :: p1

    real :: p1map

    if (p1 <= 0.0) then
       p1map = 0.0
    else 
       p1map = p1
    end if

    pirep_interest_map = p1map
    return
  end function pirep_interest_map

!**********************************************************************
! *
  real function pirep_weight_map(p3)
    IMPLICIT NONE
    real, intent(in) :: p3

    real :: p3map

    if ((p3 >= 0.0) .and. (p3 <= 1.0)) then
       p3map = p3
    else if (p3 > 1.0) then
       p3map = 1.0
    else
       p3map = 0.0
    end if

    pirep_weight_map = p3map
    return 
  end function pirep_weight_map


!**********************************************************************
! *
  real function dq_dte_map(dte)
    IMPLICIT NONE
    real, intent(in) :: dte

    real :: dtemap

    dtemap = (4.0 - dte) / 4.0

    if (dtemap > 1.0) then
       dtemap = 1.0
    else if (dtemap < 0.0) then
       dtemap = 0.0
    end if

    dq_dte_map = dtemap
    return 
  end function dq_dte_map

!**********************************************************************
! *
  real function icing_temp_map(temp)
    IMPLICIT NONE
    real, intent(in) :: temp

    real :: tmap
  
    ! New 9/18/03
    if((temp >= 248.15) .and. (temp <= 265.15)) then
       tmap=(temp-248.15)/17.0
    else if((temp > 265.15) .and. (temp <= 269.15)) then
       tmap=1.0
    else if((temp > 269.15) .and. (temp <= 273.15)) then
       tmap= ((273.15 - temp)/4.0) ** 0.50
    else
       tmap=0.0
    end if
  
    icing_temp_map = tmap
    return
  end function icing_temp_map

!**********************************************************************
! *
  real function icing_th_temp_map(temp)
    IMPLICIT NONE
    real, intent(in) :: temp
 
    real :: tmap
 
    ! New 9/18/03
    if((temp >= 243.15) .and. (temp <= 265.15)) then
       tmap=(temp-243.15)/22.0
    else if((temp > 265.15) .and. (temp <= 269.15)) then
       tmap=1.0
    else if((temp > 269.15) .and. (temp <= 273.15)) then
       tmap= ((273.15 - temp)/4.0) **0.50
    else
       tmap=0.0
    end if

    icing_th_temp_map = tmap
    return
  end function icing_th_temp_map

!**********************************************************************
! *
  real function icing_cloud_top_temp_map(cldtops, liqfrac)
    IMPLICIT NONE
    real, intent(in) :: cldtops
    real, intent(in) :: liqfrac

    real :: liqtop
    real :: interest
    real, parameter :: wt_liqtop  = 0.5
    real, parameter :: wt_liqfrac = 0.5
  
    if((cldtops >= 261.0) .and. (cldtops < 280.)) then
       liqtop = 1.0
    else if((cldtops > 223.0) .and. (cldtops < 261.0 )) then
       liqtop = 0.2 + 0.8 * ((cldtops - 223.0)/38.0)**2.0
    else if(cldtops <= 223.0) then
       liqtop = 0.2 
    else
       liqtop = 0.2
    end if

    if(liqfrac >= 0.0) then
       interest = (wt_liqtop*liqtop + wt_liqfrac*liqfrac)/(wt_liqtop + wt_liqfrac)
    else
       interest = liqtop
    end if
  
    icing_cloud_top_temp_map = interest
    return
  end function icing_cloud_top_temp_map

!**********************************************************************
! *
  real function icing_vv_map(vertv)
    IMPLICIT NONE
    real, intent(in) :: vertv

    real :: vvmap
  
    if(vertv <= -0.50) then
       vvmap = 1.0
    else if((vertv > -0.5) .and. (vertv <= 0.0)) then
       vvmap = vertv/(-0.5 )
    else if((vertv > 0.0) .and. (vertv <= 0.25)) then
       vvmap = 0.0 
    else if((vertv > 0.25) .and. (vertv <= 1.0)) then
       vvmap = -1.0 * ((vertv - 0.25) / 0.75) 
    else 
       vvmap = -1.0
    end if
  
    icing_vv_map = vvmap
    return
  end function icing_vv_map


!**********************************************************************
! * correct?
  real function icing_warm_nose_temp_map(temp, tmap)
    IMPLICIT NONE
    real, intent(in) :: temp
    real, intent(in) :: tmap

    icing_warm_nose_temp_map = tmap  
    return
  end function icing_warm_nose_temp_map

!**********************************************************************
! *
  real function icing_rel_hum_map(rh)
    IMPLICIT NONE
    real, intent(in) :: rh
 
    real :: rhmap
  
    ! New RH map add on 05 May 2010

    if (rh < 0.5) then
       rhmap = 0.0
    else if (rh <= 0.8) then
       rhmap = (rh-0.5)*(1.0/3.0)
    else if (rh < 0.95) then
       rhmap = ((100.0/27.0)*(rh-0.68)) ** 3.0
    else
       rhmap = 1.0
    end if
  
    icing_rel_hum_map = rhmap
    return
  end function icing_rel_hum_map


!**********************************************************************
! *
  subroutine icing_repmapN(icing, pirep, LWC, vert)
    IMPLICIT NONE
    real, intent(inout) :: icing
    real, intent(in) :: pirep
    real, intent(in) :: LWC
    real, intent(in) :: vert

    real :: SLW, pirep_factor, slw_factor, vv_factor

    SLW = 0.0
    pirep_factor = 0.0
    slw_factor = 0.0
    vv_factor = 0.0

    if (LWC > 0.0000001) SLW = 1.0
    slw_factor = (1.0 - icing) * (0.4 * SLW)

    if (pirep >= 0.0) then
       pirep_factor = (1.0 - icing) * (0.35 * pirep)
    else
       pirep_factor = icing * (0.35 * pirep)
    end if

    if (vert >= 0.0) then
       vv_factor = (1.0 - icing) * (0.25 * vert)
    else
       vv_factor = icing * (0.25 * vert)
    end if
   
    icing = icing + slw_factor + pirep_factor + vv_factor

    if (icing > 1.0)   icing = 1.0

    return
  end subroutine icing_repmapN

!**********************************************************************
! *
  real function cloudtop_modeltemperature_map(model_ct_temp_C, sat_ct_temp_C)
    IMPLICIT NONE
    real, intent(in) :: model_ct_temp_C
    real, intent(in) :: sat_ct_temp_C

    real :: interest
    ! model_ct_temp_C = RUC Cloud-top Temperature in Celsius
    ! sat_ct_temp_C   = Satellite Cloud-top Temperature in Celsius
    real :: delta_temp_C

    delta_temp_C = model_ct_temp_C - sat_ct_temp_C
    if((delta_temp_C >= -2.0) .and. (delta_temp_C <= 6.0)) then
       interest = 1.0
    else if((delta_temp_C > 6.0) .and. (delta_temp_C <= 8.0)) then
       interest = (delta_temp_C/(-2.0)) + 4.0
    else
       interest = 0.0
    end if

    cloudtop_modeltemperature_map = interest
    return
  end function cloudtop_modeltemperature_map


!**********************************************************************
! *
  real function icing_radar_map(refl)
    IMPLICIT NONE
    real, intent(in) :: refl

    real :: interest

    if (refl < 15.0) then
       interest = 0.0
    else if ((refl >= 15.0) .and. (refl <= 35.0)) then
       interest = (refl - 15.0) / 20.0
    else
       interest = 1.0
    end if
 
    icing_radar_map = interest

    return
  end function icing_radar_map

end module Icing
