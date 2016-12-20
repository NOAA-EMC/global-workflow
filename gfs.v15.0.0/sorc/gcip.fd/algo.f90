
!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!
! Module:      AlgoMain
!
! Author:      Yali Mao
!
! Date:        January 2011, modified in December 2011
!
! Note:        The intermediate vertical arrays, i.e. of (id, 
!              int_d, and local functions) are all 0-index.
!
!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

module Algo

  use Kinds
  use Icing
  IMPLICIT NONE

  private
  public runAlgo


  real, parameter    :: CLOUD_TEMP_K_DEFAULT = 300.0


  real, parameter :: MIN_GROUND_TEMP_C = -35.0
  real, parameter :: MIN_CLOUD_COVER = 3.9
  real, parameter :: CLOUD_SURF_DT = 15.0 ! Make the cloud-surf delta T rule

  integer, parameter :: MAX_RESOLUTION_1D = 25

  integer, parameter :: NUM_CT = 11
  real, parameter :: CTT_PCNTL(0:NUM_CT-1) = &
       (/ 0.05, 0.10, 0.2, 0.3, 0.4, 0.5 , 0.6, 0.7, 0.8, 0.9, 0.95 /) 

  integer, parameter :: NUM_IR_BINS = 70
  real, parameter :: CTT(0:NUM_IR_BINS-1) = (/ &
        10.0,   7.5,   5.0,   4.0,   3.0,   2.0,   1.0,   0.0,  -0.5,  -1.0, &
        -1.5,  -2.0,  -2.5,  -3.0,  -3.5,  -4.0,  -4.5,  -5.0,  -5.5,  -6.0, &
        -6.5,  -7.0 , -7.5,  -8.0,  -8.5,  -9.0 , -9.5, -10.0, -10.5, -11.0, &
       -11.5, -12.0, -12.5, -13.0, -13.5, -14.0, -14.5, -15.0, -15.5, -16.0, &
       -16.5, -17.0, -17.5, -18.0, -18.5, -19.0, -19.5, -20.0, -21.0, -22.0, &
       -23.0, -24.0, -25.0, -26.0, -27.0, -28.0, -29.0, -30.0, -31.0, -32.0, &
       -33.0, -34.0, -35.0, -37.5, -40.0, -42.5, -45.0, -47.5, -50.0, -50.0 /)
  ! lines are for 10, 20, 30, 40, 50, 60, 70
  ! changed the last element from -51.0 to -50.0 to make m_fill_bins() smooth  - Y Mao


  type :: sat_dat_t 
     real, dimension(MAX_RESOLUTION_1D) :: vis
     real, dimension(MAX_RESOLUTION_1D) :: swir
     real, dimension(MAX_RESOLUTION_1D) :: ch4
     real, dimension(MAX_RESOLUTION_1D) :: sunz
     real, dimension(MAX_RESOLUTION_1D) :: solar_angle_deg
     real, dimension(MAX_RESOLUTION_1D) :: satice
     real, dimension(MAX_RESOLUTION_1D) :: ch2_ref
     real, dimension(MAX_RESOLUTION_1D) :: ch2mch4
     integer :: nvalid   ! total number of satellite pixels with valid ch4 data
     real :: sat_data_fraction ! Fraction of GDCP satellite pixels that are valid
     integer :: ct(0:NUM_CT-1) ! Cloud-top temperature values for 5%,10%,20%,...,90%,95%
  end type sat_dat_t



  !================================================!
  ! * constants
  ! *
  real, parameter :: MIN_SAT_PTS = 0.10 ! % of sat pixels needed at gp
  real, parameter :: MIN_CLOUD_FRACTION  = 0.40  ! % of cloudy pixels

!real, allocatable :: m_satcloud(:,:)
!real, allocatable :: m_metarPrcp(:, :)
!real, allocatable :: m_ctt(:, :, :)
!real, allocatable :: m_slw(:, :, :)


contains


!**********************************************************************
! * subroutine: runAlgo
! *

  subroutine runAlgo(nz, kgds, nfiner, inputs, outdat, iret)
    IMPLICIT NONE
    integer, intent(in) :: nz
    integer, intent(in) :: kgds(:)
    integer, intent(in) :: nfiner
    type(input_t),  intent(inout) :: inputs ! some will be modified before running algorithm
    type(icing_output_t), intent(inout) :: outdat
    integer, intent(out) :: iret

    type(input_data_t) :: id
    type(interp_data_t) :: int_d

    integer :: nx, ny
    integer :: i, j, k
!integer :: unit


    iret = -1
    nx = kgds(2)
    ny = kgds(3)

!allocate(m_satcloud(m_nx, m_ny))
!allocate(m_metarPrcp(m_nx, m_ny))
!allocate(m_ctt(m_nx, m_ny, 0:(m_nz-1)))
!allocate(m_slw(m_nx, m_ny, 0:(m_nz-1)))


    ! memory of outdat, id and int_d  is allocated here
    call init_algo(nx, ny, nz, inputs, outdat, id, int_d, iret)
    if(iret /= 0) return

    do j = 1, ny
       do i = 1, nx
          call run_algo(nz, kgds, nfiner, i, j,  inputs, id, int_d)
          call update_outdat(nz, i, j, int_d, inputs%model, outdat)

        !===================================================================
        ! prepare data to conversion to GRIB format

        do k = 1, nz
 
          !--------------------------
          ! Icing Probability
          outdat%probability(i, j, k) = max(outdat%probability(i, j, k), 0.)
          outdat%probability(i, j, k) = min(outdat%probability(i, j, k), 1.)
          !--------------------------
          ! SLD
          outdat%sld(i, j, k) = max(outdat%sld(i, j, k), 0.)
          outdat%sld(i, j, k) = min(outdat%sld(i, j, k), 1.)

          if (outdat%probability(i, j, k) < 0.01) then
             outdat%sld(i, j, k) = 0.
          else
             if (outdat%sld(i, j, k) < 0.01) &
                outdat%sld(i, j, k) = SLD_SPECIAL_VALUE
          endif

          !===================================================================
          !--------------------------
          ! Severity Category
          outdat%severity(i, j, k) = mapSev2Cat(outdat%severity(i, j, k))
          if (outdat%probability(i, j, k) < 0.01) then
             outdat%severity(i, j, k) = 0
          endif

        end do
       end do
    end do

    ! release memory of id and int_d
    call cleanup_Algo(id, int_d)

    iret = 0
    return
  end subroutine runAlgo


!**********************************************************************
! * subroutine: 
! *
! * Description: initialize the input pointers, adjust model inputs
! *              and allocate memeory for output, id and int_d
! * 
! * Returns:
! *
! * Notes: Operating on arrays
! *
  subroutine init_algo(nx, ny, nz, inputs, outdat, id, int_d, iret)
    implicit none
    integer, intent(in) :: nx, ny, nz
    type(input_t), intent(inout) :: inputs ! units of condensates are changed
    type(icing_output_t),        intent(inout) :: outdat
    type(input_data_t),    intent(inout) :: id
    type(interp_data_t),   intent(inout) :: int_d
    integer, intent(out) :: iret

!    real :: species_tmp(nx,ny,nz) ! for model
    real species_tmp ! for model
    integer :: i, j, k

    iret = -1

    !================================================!
    ! adjust model inputs
    !================================================!
    ! convert species from  g/kg to  g/m^3
!    where(inputs%model%p > 0. .and. inputs%model%t > MISSING+1.)
!       species_tmp = inputs%model%p / (GAS_CONSTANT * inputs%model%t)
!       where(inputs%model%lqc > MISSING+1.) inputs%model%lqc = inputs%model%lqc * species_tmp
!       where(inputs%model%icc > MISSING+1.) inputs%model%icc = inputs%model%icc * species_tmp
!       where(inputs%model%slw > MISSING+1.) inputs%model%slw = inputs%model%slw * species_tmp
!    endwhere
    ! It looks Zeus doesn't support elemental operations of two and above
    ! arrays, so change from 'where' to 'if'
    do k = 1, nz
    do j = 1, ny
    do i = 1, nx
      if(inputs%model%p(i,j,k) > 0. .and. inputs%model%t(i,j,k) > MISSING+1.) then
        species_tmp = inputs%model%p(i,j,k) / (GAS_CONSTANT * inputs%model%t(i,j,k))
        if(inputs%model%lqc(i,j,k) > MISSING+1.) &
            inputs%model%lqc(i,j,k) = inputs%model%lqc(i,j,k) * species_tmp
        if(inputs%model%icc(i,j,k) > MISSING+1.) &
            inputs%model%icc(i,j,k) = inputs%model%icc(i,j,k) * species_tmp
        if(inputs%model%slw(i,j,k) > MISSING+1.) &
            inputs%model%slw(i,j,k) = inputs%model%slw(i,j,k) * species_tmp
      endif
    enddo
    enddo
    enddo

    ! convert RH from percent to (0.0, 1.0) scale
    where(inputs%model%rh > MISSING+1.) inputs%model%rh =  0.01 * inputs%model%rh

    !================================================!
    ! allocate memory for outdat before running the algorithm
    !================================================!
    allocate(outdat%levels     (nz), stat=iret)
    allocate(outdat%severity   (nx, ny, nz), stat=iret)
    allocate(outdat%probability(nx, ny, nz), stat=iret)
    allocate(outdat%sld        (nx, ny, nz), stat=iret)

    !================================================!
    ! allocate memory for id before running the algorithm
    !================================================!
    ! model
    allocate(id%rh    (0:nz-1))
    allocate(id%temps (0:nz-1))
    allocate(id%hgt   (0:nz-1))
    allocate(id%vv    (0:nz-1))
    allocate(id%liqc  (0:nz-1))
    allocate(id%icec  (0:nz-1))
    allocate(id%thetaE(0:nz-1))
    allocate(id%slw   (0:nz-1))
    allocate(id%twp   (0:nz-1))
    ! pirep
    allocate(id%pirep_interest(0:nz-1))
    allocate(id%pirep_weight  (0:nz-1))

    !================================================!
    ! allocate memory for int_d before running the algorithm
    !================================================!
    allocate(int_d%gdcp_liq_cld_frac(0:nz-1))
    allocate(int_d%layer_num(0:nz-1))
    allocate(int_d%cbz(0:nz-1))
    allocate(int_d%ctt(0:nz-1))
    allocate(int_d%dz_cloudbase(0:nz-1))
    allocate(int_d%dq (0:nz-1))
    allocate(int_d%icing(0:nz-1))
    allocate(int_d%sld(0:nz-1))
    allocate(int_d%ice_intensity(0:nz-1))
    allocate(int_d%scenario(0:nz-1))
    allocate(int_d%ice_prob(0:nz-1))

    iret = 0

    return
  end subroutine init_algo

!**********************************************************************
! * subroutine: cleanup_Algo
! *
! * Description: release memory of id and int_d
! * 
! * Returns:
! *

  subroutine cleanup_Algo(id, int_d)
    IMPLICIT NONE
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d

    !================================================!
    ! for id
    !================================================!
    ! model
    deallocate(id%rh    )
    deallocate(id%temps )
    deallocate(id%hgt   )
    deallocate(id%vv    )
    deallocate(id%liqc  )
    deallocate(id%icec  )
    deallocate(id%thetaE)
    deallocate(id%slw   )
    deallocate(id%twp   )
    ! pirep
    deallocate(id%pirep_interest)
    deallocate(id%pirep_weight  )

    !================================================!
    ! for int_d
    !================================================!
    deallocate(int_d%gdcp_liq_cld_frac)
    deallocate(int_d%layer_num)
    deallocate(int_d%cbz)
    deallocate(int_d%ctt)
    deallocate(int_d%dz_cloudbase)
    deallocate(int_d%dq )
    deallocate(int_d%icing)
    deallocate(int_d%sld)
    deallocate(int_d%ice_intensity)
    deallocate(int_d%scenario)
    deallocate(int_d%ice_prob)
   
  end subroutine cleanup_Algo


!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: Operating on each element: id, int_d
! *
  subroutine run_algo(nz, kgds, nfiner, i, j, inputs, id, int_d)
    implicit none
    integer, intent(in) :: nz
    integer, intent(in) :: kgds(:)
    integer, intent(in) :: nfiner
    integer, intent(in) :: i, j
    type(input_t), intent(in) :: inputs ! units of condensates are changed
    type(input_data_t), intent(inout) :: id ! id stands for input data
    type(interp_data_t), intent(inout) :: int_d

    type(sat_dat_t) :: sd
    logical :: satcloudy


    !================================================!
    !         ASSIGN DATA to id on each grid(i,j)
    !================================================!
    ! check whether the optional data (radar/pirep/lightning)
    ! are available or not, depending on whether they are
    ! allocated or not. If not, set id% to zero
    ! (see subroutine inside for details)

    call m_model(nz, i, j, inputs%model, id, int_d) 
    call m_radar(i, j, inputs%radar, id) ! be ahead of m_surface() 
    call m_surface(i, j, inputs%metar, inputs%lightning, id, int_d)
    call m_pirep(nz, i, j, inputs%pirep, id, int_d)
    ! Examine the satellite pixels and determine the fraction that are valid
    call m_satellite(kgds, nfiner, i, j, id, inputs%sat, sd) 
    !================================================!
    !             CLOUD PROCESSING
    !------------------------------------------------!
    ! modified: ignore GDCP data available branch
    !         :  Use satellite data directly
    ! -- Y Mao January 2011
    !================================================!
    !

!m_satcloud(i,j) = int_d%cnt

!write(*,*) sd%sat_data_fraction, cloud_fraction, MIN_CLOUD_FRACTION

    !  Three cases based on satellite data:
    !    1) Insufficient satellite data (sd%sat_data_fraction too small). In this
    !       case, model data is used exclusively to assign cloud layers.
    !    2) Valid satellite data with enough cloudy pixels. Use satellite data to
    !       assign the initial (highest) layers, and then use Layers::run_layers()
    !       to extend layering from the satellite base downward.
    !    3) Valid satellite data with insufficient number of cloudy pixels. The
    !       satellite data indicates that no clouds exist. A lack of clouds obviously
    !       implies no cloud layers.
    if_sat_fraction: if(sd%sat_data_fraction >= MIN_SAT_PTS) then

       !  examine the fraction of the valid pixels that are cloudy.
       call sat_cld(sd, id, int_d, satcloudy)
!write(*,*) "sat used"
       if(satcloudy) then
!write(*,*) "satcloudy"
       !================================================!
       ! case  2  Valid satellite data with enough cloudy pixels.
       !================================================!


       ! Locate sub-maximum cloud layers (those lower than the highest level) from satellite ch4 values.
       !!! Satellite::sat_layers()
          call sat_layers(nz, id, int_d, sd)

       ! Find the cloud base level
          int_d%cloud_baseK = m_baseK(nz, id, int_d)

       ! Check for inconsistencies between METAR-based cloud base and 
       ! satellite-based cloud base.
          if (int_d%cloud_baseK >= int_d%bases(0)) then 
          ! METAR-based cloud base is higher than lowest cloud detected by satellite.
          ! Set cloud base to one level below satellite base.
             int_d%cloud_baseK = int_d%bases(0)-1
          ! The lowest level is 0, in case array id%hgt is out of bound - Y Mao March 2012
             if (int_d%cloud_baseK < 0) int_d%cloud_baseK = 0
             id%cloud_base = id%hgt(int_d%cloud_baseK)
          end if
    
       !!! Layers::run_layers()
          call run_layers(nz, id, int_d) 
       else ! if(cloud_fraction < MIN_CLOUD_FRACTION) then
!write(*,*) "case 3", i, j
       !================================================!
       ! case  3 - Valid satellite data with no clouds.
       !================================================!
       ! Valid satellite data indicates no clouds.
       ! Set cloud layer count to zero and continue.
          int_d%cnt = 0

       !================================================!
       ! end case
       !================================================!
       end if
    else
       !================================================!
       ! case 1
       !   Insufficient satellite data at the grid point.
       !   Satellite data cannot be used to assign the highest layers.
       !   Set variables accordingly, and use Layers::run_layers() to assign all layers.
       !================================================!
       int_d%cnt = 0
       int_d%bases(0) = 32
       int_d%tops(0) = 0
       int_d%prcp = 0
       int_d%allsno = 0
!write(*, *) "case 1", i, j
       ! find the cloud base grid point
       int_d%cloud_baseK = m_baseK(nz, id, int_d)
       ! Use model data to assign all cloud layers.
       !!! Layers::run_layers()
       call run_layers(nz, id, int_d)
       ! only use model clouds colder than +5 C
       if(id%temps(int_d%tops(0)) > 278.1) int_d%cnt = 0

    end if if_sat_fraction
!write(*,*) "layers done"

!m_ctt(i,j, :) = int_d%ctt(:)

!m_slw(i,j, :) = id%slw(:)
    !================================================!
    !                ICING ALGORITHM
    !================================================!
    if(int_d%cnt >= 1) then ! cloud found 

       call run_icing(nz, id, int_d) ! int_d%gdcp_liq_cld_frac == MISSING
!write(*,*) "icing_algo done"
       call run_intensity(nz, id, int_d) ! icing intensity
!write(*,*) "icing intensity done"
    else if(int_d%cnt == 0) then
       call m_reset_interp_data(int_d) 
    end if
 
    return
  end subroutine run_algo

!**********************************************************************
! * subroutine: 
! *
! * Description: assign values to output
! * 
! * Returns:
! *
! * Notes: Operating on (i, j)
! *
  subroutine update_outdat(nz, i, j, int_d, model, outdat)
    implicit none
    integer, intent(in) :: nz
    integer, intent(in) :: i, j
    type(interp_data_t), intent(in) :: int_d
    type(model_inputs_t), intent(in) :: model
    type(icing_output_t), intent(inout) :: outdat

    integer :: k

    outdat%levels(1:nz) = model%p(1,1,:)
    do k = 1, nz
       outdat%severity(i, j, k)    = int_d%ice_intensity(k-1) !\\
       outdat%probability(i, j, k) = int_d%ice_prob(k-1)      ! arrays in int_d are 0-index
       outdat%sld(i, j, k)         = int_d%sld(k-1)           !// 

       ! prepare icing for output
       if(outdat%probability(i, j, k) <= 0.0) &
            outdat%probability(i, j, k) = MISSING

       if((outdat%sld(i, j, k) <= 0.0) .and. (outdat%sld(i, j, k) > -9.8)) &
            outdat%sld(i, j, k) = MISSING

       if(outdat%severity(i, j, k) <= 0.0) &
            outdat%severity(i, j, k) = MISSING

       ! more adjustments according to topography
       if(model%h(i, j, k) < model%top(i,j)) then
!print *, "under h=", model%h(i, j, k), "top=",model%top(i,j)
	  outdat%probability(i, j, k) = MISSING 
	  outdat%sld(i, j, k) = MISSING
	  outdat%severity(i, j, k) = MISSING
       else
!print *, "h=", model%h(i, j, k), "top=",model%top(i,j)
       end if

    end do ! k loop
    return
  end subroutine update_outdat


!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: Operating on each element: id, int_d (if needed)
! *
  subroutine m_radar(i, j, radar, id)
    IMPLICIT NONE
    integer, intent(in) :: i, j
    type(radar_data_t), intent(in) :: radar
    type(input_data_t), intent(inout) :: id ! id stands for input data

    if(.not. (allocated(radar%vipPct) .and. allocated(radar%dbzPct))) then
       id%vipge1 = MISSING
       id%r25 = MISSING
       id%r75 = MISSING
       return
    endif

    id%vipge1 = radar%vipPct(i,j,1)  ! vip>=1
    id%r25    = radar%dbzPct(i,j,1)
    id%r75    = radar%dbzPct(i,j,2)

    if(id%vipge1 < 0.0)  id%vipge1 = 0.0

    return
  end subroutine m_radar


!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: Operating on each element: id, int_d (if needed)
! *
  subroutine m_model(nz, i, j, model, id, int_d)
    implicit none
    integer, intent(in) :: nz
    integer, intent(in) :: i, j
    type(model_inputs_t), intent(in) :: model
    type(input_data_t), intent(inout) :: id ! id stands for input data
    type(interp_data_t), intent(inout) :: int_d

    integer :: tempnoseK, above0
    real :: topo_val

    integer :: k  

    do k = 0, nz-1
       id%rh(k)     = model%rh (i,j,k+1)
       id%temps(k)  = model%t  (i,j,k+1)
       id%hgt(k)    = model%h  (i,j,k+1)
       id%vv(k)     = model%pvv(i,j,k+1)
       id%liqc(k)   = model%lqc(i,j,k+1)
       id%icec(k)   = model%icc(i,j,k+1)
       id%thetaE(k) = model%ept(i,j,k+1)
       id%slw(k)    = model%slw(i,j,k+1)
       id%twp(k)    = model%twp(i,j,k+1)
    end do ! do k = 0, nz-1

!if(mod(i, 50)==0 .and. mod(j, 50)==0)   write(*,*) "^^^^^^", id%hgt

    ! Get the topography K level
    topo_val = model%top(i,j)
    int_d%topoK = 0
    do k = nz-1, 0, -1
       if( id%hgt(k) <= topo_val) then
          int_d%topoK = k
          exit
       end if
    end do

    ! Find the warmnose K level
    int_d%warmnoseK = int_d%topoK
    tempnoseK = 0
    above0 = 0
    do k = nz-1, int_d%topoK, -1

       !  find the possible warm_nose
       if(id%temps(k) <= 273.15) then
          ! temp <= 0 C
          if(above0 == 1) int_d%warmnoseK = tempnoseK !above 0 then below 0
          above0 = 0
       else 
          ! temp > 0
          if(above0 == 0) tempnoseK = k
          above0 = 1
       end if
    end do ! do k = nz, int_d%topoK, -1

    return
  end subroutine m_model

!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: Operating on each element: id, int_d (if needed)
! *
  subroutine m_surface(i, j, metar, lightning, id, int_d)
    implicit none
    integer, intent(in) :: i, j
    type(metar_data_t), allocatable, intent(in) :: metar(:,:)
    type(lightning_data_t), allocatable, intent(in) :: lightning(:,:)
    type(input_data_t), intent(inout) :: id ! id stands for input data
    type(interp_data_t), intent(inout) :: int_d
 
    !================================================!
    ! metar
    !================================================!
    if(.not. allocated(metar)) then
      id%cloud_cover = 0
      id%cloud_base = MISSING
      id%zl  = MISSING
      id%zr  = MISSING
      id%ip  = MISSING
      id%rn  = MISSING
      id%sno = MISSING
      id%dzl = MISSING
    else
      id%cloud_cover = metar(i,j)%cloudCoverage
      id%cloud_base = metar(i,j)%cloudBaseHeight ! first from cloud_base_hgt, then modified.
      id%zl  = metar(i,j)%dist2FreezingDrizzle   ! fzdz
      id%zr  = metar(i,j)%dist2FreezingRain      ! fzra
      id%ip  = metar(i,j)%dist2IcePellets        ! pl
      id%rn  = metar(i,j)%dist2Rain              ! ra
      id%sno = metar(i,j)%dist2Snow              ! sn
      id%dzl = metar(i,j)%dist2Drizzle           ! dz
    endif

    int_d%z_prcp = 0
    int_d%prcp = 0
    int_d%allsno = 0

    ! Make the freezing precip variable true(=1) if any valid
    if ((id%zl >=  0.0) .or. (id%zr >= 0.0) .or. (id%ip >= 0.0))  int_d%z_prcp = 1

    ! Make the any precip variable if valid
    if ((id%zl >= 0.0) .or. (id%zr >= 0.0) .or. (id%ip >= 0.0) .or. &
         (id%rn >= 0.0) .or. (id%sno >= 0.0) .or. (id%dzl >= 0.0) .or. & 
         (id%r75 >= 18.0)) then
       int_d%prcp = 1
    end if

    ! Make the all snow variable if gridpoint contains only snow
    if ((id%sno >= 0.0) .and. (int_d%z_prcp == 0) .and. (id%rn < 0.0) .and. & 
         (id%dzl < 0.0))  then
       int_d%allsno = 1
    end if

!m_metarPrcp(i,j) = int_d%prcp + 10*int_d%z_prcp + 20 * int_d%allsno

    !================================================!
    ! lightning
    !================================================!
    if(.not. allocated(lightning)) then
       id%thdr = MISSING
       id%nstrikes = 0.
    else
       id%thdr     = lightning(i, j)%dist
       id%nstrikes = lightning(i, j)%rate
    endif

    return
  end subroutine m_surface

!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes: Operating on each element: id, int_d (if needed)
! *
  subroutine m_pirep(nz, i, j, pirep, id, int_d)
    implicit none
    integer, intent(in) :: nz
    integer, intent(in) :: i, j
    type(pirep_data_t), allocatable, intent(in) :: pirep(:, :, :)
    type(input_data_t), intent(inout) :: id ! id stands for input data
    type(interp_data_t), intent(inout) :: int_d

    integer :: k

    if(.not. allocated(pirep)) then
       id%pirep_interest(:) = MISSING
       id%pirep_weight(:) = MISSING
       return
    endif

    do k = 0, nz-1
       id%pirep_interest(k) = pirep(i, j, k+1)%interest
       id%pirep_weight(k)   = pirep(i, j, k+1)%weight
    end do

    return
  end subroutine m_pirep

!**********************************************************************
! * subroutine: m_satellite
! *
! * Description: Needs to expand satellite data range for each grid (i,j)
! * 
! * Returns:
! *
! * Notes: Operating on each element: sd
! * 
  subroutine m_satellite(kgds, nfiner, i, j, id, sat, sd)
    implicit none
    integer, intent(in) :: kgds(:)
    integer, intent(in) :: nfiner
    integer, intent(in) :: i, j
    type(input_data_t), intent(inout) :: id  ! id%solar_angle_cos
    type(satellite_data_t), intent(in) :: sat
    type(sat_dat_t), intent(inout) :: sd

    ! at a finer resolution than model's
    integer :: nx_sat, ny_sat   ! dimension of satellite data

    integer :: isat, jsat
    integer :: irel, jrel ! relative position
    integer :: ii, jj     ! absolute position

    integer :: n_valid    ! number of satellite pixels valid
    integer :: n_expected ! number of satellite pixels expected

    if(.not. allocated(sat%vis)) then
       sd%sat_data_fraction = MISSING
       id%vis75 = MISSING
       id%c2mc475 = MISSING
       id%sunz50 = MISSING
       id%solar_angle_cos = MISSING
       return
    endif

    nx_sat = kgds(2) * nfiner
    ny_sat = kgds(3) * nfiner

    isat = (i-1) * nfiner
    jsat = (j-1) * nfiner

    n_valid = 0
    n_expected = 0
    do jrel = 1, nfiner
       do irel = 1, nfiner
          ii = isat + irel
          jj = jsat + jrel
          if (jj < 1 .or. jj > ny_sat) cycle
          if (ii < 1 .or. ii > nx_sat) then
             if(kgds(1) == 3) then ! RAP
                cycle
             elseif(kgds(1) == 4 .or. kgds(1) == 0) then ! GFS
                if(ii < 1) ii = ii + nx_sat
                if(ii > nx_sat) ii = ii - nx_sat
             else
                cycle
             endif
          endif
          n_expected = n_expected + 1
          if(sat%ch4(ii,jj) > MISSING+1. .and. sat%ch4(ii,jj) < 50.0) then
             ! Channel 4 (IR) has good data
             n_valid = n_valid + 1
             ! save data only when channel 4 (IR) has good data
             sd%vis (n_valid) = sat%vis(ii, jj)
             sd%swir(n_valid) = sat%ch2(ii, jj)
             sd%ch4 (n_valid) = sat%ch4(ii, jj)  
             ! convert the angles in degrees to cosine of angle
             sd%sunz(n_valid) = cos(sat%sunz(ii, jj) * D2R)
             sd%satice (n_valid) = sat%satice(ii, jj)
             sd%ch2_ref(n_valid) = sat%ch2_ref(ii, jj)
             sd%ch2mch4(n_valid) = sat%ch2mch4(ii, jj)
          end if
       end do
    end do

    if(n_expected > 0) then
       sd%sat_data_fraction = real(n_valid) / real(n_expected)
    else
       sd%sat_data_fraction = 0.0
    end if

    sd%nvalid = n_valid

    ! the center grid represents the whole range
    id%solar_angle_cos = cos(sat%sunz(isat+(nfiner+1)/2, jsat+(nfiner+1)/2) * D2R)

    return
  end subroutine m_satellite

!**********************************************************************
! * function: m_baseK()
! *
! * Description:
! *   Uses the METAR-derived cloud-base height to estimate the model level
! *   of cloud base.
! *   CT_NONE=0, CT_CLR=1, CT_FEW=2, CT_SCT=3, CT_BKN=4, CT_OVC=5, CT_XOB=6
! *
! * Returns:
! *   Integer representing the model level of the cloud base.
! *
! * Notes:
! *
  integer function m_baseK(nz, id, int_d)
    implicit none
    integer, intent(in) :: nz
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d

    integer :: observable_baseK, fip_base_start
    integer :: k

    int_d%cloud_baseK = nz

    if_cloudcover: if((int_d%prcp == 0 .or. int_d%allsno == 1) .and. &
       id%cloud_base>MISSING+1. .and. id%cloud_cover > 1.5 ) then
       !================================================!
       ! case 1 cloudy (sct or more coverage) and have a cloud base height and no precip except perhaps snow 
       !================================================!
       ! FEW cloud's cloud_base was set to MISSING previously

       do k = int_d%topoK, nz-1
          if(id%hgt(k) > id%cloud_base) then
             observable_baseK = k -1
             exit
          end if
       end do

       ! Some METAR might report higher cloud base than some layers with rh>=0.8,
       ! it may create a 'hole' in the final icing.
       ! Searching lower cloud base  -- added Y Mao 2014 Nov
       int_d%cloud_baseK = observable_baseK
       do k = int_d%topoK, observable_baseK
          if(id%rh(k) >= 0.80)  then
             int_d%cloud_baseK = k
             exit
          endif
       enddo

    else if (id%cloud_cover > 0.5 .and. id%cloud_cover < 1.5 ) then
       !================================================!
       ! case 2 cloudy but surface ob reporting clear, loop down from sat base to 12,000' AGL
       !================================================!
       ! find the k-level for 12,000' ALG
       do k = int_d%topoK, nz-1
          if( (id%hgt(k) - id%hgt(int_d%topoK)) > (12000.0/3.28) ) then
             observable_baseK = k
             exit
          end if
       end do

       ! loop down from cloud top to obseravable base, looking for the lowest 80% RH above 12,000 AGL
       int_d%cloud_baseK = int_d%bases(0)-1
       do k = int_d%bases(0)-2, observable_baseK, -1
          if(id%rh(k) >= 0.80)  int_d%cloud_baseK = k
       end do

    ! else if (id%cloud_cover < 0.5 .or. fabs(id%cloud_cover-2) < 0.0001) then ! considering FEW
    else if (id%cloud_cover < 0.5) then
       !================================================!
       ! case 3 if no metar (i.e. cloud cover = missing or = NONE), 
       !        or if there is a cloud but cloud base is missing,
       !        then use the FIP technique lowest RH above 1000' >= 80%) 
       !================================================!
       ! find the 1000' AGL level in the model 
       do k = int_d%topoK, nz-1
          if ((id%hgt(k) - id%hgt(int_d%topoK)) > (1000.0/3.28) ) then
             fip_base_start = k
             exit
          end if
       end do

       ! find the lowest 80% RH base above 1000' AGL 
       do k = fip_base_start, nz-1
          if ( id%rh(k) >= 0.80 ) then
             int_d%cloud_baseK = k
             exit
          end if
       end do

    end if if_cloudcover

    ! For non-snow precipitation, set cloud base to ground level.
    if((int_d%prcp == 1 .and. int_d%allsno == 0) .or. (int_d%topoK > int_d%cloud_baseK)) then
       int_d%cloud_baseK = int_d%topoK
    end if

    m_baseK = int_d%cloud_baseK

    return
  end function m_baseK

!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes:
! *
  subroutine m_reset_interp_data(int_d)
    IMPLICIT NONE
    type(interp_data_t), intent(inout) :: int_d

    int_d%topoK = 0
    int_d%warmnoseK = 0
    int_d%cloud_baseK = 0
    int_d%prcp = 0
    int_d%z_prcp = 0
    int_d%allsno = 0
    int_d%cnt = 0

    ! arrays size = MAX_CLOUD_LAYERS
    int_d%tops = 0
    int_d%bases = 0
    int_d%moist_bases = 0

    ! arrays size = m_nz
    int_d%ctt = CLOUD_TEMP_K_DEFAULT
    int_d%dz_cloudbase = 0.0
    int_d%layer_num = 0
    int_d%cbz = 0.0
    int_d%dq = 0.0
    int_d%icing = 0.0
    int_d%sld = 0.0
    int_d%ice_intensity = 0.0
    int_d%scenario = 0.0
    int_d%ice_prob = 0.0
    int_d%gdcp_liq_cld_frac = MISSING ! used for Icing_Algo, not only for GDCP
  
    return
  end subroutine m_reset_interp_data


!**********************************************************************
! * subroutine: sat_cld()
! *
! * Description:
! *   Determines the variables used to assess whether or not enough valid
! *   satellite data is available for processing.
! *   Also assigns percentile values for satellite bin data (e.g., vis,
! *   c2mc4, etc.).
! *
! * Returns:
! *   No explicit return. Assigns the sat_data_fraction and cloud_fraction
! *   values to the sat_dat_t structure sd%
! *
! * Notes:
! *   Only used for satellite, not GDCP.
! *
  subroutine  sat_cld(sd, id, int_d, satcloudy)
    implicit none
    type(sat_dat_t), intent(inout) :: sd
    type(input_data_t), intent(inout) :: id
    type(interp_data_t), intent(inout) :: int_d
    logical, intent(out) :: satcloudy

    logical :: cloudy ! a pixel cloudy or not
    ! ncloudy is a counter to keep track of how many cloudy pixels have been detected.
    integer :: ncloudy
    real :: cloud_fraction ! Fraction of valid GOES satellite pixels that are cloudy.
          

    integer :: ir_bins(0:NUM_IR_BINS-1) ! Cloud-top temperature bins.

    real :: vis_bins(0:MAX_RESOLUTION_1D-1), c2mc4_bins(0:MAX_RESOLUTION_1D-1) 
    real :: sunz_bins(0:MAX_RESOLUTION_1D-1)

    integer :: k

    sd%ct(:) = 0
    ir_bins(:) = 0


    cloudy = .false.
    ncloudy = 0


    ! loop through the satellite points only with good valid  channel 4 data
    do_k: do k = 1, sd%nvalid

       ! check to see if any supercooled clouds
       if_sunz: if(sd%sunz(k) >= COS_MAX_DAY_ANGLE) then 
          !================================================!
          ! case 1  Daytime Cloud-cover Algorithm
          !================================================!

          ! If visible channel is good and visible albedo is less than 20 - 
          ! we are probably seeing ground - NO CLOUDS
          if((sd%vis(k) < MISSING+1. .or. sd%vis(k) >= 20.0) .and. &
             ! Visible channel is bad or visible albedo > 20%. Try to use other satellite channels.
             ! G. Thompson icing cloudtop test - if satice == 1, icing pixel - CLOUDY.
             ! satice is not turned on yet - Y Mao, March 2012
             ((sd%satice(k) > 1.9 .and. sd%satice(k) < 1.5) .or. &
              ! If IR <= -35C - CLOUDY
              (sd%ch4(k) > MISSING+1. .and. sd%ch4(k) <= -35.0) .or. &
              ! If longwave IR channel is between +20C and -35C AND ch2-ch4 > 10C, it CLOUDY
              ((sd%ch4(k) > MISSING+1. .and. sd%ch4(k) < 20.0) .and. sd%ch2mch4(k) >10.) .or. &
              ! If LW IR between -10C and -35C, AND surface observation (METAR) indicates BKN or greater - CLOUDY.
              ! If in this temperature range, we need to see some indication of cloud cover (thus METAR ob).
              ((sd%ch4(k) > MISSING+1. .and. sd%ch4(k) <= -10.) .and. id%cloud_cover >= 4.0) .or. &
              ! If Channel 2 reflectance > 20% AND visible albedo >= 25% - CLOUDY.
              ! Attempt to remove emission component of ch2
              ! "Add on" to find warmer cloud tops.
              ((sd%ch4(k) < 20.0 .and. sd%ch4(k) > 5.) .and. &
               (sd%ch2_ref(k) >= 20.0 .and. sd%vis(k) > 25.0)))) then
             cloudy = .true.
          else
             cloudy = .false.
          end if ! if(sd%vis
             
       else if (sd%sunz(k) <= COS_MIN_NIGHT_ANGLE) then ! 88 degrees
          !================================================!
          ! case 2  Nighttime Cloud-cover Algorithm
          !================================================!

          ! If IR <= -35C - CLOUDY
          if((sd%ch4(k) > MISSING+1. .and. sd%ch4(k) <= -35.0) .or. &
             ! If ch4 (LW IR) <= 5C AND ch2-ch4 <= -2C (liq cloudtop) or ch2-ch4 > +4C (ice cloudtop) - CLOUDY.
             ! If ch2-ch4 is between -2.1C and 4.1C, we are seeing ground (NO CLOUD).
             ! If surface observation (METAR) indicates BKN or greater - CLOUDY.
             ((sd%ch4(k) > MISSING+1. .and. sd%ch4(k) <= 5.0) .and. &
              (((sd%ch2mch4(k)>MISSING+1. .and. sd%ch2mch4(k)<=-2.1) .or. sd%ch2mch4(k)>=4.1) &
               .or. id%cloud_cover >= 4.0)) .or. &
             ! Just like 2nd rule, except now IR (ch4) is between +20C and +5C, AND ch2-ch4 > 10C - CLOUDY.
             ! "Add on" to find warmer cloud tops.
             ! At night, if ch2-ch4 <= -2.1C (liq water cloud) - CLOUDY.
             ((sd%ch4(k) <= 20.0 .and. sd%ch4(k) >= 5.0 ) .and. (sd%ch2mch4(k)>MISSING+1. .and. sd%ch2mch4(k) <= -2.1))) then
             cloudy = .true.
!write(*,*) "case 2", " ch4=", sd%ch4(k), "ch2-ch4=", sd%ch2mch4(k), "cloud=", id%cloud_cover
          else 
             cloudy = .false.
          end if
             ! Lose meaningful ch2 and visible information (because insufficient sunlight). Day and night rules cannot apply.

       else
          !================================================!
          ! case 3  Terminator Cloud-cover Algorithm
          !================================================!

          ! Look for excessively cold top.
          ! If ch4 <= -35C, ground is never this cold (within CONUS), regardless of surface cloud cover. Must be CLOUDY.
          if((sd%ch4(k) > MISSING+1. .and. sd%ch4(k) <= -35.0) .or. &
               ! If -35C < ch4 < +5C, check surface observation (METAR) cloud cover, if >=BKN, CLOUDY.
               ! If +5C < ch4 < +20C, check surface observation (METAR) cloud cover, if >=BKN, CLOUDY.
             ((sd%ch4(k) > MISSING+1. .and. sd%ch4(k) <= 20.0) .and. id%cloud_cover >= 4.0) .or. &
                 ! "Skin temperature" (ground temperature)
                ! If IR (ch4) temperature <= skin temperature - threshold (CLOUD_SURF_DT = 10C or 15C), CLOUDY.
                !  In other words, if IR temperature is colder than skin temperature minus threshold - CLOUDY.
             (sd%ch4(k) > MISSING+1. .and. (sd%ch4(k) + 273.15) <= (id%temps(int_d%topoK) - CLOUD_SURF_DT))) then
             cloudy = .true.
!write(*,*) "case 3", " ch4=", sd%ch4(k), "top temp=", id%temps(int_d%topoK), "cloud=", id%cloud_cover
          else 
             cloudy = .false.
          end if

          !================================================!
          ! end case
          !================================================!
       end if if_sunz


       ! Load the bins if a cloud was found
       if(cloudy) then
          vis_bins(ncloudy) = sd%vis(k)
          c2mc4_bins(ncloudy) = sd%ch2mch4(k)
          sunz_bins(ncloudy) = sd%sunz(k)
          call m_fill_bins(sd%ch4(k), ir_bins)
          ncloudy = ncloudy + 1
       end if

    end do do_k


    if(sd%nvalid == 0) then
       cloud_fraction = 0.0
    else
       cloud_fraction = real(ncloudy) / real(sd%nvalid) 
    end if

    if(cloud_fraction < MIN_CLOUD_FRACTION) then
       satcloudy = .false.
    else
       satcloudy = .true.
       ! Calculate the necessary percentiles for vis, c2mc4, and sunz at all times
       call m_calc_sat_percentiles(vis_bins, c2mc4_bins, sunz_bins, ncloudy, id)

       ! If visible data are bad, then they should not be used in the intensity algorithm
       ! (satcombo) or in the damping for icing and severity.  Setting the solar zenith angle
       ! to 0 will guarantee this won't happen. (see solar_angle_map)
       if (id%vis75 <= 0.0)  id%sunz50 = 0.0

       call calc_ctt(ncloudy, ir_bins, sd)  ! set sd%ct(:) array
    end if
  
    return
  end subroutine sat_cld


!**********************************************************************
! * subroutine: 
! *
! * Description:
! * 
! * Returns:
! *
! * Notes:
! *
  subroutine calc_ctt(ncloudy, ir_bins, sd)
    implicit none
    integer, intent(in) :: ncloudy
    integer, dimension(0:), intent(in) :: ir_bins
    type(sat_dat_t), intent(inout) :: sd
          
    integer :: sum
    integer :: i, j

    ! This subroutine loops through the cloud bins 
    ! checking for 5% and 10% ... 90% ctt
    do i = 0, NUM_CT-1

       sum = 0

       do j = NUM_IR_BINS-1, 0, -1
          sum = sum + ir_bins(j)

          if(real(sum)/real(ncloudy) >= CTT_PCNTL(i)) then
             sd%ct(i) = j
             exit
          end if
       end do !  do j = NUM_IR_BINS-1, 0, -1

    end do ! do i = 0, NUM_CT-1

    return
  end subroutine calc_ctt


!**********************************************************************
! * subroutine: sat_layers()
! *
! * Description: 
! *   Determines and assigns cloud layer information that is derived from
! *   the satellite data
! *
! * Returns:
! *   No explicit return. Assigns values to the ctt(), tops(), bases(),
! *   ct(), and cnt values of the interp_data_t structure int_d%
! *
! * Notes:
! *   Based on satellite data.
! *
          
  subroutine sat_layers(nz, id, int_d, sd)
    IMPLICIT NONE
    integer, intent(in) :: nz
    type(sat_dat_t), intent(in) :: sd
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(inout) :: int_d

    integer :: k_TROP ! k level of the tropopause (approx 250 mb)

    integer :: sat_base_k
    integer :: current_sat_layer, found, actual_top
    integer :: cld(0:nz) ! pre-set, enough for vertical levels

  
    integer :: i, k, n

    k_TROP = nz -2


    ! local variables initialization
    sat_base_k = MISSING_INT
    current_sat_layer = 0
    found = 0
    actual_top = k_TROP

    ! initialization
    int_d%tops(0) = 0
    int_d%bases(0) = k_TROP

    ! array initialization
    cld(:) = MISSING_INT
    int_d%ctt(:) = CLOUD_TEMP_K_DEFAULT ! 300.0

    actual_top = best_guess_sat_top(k_TROP, sd, id)
    loop_k: do k = actual_top, 0, -1

       ! Ten percentile bins in existing method. Determined from 70-element IR bins.
       ! Applying cloudtop temperatures.
       ! Looping through 11 CTT bins. Replace with loop through 3 CTT bins (med-std, med, med+std)?
       ! Changes should be limited to this part of code.
       loop_cld_bins: do n = 1, NUM_CT-1 ! do n = 1, 10


          if_k_CTT: if(id%temps(k) >= (CTT(sd%ct(n)) + 273.15)) then
          ! Temperature at this level warmer than satellite-based temperature.

             if_kplus_CTT: if(id%temps(k+1) < (CTT(sd%ct(n)) + 273.15)) then
             ! Temperature above colder than satellite temperature.
                ! found a cloud
                cld(k+1) = n  ! greater n is, the lower the height is.
                found = 1
                exit
             end if if_kplus_CTT

          end if if_k_CTT

       end do loop_cld_bins

    end do loop_k ! come down the temp column

    ! check to see if the warmest CTT 95% is too cold
    if( id%temps(k_TROP) > CTT(sd%ct(NUM_CT-1)) .and. found == 0 ) then
       cld(k_TROP) = NUM_CT-1
       found = 1
    end if

    if(found == 0) then
       int_d%cnt = 0
       return
    else 
       int_d%cnt = 1
    end if

    ! Identify the top and base of the layers that can be seen by satellite
    do k = k_TROP, 0, -1
       if(cld(k) >= sat_base_k) then

          int_d%bases(0) = k
          sat_base_k = cld(k)

          if(cld(k) >= 0 .and. k >= int_d%tops(0)) int_d%tops(0) = k 

       end if
    end do

    ! Fill in gaps (extend layer top temperatures downward to next layer top).
    do k =  int_d%tops(0), int_d%bases(0), -1
       if(cld(k) > MISSING_INT) then
          current_sat_layer = cld(k) 
       else
          cld(k) = current_sat_layer
       end if
    end do
!print *, int_d%bases(0), int_d%tops(0)
!print *, cld(int_d%bases(0):int_d%tops(0))
    int_d%ctt(int_d%bases(0):int_d%tops(0)) = CTT(sd%ct(cld(int_d%bases(0):int_d%tops(0)))) + 273.15

    ! Set GDCP-inferred cloud liquid phase to MISSING
    int_d%gdcp_liq_cld_frac(:) = MISSING ! usefull for Layers and Icing

    return
  end subroutine sat_layers



!**********************************************************************
! * subroutine: m_fill_bins()
! *
! * Description:
! * 
! * Returns:
! *
! * Notes:
! *
  subroutine m_fill_bins(lwir, ir_bins)
    implicit none
    real, intent(in) :: lwir
    integer, dimension(0:), intent(inout) :: ir_bins ! must specified 0 index,  though the original one is

    integer :: i


    if(lwir <= CTT(NUM_IR_BINS-1)) then
       ir_bins(NUM_IR_BINS-1) = ir_bins(NUM_IR_BINS-1) + 1
       return
    end if

    do i = 0,  NUM_IR_BINS-2
       if(lwir > CTT(i)) then
          ir_bins(i) = ir_bins(i) + 1
          exit
       end if
    end do

    return
  end subroutine m_fill_bins

 
!**********************************************************************
! * function: best_guess_sat_top()
! *
! * Description: This function finds the Cloud top temp with the RH > 50%  
! * 
! * Returns:
! *
! * Notes:
! *
  integer function best_guess_sat_top(k_TROP, sd, id)
    IMPLICIT NONE
    integer, intent(in) :: k_TROP
    type(sat_dat_t), intent(in) :: sd
    type(input_data_t), intent(in) :: id

    integer :: num, pos_tops(0:9) ! the original size of pos_tops is 4. what if more?
 
    integer :: i, k

    best_guess_sat_top = 0 
    num = 0
    ! array initialization
    pos_tops(:) = -9

    ! find all the k's where the temp at k is > CTT 5% and (k+1) <= CTT 5%
    loop_k: do k = k_TROP, 0, -1

       if(id%temps(k) > (CTT(sd%ct(0)) + 273.15)) then

          if(id%temps(k+1) <= (CTT(sd%ct(0)) + 273.15)) then
             pos_tops(num) = k + 1
             num = num +1
          end if !see if temp(k+1) <= CTT 

       end if !see if temp(k) > CTT

    end do loop_k

!write(*,*) "best_guess_top num= ", num
!if(num > 1) then 
!write(*,*) m_ct(:)
!write(*,*) id%rh(0:)
!write(*,*) id%temps(0:)
!end if

    ! found more than 1 top
    if_num: if(num > 1) then

       ! set lower pos_tops to zero, in case id%rh is out of bound in the following loop. - Y Mao
       ! It also means 0 will be returned if no level with rh > 0.5
       pos_tops(num:) = 0

       do i = 0, num-1
          do k = pos_tops(i), pos_tops(i+1)+1, -1

!write(*,*) "               i k ",  i, k, pos_tops(i), pos_tops(i+1), k_TROP

             if(id%rh(k) > 0.5) then
                best_guess_sat_top = pos_tops(i) 
                return
             end if

          end do ! loop between the layers 

       end do ! loop thru the layers 

    else if(num == 0 .and. CTT(sd%ct(0)) + 273.15 < id%temps(k_TROP)) then
       best_guess_sat_top = k_TROP ! instead of pos_tops(num) = k_TROP ---- Y Mao
    else
       best_guess_sat_top=pos_tops(0)
    end if if_num

    return
  end function best_guess_sat_top

!**********************************************************************
! * subroutine: 
! *
! * Description: This subroutine calculates the percentiles for certain sat fields.
! * 
! * Returns:
! *
! * Notes:
! *
  subroutine m_calc_sat_percentiles(vis_bins, c2mc4_bins,  sunz_bins, nsize, id)
    use Quicksort
    implicit none

    real, intent(inout), dimension(0:) :: vis_bins, c2mc4_bins,  sunz_bins
    integer, intent(in) :: nsize
    type(input_data_t), intent(inout) :: id ! %vis75, %c2mc475, %sunz50
  
    real :: t, index, decimal
    integer :: lower, upper, a, b

    t = 0

!print *, "nsize=",nsize

    if (nsize > 1) then

       ! sort the bins in ascending order
       ! Visible bins
!print *, "vis0", vis_bins(0:nsize-1)
       call quick_sort(vis_bins(0:nsize-1), nsize)
!print *, "vis1", vis_bins(0:nsize-1)
       ! Ch2mCh4 bins
!print *, "m0", c2mc4_bins(0:nsize-1)
       call quick_sort(c2mc4_bins(0:nsize-1), nsize)
!print *, "m1", c2mc4_bins(0:nsize-1)
       ! Sun Zenith bins
!print *, "sun0",sunz_bins(0:nsize-1)
       call quick_sort(sunz_bins(0:nsize-1), nsize)
!print *, "sun1", sunz_bins(0:nsize-1)

       ! Calculate the percentiles 
       ! 75th for vis and c2mc4
       index = real(nsize + 1) * 0.75
       lower = floor(index) - 1
       upper = ceiling(index) - 1
       decimal = index - floor(index)  
       ! add conditions -- Dec 2011 - Y. Mao
       if(vis_bins(lower)>MISSING+1. .and. vis_bins(upper)>MISSING+1.) then
          id%vis75 = vis_bins(lower) * (1.0 - decimal) + vis_bins(upper) * decimal
       else if(vis_bins(lower) < MISSING+1.) then
          id%vis75 = vis_bins(upper)
       else
          id%vis75 = vis_bins(lower)
       end if
       ! add conditions -- Dec 2011 - Y. Mao
       if(c2mc4_bins(lower)>MISSING+1. .and. c2mc4_bins(upper)>MISSING+1.) then
          id%c2mc475 = c2mc4_bins(lower) * (1.0 - decimal) + c2mc4_bins(upper) * decimal
       else if(c2mc4_bins(lower)<MISSING+1.) then
          id%c2mc475 =  c2mc4_bins(upper)
       else
          id%c2mc475 =  c2mc4_bins(lower)
       end if

       ! 50th for sunz
       index = real(nsize + 1) * 0.5
       lower = floor(index) - 1
       upper = ceiling(index) - 1
       decimal = index - floor(index)  
       ! add conditions -- Dec 2011 - Y. Mao
       if(sunz_bins(lower)>MISSING+1. .and. sunz_bins(upper)>MISSING+1.) then
          id%sunz50 = sunz_bins(lower) * (1.0 - decimal) + sunz_bins(upper) * decimal
       else if(sunz_bins(lower)<MISSING+1.) then
          id%sunz50 = sunz_bins(upper)
       else
          id%sunz50 = sunz_bins(lower)
       end if

       if(id%sunz50 < 0.0) id%sunz50 = 0.0
       
    else ! nsize <= 1
       id%vis75 = vis_bins(0)
       id%c2mc475 = c2mc4_bins(0)
       id%sunz50 = sunz_bins(0)
       if(id%sunz50 < 0.0)  id%sunz50 = 0.0
       
    end if

    return
  end subroutine m_calc_sat_percentiles


!**********************************************************************
! * subroutine: run_layers()
! *
! * Description:
! * 
! * Returns: Nothing
! *
! * Notes: Base (int_d.bases) refers to the level just above the next cloud
! *   below the existing cloud. It does not represent actual cloud base
! *   (i.e., physical bottom of the cloud), which is represented by
! *   int_d.moist_bases(:)).
! *
  subroutine run_layers(nz, id, int_d)
    implicit none

    integer, intent(in) :: nz
    type(input_data_t), intent(in) :: id
    type(interp_data_t), intent(inout) :: int_d

    integer ::  cloud_on, dry, sat_cldbase, orig_cnt, cbk
    real :: thick(0:MAX_CLOUD_LAYERS-1)

    integer :: k, n

    dry = 0

    ! Initialize arrays, size=nz
    int_d%layer_num(:) = 0
    int_d%dz_cloudbase(:) = 0.0
    int_d%cbz(:) = 0.0

    ! Initialize layer-based arrays, size=MAX_CLOUD_LAYERS
    thick(0) = 0.0
    int_d%tops(int_d%cnt:) = 0 
    int_d%bases(int_d%cnt:) = 0
    thick(int_d%cnt:) = 0.0
    int_d%moist_bases(int_d%cnt:) = nz

    ! If satellite-inferred cloud exists cnt = 1, otherwise cnt = 0
    orig_cnt = int_d%cnt

    if_1_orig_cnt: if(orig_cnt == 0) then
       ! No satellite cloud, assign dummy values.
       ! Only model data will be used to look for cloud layers.

       ! Initialize arrays, size=nz
       int_d%ctt(:) = CLOUD_TEMP_K_DEFAULT
       int_d%gdcp_liq_cld_frac(:) = MISSING ! not used yet

       ! Set cloud_on to 0 so that RUC layer detection begins outside of a cloud.
       cloud_on = 0
       sat_cldbase = 32
    else 
       ! Satellite-inferred cloud exists.
       ! Set cloud_on to 1 so that RUC layer detection begins within a cloud.
       sat_cldbase = int_d%bases(0)
       cloud_on = 1
    end if if_1_orig_cnt

    ! Identify discrete cloud layers.

    ! Loop from one level below the lowest satellite cloud level to the METAR-inferred cloud base.
    loop_k_base: do k = sat_cldbase-1, int_d%cloud_baseK, -1

!write(*,*) id%rh(k), sat_cldbase, int_d%cloud_baseK, cloud_on, int_d%cnt, dry

       if_cloudon: if(cloud_on == 1) then 
          if(id%rh(k) > 0.5) then
             ! This level is wet enough if it is already in a cloud.
             !  So push the base of the current cloud to this level.
             int_d%bases(int_d%cnt - 1) = k
          else
             !  We were in a cloud, and just went "out of cloud" because the current
             !   level is too dry. Turn cloud monitor off and increment dry layer
             !   counter by 1. Push cloud base down because this level is in the previous
             !   cloud layer until a new cloud is detected (cloud base is defined as one
             !   level above the next downward cloud).
             cloud_on=0 
             int_d%bases(int_d%cnt - 1) = k
             dry = dry + 1
          end if ! if(id%rh(k) > 0.5)
       else ! if(cloud_on == 0)
          ! Current level is "out of cloud".
          if (id%rh(k) >= 0.7) then
             ! Turn cloud monitor flag back on.
             cloud_on = 1
             ! Reset dry layer counter.
             dry = 0
             ! This level is wet enough to be new a cloud, but did enough dry levels
             !  precede this layer to qualify as a new cloud. If, however, this is the first
             !  wet level (i.e., no satellite-inferred cloud), three dry levels are not required.
             if(dry >= 3 .or. int_d%cnt == 0) then
                ! Increment cloud layer counter.
                int_d%cnt = int_d%cnt + 1
                ! Assign top level of new cloud.
                int_d%tops(int_d%cnt - 1) = k+1
             ! end branch (dry >= 3)
             else if(dry < 3) then
                ! Not enough intervening dry layers for current layer (although wet)
                !  to qualify as a "new" cloud. Level is still in previous cloud.
                ! Since this level has been determined to be in the previous cloud,
                !  extend the cloud base down to the current level.
                int_d%bases(int_d%cnt - 1) = k
             end if ! end (dry >= 3)
          ! End branch (rh >= 70%) conditional.
          else ! if(id%rh(k) < 0.7) 
             ! Because this level is "out of cloud", it is too dry to count as a new cloud.
             !  Increment dry layer counter.
             dry = dry + 1
          end if ! End of id%rh conditionals.
       end if if_cloudon

    end do loop_k_base
    ! End of cloud layer detection.

!write(*,*) int_d%cnt, cloud_on, sat_cldbase
    if_cnt: if(int_d%cnt > 0) then
       ! At least one cloud layer exists.

       ! Force the base of the lowest cloud layer to be the surface
       !  (METAR) cloud observation level.
       if(sat_cldbase-1 >= int_d%cloud_baseK) then
          int_d%bases(int_d%cnt - 1) = int_d%cloud_baseK
       end if

       ! Calculate the thickness of each cloud layer (not a model layer)
       thick(0:int_d%cnt-1) = id%hgt(int_d%tops (0:int_d%cnt-1)) - &
                              id%hgt(int_d%bases(0:int_d%cnt-1))

       !================================================!
       ! set (ctt, dz, layer_num) for each cloud layer
       !================================================!

       ! loop through cloud layers
       loop_cloud_n: do n = 0, int_d%cnt-1 

          ! check if last k and move base up
          if(int_d%tops(n) == 0) then ! impossible - Y Mao
             int_d%tops(n)=int_d%cloud_baseK 
          end if

          ! check if last k and move base up
          if(int_d%tops(n + 1) == 0) then
             int_d%tops(n + 1) = int_d%cloud_baseK
          end if

          ! Note that this is a loop through model layers (not cloud layers).
          loop_model_k: do k = int_d%tops(n), int_d%tops(n+1), -1

             ! Assign same thickness to each model level within the current cloud layer.
             ! which layer number
             int_d%layer_num(k) = n+1

             if(orig_cnt > 0 .and. n == 0) then
             ! 'satellite layer' exists
             ! This layer has detailed CTT info from satellite: ctt(bases(0):tops(0))

                ! Assign the lowermost cloud-top temperature and liquid fraction values
                ! to all new levels between sat_cldbase and next/lower cloud top
                if (k < sat_cldbase) then
                   int_d%ctt(k) = int_d%ctt(sat_cldbase)
                   int_d%gdcp_liq_cld_frac(k) = int_d%gdcp_liq_cld_frac(sat_cldbase)
                end if
             else
                int_d%ctt(k) = id%temps(int_d%tops(n)) 
                int_d%gdcp_liq_cld_frac(k) = int_d%gdcp_liq_cld_frac(sat_cldbase+1)
             end if

          end do loop_model_k

!write(*,*) "in the loop ", int_d%topoK,  int_d%cloud_baseK, int_d%tops(n), int_d%tops(n + 1), int_d%bases(n + 1), k, orig_cnt, k,  int_d%layer_num(k)

       end do loop_cloud_n

       !================================================!
       ! Find the lowest cloud base k level, int_d%cloud_baseK 
       ! may be all the way to the surface if precipitating
       !================================================!
       if (id%cloud_base > 0.0) then
          do k = 0, nz-1
             if (id%hgt(k) > id%cloud_base) then
                cbk = k - 1
                exit
             end if
          end do
       else 
          cbk = int_d%bases(int_d%cnt-1) - 1
       end if
       ! Make sure that cloud base isn't below the topography
       if (cbk < int_d%topoK)    cbk = int_d%topoK

    end if if_cnt
    ! the if should end here, instead of after the k loop -- Yali
    ! otherise in the above i loop, int_d%bases(int_d%cnt-1) might be out of bound

!write(*,*) int_d%cnt, cloud_on, sat_cldbase, int_d%tops(0), cbk

    !================================================!
    ! Calculate the moisture base (k level of RH >= 70%), 
    ! CBZ (at moisture base) and height above CBZ for each level.
    !================================================!

    ! If there is only one layer then it's an easy task
    if (int_d%cnt == 1) then
       int_d%moist_bases(0) = cbk
       do k = int_d%tops(0), cbk, -1
          int_d%cbz(k) = id%hgt(cbk)
          int_d%dz_cloudbase(k) = (id%hgt(k) - int_d%cbz(k)) * 3.281
       end do
    else if (int_d%cnt > 1) then
       cloud_on = 0

       ! Find the moisture base k levels first
       do k = int_d%tops(0), cbk, -1

          n = int_d%layer_num(k) - 1

!write(*,*) i, cbk

          if_k3: if (k >= 3) then
             if (id%rh(k-1) < 0.5 .and. id%rh(k-2) < 0.7 .and. &
                 id%rh(k-3) < 0.7 .and. cloud_on == 0) then
                int_d%moist_bases(n) = k
                cloud_on = 1
             else if (int_d%layer_num(k-1) == int_d%layer_num(k) + 1 .and. &
                      cloud_on == 0) then
                int_d%moist_bases(n) = k
                cloud_on = 1
             else if (int_d%layer_num(k) == int_d%cnt .and. &
		      cloud_on == 0) then
                int_d%moist_bases(n) = cbk
                cloud_on = 1
             end if
             ! Move to here since cloud_on is used only when k>=3.
             ! And avoid the case k==0  - Y Mao
             if (int_d%layer_num(k) /= int_d%layer_num(k-1))  cloud_on = 0
          else
             ! Too close to the surface so we have the lowest cloud layer
             int_d%moist_bases(n) = cbk
          end if if_k3

       end do ! k loop

       ! Apply the cbz and dz_cloudbase to each cloudy 
       ! point in the layer
       do n = 0, int_d%cnt-1
          do k = int_d%tops(n), int_d%moist_bases(n), -1
             int_d%cbz(k) = id%hgt(int_d%moist_bases(n))
             int_d%dz_cloudbase(k) = (id%hgt(k) - int_d%cbz(k)) * 3.281
          end do ! k loop
       end do    ! n loop
  
    end if ! if (int_d%cnt == 1)

!if(int_d%cnt>=1) write(*,*) "layers number=", int_d%cnt, int_d%topoK,  int_d%tops(0:2),  int_d%bases(0:2)


    return
  end subroutine run_layers

end module Algo
