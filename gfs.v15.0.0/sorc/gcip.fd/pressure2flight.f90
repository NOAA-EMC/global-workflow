!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!
! Module:      Pressure2Flight
!
! Author:      Yali Mao
!
! Date:        January 2011
!
! Note:        In metar. Severity Category is included.
!
!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

module Pressure2Flight

  use Kinds

  use Icing, only : mapSev2Cat, SLD_SPECIAL_VALUE

  IMPLICIT NONE
  private
  public runPressure2Flight

  real, parameter :: DELTA_HEIGHT = 10.0
  real, parameter :: FEET_TO_METERS = 0.3048

  ! In foot, to be converted to metar in the final output
  real, parameter :: START_FLIGHT_LEVEL = 1000.
  real, parameter :: FLIGHT_LEVEL_DZ = 1000.0
  integer, parameter :: NUM_FLIGHT_LEVELs = 30

  real, parameter :: m_EPSILON = 10.e-4

contains


!**********************************************************************
! * subroutine: runPressure2Flight()
! *
! * Description: 
! * 
! * Returns:
! *
! * Notes: 
! *
  subroutine runPressure2Flight(kgds, height, outdat, outdat_FL, iret)
    integer, intent(in) :: kgds(:)
    real, intent(in) :: height(:,:,:)
    type(icing_output_t), target, intent(in):: outdat
    type(icing_output_t), target, intent(inout):: outdat_FL
    integer, intent(out) :: iret

    real, dimension(:, :, :), pointer :: iceProbability, sldPotential, iceSeverity
    real, dimension(:, :, :), pointer :: iceProbability_FL, sldPotential_FL, iceSeverity_FL
    real, pointer :: flightLevels(:)
    real :: flightlevel

    integer :: nx ,ny, nz, nzFL

    integer :: i, j, k, m


    nx = kgds(2)
    ny = kgds(3)
    nz = size(height, dim=3)
    nzFL = NUM_FLIGHT_LEVELs

    allocate(outdat_FL%levels(nzFL), stat=iret)
    allocate(outdat_FL%severity(nx, ny, nzFL), stat=iret)
    allocate(outdat_FL%probability(nx, ny, nzFL), stat=iret)
    allocate(outdat_FL%sld(nx, ny, nzFL), stat=iret)
    outdat_FL%levels = MISSING
    outdat_FL%severity = MISSING
    outdat_FL%probability = MISSING
    outdat_FL%sld = MISSING

    flightLevels =>  outdat_FL%levels
    iceProbability_FL => outdat_FL%probability
    sldPotential_FL => outdat_FL%sld
    iceSeverity_FL => outdat_FL%severity

    iceProbability => outdat%probability
    sldPotential => outdat%sld
    iceSeverity => outdat%severity

    do k = 1, nzFL
       ! By default, MISSING is for flight-level icing products.
       flightLevels(k) = (START_FLIGHT_LEVEL + &
               (k-1) * FLIGHT_LEVEL_DZ) * FEET_TO_METERS
    end do

    ! now perform the interpolation on icing
    do j = 1, ny
    do i = 1, nx
      do k = 1, nzFL
        ! By default, MISSING is for flight-level icing products.
        flightLevel = flightLevels(k)
        do m = 1, nz
          if (abs(flightLevel - height(i, j, m)) < DELTA_HEIGHT) then
          !===================================================================
          ! Flight level and pressure level are close enough. No need to interpolate.
            !--------------------------
            ! Icing Probability
            if (m_isDataValid(iceProbability, i, j, m)) &
                   iceProbability_FL(i, j, k) = iceProbability(i, j, m)

            if (iceProbability_FL(i, j, k) < 0.01) &
                   iceProbability_FL(i, j, k) = 0.0

            !--------------------------
            ! SLD
            if (m_isDataValid(sldPotential, i, j, m)) &
                   sldPotential_FL(i, j, k) = sldPotential(i, j, m)

            ! -0.1 indicates "icing was present, but there was not enough information 
            !                 to determine whether SLD were present".
            if (iceProbability_FL(i, j, k) < 0.01) then
               sldPotential_FL(i, j, k) = 0.0
            else
               if (sldPotential_FL(i, j, k) < 0.01) &
                    sldPotential_FL(i, j, k) = -0.1
            end if

            ! Severity
	    if (m_isDataValid(iceSeverity, i, j, m)) then
              iceSeverity_FL(i, j, k) = iceSeverity(i, j, m)

              if (iceSeverity_FL(i, j, k) <= 0.0) &
                   iceSeverity_FL(i, j, k) = MISSING
            end if

            exit

          else if ((height(i, j, m) > flightLevel) .and. (m > 1)) then
          !===================================================================
          !Flight level and pressure level are too far apart. Must interpolate.

            !--------------------------
            ! Icing Probability
            iceProbability_FL(i, j, k) = &
                   m_interp(iceProbability, height, i, j, m, flightLevel)
            if (iceProbability_FL(i, j, k) < 0.01) &
                   iceProbability_FL(i, j, k) = 0.0

            !--------------------------
            ! SLD
            if(abs(sldPotential(i, j, m) + 0.1) < m_EPSILON) then
              sldPotential_FL(i, j, k) = -0.1
            else if (abs(sldPotential(i, j, m-1) + 0.1) < m_EPSILON) then
              sldPotential_FL(i, j, k) = -0.1
            else
              sldPotential_FL(i, j, k) = &
                     m_interp(sldPotential, height, i, j, m, flightLevel)
            end if !  if(abs(sldPotential(i, j, m) + 0.1) < m_EPSILON)

            if (iceProbability_FL(i, j, k) < 0.01) then
               sldPotential_FL(i, j, k) = 0.0
            else
               if (sldPotential_FL(i, j, k) < 0.01) &
                    sldPotential_FL(i, j, k) = -0.1
            end if

            !--------------------------
            ! Severity
            iceSeverity_FL(i, j, k) = &
                   m_interp(iceSeverity, height, i, j, m, flightLevel)
            if (iceSeverity_FL(i, j, k) <= 0.0) &
                   iceSeverity_FL(i, j, k) = MISSING              

            exit

          end if !  if (abs(flightLevel - height(i, j, m)) < DELTA_HEIGHT)
        end do !  do m = 1, Hybrid2Pres_Params%num_pressure_levels

        !===================================================================
        ! prepare data to conversion to GRIB format

        !--------------------------
        ! Icing Probability
        iceProbability_FL(i, j, k) = max(iceProbability_FL(i, j, k), 0.)
        iceProbability_FL(i, j, k) = min(iceProbability_FL(i, j, k), 1.)

        !--------------------------
        ! SLD
        if (.not. m_isDataValid(sldPotential_FL, i, j, k)) &
             sldPotential_FL(i, j, k) = 0.0

        if (sldPotential_FL(i, j, k) < SLD_SPECIAL_VALUE) &
             sldPotential_FL(i, j, k) = SLD_SPECIAL_VALUE
        
        if (sldPotential_FL(i, j, k) > 1.0)   sldPotential_FL(i, j, k) = 1.0

        !===================================================================
        !--------------------------
        ! Severity Category
!        if  (m_isDataValid(iceSeverity_FL, i, j, k)) &
        iceSeverity_FL(i, j, k) = mapSev2Cat(iceSeverity_FL(i, j, k))

      end do ! do k = 1, Press2Flight_Params%num_flight_levels
    end do ! do i = 1, grid%nx
    end do ! do j = 1, grid%ny

    return
  end subroutine runPressure2Flight


!**********************************************************************
! * function:    m_isDataValid()
! *
! * Description: test if the data is valid.
! * 
! * Returns:    
! *
! * Notes: 
! *

  logical function m_isDataValid(data, i, j, k)
    real, dimension(:,:,:), intent(in) :: data
    integer, intent(in) :: i, j, k
    if (data(i, j, k) > MISSING + 10.) then
      m_isDataValid = .true.
    else
      m_isDataValid = .false.
    end if
    return
  end function m_isDataValid




!**********************************************************************
! * function:    m_interp()
! *
! * Description: performs the interpolation.
! * 
! * Returns:    
! *
! * Notes: 
! *

  real function m_interp(data, hgt, i, j, k, fl)
    real, dimension(:,:,:), intent(in) :: data, hgt
    integer, intent(in) :: i, j, k
    real, intent(in) :: fl

    real :: val_a, val_b

    val_a = 0.0
    val_b = 0.0

    if (m_isDataValid(data, i, j, k)) val_a = data(i, j, k)

    if (m_isDataValid(data, i, j, k-1)) val_b = data(i, j, k-1)

    m_interp = val_b + abs(fl - hgt(i, j, k-1)) * &
         ((val_a - val_b) / (hgt(i, j, k) - hgt(i, j, k-1)))
    return

  end function m_interp
end module Pressure2Flight
