!------------------------------------------------------------------------------
!
! MODULE: Derived_Fields
!
! DESCRIPTION:
!> Calculate derived fields
!
! REVISION HISTORY:
! September 2010, modified November 2011
! Feb 2014 - modified
!
!------------------------------------------------------------------------------

module Derived_Fields

  use Kinds

  IMPLICIT NONE

  private
  public calculateDerived_Fields

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! Parameters
  real, parameter :: EPSILON = 0.01
  real, parameter :: FREEZING = 273.15 ! degrees K

  real, parameter :: PASCAL2BAR = 0.00001
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! private members
  integer :: m_nx, m_ny, m_nz

contains

!------------------------------------------------------------------------------
! Description
!> Calculate values of derived fields
!
! Notes
!> For RAP, calculate value of relativeHumidity
!> GFS has different ice condensate and liquid condensate from RAP and NAM 
!------------------------------------------------------------------------------

  subroutine calculateDerived_Fields(nx, ny, nz, cmodel, fields, modelData, iret)
    IMPLICIT NONE

    integer, intent(in) :: nx, ny, nz
    character(*), intent(in) :: cmodel ! RAP NAM GFS
    type(model_fields3D_t), target, INTENT(inout) :: fields ! spfh to wvm
    type(model_inputs_t),   target, intent(inout) :: modelData
    integer, intent(out):: iret

    ! Two fields are temporary for calculating equivPotentialTemp
    real, dimension(:,:,:), allocatable :: dewPointTemp
    real, dimension(:,:,:), allocatable :: liftedCondLevelTemp

    m_nx = nx
    m_ny = ny
    m_nz = nz

    iret = -1

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! allocate memory
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    ! allocate memory for temporary variable
    allocate(dewPointTemp(nx, ny, nz))
    allocate(liftedCondLevelTemp(nx, ny, nz))

    ! allocate memories for modelData
    ! 3D original
    allocate(modelData%  h(nx, ny, nz)) ! geopotentialHeight
    allocate(modelData%pvv(nx, ny, nz)) ! pressureVerticalVelocity
    allocate(modelData%  p(nx, ny, nz)) ! pressure
    allocate(modelData%  t(nx, ny, nz)) ! temperature
    ! 3D original/derived
    allocate(modelData% rh(nx, ny, nz)) ! relativeHumidity, derived for RAP
    ! 3D derived
    allocate(modelData%ept(nx, ny, nz)) ! equivPotentialTemp     , MISSING
    allocate(modelData%icc(nx, ny, nz)) ! iceCondensate          , MISSING
    allocate(modelData%lqc(nx, ny, nz)) ! liquidCondensate       , MISSING
    allocate(modelData%slw(nx, ny, nz)) ! superCooledLiquidWater , MISSING
    allocate(modelData%twp(nx, ny, nz)) ! totalWaterPath         , 0.0
    ! 2D original, topography only, allocated when reading model data
    ! 2D derived, no data

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Assignment and calculations
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Assign basic 3D values from model fields to  modelData
    modelData% h(:,:,:)   = fields%h(:,:,:)
    modelData% pvv(:,:,:) = fields%pvv(:,:,:)
    modelData% p(:,:,:)   = fields%p(:,:,:)
    modelData% t(:,:,:)   = fields%t(:,:,:) 

    ! convert specific humidity to vapor mixing ratio
    where(fields%wvm > MISSING+1.) fields%wvm = fields%wvm / (1.-fields%wvm)

    ! equivPotentialTemp
    dewPointTemp = m_calcDewPointTemperature(fields%p, fields%wvm)
    liftedCondLevelTemp = m_calcLiftedCondLevelTemp(fields%t, dewPointTemp)
    modelData%ept = m_calcEquivPotentialTemp(fields%p, fields%t, fields%wvm, liftedCondLevelTemp)

    ! relative humidity
    if(cmodel == "RAP") then
       modelData%rh = m_calcRelativeHumidity(fields%t, fields%wvm, fields%p)
    else        ! GFS NAM
       modelData%rh = fields%rh
    endif

    ! condensate and supercooled liquid water
    if(cmodel == "GFS") then
       ! use the same approach as in /nwprod/./sorc/global_fcst.fd/radiation_clouds.f
       call m_calcCondensate(fields%t, fields%cwm, modelData%icc, modelData%lqc, modelData%slw)

    else   ! RAP NAM
       modelData%icc = m_calcIceCondensate(fields%icm, fields%snm, fields%gpm)
       modelData%lqc = m_calcLiquidCondensate(fields%rwm, fields%cwm)
       modelData%slw = m_calcSuperCooledLiquidWater(fields%t, fields%rwm, fields%cwm)
    endif

    ! for icing severity
    call m_calcTotalWaterPath(fields%h, fields%p, fields%t, &
                              modelData%icc + modelData%lqc, modelData%twp)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! deallocate memories for temporary variables 
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    deallocate(dewPointTemp)
    deallocate(liftedCondLevelTemp)

    iret = 0
    return
  end subroutine calculateDerived_Fields

!------------------------------------------------------------------------------
! Description
!> Calculate the relative humidity
!
!> @param[in] temperature           - in degrees K.
!> @param[in] waterVaporMixingRatio - in kg/kg.
!> @param[in] pressure              - in Pascals.
!> @Returns:  relative humidity value in %
!------------------------------------------------------------------------------

  elemental real function m_calcRelativeHumidity(&
                                temperature, waterVaporMixingRatio, pressure)
    real, intent(in) ::  temperature, waterVaporMixingRatio, pressure

    real, parameter :: MAX_RH = 105.

    real :: eSubS, e

    m_calcRelativeHumidity = MISSING
    if ((temperature < MISSING + 1.) .or. &
        (waterVaporMixingRatio < MISSING + 1.) .or. &
        (pressure < MISSING + 1.)) then
      return
    end if

    eSubS =  m_calcESubS(temperature)
    e = waterVaporMixingRatio * pressure / 0.622
    if (abs(eSubS) >  EPSILON) then
      e = 100.0 * e / eSubS !! make units percent
      if (e > MAX_RH)  e = MAX_RH

      m_calcRelativeHumidity = e
    end if

    return
  end function m_calcRelativeHumidity

!------------------------------------------------------------------------------
! Description
!> Calculate the dew point temperature
!
!> @param[in] pressure              - in Pascals.
!> @param[in] waterVaporMixingRatio - in kg/kg.
!> @Returns:  dew point temperature in degrees K
!------------------------------------------------------------------------------

  elemental real function m_calcDewPointTemperature(pressure, waterVaporMixingRatio)
    real, intent(in) :: pressure, waterVaporMixingRatio

    real :: rr, es, esln, denom

    m_calcDewPointTemperature = MISSING
    if ((pressure < MISSING + 1.) .or. &
        (waterVaporMixingRatio < MISSING + 1.)) then
      return
    end if

    rr = waterVaporMixingRatio + 1.e-8
    es = pressure * rr / (0.622+rr)
    esln = log(es)
    denom = esln - 23.6837
    if (abs(denom) >  EPSILON) then
      m_calcDewPointTemperature  = (35.86*esln - 4947.2325) / denom
    end if

    return
  end function m_calcDewPointTemperature

!------------------------------------------------------------------------------
! Description
!> Calculate Lifted Condensation Level Temperature.
!
!> @param[in] temperature  - in degrees K.
!> @param[in] dewPointTemp - in degrees K
!> @Returns:  Lifted Condensation Level Temperature in degrees K
!------------------------------------------------------------------------------

  elemental real function m_calcLiftedCondLevelTemp(temperature, dewPointTemp)
    real, intent(in) :: temperature, dewPointTemp

    real :: denom

    m_calcLiftedCondLevelTemp = MISSING
    if ((temperature < MISSING + 1.) .or. &
        (dewPointTemp < MISSING + 1.)) then
      return
    end if

    denom =  1.0/(dewPointTemp - 56.0) + log(temperature/dewPointTemp)/800.0
    if (abs(denom) >  EPSILON) then
      m_calcLiftedCondLevelTemp = ( 1.0 / denom ) + 56.0
    end if

    return
  end function m_calcLiftedCondLevelTemp

!------------------------------------------------------------------------------
! Description
!> Calculate equivalent potential temperature
!
!> @param[in] pressure              - in Pascals.
!> @param[in] temperature           - in degrees K.
!> @param[in] waterVaporMixingRatio - in kg/kg.
!> @param[in] liftedCondLevelTemp   - in degrees K
!> @Returns:  equivalent potential temperature in degrees K
!------------------------------------------------------------------------------

  elemental real function m_calcEquivPotentialTemp(&
                                pressure, temperature, waterVaporMixingRatio, liftedCondLevelTemp)
    real, intent(in) :: pressure, temperature, waterVaporMixingRatio, liftedCondLevelTemp

    real :: rr, p1, p2, xx
    real :: x, power

    m_calcEquivPotentialTemp = MISSING
    if ((pressure < MISSING + 1.) .or. &
        (temperature < MISSING + 1.) .or. &
        (waterVaporMixingRatio < MISSING + 1.) .or. &
        (liftedCondLevelTemp < MISSING + 1.)) then
      return
    end if

    rr = waterVaporMixingRatio + 1.0e-8
    power = 0.2854 * (1.0 - 0.28*rr) ! Double precision
    x = 1.0/(pressure*PASCAL2BAR) ! Double precision
    xx = temperature * (x**power)
    p1 = 3.376/liftedCondLevelTemp - 0.00254
    p2 = (rr*1000.0) * (1.0 + 0.81*rr)
    
    m_calcEquivPotentialTemp = xx * exp(p1*p2)

    return
  end function m_calcEquivPotentialTemp

!------------------------------------------------------------------------------
! Description
!> Calculate ice condensate
!
!> @param[in] iceMixingRatio     - in kg/kg
!> @param[in] snowMixingRatio    - in kg/kg
!> @param[in] graupelMixingRatio - in kg/kg
!> @return  ice condensate in g/kg
!------------------------------------------------------------------------------

  elemental real function m_calcIceCondensate(iceMixingRatio, snowMixingRatio, graupelMixingRatio)
    real, intent(in) :: iceMixingRatio, snowMixingRatio, graupelMixingRatio

    m_calcIceCondensate = MISSING
    if ((iceMixingRatio < MISSING + 1.) .or. &
        (snowMixingRatio < MISSING + 1.) .or. &
        (graupelMixingRatio < MISSING + 1.)) then
      return
    end if

    ! convert from kg/kg to g/kg
    m_calcIceCondensate = 1000.0 * (iceMixingRatio + snowMixingRatio + graupelMixingRatio)

    return
  end function m_calcIceCondensate


!------------------------------------------------------------------------------
! Description
!> Calculate liquid condensate
!
!> @param[in] rainWaterMixingRatio  - in kg/kg
!> @param[in] cloudWaterMixingRatio - in kg/kg
!> @return  liquid condensate in g/kg
!------------------------------------------------------------------------------

  elemental real function m_calcLiquidCondensate(rainWaterMixingRatio, cloudWaterMixingRatio)
    real, intent(in) :: rainWaterMixingRatio, cloudWaterMixingRatio

    m_calcLiquidCondensate = MISSING
    if ((rainWaterMixingRatio < MISSING + 1.) .or. &
        (cloudWaterMixingRatio < MISSING + 1.)) then
      return
    end if

    ! convert from kg/kg to g/kg
    m_calcLiquidCondensate  = 1000.0 * (rainWaterMixingRatio + cloudWaterMixingRatio)

    return
  end function m_calcLiquidCondensate

!------------------------------------------------------------------------------
! Description
!> Calculate supercooled liquid water
!
!> @param[in] temperature           - in degrees K.
!> @param[in] rainWaterMixingRatio  - in kg/kg
!> @param[in] cloudWaterMixingRatio - in kg/kg
!> @return   supercooled liquid water in g/kg
!------------------------------------------------------------------------------

  elemental real function m_calcSuperCooledLiquidWater(&
                                temperature, rainWaterMixingRatio, cloudWaterMixingRatio)
    real, intent(in) :: temperature, rainWaterMixingRatio, cloudWaterMixingRatio

    m_calcSuperCooledLiquidWater = MISSING
    if ((temperature < MISSING + 1.) .or. &
        (cloudWaterMixingRatio < MISSING + 1.) .or. &
        (rainWaterMixingRatio < MISSING + 1.)) return

    m_calcSuperCooledLiquidWater = 0.0
    ! convert from kg/kg to g/kg
    if(temperature <= FREEZING) m_calcSuperCooledLiquidWater = 1000.0 * (cloudWaterMixingRatio + rainWaterMixingRatio)

    return
  end function m_calcSuperCooledLiquidWater

!------------------------------------------------------------------------------
! Description
!> Same as the approach in /nwprod/./sorc/global_fcst.fd/radiation_clouds.f
!
!> @param[in] temperature             - in degrees K
!> @param[in] cloudWaterMixingRatio   - in kg/kg
!> @param[out] iceCondensate          - in g/kg
!> @param[out] liquidCondensate       - in g/kg
!> @param[out] superCooledLiquidWater - in g/kg
!------------------------------------------------------------------------------

  elemental subroutine m_calcCondensate(temperature, cloudWaterMixingRatio, &
                                       iceCondensate, liquidCondensate, superCooledLiquidWater)
    real, intent(in) :: temperature
    real, intent(in) :: cloudWaterMixingRatio
    real, intent(out) :: iceCondensate, liquidCondensate, superCooledLiquidWater

    real :: tem2d

    iceCondensate = MISSING
    liquidCondensate = MISSING
    superCooledLiquidWater = MISSING

    if((temperature < MISSING + 1.) .or. (cloudWaterMixingRatio < MISSING + 1.)) return

    tem2d = min(1., max(0.0, (FREEZING-temperature)*0.05))
    if(cloudWaterMixingRatio * 1000. >= 0.01) then ! avoid noises from GFS master file
       iceCondensate = cloudWaterMixingRatio * tem2d
       liquidCondensate = cloudWaterMixingRatio - iceCondensate
    else
       iceCondensate = 0.
       liquidCondensate = 0.
    endif

    if (temperature <= FREEZING) then
       superCooledLiquidWater = liquidCondensate
    else
       superCooledLiquidWater = 0.
    end if

    ! convert from kg/kg to g/kg
    iceCondensate = 1000.0 * iceCondensate
    liquidCondensate = 1000.0 * liquidCondensate
    superCooledLiquidWater = 1000.0 * superCooledLiquidWater
    return
  end subroutine m_calcCondensate

!------------------------------------------------------------------------------
! Description
!> Calculate total water path, for icing severity
!
!> @param[in] height          - in meter
!> @param[in] pressure        - in Pascals
!> @param[in] temperature     - in degrees K
!> @param[in] totalCondensate - in g/kg
!> @param[out] totalWaterPath
!------------------------------------------------------------------------------

   subroutine m_calcTotalWaterPath(height, pressure, temperature, &
                                   totalCondensate, totalWaterPath)
    real, dimension(:, :, :), intent(in) :: height, pressure, temperature, &
                                         totalCondensate
    real, dimension(:, :, :), intent(out) :: totalWaterPath

    real :: integratedCond
    integer :: topIdx, baseIdx

    integer :: i, j, k, m

    do j = 1, m_ny
    do i = 1, m_nx

      do k = m_nz, 1, -1

         topIdx = -1
         baseIdx = 1

         !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
         ! get the layer top and base for the total condensate layer
         !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
         do m = k, m_nz - 1

            if (totalCondensate(i,j,m) < MISSING + 1.) cycle

            if (totalCondensate(i,j,m) > 0.001) then
               topIdx =m
            else
               exit
            end if
         end do ! do m = k, m_nz - 1

         do m = k, 1, -1

            if (totalCondensate(i,j,m) < MISSING + 1.) cycle

            if (totalCondensate(i,j,m) > 0.001) then
               baseIdx =m
            else
               exit
            end if
         end do ! do m=k, 2, -1

         !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
         ! get the integrated condensate from the rep to the layer top
         !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
         integratedCond = 0.0
         ! assert(top <= (m_nz - 2))
         do m = topIdx, baseIdx, -1
            if(temperature(i, j, m) <= FREEZING) then
               if (totalCondensate(i, j, m) < MISSING + 1.) cycle

               integratedCond = integratedCond + (((totalCondensate(i,j,m) * pressure(i, j, m))/ &
                                (GAS_CONSTANT * temperature(i, j, m))) * &
                                (height(i, j, m+1) - height(i, j, m)) )
            end if
         end do ! do m = topIdx, baseIdx, -1

         totalWaterPath(i, j, k) = integratedCond
      end do ! do k = m_nz, 1, -1
      
    end do ! do i = 1, m_nx
    end do ! do j = 1, m_ny

    return
  end subroutine m_calcTotalWaterPath

!------------------------------------------------------------------------------
! Description
!> Calculate ESubS
!
!> @param[in] temp       - in celcius
!------------------------------------------------------------------------------

  elemental  function m_calcESubS(temp)
    real :: m_calcESubS
    real, intent(in) :: temp

    real, parameter :: coeffs(9) = (/ 610.5851, 44.40316, 1.430341, 0.02641412, &
                           2.995057e-4, 2.031998e-6, 6.936113e-9, 2.564861e-12, -3.704404e-14 /)

    real :: x 

    x= max(-80.0, temp - 273.15)

    m_calcESubS = coeffs(1) + x*(coeffs(2) + x*(coeffs(3) + x*(coeffs(4) + x*(coeffs(5) + &
		 x*(coeffs(6) + x*(coeffs(7) + x*(coeffs(8) + x*coeffs(9))))))))

    return
  end function m_calcESubS

end module Derived_Fields
