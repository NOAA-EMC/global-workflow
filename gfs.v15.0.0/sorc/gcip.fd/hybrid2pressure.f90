!------------------------------------------------------------------------------
!
! MODULE: Hybrid2Pressure (for model on Hybrid/Sigma level)
!
! REVISION HISTORY:
! September 2010, modified November 2011
! March 2014 - modified
!
!------------------------------------------------------------------------------
module Hybrid2Pressure

  use Kinds

  IMPLICIT NONE

  private
  public initHybrid2Pressure, processHybrid2Pressure

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! Parameters
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  real, parameter :: m_GAMMA = 0.0065 ! 6.5 K/km
  real, parameter :: m_GAMD = 0.0100 ! 10  K/km
  real, parameter :: m_CPD_P = 1004.686 ! J/(kg*K) Specific heat at constant pressure
  real, parameter :: m_ROVCP_P = 0.285714 ! non-dim. <RD>/<CPD>
  real, parameter :: m_G0_P = 9.80665 ! m/(s*s)
  real, parameter :: m_RD_P = 287.04 ! gas constant
  real, parameter :: m_PA_2_BAR = 0.00001 ! converts Pa to bars

  integer, parameter :: m_NZP_P = 8
  real, parameter, dimension(m_NZP_P) :: m_pmLev = &
       (/ 85000., 70000., 50000., 40000., 30000., 25000., 20000., 15000. /)

  ! smooth factors
  type :: individual_smooth_info_t 
     logical :: extrap_at_top 
     logical :: smooth
     integer :: passes(8)
     real    :: factor
  end type individual_smooth_info_t
  type :: smooth_info_t   
     integer :: below_ground_level_index
     integer :: lapse_rate_top_index
     integer :: extrapolate_offset
     ! individual smooth info
     type(individual_smooth_info_t) :: hgt
     type(individual_smooth_info_t) :: tmp
     type(individual_smooth_info_t) :: pvv
     type(individual_smooth_info_t) :: wvm
     type(individual_smooth_info_t) :: cwm
     type(individual_smooth_info_t) :: rwm
     type(individual_smooth_info_t) :: snm
     type(individual_smooth_info_t) :: icm
     type(individual_smooth_info_t) :: gpm
  end type smooth_info_t
  type(smooth_info_t), parameter :: SMOOTH = smooth_info_t(4, 5, 3, &
     individual_smooth_info_t(.true., .true., (/ 3,2,2,2,2,2,2,3 /), 0.5), &
     individual_smooth_info_t(.true., .true., (/ 2,1,1,1,1,1,1,1 /), 0.5), &
     individual_smooth_info_t(.true., .true., (/ 1,1,1,1,1,1,1,1 /), 0.5), &
     individual_smooth_info_t(.true., .true., (/ 1,1,1,1,1,1,1,1 /), 0.5), &
     individual_smooth_info_t(.true., .true., (/ 1,1,1,1,1,1,1,1 /), 0.5), &
     individual_smooth_info_t(.true., .true., (/ 1,1,1,1,1,1,1,1 /), 0.5), &
     individual_smooth_info_t(.true., .true., (/ 1,1,1,1,1,1,1,1 /), 0.5), &
     individual_smooth_info_t(.true., .true., (/ 1,1,1,1,1,1,1,1 /), 0.5), &
     individual_smooth_info_t(.true., .true., (/ 1,1,1,1,1,1,1,1 /), 0.5) )

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! private members
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  integer :: m_nx, m_ny, m_nz, m_np
  ! initialized from pressure_H
  ! hybrid level arrays
  real, allocatable :: m_exnerVals(:, :, :), m_delta(:, :, :)
  real, allocatable :: m_supportPoints(:)
  real, allocatable :: m_hold(:, :) ! 2D: m_hold(m_nx, 2)
  !  pressure level tool arrays
  integer, allocatable :: m_ksm(:)
  real,    allocatable :: m_exnerLevels(:), m_pressureLevels(:)

contains

  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Allocate memories for private members
  !> Initialize the value of pressure, one of the pressure-level fields
  !> Allocate memories for fields at pressure level
  !
  !> @param[in] nx         - model dimension
  !> @param[in] ny         - model dimension
  !> @param[in] nz         - number of hybrid levels
  !> @param[in] np         - number of pressure levels
  !> @param[in] p          - pressure levels
  !> @param[in] pressure_H - pressure values, on hybrid levels
  !> @param[out] fields    - type of model_fields3D_t, on pressure levels
  !
  !------------------------------------------------------------------------------

  subroutine initHybrid2Pressure(nx, ny, nz, np, p, pressure_H, fields)
    IMPLICIT NONE
    integer, intent(in) :: nx, ny
    integer, intent(in) :: nz     ! number of Hybrid   levels
    integer, intent(in) :: np     ! number of pressure levels
    real,    intent(in) :: p(np)  ! pressure levels
    real ,   intent(in) :: pressure_H(nx, ny, nz)
    type(model_fields3D_t), intent(inout) :: fields

    integer :: k
    integer :: kret   ! To generally locate an exception.

    m_nx = nx
    m_ny = ny
    m_nz = nz
    m_np = np

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! allocate private members on hybrid level
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    allocate(m_exnerVals    (nx, ny, nz)) !3-D array
    allocate(m_delta        (nx, ny, nz)) !3-D array
    allocate(m_supportPoints(nz))         !1-D array, changed on (i,j)
    allocate(m_hold         (nx, 2))      !2-D array

     
    m_exnerVals(:,:,:) = 0.0
    where (pressure_H(:,:,:) > MISSING + 1.)
       m_exnerVals(:,:,:) = m_calcExner(pressure_H(:,:,:))
    end where

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! allocate private members on pressure level
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    allocate(m_ksm           (np)) !1-D array
    allocate(m_exnerLevels   (np)) !1-D array
    allocate(m_pressureLevels(np)) !1-D array

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! allocate fields on pressure level
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    allocate(fields%p  (nx, ny, np))
    allocate(fields%h  (nx, ny, np))
    allocate(fields%pvv(nx, ny, np))
    allocate(fields%cwm(nx, ny, np))
    allocate(fields%t  (nx, ny, np))
    allocate(fields%wvm(nx, ny, np))
    allocate(fields%rwm(nx, ny, np))
    allocate(fields%snm(nx, ny, np))
    allocate(fields%icm(nx, ny, np))
    allocate(fields%gpm(nx, ny, np))

    do k = 1, m_np
       m_pressureLevels(k) = p(k)
       m_exnerLevels(k)    = m_calcExner(m_pressureLevels(k))

       ! Initialize the pressure level for each grid
       fields%p(:, :, k)  = p(k)

    end do

    return

  end subroutine initHybrid2Pressure

  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> set values of interpolated fields.
  !> This method attempts to implement the Fortran subroutine
  !> written by Stan Benjamin and used by FSL.
  !> see http://ruc.noaa.gov/fslparms/hb2ppkg_40.code
  !
  !> @param[in] itype        - interpolation type: 1-LINEAR, 2-HERMITE
  !> @param[in] input_fields - input model fields
  !> @param[inout] interp_fields - interpolated fields
  !> @param[out] iret            - status (0 is successful)
  !
  !------------------------------------------------------------------------------

  subroutine processHybrid2Pressure(itype, input_fields, interp_fields , iret)
    integer, intent(in) :: itype ! interpolation type: 1-LINEAR, 2-HERMITE
    type(model_fields3D_t), intent(in)    :: input_fields
    type(model_fields3D_t), intent(inout) :: interp_fields
    integer, intent(out):: iret
    
    integer :: m

    iret = -1

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! interpolate all 3D fields (except pressure) to pressure level
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    write(*,*) "Hybrid2Pressure: Interpolate ..."
    call runInterpolate(itype, input_fields%h,   interp_fields%h)
    call runInterpolate(itype, input_fields%pvv, interp_fields%pvv)
    call runInterpolate(itype, input_fields%cwm, interp_fields%cwm)
    call runInterpolate(itype, input_fields%t,   interp_fields%t)
    call runInterpolate(itype, input_fields%wvm, interp_fields%wvm)
    call runInterpolate(itype, input_fields%rwm, interp_fields%rwm)
    call runInterpolate(itype, input_fields%snm, interp_fields%snm)
    call runInterpolate(itype, input_fields%icm, interp_fields%icm)
    call runInterpolate(itype, input_fields%gpm, interp_fields%gpm)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! interpolate and extrapolate at surface
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    write(*,*) "               : Interpolate and extrapolate at surface"
    call m_interpolateAtSurface(input_fields%h, input_fields%t, input_fields, interp_fields)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! find closest mandatory levels to each isobaric level, m_ksm
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    write(*,*) "               : find closest mandatory levels to each isobaric level"
    call m_findLevels()

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! extrapolate at top of atmosphere
    ! Extrapolate to upper isobaric levels, if necessary Check from 175 to 100 mb.
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    write(*,*) "               : Extrapolate to upper isobaric levels"
    !Pressure has been initialized in initHybrid2Pressure
    call  m_extrapolateAtTop(input_fields%h, input_fields%t, &
                             input_fields%p, input_fields, interp_fields)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! smooth all the 3D fields
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    write(*,*) "               : Smooth all the 3D fields", LF
    if(SMOOTH%hgt%smooth) &
         call m_smooth(SMOOTH%hgt%passes, SMOOTH%hgt%factor, interp_fields%h)
    if(SMOOTH%pvv%smooth) &
         call m_smooth(SMOOTH%pvv%passes, SMOOTH%pvv%factor, interp_fields%pvv)
    if(SMOOTH%cwm%smooth) &
         call m_smooth(SMOOTH%cwm%passes, SMOOTH%cwm%factor, interp_fields%cwm)
    if(SMOOTH%tmp%smooth) &
         call m_smooth(SMOOTH%tmp%passes, SMOOTH%tmp%factor, interp_fields%t)
    if(SMOOTH%wvm%smooth) &
         call m_smooth(SMOOTH%wvm%passes, SMOOTH%wvm%factor, interp_fields%wvm)
    if(SMOOTH%rwm%smooth) &
         call m_smooth(SMOOTH%rwm%passes, SMOOTH%rwm%factor, interp_fields%rwm)
    if(SMOOTH%rwm%smooth) &
         call m_smooth(SMOOTH%rwm%passes, SMOOTH%rwm%factor, interp_fields%rwm)
    if(SMOOTH%snm%smooth) &
         call m_smooth(SMOOTH%snm%passes, SMOOTH%snm%factor, interp_fields%snm)
    if(SMOOTH%icm%smooth) &
         call m_smooth(SMOOTH%icm%passes, SMOOTH%icm%factor, interp_fields%icm)
    if(SMOOTH%gpm%smooth) &
         call m_smooth(SMOOTH%gpm%passes, SMOOTH%gpm%factor, interp_fields%gpm)

    call  m_cleanup()

    iret = 0

    return
  end subroutine processHybrid2Pressure

  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> release all memories allocated for interpolation
  !
  !------------------------------------------------------------------------------

  subroutine m_cleanup()

    deallocate(m_exnerVals)
    deallocate(m_delta)
    deallocate(m_supportPoints)
    deallocate(m_hold)
 
    deallocate(m_ksm)
    deallocate(m_exnerLevels)
    deallocate(m_pressureLevels)

    return
  end subroutine m_cleanup

  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> dispatch interpoaltion types
  !
  !> @param[in] itype        - interpolation type: 1-LINEAR, 2-HERMITE
  !> @param[in] in_data      - input data
  !> @param[inout] out_data  - interpolated data
  !
  !------------------------------------------------------------------------------

  subroutine runInterpolate(itype, in_data, out_data)
    integer, intent(in) :: itype ! interpolation type: 1-LINEAR, 2-HERMITE
    real, dimension(:, :, :), intent(in) :: in_data
    real, dimension(:, :, :), intent(inout) :: out_data

    if(itype == 1) then
       call m_linearInterpolate(in_data, out_data)
    elseif(itype == 2) then
       write(*, *) "WARNING--runInterpolate(): Hermite interpolation not implemented yet."
       return
       call m_hermiteInterpolate(in_data, out_data)
    else
       write(*, *) "WARNING--runInterpolate(): Unknown interpolation type (LINEAR/HERMITE)."
    end if

    return
  end subroutine runInterpolate


  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> linear interpoaltion
  !
  !> @param[in] in_data      - input data
  !> @param[inout] out_data  - interpolated data
  !
  !------------------------------------------------------------------------------

  subroutine m_linearInterpolate(in_data, out_data)
    real, dimension(:, :, :), intent(in) :: in_data
    real, dimension(:, :, :), intent(inout) :: out_data

    integer :: i, j, k
    integer :: pIdx

    real :: numerator, denominator, xbar

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! calculate derivative
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    do j = 1, m_ny
    do i = 1, m_nx
      do k = 1, m_nz - 1 ! don't go out the top
	numerator = in_data(i, j, k+1) - in_data(i, j, k)
	denominator = m_exnerVals(i, j, k+1) - m_exnerVals(i, j, k)
	m_delta(i, j, k) = 0.0
	if(abs(denominator) > 1.0e-6) then
	  m_delta(i, j, k) = numerator/denominator
        end if
      end do ! do k = 1, m_nz - 1

    ! at the end of the loop, k == m_nz now
    m_delta(i, j, k) = m_delta(i, j, k-1) ! The top two has the same value

    end do ! do i = 1, m_nx
    end do ! do j =1, m_ny

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! interpolate
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    do j = 1, m_ny
    do i = 1, m_nx
      do k = 1, m_nz ! Hybrid-Level
	m_supportPoints(k) = m_exnerVals(i, j, k)
      end do

      do k = 1, m_np ! Pressure-Level
        xbar = m_exnerLevels(k)
	pIdx = m_pFind(xbar, m_supportPoints, m_nz - 1)
	out_data(i, j, k) = in_data(i, j, pIdx) + m_delta(i, j, pIdx) * &
             (xbar - m_supportPoints(pIdx))
      end do ! do k = 1, m_np
    end do ! do i = 1, m_nx
    end do ! do j =1, m_ny

    return
  end subroutine m_linearInterpolate


  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> hermite interpoaltion
  !
  !> @param[in] in_data      - input data
  !> @param[inout] out_data  - interpolated data
  !
  ! Notes: 
  !   Not done yet; do nothing
  !
  !------------------------------------------------------------------------------

  subroutine m_hermiteInterpolate(in_data, out_data)
    real, dimension(:, :, :), intent(in) :: in_data
    real, dimension(:, :, :), intent(inout) :: out_data
  end subroutine m_hermiteInterpolate


  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> calculate exner value
  !
  !> @param[in] val - input value
  !> @return exner value
  !
  !------------------------------------------------------------------------------
  real elemental function m_calcExner(val)
    real, intent(in) :: val

    m_calcExner = m_CPD_P * (val * m_PA_2_BAR)**m_ROVCP_P

    return
  end function m_calcExner


  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> calculate the index value at xbar of the linear interpolation
  !>	on nz intervals with breakpoint sequence supportPoints
  !
  !> @param[in] xbar
  !> @param[in] supportPoints
  !> @param[in] nz
  !> @return index value at xbar 
  !
  !------------------------------------------------------------------------------

  integer function m_pFind(xbar, supportPoints, nz)
    real, intent(in) :: xbar
    real, dimension(:), intent(in) :: supportPoints
    integer, intent(in) :: nz

    integer :: k
 
    if (xbar <= supportPoints(1)) then
      ! Bottom up search
      do k = 1, nz
        if (xbar > supportPoints(k+1)) then
          m_pFind = k
          return
        end if
      end do ! do k = 1, nz

      m_pFind = k ! k == nz+1 now
    else 

      ! Top down search, actually impossible mission
!      do k = nz - 1, 1, -1
!        if (xbar <= supportPoints(k)) then
!          m_pFind = k
!          return
!        end if
!      end do !  do k = nz - 1, 1, -1

      m_pFind = 1
    end if !  if (xbar <= supportPoints(1)) then

    return
  end function m_pFind

  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> interpolate at surface
  !
  !> @param[in] in_height
  !> @param[in] in_temp
  !> @param[in] input_fields
  !> @param[inout] interp_fields
  !
  ! Notes:
  !   The interpolation is considered in 3 layers: A,B,C
  !   A = above IGth hybrid-b level (about 15-25 hPa above ground)
  !   B = between IGth level and model surface
  !   C = below model surface (beneath ground)
  !
  !          ht      temp    RH     u    v    x
  !         ------   ------  ----- ----- ---- -----
  !   A    interp   interp  inter inter inter inter
  !   - - - - - - - - - - - - - - - - - - - - - - - -
  !   B    reduce   reduce  inter inter inter inter
  !   _______________________________________________
  !   / / / / / / / / / surface / / / / / / / / / / /
  !   C    reduce   reduce  =k=1  =k=1  =k=1  =k=1
  !
  !   IG = Params::below_ground_level_index
  !
  ! Here: temperature is treated as RH above
  !
  !------------------------------------------------------------------------------
 
  subroutine m_interpolateAtSurface(in_height,  in_temp, input_fields, interp_fields)
    real, dimension(:,:,:), intent(in) :: in_height, in_temp
    type(model_fields3D_t), intent(in)    :: input_fields
    type(model_fields3D_t), intent(inout) :: interp_fields

    integer :: i, j, k, m
    integer :: groundIdx, lapseIdx

    real :: doubleT, ex1
    real :: exnerAtSurf, pressAtSurf
    real :: tempAtSurf, heightAtSurf
    real :: exnerAtK_1, tempAtK_1, heightAtK_1
    real :: exnerAtLapseIdx, tempAtLapseIdx, heightAtLapseIdx
    real :: lapseRate
    real :: presFac, tempBelow


    groundIdx = SMOOTH%below_ground_level_index
    lapseIdx = SMOOTH%lapse_rate_top_index

    do j = 1, m_ny
    do i = 1, m_nx
       
      exnerAtSurf = m_exnerVals(i, j, groundIdx)
      tempAtSurf = exnerAtSurf * in_temp(i, j, groundIdx) / m_CPD_P
      heightAtSurf = in_height(i, j, groundIdx)
      doubleT = exnerAtSurf / m_CPD_P
      pressAtSurf = doubleT ** 3.5 / m_PA_2_BAR
     
      exnerAtK_1 = m_exnerVals(i, j, 1)
      tempAtK_1 = exnerAtK_1 * in_temp(i, j, 1) / m_CPD_P
      heightAtK_1 = in_height(i, j, 1)
       
      exnerAtLapseIdx = m_exnerVals(i, j, lapseIdx)
      tempAtLapseIdx = exnerAtLapseIdx * in_temp(i, j, lapseIdx) / m_CPD_P
      heightAtLapseIdx = in_height(i, j, lapseIdx)
       
      lapseRate = (tempAtK_1 - tempAtLapseIdx) / (heightAtLapseIdx - heightAtK_1)
      ! choose between dry and moist adiabatic lapse rates
      lapseRate = min(m_GAMD, max(lapseRate, m_GAMMA))
       
      ex1 =  m_RD_P * lapseRate / m_G0_P

      do k = 1, m_np
        doubleT = m_pressureLevels(k) / pressAtSurf ! Dboule precision
        presFac = doubleT ** ex1	 

          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
          ! case A: Nothing will be changed
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
	if ((m_exnerLevels(k) > exnerAtSurf) .and. (m_exnerLevels(k) < exnerAtK_1)) then
	  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
	  ! case B: Only height needs to be adjusted
	  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
	  tempBelow = tempAtSurf * presFac
	  interp_fields%h(i,j,k) = heightAtSurf - (tempBelow - tempAtSurf) / lapseRate
	else if (m_exnerLevels(k) >= exnerAtSurf) then
	  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
	  ! case C: height needs to be adjusted, all other members are adjusted as k=1
	  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
	  tempBelow = tempAtSurf * presFac
	  interp_fields%h(i,j,k) = heightAtSurf - (tempBelow - tempAtSurf) / lapseRate

          ! height has been taken care of
          ! Now the other fields.
          ! all other 3D variables are set as k=1
          interp_fields%pvv(i,j,k) = input_fields%pvv(i, j, 1)
          interp_fields%cwm(i,j,k) = input_fields%cwm(i, j, 1)
          interp_fields%  t(i,j,k) = input_fields%  t(i, j, 1)
          interp_fields%wvm(i,j,k) = input_fields%wvm(i, j, 1)
          interp_fields%rwm(i,j,k) = input_fields%rwm(i, j, 1)
          interp_fields%snm(i,j,k) = input_fields%snm(i, j, 1)
          interp_fields%icm(i,j,k) = input_fields%icm(i, j, 1)
          interp_fields%gpm(i,j,k) = input_fields%gpm(i, j, 1)

        end if ! if ((m_exnerLevels(k) > exnerAtSurf) .and. ....

      end do ! do k = 1, m_np
    end do ! do i = 1, m_nx
    end do ! do j = 1, m_ny

    return
  end subroutine m_interpolateAtSurface

!**********************************************************************
! * Subroutine: m_findLevels()
! *

  subroutine m_findLevels()

    integer :: k, m
    real :: diff1, diff2
    do k = 1, m_np
      m_ksm(k) = m_NZP_P ! m_NZP_P: 8
      do m = 1, m_NZP_P

        if (m_pressureLevels(k) >= m_pmLev(m)) then
          if (m /= 1) then
            diff1 = m_pressureLevels(k) - m_pmLev(m)
            diff2 = m_pmLev(m-1) - m_pressureLevels(k)
	  
            if (diff1 > diff2) then
              m_ksm(k) = m - 1
            else 
              m_ksm(k) = m
            end if !  if (diff1 > diff2)

          else  
            m_ksm(k) = 1
          end if ! if (m /= 1)

        end if !  if (m_pressureLevels(k) >= m_pmLev(m))

      end do !  do m = 1, m_NZP_P   
    end do !  do k = 1, m_np

    return
  end subroutine m_findLevels

  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> extrapolate at top
  !
  !> @param[in] in_height
  !> @param[in] in_temp
  !> @param[in] in_pressure
  !> @param[in] input_fields
  !> @param[inout] interp_fields
  !
  !  Notes:  
  !    Extrapolate to upper isobaric levels, if necessary
  !    Check from 175 to 100 mb.
  !------------------------------------------------------------------------------

  subroutine m_extrapolateAtTop(in_height,  in_temp, in_pressure, input_fields, interp_fields)
    real, dimension(:,:,:), intent(in) :: in_height, in_temp, in_pressure
    type(model_fields3D_t), intent(in)    :: input_fields
    type(model_fields3D_t), intent(inout) :: interp_fields

    integer :: i, j, k, m
    integer :: extIdx

    real :: locTemp

    do k = (m_np - SMOOTH%extrapolate_offset), m_np
    do j = 1, m_ny
    do i = 1, m_nx
      extIdx = m_nz

      locTemp = in_temp(i, j, extIdx) * m_exnerVals(i, j, extIdx) / m_CPD_P

      if (locTemp < 0.0) then

        if (SMOOTH%tmp%extrap_at_top) then
           interp_fields%t(i, j, k) = in_temp(i, j, extIdx)
        end if

        if (SMOOTH%hgt%extrap_at_top) then
           interp_fields%h(i, j, k) = in_height(i, j, extIdx) + &
                m_RD_P * locTemp / m_G0_P * log(in_pressure(i, j, extIdx) / m_pressureLevels(k))
        end if

        ! height and temperature have been taken care of
        ! Now the other fields.
        ! all other 3D variables are set as k=extIdx
        if(SMOOTH%pvv%extrap_at_top) interp_fields%pvv(i,j,k) = input_fields%pvv(i, j, extIdx)
        if(SMOOTH%cwm%extrap_at_top) interp_fields%cwm(i,j,k) = input_fields%cwm(i, j, extIdx)
        if(SMOOTH%wvm%extrap_at_top) interp_fields%wvm(i,j,k) = input_fields%wvm(i, j, extIdx)
        if(SMOOTH%rwm%extrap_at_top) interp_fields%rwm(i,j,k) = input_fields%rwm(i, j, extIdx)
        if(SMOOTH%snm%extrap_at_top) interp_fields%snm(i,j,k) = input_fields%snm(i, j, extIdx)
        if(SMOOTH%icm%extrap_at_top) interp_fields%icm(i,j,k) = input_fields%icm(i, j, extIdx)
        if(SMOOTH%gpm%extrap_at_top) interp_fields%gpm(i,j,k) = input_fields%gpm(i, j, extIdx)

      end if ! if (locTemp < 0.0)

    end do ! do i = 1, m_nx
    end do ! do j = 1, m_ny
    end do ! do k = ...

    return
  end subroutine m_extrapolateAtTop

  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> smooth a meteorological field
  !
  !> @param[in] passes(:)
  !> @param[in] smth_fac
  !> @param[inout] data
  !
  ! Notes:
  !   reference: Shapiro, 1970: "Smoothing, Filtering, and
  !   Boundary Effects", Rev. Geophys. sp. Phys., 359-387.
  !   this filter is of the type 
  !   z(i) = (1-s)z(i) + s(z(i+1)+z(i-1))/2
  !   for a filter which is supposed to damp 2dx waves completely
  !   but leave 4dx and longer with little damping,
  !   it should be run with 2 passes using smth (or s) of 0.5 and -0.5.
  !
  !------------------------------------------------------------------------------
 
  subroutine m_smooth(passes, smth_fac, data)
    implicit none
    integer, intent(in) :: passes(:)
    real,    intent(in) :: smth_fac
    real, intent(inout) :: data(:, :, :)

    real :: c1, c2, c3, c4, c5
    real :: sum1, sum2

    integer :: i, j, k, pcnt, nx, ny

    integer :: numPasses, icnt1, icnt2, icnt3

    nx = m_nx
    ny = m_ny

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! calculate filter coefficients
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    c1 = 0.25 * smth_fac * smth_fac
    c2 = 0.5 * smth_fac * (1.0 - smth_fac)
    c3 = (1.0 - smth_fac) * (1.0 - smth_fac)
    c4 = (1.0 - smth_fac)
    c5 = 0.5 * smth_fac

    do k = 1, m_np
    
      numPasses = passes(m_ksm(k))

      icnt1 = 2
      icnt2 = 1

      do pcnt = 1, numPasses

        do j = 2, ny - 1

          icnt3 = icnt1
          icnt1 = icnt2
          icnt2 = icnt3

          do i = 2, nx - 1
            sum1 = data(i-1, j+1, k) + data(i-1, j-1, k) + &
                   data(i+1, j+1, k) + data(i+1, j-1, k) 	    
            sum2 = data(i, j+1, k) + data(i+1, j, k) +&
                   data(i, j-1, k) + data(i-1, j, k)
            m_hold(i, icnt1) = c1 * sum1 + c2 * sum2 + c3 * data(i, j, k)
          end do !  do i = 2, nx - 1

          if (j /= 2) then
            do i = 2, nx - 1
              data(i, j-1, k) = m_hold(i, icnt2)
            end do ! do i = 2, nx - 1
          end if !  if (j /= 2) 

        end do ! do j = 2, ny - 1

        do i = 2, nx - 1
          data(i, ny, k) = m_hold(i, icnt2)
        end do ! do i = 2, nx - 1

        do i = 2, nx - 1
          data(i, 1, k) = c4 * data(i, 1, k) + &
                          c5 * (data(i-1, 1, k) + data(i+1, 1, k))
          data(i, ny, k) = c4 * data(i, ny, k) + &
                           c5 * (data(i-1, ny, k) + data(i+1, ny, k))
        end do ! do i = 2, nx - 1
       
        do j = 2, ny -1
           data(1, j, k) = c4 * data(1, j, k) + &
                           c5 * (data(1, j+1, k) + data(1, j-1, k))
           data(nx, j, k) = c4 * data(nx, j, k) + &
                            c5 * (data(nx, j+1, k) + data(nx, j-1, k))
        end do !  do j = 2, ny

      end do ! do pcnt = 1, numPasses
    end do !do k = 1, m_np

    return
  end subroutine m_smooth

end module Hybrid2Pressure
