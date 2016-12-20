!------------------------------------------------------------------------------
!
! MODULE: Model (for GFS/NAM/RAP model)
!
! DESCRIPTION:
!> Read in model data, convert to pressure levels for RAP and derive fields
!> Write icing products
!
! REVISION HISTORY:
! September 2010, modified November 2011
! March 2014 - modified
!
!------------------------------------------------------------------------------

module Model
  use Kinds
  use Config

  use Grib1
  use Grib2
  use Hybrid2Pressure  ! For RAP
  use Derived_Fields

  IMPLICIT NONE

  private
  public runModel, writeIcing

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! Parameters
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  integer, parameter :: INTERPOLATION_TYPE = 1 ! RAP only

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! private members (configration related)
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  character(len=3)  :: CFG_CMODEl ! RAP/NAM/GFS
  integer :: CFG_GRIB  ! 1/2, input model file in grib 1 or 2
  integer :: CFG_NUM_HYBRID 	!= 50     ! RAP only
  integer :: CFG_NUM_PRESSURE	!= 37
  real    :: CFG_MAX_PRESSURE	!= 1000.0 ! in hPa
  real    :: CFG_PRESSURE_STEP	!= -25.0  ! in hPa

contains

  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Read in all model fields (dispatching to grib 1 or 2),
  !> convert to pressure levels for RAP and derive fields
  !
  !> @param[in]  filename  - input model file
  !> @param[in]  cfg       - configurations
  !> @param[out] kpds      - model PDS	! for Grib1
  !> @param[out] kgds      - model GDS	! for Grib1, Grib2
  !> @param[out] gfld      - model gfld	! for Grib2
  !> @param[out] modelData - type of model_inputs_t, including derived fields
  !> @param[out] iret      - status (0 is successful)
  !
  !------------------------------------------------------------------------------

  subroutine runModel(filename, cfg, kpds, kgds, gfld, modelData, iret)
    IMPLICIT NONE
    character(len=*),  intent(in):: filename
    type(config_t), intent(in)   :: cfg
    integer, intent(out)         :: kpds(:) ! for Grib1
    integer, intent(out)         :: kgds(:) ! for Grib1, Grib2
    type(gribfield), intent(out) :: gfld    ! for Grib2
    type(model_inputs_t), intent(out) :: modelData
    integer, intent(out) :: iret

    integer :: nx, ny, nz
    integer :: np, k
    real, allocatable :: pressures(:)
    integer, allocatable :: pds7(:) ! vertical information

    type(model_fields3D_t) :: fields
    ! For RAP 3D fields, they will go through Hybrid2Pressure,
    ! So temporary variable is needed.
    type(model_fields3D_t) :: fields_H ! for RAP

    character(len=*), parameter :: myself = "Model::runModel() "

    ! configuration fields
    CFG_CMODEL = cfg% model% name
    CFG_GRIB = cfg% model% grib
    CFG_NUM_HYBRID = cfg% levels% num_hybrid
    CFG_NUM_PRESSURE = cfg% levels% num_pressure
    CFG_MAX_PRESSURE = cfg% levels% max_pressure
    CFG_PRESSURE_STEP = cfg% levels% pressure_step

    iret = -1
    np = CFG_NUM_PRESSURE

    select case(CFG_CMODEL)
    case('RAP')

       nz = CFG_NUM_HYBRID
       allocate(pds7(nz))
       do k = 1, nz
          pds7(k) = k
       end do

       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! read in RAP data. 3D data in fields3D_H, 2D data in modelData
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

       if(CFG_GRIB == 1) then
          call readModelGB1(filename,CFG_CMODEL,pds7,fields_H,modelData,kpds,kgds,iret)
       else if(CFG_GRIB == 2) then
          call readModelGB2(filename,CFG_CMODEL,pds7,fields_H,modelData,gfld,kgds,iret)
       end if

       nx = kgds(2)
       ny = kgds(3)

       if(iret /= 0) then
          write(*,*) myself, "unable to read in ", CFG_cmodel, " grib file"
          return
       end if

       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! hybrid level => pressure level
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       allocate(pressures(np))
       do k = 1, np
          pressures(k) = CFG_MAX_PRESSURE + (k-1) * CFG_PRESSURE_STEP
          pressures(k) = pressures(k) * 100  ! from millibar to pascal
       end do

       ! Pressure on Hybrid/Sigma level is needed for initialization.
       ! When initialization, pressure of pressure-level is set and
       ! memories of fields on pressure-level are allocated.
       call initHybrid2Pressure(nx, ny, nz, np, pressures, fields_H%p, fields)

       ! All fields need interpolation except for pressure.
       call processHybrid2Pressure(INTERPOLATION_TYPE, fields_H, fields, iret)
       if(iret /= 0) then
          write(*,*) myself, "unable to process Hybrid2Pressure"
          return
       end if

       deallocate(pressures)
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! release the memory of 3D fields on Hybrid levels
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       call m_doneModel(fields_H)

       ! change value of nz after Hybrid2Pressure
       nz = CFG_NUM_PRESSURE

    case('GFS', 'NAM')

       nz =  CFG_NUM_PRESSURE
       allocate(pds7(nz)) ! in hPa
       do k = 1, nz
          pds7(k) = int(CFG_MAX_PRESSURE) + (k-1) * int(CFG_PRESSURE_STEP)
       end do

       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! read in GFS/NAM data. 3D data in fields3D, 2D data in modelData.
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       if(CFG_GRIB == 1) then
          call readModelGB1(filename,CFG_CMODEL,pds7,fields,modelData,kpds,kgds,iret)
       else if(CFG_GRIB == 2) then
          call readModelGB2(filename,CFG_CMODEL,pds7,fields,modelData,gfld,kgds,iret)
       end if

       nx = kgds(2)
       ny = kgds(3)

       if(iret /= 0) then
          write(*,*) myself, "unable to read in ", CFG_CMODEL, " grib file"
          return
       end if

    end select

    deallocate(pds7)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! calculate derived fields for all models
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    call calculateDerived_Fields(nx, ny, nz, CFG_CMODEL, fields, modelData, iret)
    if(iret /= 0) then
       write(*,*) myself, "unable to derive fields"
       return
    end if
       
   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
   ! release the memory of native 3D model fields
   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
   call m_doneModel(fields)

   return
  end subroutine runModel

  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> write out icing products interface, dispatch to output in grib 1 or grib 2
  !
  !> @param[in]  filename  - output file name
  !> @param[in]  grib      - 1/2, write out icing products in grib 1 or 2
  !> @param[in]  icingData - type of icing_output_t, including all icing products
  !> @param[out] iret      - status (0 is successful)
  !> @param[in]  kpds      - PDS	! for Grib 1
  !> @param[in]  kgds      - GDS	! for Grib 1
  !> @param[in]  gfld      - gribfield	! for Grib 2
  !
  !------------------------------------------------------------------------------

  subroutine writeIcing(filename, icingData, iret, kpds, kgds, gfld )
    IMPLICIT NONE
    character(*), intent(in) :: filename
    type(icing_output_t), intent(in) :: icingData
    integer,      intent(out) :: iret
    integer, intent(in), optional :: kpds(:), kgds(:)	! for Grib 1
    type(gribfield), intent(in), optional :: gfld	! for Grib 2

    if(present(kpds) .and. present(kgds)) then
       call writeIcingGB1(filename, kpds, kgds, icingData, iret)
    else if(present(gfld)) then
       call writeIcingGB2(filename, gfld, icingData, iret)
    end if

    return
  end subroutine writeIcing


  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> release the memory allocated for model native 3D input fields
  !
  !> @param[in]  fields - type of model_fields3D_t
  !
  !------------------------------------------------------------------------------

  subroutine m_doneModel(fields)
    type(model_fields3D_t), intent(inout) :: fields

    if(allocated(fields%p))   deallocate(fields%p)
    if(allocated(fields%h))   deallocate(fields%h)
    if(allocated(fields%pvv)) deallocate(fields%pvv)
    if(allocated(fields%cwm)) deallocate(fields%cwm)
    if(allocated(fields%t))   deallocate(fields%t)
    if(allocated(fields%wvm)) deallocate(fields%wvm)
    if(allocated(fields%rwm)) deallocate(fields%rwm)
    if(allocated(fields%snm)) deallocate(fields%snm)
    if(allocated(fields%icm)) deallocate(fields%icm)
    if(allocated(fields%gpm)) deallocate(fields%gpm)
    if(allocated(fields%rh))  deallocate(fields%rh)

    return
  end subroutine m_doneModel

end module Model
