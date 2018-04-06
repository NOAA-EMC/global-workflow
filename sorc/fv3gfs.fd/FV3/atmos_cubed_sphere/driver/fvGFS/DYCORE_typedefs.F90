!***********************************************************************
!*                   GNU Lesser General Public License
!*
!* This file is part of the FV3 dynamical core.
!*
!* The FV3 dynamical core is free software: you can redistribute it
!* and/or modify it under the terms of the
!* GNU Lesser General Public License as published by the
!* Free Software Foundation, either version 3 of the License, or
!* (at your option) any later version.
!*
!* The FV3 dynamical core is distributed in the hope that it will be
!* useful, but WITHOUT ANYWARRANTY; without even the implied warranty
!* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!* See the GNU General Public License for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with the FV3 dynamical core.
!* If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************
module DYCORE_typedefs

!>@brief Coupling interface layer for FV3 dynamical core.

  use platform_mod, only: r8_kind

  !--- parameter constants used for default initializations
  real(kind=r8_kind), parameter :: zero      = 0.0_r8_kind
  real(kind=r8_kind), parameter :: huge      = 9.9999D15
  real(kind=r8_kind), parameter :: clear_val = zero

  type var_subtype
    real(kind=r8_kind), pointer :: var2p(:)   => null()  !< 2D data saved in packed format [dim(ix)]
    real(kind=r8_kind), pointer :: var3p(:,:) => null()  !< 3D data saved in packed format [dim(ix,levs)]
  end type var_subtype


!---------------------------------------------------------------------
! DYCORE_coupling_type
!   fields to/from other coupled components (e.g. land/ice/ocean/etc.)
!---------------------------------------------------------------------
  type DYCORE_coupling_type

    !--- surface pressure
    real (kind=r8_kind), pointer :: p_srf(:) => null()

    !--- bottom layer temperature, pressure, winds, and height
    real (kind=r8_kind), pointer :: t_bot(:) => null()
    real (kind=r8_kind), pointer :: p_bot(:) => null()
    real (kind=r8_kind), pointer :: u_bot(:) => null()
    real (kind=r8_kind), pointer :: v_bot(:) => null()
    real (kind=r8_kind), pointer :: z_bot(:) => null()

    !--- sea-level pressure
    real (kind=r8_kind), pointer :: slp(:) => null()

    !--- bottom layer tracers
    real (kind=r8_kind), pointer :: tr_bot(:,:) => null()

    !--- outgoing accumulated quantities
    contains
      procedure :: create  => coupling_create  !<   allocate array data
  end type DYCORE_coupling_type

!-------------------------------------------------------------
! DYCORE_diag_type
!  diagnostic type to be used with an external write component
!-------------------------------------------------------------
  type DYCORE_diag_type
    character(len=32)   :: name           !< variable name in source
    character(len=32)   :: output_name    !< output name for variable
    character(len=32)   :: mod_name       !< module name (e.g. physics, radiation, etc)
    character(len=32)   :: file_name      !< output file name for variable
    character(len=128)  :: desc           !< long description of field
    character(len=32)   :: unit           !< units associated with fields
    character(len=32)   :: type_stat_proc !< type of statistic processing:
                                          !< average, accumulation, maximal,
                                          !minimal, etc.
    character(len=32)   :: level_type     !< vertical level of the field
    integer             :: level          !< vertical level(s)
    real(kind=r8_kind)  :: cnvfac         !< conversion factors to output in specified units
    real(kind=r8_kind)  :: zhour          !< forecast hour when bucket was last emptied for statistical processing
    real(kind=r8_kind)  :: fcst_hour      !< current forecast hour (same as fhour)
    type(var_subtype), allocatable :: data(:) !< holds pointers to data in packed format (allocated to nblks)
    contains
      procedure create => diag_create
      procedure zero   => diag_zero
  end type DYCORE_diag_type


!----------------------
! DYCORE_Data container
!----------------------
  type DYCORE_data_type
    type(DYCORE_coupling_type) :: Coupling
!   type(xxxxxxxx_type) :: Xxxxxxxx
  end type DYCORE_data_type

!----------------
! PUBLIC ENTITIES
!----------------
  public DYCORE_data_type, DYCORE_diag_type

!*******************************************************************************************
  CONTAINS


!------------------------
! DYCORE_coupling_type%create
!------------------------
  subroutine coupling_create (Coupling, IM, Ntracers)
    implicit none

    class(DYCORE_coupling_type)            :: Coupling
    integer,                    intent(in) :: IM
    integer,                    intent(in) :: Ntracers

    !--- surface pressure
    allocate (Coupling%p_srf (IM))
    Coupling%p_srf = clear_val

    !--- bottom layer temperature, pressure, winds, and height
    allocate (Coupling%t_bot (IM))
    allocate (Coupling%p_bot (IM))
    allocate (Coupling%u_bot (IM))
    allocate (Coupling%v_bot (IM))
    allocate (Coupling%z_bot (IM))
    Coupling%t_bot = clear_val
    Coupling%p_bot = clear_val
    Coupling%u_bot = clear_val
    Coupling%v_bot = clear_val
    Coupling%z_bot = clear_val

    !--- sea-level pressure
    allocate (Coupling%slp (IM))
    Coupling%slp = clear_val

    !--- bottom layer tracers
    allocate (Coupling%tr_bot (IM,Ntracers))
    Coupling%tr_bot = clear_val

  end subroutine coupling_create


!------------------------
! DYCORE_diag_type%create
!------------------------
  subroutine diag_create (Diag, IM, Ntracers)
    implicit none

    class(DYCORE_diag_type)            :: Diag
    integer,                intent(in) :: IM
    integer,                intent(in) :: Ntracers

  end subroutine diag_create


!------------------------
! DYCORE_diag_type%zero
!------------------------
  subroutine diag_zero (Diag)
    implicit none

    class(DYCORE_diag_type)            :: Diag

  end subroutine diag_zero 

end module DYCORE_typedefs
