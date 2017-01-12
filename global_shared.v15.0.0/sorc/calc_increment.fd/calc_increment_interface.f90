!    Copyright (C) 2015 Henry R. Winterbottom

!    Email: Henry.Winterbottom@noaa.gov

!    Snail-mail:

!    Henry R. Winterbottom
!    NOAA/OAR/PSD R/PSD1
!    325 Broadway
!    Boulder, CO 80303-3328

!    This file is part of global-model-py.

!    global-model-py is free software: you can redistribute it and/or
!    modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of
!    the License, or (at your option) any later version.

!    global-model-py is distributed in the hope that it will be
!    useful, but WITHOUT ANY WARRANTY; without even the implied
!    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!    See the GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with global-model-py.  If not, see
!    <http://www.gnu.org/licenses/>.

module calc_increment_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use fv3_interface
  use namelist_def

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: calc_increment

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! calc_increment.f90:

  !-----------------------------------------------------------------------

  subroutine calc_increment()

    !=====================================================================

    ! Define local variables

    call namelistparams()

    ! Check local variable and proceed accordingly

    call fv3_calc_increment()

    !=====================================================================

  end subroutine calc_increment

  !=======================================================================

end module calc_increment_interface
