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

module variable_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use constants
  use kinds

  !-----------------------------------------------------------------------

  use namelist_def

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define all data and structure types for routine; these variables
  ! are variables required by the subroutines within this module

  type varinfo
     character(len=20)                                                 :: var_name
     character(len=20)                                                 :: nems_name
     character(len=20)                                                 :: nems_levtyp
     integer                                                           :: ndims
  end type varinfo

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: varinfo
  public :: variable_lookup
  public :: variable_clip

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! variable_clip.f90:

  !-----------------------------------------------------------------------

  subroutine variable_clip(grid)

   ! Define variables passed to routine

    real(r_double)                                                       :: grid(:)

    ! Define variables computed within routine

    real(r_double)                                                       :: clip

    !=====================================================================

    ! Define local variables

    clip = tiny(grid(1))
    where(grid .le. dble(0.0)) grid = clip

    !=====================================================================

  end subroutine variable_clip

  !=======================================================================

  ! variable_lookup.f90:

  !-----------------------------------------------------------------------

  subroutine variable_lookup(grid)

    ! Define variables passed to routine

    type(varinfo)                                                        :: grid

    !=====================================================================

    ! Check local variable and proceed accordingly
    
    if(trim(adjustl(grid%var_name)) .eq. 'psfc') then
       
       ! Define local variables
       
       grid%nems_name   = 'pres'
       grid%nems_levtyp = 'sfc'
       
    end if ! if(trim(adjustl(grid%var_name)) .eq. 'psfc')

    ! Check local variable and proceed accordingly
    
    if(trim(adjustl(grid%var_name)) .eq. 'dpres') then
       
       ! Define local variables
       
       grid%nems_name   = 'dpres'
       grid%nems_levtyp = 'mid layer'
       
    end if ! if(trim(adjustl(grid%var_name)) .eq. 'dpres')

    ! Check local variable and proceed accordingly
    
    if(trim(adjustl(grid%var_name)) .eq. 'ugrd') then
       
       ! Define local variables
       
       grid%nems_name   = 'ugrd'
       grid%nems_levtyp = 'mid layer'
       
    end if ! if(trim(adjustl(grid%var_name)) .eq. 'ugrd')

    ! Check local variable and proceed accordingly
    
    if(trim(adjustl(grid%var_name)) .eq. 'vgrd') then
       
       ! Define local variables
       
       grid%nems_name   = 'vgrd'
       grid%nems_levtyp = 'mid layer'
       
    end if ! if(trim(adjustl(grid%var_name)) .eq. 'vgrd')

    ! Check local variable and proceed accordingly
    
    if(trim(adjustl(grid%var_name)) .eq. 'spfh') then
       
       ! Define local variables
       
       grid%nems_name   = 'spfh'
       grid%nems_levtyp = 'mid layer'
       
    end if ! if(trim(adjustl(grid%var_name)) .eq. 'spfh')

    ! Check local variable and proceed accordingly
    
    if(trim(adjustl(grid%var_name)) .eq. 'tmp') then
       
       ! Define local variables
       
       grid%nems_name   = 'tmp'
       grid%nems_levtyp = 'mid layer'
       
    end if ! if(trim(adjustl(grid%var_name)) .eq. 'tmp')

    ! Check local variable and proceed accordingly
    
    if(trim(adjustl(grid%var_name)) .eq. 'clwmr') then
       
       ! Define local variables
       
       grid%nems_name   = 'clwmr'
       grid%nems_levtyp = 'mid layer'
       
    end if ! if(trim(adjustl(grid%var_name)) .eq. 'clwmr')
    
    ! Check local variable and proceed accordingly
    
    if(trim(adjustl(grid%var_name)) .eq. 'o3mr') then
       
       ! Define local variables
       
       grid%nems_name   = 'o3mr'
       grid%nems_levtyp = 'mid layer'
       
    end if ! if(trim(adjustl(grid%var_name)) .eq. 'o3mr')

    !=====================================================================

  end subroutine variable_lookup

  !=======================================================================

end module variable_interface
