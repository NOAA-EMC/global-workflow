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

program calc_increment_main

  !=====================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  !
  ! ABSTRACT:
  !
  !  This routine provides an interface to the National Oceanic and
  !  Atmospheric Administration (NOAA) National Centers for
  !  Environmental Prediction (NCEP) implemented NOAA Environmental
  !  Modeling System (NEMS) input/output file format. The routine
  !  prepares an appropriate Network Common Data Format (netcdf) file
  !  containing the analysis increments (analysis - background) for
  !  the respective unstructured global forecast model such that the
  !  increments for the respective forecast model can be interpolated
  !  back to the native forecast model simulation grid.
  !
  ! REFERENCE(S):
  !
  ! NOTES:
  !
  ! PRGMMR: Winterbottom        
  ! ORG:    ESRL/PSD1       
  ! DATE:   2016-03-26
  !
  ! PROGRAM HISTORY LOG:
  !
  !  2016-03-26 Initial version. Henry R. Winterbottom
  !
  ! LANGUAGE ATTRIBUTES: FORTRAN 90
  ! COMPILER ATTRIBUTES: INTEL
  !
  ! EXTERNAL I/O ROUTINES:
  !
  !  > namelist.f90
  !
  ! EXTERNAL COMPUTATIONAL MODULES:
  !
  !  > constants.f90
  !  > gfs_nems_interface.f90
  !  > calc_increment_interface.f90
  !  > kinds.f90
  !
  ! EXTERNAL FORTRAN SUBROUTINES:
  !
  !  > ncep_routines.f
  !
  !$$$

  !=====================================================================

  ! Define associated modules and subroutines

  !---------------------------------------------------------------------

  use kinds

  !---------------------------------------------------------------------

  use calc_increment_interface

  implicit none

  call calc_increment()

end program calc_increment_main
