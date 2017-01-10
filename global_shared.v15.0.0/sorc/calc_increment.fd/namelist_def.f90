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

module namelist_def

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use kinds

  !-----------------------------------------------------------------------

  implicit none
  
  !-----------------------------------------------------------------------

  ! Define global variables

  character(len=500)                             :: analysis_filename                       = 'NOT USED'
  character(len=500)                             :: firstguess_filename                     = 'NOT USED'
  character(len=500)                             :: increment_filename                      = 'fv3_increment.nc'
  character(len=500)                             :: datapath                                = './'
  logical                                        :: debug                                   = .false.
  namelist /share/ debug, analysis_filename,         &
       & firstguess_filename, increment_filename, datapath

  !---------------------------------------------------------------------

contains

  !=====================================================================

  ! namelistparams.f90:

  !---------------------------------------------------------------------

  subroutine namelistparams()

    ! Define variables computed within routine

    logical                                                            :: is_it_there
    integer                                                            :: unit_nml

    !===================================================================
    
    ! Define local variables

    unit_nml    = 9
    is_it_there = .false.
    inquire(file='calc-increment.input',exist = is_it_there)

    ! Check local variable and proceed accordingly

    if(is_it_there) then

       ! Define local variables

       open(file   = 'calc-increment.input',                        &
            unit   = unit_nml        ,                                   &
            status = 'old'         ,                                     &
            form   = 'formatted'     ,                                   &
            action = 'read'        ,                                     &
            access = 'sequential'  )
       read(unit_nml,NML = share)
       close(unit_nml)

    else  ! if(is_it_there)

       ! Define local variables

       write(6,500)

    end if ! if(.not. is_it_there)

    !===================================================================

    ! Check local variable and proceed accordingly


    ! Define local variables
    
    write(6,*) '&SHARE'
    write(6,*) 'DEBUG                         = ', debug
    write(6,*) 'ANALYSIS_FILENAME             = ',                    &
         & trim(adjustl(analysis_filename))
    write(6,*) 'FIRSTGUESS_FILENAME           = ',                    &
         & trim(adjustl(firstguess_filename))
    write(6,*) 'INCREMENT_FILENAME           = ',                    &
         & trim(adjustl(increment_filename))
    write(6,*) 'DATAPATH                      = ',                    &
         & trim(adjustl(datapath))
    write(6,*) '/'

    !===================================================================

    ! Define format statements

500 format('NAMELISTPARAMS: calc-increment.input not found in the', &
         & ' current working directory. ABORTING!!!!')

    !===================================================================

  end subroutine namelistparams

  !=====================================================================
  
end module namelist_def
