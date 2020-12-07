 program recenter

 use setup, only       : program_setup
 use interp, only      : gaus_to_gaus, adjust_for_terrain
 use input_data, only  : read_input_data,  &
                         read_vcoord_info
 use output_data, only : set_output_grid, write_output_data

 implicit none

 call w3tagb('CHGRES_RECENTER',2018,0179,0055,'NP20')

 print*,"STARTING PROGRAM"

!--------------------------------------------------------
! Read configuration namelist.
!--------------------------------------------------------

 call program_setup

!--------------------------------------------------------
! Read input grid data
!--------------------------------------------------------

 call read_input_data

!--------------------------------------------------------
! Read vertical coordinate info
!--------------------------------------------------------

 call read_vcoord_info

!--------------------------------------------------------
! Get output grid specs
!--------------------------------------------------------

 call set_output_grid

!--------------------------------------------------------
! Interpolate data to output grid
!--------------------------------------------------------

 call gaus_to_gaus

!--------------------------------------------------------
! Adjust output fields for differences between
! interpolated and external terrain.
!--------------------------------------------------------

 call adjust_for_terrain

!--------------------------------------------------------
! Write output data to file.
!--------------------------------------------------------

 call write_output_data

 print*
 print*,"PROGRAM FINISHED NORMALLY!"

 call w3tage('CHGRES_RECENTER')

 stop
 
 end program recenter
