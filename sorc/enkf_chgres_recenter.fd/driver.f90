 program recenter

 use setup, only       : program_setup
 use grid2grid, only   : hinterp_grid2grid, adjust_for_terrain, &
                         read_vcoord_info, set_output_grid
 use output_data, only : write_output_data

 implicit none

 call w3tagb('CHGRES_RECENTER',2018,0179,0055,'NP20')

 print*,"STARTING PROGRAM"

!--------------------------------------------------------
! Read configuration namelist.
!--------------------------------------------------------

 call program_setup

!--------------------------------------------------------
! Get output grid specs
!--------------------------------------------------------

 call set_output_grid

!--------------------------------------------------------
! Read input and horizontally interpolate to output grid
!--------------------------------------------------------

 call hinterp_grid2grid

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
