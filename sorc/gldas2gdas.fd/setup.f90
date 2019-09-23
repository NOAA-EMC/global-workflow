 module setup

 character(len=200) :: orog_dir_gdas_grid, orog_files_gdas_grid(6)

 contains


 subroutine namelist_read

 implicit none

 integer :: error 

 namelist /config/ orog_dir_gdas_grid, orog_files_gdas_grid

 open(41, file="./fort.41", iostat=error)
 if (error /= 0) call error_handler("OPENING SETUP NAMELIST.", error)
 read(41, nml=config, iostat=error)
 if (error /= 0) call error_handler("READING SETUP NAMELIST.", error)
 close (41)



 end subroutine namelist_read

 end module setup
