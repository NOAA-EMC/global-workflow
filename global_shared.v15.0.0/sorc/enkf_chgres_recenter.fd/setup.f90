 module setup

 use nemsio_module

 implicit none

 private

 character(len=300), public       :: input_file
 character(len=300), public       :: output_file
 character(len=300), public       :: terrain_file
 character(len=300), public       :: vcoord_file

 integer(nemsio_intkind), public  :: i_output
 integer(nemsio_intkind), public  :: j_output
 integer                , public  :: ij_output

 public                           :: program_setup

 contains

 subroutine program_setup

 implicit none

 integer                           :: istat

 namelist /nam_setup/ i_output, j_output, input_file, output_file, &
                      terrain_file, vcoord_file

 print*
 print*,"OPEN SETUP NAMELIST."
 open(43, file="./fort.43", iostat=istat)
 if (istat /= 0) then
   print*,"FATAL ERROR OPENING NAMELIST FILE. ISTAT IS: ",istat
   call errexit(30)
 endif

 print*,"READ SETUP NAMELIST."
 read(43, nml=nam_setup, iostat=istat)
 if (istat /= 0) then
   print*,"FATAL ERROR READING NAMELIST FILE. ISTAT IS: ",istat
   call errexit(31)
 endif

 ij_output = i_output * j_output

 close(43)

 end subroutine program_setup

 end module setup
