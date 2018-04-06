 module native_endianness
!$$$ module documentation block
!           .      .    .                                       .
! module:   native_endianness
!   prgmmr: parrish          org: wx22                date: 2012-10-11
!
! abstract:  This module was written by Dusan Jovic and has been adapted to GSI for internal translation
!             of WRF ARW and NMM binary restart files as required to match the machine native 
!             endian storage format.  The original code only converted from big-endian to little-endian.
!             There are no restrictions in this version.
!             This is required for these two types of files, because they are read/written to using mpi-io,
!             which has no compiler option for automatic switching to machine native endian format
!             for fortran unformatted read/write.
!
! program history log:
!   2012-10-11  parrish - copy/modify original module native_endianness provided by Dusan Jovic, NCEP/EMC 2012
!   2012-10-19  parrish - additional modifications to improve efficiency.  Remove interface and make
!                          to_native_endianness to work only with integer(4) arguments.
!                          Put to_native_endianness_i4 outside module.
!
! subroutines included:
!
! functions included:
!   is_little_endian - no argument--returns true for little-endian machine, false for big-endian machine
!
! variables included:
!   byte_swap            - false if machine and wrf binary file are same endian, true if different
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

 use kinds, only: i_byte,i_long
 implicit none

 private

 public byte_swap
 public is_little_endian

 logical byte_swap

 contains

 logical function is_little_endian()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    is_little_endian
!   prgmmr: parrish          org: wx22                date: 2012-10-11
!
! abstract: test to see if machine is little-endian.  Returns true for little-endian, false for big-endian.
!
! program history log:
!   2012-10-11  parrish - add doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   implicit none

   integer(i_byte) :: i1
   integer(i_long) :: i2

   i1 = 1
   i2 = 0
   i2 = transfer(i1,i2)

   is_little_endian = (i1 == i2)

 end function is_little_endian

 end module native_endianness

!----------------------------------------------------------------------
! convert 4-byte integer scalar from big-endian to native-endian
!----------------------------------------------------------------------

 subroutine to_native_endianness_i4(i4,num)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    to_native_endianness_i4
!   prgmmr: parrish          org: wx22                date: 2012-10-11
!
! abstract: swap bytes of argument.
!
! program history log:
!   2012-10-11  parrish - add doc block
!   2012-10-19  parrish - additional modifications to improve efficiency.  Remove interface and make
!                          to_native_endianness to work only with integer(4) arguments.
!                          Put to_native_endianness_i4 outside module.
!
!   input argument list:
!    i4 - input 4 byte integer array
!    num - length of array i4  (NOTE:  type of num must be i_llong (8 byte integer) )
!
!   output argument list:
!    i4 - output 4 byte integer array with bytes in reverse order
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

 use kinds, only: i_byte,i_long,i_llong
 implicit none

 integer(i_llong), intent(in) :: num
 integer(i_long), intent(inout) :: i4(num)

 integer(i_byte), dimension(4) :: byte_arr, byte_arr_tmp
 integer(i_long) :: i,n

 do n=1,num
    byte_arr_tmp = transfer (i4(n), byte_arr)
    byte_arr(1)=byte_arr_tmp(4)
    byte_arr(2)=byte_arr_tmp(3)
    byte_arr(3)=byte_arr_tmp(2)
    byte_arr(4)=byte_arr_tmp(1)
    i4(n) = transfer (byte_arr, i4(n))
 end do

 return

 end subroutine to_native_endianness_i4
