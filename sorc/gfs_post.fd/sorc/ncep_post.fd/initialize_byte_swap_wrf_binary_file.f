subroutine initialize_byte_swap_wrf_binary_file(in_unit,wrfges)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    initialize_byte_swap_wrf_binary_file  set byte_swap
!   prgmmr: parrish          org: np22                date: 2012-10-11
!
! abstract:  compare endian format of binary file wrfges and set variable byte_swap (a public variable in
!              module native_endianness) true if file endian format is different from machine endian format,
!              otherwise set byte_swap=false.
!
! program history log:
!   2012-10-11  parrish
!
!   input argument list:
!     in_unit          - fortran unit number where input file is opened through.
!     wrfges           - binary input file name.
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: i_byte,i_long,i_llong,i_kind
  use native_endianness, only: byte_swap
  implicit none

  integer(i_kind) ,intent(in   ) :: in_unit
  character(9)    ,intent(in   ) :: wrfges

  character(10) cwrfges
  integer(i_llong) nextbyte,locbyte,thisblock
  integer(i_byte) lenrec4(4)
  integer(i_byte) lenrec4_swap(4)
  integer(i_long) lenrec,lensave
  integer(i_long) lenrec_swap
  equivalence (lenrec4(1),lenrec)
  equivalence (lenrec4_swap(1),lenrec_swap)
  integer(i_llong),parameter:: lrecl=2**20_i_llong
  integer(i_llong),parameter:: lword=2**18_i_llong
  integer(i_long) buf4(lword)
  integer(i_byte) buf(lrecl)
  equivalence(buf4(1),buf(1))
  integer(i_kind) i,nreads
  logical lastbuf
  integer(i_kind) ierr


  cwrfges = wrfges
  cwrfges(10:10) = char(0)
  call openfileread (in_unit, ierr, cwrfges)
! open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)
  nextbyte=0_i_llong
  locbyte=lrecl
  nreads=0
  lastbuf=.false.

! get length of 1st record, then use to set byte_swap.

  do i=1,4
     nextbyte=nextbyte+1_i_llong
     locbyte=locbyte+1_i_llong
     if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
     lenrec4(i)=buf(locbyte)
     lenrec4_swap(5-i)=buf(locbyte)
  end do
  byte_swap = lenrec <= 0 .or. lenrec > 4096
     
  write(6,*)' byte_swap,lenrec4,lenrec4_swap=',byte_swap,lenrec4,lenrec4_swap
  write(6,*)' byte_swap,lenrec,lenrec_swap=',byte_swap,lenrec,lenrec_swap

  call closefile(in_unit,ierr)

end subroutine initialize_byte_swap_wrf_binary_file

