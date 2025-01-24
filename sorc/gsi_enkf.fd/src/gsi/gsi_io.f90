module gsi_io
!$$$ module documentation block
!           .      .    .                                       .
! module:   gsi_io
!   prgmmr: treadon     org: np23                date: 2006-04-15
!
! abstract: This module contains routines which handle input/output
!           operations for GSI atmospheric and surface files.
!
! program history log:
!   2006-04-15 treadon
!   2007-05-25 todling - make reorder public; add interface to reorder
!
! Subroutines Included:
!   sub init_io           - initial i/o parameters
!
! Variable Definitions:
!   def lendian_in        - unit number reserved for little endian input
!   def lendian_out       - unit number reserved for little endian output
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind
  implicit none

  integer(i_kind):: lendian_in,lendian_out
  integer(i_kind):: mype_io
  logical verbose
  logical print_obs_para

  private
  public lendian_in, lendian_out
  public mype_io
  public init_io
  public verbose
  public print_obs_para

  character(len=*), parameter :: myname='gsi_io'

contains

  subroutine init_io(mype,iope)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_io                initialize quanities related 
!                                       to gsi i/o
!   prgmmr: treadon          org: np23                date: 2006-05-25
!
! abstract: initialize quantities related to gsi i/o
!
! program history log:
!   2006-05-25  treadon
!
!   input argument list:
!     mype     - mpi task id
!     iope     - io server mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    implicit none

!   Declare passed variables
    integer(i_kind),intent(in   ) :: mype
    integer(i_kind),intent(in   ) :: iope


!   Set unit numbers reserved for little endian input and output
    lendian_in  = 15
    lendian_out = 66
    verbose = .false.
    print_obs_para = .false.

    if (mype==0) write(6,*)'INIT_IO:  reserve units lendian_in=',lendian_in,&
       ' and lendian_out=',lendian_out,' for little endian i/o'

!   Set mpi io task
    mype_io=iope
    if (mype==mype_io) write(6,*)'INIT_IO:  set IO server task to mype_io=',mype_io

  end subroutine init_io

end module gsi_io
