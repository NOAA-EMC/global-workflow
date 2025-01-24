module m_stats
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_stats
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-24
!
! abstract: summarize contents of input vectors
!
! program history log:
!   2008-04.28  guo - initial code
!   2008-12-09  todling - add comments/prologue
!   2008-12-12  todling - remove dependence on mpeu
!   2008-04-02  todling - mpi_allreduce cannot alias buffer
!   2010-03-24  j guo   - updated this document block
!   2011-08-01  lueken  - replaced F90 with f90 (no machine logic)
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

use kinds,only : r_kind
use kinds,only : i_kind
use constants, only: zero
use mpimod, only: ierror,mpi_rtype,mpi_sum,mpi_max

implicit none
private

public :: stats_sum
public :: stats_allreduce

interface stats_sum
   module procedure sum_
end interface

interface stats_allreduce
   module procedure allreduce_
end interface

character(len=*),parameter :: myname="m_stats"
contains

! Usecase 1 example:
!
!    use m_stats,only : stats_sum
!    use m_stats,only : stats_allreduce
!
!    type vectors
!      real(r_kind),dimension(:),pointer :: v
!    endtype vectors
!    type(vectors),dimension(:) :: a
!
!    real(r_kind) :: vdot,vsum,vmin,vmax
!    integer(i_kind) :: vnum
!
!    do i=1,size(a)
!      call stats_sum(a(i)%v,vdot,vsum,vmin,vmax,vnum,add=i>1)
!    enddo
!    call stats_allreduce(vdot,vsum,vmin,vmax,vnum,comm)
!

subroutine sum_(v,vdot,vsum,vmin,vmax,vdim,add)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sum_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    v
!    vdot,vsum
!    vmin,vmax
!    vdim
!    add
!
!   output argument list:
!    vdot,vsum
!    vmin,vmax
!    vdim
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  real(r_kind),dimension(:),intent(in   ) :: v
  real(r_kind)             ,intent(inout) :: vdot,vsum
  real(r_kind)             ,intent(inout) :: vmin,vmax
  integer(i_kind)          ,intent(inout) :: vdim
  logical,optional         ,intent(in   ) :: add

  logical :: add_
  add_=.false.
  if(present(add)) add_=add

  if(.not.add_) then
     vdot=zero
     vsum=zero
     vmin=+HUGE(vmin)
     vmax=-HUGE(vmax)
     vdim=0
  endif

  vdot = vdot + dot_product(v,v)
  vsum = vsum + sum(v)
  vmin = min(vmin,minval(v))
  vmax = max(vmax,maxval(v))
  vdim = vdim + size(v)
end subroutine sum_

subroutine allreduce_(vdot,vsum,vmin,vmax,vdim,comm)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    allreduce_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    vdot,vsum
!    vmin,vmax
!    vdim
!    comm
!
!   output argument list:
!    vdot,vsum
!    vmin,vmax
!    vdim
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  real(r_kind)   ,intent(inout) :: vdot,vsum
  real(r_kind)   ,intent(inout) :: vmin,vmax
  integer(i_kind),intent(inout) :: vdim
  integer        ,intent(in   ) :: comm
  
  integer(i_kind):: vdim_local

  character(len=*),parameter :: myname_=myname//"::allreduce_"
  real(kind(vdot)),dimension(2) :: bufr

  call mpi_allreduce((/vdot,vsum/),bufr,size(bufr),mpi_rtype, &
                     mpi_sum,comm,ierror)
  if(ierror/=0) then
     write(6,*)'m_stats: MPI_allreduce(dot-sum)'
     call stop2(143)
  end if
  vdot=bufr(1)
  vsum=bufr(2)

  call mpi_allreduce((/-vmin,vmax/),bufr,size(bufr),mpi_rtype, &
                     mpi_max,comm,ierror)
  if(ierror/=0) then
     write(6,*)'m_stats: MPI_allreduce(min-max)'
     call stop2(144)
  end if
  vmin=-bufr(1)
  vmax=+bufr(2)

  vdim_local=vdim
  call mpi_allreduce(vdim_local,vdim,1,mpi_rtype,mpi_sum,comm,ierror)
  if(ierror/=0) then
     write(6,*)'m_stats: MPI_allreduce(dim)'
     call stop2(145)
  end if

end subroutine allreduce_
end module m_stats
