! ------------------------------------------------------------------------------
! SET_CVSECTION
! ------------------------------------------------------------------------------
subroutine set_cvsection(psec,ydcv,kbgn,kend)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_cvsection
!   prgmmr: tremolet
!
! abstract: Sets a section of a control vector
!
! program history log:
!   2007-05-16 tremolet
!   2009-08-14 lueken - update documentation
!   2010-06-14 treadon/todling - change for generalized CV
!
!   input argument list:
!    kbgn,kend
!    psec
!    ydcv
!
!   output argument list:
!    ydvc
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind
use mpimod, only: mype, nvar_pe
use gridmod, only: nsig
use jfunc, only: nval2d
use control_vectors, only: control_vector
use control_vectors, only: nc3d,nc2d,mvars

IMPLICIT NONE

integer(i_kind)     , intent(in   ) :: kbgn,kend
real(r_kind)        , intent(in   ) :: psec(kbgn:kend)
type(control_vector), intent(inout) :: ydcv

integer(i_kind) :: indx,ival,iloc,iend,ilen,ioff,jj

! Look for starting index
if (kbgn<1.or.kbgn>(nc3d*nsig+nc2d+mvars)*nval2d) then
   write(6,*)'set_cvsection: kbgn out of range',kbgn
   call stop2(119)
end if
if (kend<1.or.kend>(nc3d*nsig+nc2d+mvars)*nval2d) then
   write(6,*)'set_cvsection: kend out of range',kend
   call stop2(120)
end if

indx=kbgn
do while (indx<=kend)
   ival=indx/nval2d+1
   iloc=indx-nval2d*(ival-1)
   iend=MIN(kend,ival*nval2d)
   ilen=iend-iloc
   if (mype==nvar_pe(ival,1)) then
      ioff=(nvar_pe(ival,2)-1)*nval2d+iloc
      do jj=0,ilen
         ydcv%values(ioff+jj)=psec(indx+jj)
      enddo
   endif
   indx=indx+ilen+1
enddo

return
end subroutine set_cvsection

! ------------------------------------------------------------------------------
! ALLGATHER_CVSECTION - Gather part of the control variable on all PEs.
! ------------------------------------------------------------------------------
subroutine allgather_cvsection(ydcv,psec,kbgn,kend)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    allgather_cvsection
!   prgmmr: tremolet
!
! abstract: Gathers a section of a control vector
!
! program history log:
!   2007-05-16 tremolet
!   2009-08-14 lueken - update documentation
!   2010-06-14 treadon/todling - change for generalized CV
!
!   input argument list:
!    ydcv
!    kbgn,kend
!
!   output argument list:
!    psec
!
! NOTE: this routine is inefficient to gather long sections
! or full vectors. In practice, it is only used for very short
! sections of the control_vector in lanczos:preppcm.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind
use mpimod, only: mype, nvar_pe
use gridmod, only: nsig
use jfunc, only: nval2d
use control_vectors, only: control_vector
use control_vectors, only: nc3d,nc2d,mvars

IMPLICIT NONE

type(control_vector), intent(in   ) :: ydcv
integer(i_kind)     , intent(in   ) :: kbgn,kend
real(r_kind)        , intent(  out) :: psec(kbgn:kend)

integer(i_kind) :: indx,ival,iloc,iend,ilen,ioff,jj
real(r_kind) :: work(nval2d)

! Look for starting index
if (kbgn<1.or.kbgn>(nc3d*nsig+nc2d+mvars)*nval2d) then
   write(6,*)'all_cvsection: kbgn out of range',kbgn
   call stop2(121)
end if
if (kend<1.or.kend>(nc3d*nsig+nc2d+mvars)*nval2d) then
   write(6,*)'all_cvsection: kend out of range',kend
   call stop2(122)
end if

indx=kbgn
do while (indx<=kend)
   ival=indx/nval2d+1
   iloc=indx-nval2d*(ival-1)
   iend=MIN(kend,ival*nval2d)
   ilen=iend-iloc
   if (mype==nvar_pe(ival,1)) then
      ioff=(nvar_pe(ival,2)-1)*nval2d+iloc
      do jj=0,ilen
         work(jj+1)=ydcv%values(ioff+jj)
      enddo
   endif
   call mpl_bcast(nvar_pe(ival,1),ilen+1,work)
   do jj=0,ilen
      psec(indx+jj)=work(jj+1)
   enddo
   indx=indx+ilen+1
enddo

return
end subroutine allgather_cvsection
! ------------------------------------------------------------------------------
