module inttcpmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   inttcpmod    module for intps and its tangent linear intps_tl
!   prgmmr:
!
! abstract: module for inttcp 
!
! program history log:
!   2005-05-12  Yanqiu zhu - wrap intps and its tangent linear intps_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intps_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2013-10-28  todling - rename p3d to prse
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub inttcp_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode, only: obsNode
use m_tcpNode, only: tcpNode
use m_tcpNode, only: tcpNode_typecast
use m_tcpNode, only: tcpNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC inttcp

interface inttcp; module procedure &
          inttcp_
end interface

contains

subroutine inttcp_(tcphead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inttcp       apply nonlin qc obs operator for tcps
!   prgmmr: kleist            org: np23                date: 2009-02-02
!
! abstract: apply observation operator and adjoint for tcps observations
!
! program history log:
!   2009-02-02  kleist
!   2010-05-13  todling - update to use gsi_bundle; update interface
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     tcphead - obs type pointer to obs structure
!     sp      - ps increment in grid space
!     rp
!
!   output argument list:
!     rp      - ps results from observation operator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  class(obsNode),  pointer, intent(in   ) :: tcphead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,ier,istatus
! real(r_kind) penalty
  real(r_kind) cg_ps,val,p0,grad,wnotgross,wgross,ps_pg
  real(r_kind) w1,w2,w3,w4
  real(r_kind),pointer,dimension(:) :: sp
  real(r_kind),pointer,dimension(:) :: rp
  type(tcpNode), pointer :: tcpptr

!  If no tcp data return
  if(.not. associated(tcphead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'prse',sp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'prse',rp,istatus);ier=istatus+ier
  if(ier/=0)return

  !tcpptr => tcphead
  tcpptr => tcpNode_typecast(tcphead)
  do while (associated(tcpptr))
     j1=tcpptr%ij(1)
     j2=tcpptr%ij(2)
     j3=tcpptr%ij(3)
     j4=tcpptr%ij(4)
     w1=tcpptr%wij(1)
     w2=tcpptr%wij(2)
     w3=tcpptr%wij(3)
     w4=tcpptr%wij(4)
     
!    Forward model
     val=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*tcpptr%raterr2*tcpptr%err2
           !-- tcpptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(tcpptr%diags,jiter=jiter,obssen=grad)
        else
           !-- if (tcpptr%luse) tcpptr%diags%tldepart(jiter)=val
           if (tcpptr%luse) call obsdiagNode_set(tcpptr%diags,jiter=jiter,tldepart=val)
        endif
     endif

     if(l_do_adjoint)then
        if (.not. lsaveobsens) then
           if( .not. ladtest_obs ) val=val-tcpptr%res
!          gradient of nonlinear operator
 
           if (nlnqc_iter .and. tcpptr%pg > tiny_r_kind .and.  &
                                tcpptr%b  > tiny_r_kind) then
              ps_pg=tcpptr%pg*varqc_iter
              cg_ps=cg_term/tcpptr%b                           ! b is d in Enderson
              wnotgross= one-ps_pg                            ! pg is A in Enderson
              wgross =ps_pg*cg_ps/wnotgross                   ! wgross is gama in Enderson
              p0=wgross/(wgross+exp(-half*tcpptr%err2*val**2)) ! p0 is P in Enderson
              val=val*(one-p0)                                ! term is Wqc in Enderson
           endif

           if( ladtest_obs ) then
              grad     = val
           else
              grad     = val*tcpptr%raterr2*tcpptr%err2
           end if
        end if

!       Adjoint
        rp(j1)=rp(j1)+w1*grad
        rp(j2)=rp(j2)+w2*grad
        rp(j3)=rp(j3)+w3*grad
        rp(j4)=rp(j4)+w4*grad
   
     end if
     !tcpptr => tcpptr%llpoint
     tcpptr => tcpNode_nextcast(tcpptr)
  end do
  return
end subroutine inttcp_

end module inttcpmod
