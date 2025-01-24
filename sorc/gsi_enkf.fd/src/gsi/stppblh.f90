module stppblhmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stppblhmod    module for stppblh
!  prgmmr:
!
! abstract: module for stppblh
!
! program history log:
!   2009-02-24  zhu
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stppblh
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stppblh

contains

subroutine stppblh(pblhhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppblh      calculate penalty and contribution to stepsize
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: calculate penalty and contribution to stepsize for surface pressure
!            with addition of nonlinear qc
!
! program history log:
!   2011-02-23  zhu  - update
!
!   input argument list:
!     pblhhead
!     rpblh     - search direction for pblh
!     spblh     - analysis increment for pblh
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional pblh - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_pblhNode, only: pblhNode
  use m_pblhNode, only: pblhNode_typecast
  use m_pblhNode, only: pblhNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: pblhhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_pblh,pblh,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_pblh
  real(r_kind),pointer,dimension(:) :: spblh
  real(r_kind),pointer,dimension(:) :: rpblh
  type(pblhNode), pointer :: pblhptr

  out=zero_quad

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'pblh',spblh,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'pblh',rpblh,istatus);ier=istatus+ier
  if(ier/=0)return

  pblhptr => pblhNode_typecast(pblhhead)
  do while (associated(pblhptr))
     if(pblhptr%luse)then
        if(nstep > 0)then
           j1=pblhptr%ij(1)
           j2=pblhptr%ij(2)
           j3=pblhptr%ij(3)
           j4=pblhptr%ij(4)
           w1=pblhptr%wij(1)
           w2=pblhptr%wij(2)
           w3=pblhptr%wij(3)
           w4=pblhptr%wij(4)

           val =w1*rpblh(j1)+w2*rpblh(j2)+w3*rpblh(j3)+w4*rpblh(j4)
           val2=w1*spblh(j1)+w2*spblh(j2)+w3*spblh(j3)+w4*spblh(j4)-pblhptr%res

           do kk=1,nstep
              pblh=val2+sges(kk)*val
              pen(kk)= pblh*pblh*pblhptr%err2
           end do
        else
           pen(1)=pblhptr%res*pblhptr%res*pblhptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. pblhptr%pg > tiny_r_kind .and.  &
                             pblhptr%b  > tiny_r_kind) then
           pg_pblh=pblhptr%pg*varqc_iter
           cg_pblh=cg_term/pblhptr%b
           wnotgross= one-pg_pblh
           wgross = pg_pblh*cg_pblh/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*pblhptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*pblhptr%raterr2
        end do
     end if

     pblhptr => pblhNode_nextcast(pblhptr)

  end do
  
  return
end subroutine stppblh

end module stppblhmod
