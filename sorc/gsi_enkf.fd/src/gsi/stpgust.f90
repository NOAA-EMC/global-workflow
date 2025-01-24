module stpgustmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpgustmod    module for stpgust
!  prgmmr:
!
! abstract: module for stpgust
!
! program history log:
!   2009-02-24  zhu
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stpgust
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpgust

contains

subroutine stpgust(gusthead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpgust      calculate penalty and contribution to stepsize
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: calculate penalty and contribution to stepsize for surface pressure
!            with addition of nonlinear qc
!
! program history log:
!   2011-02-23  zhu  - update
!
!   input argument list:
!     gusthead
!     rgust     - search direction for gust
!     sgust     - analysis increment for gust
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional gust - sges(1:nstep)
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
  use m_gustNode, only: gustNode
  use m_gustNode, only: gustNode_typecast
  use m_gustNode, only: gustNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode ),pointer             ,intent(in   ) :: gusthead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_gust,gust,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_gust
  real(r_kind),pointer,dimension(:) :: sgust
  real(r_kind),pointer,dimension(:) :: rgust
  type(gustNode), pointer :: gustptr

  out=zero_quad

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'gust',sgust,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'gust',rgust,istatus);ier=istatus+ier
  if(ier/=0)return

  gustptr => gustNode_typecast(gusthead)
  do while (associated(gustptr))
     if(gustptr%luse)then
        if(nstep > 0)then
           j1=gustptr%ij(1)
           j2=gustptr%ij(2)
           j3=gustptr%ij(3)
           j4=gustptr%ij(4)
           w1=gustptr%wij(1)
           w2=gustptr%wij(2)
           w3=gustptr%wij(3)
           w4=gustptr%wij(4)

           val =w1*rgust(j1)+w2*rgust(j2)+w3*rgust(j3)+w4*rgust(j4)
           val2=w1*sgust(j1)+w2*sgust(j2)+w3*sgust(j3)+w4*sgust(j4)-gustptr%res

           do kk=1,nstep
              gust=val2+sges(kk)*val
              pen(kk)= gust*gust*gustptr%err2
           end do
        else
           pen(1)=gustptr%res*gustptr%res*gustptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. gustptr%pg > tiny_r_kind .and.  &
                             gustptr%b  > tiny_r_kind) then
           pg_gust=gustptr%pg*varqc_iter
           cg_gust=cg_term/gustptr%b
           wnotgross= one-pg_gust
           wgross = pg_gust*cg_gust/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*gustptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*gustptr%raterr2
        end do
     end if

     gustptr => gustNode_nextcast(gustptr)

  end do
  
  return
end subroutine stpgust

end module stpgustmod
