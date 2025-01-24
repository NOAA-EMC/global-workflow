module stpvismod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpvismod    module for stpvis
!  prgmmr:
!
! abstract: module for stpvis
!
! program history log:
!   2009-02-24  zhu
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stpvis
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpvis

contains

subroutine stpvis(vishead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpvis      calculate penalty and contribution to stepsize
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: calculate penalty and contribution to stepsize for surface pressure
!            with addition of nonlinear qc
!
! program history log:
!   2011-02-23  zhu  - update
!
!   input argument list:
!     vishead
!     rvis     - search direction for vis
!     svis     - analysis increment for vis
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional vis - sges(1:nstep)
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
  use m_visNode, only: visNode
  use m_visNode, only: visNode_typecast
  use m_visNode, only: visNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: vishead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_vis,vis,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_vis
  real(r_kind),pointer,dimension(:) :: svis
  real(r_kind),pointer,dimension(:) :: rvis
  type(visNode), pointer :: visptr

  out=zero_quad

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'vis',svis,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'vis',rvis,istatus);ier=istatus+ier
  if(ier/=0)return

  visptr => visNode_typecast(vishead)
  do while (associated(visptr))
     if(visptr%luse)then
        if(nstep > 0)then
           j1=visptr%ij(1)
           j2=visptr%ij(2)
           j3=visptr%ij(3)
           j4=visptr%ij(4)
           w1=visptr%wij(1)
           w2=visptr%wij(2)
           w3=visptr%wij(3)
           w4=visptr%wij(4)

           val =w1*rvis(j1)+w2*rvis(j2)+w3*rvis(j3)+w4*rvis(j4)
           val2=w1*svis(j1)+w2*svis(j2)+w3*svis(j3)+w4*svis(j4)-visptr%res

           do kk=1,nstep
              vis=val2+sges(kk)*val
              pen(kk)= vis*vis*visptr%err2
           end do
        else
           pen(1)=visptr%res*visptr%res*visptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. visptr%pg > tiny_r_kind .and.  &
                             visptr%b  > tiny_r_kind) then
           pg_vis=visptr%pg*varqc_iter
           cg_vis=cg_term/visptr%b
           wnotgross= one-pg_vis
           wgross = pg_vis*cg_vis/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*visptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*visptr%raterr2
        end do
     end if

     visptr => visNode_nextcast(visptr)

  end do
  
  return
end subroutine stpvis

end module stpvismod
