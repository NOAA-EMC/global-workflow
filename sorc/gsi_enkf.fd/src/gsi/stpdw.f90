module stpdwmod

!$$$  module documentation block
!            .      .    .                                       .
! module:    stpdwmod    module for stpdw and its tangent linear stpdw_tl
!  prgmmr:
!
! abstract: module for stpdw and its tangent linear stpdw_tl
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stpdw and its tangent linear stpdw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpdw_tl
!   2009-08-12  lueken  - updated documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stpdw
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpdw

contains

subroutine stpdw(dwhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpdw  calculate contribution to penalty and
!                stepsize from dw, with nonlinear qc added.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate contribution to penalty and stepsize from lidar winds
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpdw and stpdw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber   - modify to use new inner loop obs data structure
!                        - unify NL qc
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-04-11  safford - rm unused vars
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling - update to use gsi_bundle; update interface
!
!   input argument list:
!     dwhead
!     ru   - search direction for u
!     rv   - search direction for v
!     su   - current analysis increment for u
!     sv   - current analysis increment for v
!     sges - step size estimates (4)
!     nstep- number of step sizes (== 0 means use outer iteration values)
!
!   output argument list:                                      
!     out(1:nstep) - penalty contribution from lidar winds sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_dwNode , only: dwNode
  use m_dwNode , only: dwNode_typecast
  use m_dwNode , only: dwNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode),pointer,intent(in):: dwhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk,ier,istatus
  real(r_kind) valdw,facdw,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) pg_dw,dw
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind) cg_dw,wgross,wnotgross
  real(r_kind),pointer,dimension(:) :: su,sv
  real(r_kind),pointer,dimension(:) :: ru,rv
  type(dwNode), pointer :: dwptr

  out=zero_quad

!  If no dw data return
  if(.not. associated(dwhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  if(ier/=0)return

  dwptr => dwNode_typecast(dwhead)
  do while (associated(dwptr))
     if(dwptr%luse)then
        if(nstep > 0)then
           j1=dwptr%ij(1)
           j2=dwptr%ij(2)
           j3=dwptr%ij(3)
           j4=dwptr%ij(4)
           j5=dwptr%ij(5)
           j6=dwptr%ij(6)
           j7=dwptr%ij(7)
           j8=dwptr%ij(8)
           w1=dwptr%wij(1)
           w2=dwptr%wij(2)
           w3=dwptr%wij(3)
           w4=dwptr%wij(4)
           w5=dwptr%wij(5)
           w6=dwptr%wij(6)
           w7=dwptr%wij(7)
           w8=dwptr%wij(8)


           valdw=(w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4)+&
                  w5* ru(j5)+w6* ru(j6)+w7* ru(j7)+w8* ru(j8))*dwptr%sinazm+&
                 (w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4)+&
                  w5* rv(j5)+w6* rv(j6)+w7* rv(j7)+w8* rv(j8))*dwptr%cosazm
 
           facdw=(w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)+&
                  w5* su(j5)+w6* su(j6)+w7* su(j7)+w8* su(j8))*dwptr%sinazm+&
                 (w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)+&
                  w5* sv(j5)+w6* sv(j6)+w7* sv(j7)+w8* sv(j8))*dwptr%cosazm-&
                 dwptr%res
           do kk=1,nstep
              dw=facdw+sges(kk)*valdw
              pen(kk)=dw*dw*dwptr%err2
           end do
        else
           pen(1)=dwptr%res*dwptr%res*dwptr%err2
        end if

!       Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. dwptr%pg > tiny_r_kind .and. dwptr%b > tiny_r_kind) then
           pg_dw=dwptr%pg*varqc_iter
           cg_dw=cg_term/dwptr%b
           wnotgross= one-pg_dw
           wgross = pg_dw*cg_dw/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*dwptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*dwptr%raterr2
        end do
     end if

     dwptr => dwNode_nextcast(dwptr)

  end do

  return
end subroutine stpdw

end module stpdwmod
