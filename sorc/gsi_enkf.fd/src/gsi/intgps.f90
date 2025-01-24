module intgpsmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intrefmod    module for intref and its tangent linear intref_tl
!   prgmmr:
!
! abstract: module for intref and its tangent linear intref_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intref and its tangent linear intref_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-28  Todling - add interface back
!   2009-08-13  lueken - update documentation
!   2013-10-28  todling - rename p3d to prse
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub intgps_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use m_obsNode, only: obsNode
use m_gpsNode, only: gpsNode
use m_gpsNode, only: gpsNode_typecast
use m_gpsNode, only: gpsNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intgps

interface intgps; module procedure &
          intgps_
end interface

contains

subroutine intgps_(gpshead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intref      apply nonlinqc obs operator refractivity
!   prgmmr: cucurull, l.     org: JCSDA/NCEP          date: 2004-04-29
!
! abstract: apply gps local refractivity operator and adjoint with
!           addition of nonlinear qc.
!
! program history log:
!   2004-04-29  cucurull- original code
!   2004-06-21  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2004-11-19  cucurull- add increments for surface pressure and temperature at levels
!                          below observation. Install non-linear forward operator.
!   2005-01-26  cucurull- Implement local GPS RO operator
!   2005-03-01  parrish - nonlinear qc change as above; correct bug in zeroing of tl_AD
!   2005-03-23  cucurull- correct bounds for obs below the second level; place 
!                         bounds for k1 and k2
!   2005-04-11  treadon - merge intref and intref_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-12-02  cucurull - fix bug for dimensions of sp and rp
!   2006-01-03  treadon - include r_kind type in w1,w2,...,w12 declaration
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-06  cucurull - generalize code to hybrid vertical coordinate and modify to use
!                          surface pressure
!   2007-01-13  derber - clean up code and use coding standards
!   2007-03-28  derber - turn intref into generalized intgps
!   2007-07-26  cucurull - in/out 3d pressure to update code to generalized vertical coordinate
!   2008-06-02  safford - rm unused vars
!   2008-11-26  todling  - add 4dvar and GSI adjoint capability (obs binning, obsens, etc) 
!   2008-11-26  todling - turned FOTO optional; changed ptr%time handle
!   2010-05-13  todling - update to use gsi_bundle; update interface
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!   
!   input argument list:
!     gpshead  - obs type pointer to obs structure
!     st       - input temperature correction field
!     sq       - input q correction field
!     sp       - input (3D) p correction field
!
!   output argument list:
!     gpshead  - obs type pointer to obs structure
!     rt       - output t vector after inclusion of gps local refractivity
!     rq       - output q vector after inclusion of gps local refractivity
!     rp       - output p vector after inclusion of gps local refractivity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: nsig
  use constants, only: zero,one,half,tiny_r_kind,cg_term,r3600
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  class(obsNode),  pointer, intent(in   ) :: gpshead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) j,ier,istatus
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
  real(r_kind) :: w1,w2,w3,w4
  real(r_kind) :: p_TL,p_AD,t_TL,t_AD,q_TL,q_AD
  real(r_kind) :: val,pg_gps
  real(r_kind),dimension(nsig) :: valk
  real(r_kind) ::cg_gps,grad,p0,wnotgross,wgross
  real(r_kind),pointer,dimension(:) :: st,sq
  real(r_kind),pointer,dimension(:) :: rt,rq
  real(r_kind),pointer,dimension(:) :: sp
  real(r_kind),pointer,dimension(:) :: rp
  type(gpsNode), pointer :: gpsptr

!  If no gps obs return
  if(.not. associated(gpshead))return
! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'tv'  ,st,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'q'   ,sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'prse',sp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'tv'  ,rt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q'   ,rq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'prse',rp,istatus);ier=istatus+ier
  if(ier/=0)return

  !gpsptr => gpshead
  gpsptr => gpsNode_typecast(gpshead)
  do while (associated(gpsptr))

! Load location information into local variables
     do j=1,nsig
        i1(j)= gpsptr%ij(1,j)
        i2(j)= gpsptr%ij(2,j)
        i3(j)= gpsptr%ij(3,j)
        i4(j)= gpsptr%ij(4,j)
     enddo
     w1=gpsptr%wij(1)
     w2=gpsptr%wij(2)
     w3=gpsptr%wij(3)
     w4=gpsptr%wij(4)

!  local refractivity (linear operator)

!$omp parallel do schedule(dynamic,1) private(j,t_TL,q_TL,p_TL)
     do j=1,nsig
        t_TL=w1*st(i1(j))+w2*st(i2(j))+w3*st(i3(j))+w4*st(i4(j))
        q_TL=w1*sq(i1(j))+w2*sq(i2(j))+w3*sq(i3(j))+w4*sq(i4(j))
        p_TL=w1*sp(i1(j))+w2*sp(i2(j))+w3*sp(i3(j))+w4*sp(i4(j))
        valk(j) = p_TL*gpsptr%jac_p(j) + t_TL*gpsptr%jac_t(j)+q_TL*gpsptr%jac_q(j)
     end do

     val=zero
     do j=1,nsig
       val = val+valk(j)
     end do

     if (luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*gpsptr%raterr2*gpsptr%err2
           !-- gpsptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(gpsptr%diags,jiter=jiter,obssen=grad)
        else
           !-- if (gpsptr%luse) gpsptr%diags%tldepart(jiter)=val
           if (gpsptr%luse) call obsdiagNode_set(gpsptr%diags,jiter=jiter,tldepart=val)
        endif
     endif

!    Do adjoint
     if (l_do_adjoint) then

        if (.not. lsaveobsens) then
           if( .not. ladtest_obs)  val=val-gpsptr%res
 
!          needed for gradient of nonlinear qc operator
           if (nlnqc_iter .and. gpsptr%pg > tiny_r_kind .and.  &
                                gpsptr%b  > tiny_r_kind) then
              pg_gps=gpsptr%pg*varqc_iter
              cg_gps=cg_term/gpsptr%b
              wnotgross= one-pg_gps
              wgross = pg_gps*cg_gps/wnotgross
              p0   = wgross/(wgross+exp(-half*gpsptr%err2*val**2))
              val = val*(one-p0)
           endif
       
           if( ladtest_obs) then
              grad = val
           else
              grad = val*gpsptr%raterr2*gpsptr%err2
           end if
        endif


!       adjoint 

!$omp parallel do schedule(dynamic,1) private(j,t_AD,q_AD,p_AD)
        do j=1,nsig
           t_AD = grad*gpsptr%jac_t(j)
           rt(i1(j))=rt(i1(j))+w1*t_AD
           rt(i2(j))=rt(i2(j))+w2*t_AD
           rt(i3(j))=rt(i3(j))+w3*t_AD
           rt(i4(j))=rt(i4(j))+w4*t_AD
           q_AD = grad*gpsptr%jac_q(j)
           rq(i1(j))=rq(i1(j))+w1*q_AD
           rq(i2(j))=rq(i2(j))+w2*q_AD
           rq(i3(j))=rq(i3(j))+w3*q_AD
           rq(i4(j))=rq(i4(j))+w4*q_AD
           p_AD = grad*gpsptr%jac_p(j)
           rp(i1(j))=rp(i1(j))+w1*p_AD
           rp(i2(j))=rp(i2(j))+w2*p_AD
           rp(i3(j))=rp(i3(j))+w3*p_AD
           rp(i4(j))=rp(i4(j))+w4*p_AD
        enddo

     endif

     !gpsptr => gpsptr%llpoint
     gpsptr => gpsNode_nextcast(gpsptr)

  end do

  return
end subroutine intgps_


end module intgpsmod
