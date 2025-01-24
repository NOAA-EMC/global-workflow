module stpgpsmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpgpsmod    module for stpref and its tangent linear stpref_tl
!  prgmmr:
!
! abstract: module for stpref and its tangent linear stpref_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpref and its tangent linear stpref_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2009-08-12  lueken - updated documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2013-10-28  todling - rename p3d to prse
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stpgps
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpgps

contains

subroutine stpgps(gpshead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stpref    compute contribution to penalty and stepsize
!                       from ref, using nonlinear qc    
!   prgmmr: cucurull,l.     org: JCSDA/NCEP           date: 2004-05-06
!
! abstract:  This routine applies the (linear) operator for local 
!            refractivity and linear linear estimate for step size. 
!            This version includes nonlinear qc.
!
! program history log:
!   2004-05-06  cucurull 
!   2004-06-21  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004=10-08  parrish - add nonlinear qc option
!   2004-11-30  cucurull- add increments for surface pressure and temperature at levels
!                         below observation. Install non-linear forward operator.
!   2005-01-26  cucurull- Implement local GPS RO operator
!   2005-03-23  cucurull- correct bouds for obs below the second level
!   2005-04-11  treadon - merge stpref and stpref_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-12-02  cucurull - fix bug for dimensions of sp and rp
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-06  cucurull - generalize code to hybrid vertical coordinate and modify to use 
!                          surface pressure
!   2007-01-13  derber - clean up code and use coding standards
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2007-07-26  cucurull - input 3d pressure to update code to generalized vertical coordinate
!   2008-04-11  safford - rm unused vars
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling - update to use gsi_bundle
!
!   input argument list:
!     gpshead
!     rt    - search direction (gradxJ) for virtual temperature
!     rq    - search direction (gradxJ) for specific humidity
!     rp    - search direction (gradxJ) for (3D) pressure
!     st    - analysis increment (correction) for virtual temperature
!     sq    - analysis increment (correction) for specific humidity
!     sp    - analysis increment (correction) for (3D) pressure
!     sges  - stepsize estimates (nstep)
!     nstep - number of stepsize estimates (==0 means use outer iteration values)
!                                         
!   output argument list:
!     out(1:nstep)- contribution to penalty from local gps refractivity - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,one,two,half,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: nsig
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_gpsNode, only: gpsNode
  use m_gpsNode, only: gpsNode_typecast
  use m_gpsNode, only: gpsNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode   ),pointer           ,intent(in   ) :: gpshead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind):: j,kk,ier,istatus
  integer(i_kind):: i1,i2,i3,i4
  real(r_kind) :: val,val2
  real(r_kind) :: w1,w2,w3,w4
  real(r_kind) :: q_TL,p_TL,t_TL
  real(r_kind) :: rq_TL,rp_TL,rt_TL
  real(r_kind),dimension(nsig) :: valk2,valk
  real(r_kind),pointer,dimension(:) :: st,sq
  real(r_kind),pointer,dimension(:) :: rt,rq
  real(r_kind),pointer,dimension(:) :: sp
  real(r_kind),pointer,dimension(:) :: rp
  type(gpsNode), pointer :: gpsptr

  real(r_kind) cg_gps,wgross,wnotgross
  real(r_kind) pg_gps,nref
  real(r_kind),dimension(max(1,nstep))::pen

! Initialize penalty, b1, and b3 to zero
  out=zero_quad

!  If no gps data return
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


! Loop over observations
  gpsptr => gpsNode_typecast(gpshead)
  do while (associated(gpsptr))

     if(gpsptr%luse)then

        val2=-gpsptr%res
        if(nstep > 0)then
           w1=gpsptr%wij(1)
           w2=gpsptr%wij(2)
           w3=gpsptr%wij(3)
           w4=gpsptr%wij(4)


!$omp parallel do schedule(dynamic,1) private(j,t_TL,rt_TL,q_TL,rq_TL,p_TL,rp_TL,i1,i2,i3,i4)
           do j=1,nsig
              i1= gpsptr%ij(1,j)
              i2= gpsptr%ij(2,j)
              i3= gpsptr%ij(3,j)
              i4= gpsptr%ij(4,j)
              t_TL =w1* st(i1)+w2* st(i2)+w3* st(i3)+w4* st(i4)
              rt_TL=w1* rt(i1)+w2* rt(i2)+w3* rt(i3)+w4* rt(i4)
              q_TL =w1* sq(i1)+w2* sq(i2)+w3* sq(i3)+w4* sq(i4)
              rq_TL=w1* rq(i1)+w2* rq(i2)+w3* rq(i3)+w4* rq(i4)
              p_TL =w1* sp(i1)+w2* sp(i2)+w3* sp(i3)+w4* sp(i4)
              rp_TL=w1* rp(i1)+w2* rp(i2)+w3* rp(i3)+w4* rp(i4)
              valk2(j) =  t_tl*gpsptr%jac_t(j)+ q_tl*gpsptr%jac_q(j)+ p_tl*gpsptr%jac_p(j) 
              valk(j)  = rt_tl*gpsptr%jac_t(j)+rq_tl*gpsptr%jac_q(j)+rp_tl*gpsptr%jac_p(j)
           enddo

           val=zero
           do j=1,nsig
              val2 = val2 + valk2(j) 
              val  = val + valk(j)
           enddo
!          penalty and gradient

           do kk=1,nstep
              nref=val2+sges(kk)*val
              pen(kk)=nref*nref*gpsptr%err2
           end do
        else
           pen(1)=val2*val2*gpsptr%err2
        end if

!       Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. gpsptr%pg > tiny_r_kind .and. gpsptr%b > tiny_r_kind) then
           pg_gps=gpsptr%pg*varqc_iter
           cg_gps=cg_term/gpsptr%b
           wnotgross= one-pg_gps
           wgross = pg_gps*cg_gps/wnotgross
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif
  
!       Cost function
        out(1) = out(1)+pen(1)*gpsptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*gpsptr%raterr2
        end do

     endif
  
     gpsptr => gpsNode_nextcast(gpsptr)

  end do

 return
end subroutine stpgps


end module stpgpsmod
