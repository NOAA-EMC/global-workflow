module stpsstmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpsstmod    module for stpsst and its tangent linear stpsst_tl
!  prgmmr:
!
! abstract: module for stpsst and its tangent linear stpsst_tl
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stpsst and its tangent linear stpsst_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpsst_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2011-04-03  li      - modify for Tr analysis
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stpsst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpsst

contains

subroutine stpsst(ssthead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsst      calculate penalty and contribution to stepsize
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: calculate penalty and contribution to stepsize for surface pressure
!            with addition of nonlinear qc
!
! program history log:
!   2004-07-20  derber
!   2004-07-30  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpsst and stpsst_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output for b1 and b3
!   2006-12-11  li      - correct bug in alpha and add cc
!   2007-03-19  tremolet - binning of observations
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-06-02  safford - rm unused var and uses
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling  - update to use gsi_bundle
!
!   input argument list:
!     ssthead
!     rsst     - search direction for sst
!     ssst     - analysis increment for sst
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional sst - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad
  use gsi_nstcouplermod, only: nst_gsi
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_sstNode, only: sstNode
  use m_sstNode, only: sstNode_typecast
  use m_sstNode, only: sstNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: ssthead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_sst,sst,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_sst
  real(r_kind),pointer,dimension(:) :: ssst
  real(r_kind),pointer,dimension(:) :: rsst
  type(sstNode), pointer :: sstptr

  out=zero_quad

!  If no sst data return
  if(.not. associated(ssthead))return
  if(.not.  nst_gsi > 2 ) return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'sst',ssst,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'sst',rsst,istatus);ier=istatus+ier
  if(ier/=0)return

  sstptr => sstNode_typecast(ssthead)
  do while (associated(sstptr))
     if(sstptr%luse)then
        if(nstep > 0)then
           j1=sstptr%ij(1)
           j2=sstptr%ij(2)
           j3=sstptr%ij(3)
           j4=sstptr%ij(4)
           w1=sstptr%wij(1)
           w2=sstptr%wij(2)
           w3=sstptr%wij(3)
           w4=sstptr%wij(4)

           val =w1*rsst(j1)+w2*rsst(j2)+w3*rsst(j3)+w4*rsst(j4)
           val2=w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)

           val  = sstptr%tz_tr*val
           val2 = sstptr%tz_tr*val2 
           val2=val2-sstptr%res

           do kk=1,nstep
              sst=val2+sges(kk)*val
              pen(kk)= sst*sst*sstptr%err2
           end do
        else
           pen(1)=sstptr%res*sstptr%res*sstptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. sstptr%pg > tiny_r_kind .and.  &
                             sstptr%b  > tiny_r_kind) then
           pg_sst=sstptr%pg*varqc_iter
           cg_sst=cg_term/sstptr%b
           wnotgross= one-pg_sst
           wgross = pg_sst*cg_sst/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*sstptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*sstptr%raterr2
        end do
     end if

     sstptr => sstNode_nextcast(sstptr)

  end do
  
  return
end subroutine stpsst

end module stpsstmod
