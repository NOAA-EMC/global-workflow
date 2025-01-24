module stppsmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stppsmod    module for stpps and its tangent linear stpps_tl
!  prgmmr:
!
! abstract: module for stpps and its tangent linear stpps_tl
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stpps and its tangent linear stpps_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpps_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2013-10-28  todling - reame p3d to prse
!   2014-04-12       su - add non linear qc from Purser's scheme
!   2015-02-26       su - add njqc as an option to chose new non linear qc
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2019-09-20       su -remove VQC lines and add to call VQC subroutine
!
! subroutines included:
!   sub stpps
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpps

contains

subroutine stpps(pshead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpps       calculate penalty and contribution to
!                             stepsize for sfcp, using nonlinear qc
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize for
!           surface pressure with nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1997-12-14  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpps and stpps_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-09-18  derber  - modify to output of b1 and b3
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling  - update to use gsi_bundlemod
!   2015-12-21  yang    - Parrish's correction to the previous code in new varqc.
!
!   input argument list:
!     pshead
!     rp       - search direction for ps
!     sp       - analysis increment for ps
!     sges     - step size estimates (nstep)
!     nstep    - number of stepsize estimates  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for surface pressure - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter,njqc,vqc,nvqc
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600,zero
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_psNode , only: psNode
  use m_psNode , only: psNode_typecast
  use m_psNode , only: psNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: pshead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus,ibb,ikk
  real(r_kind) val,val2,w1,w2,w3,w4
  real(r_kind) cg_t,ps,t_pg,var_jb
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind),pointer,dimension(:) :: sp
  real(r_kind),pointer,dimension(:) :: rp
  type(psNode), pointer :: psptr

  out=zero_quad

!  If no ps data return
  if(.not. associated(pshead))return
! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'prse',sp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'prse',rp,istatus);ier=istatus+ier
  if(ier/=0)return

  psptr => psNode_typecast(pshead)
  do while (associated(psptr))
     if(psptr%luse)then
        if(nstep > 0)then
           j1 = psptr%ij(1)
           j2 = psptr%ij(2)
           j3 = psptr%ij(3)
           j4 = psptr%ij(4)
           w1 = psptr%wij(1)
           w2 = psptr%wij(2)
           w3 = psptr%wij(3)
           w4 = psptr%wij(4)
           val =w1* rp(j1)+w2* rp(j2)+w3* rp(j3)+w4* rp(j4)
           val2=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)-psptr%res
           do kk=1,nstep
              ps=val2+sges(kk)*val
              pen(kk)=ps*ps*psptr%err2
           end do
        else
           pen(1)=psptr%res*psptr%res*psptr%err2
        end if

    
!  Modify penalty term if nonlinear QC
! EC VQC

        if (vqc .and. nlnqc_iter .and. psptr%pg > tiny_r_kind .and.  &
                             psptr%b  > tiny_r_kind) then
           t_pg=psptr%pg*varqc_iter
           cg_t=cg_term/psptr%b
        else
           t_pg=zero
           cg_t=zero
        endif

!   for Dr. Jim purser' non liear quality control
        if(njqc  .and. psptr%jb > tiny_r_kind .and. psptr%jb <10.0_r_kind) then
           var_jb =psptr%jb
        else
           var_jb=zero
        endif
!  mix model VQC
        if(nvqc .and. psptr%ib >0) then
           ibb=psptr%ib
           ikk=psptr%ik
        else
           ibb=0
           ikk=0
        endif
      

         call vqc_stp(pen,nstep,t_pg,cg_t,var_jb,ibb,ikk)


         out(1) = out(1)+pen(1)*psptr%raterr2

         do kk=2,nstep
            out(kk) = out(kk)+(pen(kk)-pen(1))*psptr%raterr2
         end do

     end if

     psptr => psNode_nextcast(psptr)
  end do
  
  return
end subroutine stpps

end module stppsmod
