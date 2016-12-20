subroutine strong_baldiag_inc(sval,nsval)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_baldiag_inc    get balance diagnostics
!   prgmmr: parrish          org: np23                date: 2006-08-12
!
! abstract: get balance diagnostic statistics of increment
!
! program history log:
!   2006-08-12  parrish
!   2007-04-16  kleist   - modified to be used for diagnostics only
!   2007-07-26 cucurull  - call getprs; add xhat3dp and remove ps in calctends_tl argument list
!   2007-08-08  derber - only calculate dynamics time derivatives
!   2008-04-09  safford  - rm unused vars and uses
!   2009-01-17  todling  - per early changes from Tremolet (revisited)
!   2010-05-13  todling  - udpate to use gsi_bundle
!                          BUG FIX: was missing deallocate_state call
!   2011-06-07  guo      - fixed the dimension of argument sval and added nsval
!   2011-07-03  todling  - avoid explicit reference to internal bundle arrays
!   2011-11-01  eliu     - add handling for ql and qi increments 
!   2012-02-08  kleist   - add uvflag=.true. to call to strong_bal_correction
!   2012-03-11  tong     - modified to take into account the case when cw is not
!                          a control variable or not included in met_guess
!   2013-10-19  todling  - guess in metguess now
!   2013-10-28  todling  - rename p3d to prse
!   2014-06-05  eliu     - add condition to get pointer for cw tendency  
!
!   input argument list:
!     sval    - current solution in state space
!
!   output argument list:
!     sval
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind
  use mpimod, only: mype
  use gridmod, only: nnnn1o
  use gridmod, only: lat2,lon2,nsig
  use gsi_4dvar, only: nsubwin
  use mod_vtrans,only: nvmodes_keep
  use state_vectors, only: allocate_state
  use state_vectors, only: deallocate_state
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: assignment(=)
  use gsi_metguess_mod,  only: gsi_metguess_get
  use constants, only: zero
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(inout) :: sval(nsval)
  integer(i_kind) ,intent(in   ) :: nsval

! Declare local variables
  character(len=*),parameter::myname='strong_baldiag_inc' 
  integer(i_kind) ii,ier,iqi,iql,icw,istatus   
  integer(i_kind) is_u,is_v,is_t,is_q,is_qi,is_ql,is_cw,is_oz,is_p,is_prse
  real(r_kind),pointer,dimension(:,:,:) :: dhat_dt_u  =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: dhat_dt_v  =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: dhat_dt_t  =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: dhat_dt_q  =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: dhat_dt_oz =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: dhat_dt_cw =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: dhat_dt_ql =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: dhat_dt_qi =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: dhat_dt_prse=>NULL()
!
  real(r_kind),pointer,dimension(:,:,:) :: p_u  =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_v  =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_q  =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_t  =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_cw =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_ql =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: p_qi =>NULL()
  real(r_kind),pointer,dimension(:,:  ) :: p_ps =>NULL()
  real(r_kind),allocatable,dimension(:,:,:) :: cw_hold
  logical fullfield
  type(gsi_bundle) dhat_dt
  logical lcld

!************************************************************************************  
! Initialize variable

! Get relevant pointers; return if not found
  ier=0;icw=0;iql=0;iqi=0
  call gsi_bundlegetpointer(sval(1),'u',   is_u,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval(1),'v',   is_v,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval(1),'tv',  is_t,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval(1),'q',   is_q,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval(1),'oz',  is_oz,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval(1),'cw',  is_cw,  istatus);icw=istatus+icw           
  call gsi_bundlegetpointer(sval(1),'ql',  is_ql,  istatus);iql=istatus+iql           
  call gsi_bundlegetpointer(sval(1),'qi',  is_qi,  istatus);iqi=istatus+iqi           
  call gsi_bundlegetpointer(sval(1),'ps',  is_p,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval(1),'prse',is_prse,istatus);ier=istatus+ier
  if(ier/=0)then
    write(6,*) myname, 'trouble getting sval pointers, ier= ', ier 
    call stop2(999)
  endif

  call allocate_state(dhat_dt) 
  dhat_dt=zero
  ier=0;icw=0;iql=0;iqi=0
  call gsi_bundlegetpointer(dhat_dt,'u',  dhat_dt_u,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dhat_dt,'v',  dhat_dt_v,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dhat_dt,'tv', dhat_dt_t,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dhat_dt,'q',  dhat_dt_q,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dhat_dt,'oz', dhat_dt_oz,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dhat_dt,'cw', dhat_dt_cw, istatus);icw=istatus+icw
  call gsi_bundlegetpointer(dhat_dt,'ql', dhat_dt_ql, istatus);iql=istatus+iql
  call gsi_bundlegetpointer(dhat_dt,'qi', dhat_dt_qi, istatus);iqi=istatus+iqi
  call gsi_bundlegetpointer(dhat_dt,'prse',dhat_dt_prse,istatus);ier=istatus+ier

  if(ier/=0) then
    write(6,*) myname, ': trouble getting temporary tendency pointers, ier= ', ier           
    call stop2(999)
  endif

!     compute derivatives
! Determine how many vertical levels each mpi task will
! handle in computing horizontal derivatives

  lcld = (icw==0 .or. (iql+iqi)==0)

  do ii=1,nsval
!    if(mype==0) write(6,'(1x,a,i0,a)') 'strong_baldiag_inc: sval(',ii,')'

     call gsi_bundlegetpointer(sval(ii),'u',   p_u,  istatus)
     call gsi_bundlegetpointer(sval(ii),'v',   p_v,  istatus)
     call gsi_bundlegetpointer(sval(ii),'tv',  p_t,  istatus)
     call gsi_bundlegetpointer(sval(ii),'q',   p_q,  istatus) 
!_RT call gsi_bundlegetpointer(sval(ii),'oz',  p_oz, istatus)
!    call gsi_bundlegetpointer(sval(ii),'cw',  p_cw, istatus)
     call gsi_bundlegetpointer(sval(ii),'ps',  p_ps, istatus)
!_RT call gsi_bundlegetpointer(sval(ii),'prse',p_prse,istatus)

     if (lcld) then
        if (icw==0) then
!_RT       call gsi_bundlegetpointer(sval(ii),'cw', p_cw, istatus)
           call calctends_tl(sval(ii),dhat_dt,mype)
        else
           call gsi_bundlegetpointer(sval(ii),'qi', p_cw, istatus)
           call gsi_bundlegetpointer(sval(ii),'ql', p_ql, istatus)
           call gsi_bundlegetpointer(sval(ii),'qi', p_qi, istatus)
           allocate(cw_hold(size(p_ql,1),size(p_ql,2),size(p_ql,3)))
           cw_hold=p_cw
           p_cw=p_ql+p_qi
           call calctends_tl(sval(ii),dhat_dt,mype)
           p_cw=cw_hold
           deallocate(cw_hold)
!          call calctends_tl( &
!            p_u,p_v ,p_t,  &
!            p_q,p_oz,(p_ql+p_qi), &
!            mype, nnnn1o,          &
!            dhat_dt_u,dhat_dt_v ,dhat_dt_t,dhat_dt_prse, &
!            dhat_dt_q,dhat_dt_oz,dhat_dt_ql,p_prse)    ! eliu: for now, just use
                                                       ! dhat_dt_ql to hold time tendency terms for cw
                                                       ! since it is
                                                       ! calculated but
                                                       ! not used 
             !RTodling: Emily we need to talk about ... dhat_dt_ql not used anyway!
        end if
     else
        call calctends_tl(sval(ii),dhat_dt,mype) 
     end if

     if(nvmodes_keep>0) then
        fullfield=.false.
        call strong_bal_correction(dhat_dt_u,dhat_dt_v,dhat_dt_t,dhat_dt_prse,&
                                   mype,p_u,p_v,&
                                        p_t,p_ps,&   
                                   .true.,fullfield,.false.,.true.)
     end if
  enddo
  call deallocate_state(dhat_dt)   

  return
end subroutine strong_baldiag_inc
