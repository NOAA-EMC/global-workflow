!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 601.1     !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: model_ad:  
!
! !INTERFACE:

subroutine model_ad(xini,xobs,ldprt)

! !USES:

use kinds, only: r_kind,i_kind
use gsi_4dvar, only: nsubwin,nobs_bins,winlen,winsub,hr_obsbin
use gsi_4dvar, only: iadateend,idmodel
use gsi_4dvar, only: liauon
use constants, only: zero,r3600
use state_vectors, only: allocate_state,deallocate_state,dot_product
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundleAddMul
use gsi_bundlemod, only: self_add,assignment(=)
use gsi_4dcouplermod, only: gsi_4dcoupler_init_model_ad
use gsi_4dcouplermod, only: gsi_4dcoupler_model_ad
use gsi_4dcouplermod, only: gsi_4dcoupler_final_model_ad
use m_tick, only: tick
use timermod, only: timer_ini,timer_fnl
use mpeu_util,only: die,tell
use mpimod, only: mype

#ifdef _LAG_MODEL_
use lag_fields, only: nlocal_orig_lag, ntotal_orig_lag
use lag_fields, only: lag_ad_vec,lag_tl_spec_i,lag_tl_spec_r
use lag_fields, only: lag_u_full,lag_v_full
use lag_fields, only: lag_ADscatter_stateuv
use lag_traj, only: lag_rk2iter_ad
#endif
! use lag_traj, only: lag_rk4iter_ad

implicit none

! !INPUT PARAMETERS:

type(gsi_bundle), target, intent(in   ) :: xobs(nobs_bins) ! Adjoint state variable at observations times
logical         , intent(in   ) :: ldprt           ! Print-out flag

! !INPUT/OUTPUT PARAMETERS:

type(gsi_bundle), target, intent(inout) :: xini(nsubwin)   ! Adjoint state variable at control times

! !DESCRIPTION: Run AGCM adjoint model.
!
! !REVISION HISTORY:
!
!  19Apr2007  tremolet - initial code
!  29May2007  todling  - add actual calls to interface and AGCM AD model
!  29Jun2007  todling  - adm verified against tlm
!  30Sep2007  todling  - add timer
!  30Apr2009  meunier  - add trajectory model for lagrangian data
!  13May2010  todling  - update to use gsi_bundle
!  27May2010  todling  - gsi_4dcoupler; remove all user-specific TL-related references
!  31Aug2010  Guo      - new implementation of model_ad, which separates
!			 full perturbation vector xx, to become xini for
!			 a output increment perturbation and xobs for an
!  			 input perturbation.
!  13Oct2010  Guo      - cleaned up idmodel related operations.  idmodel
!			 mode of pertmod is now controled by its actual
!			 implementation behind module gsi_4dcouplermod.
!
!EOP
!-----------------------------------------------------------------------

! Declare local variables
character(len=*), parameter :: myname = 'model_ad'

#ifdef _LAG_MODEL_
integer(i_kind)    :: ii,jj
real(r_kind),pointer,dimension(:,:,:)  :: xx_u,xx_v
real(r_kind),dimension(3):: ad_tmp_locvect
#endif
integer(i_kind)    :: nstep,istep,nfrctl,nfrobs,ierr,n
integer(i_kind)    :: nymdi,nhmsi,ndt,dt,ndtpert
real(r_kind)       :: d0,tstep
type(gsi_bundle),pointer:: p_xini
type(gsi_bundle),pointer:: p_xobs
type(gsi_bundle) :: xxpert	! perturbation state, persistent between steps
logical:: ldprt_,iau_on_

! Temporary vector for lagrangian backward integration
real(r_kind):: wt

!******************************************************************************

! Initialize timer
call timer_ini('model_ad')
	n=size(xobs)
	if(n<1) call die(myname,'unexpected size, size(xobs) =',n)

ldprt_=ldprt	! .or.mype==0	!! in case one needs to debug locally
iau_on_=liauon

! Initialize AD model
	! Get [date,time]
nymdi  =  iadateend/100
nhmsi  = (iadateend-100*nymdi)*10000

!----	call gsi_4dcoupler_init_model_ad(nymdi,nhmsi,ndtpert)
	! Get ndtpert for pertmod_AD time step in seconds;
	! Create and initialize a persistent state
call gsi_4dcoupler_init_model_ad(xxpert,xobs(1),nymdi,nhmsi,ndtpert,rc=ierr)
	if(ierr/=0) call die(myname,'gsi_4dcoupler_init_model_ad(), rc =',ierr)
do n=1,nsubwin
  xini(n)=0._r_kind
enddo

! Determine corresponding GSI time step parameters.
! A GSI time step is a hr_obsbin time interval.
ndt    = NINT(hr_obsbin*r3600/ndtpert)	! count of pertmod_TL time step in 1 hr_obsbin
dt     = ndt*ndtpert			! one GSI time step in seconds
tstep  = dt				! one GSI time step in seconds

nstep  = NINT(winlen*r3600/tstep)	! e.g. 6
nfrctl = NINT(winsub*r3600/tstep)	! e.g. 6
nfrobs = NINT(hr_obsbin*r3600/tstep)	! e.g. 1

wt=0.
if(iau_on_) then
  wt=1._r_kind/nfrctl
  if(ldprt_.and.mype==0) call tell(myname,'increment weighting, wt =',wt)
endif

if (ldprt_.and.mype==0) write(6,'(a,3(1x,i4))')'model_ad: nstep,nfrctl,nfrobs=',nstep,nfrctl,nfrobs

! Locate (nstep) in xobs, if any.  Then add this adjoint increment to
! the current state (xxpert).

	p_xobs => istep_locate_(xobs,nstep,nfrobs, &
		ldprt_.and.mype==0,myname//".xobs-",nymdi,nhmsi)

if(associated(p_xobs)) call self_add(xxpert,p_xobs)	! xxpert += p_xobs

! Locate (nstep) in xini, if any.  Then store the current adjoint state
! (xxpert) to xini.

      if(iau_on_) then
	p_xini => iau_locate_(xini,nstep,nfrctl, &
		ldprt_.and.mype==0,myname//".xini-",nymdi,nhmsi)
      else
	p_xini => istep_locate_(xini,nstep,nfrctl, &
		ldprt_.and.mype==0,myname//".xini-",nymdi,nhmsi)
      endif

if(associated(p_xini)) then
  if(iau_on_) then
    call gsi_bundleAddMul(p_xini,wt,xxpert)	! p_xini += wt*xxpert
  else
    call self_add(p_xini,xxpert)		! p_xini += xxpert
  endif
endif

! Run AD model
do istep=nstep-1,0,-1
  ! Locate (istep+1) in xobs, if any.  Then apply AD model from istep+1
  ! (xxpert, p_xobs) to istep (xxpert).
  	p_xobs => istep_locate_(xobs,istep+1,nfrobs, &
		ldprt_.and.mype==0,myname//".xobs+",nymdi,nhmsi)

  ! get (date,time) at (istep).
  call tick(nymdi,nhmsi,-dt)

  call gsi_4dcoupler_model_ad(xxpert,p_xobs,nymdi,nhmsi,ndt,rc=ierr)
	if(ierr/=0) call die(myname,'gsi_4dcoupler_model_ad(), rc =',ierr)

#ifdef _LAG_MODEL_
!  Apply AD trajectory model (same time steps as obsbin)
  if(ntotal_orig_lag>0) then
	! When there is a lagmod to do , adjoint integrate from istep+1 back
	! to istep, using xxpert at time (istep)
      ii=istep	! step count for lagmod is off by 1.

      ! Execute AD model for each balloon (loop step insensitive)
      do jj=1,nlocal_orig_lag
         ad_tmp_locvect = lag_ad_vec(jj,ii+1,:)
         ! if (.not.idmodel) then
         call lag_rk2iter_ad(lag_tl_spec_i(jj,ii,:),lag_tl_spec_r(jj,ii,:),&
           &ad_tmp_locvect(1),ad_tmp_locvect(2),ad_tmp_locvect(3),&
           &lag_u_full(:,:,ii),lag_v_full(:,:,ii))
         print '(A,I3,A,F16.6,F16.6)',"ADiter: ",ii," location",lag_ad_vec(jj,ii,1),lag_ad_vec(jj,ii,2)
         ! end if
         lag_ad_vec(jj,ii,:)=lag_ad_vec(jj,ii,:)+ad_tmp_locvect
      end do

      ! Give the sensitivity back to the GCM
      call gsi_bundlegetpointer(xxpert,'u',xx_u,ierr)
      call gsi_bundlegetpointer(xxpert,'v',xx_v,ierr)
      call lag_ADscatter_stateuv(xx_u,xx_v,ii)

      ! To not add the contribution 2 times
      lag_u_full(:,:,ii)=zero; lag_v_full(:,:,ii)=zero;

  endif
#endif

  ! Locate (istep) in xobs, if any.  Then add adjoint increment to
  ! the current adjoint state (xxpert).
  	p_xobs => istep_locate_(xobs,istep,nfrobs, &
		ldprt_.and.mype==0,myname//".xobs-",nymdi,nhmsi)

  if(associated(p_xobs)) call self_add(xxpert,p_xobs)	! xxpert += p_xobs

  ! Locate (istep) in xini, if any.  Then store the current adjoint
  ! state (xxpert) to xini.
      if(iau_on_) then
  	p_xini => iau_locate_(xini,istep,nfrctl, &
		ldprt_.and.mype==0,myname//".xini-",nymdi,nhmsi)
      else
  	p_xini => istep_locate_(xini,istep,nfrctl, &
		ldprt_.and.mype==0,myname//".xini-",nymdi,nhmsi)
      endif

  if(associated(p_xini)) then
    if(iau_on_) then
      call gsi_bundleAddMul(p_xini,wt,xxpert)	! p_xini += wt*xxpert
    else
      call self_add(p_xini,xxpert)		! p_xini += xxpert
    endif
  endif

enddo

d0 = zero
do n=lbound(xini,1),ubound(xini,1)
  d0 = d0+dot_product(xini(n),xini(n))
enddo
if(ldprt_) print *, myname, ': total (gsi) dot product ', d0

! Finalize AD model, and destroy xxpert at the same time.
call gsi_4dcoupler_final_model_ad(xxpert,xobs(1),nymdi,nhmsi,rc=ierr)
	if(ierr/=0) call die(myname,'gsi_rccoupler_final_model_ad(), rc =',ierr)

! Finalize timer
call timer_fnl('model_ad')

return
contains

  function istep_locate_(x,istep,intvl, verbose,which,nymdi,nhmsi) result(p_)
  	!-- locate istep-th element in x, which is defined only at every intvl
	!-- isteps.  i.e., p_ => x(1), if istep=0;
	!--                      x(2), if istep=1*intvl; 
	!--                      x(3), if istep=2*intvl; etc.
	!--			 null, otherwise.

    use mpeu_util,only: tell,warn,stdout
    implicit none
    type(gsi_bundle),pointer:: p_
    type(gsi_bundle),target,dimension(:),intent(in):: x
    integer(i_kind),intent(in):: istep
    integer(i_kind),intent(in):: intvl	! istep interval of two x(:) elements

    logical         ,intent(in):: verbose	! if information is needed
    character(len=*),intent(in):: which		! for which this call is made.
    integer(i_kind) ,intent(in):: nymdi,nhmsi	! current clock time

    integer:: i,isize_,intvl_

    isize_= size(x)
    intvl_= max(1,intvl)

    p_   =>null()
    if (MOD(istep,intvl_)/=0) return

    i=istep/intvl_+1
    if (i<1.or.i>isize_) return

    if(verbose) write(stdout,'(1x,2a,i8.8,i7.6,2(i6,"/",i2.2))') which, &
		'() -- (nymd,nhms,istep/intvl,i/size) =',nymdi,nhmsi,istep,intvl_,i,isize_

    p_ => x(i)
  end function istep_locate_

  function iau_locate_(x,istep,intvl, verbose,which,nymdi,nhmsi) result(p_)
  	!-- locate IAU istep-th element in x, which is defined at corresponding
	!-- istep values.  i.e., p_=> null, if istep=0;
	!--                           x(1), if istep=0*intvl+1 .. 1*intvl;
	!--                           x(2), if istep=1*intvl+1 .. 2*intvl; etc.
	!--                           x(3), if istep=2*intvl+1 .. 3*intvl; etc.
	!--			      null, otherwise.

    use mpeu_util,only: tell,warn,stdout
    implicit none
    type(gsi_bundle),pointer:: p_
    type(gsi_bundle),target,dimension(:),intent(in):: x
    integer(i_kind),intent(in):: istep
    integer(i_kind),intent(in):: intvl	! istep interval of two x(:) elements

    logical         ,intent(in):: verbose	! if information is needed
    character(len=*),intent(in):: which		! for which this call is made.
    integer(i_kind) ,intent(in):: nymdi,nhmsi	! current clock time

    integer:: i,isize_,intvl_

    isize_= size(x)
    intvl_= max(1,intvl)

    i=(istep+intvl-1)/intvl_

    p_ => null()
    if (i<1.or.i>isize_) return

    if(verbose) write(stdout,'(1x,2a,i8.8,i7.6,2(i6,"/",i2.2))') which, &
		'() -- (nymd,nhms,istep/intvl,i/size) =',nymdi,nhmsi,istep,intvl_,i,isize_

    p_ => x(i)
  end function iau_locate_
end subroutine model_ad
