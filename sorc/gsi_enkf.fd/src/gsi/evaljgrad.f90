subroutine evaljgrad(xhat,fjcost,gradx,lupdfgs,nprt,calledby)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    evaljgrad
!   prgmmr: tremolet
!
! abstract: Evaluate cost function and its gradient at point xhat.
!
! program history log:
!   2007-04-27  tremolet - initial code
!   2007-07-09  tremolet - observation sensitivity
!   2007-10-18  tremolet - Jc DFI
!   2008-12-04  todling - update interface to intjo
!   2009-01-18  todling - calc dot-prods in quad precision
!                       - add diagnostic call when using strong constraint
!   2009-08-14  lueken  - update documentation
!   2009-11-20  todling - add geos_pgcmtest
!   2010-01-11  todling - bypass call to model_xx based on idmodel as well
!   2010-05-13  todling - update interface to evalqlim; use gsi_bundle
!   2010-05-27  todling - replace geos_pgcmtest w/ general gsi_4dcoupler
!   2010-08-19  lueken  - add only to module use
!   2010-10-13  jing    - moved idmodel handling to the pertmod implementation
!   2013-05-18  todling - evaljcdfi placed in intjcmod w/ name intjcdfi
!   2014-01-30  todling - adding components to enable ens-hyb option
!   2014-02-07  todling - update bias when doing 4dvar
!   2014-10-14  todling - write-all only called at last outer iteration
!   2015-09-03  guo     - obsmod::yobs has been replaced with m_obsHeadBundle,
!                         where yobs is created and destroyed when and where it
!                         is needed.
!   2018-08-10  guo     - replace intjo() related implementations with a new
!                         polymoprhic implementation of intjomod::intjo().
!
!   input argument list:
!    xhat - current state estimate (in control space)
!    nprt - print level
!    lupdfgs
!    calledby
!
!   output argument list:
!    fjcost - value of cost function
!    gradx  - gradient (in control space)
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use gsi_4dvar, only: nobs_bins, nsubwin, l4dvar, ltlint, iwrtinc
use constants, only: zero,zero_quad
use mpimod, only: mype
use jfunc, only: xhatsave
use jfunc, only: nrclen,nsclen,npclen,ntclen
use jfunc, only: jiter,miter
use jcmod, only: ljcdfi
use gridmod, only: twodvar_regional
use hybrid_ensemble_parameters, only: l_hyb_ens,ntlevs_ens
use obsmod, only: lsaveobsens, l_do_adjoint
use obs_sensitivity, only: fcsens
use mod_strong, only: l_tlnmc,baldiag_inc
use control_vectors, only: control_vector,prt_control_norms,dot_product,assignment(=)
use state_vectors, only: allocate_state,deallocate_state,prt_state_norms
use bias_predictors, only: predictors,allocate_preds,deallocate_preds,assignment(=)
use bias_predictors, only: update_bias_preds
use intjomod, only: intjo
use intjcmod, only: intjcdfi
use gsi_4dcouplermod, only: gsi_4dcoupler_grtests
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundleCreate
use gsi_bundlemod, only: gsi_bundleDestroy
use gsi_bundlemod, only: self_add,assignment(=)
use xhat_vordivmod, only : xhat_vordiv_init, xhat_vordiv_calc, xhat_vordiv_clean
use mpeu_util, only: die
use mpl_allreducemod, only: mpl_allreduce
use intradmod, only: setrad

implicit none

! Declare passed variables
type(control_vector), intent(in   ) :: xhat
real(r_quad)        , intent(  out) :: fjcost
type(control_vector), intent(inout) :: gradx
logical             , intent(in   ) :: lupdfgs
integer(i_kind)     , intent(in   ) :: nprt
character(len=*)    , intent(in   ) :: calledby

! Declare local variables  
character(len=*), parameter :: myname='evaljgrad'
type(gsi_bundle) :: sval(nobs_bins), rval(nobs_bins)
type(gsi_bundle) :: mval(nsubwin)
type(gsi_bundle) :: eval(ntlevs_ens)
type(gsi_bundle),dimension(nobs_bins) :: adtest_sval, adtest_rval
type(gsi_bundle),dimension(nsubwin  ) :: adtest_mval
type(predictors) :: sbias, rbias
real(r_quad) :: zjb,zjo,zjc,zjl
integer(i_kind) :: ii,iobs,ibin,i
logical :: llprt,llouter
logical,parameter:: pertmod_adtest=.true.
character(len=255) :: seqcalls
real(r_quad),dimension(max(1,nrclen)) :: qpred


!**********************************************************************

llprt=(mype==0.and.nprt>=2)
llouter=.false.
seqcalls = trim(calledby)//'::'//trim(myname)

! Allocate local variables
do ii=1,nobs_bins
   call allocate_state(sval(ii))
   call allocate_state(rval(ii))
end do
do ii=1,nsubwin
   call allocate_state(mval(ii))
end do
if(l_hyb_ens) then
   do ii=1,ntlevs_ens
      call allocate_state(eval(ii))
   end do
endif
call allocate_preds(sbias)
call allocate_preds(rbias)

! Contribution from background term
gradx = xhat

if (lsaveobsens) then
! Observation sensitivity right hand side
   do ii=1,gradx%lencv
      gradx%values(ii) = gradx%values(ii) - fcsens%values(ii)
   enddo
else
! Contribution from previous background term
   do ii=1,gradx%lencv
      gradx%values(ii) = gradx%values(ii) + xhatsave%values(ii)
   enddo
endif

zjb=dot_product(gradx,gradx,r_quad)

! Convert from control space to model space
call control2model(xhat,mval,sbias)

if (nprt>=2) then
   do ii=1,nsubwin
      call prt_state_norms(mval(ii),'mval')
   enddo
endif

! Run TL model to fill sval
if (l4dvar) then
   if (l_hyb_ens) then
       call ensctl2model(xhat,mval(1),eval)
       mval(1)=eval(1)
   end if

   if(l_do_adjoint.and.pertmod_adtest) &
         call adtest_copy_(mval,adtest_mval)

   call model_tl(mval,sval,llprt)

   if(l_do_adjoint.and.pertmod_adtest) &
         call adtest_copy_(sval,adtest_sval)

else
   if (l_hyb_ens) then
       call ensctl2model(xhat,mval(1),eval)
       do ii=1,nobs_bins
          sval(ii)=eval(ii)
       enddo
   else
       do ii=1,nobs_bins
          sval(ii)=mval(1)
       enddo
   end if
end if

! Perform test of AGCM TLM and ADM
call gsi_4dcoupler_grtests(mval,sval,nsubwin,nobs_bins)

if (nprt>=2) then
   do ii=1,nobs_bins
      call prt_state_norms(sval(ii),'sval')
   enddo
endif

! Zero gradient
do ii=1,nobs_bins
   rval(ii)=zero
end do
rbias=zero
do ii=1,nsubwin
   mval(ii)=zero
end do

qpred=zero_quad
call setrad(sval(1))

! Compare obs to solution and transpose back to grid (H^T R^{-1} H)
call intjo(rval,qpred,sval,sbias)

! Take care of background error for bias correction terms

call mpl_allreduce(nrclen,qpvals=qpred)

do i=1,nsclen
  rbias%predr(i)=rbias%predr(i)+qpred(i)
end do
do i=1,npclen
   rbias%predp(i)=rbias%predp(i)+qpred(nsclen+i)
end do
if (ntclen>0) then
   do i=1,ntclen
      rbias%predt(i)=rbias%predt(i)+qpred(nsclen+npclen+i)
   end do
end if

! Evaluate Jo
call evaljo(zjo,iobs,nprt,llouter)

if (l_do_adjoint) then
! Moisture constraint
   zjl=zero_quad
   if (.not.ltlint) then
      do ibin=1,nobs_bins
         call evalqlim(sval(ibin),zjl,rval(ibin))
      enddo
   endif

   if (ljcdfi) then
      call intjcdfi(rval,sval,pjc=zjc)
   else
! Jc and other 3D-Var terms
! Don't know how to deal with Jc term so comment for now...
!     call eval3dvar(sval,zjc,rval,zdummy)
      zjc=zero_quad
   endif

   if (nprt>=2) then
      do ii=1,nobs_bins
         call prt_state_norms(rval(ii),'rval')
      enddo
   endif

!  Run adjoint model
   if (l4dvar) then
      if(l_do_adjoint.and.pertmod_adtest) &
              call adtest_copy_(rval,adtest_rval)

      call model_ad(mval,rval,llprt)

      if(l_do_adjoint.and.pertmod_adtest) then
        call adtest_show_(adtest_mval,adtest_sval,adtest_rval,mval)
        call adtest_dstr_(adtest_mval)
        call adtest_dstr_(adtest_sval)
        call adtest_dstr_(adtest_rval)
      endif

      if (l_hyb_ens) then
          eval(1)=mval(1)
          call ensctl2model_ad(eval,mval(1),gradx)
      end if

   else

      if (l_hyb_ens) then
          do ii=1,nobs_bins
              eval(ii)=rval(ii)
          enddo
          call ensctl2model_ad(eval,mval(1),gradx)
      else
         mval(1)=rval(1)
         do ii=2,nobs_bins
            call self_add(mval(1),rval(ii))
         enddo
      end if
   end if

   if (nprt>=2) then
      do ii=1,nsubwin
         call prt_state_norms(mval(ii),'mval')
      enddo
   endif

!  Adjoint of convert control var to physical space
   call control2model_ad(mval,rbias,gradx)

!  Cost function
   fjcost=zjb+zjo+zjc+zjl

!  Print diagnostics
   if (nprt>=2) call prt_control_norms(gradx,'gradx')
   if (nprt>=1.and.mype==0) write(6,999)trim(seqcalls),': grepcost J,Jb,Jo,Jc,Jl=',&
                                      fjcost,zjb,zjo,zjc,zjl
endif

! Produce diagnostic when applying strong constraint
if (lupdfgs.and.l_tlnmc.and.baldiag_inc) call strong_baldiag_inc(sval,size(sval))

! Save increment (update guess)
if (lupdfgs) then
   call xhat_vordiv_init
   call xhat_vordiv_calc(sval)
   if (iwrtinc>0) then
      if (nprt>=1.and.mype==0) write(6,*)trim(seqcalls),': evaljgrad: Setting increment for output'
      call inc2guess(sval)
      call view_st (sval,'xinc')
      call write_all(iwrtinc)
      ! NOTE: presently in 4dvar, we handle the biases in a slightly inconsistent when
      ! as when in 3dvar - that is, the state is not updated, but the biases are.
      ! This assumes GSI handles a single iteration of the outer loop at a time
      ! when doing 4dvar (that is, multiple iterations require stop-and-go).
      call update_bias_preds(twodvar_regional,sbias)
   else
      if (nprt>=1.and.mype==0) write(6,*)trim(seqcalls),': evaljgrad: Updating guess'
      call update_guess(sval,sbias)
      if(jiter == miter)call write_all(-1)
   endif
   call xhat_vordiv_clean
endif

! Release memory
call deallocate_preds(rbias)
call deallocate_preds(sbias)
if(l_hyb_ens) then
   do ii=1,ntlevs_ens
      call deallocate_state(eval(ii))
   end do
endif
do ii=1,nsubwin
   call deallocate_state(mval(ii))
end do
do ii=1,nobs_bins
   call deallocate_state(rval(ii))
   call deallocate_state(sval(ii))
end do

999 format(2A,5(1X,ES25.18))

return
contains

subroutine adtest_copy_(vi,vo)
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundleCreate
  use gsi_bundlemod, only: assignment(=)
  use kinds, only: i_kind
  use mpeu_util, only: perr,die
  implicit none
  type(gsi_bundle),dimension(:),intent(in ):: vi
  type(gsi_bundle),dimension(:),intent(out):: vo

  integer(i_kind):: iv,ierr
  character(len=*),parameter:: myname_=myname//".adtest_copy_"

  if( size(vi)/=size(vo) ) then
    call perr(myname_,'size(vi)/=size(vo)')
    call perr(myname_,'size(vo) =',size(vo))
    call perr(myname_,'size(vi) =',size(vi))
    do iv=1,size(vi)
     call perr(myname_,'name(vi) = "'//trim(vi(iv)%name)//'", iv =',iv)
    enddo
    call die(myname_)
  endif
  
  do iv=1,size(vi)
    call gsi_bundleCreate(vo(iv),vi(iv),"adtest_"//trim(vi(iv)%name),istatus=ierr)
    if(ierr/=0) then
       call perr(myname_,'gsi_bundleCreate("adtest_'//trim(vi(iv)%name)//'"), istatus =',ierr)
       call perr(myname_,'                ("adtest_'//trim(vi(iv)%name)//'"),      iv =',iv  )
       call die(myname_)
    endif
    vo(iv)=vi(iv)
  enddo
end subroutine adtest_copy_

subroutine adtest_dstr_(v)
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundleDestroy
  use kinds, only: i_kind
  use mpeu_util, only: perr,die
  implicit none
  type(gsi_bundle),dimension(:),intent(inout):: v

  integer(i_kind):: iv,ierr
  character(len=*),parameter:: myname_=myname//".adtest_dstr_"

  do iv=1,size(v)
    call gsi_bundleDestroy(v(iv),istatus=ierr)
     if(ierr/=0) then
        call perr(myname_,'gsi_bundleDestroy("adtest_'//trim(v(iv)%name)//'"), istatus =',ierr)
        call perr(myname_,'                 ("adtest_'//trim(v(iv)%name)//'"),      iv =',iv  )
        call die(myname_)
     endif
  enddo
end subroutine adtest_dstr_

subroutine adtest_show_(x,p,q,y)
  use gsi_bundlemod, only: gsi_bundle
  use state_vectors, only: dot_product
  use kinds, only: i_kind,r_kind,r_quad
  use mpeu_util, only: stdout,perr,die
  use mpimod   , only: mype
  implicit none
  type(gsi_bundle),dimension(:),intent(in):: x, p     ! some x, and p=Mx
  type(gsi_bundle),dimension(:),intent(in):: q, y     ! some q, and y=M'q

  character(len=*),parameter:: myname_=myname//".adtest_show_"
  real(r_quad):: dpp,dqq,dpq,cpq,rpq
  real(r_quad):: dxx,dyy,dxy,adjcrit
  integer(i_kind):: ipq
  logical:: IamRoot_
  integer(i_kind):: iv

  IamRoot_ = mype==0
  adjcrit=1.e-10_r_quad

  if(IamROOT_)then
    if( size(x)/=size(y) .or. &
        size(p)/=size(q) ) then
      call perr(myname_,'mismatched vector counts')
      if(size(x)/=size(y)) then
        call perr(myname_,'size(x)/=size(y)')
        call perr(myname_,'size(x) =',size(x))
        call perr(myname_,'size(y) =',size(y))
      endif
      if(size(p)/=size(q)) then
        call perr(myname_,'size(p)/=size(q)')
        call perr(myname_,'size(p) =',size(p))
        call perr(myname_,'size(q) =',size(q))
      endif
      call die(myname_)
    endif
  endif

  dpq=0._r_quad
  do iv=1,size(p)
    dpq=dpq+dot_product(p(iv),q(iv))          ! (p,q)
  enddo
  
  dxy=0._r_quad
  do iv=1,size(x)
    dxy=dxy+dot_product(x(iv),y(iv))       ! (x,y)
  enddo

  cpq=1._r_quad
  if(abs(dxy)>0._r_quad) cpq=dpq/dxy
  rpq=cpq-1._r_quad
  ipq=int(-log(abs(rpq)+tiny(rpq))/log(10._r_quad))

  if(abs(rpq) > adjcrit)then
    dpp=0._r_quad
    dqq=0._r_quad
    do iv=1,size(p)
      dpp=dpp+dot_product(p(iv),p(iv))       ! (p,p)
      dqq=dqq+dot_product(q(iv),q(iv))       ! (q,q)
    enddo

    dyy=0._r_quad
    dxx=0._r_quad
    do iv=1,size(x)
      dyy=dyy+dot_product(y(iv),y(iv))          ! (y,y)
      dxx=dxx+dot_product(x(iv),x(iv))          ! (x,x)
    enddo

    if(IamROOT_) then

      write(stdout,'(1x,2a,i2,1x,1p,3e12.5)') trim(myname_), &
            " -- n,   (    q,p=Mx), qq, pp =" ,size(p),dpq,dqq,dpp
      write(stdout,'(1x,2a,i2,1x,1p,3e12.5)') trim(myname_), &
            " -- m,   (y=M'q,   x), yy, xx =",size(x),dxy,dyy,dxx
      write(stdout,'(1x,2a,2x,1x,1p,3e12.5,2x,i4)') trim(myname_), &
            "() -- pq-xy, pq/xy,pq/xy-1,#d =",dpq-dxy,cpq,rpq,ipq," adtest failed"
    end if
  else
    if(IamROOT_)then
      write(stdout,'(1x,2a,2x,1x,1p,3e12.5,2x,i4,a)') trim(myname_), &
            "() -- pq-xy, pq/xy,pq/xy-1,#d =",dpq-dxy,cpq,rpq,ipq," adtest success"
    end if
  end if
  return
end subroutine adtest_show_
end subroutine evaljgrad
