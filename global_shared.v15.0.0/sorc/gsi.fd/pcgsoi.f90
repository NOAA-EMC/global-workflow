module pcgsoimod

!$$$ module documentation block
!           .      .    .                                       .
! module:   pcgsoimod    module for pcgsoi and its tangent linear pcgsoi_tl
!  prgmmr:
!   
! abstract: module for pcgsoi and its tangent linear pcgsoi_tl
!
! program history log:
!   2005-05-17  Yanqiu zhu - wrap pcgsoi and its tangent linear pcgsoi_tl into one module
!   2005-11-21  Derber - remove interface
!   2008-11-26  Todling - remove pcgsoi_tl
!   2009-08-12  lueken  - update documentation
!   2009-09-17  parrish - add bkerror_a_en and anbkerror_reg_a_en for hybrid ensemble control variable a_en
!   2014-12-03  derber - thread dot products and modify so obsdiag can be turned
!               off
!
! subroutines included:
!   sub pcgsoi
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC pcgsoi

contains

subroutine pcgsoi()

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcgsoi      solve inner loop of analysis equation
!   prgmmr: parrish          org: np22                date: 1991-04-02
!
! abstract: solve inner loop of analysis equation. at end update outer
!           loop variables
!
! program history log:
!   1991-04-02  parrish, d., derber,j.
!   1998-01-16  derber,j.
!   1998-07-10  yang w., mpp version
!   1999-06-28  yang w., second structure mpp version
!   1999-07-30  wu, update handling of ozone data
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1999-12-07  wu               grid version
!   2003-12-18  derber, j. modify minimization procedures
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  kleist - separate control vector for u,v
!   2004-10-15  parrish - add nonlinear qc option
!   2005-03-09  parrish - add optional call to regional anisotropic
!                         background error
!   2005-03-28  treadon - combine hopers.f90 and update_ggrid.f90 in update_guess.f90
!   2005-04-11  treadon - add logical flag for nonlinear qc, remove calls
!                         to intall_qc and stpcalc_qc
!   2005-05-02  treadon - add call getuv for restart option
!   2005-05-17  yanqiu zhu - add 'use intallmod', 'use stpcalcmod' and 'use dprodxmod' 
!   2005-05-27  derber  - changes to minimization and output
!   2005-06-03  parrish - add horizontal derivatives
!   2005-09-29  kleist,parrish - include _t (time derivatives) array
!   2006-04-06  treadon - move bias cor. and tskin update into update_guess
!   2006-04-16  derber - change call to stpcalc - move stepsize print to stpcalc
!   2006-04-21  kleist - add calls to update Jc terms
!   2006-05-26  derber - modify to improve convergence testing
!   2006-07-28  derber - remove calls to makeobs
!   2006-08-04  parrish - add changes for strong constraint option
!   2007-03-09  su      - add option for observation perturbation
!   2007-04-13  tremolet - use control vectors and state vectors                         
!   2007-05-15  todling - add AGCM/TLM/ADM tester; fix one-ob case NaN's
!   2007-07-05  todling - allow 4dvar to write out increment
!   2007-09-30  todling - add timer
!   2008-03-24  wu      - oberror tuning
!   2008-11-03  sato - enables to use global anisotropic mode
!   2008-12-01  todling - add init_/clean_ to allow clean way out from obs-error tuning
!                       - updated interface to penal
!   2009-01-28  todling - move write_all from glbsoi to here (consistent w/ 4dvar mods)
!   2009-02-06  pondeca - add option to re-biorthogonalize the gradx and and grady vectors.
!                         hardwired to work for 2dvar only
!   2009-09-17  parrish - add bkerror_a_en and anbkerror_reg_a_en for hybrid ensemble
!                         control variable a_en
!   2009-10-12  parrish - add beta12mult for scaling by hybrid blending parameters beta1inv, beta2inv
!                           called only when l_hyb_ens=.true.
!   2010-04-25  zhu     - add precond calls for new preconditioning of predictors
!   2010-05-05  derber - omp commands removed
!   2010-05-13  todling - update interface to update_geswtend; update to gsi_bundle for state vector
!                       - declare all use explicitly
!   2010-05-28  Hu      - add call for cloud analysis driver : gsdcloudanalysis
!   2010-09-24  todling - must turn off variational qc when ltlint=.t.
!   2011-04-25  eL akkraoui - add option for re-orthogonalization.
!   2011-07-10  todling - minor fixes for general precision handling. 
!   2011-11-17  kleist - add handling for separate state vector for ensemble bits (hybrid ens/var)
!   2013-01-26  parrish - WCOSS debug compile flags type mismatch for calls to ensctl2state_ad
!                          and ensctl2state.  I put in temporary fix to allow debug compile
!                          by replacing mval with mval(1).  This is likely not
!                          correct for multiple obs bins.
!   2014-12-22  Hu      -  add option i_gsdcldanal_type to control cloud analysis  
!   2016-03-02  s.liu/carley  - remove use_reflectivity and use i_gsdcldanal_type 
!                       
!
! input argument list:
!
!
! output argument list:      
!      none
!
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_double,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter,c_varqc,vqc
  use obsmod, only: destroyobs,oberror_tune,luse_obsdiag,yobs
  use jfunc, only: iter,jiter,jiterstart,niter,miter,iout_iter,&
       nclen,penorig,gnormorig,xhatsave,yhatsave,&
       iguess,read_guess_solution,diag_precon,step_start, &
       niter_no_qc,l_foto,xhat_dt,print_diag_pcg,lgschmidt
  use gsi_4dvar, only: nobs_bins, nsubwin, l4dvar, iwrtinc, ladtest, &
                       ltlint, iorthomax
  use gridmod, only: twodvar_regional
  use constants, only: zero,one,five,tiny_r_kind
  use anberror, only: anisotropic
  use mpimod, only: mype
  use mpl_allreducemod, only: mpl_allreduce
  use intallmod, only: intall
  use stpcalcmod, only: stpcalc
  use mod_strong, only: l_tlnmc,baldiag_inc
  use adjtest, only : adtest
  use control_vectors, only: control_vector, allocate_cv, deallocate_cv,&
       prt_control_norms,dot_product,qdot_prod_sub,assignment(=)
  use state_vectors, only : allocate_state,deallocate_state,&
       prt_state_norms,inquire_state
  use bias_predictors, only: allocate_preds,deallocate_preds,predictors,assignment(=)
  use bias_predictors, only: update_bias_preds
  use xhat_vordivmod, only : xhat_vordiv_init, xhat_vordiv_calc, xhat_vordiv_clean
  use timermod, only: timer_ini,timer_fnl
  use projmethod_support, only: init_mgram_schmidt, &
                                mgram_schmidt,destroy_mgram_schmidt
  use hybrid_ensemble_parameters,only : l_hyb_ens,aniso_a_en,ntlevs_ens
  use hybrid_ensemble_isotropic, only: beta12mult,bkerror_a_en
  use gsi_bundlemod, only : gsi_bundle
  use gsi_bundlemod, only : self_add,assignment(=)
  use gsi_bundlemod, only : gsi_bundleprint
  use gsi_4dcouplermod, only : gsi_4dcoupler_grtests
    use rapidrefresh_cldsurf_mod, only: i_gsdcldanal_type

  implicit none

! Declare passed variables

! Declare local parameters

! Declare local variables  
  logical iout_6,restart,end_iter,llprt,llouter
  character(5) step(2)
  integer(i_kind) i,istep,iobs,ii,nprt
  real(r_kind) stp,b,converge
  real(r_kind) gsave,small_step
  real(r_kind) gnormx,penx,penalty,penaltynew
  real(r_double) pennorm
  real(r_quad) zjo
  real(r_quad),dimension(3):: dprod
  real(r_kind),dimension(2):: gnorm
  real(r_kind) :: zgini,zfini,fjcost(4),fjcostnew(4),zgend,zfend
  real(r_kind) :: fjcost_e
  type(control_vector) :: xhat,gradx,grady,dirx,diry,dirw,ydiff,xdiff
  type(gsi_bundle) :: sval(nobs_bins), rval(nobs_bins)
  type(gsi_bundle) :: eval(ntlevs_ens)
  type(gsi_bundle) :: mval(nsubwin)
  type(predictors) :: sbias, rbias
  logical:: lanlerr
  
  type(control_vector), allocatable, dimension(:) :: cglwork
  type(control_vector), allocatable, dimension(:) :: cglworkhat
  integer(i_kind) :: iortho
  real(r_quad) :: zdla
!    note that xhatt,dirxt,xhatp,dirxp are added to carry corrected grid fields
!      of t and p from implicit normal mode initialization (strong constraint option)
!     inmi generates a linear correction to t,u,v,p.  already have xhatuv which can
!      be used for the corrected wind, but nothing for t,p.  xhatt, etc are used exactly
!       like xhatuv,dirxuv.

! Step size diagnostic strings
  data step /'good', 'SMALL'/

!**********************************************************************

! Initialize timer
  call timer_ini('pcgsoi')

  if (ladtest) call adtest()

! Set constants.  Initialize variables.
  restart=.false.
  if (jiter==0 .and. (iguess==1 .or. iguess==2)) restart=.true.
  pennorm=10.e50_r_double
  iout_6=.true.
  if (iout_iter==6) iout_6=.false.
  stp=step_start
  if(diag_precon)stp=one
  small_step=1.e-2_r_kind*stp
  end_iter=.false.
  gsave=zero
  llouter=.false.
  
  if(iorthomax>0) then 
     allocate(cglwork(iorthomax+1))
     DO ii=1,iorthomax+1
        CALL allocate_cv(cglwork(ii))
        cglwork(ii)=zero
     ENDDO
     allocate(cglworkhat(iorthomax+1))
     DO ii=1,iorthomax+1
        CALL allocate_cv(cglworkhat(ii))
        cglworkhat(ii)=zero
     END DO
  end if

! Convergence criterion needs to be relaxed a bit for anisotropic mode,
! because the anisotropic recursive filter, for reasons of computational
! efficiency, uses r_single (4 byte) arithmetic.  this generally triggers
! the warning about penalty increasing, but this doesn't happen until
! the gradient has been reduced by more than 9 orders of magnitude.
  converge=1.e-10_r_kind
  if(anisotropic) converge=1.e-9_r_kind

! Allocate required memory and initialize fields
  call init_
  if(print_diag_pcg)call prt_guess('guess')

  lanlerr=.false.
  if ( twodvar_regional .and. jiter==1 ) lanlerr=.true.
  if ( lanlerr .and. lgschmidt ) call init_mgram_schmidt
  nlnqc_iter=.false.
  call stpjo_setup(yobs,nobs_bins)

! Perform inner iteration
  inner_iteration: do iter=0,niter(jiter)

! Gradually turn on variational qc to avoid possible convergence problems
     if(vqc) then
        nlnqc_iter = iter >= niter_no_qc(jiter)
        if(jiter == jiterstart) then
           varqc_iter=c_varqc*(iter-niter_no_qc(1)+one)
           if(varqc_iter >=one) varqc_iter= one
        else
           varqc_iter=one
        endif
     end if

     do ii=1,nobs_bins
        rval(ii)=zero
     end do
     gradx=zero
     llprt=(mype==0).and.(iter<=1)

!    Convert from control space directly to physical
!    space for comparison with obs.
     call control2state(xhat,mval,sbias)
     if (l4dvar) then
        if (l_hyb_ens) then
           call ensctl2state(xhat,mval(1),eval)
           mval(1)=eval(1)
        end if

!       Perform test of AGCM TLM and ADM
        call gsi_4dcoupler_grtests(mval,sval,nsubwin,nobs_bins)

!       Run TL model to fill sval
        call model_tl(mval,sval,llprt)
     else
        if (l_hyb_ens) then
           call ensctl2state(xhat,mval(1),eval)
           do ii=1,nobs_bins
              sval(ii)=eval(ii)
           end do
        else
           do ii=1,nobs_bins
              sval(ii)=mval(1)
           end do
        end if
     end if

     if (iter<=1 .and. print_diag_pcg) then
        do ii=1,nobs_bins
           call prt_state_norms(sval(ii),'sval')
        enddo
     end if

!    Compare obs to solution and transpose back to grid
     call intall(sval,sbias,rval,rbias)

     if (iter<=1 .and. print_diag_pcg) then
        do ii=1,nobs_bins
           call prt_state_norms(rval(ii),'rval')
        enddo
     endif

     if (l4dvar) then
!       Run adjoint model
        call model_ad(mval,rval,llprt)

        if (l_hyb_ens) then
           eval(1)=mval(1)
           call ensctl2state_ad(eval,mval(1),gradx)
        end if
     else

!       Convert to control space directly from physical space.
        if (l_hyb_ens) then
           do ii=1,nobs_bins
              eval(ii)=rval(ii)
           end do
           call ensctl2state_ad(eval,mval(1),gradx)
        else
           mval(1)=rval(1)
           if (nobs_bins > 1 ) then
              do ii=2,nobs_bins
                 call self_add(mval(1),rval(ii))
              enddo
           end if
        end if

     end if
!    Adjoint of convert control var to physical space
     call control2state_ad(mval,rbias,gradx)

!    Print initial Jo table
     if (iter==0 .and. print_diag_pcg .and. luse_obsdiag) then
        nprt=2
        call evaljo(zjo,iobs,nprt,llouter)
        call prt_control_norms(gradx,'gradx')
     endif

!    Add contribution from background term
!    grady is being used as a temporary array
     do i=1,nclen
        gradx%values(i)=gradx%values(i)+yhatsave%values(i)
     end do

!    Multiply by background error
     if(anisotropic) then
        call anbkerror(gradx,grady)
        if(lanlerr .and. lgschmidt) call mgram_schmidt(gradx,grady)
     else
        call bkerror(gradx,grady)
     end if

!    first re-orthonormalization
     if(iorthomax>0) then 
        iortho=min(iorthomax,iter) 
        if(iter .ne. 0) then 
           do ii=iortho,1,-1
              zdla = DOT_PRODUCT(gradx,cglworkhat(ii))
              do i=1,nclen
                 gradx%values(i) = gradx%values(i) - zdla*cglwork(ii)%values(i)
              end do
           end do
        end if
     end if

!    If hybrid ensemble run, then multiply ensemble control variable a_en 
!                                    by its localization correlation
     if(l_hyb_ens) then
        if(aniso_a_en) then
    !      call anbkerror_a_en(gradx,grady)    !  not available yet
           write(6,*)' ANBKERROR_A_EN not written yet, program stops'
           stop
        else
           call bkerror_a_en(gradx,grady)
        end if

!       multiply static (Jb) part of grady by betas_inv(:), and
!       multiply ensemble (Je) part of grady by betae_inv(:) [default : betae_inv(:) = 1 - betas_inv(:)] 
!         (this determines relative contributions from static background Jb and ensemble background Je)

        call beta12mult(grady)

     end if

!    second re-orthonormalization
     if(iorthomax>0) then
        if(iter .ne. 0) then 
           do ii=iortho,1,-1
              zdla = DOT_PRODUCT(grady,cglwork(ii))
              do i=1,nclen
                 grady%values(i) = grady%values(i) - zdla*cglworkhat(ii)%values(i)
              end do
           end do
        end if
!       save gradients
        if (iter <= iortho) then
           zdla = sqrt(dot_product(gradx,grady,r_quad))
           do i=1,nclen
              cglwork(iter+1)%values(i)=gradx%values(i)/zdla
              cglworkhat(iter+1)%values(i)=grady%values(i)/zdla
           end do
        end if
     end if

!    Add potential additional preconditioner 
     if(diag_precon)call precond(grady)

     if (lanlerr) then
        do i=1,nclen
           xdiff%values(i)=gradx%values(i)
           ydiff%values(i)=grady%values(i)
        end do
     else
        do i=1,nclen
           xdiff%values(i)=gradx%values(i)-xdiff%values(i)
           ydiff%values(i)=grady%values(i)-ydiff%values(i)
        end do
     end if

     if (iter==0 .and. print_diag_pcg) then
        call prt_control_norms(grady,'grady')
     endif

!    Calculate new norm of gradients
     if (iter>0) gsave=gnorm(1)
     dprod(1) = qdot_prod_sub(gradx,grady)
     dprod(2) = qdot_prod_sub(xdiff,grady)
     dprod(3) = qdot_prod_sub(ydiff,gradx)
     call mpl_allreduce(3,qpvals=dprod)

     gnorm(1)=dprod(1)
!    Two dot products in gnorm(2) should be same, but are slightly different due to round off
!    so use average.
     gnorm(2)=0.5_r_quad*(dprod(2)+dprod(3))
     b=zero
     if (gsave>1.e-16_r_kind .and. iter>0) b=gnorm(2)/gsave

     if(mype == 0)write(iout_iter,*)'Minimization iteration',iter
     if (b<zero .or. b>five) then
        if (mype==0) then
           if (iout_6) write(6,105) gnorm(2),gsave,b
           write(iout_iter,105) gnorm(2),gsave,b
        endif
        b=zero
     endif
     if (mype==0) write(6,888)'pcgsoi: gnorm(1:2),b=',gnorm,b

!    Calculate new search direction
     if (.not. restart) then
        if(diag_precon)then
          do i=1,nclen
             diry%values(i)=dirw%values(i)
          end do
        end if
        do i=1,nclen
           xdiff%values(i)=gradx%values(i)
           ydiff%values(i)=grady%values(i)
           dirx%values(i)=-grady%values(i)+b*dirx%values(i)
           diry%values(i)=-gradx%values(i)+b*diry%values(i)
        end do
        if(diag_precon)then
          do i=1,nclen
             dirw%values(i)=diry%values(i)
          end do
          call precond(diry)
        end if
     else
!    If previous solution available, transfer into local arrays.
        xdiff=zero
        ydiff=zero
        call read_guess_solution(dirx,diry,mype)
        stp=one
     endif

!    Convert search direction form control space to physical space
     call control2state(dirx,mval,rbias)
     if (l4dvar) then
        if (l_hyb_ens) then
           call ensctl2state(dirx,mval(1),eval)
           mval(1)=eval(1)
        end if

!       Run TL model to fill rval
!       The TLM being linear, rval could be updated in the same way as the
!       control variable. That would eliminate the need for running the TLM
!       at the cost of storing an additional 4D state variable.
        call model_tl(mval,rval,llprt)
     else

        if (l_hyb_ens) then
           call ensctl2state(dirx,mval(1),eval)
           do ii=1,nobs_bins
              rval(ii)=eval(ii)
           end do
        else
           do ii=1,nobs_bins
              rval(ii)=mval(1)
           end do
        end if

     end if

!    Calculate stepsize
     call stpcalc(stp,sval,sbias,xhat,dirx,rval,rbias, &
                  diry,penalty,penaltynew,fjcost,fjcostnew,end_iter)

     if (lanlerr) call writeout_gradients(gradx,grady,niter(jiter),stp,b,mype)

!    Diagnostic calculations
     if (iter==0) then
        zgini=gnorm(1)
        zfini=penalty
        if (mype==0) then
           write(6,888)'Initial cost function =',zfini
           write(6,888)'Initial gradient norm =',sqrt(zgini)
        endif
        if(jiter==jiterstart .or. oberror_tune) then
           gnormorig=gnorm(1)
           penorig=penalty
        end if
     endif

     gnormx=gnorm(1)/gnormorig
     penx=penalty/penorig

     if (mype==0) then
        write(iout_iter,888)'pcgsoi: gnorm(1:2)',gnorm
        write(iout_iter,999)'costterms Jb,Jo,Jc,Jl  =',jiter,iter,fjcost
        istep=1
        if (stp<small_step) istep=2
        write(6,9992)'cost,grad,step,b,step? =',jiter,iter,penalty,sqrt(gnorm(1)),stp,b,step(istep)
        write(iout_iter,9992)'cost,grad,step,b,step? =',jiter,iter,penalty,sqrt(gnorm(1)),stp,b,step(istep)
        if (zgini>tiny_r_kind .and. zfini>tiny_r_kind) then
           write(iout_iter,9993) 'estimated penalty reduction this iteration',&
                 jiter,iter,(penalty-penaltynew),(penalty-penaltynew)/penorig,'%'
           write(iout_iter,999)'penalty and grad reduction WRT outer and initial iter=', &
               jiter,iter,penalty/zfini,sqrt(gnorm(1)/zgini),penx,gnormx
        else
           write(iout_iter,999)'grad and penalty reduction WRT outer and initial iter=N/A'
        endif

     endif
999  format(A,2(1X,I3),6(1X,ES25.18))
9992 format(A,2(1X,I3),4(1X,ES25.18),1x,A6)
9993 format(A,2(1X,I3),2(1X,ES25.18),A1)

!    Check for convergence or failure of algorithm
     if(gnormx < converge .or. penalty < converge  .or.  &
        penx >= pennorm .or. end_iter)then

        if(mype == 0)then
           if(iout_6) write(6,101)
           write(iout_iter,101)

           if(gnormx < converge) then
              if(iout_6)write(6,130)gnormx,converge
              write(iout_iter,130) gnormx,converge
           end if
           if(penalty < converge) then
              if(iout_6)write(6,131)penalty,converge
              write(iout_iter,131) penalty,converge
           end if
           if(penx >= pennorm) then
              if(iout_6)write(6,100)jiter,iter,penx,pennorm
              write(iout_iter,100)jiter,iter,penx,pennorm
           end if
           if(end_iter)then
              if(stp > zero)then
                 if(iout_6)write(6,140)
                 write(iout_iter,140)
              else
                 if(iout_6)write(6,141)
                 write(iout_iter,141)
              end if
           end if
        end if
101     format(' PCGSOI: WARNING **** Stopping inner iteration ***')
100     format(' Penalty increase or constant ',I3,1x,i4,1x,2(e25.18,1x))
105     format(' PCGSOI: WARNING **** Reset to steepest descent, gnorm(2),gsave,b= ',3(e25.18,1x))
130     format(' gnorm ', e25.18,' less than ',e25.18)
131     format(' penalty ', e25.18,' less than ',e25.18)
140     format(' Stepsize calculation terminates inner iteration - probable convergence')
141     format(' Stepsize calculation terminates inner iteration - probable error')
        exit inner_iteration
     end if
     pennorm=penx

  end do inner_iteration
  if(iorthomax>0) then 
     do ii=1,iorthomax+1
        call deallocate_cv(cglwork(ii))
     enddo
     deallocate(cglwork)
     do ii=1,iorthomax+1
        call deallocate_cv(cglworkhat(ii))
     enddo
     deallocate(cglworkhat)
  end if
  if (lanlerr .and. lgschmidt) call destroy_mgram_schmidt

! Calculate adjusted observation error factor
  if( oberror_tune .and. (.not.l4dvar) ) then
     if (mype == 0) write(6,*) 'PCGSOI:  call penal for obs perturbation'
     call control2state(xhat,mval,sbias)
     if (l_hyb_ens) then
        call ensctl2state(xhat,mval(1),eval)
        do ii=1,nobs_bins
           sval(ii)=eval(ii)
        end do
     else
        do ii=1,nobs_bins
           sval(ii)=mval(1)
        end do
     end if

     call penal(sval(1))
     xhatsave=zero
     yhatsave=zero
     call clean_
     return
  endif

! Update contributions of incremental values from current outer loop

  if (l_tlnmc .and. baldiag_inc) call strong_baldiag_inc(sval,size(sval))

  llprt=(mype==0)
  call control2state(xhat,mval,sbias)
  if (l4dvar) then
    if (l_hyb_ens) then
       call ensctl2state(xhat,mval(1),eval)
       mval(1)=eval(1)
    end if
    call model_tl(mval,sval,llprt)
  else
    if (l_hyb_ens) then
       call ensctl2state(xhat,mval(1),eval)
       do ii=1,nobs_bins
          sval(ii)=eval(ii)
       end do
    else
       do ii=1,nobs_bins
          sval(ii)=mval(1)
       end do
    end if
  end if

  if(print_diag_pcg)then

! Evaluate final cost function and gradient
     if (mype==0) write(6,*)'Minimization final diagnostics'

     do ii=1,nobs_bins
       rval(ii)=zero
     end do
     call intall(sval,sbias,rval,rbias)
     gradx=zero
     if (l4dvar) then
       call model_ad(mval,rval,llprt)
       if (l_hyb_ens) then
          eval(1)=mval(1)
          call ensctl2state_ad(eval,mval(1),gradx)
       end if
     else
       if (l_hyb_ens) then
          do ii=1,nobs_bins
            eval(ii)=rval(ii)
          end do
          call ensctl2state_ad(eval,mval(1),gradx)
       else
          mval(1)=rval(1)
          if (nobs_bins > 1 ) then
             do ii=2,nobs_bins
                call self_add(mval(1),rval(ii))
             enddo
          end if
       end if
     end if
     call control2state_ad(mval,rbias,gradx)
  
!    Add contribution from background term
     do i=1,nclen
       gradx%values(i)=gradx%values(i)+yhatsave%values(i)
     end do
  
!    Multiply by background error
     if(anisotropic) then
       call anbkerror(gradx,grady)
     else
       call bkerror(gradx,grady)
     end if
  
!    If hybrid ensemble run, then multiply ensemble control variable a_en 
!                                    by its localization correlation
     if(l_hyb_ens) then
       if(aniso_a_en) then
    !     call anbkerror_a_en(gradx,grady)    !  not available yet
          write(6,*)' ANBKERROR_A_EN not written yet, program stops'
          stop
       else
          call bkerror_a_en(gradx,grady)
       end if

!    multiply static (Jb) part of grady by betas_inv(:), and
!    multiply ensemble (Je) part of grady by betae_inv(:). [Default : betae_inv(:) =  1 - betas_inv(:) ]
!      (this determines relative contributions from static background Jb and ensemble background Je)

       call beta12mult(grady)

     end if

!    Add potential additional preconditioner 
     if(diag_precon)call precond(grady)


! Print final Jo table
     zgend=dot_product(gradx,grady,r_quad)
!    nprt=2
!    call evaljo(zjo,iobs,nprt,llouter)
     call prt_control_norms(gradx,'gradx')

     if(l_hyb_ens) then

!       If hybrid ensemble run, compute contribution to Jb and Je separately

        fjcost_e=   dot_product(xhatsave,yhatsave,r_quad,'cost_e')
!       fjcost(1) = dot_product(xhatsave,yhatsave,r_quad,'cost_b')

!    else
!       fjcost(1) = dot_product(xhatsave,yhatsave,r_quad)
     end if
!    fjcost(2) = zjo
     zfend=penaltynew
!    if(l_hyb_ens) zfend=zfend+fjcost_e

     if (mype==0) then
        if(l_hyb_ens) then

!          If hybrid ensemble run, print out contribution to Jb and Je separately

           write(iout_iter,999)'costterms Jb,Je,Jo,Jc,Jl =',jiter,iter,fjcostnew(1)- fjcost_e, &
               fjcost_e,fjcostnew(2:4)

        else
           write(iout_iter,999)'costterms Jb,Jo,Jc,Jl =',jiter,iter,fjcostnew
        end if
        gnormx=zgend/gnormorig
        penx=zfend/penorig
        istep=1
        if (stp<small_step) istep=2
        if (zgini>tiny_r_kind .and. zfini>tiny_r_kind) then
           write(iout_iter,999)'penalty and grad reduction WRT outer and initial iter=', &
               jiter,iter,zfend/zfini,sqrt(zgend/zgini),penx,gnormx
        else
           write(iout_iter,999)'penalty and grad reduction WRT outer and initial iter=N/A'
        endif


!    Print final diagnostics
        write(6,888)'Final   cost function=',zfend
        write(6,888)'Final   gradient norm=',sqrt(zgend)
        if(zfini>tiny_r_kind) write(6,888)'Final/Initial cost function=',zfend/zfini
        if(zgini>tiny_r_kind) write(6,888)'Final/Initial gradient norm=',sqrt(zgend/zgini)
     endif
888  format(A,5(1X,ES25.18))

  end if
! Calculate increments of vorticity/divergence
  call xhat_vordiv_init
  call xhat_vordiv_calc(sval)

! Update guess (model background, bias correction) fields
! if (mype==0) write(6,*)'pcgsoi: Updating guess'
  if(iwrtinc<=0) call update_guess(sval,sbias)
  if(l_foto) call update_geswtend(xhat_dt)

! cloud analysis  after iteration
! if(jiter == miter .and. i_gsdcldanal_type==1) then
  if(jiter == miter) then
    if(i_gsdcldanal_type==2) then
     call gsdcloudanalysis4nmmb(mype)
    else if(i_gsdcldanal_type==1) then
     call gsdcloudanalysis(mype)
    endif
  endif

! Write output analysis files
  if(.not.l4dvar) call prt_guess('analysis')
  call prt_state_norms(sval(1),'increment')
  if (twodvar_regional) then
      call write_all(-1)
    else
      if(jiter == miter) then
         call clean_
         call write_all(-1)
      endif
  endif

! Overwrite guess with increment (4d-var only, for now)
  if (iwrtinc>0) then
     call inc2guess(sval)
     call write_all(iwrtinc)
     call prt_guess('increment')
     ! NOTE: presently in 4dvar, we handle the biases in a slightly inconsistent way
     ! as when in 3dvar - that is, the state is not updated, but the biases are.
     ! This assumes GSI handles a single iteration of the outer loop at a time
     ! when doing 4dvar (that is, multiple iterations require stop-and-go).
     call update_bias_preds(twodvar_regional,sbias)
  endif

! Clean up increments of vorticity/divergence
  call xhat_vordiv_clean

! Clean up major fields
  if (jiter < miter) call clean_

! Finalize timer
  call timer_fnl('pcgsoi')

! End of routine
  return

contains

subroutine init_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_    initialize pcgsoi
!   prgmmr:      Todling
!   
! abstract: initialize pcgsoi required fields on way in
!
! program history log:
!   2008-12-01  Todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   mschine:
!
!$$$ end documentation block

  use jfunc, only: diag_precon
  implicit none

! Allocate local variables
  call allocate_cv(xhat)
  call allocate_cv(gradx)
  call allocate_cv(grady)
  call allocate_cv(dirx)
  call allocate_cv(diry)
  if(diag_precon)call allocate_cv(dirw)
  call allocate_cv(ydiff)
  call allocate_cv(xdiff)
  do ii=1,nobs_bins
     call allocate_state(sval(ii))
     call allocate_state(rval(ii))
  end do
  call allocate_preds(sbias)
  call allocate_preds(rbias)
  do ii=1,nsubwin
     call allocate_state(mval(ii))
  end do

  do ii=1,ntlevs_ens
     call allocate_state(eval(ii))
  end do

  gradx=zero
  grady=zero
  dirx=zero
  diry=zero
  if(diag_precon)dirw=zero
  ydiff=zero
  xdiff=zero
  xhat=zero

  if(l_foto)then
     call allocate_state(xhat_dt)
     xhat_dt=zero
  end if

end subroutine init_

subroutine clean_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    clean_    clean pcgsoi
!   prgmmr:      Todling
!   
! abstract: clean pcgsoi required fields on way out
!
! program history log:
!   2008-12-01  Todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use jfunc, only: diag_precon
  implicit none

! Deallocate obs file
  if (.not.l4dvar) call destroyobs

! Release state-vector memory
  call deallocate_cv(xhat)
  call deallocate_cv(gradx)
  call deallocate_cv(grady)
  call deallocate_cv(dirx)
  call deallocate_cv(diry)
  if(diag_precon)call deallocate_cv(dirw)
  call deallocate_cv(ydiff)
  call deallocate_cv(xdiff)
 
! Release bias-predictor memory
  call deallocate_preds(sbias)
  call deallocate_preds(rbias)

  do ii=1,nobs_bins
     call deallocate_state(sval(ii))
     call deallocate_state(rval(ii))
  end do
  do ii=1,nsubwin
     call deallocate_state(mval(ii))
  end do
  do ii=1,ntlevs_ens
     call deallocate_state(eval(ii))
  end do
  if(l_foto)then
     call deallocate_state(xhat_dt)
  end if
! call inquire_state

end subroutine clean_

end subroutine pcgsoi

end module pcgsoimod
