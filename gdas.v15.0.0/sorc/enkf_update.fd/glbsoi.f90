subroutine glbsoi(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    glbsoi               driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   1994-02-11  parrish
!   1998-05-15  weiyu yang       mpp version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1993-12-22  kleist,d., treadon, r.,derber, j.  modules, updates and comments
!   2004-06-21  treadon - update documentation
!   2004-07-08  treadon - fix vertical indexing bug in call set_ozone_var
!   2004-07-24  treadon - add only to module use, add intent in/out
!   2004-11-22  parrish - add code to handle regional netcdf i/o
!   2004-12-13  treadon - limit runtime output from CRTM & IRSSE initilizations
!   2004-12-22  treadon - add optional code to compute/write out innovation
!                         information following all outer loops
!   2005-01-22  parrish - add balmod, compact_diffs
!   2005-02-23  wu      - setup norm rh
!   2005-03-01  parrish - add parts of regional anisotropic background option 
!                         (not complete yet)
!   2005-04-14  yanqiu zhu - support for observation sensitivity study
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!   2005-05-27  kleist  - add call to destroy grids from patch interpolation
!   2005-06-08  treadon - add switch_on_derivatives for create/destroy_ges_bias_grids
!   2005-06-13  li/treadon - move destroy_sfc_grids after write_all
!   2005-07-12  kleist  - add Jc term
!   2005-08-16  guo - add gmao surface interface
!   2005-09-28  parrish - add logic to only allow NCEP global or WRF_NMM to use jc routines
!   2005-09-29  derber  - simplify observation file handling
!   2005-11-21  kleist - use tendency module, add call to get moisture diagnostics
!   2005-11-28  derber - move read_obs to glbsoi
!   2005-11-29  derber - move read guess and background error calculations outside 
!                        external iteration
!   2006-01-09  derber - absorb set_nrh_var into compute_derived
!   2006-01-10  treadon - move read*files into gesinfo, consolidate read*guess calls
!   2006-01-12  treadon - replace pCRTM with CRTM
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-12  treadon - remove mpimod (not used)
!   2006-04-21  kleist  - remove call to setupjc, no longer exists
!   2006-07-28  derber  - remove creation of obs inner loop data file name
!   2006-08-15  parrish - add call to create_vtrans (get vertical modes if nvmodes_keep > 0)
!   2006-12-04  todling - split bias and guess init/final (rename ges_bias routines)
!   2006-12-15  todling - no need to protect destroy call to tendvars
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2007-03-15  su      - to delocate the converr arrays
!   2007-05-08  kleist  - add preliminary verions of mp_compact_diffs module
!   2007-06-08  kleist/treadon - add prefix (task id or path) to obs_setup
!   2007-06-21  rancic - add pbl code
!   2007-06-27  tremolet- observation sensitivity
!   2007-06-29  jung    - update CRTM interface
!   2007-07-05  todling - skip calculating o-a when running in 4d-var mode
!   2007-08-27  tremolet- changed outer loop control for 4dvar
!   2007-09-30  todling - add timer
!   2007-10-25  todling - obsdiag files now written by observer
!   2007-11-12  todling - write sat bias moved from write_all here (ESMF-interface support)
!   2008-01-04  tremolet- outer loop for sensitivity computations
!   2008-11-03  sato    - enable use of global anisotropic mode
!   2008-12-02  todling - remove references to pcgsoi_tl and old obs_sen
!   2009-01-28  todling - move write_all to pcgsoi (for consistency w/ 4dvar branch of code)
!                       - use observer to avoid redundant code
!   2009-08-19  guo     - changed setuprhsall() interface for multi-pass observer.
!   2009-08-31  parrish - add call to fmg_initialize_e when tlnmc_type=4.  Initializes
!                          alternative regional tangent linear normal mode constraint which
!                          allows for variation of coriolis parameter and map factor.
!   2009-09-12  parrish - add call to hybrid_ensemble_setup.  if l_hyb_ens=.true., then
!                          subroutine hybrid_ensemble_setup is called, which creates 
!                          everything needed for a hybrid ensemble 3dvar analysis.
!   2009-09-14  guo     - move compact_diff related statements to observer_init() in observer.F90
!   2010-02-20  parrish - move hybrid_ensemble_setup to beginning of code and read
!                          in ensemble perturbations where hybrid_ensemble_setup was previously located.
!   2010-04-27  zhu     - add call to pcinfo. when newpc4pred=.true., pcinfo is called
!                         to calculate additional preconditioner; add newpc4pred in
!                         radinfo_write's interface
!   2010-05-12  zhu     - add option passive_bc for radiance bias correction for monitored channels
!   2010-10-01  el akkraoui/todling - add Bi-CG as optional minimization scheme
!   2011-04-07  todling - newpc4pred now in radinfo
!   2011-08-01  lueken  - replaced F90 with f90 (no machine logic)
!   2011-09-05  todling - repositioned initialization of ensemble to enable sqrt-ens feature
!   2012-07-12  todling - read yhatsave as well as xhatsave in 4dvar mode; knob for nested resolution
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!   2013-05-19  zhu     - add aircraft temperature bias correction
!   2013-07-02  parrish - remove references to init_strongvars_1, init_strongvars_2
!   2014-02-03  todling - move cost function create/destroy from observer to this routine;
!                         reposition load of ens due to init of sqrt(ens) dims dependences
!   2014-02-05  todling - update interface to prebal
!   2014-06-19  carley/zhu - Modify for R_option: optional variable correlation length twodvar_regional
!                            lcbas analysis variable
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: rearth
  use mpimod, only: npe
  use adjtest_obs, only: adtest_obs
  use jfunc, only: miter,jiter,jiterstart,jiterend,iguess,&
      write_guess_solution,R_option,&
      tendsflag,xhatsave,yhatsave,create_jfunc,destroy_jfunc
  use anberror, only: anisotropic, &
      create_anberror_vars_reg,destroy_anberror_vars_reg,&
      create_anberror_vars,destroy_anberror_vars
  use anisofilter, only: anprewgt_reg
  use anisofilter_glb, only: anprewgt
  use berror, only: create_berror_vars_reg,create_berror_vars,&
      set_predictors_var,destroy_berror_vars_reg,destroy_berror_vars,& 
      bkgv_flowdep,pcinfo,fut2ps,cwcoveqqcov
  use balmod, only: create_balance_vars_reg,create_balance_vars, &
      destroy_balance_vars_reg,destroy_balance_vars,prebal,prebal_reg
  use compact_diffs, only: create_cdiff_coefs,inisph
  use gridmod, only: nlat,nlon,nsig,rlats,regional,&
      twodvar_regional,wgtlats
  use guess_grids, only: nfldsig
  use obsmod, only: write_diag,perturb_obs,ditype,iadate
  use qcmod,only: njqc
  use turblmod, only: create_turblvars,destroy_turblvars
  use obs_sensitivity, only: lobsensfc, iobsconv, lsensrecompute, &
      init_fc_sens, save_fc_sens, lobsensincr, lobsensjb
  use smooth_polcarf, only: norsp,destroy_smooth_polcas
  use jcmod, only: ljcdfi
  use gsi_4dvar, only: l4dvar, lsqrtb, lbicg, lanczosave, lnested_loops, ladtest_obs
  use pcgsoimod, only: pcgsoi
  use control_vectors, only: dot_product
  use radinfo, only: radinfo_write,passive_bc,newpc4pred
  use pcpinfo, only: pcpinfo_write
  use converr, only: converr_destroy
  use converr_ps, only: converr_ps_destroy
  use converr_q, only: converr_q_destroy
  use converr_t, only: converr_t_destroy
  use converr_uv, only: converr_uv_destroy
  use converr_pw, only: converr_pw_destroy
  use convb_ps, only: convb_ps_destroy
  use convb_q, only: convb_q_destroy
  use convb_t, only: convb_t_destroy
  use convb_uv, only: convb_uv_destroy
  use zrnmi_mod, only: zrnmi_initialize
  use observermod, only: observer_init,observer_set,observer_finalize,ndata
  use timermod, only: timer_ini, timer_fnl
  use hybrid_ensemble_parameters, only: l_hyb_ens,destroy_hybens_localization_parameters
  use hybrid_ensemble_isotropic, only: create_ensemble,load_ensemble,destroy_ensemble, &
       hybens_localization_setup,hybens_grid_setup
  use gfs_stratosphere, only: destroy_nmmb_vcoords,use_gfs_stratosphere
  use aircraftinfo, only: aircraftinfo_write,aircraft_t_bc_pof,aircraft_t_bc,mype_airobst

  implicit none


! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  logical laltmin

  integer(i_kind) jiterlast
  real(r_kind) :: zgg,zxy
  character(len=12) :: clfile


!*******************************************************************************************
!
! Initialize timer for this procedure
  call timer_ini('glbsoi')

  if(mype==0) write(6,*) 'glbsoi: starting ...'


! If l_hyb_ens is true, then initialize machinery for hybrid ensemble 3dvar
  if(l_hyb_ens) then
     call hybens_grid_setup
     call create_ensemble
  end if

! Check for alternative minimizations
  laltmin = lsqrtb.or.lbicg

! Initialize observer
  call observer_init

! Check GSI options against available number of guess time levels
  if (nfldsig == 1) then
     if (bkgv_flowdep) then
        bkgv_flowdep = .false.
        if (mype==0) &
           write(6,*)'GLBSOI: ***WARNING*** reset bkgv_flowdep=',bkgv_flowdep,&
           ', because only ',nfldsig,' guess time level available'
     endif
  endif

! Set cost function
  call create_jfunc

! Read observations and scatter
  call observer_set

! Create/setup background error and background error balance
  if (regional)then
     call create_balance_vars_reg(mype)
     if(anisotropic) then
        call create_anberror_vars_reg(mype)
     else
        call create_berror_vars_reg
     end if
     call prebal_reg(cwcoveqqcov)
     if (.not. R_option) then
        if(anisotropic) then
           call anprewgt_reg(mype)
        else
           call prewgt_reg(mype)
        end if
     end if
  else
     call create_balance_vars
     if(anisotropic) then
        call create_anberror_vars(mype)
     else
        call create_berror_vars
     end if
     
     call prebal(fut2ps,cwcoveqqcov)

!    Load background error arrays used by recursive filters
     if(anisotropic) then
        call anprewgt(mype)
     else
        call prewgt(mype)
     end if
  end if

! If l_hyb_ens is true, then read in ensemble perturbations
  if(l_hyb_ens) then
     call load_ensemble
     call hybens_localization_setup
  end if

! Set error (variance) for predictors (only use guess)
  call set_predictors_var

! Set errors and create variables for dynamical constraint
  if (ljcdfi) call init_jcdfi

! Read output from previous min.
  if (l4dvar.and.jiterstart>1) then
     clfile='xhatsave.ZZZ'
     write(clfile(10:12),'(I3.3)') jiterstart-1
     call view_cv_ad(xhatsave,iadate,clfile,.not.lnested_loops)
     zgg=dot_product(xhatsave,xhatsave)
     if (mype==0) write(6,*)'Norm   xhatsave=',sqrt(zgg)
     if (.not.lsqrtb) then
        clfile='yhatsave.ZZZ'
        write(clfile(10:12),'(I3.3)') jiterstart-1
        call view_cv_ad(yhatsave,iadate,clfile,.not.lnested_loops)
        zgg=dot_product(yhatsave,yhatsave)
        zxy=dot_product(xhatsave,yhatsave)
        if (mype==0) then
            write(6,*)'Norm   yhatsave=',sqrt(zgg)
            write(6,*)'Norm x,yhatsave=',zxy
        endif
     endif
  endif

  jiterlast=miter
  if (lsensrecompute) jiterlast=jiterend
  if (l4dvar) jiterlast=jiterstart
  if (ladtest_obs) jiterlast=jiterstart

! Main outer analysis loop
  do jiter=jiterstart,jiterlast

     if (mype==0) write(6,*)'GLBSOI: jiter,jiterstart,jiterlast,jiterend=', &
        jiter,jiterstart,jiterlast,jiterend

!    Set up right hand side of analysis equation
     call setuprhsall(ndata,mype,.true.,.true.)

!    Estimate correlation length for lcbas if R_option==.true.
!      For this to work we need to have run setuplcbas first to get the weights.
!      Note that R_option is only applicable for lcbas and not for any
!      another twodvar_regional variables.  All other variables are handled
!      as they would have been otherwise.
     if (R_option .and. twodvar_regional .and. jiter==jiterstart) then
        if(anisotropic) then
           call anprewgt_reg(mype)
        else
           call prewgt_reg(mype)
        end if
     end if

! implement obs adjoint test and return  
     if( ladtest_obs) then
        call adtest_obs
        return
     end if

     if (jiter<=miter) then

!       Set up right hand side of adjoint of analysis equation
        if (lsensrecompute) lobsensfc=(jiter==jiterend)
        if (lobsensfc.or.iobsconv>0) call init_fc_sens
 
!       Call inner minimization loop
        if (laltmin) then
           if (newpc4pred) then
              if (mype==0) write(6,*)'GLBSOI: newpc4pred is not available for lsqrtb'
              call stop2(334)
           end if
           if (lsqrtb) then
              if (mype==0) write(6,*)'GLBSOI: Using sqrt(B), jiter=',jiter
              call sqrtmin
           endif
           if (lbicg) then
              if (mype==0) write(6,*)'GLBSOI: Using bicg, jiter=',jiter
              call bicg
           endif
        else
!          Set up additional preconditioning information
           call pcinfo

!          Standard run
           if (mype==0) write(6,*)'GLBSOI:  START pcgsoi jiter=',jiter
           call pcgsoi
        end if

!       Save information for next minimization
        if (lobsensfc) then
           clfile='obsdiags.ZZZ'
           write(clfile(10:12),'(I3.3)') 100+jiter
           call write_obsdiags(clfile)
           if (lobsensincr .or. lobsensjb) then
              clfile='xhatsave.ZZZ'
              write(clfile(10:12),'(I3.3)') jiter
              call view_cv(xhatsave,iadate,clfile,.not.lnested_loops)
           endif
        elseif (l4dvar.or.lanczosave) then
           clfile='obsdiags.ZZZ'
           write(clfile(10:12),'(I3.3)') jiter
           call write_obsdiags(clfile)
           clfile='xhatsave.ZZZ'
           write(clfile(10:12),'(I3.3)') jiter
           call view_cv(xhatsave,iadate,clfile,.not.lnested_loops)
           zgg=dot_product(xhatsave,xhatsave)
           if (mype==0) write(6,*)'Norm   xhatsave=',sqrt(zgg)
           if(.not.lsqrtb) then
              clfile='yhatsave.ZZZ'
              write(clfile(10:12),'(I3.3)') jiter
              call view_cv(yhatsave,iadate,clfile,.not.lnested_loops)
              zgg=dot_product(yhatsave,yhatsave)
              zxy=dot_product(xhatsave,yhatsave)
              if (mype==0) write(6,*)'Norm   yhatsave=',sqrt(zgg)
              if (mype==0) write(6,*)'Norm x,yhatsave=',zxy
           endif
        endif

!       Save output of adjoint of analysis equation
        if (lobsensfc.or.iobsconv>0) call save_fc_sens
     endif

! End of outer iteration loop
  end do

  if (.not.l4dvar) then
     jiter=jiterlast+1
!    If requested, write obs-anl information to output files
     if (write_diag(jiter)) then 
        call setuprhsall(ndata,mype,.true.,.true.)
        if (.not. lsqrtb) call pcinfo
        if (any(ditype=='rad') .and. passive_bc) call prad_bias
     end if

!    Write xhat- and yhat-save for use as a guess for the solution
     if (iguess==0 .or. iguess==1) call write_guess_solution(mype)
  endif

! Deallocate arrays
  if(perturb_obs) then
     if(njqc) then
        call converr_ps_destroy
        call converr_q_destroy
        call converr_t_destroy
        call converr_uv_destroy
        call converr_pw_destroy
        call convb_ps_destroy
        call convb_q_destroy
        call convb_t_destroy
        call convb_uv_destroy
     else
        call converr_destroy
     endif  
  endif

  if (regional) then
     if(anisotropic) then
        call destroy_anberror_vars_reg
     else
        call destroy_berror_vars_reg
     end if
     call destroy_balance_vars_reg
     if(use_gfs_stratosphere) call destroy_nmmb_vcoords
  else
     if(anisotropic) then
        call destroy_anberror_vars
     else
        call destroy_berror_vars
     end if
     call destroy_balance_vars
     if (norsp > 0) call destroy_smooth_polcas
  endif

  if (l_hyb_ens) call destroy_hybens_localization_parameters

! Write updated bias correction coefficients
  if (.not.twodvar_regional) then
     if (l4dvar) then
        if(mype == 0) call radinfo_write
        if(mype == npe-1) call pcpinfo_write
        if(mype==mype_airobst .and. (aircraft_t_bc_pof .or. aircraft_t_bc)) call aircraftinfo_write
     else
        if (jiter==miter+1 ) then
           if(mype == 0) call radinfo_write
           if(mype == npe-1) call pcpinfo_write
           if(mype==mype_airobst .and. (aircraft_t_bc_pof .or. aircraft_t_bc)) call aircraftinfo_write
        endif
     endif
  endif

! Finalize cost function 
  call destroy_jfunc

! Finalize observer
  call observer_finalize

! When applicable, finalize ensemble 
  if(l_hyb_ens) then
    call destroy_ensemble
  endif

  if(mype==0) write(6,*) 'glbsoi: complete'

! Finalize timer for this procedure
  call timer_fnl('glbsoi')

! End of routine

end subroutine glbsoi
