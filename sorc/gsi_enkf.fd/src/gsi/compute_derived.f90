subroutine compute_derived(mype,init_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compute_derived     compute derived quantites from current solution
!   prgmmr: derber           org: np2                 date: 2005-11-29
!
! abstract:  This routine performs various functions, all related in one
!            way or the other to the model guess or current solution.  
!
!            Functions performed in this routine include the following
!              a) compute guess-derived fields required by pcp forward model
!              b) compute 3d pressure grids
!              c) compute saturation specific humidity.  on first outer iteration
!                 save qs for normalization in background error.  qs for limq
!                 is updated each outer iteration.
!              d) compute 3d geopotential height
!              e) compute 2d tropopause pressure map
!
! program history log:
!   2005-11-21  derber  - new routine from read_*.f90 routines
!   2005-11-22  wu - bug fix qoption=2 for regional nmm on dqdp and 
!                    set dqdt=0 above sigma=0.15; for regional mass
!                    set dqdt=dqdp=0 above sigma=0.15
!   2005-12-09  guo - remove GMAO derivative computation code.  Use
!                         unified NCEP compact_diff procedures.
!   2006-01-09  derber - include calculation of sigsum add capability of set_nrh_var
!   2006-01-30  kleist - correct tropprs unit error in qoption=2 q/t decoupling
!   2006-02-02  treadon - consolidate/unify use of guess pressure arrays
!   2006-02-03  derber - clean up RH statistics printout
!   2006-03-07  treadon - remove ges_prslk and related code
!   2006-03-27  treadon - remove guess bias correction arrays since not used
!   2006-04-17  treadon - replace sigi with bk5; replace sigl with
!                         ges_prslavg/ges_psfcavg
!   2006-04-21  kleist - modify call to calctends
!   2006-07-31  kleist - changes to use ps instead of ln(ps)
!   2006-09-29  treadon - add option to compute 10m wind factor fields
!   2007-03-13  derber  - add changes to make qoption=2 variances work as others
!   2007-05-08  kleist  - remove jcdivt from use list
!   2007-06-21  rancic - add pbl code
!   2007-07-26  cucurull - call gesprs, add ges_3dp and remove ps 
!                          in calctends argument list 
!   2007-08-08  derber - pass ges_teta to calctends rather than calculate seperately
!   2008-06-05  safford - rm unused uses
!   2008-10-10  derber  - add calculation of fact_tv
!   2008-11-03  sato - add anisotropic mode procedures
!   2008-12-08  todling - move 3dprs/geop-hght calculation from here into setuprhsall
!   2009-08-19  guo     - add verifications of drv_initialized and tnd_initialized
!                         before the use of related module variables.
!   2009-10-15  parrish - add rescale of ensemble rh perturbations
!                           (currently for internal generated ensemble only)
!   2010-03-11  derber/zhu - add qvar3d to prewgt and prewgt_reg, remove rescale_ensemble_rh_perturbations
!   2010-05-28  todling - obtain variable id's on the fly (add getindex)
!   2010-06-01  todling - remove nrf3 pointer
!   2010-06-05  todling - an_amp0 coming from control_vectors
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!   2011-05-22  rancic/todling - add traj-init call here (but it is undesirable to be here)
!   2011-07-15  zhu     - add cwgues for regional; update efr_ql 
!   2011-11-01  eliu    - assign cwgues for global
!   2011-12-02  zhu     - add safe-guard for the case when there is no entry in the metguess table
!   2012-02-08  kleist  - add ges_qsat, add uvflag arg in call to strong_bal_correction,
!                         compute ges_qsat over nfldsig bins for limq (when nobs_bins /=0)
!   2013-10-19  todling - bug fix in cal to strong_bal: prs_ten was being passed as 3d array
!                       - efr_q vars move to cloud_efr
!                       - unlike original code, now all derivates available at all time slots
!   2013-10-30  jung    - add test and removal of supersaturation
!   2013-02-26  m.kim   - applying qcmin to  ges_cwmr_it
!   2013-03-04  m.kim   - saving starting ges_cwmr_it(with negative values) as cwgues_original                          
!   
!   2014-04-18  todling - revisit interface to q_diag
!   2014-03-19  pondeca - add "load wspd10m guess"
!   2014-05-07  pondeca - add "load howv guess"
!   2014-06-19  carley/zhu - add lgues and dlcbasdlog
!   2014-11-28  zhu     - move cwgues0 to cloud_efr
!   2014-11-28  zhu     - re-compute ges_cwmr & cwgues the same way as in the regional when cw is not state variable
!   2015-07-10  pondeca - load dcldchdlog used in weak-constraint for cldch
!   2016-04-28  eliu    - copy cloud water to cwgues to be used in the inner loop     
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use jfunc, only: jiter,jiterstart,&
       qoption,switch_on_derivatives,&
       tendsflag,superfact,clip_supersaturation
  use control_vectors, only: cvars3d
  use control_vectors, only: nrf_var
  use control_vectors, only: an_amp0
  use mpimod, only: levs_id
  use guess_grids, only: ges_tsen,ges_qsat,ges_prsl,ntguessig,nfldsig,&
       ges_teta,fact_tv
  use guess_grids, only: nfldsig
  use cloud_efr_mod, only: efr_ql
  use derivsmod, only: drv_initialized
  use derivsmod, only: gsi_xderivative_bundle
  use derivsmod, only: gsi_yderivative_bundle
  use derivsmod, only: qsatg,qgues,ggues,vgues,pgues,lgues,dlcbasdlog,&
       dvisdlog,w10mgues,howvgues,cwgues,cldchgues,dcldchdlog
  use tendsmod, only: tnd_initialized
  use tendsmod, only: gsi_tendency_bundle
  use gridmod, only: lat2,lon2,nsig,nsig1o  
  use gridmod, only: regional
  use gridmod, only: twodvar_regional
  use gridmod, only: wrf_mass_regional
  use berror, only: hswgt
  use mod_strong, only: l_tlnmc,baldiag_full
  use obsmod, only: write_diag
  use gsi_4dvar, only: l4dvar

  use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  use constants, only: zero,one,one_tenth,half,fv,qmin,qcmin,ten,t0c,five,r0_05 

! for anisotropic mode
  use sub2fslab_mod, only: setup_sub2fslab, sub2fslab, sub2fslab_glb, destroy_sub2fslab
  use anberror, only: anisotropic, idvar, kvar_start, ngauss, indices, indices_p, &
                      filter_all,   filter_p2,   filter_p3, &
                      pf2aP1, pf2aP2, pf2aP3, rtma_subdomain_option
  use anisofilter, only: rh0f, corz, ensamp, mlat, rllatf, fact_qopt2
  use anisofilter_glb, only: rh2f, rh3f, ensamp0f, ensamp2f, ensamp3f, &
                             p0ilatf, p2ilatf, p3ilatf, p2ilatfm, p3ilatfm, get_stat_factk

  use gsi_4dvar, only: idmodel
  use gsi_4dcouplermod, only: gsi_4dcoupler_init_traj
  use mpeu_util, only: getindex
  use mpeu_util, only: die, tell
  use gsi_io, only: verbose
  implicit none


! Declare passed variables
  integer(i_kind),intent(in   ) :: mype
  logical        ,intent(in   ) :: init_pass

! Declare local variables
  character(len=*),parameter::myname='compute_derived'
  logical ice,fullfield
  integer(i_kind) i,j,k,ii,it,l,l2,iderivative,nrf3_q,istatus,ier
  integer(i_kind) nt,n_actual_clouds

  real(r_kind) dl1,dl2
  real(r_kind) tem4,indexw
  real(r_kind),dimension(lat2,lon2,nsig+1):: ges_3dp
  real(r_kind),dimension(lat2,lon2,nsig):: rhgues

  real(r_kind),allocatable,dimension(:,:,:):: ges_prs_ten
  real(r_kind),allocatable,dimension(:,:,:):: ges_u_ten
  real(r_kind),allocatable,dimension(:,:,:):: ges_v_ten
  real(r_kind),allocatable,dimension(:,:,:):: ges_tv_ten

  real(r_kind),allocatable,dimension(:,:  ):: ges_ps
  real(r_kind),allocatable,dimension(:,:,:):: ges_u
  real(r_kind),allocatable,dimension(:,:,:):: ges_v
  real(r_kind),allocatable,dimension(:,:,:):: ges_tv

  real(r_kind),pointer,dimension(:,:  ):: ptr2d   =>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_q   =>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_ql  =>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_qi  =>NULL()
  real(r_kind),pointer,dimension(:,:,:):: ges_cwmr=>NULL()

! for anisotropic mode
  integer(i_kind):: k1,ivar,kvar,igauss,iq_loc
  real(r_kind):: factor,factk,hswgtsum
  logical print_verbose

  print_verbose=.false.
  if(verbose)print_verbose=.true.

  if(init_pass .and. (ntguessig<1 .or. ntguessig>nfldsig)) &
     call die(myname,'invalid init_pass, ntguessig =',ntguessig)

! Get required indexes from control vector names
  nrf3_q=getindex(cvars3d,'q')
  iq_loc=getindex(nrf_var,'q')

! Compute qsat regardless of presence of q in guess
  iderivative=0
  ice=.true.
  do ii=1,nfldsig
     call genqsat(ges_qsat(1,1,1,ii),ges_tsen(1,1,1,ii),ges_prsl(1,1,1,ii),lat2,lon2, &
                  nsig,ice,iderivative)
  enddo

! If q in guess, check/fix q limits
  do ii=1,nfldsig
     call gsi_bundlegetpointer (gsi_metguess_bundle(ii),'q',ges_q,ier)
     if (ier/=0) exit
     if(ii == ntguessig) call q_diag(ii,mype)
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
! Limit q to be >= qmin
              ges_q(i,j,k)=max(ges_q(i,j,k),qmin)
! limit q to be <= ges_qsat
              if(clip_supersaturation) ges_q(i,j,k) = min(ges_q(i,j,k),superfact*ges_qsat(i,j,k,ii))
           end do
        end do
     end do
  end do
 
! Load guess cw for use in inner loop
! Get pointer to cloud water mixing ratio
  it=ntguessig
  call gsi_metguess_get('clouds::3d',n_actual_clouds,ier)
  if (n_actual_clouds>0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,istatus);ier=istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,istatus);ier=ier+istatus
     if (ier==0) then
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 cwgues(i,j,k)=ges_ql(i,j,k)+ges_qi(i,j,k)
              end do
           end do
        end do
     else
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr,istatus)
        if (istatus==0) then
           do k=1,nsig
              do j=1,lon2
                 do i=1,lat2
                    cwgues(i,j,k)=ges_cwmr(i,j,k)
                 end do
              end do
           end do
        endif
     end if  ! end of ier==0

!    update efr_ql
     if(regional .and. (.not. wrf_mass_regional) .and. jiter>jiterstart) then
       do ii=1,nfldsig
          do k=1,nsig
             do j=1,lon2
                do i=1,lat2
                   tem4=max(zero,(t0c-ges_tsen(i,j,k,ii))*r0_05)
                   indexw=five + five * min(one, tem4) 
                   efr_ql(i,j,k,ii)=1.5_r_kind*indexw
                end do
             end do
          end do
       end do
     end if  ! jiter
  else
     if(associated(ges_cwmr)) ges_cwmr => cwgues
  end if  ! end of n_actual_clouds

! RTodling: The following call is in a completely undesirable place
! -----------------------------------------------------------------
! Initialize atmospheric AD and TL model trajectory
  if(l4dvar.and.jiter==jiterstart) then
    call gsi_4dcoupler_init_traj(idmodel,rc=istatus)
       if(istatus/=0) call die(myname,'gsi_4dcoupler_init_traj(), rc =',istatus)
  endif

  call init_vars_('guess')

!-----------------------------------------------------------------------------------
! Compute derivatives for .not. twodvar_regional case
  if (.not. twodvar_regional)then

     if (switch_on_derivatives) then
        if(.not.drv_initialized) &
          call die(myname,'unexpected drv_initialized =',drv_initialized)

!       Instead, update gradients of all guess fields.  these will
!       be used for forward models that need gradient of background field,
!       and for getting time derivatives of prognostic variables for
!       time extrapolation and non-linear balance constraints.

        do nt=1,nfldsig
           call get_derivatives(gsi_metguess_bundle(nt),&
                                gsi_xderivative_bundle(nt), &
                                gsi_yderivative_bundle(nt))
        enddo

        if(.not. wrf_mass_regional .and. tendsflag)then
          if(.not.tnd_initialized) &
            call die(myname,'unexpected tnd_initialized =',tnd_initialized)


! now that we have derivs, get time tendencies if necessary
          if(init_pass) then

           if(allocated(ges_ps)) call getprs(ges_ps,ges_3dp)

           call calctends(mype,ges_teta(1,1,1,it),ges_3dp,gsi_metguess_bundle(it), &
                          gsi_xderivative_bundle(it),gsi_yderivative_bundle(it),&
                          gsi_tendency_bundle)

           if(l_tlnmc .and. write_diag(jiter) .and. baldiag_full) then
              fullfield=.true.

              call init_vars_('tendency')

              call strong_bal_correction(ges_u_ten,ges_v_ten,ges_tv_ten,ges_prs_ten(:,:,1),mype, &
                                         ges_u,ges_v,ges_tv,&
                                         ges_ps,print_verbose,fullfield,.false.,.true.)

              call final_vars_('tendency')
           end if
          end if       ! (init_pass)
        end if
     end if

     if(init_pass) then

! Compute tropopause level (in pressure, hPa).  The 'pvoz'
! string means compute tropopause using potential vorticity
! and ozone. The 'temp' string means compute tropopause 
! using WMO temperature lapse rate method.

! NOTE:  tropopause pressure is not needed for 2dvar option

       if(regional)then
          call tpause(mype,'temp')
       else     ! (regional)
          call tpause(mype,'pvoz')
       end if   ! (regional)
  
     endif       ! (init_pass)

  endif         ! (!twodvar_regional)

  if(.not. init_pass) return

! Load guess q for use in limq.  Initialize saturation array to guess.
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'q',ges_q,istatus)
  if (istatus==0) then
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              qgues(i,j,k)=ges_q(i,j,k)             ! q guess
              fact_tv(i,j,k)=one/(one+fv*qgues(i,j,k)) ! factor for tv to tsen conversion
           end do
        end do
     end do
  end if

! Load guess gust, vis, pblh, lcbas, & cldch for use in limg, limv, limp, & liml.
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'gust',ptr2d,istatus)
  if (istatus==0) then
     do j=1,lon2
        do i=1,lat2
           ggues(i,j)=max(one,ptr2d(i,j))
        end do
     end do
  end if
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'vis',ptr2d,istatus)
  if (istatus==0) then
     do j=1,lon2
        do i=1,lat2
           vgues(i,j)=max(100.0_r_kind,ptr2d(i,j))
           dvisdlog(i,j)=log(ten)*ptr2d(i,j)  !d(vis)/d(log(vis))
        end do
     end do
  end if
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'pblh',ptr2d,istatus)
  if (istatus==0) then
     do j=1,lon2
        do i=1,lat2
           pgues(i,j)=max(100.0_r_kind,ptr2d(i,j))
        end do
     end do
  end if
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'wspd10m',ptr2d,istatus)
  if (istatus==0) then
     do j=1,lon2
        do i=1,lat2
           w10mgues(i,j)=max(one,ptr2d(i,j))
        end do
     end do
  end if
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'howv',ptr2d,istatus)
  if (istatus==0) then
     do j=1,lon2
        do i=1,lat2
           howvgues(i,j)=max(one,ptr2d(i,j))
        end do
     end do
  end if
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'lcbas',ptr2d,istatus)
  if (istatus==0) then
     do j=1,lon2
        do i=1,lat2
           lgues(i,j)=max(100.0_r_kind,ptr2d(i,j))
           dlcbasdlog(i,j)=log(ten)*ptr2d(i,j)  !d(lcbas)/d(log(lcbas))
        end do
     end do
  end if
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'cldch',ptr2d,istatus)
  if (istatus==0) then
     do j=1,lon2
        do i=1,lat2
           cldchgues(i,j)=max(100.0_r_kind,ptr2d(i,j))
           dcldchdlog(i,j)=log(ten)*ptr2d(i,j)  !d(cldch)/d(log(cldch))
        end do
     end do
  end if

  if(allocated(ges_tv).and.allocated(ges_ps)) then

! Compute saturation specific humidity.   
     iderivative = 0
     if(qoption == 1)then
         if(jiter == jiterstart)iderivative = 1
     else
         iderivative = 2
     end if
      
     ice=.true.
     call genqsat(qsatg,ges_tsen(1,1,1,ntguessig),ges_prsl(1,1,1,ntguessig),lat2,lon2, &
              nsig,ice,iderivative)

!   Now load over nfldsig bins for limq (when nobs_bins /= zero)
    iderivative = 0
    do ii=1,nfldsig
      call genqsat(ges_qsat(1,1,1,ii),ges_tsen(1,1,1,ii),ges_prsl(1,1,1,ii),lat2,lon2, &
             nsig,ice,iderivative)
    end do
  endif

  call final_vars_('guess')

!??????????????????????????  need any of this????
!! qoption 1:  use psuedo-RH
!  if(qoption==1)then
!
!
!! qoption 2:  use normalized RH
!  else
  if(qoption == 2) then


! Load arrays based on option for moisture background error

! variance update for anisotropic mode
     if( anisotropic .and. .not.rtma_subdomain_option ) then
        hswgtsum=sum(hswgt(1:ngauss))
        call setup_sub2fslab
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 rhgues(i,j,k)=qgues(i,j,k)/qsatg(i,j,k)
              end do
           end do
        end do
        if( regional ) then
           allocate(rh0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
           call sub2fslab(rhgues,rh0f)
           do k=indices%kps,indices%kpe
              ivar=idvar(k)
              if(ivar==iq_loc) then
                 kvar=k-kvar_start(ivar)+1
                 do k1=1,nsig1o
                    if(levs_id(k1)==kvar) exit
                 end do
                 do j=indices%jps,indices%jpe
                    do i=indices%ips,indices%ipe
                       l =max(min(int(rllatf(i,j)),mlat),1)
                       l2=min((l+1),mlat)
                       dl2=rllatf(i,j)-real(l,r_kind)
                       dl1=one-dl2

                       factk=dl1*corz(l,kvar,nrf3_q)+dl2*corz(l2,kvar,nrf3_q)
                       call fact_qopt2(factk,rh0f(i,j,k1),kvar)
 
                       do igauss=1,ngauss
                          factor=hswgt(igauss)*factk*an_amp0(ivar)/sqrt(hswgtsum)
                          filter_all(1)%amp(igauss,i,j,k)=factor*filter_all(2)%amp(igauss,i,j,k)
                          if (allocated(ensamp)) then
                             filter_all(1)%amp(igauss,i,j,k)=filter_all(1)%amp(igauss,i,j,k)*ensamp(i,j,k1)
                          end if
                       end do
                    end do
                 end do
              end if
           end do
           deallocate(rh0f)
        else
           allocate(rh0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
           allocate(rh2f(pf2aP2%nlatf,pf2aP2%nlonf,nsig1o))
           allocate(rh3f(pf2aP3%nlatf,pf2aP3%nlonf,nsig1o))

           call sub2fslab_glb (rhgues,rh0f,rh2f,rh3f)
           do k=indices%kps,indices%kpe
              ivar=idvar(k)
              if(ivar==iq_loc) then
                 kvar=k-kvar_start(ivar)+1
                 do k1=1,nsig1o
                    if(levs_id(k1)==kvar) exit
                 end do
                 ! zonal patch
                 do j=indices%jps,indices%jpe
                    do i=indices%ips,indices%ipe
                       call get_stat_factk(p0ilatf(i),ivar,kvar,factk, &
                                           rh0f(i,j,k1),one)
                       do igauss=1,ngauss
                          factor=hswgt(igauss)*factk*an_amp0(ivar)/sqrt(hswgtsum)
                          filter_all(1)%amp(igauss,i,j,k)=factor*filter_all(2)%amp(igauss,i,j,k)
                          if (allocated(ensamp0f)) then
                             filter_all(1)%amp(igauss,i,j,k)=filter_all(1)%amp(igauss,i,j,k)*ensamp0f(i,j,k1)
                          end if
                       end do
 
                    end do
                 end do
                 ! polar patches
                 do j=indices_p%jps,indices_p%jpe
                    do i=indices_p%ips,indices_p%ipe
                       ! north polar
                       if(p2ilatf(i,j)/=zero) then
                          call get_stat_factk(p2ilatf(i,j),ivar,kvar,factk, &
                                              rh2f(i,j,k1),one)
                       else
                          call get_stat_factk(p2ilatfm    ,ivar,kvar,factk, &
                                              rh2f(i,j,k1),one)
                       end if
                       do igauss=1,ngauss
                          factor=hswgt(igauss)*factk*an_amp0(ivar)/sqrt(hswgtsum)
                          filter_p2(1)%amp(igauss,i,j,k)=factor*filter_p2(2)%amp(igauss,i,j,k)
                          if(allocated(ensamp2f)) then
                             filter_p2(1)%amp(igauss,i,j,k)=filter_p2(1)%amp(igauss,i,j,k)*sqrt(ensamp2f(i,j,k))
                          end if
                       end do
                       ! south polar
                       if(p3ilatf(i,j)/=zero) then
                          call get_stat_factk(p3ilatf(i,j),ivar,kvar,factk, &
                                              rh3f(i,j,k1),one)
                       else
                          call get_stat_factk(p3ilatfm    ,ivar,kvar,factk, &
                                              rh3f(i,j,k1),one)
                       end if
                       do igauss=1,ngauss
                          factor=factk*an_amp0(ivar)/sqrt(real(ngauss,r_kind))
                          filter_p3(1)%amp(igauss,i,j,k)=factor*filter_p3(2)%amp(igauss,i,j,k)
                          if(allocated(ensamp3f)) then
                             filter_p3(1)%amp(igauss,i,j,k)=filter_p3(1)%amp(igauss,i,j,k)*sqrt(ensamp3f(i,j,k))
                          end if
                       end do

                    end do
                 end do

              end if
           end do
           deallocate(rh0f,rh2f,rh3f)
        end if
        call destroy_sub2fslab
     end if

! End of qoption block
  endif

! End of routine
  return
  contains
  subroutine init_vars_(thiscase)
  implicit none
  character(len=*) thiscase

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) istatus

! If require guess vars available, extract from bundle ...
  if(trim(thiscase)=='guess') then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2)))
         ges_ps(:,:)=rank2
     else
         if(mype==0) &
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle'
     endif
!    get u ...
     varname='u'
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_u))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_u(size(rank3,1),size(rank3,2),size(rank3,3)))
         ges_u(:,:,:)=rank3
     else
         if(mype==0) &
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle'
     endif
!    get v ...
     varname='v'
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_v))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_v(size(rank3,1),size(rank3,2),size(rank3,3)))
         ges_v(:,:,:)=rank3
     else
         if(mype==0) &
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle'
     endif
!    get tv ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_tv))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tv(size(rank3,1),size(rank3,2),size(rank3,3)))
         ges_tv(:,:,:)=rank3
     else
         if(mype==0) &
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle'
     endif
  endif
! extract tendencies from tendency bundle
  if(trim(thiscase)=='tendency'.and.tnd_initialized) then
!    get u ...
     varname='u'
     call gsi_bundlegetpointer(gsi_tendency_bundle,trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_u_ten))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_u_ten(size(rank3,1),size(rank3,2),size(rank3,3)))
         ges_u_ten(:,:,:)=rank3
     else
         if(mype==0) &
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle'
     endif
!    get v ...
     varname='v'
     call gsi_bundlegetpointer(gsi_tendency_bundle,trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_v_ten))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_v_ten(size(rank3,1),size(rank3,2),size(rank3,3)))
         ges_v_ten(:,:,:)=rank3
     else
         if(mype==0) &
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle'
     endif
!    get tv ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_tendency_bundle,trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_tv_ten))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tv_ten(size(rank3,1),size(rank3,2),size(rank3,3)))
         ges_tv_ten(:,:,:)=rank3
     else
         if(mype==0) &
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle'
     endif
!    get prs ...
     varname='prse'
     call gsi_bundlegetpointer(gsi_tendency_bundle,trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_prs_ten))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_prs_ten(size(rank3,1),size(rank3,2),size(rank3,3)))
         ges_prs_ten(:,:,:)=rank3
     else
         if(mype==0) &
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle'
     endif
  endif

  end subroutine init_vars_

  subroutine final_vars_(thiscase)
  implicit none
  character(len=*) thiscase
  if (trim(thiscase)=='guess') then
     if(allocated(ges_tv)) deallocate(ges_tv)
     if(allocated(ges_v)) deallocate(ges_v)
     if(allocated(ges_u)) deallocate(ges_u)
     if(allocated(ges_ps)) deallocate(ges_ps)
  endif

  if (trim(thiscase)=='tendency') then
     if(allocated(ges_prs_ten)) deallocate(ges_prs_ten)
     if(allocated(ges_tv_ten)) deallocate(ges_tv_ten)
     if(allocated(ges_v_ten)) deallocate(ges_v_ten)
     if(allocated(ges_u_ten)) deallocate(ges_u_ten)
  endif
  end subroutine final_vars_

end subroutine compute_derived
