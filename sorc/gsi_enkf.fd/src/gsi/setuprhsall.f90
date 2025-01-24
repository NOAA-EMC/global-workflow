subroutine setuprhsall(ndata,mype,init_pass,last_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  setuprhsall   sets up rhs of oi 
!   prgmmr: derber           org: np23                date: 2003-05-22
!
! abstract: This routine sets up the right hand side (rhs) of the 
!           analysis equation.  Functions performed in this routine
!           include:
!             a) calculate increments between current solutions and obs,
!             b) generate statistical summaries of quality control and innovations,
!             c) generate diagnostic files (optional), and
!             d) prepare/save information for use in inner minimization loop
!
! program history log:
!   2003-05-22  derber
!   2003-12-23  kleist  - ozone calculation modified to use guess pressure
!   2004-06-17  treadon - update documentation
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase dimension of work arrays for nonlin qc
!   2004-12-08  xu li   - replace local logical flag retrieval with that in radinfo
!   2004-12-22  treadon - restructure code to compute and write out 
!                         innovation information on select outer iterations
!   2005-01-20  okamoto - add ssmi/amsre/ssmis
!   2005-03-30  lpchang - statsoz call was passing ozmz var unnecessarily
!   2005-04-18  treadon - deallocate fbias
!   2005-05-27  derber  - level output change
!   2005-07-06  derber  - include mhs and hirs/4
!   2005-06-14  wu      - add OMI oz
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - simplify data file info handling
!   2005-10-20  kazumori - modify for real AMSR-E data process
!   2005-12-01  cucurull - add GPS bending angle
!   2005-12-21  treadon  - modify processing of GPS data
!   2006-01-09  derber - move create/destroy array, compute_derived, q_diag
!                        from glbsoi outer loop into this routine
!   2006-01-12  treadon - add channelinfo
!   2006-02-03  derber  - modify for new obs control and obs count- clean up!
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - add code to generate optional observation perturbations
!   2006-07-28  derber  - modify code for new inner loop obs data structure
!   2006-07-29  treadon - remove create_atm_grids and destroy_atm_grids
!   2006-07-31  kleist - change call to atm arrays routines
!   2007-02-21  sienkiewicz - add MLS ozone changes
!   2007-03-01  treadon - add toss_gps and toss_gps_sub
!   2007-03-10      su - move the observation perturbation to each setup routine 
!   2007-03-19  tremolet - Jo table
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_conv_file
!   2007-07-09  tremolet - observation sensitivity
!   2007-06-20  cucurull - changes related to gps diagnostics
!   2007-06-29  jung - change channelinfo to array
!   2007-09-30  todling  - add timer
!   2007-10-03  todling  - add observer split option
!   2007-12-15  todling  - add prefix to diag filenames
!   2008-03-28    wu - move optional randon seed for perturb_obs to read_obs
!   2008-04-14  treadon - remove super_gps, toss_gps (moved into genstats_gps)
!   2008-05-23  safford - rm unused vars and uses
!   2008-12-08  todling - move 3dprs/geop-hght calculation from compute_derivate into here
!   2009-01-17  todling - update interface to intjo
!   2009-03-05  meunier - add call to lagragean operator
!   2009-08-19  guo     - moved all rhs related statistics variables to m_rhs
!                         for multi-pass setuprhsall();
!                       - added control arguments init_pass and last_pass for
!                         multi-pass setuprhsall().
!   2009-09-14  guo     - invoked compute_derived() even under lobserver.  This is
!                         the right way to do it.  It trigged moving of statments
!                         from glbsoi() to observer_init().
!                       - cleaned up redandent calls to setupyobs() and inquire_obsdiags().
!   2009-10-22     shen - add high_gps and high_gps_sub
!   2009-12-08  guo     - fixed diag_conv output rewind while is not init_pass, with open(position='rewind')
!   2010-04-09  cucurull - remove high_gps and high_gps_sub
!   2010-04-01  tangborn - start adding call for carbon monoxide data. 
!   2010-04-28      zhu - add ostats and rstats for additional precoditioner
!   2010-05-28  todling - obtain variable id's on the fly (add getindex)
!   2010-10-14  pagowski - added pm2_5 conventional obs
!   2010-10-20  hclin   - added aod
!   2011-02-16      zhu - add gust,vis,pblh
!   2011-04-07  todling - newpc4pred now in radinfo
!   2011-09-17  todling - automatic sizes definition for mpi-reduce calls
!   2012-01-11  Hu      - add load_gsdgeop_hgt to compute 2d subdomain pbl heights from the guess fields
!   2012-04-08  Hu      - add code to skip the observations that are not used in minimization
!   2013-02-22  Carley  - Add call to load_gsdgeop_hgt for NMMB/WRF-NMM if using
!                         PBL pseudo obs
!   2013-10-19  todling - metguess now holds background
!   2013-05-24      zhu - add ostats_t and rstats_t for aircraft temperature bias correction
!   2014-03-19  pondeca - add wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2014-0?-16  carley/zhu - add tcamt and lcbas
!   2015-07-10  pondeca - add cldch
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-05  pondeca - add uwnd10m, vwund10m
!   2017-05-12  Y. Wang and X. Wang - add dbz for reflectivity DA. POC: xuguang.wang@ou.edu
!   2018-01-01  Apodaca - add GOES/GLM lightning
!   2018-02-15  wu      - add code for fv3_regional 
!   2018-08-10  guo     - replaced type specific setupXYZ() calls with a looped
!                         polymorphic implementation using %setup().
!   2019-03-15  Ladwig  - add option for cloud analysis in observer
!   2019-03-28  Ladwig  - add metar cloud obs as pseudo water vapor in var analysis
!   2020-09-08  CAPS(G. Zhao) - add 'l_use_dbz_directDA' flag not to sort obsdiag
!   2023-03-20  K Apodaca - add GNSS-R L2 Ocean Wind Speed
!
!   input argument list:
!     ndata(*,1)- number of prefiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
!     mype     - mpi task id
!
!   output argument list:
!
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad,r_single
  use constants, only: zero,one,fv,zero_quad
  use guess_grids, only: load_prsges,load_geop_hgt,load_gsdpbl_hgt
  use guess_grids, only: ges_tsen,nfldsig
  use obsmod, only: nsat1,iadate,&
       ndat,obs_setup,&
       dtype,&
       dirname,write_diag,lobserver,&
       destroyobs,lobskeep,nobskeep,lobsdiag_allocated, &
       luse_obsdiag
  use obsmod, only: lobsdiagsave
  use obsmod, only: binary_diag
  use obs_sensitivity, only: lobsensfc, lsensrecompute
  use obs_sensitivity, only: obsensCounts_realloc
  use radinfo, only: newpc4pred
  use radinfo, only: mype_rad,jpch_rad,retrieval,fbias,npred,ostats,rstats
  use aircraftinfo, only: aircraft_t_bc_pof,aircraft_t_bc,ostats_t,rstats_t,npredt,ntail
  use ozinfo, only: mype_oz,jpch_oz,ihave_oz
  use coinfo, only: mype_co,jpch_co,ihave_co
  use lightinfo, only: mype_light
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mpi_sum
  use gridmod, only: twodvar_regional,wrf_mass_regional,nems_nmmb_regional
  use gridmod, only: cmaq_regional,fv3_regional
  use gsi_4dvar, only: nobs_bins,l4dvar
  use gsi_4dvar, only: mPEs_observer
  use jfunc, only: jiter,jiterstart,miter,first,last
  use qcmod, only: npres_print
  use convinfo, only: nconvtype,diag_conv
  use timermod, only: timer_ini,timer_fnl
  use lag_fields, only: lag_presetup,lag_state_write,lag_state_read,lag_destroy_uv
  use mpeu_util, only: getindex
  use mpl_allreducemod, only: mpl_allreduce
  use rapidrefresh_cldsurf_mod, only: l_PBL_pseudo_SurfobsT,l_PBL_pseudo_SurfobsQ,&
                                      l_PBL_pseudo_SurfobsUV,i_gsdcldanal_type,&
                                      i_cloud_q_innovation
  use m_rhs, only: rhs_alloc
  use m_rhs, only: rhs_dealloc
  use m_rhs, only: rhs_allocated
  use m_rhs, only: awork  => rhs_awork
  use m_rhs, only: bwork  => rhs_bwork
  use m_rhs, only: aivals => rhs_aivals
  use m_rhs, only: stats    => rhs_stats
  use m_rhs, only: stats_co => rhs_stats_co
  use m_rhs, only: stats_oz => rhs_stats_oz
  use m_rhs, only: toss_gps_sub => rhs_toss_gps

  use m_rhs, only: i_ps,i_uv,i_t,i_q,i_pw,i_rw,i_dw,i_gps,i_sst,i_tcp,i_lag, &
                   i_gust,i_vis,i_pblh,i_wspd10m,i_gnssrspd,i_td2m,i_mxtm,i_mitm,i_pmsl,i_howv, &
                   i_tcamt,i_lcbas,i_cldch,i_uwnd10m,i_vwnd10m,i_swcp,i_lwcp
  use m_rhs, only: i_dbz
  use m_rhs, only: i_fed
  use m_rhs, only: i_light

  use m_gpsStats, only: gpsStats_genstats       ! was genstats_gps()
  use m_gpsStats, only: gpsStats_destroy        ! was done by genstats_gps()

  use gsi_bundlemod, only: GSI_BundleGetPointer
  use gsi_metguess_mod, only: GSI_MetGuess_Bundle
  use m_obsdiags, only: obsdiags
  use m_obsdiags, only: obsdiags_reset
  use m_obsdiags, only: obsdiags_read
  use m_obsdiags, only: obsdiags_sort
  use m_obsdiags, only: obsdiags_write
  use m_obsdiags, only: inquire_obsdiags => obsdiags_inquire

  use gsi_obOperTypeManager, only: obOper_typeIndex
  use gsi_obOperTypeManager, only: nobs_type => obOper_count
  use gsi_obOper, only: obOper
  use m_obsdiags, only: obOpers_config
  use m_obsdiags, only: obOper_create
  use m_obsdiags, only: obOper_destroy
  use m_obsdiagNode, only: obsdiagLList_rewind

  use mpeu_util, only: die,warn,perr
  use mpeu_util, only: basename

  use directDA_radaruse_mod, only: l_use_dbz_directDA

  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: mype
  integer(i_kind),dimension(ndat,3),intent(in   ) :: ndata
  logical                          ,intent(in   ) :: init_pass, last_pass   ! state of "setup" processing


! Declare external calls for code analysis
  external:: compute_derived
  external:: evaljo
  !external:: genstats_gps
  external:: mpi_allreduce
  external:: mpi_finalize
  external:: mpi_reduce
  !external:: read_obsdiags
  external:: statsconv
  external:: statsoz
  external:: statspcp
  external:: statsrad
  external:: statslight
  external:: stop2
  external:: w3tage

! Delcare local variables
  logical:: conv_diagsave,llouter,getodiag

  character(80):: string
  character(10)::obstype
  character(20)::isis
  character(128):: diag_conv_file
  character(len=12) :: clfile

  integer(i_kind):: lunin,is,idate
  integer(i_kind):: iobs,nprt,ii,jj
  integer(i_kind):: it,ier,istatus
  integer(i_kind):: nreal,nchanl

  real(r_quad):: zjo
  real(r_kind),dimension(40,ndat):: aivals1
  real(r_kind),dimension(7,jpch_rad):: stats1
  real(r_kind),dimension(9,jpch_oz):: stats_oz1
  real(r_kind),dimension(9,jpch_co):: stats_co1
  real(r_kind),dimension(npres_print,nconvtype,5,3):: bwork1
  real(r_kind),allocatable,dimension(:,:):: awork1

  real(r_kind),dimension(:,:,:),pointer:: ges_tv_it=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q_it =>NULL()
  character(len=*),parameter:: myname='setuprhsall'

  logical,parameter:: OBSDIAGS_RELOAD = .false.
  !logical,parameter:: OBSDIAGS_RELOAD = .true.
  logical:: opened
  character(len=256):: tmpname,tmpaccess,tmpform

  class(obOper),pointer:: is_obOper

  if(.not.init_pass .and. .not.lobsdiag_allocated) call die('setuprhsall','multiple lobsdiag_allocated',lobsdiag_allocated)
!******************************************************************************
! Initialize timer
  call timer_ini('setuprhsall')

! Because I have to make a test
  luse_obsdiag = luse_obsdiag .or. OBSDIAGS_RELOAD

! Initialize variables and constants.
  first = jiter == jiterstart   ! .true. on first outer iter
  last  = jiter == miter+1      ! .true. following last outer iter
  llouter=.true.

! Set diagnostic output flag

  conv_diagsave = write_diag(jiter) .and. diag_conv

  if(.not.rhs_allocated) call rhs_alloc()
  allocate(awork1,mold=awork)

! Reset observation pointers
  if(init_pass) then
        ! setuprhsall() has been implemented to allow multiple passes over the
        ! same observation input steams, such that an outer loop can be used to
        ! iterate through incrementally available forecast states at different
        ! times.  In each iteration ("pass"), only a subset of observations with
        ! valid forecast states are computed for their innovations and linear
        ! observation-operators.
        !
        ! init_pass is a flag marking the first pass of setuprhsall() for a
        ! given jiter (a single outloop interaction is assumed with an update-
        ! to-date state, where some initialization code are involked.  And
        ! last_pass is a flag marking the last pass, where some summary code are
        ! involked.
        !
        ! This feature is needed in particular for high resolution background
        ! states for non-linear observation operator calculations (setup-calls),
        ! often refered as the "split-observer" mode.  In this mode, preloading
        ! of the forecast states of all time steps, is often not practical.

    call obOpers_config()

    call destroyobs()   ! remaining object, obsmod::nobs_sub(:,:)
    call obsensCounts_realloc(nobs_type,nobs_bins)

!++     call obOpers_reset(jiter,luse_obsdiag=luse_obsdiag,obsdiags_keep=lobsdiagsave)   ! replacing destroyobs()
    call obsdiags_reset(obsdiags_keep=lobsdiagsave)   ! replacing destroyobs()

! Read observation diagnostics if available
    if (l4dvar) then
      getodiag=(.not.lobserver) .or. (lobserver.and.jiter>1)
      clfile='obsdiags.ZZZ'
      if (lobsensfc .and. .not.lsensrecompute) then
        write(clfile(10:12),'(I3.3)') miter
        call obsdiags_read(clfile,mPEs=mPEs_observer,jiter_expected=miter)      ! replacing read_obsdiags()
        call inquire_obsdiags(miter)
      else if (getodiag) then
        if (.not.lobserver) then
           write(clfile(10:12),'(I3.3)') jiter
           call obsdiags_read(clfile,mPEs=mPEs_observer,jiter_expected=jiter)   ! replacing read_obsdiags()
           call inquire_obsdiags(miter)
        endif
      endif
    endif

    if (jiter>1.and.lobskeep) then
      nobskeep=1
    else
      nobskeep=0
    endif
  endif ! <init_pass>

! The 3d pressure and geopotential grids are initially loaded at
! the end of the call to read_guess.  Thus, we don't need to call 
! load_prsges and load_geop_hgt on the first outer loop.  We need 
! to update these 3d pressure arrays on all subsequent outer loops.
! Hence, the conditional call to load_prsges and load_geop_hgt

  if (lobserver .or. jiter>jiterstart) then

!    Get sensible temperature (after bias correction's been applied)
     do it=1,nfldsig
        ier=0
        call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'tv',ges_tv_it,istatus);ier=ier+istatus
        call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'q' ,ges_q_it ,istatus);ier=ier+istatus
        if(ier/=0) exit
        ges_tsen(:,:,:,it)= ges_tv_it(:,:,:)/(one+fv*max(zero,ges_q_it(:,:,:)))
     enddo

!    Load 3d subdomain pressure arrays from the guess fields
     call load_prsges

!    Compute 3d subdomain geopotential heights from the guess fields
     call load_geop_hgt

!    if (sfcmod_gfs .or. sfcmod_mm5) then
!       if (mype==0) write(6,*)'COMPUTE_DERIVED:  call load_fact10'
!       call load_fact10
!    endif
  endif

! Compute 2d subdomain pbl heights from the guess fields
   if (wrf_mass_regional) then
      call load_gsdpbl_hgt(mype)
   else if (nems_nmmb_regional .or. fv3_regional) then
      if (l_PBL_pseudo_SurfobsT .or. l_PBL_pseudo_SurfobsQ .or. l_PBL_pseudo_SurfobsUV) then
         call load_gsdpbl_hgt(mype)
      end if
   endif   


! Compute derived quantities on grid
  if(.not.cmaq_regional) call compute_derived(mype,init_pass)

  ! ------------------------------------------------------------------------

  if ( (l4dvar.and.lobserver) .or. .not.l4dvar ) then

     ! Init for Lagrangian data assimilation (gather winds and NL integration)
     call lag_presetup()
     ! Save state for inner loop if in 4Dvar observer mode
     if (l4dvar.and.lobserver) then
        call lag_state_write()
     end if

!    Reset observation pointers.  This is assumed by setup*() routines.
     do ii=1,size(obsdiags,2)
        do jj=1,size(obsdiags,1)
           call obsdiagLList_rewind(obsdiags(jj,ii))
        enddo
     enddo

     lunin=1
     open(lunin,file=obs_setup,form='unformatted')
     rewind lunin
 
  
!    If requested, create conventional diagnostic files
     if(conv_diagsave.and.binary_diag)then
        write(string,900) jiter
900     format('conv_',i2.2)
        diag_conv_file=trim(dirname) // trim(string)
        if(init_pass) then
           open(7,file=trim(diag_conv_file),form='unformatted',status='unknown',position='rewind')

        else
           !  open(7,file=trim(diag_conv_file),form='unformatted',status='old',position='append')

                ! Without a close(7) until the last_pass=.true., the same file
                ! is expected to remain open "asis", equivalent to an "append"
                ! position through a re-open().  Therefore, a sequence of
                ! verification steps are taken to replace the earlier open()
                ! statement, to avoid re-open() without a close().

           inquire(unit=7,opened=opened)
           if(opened) then
             inquire(unit=7,name=tmpname,form=tmpform,access=tmpaccess)
             tmpname=basename(tmpname)
             if(trim(tmpname)/=trim(diag_conv_file)) then
               call perr(myname,'unexpectly occupied, unit =',7)
               call perr(myname,'           diag_conv_file =',trim(diag_conv_file))
               call perr(myname,'   inquire(unit=7,  name= )',trim(tmpname))
               call perr(myname,'   inquire(unit=7,  form= )',trim(tmpform))
               call perr(myname,'   inquire(unit=7,access= )',trim(tmpaccess))
               call  die(myname)
             endif

           else
             call perr(myname,'unexpectly closed, unit =',7)
             call perr(myname,'         diag_conv_file =',trim(diag_conv_file))
             call  die(myname)
           endif
        endif
        idate=iadate(4)+iadate(3)*100+iadate(2)*10000+iadate(1)*1000000
        if(init_pass .and. mype == 0)write(7)idate
     end if

     if (newpc4pred) then
        ostats=zero
        rstats=zero_quad
     end if

     if (aircraft_t_bc_pof .or. aircraft_t_bc) then
        ostats_t=zero_quad
        rstats_t=zero_quad
     end if

!    Loop over data types to process (for polymorphic obOper%setup() calls)
     do is=1,ndat

        ! Skip data streams where no obOper has been implemented for now.
        ! These streams are handled in a "lazy" approach, to preserve its
        ! current behavior of the program.
        !
        ! (1) If present in distributed obs_setup streams, they will be
        !     processed, by sequantially skipping corresponding records
        !     with possible exceptions (error messages).
        !
        ! (2) If not expected to be present in distributed obs_setup streams,
        !     A warning message body for exceptions will be issued.  And
        !     the program will proceed as normal.

        select case(trim(dtype(is)))
        case('gos_ctp', 'rad_ref', 'lghtn', 'larccld', 'larcglb')
                ! Exception (1) (see above)

          if(nsat1(is)>0)then
            read(lunin,iostat=ier) obstype,isis,nreal,nchanl
                if(ier/=0) then
                  call perr(myname,'unexpected obs_setup read(1), iostat =',ier)
                  call perr(myname,'                                  is =',is)
                  call perr(myname,'                           ndat1(is) =',nsat1(is))
                  call perr(myname,'                           dtype(is) =',trim(dtype(is)))
                  call  die(myname)
                endif

            read(lunin,iostat=ier)
                if(ier/=0) then
                  call perr(myname,'unexpected obs_setup read(2), iostat =',ier)
                  call perr(myname,'                                  is =',is)
                  call perr(myname,'                           ndat1(is) =',nsat1(is))
                  call perr(myname,'                           dtype(is) =',trim(dtype(is)))
                  call perr(myname,'                             obstype =',trim(obstype))
                  call perr(myname,'                                isis =',trim(isis))
                  call perr(myname,'                               nreal =',nreal)
                  call perr(myname,'                              nchanl =',nchanl)
                  call  die(myname)
                endif
          endif

        case default

          is_obOper => obOper_create(dtype(is))

          if(associated(is_obOper)) then
            call is_obOper%setup(lunin,mype, is, nsat1(is), init_pass,last_pass)
            call obOper_destroy(is_obOper)

          else
                ! Exception (2) (see above)
            call warn(myname,'unexpected obOper, is =',is)
            call warn(myname,'                dtype =',trim(dtype(is)))
            call warn(myname,'     obOper_typeIndex =',obOper_typeIndex(dtype(is)))
          endif
        end select

     end do
     close(lunin)

     ! run cloud analysis in observer
     if(i_gsdcldanal_type==7) then
         call gsdcloudanalysis(mype)
         ! Write output analysis files
         call write_all(-1,mype)
         call prt_guess('analysis')
     endif

  else

     ! Init for Lagrangian data assimilation (read saved parameters)
     call lag_state_read()

  endif ! < lobserver >
  lobsdiag_allocated=.true.

  if(.not.last_pass) then
     call timer_fnl('setuprhsall')
     return
     ! So the rest code are for the last_pass only
  endif

! Deallocate wind field array for Lagrangian data assimilation
  call lag_destroy_uv()

! Finalize qc and accumulate statistics for GPSRO data
  call gpsStats_genstats(bwork,awork(:,i_gps),toss_gps_sub,conv_diagsave,mype)
  call gpsStats_destroy()       ! replacing ...
  ! -- call genstats_gps(bwork,awork(1,i_gps),toss_gps_sub,conv_diagsave,mype)

  if (conv_diagsave.and.binary_diag) close(7)

  ! Sorting with obsdiags_sort() would let the contents of obsdiags, including
  ! linked-lists of obsNodes as well as obs_diags, to be sorted into
  ! the same sequences, regardless their processing orders, number of
  ! processors, or particular distributions.
  !
  ! For ob. operators not implemented to support multi-setups using
  ! luse_obsdiag, sorting could become a problem.  Among them, cases of
  ! l_PBL_pseudo_SurfobsT, l_PBL_pseudo_SurfobsQ, and l_PBL_pseudo_SurfobsUV
  ! have been fixed since, but it might be better to keep it simple for
  ! those applications.  The case of i_cloud_q_innovation==2 is new.  It is
  ! not sure why it won't work even in case of .not.luse_obsdiag.

  if(.not.(l_PBL_pseudo_SurfobsT  .or.  l_PBL_pseudo_SurfobsQ     .or. &
           l_PBL_pseudo_SurfobsUV .or. (i_cloud_q_innovation==2)) .or.  &
           l_use_dbz_directDA  ) then
     call obsdiags_sort()
  endif

! for temporary testing purposes, _write and _read.
  if(OBSDIAGS_RELOAD) then
    call obsdiags_write('obsdiags.ttt',force=.true.)
        ! call Barrier() before obsdiags_read(), to make sure all PEs have
        ! finished their obsdiags_write().
    if(mPEs_observer>0) call MPI_Barrier(mpi_comm_world,ier)

    call obsdiags_read('obsdiags.ttt',mPEs=mPEs_observer,force=.true.)
    call inquire_obsdiags(miter)
  endif

! call inquire_obsdiags(miter)

! Collect information for preconditioning
  if (newpc4pred) then
     call mpl_allreduce(jpch_rad,rpvals=ostats)
     call mpl_allreduce(npred,jpch_rad,rstats)
  end if

! Collect information for aircraft data
  if (aircraft_t_bc_pof .or. aircraft_t_bc) then
!    call mpl_allreduce(npredt,max_tail,ostats_t)
!    call mpl_allreduce(npredt,max_tail,rstats_t)
     call mpl_allreduce(npredt,ntail,ostats_t)
     call mpl_allreduce(npredt,ntail,rstats_t)
  end if

! Collect satellite and precip. statistics
  call mpi_reduce(aivals,aivals1,size(aivals1),mpi_rtype,mpi_sum,mype_rad, &
       mpi_comm_world,ierror)

  call mpi_reduce(stats,stats1,size(stats1),mpi_rtype,mpi_sum,mype_rad, &
       mpi_comm_world,ierror)

  if (ihave_oz) call mpi_reduce(stats_oz,stats_oz1,size(stats_oz1),mpi_rtype,mpi_sum,mype_oz, &
       mpi_comm_world,ierror)

  if (ihave_co) call mpi_reduce(stats_co,stats_co1,size(stats_co1),mpi_rtype,mpi_sum,mype_co, &
       mpi_comm_world,ierror)

! Collect conventional data statistics
  
  call mpi_allreduce(bwork,bwork1,size(bwork1),mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)
  
  call mpi_allreduce(awork,awork1,size(awork1),mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)

! Compute and print statistics for radiance, precipitation, and ozone data.
! These data types are NOT processed when running in 2dvar mode.  Hence
! the check on the 2dvar flag below.

  if ( (l4dvar.and.lobserver) .or. .not.l4dvar ) then

     if (.not.twodvar_regional) then

!       Compute and print statistics for radiance data
        if(mype==mype_rad) call statsrad(aivals1,stats1,ndata)

!       Compute and print statistics for precipitation data
        if(mype==mype_rad) call statspcp(aivals1,ndata)

!       Compute and print statistics for ozone
        if (mype==mype_oz .and. ihave_oz) call statsoz(stats_oz1,ndata)

!       Compute and print statistics for carbon monoxide
!????   if (mype==mype_co .and. ihave_co) call statsco(stats_co1,bwork1,awork1(1,i_co),ndata)

     endif

!    Compute and print statistics for "conventional" data
     call statsconv(mype,&
          i_ps,i_uv,i_t,i_q,i_pw,i_rw,i_dw,i_gps,i_sst,i_tcp,i_lag, &
          i_gust,i_vis,i_pblh,i_wspd10m,i_gnssrspd,i_td2m,i_mxtm,i_mitm,i_pmsl,i_howv, &
          i_tcamt,i_lcbas,i_cldch,i_uwnd10m,i_vwnd10m,i_swcp,i_lwcp,i_fed,i_dbz, &
          size(awork1,2),bwork1,awork1,ndata)

!     Compute and print statistics for "lightning" data
     if (mype==mype_light) call statslight(mype,i_light,bwork1,awork1,size(awork1,2),ndata)

  endif  ! < .not. lobserver >

  deallocate(awork1)
  call rhs_dealloc()   ! destroy the workspace: awork, bwork, etc.
! Print Jo table
  nprt=2
  llouter=.true.
  if(luse_obsdiag)call evaljo(zjo,iobs,nprt,llouter)

! If only performing sst retrieval, end program execution
  if(retrieval)then
     deallocate(fbias)
     if(mype==0)then
        write(6,*)'SETUPRHSALL:  normal completion for retrieval'
        call w3tage('GLOBAL_SSI')
     end if
     call mpi_finalize(ierror)
     stop
  end if

! Finalize timer
  call timer_fnl('setuprhsall')

  return
end subroutine setuprhsall
