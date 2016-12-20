
subroutine setupozlay(lunin,mype,stats_oz,nlevs,nreal,nobs,&
     obstype,isis,is,ozone_diagsave,init_pass)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setupozlay --- Compute rhs of oi for sbuv ozone obs
!
!   prgrmmr:     parrish          org: np22                date: 1990-10-06
!
! abstract:      For sbuv ozone observations (layer amounts and total 
!                column, this routine 
!                  a) reads obs assigned to given mpi task (geographic region),
!                  b) simulates obs from guess,
!                  c) apply some quality control to obs,
!                  d) load weight and innovation arrays used in minimization
!                  e) collects statistics for runtime diagnostic output
!                  f) writes additional diagnostic information to output file
!
! program history log:
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu, ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-23  kleist - modify to use pressure as vertical coordinate
!   2004-05-28  kleist - subroutine call update
!   2004-06-17  treadon - update documentation
!   2004-07-08  todling - added only's; removed gridmod; bug fix in diag
!   2004-07-15  todling - protex-compliant prologue; added intent's
!   2004-10-06  parrish - increase size of stats_oz for nonlinear qc,
!                         add nonlin qc penalty calc and obs count                 
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - add outer loop number to name of diagnostic file
!   2005-03-02  dee - reorganize diagnostic file writes so that
!                         concatenated files are self-contained
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-03-16  derber  - change call to sproz to save observation time
!   2005-04-11  treadon - add logical to toggle on/off nonlinear qc code
!   2005-05-18  wu - add use of OMI total ozone data
!   2005-09-22  derber - modify extensively - combine with sproz - no change
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-07  treadon - fix bug in increment of ii
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2006-01-09  treadon - remove unused variables
!   2006-02-03  derber  - modify for new obs control
!   2006-02-17  treadon - correct bug when processing data not assimilated
!   2006-03-21  treadon - add option to perturb observation
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-03-09      su  - remove option to perturb observation
!   2007-03-19  tremolet - binning of observations
!   2007-05-30  h.liu   - include rozcon with interpolation weights
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_ozone_file
!   2007-06-05  tremolet - add observation diagnostics structure
!   2008-05-23  safford - add subprogram doc block, rm unused uses and vars
!   2008-01-20  todling - add obsdiag info to diag files
!   2009-01-08  todling - re-implemented obsdiag/tail
!   2009-10-19  guo     - changed for multi-pass setup with dtime_check() and new
!			  arguments init_pass and last_pass.
!   2009-12-08  guo     - cleaned diag output rewind with open(position='rewind')
!   2011-12-07  todling - bug fix: need luse check when saving obssens
!   2012-09-10  wargan/guo - add hooks for omieff"
!   2013-01-26  parrish - change from grdcrd to grdcrd1, tintrp2a to tintrp2a1, intrp2a to intrp2a1,
!                           intrp3oz to intrp3oz1. (to allow successful debug compile on WCOSS)
!   2013-09-10  guo        - patched to take reference pressure from the observation
!   2013-10-19  todling - metguess now holds background
!   2013-11-26  guo     - removed nkeep==0 escaping to allow more than one obstype sources.
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!
!   input argument list:
!     lunin          - unit from which to read observations
!     mype           - mpi task id
!     nlevs          - number of levels (layer amounts + total column) per obs   
!     nreal          - number of pieces of non-ozone info (location, time, etc) per obs
!     nobs           - number of observations
!     isis           - sensor/instrument/satellite id
!     is             - integer(i_kind) counter for number of obs types to process
!     obstype        - type of ozone obs
!     ozone_diagsave - switch on diagnostic output (.false.=no output)
!     stats_oz       - sums for various statistics as a function of level
!
!   output argument list:
!     stats_oz       - sums for various statistics as a function of level
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block
     
! !USES:

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,i_kind

  use constants, only : zero,half,one,two,tiny_r_kind
  use constants, only : rozcon,cg_term,wgtlim,h300,r10

  use obsmod, only : ozhead,oztail,i_oz_ob_type,dplat,nobskeep
  use obsmod, only : mype_diaghdr,dirname,time_offset,ianldate
  use obsmod, only : obsdiags,lobsdiag_allocated,lobsdiagsave
  use obsmod, only : oz_ob_type,nloz_omi
  use obsmod, only : obs_diag,luse_obsdiag

  use gsi_4dvar, only: nobs_bins,hr_obsbin

  use gridmod, only : get_ij,nsig

  use guess_grids, only : nfldsig,ges_prsi,ntguessig,hrdifsig

  use ozinfo, only : jpch_oz,error_oz,pob_oz,gross_oz,nusis_oz
  use ozinfo, only : iuse_oz,b_oz,pg_oz

  use jfunc, only : jiter,last,miter
  
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  implicit none
  
! !INPUT PARAMETERS:

  integer(i_kind)                  , intent(in   ) :: lunin  ! unit from which to read observations
  integer(i_kind)                  , intent(in   ) :: mype   ! mpi task id
  integer(i_kind)                  , intent(in   ) :: nlevs  ! number of levels (layer amounts + total column) per obs   
  integer(i_kind)                  , intent(in   ) :: nreal  ! number of pieces of non-ozone info (location, time, etc) per obs
  integer(i_kind)                  , intent(in   ) :: nobs   ! number of observations
  character(20)                    , intent(in   ) :: isis   ! sensor/instrument/satellite id
  integer(i_kind)                  , intent(in   ) :: is     ! integer(i_kind) counter for number of obs types to process

  character(10)                    , intent(in   ) :: obstype          ! type of ozone obs
  logical                          , intent(in   ) :: ozone_diagsave   ! switch on diagnostic output (.false.=no output)
  logical                          , intent(in   ) :: init_pass        ! state of "setup" processing

! !INPUT/OUTPUT PARAMETERS:

  real(r_kind),dimension(9,jpch_oz), intent(inout) :: stats_oz ! sums for various statistics as 
                                                               ! a function of level
!-------------------------------------------------------------------------

! Declare local parameters  
  integer(i_kind),parameter:: iint=1
  integer(i_kind),parameter:: ireal=3
  real(r_kind),parameter:: rmiss = -9999.9_r_kind
  character(len=*),parameter:: myname="setupozlay"

! Declare external calls for code analysis
  external:: intrp2a1
  external:: tintrp2a1
  external:: intrp3oz1
  external:: grdcrd1
  external:: stop2

! Declare local variables  
  
  real(r_kind) ozobs,omg,rat_err2,dlat,dtime,dlon
  real(r_kind) cg_oz,wgross,wnotgross,wgt,arg,exp_arg,term
  real(r_kind) psi,errorinv
  real(r_kind),dimension(nlevs):: ozges,varinv3,ozone_inv
  real(r_kind),dimension(nlevs):: ratio_errors,error
  real(r_kind),dimension(nlevs-1):: ozp
  real(r_kind),dimension(nloz_omi) :: ozp_omi
  real(r_kind),dimension(nlevs):: pobs,gross,tnoise
  real(r_kind),dimension(nreal+nlevs,nobs):: data
  real(r_kind),dimension(nsig+1)::prsitmp
  real(r_single),dimension(nlevs):: pob4,grs4,err4
  real(r_single),dimension(ireal,nobs):: diagbuf
  real(r_single),allocatable,dimension(:,:,:)::rdiagbuf
  real(r_kind),dimension(nloz_omi):: apriori, efficiency,pob_oz_omi
  real(r_kind),dimension(nloz_omi+1):: ozges1

  integer(i_kind) i,nlev,ii,jj,iextra,istat,ibin, kk
  integer(i_kind) k,j,nz,jc,idia,irdim1,istatus
  integer(i_kind) ioff,itoss,ikeep,ierror_toq,ierror_poq
  integer(i_kind) isolz,ifovn,itoqf
  integer(i_kind) mm1,itime,ilat,ilon,ilate,ilone,itoq,ipoq
  integer(i_kind),dimension(iint,nobs):: idiagbuf
  integer(i_kind),dimension(nlevs):: ipos,iouse,ikeepk

  real(r_kind),dimension(4):: tempwij
  integer(i_kind) nlevp,nlayers
  
  character(12) string
  character(10) filex
  character(128) diag_ozone_file

  logical:: ozdiagexist
  logical,dimension(nobs):: luse
  logical:: l_may_be_passive, proceed

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(oz_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_oz

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  n_alloc(:)=0
  m_alloc(:)=0

  mm1=mype+1


!
!*********************************************************************************
! Initialize arrays
  do j=1,nlevs
     ipos(j)=0
     iouse(j)=-2
     tnoise(j)=1.e10_r_kind
     gross(j)=1.e10_r_kind
     pobs(j)=1.e10_r_kind
  end do

  if(ozone_diagsave)then
     irdim1=6
     if(lobsdiagsave) irdim1=irdim1+4*miter+1
     allocate(rdiagbuf(irdim1,nlevs,nobs))
  end if

! Locate data for satellite in ozinfo arrays
  itoss =1
  l_may_be_passive=.false.
  jc=0
  do j=1,jpch_oz
     if (isis == nusis_oz(j)) then
        jc=jc+1
        if (jc > nlevs) then
           write(6,*)'SETUPOZ:  ***ERROR*** in level numbers, jc,nlevs=',jc,nlevs,&
                ' ***STOP IN SETUPOZ***'
           call stop2(71)
        endif
        ipos(jc)=j

        iouse(jc)=iuse_oz(j)
        tnoise(jc)=error_oz(j)
        gross(jc)=min(r10*gross_oz(j),h300)
        if (obstype == 'sbuv2' ) then
           pobs(jc)=pob_oz(j) * 1.01325_r_kind
        else
           pobs(jc)=pob_oz(j)
        endif

        if (iouse(jc)<-1 .or. (iouse(jc)==-1 .and. &
             .not.ozone_diagsave)) then
           tnoise(jc)=1.e10_r_kind
           gross(jc) =1.e10_r_kind
        endif
        if (iouse(jc)>-1) l_may_be_passive=.true.
        if (tnoise(jc)<1.e4_r_kind) itoss=0
     endif
  end do
  nlev=jc

! Handle error conditions
  if (nlevs>nlev) write(6,*)'SETUPOZ:  level number reduced for ',obstype,' ', &
       nlevs,' --> ',nlev
  if (nlev == 0) then
     if (mype==0) write(6,*)'SETUPOZ:  no levels found for ',isis
     if (nobs>0) read(lunin) 
     goto 135
  endif
  if (itoss==1) then
     if (mype==0) write(6,*)'SETUPOZ:  all obs variances > 1.e4.  Do not use ',&
          'data from satellite ',isis
     if (nobs>0) read(lunin)
     goto 135
  endif

! Read and transform ozone data
  read(lunin) data,luse

!    index information for data array (see reading routine)
  itime=2     ! index of analysis relative obs time
  ilon=3      ! index of grid relative obs location (x)
  ilat=4      ! index of grid relative obs location (y)
  ilone=5     ! index of earth relative longitude (degrees)
  ilate=6     ! index of earth relative latitude (degrees)
  itoq=7      ! index of total ozone error flag (sbuv2 only)
  ipoq=8      ! index of profile ozone error flag (sbuv2 only)
  isolz=8     ! index of solar zenith angle   (gome and omi only)
  itoqf=9     ! index of row anomaly           (omi only)
  ifovn=14    ! index of scan position (gome and omi only)


! If requested, save data for diagnostic ouput
  if(ozone_diagsave)ii=0

! Convert observation (lat,lon) from earth to grid relative values
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)

     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        dtime=data(itime,i)
 
        if (obstype == 'sbuv2' ) then
           if (nobskeep>0) then
              write(6,*)'setupozlay: nobskeep',nobskeep
              call stop2(259)
           end if
 
           ierror_toq = nint(data(itoq,i))
           ierror_poq = nint(data(ipoq,i))

!          Note:  ozp as log(pobs)
           call intrp2a1(ges_prsi(1,1,1,ntguessig),prsitmp,dlat,&
             dlon,nsig+1,mype)
  
!          Map observation pressure to guess vertical coordinate
           psi=one/(prsitmp(1)*r10)  ! factor of 10 converts to hPa
           do nz=1,nlevs-1
              if ((pobs(nz)*psi) < one) then
                 ozp(nz) = pobs(nz)/r10
              else
                 ozp(nz) = prsitmp(1)
              end if
              call grdcrd1(ozp(nz),prsitmp,nsig+1,-1)
           enddo
        end if
       
        if (obstype == 'omieff' .or. obstype == 'tomseff') then
           pob_oz_omi(nloz_omi) = 1000.0_r_kind* 1.01325_r_kind
           do j=nloz_omi-1, 1, -1
              pob_oz_omi(j) = pob_oz_omi(j+1)/2.0
           enddo
           call intrp2a1(ges_prsi(1,1,1,ntguessig),prsitmp,dlat,&
                dlon,nsig+1,mype)
        
!          Map observation pressure to guess vertical coordinate
           psi=one/(prsitmp(1)*r10)  ! factor of 10 converts to hPa
           do nz=1,nloz_omi - 1
              if ((pob_oz_omi(nz)*psi) < one) then
                 ozp_omi(nz) = pob_oz_omi(nz)/r10
              else
                 ozp_omi(nz) = prsitmp(1)
              end if
              call grdcrd1(ozp_omi(nz),prsitmp,nsig+1,-1)
           enddo
           ozp_omi(nloz_omi) = prsitmp(1)
           call grdcrd1(ozp_omi(nloz_omi),prsitmp,nsig+1,-1)
        end if
        
        if (obstype /= 'omieff' .and. obstype /= 'tomseff') then
           call intrp3oz1(ges_oz,ozges,dlat,dlon,ozp,dtime,&
                nlevs,mype)
        endif

        

        if(ozone_diagsave .and. luse(i))then
           ii=ii+1
           idiagbuf(1,ii)=mype                  ! mpi task number
           diagbuf(1,ii) = data(ilate,i)        ! lat (degree)
           diagbuf(2,ii) = data(ilone,i)        ! lon (degree)
           diagbuf(3,ii) = data(itime,i)-time_offset ! time (hours relative to analysis)
        endif

!       Interpolate interface pressure to obs location
!       Calculate innovations, perform gross checks, and accumualte
!       numbers for statistics

!       For OMI/GOME, nlev=1 
        do k=1,nlev
           j=ipos(k)
           if (obstype == 'omieff' .or. obstype == 'tomseff' ) then
              ioff=ifovn+1 !
           else
              ioff=nreal+k ! SBUV and OMI w/o efficiency factors
           endif

!          Compute innovation and load obs error into local array
            ! KW  OMI and TOMS have averaging kernels
           if (obstype == 'omieff' .or. obstype == 'tomseff' ) then
              ! everything in data is from top to bottom
              nlayers = nloz_omi + 1
              apriori(1:nloz_omi) = data(ioff:ioff+nloz_omi -1, i)
              ioff = ioff + nloz_omi
              efficiency(1:nloz_omi) = data(ioff:ioff+nloz_omi -1, i)
              ! Compute ozges
              call intrp3oz1(ges_oz,ozges1,dlat,dlon,ozp_omi,dtime,&
                nlayers,mype)
              ozges(k) = zero
              do kk = 1, nloz_omi
                 ozges(k) = ozges(k) + apriori(kk) + efficiency(kk)*(ozges1(kk)-apriori(kk))
              end do
              ioff = 37_i_kind
              ozobs = data(ioff,i)
           else ! Applying averaging kernels for OMI
              apriori(1:nloz_omi) = -99.99 ! this will identify non-OMIEFF data for intoz
              ozobs = data(ioff,i)
           endif

           ozone_inv(k) = ozobs-ozges(k)
           error(k)     = tnoise(k)

!          Set inverse obs error squared and ratio_errors
           if (error(k)<1.e4_r_kind) then
              varinv3(k) = one/(error(k)**2)
              ratio_errors(k) = one
           else
              varinv3(k) = zero
              ratio_errors(k) = zero
           endif

!          Perform gross check
           if(abs(ozone_inv(k)) > gross(k) .or. ozobs > 1000._r_kind .or. &
                ozges(k)<tiny_r_kind) then
              varinv3(k)=zero
              ratio_errors(k)=zero
!             write(6,*)'SETUPOZ:  reset O3 varinv3=',varinv3(k)
              if(luse(i))stats_oz(2,j) = stats_oz(2,j) + one ! number of obs tossed
           endif

!          Accumulate numbers for statistics
           rat_err2 = ratio_errors(k)**2
           if (varinv3(k)>tiny_r_kind .or. &
                (iouse(k)==-1 .and. ozone_diagsave)) then
              if(luse(i))then
                 omg=ozone_inv(k)
                 stats_oz(1,j) = stats_oz(1,j) + one                          ! # obs
                 stats_oz(3,j) = stats_oz(3,j) + omg                          ! (o-g)
                 stats_oz(4,j) = stats_oz(4,j) + omg*omg                      ! (o-g)**2
                 stats_oz(5,j) = stats_oz(5,j) + omg*omg*varinv3(k)*rat_err2  ! penalty
                 stats_oz(6,j) = stats_oz(6,j) + ozobs                        ! obs

                 exp_arg = -half*varinv3(k)*omg**2
                 errorinv = sqrt(varinv3(k))
                 if (pg_oz(j) > tiny_r_kind .and. errorinv > tiny_r_kind) then
                    arg  = exp(exp_arg)
                    wnotgross= one-pg_oz(j)
                    cg_oz=b_oz(j)*errorinv
                    wgross = cg_term*pg_oz(j)/(cg_oz*wnotgross)
                    term = log((arg+wgross)/(one+wgross))
                    wgt  = one-wgross/(arg+wgross)
                 else
                    term = exp_arg
                    wgt  = one
                 endif
                 stats_oz(8,j) = stats_oz(8,j) -two*rat_err2*term
                 if(wgt < wgtlim) stats_oz(9,j)=stats_oz(9,j)+one
              end if
           endif

!          If not assimilating this observation, reset inverse variance to zero
           if (iouse(k)<1) then
              varinv3(k)=zero
              ratio_errors(k)=zero
              rat_err2 = zero
           end if
           if (rat_err2*varinv3(k)>tiny_r_kind .and. luse(i)) &
              stats_oz(7,j) = stats_oz(7,j) + one

!          Optionally save data for diagnostics
           if (ozone_diagsave .and. luse(i)) then
              rdiagbuf(1,k,ii) = ozobs
              rdiagbuf(2,k,ii) = ozone_inv(k)           ! obs-ges
              errorinv = sqrt(varinv3(k)*rat_err2)
              rdiagbuf(3,k,ii) = errorinv               ! inverse observation error
              if (obstype == 'gome' .or. obstype == 'omieff'  .or. &
                  obstype == 'omi'  .or. obstype == 'tomseff' ) then
                 rdiagbuf(4,k,ii) = data(isolz,i)       ! solar zenith angle
                 rdiagbuf(5,k,ii) = data(ifovn,i)       ! field of view number
              else
                 rdiagbuf(4,k,ii) = rmiss                
                 rdiagbuf(5,k,ii) = rmiss               
              endif
              if (obstype == 'omieff' .or. obstype == 'omi' ) then
                 rdiagbuf(6,k,ii) = data(itoqf,i)       ! row anomaly index
              else
                 rdiagbuf(6,k,ii) = rmiss                
              endif
           endif

        end do
!       Check all information for obs.  If there is at least one piece of
!       information that passed quality control, use this observation.
        ikeepk=0
        do k=1,nlevs
           if ((ratio_errors(k)**2)*varinv3(k)>1.e-10_r_kind) ikeepk(k)=1
        end do
        ikeep=maxval(ikeepk)
     endif ! (in_curbin)

!    In principle, we want ALL obs in the diagnostics structure but for
!    passive obs (monitoring), it is difficult to do if rad_diagsave
!    is not on in the first outer loop. For now we use l_may_be_passive...
     if (l_may_be_passive) then
!       Link observation to appropriate observation bin
        if (nobs_bins>1) then
           ibin = NINT( dtime/hr_obsbin ) + 1
        else
           ibin = 1
        endif
        IF (ibin<1.OR.ibin>nobs_bins) write(6,*)'SETUPOZLAY: ',mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

        if(in_curbin) then
!          Process obs have at least one piece of information that passed qc checks
           if (.not. last .and. ikeep==1) then
 
              if(.not. associated(ozhead(ibin)%head))then
                 allocate(ozhead(ibin)%head,stat=istat)
                 if(istat /= 0)write(6,*)' failure to write ozhead '
                 oztail(ibin)%head => ozhead(ibin)%head
              else
                 allocate(oztail(ibin)%head%llpoint,stat=istat)
                 if(istat /= 0)write(6,*)' failure to write oztail%llpoint '
                 oztail(ibin)%head => oztail(ibin)%head%llpoint
              end if
 
              m_alloc(ibin) = m_alloc(ibin) +1
              my_head => oztail(ibin)%head
              my_head%idv = is
              my_head%iob = i

              nlevp=max(nlev-1,1)
              if (obstype == 'omieff' .or. obstype == 'tomseff' ) nlevp = nloz_omi
              allocate(oztail(ibin)%head%res(nlev),&
                       oztail(ibin)%head%err2(nlev),oztail(ibin)%head%raterr2(nlev),&
                       oztail(ibin)%head%prs(nlevp), &
                       oztail(ibin)%head%wij(4,nsig), &
                       oztail(ibin)%head%ipos(nlev),  &
                       oztail(ibin)%head%apriori(nloz_omi), &
                       oztail(ibin)%head%efficiency(nloz_omi), stat=istatus)
              if(luse_obsdiag)allocate(oztail(ibin)%head%diags(nlev))
              if (istatus/=0) write(6,*)'SETUPOZ:  allocate error for oz_point, istatus=',istatus

!             Set number of levels for this obs
              oztail(ibin)%head%nloz = nlev-1  ! NOTE: for OMI/GOME, nloz=0

!             Set (i,j) indices of guess gridpoint that bound obs location
              call get_ij(mm1,dlat,dlon,oztail(ibin)%head%ij(1),tempwij(1))

              call tintrp2a1(ges_prsi,prsitmp,dlat,dlon,dtime,hrdifsig,&
                   nsig+1,mype,nfldsig)

              do k = 1,nsig
                 oztail(ibin)%head%wij(1,k)=tempwij(1)*rozcon*(prsitmp(k)-prsitmp(k+1))
                 oztail(ibin)%head%wij(2,k)=tempwij(2)*rozcon*(prsitmp(k)-prsitmp(k+1))
                 oztail(ibin)%head%wij(3,k)=tempwij(3)*rozcon*(prsitmp(k)-prsitmp(k+1))
                 oztail(ibin)%head%wij(4,k)=tempwij(4)*rozcon*(prsitmp(k)-prsitmp(k+1))
              end do

!             Increment data counter and save information used in
!             inner loop minimization (int* and stp* routines)
 
              oztail(ibin)%head%luse=luse(i)
              oztail(ibin)%head%time=dtime

              if (obstype == 'sbuv2' ) then
                 do k=1,nlevs-1
                    oztail(ibin)%head%prs(k) = ozp(k)
                 enddo
              else if (obstype == 'omieff' .or. obstype == 'tomseff') then
                 do k=1,nloz_omi
                    oztail(ibin)%head%prs(k) = ozp_omi(k)
                 enddo
              else ! GOME or OMI w/o efficiency factors

                 oztail(ibin)%head%prs(1) = zero   ! any value is OK, never used
              endif  
              

           endif ! < .not.last >
        endif ! (in_curbin)

!       Link obs to diagnostics structure
        do k=1,nlevs
           if(luse_obsdiag)then
              if (.not.lobsdiag_allocated) then
                 if (.not.associated(obsdiags(i_oz_ob_type,ibin)%head)) then
                    allocate(obsdiags(i_oz_ob_type,ibin)%head,stat=istat)
                    if (istat/=0) then
                       write(6,*)'setupozlay: failure to allocate obsdiags',istat
                       call stop2(260)
                    end if
                    obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%head
                 else
                    allocate(obsdiags(i_oz_ob_type,ibin)%tail%next,stat=istat)
                    if (istat/=0) then
                       write(6,*)'setupozlay: failure to allocate obsdiags',istat
                       call stop2(261)
                    end if
                    obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%tail%next
                 end if

                 allocate(obsdiags(i_oz_ob_type,ibin)%tail%muse(miter+1))
                 allocate(obsdiags(i_oz_ob_type,ibin)%tail%nldepart(miter+1))
                 allocate(obsdiags(i_oz_ob_type,ibin)%tail%tldepart(miter))
                 allocate(obsdiags(i_oz_ob_type,ibin)%tail%obssen(miter))
                 obsdiags(i_oz_ob_type,ibin)%tail%indxglb=i
                 obsdiags(i_oz_ob_type,ibin)%tail%nchnperobs=-99999
                 obsdiags(i_oz_ob_type,ibin)%tail%luse=.false.
                 obsdiags(i_oz_ob_type,ibin)%tail%muse(:)=.false.
                 obsdiags(i_oz_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
                 obsdiags(i_oz_ob_type,ibin)%tail%tldepart(:)=zero
                 obsdiags(i_oz_ob_type,ibin)%tail%wgtjo=-huge(zero)
                 obsdiags(i_oz_ob_type,ibin)%tail%obssen(:)=zero

                 n_alloc(ibin) = n_alloc(ibin) +1
                 my_diag => obsdiags(i_oz_ob_type,ibin)%tail
                 my_diag%idv = is
                 my_diag%iob = i
                 my_diag%ich = k
              else
                 if (.not.associated(obsdiags(i_oz_ob_type,ibin)%tail)) then
                    obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%head
                 else
                    obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%tail%next
                 end if
                 if (obsdiags(i_oz_ob_type,ibin)%tail%indxglb/=i) then
                    write(6,*)'setupozlay: index error'
                    call stop2(262)
                 end if
              endif
           end if

           if(in_curbin) then
              if(luse_obsdiag)then
                 obsdiags(i_oz_ob_type,ibin)%tail%luse=luse(i)
                 obsdiags(i_oz_ob_type,ibin)%tail%muse(jiter)= (ikeepk(k)==1)
                 obsdiags(i_oz_ob_type,ibin)%tail%nldepart(jiter)=ozone_inv(k)
                 obsdiags(i_oz_ob_type,ibin)%tail%wgtjo= varinv3(k)*ratio_errors(k)**2
              end if
 
              if (.not. last .and. ikeep==1) then
                 oztail(ibin)%head%ipos(k)    = ipos(k)
                 oztail(ibin)%head%res(k)     = ozone_inv(k)
                 oztail(ibin)%head%err2(k)    = varinv3(k)
                 oztail(ibin)%head%raterr2(k) = ratio_errors(k)**2
                 oztail(ibin)%head%apriori(1:nloz_omi) = apriori(1:nloz_omi)
                 oztail(ibin)%head%efficiency(1:nloz_omi) = efficiency(1:nloz_omi)
                 if(luse_obsdiag)then
                    oztail(ibin)%head%diags(k)%ptr => obsdiags(i_oz_ob_type,ibin)%tail

                    my_head => oztail(ibin)%head
                    my_diag => oztail(ibin)%head%diags(k)%ptr
                    if(my_head%idv /= my_diag%idv .or. &
                       my_head%iob /= my_diag%iob .or. &
                                 k /= my_diag%ich ) then
                       call perr(myname,'mismatching %[head,diags]%(idv,iob,ich,ibin) =', &
                             (/is,i,k,ibin/))
                       call perr(myname,'my_head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,k/))
                       call perr(myname,'my_diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
                       call die(myname)
                    endif
                 end if
              endif
       

              if (ozone_diagsave.and.lobsdiagsave.and.luse(i)) then
                 idia=6
                 do jj=1,miter
                    idia=idia+1
                    if (obsdiags(i_oz_ob_type,ibin)%tail%muse(jj)) then
                       rdiagbuf(idia,k,ii) = one
                    else
                       rdiagbuf(idia,k,ii) = -one
                    endif
                 enddo
                 do jj=1,miter+1
                    idia=idia+1
                    rdiagbuf(idia,k,ii) = obsdiags(i_oz_ob_type,ibin)%tail%nldepart(jj)
                 enddo
                 do jj=1,miter
                    idia=idia+1
                    rdiagbuf(idia,k,ii) = obsdiags(i_oz_ob_type,ibin)%tail%tldepart(jj)
                 enddo
                 do jj=1,miter
                    idia=idia+1
                    rdiagbuf(idia,k,ii) = obsdiags(i_oz_ob_type,ibin)%tail%obssen(jj)
                 enddo
              endif
           endif ! (in_curbin)

        enddo ! < over nlevs >

     else

        if(in_curbin) then
           if (ozone_diagsave.and.lobsdiagsave.and.luse(i)) then
              rdiagbuf(7:irdim1,1:nlevs,ii) = zero
           endif
        endif ! (in_curbin)
 
     endif ! < l_may_be_passive >

  end do   ! end do i=1,nobs

! If requested, write to diagnostic file
  if (ozone_diagsave .and. ii>0) then
     filex=obstype
     write(string,100) jiter
100  format('_',i2.2)
     diag_ozone_file = trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // (string)
     if(init_pass) then
        open(4,file=diag_ozone_file,form='unformatted',status='unknown',position='rewind')
     else
        inquire(file=diag_ozone_file,exist=ozdiagexist)
        if (ozdiagexist) then
           open(4,file=diag_ozone_file,form='unformatted',status='old',position='append')
        else
           open(4,file=diag_ozone_file,form='unformatted',status='unknown',position='rewind')
        endif
     endif
     iextra=0
     if (init_pass .and. mype==mype_diaghdr(is)) then
        write(4) isis,dplat(is),obstype,jiter,nlevs,ianldate,iint,ireal,iextra
        write(6,*)'SETUPOZ:   write header record for ',&
             isis,iint,ireal,iextra,' to file ',trim(diag_ozone_file),' ',ianldate
        do i=1,nlevs
           pob4(i)=pobs(i)
           grs4(i)=gross(i)
           err4(i)=tnoise(i)
        end do
        write(4) pob4,grs4,err4,iouse
     endif
     write(4) ii
     write(4) idiagbuf(:,1:ii),diagbuf(:,1:ii),rdiagbuf(:,:,1:ii)
     close(4)
  endif

! Jump to this line if problem with data
135 continue        

! Release memory of local guess arrays
  call final_vars_

! clean up
  call dtime_show('setupozlay','diagsave:oz',i_oz_ob_type)
  if(ozone_diagsave) deallocate(rdiagbuf)

! End of routine
  return

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::oz', ivar, istatus )
  proceed=ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get oz ...
     varname='oz'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_oz))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_oz(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_oz(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_oz(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_oz)) deallocate(ges_oz)
  end subroutine final_vars_

end subroutine setupozlay


subroutine setupozlev(lunin,mype,stats_oz,nlevs,nreal,nobs,&
     obstype,isis,is,ozone_diagsave,init_pass)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setupozlev --- Compute rhs of oi for mls ozone mixing ratio obs at pressure levels
!
!   prgrmmr:     H.Liu          org: np22                date: 2010-10-18
!
! abstract:      For sbuv ozone observations (layer amounts and total 
!                column, this routine 
!                  a) reads obs assigned to given mpi task (geographic region),
!                  b) simulates obs from guess,
!                  c) apply some quality control to obs,
!                  d) load weight and innovation arrays used in minimization
!                  e) collects statistics for runtime diagnostic output
!                  f) writes additional diagnostic information to output file
!
! program history log:
!   2010-10-18  h.liu   - subroutine for mls data: based on setupoz and Sienkiewicz's setupo3lv
!   2013-10-19  todling - metguess now holds background
!   2013-11-26  guo     - removed nkeep==0 escaping to allow more than one obstype sources.
!
!   input argument list:
!     lunin          - unit from which to read observations
!     mype           - mpi task id
!     nlevs          - number of levels (layer amounts + total column) per obs   
!     nreal          - number of pieces of non-ozone info (location, time, etc) per obs
!     nobs           - number of observations
!     isis           - sensor/instrument/satellite id
!     is             - integer(i_kind) counter for number of obs types to process
!     obstype        - type of ozone obs
!     ozone_diagsave - switch on diagnostic output (.false.=no output)
!     stats_oz       - sums for various statistics as a function of level
!
!   output argument list:
!     stats_oz       - sums for various statistics as a function of level
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block
     
! !USES:

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,i_kind

  use obsmod, only : o3lhead,o3ltail,i_o3l_ob_type,dplat,nobskeep
  use obsmod, only : mype_diaghdr,dirname,time_offset,ianldate
  use obsmod, only : obsdiags,lobsdiag_allocated,lobsdiagsave
  use obsmod, only : o3l_ob_type
  use obsmod, only : obs_diag,luse_obsdiag

  use guess_grids, only : nfldsig,ges_lnprsl,hrdifsig

  use constants, only : zero,half,one,two,tiny_r_kind,four
  use constants, only : cg_term,wgtlim,r10,constoz

  use gsi_4dvar, only: nobs_bins,hr_obsbin

  use gridmod, only : get_ijk,nsig

  use ozinfo, only : jpch_oz
  use ozinfo, only : b_oz,pg_oz

  use jfunc, only : jiter,last,miter
  
  use m_dtime, only: dtime_setup, dtime_check, dtime_show

  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  implicit none
  
! !INPUT PARAMETERS:

  integer(i_kind)                  , intent(in   ) :: lunin  ! unit from which to read observations
  integer(i_kind)                  , intent(in   ) :: mype   ! mpi task id
  integer(i_kind)                  , intent(in   ) :: nlevs  ! number of levels (layer amounts + total column) per obs   
  integer(i_kind)                  , intent(in   ) :: nreal  ! number of pieces of non-ozone info (location, time, etc) per obs
  integer(i_kind)                  , intent(in   ) :: nobs   ! number of observations
  character(20)                    , intent(in   ) :: isis   ! sensor/instrument/satellite id
  integer(i_kind)                  , intent(in   ) :: is     ! integer(i_kind) counter for number of obs types to process

  character(10)                    , intent(in   ) :: obstype          ! type of ozone obs
  logical                          , intent(in   ) :: ozone_diagsave   ! switch on diagnostic output (.false.=no output)
  logical                          , intent(in   ) :: init_pass        ! state of "setup" processing

! !INPUT/OUTPUT PARAMETERS:

  real(r_kind),dimension(9,jpch_oz), intent(inout) :: stats_oz ! sums for various statistics as 
                                                               ! a function of level
!-------------------------------------------------------------------------

! Declare local parameters  
  integer(i_kind),parameter:: iint=1
  integer(i_kind),parameter:: ireal=3
  real(r_kind),parameter:: rmiss = -9999.9_r_kind
  character(len=*),parameter:: myname="setupozlev"

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: tintrp3
  external:: grdcrd1
  external:: stop2

! Declare local variables  
  
  real(r_kind) o3ges, o3ppmv
  real(r_kind) rlow,rhgh,sfcchk
  real(r_kind) omg,rat_err2,dlat,dtime,dlon
  real(r_kind) cg_oz,wgross,wnotgross,wgt,arg,exp_arg,term
  real(r_kind) errorinv
  real(r_kind) psges,ozlv
  
  real(r_kind) varinv3,ratio_errors
  real(r_kind) dpres,obserror,ozone_inv,preso3l
  real(r_kind),dimension(nreal+nlevs,nobs):: data
  real(r_kind),dimension(nsig):: prsltmp
  real(r_single),dimension(ireal,nobs):: diagbuf
  real(r_single),allocatable,dimension(:,:,:)::rdiagbuf

  integer(i_kind) i,ii,jj,iextra,istat,ibin
  integer(i_kind) k,j,idia,irdim1
  integer(i_kind) isolz,iuse
  integer(i_kind) mm1,itime,ilat,ilon,ilate,ilone,iozmr,ilev,ipres,iprcs,imls_levs
  integer(i_kind),dimension(iint,nobs):: idiagbuf

  character(12) string
  character(10) filex
  character(128) diag_ozone_file

  logical,dimension(nobs):: luse,muse
  logical proceed

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(o3l_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_oz

! Check to see if required guess fields are available
! Question: Should a message be produced before return, to inform the
! system what has been going on?
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  n_alloc(:)=0
  m_alloc(:)=0

  mm1=mype+1


!
!*********************************************************************************
! Initialize arrays

  if(ozone_diagsave)then
     irdim1=6
     if(lobsdiagsave) irdim1=irdim1+4*miter+1
     allocate(rdiagbuf(irdim1,1,nobs))
     rdiagbuf=0._r_single
  end if

! index information for data array (see reading routine)
  itime=2     ! index of analysis relative obs time
  ilon=3      ! index of grid relative obs location (x)
  ilat=4      ! index of grid relative obs location (y)
  ilone=5     ! index of earth relative longitude (degrees)
  ilate=6     ! index of earth relative latitude (degrees)
  isolz=7     ! index of solar zenith angle 
  iuse=8      ! index of usage flag
  ipres=9     ! index of pressure in log(cb)
  iprcs=10    ! index of mixing ratio precision in ppmv
  ilev=11     ! index of obs level
  imls_levs=12 ! index of mls nrt vertical levels
  iozmr=13    ! index of ozone mixing ratio in ppmv

! Read and transform ozone data
  read(lunin) data,luse

! Set flag for obs use
  do i=1,nobs
     muse(i)=nint(data(iuse,i))<=jiter
  end do

! If requested, save data for diagnostic ouput
  if(ozone_diagsave)ii=0

! Convert observation (lat,lon) from earth to grid relative values
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)

     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     dpres=data(ipres,i)   !pressure in log(cb)
     preso3l =r10*exp(dpres)

     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dtime=data(itime,i)
     obserror=data(iprcs,i)
              
     if (nobskeep>0) then
        write(6,*)'setupozlev: nobskeep',nobskeep
        call stop2(338)
     end if
 
!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*) 'SETUPOZLEV: ', mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     if(luse_obsdiag)then
        if (.not.lobsdiag_allocated) then
           if (.not.associated(obsdiags(i_o3l_ob_type,ibin)%head)) then
              allocate(obsdiags(i_o3l_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupozlev: failure to allocate obsdiags',istat
                 call stop2(256)
              end if
              obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%head
           else
              allocate(obsdiags(i_o3l_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupozlev: failure to allocate obsdiags',istat
                 call stop2(257)
              end if
              obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%tail%next
           end if

           allocate(obsdiags(i_o3l_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_o3l_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_o3l_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_o3l_ob_type,ibin)%tail%indxglb=i
           obsdiags(i_o3l_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_o3l_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_o3l_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_o3l_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_o3l_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_o3l_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_o3l_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = i
           my_diag%ich = 1

        else
           if (.not.associated(obsdiags(i_o3l_ob_type,ibin)%tail)) then
              obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%head
           else
              obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_o3l_ob_type,ibin)%tail%indxglb/=i) then
              write(6,*)'setupozlev: index error'
              call stop2(258)
           end if
        endif
     endif

     if(.not.in_curbin) cycle

!    Interpolate ps to obs locations/times
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig, &
          mype,nfldsig)

!    Interpolate log(pres) at mid-layers to obs locations/times
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig, &
          nsig,mype,nfldsig)

!    Get approximate k value of surface by using surface pressure
!    for surface check.
     sfcchk=log(psges)
     call grdcrd1(sfcchk,prsltmp,nsig,-1)

     if(ozone_diagsave .and. luse(i))then
        ii=ii+1
        idiagbuf(1,ii)=mype                  ! mpi task number
        diagbuf(1,ii) = data(ilate,i)        ! lat (degree)
        diagbuf(2,ii) = data(ilone,i)        ! lon (degree)
        diagbuf(3,ii) = data(itime,i)-time_offset ! time (hours relative to analysis)
     endif

     ozlv=data(iozmr,i)      ! ozone mixing ratio in ppmv at pressure level

!    Pressure level of data (dpres) converted to grid coordinate
!    (wrt mid-layer pressure)
     call grdcrd1(dpres,prsltmp,nsig,-1)

!    Check if observation above model top or below model surface

     rlow=max(sfcchk-dpres,zero)
     rhgh=max(dpres-0.001_r_kind-float(nsig),zero)

!    calculate factor for error adjustment if too (high,low)
     ratio_errors=obserror/(obserror+1.0e6_r_kind*rhgh+four*rlow)

!    Check to see if observations is above the top of the model
     if (dpres > float(nsig)) then
         ratio_errors=zero
         obserror=1.0e6_r_kind
     end if

!    Interpolate guess ozone to observation location and time
     call tintrp31(ges_oz,o3ges,dlat,dlon,dpres,dtime, &
       hrdifsig,mype,nfldsig)

!    Compute innovations - background o3ges in g/g so adjust units
!    Leave increment in ppmv for gross checks,  etc.

     o3ppmv = o3ges * constoz
     ozone_inv=ozlv - o3ppmv

!    Perform gross checks, and accumualte numbers for statistics

     j=nint(data(ilev,i))  !the entry # in ozinfo.txt

!    Set inverse obs error squared and ratio_errors
     if (obserror>zero .and. obserror<1.e4_r_kind) then
        varinv3 = one/(obserror**2)
        ratio_errors = one*ratio_errors
     else
        varinv3 = zero
        ratio_errors = zero
     endif

!    toss the obs not recommended by the data provider
     if (nint(data(iuse,i)) == 1000 ) then
        varinv3=zero
        ratio_errors=zero
     endif

!    Perform gross check
     if( ozlv > 1.e+02_r_kind ) then
        varinv3=zero
        ratio_errors=zero
        if(luse(i))stats_oz(2,j) = stats_oz(2,j) + one ! number of obs tossed
     endif

!    check if gross check failed, mark failed obs for non-use
     if (ratio_errors/obserror <=tiny_r_kind) then
         muse(i)=.false.
     end if

!    Accumulate numbers for statistics
     rat_err2 = ratio_errors**2
     if (varinv3>tiny_r_kind .or. ozone_diagsave) then
        if(luse(i))then
           omg=ozone_inv
           stats_oz(1,j) = stats_oz(1,j) + one                          ! # obs
           stats_oz(3,j) = stats_oz(3,j) + omg                          ! (o-g)
           stats_oz(4,j) = stats_oz(4,j) + omg*omg                      ! (o-g)**2
           stats_oz(5,j) = stats_oz(5,j) + omg*omg*varinv3*rat_err2  ! penalty
           stats_oz(6,j) = stats_oz(6,j) + ozlv                        ! obs

           exp_arg = -half*varinv3*omg**2
           errorinv = sqrt(varinv3)
           if (pg_oz(j) > tiny_r_kind .and. errorinv > tiny_r_kind) then
              arg  = exp(exp_arg)
              wnotgross= one-pg_oz(j)
              cg_oz=b_oz(j)*errorinv
              wgross = cg_term*pg_oz(j)/(cg_oz*wnotgross)
              term = log((arg+wgross)/(one+wgross))
              wgt  = one-wgross/(arg+wgross)
           else
              term = exp_arg
              wgt  = one
           endif

           stats_oz(8,j) = stats_oz(8,j) -two*rat_err2*term
           if(wgt < wgtlim) stats_oz(9,j)=stats_oz(9,j)+one
        end if
     endif

!    If not assimilating this observation, reset inverse variance to zero
     if ( .not. muse(i)) then
        varinv3=zero
        ratio_errors=zero
        rat_err2 = zero
     end if
     if (rat_err2*varinv3>tiny_r_kind .and. luse(i)) &
        stats_oz(7,j) = stats_oz(7,j) + one

     if(luse_obsdiag)then
        obsdiags(i_o3l_ob_type,ibin)%tail%luse=luse(i)
        obsdiags(i_o3l_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(jiter)=ozone_inv
        obsdiags(i_o3l_ob_type,ibin)%tail%wgtjo= varinv3*ratio_errors**2
     end if

     if (.not. last .and. muse(i) ) then

        if(.not. associated(o3lhead(ibin)%head))then
           allocate(o3lhead(ibin)%head,stat=istat)
           if(istat /= 0)write(6,*)' failure to write o3lhead '
           o3ltail(ibin)%head => o3lhead(ibin)%head
        else
           allocate(o3ltail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write o3ltail%llpoint '
           o3ltail(ibin)%head => o3ltail(ibin)%head%llpoint
        end if
 
        m_alloc(ibin) = m_alloc(ibin) +1
        my_head => o3ltail(ibin)%head
        my_head%idv = is
        my_head%iob = i

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        call get_ijk(mm1,dlat,dlon,dpres,&
        o3ltail(ibin)%head%ij(1),o3ltail(ibin)%head%wij(1))

        do k=1,8
           o3ltail(ibin)%head%wij(k)=o3ltail(ibin)%head%wij(k)*constoz
        end do

        o3ltail(ibin)%head%res        = ozone_inv
        o3ltail(ibin)%head%err2       = varinv3
        o3ltail(ibin)%head%raterr2    = ratio_errors**2
        o3ltail(ibin)%head%luse       = luse(i)
        o3ltail(ibin)%head%time       = dtime
        o3ltail(ibin)%head%b          = b_oz(j)
        o3ltail(ibin)%head%pg         = pg_oz(j)

        if(luse_obsdiag)then
           o3ltail(ibin)%head%diags => obsdiags(i_o3l_ob_type,ibin)%tail

           my_head => o3ltail(ibin)%head
           my_diag => o3ltail(ibin)%head%diags
           if(my_head%idv /= my_diag%idv .or. &
              my_head%iob /= my_diag%iob ) then
              call perr(myname,'mismatching %[head,diags]%(idv,iob,ich,ibin) =', &
                    (/is,i,k,ibin/))
              call perr(myname,'my_head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,k/))
              call perr(myname,'my_diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
              call die(myname)
           endif
        endif

     endif

!    Optionally save data for diagnostics
     if (ozone_diagsave .and. luse(i)) then
        rdiagbuf(1,1,ii) = ozlv                ! obs
        rdiagbuf(2,1,ii) = ozone_inv           ! obs-ges
        errorinv = sqrt(varinv3*rat_err2)
        rdiagbuf(3,1,ii) = errorinv            ! inverse observation error
        rdiagbuf(4,1,ii) = preso3l             ! override solar zenith angle with a reference pressure (in hPa)
        rdiagbuf(5,1,ii) = rmiss               ! fovn
        rdiagbuf(6,1,ii) = obserror               ! ozone mixing ratio precision

        if (lobsdiagsave) then
           idia=6
           do jj=1,miter
              idia=idia+1
              if (obsdiags(i_o3l_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(idia,1,ii) = one
              else
                 rdiagbuf(idia,1,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              idia=idia+1
              rdiagbuf(idia,1,ii) = obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+1
              rdiagbuf(idia,1,ii) = obsdiags(i_o3l_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+1
              rdiagbuf(idia,1,ii) = obsdiags(i_o3l_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif
     end if   !end if(ozone_diagsave )

  end do   ! end do i=1,nobs

! If requested, write to diagnostic file
  if (ozone_diagsave .and. ii>0) then
     filex=obstype
     write(string,100) jiter
100  format('_',i2.2)
     diag_ozone_file = trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // (string)
     if(init_pass) then
        open(4,file=diag_ozone_file,form='unformatted',status='unknown',position='rewind')
     else
        open(4,file=diag_ozone_file,form='unformatted',status='old',position='append')
     endif
     iextra=0
     if (init_pass .and. mype==mype_diaghdr(is)) then
        write(4) isis,dplat(is),obstype,jiter,nlevs,ianldate,iint,ireal,iextra
        write(6,*)'SETUPOZLV:   write header record for ',&
             isis,iint,ireal,iextra,' to file ',trim(diag_ozone_file),' ',ianldate
     endif
     write(4) ii
     write(4) idiagbuf(:,1:ii),diagbuf(:,1:ii),rdiagbuf(:,1,1:ii)
     close(4)
  endif

! Release memory of local guess arrays
  call final_vars_

! clean up
  call dtime_show('setupozlev','diagsave:ozlv',i_o3l_ob_type)
  if(ozone_diagsave) deallocate(rdiagbuf)

! End of routine
  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::oz' , ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get oz ...
     varname='oz'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_oz))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_oz(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_oz(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_oz(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_oz)) deallocate(ges_oz)
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setupozlev
