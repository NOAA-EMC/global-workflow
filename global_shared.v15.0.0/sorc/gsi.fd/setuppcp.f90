!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  setuppcp --- Compute rhs of oi equation for precipitation
!
! !INTERFACE:
!
subroutine setuppcp(lunin,mype,aivals,nele,nobs,&
     obstype,isis,is,pcp_diagsave,init_pass)

! !USES:

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,i_kind

  use pcpinfo, only: deltim
  use pcpinfo, only: dtphys
  use pcpinfo, only: npcptype
  use pcpinfo, only: iusep
  use pcpinfo, only: xkt2d
  use pcpinfo, only: npredp
  use pcpinfo, only: predxp
  use pcpinfo, only: varchp
  use pcpinfo, only: gross_pcp
  use pcpinfo, only: b_pcp
  use pcpinfo, only: pg_pcp
  use pcpinfo, only: ibias
  use pcpinfo, only: nupcp

  use gridmod, only: nlat           ! no. lat
  use gridmod, only: nsig,nsig2,nsig3,nsig4,nsig5       ! no. levels
  use gridmod, only: rbs2           ! 1./sin(grid latitudes))**2
  use gridmod, only: istart         ! start lat of the whole array on each pe
  use gridmod, only: get_ij

  use guess_grids, only: geop_hgtl,hrdifsig,nfldsig
  use guess_grids, only: ges_prsl,ges_prsi,ges_tsen
  use guess_grids, only: isli2

  use derivsmod, only: drv_initialized
  use derivsmod, only: gsi_xderivative_bundle
  use derivsmod, only: gsi_yderivative_bundle
  use tendsmod, only: tnd_initialized
  use tendsmod, only: gsi_tendency_bundle

  use obsmod, only: ndat,dplat,pcphead,pcptail,time_offset
  use obsmod, only: i_pcp_ob_type,obsdiags,lobsdiagsave,ianldate
  use obsmod, only: mype_diaghdr,nobskeep,lobsdiag_allocated,dirname
  use obsmod, only: pcp_ob_type
  use obsmod, only: obs_diag,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin,l4dvar,l4densvar

  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  
  use constants, only: rd,cp,pi,zero,quarter,r60, &
       half,one,two,three,tiny_r_kind,one_tenth,cg_term,r1000,wgtlim,fv,r3600,r10

  use jfunc, only: jiter,miter

  use m_dtime, only: dtime_setup, dtime_check, dtime_show

  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  implicit none    ! Turn off implicit typing

! !INPUT PARAMETERS:

  integer(i_kind)                , intent(in   ) :: lunin          ! unit from which to read 
                                                                   !   precpitation observations
  integer(i_kind)                , intent(in   ) :: mype           ! mpi task id
  integer(i_kind)                , intent(in   ) :: nele           ! number of pieces of information 
                                                                   !   per precipitation observation
  integer(i_kind)                , intent(in   ) :: nobs           ! number of precipitation obs to process
  character(len=20)              , intent(in   ) :: isis           ! sensor/instrument/satellite id
  integer(i_kind)                , intent(in   ) :: is             ! counter for number of obs types to process
 

  character(10)                  , intent(in   ) :: obstype ! type of precipitation observation
 
  logical                        , intent(in   ) :: pcp_diagsave   ! switch diagnostic output on/off
                                                                   !   (.false.=no output)
  logical                        , intent(in   ) :: init_pass      ! state of "setup" processing


! !INPUT/OUTPUT PARAMETERS:

  real(r_kind),dimension(40,ndat), intent(inout) :: aivals ! array holding sums for
                                                           !  various statistical output

! !DESCRIPTION:  For precipitation rate observations, this routine
!  \begin{enumerate}
!        \item reads obs assigned to given mpi task (geographic region),
!        \item simulates obs from guess,
!        \item apply some quality control to obs,
!        \item load weight and innovation arrays used in minimization
!        \item collects statistics for runtime diagnostic output
!        \item writes additional diagnostic information to output file
!  \end{enumerate}
!
! !REVISION HISTORY:
!
!   1998-04-30  treadon
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-05-28  kleist, subroutine call update
!   2004-06-15  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue; added intent/only's; removed unused mods
!   2004-10-06  parrish - modifications for nonlinear qc
!   2004-10-28  treadon - replace tiny with tiny_r_kind
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - add outer loop number to name of diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-03-16  derber  - save observation time
!   2005-04-11  treadon - add logical to toggle on/off nonlinear qc code
!   2005-08-05  treadon - increase size of diag_pcp_sat character variable;
!                         remove mype and add idate to diag_pcp_sat header
!   2005-09-28  derber - modify for surface info from read routines and 
!                        additional input info from pcpinfo
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-17  treadon - generalize accessing of elements from obs array
!   2005-11-29  parrish - remove call to deter_sfc_reg (earlier patch for regional mode)
!   2006-01-09  treadon - introduce get_ij
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-03-21  treadon - add option to perturb observation
!   2006-04-12  treadon - replace 1d del,sl with 2d del0,sl0
!   2006-05-03  derber  - modify to write data in jppf chunks
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!                       - process one ob at a time
!   2006-07-31  kleist  - use separate derivative arrays, change to use ges_ps
!   2006-10-12  treadon - replace virtual temperature with sensible
!   2007-01-19  derber  - load log(one+satpcp) instead of satpcp into structure
!   2007-03-19  tremolet - binning of observations
!   2007-03-21      su  - remove option to perturb observation
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_pcp_file
!   2008-05-23  safford - rm unused vars and uses
!   2008-12-03  todling - changed handle of tail%time
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check(), and
!			  new arguments init_pass and last_pass.
!			- fixed a bug of using "i" instead of "n" when setting the
!			  vericiation index of obsdiag.
!   2009-12-08  guo     - cleaned diag output rewind with open(position='rewind')
!			- fixed a bug in diag header output while is not init_pass.
!   2011-05-01  todling - add metguess-bundle; cwmr no longer in guess-grids
!   2013-10-19  todling - metguess now holds background
!                         tendencies now in bundle
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-08-31  thomas - Added 4DEnVar to logical to exclude downweighting of
!                        obs away from center of window 
!
!
! !REMARKS:  This routine is NOT correctly set up if running
!            with the hybrid vertical coordinate, but will
!            essentially assign a sigma-like vertical structure
!            based on the 'last' observation location
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!    treadon          org: np23                 date: 1998-04-30
!
!EOP
!-------------------------------------------------------------------------

! Declare local parameters
  integer(i_kind),parameter:: iint=8
  integer(i_kind),parameter:: ncld=1

! Declare external calls for code analysis
  external:: stop2
  external:: pcp_k

! Declare local variables
  integer(i_kind) isatid,itime,ilon,ilat,isfcflg,ipcp,isdv
  integer(i_kind) icnt,ilone,ilate,icnv,itype,iclw,icli
  integer(i_kind) itim,itimp,istat

  logical sea
  logical ssmi,amsu,tmi,stage3,muse
  logical land,coast,ice
  logical,allocatable,dimension(:):: luse

  character(10) filex
  character(12) string        
  character(128) diag_pcp_file

  integer(i_kind) km1,mm1,iiflg,iextra,ireal
  integer(i_kind) ii,i,j,k,m,n,ibin,ioff,ioff0
  integer(i_kind) ipt
  integer(i_kind) nsphys,ixp,iyp,ixx,iyy
  integer(i_kind) km,ncnt
  integer(i_kind) kx,jj
  integer(i_kind) kdt
  integer(i_kind) ncloud
  integer(i_kind),dimension(4):: jgrd
  integer(i_kind),dimension(iint):: idiagbuf
  integer(i_kind) kbcon,jmin,ktcon,kuo,kbcon4,jmin4,ktcon4
  integer(i_kind) ksatid,isflg,istatus


  real(r_kind) avg,sdv,rterm1,rterm2,rterm
  real(r_kind) error,a0,a1
  real(r_kind) errlog
  real(r_kind) rdocp,frain,dtp,dtf,sum,sixthpi
  real(r_kind) drad,vfact,efact,fhour,rtime
  real(r_kind) xlo,xhi
  real(r_kind) wgtij
  real(r_kind) rmiss,pterm,delt,deltp
  real(r_kind) rmmhr,term,detect_threshold
  real(r_kind) dzmax,dtdz,dqdz,dudz,dvdz,dcwmdz
  real(r_kind) cg_pcp,wgross,wnotgross,wgt,arg

  real(r_single),allocatable,dimension(:):: diagbuf

  real(r_kind),dimension(nsig):: sl
  real(r_kind),dimension(nsig+1):: si
  real(r_kind),dimension(4):: wgrd
  real(r_kind),dimension(npredp):: pred
  real(r_kind),dimension(2):: dpcpmag
  real(r_kind) errf,varinv,error0,dtime,slats,slons,satpcp,pcpsdv, &
       pcpnum,cenlat,cenlon,satclw,satcli,obserr,pcpnbc,varinvint, &
       pcpbc,pcpsas,pcplrg4,pcpsas4,xkt4,rbs0,rmask0,psexp4,satcnv, &
       plon0,plat0,pcplrg,rn1_ad,rn4,psexp,cldwrk,cldwrk4,xkt2

  real(r_kind),allocatable,dimension(:,:):: data_p
  real(r_kind),dimension(nsig)::tsas,qsas,tlrg,qlrg, &
       t0,q0,cwm0,u0,v0,div0,t1,q1,cwm1,u1,v1,&
       t0_ad,q0_ad,cwm0_ad,&
       u0_ad,v0_ad,div0_ad,t1_ad,q1_ad,cwm1_ad,&
       u1_ad,v1_ad,t4_ad,q4_ad,cwm4_ad,u4_ad,v4_ad,div4_ad, &
       t_ges,q_ges,u_ges,v_ges,div_ges,cwm_ges,zges,z0,&
       prsl0,del0,sl0,tsen_ten0,q_ten0,p_ten0
  real(r_kind),dimension(nsig+1):: prsi0
  real(r_kind),pointer,dimension(:,:,:)::ges_cwmr_im,ges_cwmr_ip

  real(r_kind),parameter::  zero_7  = 0.7_r_kind
  real(r_kind),parameter::  r1em6   = 0.000001_r_kind
  real(r_kind),parameter::  r15     = 15.0_r_kind
  real(r_kind),parameter::  r25     = 25.0_r_kind
! real(r_kind),parameter::  r40     = 40.0_r_kind
  real(r_kind),parameter::  r45     = 45.0_r_kind
! real(r_kind),parameter::  r272_16 = 272.16_r_kind
  real(r_kind),parameter::  r10000  = 10000.0_r_kind
  character(len=*),parameter:: myname='setuppcp'

  logical:: in_curbin, in_anybin
  logical   proceed
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(pcp_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_div
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q

  real(r_kind),pointer,dimension(:,:,:)   :: ges_prs_ten=>NULL()
  real(r_kind),pointer,dimension(:,:,:)   :: ges_tv_ten =>NULL()
  real(r_kind),pointer,dimension(:,:,:)   :: ges_q_ten  =>NULL()

  real(r_kind),allocatable,dimension(:,:,:) :: ges_ps_lon
  real(r_kind),allocatable,dimension(:,:,:) :: ges_ps_lat

  data  rmiss / -999._r_kind /

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!! Verify preconditions
if(.not. (drv_initialized.and.tnd_initialized) ) then
  if(.not.drv_initialized) call perr(myname,'drv_initialized =',drv_initialized)
  if(.not.tnd_initialized) call perr(myname,'tnd_initialized =',tnd_initialized)
  call die(myname)
endif

  n_alloc(:)=0
  m_alloc(:)=0
!*********************************************************************************
! ONE TIME, INITIAL SETUP PRIOR TO PROCESSING SATELLITE DATA
!
! Initialize variables
  iiflg  = 1
  ncloud = ncld
  nsphys = max(int(two*deltim/dtphys+0.9999_r_kind),1)
  dtp    = two*deltim/nsphys
  dtf    = half*dtp
  frain  = dtf/dtp
  kdt    = 1
  fhour  = kdt*deltim/r3600
  rtime  = one/(r3600*fhour)
  rmmhr  = r1000*rtime * r3600
  mm1    = mype+1
  detect_threshold = one_tenth


! Initialize variables.
  rdocp    = rd/cp
!
!
! Set other constants.
  sixthpi = half*pi/three
!
!
!*****
!    INITIALIZE VARIABLE AND DATA ARRAYS
!*****

  ncnt = 0

! Initialize logical flags for satellite platform
  ssmi  = obstype == 'pcp_ssmi'
  tmi   = obstype == 'pcp_tmi'
  amsu  = obstype == 'pcp_amsu'
  stage3= obstype == 'pcp_stage3'


  if(pcp_diagsave)then
     iextra=0
     filex=obstype
     if(ssmi)filex='pcp_ssmi'
     write(string,1976)jiter
1976 format('_',i2.2)
     diag_pcp_file= trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // trim(string)
     if(init_pass) then
       open(4,file=trim(diag_pcp_file),form='unformatted',status='unknown',position='rewind')
     else
       open(4,file=trim(diag_pcp_file),form='unformatted',status='old',position='append')
     endif
 
     ioff0=22
     ireal=ioff0
     if (lobsdiagsave) ireal=ireal+4*miter+1
     allocate(diagbuf(ireal))

!
! Initialize/write parameters for precip. diagnostic file on
! first outer iteration.
     if (init_pass .and. mype==mype_diaghdr(is)) then
        write(4) isis,dplat(is),obstype,jiter,ianldate,iint,ireal,iextra,ioff0
        write(6,*)'SETUPPCP:  write header record for ',&
             isis,iint,ireal,iextra,' to file ',trim(diag_pcp_file),' ',ianldate
     endif
     idiagbuf= 0
     diagbuf = rmiss
  end if
!
!    Load data array for current satellite
  allocate(data_p(nele,nobs))
  allocate(luse(nobs))
  read(lunin) data_p,luse


! Index information for data array (see reading routine)
  isatid  = 1     ! index of satellite id
  itime   = 2     ! index of analysis relative obs time (
  ilon    = 3     ! index of grid relative obs location (x)
  ilat    = 4     ! index of grid relative obs location (y)
  isfcflg = 5     ! index of surface flag
  ipcp    = 6     ! index of rain rate (mm/hr)
  isdv    = 7     ! index of standard deviation of superob
  icnt    = 8     ! index of number of obs in superob
  ilone   = 9     ! index of earth realtive obs longitude (degrees)
  ilate   = 10    ! index of earth relative obs latitude (degrees)

  if (tmi) then
     icnv  = 7
     iclw  = 8
     icli  = 9
     icnt  = 10
     ilone = 11
     ilate = 12
  elseif (amsu) then
     itype = 8
  endif
  rterm1=one/float(nsig)
  rterm2=one/float(nsig*(nsig-1))

  call dtime_setup()
  do n = 1,nobs
!    Extract obs date/time.
     dtime = data_p(itime,n)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
!
!       Determine block of data to use
!
!       Initialize variables/arrays.
        cldwrk  = zero
        psexp   = zero
        xkt2    = zero
        kbcon   = 0
        ktcon   = 0
        jmin    = 0
 

!       INITIAL PROCESSING OF SATELLITE DATA
!

!       Extract satellite id
        ksatid = nint(data_p(isatid,n))
        kx = -999
        do i = 1,npcptype
           if (isis == nupcp(i)) kx = i
        end do
        if (kx<=0) write(6,*) &
           'SETUPPCP:  ***WARNING*** Problem with satellite id.  ksatid,kx=',&
           ksatid,kx,isis,(nupcp(i),i=1,npcptype)
!
!       Set observation error
        error0 = varchp(kx)
!
!       Extract obs date/time.
        dtime = data_p(itime,n)
!
!       Extract obs lon and lat.
        slons  = data_p(ilon,n) ! grid relative longitude
        slats  = data_p(ilat,n) ! grid relative latitude

!       Extract land/sea/ice flag (0=sea, 1=land, 2=ice, 3=snow, 4=mixed)
        isflg = data_p(isfcflg,n)

!       Set logical flags
 
        sea = isflg == 0
        land = isflg == 1 .or. isflg == 3
        ice  = isflg  == 2
        coast = isflg >= 4        !    Check for coastal (transition) points

!
!       Extract observations
        satpcp = data_p(ipcp,n)   ! superob rain rate (mm/hr)
        cenlat = data_p(ilate,n)  ! earth relative latitude (degrees)
        cenlon = data_p(ilone,n)  ! earth relative longitude (degrees)
        satcnv = rmiss
        if (tmi) then
           pcpsdv = rmiss
           satcnv = data_p(icnv,n)
           satclw = data_p(iclw,n)
           satcli = data_p(icli,n)
           pcpnum = data_p(icnt,n)   ! number of obs in superob
        else
           satclw = rmiss            ! cloud liquid water
           satcli = rmiss            ! cloud ice
           if(.not. amsu)then
              pcpsdv = data_p(isdv,n)   ! standard deviation of superob
              pcpnum = data_p(icnt,n)   ! number of obs in superob
           else
              pcpsdv = rmiss                                  
              pcpnum = rmiss                                   
           end if
        endif

!
!       Adjust obs error based on precipitation rate.
!
!       Bill Olson TMI error model.
        if (ksatid==211) then
           if (sea) then
              a0=0.137_r_kind; a1=0.118_r_kind  !tmi,sea
           else
              a0=0.335_r_kind; a1=0.045_r_kind  !tmi, land
           endif
 
!       Bob Kuligowski SSMI and AMSU error models
        elseif (ksatid==264) then
           if (sea) then
              a0=0.3835_r_kind;  a1=0.1699_r_kind  !ssmi, sea  (n=50 bin) (scattering)
           else
              a0=0.3148_r_kind; a1=0.1781_r_kind   !ssmi, land (n=50 bin)
           endif
        elseif (ksatid==258) then
           if (sea) then
              a0=0.2708_r_kind; a1=0.1997_r_kind   !amsu, sea, (n=50 bin)
           else
              a0=0.2232_r_kind; a1=0.2619_r_kind   !amsu, land (n=50 bin)
           endif
        else
           a0=varchp(kx); a1=zero
        endif
        term   = log(one+satpcp)
        errlog = a0 + a1*term
        obserr = errlog
 
        error = obserr
        errf = error
        varinv = one/(error*error)
 

!       If negative precipitation, reset to zero
        if (satpcp < zero) satpcp=zero
     endif ! (in_curbin)

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     if(luse_obsdiag)then
        if (.not.lobsdiag_allocated) then
           if (.not.associated(obsdiags(i_pcp_ob_type,ibin)%head)) then
              allocate(obsdiags(i_pcp_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setuppcp: failure to allocate obsdiags',istat
                 call stop2(263)
              end if
              obsdiags(i_pcp_ob_type,ibin)%tail => obsdiags(i_pcp_ob_type,ibin)%head
           else
              allocate(obsdiags(i_pcp_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setuppcp: failure to allocate obsdiags',istat
                 call stop2(264)
              end if
              obsdiags(i_pcp_ob_type,ibin)%tail => obsdiags(i_pcp_ob_type,ibin)%tail%next
           end if
           allocate(obsdiags(i_pcp_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_pcp_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_pcp_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_pcp_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_pcp_ob_type,ibin)%tail%indxglb=n
           obsdiags(i_pcp_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_pcp_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_pcp_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_pcp_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_pcp_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_pcp_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_pcp_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_pcp_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = n
           my_diag%ich = 1
        else
           if (.not.associated(obsdiags(i_pcp_ob_type,ibin)%tail)) then
              obsdiags(i_pcp_ob_type,ibin)%tail => obsdiags(i_pcp_ob_type,ibin)%head
           else
              obsdiags(i_pcp_ob_type,ibin)%tail => obsdiags(i_pcp_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_pcp_ob_type,ibin)%tail%indxglb/=n) then
              write(6,*)'setuppcp: index error'
              call stop2(265)
           end if
        endif
     endif

     if(.not.in_curbin) cycle

!
!    Do some prelimiary qc of the data.  Check for points covered
!        with ice/snow, land points, and coastal points.
!


!
!
!*****
!    CALL Precipitation (FORWARD) MODEL
!*****
!
!
!        Call precipitation model.  Set ajm forcing
!        for precipitation rate to unity.

!    Initialize variables and arrays
     km   = nsig

     do k = 1,nsig
        t0_ad(k)   = zero
        q0_ad(k)   = zero
        u0_ad(k)   = zero
        v0_ad(k)   = zero
        div0_ad(k) = zero
        cwm0_ad(k) = zero
        t_ges(k)    = zero
        q_ges(k)    = zero
        u_ges(k)    = zero
        v_ges(k)    = zero
        cwm_ges(k)  = zero
        div_ges(k)  = zero
        zges(k)    = zero
     end do
     pcpnbc = zero
     pcpsas = zero
     pcplrg = zero

     if(dtime > hrdifsig(1) .and. dtime < hrdifsig(nfldsig))then
        do jj=1,nfldsig-1
           if(dtime > hrdifsig(jj) .and. dtime <= hrdifsig(jj+1))then
              itim=jj
              itimp=jj+1
              delt=((hrdifsig(jj+1)-dtime)/(hrdifsig(jj+1)-hrdifsig(jj)))
           end if
        end do
     else if(dtime <=hrdifsig(1))then
        itim=1
        itimp=1
        delt=one
     else
        itim=nfldsig
        itimp=nfldsig
        delt=one
     end if
     deltp=one-delt

!    Get pointer to could water mixing ratio
     call gsi_bundlegetpointer (gsi_metguess_bundle(itim), 'cw',ges_cwmr_im,istatus)
     if (istatus/=0) call die('setuppcp','cannot get pointer to cwmr(itim), istatus =',istatus)
     call gsi_bundlegetpointer (gsi_metguess_bundle(itimp),'cw',ges_cwmr_ip,istatus)
     if (istatus/=0) call die('setuppcp','cannot get pointer to cwmr(itimp), istatus =',istatus)

!    Set and save spatial interpolation indices and weights.
     call get_ij(mm1,slats,slons,jgrd,wgrd,ixx,iyy)
     ixp=ixx+1
     iyp=iyy+1
!    Loop over surrounding analysis gridpoints
     do ipt=1,4


!       Set (i,j) index for ipt-th point for observation n.
        if(ipt == 1)then
           i=ixx
           j=iyy
        else if (ipt == 2)then
           i=ixp
           j=iyy
        else if (ipt == 3)then
           i=ixx
           j=iyp
        else 
           i=ixp
           j=iyp
        end if


!       Load arrays used by forward/adjoint model
        xkt4  = xkt2d(i,j)
        ii    = i+istart(mm1)-2
        ii    = max(1,min(ii,nlat))
        rbs0  = rbs2(ii)
        rmask0= isli2(i,j)

!       Perform interpolation.
        pterm = delt*ges_ps(i,j,itim) + deltp*ges_ps(i,j,itimp)
        psexp4 = pterm

        plon0 = delt*ges_ps_lon(i,j,itim) + deltp*ges_ps_lon(i,j,itimp)
        plat0 = delt*ges_ps_lat(i,j,itim) + deltp*ges_ps_lat(i,j,itimp)


        do k=1,nsig
           t0(k)    = delt *ges_tsen(i,j,k,itim)  + deltp*ges_tsen(i,j,k,itimp)
           q0(k)    = delt *ges_q(i,j,k,itim)     + deltp*ges_q(i,j,k,itimp)
           u0(k)    = delt *ges_u(i,j,k,itim)     + deltp*ges_u(i,j,k,itimp)
           v0(k)    = delt *ges_v(i,j,k,itim)     + deltp*ges_v(i,j,k,itimp)
           div0(k)  = delt *ges_div(i,j,k,itim)   + deltp*ges_div(i,j,k,itimp)
           cwm0(k)  = delt *ges_cwmr_im(i,j,k)    + deltp*ges_cwmr_ip(i,j,k)
           z0(k)    = delt *geop_hgtl(i,j,k,itim) + deltp*geop_hgtl(i,j,k,itimp)
           prsl0(k) = delt *ges_prsl(i,j,k,itim)  + deltp*ges_prsl(i,j,k,itimp)
           prsi0(k) = delt *ges_prsi(i,j,k,itim)  + deltp*ges_prsi(i,j,k,itimp)

           q_ten0(k)  = ges_q_ten(i,j,k)
           tsen_ten0(k) = (ges_tv_ten(i,j,k) - fv*t0(k)*ges_q_ten(i,j,k))/(one+fv*q0(k))

!          NOTE:  k=1 is surface pressure tendency
           p_ten0(k)  = ges_prs_ten(i,j,k+1)


        end do
        k=nsig+1
        prsi0(k) = delt *ges_prsi(i,j,k,itim ) + deltp*ges_prsi(i,j,k,itimp)

!       Create sigma-like coefficients (used in pcp_k)
        do k=1,nsig
           sl(k) = prsl0(k) / psexp4
           si(k) = prsi0(k) / psexp4
        end do
        k=nsig+1
        si(k) = prsi0(k) / psexp4

        do k=1,nsig
           del0(k) = si(k)-si(k+1)
           sl0(k)  = sl(k)
        end do

           
!       Initialize arrays below prior to each call to forward/adjoint model
        do k = 1,nsig
           t4_ad(k)   = zero
           q4_ad(k)   = zero
           u4_ad(k)   = zero
           v4_ad(k)   = zero
           div4_ad(k) = zero
           cwm4_ad(k) = zero
           t1_ad(k)   = zero
           q1_ad(k)   = zero
           u1_ad(k)   = zero
           v1_ad(k)   = zero
           cwm1_ad(k) = zero
           t1(k)      = zero
           q1(k)      = zero
           u1(k)      = zero
           v1(k)      = zero
           cwm1(k)    = zero
        end do
        rn4    = zero
        rn1_ad = one
        pcpsas4 = zero
        pcplrg4 = zero

!       Call forward/adjoint model for current analysis gridpoint

        call pcp_k(km,dtp,del0,sl0,rbs0, &
             rmask0,xkt4,ncloud,frain,rmmhr,&
             psexp4,plat0,plon0,&
             t0,q0,u0,v0,div0,cwm0,&
             tsen_ten0,q_ten0,p_ten0,&
             tsas,qsas,pcpsas4,cldwrk4,kbcon4,ktcon4,jmin4,kuo, &
             tlrg,qlrg,pcplrg4, &
             t1,q1,cwm1,u1,v1,rn4,&
             t4_ad,q4_ad,cwm4_ad,u4_ad,v4_ad,div4_ad,&
             t1_ad,q1_ad,cwm1_ad,u1_ad,v1_ad,rn1_ad)

!       Based on bilinear interpolation weights, accumulate rain rates
!       and sensitivity over four surrounding analysis gridpoints.        
        wgtij=wgrd(ipt)
        pcpsas = pcpsas + wgtij*pcpsas4
        pcplrg = pcplrg + wgtij*pcplrg4
        pcpnbc = pcpnbc + wgtij*rn4
        kbcon  = kbcon  + wgtij*kbcon4
        jmin   = jmin   + wgtij*jmin4
        ktcon  = ktcon  + wgtij*ktcon4
        psexp  = psexp  + wgtij*psexp4
        cldwrk = cldwrk + wgtij*cldwrk4
        xkt2   = xkt2   + wgtij*xkt4
        do k=1,nsig
           t0_ad(k)    = t0_ad(k)    + wgtij*t4_ad(k)
           q0_ad(k)    = q0_ad(k)    + wgtij*q4_ad(k)
           cwm0_ad(k)  = cwm0_ad(k)  + wgtij*cwm4_ad(k)
           u0_ad(k)    = u0_ad(k)    + wgtij*u4_ad(k)
           v0_ad(k)    = v0_ad(k)    + wgtij*v4_ad(k)
           div0_ad(k)  = div0_ad(k)  + wgtij*div4_ad(k)
           t_ges(k)    = t_ges(k)    + wgtij*t0(k)
           q_ges(k)    = q_ges(k)    + wgtij*q0(k)
           u_ges(k)    = u_ges(k)    + wgtij*u0(k)
           v_ges(k)    = v_ges(k)    + wgtij*v0(k)
           cwm_ges(k)  = cwm_ges(k)  + wgtij*cwm0(k)
           div_ges(k)  = div_ges(k)  + wgtij*div0(k)
           zges(k)     = zges(k)     + wgtij*z0(k)
        end do

!    End of loop over surrounding gridpoints
     end do

!    If magnitude of sensitivity vector is zero, set inverse observation
!    error to zero since we don't have gradient information for this
!    observation.  For now, ommit divergence sensitivity since it is
!    not used in pcgsoi.  Likewise, if the gradient is too large, do not
!    use this observation (this check rarely catches any points).

     dpcpmag=zero
     dzmax  =-9.99e11_r_kind
     do k=1,nsig
        dpcpmag(1) = dpcpmag(1) + t0_ad(k)
        dpcpmag(1) = dpcpmag(1) + q0_ad(k)
        dpcpmag(1) = dpcpmag(1) + u0_ad(k)
        dpcpmag(1) = dpcpmag(1) + v0_ad(k)
        dpcpmag(1) = dpcpmag(1) + cwm0_ad(k)

        dpcpmag(2) = dpcpmag(2) + t0_ad(k)*t0_ad(k)
        dpcpmag(2) = dpcpmag(2) + q0_ad(k)*q0_ad(k)
        dpcpmag(2) = dpcpmag(2) + u0_ad(k)*u0_ad(k)
        dpcpmag(2) = dpcpmag(2) + v0_ad(k)*v0_ad(k)
        dpcpmag(2) = dpcpmag(2) + cwm0_ad(k)*cwm0_ad(k)
         
        km1=max(1,k-1)
        term=zges(k)-zges(km1)
!       rterm=one/term
        rterm = zero
        if (k>km1) rterm=one/term
        dtdz = abs((t0_ad(k)-t0_ad(km1))*rterm)
        dqdz = abs((q0_ad(k)-q0_ad(km1))*rterm)
        dudz = abs((u0_ad(k)-u0_ad(km1))*rterm)
        dvdz = abs((v0_ad(k)-v0_ad(km1))*rterm)
        dcwmdz = abs((cwm0_ad(k)-cwm0_ad(km1))*rterm)
        dzmax = max(dtdz,dqdz,dudz,dvdz,dcwmdz,dzmax)

     end do

     avg = dpcpmag(1)*rterm1
     term= (nsig*dpcpmag(2)-dpcpmag(1)**2)*rterm2
     sdv = zero
     if (term>zero) sdv=sqrt(term)

     dpcpmag(2)=sqrt(dpcpmag(2))


     if (dpcpmag(2)<tiny_r_kind) then
        if(luse(n))aivals(26,is) = aivals(26,is) + one
        varinv=zero
     endif

     if (dpcpmag(2)>r10000) then
        if(luse(n))aivals(27,is) = aivals(27,is) + one
        varinv=zero
     elseif (abs(avg)>zero) then
        if (abs(sdv/avg)>r25) then
           if(luse(n))aivals(28,is) = aivals(28,is) + one
           varinv = zero
        endif
     endif

     if (abs(dzmax)>one) then
        if(luse(n))aivals(29,is) = aivals(29,is) + one
        varinv = zero
     endif

     if(luse(n))aivals(1,is) = aivals(1,is) + one
     aivals(2,is) = aivals(2,is) + one
     if (varinv>r1em6 .and. luse(n)) aivals(3,is) = aivals(3,is) + one
!
!
!*****
!    COMPUTE AND APPLY BIAS CORRECTION TO SIMULATED VALUES
!*****
!
!    Prepare for application of bias correction to simulated values.
!
!    Construct predictors for bias correction.
     do k=1,npredp
        pred(k) = zero
     end do
!  
!    Apply bias correction to simulated values.

     sum = zero
     do j = 1,npredp
        sum = sum + predxp(j,kx)*pred(j)*ibias(kx)
     end do
     pcpbc = pcpnbc + sum
     pcpbc = max(pcpbc,zero)

! 
!
!***
!   QC OBSERVATIONS BASED ON VARIOUS CRITERIA
!***
!
!    QC SSMI, TMI, AMSU, and STAGE3 precipitation.
     if (ssmi .or. tmi .or. amsu .or. stage3) then
!     
        efact   = one
        vfact   = quarter

!       Reduce qc bounds in extratropics.  Between 45
!       and 60 degrees latitude linearly reduce error
!       bound.  Poleward of 60 degrees latitude do not use
        if ( abs(cenlat) > r45 ) then
           if(luse(n))aivals(4,is) = aivals(4,is) + one
           term  = (r60-abs(cenlat))/r15 
           term  = max(zero,term)
           term  = min(term,one)
           if (abs(cenlat) > r60) term=zero
           efact = term*efact
        endif
!  
!       If land obs, reduce error bounds and weight
        if (land) then
           efact = zero_7*efact
           vfact = zero_7*vfact
           if(luse(n))aivals(5,is) = aivals(5,is) + one
        end if
!     
!       Toss obs if over snow or ice
        if (ice) then
           efact   = zero
           vfact   = zero
           if(luse(n))aivals(6,is) = aivals(6,is) + one
        end if

!       If coastal obs, reduce error bounds and weight
        if (coast) then
           efact   = half*efact
           vfact   = half*vfact
           if(luse(n))aivals(7,is) = aivals(7,is) + one
        end if
!
!       Reduce weight and tighten obs-ges bound as function of obs time.
        if(.not.(l4dvar.or.l4densvar))then
           term  = cos(sixthpi*(dtime-half*hr_obsbin))
           term  = term**3
           term  = max(zero,min(term,one))
           efact = term*efact
           vfact = term*vfact
        end if


!       If the rain rate is greater than zero but less than the 
!       detection threshold (currently take to be 0.1 mm/hr), then 
!       set the observation error to zero.  
!       That is, do not use this observation in the analysis.
        if ( (satpcp>zero) .and. (satpcp<detect_threshold) ) then
           efact = zero
           vfact = zero
           if(luse(n))aivals(30,is) = aivals(30,is) + one
        endif

!
!       Generate q.c. bounds and modified variances.
        errf   = efact*errf
        varinv = vfact*varinv
        varinvint = varinv

!    End of SSMI, TMI, AMSU, and STAGE3 qc block
     endif
!
!
!    Apply gross check to good pcp obs.
     xlo = -gross_pcp(kx)*errf
     xhi = +gross_pcp(kx)*errf
     drad = log(one+satpcp) - log(one+pcpbc)
     if (varinv > r1em6) then
        if (drad < xlo .or. drad > xhi) then
           varinv = zero
           if(luse(n))aivals(12,is)  = aivals(12,is) + one
        endif
     endif

     muse= (varinv>r1em6.and.iusep(kx)>=1)
     if(luse_obsdiag)then
        if (nobskeep>0) muse=obsdiags(i_pcp_ob_type,ibin)%tail%muse(nobskeep)

        obsdiags(i_pcp_ob_type,ibin)%tail%luse=luse(n)
        obsdiags(i_pcp_ob_type,ibin)%tail%muse(jiter)=muse
        obsdiags(i_pcp_ob_type,ibin)%tail%nldepart(jiter)= drad
        obsdiags(i_pcp_ob_type,ibin)%tail%wgtjo= varinv
     end if


!
!***
!   CONSTRUCT SENSITIVITY VECTORS AND SAVE INFO FOR INNER ITERATION
!***
!
!    Optionally, set parameter iusep to turn off data.
     if (muse) then
        ncnt  = ncnt+1

        if(.not. associated(pcphead(ibin)%head))then
           allocate(pcphead(ibin)%head,stat=istat)
           if(istat /= 0)write(6,*)' failure to write pcphead '
           pcptail(ibin)%head => pcphead(ibin)%head
        else
           allocate(pcptail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write pcptail%llpoint '
           pcptail(ibin)%head => pcptail(ibin)%head%llpoint
        end if

        m_alloc(ibin) = m_alloc(ibin) +1
        my_head => pcptail(ibin)%head
        my_head%idv = is
        my_head%iob = n

        allocate(pcptail(ibin)%head%predp(npredp),pcptail(ibin)%head%dpcp_dvar(nsig5), &
             stat=istat)
        if(istat /= 0)write(6,*)' failure to write pcptail arrays '

        do m=1,4
           pcptail(ibin)%head%ij(m)=jgrd(m)
           pcptail(ibin)%head%wij(m)=wgrd(m)
        end do

        pcptail(ibin)%head%err2=one/error0**2
        pcptail(ibin)%head%raterr2=error0**2*varinv
        do m=1,npredp
           pcptail(ibin)%head%predp(m)=pred(m)
        end do
        pcptail(ibin)%head%obs=log(one+satpcp)
        pcptail(ibin)%head%ges=pcpbc
        pcptail(ibin)%head%time=dtime
        do m=1,nsig
           pcptail(ibin)%head%dpcp_dvar(m)=t0_ad(m)
           pcptail(ibin)%head%dpcp_dvar(m+nsig)=q0_ad(m)
           pcptail(ibin)%head%dpcp_dvar(m+nsig2)=u0_ad(m)
           pcptail(ibin)%head%dpcp_dvar(m+nsig3)=v0_ad(m)
           pcptail(ibin)%head%dpcp_dvar(m+nsig4)=cwm0_ad(m)
!          pcptail(ibin)%head%dpcp_dvar(m+nsig5)=div0_ad(m)  !need to increase size
        end do
        pcptail(ibin)%head%icxp=kx
        pcptail(ibin)%head%luse=luse(n)
        if(luse_obsdiag)then
           pcptail(ibin)%head%diags => obsdiags(i_pcp_ob_type,ibin)%tail

           my_head => pcptail(ibin)%head
           my_diag => pcptail(ibin)%head%diags
           if(my_head%idv /= my_diag%idv .or. &
              my_head%iob /= my_diag%iob ) then
              call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =', &
                    (/is,n,ibin/))
              call perr(myname,'my_head%(idv,iob) =',(/my_head%idv,my_head%iob/))
              call perr(myname,'my_diag%(idv,iob) =',(/my_diag%idv,my_diag%iob/))
              call die(myname)
           endif
        endif
     end if

     if(luse(n))then
!
!    Save data in diagnostic arrays.
        if (pcp_diagsave) then
           idiagbuf(1) = ksatid
           idiagbuf(2) = 0                    ! spare
           idiagbuf(3) = 0                    ! spare
           idiagbuf(4) = isflg
           idiagbuf(5) = 0                    ! spare
           idiagbuf(6) = kbcon           
           idiagbuf(7) = ktcon
           idiagbuf(8) = jmin
 
           diagbuf(1)  = cenlat
           diagbuf(2)  = cenlon
           diagbuf(3)  = dtime-time_offset
           diagbuf(4)  = pcpnum
           diagbuf(5)  = satpcp
           diagbuf(6)  = pcpsdv
           diagbuf(7)  = satcnv
           diagbuf(8)  = satclw
           diagbuf(9)  = satcli
           diagbuf(10) = one/(error*error)
           diagbuf(11) = error
           if (kbcon>=1 .and. kbcon<=nsig) diagbuf(12) = r10*psexp * sl0(kbcon)
           if (ktcon>=1 .and. ktcon<=nsig) diagbuf(13) = r10*psexp * sl0(ktcon)
           if (jmin>=1  .and. jmin<=nsig)  diagbuf(14) = r10*psexp * sl0(jmin)
           diagbuf(15) = cldwrk
           diagbuf(16) = pcpsas
           diagbuf(17) = pcpnbc
           diagbuf(18) = pcpbc
           diagbuf(19) = varinvint
           diagbuf(20) = varinv
           diagbuf(21) = errf
           diagbuf(22) = xkt2
 
           ioff=ioff0
           if (lobsdiagsave) then
              do jj=1,miter
                 ioff=ioff+1
                 if (obsdiags(i_pcp_ob_type,ibin)%tail%muse(jj)) then
                    diagbuf(ioff) = one
                 else
                    diagbuf(ioff) = -one
                 endif
              enddo
              do jj=1,miter+1
                 ioff=ioff+1
                 diagbuf(ioff) = obsdiags(i_pcp_ob_type,ibin)%tail%nldepart(jj)
              enddo
              do jj=1,miter
                 ioff=ioff+1
                 diagbuf(ioff) = obsdiags(i_pcp_ob_type,ibin)%tail%tldepart(jj)
              enddo
              do jj=1,miter
                 ioff=ioff+1
                 diagbuf(ioff) = obsdiags(i_pcp_ob_type,ibin)%tail%obssen(jj)
              enddo
           endif

!          Write diagnostics to output file.
           write(4) mype,(idiagbuf(m),m=1,iint),(diagbuf(m),m=1,ireal)

        endif
!
!
!
!******
!   DIAGNOSTIC CALCULATIONS.
!******
!
!
!    Accumulate data for diagnostic print.

        if (varinv > r1em6) then
           drad   = log(one+satpcp) - log(one+pcpbc)
           obserr = sqrt(one/varinv)
           if (iusep(kx)<1) varinv=zero
 
           aivals(11,is)  = aivals(11,is) + one
           aivals(13,is)  = aivals(13,is) + drad                !bias
           aivals(14,is)  = aivals(14,is) + drad*drad           !bias**2
           aivals(15,is)  = aivals(15,is) + drad*drad*varinv    !pen
           aivals(16,is)  = aivals(16,is) + obserr              !obs error error   
 
           aivals(21,is) = aivals(21,is) + satpcp            
           aivals(22,is) = aivals(22,is) + pcpnbc            
           aivals(23,is) = aivals(23,is) + pcpbc             
           aivals(24,is) = aivals(24,is) + pcpsas            
           aivals(25,is) = aivals(25,is) + one
 
           term = (drad/error0)**2
           if (pg_pcp(kx) > tiny_r_kind .and. error > tiny_r_kind) then
              arg  = exp(-half*term)
              wnotgross= one-pg_pcp(kx)
              cg_pcp=b_pcp(kx)*error
              wgross = cg_term*pg_pcp(kx)/(cg_pcp*wnotgross)
              term = log((arg+wgross)/(one+wgross))
              wgt  = one-wgross/(arg+wgross)
              if(wgt < wgtlim) aivals(40,is)=aivals(40,is)+one
           endif
           aivals(39,is) = aivals(39,is) +(error0**2)*varinv*term              
        else
           aivals(31,is) = aivals(31,is) + satpcp
           aivals(32,is) = aivals(32,is) + pcpnbc
           aivals(33,is) = aivals(33,is) + pcpbc 
           aivals(34,is) = aivals(34,is) + pcpsas
           aivals(35,is) = aivals(35,is) + one
        endif
     end if
!    End of loop over observations
  end do

! Release memory of local guess arrays
  call final_vars_
!
! Deallocate arrays
!
  deallocate(data_p)
  deallocate(luse)
  if (pcp_diagsave) then
     close(4)
     deallocate(diagbuf)
     call dtime_show(myname,'diagsave:pcp',i_pcp_ob_type)
  endif

! End of routine

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::u' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::v' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::div', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::q', ivar, istatus )
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
!    get u ...
     varname='u'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_u))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_u(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_u(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_u(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get v ...
     varname='v'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_v))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_v(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_v(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_v(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get div ...
     varname='div'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_div))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_div(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_div(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_div(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get q ...
     varname='q'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_q))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_q(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_q(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_q(:,:,:,ifld)=rank3
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
! extract tendencies from tendency bundle
  if(tnd_initialized) then
!    get prs_ten ...
     varname='prse'
     call gsi_bundlegetpointer(gsi_tendency_bundle,trim(varname),ges_prs_ten,istatus)
     if (istatus/=0) then
         write(6,*) trim(myname),': ', trim(varname), ' not found in tend bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tv_ten ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_tendency_bundle,trim(varname),ges_tv_ten,istatus)
     if (istatus/=0) then
         write(6,*) trim(myname),': ', trim(varname), ' not found in tend bundle, ier= ',istatus
         call stop2(999)
     endif
!    get q_ten ...
     varname='q'
     call gsi_bundlegetpointer(gsi_tendency_bundle,trim(varname),ges_q_ten,istatus)
     if (istatus/=0) then
         write(6,*) trim(myname),': ', trim(varname), ' not found in tend bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': tendency bundle not properly allocated, aborting ... '
     call stop2(999)
  endif
! extract derivatives from derivatives bundle
  if(drv_initialized) then
!    get ps_lon ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_xderivative_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps_lon))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps_lon(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps_lon(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_xderivative_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps_lon(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get ps_lat ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_yderivative_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps_lat))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps_lat(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps_lat(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_xderivative_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps_lat(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  endif
  end subroutine init_vars_

  subroutine final_vars_
    if(associated(ges_q_ten  )) nullify(ges_q_ten  )
    if(associated(ges_tv_ten )) nullify(ges_tv_ten )
    if(associated(ges_prs_ten)) nullify(ges_prs_ten)
!
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_div))deallocate(ges_div)
    if(allocated(ges_v )) deallocate(ges_v )
    if(allocated(ges_u )) deallocate(ges_u )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setuppcp
