subroutine setupref(lunin,mype,awork,nele,nobs,toss_gps_sub,is,init_pass,last_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupref    compute rhs of oi for gps refractivity
!   prgmmr: cucurull, l.    org: JCSDA/NCEP           date: 2004-03-24
!
! abstract:  For gps refractivity observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2004-03-24  cucurull
!   2004-06-17  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase size of refwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-11-29  cucurull- install non-linear forward operator
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-01-26  cucurull- save innovation vector for linear RO code
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-03-16  treadon - place upper bound on k1
!   2005-03-23  cucurull- correct bouds for obs below the second level;
!                         compute minimizations coeffs for 'acceptable' obs only;
!                         compute diagnostic file 
!   2005-05-27  derber - level output change
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-07-27  derber  - rewrite and combine with prepref and sprref
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-11-29  derber - remove psfcg and use ges_lnps instead
!   2005-12-01  cucurull - change some indexes on input data; initialize some
!                          arrays; fix bug on counting obs below/above model
!   2005-12-21  treadon - add super_gps, move some diagnostics statistics
!                         to genstat_gps
!   2006-01-04  treadon - correct inconsistency when using qcfail
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-02-24  cucurull - update QC parameters and compute preliminary representativeness
!                          error, fix bug when countin obs that fail gross check
!   2006-04-14  middlecoff - changed IF test to avoid out-of-bounds-reference on DATA
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-07-31  kleist - use ges_ps instead of lnps
!   2006-09-20  cucurull - use geopotential heights at intermediate levels instead of mid
!                          layers,remove dlnp, new QC checks,
!                          penalize high elevation obs, remove termt1, modify the adjoint terms
!                          to generalize the GPS code to hybrid vertical coordinate,
!                          remove obs above 30km, remove psges, remove zsges
!   2006-10-20 cucurull - update QC statistical checks and representativeness error with the use of 
!                         COSMIC data
!                       - add information to diagnostic file
!   2007-03-01 derber   - add toss_gps_sub; simplify profile qc loop
!   2007-03-19 tremolet - binning of observations
!   2007-04-13 treadon  - tighten data cutoff in tropics
!   2007-06-05 tremolet - add observation diagnostics structure
!   2007-06-22 cucurull - generalize qc structure to enable regional GSI;
!                         reduce gpswork2; remove conv_diagsave from argument list; 
!                         consistent data counts for qc checks; 
!                         update diagnostic information to be consistent with other obs;
!                         modify diagnostic structure 
!  2007-07-26  cucurull - update code to generalized vertical coordinate (3d pressure)
!  2007-09-21  cucurull - remove obs above 40km from qc checks
!  2007-10-19  cucurull - modify obs location within model vertical grid to improve forward
!                         operator in complex topography; toss obs above/below top/first 
!                         model layer; add elev-zsges in diagnostic structure.
!  2007-12-23  cucurull - correct the interpolation weights assigned to levels surrounding obs
!  2008-02-27  cucurull - modify diagnostics output file
!  2008-04-12  treadon  - remove super_gps (moved to genstats_gps)
!  2008-05-23  safford - rm unused vars and uses
!  2008-12-03  todling  - revisited Tremolet modifications in light of newer GSI
!                       - changed handle of tail%time
!  2009-02-05  cucurull - update qc, obs error and refractivity operator
!  2009-04-27  cucurull - update qc to enable GRAS and GRACE assimilation
!  2009-08-19  guo      - changed for multi-pass setup with dtime_check().
!  2009-10-22      shen - add regional obs error for regional GSI
!			- add high_gps_sub
!  2010-04-09 cucurull - remove high_gps_sub (regional QC moved to genstats.f90)
!                      - load obs dianostics structure into gps_alltail
!                      - remove idia
!                      - remove observation dianostics structure (I moved this structure to genstats.f90
!                        a while back and shouldn't have been coded here again!)
!  2010-07-22 treadon  - remove last check for gpshead allocate
!  2010-07-23 treadon  - clean up use statements
!  2010-10-25 cucurull - add quality control options for C/NOFS satellite
!  2011-01-05 cucurull - add gpstop to reject anything above this value 
!  2011-01-13 lueken   - corrected init_pass and last_pass indentation
!  2011-01-18 cucurull - increase the size of mreal by one element to add gps_dtype information
!  2011-08-16 cucurull - fix bug in statistics qc
!  2011-08-17 cucurull - add METOP-B GRAS (plus Oceansat-2, SAC-D and M-T) assimilation capabilities
!  2012-09-11 cucurull - fix bug in qc printout and setup super-refraction quality control
!  2012-10-16 cucurull - increase mreal to 21 to add qrefges, remove qcfail_stats_1 and qcfail_stats_2, consolidate to qcfail
!  2013-01-16 cucurull - remove GRAS data below 8 km
!  2013-01-26 parrish - change grdcrd to grdcrd1, tintrp2a to tintrp2a1, tintrp2a11,
!                                   tintrp3 to tintrp31 (so debug compile works on WCOSS)
!  2013-10-19 todling - metguess now holds background
!  2014-04-10 todling - 4dvar fix: obs must be in current time bin
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,i_kind
  use obsmod, only: nprof_gps,gpshead,gpstail,gps_allhead,gps_alltail,&
       i_gps_ob_type,obsdiags,lobsdiagsave,nobskeep,lobsdiag_allocated,&
       time_offset
  use obsmod, only: gps_ob_type
  use obsmod, only: obs_diag,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use guess_grids, only: ges_lnprsi,hrdifsig,geop_hgti,geop_hgtl,nfldsig,&
       gpstop
  use gridmod, only: nsig
  use gridmod, only: latlon11,get_ij,regional
  use constants, only: fv,n_a,n_b,n_c,deg2rad,tiny_r_kind,quarter
  use constants, only: zero,one,two,eccentricity,semi_major_axis,&
       grav_equator,somigliana,flattening,grav_ratio,grav,rd,eps,&
       three,four,five,half,r0_01
  use jfunc, only: jiter,miter
  use convinfo, only: cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use m_gpsrhs, only: muse
  use m_gpsrhs, only: termq
  use m_gpsrhs, only: termpk,termpl1,termpl2
  use m_gpsrhs, only: termt ,termtk ,termtl
  use m_gpsrhs, only: pressure,error,error_adjst
  use m_gpsrhs, only: ratio_errors,dpresl
  use m_gpsrhs, only: rdiagbuf,cdiagbuf
  use m_gpsrhs, only: qcfail
  use m_gpsrhs, only: qcfail_loc,qcfail_high,qcfail_gross
  use m_gpsrhs, only: data_ier,data_igps,data_ihgt
  use m_gpsrhs, only: gpsrhs_alloc
  use m_gpsrhs, only: gpsrhs_dealloc
  use m_gpsrhs, only: gpsrhs_aliases
  use m_gpsrhs, only: gpsrhs_unaliases

  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle

  implicit none

! Declare local parameters
  real(r_kind),parameter:: r0_455 = 0.455_r_kind
  real(r_kind),parameter:: r2_5 = 2.5_r_kind
  real(r_kind),parameter:: ten = 10.0_r_kind
  real(r_kind),parameter:: r20 = 20.0_r_kind
  real(r_kind),parameter:: r52_075 = 52.075_r_kind
  real(r_kind),parameter:: r240 = 240.0_r_kind
  real(r_kind),parameter:: r1em3 = 1.0e-3_r_kind
  real(r_kind),parameter:: six = 6.0_r_kind
  real(r_kind),parameter:: eight = 8.0_r_kind
  real(r_kind),parameter:: nine = 9.0_r_kind
  real(r_kind),parameter:: eleven = 11.0_r_kind
  character(len=*),parameter :: myname='setupref'
  real(r_kind),parameter:: crit_grad = 157.0_r_kind

! Declare passed variables
  integer(i_kind)                            ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)  ,intent(inout) :: awork
  real(r_kind),dimension(max(1,nprof_gps)),intent(inout) :: toss_gps_sub
  integer(i_kind)                            ,intent(in   ) :: is	! ndat index
  logical                                    ,intent(in   ) :: init_pass	! the pass with the first set of background bins
  logical                                    ,intent(in   ) :: last_pass	! the pass with all background bins processed

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: tintrp31
  external:: grdcrd1
  external:: stop2

! Declare local variables

  real(r_kind) cutoff,cutoff1,cutoff2,cutoff3,cutoff12,cutoff23
  real(r_kind) rsig,dtime,dlat,dlon,tmean,mult_p
  real(r_kind) errinv_input,errinv_adjst,errinv_final,err_final,repe_gps
  real(r_kind) hgeso,trefges,pobl,grad_mod,grad_obs
  real(r_kind) sin2,termg,termr,termrg,hob,hobl,qrefges,zsges
  real(r_kind) fact,pw,nrefges1,nrefges2,nrefges3,nrefges,dpres,elev,k4,alt
  real(r_kind) ratio,residual,obserror,obserrlm,delz
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nsig):: tges,hgesl
  real(r_kind),dimension(nsig+1) :: prsltmp,hges
  
  integer(i_kind):: ier,ilon,ilat,ihgt,igps,itime,ikx,iuse,ikxx
  integer(i_kind):: iprof,ipctc,iroc,isatid,iptid
  integer(i_kind):: ilate,ilone,mm1,ibin,ioff
  integer(i_kind) i,j,k,k1,k2,nreal,mreal,jj
  integer(i_kind) :: kl,k1l,k2l
  integer(i_kind) kprof,istat,jprof
  integer(i_kind),dimension(4):: gps_ij
  integer(i_kind):: satellite_id,transmitter_id

  logical,dimension(nobs):: luse
  logical proceed

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(gps_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q

  n_alloc(:)=0
  m_alloc(:)=0
!*******************************************************************************
! List of GPS RO satellites and corresponding BUFR id
!740 => COSMIC FM1
!741 => COSMIC FM2
!742 => COSMIC FM3
!743 => COSMIC FM4
!744 => COSMIC FM5
!745 => COSMIC FM6
!4   => MetOpA
!41  => Champ
!722 => GRACE A
!723 => GRACE B
!820 => SACC
!42  => TerraSAR-X
!43  => Tandem-X
!786 => C/NOFS
!421 => OCEANSAT-2
!3   => MetOpB
!440 => Megha-Tropiques
!821 => SACD
!44  => PAZ
!750-755 => COSMIC-2 Equatorial
!724-729 => COSMIC-2 Polar

! Read and reformat observations in work arrays.
  read(lunin)data,luse


! Set indices for quantities in data array
  ier=1        ! index of obs error in data array (now 1/(original obs error))
  ilon=2       ! index of grid relative obs location (x)
  ilat=3       ! index of grid relative obs location (y)
  ihgt=4       ! index of obs vertical coordinate in data array
  igps=5       ! index of gps data (or residual) in data array
  itime=6      ! index of obs time in data array
  ikxx=7       ! index of observation type
  iprof=8      ! index of profile
  ipctc=9      ! index of percent confidence
  iroc=10      ! index of local radius of curvature
  isatid=11    ! index of satellite identifier
  iptid=12     ! index of platform transmitter id number
  iuse=13      ! index of use parameter
  ilone=14     ! index of earth relative longitude (degrees)
  ilate=15     ! index of earth relative latitude (degrees)

! Initialize variables
  rsig=float(nsig)
  mm1=mype+1

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

! Allocate arrays for output to diagnostic file
  mreal=21
  nreal=mreal
  if (lobsdiagsave) nreal=nreal+4*miter+1

  if(init_pass) call gpsrhs_alloc(is,'ref',nobs,nsig,nreal,-1,-1)
  call gpsrhs_aliases(is)
  if(nreal/=size(rdiagbuf,1)) then
     call perr(myname,'nreal/=size(rdiagbuf,1)')
     call perr(myname,'nreal=',nreal)
     call perr(myname,'size(rdiagbuf,1)=',size(rdiagbuf,1))
     call die(myname)
  endif
  nreal=size(rdiagbuf,1)

  if(init_pass) then
!    Initialize saved arrays
     data_ier (:)=data(ier ,:)
     data_ihgt(:)=data(ihgt,:)
     data_igps(:)=data(igps,:)
 
     ! these statements should moved into gpsrhs_alloc(), but
     ! left unchanged for verification purposes.
     qcfail=.false.
     qcfail_loc=zero
     qcfail_gross=zero
     qcfail_high=zero 

     muse(:)=.false.

!    Initialize arrays
     termtl=zero; termt= zero; termtk=zero; termq=zero
     termpk=zero; termpl1=zero; termpl2=zero

  else ! (init_pass)

!    Restore these arrays saved from the previous pass
     data(ier ,:)=data_ier (:)
     data(ihgt,:)=data_ihgt(:)
     data(igps,:)=data_igps(:)
  endif ! (init_pass)

! Save height,lat, and lon of the observations for later
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then

        muse(i)=nint(data(iuse,i)) <= jiter
        sin2  = sin(data(ilate,i)*deg2rad)**2
        dlon=data(ilon,i)
        dlat=data(ilat,i)
        dpres=data(ihgt,i)
        elev=dpres

        ikx=nint(data(ikxx,i))
 
!       Interpolate log(pres), terrain, and geop heights to obs location
        call tintrp2a1(ges_lnprsi,prsltmp,dlat,dlon,dtime,hrdifsig,&
             nsig+1,mype,nfldsig)
        call tintrp2a1(ges_tv,tges,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(geop_hgti,hges,dlat,dlon,dtime,hrdifsig,&
             nsig+1,mype,nfldsig)
        call tintrp2a1(geop_hgtl,hgesl,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)

!       Convert geometric height at observation to geopotential height using
!       equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
!       measures of altitude" (2001).  Available on the web at
!       http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!       termg  = equation 17
!       termr  = equation 21
!       termrg = first term in the denominator of equation 23

        termg = grav_equator * &
             ( (one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2) )
        termr = semi_major_axis / (one + flattening + grav_ratio - two*flattening*sin2)
        termrg = (termg/grav)*termr

!       Surface-corrected geopotential height of the observation
        dpres=dpres-zsges
        hgeso=(termg/grav)*((termr*dpres)/(termr+dpres))

!       Convert observation height (in dpres) from meters to grid relative units
        hob=hgeso
        hobl=hgeso
        call grdcrd1(hob,hges,nsig,1)   ! interface levels
        call grdcrd1(hobl,hgesl,nsig,1) ! midpoint layers
        dpres=hob
        dpresl(i)=hobl
        data(ihgt,i)=dpres
 
!       Get temperature at observation location
        call tintrp31(ges_tv,trefges,dlat,dlon,hobl,&
             dtime,hrdifsig,mype,nfldsig)
 
!       Set indices of model levels below (k1) and above (k2) observation.
        k=dpres
        k1=min(max(1,k),nsig)
        k2=max(1,min(k+1,nsig))

!       Get observation pressure from hypsometric equation
        if(k1==1) then
           pobl=two*grav*(hgeso-hges(k1))/(rd*(trefges+tges(k1)))
        else
           tmean=(tges(k1)+tges(k1-1))/two ! temperature at interface level k1
           pobl=two*grav*(hgeso-hges(k1))/(rd*(trefges+tmean))
        endif
        pobl=prsltmp(k1)-pobl

!       Get finite pressure when obs is above the top model or below first level
        if(k1 == k2) pobl= prsltmp(k1)
        pressure(i)=ten*exp(pobl) !in hPa

!       Tune observation error to account for representativeness error.
!       Preliminary values

        repe_gps=one
        alt=r1em3*elev

        if(.not.regional) then ! for global

           if((data(ilate,i)>= r20).or.(data(ilate,i)<= -r20)) then
              repe_gps=-1.321_r_kind+0.341_r_kind*alt-0.005_r_kind*alt**2

           else
              if(alt > ten) then
                 repe_gps=2.013_r_kind-0.060_r_kind*alt+0.0045_r_kind*alt**2
              else
                 repe_gps=-1.18_r_kind+0.058_r_kind*alt+0.025_r_kind*alt**2
              endif
           endif

        else ! for regional

           if((data(ilate,i)>= r20).or.(data(ilate,i)<= -r20)) then
              if(alt > ten) then
                 repe_gps=-1.321_r_kind+0.341_r_kind*alt-0.005_r_kind*alt**2
              else
                 repe_gps=-1.2_r_kind+0.065_r_kind*alt+0.021_r_kind*alt**2
              endif
           else
              if(alt > ten) then
                 repe_gps=2.013_r_kind-0.120_r_kind*alt+0.0065_r_kind*alt**2
              else
                 repe_gps=-1.19_r_kind+0.03_r_kind*alt+0.023_r_kind*alt**2
              endif
           endif

        endif

        repe_gps=exp(repe_gps)
        repe_gps=one/abs(repe_gps) ! representativeness error

!       ratio_errors(i) = data(ier,i)/abs(data(ier,i)*repe_gps)
        ratio_errors(i) = data(ier,i)/abs(repe_gps)

        error(i)=one/data(ier,i) ! one/original error
        data(ier,i)=one/data(ier,i)
        error_adjst(i)= ratio_errors(i)* data(ier,i) !one/adjusted error

!       Remove observation if below surface or at/above the top layer 
!       of the model by setting observation (1/error) to zero.
!       Make no adjustment if observation falls within vertical
!       domain.

        if (hobl < one .or. hobl > rsig) then
           data(ier,i) = zero
           ratio_errors(i) = zero
           muse(i)=.false.
           qcfail_loc(i)=one
        endif

!       Increment obs counter along with low and high obs counters
        if(luse(i))then
           awork(1)=awork(1)+one
           if(hobl <  one) awork(2)=awork(2)+one
           if(hobl > rsig) awork(3)=awork(3)+one
        endif

!       Save some diagnostic information
 
!       occultation identification
        satellite_id         = data(isatid,i) ! receiver occ id 
        transmitter_id       = data(iptid,i)  ! transmitter occ id 
        write(cdiagbuf(i),'(2(i4.4))') satellite_id,transmitter_id
 
        rdiagbuf(:,i)         = zero
 
        rdiagbuf(1,i)         = ictype(ikx)    ! observation type
        rdiagbuf(20,i)        = zero           ! uses gps_ref (one = use of bending angle)
        rdiagbuf(2,i)         = data(iprof,i)  ! profile identifier
        rdiagbuf(3,i)         = data(ilate,i)  ! lat in degrees
        rdiagbuf(4,i)         = data(ilone,i)  ! lon in degrees
        rdiagbuf(6,i)         = pressure(i)    ! guess observation pressure (hPa)
        rdiagbuf(7,i)         = elev           ! height in meters
        rdiagbuf(8,i)         = dtime-time_offset ! obs time (hours relative to analysis time)
!       rdiagbuf(9,i)         = data(ipctc,i)  ! input bufr qc - index of per cent confidence    
        rdiagbuf(9,i)         = elev-zsges     ! height above model terrain (m)      
        rdiagbuf(11,i)        = data(iuse,i)   ! data usage flag
        rdiagbuf(19,i)        = hobl           ! model vertical grid  (midpoint)

        if (ratio_errors(i) > tiny_r_kind) then  ! obs inside vertical grid

!          Compute guess local refractivity at obs location.
!          Also compute terms needed in minimization

           call tintrp31(ges_q,qrefges,dlat,dlon,hobl,dtime,&
                 hrdifsig,mype,nfldsig)

!          Compute guess local refractivity
           fact=(one+fv*qrefges)
           pw=eps+qrefges*(one-eps)
           k4=n_c-n_a
           nrefges1=n_a*(pressure(i)/trefges)*fact
           nrefges2=n_b*qrefges*pressure(i)*fact**2/(trefges**2*pw)
           nrefges3=k4*fact*qrefges*pressure(i)/(trefges*pw)
           nrefges=nrefges1+nrefges2+nrefges3 !total refractivity

!          Accumulate diagnostic information        
           rdiagbuf(5,i)   = (data(igps,i)-nrefges)/data(igps,i) ! incremental refractivity (x100 %)

           rdiagbuf(17,i)  = data(igps,i)  ! refractivity observation (units of N)
           rdiagbuf(18,i)  = trefges       ! temperature at obs location in Kelvin
           rdiagbuf(21,i)  = qrefges       ! specific humidity at obs location (kg/kg)

           data(igps,i)=data(igps,i)-nrefges  ! innovation vector

           if(alt <= gpstop) then ! go into qc checks

!             Gross error check 
              obserror = one/max(ratio_errors(i)*data(ier,i),tiny_r_kind)
              obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
              residual = abs(data(igps,i))
              ratio    = residual/obserrlm
 
              if (ratio > cgross(ikx)) then
                 if (luse(i)) then
                    awork(4) = awork(4)+one
                 endif
                 qcfail_gross(i)=one
                 data(ier,i) = zero
                 ratio_errors(i) = zero
                 muse(i)=.false.
              else 
!                Statistics QC check if obs passed gross error check 
                 cutoff=zero
                 cutoff1=quarter+half*cos(data(ilate,i)*deg2rad)
                 if(trefges<=r240) then
                    cutoff2=half
                 else
                    cutoff2=r1em3*trefges**2-r0_455*trefges+r52_075
                 endif
                 if((ictype(ikx)==41).or.(ictype(ikx)==722).or.(ictype(ikx)==723).or.&
                    (ictype(ikx)==4).or.(ictype(ikx)==786).or.(ictype(ikx)==3)) then !CL
                    cutoff3=(half+two*cos(data(ilate,i)*deg2rad))/three
                 else
                    cutoff3=(one+r2_5*cos(data(ilate,i)*deg2rad))/three
                 endif
                 cutoff12=((eleven-alt)/two)*cutoff2+&
                          ((alt-nine)/two)*cutoff1
                 cutoff23=((six-alt)/two)*cutoff3+&
                          ((alt-four)/two)*cutoff2

                 if(alt>eleven) cutoff=cutoff1
                 if((alt<=eleven).and.(alt>nine)) cutoff=cutoff12
                 if((alt<=nine).and.(alt>six)) cutoff=cutoff2
                 if((alt<=six).and.(alt>four)) cutoff=cutoff23
                 if(alt<=four) cutoff=cutoff3
 
                 cutoff=three*cutoff*r0_01

                 if(abs(rdiagbuf(5,i)) > cutoff) then
                    qcfail(i)=.true.
                    data(ier,i) = zero
                    ratio_errors(i) = zero
                    muse(i) = .false.
                 end if
              end if ! gross qc check 
 
           end if ! qc checks (only below 30km)

!          Remove obs above 30 km in order to avoid increments at top model
           if(alt > gpstop) then
              data(ier,i) = zero
              ratio_errors(i) = zero
              qcfail_high(i)=one 
              muse(i)=.false.
           endif

!         Remove MetOP/GRAS data below 8 km
          if ((alt <= eight) .and. ((data(isatid,i)==4) .or. (data(isatid,i)==3))) then
             data(ier,i) = zero
             ratio_errors(i) = zero
             qcfail(i)=.true.
             muse(i)=.false.
          endif

!          If obs is "acceptable", compute coefficients for adjoint
           if ((data(ier,i)*ratio_errors(i)) > tiny_r_kind) then
 
              if(k1==1) then
                 tmean=tges(k1)
              else
                 tmean=(tges(k1)+tges(k1-1))/two
              endif
 
              mult_p=pressure(i)*((n_a/trefges)*fact+&
                     (n_b/(trefges**2*pw))*qrefges*fact**2+&
                     (k4/(trefges*pw))*qrefges*fact)
 
!             term for q_TL
              termq(i)   = n_a*(pressure(i)/trefges)*fv+&
                   (n_b/(trefges**2*pw))*pressure(i)*fact**2+&
                   (n_b/(trefges**2*pw))*pressure(i)*qrefges*two*fv*fact-&
                   (n_b/(trefges**2*pw**2))*pressure(i)*qrefges*fact**2*(one-eps)+&
                   (k4*pressure(i)/(trefges*pw))*(fv*qrefges+fact)-&
                   (k4/(trefges*pw**2))*fact*qrefges*pressure(i)*(one-eps)

!             term for pk_TL
              termpk(i) = mult_p/exp(prsltmp(k1))

!             term for pl_TL(j) and pl_TL(j-1)
              if (k1 >= 2) then
                 do j=2,k1
                    termpl1(j,i) = mult_p*two*tges(j-1)/&
                                  ((trefges+tmean)*exp(prsltmp(j-1)))
                    termpl2(j,i) = mult_p*two*tges(j-1)/&
                                  ((trefges+tmean)*exp(prsltmp(j)))
                 end do
              endif

!             term for t_TL
              termt(i) = mult_p*(prsltmp(k1)-pobl)/(trefges+tmean)-&
                  n_a*fact*(pressure(i)/trefges**2)-n_b*qrefges*fact**2*two*&
                  (pressure(i)/(trefges**3*pw))-&
                  (k4/(trefges**2*pw))*fact*qrefges*pressure(i)

!             term for tk_TL and tk-1_TL
              termtk(i) =mult_p*(prsltmp(k1)-pobl)/(two*(trefges+tmean))
 
!             term for tl_TL(j-1)
              if (k1 >= 2) then
                 do j=2,k1
                    termtl(j,i)= mult_p*&
                        two*((prsltmp(j-1)-prsltmp(j))/(trefges+tmean))
                 end do
              endif
           endif

        end if ! obs inside the vertical grid
     endif ! (in_curbin)
  end do ! end of loop over observations

! Loop over observation profiles. Compute penalty
! terms, and accumulate statistics.
  if(last_pass) then
     do i=1,nobs-1
     
        if(r1em3*rdiagbuf(7,i) <= 3.0_r_kind) then ! check for SR-likely conditions below 3 km
            kprof = data(iprof,i)
            jprof = data(iprof,i+1)
            if( kprof == jprof .and. .not. qcfail(i) .and. qcfail_loc(i) == zero .and. qcfail_loc(i+1) == zero .and. qcfail_gross(i) == zero)then
                grad_mod=1000.0_r_kind*&
                               ((rdiagbuf(17,i+1)*(one-rdiagbuf(5,i+1)))-(rdiagbuf(17,i)*(one-rdiagbuf(5,i))))/(rdiagbuf(7,i+1)-rdiagbuf(7,i))
                grad_obs=1000.0_r_kind*(rdiagbuf(17,i+1)-rdiagbuf(17,i))/(rdiagbuf(7,i+1)-rdiagbuf(7,i))
                if ((abs(grad_mod)>= half*crit_grad) .or. (abs(grad_obs)>=half*crit_grad)) then
                   qcfail(i)  = .true.
                   if( qcfail_gross(i+1) == zero .and. .not. qcfail(i+1)) then
                       qcfail(i+1)= .true.
                   end if
                end if
            end if
        end if

        if(qcfail(i)) then
           kprof = data(iprof,i)
           do j=1,nobs
              jprof = data(iprof,j)
              if( kprof == jprof .and. .not. qcfail(j) .and. qcfail_loc(j) == zero .and. qcfail_gross(j) == zero)then

!             Remove data below
                 if(r1em3*rdiagbuf(7,j) < r1em3*rdiagbuf(7,i))then
                    if((rdiagbuf(1,i)==41).or.(rdiagbuf(1,i)==722).or.(rdiagbuf(1,i)==723).or.&
                       (rdiagbuf(1,i)==4).or.(rdiagbuf(1,i)==786).or.(rdiagbuf(1,i)==3)) then
                       if(r1em3*rdiagbuf(7,i)<= ten) then
                          qcfail(j) = .true.
                       endif
                    else
                       if(r1em3*rdiagbuf(7,i)< five) then
                          qcfail(j) = .true. 
                       endif
                    endif
                 endif
              end if
           end do
        endif
     end do

     do i=1,nobs

        alt=r1em3*rdiagbuf(7,i) ! altitude in km
        if(qcfail(i)) then
           kprof = data(iprof,i)
           data(ier,i) = zero
           ratio_errors(i) = zero
           muse(i) = .false.
           if ( (rdiagbuf(1,i)==41).or.(rdiagbuf(1,i)==722).or.(rdiagbuf(1,i)==723).or.&
                (rdiagbuf(1,i)==4).or.(rdiagbuf(1,i)==786).or.(rdiagbuf(1,i)==3)) then
              if(alt<=ten) then
                 toss_gps_sub(kprof) = max(toss_gps_sub(kprof),data(ihgt,i))
              endif
           else
              if (alt < five) then
                 toss_gps_sub(kprof) = max(toss_gps_sub(kprof),data(ihgt,i))
              end if
           end if
 


!    Counting obs tossed due to the stats qc check 
!    This calculation will be updated in genstats_gps due to toss_gps_sub

           if (luse(i)) then
              if(data(ilate,i)> r20) then
                 awork(22) = awork(22)+one                !NH
              else if(data(ilate,i)< -r20)then
                 awork(23) = awork(23)+one                !SH
              else
                 awork(24) = awork(24)+one                !TR
              end if
           end if
        end if
     end do
  endif ! (last_pass)


! Loop to load arrays used in statistics output
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(last_pass) then
        if (ratio_errors(i)*data(ier,i) <= tiny_r_kind) muse(i) = .false.
        ikx=nint(data(ikxx,i))
        dtime=data(itime,i)
 
        ! flags for observations that failed qc checks
        ! zero = observation is good

        if(qcfail_gross(i) == one)   rdiagbuf(10,i) = three
        if(qcfail(i))                rdiagbuf(10,i) = four !modified in genstats due to toss_gps_sub
        if(qcfail_loc(i) == one)     rdiagbuf(10,i) = one
        if(qcfail_high(i) == one)    rdiagbuf(10,i) = two

        if(muse(i)) then            ! modified in genstats_gps due to toss_gps_sub
           rdiagbuf(12,i) = one     ! minimization usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,i) = -one
        endif
 
        if (ratio_errors(i)*data(ier,i)>tiny_r_kind) then
           err_final = ratio_errors(i)*data(ier,i)
        else
           err_final = tiny_r_kind
        endif

        errinv_input  = tiny_r_kind
        errinv_adjst  = tiny_r_kind
        errinv_final  = tiny_r_kind


        if (error(i)>tiny_r_kind)       errinv_input=error(i)
        if (error_adjst(i)>tiny_r_kind) errinv_adjst=error_adjst(i)
        if (err_final>tiny_r_kind)      errinv_final=err_final
 
        rdiagbuf(13,i) = zero         ! nonlinear qc relative weight - will be defined in genstats_gps
        rdiagbuf(14,i) = errinv_input ! original inverse gps obs error (N**-1)
        rdiagbuf(15,i) = errinv_adjst ! original + represent error inverse gps 
                                      ! obs error (N**-1)
        rdiagbuf(16,i) = errinv_final ! final inverse observation error due to 
                                      ! superob factor (N**-1)
                                      ! modified in genstats_gps
     endif ! (last_pass)

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
           if (.not.associated(obsdiags(i_gps_ob_type,ibin)%head)) then
              allocate(obsdiags(i_gps_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupref: failure to allocate obsdiags',istat
                 call stop2(282)
              end if
              obsdiags(i_gps_ob_type,ibin)%tail => obsdiags(i_gps_ob_type,ibin)%head
           else
              allocate(obsdiags(i_gps_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupref: failure to allocate obsdiags',istat
                 call stop2(283)
              end if
              obsdiags(i_gps_ob_type,ibin)%tail => obsdiags(i_gps_ob_type,ibin)%tail%next
           end if
           allocate(obsdiags(i_gps_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_gps_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_gps_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_gps_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_gps_ob_type,ibin)%tail%indxglb=i
           obsdiags(i_gps_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_gps_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_gps_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_gps_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_gps_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_gps_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_gps_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_gps_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = i
           my_diag%ich = 1

        else 
           if (.not.associated(obsdiags(i_gps_ob_type,ibin)%tail)) then
              obsdiags(i_gps_ob_type,ibin)%tail => obsdiags(i_gps_ob_type,ibin)%head
           else
              obsdiags(i_gps_ob_type,ibin)%tail => obsdiags(i_gps_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_gps_ob_type,ibin)%tail%indxglb/=i) then
              write(6,*)'setupref: index error'
              call stop2(284)
           end if
        endif
        if(last_pass .and. nobskeep>0)muse(i)=obsdiags(i_gps_ob_type,ibin)%tail%muse(nobskeep)
     endif

     if(last_pass) then

!       Save values needed for generation of statistics for all observations
        if(.not. associated(gps_allhead(ibin)%head))then
           allocate(gps_allhead(ibin)%head,stat=istat)
           if(istat /= 0)write(6,*)' failure to write gps_allhead '
           gps_alltail(ibin)%head => gps_allhead(ibin)%head
        else
           allocate(gps_alltail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write gps_alltail%llpoint '
           gps_alltail(ibin)%head => gps_alltail(ibin)%head%llpoint
        end if
        gps_alltail(ibin)%head%idv=is
        gps_alltail(ibin)%head%iob=i

        allocate(gps_alltail(ibin)%head%rdiag(nreal),stat=istat)
        if (istat/=0) write(6,*)'SETUPREF:  allocate error for gps_point, istat=',istat
 
        gps_alltail(ibin)%head%ratio_err= ratio_errors(i)
        gps_alltail(ibin)%head%obserr   = data(ier,i)
        gps_alltail(ibin)%head%dataerr  = data(ier,i)*data(igps,i)
        gps_alltail(ibin)%head%pg       = cvar_pg(ikx)
        gps_alltail(ibin)%head%b        = cvar_b(ikx)
        gps_alltail(ibin)%head%loc      = data(ihgt,i)
        gps_alltail(ibin)%head%kprof    = data(iprof,i)
        gps_alltail(ibin)%head%type     = data(ikxx,i)
        gps_alltail(ibin)%head%luse     = luse(i) ! logical
        gps_alltail(ibin)%head%muse     = muse(i) ! logical
        gps_alltail(ibin)%head%cdiag    = cdiagbuf(i)

!       Fill obs diagnostics structure
        if(luse_obsdiag)then
           obsdiags(i_gps_ob_type,ibin)%tail%luse=luse(i)
           obsdiags(i_gps_ob_type,ibin)%tail%muse(jiter)=muse(i)
           obsdiags(i_gps_ob_type,ibin)%tail%nldepart(jiter)=data(igps,i)
           obsdiags(i_gps_ob_type,ibin)%tail%wgtjo=(data(ier,i)*ratio_errors(i))**2
        end if

!       Load additional obs diagnostic structure
        ioff=mreal
        if (lobsdiagsave) then
           do jj=1,miter
              ioff=ioff+1
              if (obsdiags(i_gps_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,i) = one
              else
                 rdiagbuf(ioff,i) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,i) = obsdiags(i_gps_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,i) = obsdiags(i_gps_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,i) = obsdiags(i_gps_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

        do j=1,nreal
           gps_alltail(ibin)%head%rdiag(j)= rdiagbuf(j,i)
        end do

!       If obs is "acceptable", load array with obs info for use
!       in inner loop minimization (int* and stp* routines)

        if ( in_curbin .and. muse(i) ) then
 
           if(.not. associated(gpshead(ibin)%head))then
              allocate(gpshead(ibin)%head,stat=istat)
              if(istat /= 0)write(6,*)' failure to write gpshead '
              gpstail(ibin)%head => gpshead(ibin)%head
           else
              allocate(gpstail(ibin)%head%llpoint,stat=istat)
              if(istat /= 0)write(6,*)' failure to write gpstail%llpoint '
              gpstail(ibin)%head => gpstail(ibin)%head%llpoint
           end if

           m_alloc(ibin) = m_alloc(ibin) +1
           my_head => gpstail(ibin)%head
           my_head%idv = is
           my_head%iob = i

           allocate(gpstail(ibin)%head%jac_t(nsig),gpstail(ibin)%head%jac_q(nsig), &
                    gpstail(ibin)%head%jac_p(nsig+1),gpstail(ibin)%head%ij(4,nsig),stat=istat)
           if (istat/=0) write(6,*)'SETUPREF:  allocate error for gps_point, istat=',istat


           gps_alltail(ibin)%head%mmpoint => gpstail(ibin)%head

!          Set (i,j,k) indices of guess gridpoint that bound obs location
           call get_ij(mm1,data(ilat,i),data(ilon,i),gps_ij,gpstail(ibin)%head%wij(1))

           do j=1,nsig
              gpstail(ibin)%head%ij(1,j)=gps_ij(1)+(j-1)*latlon11
              gpstail(ibin)%head%ij(2,j)=gps_ij(2)+(j-1)*latlon11
              gpstail(ibin)%head%ij(3,j)=gps_ij(3)+(j-1)*latlon11
              gpstail(ibin)%head%ij(4,j)=gps_ij(4)+(j-1)*latlon11
           enddo
           do j=1,nsig
              gpstail(ibin)%head%jac_q(j)=zero
              gpstail(ibin)%head%jac_t(j)=zero
              gpstail(ibin)%head%jac_p(j)=zero
           enddo
           gpstail(ibin)%head%jac_p(nsig+1)=zero
           dpres=data(ihgt,i)
           k=dpres
           k1=min(max(1,k),nsig)
           k2=max(1,min(k+1,nsig))
           gpstail(ibin)%head%jac_t(k1)=gpstail(ibin)%head%jac_t(k1)+termtk(i)
           gpstail(ibin)%head%jac_p(k1)=gpstail(ibin)%head%jac_p(k1)+termpk(i)
           if(k1 == 1)then
              gpstail(ibin)%head%jac_t(k1)=gpstail(ibin)%head%jac_t(k1)+termtk(i)
           else
              gpstail(ibin)%head%jac_t(k1-1)=gpstail(ibin)%head%jac_t(k1-1)+termtk(i)
              do j=2,k1
                 gpstail(ibin)%head%jac_t(j-1)=gpstail(ibin)%head%jac_t(j-1)+termtl(j,i)
                 gpstail(ibin)%head%jac_p(j-1)=gpstail(ibin)%head%jac_p(j-1)+termpl1(j,i)
                 gpstail(ibin)%head%jac_p(j)=gpstail(ibin)%head%jac_p(j)-termpl2(j,i)
              end do
           end if
!          delz=dpres-float(k1)
           kl=dpresl(i)
           k1l=min(max(1,kl),nsig)
           k2l=max(1,min(kl+1,nsig))
           delz=dpresl(i)-float(k1l)
           delz=max(zero,min(delz,one))
           gpstail(ibin)%head%jac_t(k1l)=gpstail(ibin)%head%jac_t(k1l)+termt(i)*(one-delz)
           gpstail(ibin)%head%jac_t(k2l)=gpstail(ibin)%head%jac_t(k2l)+termt(i)*delz
           gpstail(ibin)%head%jac_q(k1l)=gpstail(ibin)%head%jac_q(k1l)+termq(i)*(one-delz)
           gpstail(ibin)%head%jac_q(k2l)=gpstail(ibin)%head%jac_q(k2l)+termq(i)*delz
           gpstail(ibin)%head%res       = data(igps,i)
           gpstail(ibin)%head%err2      = data(ier,i)**2
           gpstail(ibin)%head%raterr2   = ratio_errors(i)**2    
           gpstail(ibin)%head%time      = data(itime,i)
           gpstail(ibin)%head%b         = cvar_b(ikx)
           gpstail(ibin)%head%pg        = cvar_pg(ikx)
           gpstail(ibin)%head%luse      = luse(i)

           if(luse_obsdiag)then
              gpstail(ibin)%head%diags     => obsdiags(i_gps_ob_type,ibin)%tail

              my_head => gpstail(ibin)%head
              my_diag => gpstail(ibin)%head%diags
              if(my_head%idv /= my_diag%idv .or. &
                 my_head%iob /= my_diag%iob ) then
                 call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =', &
                       (/is,i,ibin/))
                 call perr(myname,'my_head%(idv,iob) =',(/my_head%idv,my_head%iob/))
                 call perr(myname,'my_diag%(idv,iob) =',(/my_diag%idv,my_diag%iob/))
                 call die(myname)
              endif
           endif

        endif ! (in_curbin .and. muse=1)
     endif ! (last_pass)
  end do

  ! Release memory of local guess arrays
  call final_vars_

  ! Save these arrays for later passes
  data_ier (:)=data(ier ,:)
  data_ihgt(:)=data(ihgt,:)
  data_igps(:)=data(igps,:)

  call dtime_show(myname,'diagsave:ref',i_gps_ob_type)
  call gpsrhs_unaliases(is)
  if(last_pass) call gpsrhs_dealloc(is)

! End of routine

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::q', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get z ...
     varname='z'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_z(size(rank2,1),size(rank2,2),nfldsig))
         ges_z(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_z(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tv ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_tv))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tv(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_tv(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_tv(:,:,:,ifld)=rank3
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
  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_z )) deallocate(ges_z )
  end subroutine final_vars_

end subroutine setupref
