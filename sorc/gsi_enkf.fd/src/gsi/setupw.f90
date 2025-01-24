module w_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupw; end interface

contains
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  setupw --- Compute rhs of oi for wind component obs
!
! !INTERFACE:
!

subroutine setupw(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)

! !USES:

  use mpeu_util, only: die,perr,getindex
  use state_vectors, only: svars3d, levels
  use kinds, only: r_kind,r_single,r_double,i_kind
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert

  use obsmod, only: rmiss_single,perturb_obs,oberror_tune,lobsdiag_forenkf,&
       lobsdiagsave,nobskeep,lobsdiag_allocated,&
       time_offset,bmiss,ianldate,aircraft_recon
  use m_obsNode, only: obsNode
  use m_wNode, only: wNode
  use m_wNode, only: wNode_appendto
  use m_wNode, only: wNode_ich0
  use m_wNode, only: wNode_ich0_PBL_pseudo
  use m_obsLList, only: obsLList

  use obsmod, only: luse_obsdiag
  use obsmod, only: netcdf_diag, binary_diag, dirname
  use obsmod, only: l_obsprvdiag
  use obsmod, only: neutral_stability_windfact_2dvar,use_similarity_2dvar
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_metadata_to_single
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use gsi_4dvar, only: nobs_bins,hr_obsbin,min_offset
  use qcmod, only: npres_print,ptop,pbot,dfact,dfact1,qc_satwnds,njqc,vqc
  use qcmod, only: nvqc
  use oneobmod, only: oneobtest,oneob_type,magoberr,maginnov 
  use gridmod, only: get_ijk,nsig,twodvar_regional,regional,wrf_nmm_regional,&
      rotate_wind_xy2ll,pt_ll,fv3_regional
  use guess_grids, only: nfldsig,hrdifsig,geop_hgtl,sfcmod_gfs
  use guess_grids, only: tropprs,sfcmod_mm5
  use guess_grids, only: ges_lnprsl,comp_fact10,pbl_height
  use constants, only: zero,half,one,tiny_r_kind,two, &
           three,rd,grav,four,five,huge_single,r1000,wgtlim,r10,r400
  use constants, only: grav_ratio,flattening,deg2rad, &
       grav_equator,somigliana,semi_major_axis,eccentricity
  use jfunc, only: jiter,last,jiterstart,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype,ibeta,ikapa
  use converr_uv, only: ptabl_uv
  use converr, only: ptabl
  use rapidrefresh_cldsurf_mod, only: l_PBL_pseudo_SurfobsUV, pblH_ration,pps_press_incr
  use rapidrefresh_cldsurf_mod, only: l_closeobs, i_gsdqc

  use m_dtime, only: dtime_setup, dtime_check

  use gsi_bundlemod, only : gsi_bundlegetpointer
  use hdraobmod, only: nhduv,hduvlist
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use sparsearr, only: sparr2, new, size, writearray, fullarray
  use aux2dvarflds, only: rtma_comp_fact10


  ! The following variables are the coefficients that describe the
  ! linear regression fits that are used to define the dynamic
  ! observation error (DOE) specifications for all reconnissance
  ! observations collected within hurricanes/tropical cyclones; these
  ! apply only to the regional forecast models (e.g., HWRF); Henry
  ! R. Winterbottom (henry.winterbottom@noaa.gov).
  
  use obsmod, only: uv_doe_a_236,uv_doe_a_237,uv_doe_b_236,uv_doe_b_237

  implicit none
  
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

! !INPUT PARAMETERS:

   integer(i_kind)                                  ,intent(in   ) :: lunin ! unit from which to read observations
   integer(i_kind)                                  ,intent(in   ) :: mype  ! mpi task id
   integer(i_kind)                                  ,intent(in   ) :: nele  ! number of data elements per observation
   integer(i_kind)                                  ,intent(in   ) :: nobs  ! number of observations
   integer(i_kind)                                  ,intent(in   ) :: is    ! ndat index
   logical                                          ,intent(in   ) :: conv_diagsave ! logical to save innovation dignostics
   
! !INPUT/OUTPUT PARAMETERS:

   real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork ! obs-ges stats
   real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork ! data counts and gross checks

!
! !DESCRIPTION:  For wind component observations, this routine
!  \begin{enumerate}
!       \item reads obs assigned to given mpi task (geographic region),
!       \item simulates obs from guess,
!       \item apply some quality control to obs,
!       \item load weight and innovation arrays used in minimization
!       \item collects statistics for runtime diagnostic output
!       \item writes additional diagnostic information to output file
!  \end{enumerate}
!
! !REVISION HISTORY:
!
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu - ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-17  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue; added intent/only's
!   2004-10-06  parrish - increase size of vwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-22  jung - add modis winds
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-21  su  - modified variational qc and diagnose output
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-22  wu - add option to perturb conventional obs
!   2005-11-29 derber - remove psfcg and use ges_lnps instead
!   2006-01-13 treadon - correct bugs in modis wind qc
!   2006-01-31  todling - storing wgt/wgtlim in diag file instead of wgt only
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-08  treadon - correct vertical dimension (nsig) in call tintrp2a(ges_tv...)
!   2006-02-15  treadon - use height when processing type 223, 224, 229 winds
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-04-03  derber  - fix bugs and move all surface data to height calculation
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!   2006-07-31  kleist - use ges_ps instead of ln(ps)
!   2006-08-28      su - fix a bug in variational qc
!   2006-11-30  jung/sienkiewicz - add type 259 for modis winds
!   2006-10-28      su - turn off rawinsonde Vqc at south hemisphere
!   2007-03-09  su     - modify observation pertabation for adjusting obs error
!   2007-03-19  tremolet - binning of observations
!   2007-03-27  li.bi - add qc for type 289 windsat winds
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28  su     - modify observation gross check error 
!   2008-03-24  wu     - oberror tuning and perturb obs
!   2008-03-31  li.bi - add qc for type 290 ascat winds
!   2008-05-20  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2008-12-03  todling - changed handle of tail%time
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2009-02-06  pondeca - for each observation site, add the following to the
!                         diagnostic file: local terrain height, dominate surface
!                         type, station provider name, and station subprovider name
!   2010-11-25  su      - data items to hold quality mark for satellite wind 
!   2011-03-08  parrish - for regional=.true., convert wind components in rdiagbuf from grid relative
!                           to earth relative, using subroutine rotate_wind_xy2ll.
!   2011-05-05  su      - ome quality control for satellite satellite winds 
!   2012-01-10  hu      - add additional quality control for PBL profiler 223, 224, 227 
!   2011-12-14  wu      - add code for rawinsonde level enhancement ( ext_sonde )
!   2012-07-19  todling - add qc_satwnds flag to allow bypass QC-satwinds (QC not good for GMAO)
!   2011-10-14  Hu      - add code for producing pseudo-obs in PBL 
!                               layer based on surface obs UV
!   2013-01-08  Su      -add more quality control for satellite winds and profiler winds
!   2013-01-26  parrish - change grdcrd to grdcrd1, intrp2a to intrp2a11, tintrp2a to tintrp2a1, tintrp2a11,
!                           tintrp3 to tintrp31 (so debug compile works on WCOSS)
!   2013-02-15  parrish - WCOSS debug runtime error--ikx outside range 1 to nconvtype.  Add counter
!                            num_bad_ikx and print 1st 10 instances of ikx out of range
!                            and also print num_bad_ikx after all data processed if > 0 .
!   2013-05-24  wu      - move rawinsonde level enhancement ( ext_sonde ) to read_prepbufr
!   2013-07-19  Hu/Olson/Carley  - Add tall tower (type=261) winds
!   2013-10-19  todling - metguess now holds background
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-04-12       su - add non linear qc from Purser's scheme
!   2014-12-30  derber  - Modify for possibility of not using obsdiag
!   2015-05-01  Liu Ling - Added ISS Rapidscat wind (u,v) qc 
!   2015-03-14  Nebuda  - add departure check and near surface check for clear air WV AMV (WVCS) from GOES type 247
!   2015-10-01  guo     - full res obvsr: index to allow redistribution of obsdiags
!   2015-12-21  yang    - Parrish's correction to the previous code in new varqc.
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2016-11-29  shlyaeva - save linearized H(x) for EnKF
!   2016-12-09  mccarty - add netcdf_diag capability
!   2016-12-13  pondeca - add Tyndall & Horel QC for mesonet winds (WAF 2013, Vol. 8, pg. 285) to GSI's 2dvar option
!   2017-03-31  Hu      -  addd option l_closeobs to use closest obs to analysis
!                                     time in analysis
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!   2018-04-09  pondeca -  introduce duplogic to correctly handle the characterization of
!                          duplicate obs in twodvar_regional applications
!   2019-06-07  levine/pondeca - for twodvar_regional applications, switch from processing surface obs 
!                                with reported pressure to surface obs with reported geometric height.
!                                This is mainly to account for mesonets with sensor height < 10 m.
!   2019-07-12  levine  -  introduce logic to read in mesonet wind sensor heights from prepbufr
!                          rather than assuming sensor height is 10 m AGL.
!   2019-08-12  zhang/levine/pondeca -  add option to adjust 10-m bckg wind with the help of similarity
!                                       theory in twodvar_regional applications
!   2019-09-20  Su      -  remove current VQC part and add subroutine call on VQC
!   2019-09-25  Su      -  put hibert curve on aircraft data
!   2020-01-27  Winterbottom - moved the linear regression derived
!                              coefficients for the dynamic observation
!                              error (DOE) calculation to the namelist
!                              level; they are now loaded by
!                              aircraftinfo.
!   2020-05-04  wu   - no rotate_wind for fv3_regional
!   2021-10-xx  pondeca/morris/zhao - added observation provider/subprovider
!                         information in diagonostic file, which is used
!                         in offline observation quality control program (AutoObsQC) 
!                         for 3D-RTMA (if l_obsprvdiag is true).
!
! REMARKS:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
! !AUTHOR: parrish          org: np22                date: 1990-10-06
!
!EOP
!-------------------------------------------------------------------------

! Declare local parameters
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r0_1=1.0_r_kind
  real(r_kind),parameter:: r6=6.0_r_kind
  real(r_kind),parameter:: r7=7.0_r_kind
  real(r_kind),parameter:: r15=15.0_r_kind
  real(r_kind),parameter:: r20=20.0_r_kind
  real(r_kind),parameter:: r50=50.0_r_kind
  real(r_kind),parameter:: r200=200.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: r0_1_bmiss=0.1_r_kind*bmiss

  character(len=*),parameter:: myname='setupw'

! Declare external calls for code analysis
  external:: intrp2a11,tintrp2a1,tintrp2a11
  external:: tintrp31
  external:: grdcrd1
  external:: stop2

! Declare local variables

  real(r_double) rstation_id
  real(r_kind) qcu,qcv,trop5,tfact,fact
  real(r_kind) scale,ratio,obserror,obserrlm
  real(r_kind) residual,ressw,ress,vals,val2,dudiff,dvdiff,rat_err2u
  real(r_kind) valqc,valu,valv,dx10,rlow,rhgh,drpx,prsfc,var_jb,rat_err2v
  real(r_kind) cg_t,cvar,wgt,term,qcgross,valqcu,valqcv
  real(r_kind) presw,factw,dpres,ugesin,vgesin,rwgt,dpressave
  real(r_kind) sfcchk,prsln2,error,dtime,dlon,dlat,r0_001,rsig,thirty,rsigp
  real(r_kind) ratio_errors,goverrd,spdges,spdob,ten,psges,zsges
  real(r_kind) slat,sin2,termg,termr,termrg,pobl,uob,vob
  real(r_kind) uob_reg,vob_reg,uob_e,vob_e,dlon_e,uges_e,vges_e,dudiff_e,dvdiff_e
  real(r_kind) dz,zob,z1,z2,p1,p2,dz21,dlnp21,spdb,dstn
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,skint,sfcr
  real(r_kind) dudiff_opp, dvdiff_opp, vecdiff, vecdiff_opp
  real(r_kind) dudiff_opp_rs, dvdiff_opp_rs, vecdiff_rs, vecdiff_opp_rs
  real(r_kind) oscat_vec,rapidscat_vec
! real(r_kind) ascat_vec
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig)::prsltmp,tges,zges
  real(r_kind) wdirob,wdirgesin,wdirdiffmax
  real(r_kind),dimension(34)::ptabluv
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  integer(i_kind) i,nchar,nreal,k,j,l,ii,itype,ijb
! Variables needed for new polar winds QC based on Log Normalized Vector Departure (LNVD)
  real(r_kind) LNVD_wspd
  real(r_kind) LNVD_omb
  real(r_kind) LNVD_ratio
  real(r_kind) LNVD_threshold

  integer(i_kind) jsig,mm1,iptrbu,iptrbv,jj,icat
  integer(i_kind) k1,k2,ikxx,nn,isli,ibin,ioff,ioff0
  integer(i_kind) ier,ilon,ilat,ipres,iuob,ivob,id,itime,ikx,ielev,iqc
  integer(i_kind) ihgt,ier2,iuse,ilate,ilone
  integer(i_kind) izz,iprvd,isprvd
  integer(i_kind) idomsfc,isfcr,iskint,iff10
  integer(i_kind) ibb,ikk,ihil,idddd

  integer(i_kind) num_bad_ikx,iprev_station

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  type(sparr2) :: dhx_dx_u, dhx_dx_v
  integer(i_kind) :: iz, u_ind, v_ind, nnz, nind
  real(r_kind) :: delz
  logical z_height,sfc_data
  logical,dimension(nobs):: luse,muse
  logical:: muse_u,muse_v
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical lowlevelsat,duplogic
  logical msonetob
  logical proceed

  logical:: l_pbl_pseudo_itype
  integer(i_kind):: ich0

  logical:: in_curbin, in_anybin, save_jacobian
  type(wNode),pointer :: my_head
  type(obs_diag),pointer :: jj_diag
  type(obs_diag),pointer :: my_diagu, my_diagu_pbl
  type(obs_diag),pointer :: my_diagv, my_diagv_pbl
  type(obs_diags),pointer :: my_diagLL
  real(r_kind) :: thisPBL_height,ratio_PBL_height,prest,prestsfc,dudiffsfc,dvdiffsfc
  real(r_kind) :: hr_offset
  real(r_kind) :: magomb


  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv

  type(obsLList),pointer,dimension(:):: whead
  whead => obsLL(:)

  save_jacobian = conv_diagsave .and. jiter==jiterstart .and. lobsdiag_forenkf

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!******************************************************************************
! Read and reformat observations in work arrays.
  spdb=zero

  read(lunin)data,luse,ioid


!    index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  ihgt=5      ! index of height
  iuob=6      ! index of u observation
  ivob=7      ! index of v observation
  id=8        ! index of station id
  itime=9     ! index of observation time in data array
  ikxx=10     ! index of ndex ob type in convinfo file
  ielev=11    ! index of station elevation
  iqc=12      ! index of quality mark
  ier2=13     ! index of original-original obs error ratio
  iuse=14     ! index of use parameter
  idomsfc=15  ! index of dominant surface type
  iskint=16   ! index of surface skin temperature
  iff10=17    ! index of 10 meter wind factor
  isfcr=18    ! index of surface roughness
  ilone=19    ! index of longitude (degrees)
  ilate=20    ! index of latitude (degrees)
  izz=21      ! index of surface height
  iprvd=22    ! index of observation provider
  isprvd=23   ! index of observation subprovider
  icat=24     ! index of data level category
  ijb=25      ! index of non linear qc parameter
  ihil=26     ! index of  hilbert curve weight
  iptrbu=27   ! index of u perturbation
  iptrbv=28   ! index of v perturbation

  mm1=mype+1
  scale=one
  rsig=nsig
  thirty = 30.0_r_kind
  ten = 10.0_r_kind
  r0_001=0.001_r_kind
  rsigp=rsig+one
  goverrd=grav/rd
  var_jb=zero

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     ioff0=25
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+7*miter+2
     if (twodvar_regional .or. l_obsprvdiag) then
       nreal=nreal+2                           ! account for idomsfc,izz
       allocate(cprvstg(nobs),csprvstg(nobs))  ! obs provider info
     endif
     if (save_jacobian) then
       nnz   = 2                   ! number of non-zero elements in dH(x)/dx profile
       nind   = 1
       call new(dhx_dx_u, nnz, nind)
       call new(dhx_dx_v, nnz, nind)
       nreal = nreal + 2*size(dhx_dx_u)
     endif

     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     if (netcdf_diag) call init_netcdf_diag_
  end if

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter .and. nint(data(iqc,i)) < 8
  end do
!  If HD raobs available move prepbufr version to monitor
  if(nhduv > 0)then
     iprev_station=0
     do i=1,nobs
        ikx=nint(data(ikxx,i))
        itype=ictype(ikx)
        if(itype == 220 .or. itype == 221) then
           rstation_id     = data(id,i)
           read(station_id,'(i5,3x)',err=1200) idddd
           if(idddd == iprev_station)then
             data(iuse,i)=108._r_kind
             muse(i) = .false.
           else
              stn_loop:do j=1,nhduv
                if(idddd == hduvlist(j))then
                   iprev_station=idddd
                   data(iuse,i)=108._r_kind
                   muse(i) = .false.
                   exit stn_loop
                end if
              end do stn_loop
           end if
        end if
1200    continue
     end do
  end if

!  handle multiple-report observations at a station
  hr_offset=min_offset/60.0_r_kind
  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if (twodvar_regional) then
           duplogic=data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l)
         else
           duplogic=data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ipres,k) == data(ipres,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l)
        end if

        if (duplogic) then
           if(l_closeobs) then
              if(abs(data(itime,k)-hr_offset)<abs(data(itime,l)-hr_offset)) then
                  muse(l)=.false.
              else
                  muse(k)=.false.
              endif
!              write(*,'(a,2f10.5,2I8,2L10)') 'chech wind obs time==',&
!              data(itime,k)-hr_offset,data(itime,l)-hr_offset,k,l,&
!                           muse(k),muse(l)
           else
              tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
              dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
              dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
           endif
        end if
     end do
  end do

  call dtime_setup()
  num_bad_ikx=0
  loop_for_all_obs: &
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     ikx=nint(data(ikxx,i))

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        rstation_id     = data(id,i)
        error=data(ier2,i)
        var_jb=data(ijb,i)
        if(ikx < 1 .or. ikx > nconvtype) then
           num_bad_ikx=num_bad_ikx+1
           if(num_bad_ikx<=10) write(6,*)' in setupw, bad ikx, ikx,i,nconvtype=',ikx,i,nconvtype,mype
           cycle
        end if
        isli = data(idomsfc,i)
     endif

     if(ikx < 1 .or. ikx > nconvtype) cycle
     itype=ictype(ikx)

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif

     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     if (luse_obsdiag) my_diagLL => odiagLL(ibin)

     ! Flag static conditions to turn pbl_pseudo_surfobs on
     l_pbl_pseudo_itype = l_PBL_pseudo_SurfobsUV .and.        &
                          ( itype==281 .or. itype==283 .or.itype==287 )

     if (luse_obsdiag) then
        my_diagu => null()
        my_diagv => null()
        my_diagu_pbl => null()
        my_diagv_pbl => null()

        ich0 = wNode_ich0
        if(l_pbl_pseudo_itype) ich0 = wNode_ich0_pbl_pseudo

        do jj=1,ich0+2  ! "2" for there are u and v components
           jj_diag => obsdiagLList_nextNode(my_diagLL, &
                create = .not.lobsdiag_allocated ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = jj             ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )

           if(.not.associated(jj_diag)) then
             call perr(myname,'obsdiagLList_nextNode(), create =',.not.lobsdiag_allocated)
             call perr(myname,'                           ich0 =',ich0)
             call perr(myname,'                             jj =',jj)
             call  die(myname)
           endif

           select case(jj)
           case(1); my_diagu => jj_diag
           case(2); my_diagv => jj_diag
           case(3); my_diagu_pbl => jj_diag
           case(4); my_diagv_pbl => jj_diag
           end select
        enddo
     endif

     if(.not.in_curbin) cycle

!    Load observation error and values into local variables
     obserror = max(cermin(ikx),min(cermax(ikx),data(ier,i)))
     uob = data(iuob,i)
     vob = data(ivob,i)
     spdob=sqrt(uob*uob+vob*vob)
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

!    Type 221=pibal winds contain a mixture of wind observations reported
!    by pressure and others by height.  Those levels only reported by 
!    pressure have a missing value (ie, large) value for the reported 
!    height.  The logic below determines whether to process type 221 
!    wind observations using height or pressure as the vertical coordinate.
!    If height is not bad (less than r0_1_bmiss), we use height in the
!    forward model.  Otherwise, use reported pressure.

     z_height = .false.
     if ((itype>=221 .and. itype <= 229) .and. (data(ihgt,i)<r0_1_bmiss)) z_height = .true.
     if ((itype==261) .and. (data(ihgt,i)<r0_1_bmiss)) z_height = .true.
     if (itype == 218) z_height = .true.


!    Process observations reported with height differently than those
!    reported with pressure.  Type 223=profiler and 224=vadwnd are 
!    encoded in NCEP prepbufr files with geometric height above 
!    sea level.  Type 229=pibal profiler is reported using 
!    geopotenital height.  Some type 221=pibal wind observations are
!    also repoted using geopotential height.

     sfc_data = (itype >=280 .and. itype < 300)
     if (z_height .or. sfc_data) then

        drpx = zero
        dpres = data(ihgt,i)
        dstn = data(ielev,i)
        call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)
!       Subtract off combination of surface station elevation and
!       model elevation depending on how close to surface
        fact = zero
        if(dpres-dstn > 10._r_kind)then
           if(dpres-dstn > r1000)then
              fact = one
           else
              fact=(dpres-dstn)/990._r_kind
           end if
        end if
        dpres=dpres-(dstn+fact*(zsges-dstn))
        if(itype==261) dpres = data(ihgt,i)

!       Get guess surface elevation and geopotential height profile 
!       at observation location.
        call tintrp2a1(geop_hgtl,zges,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)

!       For observation reported with geometric height above sea level,
!       convert geopotential to geometric height.

        if (((itype>=223 .and. itype<=228) .or. itype == 218 .or. sfc_data) .and. .not.twodvar_regional) then
!          Convert geopotential height at layer midpoints to geometric 
!          height using equations (17, 20, 23) in MJ Mahoney's note 
!          "A discussion of various measures of altitude" (2001).  
!          Available on the web at
!          http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!          termg  = equation 17
!          termr  = equation 21
!          termrg = first term in the denominator of equation 23
!          zges  = equation 23

           slat = data(ilate,i)*deg2rad
           sin2  = sin(slat)*sin(slat)
           termg = grav_equator * &
                ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
           termr = semi_major_axis /(one + flattening + grav_ratio -  &
                two*flattening*sin2)
           termrg = (termg/grav)*termr
           do k=1,nsig
              zges(k) = (termr*zges(k)) / (termrg-zges(k))  ! eq (23)
           end do
        else if (twodvar_regional) then
           zges(1) = ten
        endif

!       Given observation height, (1) adjust 10 meter wind factor if
!       necessary, (2) convert height to grid relative units, (3) compute
!       compute observation pressure (for diagnostic purposes only), and
!       (4) compute location of midpoint of first model layer above surface
!       in grid relative units

!       Adjust 10m wind factor if necessary.  Rarely do we have a
!       profiler/vad obs within 10 meters of the surface.  Almost always,
!       the code below resets the 10m wind factor to 1.0 (i.e., no
!       reduction in wind speed due to surface friction).

!       Convert observation height (in dpres) from meters to grid relative
!       units.  Save the observation height in zob for later use.
        zob = dpres
        call grdcrd1(dpres,zges,nsig,1)

!       Interpolate guess u and v to observation location and time.
 
        call tintrp31(ges_u,ugesin,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        call tintrp31(ges_v,vgesin,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)

        iz = max(1, min( int(dpres), nsig))
        delz = max(zero, min(dpres - real(iz,r_kind), one))

        if (save_jacobian) then

           u_ind = getindex(svars3d, 'u')
           if (u_ind < 0) then
              print *, 'Error: no variable u in state vector. Exiting.'
              call stop2(1300)
           endif
           v_ind = getindex(svars3d, 'v')
           if (v_ind < 0) then
              print *, 'Error: no variable v in state vector. Exiting.'
              call stop2(1300)
           endif

           dhx_dx_u%st_ind(1)  = iz               + sum(levels(1:u_ind-1))
           dhx_dx_u%end_ind(1) = min(iz + 1,nsig) + sum(levels(1:u_ind-1))
           dhx_dx_v%st_ind(1)  = iz               + sum(levels(1:v_ind-1))
           dhx_dx_v%end_ind(1) = min(iz + 1,nsig) + sum(levels(1:v_ind-1))

           dhx_dx_u%val(1) = one - delz
           dhx_dx_u%val(2) = delz
           dhx_dx_v%val = dhx_dx_u%val
        endif

        msonetob=itype==288.or.itype==295

        if (zob <= zero .and. twodvar_regional) zob=ten !trap for stations with negative zob

        if (zob > zges(1)) then
           factw=one
        else
           factw = data(iff10,i)
           if(sfcmod_gfs .or. sfcmod_mm5) then
              sfcr = data(isfcr,i)
              skint = data(iskint,i)
              call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
           end if

           if (zob <= ten) then
              if(zob < ten)then
                 if (msonetob .and. twodvar_regional .and. use_similarity_2dvar) then
                    if (neutral_stability_windfact_2dvar) then
                       sfcr = data(isfcr,i)
                       factw=log(max(sfcr,zob)/sfcr)/log(ten/sfcr)
                    else
                       sfcr = data(isfcr,i)
                       skint = data(iskint,i)
                       call rtma_comp_fact10(dlat,dlon,dtime,zob,skint,sfcr,isli,mype,factw)
                    endif
                 else
                    term = max(zob,zero)/ten
                    factw = term*factw
                 endif
              end if
           else
              term = (zges(1)-zob)/(zges(1)-ten)
              factw = one-term+factw*term
           end if

           ugesin=factw*ugesin
           vgesin=factw*vgesin

           if (save_jacobian) then
              dhx_dx_u%val = factw * dhx_dx_u%val
              dhx_dx_v%val = factw * dhx_dx_v%val
           endif
        endif

        if(sfc_data .or. dpres < one) then
           drpx=0.005_r_kind*abs(dstn-zsges)*(one-fact)
        end if
 
!       Compute observation pressure (only used for diagnostics)
        
        if (twodvar_regional) then
           presw = ten*exp(data(ipres,i))
        else
!          Set indices of model levels below (k1) and above (k2) observation.
           if (dpres<one) then
              z1=zero;    p1=log(psges)
              z2=zges(1); p2=prsltmp(1)
           elseif (dpres>nsig) then
              z1=zges(nsig-1); p1=prsltmp(nsig-1)
              z2=zges(nsig);   p2=prsltmp(nsig)
              drpx = 1.e6_r_kind
           else
              k=dpres
              k1=min(max(1,k),nsig)
              k2=max(1,min(k+1,nsig))
              z1=zges(k1); p1=prsltmp(k1)
              z2=zges(k2); p2=prsltmp(k2)
           endif
        
           dz21     = z2-z1
           dlnp21   = p2-p1
           dz       = zob-z1
           pobl     = p1 + (dlnp21/dz21)*dz
           presw    = ten*exp(pobl)
        endif
!          Determine location in terms of grid units for midpoint of
!          first layer above surface
        sfcchk=zero
!       call grdcrd1(sfcchk,zges,nsig,1)

!    Process observations with reported pressure
     else
        dpres = data(ipres,i)
        presw = ten*exp(dpres)
        dpres = dpres-log(psges)
        drpx=zero

        prsfc=psges
        prsln2=log(exp(prsltmp(1))/prsfc)
        dpressave=dpres

!       Put obs pressure in correct units to get grid coord. number
        dpres=log(exp(dpres)*prsfc)
        call grdcrd1(dpres,prsltmp(1),nsig,-1)
 
!       Interpolate guess u and v to observation location and time.
 
        call tintrp31(ges_u,ugesin,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        call tintrp31(ges_v,vgesin,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)

        iz = max(1, min( int(dpres), nsig))
        delz = max(zero, min(dpres - real(iz,r_kind), one))

        if (save_jacobian) then
           u_ind = getindex(svars3d, 'u')
           if (u_ind < 0) then
              print *, 'Error: no variable u in state vector. Exiting.'
              call stop2(1300)
           endif
           v_ind = getindex(svars3d, 'v')
           if (v_ind < 0) then
              print *, 'Error: no variable v in state vector. Exiting.'
              call stop2(1300)
           endif

           dhx_dx_u%st_ind(1)  = iz               + sum(levels(1:u_ind-1))
           dhx_dx_u%end_ind(1) = min(iz + 1,nsig) + sum(levels(1:u_ind-1))
           dhx_dx_v%st_ind(1)  = iz               + sum(levels(1:v_ind-1))
           dhx_dx_v%end_ind(1) = min(iz + 1,nsig) + sum(levels(1:v_ind-1))

           dhx_dx_u%val(1) = one - delz
           dhx_dx_u%val(2) = delz
           dhx_dx_v%val = dhx_dx_u%val
        endif

        if(dpressave <= prsln2)then
           factw=one
        else
           factw = data(iff10,i)
           if(sfcmod_gfs .or. sfcmod_mm5) then
              sfcr = data(isfcr,i)
              skint = data(iskint,i)
              call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
           end if
 
           call tintrp2a1(ges_tv,tges,dlat,dlon,dtime,hrdifsig,&
              nsig,mype,nfldsig)
!          Apply 10-meter wind reduction factor to guess winds
           dx10=-goverrd*ten/tges(1)
           if (dpressave < dx10)then
              term=(prsln2-dpressave)/(prsln2-dx10)
              factw=one-term+factw*term
           end if
           ugesin=factw*ugesin   
           vgesin=factw*vgesin

           if (save_jacobian) then
              dhx_dx_u%val = factw * dhx_dx_u%val
              dhx_dx_v%val = factw * dhx_dx_v%val 
           endif
        end if
       
!       Get approx k value of sfc by using surface pressure
        sfcchk=log(psges)
        call grdcrd1(sfcchk,prsltmp(1),nsig,-1)
 
     endif


!    Checks based on observation location relative to model surface and top
     rlow=max(sfcchk-dpres,zero)
     rhgh=max(dpres-r0_001-rsigp,zero)
     if(luse(i))then
        awork(1) = awork(1) + one
        if(rlow/=zero) awork(2) = awork(2) + one
        if(rhgh/=zero) awork(3) = awork(3) + one
     end if
     ratio_errors=error/(data(ier,i)+drpx+1.0e6_r_kind*rhgh+four*rlow)

!    Compute innovations
     lowlevelsat=itype==242.or.itype==243.or.itype==245.or.itype==246.or. &
                 itype==247.or.itype==250.or.itype==251.or.itype==252.or. &
                 itype==253.or.itype==254.or.itype==257.or.itype==258.or. &
                 itype==259.or.itype==241
     if (lowlevelsat .and. twodvar_regional) then
         call windfactor(presw,factw)
         data(iuob,i)=factw*data(iuob,i)
         data(ivob,i)=factw*data(ivob,i)
         uob = data(iuob,i)
         vob = data(ivob,i)
     endif
     dudiff=uob-ugesin
     dvdiff=vob-vgesin
     spdb=sqrt(uob**2+vob**2)-sqrt(ugesin**2+vgesin**2)
    
!    Setup dynamic ob error specification for aircraft recon in hurricanes 
     if (aircraft_recon) then
       if (itype==236) then
         magomb=sqrt(dudiff*dudiff+dvdiff*dvdiff)
         ratio_errors=error/((uv_doe_a_236*magomb+uv_doe_b_236)+drpx+1.0e6_r_kind*rhgh+four*rlow)
       else if (itype==237) then
         magomb=sqrt(dudiff*dudiff+dvdiff*dvdiff)
         ratio_errors=error/((uv_doe_a_237*magomb+uv_doe_b_237)+drpx+1.0e6_r_kind*rhgh+four*rlow)
       endif
     endif

!    Invert observation error
     error=one/error

!    Check to see if observation below model surface or above model top.
!    If so, don't use observation
     if (dpres > rsig )then
        if( regional .and. presw > pt_ll )then
           dpres=rsig
        else
           ratio_errors=zero
        endif
     endif

     if ( (itype>=221 .and. itype<=229).and. (dpres<zero) ) ratio_errors=zero


! QC PBL profiler  227 and 223, 224
     if(itype==227 .or. itype==223 .or. itype==224 .or. itype==228 .or. itype==229) then
        if(abs(uob) < 1.0_r_kind .and. abs(vob) <1.0_r_kind )  then
           muse(i)=.false.
           error=zero
        endif
     endif

!    VADWND and aircraft winds quality control
     if( itype ==224 .and. presw < 226.0_r_kind) then
        error=zero
     endif
     if(itype >=230 .and. itype <=239 .and.  presw <126.0_r_kind ) then
        error=zero
     endif

!    Quality control for satellite winds

     if ( qc_satwnds ) then
        if (itype >=240 .and. itype <=260) then
           call intrp2a11(tropprs,trop5,dlat,dlon,mype)
           if(presw < trop5-r50) error=zero            ! tropopose check for all satellite winds 
           if(i_gsdqc==2) then
              prsfc = r10*psges
              if( prsfc-presw < 100.0_r_kind) error =zero ! add check for obs within 100 hPa of sfc
           else
              if( presw >950.0_r_kind) error =zero       ! screen data beloww 950mb
           endif
           if(itype == 241 ) then
              if( presw >399.0_r_kind .and. presw <601.0_r_kind) then  !CIMISS(enhanced AMV) winds
                 error=zero                          !  no data between400-600mb
              endif
           else if(itype ==242 .or. itype ==243 ) then  !  visible winds from JMA and EUMETSAT
              if(presw <700.0_r_kind) error=zero    !  no visible winds above 700mb
           else if(itype ==245 ) then
              if( presw >399.0_r_kind .and. presw <801.0_r_kind) then  !GOES IR  winds
                 error=zero                          !  no data between 400-800mb
              endif
           else if(itype == 252 )then
              if( presw >499.0_r_kind .and. presw <801.0_r_kind) then  ! JMA IR winds
                 error=zero
              end if
           else if(itype == 253 )  then
              if(presw >401.0_r_kind .and. presw <801.0_r_kind) then  ! EUMET IR winds
                 error=zero
              endif
           else if( itype == 246 .or. itype == 250 .or. itype == 254 )   then     ! water vapor cloud top
              if(presw >399.0_r_kind) error=zero

!       QC GOES CAWV - some checks above as well
           else if (itype==247) then
              prsfc = r10*psges       ! surface pressure in hPa

!             Compute observed and guess wind speeds (m/s).  
              spdges = sqrt(ugesin* ugesin +vgesin* vgesin )

!             Set and compute GOES CAWV specific departure parameters
              LNVD_wspd = spdob
              LNVD_omb = sqrt(dudiff*dudiff + dvdiff*dvdiff)
              LNVD_ratio = LNVD_omb / log(LNVD_wspd)
              LNVD_threshold = 3.0_r_kind
              if( .not. wrf_nmm_regional) then   ! LNVD check not use for CAWV winds in HWRF
                 if(LNVD_ratio >= LNVD_threshold .or. &      ! LNVD check
                    (presw > prsfc-110.0_r_kind .and. isli /= 0))then ! near surface check 110 ~1km
                    error = zero
                 endif
              endif
!       check for direction departure gt 50 deg 
              wdirdiffmax=50._r_kind
              call getwdir(uob,vob,wdirob)
              call getwdir(ugesin,vgesin,wdirgesin)
              if ( min(abs(wdirob-wdirgesin),abs(wdirob-wdirgesin+r360), &
                       abs(wdirob-wdirgesin-r360)) > wdirdiffmax ) then
                 error = zero
              endif
   
!    QC MODIS winds
           else if (itype==257 .or. itype==258 .or. itype==259 .or. itype ==260) then
              if(itype ==257 .and. presw <249.0_r_kind) error=zero
              if(itype ==258 .and. presw >600.0_r_kind) error=zero
              if(itype ==259 .and. presw >600.0_r_kind) error=zero
              if(itype ==259 .and. presw <249.0_r_kind) error=zero
!             Get guess values of tropopause pressure and sea/land/ice
!             mask at observation location
              prsfc = r10*prsfc       ! surface pressure in hPa

!             Compute observed and guess wind speeds (m/s).  
              spdges = sqrt(ugesin* ugesin +vgesin* vgesin )
 
!             Set and computes modis specific qc parameters
              LNVD_wspd = spdob
              LNVD_omb = sqrt(dudiff*dudiff + dvdiff*dvdiff)
              LNVD_ratio = LNVD_omb / log(LNVD_wspd)
              LNVD_threshold = 3.0_r_kind
              if(LNVD_ratio >= LNVD_threshold .or. &      ! LNVD check
                  (presw > prsfc-r200 .and. isli /= 0))then ! near surface check
                 error = zero
              endif

!          QC AVHRR winds
           else if (itype==244) then
!             Get guess values of tropopause pressure and sea/land/ice
!             mask at observation location
              prsfc = r10*prsfc       ! surface pressure in hPa

!             Set and computes modis specific qc parameters
              LNVD_wspd = spdob
              LNVD_omb = sqrt(dudiff*dudiff + dvdiff*dvdiff)
              LNVD_ratio = LNVD_omb / log(LNVD_wspd)
              LNVD_threshold = 3.0_r_kind

              if(LNVD_ratio >= LNVD_threshold .or. &      ! LNVD check
                  (presw > prsfc-r200 .and. isli /= 0))then ! near surface check
                 error = zero
              endif
           endif                                                  ! end if all satellite winds
        endif
     endif
     

!    QC WindSAT winds
     if (itype==289) then
        qcu = r6
        qcv = r6
        if ( spdob > r20 .or. &          ! high wind speed check
             abs(dudiff) > qcu  .or. &   ! u component check
             abs(dvdiff) > qcv ) then    ! v component check
           error = zero
        endif

!    QC ASCAT winds
     else if (itype==290) then
        qcu = five
        qcv = five
!       Compute innovations for opposite vectors
        dudiff_opp = -uob - ugesin
        dvdiff_opp = -vob - vgesin
        vecdiff = sqrt(dudiff**2 + dvdiff**2)
        vecdiff_opp = sqrt(dudiff_opp**2 + dvdiff_opp**2)
!       ascat_vec = sqrt((dudiff**2 + dvdiff**2)/spdob**2)

        if ( abs(dudiff) > qcu  .or. &       ! u component check
             abs(dvdiff) > qcv  .or. &       ! v component check
             vecdiff > vecdiff_opp ) then    ! ambiguity check
 
           error = zero
        endif

!    QC RAPIDSCAT winds
     else if (itype==296) then
        qcu = five
        qcv = five
!       Compute innovations for opposite vectors
        dudiff_opp_rs = -uob - ugesin
        dvdiff_opp_rs = -vob - vgesin
        vecdiff_rs = sqrt(dudiff**2 + dvdiff**2)
        vecdiff_opp_rs = sqrt(dudiff_opp_rs**2 + dvdiff_opp_rs**2)
        rapidscat_vec = sqrt((dudiff**2 + dvdiff**2)/spdob**2)
        if ( abs(dudiff) > qcu  .or. &       ! u component check
             abs(dvdiff) > qcv  .or. &       ! v component check
             vecdiff_rs > vecdiff_opp_rs ) then    ! ambiguity check
           error = zero
        endif

!    QC OSCAT winds     
     else if (itype==291) then
        qcu = r6
        qcv = r6
        oscat_vec = sqrt((dudiff**2 + dvdiff**2)/spdob**2)

!        if ( spdob > r20 .or. &          ! high wind speed check
!             abs(dudiff) > qcu  .or. &   ! u component check
!             oscat_vec > r0_1 .or. &
!             abs(dvdiff) > qcv ) then    ! v component check
!           error = zero
!        else
!           write(6,2000) "999291291", data(ilate,i), &
!                      data(ilone,i), uob, vob, ugesin, vgesin, &
!                      jiter 
!        endif

        if (spdob > r20 .or. &
            abs(dudiff) > qcu .or. &
            oscat_vec > r0_1 .or. &
            abs(dvdiff) > qcv) then                                               
           error = zero
        endif
     endif


2000 format(a9,1x,2(f8.2,1x),4(f8.2,1x),3x,i3)
2001 format(a6,1x,2(f8.2,1x),4(f8.2,1x),3x,i3,3x,f8.2)

!    If requested, setup for single obs test.
     if (oneobtest) then
        if (oneob_type=='u') then
           dudiff=maginnov
           dvdiff=zero
        elseif (oneob_type=='v') then
           dudiff=zero
           dvdiff=maginnov
        endif
        error=one/magoberr
        ratio_errors=one
     end if
 
!    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = sqrt(dudiff**2+dvdiff**2)
     ratio    = residual/obserrlm
!!   modify cgross depending on the quality mark, qcmark=3, cgross=0.7*cgross
!!   apply asymetric gross check for satellite winds
     qcgross=cgross(ikx)
     if(data(iqc,i) == three  ) then
        qcgross=r0_7*cgross(ikx)
     endif

     if(spdb <0 )then
        if(itype ==244) then   ! AVHRR, use same as MODIS
          qcgross=r0_7*cgross(ikx)
        endif
        if( itype == 245 .or. itype ==246 .or. itype ==241) then
           if(presw <400.0_r_kind .and. presw >300.0_r_kind ) qcgross=r0_7*cgross(ikx)
        endif
        if(itype == 253 .or. itype ==254) then
           if( presw <400.0_r_kind .and. presw >200.0_r_kind) qcgross=r0_7*cgross(ikx)
        endif
        if(itype >=257 .and. itype <=259 ) then
          qcgross=r0_7*cgross(ikx)
        endif
     endif

     if (ratio>qcgross .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(4) = awork(4)+one
        error = zero
        ratio_errors = zero
     else
        ratio_errors = ratio_errors/sqrt(dup(i))
     end if

     if (lowlevelsat .and. twodvar_regional) then
        if (data(idomsfc,i) /= 0 .and. data(idomsfc,i) /= 3 ) then
           error = zero
           ratio_errors = zero
        endif
     endif

     if (twodvar_regional) then
        if (lowlevelsat .or. itype==289 .or. itype==290) then
            wdirdiffmax=45._r_kind
          else
           wdirdiffmax=100000._r_kind
        endif
        if (spdob > zero .and. (spdob-spdb) > zero) then
           call getwdir(uob,vob,wdirob)
           call getwdir(ugesin,vgesin,wdirgesin)
           if ( min(abs(wdirob-wdirgesin),abs(wdirob-wdirgesin+r360), &
                          abs(wdirob-wdirgesin-r360)) > wdirdiffmax ) then
               error = zero
               ratio_errors = zero
           endif
        endif
        if (itype==288 .or. itype==295) then !Tyndall & Horel QC for mesonet winds /(WAF 2013, Vol. 28, pg. 285)
           if (spdob < one .and. (spdob-spdb) > five) then
               error = zero
               ratio_errors = zero
           endif
        endif
     endif

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
     if ( (itype==261) .and. (ratio_errors*error <= 1.0E-100_r_kind) ) muse(i)=.false.

        ! As a 2-component observation, muse(i) is not the same as muse_u or muse_v in an obs_diag.
     if (nobskeep>0 .and. luse_obsdiag) then
       call obsdiagNode_get(my_diagu, jiter=nobskeep, muse=muse_u)
       call obsdiagNode_get(my_diagv, jiter=nobskeep, muse=muse_v)
       muse(i) = muse_u.and.muse_v
     endif

!    Oberror Tuning and Perturb Obs
     if(muse(i)) then
        if(oberror_tune )then
           if( jiter > jiterstart ) then
              dudiff=dudiff+data(iptrbu,i)/error/ratio_errors
              dvdiff=dvdiff+data(iptrbv,i)/error/ratio_errors
           endif
        else if(perturb_obs )then
           dudiff=dudiff+data(iptrbu,i)/error/ratio_errors
           dvdiff=dvdiff+data(iptrbv,i)/error/ratio_errors
        endif
     endif
 
     valu     = error*dudiff
     valv     = error*dvdiff
     if(nvqc .and. ibeta(ikx) >0  ) ratio_errors=0.8_r_kind*ratio_errors
     ratio_errors=ratio_errors*sqrt(data(ihil,i))       ! hilbert weight
!    Compute penalty terms (linear & nonlinear qc).
     if(luse(i))then
        if(vqc) then
           cg_t=cvar_b(ikx)
           cvar=cvar_pg(ikx)
        else
           cg_t=zero
           cvar=zero
        endif
        if(nvqc) then
           ibb=ibeta(ikx)
           ikk=ikapa(ikx)
        else
           ibb=0
           ikk=0
        endif
        vals=valu
        call vqc_setup(vals,ratio_errors,error,cvar,&
                      cg_t,ibb,ikk,var_jb,rat_err2u,wgt,valqcu)
        rwgt = 0.5_r_kind*wgt/wgtlim
        vals=valv
        call vqc_setup(vals,ratio_errors,error,cvar,&
                      cg_t,ibb,ikk,var_jb,rat_err2v,wgt,valqcv)
        rwgt = rwgt+0.5_r_kind*wgt/wgtlim
        valqc=half*(valqcu+valqcv)

!       Accumulate statistics for obs belonging to this task
        if (muse(i)) then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(4*nsig+jsig+100)=awork(4*nsig+jsig+100)+valu*valu*rat_err2u
           awork(5*nsig+jsig+100)=awork(5*nsig+jsig+100)+valv*valv*rat_err2v
           awork(6*nsig+jsig+100)=awork(6*nsig+jsig+100)+one
           awork(3*nsig+jsig+100)=awork(3*nsig+jsig+100)+valqc
        end if

!       Loop over pressure level groupings and obs to accumulate statistics
!       as a function of observation type.
        ress  = scale*sqrt(dudiff**2+dvdiff**2)
        ressw = ress*ress
        val2    = half*(valu*valu*rat_err2u+valv*valv*rat_err2v)
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(error*ratio_errors >= tiny_r_kind)nn=3
!          if((data(iqc,i) >= 8 .and. data(iqc,i) <= 10) .or.  &
!              error*ratio_errors >= tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(presw >ptop(k) .and. presw<=pbot(k))then
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+spdb           ! speed bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2           ! penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
 
           end if
        end do
     end if


!    Fill obs to diagnostics structure
     if (luse_obsdiag) then
     ! U
        call obsdiagNode_set(my_diagu, wgtjo=(error*ratio_errors)**2 ,&
                jiter=jiter, muse=muse(i), nldepart=dudiff   )
     ! V
        call obsdiagNode_set(my_diagv, wgtjo=(error*ratio_errors)**2 ,&
                jiter=jiter, muse=muse(i), nldepart=dvdiff   )
     endif

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
 
     if (.not. last .and. muse(i)) then

        allocate(my_head)
        call wNode_appendto(my_head,whead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%ich0= wNode_ich0        ! Flagged as a normal obs.
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

        my_head%dlev = dpres
        my_head%factw= factw
        call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij)

        do j=1,8
           my_head%wij(j)=factw*my_head%wij(j)
        end do

        my_head%ures=dudiff
        my_head%vres=dvdiff
        my_head%err2=error**2
        my_head%raterr2=ratio_errors **2  
        my_head%time = dtime
        my_head%b=cvar_b(ikx)
        my_head%pg=cvar_pg(ikx)
        my_head%jb=var_jb
        my_head%ib=ibeta(ikx)
        my_head%ik=ikapa(ikx)
        my_head%luse=luse(i)

!        if( i==3) print *,'SETUPW',my_head%ures,my_head%vres,my_head%err2
         

        if(oberror_tune) then
           my_head%upertb=data(iptrbu,i)/error/ratio_errors
           my_head%vpertb=data(iptrbv,i)/error/ratio_errors
           my_head%kx=ikx
           if (njqc) then
              ptabluv=ptabl_uv
           else
              ptabluv=ptabl
           endif
           if(presw > ptabluv(2))then
              my_head%k1=1
           else if( presw <= ptabluv(33)) then
              my_head%k1=33
           else
              k_loop: do k=2,32
                 if(presw > ptabluv(k+1) .and. presw <= ptabluv(k)) then
                    my_head%k1=k
                    exit k_loop
                 endif
              enddo k_loop
           endif
        endif


        if (luse_obsdiag) then
           call obsdiagNode_assert(my_diagu, my_head%idv,my_head%iob,my_head%ich0+1_i_kind,myname,"my_diagu:my_head")
           call obsdiagNode_assert(my_diagv, my_head%idv,my_head%iob,my_head%ich0+2_i_kind,myname,"my_diagv:my_head")

           my_head%diagu => my_diagu
           my_head%diagv => my_diagv
        endif ! (luse_obsdiag)
 
        my_head => null()
     end if

!    Save select output for diagnostic file
     if (conv_diagsave .and. luse(i)) then
        ii=ii+1
        rstation_id     = data(id,i)
        err_input = data(ier2,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif
 
        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final
 
        if (binary_diag) call contents_binary_diag_(my_diagu,my_diagv)
        if (netcdf_diag) call contents_netcdf_diag_(my_diagu,my_diagv)

     endif

!!!!!!!!!!!!!!  PBL pseudo surface obs  !!!!!!!!!!!!!!!!
     if( .not. last .and. l_pbl_pseudo_itype .and.        &
           muse(i) .and. dpres > -1.0_r_kind ) then
        prest=presw     ! in mb
        prestsfc=prest
        dudiffsfc=dudiff
        dvdiffsfc=dvdiff
        call tintrp2a11(pbl_height,thisPBL_height,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)
        ratio_PBL_height = (prest - thisPBL_height) * pblH_ration
        if(ratio_PBL_height > zero) thisPBL_height = prest - ratio_PBL_height
        prest = prest - pps_press_incr
        DO while (prest > thisPBL_height)
           ratio_PBL_height=1.0_r_kind-(prestsfc-prest)/(prestsfc-thisPBL_height)

           allocate(my_head)
           call wNode_appendto(my_head,whead(ibin))

           my_head%idv = is             ! information needed for re-distribution
           my_head%iob = ioid(i)
           my_head%ich0= wNode_ich0_PBL_pseudo  ! Flagged as a PBL pseudo obs for %diag
           my_head%elat= data(ilate,i)
           my_head%elon= data(ilone,i)

!!! find uob and vob 
           uob = data(iuob,i)
           vob = data(ivob,i)


!    Put obs pressure in correct units to get grid coord. number
           dpres=log(prest/r10)
           call grdcrd1(dpres,prsltmp(1),nsig,-1)

!    Interpolate guess u and v to observation location and time.

           call tintrp31(ges_u,ugesin,dlat,dlon,dpres,dtime, &
              hrdifsig,mype,nfldsig)
           call tintrp31(ges_v,vgesin,dlat,dlon,dpres,dtime, &
              hrdifsig,mype,nfldsig)

!!! Set (i,j,k) indices of guess gridpoint that bound obs location
           my_head%dlev = dpres
           my_head%factw= 1._r_kind
           call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij)
!!! find ddiff

           dudiff = dudiffsfc*(0.5_r_kind + 0.5_r_kind*ratio_PBL_height)
           dvdiff = dvdiffsfc*(0.5_r_kind + 0.5_r_kind*ratio_PBL_height)

           error=one/data(ier2,i)

           my_head%ures=dudiff
           my_head%vres=dvdiff
           my_head%err2=error**2
           my_head%raterr2=ratio_errors **2
           my_head%time = dtime
           my_head%b=cvar_b(ikx)
           my_head%pg=cvar_pg(ikx)
           my_head%jb=var_jb
           my_head%ib=ibeta(ikx)
           my_head%ik=ikapa(ikx)
           my_head%luse=luse(i)

           if (luse_obsdiag) then
              call obsdiagNode_assert(my_diagu_pbl, my_head%idv,my_head%iob,my_head%ich0+1_i_kind,myname,"my_diagu_pbl:my_head")
              call obsdiagNode_assert(my_diagv_pbl, my_head%idv,my_head%iob,my_head%ich0+2_i_kind,myname,"my_diagv_pbl:my_head")

              !U_pbl_pseudo
              call obsdiagNode_set(my_diagu_pbl, wgtjo=(error*ratio_errors)**2 ,&
                      jiter=jiter, muse=muse(i), nldepart=dudiff )
              !V_pbl_pseudo
              call obsdiagNode_set(my_diagv_pbl, wgtjo=(error*ratio_errors)**2 ,&
                      jiter=jiter, muse=muse(i), nldepart=dvdiff )

              my_head%diagu => my_diagu_pbl
              my_head%diagv => my_diagv_pbl
           endif

           prest = prest - pps_press_incr

           my_head => null()
        ENDDO

     endif  ! 281,283,287
!!!!!!!!!!!!!!!!!!  PBL pseudo surface obs  !!!!!!!!!!!!!!!!!!!!!!!

  end do loop_for_all_obs
! End of loop over observations
  if(num_bad_ikx > 0) write(6,*)' in setupw, num_bad_ikx ( ikx<1 or ikx>nconvtype ) = ',num_bad_ikx

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave)then
    if(netcdf_diag) call nc_diag_write
    if(binary_diag .and. ii>0)then
       write(7)' uv',nchar,nreal,ii,mype,ioff0
       write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
  
       if (twodvar_regional .or. l_obsprvdiag) then
          write(7)cprvstg(1:ii),csprvstg(1:ii)
          deallocate(cprvstg,csprvstg)
       endif
    end if
    deallocate(cdiagbuf,rdiagbuf)
  end if


! End of routine

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::u' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::v' , ivar, istatus )
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
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine init_netcdf_diag_
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false.

     write(string,900) jiter
900  format('conv_uv_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists.  Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        if (save_jacobian) then
          call nc_diag_header("jac_nnz", nnz)
          call nc_diag_header("jac_nind", nind)
        endif
     endif
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_(udiag,vdiag)
     type(obs_diag),pointer,intent(in):: udiag,vdiag
        cdiagbuf(ii)    = station_id         ! station id
 
        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype

        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters) 
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = data(ihil,i)       ! hilbert curve weight 
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (m/s)**-1
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (m/s)**-1
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (m/s)**-1

        rdiagbuf(17,ii) = data(iuob,i)       ! u wind component observation (m/s)
        rdiagbuf(18,ii) = dudiff             ! u obs-ges used in analysis (m/s)
        rdiagbuf(19,ii) = uob-ugesin         ! u obs-ges w/o bias correction (m/s) (future slot)

        rdiagbuf(20,ii) = data(ivob,i)       ! v wind component observation (m/s)
        rdiagbuf(21,ii) = dvdiff             ! v obs-ges used in analysis (m/s)
        rdiagbuf(22,ii) = vob-vgesin         ! v obs-ges w/o bias correction (m/s) (future slot)

        if(regional .and. .not. fv3_regional) then

!           replace positions 17-22 with earth relative wind component information
           uob_reg=data(iuob,i)
           vob_reg=data(ivob,i)
           dlon_e=data(ilone,i)*deg2rad
           call rotate_wind_xy2ll(uob_reg,vob_reg,uob_e,vob_e,dlon_e,dlon,dlat)
           call rotate_wind_xy2ll(ugesin,vgesin,uges_e,vges_e,dlon_e,dlon,dlat)
           call rotate_wind_xy2ll(dudiff,dvdiff,dudiff_e,dvdiff_e,dlon_e,dlon,dlat)
           rdiagbuf(17,ii) = uob_e         ! earth relative u wind component observation (m/s)
           rdiagbuf(18,ii) = dudiff_e      ! earth relative u obs-ges used in analysis (m/s)
           rdiagbuf(19,ii) = uob_e-uges_e  ! earth relative u obs-ges w/o bias correction (m/s) (future slot)

           rdiagbuf(20,ii) = vob_e         ! earth relative v wind component observation (m/s)
           rdiagbuf(21,ii) = dvdiff_e      ! earth relative v obs-ges used in analysis (m/s)
           rdiagbuf(22,ii) = vob_e-vges_e  ! earth relative v obs-ges w/o bias correction (m/s) (future slot)
        end if

        rdiagbuf(23,ii) = factw            ! 10m wind reduction factor
        rdiagbuf(24,ii) = 1.e+10_r_single  ! u spread (filled in by EnKF)
        rdiagbuf(25,ii) = 1.e+10_r_single  ! v spread (filled in by EnKF)

        ioff=ioff0
        if (lobsdiagsave) then
              !?? In this implmentation, only udiag%muse is used.  Is this by design
              !?? or an unexpected bug?
           do jj=1,miter
              ioff=ioff+1
              if (udiag%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
              !?? Both u-diag and v-diag are implemented for binary diag output,
              !?? but not so for netcdf diag output!  See below in subroutine
              !?? contents_netcdf_diag_()
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = udiag%nldepart(jj)
              ioff=ioff+1
              rdiagbuf(ioff,ii) = vdiag%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = udiag%tldepart(jj)
              ioff=ioff+1
              rdiagbuf(ioff,ii) = vdiag%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = udiag%obssen(jj)
              ioff=ioff+1
              rdiagbuf(ioff,ii) = vdiag%obssen(jj)
           enddo
        endif

        if (twodvar_regional .or. l_obsprvdiag) then
           ioff = ioff + 1
           rdiagbuf(ioff,ii) = data(idomsfc,i) ! dominate surface type
           ioff = ioff + 1
           rdiagbuf(ioff,ii) = data(izz,i)     ! model terrain at ob location
           r_prvstg            = data(iprvd,i)
           cprvstg(ii)         = c_prvstg        ! provider name
           r_sprvstg           = data(isprvd,i)
           csprvstg(ii)        = c_sprvstg       ! subprovider name
        endif

        if (save_jacobian) then
           call writearray(dhx_dx_u, rdiagbuf(ioff+1:nreal,ii))
           ioff = ioff + size(dhx_dx_u)
           call writearray(dhx_dx_v, rdiagbuf(ioff+1:nreal,ii))
           ioff = ioff + size(dhx_dx_v)
        endif

  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_(udiag,vdiag)
  type(obs_diag),pointer,intent(in):: udiag,vdiag
! Observation class
  character(7),parameter     :: obsclass = '     uv'
  real(r_kind),dimension(miter) :: obsdiag_iuse

           call nc_diag_metadata("Station_ID",              station_id             )
           call nc_diag_metadata("Observation_Class",       obsclass               )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)         )
           call nc_diag_metadata_to_single("Latitude",data(ilate,i)     )
           call nc_diag_metadata_to_single("Longitude",data(ilone,i)     )
           call nc_diag_metadata_to_single("Station_Elevation",data(ielev,i)     )
           call nc_diag_metadata_to_single("Pressure",presw             )
           call nc_diag_metadata_to_single("Height",data(ihgt,i)      )
           call nc_diag_metadata_to_single("Time",dtime,time_offset,'-')
           call nc_diag_metadata_to_single("Prep_QC_Mark",data(iqc,i)       )
!           call nc_diag_metadata("Setup_QC_Mark",           rmiss_single           )
           call nc_diag_metadata_to_single("Setup_QC_Mark",bmiss             )
           call nc_diag_metadata_to_single("Nonlinear_QC_Var_Jb",var_jb            )
           call nc_diag_metadata_to_single("Prep_Use_Flag",data(iuse,i)      )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    sngl(one)              )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    sngl(-one)             )
           endif

           call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",rwgt          )
           call nc_diag_metadata_to_single("Errinv_Input",errinv_input          )
           call nc_diag_metadata_to_single("Errinv_Adjust",errinv_adjst         )
           call nc_diag_metadata_to_single("Errinv_Final",errinv_final          )
           call nc_diag_metadata_to_single("Wind_Reduction_Factor_at_10m",factw )

           if (.not. regional .or. fv3_regional) then
              call nc_diag_metadata_to_single("u_Observation",data(iuob,i)     )
              call nc_diag_metadata_to_single("u_Obs_Minus_Forecast_adjusted",dudiff           )
              call nc_diag_metadata_to_single("u_Obs_Minus_Forecast_unadjusted",uob,ugesin,'-')

              call nc_diag_metadata_to_single("v_Observation",data(ivob,i)     )
              call nc_diag_metadata_to_single("v_Obs_Minus_Forecast_adjusted",dvdiff           )
              call nc_diag_metadata_to_single("v_Obs_Minus_Forecast_unadjusted",vob,vgesin,'-')
           else ! (if regional)
!              replace positions 17-22 with earth relative wind component information
   
              uob_reg=data(iuob,i)
              vob_reg=data(ivob,i)
              dlon_e=data(ilone,i)*deg2rad
              call rotate_wind_xy2ll(uob_reg,vob_reg,uob_e,vob_e,dlon_e,dlon,dlat)
              call rotate_wind_xy2ll(ugesin,vgesin,uges_e,vges_e,dlon_e,dlon,dlat)
              call rotate_wind_xy2ll(dudiff,dvdiff,dudiff_e,dvdiff_e,dlon_e,dlon,dlat)

              call nc_diag_metadata_to_single("u_Observation",uob_e            )
              call nc_diag_metadata_to_single("u_Obs_Minus_Forecast_adjusted",dudiff_e         )
              call nc_diag_metadata_to_single("u_Obs_Minus_Forecast_unadjusted",uob_e,uges_e,'-')

              call nc_diag_metadata_to_single("v_Observation",vob_e            )
              call nc_diag_metadata_to_single("v_Obs_Minus_Forecast_adjusted",dvdiff_e         )
              call nc_diag_metadata_to_single("v_Obs_Minus_Forecast_unadjusted",vob_e,vges_e,'-')
           endif

           if (lobsdiagsave) then
              !?? In current implmentation, only udiag is used.  Is this by design
              !?? or an unexpected bug?
              do jj=1,miter
                 if (udiag%muse(jj)) then
                    obsdiag_iuse(jj) =  one
                 else
                    obsdiag_iuse(jj) = -one
                 endif
              enddo

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse                             )
              call nc_diag_data2d("ObsDiagSave_nldepart", udiag%nldepart )
              !++ call nc_diag_data2d("ObsDiagSave_nldepart", vdiag%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", udiag%tldepart )
              !++ call nc_diag_data2d("ObsDiagSave_tldepart", vdiag%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen",   udiag%obssen   )
              !++ call nc_diag_data2d("ObsDiagSave_obssen",   vdiag%obssen   )
           endif

           if (twodvar_regional .or. l_obsprvdiag) then
              call nc_diag_metadata("Dominant_Sfc_Type", data(idomsfc,i)              )
              call nc_diag_metadata("Model_Terrain",     data(izz,i)                  )
              r_prvstg            = data(iprvd,i)
              call nc_diag_metadata("Provider_Name",     c_prvstg                     )
              r_sprvstg           = data(isprvd,i)
              call nc_diag_metadata("Subprovider_Name",  c_sprvstg                    )
           endif
           if (save_jacobian) then
              call nc_diag_data2d("u_Observation_Operator_Jacobian_stind", dhx_dx_u%st_ind)
              call nc_diag_data2d("u_Observation_Operator_Jacobian_endind", dhx_dx_u%end_ind)
              call nc_diag_data2d("u_Observation_Operator_Jacobian_val", real(dhx_dx_u%val,r_single))
              call nc_diag_data2d("v_Observation_Operator_Jacobian_stind", dhx_dx_v%st_ind)
              call nc_diag_data2d("v_Observation_Operator_Jacobian_endind", dhx_dx_v%end_ind)
              call nc_diag_data2d("v_Observation_Operator_Jacobian_val", real(dhx_dx_v%val,r_single))
           endif

  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_v )) deallocate(ges_v )
    if(allocated(ges_u )) deallocate(ges_u )
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setupw
end module w_setup
