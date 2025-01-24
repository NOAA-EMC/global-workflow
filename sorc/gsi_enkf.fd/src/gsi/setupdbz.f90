module dbz_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupdbz; end interface

contains
subroutine setupdbz(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,radardbz_diagsave,init_pass)
! modified from setupdbz, now dbz is also a state variable
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupdbz     compute rhs of oi for radar reflectivity (dBZ)
!   prgmmr: carley          org: np22                date: 2011-04-05
!
! abstract: For radar reflectivity observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2011-05-19  carley  - Cleaned up fields loaded into dbzptr.
!                         Removed linearization from inner loop routines
!                         and placed it here (see jqr and jqli).
!   2011-08-11  carley  - Turn on gross error checks.
!   2011-09-19  carley  - Include temporary fix from setuprw to prevent out of
!                         bounds array references associated with dpres<zero
!   2012-02-12  carley  - Update to include use of metguess bundle with qr and qli
!   2016-02-15  Johnson, Y. Wang, X. Wang - Develop the reflectivity operator for WRF ARW 
!                                           (Johnson et al. 2015 MWR; Wang and Wang 2017 MWR).
!                                           Two options were developed,
!                                           1) Explicitly apply the operator H(qr, qs, qg) to hydrometeors
!                                           2) Directly use the reflectivity from the wrfout
!                                           POC: xuguang.wang@ou.edu
!   2017-02-09  guo     - Removed m_alloc, n_alloc.
!                       . Removed my_node with corrected typecast().
!   2017-05-12 Y. Wang and X. Wang - Following Guo replacing ob_type with polymorphic obsNode through type casting,
!                                           POC: xuguang.wang@ou.edu
!   2015-09-xx  CAPS(G. Zhao)  - based on setupt, setupdw and setupq, to build up setupdbz (based on GSIv3.3)
!                              - referenced to setupt/dw/q and Carley's setupdbz code.
!                              - Inquire about the cloud guess fields  (merged inti vars_init_)       
!   2015-10-xx  CAPS(G. Zhao)  - obs operator is based on log(qr/s/g)
!   2016-11-07  CAPS(G. Zhao)  - using ges_tv, avoiding computing tv from tsen, and avoiding using ges_q (based on GSIv3.5)
!   2016-11-17  CAPS(G. Zhao)  - using ges_tsen, and ges_q, not using ges_tv
!                              - skipping geo-potential height to geometric height
!                                (since latitude data(ilate) is not correct from read_radarref_directDA.)
!                              - debugging diag_conv
!                              - put grid index into diag_conv, not lat/lon.
!                              - comment off Ze=Ze+1.0 if Ze < 1.0
!                              - convert specific humidity Q1D to mixign ratio Q1D
!                              - re-set qr/qs/qg to zero (if <=1.0e-8), but set
!                                qrexp/qsexp/qgexp to 1.0e-8 (if <=1.0e-8) to guaruantee
!                                non-zero Ze and Jacobian for TLM/ADM.
!   2017-02-20  CAPS(G. Zhao)  - using log(Qr/s/g) interpolation, not Qr/s/g interpolation
!   2019-02-19  CAPS(C. Tong)  - modified to comply with new type structure used for GSIv3.7      
!   2019-xx-xx  CAPS(T. Supinie)  -Add variables and modules for dbz operator using radarz libary
!   2019-12-xx  CAPS(C. Liu, L. Chen, and H. Li)
!                              - add thompson operator for dBZ (mphyopt=108)
!                              - add power transform option for hydrometeors
!
!                              - LIN operator
!                              - Forward model for reflectivity is only compatible with Lin Microphysics
!                                and takes the following form (similar to operator used in arps-3dvar
!                                developed by Drs. Jidong Gao and Chengsi Liu:
!
!                                dBZ =10*LOG_10( Zer + Zes + Zeh)
!                                Where:
!                                Zer is equivalent radar reflectivity factor from rain (mm^6 m^-3)
!                                Zes is equivalent radar reflectivity factor from precipiation snow (mm^6 m^-3)
!                                Zeg is equivalent radar reflectivity factor from precipiation hail/graupel (mm^6 m^-3)
!                                dBZ is simulated radar reflectivity in units of dBZ
!
!                                Plugging in the constants yields the following form:
!
!                                Zer  = Cr * (rho*qr)^1.75
!                                Zes  = Cs * (rho*qs)^1.75
!                                Zeg  = Cg * (rho*qg)^(1.75*0.95)
!
!                                where:
!                                Cr     = 3.6308 * 10^9 (rain drop)
!                                Csneg  = 9.5889e+08   (frozen snow)
!                                Cspos  = 4.2607e+11   (melting snow)
!                                Cg     = 6.1264e+10   (hail/graupel)
!
!                                Which yields the forward model:
!                                dBZ=10*log10(Zer+Zes+Zeg)
!   2021-08-31  CAPS(J. Park and C. Liu)
!                              - modified TM operator for EnKF DA (to provide air temperature)
!                              - added subroutines to reduce interpolation errors
!                                when cvpq is employed.
!                              - Updated Zes and Zeg calculations of TM operator
!                                for variational DA applications
!                              - modified jacobian terms to prevent possible
!                                coding errors
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert
  use obsmod, only: rmiss_single,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset,&
                    ens_hx_dbz_cut,static_gsi_nopcp_dbz
  use obsmod, only: oberror_tune
  use m_obsNode, only: obsNode
  use m_dbzNode, only: dbzNode
  use m_dbzNode, only: dbzNode_appendto
  use m_obsLList,only: obsLList
                     
  use hybrid_ensemble_parameters,only: l_hyb_ens
  use obsmod, only: luse_obsdiag, netcdf_diag, binary_diag, dirname, ianldate
  use obsmod, only: doradaroneob,oneobddiff,oneobvalue
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_metadata_to_single
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim,nc_diag_read_close
  use oneobmod, only: oneobtest
  use oneobmod, only: maginnov
  use oneobmod, only: magoberr
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use qcmod, only: npres_print,ptop,pbot 
  use guess_grids, only: hrdifsig,geop_hgtl,nfldsig,&
       ges_lnprsl,ges_rho,ges_tsen
  use gridmod, only: nsig,get_ijk
  use gsi_metguess_mod, only: gsi_metguess_bundle,gsi_metguess_get
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use constants, only: flattening,semi_major_axis,grav_ratio,zero,grav,wgtlim,&
       half,one,two,grav_equator,eccentricity,somigliana,rad2deg,deg2rad,&
       r60,tiny_r_kind,cg_term,huge_single
  use jfunc, only: jiter,last,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check
  use obsmod, only   : if_model_dbz, inflate_dbz_obserr
  use setupdbz_lib, only:hx_dart,jqr_dart,jqs_dart,jqg_dart 
  use gridmod, only: wrf_mass_regional,nems_nmmb_regional, fv3_regional
  use sparsearr, only: sparr2, new, size, writearray, fullarray
  use state_vectors, only: nsdim
  use constants, only: r1000
  use constants, only: rd
  use converr, only: ptabl
  use directDA_radaruse_mod, only: l_use_cvpqx, cvpqx_pval, l_gpht2gmht, lvldbg, l_set_oerr_ratio_dbz
  use directDA_radaruse_mod, only: l_cvpnr, cvpnr_pval
  use directDA_radaruse_mod, only: i_melt_snow, i_melt_graupel
  use directDA_radaruse_mod, only: l_use_tdep_radarz
  use directDA_radaruse_mod, only: Cr,     Pr,                     &
                                   Cs_dry, Ps_dry, Cs_wet, Ps_wet, &
                                   Cg_dry, Pg_dry, Cg_wet, Pg_wet
! modules added for dbz operator using radarz libary
  use radarz_cst, only: mphyopt
  use radarz_cst, only: nscalar
  use radarz_cst, only: P_qr, P_qs, P_qg, P_qh, P_nr
  use radarz_module, only: t_obs_dual, t_para_dsd, init_refl, init_para_dsd, calcMDR, &
                      qgh_opt, calcConstants, get_qgh_opt, set_dsd_para, rdr_obs, ta
  use directDA_radaruse_mod, only: l_use_dbz_directDA
 
  implicit none
! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index
  logical                                          ,intent(in   ) :: radardbz_diagsave
  logical                                          ,intent(in   ) :: init_pass ! state of "setup" parameters

! Declare local parameters
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r8     = 8.0_r_kind
  real(r_kind),parameter:: ten    = 10.0_r_kind

  real(r_kind),parameter:: D608=0.608_r_kind

  real(r_kind) denom
  real(r_kind) jqr_num,jqs_num,jqg_num
  real(r_kind) wgt_dry, wgt_wet
  real(r_kind) jqg_num_dry, jqg_num_wet
  real(r_kind) qrges,qsges,qgges
  real(r_kind) iqrges
  real(r_kind) Ze
  real(r_kind) rdBZr,rdBZs,rdBZg
  real(r_kind) Ze_orig, Zer, Zes, Zeg
  real(r_kind) Zeg_dry, Zeg_wet
  real(r_kind) qrexp, qsexp, qgexp

  real(r_kind), parameter :: pi = 3.141592_r_kind   ! pi
  real(r_kind), parameter :: rhor=1000._r_kind ! Density of rain (kg m**-3)
  real(r_kind), parameter :: rhoh=913._r_kind  ! Density of hail (kg m**-3)
  real(r_kind), parameter :: rhos=100._r_kind  ! Density of snow (kg m**-3)
  real(r_kind), parameter :: rhog=500._r_kind  ! Density of graupel (kg m**-3)
  real(r_kind), parameter :: am_s = 0.069_r_kind ! from WRF
  real(r_kind)            :: Zero1, Zeso, Zego, Zro, Zso, Zgo,Zo,Zeo

! Declare external calls for code analysis
  external:: tintrp2a1, tintrp2a11
  external:: tintrp3
  external:: grdcrd
  external:: stop2
! Declare local variables
  real(r_kind) rlow,rhgh,rsig
!  real(r_kind) dz,denom,jqr_num,jqli_num,jqr,jqli !modified
  real(r_kind) jqnr_num,jqnr
  real(r_kind) dz,jqr,jqs,jqg
  real(r_kind) dlnp,pobl,zob
  real(r_kind) sin2,termg,termr,termrg
  real(r_kind) psges,zsges
  real(r_kind),dimension(nsig):: zges,hges
  real(r_kind) prsltmp(nsig)
  real(r_kind) sfcchk 
  real(r_kind) residual,obserrlm,obserror,ratio,scale,val2
  real(r_kind) ress,ressw
  real(r_kind) val,valqc,rwgt
  real(r_kind) cg_w,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_double) rstation_id
  real(r_kind) dlat,dlon,dtime,dpres,ddiff,error,slat
 
  real(r_kind) ratio_errors
  real(r_kind) dbzgesin,qrgesin,qsgesin,qggesin,rhogesin,tempgesin,qligesin
  real(r_kind) qrgesin1,qsgesin1,qggesin1, qligesin1
  real(r_kind) rdBZ,presw,dbznoise
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  integer(i_kind) i,nchar,nreal,k,k1,ii
  integer(i_kind) mm1,jj,k2
  integer(i_kind) jsig,ikxx,nn,ibin,ioff, ioff0
  integer(i_kind) ier,ilat,ilon,ihgt,idbzob,ikx,itime,iuse
  integer(i_kind) ielev,id,itilt,iazm,ilone,ilate,irange
  integer(i_kind) ier2,idbznoise,idmiss2opt
  integer(i_kind) ipres,iqmax,iqc,icat,itemp
  integer(i_kind) istnelv,iobshgt,izz,iprvd,isprvd,iptrb
  integer(i_kind) idomsfc,iskint,isfcr,iff10

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(80):: string
  character(128):: diag_file
  logical :: diagexist
  integer(i_kind):: lu_diag

  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical :: proceed
  logical,dimension(nobs):: luse,muse
  equivalence(rstation_id,station_id)
  real(r_kind) wrange
  integer(i_kind) numequal,numnotequal
 
  logical:: debugging

  type(dbzNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  character(len=*),parameter:: myname='setupdbz'
  integer(i_kind) irefsmlobs, irejrefsmlobs

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z

  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qr
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_iqr
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qs
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qg
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qli
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_dbz

  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_nr

! variables added for dbz operator using radarz libary
  real(r_kind),dimension(nscalar) :: qscalar
  type(t_obs_dual) :: obs_dual
  type(t_para_dsd) :: var_dsd

  real(r_kind) :: presq
  real(r_kind) :: P1D,T1D,Q1D,rho
  real(r_kind) :: qges,tsenges       ! used to calculate tv - virtual temperature
  real(r_kind) :: lnprslges          ! use log(p) for vertical interpolation
  real(r_kind) :: qr_min, qs_min, qg_min
  real(r_kind) :: a_dry_snow_tm, b_dry_snow_tm ! dry snow coeffs for TM
  real(r_kind) :: smoz, alpha_const_tm_dry_snow
  real(r_kind) :: oams

!------------------------------------------------!
! variable used for invocation of multi-momemnt scheme by Tim Supinie
  logical :: firstcalled
  save firstcalled
  data firstcalled/.true./

!------------------------------------------------!
  integer(i_kind) :: icnt_nouse

  type(obsLList),pointer,dimension(:):: dbzhead
  dbzhead => obsLL(:)

!====================================================================================!
!

!******************************************************************************* 
! Flag is appiled since data arrays are different from direct reflectivity DA and others.
  if ( l_use_dbz_directDA ) then
    ! set coeffs for TM operator for variational DA before loop
    oams = one/am_s
    alpha_const_tm_dry_snow =  (0.176_r_kind/0.93_r_kind) * (6.0_r_kind/pi)*(6.0_r_kind/pi)     &
                              *(am_s/900.0_r_kind)*(am_s/900.0_r_kind)*1.e18_r_double

    ! Read and reformat observations in work arrays.
    read(lunin)data,luse,ioid

    icnt_nouse = 0

    if (lvldbg >= 100) &
        write(6,*)myname,'(pe=',mype,') nele nobs =',nele,nobs,     &
                 ' luse_obsdiag=',luse_obsdiag

    ! obs data array
    ier=1       ! index of obs error
    ilon=2      ! index of grid relative obs location (x)
    ilat=3      ! index of grid relative obs location (y)
    ipres=4     ! index of pressure
    idbzob=5    ! index of dbz observation
    id=6        ! index of station id
    itime=7     ! index of observation time in data array
    ikxx=8      ! index of ob type
    iqmax=9     ! index of max error
    itemp=10    ! index of dry temperature
    iqc=11      ! index of quality mark
    ier2=12     ! index of original-original obs error ratio
    iuse=13     ! index of use parameter
    idomsfc=14  ! index of dominant surface type
    iskint=15   ! index of surface skin temperature
    iff10=16    ! index of 10 meter wind factor
    isfcr=17    ! index of surface roughness
    ilone=18    ! index of longitude (degrees)
    ilate=19    ! index of latitude (degrees)
    istnelv=20  ! index of station elevation (m)
    iobshgt=21  ! index of observation height (m)
    izz=22      ! index of surface height
    iprvd=23    ! index of observation provider
    isprvd=24   ! index of observation subprovider
    icat =25    ! index of data level category
    iptrb=26    ! index of dbz perturbation

    do i=1,nobs
       muse(i)=nint(data(iuse,i)) <= jiter .and. nint(data(iqc,i)) < 8

       if ( .not. luse(i) ) then
           icnt_nouse = icnt_nouse + 1
           if (lvldbg >= 100) &
               write(6,*)myname,'(pe:',mype,') Data:',data(idbzob,i),'at ilon ilat:', &
                    data(ilon,i),data(ilat,i),' not use -->',luse(i),' ict:',icnt_nouse
       end if
    end do
    if (lvldbg>1) &
        write(6,*)myname,'(pe=',mype,') number of no use obs on this pe =',icnt_nouse

! Skipping the duplicate observation check (often used in prepbufr conv. obs)

  else 
  ! Read and reformat observations in work arrays.
    read(lunin)data,luse, ioid
!    index information for data array (see reading routine)
    ier=1        ! index of obs error
    ilon=2       ! index of grid relative obs location (x)
    ilat=3       ! index of grid relative obs location (y)
    ihgt=4       ! index of obs elevation
    idbzob=5     ! index of radar reflectivity observation (dBZ)
    iazm=6       ! index of azimuth angle in data array
    itime=7      ! index of observation time in data array (hour)        ! Analysis relative time!
    ikxx=8       ! index of obs type in data array                       ! from the convinfo file (order in the list)
    itilt=9      ! index of tilt angle in data array
    ielev=10     ! index of radar elevation
    id=11        ! index of station id
    iuse=12      ! index of use parameter
    ilone=13     ! index of longitude (degrees)
    ilate=14     ! index of latitude (degrees)
    irange=15    ! index of range in m of obs from radar
    ier2=16      ! index of original-original obs error
    idbznoise=17 ! index of noise threshold for reflectivity (dBZ)
    idmiss2opt=18 ! index of if it is converted from the missing value
 
    irefsmlobs=0
    irejrefsmlobs=0 

  end if

  numequal=0
  numnotequal=0

!
! If requested, save select data for output to diagnostic file
  if(radardbz_diagsave)then
     if ( l_use_dbz_directDA ) then           !! direct reflectivity DA
        ii=0
        nchar=1
        ioff0=26               ! 21 + 5 (22->Zr; 23->Zs; 24->Zg;25->tsenges; 26->rho;)
        nreal=ioff0
     else
        ii=0
        nchar=1
        ioff0=25
        nreal=27
     end if
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (.not.allocated(cdiagbuf)) allocate(cdiagbuf(nobs))
     if (.not.allocated(rdiagbuf)) allocate(rdiagbuf(nreal,nobs))
     if(netcdf_diag) call init_netcdf_diag_
  end if
  mm1=mype+1
  scale=one
  rsig=nsig

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  if ( l_use_dbz_directDA ) then  !! direct reflectivity DA; set zero on qscalar
    qscalar=zero
    if (mype==0) then
       write(6,*)myname,'(pe=',mype,') mphyopt for obs forwrd operator:',mphyopt
    end if
    ! restore ges_qx from cvpqx space to qx space if cvpq is employed
    if ( l_use_cvpqx .and. cvpqx_pval > 0._r_kind  ) then
       call convert_cvpqx2qx(ges_qr,cvpqx_pval)
       call convert_cvpqx2qx(ges_qs,cvpqx_pval)
       call convert_cvpqx2qx(ges_qg,cvpqx_pval)
    end if
  end if

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do
  
! - Observation times are checked in read routine - comment out for now

!  call dtime_setup()
  do i=1,nobs
     debugging=.false.
     if(doradaroneob) debugging=.true.
     dtime=data(itime,i)
     dlat=data(ilat,i)
     dlon=data(ilon,i)
     if ( l_use_dbz_directDA ) then  ! Obs processed from 'read_radarref_directDA' uses ipres index 
        dpres=data(ipres,i)      ! from rdararef_directDA: this height abv MSL
     else
        dpres=data(ihgt,i)
        dbznoise=data(idbznoise,i)
        wrange=data(irange,i)
     end if
     ikx = nint(data(ikxx,i))
     error=data(ier2,i)
     slat=data(ilate,i)*deg2rad               ! needed when converting geophgt to 
                                              ! geometric hgh (hges --> zges below)

     if(debugging) then
       print * , "============="
       print *, dlat,dlon,dpres
       print *, data(ilate,i),data(ilone,i)
     endif


!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

     if (luse_obsdiag) my_diagLL => odiagLL(ibin)

!    Link obs to diagnostics structure
     if(luse_obsdiag)then
        my_diag => obsdiagLList_nextNode(my_diagLL      ,&
                create = .not.lobsdiag_allocated        ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = 1              ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )
        if(.not.associated(my_diag)) call die(myname, &
                'obsdiagLList_nextNode(), create =',.not.lobsdiag_allocated)
     endif

!     (The following part is used in subroutine setupdw)
!     Interpolate terrain height(model elevation) to obs location.
     call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
!     1. dpres (MRMS obs height is height above MSL) is adjusted by zsges, so it
!        is changed to height relative to model elevation (terrain).
!        because in GSI, geop_hgtl is the height relative to terrain (ges_z) (subroutine guess_grids)
     dpres=dpres-zsges
     if ( .not. l_use_dbz_directDA ) then ! direct reflectivitiy DA also uses OBS above 10km 
        if(dpres > 10000.0_r_kind) cycle !don't need obs above 10 km
     end if 
     if (dpres<zero) then
       cycle  !  temporary fix to prevent out of bounds array reference in zges,prsltmp
     endif

!     Interpolate log(ps) & log(pres) at mid-layers and geo-potential height to obs locations/times
!     Note: geop_hgtl is relative to model terrain, i.e. height - ges_z (ref. to subroutine guess_grids)

     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)
     call tintrp2a1(geop_hgtl,hges,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)
   
! 2. Convert geopotential height at layer midpoints to geometric height using
!    equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
!    measures of altitude" (2001).  Available on the web at
!    http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!    termg  = equation 17
!    termr  = equation 21
!    termrg = first term in the denominator of equation 23
!    zges   = equation 23
     sin2  = sin(slat)*sin(slat)
     termg = grav_equator * &
          ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
     termr = semi_major_axis /(one + flattening + grav_ratio -  &
          two*flattening*sin2)
     termrg = (termg/grav)*termr


     if ( l_use_dbz_directDA ) then           !! direct reflectivitiy DA
        if (l_gpht2gmht) then
           do k=1,nsig
              zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23)
           end do
        else
           do k=1,nsig
              zges(k) = hges(k)
           end do
        end if
     else
        do k=1,nsig
           zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23)
        end do
     end if

!    Given observation height (1) convert height to grid relative units, (2) compute
!    compute observation pressure (for diagnostic purposes only), and
!    (3) compute location of midpoint of first model layer above surface
!    in grid relative units
!    Convert observation height (in dpres) from meters to grid relative
!    units.  Save the observation height in zob for later use.
     zob = dpres
     call grdcrd(dpres,1,zges,nsig,1)
  
!    Set indices of model levels below (k1) and above (k2) observation.
     k=dpres
!    wm - updated so {k1,k2} are at min {1,2} and at max {nsig-1,nsig}
     if ( l_use_dbz_directDA ) then
       k1=min(max(1,k),nsig-1)
       k2=min(k1+1,nsig)
     else
       k1=max(1,k)         ! org method
       k2=min(k+1,nsig)
     end if

!    Compute observation pressure (only used for diagnostics)
     dz     = zges(k2)-zges(k1)
     dlnp   = prsltmp(k2)-prsltmp(k1)
     if ( l_use_dbz_directDA ) then
        pobl   = prsltmp(k1) + (dlnp/dz)*(zob-zges(k1))
        presw  = ten*exp(pobl)
        presq  = presw
     else
        if( (k1 == k2) .and. (k1 == 1) ) then
           presw  = ten*exp(prsltmp(k1))
        else
           pobl   = prsltmp(k1) + (dlnp/dz)*(zob-zges(k1))
           presw  = ten*exp(pobl)
        end if
     end if

!    solution to Nan in some members only for EnKF which causes problem?
!    Determine location in terms of grid units for midpoint of
!    first layer above surface
     sfcchk=log(psges)
     call grdcrd(sfcchk,1,prsltmp,nsig,-1)
!    Check to see if observation is below midpoint of first
!    above surface layer.  If so, set rlow to that difference
     rlow=max(sfcchk-dpres,zero)
!    Check to see if observation is above midpoint of layer
!    at the top of the model.  If so, set rhgh to that difference.
     rhgh=max(dpres-r0_001-nsig,zero)
!    Increment obs counter along with low and high obs counters
     if(luse(i))then
        awork(1)=awork(1)+one
        if(rhgh/=zero) awork(2)=awork(2)+one
        if(rlow/=zero) awork(3)=awork(3)+one
     end if

! direct reflectivitiy DA uses l_set_oerr_ratio_dbz. Thus, l_use_dbz_directDA flag is used.
     if ( l_use_dbz_directDA ) then 
!    Adjust observation error.   
!    Observation error currently assumed from user-defined namelist (oe_dbz) 
!    and is *not* adjusted
        if(l_set_oerr_ratio_dbz) then
           ratio_errors = error/(abs(data(ier,i) + 1.0e6_r_kind*rhgh +  &
                 r8*rlow))
        else
           ratio_errors = one
        end if
!
!--- be careful with dpres, especially when it is used in vertical
!    interpolation.  For radar obs, the better way may be use height in vertical
!    interpolation. Anyway, "dpres" used in tintrp3, is not in pressure unit, it
!    is in grid unit.

!    Not adjusting obs error based upon ob vertical location relative to grid box
        if(l_set_oerr_ratio_dbz) then
           ratio_errors = error/(abs(data(ier,i)))
        else
           ratio_errors = one
        end if
     else
     !Not adjusting obs error based upon ob vertical location relative to grid box
        ratio_errors = error/(abs(data(ier,i)))   
     end if
   
     error = one/error

     if(dpres < zero .or. dpres > rsig)ratio_errors = zero

     if ( l_use_dbz_directDA ) then 
!----------------------------------------------------------------------------!
!                                                                            !
! Implementation of forward operator for radar dBZ --------------------------!
!                                                                            !
!----------------------------------------------------------------------------!
!    Interpolate guess q, ts, lnprsl, rho, and hydrometeors  to observation
!    location and time.

        call tintrp31(ges_q,qges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        call tintrp31(ges_tsen,tsenges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        call tintrp31(ges_lnprsl,lnprslges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        P1D=exp(lnprslges)*r1000         ! unit: kPa --> Pa
        Q1D=qges                         ! it is specific humidity
        Q1D=Q1D/(one-Q1D)                ! convert specific humidy to mixing ratio
        T1D=tsenges
        rho=P1D/(rd*T1D*(one+D608*Q1D))  ! air density in kg m^-3      

        qrges=zero
        call tintrp31(ges_qr,qrges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        if ( mphyopt == 108 .and. miter > 0 ) then
           iqrges=zero
           call tintrp31(ges_iqr,iqrges,dlat,dlon,dpres,dtime, &
              hrdifsig,mype,nfldsig)
        end if
        qsges=zero
        call tintrp31(ges_qs,qsges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        qgges=zero
        call tintrp31(ges_qg,qgges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
        if (mphyopt == 108 .or. mphyopt == 2 ) then
           call tintrp31(ges_nr,qscalar(P_nr),dlat,dlon,dpres,dtime, &
              hrdifsig,mype,nfldsig)
           if (l_cvpnr) then
              qscalar(P_nr)=(cvpnr_pval*qscalar(P_nr)+1)**(1/cvpnr_pval)
           end if
           qscalar(P_nr)=qscalar(P_nr)*rho  ! convert qnr to unit demanded by operators
        end if

!    Convert guess log(qr, qs, qg) on obs point to qr/qs/qg 
!    Note: using log-transformed Qrsg for spatial interpolation
        if ( l_use_cvpqx ) then
!        Note: the zero qx or very tiny value of qx is re-set to be non-zero
!        "bigger" tiny value in read_wrf_mass_guess.F90 for log-transformed qx
           if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
              qrexp = exp(qrges)
              qsexp = exp(qsges)
              qgexp = exp(qgges)
           else if ( cvpqx_pval > 0._r_kind ) then ! CVpq
              qr_min = 1.0E-8_r_kind
              qs_min = 1.0E-8_r_kind
              qg_min = 1.0E-8_r_kind
              qrexp = max(qrges, qr_min)
              qsexp = max(qsges, qs_min)
              qgexp = max(qgges, qg_min)
           end if
        else  ! CV_q
!        Note: the zero qx or very tiny value of qx needs to be re-set to non-zero
!        "bigger" tiny value here for log10(Zer + Zes + Zeg)
           if ( miter == 0 ) then ! EnKF does not need this limitation
              qrexp=qrges
              qsexp=qsges
              qgexp=qgges
           else
              qr_min = 1.0E-8_r_kind
              qs_min = 1.0E-8_r_kind
              qg_min = 1.0E-8_r_kind
              qrexp = max(qrges, qr_min)
              qsexp = max(qsges, qs_min)
              qgexp = max(qgges, qg_min)
           end if

        end if
!    array qscalar is used in dbz forward operator in radarz package    
        if ( P_qr > 0 ) qscalar(P_qr) = qrexp
        if ( P_qs > 0 ) qscalar(P_qs) = qsexp
        if ( P_qg > 0 ) qscalar(P_qg) = qgexp
        if ( P_qh > 0 ) qscalar(P_qh) = qgexp             ! no hail for WRF-ARW run

        Zer = zero ;         Zes = zero ;          Zeg = zero ;
        jqr = zero ;         jqs = zero ;          jqg = zero ;
        jqr_num = zero ;     jqs_num = zero ;      jqg_num = zero ;
        wgt_dry = zero ;     wgt_wet = zero ;
        Zeg_dry = zero ;     Zeg_wet = zero ;
        jqg_num_dry = zero ; jqg_num_wet = zero ;
        denom = zero;
        jqnr = zero ; jqnr_num = zero

!    Compute simulated *equivalent* radar reflectivity
!    also Jacobian used for TLM and ADM

        select case (mphyopt)
           case (2,3,4,5,6,7)
!             mphyopt = 2/3/4/5/6/7 : single moment MicroPhyics scheme

!             rain
              Zer = Cr  * (rho * qrexp)**(Pr)

!             snow
              if ( i_melt_snow < 0 ) then
!                no melting: dry snow at any temperature
                 Zes = Cs_dry * (rho * qsexp)**(Ps_dry)
              else if ( i_melt_snow  == 100 ) then
!                melting: wet snow at any temperature
                 Zes = Cs_wet * (rho * qsexp)**(Ps_wet)
              else
!                melting: depending on temperature
                 if (T1D < 273.15_r_kind) then
                    Zes = Cs_dry * (rho * qsexp)**(Ps_dry)
                 else
                    Zes = Cs_wet * (rho * qsexp)**(Ps_wet)
                 end if
              end if

!             graupel/hail
              if ( i_melt_graupel < 0 ) then
!                no melting: dry grauple/hail at any temperature
                 Zeg = Cg_dry * (rho * qgexp)**(Pg_dry)
              else if ( i_melt_graupel  == 100 ) then
!                melting: wet graupel at any temperature
                 Zeg = Cg_wet * (rho * qgexp)**(Pg_wet)
              else
!                melting: depending on the temperature
                 if (T1D < (273.15_r_kind - 2.5_r_kind)) then
                    Zeg = Cg_dry * (rho * qgexp)**(Pg_dry)
                 else if (T1D > (273.15_r_kind + 2.5_r_kind)) then
                    Zeg = Cg_wet * (rho * qgexp)**(Pg_wet)
                 else
                    wgt_dry = abs(T1D - (273.15_r_kind + 2.5_r_kind))/5.0_r_kind
                    wgt_wet = abs(T1D - (273.15_r_kind - 2.5_r_kind))/5.0_r_kind
                    Zeg_dry = Cg_dry * (rho * qgexp)**(Pg_dry)
                    Zeg_wet = Cg_wet * (rho * qgexp)**(Pg_wet)
                    Zeg     = wgt_dry*Zeg_dry + wgt_wet*Zeg_wet
                 end if
              end if
!
              Ze=Zer+Zes+Zeg

!             Zelim treatment
              if(Ze <1.0_r_kind) then
                 Ze=Ze + 1.0_r_kind
              end if

!             Convert to simulated radar reflectivity in units of dBZ
              Ze_orig = Ze
              rdBZ = ten * log10(Ze)
              rdBZr = ten * log10(Zer)
              rdBZs = ten * log10(Zes)
              rdBZg = ten * log10(Zeg)

!             find dqr/ddBZ, dqs/ddBZ, dqg/ddBZ (used in inner loop routine)
!             Jacobian used for TLM and ADM
              if ( l_use_cvpqx ) then
!                rain
                 if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
                    jqr_num = Pr*Zer
                 else if ( cvpqx_pval > 0._r_kind ) then ! CVpq
                    jqr_num = Cr*((rho)**Pr)*Pr*((qrexp)**(Pr-cvpqx_pval))
                 end if

!                snow
                 if ( i_melt_snow < 0 ) then
!                   no melting: dry snow at any temperature
                    if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
                       jqs_num = Ps_dry*Zes
                    else if ( cvpqx_pval > 0._r_kind ) then ! CVpq
                       jqs_num = Cs_dry*((rho)**Ps_dry)*Ps_dry*((qsexp)**(Ps_dry-cvpqx_pval))
                    end if
                 else if ( i_melt_snow  == 100 ) then
!                   melting: wet snow at any temperature
                    if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
                       jqs_num = Ps_wet*Zes
                    else if ( cvpqx_pval > 0._r_kind ) then ! CVpq
                       jqs_num = Cs_wet*((rho)**Ps_wet)*Ps_wet*((qsexp)**(Ps_wet-cvpqx_pval))
                    end if
                 else
!                   melting: depending on temperature
                    if (T1D < 273.15_r_kind) then
                       if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
                          jqs_num = Ps_dry*Zes
                       else if ( cvpqx_pval > 0._r_kind ) then ! CVpq
                          jqs_num = Cs_dry*((rho)**Ps_dry)*Ps_dry*((qsexp)**(Ps_dry-cvpqx_pval))
                       end if
                    else
                       if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
                          jqs_num = Ps_wet*Zes
                       else if ( cvpqx_pval > 0._r_kind ) then ! CVpq
                          jqs_num = Cs_wet*((rho)**Ps_wet)*Ps_wet*((qsexp)**(Ps_wet-cvpqx_pval))
                       end if
                    end if
                 end if

!                graupel/hail
                 if ( i_melt_graupel < 0 ) then
!                   no melting: dry grauple/hail at any temperature
                    if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
                       jqg_num = Pg_dry*Zeg
                    else if ( cvpqx_pval > 0_r_kind ) then ! CVpq
                       jqg_num = Cg_dry*((rho)**Pg_dry)*Pg_dry*((qgexp)**(Pg_dry-cvpqx_pval))
                    end if
                 else if ( i_melt_graupel  == 100 ) then
!                   melting: wet graupel at any temperature
                    if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
                       jqg_num = Pg_wet*Zeg
                    else if ( cvpqx_pval > 0._r_kind ) then ! CVpq
                       jqg_num = Cg_wet*((rho)**Pg_wet)*Pg_wet*((qgexp)**(Pg_wet-cvpqx_pval))
                    end if
                 else
!                   melting: depending on the temperature
                    if (T1D < (273.15_r_kind - 2.5_r_kind)) then
                       if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
                          jqg_num = Pg_dry*Zeg
                       else if ( cvpqx_pval > 0._r_kind ) then ! CVpq
                          jqg_num = Cg_dry*((rho)**Pg_dry)*Pg_dry*((qgexp)**(Pg_dry-cvpqx_pval))
                       end if
                    else if (T1D > (273.15_r_kind + 2.5_r_kind)) then
                       if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
                          jqg_num = Pg_wet*Zeg
                       else if ( cvpqx_pval > 0._r_kind ) then ! CVpq
                          jqg_num = Cg_wet*((rho)**Pg_wet)*Pg_wet*((qgexp)**(Pg_wet-cvpqx_pval))
                       end if
                    else
                        wgt_dry = abs(T1D - (273.15_r_kind + 2.5_r_kind))/5.0_r_kind
                        wgt_wet = abs(T1D - (273.15_r_kind - 2.5_r_kind))/5.0_r_kind
                        if ( cvpqx_pval == 0._r_kind ) then ! CVlogq
                           jqg_num_dry = Pg_dry*Zeg
                           jqg_num_wet = Pg_wet*Zeg
                        else if ( cvpqx_pval > 0._r_kind ) then ! CVpq
                           jqg_num_dry = Cg_dry*((rho)**Pg_dry)*Pg_dry*((qgexp)**(Pg_dry-cvpqx_pval))
                           jqg_num_wet = Cg_wet*((rho)**Pg_wet)*Pg_wet*((qgexp)**(Pg_wet-cvpqx_pval))
                        end if
                        jqg_num = wgt_dry*jqg_num_dry + wgt_wet*jqg_num_wet
                    end if
                 end if
   
              else ! CVq
!                rain
                 jqr_num = Cr*((rho)**Pr)*Pr*((qrexp)**(Pr - one))
   
!                snow
                 if ( i_melt_snow < 0 ) then
!                   no melting: dry snow at any temperature
                    jqs_num = Cs_dry*((rho)**Ps_dry)*Ps_dry*((qsexp)**(Ps_dry - one))
                 else if ( i_melt_snow  == 100 ) then
!                   melting: wet snow at any temperature
                    jqs_num = Cs_wet*((rho)**Ps_wet)*Ps_wet*((qsexp)**(Ps_wet - one))
                 else
!                   melting: depending on temperature
                    if (T1D < 273.15_r_kind) then
                       jqs_num = Cs_dry*((rho)**Ps_dry)*Ps_dry*((qsexp)**(Ps_dry - one))
                    else
                       jqs_num = Cs_wet*((rho)**Ps_wet)*Ps_wet*((qsexp)**(Ps_wet - one))
                    end if
                 end if
   
!                graupel/hail
                 if ( i_melt_graupel < 0 ) then
!                   no melting: dry grauple/hail at any temperature
                    jqg_num = Cg_dry*((rho)**(Pg_dry))*(Pg_dry)*((qgexp**(Pg_dry - one)))
                 else if ( i_melt_graupel  == 100 ) then
!                   melting: wet graupel at any temperature
                    jqg_num = Cg_wet*((rho)**(Pg_wet))*(Pg_wet)*((qgexp**(Pg_wet - one)))
                 else
!                   melting: depending on the temperature
                    if (T1D < (273.15_r_kind - 2.5_r_kind)) then
                       jqg_num = Cg_dry*((rho)**(Pg_dry))*(Pg_dry)*((qgexp**(Pg_dry - one)))
                    else if (T1D > (273.15_r_kind + 2.5_r_kind)) then
                       jqg_num = Cg_wet*((rho)**(Pg_wet))*(Pg_wet)*((qgexp**(Pg_wet - one)))
                    else
                       wgt_dry = abs(T1D - (273.15_r_kind + 2.5_r_kind))/5.0_r_kind
                       wgt_wet = abs(T1D - (273.15_r_kind - 2.5_r_kind))/5.0_r_kind
                       jqg_num_dry = Cg_dry*((rho)**(Pg_dry))*(Pg_dry)*((qgexp**(Pg_dry - one)))
                       jqg_num_wet = Cg_wet*((rho)**(Pg_wet))*(Pg_wet)*((qgexp**(Pg_wet - one)))
                       jqg_num = wgt_dry*jqg_num_dry + wgt_wet*jqg_num_wet
                    end if
                 end if
   
              end if
   
              denom=(log(ten))*Ze
   
              ! multiply ten for numerators
              jqr  = ten*jqr_num/denom
              jqs  = ten*jqs_num/denom
              jqg  = ten*jqg_num/denom
   
           case (108)
!          mphyopt = 108 :   Thompson MP scheme with double moment
         
              if ( miter == 0 ) then ! For EnKF
                 qgh_opt = get_qgh_opt(1, 0)

                 call set_dsd_para()
                 call calcMDR()
                 if (firstcalled) then
                    call calcConstants()
                    firstcalled = .false.
                 end if

                 obs_dual = init_refl()
                 var_dsd = init_para_dsd()
                 if (l_use_tdep_radarz) ta = T1D ! pass air temperature to radarZ
                 call rdr_obs(real(rho, kind=r_kind), real(qscalar, kind=r_kind), obs_dual, var_dsd, 1, 1)
                 rdBZ = real(obs_dual%T_log_ref, kind=r_kind)
                 Ze = ten ** (rdBZ / ten)

              else                   ! For Variational DA
                 Cr=3630803456.00000_r_kind
                 Pr=1.75_r_kind
                 Cs_dry=958893312.000000_r_kind
                 Cg_dry=5743808512.00000_r_kind
                 pg_dry=1.75_r_kind
                 Ps_dry=1.75_r_kind
!----TM-------------
                 Cg_wet= 5.54914E+12_r_kind
                 Cg_dry= 1.90409E+12_r_kind
                 Pg_wet= 2.5_r_kind

!                rain
                 Zero1 = Cr  * (rho * qrexp)**(Pr)
                 if (  qrexp > 10E-7_r_kind .and. qscalar(P_nr) > 20._r_kind  ) then
                    Zer = 720_r_kind *(rho*qrexp)**2_r_kind*ten**18_r_kind/(pi**2_r_kind*rhor**2_r_kind*qscalar(P_nr))
                 else
                    Zer = 10E-8_r_kind
                 endif
    
!                snow
                 Zeso  = Cs_dry * (rho * qsexp)**(Ps_dry)
                 if (qsexp > 10E-7_r_kind) then 
                    if ( T1D > 274.15_r_kind .and. iqrges > 1.E-12_r_kind) then
                       Zes = (1.47E+05_r_kind)*(qsexp*1000._r_kind)**2.67_r_kind
                    else
                       ! calc coeffs for dry snow based on TM MP code 
                       call calc_coeffs_dry_snow_tm(T1D,a_dry_snow_tm,b_dry_snow_tm)
                       smoz = a_dry_snow_tm * (qsexp*rho*oams)**b_dry_snow_tm
                       Zes  = alpha_const_tm_dry_snow*smoz
                    end if
                 else
                    Zes = 10E-8_r_kind
                 endif
    
!                graupel/hail
                 Zego  = Cg_dry * (rho * qgexp)**(Pg_dry)
                 if ( qgexp > 10E-7_r_kind ) then
                    if ( T1D > 274.15_r_kind .and. iqrges > 1.E-12_r_kind) then
                       Zeg = Cg_wet*rho**1.75_r_kind*qgexp**Pg_wet
                    else
                       Zeg = Cg_dry*rho**1.75_r_kind*qgexp**Pg_wet
                    end if
                 else
                    Zeg=10E-8_r_kind
                 endif
    
                 Zeo=Zero1+Zeso+Zego
    
                 if(Zeo <1.0_r_kind) then
                    Zeo=Zeo + 1.0_r_kind
                 end if
    
                 Zo = ten * log10(Zeo)
                 Zro = ten * log10(Zero1)
                 Zso = ten * log10(Zeso)
                 Zgo = ten * log10(Zego)
    
                 Ze=Zer+Zes+Zeg
    
!                Zelim treatment
                 if(Ze <1.0_r_kind) then
                    Ze=Ze + 1.0_r_kind
                 end if
    
!                Convert to simulated radar reflectivity in units of dBZ
                 Ze_orig = Ze
                 rdBZ = ten * log10(Ze)
                 rdBZr = ten * log10(Zer)
                 rdBZs = ten * log10(Zes)
                 rdBZg = ten * log10(Zeg)
    
!                find dqr/ddBZ, dqs/ddBZ, dqg/ddBZ (used in inner loop routine)
!                Jacobian used for TLM and ADM
                 if ( l_use_cvpqx ) then
!                   rain
                    if ( qrexp > 10E-7_r_kind .and. qscalar(P_nr) > 20._r_kind ) then
                       jqr_num = (2_r_kind*720_r_kind*rho**2_r_kind*qrexp**(2_r_kind-cvpqx_pval)/&
                                 (pi**2_r_kind*rhor**2_r_kind*qscalar(P_nr)))*ten**12_r_kind*ten**6_r_kind
                       if (l_cvpnr) then
                          jqnr_num = (-720_r_kind*rho**2_r_kind*qrexp**2_r_kind/&
                                     (pi**2_r_kind*rhor**2_r_kind*qscalar(P_nr)**(1_r_kind+cvpnr_pval)))&
                                     *ten**12_r_kind*ten**6_r_kind
                       else
                          jqnr_num = (-720_r_kind*rho**2_r_kind*qrexp**2_r_kind/&
                                     (pi**2_r_kind*rhor**2_r_kind*qscalar(P_nr)**2_r_kind))&
                                     *ten**12_r_kind*ten**6_r_kind
                       end if
                    else
                       jqr_num = 10E-8_r_kind
                       jqnr_num = 10E-8_r_kind
                    endif
    
!                   snow
                    if (qsexp > 10E-7_r_kind) then
                       if ( T1D > 274.15_r_kind .and. iqrges > 1.E-12_r_kind) then
                          jqs_num= (1.47E+05_r_kind)*(1000._r_kind)**2.67_r_kind*(2.67_r_kind)*(qsexp)**(2.67_r_kind-cvpqx_pval)
                       else
                          jqs_num= alpha_const_tm_dry_snow*a_dry_snow_tm*b_dry_snow_tm &
                                   *((rho)**b_dry_snow_tm)*((oams)**b_dry_snow_tm)*((qsexp)**(b_dry_snow_tm-cvpqx_pval))
                       end if
                    else
                       jqs_num = 10E-8_r_kind
                    endif
    
!                   graupel/hail
                    if ( qgexp > 10E-7_r_kind ) then
                       if ( T1D > 274.15_r_kind .and. iqrges > 1.E-12_r_kind) then
                          jqg_num=Cg_wet*((rho)**1.75_r_kind)*Pg_wet*((qgexp)**(Pg_wet-cvpqx_pval))
                       else
                          jqg_num=Cg_dry*((rho)**1.75_r_kind)*Pg_wet*((qgexp)**(Pg_wet-cvpqx_pval))
                       end if
                    else
                       jqg_num=10E-8_r_kind
                    endif
    
                 else ! CVq
    
!                   rain
                    if ( qrexp > 10E-7_r_kind .and. qscalar(P_nr) > 1._r_kind ) then
                       jqr_num = (2_r_kind*720_r_kind*rho**2_r_kind*qrexp/&
                                 (pi**2_r_kind*rhor**2_r_kind*qscalar(P_nr)))*ten**12_r_kind*ten**6_r_kind
                       jqnr_num = (-720_r_kind*rho**2_r_kind*qrexp**2_r_kind/&
                                  (pi**2_r_kind*rhor**2_r_kind*qscalar(P_nr)**2_r_kind))*ten**12_r_kind*ten**6_r_kind
                    else
                       jqr_num = 10E-8_r_kind
                       jqnr_num = 10E-8_r_kind
                    endif
    
!                   snow
                    if (qsexp > 10E-7_r_kind) then
                       if ( T1D >= 273.15_r_kind .and. iqrges > 1.E-12_r_kind) then
                          jqs_num= (1.47E+05_r_kind)*(1000._r_kind)**2.67_r_kind*(2.67_r_kind)*(qsexp)**(2.67_r_kind-1.0_r_kind)
                       else
                          jqs_num= alpha_const_tm_dry_snow*a_dry_snow_tm*b_dry_snow_tm &
                                   *((rho)**b_dry_snow_tm)*((oams)**b_dry_snow_tm)*((qsexp)**(b_dry_snow_tm-1.0_r_kind))
                       end if
                    else
                       jqs_num = 10E-8_r_kind
                    endif
    
!                   graupel/hail
                    if ( qgexp > 10E-7_r_kind ) then
                       if ( T1D > 274.15_r_kind .and. iqrges > 1.E-12_r_kind) then
                          jqg_num=Cg_wet*((rho)**1.75_r_kind)*Pg_wet*((qgexp)**(Pg_wet-1.0_r_kind))
                       else
                          jqg_num=Cg_dry*((rho)**1.75_r_kind)*Pg_wet*((qgexp)**(Pg_wet-1.0_r_kind))
                       end if
    
                    else
                       jqg_num=10E-8_r_kind
                    endif
                 end if 
    
                 denom=(log(ten))*Ze
      
                 ! multiply ten for numerators
                 jqr  = ten*jqr_num/denom
                 jqs  = ten*jqs_num/denom
                 jqg  = ten*jqg_num/denom
                 jqnr = ten*jqnr_num/denom
    
              end if
    
           case default
              write(6,*) 'not recognized mphyopt-->',mphyopt
              call stop2(999)
        end select

     else


!    Interpolate guess dbz to observation location and time.
        if(if_model_dbz) then
        call tintrp31(ges_dbz,dbzgesin,dlat,dlon,dpres,dtime,& !modified
             hrdifsig,mype,nfldsig)
        endif
!    Interpolate guess qr, qli, and rho to observation location and time.
        call tintrp31(ges_qr,qrgesin,dlat,dlon,dpres,dtime,& !modified
             hrdifsig,mype,nfldsig)
        if( wrf_mass_regional )then
          call tintrp31(ges_qs,qsgesin,dlat,dlon,dpres,dtime,& 
               hrdifsig,mype,nfldsig)
          call tintrp31(ges_qg,qggesin,dlat,dlon,dpres,dtime,& 
               hrdifsig,mype,nfldsig)
        else if(nems_nmmb_regional) then
          call tintrp31(ges_qli,qligesin,dlat,dlon,dpres,dtime,&
               hrdifsig,mype,nfldsig)
        endif
        call tintrp31(ges_rho,rhogesin,dlat,dlon,dpres,dtime,&
             hrdifsig,mype,nfldsig)
        call tintrp31(ges_tsen,tempgesin,dlat,dlon,dpres,dtime,&
             hrdifsig,mype,nfldsig)

        if( nems_nmmb_regional ) then
          qrgesin1  = max(qrgesin,1.e-6_r_kind)
          qligesin1 = max(qligesin,1.e-6_r_kind)
        else if( wrf_mass_regional ) then
          qrgesin1  = max(qrgesin,1.e-6_r_kind)
          qsgesin1  = max(qsgesin,1.e-6_r_kind) 
          qggesin1  = max(qggesin,1.e-5_r_kind) 
        end if

        if(if_model_dbz) then
          rDBZ=dbzgesin
        else
          if( wrf_mass_regional )then
             call hx_dart(qrgesin,qggesin,qsgesin,rhogesin,tempgesin,rDBZ,debugging)
          else if( nems_nmmb_regional ) then
             write(6,*) "if_model_dbz should be set as .true."
             STOP
          endif
        endif !if_model_dbz

        if(miter == 0.or.l_hyb_ens) then !ie an enkf run
! DCD 1 March 2019:  changed 0.0 to static_gsi_nopcp_dbz
!          if(rDBZ < 0_r_kind) rDBZ=0.0_r_kind ! should be the same as in the read_dbz when nopcp=.true.
           if(rDBZ < static_gsi_nopcp_dbz) rDBZ=static_gsi_nopcp_dbz ! should be the same as in the read_dbz when nopcp=.true.
        endif
        if(miter == 0.and.ens_hx_dbz_cut) then !ie an enkf run
          if(rDBZ > 60_r_kind) rDBZ=60_r_kind
        endif

        jqr = 0.0_r_kind
        jqs = 0.0_r_kind
        jqg = 0.0_r_kind

        if( .not. if_model_dbz )then
          if( wrf_mass_regional ) then
            call jqr_dart(qrgesin1,qsgesin1,qggesin1,rhogesin,tempgesin,jqr)
            call jqs_dart(qrgesin1,qsgesin1,qggesin1,rhogesin,tempgesin,jqs)
            call jqg_dart(qrgesin1,qsgesin1,qggesin1,rhogesin,tempgesin,jqg)
          else if( nems_nmmb_regional ) then
             write(6,*) "if_model_dbz should be set as .true."
             STOP
          endif
        end if
  
     end if
!
   
     if(rdBZ==data(idbzob,i)) then
        numequal=numequal+1
     else
        numnotequal=numnotequal+1
     end if
    
! Compute innovations
     !--------------Calculate departure from observation----------------!
     ddiff = data(idbzob,i) - rdBZ

     if ( .not. l_use_dbz_directDA ) then 
        if(miter > 0.and..not.l_hyb_ens) ddiff = max(min(ddiff,20.0_r_kind),-20.0_r_kind)
     end if

     if(debugging) print *, "DDIFF1: ",ddiff,data(idbzob,i),rdBZ

! If requested, setup for single obs test.
     if (oneobtest) then
        ddiff = maginnov
        error=one/magoberr
        ratio_errors=one
     end if

     if (doradaroneob) then
        if(oneobvalue > -900_r_kind) then
           data(idbzob,i) = oneobvalue
           ddiff = data(idbzob,i) - rdBZ
        else
           ddiff = oneobddiff
           data(idbzob,i) = rdBZ+ddiff
        endif
     endif !oneob

     if(rdBZ >= 5_r_kind) irefsmlobs=irefsmlobs+1

     if(debugging) print *, "DDIFF2: ",ddiff,data(idbzob,i),rdBZ

!    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)     
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     if ( l_use_dbz_directDA ) then    ! direct reflectivity DA
        if (l_set_oerr_ratio_dbz) then
            if (ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
               if (luse(i)) awork(4) = awork(4)+one
               error = zero
               ratio_errors = zero
            end if
         else
            ratio_errors = one
         end if

     else

!       Apply gross error check only to reflectivity observations in precipitation (>= 5 dBZ).
        if ( ( (data(idbzob,i) >= 5_r_kind) .and. (ratio > cgross(ikx)) ) .or. (ratio_errors < tiny_r_kind) ) then

           if ( inflate_dbz_obserr .and. (ratio-cgross(ikx)) <= cgross(ikx) .and. ratio_errors >= tiny_r_kind) then 
           ! Since radar reflectivity can be very different from the model background
           ! good observations may be rejected during this QC step.  However, if these observations
           ! are allowed through, they can yield problems with convergence.  Therefore the error
           ! is inflated here up to twice the observation error in a manner that is
           ! proportional to the residual.  If this IF-TEST for this inflation fails, the
           ! observation is subsequently rejected.
                    
              obserror = residual/cgross(ikx)
              error = one/obserror
           
           else
              if (luse(i)) awork(4) = awork(4)+one 
              error = zero 
              ratio_errors = zero 
       
              if(rdBZ <= 5_r_kind) irejrefsmlobs=irejrefsmlobs+1
           end if
        end if

     end if

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false. 
     !-- if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_dbz_ob_type,ibin)%tail%muse(nobskeep)
     if (nobskeep>0.and.luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))
     
     val     = error*ddiff
             
!    Compute penalty terms (linear & nonlinear qc).
     if(luse(i))then
        exp_arg  = -half*val**2
        rat_err2 = ratio_errors**2
        val2=val*val
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_w=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_w*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term
       
!       Accumulate statistics for obs belonging to this task
        if (muse(i)) then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(6*nsig+jsig+100)=awork(6*nsig+jsig+100)+val2*rat_err2
           awork(5*nsig+jsig+100)=awork(5*nsig+jsig+100)+one
           awork(3*nsig+jsig+100)=awork(3*nsig+jsig+100)+valqc
        end if
!       Loop over pressure level groupings and obs to accumulate
!       statistics as a function of observation type.
        ress  = scale*ddiff
        ressw = ress*ress
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(presw >=ptop(k) .and. presw<=pbot(k))then  
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+ddiff          ! bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
             
           end if
        end do
     end if
     if(luse_obsdiag)then
        if ( l_use_dbz_directDA ) then    ! direct reflectivitiy DA
           call obsdiagNode_set(my_diag, luse=luse(i), wgtjo=(error*ratio_errors)**2, &
              jiter=jiter, muse=muse(i), nldepart=ddiff)
        else
           call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
              jiter=jiter, muse=muse(i), nldepart=ddiff)
        endif
     end if

     
!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then             
       
        allocate(my_head)
        call dbzNode_appendto(my_head,dbzhead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij)   

        my_head%raterr2 = ratio_errors**2
        my_head%res    = ddiff   
        my_head%err2    = error**2
        my_head%time    = dtime
        my_head%luse    = luse(i)
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)
        my_head%jqr     = jqr

        if ( wrf_mass_regional .or. fv3_regional) then
          my_head%jqs     = jqs
          my_head%jqg     = jqg
          if ( l_use_dbz_directDA ) then
             my_head%jqnr    = jqnr                ! for TL and AD
          end if
        end if


        if ( l_use_dbz_directDA ) then
           if(oberror_tune) then
              my_head%dbzpertb=data(iptrb,i)/error/ratio_errors
              my_head%kx=ikx
              if(presq > ptabl(2))then
                 my_head%k1=1
              else if( presq <= ptabl(33)) then
                 my_head%k1=33
              else
                 k_loop: do k=2,32
                    if(presq > ptabl(k+1) .and. presq <= ptabl(k)) then
                       my_head%k1=k
                       exit k_loop
                    endif
                 enddo k_loop
              endif
           endif
        end if

        if(luse_obsdiag)then
          call obsdiagNode_assert(my_diag,my_head%idv,my_head%iob,1,myname,'my_diag:my_head')
          my_head%diags => my_diag
        endif

        my_head => null()
     endif

!    Save select output for diagnostic file
     if(radardbz_diagsave .and. luse(i) )then


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

        if ( l_use_dbz_directDA ) then    ! direct reflectivity DA
           if(binary_diag) call contents_binary_dirZDA_diag_(my_diag)
           if(netcdf_diag) write(6,*) 'Currently not supported for direct reflectivity DA!!!'
        else

           if(binary_diag) call contents_binary_diag_(my_diag)
           if(netcdf_diag) call contents_netcdf_diag_(my_diag)
        end if

     end if
  end do

! reconvert ges_qx from qx space to cvpqx space if cvpq is employed
  if ( l_use_cvpqx .and. cvpqx_pval > 0._r_kind  ) then
     call convert_qx2cvpqx(ges_qr,cvpqx_pval)
     call convert_qx2cvpqx(ges_qs,cvpqx_pval)
     call convert_qx2cvpqx(ges_qg,cvpqx_pval)
  end if

! Release memory of local guess arrays
  call final_vars_
! Write information to diagnostic file
  if(radardbz_diagsave .and. netcdf_diag) call nc_diag_write
  if(radardbz_diagsave .and. binary_diag .and. ii>0  )then

     if( .not. l_use_dbz_directDA .and. .not. if_model_dbz )then
        write(7)'dbz',nchar,nreal,ii,mype,ioff0
        write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        deallocate(cdiagbuf,rdiagbuf)
     else

        write(string,600) jiter
600     format('radardbz_',i2.2)
        diag_file=trim(dirname) // trim(string)
        if(init_pass) then
           open(newunit=lu_diag,file=trim(diag_file),form='unformatted',status='unknown',position='rewind')
        else
           inquire(file=trim(diag_file),exist=diagexist)
           if (diagexist) then
              open(lu_diag,file=trim(diag_file),form='unformatted',status='old',position='append')
           else
              open(lu_diag,file=trim(diag_file),form='unformatted',status='unknown',position='rewind')
           endif
        endif
        if(init_pass .and. mype == 0) then
           if ( .not. l_use_dbz_directDA ) then    ! EnKF uses these diagnostics and EnKF uses single OBS file for now.
              write(lu_diag) ianldate          ! So do not write analysis date for binary in case of using direct reflectivity DA.
           end if
           write(6,*)'SETUPDBZ:   write time record to file ',&
                   trim(diag_file), ' ',ianldate
        endif

        write(lu_diag)'dbz',nchar,nreal,ii,mype,ioff0
        write(lu_diag)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        deallocate(cdiagbuf,rdiagbuf)
        close(lu_diag)
     end if
  end if
  write(6,*)'mype, irefsmlobs,irejrefsmlobs are ',mype,' ',irefsmlobs, ' ',irejrefsmlobs
! close(52) !simulated obs
! End of routine
  contains

  subroutine check_vars_ (proceed)

  use radarz_cst, only: mphyopt
  use directDA_radaruse_mod, only: l_use_dbz_directDA
  use obsmod, only: if_model_dbz
  use gridmod, only: wrf_mass_regional, nems_nmmb_regional, fv3_regional

  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  if( .not. l_use_dbz_directDA) then ! direct reflectvitiy DA does not use tv
     call gsi_metguess_get ('var::tv', ivar, istatus )
     proceed=proceed.and.ivar>0
  end if
  if( if_model_dbz ) then
      call gsi_metguess_get ('var::dbz', ivar, istatus )
      proceed=proceed.and.ivar>0
  end if
  call gsi_metguess_get ('var::qr', ivar, istatus )
  proceed=proceed.and.ivar>0
  if(wrf_mass_regional .or. fv3_regional )then
     call gsi_metguess_get ('var::qs', ivar, istatus )
     proceed=proceed.and.ivar>0
     call gsi_metguess_get ('var::qg', ivar, istatus )
     proceed=proceed.and.ivar>0
  end if
  if(nems_nmmb_regional)then
     call gsi_metguess_get ('var::qli', ivar, istatus )
     proceed=proceed.and.ivar>0
  end if
  if ( l_use_dbz_directDA ) then
     call gsi_metguess_get ('var::q' , ivar, istatus )
     proceed=proceed.and.ivar>0
     if ( mphyopt == 2 .or. mphyopt == 108 ) then
        call gsi_metguess_get ('var::qnr', ivar, istatus )
        proceed=proceed.and.ivar>0
     end if
  end if
  end subroutine check_vars_

  subroutine init_vars_

  use radarz_cst, only: mphyopt
  use directDA_radaruse_mod, only: l_use_dbz_directDA
  use obsmod, only: if_model_dbz
  use gridmod, only: wrf_mass_regional, nems_nmmb_regional, fv3_regional

  real(r_kind),dimension(:,:  ),pointer:: rank2
  real(r_kind),dimension(:,:,:),pointer:: rank3
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

     if(if_model_dbz)then
     !    get dbz ....
         varname='dbz'
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
         if (istatus==0) then
           if(allocated(ges_dbz))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
           endif
           allocate(ges_dbz(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
           ges_dbz(:,:,:,1)=rank3
           do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_dbz(:,:,:,ifld)=rank3
           enddo
         else
           write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
           call stop2(999)
         endif
     endif

!    get qr ...
     varname='qr'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qr))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qr(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qr(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qr(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif

     if(wrf_mass_regional .or. fv3_regional )then
!    get qs ...
     varname='qs'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qs))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qs(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qs(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qs(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif

!    get qg ...
     varname='qg'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qg))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qg(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qg(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qg(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif


     end if

     if(nems_nmmb_regional)then
!      get qli ...
       varname='qli'
       call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
       if (istatus==0) then
           if(allocated(ges_qli))then
              write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
              call stop2(999)
           endif
           allocate(ges_qli(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
           ges_qli(:,:,:,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
              ges_qli(:,:,:,ifld)=rank3
           enddo
       else
           write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
           call stop2(999)
       endif
     end if

     if ( l_use_dbz_directDA ) then
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
!    get qnr ...
        if ( mphyopt == 2 .or. mphyopt == 108 ) then
            varname='qnr'
            call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus) 
            if (istatus==0) then
                if(allocated(ges_nr))then
                    write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
                    call stop2(999)
                endif
                allocate(ges_nr(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
                ges_nr(:,:,:,1)=rank3
                do ifld=2,nfldsig
                    call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
                    ges_nr(:,:,:,ifld)=rank3
                enddo
            else
                write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
                call stop2(999)
            endif
        end if
!       get iqr ... for variational DA
        if ( mphyopt == 108 .and. miter > 0) then
            varname='iqr'
            call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
            if (istatus==0) then
                if(allocated(ges_iqr))then
                   write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
                   call stop2(999)
                endif
                allocate(ges_iqr(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
                ges_iqr(:,:,:,1)=rank3
                do ifld=2,nfldsig
                    call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
                    ges_iqr(:,:,:,ifld)=rank3
                enddo
            else
                write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
                call stop2(999)
            endif
        end if
     end if
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
900  format('conv_dbz_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists. Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)
    
     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        call nc_diag_header("Number_of_state_vars", nsdim          )
     endif
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag

        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype

        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters)
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = (dtime*r60)-time_offset  ! obs time (sec relative to analysis time)
        rdiagbuf(9,ii)  = rmiss_single       ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use,-1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        rdiagbuf(13,ii) = rwgt                 ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input         ! prepbufr inverse obs error (dBZ)**-1
        rdiagbuf(15,ii) = errinv_adjst         ! read_prepbufr inverse obs error (dBZ)**-1
        rdiagbuf(16,ii) = errinv_final         ! final inverse observation error (dBZ)**-1
        rdiagbuf(17,ii) = data(idbzob,i)       ! radar reflectivity observation (dBZ)
        rdiagbuf(18,ii) = ddiff                ! obs-ges (dBZ)
        rdiagbuf(19,ii) = data(idbzob,i)-rdBZ  ! obs-ges w/o bias correction (dBZ) (future slot)
        rdiagbuf(20,ii)=data(iazm,i)*rad2deg   ! azimuth angle
        rdiagbuf(21,ii)=data(itilt,i)*rad2deg  ! tilt angle
        rdiagbuf(22,ii)=data(irange,i) ! the range in km
        rdiagbuf(23,ii)=data(idmiss2opt,i) ! the range in km

        rdiagbuf(23,ii) = 1.e+10_r_single    ! ges ensemble spread (filled in EnKF)
        rdiagbuf(24,ii) = 1.e+10_r_single    ! ges ensemble spread (filled in EnKF)

        if (lobsdiagsave) then
            write(6,*)'wrong here, stop in setupdbz.f90 '
            stop
           ioff=nreal
           do jj=1,miter
              ioff=ioff+1
              if (odiag%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%obssen(jj)
           enddo
        endif

  end subroutine contents_binary_diag_
  subroutine contents_binary_dirZDA_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag

        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype

        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = presq              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (sec relative to analysis time)
        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use,-1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        rdiagbuf(13,ii) = rwgt                 ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input         ! prepbufr inverse obs error (dBZ)**-1
        rdiagbuf(15,ii) = errinv_adjst         ! read_prepbufr inverse obs error (dBZ)**-1
        rdiagbuf(16,ii) = errinv_final         ! final inverse observation error (dBZ)**-1
        rdiagbuf(17,ii) = data(idbzob,i)       ! radar reflectivity observation (dBZ)
        rdiagbuf(18,ii) = ddiff                ! obs-ges (dBZ)
        rdiagbuf(19,ii) = data(idbzob,i)-rdBZ  ! obs-ges w/o bias correction (dBZ) (future slot)
        rdiagbuf(20,ii)  = data(ilat,i)        ! index of grid relative obs location (y)
        rdiagbuf(21,ii)  = data(ilon,i)        ! index of grid relative obs location (x)
        rdiagbuf(22,ii) = rdBZr                ! dBZ from rain water
        rdiagbuf(23,ii) = rdBZs                ! dBZ from snow
        rdiagbuf(24,ii) = rdBZg                ! dBZ from graupel
        rdiagbuf(25,ii) = T1D                  ! temperature (sensible, K)
        rdiagbuf(26,ii) = rho                  ! air density (kg/m**3)


        ioff=ioff0
        if (lobsdiagsave) then
           write(6,*)'wrong here, stop in setupdbz.f90 '
           stop
           ioff=nreal
           do jj=1,miter
              ioff=ioff+1
              if (odiag%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%obssen(jj)
           enddo
        endif

  end subroutine contents_binary_dirZDA_diag_
  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '    dbz'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id          )
           call nc_diag_metadata("Observation_Class",       obsclass            )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)         )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)      )
           call nc_diag_metadata_to_single("Latitude",      data(ilate,i)       )
           call nc_diag_metadata_to_single("Longitude",     data(ilone,i)       )
           call nc_diag_metadata_to_single("Station_Elevation",data(ielev,i)    )
           call nc_diag_metadata_to_single("Pressure",      presw               )
           call nc_diag_metadata_to_single("Height",        data(ihgt,i)        )
           call nc_diag_metadata_to_single("Time",          dtime, time_offset, "-")
           call nc_diag_metadata_to_single("Prep_QC_Mark",  zero                )
           call nc_diag_metadata_to_single("Prep_Use_Flag", data(iuse,i)        )
!          call nc_diag_metadata("Nonlinear_QC_Var_Jb",     var_jb   !          )
           call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",rwgt          )
           if(muse(i)) then
              call nc_diag_metadata_to_single("Analysis_Use_Flag",one           )
           else
              call nc_diag_metadata_to_single("Analysis_Use_Flag",-one          )
           endif

           call nc_diag_metadata_to_single("Errinv_Input",  errinv_input        )
           call nc_diag_metadata_to_single("Errinv_Adjust", errinv_adjst        )
           call nc_diag_metadata_to_single("Errinv_Final",  errinv_final        )

           call nc_diag_metadata_to_single("Observation",   data(idbzob,i)      )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_adjusted",ddiff  )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_unadjusted", data(idbzob,i), rdBZ, "-")

           if (lobsdiagsave) then
              do jj=1,miter
                 if (odiag%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else
                       obsdiag_iuse(jj) = -one
                 endif
              enddo

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse              )
              call nc_diag_data2d("ObsDiagSave_nldepart", odiag%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", odiag%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen"  , odiag%obssen   )
           endif

  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
    if(allocated(ges_qr)) deallocate(ges_qr)
    if(allocated(ges_qs)) deallocate(ges_qs)
    if(allocated(ges_qg)) deallocate(ges_qg)
    if(allocated(ges_qli)) deallocate(ges_qli)
    if(allocated(ges_dbz)) deallocate(ges_dbz)
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_nr)) deallocate(ges_nr)
  end subroutine final_vars_

  subroutine calc_coeffs_dry_snow_tm(temp,a_out,b_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_coeffs_dry_snow_tm - calculate coefficients for dry snow
!                                          dependent on temperature 
! program history log:
!   2021-08-30  CAPS (J. Park) - Initial code; based on WRF v4.0
!                              - (module_mp_thompson.F)
!                              - added kind attribute and simpified
!   input argument list:
!     temp     - air temperature (K)
!
!   output argument list:
!     a_out - coefficient for gamma distribution of dry snow
!     b_out - coefficient for gamma distribution of dry snow
!
    implicit none

    !..For snow moments conversions (from Field et al. 2005)
    real(r_kind), intent(in   ) :: temp
    real(r_kind), intent(  out) :: a_out, b_out
    ! LOCAL
    real(r_kind) :: a_, b_, loga_, tc0
    real(r_kind), dimension(10), parameter:: &
    sa = (/ 5.065339_r_kind, -0.062659_r_kind, -3.032362_r_kind, &
            0.029469_r_kind, -0.000285_r_kind, &
            0.31255_r_kind,   0.000204_r_kind,  0.003199_r_kind, &
            0.0_r_kind,      -0.015952_r_kind/)
    real(r_kind), dimension(10), parameter:: &
    sb = (/ 0.476221_r_kind, -0.015896_r_kind,  0.165977_r_kind, &
            0.007468_r_kind, -0.000141_r_kind, &
            0.060366_r_kind,  0.000079_r_kind,  0.000594_r_kind, &
            0.0_r_kind,      -0.003577_r_kind/)
    real(r_kind), parameter:: mu_s = 0.6357_r_kind
    real(r_kind), parameter:: bm_s = 2.0_r_kind
    real(r_kind), parameter:: bv_s = 0.55_r_kind

    real(r_kind), dimension(18):: cse

    cse(1) = bm_s + 1._r_kind
    cse(2) = bm_s + 2._r_kind
    cse(3) = bm_s*2._r_kind
    cse(4) = bm_s + bv_s + 1._r_kind
    cse(5) = bm_s*2._r_kind + bv_s + 1._r_kind
    cse(6) = bm_s*2._r_kind + 1._r_kind
    cse(7) = bm_s + mu_s + 1._r_kind
    cse(8) = bm_s + mu_s + 2._r_kind
    cse(9) = bm_s + mu_s + 3._r_kind
    cse(10) = bm_s + mu_s + bv_s + 1._r_kind
    cse(11) = bm_s*2._r_kind + mu_s + bv_s + 1._r_kind
    cse(12) = bm_s*2._r_kind + mu_s + 1._r_kind
    cse(13) = bv_s + 2._r_kind
    cse(14) = bm_s + bv_s
    cse(15) = mu_s + 1._r_kind
    cse(16) = 1.0_r_kind + (1.0_r_kind + bv_s)/2._r_kind
    cse(17) = cse(16) + mu_s + 1._r_kind
    cse(18) = bv_s + mu_s + 3._r_kind

!..Sum of two gamma distrib for snow (Field et al. 2005).
    tc0 = MIN(-0.1_r_kind, temp-273.15_r_kind)

    loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(3) &
     &     + sa(4)*tc0*cse(3) + sa(5)*tc0*tc0 &
     &     + sa(6)*cse(3)*cse(3) + sa(7)*tc0*tc0*cse(3) &
     &     + sa(8)*tc0*cse(3)*cse(3) + sa(9)*tc0*tc0*tc0 &
     &     + sa(10)*cse(3)*cse(3)*cse(3)
    a_ = 10.0_r_kind**loga_
    b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(3) + sb(4)*tc0*cse(3) &
     &    + sb(5)*tc0*tc0 + sb(6)*cse(3)*cse(3) &
     &    + sb(7)*tc0*tc0*cse(3) + sb(8)*tc0*cse(3)*cse(3) &
     &    + sb(9)*tc0*tc0*tc0 + sb(10)*cse(3)*cse(3)*cse(3)

    a_out = a_
    b_out = b_
  end subroutine calc_coeffs_dry_snow_tm

  subroutine convert_qx2cvpqx(qx_arr,cvpqx_pvalue)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_qx2cvpqx   convert 'ges_qx' from qx to cvpqx space
!
! program history log:
!   2021-08-30  CAPS (J. Park) - Initial code
!
!   input argument list:
!     qx_arr       - ges_qx array (qx space)
!     cvpqx_pvalue - value for power transform
!
!   output argument list:
!     qx_arr       - ges_qx array (cvpqx space)
!
    implicit none

    real(r_kind), dimension(:,:,:,:), intent(inout) :: qx_arr
    real(r_kind),                     intent(in   ) :: cvpqx_pvalue
    integer(i_kind) :: i, j, k, l
    integer(i_kind) :: ni, nj, nk, nl
  
    ni=size(qx_arr,1)
    nj=size(qx_arr,2)
    nk=size(qx_arr,3)
    nl=size(qx_arr,4)

    do l=1,nl
       do k=1,nk
          do j=1,nj
             do i=1,ni
                qx_arr(i,j,k,l)=((qx_arr(i,j,k,l))**cvpqx_pvalue-1)/cvpqx_pvalue
             end do
          end do
       end do
    end do

  end subroutine convert_qx2cvpqx

  subroutine convert_cvpqx2qx(qx_arr,cvpqx_pvalue)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_cvpqx2qx - convert 'ges_qx' from cvpqx to qx space
!
! program history log:
!   2021-08-30  CAPS (J. Park) - Initial code
!
!   input argument list:
!     qx_arr       - ges_qx array (cvpqx space)
!     cvpqx_pvalue - value for power transform
!
!   output argument list:
!     qx_arr       - ges_qx array (qx space)
!
    implicit none

    real(r_kind), dimension(:,:,:,:), intent(inout) :: qx_arr
    real(r_kind),                     intent(in   ) :: cvpqx_pvalue
    integer(i_kind) :: i, j, k, l
    integer(i_kind) :: ni, nj, nk, nl
  
    ni=size(qx_arr,1)
    nj=size(qx_arr,2)
    nk=size(qx_arr,3)
    nl=size(qx_arr,4)

    do l=1,nl
       do k=1,nk
          do j=1,nj
             do i=1,ni
                qx_arr(i,j,k,l)=(cvpqx_pvalue*qx_arr(i,j,k,l)+1)**(1/cvpqx_pvalue)
             end do
          end do
       end do
    end do

  end subroutine convert_cvpqx2qx
end subroutine setupdbz
end module dbz_setup
