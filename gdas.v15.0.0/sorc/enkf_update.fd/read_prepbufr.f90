subroutine read_prepbufr(nread,ndata,nodata,infile,obstype,lunout,twindin,sis,&
     prsl_full,nobs,nrec_start)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_prepbuf                read obs from prepbufr file
!   prgmmr: parrish          org: np22                date: 1990-10-07
!
! abstract:  This routine reads conventional data found in the prepbufr
!            file.  Specific observation types read by this routine 
!            include surface pressure, temperature, winds (components
!            and speeds), moisture and total precipitable water.  
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   1990-10-07  parrish
!   1998-05-15  weiyu yang 
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-02-13  derber, j. - clean up and modify vertical weighting
!   2004-06-16  treadon - update documentation
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-07-30  derber  - generalize number of data records per obs type
!   2004-08-26  derber  - fix many errors in reading of sst data
!   2004-08-27  kleist  - modify pressure calculation
!   2004-10-28  kleist  - correct array index bug in hybrid pressure calculation
!   2004-11-16  treadon - deallocate(etabl) prior to exiting routine
!   2005-02-10  treadon - add call destroygrids for obstype = sst
!   2005-05-24  pondeca - add surface analysis option
!   2005-02-24  treadon - remove hardwired setting of metar ps obs error
!   2005-05-27  derber  - reduce t, uv, ps error limits
!   2005-05-27  kleist/derber - add option to read in new ob error table
!   2005-07-19  derber - clean up code a bit
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-08-02  derber - modify to use convinfo file
!   2005-09-08  derber - modify to use input group time window
!   2005-10-11  treadon - change convinfo read to free format
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-26  treadon - add routine tag to convinfo printout
!   2006-02-03  derber  - modify to count read/keep data and new obs control
!   2006-02-03  treadon - use interpolated guess 3d pressure field in errormod
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-09  treadon - save height for wind observations
!   2006-02-23  kistler - modify to add optional data thinning
!   2006-02-23  kistler - raob instument as subtype and solar elv angle computed
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-03  derber  - modify to properly handle height of surface obs
!   2006-04-05  wu - changes to read in GPS IPW (type 153)
!   2006-05-18  middlecoff/treadon - add huge_i_kind upper limit on nint
!   2006-05-29  treadon - increase nreal to pass more information to setup routines
!   2006-06-08  su - added the option to turn off oiqc
!   2006-06-21  wu - deallocate etabl array
!   2006-07-28  derber  - temporarily add subtype for meteosat winds based on sat ID
!   2006-07-31  kleist  - change to surface pressure ob error from ln(ps) to ps(cb)
!   2006-10-25  sienkiewicz - add blacklist of raob data
!   2006-12-01  todling - embedded blacklist into a module
!   2007-02-13  parrish - add ability to use obs files with ref time different from analysis time
!   2007-02-20  wu - correct errors in quality mark checks
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2007-03-15  su - remove the error table reading part to a subroutine
!   2007-04-24  wu - add TAMDAR (134) to be used as sensible T
!   2007-05-17  kleist - generalize flag for virtual/sensible temperature obs
!   2007-09-28  treadon - truncate/expand obs time to remove extraneous bits 
!   2007-10-03  su  -   Add reading qc mark from satellite wind
!   2007-10-24  Pondeca - add ability to use use_list on mesonet winds
!   2007-11-03  su  -   modify conventional thinning algorithm
!   2008-03-28  wu - add code to generate optional observation perturbations
!   2008-03-31  li.bi - add ascat
!   2008-05-27  safford - rm unused vars and uses
!   2008-06-02  treadon - check iret from inital readmg and act accordingly
!   2008-09-08  lueken  - merged ed's changges into q1fy09 code
!   2008-21-25  todling - adapted Tremolet 2007-03-01 change of time window
!                       - remove unused vars
!   2009-07-08  pondeca - add ability to convert virtual temperature
!                         obs into sensible temperature for 2dvar
!   2009-07-08  park,pondeca - add option to use the hilbert curve-based
!                              cross-validation for 2dvar
!   2009-07-08  pondeca - move handling of "provider use_list" for mesonet winds 
!                         to the new module sfcobsqc
!   2010-03-29  hu - add code to read cloud observation from METAR and NESDIS cloud products
!   2010-05-15  kokron - safety measure: initialize cdata_all to zero
!   2010-08-23  tong - add flg as an input argument of map3grids, so that the subroutine can be used for 
!                      thinning grid with either pressure or height as the vertical coordinate. 
!                      flg=-1 for prepbufr data thinning grid (pressure as the vertical coordinate). 
!   2010-09-08  parrish - remove subroutine check_rotate_wind.  This was a debug routine introduced when
!                           the reference wind rotation angle was stored as an angle, beta_ref.  This field
!                           had a discontinuity at the date line (180E), which resulted in erroneous wind
!                           rotation angles for a small number of winds whose rotation angle was interpolated
!                           from beta_ref values across the discontinuity.  This was fixed by replacing the
!                           beta_ref field with cos_beta_ref, sin_beta_ref.
!   2010-10-19  wu - add code to limit regional use of MAP winds with P less than 400 mb
!   2010-11-13  su - skip satellite winds from prepbufr 
!   2010-11-18  treadon - add check for small POB (if POB<tiny_r_kind then POB=bmiss)
!   2011-02-14  zhu - add gust and visibility
!   2011-07-13  wu     - not use mesonet Psfc when 8th character of sid is "x"
!   2011-08-01  lueken  - added module use deter_sfc_mod and fixed indentation
!   2011-08-27  todling - add use_prepb_satwnd; cleaned out somebody's left over's
!   2011-11-14  wu     - pass CAT to setup routines for raob level enhancement
!   2012-04-03  s.liu    - thin new VAD wind 
!   2012-11-12  s.liu    - identify new VAD wind by vertical resolution 
!   2012-08-29  akella  - extend nst_gsi option to handle sstobs
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-01-26  parrish - WCOSS debug compile error for pflag used before initialized.
!                                    Initialize pflag=0 at beginning of subroutine.
!   2013-02-28  sienkiewicz - put in subset via SAID for kx=290 ASCAT to allow
!                        separate control of metop-a and metop-b ASCAT if 
!                        needed
!   2013-05-03  sienkiewicz - if ACARS SID == 'ACARS' take ID from ACID instead
!   2013-05-15  zhu  - add phase of aircraft flight and vertical velocity for aircraft data
!                    - match aircraft obs with temperature bias file 
!                    - add new tail number info if there is any
!                    - add aircraft_t_bc_pof and aircraft_t_bc
!   2013-05-28  wu     - add subroutine sonde_ext and call to the subroutine for ext_sonde option
!   2013-06-07  zhu  - read aircraft data from prepbufr_profl when aircraft_t_bc=.true.
!   2013-09-08  s.liu  - increase nmsgmax to 100000 to read NESDIS cloud product
!   2013-12-08  s.liu  - identify VAD wind based on sub type
!   2014-02-28  sienkiewicz - added code for option aircraft_t_bc_ext for external aircraft bias table
!   2014-03-19  pondeca - add 10m wind speed
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-04-15  Su      - read errtable and non linear qc b table
!   2014-05-07  pondeca - add significant wave height (howv)
!   2014-06-16  carley/zhu - add tcamt and lcbas
!   2014-06-26  carley - simplify call to apply_hilbertcurve 
!   2014-11-20  zhu  - added code for aircraft temperature kx=130
!   2014-10-01  Xue    - add gsd surface observation uselist
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-03-23  Su      -fix array size with maximum message and subset  number from fixed number to
!                        dynamic allocated array
!   2015-07-10  pondeca - add cloud ceiling height (cldch)
!   2016-02-10  s.liu  - thin new VAD wind in time level
!   2016-03-15  Su      - modified the code so that the program won't stop when no subtype
!                         is found in non linear qc error tables and b table
!

!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     prsl_full- 3d pressure on full domain grid
!     nrec_start - number of subsets without useful information
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     nodata   - number of individual "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     twindin  - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,r_kind,r_double,i_kind
  use constants, only: zero,one_tenth,one,deg2rad,fv,t0c,half,&
      three,four,rad2deg,tiny_r_kind,huge_r_kind,huge_i_kind,&
      r60inv,r10,r100,r2000
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
      tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
      rlats,rlons,twodvar_regional
  use convinfo, only: nconvtype,ctwind, &
      ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype, &
      ithin_conv,rmesh_conv,pmesh_conv, &
      id_bias_ps,id_bias_t,conv_bias_ps,conv_bias_t,use_prepb_satwnd

  use obsmod, only: iadate,oberrflg,perturb_obs,perturb_fact,ran01dom,hilbert_curve
  use obsmod, only: blacklst,offtime_data,bmiss,ext_sonde
  use aircraftinfo, only: aircraft_t_bc,aircraft_t_bc_pof,ntail,taillist,idx_tail,npredt,predt, &
      aircraft_t_bc_ext,ntail_update,max_tail,nsort,itail_sort,idx_sort,timelist
  use converr,only: etabl
  use converr_ps,only: etabl_ps,isuble_ps,maxsub_ps
  use converr_q,only: etabl_q,isuble_q,maxsub_q
  use converr_t,only: etabl_t,isuble_t,maxsub_t
  use converr_uv,only: etabl_uv,isuble_uv,maxsub_uv
  use converr_pw,only: etabl_pw,isuble_pw,maxsub_pw
  use convb_ps,only: btabl_ps
  use convb_q,only: btabl_q
  use convb_t,only: btabl_t
  use convb_uv,only: btabl_uv
  use gsi_4dvar, only: l4dvar,l4densvar,time_4dvar,winlen,thin4d
  use qcmod, only: errormod,noiqc,newvad,njqc,vqc
  use convthin, only: make3grids,map3grids,del3grids,use_all
  use blacklist, only : blacklist_read,blacklist_destroy
  use blacklist, only : blkstns,blkkx,ibcnt
  use sfcobsqc,only: init_rjlists,get_usagerj,get_gustqm,destroy_rjlists
  use sfcobsqc,only: init_gsd_sfcuselist,apply_gsd_sfcuselist,destroy_gsd_sfcuselist                       
  use hilbertcurve,only: init_hilbertcurve, accum_hilbertcurve, &
                         apply_hilbertcurve,destroy_hilbertcurve
  use ndfdgrids,only: init_ndfdgrid,destroy_ndfdgrid,relocsfcob,adjust_error
  use jfunc, only: tsensible
  use deter_sfc_mod, only: deter_sfc_type,deter_sfc2
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_deter
  use aircraftobsqc, only: init_aircraft_rjlists,get_aircraft_usagerj,&
                           destroy_aircraft_rjlists
  use adjust_cloudobs_mod, only: adjust_convcldobs,adjust_goescldobs
  use mpimod, only: npe
  use rapidrefresh_cldsurf_mod, only: i_gsdsfc_uselist

  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=20)                     ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout,nrec_start
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe)        ,intent(inout) :: nobs
  real(r_kind)                          ,intent(in   ) :: twindin
  real(r_kind),dimension(nlat,nlon,nsig),intent(in   ) :: prsl_full

! Declare local parameters
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r0_75 = 0.75_r_kind
  real(r_kind),parameter:: r0_7 = 0.7_r_kind
  real(r_kind),parameter:: r1_2 = 1.2_r_kind
  real(r_kind),parameter:: r3_33= three + one/three
  real(r_kind),parameter:: r6   = 6.0_r_kind
  real(r_kind),parameter:: r20  = 20.0_r_kind
  real(r_kind),parameter:: r50  = 50.0_r_kind
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r500 = 500.0_r_kind
  real(r_kind),parameter:: r999 = 999.0_r_kind
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: convert= 1.0e-6_r_kind
  real(r_kind),parameter:: emerr= 0.2_r_kind
  real(r_kind),parameter:: r0_1_bmiss=one_tenth*bmiss
  real(r_kind),parameter:: r0_01_bmiss=r0_01*bmiss
  character(80),parameter:: cspval= '88888888'

!  integer(i_kind),parameter:: mxtb=5000000
!  integer(i_kind),parameter:: nmsgmax=100000 ! max message count

! Declare local variables
  logical tob,qob,uvob,spdob,sstob,pwob,psob,gustob,visob,tdob,mxtmob,mitmob,pmob,howvob,cldchob
  logical metarcldobs,goesctpobs,tcamtob,lcbasob
  logical outside,driftl,convobs,inflate_error
  logical sfctype
  logical luse,ithinp,windcorr
  logical patch_fog
  logical aircraftset,aircraftobst,aircrafttype
  logical acft_profl_file
  logical,allocatable,dimension(:,:):: lmsg           ! set true when convinfo entry id found in a message

  character(40) drift,hdstr,qcstr,oestr,sststr,satqcstr,levstr,hdstr2
  character(40) metarcldstr,goescldstr,metarvisstr,metarwthstr,cldseqstr,cld2seqstr,cldceilhstr
  character(40) maxtmintstr,owavestr
  character(80) obstr
  character(10) date
  character(8) subset
  character(8) prvstr,sprvstr     
  character(8) c_prvstg,c_sprvstg 
  character(8) c_station_id
  character(8) cc_station_id
  character(1) sidchr(8)
  character(8) stnid
  character(10) aircraftstr
  character(1) cb
  logical lhilbert

  integer(i_kind) ireadmg,ireadsb,icntpnt,icntpnt2,icount,iiout
  integer(i_kind) lunin,i,maxobs,j,idomsfc,it29,nmsgmax,mxtb
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) nc,nx,isflg,ntread,itx,ii,ncsave
  integer(i_kind) ihh,idd,idate,iret,im,iy,k,levs
  integer(i_kind) metarcldlevs,metarwthlevs,cldseqlevs,cld2seqlevs
  integer(i_kind) kx,kx0,nreal,nchanl,ilat,ilon,ithin
  integer(i_kind) cat,zqm,pwq,sstq,qm,lim_qm,lim_zqm,gustqm,visqm,tdqm,mxtmqm,mitmqm,howvqm,cldchqm
  integer(i_kind) lim_tqm,lim_qqm
  integer(i_kind) nlevp         ! vertical level for thinning
  integer(i_kind) ntmp,iout
  integer(i_kind) pflag,irec
  integer(i_kind) ntest,nvtest,iosub,ixsub,isubsub,iobsub
  integer(i_kind) kl,k1,k2,k1_ps,k1_q,k1_t,k1_uv,k1_pw,k2_q,k2_t,k2_uv,k2_pw,k2_ps
  integer(i_kind) itypex,itypey
  integer(i_kind) minobs,minan
  integer(i_kind) ntb,ntmatch,ncx
  integer(i_kind) nmsg                ! message index
  integer(i_kind) idx                 ! order index of aircraft temperature bias
  integer(i_kind) tcamt_qc,lcbas_qc
  integer(i_kind) low_cldamt_qc,mid_cldamt_qc,hig_cldamt_qc
  integer(i_kind) iyyyymm
  integer(i_kind) jj,start,next,ncount_ps,ncount_q,ncount_uv,ncount_t,ncount_pw
  integer(i_kind),dimension(5):: idate5
  integer(i_kind),dimension(255):: pqm,qqm,tqm,wqm,pmq
  integer(i_kind),dimension(nconvtype)::ntxall
  integer(i_kind),dimension(nconvtype+1)::ntx
  integer(i_kind),allocatable,dimension(:):: isort,iloc,nrep
  integer(i_kind),allocatable,dimension(:,:):: tab
  integer(i_kind) ibfms,thisobtype_usage
  integer(i_kind) ierr_ps,ierr_q,ierr_t,ierr_uv,ierr_pw !  the position of error table collum
  real(r_kind) time,timex,time_drift,timeobs,toff,t4dv,zeps
  real(r_kind) qtflg,tdry,rmesh,ediff,usage,ediff_ps,ediff_q,ediff_t,ediff_uv,ediff_pw
  real(r_kind) u0,v0,uob,vob,dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) qoe,qobcon,pwoe,pwmerr,dlnpob,ppb,poe,gustoe,visoe,qmaxerr
  real(r_kind) toe,woe,errout,oelev,dlat,dlon,sstoe,dlat_earth,dlon_earth
  real(r_kind) tdoe,mxtmoe,mitmoe,pmoe,howvoe,cldchoe
  real(r_kind) selev,elev,stnelev
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterrmax,u00,v00
  real(r_kind) del,terrmin,werrmin,perrmin,qerrmin,pwerrmin,del_ps,del_q,del_t,del_uv,del_pw
  real(r_kind) pjbmin,qjbmin,tjbmin,wjbmin
  real(r_kind) tsavg,ff10,sfcr,zz
  real(r_kind) crit1,timedif,xmesh,pmesh
  real(r_kind) time_correction
  real(r_kind) tcamt,lcbas,ceiling
  real(r_kind) tcamt_oe,lcbas_oe
  real(r_kind) low_cldamt,mid_cldamt,hig_cldamt
  real(r_kind),dimension(nsig):: presl
  real(r_kind),dimension(nsig-1):: dpres
  real(r_kind),dimension(255)::plevs
  real(r_kind),dimension(255):: tvflg
  real(r_kind),allocatable,dimension(:):: presl_thin
  real(r_kind),allocatable,dimension(:,:):: cdata_all,cdata_out
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr

  real(r_double) rstation_id,qcmark_huge
  real(r_double) vtcd
  real(r_double),dimension(8):: hdr,hdrtsb
  real(r_double),dimension(3,255):: hdr3
  real(r_double),dimension(8,255):: drfdat,qcmark,obserr,var_jb
  real(r_double),dimension(13,255):: obsdat
  real(r_double),dimension(8,1):: sstdat
  real(r_double),dimension(2,1):: cld2seq
  real(r_double),dimension(3,10):: cldseq
  real(r_double),dimension(2,10):: metarcld
  real(r_double),dimension(1,10):: metarwth
  real(r_double),dimension(2,1) :: metarvis
  real(r_double),dimension(4,1) :: goescld
  real(r_double),dimension(2,255):: maxtmint
  real(r_double),dimension(1,255):: owave
  real(r_double),dimension(1,255):: cldceilh
  real(r_double),dimension(1):: satqc
  real(r_double),dimension(1,1):: r_prvstg,r_sprvstg 
  real(r_double),dimension(1,255):: levdat
  real(r_double),dimension(255,20):: tpc
  real(r_double),dimension(2,255,20):: tobaux
  real(r_double),dimension(2,255):: aircraftwk

!  equivalence to handle character names
  equivalence(r_prvstg(1,1),c_prvstg) 
  equivalence(r_sprvstg(1,1),c_sprvstg) 
  equivalence(rstation_id,c_station_id)
  equivalence(rstation_id,sidchr)

!  data statements
  data hdstr  /'SID XOB YOB DHR TYP ELV SAID T29'/
  data hdstr2 /'TYP SAID T29 SID'/
  data obstr  /'POB QOB TOB ZOB UOB VOB PWO MXGS HOVI CAT PRSS TDO PMO' /
  data drift  /'XDR YDR HRDR                    '/
  data sststr /'MSST DBSS SST1 SSTQM SSTOE           '/
  data qcstr  /'PQM QQM TQM ZQM WQM NUL PWQ PMQ'/
  data oestr  /'POE QOE TOE NUL WOE NUL PWE     '/
! data satqcstr  /'RFFL QIFY QIFN EEQF'/
  data satqcstr  /'QIFN'/
  data prvstr /'PRVSTG'/   
  data sprvstr /'SPRVSTG'/ 
  data levstr  /'POB'/
  data cld2seqstr /'TOCC HBLCS'/      ! total cloud cover and height above surface of base of lowest cloud seen
  data cldseqstr /'VSSO CLAM HOCB'/   ! vertical significance, cloud amount and cloud base height
  data metarcldstr /'CLAM HOCB'/      ! cloud amount and cloud base height
  data metarwthstr /'PRWE'/           ! present weather
  data metarvisstr /'HOVI TDO'/       ! visibility and dew point
  data goescldstr /'CDTP TOCC GCDTT CDTP_QM'/   ! NESDIS cloud products: cloud top pressure, total cloud amount,
                                                !   cloud top temperature, cloud top temp. qc mark
  data aircraftstr /'POAF IALR'/      ! phase of aircraft flight and vertical velocity
  data maxtmintstr  /'MXTM MITM'/
  data owavestr  /'HOWV'/
  data cldceilhstr /'CEILING'/

  data lunin / 13 /
  data ithin / -9 /
  data rmesh / -99.999_r_kind /
  !* test new vad wind
  !* for match loction station and time
!       character(7*2000) cstn_idtime,cstn_idtime2
!       character(7) stn_idtime(2000),stn_idtime2(2000)
!       equivalence (stn_idtime(1),cstn_idtime)
!       equivalence (stn_idtime2(1),cstn_idtime2)
!       integer :: ii1,atmp,btmp,mytimeyy,ibyte
!       character(4) stid
!       real(8) :: rval
!       character(len=8) :: cval
!       equivalence (rval,cval)
!       character(7) flnm
        integer:: icase,klev,ikkk,tkk
        real:: diffhgt,diffuu,diffvv

  real(r_double),dimension(3,1500):: fcstdat
  
! File type
  acft_profl_file = index(infile,'_profl')/=0

! Initialize variables

  vdisterrmax=zero
  pflag=0                  !  dparrish debug compile run flags pflag as not defined ???????????
  nreal=0
  satqc=zero
  tob = obstype == 't'
  uvob = obstype == 'uv'  ; if (twodvar_regional) uvob = uvob .or. obstype == 'wspd10m'
  spdob = obstype == 'spd'
  psob = obstype == 'ps'
  qob = obstype == 'q'
  pwob = obstype == 'pw'
  sstob = obstype == 'sst'
  gustob = obstype == 'gust'
  visob = obstype == 'vis'
  tdob = obstype == 'td2m'
  mxtmob = obstype == 'mxtm'
  mitmob = obstype == 'mitm'
  pmob = obstype == 'pmsl'
  howvob = obstype == 'howv'
  metarcldobs = obstype == 'mta_cld'
  goesctpobs = obstype == 'gos_ctp'
  tcamtob = obstype == 'tcamt'
  lcbasob = obstype == 'lcbas'
  cldchob = obstype == 'cldch'
  newvad=.false.
  convobs = tob .or. uvob .or. spdob .or. qob .or. gustob .or. &
            tdob .or. mxtmob .or. mitmob .or. pmob .or. howvob .or. &
            tcamtob .or. lcbasob .or. cldchob
  aircraftobst=.false.
  if(tob)then
     nreal=25
  else if(uvob) then 
     nreal=25
  else if(spdob) then
     nreal=24
  else if(psob) then
     nreal=20
  else if(qob) then
     nreal=26
  else if(pwob) then
     nreal=20
  else if(sstob) then
     if (nst_gsi > 0) then
        nreal=18 + nstinfo
     else
        nreal=18
     end if
  else if(gustob) then
     nreal=21
  else if(visob) then
     nreal=18
  else if(tdob) then
     nreal=25
  else if(mxtmob) then
     nreal=24
  else if(mitmob) then
     nreal=24
  else if(pmob) then
     nreal=24
  else if(howvob) then
     nreal=23
  else if(metarcldobs) then
     nreal=25
  else if(goesctpobs) then
     nreal=8
  else if(tcamtob) then
     nreal=20
  else if(lcbasob) then
     nreal=23
  else if(cldchob) then
     nreal=18
  else 
     write(6,*) ' illegal obs type in READ_PREPBUFR ',obstype
     call stop2(94)
  end if

!  Set qc limits based on noiqc flag
  if (noiqc) then
     lim_qm=8
     if (psob)         lim_zqm=7
     if (qob.or.tdob)  lim_tqm=7
     if (tob)          lim_qqm=8
  else
     lim_qm=4
     if (psob)         lim_zqm=4
     if (qob.or.tdob)  lim_tqm=4
     if (tob)          lim_qqm=4
  endif

  if (tob .and. (aircraft_t_bc_pof .or. aircraft_t_bc .or.&
       aircraft_t_bc_ext )) nreal=nreal+3
  if(perturb_obs .and. (tob .or. psob .or. qob))nreal=nreal+1
  if(perturb_obs .and. uvob )nreal=nreal+2

  qcmark_huge = huge_i_kind

  lhilbert = twodvar_regional .and. hilbert_curve

  if (blacklst) call blacklist_read(obstype)

  terrmin=half
  werrmin=one
  perrmin=0.3_r_kind
  qerrmin=0.05_r_kind
  pwerrmin=one
  tjbmin=zero
  qjbmin=zero
  wjbmin=zero
  pjbmin=zero
!------------------------------------------------------------------------
  ntread=1
  ntmatch=0
  ntx(ntread)=0
  ntxall=0
  var_jb=zero
  do nc=1,nconvtype
     if(trim(ioctype(nc)) == trim(obstype))then
       if(.not.use_prepb_satwnd .and. (trim(ioctype(nc)) == 'uv' .or. trim(ioctype(nc)) == 'wspd10m') .and. ictype(nc) >=241 &
          .and. ictype(nc) <260) then 
          cycle
       else
          if (aircraft_t_bc) then
             aircrafttype=(ictype(nc) == 130 .or. ictype(nc) == 131 .or. (ictype(nc) >= 133 .and. ictype(nc)<140) .or. &
                           ictype(nc) == 230 .or. ictype(nc) == 231 .or. (ictype(nc) >= 233 .and. ictype(nc)<240))
             if (.not. acft_profl_file .and. aircrafttype) cycle    ! skip aircrafttype for prepbufr
             if (acft_profl_file .and. (.not. aircrafttype)) cycle  ! skip non-aircrafttype for prepbufr_profl
          end if
          ntmatch=ntmatch+1
          ntxall(ntmatch)=nc
       endif
     end if
     if(trim(ioctype(nc)) == trim(obstype) .and. abs(icuse(nc)) <= 1)then
        if(.not.use_prepb_satwnd .and. (trim(ioctype(nc)) == 'uv' .or. trim(ioctype(nc)) == 'wspd10m') .and. ictype(nc) >=241 &
            .and. ictype(nc) <260) then
            cycle
        else
           if (aircraft_t_bc) then
             aircrafttype=(ictype(nc) == 130 .or. ictype(nc) == 131 .or. (ictype(nc) >= 133 .and. ictype(nc)<140).or. &
                           ictype(nc) == 230 .or. ictype(nc) == 231 .or. (ictype(nc) >= 233 .and. ictype(nc)<240))
              if (.not. acft_profl_file .and. aircrafttype) cycle    ! skip aircrafttype for prepbufr
              if (acft_profl_file .and. (.not. aircrafttype)) cycle  ! skip non-aircrafttype for prepbufr_profl
           end if
           ithin=ithin_conv(nc)
           if(ithin > 0)then
              ntread=ntread+1
              ntx(ntread)=nc
           end if
        endif
     end if
  end do
  if(ntmatch == 0)then
     write(6,*) ' no matching obstype found in obsinfo ',obstype
     return
  end if

!! get message and subset counts

  call getcount_bufr(infile,nmsgmax,mxtb)
  allocate(lmsg(nmsgmax,ntread),tab(mxtb,3),nrep(nmsgmax))

  lmsg = .false.
  maxobs=0
  tab=0
  nmsg=0
  nrep=0
  ntb = 0

  irec = 0
  ncount_ps=0;ncount_q=0;ncount_t=0;ncount_uv=0;ncount_pw=0

! Open, then read date from bufr data
  call closbf(lunin)
  open(lunin,file=trim(infile),form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)

  msg_report: do while (ireadmg(lunin,subset,idate) == 0)
     irec = irec + 1
     if(irec < nrec_start) cycle msg_report
     if(.not.use_prepb_satwnd .and. trim(subset) == 'SATWND') cycle msg_report
     if (aircraft_t_bc) then
        aircraftset = trim(subset)=='AIRCFT' .or. trim(subset)=='AIRCAR'
        if (.not. acft_profl_file .and. aircraftset) cycle msg_report
        if (acft_profl_file .and. (.not. aircraftset)) cycle msg_report
     end if

!    Time offset
     if(nmsg == 0) call time_4dvar(idate,toff)
     nmsg=nmsg+1
     if (nmsg>nmsgmax) then
        write(6,*)'READ_PREPBUFR: messages exceed maximum ',nmsgmax
        call stop2(50)
     endif
     loop_report: do while (ireadsb(lunin) == 0)
        ntb = ntb+1
        nrep(nmsg)=nrep(nmsg)+1
        if (ntb>mxtb) then
           write(6,*)'READ_PREPBUFR: reports exceed maximum ',mxtb
           call stop2(50)
        endif

!       Extract type information
        call ufbint(lunin,hdr,4,1,iret,hdstr2)
        kx=hdr(1)
        if (aircraft_t_bc .and. acft_profl_file) then
           kx0=kx
           if (.not. uvob) then
              if (kx0==330 .or. kx0==430 .or. kx0==530) kx=130
              if (kx0==331 .or. kx0==431 .or. kx0==531) kx=131
              if (kx0==332 .or. kx0==432 .or. kx0==532) kx=132
              if (kx0==333 .or. kx0==433 .or. kx0==533) kx=133
              if (kx0==334 .or. kx0==434 .or. kx0==534) kx=134
              if (kx0==335 .or. kx0==435 .or. kx0==535) kx=135
           else
              if (kx0==330 .or. kx0==430 .or. kx0==530) kx=230
              if (kx0==331 .or. kx0==431 .or. kx0==531) kx=231
              if (kx0==332 .or. kx0==432 .or. kx0==532) kx=232
              if (kx0==333 .or. kx0==433 .or. kx0==533) kx=233
              if (kx0==334 .or. kx0==434 .or. kx0==534) kx=234
              if (kx0==335 .or. kx0==435 .or. kx0==535) kx=235
           end if
        end if
        !* for new vad wind
        if(kx==224 .and. .not.newvad) then
           call ufbint(lunin,hdrtsb,1,1,iret,'TSB')
            if(hdrtsb(1)==2) then
            newvad=.true.
            go to 288
           end if
           call ufbint(lunin,obsdat,13,255,levs,obstr)
           if(levs>1)then
           do k=1, levs-1
             diffuu=abs(obsdat(4,k+1)-obsdat(4,k))
             if(diffuu==50.0) then
                   newvad=.true.
                   go to 288
             end if
           end do
           end if
288     continue
        end if
        !* END new vad wind

        if(twodvar_regional)then
!          If running in 2d-var (surface analysis) mode, check to see if observation
!          is surface type or GOES cloud product(kx=151).  If not, read next observation report from bufr file
           sfctype=(kx>179.and.kx<190).or.(kx>=280.and.kx<=290).or. &
                   (kx>=192.and.kx<=199).or.(kx>=292.and.kx<=299) .or. &
                   (kx==151)
           if (.not.sfctype ) cycle loop_report

        end if

! temporary specify iobsub until put in bufr file
        iobsub = 0                                                  
        if(kx == 280 .or. kx == 180 ) iobsub=hdr(3)                                            
        if(kx == 280 .or. kx ==180) then
          if ( hdr(3) >555.0_r_kind .and. hdr(3) <565.0_r_kind ) then
            iobsub=00
          else
            iobsub=01
          endif
        endif
! Su suggested to keep both 289 and 290.  But trunk only keep 290
! ???       if(kx == 289 .or. kx == 290) iobsub=hdr(2)

        if(kx == 290) iobsub=hdr(2)
        if(use_prepb_satwnd .and. (kx >= 240 .and. kx <=260 )) iobsub = hdr(2)

!       For the satellite wind to get quality information and check if it will be used
        if(use_prepb_satwnd .and. (kx == 243 .or. kx == 253 .or. kx ==254) ) then
           call ufbint(lunin,satqc,1,1,iret,satqcstr)
           if(satqc(1) <  85.0_r_double) cycle loop_report   ! QI w/o fcst (su's setup
!!         if(satqc(2) <= 80.0_r_double) cycle loop_report   ! QI w/ fcst (old prepdata)
        endif

!       Check for blacklisting of station ID
        if (blacklst .and. ibcnt > 0) then
           stnid = transfer(hdr(4),stnid)
           do i = 1,ibcnt
              if( kx == blkkx(i) .and. stnid == blkstns(i) ) then
                 write(6,*)'READ_PREPBUFR: blacklist station ',stnid, &
                    'for obstype ',trim(obstype),' and kx=',kx
                 cycle loop_report
              endif
           enddo
        endif

!  Match ob to proper convinfo type
        ncsave=0
        matchloop:do ncx=1,ntmatch
           nc=ntxall(ncx)
           if (kx /= ictype(nc))cycle 

!  Find convtype which match ob type and subtype
           if(icsubtype(nc) == iobsub) then
              ncsave=nc
              exit matchloop
           else
!  Find convtype which match ob type and subtype group (isubtype == ?*)
!       where ? specifies the group and icsubtype = ?0)
              ixsub=icsubtype(nc)/10
              iosub=iobsub/10
              isubsub=icsubtype(nc)-ixsub*10
              if(ixsub == iosub .and. isubsub == 0) then
                 ncsave=nc
!  Find convtype which match ob type and subtype is all remaining 
!       (icsubtype(nc) = 0)
              else if (ncsave == 0 .and. icsubtype(nc) == 0) then
                 ncsave=nc
              end if
           end if
        end do matchloop

!  Save information for next read
        if(ncsave /= 0) then

           call ufbint(lunin,levdat,1,255,levs,levstr)
           maxobs=maxobs+max(1,levs)
           nx=1
           if(ithin_conv(ncsave) > 0)then
              do ii=2,ntread
                 if(ntx(ii) == ncsave)nx=ii
              end do
           end if
           tab(ntb,1)=ncsave
           tab(ntb,2)=nx
           tab(ntb,3)=levs
           lmsg(nmsg,nx) = .true.
        end if

     end do loop_report
  enddo msg_report
  if (nmsg==0) goto 900
  write(6,*)'READ_PREPBUFR: messages/reports = ',nmsg,'/',ntb,' ntread = ',ntread



  if(tob) write(6,*)'READ_PREPBUFR: time offset is ',toff,' hours.'
!------------------------------------------------------------------------

! Obtain program code (VTCD) associated with "VIRTMP" step
  call ufbqcd(lunin,'VIRTMP',vtcd)

  call init_rjlists
  call init_aircraft_rjlists
  if(i_gsdsfc_uselist==1) call init_gsd_sfcuselist

  if (lhilbert) call init_hilbertcurve(maxobs)

  if (twodvar_regional) call init_ndfdgrid 

! loop over convinfo file entries; operate on matches
  
  allocate(cdata_all(nreal,maxobs),isort(maxobs))
  isort = 0
  cdata_all=zero
  nread=0
  ntest=0
  nvtest=0
  nchanl=0
  ilon=2
  ilat=3
  loop_convinfo: do nx=1, ntread

     use_all = .true.
     ithin=0
     if(nx > 1) then
        nc=ntx(nx)
        ithin=ithin_conv(nc)
        if (ithin > 0 ) then
           rmesh=rmesh_conv(nc)
           pmesh=pmesh_conv(nc)
           use_all = .false.
           if(pmesh > zero) then
              pflag=1
              nlevp=r1200/pmesh
           else
              pflag=0
              nlevp=nsig
           endif
           xmesh=rmesh

           call make3grids(xmesh,nlevp)

           if (.not.use_all) then
              allocate(presl_thin(nlevp))
              if (pflag==1) then
                 do k=1,nlevp
                    presl_thin(k)=(r1200-(k-1)*pmesh)*one_tenth
                 enddo
              endif
           endif
     
           write(6,*)'READ_PREPBUFR: at line 779: obstype,ictype(nc),rmesh,pflag,nlevp,pmesh=',&
              trim(ioctype(nc)),ictype(nc),rmesh,pflag,nlevp,pmesh
        endif
     endif
       

     call closbf(lunin)
     write(6,*)'new vad flag::', newvad 
     open(lunin,file=infile,form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)

!    Big loop over prepbufr file	

     ntb = 0
     nmsg = 0
     icntpnt=0
     icntpnt2=0
     disterrmax=-9999.0_r_kind
     irec = 0
     loop_msg: do while (ireadmg(lunin,subset,idate)== 0)
        irec = irec + 1
        if(irec < nrec_start) cycle loop_msg
        if(.not.use_prepb_satwnd .and. trim(subset) =='SATWND') cycle loop_msg
        if (aircraft_t_bc) then
           aircraftset = trim(subset)=='AIRCFT' .or. trim(subset)=='AIRCAR'
           if (.not. acft_profl_file .and. aircraftset) cycle loop_msg
           if (acft_profl_file .and. (.not. aircraftset)) cycle loop_msg
        end if

        nmsg = nmsg+1
        if(.not.lmsg(nmsg,nx)) then
           do i=ntb+1,ntb+nrep(nmsg)
              icntpnt2=icntpnt2+tab(i,3)
           end do
           ntb=ntb+nrep(nmsg)
           cycle loop_msg ! no useable reports this mesage, skip ahead report count
        end if 

        loop_readsb: do while(ireadsb(lunin) == 0)
!          use msg lookup table to decide which messages to skip
!          use report id lookup table to only process matching reports
           ntb = ntb+1
           if(icntpnt < icntpnt2)icntpnt=icntpnt2
           icntpnt2=icntpnt2+tab(ntb,3)
           nc=tab(ntb,1)
           if(nc <= 0 .or. tab(ntb,2) /= nx) cycle loop_readsb
                 
!          Extract type, date, and location information
           call ufbint(lunin,hdr,8,1,iret,hdstr)
           kx=hdr(5)

           if (.not.(aircraft_t_bc .and. acft_profl_file)) then
              if(abs(hdr(3))>r90 .or. abs(hdr(2))>r360) cycle loop_readsb
              if(hdr(2)== r360)hdr(2)=hdr(2)-r360
              if(hdr(2) < zero)hdr(2)=hdr(2)+r360
              dlon_earth=hdr(2)*deg2rad
              dlat_earth=hdr(3)*deg2rad

!             check VAD subtype. 1--old, 2--new, other--old 
              if(kx==224) then
                call ufbint(lunin,hdrtsb,1,1,iret,'TSB')
                if(.not.newvad .and. hdrtsb(1)==2) cycle loop_readsb
                if(newvad .and. hdrtsb(1)/=2) cycle loop_readsb
              end if
              !* thin new VAD in time level
              if(kx==224.and.newvad)then
                icase=0
                if(abs(hdr(4))>0.17_r_kind.and.abs(hdr(4))<0.32_r_kind) icase=1
                if(abs(hdr(4))>0.67_r_kind.and.abs(hdr(4))<0.82_r_kind) icase=1
                if(abs(hdr(4))>1.17_r_kind.and.abs(hdr(4))<1.32_r_kind) icase=1
                if(abs(hdr(4))>1.67_r_kind.and.abs(hdr(4))<1.82_r_kind) icase=1
                if(abs(hdr(4))>2.17_r_kind.and.abs(hdr(4))<2.62_r_kind) icase=1
                if(abs(hdr(4))>2.67_r_kind.and.abs(hdr(4))<2.82_r_kind) icase=1
                if(icase/=1) cycle
              end if

              if(regional)then
                 call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
                 if(diagnostic_reg) then
                    call txy2ll(dlon,dlat,rlon00,rlat00)
                    ntest=ntest+1
                    cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                         (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
                    cdist=max(-one,min(cdist,one))
                    disterr=acos(cdist)*rad2deg
                    disterrmax=max(disterrmax,disterr)
                 end if
                 if(outside) cycle loop_readsb   ! check to see if outside regional domain
              else
                 dlat = dlat_earth
                 dlon = dlon_earth
                 call grdcrd1(dlat,rlats,nlat,1)
                 call grdcrd1(dlon,rlons,nlon,1)
              endif
           else
              call ufbint(lunin,hdr3,3,255,levs,'XDR YDR HRDR')
              kx0=kx
              if (.not. uvob) then
                 if (kx0==330 .or. kx0==430 .or. kx0==530) kx=130
                 if (kx0==331 .or. kx0==431 .or. kx0==531) kx=131
                 if (kx0==332 .or. kx0==432 .or. kx0==532) kx=132
                 if (kx0==333 .or. kx0==433 .or. kx0==533) kx=133
                 if (kx0==334 .or. kx0==434 .or. kx0==534) kx=134
                 if (kx0==335 .or. kx0==435 .or. kx0==535) kx=135
              else
                 if (kx0==330 .or. kx0==430 .or. kx0==530) kx=230
                 if (kx0==331 .or. kx0==431 .or. kx0==531) kx=231
                 if (kx0==332 .or. kx0==432 .or. kx0==532) kx=232
                 if (kx0==333 .or. kx0==433 .or. kx0==533) kx=233
                 if (kx0==334 .or. kx0==434 .or. kx0==534) kx=234
                 if (kx0==335 .or. kx0==435 .or. kx0==535) kx=235
              end if
           endif

!------------------------------------------------------------------------

           if(offtime_data) then
 
!             in time correction for observations to account for analysis
!                      time being different from obs file time.
              write(date,'( i10)') idate
              read (date,'(i4,3i2)') iy,im,idd,ihh
              idate5(1)=iy
              idate5(2)=im
              idate5(3)=idd
              idate5(4)=ihh
              idate5(5)=0
              call w3fs21(idate5,minobs)    !  obs ref time in minutes relative to historic date
              idate5(1)=iadate(1)
              idate5(2)=iadate(2)
              idate5(3)=iadate(3)
              idate5(4)=iadate(4)
              idate5(5)=0
              call w3fs21(idate5,minan)    !  analysis ref time in minutes relative to historic date
 
!             Add obs reference time, then subtract analysis time to get obs time relative to analysis
 
              time_correction=float(minobs-minan)*r60inv

           else
              time_correction=zero
           end if

           if (.not. (aircraft_t_bc .and. acft_profl_file)) then
              timeobs=real(real(hdr(4),r_single),r_double)
              t4dv=timeobs + toff
              zeps=1.0e-8_r_kind
              if (t4dv<zero  .and.t4dv>      -zeps) t4dv=zero
              if (t4dv>winlen.and.t4dv<winlen+zeps) t4dv=winlen
              t4dv=t4dv + time_correction
              time=timeobs + time_correction
           end if
           if(use_prepb_satwnd .and. (kx >= 240 .and. kx <= 260)) iobsub = hdr(7)

 
!          Balloon drift information available for these data
           driftl=kx==120.or.kx==220.or.kx==221

           if (.not. (aircraft_t_bc .and. acft_profl_file)) then
              if (l4dvar.or.l4densvar) then
                 if ((t4dv<zero.OR.t4dv>winlen) .and. .not.driftl) cycle loop_readsb ! outside time window
              else
                 if((real(abs(time)) > real(ctwind(nc)) .or. real(abs(time)) > real(twindin)) &
                    .and. .not. driftl)cycle loop_readsb ! outside time window
              endif

              timex=time
           end if

!          If ASCAT data, determine primary surface type.  If not open sea,
!          skip this observation.  This check must be done before thinning.
           if (kx==290 .or. kx==289 .or. kx==285) then
              call deter_sfc_type(dlat_earth,dlon_earth,t4dv,isflg,tsavg)
              if (isflg /= 0) cycle loop_readsb
              if (tsavg <= 273.0_r_kind) cycle loop_readsb
           endif

           sfctype=(kx>179.and.kx<190).or.(kx>=280.and.kx<=290).or. &
                   (kx>=192.and.kx<=199).or.(kx>=292.and.kx<=299)

           if (sfctype) then
              call ufbint(lunin,r_prvstg,1,1,iret,prvstr)
              call ufbint(lunin,r_sprvstg,1,1,iret,sprvstr)
           else
              c_prvstg=cspval
              c_sprvstg=cspval
           endif
     
!          Extract data information on levels
           call ufbint(lunin,obsdat,13,255,levs,obstr)
           if (twodvar_regional) then
              if (mxtmob .or. mitmob) call ufbint(lunin,maxtmint,2,255,levs,maxtmintstr)
              if (howvob)             call ufbint(lunin,owave,1,255,levs,owavestr)
              if (cldchob)            call ufbint(lunin,cldceilh,1,255,levs,cldceilhstr)
           endif
           if(kx==224 .and. newvad) then
           call ufbint(lunin,fcstdat,3,255,levs,'UFC VFC TFC ')
           end if
           call ufbint(lunin,qcmark,8,255,levs,qcstr)
           call ufbint(lunin,obserr,8,255,levs,oestr)
           call ufbevn(lunin,tpc,1,255,20,levs,'TPC')

!          If available, get obs errors from error table
           
           if(oberrflg)then

!             Set lower limits for observation errors
              terrmin=half
              werrmin=one
              perrmin=0.3_r_kind
              qerrmin=0.05_r_kind
              pwerrmin=one
              tjbmin=zero
              qjbmin=zero
              wjbmin=zero
              pjbmin=zero
              itypey=kx
              if( njqc) then
                 if (psob)  then
                    itypex=itypey
                    ierr_ps=0
                    do i =1,maxsub_ps
                       if(icsubtype(nc)==isuble_ps(itypex,i)) then
                          ierr_ps=i+1
                          exit
                       else if(i== maxsub_ps .and. icsubtype(nc) /= isuble_ps(itypex,i)) then
                          ncount_ps=ncount_ps+1
                          do j=1,maxsub_ps
                             if(isuble_ps(itypex,j) ==0 ) then
                                ierr_ps=j+1
                                exit
                             endif
                          enddo
                          if (ncount_ps ==1) then
                             write(6,*) 'READ_PREPBUFR: WARNING!!psob: cannot find subtyep in the &
                                        error table,itype,iosub=',itypex,icsubtype(nc)
                             write(6,*) 'read error table at colomn subtype as 0, error table column=',ierr_ps
                          endif
                       endif
                    enddo
                    do k=1,levs
                       ppb=obsdat(1,k)
                      cat=nint(min(obsdat(10,k),qcmark_huge))
                       if ( cat /=0 ) cycle 
                       ppb=max(zero,min(ppb,r2000))
                       if(ppb>=etabl_ps(itypex,1,1)) k1_ps=1
                       do kl=1,32
                          if(ppb>=etabl_ps(itypex,kl+1,1).and.ppb<=etabl_ps(itypex,kl,1)) k1_ps=kl
                       end do
                       if(ppb<=etabl_ps(itypex,33,1)) k1_ps=5
                       k2_ps=k1_ps+1
                       ediff_ps = etabl_ps(itypex,k2_ps,1)-etabl_ps(itypex,k1_ps,1)
                       if (abs(ediff_ps) > tiny_r_kind) then
                          del_ps = (ppb-etabl_ps(itypex,k1_ps,1))/ediff_ps
                       else
                         del_ps = huge_r_kind
                       endif
                       del_ps=max(zero,min(del_ps,one))
                       if(oberrflg)then
!                         write(6,*) 'READ_PREPBUFR_PS:',itypex,k1_ps,ierr_ps,k2_ps,ierr_ps
                          obserr(1,k)=(one-del_ps)*etabl_ps(itypex,k1_ps,ierr_ps)+del_ps*etabl_ps(itypex,k2_ps,ierr_ps)
! Surface pressure error
                          obserr(1,k)=max(obserr(1,k),perrmin)
                       endif
! Surface pressure b
                      var_jb(1,k)=(one-del_ps)*btabl_ps(itypex,k1_ps,ierr_ps)+del_ps*btabl_ps(itypex,k2_ps,ierr_ps)
                       var_jb(1,k)=max(var_jb(1,k),pjbmin)
                       if (var_jb(1,k) >=10.0_r_kind) var_jb(1,k)=zero
                       if(itypey==180 .and. ierr_ps == 0 ) then
                          write(6,*) 'READ_PREPBUFR:180_ps,obserr,var_jb=',obserr(1,k),var_jb(1,k),ppb,k,hdr(2),hdr(3)
                       endif
                    enddo
                 endif
                if (tob) then
                    itypex=itypey
                    ierr_t=0
                    do i =1,maxsub_t
                       if( icsubtype(nc) == isuble_t(itypex,i) ) then
                          ierr_t=i+1
                          exit
                       else if( i == maxsub_t .and. icsubtype(nc) /= isuble_t(itypex,i)) then
                          ncount_t=ncount_t+1
                          do j=1,maxsub_t
                             if(isuble_t(itypex,j) ==0 ) then
                                ierr_t=j+1
                                exit
                             endif
                          enddo
                          if( ncount_t ==1) then
                             write(6,*) 'READ_PREPBUFR,WARNING!! tob:cannot find subtyep in the error,& 
                                         table,itype,iosub=',itypex,icsubtype(nc)
                             write(6,*) 'read error table at colomn subtype as 0,error table column=',ierr_t
                          endif
                       endif
                    enddo
                    do k=1,levs
                       ppb=obsdat(1,k)
                       if(kx==153)ppb=obsdat(11,k)*0.01_r_kind
                       ppb=max(zero,min(ppb,r2000))
                       if(ppb>=etabl_t(itypex,1,1)) k1_t=1
                       do kl=1,32
                          if(ppb>=etabl_t(itypex,kl+1,1).and.ppb<=etabl_t(itypex,kl,1)) k1_t=kl
                       end do
                       if(ppb<=etabl_t(itypex,33,1)) k1_t=5
                       k2_t=k1_t+1
                       ediff_t = etabl_t(itypex,k2_t,1)-etabl_t(itypex,k1_t,1)
                       if (abs(ediff_t) > tiny_r_kind) then
                          del_t = (ppb-etabl_t(itypex,k1_t,1))/ediff_t
                       else
                         del_t = huge_r_kind
                       endif
                       del_t=max(zero,min(del_t,one))
! Temperature error
                       if(oberrflg)then
!                         write(6,*) 'READ_PREPBUFR_T:',itypex,k1_t,itypey,k2_t,ierr_t,nc,kx,ppb
                          obserr(3,k)=(one-del_t)*etabl_t(itypex,k1_t,ierr_t)+del_t*etabl_t(itypex,k2_t,ierr_t)
                          obserr(3,k)=max(obserr(3,k),terrmin)
                       endif
!Temperature b
                       var_jb(3,k)=(one-del_t)*btabl_t(itypex,k1_t,ierr_t)+del_t*btabl_t(itypex,k2_t,ierr_t)
                       var_jb(3,k)=max(var_jb(3,k),tjbmin)
                       if (var_jb(3,k) >=10.0_r_kind) var_jb(3,k)=zero
                        if(itypey==180) then
                          write(6,*) 'READ_PREPBUFR:180_t,obserr,var_jb=',obserr(3,k),var_jb(3,k),ppb
                        endif
                    enddo
                 endif
                 if (qob) then
                    itypex=itypey
                    ierr_q=0
                    do i =1,maxsub_q
                       if( icsubtype(nc) == isuble_q(itypex,i) ) then
                          ierr_q=i+1
                          exit
                       else if( i == maxsub_q .and. icsubtype(nc) /= isuble_q(itypex,i)) then
                          ncount_q=ncount_q+1
                          do j=1,maxsub_q
                             if(isuble_q(itypex,j) ==0 ) then
                                ierr_q=j+1
                                exit
                             endif
                          enddo
                          if(ncount_q ==1 ) then
                             write(6,*) 'READ_PREPBUFR,WARNING!! qob:cannot find subtyep in the & 
                                        error table,itype,iosub=',itypex,icsubtype(nc)
                             write(6,*) 'read error table at colomn subtype as 0,error table column=',ierr_q
                          endif
                       endif
                    enddo
                    do k=1,levs
                       ppb=obsdat(1,k)
                       if(kx==153)ppb=obsdat(11,k)*0.01_r_kind
                       ppb=max(zero,min(ppb,r2000))
                       if(ppb>=etabl_q(itypex,1,1)) k1_q=1
                       do kl=1,32
                          if(ppb>=etabl_q(itypex,kl+1,1).and.ppb<=etabl_q(itypex,kl,1)) k1_q=kl
                       end do
                       if(ppb<=etabl_q(itypex,33,1)) k1_q=5
                       k2_q=k1_q+1
                       ediff_q = etabl_q(itypex,k2_q,1)-etabl_q(itypex,k1_q,1)
                       if (abs(ediff_q) > tiny_r_kind) then
                          del_q = (ppb-etabl_q(itypex,k1_q,1))/ediff_q
                       else
                         del_q = huge_r_kind
                       endif
                       del_q=max(zero,min(del_q,one))
! Humidity error
                       if(oberrflg)then
!                          write(6,*) 'READ_PREPBUFR_Q:',itypex,k1_q,itypey,k2_q,ierr_q,nc,kx,ppb
                          obserr(2,k)=(one-del_q)*etabl_q(itypex,k1_q,ierr_q)+del_q*etabl_q(itypex,k2_q,ierr_q)
                          obserr(2,k)=max(obserr(2,k),qerrmin)
                       endif
!Humidity b
                       var_jb(2,k)=(one-del_q)*btabl_q(itypex,k1_q,ierr_q)+del_q*btabl_q(itypex,k2_q,ierr_q)
                       var_jb(2,k)=max(var_jb(2,k),qjbmin)
                       if (var_jb(2,k) >=10.0_r_kind) var_jb(2,k)=zero
!                      if(itypey==120  ) then
!                        write(6,*) 'READ_PREPBUFR:120_q,obserr,var_jb=',obserr(2,k),var_jb(2,k),ppb
!                      endif
                    enddo
                endif
                if (uvob) then
                   itypex=itypey
                   ierr_uv=0
                   do i =1,maxsub_uv
                      if( icsubtype(nc) == isuble_uv(itypex,i) ) then
                          ierr_uv=i+1
                          exit
                       else if( i == maxsub_uv .and. icsubtype(nc) /= isuble_uv(itypex,i)) then
                          ncount_uv=ncount_uv+1
                          do j=1,maxsub_uv
                             if(isuble_uv(itypex,j) ==0 ) then
                                ierr_uv=j+1
                                exit
                             endif
                          enddo
                          if( ncount_uv == 1) then
                             write(6,*) 'READ_PREPBUFR,WARNING!! uvob:cannot find subtyep in the error,&
                                         table,itype,iosub=',itypex,icsubtype(nc)
                             write(6,*) 'read error table at colomn subtype as 0,error table column=',ierr_uv
                          endif
                       endif
                    enddo
                   do k=1,levs
                      ppb=obsdat(1,k)
                      if(kx==153)ppb=obsdat(11,k)*0.01_r_kind
                      ppb=max(zero,min(ppb,r2000))
                      if(ppb>=etabl_uv(itypex,1,1)) k1_uv=1
                      do kl=1,32
                         if(ppb>=etabl_uv(itypex,kl+1,1).and.ppb<=etabl_uv(itypex,kl,1)) k1_uv=kl
                      end do
                      if(ppb<=etabl_uv(itypex,33,1)) k1_uv=5
                      k2_uv=k1_uv+1
                      ediff_uv = etabl_uv(itypex,k2_uv,1)-etabl_uv(itypex,k1_uv,1)
                      if (abs(ediff_uv) > tiny_r_kind) then
                         del_uv = (ppb-etabl_uv(itypex,k1_uv,1))/ediff_uv
                      else
                        del_uv = huge_r_kind
                      endif
                      del_uv=max(zero,min(del_uv,one))
! Wind error
!                         write(6,*) 'READ_PREPBUFR_UV:',itypex,k1_uv,itypey,k2_uv,ierr_uv,nc,kx,ppb
                         obserr(5,k)=(one-del_uv)*etabl_uv(itypex,k1_uv,ierr_uv)+del_uv*etabl_uv(itypex,k2_uv,ierr_uv)
                         obserr(5,k)=max(obserr(5,k),werrmin)
!Wind b
                      var_jb(5,k)=(one-del_uv)*btabl_uv(itypex,k1_uv,ierr_uv)+del_uv*btabl_uv(itypex,k2_uv,ierr_uv)
                      var_jb(5,k)=max(var_jb(5,k),wjbmin)
                      if (var_jb(5,k) >=10.0_r_kind) var_jb(5,k)=zero
!                      if(itypey==220) then
!                         write(6,*) 'READ_PREPBUFR:220_uv,obserr,var_jb=',obserr(5,k),var_jb(5,k),ppb,k2_uv,del_uv
!                      endif
                   enddo
                endif
                if (pwob)  then
                   itypex=itypey
                   ierr_pw=0
                   do i =1,maxsub_pw
                      if(icsubtype(nc) == isuble_pw(itypex,i) ) then
                         ierr_pw=i+1
                         exit
                      else if( i == maxsub_pw .and. icsubtype(nc) /= isuble_pw(itypex,i)) then
                         ncount_pw=ncount_pw+1
                         do j=1,maxsub_pw
                            if(isuble_pw(itypex,j) ==0 ) then
                               ierr_pw=j+1
                               exit
                            endif
                         enddo
                         if(ncount_pw ==1 ) then
                            write(6,*) 'READ_PREPBUFR,WARNING!! pwob:cannot find subtyep in the error,&
                                        table,itypex,iosub=',itypex,icsubtype(nc)
                            write(6,*) 'read error table at colomn subtype as 0,error table column=',ierr_pw
                         endif
                      endif
                   enddo
                   do k=1,levs
                      ppb=obsdat(1,k)
                      if(kx==153)ppb=obsdat(11,k)*0.01_r_kind
                      ppb=max(zero,min(ppb,r2000))
                      if(ppb>=etabl_pw(itypex,1,1)) k1_pw=1
                      do kl=1,32
                         if(ppb>=etabl_pw(itypex,kl+1,1).and.ppb<=etabl_pw(itypex,kl,1)) k1_pw=kl
                      end do
                      if(ppb<=etabl_pw(itypex,33,1)) k1_pw=5
                      k2_pw=k1_pw+1
                      ediff_pw = etabl_pw(itypex,k2_pw,1)-etabl_pw(itypex,k1_pw,1)
                      if (abs(ediff_pw) > tiny_r_kind) then
                         del_pw = (ppb-etabl_pw(itypex,k1_pw,1))/ediff_pw
                      else
                         del_pw = huge_r_kind
                      endif
                      del_pw=max(zero,min(del_pw,one))
                      if(oberrflg)then
! Precip water error
!                        write(6,*) 'READ_PREPBUFR_Pw:',itypex,itypey,ierr_pw,k2_pw,ierr_pw,nc,kx,ppb
                         obserr(7,k)=(one-del_pw)*etabl_pw(itypex,k1_pw,ierr_pw)+del_pw*etabl_pw(itypex,k2_pw,ierr_pw)
                         obserr(7,k)=max(obserr(7,k),pwerrmin)
                      endif
                   enddo
                endif
             else
                do k=1,levs
                   itypex=kx
                   ppb=obsdat(1,k)
                   if(kx==153)ppb=obsdat(11,k)*0.01_r_kind
                   ppb=max(zero,min(ppb,r2000))
                   if(ppb>=etabl(itypex,1,1)) k1=1
                   do kl=1,32
                      if(ppb>=etabl(itypex,kl+1,1).and.ppb<=etabl(itypex,kl,1)) k1=kl
                   end do
                   if(ppb<=etabl(itypex,33,1)) k1=5
                   k2=k1+1
                   ediff = etabl(itypex,k2,1)-etabl(itypex,k1,1)
                   if (abs(ediff) > tiny_r_kind) then
                      del = (ppb-etabl(itypex,k1,1))/ediff
                   else
                      del = huge_r_kind
                   endif
                   del=max(zero,min(del,one))
                   obserr(3,k)=(one-del)*etabl(itypex,k1,2)+del*etabl(itypex,k2,2)
                   obserr(2,k)=(one-del)*etabl(itypex,k1,3)+del*etabl(itypex,k2,3)
                   obserr(5,k)=(one-del)*etabl(itypex,k1,4)+del*etabl(itypex,k2,4)
                   obserr(1,k)=(one-del)*etabl(itypex,k1,5)+del*etabl(itypex,k2,5)
                   obserr(7,k)=(one-del)*etabl(itypex,k1,6)+del*etabl(itypex,k2,6)

                   obserr(3,k)=max(obserr(3,k),terrmin)
                   obserr(2,k)=max(obserr(2,k),qerrmin)
                   obserr(5,k)=max(obserr(5,k),werrmin)
                   obserr(1,k)=max(obserr(1,k),perrmin)
                   obserr(7,k)=max(obserr(7,k),pwerrmin)
                enddo
             endif      ! endif for njqc
           endif        ! endif for oberrflg

!          If data with drift position, get drift information
           if(driftl)call ufbint(lunin,drfdat,8,255,iret,drift)
     
! raob level enhancement on temp and q obs 
           if(ext_sonde .and. kx==120) call sonde_ext(obsdat,tpc,qcmark,obserr,drfdat,levs,kx,vtcd)

           nread=nread+levs
           aircraftobst = .false.
           if(uvob)then
              nread=nread+levs
           else if(tob) then
!             aircraft temperature data
!             aircraftobst = kx>129.and.kx<140
              aircraftobst = (kx==131) .or. (kx>=133 .and. kx<=135) .or. (kx==130) ! for currently known types

              aircraftwk = bmiss
              if (aircraftobst) then
                 if (aircraft_t_bc) then
                    call ufbint(lunin,aircraftwk,2,255,levs,aircraftstr)
                    if (kx0>=330 .and. kx0<340) aircraftwk(2,:) = zero
                 else if (aircraft_t_bc_pof) then
                    call ufbint(lunin,aircraftwk,2,255,levs,aircraftstr)
                    aircraftwk(2,:) = bmiss
                    if (kx==130) aircraftwk(1,:) = 3.0_r_kind 
                 else if (aircraft_t_bc_ext) then
                    call ufbint(lunin,aircraftwk,2,255,levs,aircraftstr)
                    aircraftwk(2,:) = bmiss
                 end if
              end if
           else if(sstob)then 
              sstdat=bmiss
              call ufbint(lunin,sstdat,8,1,levs,sststr)
           else if(metarcldobs) then
              metarcld=bmiss
              metarwth=bmiss
              metarvis=bmiss
              call ufbint(lunin,metarcld,2,10,metarcldlevs,metarcldstr)
              call ufbint(lunin,metarwth,1,10,metarwthlevs,metarwthstr)
              call ufbint(lunin,metarvis,2,1,iret,metarvisstr)
              if(levs /= 1 ) then
                 write(6,*) 'READ_PREPBUFR: error in Metar observations, levs sould be 1 !!!'
                 call stop2(110)
              endif
           else if(goesctpobs) then
              goescld=bmiss
              call ufbint(lunin,goescld,4,1,levs,goescldstr)
           else if (visob) then
              metarwth=bmiss
              call ufbint(lunin,metarwth,1,10,metarwthlevs,metarwthstr)
           else if(tcamtob .or. lcbasob) then
              if (trim(subset) == 'GOESND') then
                 goescld=bmiss
                 call ufbint(lunin,goescld,4,1,levs,goescldstr)
                 if (all(goescld==bmiss)) cycle
              else
                 cldseq=bmiss
                 metarwth=bmiss
                 cld2seq =bmiss
                 call ufbint(lunin,cldseq,3,10,cldseqlevs,cldseqstr)
                 call ufbrep(lunin,cld2seq,2,1,cld2seqlevs,cld2seqstr)
                 call ufbint(lunin,metarwth,1,10,metarwthlevs,metarwthstr)
                 if (all(cldseq==bmiss) .and. all(cld2seq==bmiss) .and. all(metarwth==bmiss)) cycle
              endif

           endif

!          Set station ID
           rstation_id=hdr(1)
           if ((kx==133 .or. kx==233) .and. c_station_id=='ACARS') then
              call ufbint(lunin,hdr,1,1,iret,'ACID')
              if(ibfms(hdr(1))==0) rstation_id=hdr(1)
           end if

!          Check for valid reported pressure (POB).  Set POB=bmiss if POB<tiny_r_kind
           rstation_id=hdr(1)
           do k=1,levs
              if (obsdat(1,k)<tiny_r_kind) then
                 write(6,*)'READ_PREPBUFR:  ***WARNING*** invalid pressure pob=',&
                    obsdat(1,k),' at k=',k,' for obstype=',obstype,' kx=',kx,&
                    ' c_station_id=',c_station_id,' reset pob=',bmiss
                 obsdat(1,k)=bmiss
              endif
           end do

!          Determine tail number for aircraft temperature data
           idx = 0
           iyyyymm = iadate(1)*100+iadate(2)
           if (tob)then
            if (aircraftobst .and. (aircraft_t_bc_pof .or. &
                aircraft_t_bc .or. aircraft_t_bc_ext)) then
!             Determine if the tail number is included in the taillist
              do j=1,nsort
!                special treatment since kx130 has only flight NO. info, no
!                aircraft type info
                 if (kx==130) then
                    cc_station_id = 'KX130'
                 else
                    cc_station_id = c_station_id
                 end if
                 cb = cc_station_id(1:1)
                 if (cb==itail_sort(j)) then
                    start = idx_sort(j)
                    if (j==nsort) then
                       next = ntail
                    else
                       next=idx_sort(j+1)-1
                    end if
                    do jj=start,next
                       if (trim(cc_station_id)==trim(taillist(jj))) then
                          idx = jj
                          if (timelist(jj)/=iyyyymm) timelist(jj) = iyyyymm
                          exit
                       end if
                    end do
                 end if
              end do

              if (idx==0 .and. ntail_update>ntail) then
                 do j = ntail+1,ntail_update
                    if (cc_station_id == trim(taillist(j))) then
                       idx = j
                       exit
                    end if
                 end do
              end if

!             Append new tail number at the end of existing tail numbers.
!             At 1st analysis, the obs will be used without bias correction,
!             patch new tail number;
!             At 2nd analysis, bias coefficients will be generated for this new
!             tail number.
              if (idx == 0) then
                 ntail_update = ntail_update+1
!                print*, cc_station_id, ' ntail_update=',ntail_update,'
!                ntail=',ntail
                 if (ntail_update > max_tail) then
                    write(6,*)'READ_PREPBUFR: ***ERROR*** tail number exceeds maximum'
                    write(6,*)'READ_PREPBUFR: stop program execution'
                    call stop2(341)
                 end if
                 idx_tail(ntail_update) = ntail_update
                 taillist(ntail_update) = cc_station_id
                 timelist(ntail_update) = iyyyymm
                 do j = 1,npredt
                    predt(j,ntail_update) = zero
                 end do
              end if

!             Re-set idx if idx>ntail 
              if (idx>ntail) idx = 0
            end if
           end if

!          Loop over levels
           do k=1,levs
              do i=1,8
                 qcmark(i,k) = min(qcmark(i,k),qcmark_huge)
              end do

              if (kx == id_bias_ps) then
                 plevs(k)=one_tenth*obsdat(1,k)+conv_bias_ps   ! convert mb to cb
              else
                 plevs(k)=one_tenth*obsdat(1,k)   ! convert mb to cb
              endif
              if (kx == 290) plevs(k)=101.0_r_kind  ! Assume 1010 mb = 101.0 cb
              if (goesctpobs) plevs(k)=goescld(1,k)/1000.0_r_kind ! cloud top pressure in cb
              pqm(k)=nint(qcmark(1,k))
              qqm(k)=nint(qcmark(2,k))
              tqm(k)=nint(qcmark(3,k))
              wqm(k)=nint(qcmark(5,k))
              pmq(k)=nint(qcmark(8,k))
           end do

!          If temperature ob, extract information regarding virtual
!          versus sensible temperature
           if(tob) then
              if (.not. twodvar_regional .or. .not.tsensible) then
                 do k=1,levs
                    tvflg(k)=one                               ! initialize as sensible
                    do j=1,20
                       if (tpc(k,j)==vtcd) tvflg(k)=zero       ! reset flag if virtual
                       if (tpc(k,j)>=bmiss) exit               ! end of stack
                    end do
                 end do
              else         !peel back events to store sensible temp in case temp is virtual
                 call ufbevn(lunin,tobaux,2,255,20,levs,'TOB TQM')
                 do k=1,levs
                    tvflg(k)=one                              ! initialize as sensible
                    do j=1,20
                       if (tpc(k,j)==vtcd) then
                          obsdat(3,k)=tobaux(1,k,j+1)
                          qcmark(3,k)=min(tobaux(2,k,j+1),qcmark_huge)
                          tqm(k)=nint(qcmark(3,k))
                       end if
                       if (tpc(k,j)>=bmiss) exit              ! end of stack
                    end do
                 end do
              end if
           end if

           stnelev=hdr(6)
           ithin=ithin_conv(nc)
           ithinp = ithin > 0 .and. pflag /= 0
           if(.not. (driftl .or. (aircraft_t_bc .and. acft_profl_file)) .and. &
              (((tob .or. qob .or. uvob).and. levs > 1) .or. ithinp))then
!             Interpolate guess pressure profile to observation location
              klon1= int(dlon);  klat1= int(dlat)
              dx   = dlon-klon1; dy   = dlat-klat1
              dx1  = one-dx;     dy1  = one-dy
              w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy
 
              klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
              if (klon1==0) klon1=nlon
              klatp1=min(nlat,klat1+1); klonp1=klon1+1
              if (klonp1==nlon+1) klonp1=1
              do kk=1,nsig
                 presl(kk)=w00*prsl_full(klat1 ,klon1 ,kk) +  &
                           w10*prsl_full(klatp1,klon1 ,kk) + &
                           w01*prsl_full(klat1 ,klonp1,kk) + &
                           w11*prsl_full(klatp1,klonp1,kk)
              end do

!             Compute depth of guess pressure layersat observation location
              if (.not.twodvar_regional .and. levs > 1) then
                 do kk=1,nsig-1
                    dpres(kk)=presl(kk)-presl(kk+1)
                 end do
              endif
           end if
           LOOP_K_LEVS: do k=1,levs
                 if(kx==224 .and. newvad)then
                    if(mod(k,6)/=0) cycle LOOP_K_LEVS
                 end if

              icntpnt=icntpnt+1

!             Extract quality marks
              if(tob)then
                 qm=tqm(k)
              else if(uvob) then 
                 qm=wqm(k)
              else if(spdob) then
                 qm=wqm(k)
              else if(psob) then
                 qm=pqm(k)
              else if(qob) then
                 if(obsdat(2,k) > r0_01_bmiss)cycle loop_k_levs
                 qm=qqm(k)
              else if(pwob) then
                 pwq=nint(qcmark(7,k))
                 qm=pwq
              else if(sstob) then
                 sstq=100
                 if (k==1) sstq=nint(min(sstdat(4,k),qcmark_huge))
                 qm=sstq
              else if(gustob) then
                 gustqm=0
                 if (kx==188 .or. kx==288 .or. kx==195 .or. kx==295 ) &
                 call get_gustqm(kx,c_station_id,c_prvstg,c_sprvstg,gustqm)
                 qm=gustqm
              else if(visob) then
                 visqm=0    ! need to fix this later
                 qm=visqm
!! RY: check this late when using tdob??
              else if(tdob) then
                 if(obsdat(12,k) > r0_01_bmiss)cycle loop_k_levs
                 tdqm=qqm(k)
                 qm=tdqm
              else if(mxtmob) then
                 mxtmqm=0
                 qm=mxtmqm
              else if(mitmob) then
                 mitmqm=0
                 qm=mitmqm
              else if(pmob) then
                 qm=pmq(k)
              else if(howvob) then
                 howvqm=0
                 qm=howvqm
              else if(cldchob) then
                 cldchqm=0
                 qm=cldchqm
              else if(metarcldobs) then
                 qm=0      
              else if(goesctpobs) then
                 qm=0
              else if(tcamtob) then
                 qm=0
                 if (kx==151)pqm=0 !Make sure GOESND data are not rejected due to the pressure quality mark
              else if(lcbasob) then
                 qm=0
                 if (kx==151)pqm=0 !Make sure GOESND data are not rejected due to the pressure quality mark
             end if
 

!             Check qc marks to see if obs should be processed or skipped

              if (visob) then
                 if (obsdat(9,k) > r0_1_bmiss) then
                    patch_fog=(metarwth(1,1)>= 40.0_r_kind .and. metarwth(1,1)<= 49.0_r_kind) .or. &
                              (metarwth(1,1)>=130.0_r_kind .and. metarwth(1,1)<=135.0_r_kind) .or. &
                              (metarwth(1,1)>=241.0_r_kind .and. metarwth(1,1)<=246.0_r_kind)
                    if (patch_fog) obsdat(9,k)=1000.0_r_kind
                    if (metarwth(1,1)==247.0_r_kind) obsdat(9,k)=75.0_r_kind
                    if (metarwth(1,1)==248.0_r_kind) obsdat(9,k)=45.0_r_kind
                    if (metarwth(1,1)==249.0_r_kind) obsdat(9,k)=15.0_r_kind
                 end if
              end if

              if (psob) then
                 cat=nint(min(obsdat(10,k),qcmark_huge))
                 if ( cat /=0 ) cycle loop_k_levs
                 if ( obsdat(1,k)< r500) qm=100
                 zqm=nint(qcmark(4,k))
                 if (zqm>=lim_zqm .and. zqm/=15 .and. zqm/=9) qm=9
              endif

!             if(convobs .and. pqm(k) >=lim_qm .and. qm/=15 .and. qm/=9 )cycle loop_k_levs
!             if(qm >=lim_qm .and. qm /=15 .and. qm /=9)cycle loop_k_levs
              if(qm > 15 .or. qm < 0) cycle loop_k_levs

!             extract aircraft profile information
              if (aircraft_t_bc .and. acft_profl_file) then
                 if (nint(obsdat(10,k))==7) cycle LOOP_K_LEVS
                 if(abs(hdr3(2,k))>r90 .or. abs(hdr3(1,k))>r360) cycle LOOP_K_LEVS
                 if(hdr3(1,k)== r360)hdr3(1,k)=hdr3(1,k)-r360
                 if(hdr3(1,k) < zero)hdr3(1,k)=hdr3(1,k)+r360
                 dlon_earth=hdr3(1,k)*deg2rad
                 dlat_earth=hdr3(2,k)*deg2rad

                 if(regional)then
                    call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    !  convert to rotated coordinate
                    if(outside) cycle loop_readsb   ! check to see if outside regional domain
                 else
                    dlat = dlat_earth
                    dlon = dlon_earth
                    call grdcrd1(dlat,rlats,nlat,1)
                    call grdcrd1(dlon,rlons,nlon,1)
                 endif

                 timeobs=real(real(hdr3(3,k),r_single),r_double)
                 t4dv=timeobs + toff
                 zeps=1.0e-8_r_kind
                 if (t4dv<zero  .and.t4dv>      -zeps) t4dv=zero
                 if (t4dv>winlen.and.t4dv<winlen+zeps) t4dv=winlen
                 t4dv=t4dv + time_correction
                 time=timeobs + time_correction
                 if (l4dvar.or.l4densvar) then
                    if (t4dv<zero.OR.t4dv>winlen) cycle LOOP_K_LEVS
                 else
                    if (real(abs(time))>real(ctwind(nc)) .or.  real(abs(time))>real(twindin)) cycle LOOP_K_LEVS
                 endif
              end if

!             If needed, extract drift information.   
              if(driftl)then
                 if(drfdat(1,k) >= r360)drfdat(1,k)=drfdat(1,k)-r360
                 if(drfdat(1,k) <  zero)drfdat(1,k)=drfdat(1,k)+r360
                 if(abs(drfdat(2,k)) > r90 .or. drfdat(1,k) > r360 .or. drfdat(1,k) < zero)then
                    drfdat(2,k)=hdr(3)
                    drfdat(1,k)=hdr(2)
                 end if

!                Check to ensure header lat and drift lat similar
                 if(abs(drfdat(2,k)-hdr(3)) > r10 .and.  &
                    abs(drfdat(1,k)-hdr(2)) > r10)then
                    drfdat(2,k)=hdr(3)
                    drfdat(1,k)=hdr(2)
                 end if

!                Check to see if the time is outrageous if so set to header value
                 timeobs = real(real(drfdat(3,k),r_single),r_double)
                 time_drift = timeobs + time_correction
                 if (abs(time_drift-time)>four) time_drift = time
 
!                Check to see if the time is outside range
                 if (l4dvar.or.l4densvar) then
                    t4dv=toff+time_drift
                    if (t4dv<zero .or. t4dv>winlen) then
                       t4dv=toff+timex
                       if (t4dv<zero .or. t4dv>winlen) CYCLE LOOP_K_LEVS
                    end if
                 else
                    if(abs(time_drift) > ctwind(nc) .or. abs(time_drift) > twindin)then
                       time_drift=timex
                       if(abs(timex) > ctwind(nc) .or. abs(timex) > twindin) CYCLE LOOP_K_LEVS
                    end if
                    t4dv = toff + time_drift
                 endif

                 dlat_earth = drfdat(2,k) * deg2rad
                 dlon_earth = drfdat(1,k) * deg2rad
 
                 if(regional)then
                    call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
                    if(outside) cycle LOOP_K_LEVS 
                 else
                    dlat = dlat_earth
                    dlon = dlon_earth
                    call grdcrd1(dlat,rlats,nlat,1)
                    call grdcrd1(dlon,rlons,nlon,1)
                 endif
              end if

              if((driftl .or. (aircraft_t_bc .and. acft_profl_file))  &
                 .and. ((tob.or. qob.or. uvob .and. levs > 1) .or. ithinp))then
!                Interpolate guess pressure profile to observation location
                 klon1= int(dlon);  klat1= int(dlat)
                 dx   = dlon-klon1; dy   = dlat-klat1
                 dx1  = one-dx;     dy1  = one-dy
                 w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

                 klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
                 if (klon1==0) klon1=nlon
                 klatp1=min(nlat,klat1+1); klonp1=klon1+1
                 if (klonp1==nlon+1) klonp1=1

                 do kk=1,nsig
                    presl(kk)=w00*prsl_full(klat1 ,klon1 ,kk) +  &
                              w10*prsl_full(klatp1,klon1 ,kk) + &
                              w01*prsl_full(klat1 ,klonp1,kk) + &
                              w11*prsl_full(klatp1,klonp1,kk)
                 end do

!                Compute depth of guess pressure layersat observation location
                 if (.not.twodvar_regional .and. levs > 1) then
                    do kk=1,nsig-1
                       dpres(kk)=presl(kk)-presl(kk+1)
                    end do
                 endif
              end if

!             Special block for data thinning - if requested
              if (ithin > 0) then
                 ntmp=ndata  ! counting moved to map3gridS
           
!                Set data quality index for thinning
                 if (thin4d) then
                    timedif = zero
                 else
                    timedif=abs(t4dv-toff)
                 endif
                 if(kx == 243 .or. kx == 253 .or. kx ==254) then
                    call ufbint(lunin,satqc,1,1,iret,satqcstr)
                    crit1 = timedif/r6+half + four*(one-satqc(1)/r100)*r3_33
                 else
                    crit1 = timedif/r6+half
                 endif

                 if (pflag==0) then
                    do kk=1,nsig
                       presl_thin(kk)=presl(kk)
                    end do
                 endif

                 call map3grids(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                    plevs(k),crit1,ndata,iout,icntpnt,iiout,luse,.false.,.false.)

                 if (.not. luse) then
                    if(k==levs) then
                       cycle loop_readsb
                    else
                       cycle LOOP_K_LEVS
                    endif
                 endif
                 if(iiout > 0) isort(iiout)=0
                 if(ndata >  ntmp)then
                    nodata=nodata+1
                    if(uvob)nodata=nodata+1
                 end if
                 isort(icntpnt)=iout

              else
                 ndata=ndata+1
                 nodata=nodata+1
                 if(uvob)nodata=nodata+1
                 iout=ndata
                 isort(icntpnt)=iout
              endif

              if(ndata > maxobs) then
                 write(6,*)'READ_PREPBUFR:  ***WARNING*** ndata > maxobs for ',obstype
                 ndata = maxobs
              end if

!             Set usage variable              
              usage = zero


              if(icuse(nc) <= 0)usage=100._r_kind
              if(qm == 15 .or. qm == 12 .or. qm == 9)usage=100._r_kind
              if(qm >=lim_qm )usage=101._r_kind
              if(convobs .and. pqm(k) >=lim_qm )usage=102._r_kind
              if((kx>=192.and.kx<=195) .and. psob )usage=r100
              if (gustob .and. obsdat(8,k) > r0_1_bmiss) usage=103._r_kind
              if (visob  .and. obsdat(9,k) > r0_1_bmiss) usage=103._r_kind
              if (tdob  .and. obsdat(12,k) > r0_1_bmiss) usage=103._r_kind
              if (pmob  .and. obsdat(13,k) > r0_1_bmiss) usage=103._r_kind
              if (mxtmob  .and. maxtmint(1,k) > r0_1_bmiss) usage=103._r_kind
              if (mitmob  .and. maxtmint(2,k) > r0_1_bmiss) usage=103._r_kind
              if (howvob  .and. owave(1,k) > r0_1_bmiss) usage=103._r_kind
              if (cldchob  .and. cldceilh(1,k) > r0_1_bmiss) usage=103._r_kind

              if (sfctype) then 
                 if (i_gsdsfc_uselist==1 ) then
                    if (kx==188 .or. kx==195 .or. kx==288.or.kx==295)  &
                    call apply_gsd_sfcuselist(kx,obstype,c_station_id,c_prvstg,c_sprvstg, &
                                            usage)
                 else
                    call get_usagerj(kx,obstype,c_station_id,c_prvstg,c_sprvstg, &
                                            dlon_earth,dlat_earth,idate,t4dv-toff,      &
                                            obsdat(5,k),obsdat(6,k),usage)
                 endif
              endif

              if ((kx>129.and.kx<140).or.(kx>229.and.kx<240) ) then
                 call get_aircraft_usagerj(kx,obstype,c_station_id,usage)
              endif
              if(plevs(k) < 0.0001_r_kind) then
                 write(*,*) 'warning: obs pressure is too small:',kx,k,plevs(k)
                 cycle
              endif

              if(ncnumgrp(nc) > 0 .and. .not.lhilbert )then                 ! default cross validation on
                 if(mod(ndata+1,ncnumgrp(nc))== ncgroup(nc)-1)usage=ncmiter(nc)
              end if

! Flag regional MAP wind above 400mb for monitoring 
              if(regional .and. kx==227 .and. obsdat(1,k)<400._r_kind ) usage=r100
 
! don't use MESONET psfc obs if  8th character of station id is "x")
              if( kx==188 .and. psob .and. sidchr(8)=='x' ) usage=r100


! Get information from surface file necessary for conventional data here
              call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)

              if(lhilbert) & 
                  call accum_hilbertcurve(usage,c_station_id,c_prvstg,c_sprvstg, &
                       dlat_earth,dlon_earth,dlat,dlon,t4dv,toff,nc,kx,iout)


!             Extract pressure level and quality marks
              dlnpob=log(plevs(k))  ! ln(pressure in cb)

!             Set inflate_error logical based on qm flag
              inflate_error=.false.
              if (qm==3 .or. qm==7) inflate_error=.true.
 
!             Temperature
              if(tob) then
                 ppb=obsdat(1,k)
                 call errormod(pqm,tqm,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
                 toe=obserr(3,k)*errout
                 qtflg=tvflg(k) 
                 if (inflate_error) toe=toe*r1_2
                 if(ppb < r100)toe=toe*r1_2
                 if (aircraft_t_bc .and. kx==130 .and. ppb>=500.0_r_kind) toe=toe*r10
                 cdata_all(1,iout)=toe                     ! temperature error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)

                 if (kx == id_bias_t) then
                    cdata_all(5,iout)=obsdat(3,k)+t0c+conv_bias_t   ! temperature ob.+bias
                 else
                    cdata_all(5,iout)=obsdat(3,k)+t0c               ! temperature ob.
                 endif

                 cdata_all(6,iout)=rstation_id             ! station id
                 cdata_all(7,iout)=t4dv                    ! time
                 cdata_all(8,iout)=nc                      ! type
                 cdata_all(9,iout)=qtflg                   ! qtflg (virtual temperature flag)
                 cdata_all(10,iout)=tqm(k)                 ! quality mark
                 cdata_all(11,iout)=obserr(3,k)            ! original obs error            
                 cdata_all(12,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=12         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(13,iout)=idomsfc                ! dominate surface type
                 cdata_all(14,iout)=tsavg                  ! skin temperature
                 cdata_all(15,iout)=ff10                   ! 10 meter wind factor
                 cdata_all(16,iout)=sfcr                   ! surface roughness
                 cdata_all(17,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(18,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(19,iout)=stnelev                ! station elevation (m)
                 cdata_all(20,iout)=obsdat(4,k)            ! observation height (m)
                 cdata_all(21,iout)=zz                     ! terrain height at ob location
                 cdata_all(22,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(23,iout)=r_sprvstg(1,1)         ! subprovider name
                 cdata_all(24,iout)=obsdat(10,k)           ! cat
                 cdata_all(25,iout)=var_jb(3,k)            ! non linear qc for T
                 if (aircraft_t_bc_pof .or. aircraft_t_bc .or.aircraft_t_bc_ext) then
                    cdata_all(26,iout)=aircraftwk(1,k)     ! phase of flight
                    cdata_all(27,iout)=aircraftwk(2,k)     ! vertical velocity
                    cdata_all(28,iout)=idx                 ! index of temperature bias
                 end if
                 if(perturb_obs)cdata_all(nreal,iout)=ran01dom()*perturb_fact ! t perturbation
                 if (twodvar_regional) &
                    call adjust_error(cdata_all(17,iout),cdata_all(18,iout),cdata_all(11,iout),cdata_all(1,iout))

!             Winds 
              else if(uvob) then 
                 call errormod(pqm,wqm,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
                 woe=obserr(5,k)*errout
                 if (inflate_error) woe=woe*r1_2
                 if(obsdat(1,k) < r50)woe=woe*r1_2
                 selev=stnelev
                 oelev=obsdat(4,k)
                 if(kx >= 280 .and. kx < 300 )then
                    oelev=r10+selev
                    if (kx == 280 )then
                       it29=nint(hdr(8))
                       if(it29 == 522 .or. it29 == 523 .or. it29 == 531)then
!                         oelev=r20+selev
                          oelev=r20
                       end if
                    end if
 
                    if (kx == 282) oelev=r20+selev
                    if (kx == 285 .or. kx == 289 .or. kx == 290) then
                       oelev=selev
                       selev=zero
                    endif
                 else
                    if((kx >= 221 .and.  kx <= 229) &
                       .and. selev >= oelev) oelev=r10+selev
                 end if

!                Rotate winds to rotated coordinate
                 uob=obsdat(5,k)
                 vob=obsdat(6,k)
                 !* thin new VAD wind and generate VAD superob
                 if(kx==224.and.newvad)then
                         klev=k+5 !*average over 6 points
                       !  klev=k    !* no average
                         if(klev>levs) cycle loop_readsb
                         diffuu=obsdat(5,k)-fcstdat(1,k)
                         diffvv=obsdat(6,k)-fcstdat(2,k)
                         if(sqrt(diffuu**2+diffvv**2)>10.0_r_kind) cycle loop_k_levs
                         if(abs(diffvv)>8.0_r_kind) cycle loop_k_levs
                        !if(abs(diffvv)>5.0.and.oelev<5000.0.and.fcstdat(3,k)>276.3) cycle loop_k_levs
                         if(oelev>7000.0_r_kind) cycle loop_k_levs
                         if(abs(diffvv)>5.0_r_kind.and.oelev<5000.0_r_kind) cycle loop_k_levs
                        ! write(6,*)'sliu diffuu,vv::',diffuu, diffvv
                         uob=0.0
                         vob=0.0
                         oelev=0.0
                         tkk=0
                         do ikkk=k,klev
                           diffhgt=obsdat(4,ikkk)-obsdat(4,k)
                           if(diffhgt<301.0_r_kind)then
                           uob=uob+obsdat(5,ikkk)
                           vob=vob+obsdat(6,ikkk)
                           oelev=oelev+obsdat(4,ikkk)
                           tkk=tkk+1
                           end if
                         end do
                         uob=uob/tkk
                         vob=vob/tkk
                         oelev=oelev/tkk

                         diffuu=5.0_r_kind;diffvv=5.0_r_kind
                         diffhgt=0.0_r_kind
                         do ikkk=k,klev
                           diffuu=abs(obsdat(5,ikkk)-uob)
                           if(diffhgt<diffuu)diffhgt=diffuu
                           diffvv=abs(obsdat(6,ikkk)-vob)
                           if(diffhgt<diffvv)diffhgt=diffvv
                         end do

                     if(diffhgt>5.0_r_kind)cycle LOOP_K_LEVS !* if u-u_avg>5.0, reject
                     if(tkk<3) cycle LOOP_K_LEVS      !* obs numb<3, reject
                     !* unreasonable observation, will fix this in QC package
                     if(sqrt(uob**2+vob**2)>60.0_r_kind)cycle LOOP_readsb
                 end if

                 if(regional)then
                    u0=uob
                    v0=vob
                    call rotate_wind_ll2xy(u0,v0,uob,vob,dlon_earth,dlon,dlat)
                    if(diagnostic_reg) then
                       call rotate_wind_xy2ll(uob,vob,u00,v00,dlon_earth,dlon,dlat)
                       nvtest=nvtest+1
                       disterr=sqrt((u0-u00)**2+(v0-v00)**2)
                       vdisterrmax=max(vdisterrmax,disterr)
                    end if
                 endif

                 cdata_all(1,iout)=woe                     ! wind error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=oelev                   ! height of observation
                 cdata_all(6,iout)=uob                     ! u obs
                 cdata_all(7,iout)=vob                     ! v obs
                 cdata_all(8,iout)=rstation_id             ! station id
                 cdata_all(9,iout)=t4dv                    ! time
                 cdata_all(10,iout)=nc                     ! type
                 cdata_all(11,iout)=selev                  ! station elevation
                 cdata_all(12,iout)=wqm(k)                 ! quality mark
                 cdata_all(13,iout)=obserr(5,k)            ! original obs error
                 cdata_all(14,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=14         ! save INDEX of where usage 
                                                           ! is stored for hilbertcurve cross validation (if requested)
                 cdata_all(15,iout)=idomsfc                ! dominate surface type
                 cdata_all(16,iout)=tsavg                  ! skin temperature
                 cdata_all(17,iout)=ff10                   ! 10 meter wind factor
                 cdata_all(18,iout)=sfcr                   ! surface roughness
                 cdata_all(19,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(20,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(21,iout)=zz                     ! terrain height at ob location
                 cdata_all(22,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(23,iout)=r_sprvstg(1,1)         ! subprovider name
                 cdata_all(24,iout)=obsdat(10,k)           ! cat
                 cdata_all(25,iout)=var_jb(5,k)            ! non linear qc parameter
                 if(perturb_obs)then
                    cdata_all(26,iout)=ran01dom()*perturb_fact ! u perturbation
                    cdata_all(27,iout)=ran01dom()*perturb_fact ! v perturbation
                 endif
 
              else if(spdob) then 
                 woe=obserr(5,k)
                 if (inflate_error) woe=woe*r1_2
                 elev=r20
                 oelev=obsdat(4,k)
                 if(kx == 260 .or. kx == 261) elev = oelev ! Nacelle and tower wind speed

                 cdata_all(1,iout)=woe                     ! wind error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=obsdat(5,k)             ! u obs
                 cdata_all(6,iout)=obsdat(6,k)             ! v obs
                 cdata_all(7,iout)=rstation_id             ! station id
                 cdata_all(8,iout)=t4dv                    ! time
                 cdata_all(9,iout)=nc                      ! type
                 cdata_all(10,iout)=elev                   ! elevation of observation
                 cdata_all(11,iout)=wqm(k)                 ! quality mark
                 cdata_all(12,iout)=obserr(5,k)            ! original obs error
                 cdata_all(13,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=13         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(14,iout)=idomsfc                ! dominate surface type
                 cdata_all(15,iout)=tsavg                  ! skin temperature
                 cdata_all(16,iout)=ff10                   ! 10 meter wind factor
                 cdata_all(17,iout)=sfcr                   ! surface roughness
                 cdata_all(18,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(19,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(20,iout)=stnelev                ! station elevation (m)
                 cdata_all(21,iout)=zz                     ! terrain height at ob location
                 cdata_all(22,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(23,iout)=r_sprvstg(1,1)         ! subprovider name

!             Surface pressure 
              else if(psob) then

                 poe=obserr(1,k)*one_tenth                  ! convert from mb to cb
                 if (inflate_error) poe=poe*r1_2
                 cdata_all(1,iout)=poe                     ! surface pressure error (cb)
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude

                 cdata_all(4,iout)=exp(dlnpob)             ! pressure (in cb)

                 cdata_all(5,iout)=obsdat(4,k)             ! surface height
                 cdata_all(6,iout)=obsdat(3,k)+t0c         ! surface temperature
                 cdata_all(7,iout)=rstation_id             ! station id
                 cdata_all(8,iout)=t4dv                    ! time
                 cdata_all(9,iout)=nc                      ! type
                 cdata_all(10,iout)=pqm(k)                 ! quality mark
                 cdata_all(11,iout)=obserr(1,k)*one_tenth  ! original obs error (cb)
                 cdata_all(12,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=12         ! save INDEX of where usage is stored 
                                                           ! for hilbertcurve cross validation (if requested)
                 cdata_all(13,iout)=idomsfc                ! dominate surface type
                 cdata_all(14,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(15,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(16,iout)=stnelev                ! station elevation (m)
                 cdata_all(17,iout)=zz                     ! terrain height at ob location
                 cdata_all(18,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(19,iout)=r_sprvstg(1,1)         ! subprovider name
                 cdata_all(20,iout)=var_jb(1,k)            ! non linear qc b parameter 
                 if(perturb_obs)cdata_all(21,iout)=ran01dom()*perturb_fact ! ps perturbation
                 if (twodvar_regional) &
                    call adjust_error(cdata_all(14,iout),cdata_all(15,iout),cdata_all(11,iout),cdata_all(1,iout))

!             Specific humidity 
              else if(qob) then
                 qmaxerr=emerr
                 call errormod(pqm,qqm,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
                 qoe=obserr(2,k)*one_tenth*errout
                 if (inflate_error) then
                    qmaxerr=emerr*r0_7; qoe=qoe*r1_2
                 end if
                 qobcon=obsdat(2,k)*convert
                 tdry=r999
                 if (tqm(k)<lim_tqm) tdry=(obsdat(3,k)+t0c)/(one+fv*qobcon)
                 cdata_all(1,iout)=qoe                     ! q error   
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=qobcon                  ! q ob
                 cdata_all(6,iout)=rstation_id             ! station id
                 cdata_all(7,iout)=t4dv                    ! time
                 cdata_all(8,iout)=nc                      ! type
                 cdata_all(9,iout)=qmaxerr                 ! q max error
                 cdata_all(10,iout)=tdry                   ! dry temperature (obs is tv)
                 cdata_all(11,iout)=qqm(k)                 ! quality mark
                 cdata_all(12,iout)=obserr(2,k)*one_tenth  ! original obs error
                 cdata_all(13,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=13         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(14,iout)=idomsfc                ! dominate surface type
                 cdata_all(15,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(16,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(17,iout)=stnelev                ! station elevation (m)
                 cdata_all(18,iout)=obsdat(4,k)            ! observation height (m)
                 cdata_all(19,iout)=zz                     ! terrain height at ob location
                 cdata_all(20,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(21,iout)=r_sprvstg(1,1)         ! subprovider name
                 cdata_all(22,iout)=obsdat(10,k)           ! cat
                 cdata_all(23,iout)=var_jb(2,k)            ! non linear qc b parameter
                 if(perturb_obs)cdata_all(24,iout)=ran01dom()*perturb_fact ! q perturbation
                 if (twodvar_regional) &
                    call adjust_error(cdata_all(15,iout),cdata_all(16,iout),cdata_all(12,iout),cdata_all(1,iout))
 
!             Total precipitable water (ssm/i)
              else if(pwob) then

                 pwoe=obserr(7,k)
                 pwmerr=pwoe*three
                 cdata_all(1,iout)=pwoe                    ! pw error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=obsdat(7,k)             ! pw obs
                 cdata_all(5,iout)=rstation_id             ! station id
                 cdata_all(6,iout)=t4dv                    ! time
                 cdata_all(7,iout)=nc                      ! type
                 cdata_all(8,iout)=pwmerr                  ! pw max error
                 cdata_all(9,iout)=pwq                     ! quality mark
                 cdata_all(10,iout)=obserr(7,k)            ! original obs error
                 cdata_all(11,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=11         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(12,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(13,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(14,iout)=stnelev                ! station elevation (m)
                 cdata_all(15,iout)=obsdat(1,k)            ! observation pressure (hPa)
                 cdata_all(16,iout)=obsdat(4,k)            ! observation height (m)
 

!             Conventional sst observations
              else if(sstob) then

!                Locate the observation on the analysis grid.  Get land/sea/ice
!                mask at nearest analysis grid points.
 
                 sstoe=r0_75

                 cdata_all(1,iout)=sstoe                   ! sst error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=sstdat(3,k)             ! sst obs
                 cdata_all(5,iout)=rstation_id             ! station id
                 cdata_all(6,iout)=t4dv                    ! time
                 cdata_all(7,iout)=nc                      ! type
                 cdata_all(8,iout)=sstoe*three             ! pw max error
                 cdata_all(9,iout)=sstdat(2,k)             ! depth of measurement
                 cdata_all(10,iout)=sstdat(1,k)            ! measurement type
                 cdata_all(11,iout)=sstq                   ! quality mark
                 cdata_all(12,iout)=sstdat(5,k)            ! original obs error
                 cdata_all(13,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=13         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(14,iout)=idomsfc                ! dominate surface type
                 cdata_all(15,iout)=tsavg                  ! skin temperature
                 cdata_all(16,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(17,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(18,iout)=stnelev                ! station elevation (m)

                 if( nst_gsi > 0) then
                   zob   = sstdat(2,k)
                   if (zob > 10.0) then
                      tref  = tsavg
                      dtw   = zero
                      dtc   = zero
                      tz_tr = one
                   else
                      call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
                   end if

                   cdata_all(19,iout) = tref               ! foundation temperature
                   cdata_all(20,iout) = dtw                ! dt_warm at zob
                   cdata_all(21,iout) = dtc                ! dt_cool at zob
                   cdata_all(22,iout) = tz_tr              ! d(Tz)/d(Tr)
                 end if

!          Measurement types
!             0       Ship intake
!             1       Bucket
!             2       Hull contact sensor
!             3       Reversing Thermometer
!             4       STD/CTD sensor
!             5       Mechanical BT
!             6       Expendable BT
!             7       Digital BT
!             8       Thermistor chain
!             9       Infra-red scanner
!             10      Micro-wave scanner
!             11-14   Reserved

!             Wind gusts
              else if(gustob) then
   
!                need to find out gustoe
!                gustoe=1.8
                 gustoe=1.0
                 selev=stnelev
                 oelev=obsdat(4,k)
                 if(selev == oelev)oelev=r10+selev
                 if((kx >= 280 .and. kx < 300).or.(kx >= 180 .and. kx < 200))then
                   oelev=r10+selev
                   if ((kx==280).or.(kx==180)) oelev=r20+selev
                   if ((kx==282).or.(kx==182)) oelev=r20+selev
                   if ((kx==285).or.(kx==185)) then
                      oelev=selev
                      selev=zero
                   end if

                   if ((kx==188).or.(kx==288) .or.(kx==195) .or.(kx==295)) then
!                     gustoe=2.5
                      gustoe=1.0
                      windcorr=abs(obsdat(5,k))<1.0 .and. abs(obsdat(6,k))<1.0 .and. obsdat(8,k)>10.0
                      if (windcorr) gustoe=gustoe*1.5_r_kind

                      if (abs(obsdat(8,k)-sqrt(obsdat(5,k)**2+obsdat(6,k)**2))<1.5) then
                         gustoe=gustoe*1.5_r_kind
                      end if
                   end if
                 end if
                 if (inflate_error) gustoe=gustoe*1.5_r_kind

                 cdata_all(1,iout)=gustoe                  ! wind gusts error (cb)
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=oelev                   ! observation height
                 cdata_all(6,iout)=obsdat(8,k)             ! wind gusts obs
                 cdata_all(7,iout)=rstation_id             ! station id
                 cdata_all(8,iout)=t4dv                    ! time
                 cdata_all(9,iout)=nc                      ! type
                 cdata_all(10,iout)=gustoe*three           ! max error
                 cdata_all(11,iout)=gustqm                 ! quality mark
                 cdata_all(12,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=12         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(13,iout)=idomsfc                ! dominate surface type
                 cdata_all(14,iout)=tsavg                  ! skin temperature
                 cdata_all(15,iout)=ff10                   ! 10 meter wind factor
                 cdata_all(16,iout)=sfcr                   ! surface roughness
                 cdata_all(17,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(18,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(19,iout)=selev                  ! station elevation (m)
                 cdata_all(20,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(21,iout)=r_sprvstg(1,1)         ! subprovider name

!             Visibility
              else if(visob) then

                 visoe=4000.0  ! temporarily
                 if ((kx==283).or.(kx==183)) visoe=4500.0
                 if (inflate_error) visoe=visoe*r1_2

                 cdata_all(1,iout)=visoe                   ! visibility error (cb)
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=obsdat(9,k)             ! visibility obs
                 cdata_all(5,iout)=rstation_id             ! station id
                 cdata_all(6,iout)=t4dv                    ! time
                 cdata_all(7,iout)=nc                      ! type
                 cdata_all(8,iout)=visoe*three             ! max error
                 cdata_all(9,iout)=visqm                   ! quality mark
                 cdata_all(10,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=10         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(11,iout)=idomsfc                ! dominate surface type
                 cdata_all(12,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(13,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(14,iout)=stnelev                ! station elevation (m)
                 cdata_all(15,iout)=obsdat(4,k)            ! observation height (m)
                 cdata_all(16,iout)=zz                     ! terrain height at ob location
                 cdata_all(17,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(18,iout)=r_sprvstg(1,1)         ! subprovider name

!             2m-Dewpoint
              else if(tdob) then
                 tdoe=obserr(3,k)*r1_2
                 qobcon=obsdat(2,k)*convert
                 tdry=r999
                 if (tqm(k)<lim_tqm) tdry=(obsdat(3,k)+t0c)/(one+fv*qobcon)
                 cdata_all(1,iout)=tdoe                    ! td error   
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=obsdat(12,k)+t0c        ! td ob
                 cdata_all(6,iout)=rstation_id             ! station id
                 cdata_all(7,iout)=t4dv                    ! time
                 cdata_all(8,iout)=nc                      ! type
                 cdata_all(9,iout)=tdoe*three              ! td max error
                 cdata_all(10,iout)=tdry                   ! dry temperature (obs is tv)
                 cdata_all(11,iout)=tdqm                   ! quality mark
                 cdata_all(12,iout)=tdoe                   ! original obs error
                 cdata_all(13,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=13         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(14,iout)=idomsfc                ! dominate surface type
                 cdata_all(15,iout)=tsavg                  ! skin temperature
                 cdata_all(16,iout)=ff10                   ! 10 meter wind factor
                 cdata_all(17,iout)=sfcr                   ! surface roughness
                 cdata_all(18,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(19,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(20,iout)=stnelev                ! station elevation (m)
                 cdata_all(21,iout)=obsdat(4,k)            ! observation height (m)
                 cdata_all(22,iout)=zz                     ! terrain height at ob location
                 cdata_all(23,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(24,iout)=r_sprvstg(1,1)         ! subprovider name
                 cdata_all(25,iout)=obsdat(10,k)           ! cat

!             Maximum temperature
              else if(mxtmob) then
                 mxtmoe=obserr(3,k)
                 qtflg=one
                 cdata_all(1,iout)=mxtmoe                  ! maximum temperature error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=maxtmint(1,k)           ! maximum temperature ob.
                 cdata_all(6,iout)=rstation_id             ! station id
                 cdata_all(7,iout)=t4dv                    ! time
                 cdata_all(8,iout)=nc                      ! type
                 cdata_all(9,iout)=qtflg                   ! qtflg (virtual temperature flag)
                 cdata_all(10,iout)=mxtmqm                 ! quality mark
                 cdata_all(11,iout)=obserr(3,k)            ! original obs error
                 cdata_all(12,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=12         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(13,iout)=idomsfc                ! dominate surface type
                 cdata_all(14,iout)=tsavg                  ! skin temperature
                 cdata_all(15,iout)=ff10                   ! 10 meter wind factor
                 cdata_all(16,iout)=sfcr                   ! surface roughness
                 cdata_all(17,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(18,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(19,iout)=stnelev                ! station elevation (m)
                 cdata_all(20,iout)=obsdat(4,k)            ! observation height (m)
                 cdata_all(21,iout)=zz                     ! terrain height at ob location
                 cdata_all(22,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(23,iout)=r_sprvstg(1,1)         ! subprovider name
                 cdata_all(24,iout)=obsdat(10,k)           ! cat

!             Minimum temperature
              else if(mitmob) then
                 mitmoe=obserr(3,k)
                 qtflg=one
                 cdata_all(1,iout)=mitmoe                  ! minimum temperature error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
                 cdata_all(5,iout)=maxtmint(2,k)           ! minimum temperature ob.
                 cdata_all(6,iout)=rstation_id             ! station id
                 cdata_all(7,iout)=t4dv                    ! time
                 cdata_all(8,iout)=nc                      ! type
                 cdata_all(9,iout)=qtflg                   ! qtflg (virtual temperature flag)
                 cdata_all(10,iout)=mitmqm                 ! quality mark
                 cdata_all(11,iout)=obserr(3,k)            ! original obs error
                 cdata_all(12,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=12         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(13,iout)=idomsfc                ! dominate surface type
                 cdata_all(14,iout)=tsavg                  ! skin temperature
                 cdata_all(15,iout)=ff10                   ! 10 meter wind factor
                 cdata_all(16,iout)=sfcr                   ! surface roughness
                 cdata_all(17,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(18,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(19,iout)=stnelev                ! station elevation (m)
                 cdata_all(20,iout)=obsdat(4,k)            ! observation height (m)
                 cdata_all(21,iout)=zz                     ! terrain height at ob location
                 cdata_all(22,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(23,iout)=r_sprvstg(1,1)         ! subprovider name
                 cdata_all(24,iout)=obsdat(10,k)           ! cat

!             Pressure at mean sea level
              else if(pmob) then

                 pmoe=obserr(1,k)*one_tenth                ! convert from mb to cb
                 if (inflate_error) pmoe=pmoe*r1_2
                 cdata_all(1,iout)=pmoe                    ! pressure at mean sea level error (cb)
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=exp(dlnpob)             ! pressure (in cb)

                 cdata_all(5,iout)=one_tenth*obsdat(13,k)  ! pressure at mean sea level (in cb)

                 cdata_all(6,iout)=obsdat(4,k)             ! surface height
                 cdata_all(7,iout)=obsdat(3,k)+t0c         ! surface temperature
                 cdata_all(8,iout)=rstation_id             ! station id
                 cdata_all(9,iout)=t4dv                    ! time
                 cdata_all(10,iout)=nc                     ! type
                 cdata_all(11,iout)=pmq(k)                 ! quality mark
                 cdata_all(12,iout)=obserr(1,k)*one_tenth  ! original obs error (cb)
                 cdata_all(13,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=13         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(14,iout)=idomsfc                ! dominate surface type
                 cdata_all(15,iout)=tsavg                  ! skin temperature
                 cdata_all(16,iout)=ff10                   ! 10 meter wind factor
                 cdata_all(17,iout)=sfcr                   ! surface roughness
                 cdata_all(18,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(19,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(20,iout)=stnelev                ! station elevation (m)
                 cdata_all(21,iout)=zz                     ! terrain height at ob location
                 cdata_all(22,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(23,iout)=r_sprvstg(1,1)         ! subprovider name
                 cdata_all(24,iout)=obsdat(10,k)           ! cat

!             Significant wave height
              else if(howvob) then

                 howvoe=0.3_r_kind                         ! use temporarily
                 cdata_all(1,iout)=howvoe                  ! significant wave height error (m)
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=exp(dlnpob)             ! pressure (in cb)

                 cdata_all(5,iout)=owave(1,k)              ! significant wave height (in m)

                 cdata_all(6,iout)=rstation_id             ! station id
                 cdata_all(7,iout)=t4dv                    ! time
                 cdata_all(8,iout)=nc                      ! type
                 cdata_all(9,iout)=howvqm                  ! quality mark
                 cdata_all(10,iout)=howvoe                 ! original obs error (m)
                 cdata_all(11,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=11         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(12,iout)=idomsfc                ! dominate surface type
                 cdata_all(13,iout)=tsavg                  ! skin temperature
                 cdata_all(14,iout)=ff10                   ! 10 meter wind factor
                 cdata_all(15,iout)=sfcr                   ! surface roughness
                 cdata_all(16,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(17,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(18,iout)=stnelev                ! station elevation (m)
                 cdata_all(19,iout)=obsdat(4,k)            ! observation height (m)
                 cdata_all(20,iout)=zz                     ! terrain height at ob location
                 cdata_all(21,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(22,iout)=r_sprvstg(1,1)         ! subprovider name
                 cdata_all(23,iout)=obsdat(10,k)           ! cat

! METAR cloud observation
              else if(metarcldobs) then
                 cdata_all(1,iout)=rstation_id    !  station ID
                 cdata_all(2,iout)=dlon           !  grid relative longitude
                 cdata_all(3,iout)=dlat           !  grid relative latitude
                 cdata_all(4,iout)=stnelev        !  station  elevation
                 if(metarvis(1,1) < r0_1_bmiss) then
                    cdata_all(5,iout)=metarvis(1,1)  !  visibility (m)
                 else
                    cdata_all(5,iout) = -99999.0_r_kind
                 endif
                 do kk=1, 6
                    if(metarcld(1,kk) < r0_1_bmiss) then
                       cdata_all(5+kk,iout) =metarcld(1,kk)  !  cloud amount
                    else
                       cdata_all(5+kk,iout) = -99999.0_r_kind
                    endif
                    if(metarcld(2,kk) < r0_1_bmiss) then
                       cdata_all(11+kk,iout)=metarcld(2,kk)  !  cloud bottom height (m)
                    else
                       cdata_all(11+kk,iout)= -99999.0_r_kind
                    endif
                 enddo
                 do kk=1, 3
                    if(metarwth(1,kk) < r0_1_bmiss) then
                       cdata_all(17+kk,iout)=metarwth(1,kk)  !  weather
                    else
                       cdata_all(17+kk,iout)= -99999.0_r_kind
                    endif
                 enddo
                 cdata_all(21,iout)=timeobs     !  time observation
                 cdata_all(22,iout)=usage
                 if (lhilbert) thisobtype_usage=22         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(23,iout)=0.0_r_kind  ! reserved for distance between obs and grid
!     Calculate dewpoint depression from surface obs, to be used later
!         with haze and ceiling logic to exclude dust-caused ceiling obs
!         from cloud analysis
                 if(metarvis(2,1)  < 1.e10_r_kind) then
                    cdata_all(24,iout)=obsdat(3,1)-metarvis(2,1)  ! temperature - dew point
                 else
                    cdata_all(24,iout)=-99999.0_r_kind  ! temperature - dew point
                 endif
! cdata_all(24,iout) and cdata_all(25,iout) will be used to save dlon and dlat
! NESDIS cloud products
              else if(goesctpobs) then
                 cdata_all(1,iout)=rstation_id    !  station ID
                 cdata_all(2,iout)=dlon                 !  grid relative longitude
                 cdata_all(3,iout)=dlat                 !  grid relative latitude
                 cdata_all(4,iout)=goescld(1,k)/100.0_r_kind   !  cloud top pressure (pa)
                 cdata_all(5,iout)=goescld(2,k)         !  cloud cover
                 cdata_all(6,iout)=goescld(3,k)         !  Cloud top temperature (K)
                 cdata_all(7,iout)=timeobs              !  time
                 cdata_all(8,iout)=usage
                 if (lhilbert) thisobtype_usage=8       ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
!             Total cloud amount
              else if(tcamtob) then
                 if (k==1) then
!                   adjust quality mark/usage parameter
                    if (trim(subset) == 'GOESND') then
                       call adjust_goescldobs(goescld(2,1),timeobs,dlat_earth,dlon_earth, &
                                  low_cldamt,low_cldamt_qc,mid_cldamt,mid_cldamt_qc, &
                                  hig_cldamt,hig_cldamt_qc,tcamt,tcamt_qc)
                    else
                       call adjust_convcldobs(cld2seq,cld2seqlevs,cldseq,cldseqlevs,metarwth,metarwthlevs, &
                                  low_cldamt,low_cldamt_qc,mid_cldamt,mid_cldamt_qc, &
                                  hig_cldamt,hig_cldamt_qc,tcamt,lcbas,tcamt_qc,lcbas_qc,ceiling,stnelev)
                    end if

                    if(tcamt_qc==15 .or. tcamt_qc==12 .or. tcamt_qc==9) usage=100._r_kind
                    tcamt_oe=20.0_r_kind
                    if(tcamt_qc==1) tcamt_oe=tcamt_oe*1.25_r_kind 
                    if(tcamt_qc==2) tcamt_oe=tcamt_oe*1.50_r_kind
                    if(tcamt_qc==3) tcamt_oe=tcamt_oe*1.75_r_kind

                    cdata_all( 1,iout)=tcamt_oe               !  obs error
                    cdata_all( 2,iout)=dlon                   !  grid relative longitude
                    cdata_all( 3,iout)=dlat                   !  grid relative latitude
                    cdata_all( 4,iout)=tcamt                  !  total cloud amount (%)
                    cdata_all( 5,iout)=rstation_id            !  station ID
                    cdata_all( 6,iout)=t4dv                   !  time
                    cdata_all( 7,iout)=nc                     !  type
                    cdata_all( 8,iout)=tcamt_qc               !  quality mark
                    cdata_all( 9,iout)=usage                  !  usage parameter
                    if (lhilbert) thisobtype_usage=9          ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                    cdata_all(10,iout)=idomsfc                !  dominate surface type
                    cdata_all(11,iout)=tsavg                  ! skin temperature
                    cdata_all(12,iout)=ff10                   ! 10 meter wind factor
                    cdata_all(13,iout)=sfcr                   ! surface roughness
                    cdata_all(14,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                    cdata_all(15,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                    cdata_all(16,iout)=stnelev                ! station elevation (m)
                    cdata_all(17,iout)=obsdat(4,k)            ! observation height (m)
                    cdata_all(18,iout)=zz                     ! terrain height at ob location
                    cdata_all(19,iout)=r_prvstg(1,1)          ! provider name
                    cdata_all(20,iout)=r_sprvstg(1,1)         ! subprovider name
                 end if

!             Base height of the lowest cloud seen
              else if(lcbasob) then
                 if (k==1) then
!                   adjust quality mark/usage parameter
                    call adjust_convcldobs(cld2seq,cld2seqlevs,cldseq,cldseqlevs,metarwth,metarwthlevs, &
                                  low_cldamt,low_cldamt_qc,mid_cldamt,mid_cldamt_qc, &
                                  hig_cldamt,hig_cldamt_qc,tcamt,lcbas,tcamt_qc,lcbas_qc,ceiling,stnelev)

                    if(lcbas_qc==15 .or. lcbas_qc==12 .or. lcbas_qc==9) usage=100._r_kind
                    lcbas_oe=4500.0_r_kind
                    if(lcbas_qc==3) lcbas_oe=lcbas_oe*1.25_r_kind
                    if(lcbas_qc==4) lcbas_oe=lcbas_oe*1.5_r_kind

                    cdata_all( 1,iout)=lcbas_oe               !  obs error
                    cdata_all( 2,iout)=dlon                   !  grid relative longitude
                    cdata_all( 3,iout)=dlat                   !  grid relative latitude
                    cdata_all( 4,iout)=lcbas                  !  base height of lowest cloud (m)
                    cdata_all( 5,iout)=rstation_id            !  station ID
                    cdata_all( 6,iout)=t4dv                   !  time
                    cdata_all( 7,iout)=nc                     !  type
                    cdata_all( 8,iout)=lcbas_qc               !  quality mark
                    cdata_all( 9,iout)=usage                  !  usage parameter
                    if (lhilbert) thisobtype_usage=9          ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                    cdata_all(10,iout)=idomsfc                !  dominate surface type
                    cdata_all(11,iout)=tsavg                  ! skin temperature
                    cdata_all(12,iout)=ff10                   ! 10 meter wind factor
                    cdata_all(13,iout)=sfcr                   ! surface roughness
                    cdata_all(14,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                    cdata_all(15,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                    cdata_all(16,iout)=stnelev                ! station elevation (m)
                    cdata_all(17,iout)=obsdat(4,k)            ! observation height (m)
                    cdata_all(18,iout)=zz                     ! terrain height at ob location
                    cdata_all(19,iout)=ceiling                ! cloud ceiling obs
                    if (trim(subset) == 'GOESND') then
!                      cdata_all(20,iout)=goescld(1,k)/100.0_r_kind   !  cloud top pressure (pa)
                       cdata_all(20,iout)=goescld(1,k)           !  cloud top pressure
                       cdata_all(21,iout)=goescld(3,k)           !  Cloud top temperature (K)
                    else
                       cdata_all(20,iout)=bmiss
                       cdata_all(21,iout)=bmiss
                    end if
                    cdata_all(22,iout)=r_prvstg(1,1)          ! provider name
                    cdata_all(23,iout)=r_sprvstg(1,1)         ! subprovider name
                 end if

!             Cloud ceiling height
              else if(cldchob) then

                 cldchoe=4000.0  ! temporarily
                 if (inflate_error) cldchoe=cldchoe*r1_2

                 cdata_all(1,iout)=cldchoe                 ! cloud ceiling height error (m)
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=cldceilh(1,k)           ! cloud ceiling height obs
                 cdata_all(5,iout)=rstation_id             ! station id
                 cdata_all(6,iout)=t4dv                    ! time
                 cdata_all(7,iout)=nc                      ! type
                 cdata_all(8,iout)=cldchoe*three           ! max error
                 cdata_all(9,iout)=cldchqm                 ! quality mark
                 cdata_all(10,iout)=usage                  ! usage parameter
                 if (lhilbert) thisobtype_usage=10         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
                 cdata_all(11,iout)=idomsfc                ! dominate surface type
                 cdata_all(12,iout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
                 cdata_all(13,iout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
                 cdata_all(14,iout)=stnelev                ! station elevation (m)
                 cdata_all(15,iout)=obsdat(4,k)            ! observation height (m)
                 cdata_all(16,iout)=zz                     ! terrain height at ob location
                 cdata_all(17,iout)=r_prvstg(1,1)          ! provider name
                 cdata_all(18,iout)=r_sprvstg(1,1)         ! subprovider name

              end if

!
!    End k loop over levs
           end do  LOOP_K_LEVS
        end do loop_readsb

!
!   End of bufr read loop
     enddo loop_msg
!    Close unit to bufr file
     call closbf(lunin)

!    Deallocate arrays used for thinning data
     if (.not.use_all) then
        deallocate(presl_thin)
        call del3grids
     endif

! Normal exit

  enddo loop_convinfo! loops over convinfo entry matches
  deallocate(lmsg,tab,nrep)

! Apply hilbert curve for cross validation if requested

    if(lhilbert) &
       call apply_hilbertcurve(maxobs,obstype,cdata_all(thisobtype_usage,1:maxobs))   

! Write header record and data to output file for further processing
  allocate(iloc(ndata))
  icount=0
  do i=1,maxobs
     if(isort(i) > 0)then
       icount=icount+1
       iloc(icount)=isort(i)
     end if
  end do
  if(ndata /= icount)then
     write(6,*) ' PREPBUFR: mix up in read_prepbufr ,ndata,icount ',ndata,icount
     call stop2(50)
  end if
  allocate(cdata_out(nreal,ndata))
  do i=1,ndata
     itx=iloc(i)
     do k=1,nreal
        cdata_out(k,i)=cdata_all(k,itx)
     end do
  end do
  deallocate(iloc,isort,cdata_all)

! define a closest METAR cloud observation for each grid point

  if(metarcldobs .and. ndata > 0) then
     maxobs=2000000
     allocate(cdata_all(nreal,maxobs))
     call reorg_metar_cloud(cdata_out,nreal,ndata,cdata_all,maxobs,iout)
     ndata=iout
     deallocate(cdata_out)
     allocate(cdata_out(nreal,ndata))
     do i=1,nreal
        do j=1,ndata
          cdata_out(i,j)=cdata_all(i,j)
        end do
     end do
     deallocate(cdata_all)
  endif
  call count_obs(ndata,nreal,ilat,ilon,cdata_out,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
  write(lunout) cdata_out

  deallocate(cdata_out)
  call destroy_rjlists
  call destroy_aircraft_rjlists
  if(i_gsdsfc_uselist==1) call destroy_gsd_sfcuselist
  if (lhilbert) call destroy_hilbertcurve
  if (twodvar_regional) call destroy_ndfdgrid

900 continue
  if(diagnostic_reg .and. ntest>0) write(6,*)'READ_PREPBUFR:  ',&
     'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_PREPBUFR:  ',&
     'nvtest,vdisterrmax=',ntest,vdisterrmax

  if (ndata == 0) then 
     call closbf(lunin)
     write(6,*)'READ_PREPBUFR:  closbf(',lunin,')'
  endif

  close(lunin)

  close(55)

! End of routine
  return

end subroutine read_prepbufr

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!
! !ROUTINE:  sonde_ext -level enhancemnt for raob
!
! !INTERFACE:
!
subroutine sonde_ext(obsdat,tpc,qcmark,obserr,drfdat,levsio,kx,vtcd)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sonde_ext                   level enhancemnt for raob
!   prgmmr: wu               org: np22                date: 2013-05-17
!
! abstract:  This routine adds bogus raob so that at least one report
!            at each model layer, by interpolate between a significant
!            report and the neighboring obs 
!
! program history log:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
! !USES:

  use kinds, only: r_kind,r_single,r_double,i_kind
  use constants, only: zero,one,one_tenth
  use guess_grids, only:  ges_psfcavg,ges_prslavg
  use gridmod, only: nsig
  use obsmod, only: bmiss

  implicit none

! !INPUT PARAMETERS:
  integer(i_kind)                                  , intent(in   ) ::kx
  real(r_double)                                   , intent(in   ) ::vtcd

! !INPUT/OUTPUT PARAMETERS:
  integer(i_kind)                                  , intent(inout) ::levsio
  real(r_double),dimension(13,255), intent(inout) :: obsdat
  real(r_double),dimension(8,255), intent(inout) :: drfdat,qcmark,obserr
  real(r_double),dimension(255,20), intent(inout) :: tpc

  real(r_kind) wim,wi
  real(r_kind),dimension(nsig) :: prsltmp,dpmdl
  integer(i_kind) i,j,k,levs
  integer(i_kind) ku,kl,ll,im
  real rsig(nsig)
  integer(i_kind),dimension(255):: pqm,qqm,tqm,wqm,cat,zqm
  real(r_kind),dimension(255):: dpres,tvflg,dpobs

!!! find averaged sigma levels !!!!!!!!
  levs=levsio
  ll=levsio

  do k=1,nsig
     rsig(k)=ges_prslavg(k)/ges_psfcavg
  enddo

  do k=1,levs
     cat(k)=nint(obsdat(10,k))
  enddo


!!! find model levels at obs location in log(cb) !!!!!!!!
  do k=1,nsig
     dpmdl(k)=obsdat(1,1)*rsig(k)
     prsltmp(k)=log(dpmdl(k)*one_tenth)
  enddo
!!! find obs levels in log(cb)     !!!!!!!!
  do k=1,levs
     dpres(k)=log(obsdat(1,k)*one_tenth)
     dpobs(k)=dpres(k)
  enddo


  if(kx==120)then
     pqm(1)=nint(min(qcmark(1,1),10000.0))
     qqm(1)=nint(min(qcmark(2,1),10000.0))
     tqm(1)=nint(min(qcmark(3,1),10000.0))
     zqm(1)=nint(min(qcmark(4,1),10000.0))
     call grdcrd(dpres,levs,prsltmp(1),nsig,-1)
        do k=1,levs
           tvflg(k)=one                               ! initialize as sensible
           do j=1,20
              if (tpc(k,j)==vtcd) tvflg(k)=zero       ! reset flag if virtual
              if (tpc(k,j)>=bmiss) exit               ! end of stack
           end do
        end do

        do i=2,levs
           im=i-1
           pqm(i)=nint(min(qcmark(1,i),10000.0))
           qqm(i)=nint(min(qcmark(2,i),10000.0))
           tqm(i)=nint(min(qcmark(3,i),10000.0))
           zqm(i)=nint(min(qcmark(4,i),10000.0))
           if ( (cat(i)==2 .or. cat(im)==2 .or. cat(i)==5 .or. cat(im)==5) .and. &
           pqm(i)<4 .and.  pqm(im)<4    )then
              ku=dpres(i)-1
              ku=min(nsig,ku)
              kl=dpres(im)+2
              kl=max(2,kl)
              do k = kl,ku
                 ll=ll+1
                 if(ll>255)then
                    write(6,*)'error in SONDE_EXT levs > 255'
                    return
                 endif
                 obsdat(1,ll)=dpmdl(k)
                 qcmark(1,ll)  =max (qcmark(1,i),qcmark(1,im)) !PQM
                 qcmark(2,ll) = bmiss
                 qcmark(3,ll) = bmiss
                 qcmark(4,ll) = bmiss
                 qcmark(5,ll) = bmiss
                 qcmark(7,ll) = bmiss
                 do j=1,20
                    tpc(ll,j)=tpc(i,j)
                 end do
                 wim=(prsltmp(k)-dpobs(i))/(dpobs(im)-dpobs(i))
                 wi=(dpobs(im)-prsltmp(k))/(dpobs(im)-dpobs(i))
!!! find tob, only bogus if both good obs and of the same type (sensible/virtual)
                 if(  tqm(i)<4 .and.  tqm(im)<4 .and. tvflg(i)==tvflg(im) ) then
                    obsdat(3,ll)=obsdat(3,im)*wim + obsdat(3,i)*wi
                    drfdat(1,ll)  = drfdat(1,im)*wim + drfdat(1,i)*wi
                    drfdat(2,ll)  = drfdat(2,im)*wim + drfdat(2,i)*wi
                    drfdat(3,ll)  = drfdat(3,im)*wim + drfdat(3,i)*wi
                    qcmark(3,ll)  =max (qcmark(3,i),qcmark(3,im)) !TQM
                    obserr(3,ll)  =max (obserr(3,i),obserr(3,im))  ! TOE
                 endif
!!! find qob
                 if(  qqm(i)<4 .and.  qqm(im)<4  ) then
                    obsdat(2,ll)=obsdat(2,im)*wim + obsdat(2,i)*wi
                    drfdat(1,ll)  = drfdat(1,im)*wim + drfdat(1,i)*wi
                    drfdat(2,ll)  = drfdat(2,im)*wim + drfdat(2,i)*wi
                    drfdat(3,ll)  = drfdat(3,im)*wim + drfdat(3,i)*wi
                    qcmark(2,ll)  =max (qcmark(2,i),qcmark(2,im)) !QQM
                    obserr(2,ll)  =max (obserr(2,i),obserr(2,im))  ! QOE
                 endif
!!! define zob
                 if(  zqm(i)<4 .and.  zqm(im)<4  ) then
                    obsdat(4,ll)=obsdat(4,im)*wim + obsdat(4,i)*wi
                 else
                    obsdat(4,ll)=max(obsdat(4,im),obsdat(4,i))
                 endif
                 qcmark(4,ll)  =max (qcmark(4,i),qcmark(4,im)) !ZQM

              enddo !kl,ku
           endif !cat
        enddo !levs
!!!!!!!!! w (not used) !!!!!!!!!!!!!!!!!!!!!!!!!!!
  elseif(kx==220)then
     pqm(1)=nint(min(qcmark(1,1),10000.0))
     wqm(1)=nint(min(qcmark(5,1),10000.0))
     call grdcrd(dpres,levs,prsltmp(1),nsig,-1)
     do i=2,levs
        im=i-1
        wqm(i)=nint(min(qcmark(5,i),10000.0))
        zqm(i)=nint(min(qcmark(4,i),10000.0))
        pqm(i)=nint(min(qcmark(1,i),10000.0))
        if(  wqm(i)<4 .and.  wqm(im)<4 .and.  pqm(i)<4 .and.  pqm(im)<4 .and.&
        (cat(i)==2 .or. cat(im)==2 .or. cat(i)==5 .or. cat(im)==5) )then
           ku=dpres(i)-1
           ku=min(nsig,ku)
           kl=dpres(im)+2
           kl=max(2,kl)
           do k = kl,ku
              ll=ll+1
              if(ll>255)then
                 write(6,*)'error in SONDE_EXT levs > 255'
                 return
              endif
              obsdat(1,ll)=dpmdl(k)
              qcmark(1,ll)  =max (qcmark(1,i),qcmark(1,im)) !PQM
              qcmark(2,ll) = bmiss
              qcmark(3,ll) = bmiss
              qcmark(4,ll) = bmiss
              qcmark(5,ll) = bmiss
              qcmark(7,ll) = bmiss
              wim=(prsltmp(k)-dpobs(i))/(dpobs(im)-dpobs(i))
              wi=(dpobs(im)-prsltmp(k))/(dpobs(im)-dpobs(i))
!!! find wob (wint)
              obsdat(5,ll)=obsdat(5,im)*wim + obsdat(5,i)*wi
              obsdat(6,ll)=obsdat(6,im)*wim + obsdat(6,i)*wi
              drfdat(1,ll)  = drfdat(1,im)*wim + drfdat(1,i)*wi
              drfdat(2,ll)  = drfdat(2,im)*wim + drfdat(2,i)*wi
              drfdat(3,ll)  = drfdat(3,im)*wim + drfdat(3,i)*wi
              qcmark(5,ll)  =max (qcmark(5,i),qcmark(5,im)) !WQM
              obserr(5,ll)  =max (obserr(5,i),obserr(5,im))  ! WOE
              qcmark(1,ll)  =max (qcmark(1,i),qcmark(1,im))
!!! find zob
              if(  zqm(i)<4 .and.  zqm(im)<4  ) then
                 obsdat(4,ll)=obsdat(4,im)*wim + obsdat(4,i)*wi
              else
                 obsdat(4,ll)=max(obsdat(4,im),obsdat(4,i))
              endif
           enddo !kl,ku
        endif !cat
     enddo !levs
  endif ! 120,220

!11 change the number of levels and output
  levsio=ll

! End of routine
  return

end subroutine sonde_ext

