subroutine read_fl_hdob(nread,ndata,nodata,infile,obstype,lunout,gstime,twind,sis,&
                        prsl_full,nobs)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_fl_hdob            read obs from hdob bufr file
!   prgmmr: eliu          org: np22                date: 2013-02-05
!
! abstract:  This routine reads high-density flight-level observations and surface data 
!            from Stepped Frequency Microwave Radiometer (SFMR). The observation   
!            types read by this routine include temperature, dew-point temperature,     
!            wind direction, wind speed, surface wind speed, and total rain rate 
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain

! program history log:
!   2013-02-05  eliu     - initial coding
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-02-26  su      - add njqc as an option to choose new non linear qc
!   2016-03-15  Su      - modified the code so that the program won't stop when no subtype is found in non 
!                         linear qc error table and b table

!   2015-10-01  guo      - calc ob location once in deg
!   2020-05-04  wu       - no rotate_wind for fv3_regional
!
!   input argument list:
!     infile    - unit from which to read BUFR data
!     obstype   - observation type to process
!     lunout    - unit to which to write data for further processing
!     gstime    - analysis time in minutes from reference date 
!     twind     - input group time window (hours)
!     sis       - satellite/instrument/sensor indicator
!     prsl_full - 3d pressure on full domain grid
!
!   output argument list:
!     nread     - number of type "obstype" observations read
!     nodata    - number of individual "obstype" observations read
!     ndata     - number of type "obstype" observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
     use kinds, only: r_single,r_kind,r_double,i_kind
     use constants, only: zero,one_tenth,one,two,ten,deg2rad,t0c,half,&
         three,four,rad2deg,tiny_r_kind,huge_r_kind,r0_01,&
         r60inv,r10,r100,r2000,hvap,eps,omeps,rv,grav,r_missing
     use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
         tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
         rlats,rlons,twodvar_regional,fv3_regional
     use convinfo, only: nconvtype, &
         icuse,ictype,icsubtype,ioctype, &
         ithin_conv,rmesh_conv,pmesh_conv,pmot_conv
     use obsmod, only: perturb_obs,perturb_fact,ran01dom
     use obsmod, only: bmiss,reduce_diag
     use aircraftinfo, only: aircraft_t_bc,aircraft_t_bc_pof,aircraft_t_bc_ext
     use converr,only: etabl
     use converr_ps,only: etabl_ps,isuble_ps,maxsub_ps
     use converr_q,only: etabl_q,isuble_q,maxsub_q
     use converr_t,only: etabl_t,isuble_t,maxsub_t
     use converr_uv,only: etabl_uv,isuble_uv,maxsub_uv
     use convb_ps,only: btabl_ps
     use convb_q,only: btabl_q
     use convb_t,only: btabl_t
     use convb_uv,only: btabl_uv
     use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,time_4dvar,winlen,thin4d
     use qcmod, only: errormod,njqc
     use convthin, only: make3grids,map3grids_m,del3grids,use_all
     use ndfdgrids,only: init_ndfdgrid,destroy_ndfdgrid,relocsfcob,adjust_error
     use deter_sfc_mod, only: deter_sfc_type,deter_sfc2
     use mpimod, only: npe
                                                                                                      
     implicit none

!    Declare passed variables
     character(len=*), intent(in   ) :: infile,obstype
     character(len=20),intent(in   ) :: sis
     integer(i_kind) , intent(in   ) :: lunout
     integer(i_kind) , dimension(npe), intent(inout) :: nobs
     integer(i_kind) , intent(inout) :: nread,ndata,nodata
     real(r_kind)    , intent(in   ) :: twind
     real(r_kind)    , intent(in   ) :: gstime 
     real(r_kind)    , intent(in   ) :: prsl_full(nlat,nlon,nsig)
   
!    Declare local variables
!    Logical variables
     logical :: outside 
     logical :: inflate_error
     logical :: ltob,lqob,luvob,lspdob,lpsob
     logical :: luse

!    Character variables
     character(40) :: timestr,locstr,tmpstr,mststr,wndstr,sfmrstr  
     character(40) :: psfstr,prsstr,g10str,qcmstr  
     character(40) :: obs_region  
     character( 8) :: subset
     character( 8) :: c_prvstg,c_sprvstg
     character( 8) :: c_station_id
     character( 6) :: bulstr1,bulstr2  
     character( 6) :: obsbul(2,1)  

!    Integer variables
     integer(i_kind), parameter :: mxib  = 31
     integer(i_kind), parameter :: ietabl= 19 

     integer(i_kind) :: i,k,kl,k1,k2,j 
     integer(i_kind) :: lunin 
     integer(i_kind) :: ireadmg,ireadsb
     integer(i_kind) :: idate
     integer(i_kind) :: ilat,ilon 
     integer(i_kind) :: nlv
     integer(i_kind) :: nreal,nchanl
     integer(i_kind) :: idomsfc,isflg
     integer(i_kind) :: ithin,iout 
     integer(i_kind) :: nc,ncsave
     integer(i_kind) :: ntmatch,ntb
     integer(i_kind) :: nmsg   
     integer(i_kind) :: maxobs 
     integer(i_kind) :: itype,itypey,iecol
     integer(i_kind) :: ierr_ps,ierr_q,ierr_t,ierr_uv,ncount_ps,ncount_q,ncount_t,ncount_uv 
     integer(i_kind) :: qcm,lim_qm
     integer(i_kind) :: p_qm,g_qm,t_qm,q_qm,uv_qm,wspd_qm,ps_qm
     integer(i_kind) :: ntest,nvtest
!    integer(i_kind) :: m,itypex,lcount,iflag
     integer(i_kind) :: nlevp   ! vertical level for thinning
     integer(i_kind) :: pflag   
     integer(i_kind) :: kk,klon1,klat1,klonp1,klatp1
     integer(i_kind) :: iuse
     integer(i_kind) :: nmind
     integer(i_kind) :: nib 
 
     integer(i_kind) :: ibit(mxib)
     integer(i_kind) :: idate5(5)

     logical, allocatable,dimension(:)     :: rusage,rthin
     logical save_all
!    integer(i_kind)  numthin,numqc,numrem,numall
     integer(i_kind) pmot,iqm
     integer(i_kind) nxdata

!    Real variables
     real(r_kind), parameter :: r0_001  =  0.001_r_kind
     real(r_kind), parameter :: r1_2    =    1.2_r_kind
     real(r_kind), parameter :: r0_7    =    0.7_r_kind
     real(r_kind), parameter :: r6      =    6.0_r_kind
     real(r_kind), parameter :: r50     =   50.0_r_kind
     real(r_kind), parameter :: r1200   = 1200.0_r_kind
     real(r_kind), parameter :: emerr   =    0.2_r_kind ! RH
     real(r_kind), parameter :: missing = 1.0e+11_r_kind

     real(r_kind) :: toff,t4dv
     real(r_kind) :: rmesh
     real(r_kind) :: usage
     real(r_kind) :: woe,toe,qoe,psoe,obserr,var_jb
     real(r_kind) :: dlat,dlon,dlat_earth,dlon_earth
     real(r_kind) :: dlat_earth_deg,dlon_earth_deg
     real(r_kind) :: cdist,disterr,disterrmax,rlon00,rlat00
     real(r_kind) :: vdisterrmax,u00,v00,u0,v0
     real(r_kind) :: dx,dy,dx1,dy1,w00,w10,w01,w11
     real(r_kind) :: wdir,wspd
     real(r_kind) :: tob,uob,vob,qob,spdob,rrob
     real(r_kind) :: rhob,tdob
     real(r_kind) :: pob_mb,pob_cb,pob_pa,gob
     real(r_kind) :: psob_mb,psob_cb,psob_pa
     real(r_kind) :: qmaxerr 
     real(r_kind) :: dlnpsob,dlnpob,ppb
     real(r_kind) :: crit1,timedif,xmesh,pmesh
     real(r_kind) :: sstime,tdiff 
     real(r_kind) :: tsavg,ff10,sfcr,zz
     real(r_kind) :: es,qsat,rhob_calc,tdob_calc,tdry
     real(r_kind) :: dummy 
     real(r_kind) :: del,ediff,errmin,jbmin
     real(r_kind) :: tvflg,log100 

     real(r_kind) :: presl(nsig)
     real(r_kind) :: obstime(6,1)
     real(r_kind) :: obsloc(2,1)
     real(r_kind) :: obstmp(2,1)
     real(r_kind) :: obswnd(4,1)
     real(r_kind) :: obsfmr(2,1)
     real(r_kind) :: obsmst(3,1)
     real(r_kind) :: obsprs(1,1)
     real(r_kind) :: obspsf(1,1)
     real(r_kind) :: obsg10(1,1)
     real(r_kind) :: obsqcm(2,1)

     real(r_double) :: rstation_id
     real(r_double) :: r_prvstg(1,1),r_sprvstg(1,1)

     real(r_kind), allocatable,dimension(:,:) :: cdata_all
     real(r_kind), allocatable,dimension(:)   :: presl_thin

!    Equivalence to handle character names
     equivalence(r_prvstg(1,1),c_prvstg)
     equivalence(r_sprvstg(1,1),c_sprvstg)
     equivalence(rstation_id,c_station_id)

!    Data 
     data bulstr1  / 'BUHD' /
     data bulstr2  / 'BORG' /
     data timestr  / 'YEAR MNTH DAYS HOUR MINU SECO' /
     data sfmrstr  / 'PKSWSP TRRT' /
     data locstr   / 'CLAT CLON' /
     data tmpstr   / 'QMAT TMDB' /
     data mststr   / 'QMDD TMDP REHU' /
     data wndstr   / 'QMWN WDIR WSPD PKWDSP' /
     data prsstr   / 'PRLC' /
     data psfstr   / '' /        ! nor in the bufr yet
     data g10str   / 'GP10' /
     data qcmstr   / 'QHDOP QHDOM'/
     data lunin    / 13 /
     data ithin    / -9 /
     data rmesh    / -99.999_r_kind /
 
!------------------------------------------------------------------------------------------------

     write(6,*)'READ_FL_HDOB: begin to read flight-level high density data ...'

!    Initialize parameters

!    Set common variables
     ltob   = obstype == 't'
     luvob  = obstype == 'uv'
     lspdob = obstype == 'spd'
     lpsob  = obstype == 'ps'
     lqob   = obstype == 'q'

     nreal  = 0
     iecol  = 0
     ierr_ps  = 0
     ierr_q  = 0
     ierr_t  = 0
     ierr_uv  = 0
     var_jb=zero
     jbmin=zero
     log100=log(100._r_kind)
 
 
     lim_qm = 4
     iecol=0
     if (ltob) then
        nreal  = 25
        if (aircraft_t_bc_pof .or. aircraft_t_bc .or.aircraft_t_bc_ext) nreal=nreal+3
        iecol  =  2 
        errmin = half      ! set lower bound of ob error for T or Tv
     else if (luvob) then
        nreal  = 26
        iecol  =  4  
        errmin = one       ! set lower bound of ob error for u,v winds
     else if (lspdob) then
        nreal  = 23
        iecol  =  4  
        errmin = one
     else if (lqob) then   ! set lower bound of ob err for surface wind speed
        nreal  = 26
        iecol  =  3 
        errmin = half      ! set lower bound of ob error for moisture (RH) 
     else if (lpsob) then  
        nreal  = 23 
        iecol  =  5 
        errmin = one_tenth ! set lower bound of ob error for surface pressure 
     else 
        write(6,*) ' illegal obs type in read_fl_hdob '
        call stop2(94)
     end if
     if (perturb_obs .and. luvob) nreal = nreal+2
     if (perturb_obs .and. (ltob .or. lpsob .or. lqob)) nreal = nreal+1

     inflate_error = .false.

!    Read in entire observation error table
!    33 pressure levels & 6 variables
!    1: pressure levels [mb] 
!    2: temperature error [K]
!    3: relative humidity error*10 
!    4: wind speed error [m/s]
!    5: surface pressure error [mb] 
!    6: total precipitable water error [?]  
!     open(ietabl,file='errtable',form='formatted')
!     rewind ietabl 
!     lcount = 0
!     etabl  = 1.e9_r_kind
!     loopd : do
!        read(ietabl,100,IOSTAT=iflag) itypex
!        if( iflag /= 0 ) exit loopd
!100     format(1x,i3)
!        lcount = lcount+1
!        do k = 1,33
!           read(ietabl,110)(etabl(itypex,k,m),m=1,6)
!110        format(1x,6e12.5)
!        end do
!     end do loopd

!     if (lcount <= 0) then
!        write(6,*)'READ_FL_HDOB: obs error table not available'
!        call stop2(49) 
!     else
!        write(6,*)'READ_FL_HDOB: obs errors provided by local file errtable'   
!     endif
!
!    Check if the obs type specified in the convinfo is in the fl hdob bufr file 
!    If found, get the index (nc) from the convinfo for the specified type
     ntmatch =  0
     ncsave  =  0
     do nc = 1, nconvtype
        if (trim(ioctype(nc)) == trim(obstype))then
           if (trim(ioctype(nc)) == 'uv'  .and. ictype(nc) == 236 .or. &
               trim(ioctype(nc)) == 'spd' .and. ictype(nc) == 213 .or. &
               trim(ioctype(nc)) == 't'   .and. ictype(nc) == 136 .or. &
               trim(ioctype(nc)) == 'q'   .and. ictype(nc) == 136 .or. & 
               trim(ioctype(nc)) == 'ps'  .and. ictype(nc) == 136 ) then
               ntmatch = ntmatch+1
               ncsave  = nc
               ithin   = ithin_conv(nc)  ! 0: no thinning 1: thinning
               itype   = ictype(nc)
           end if
        end if
     enddo
     if(ntmatch == 0)then  ! Return if not specified in convinfo 
        write(6,*) ' READ_FL_HDOB: No matching obstype found in obsinfo ',obstype
        return
     else 
        nc = ncsave
        write(6,*) ' READ_FL_HDOB: Processing FL HDOB data : ', ntmatch, nc, ioctype(nc), ictype(nc), itype 
     end if

     ncount_ps=0;ncount_q=0;ncount_t=0;ncount_uv=0
!    Setup thinning parameters
     use_all = .true.
     ithin   =  ithin_conv(nc)
     if (ithin > 0) then
        rmesh   = rmesh_conv(nc)  ! horizontal mesh size
        pmesh   = pmesh_conv(nc)  ! vertical mesh size 
        use_all = .false.
        if(pmesh > zero) then
           pflag = 1
           nlevp = r1200/pmesh
        else
           pflag = 0
           nlevp = nsig
        endif
        xmesh = rmesh
        call make3grids(xmesh,nlevp)
        if (.not.use_all) then
           allocate(presl_thin(nlevp))
           if (pflag == 1) then
              do k = 1,nlevp
                 presl_thin(k) = (r1200-(k-1)*pmesh)*one_tenth
              enddo
           endif
        endif
        write(6,*)'READ_FL_HDOB: ictype(nc),rmesh,pflag,nlevp,pmesh,nc ',&
                   ioctype(nc),ictype(nc),rmesh,pflag,nlevp,pmesh,nc
     endif
     pmot=nint(pmot_conv(nc))
     if(reduce_diag .and. pmot < 2)pmot=pmot+2
     save_all=.false.
     if(pmot /= 2 .and. pmot /= 0) save_all=.true.


!------------------------------------------------------------------------------------------------

!    Go through the bufr file to find out how mant subsets to process
     nmsg   = 0
     maxobs = 0
     open(lunin,file=trim(infile),form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)

     loop_msg1: do while(ireadmg(lunin,subset,idate) >= 0)
        if(nmsg == 0) call time_4dvar(idate,toff)   ! time offset (hour)
        nmsg = nmsg+1
        loop_readsb1: do while(ireadsb(lunin) == 0)
           maxobs = maxobs+1     
        end do loop_readsb1
     end do loop_msg1
     call closbf(lunin)
     close(lunin)
     write(6,*) 'READ_FL_HDOB: total number of data found in the bufr file ',maxobs,obstype      
     write(6,*) 'READ_FL_HDOB: time offset is ',toff,' hours'

!---------------------------------------------------------------------------------------------------

!    Allocate array to hold data
     allocate(cdata_all(nreal,maxobs),rusage(maxobs),rthin(maxobs))

!    Initialize
     cdata_all = zero 
     nread     = 0
     nchanl    = 0
     ntest     = 0
     nvtest    = 0
     ilon      = 2 
     ilat      = 3 
     rusage = .true.
     rthin = .false.
     use_all=.true.

!    Open bufr file again for reading
     open(lunin,file=trim(infile),form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     ntb   = 0     
!    Loop through BUFR file
     loop_msg2: do while(ireadmg(lunin,subset,idate) >= 0)
        loop_readsb2: do while(ireadsb(lunin) == 0)

           ntb = ntb+1

!          Extract observation bulletin info
!          URNT15 --- data in Atlantic region
!          URPN15 --- data in Eastern and Central Pacific region
!          URPA15 --- data in West Pacitic region
!          KNHC   --- Air Force product
!          KWBC   --- NOAA product
!          KBIX   --- Air Force backup product
           call readlc(lunin,obsbul(1,1),bulstr1)
           call readlc(lunin,obsbul(2,1),bulstr2)
           obs_region = 'Unknown'
           if (obsbul(1,1) == 'URNT15') obs_region = 'Atlantic'
           if (obsbul(1,1) == 'URPN15') obs_region = 'East and Central Pacific' 
           if (obsbul(1,1) == 'URPA15') obs_region = 'West Pacific' 

           c_station_id = 'FL_HDOB'
           c_prvstg     = obsbul(2,1) 
           c_sprvstg    = obsbul(1,1) 

!          QC mark 9: will be monitored but not assimilated
!          QC mark 4: reject - will not be monitored nor assimilated 
!          QC mark 3: suspect
!          QC mark 2: neutral or not checked 
!          QC mark 1: good
!          QC mark 0: keep - will be always assimilated
           qcm     = 0 
           p_qm    = 0 
           g_qm    = 0 
           t_qm    = 0 
           q_qm    = 0 
           uv_qm   = 0 
           wspd_qm = 0 
           ps_qm   = 0 

!          Read observation time 
           call ufbint(lunin,obstime,6,1,nlv,timestr) 

           idate5(1) = obstime(1,1)  ! year 
           idate5(2) = obstime(2,1)  ! month
           idate5(3) = obstime(3,1)  ! day 
           idate5(4) = obstime(4,1)  ! hour
           idate5(5) = obstime(5,1)  ! minute

           call w3fs21(idate5,nmind)
           t4dv = real((nmind-iwinbgn),r_kind)*r60inv
           sstime = real(nmind,r_kind)
           tdiff  = (sstime-gstime)*r60inv

           if (l4dvar.or.l4densvar) then
              if (t4dv < zero .OR. t4dv > winlen) cycle loop_readsb2
           else
              if (abs(tdiff)>twind) cycle loop_readsb2
           endif
           nread = nread+2

!          Read QC control flag for HDOB positional data
!          QHDOP: 0  all parameters of nominal accuracy
!                 1  lat/lon questionable
!                 2  geopotential altitude or static pressure questionable
!                 3  both lat/lon abd GA/PS questionable

           call ufbint(lunin,obsqcm,2,1,nlv,qcmstr)
           call upftbv(lunin,"QHDOP",obsqcm(1,1),mxib,ibit,nib)
           if (nib > 0) then  
               ibit(1:nib) = ibit(1:nib)-1
               if (any(ibit(1:nib) > 0)) then
                  write(6,*) 'READ_FL_HDOB: bad positional data ... toss away'
                  cycle loop_readsb2
               endif
           else ! will keep for further QC check
              write(6,*) 'READ_FL_HDOB: missing QC info '
              cycle loop_readsb2
           endif

!          Read QC mark for HDOB meteorological status
!          QHDOP: 0  all parameters of nominal accuracy
!                 1  T or Td questionable
!                 2  flight-level winds questionable
!                 3  SFMR parameters questionable
!                 4  T/Td and FL winds questionable
!                 5  T/Td and SFMR questionable
!                 6  FL winds and SFMR questionable
!                 9  T/Td, FL winds, and SFMR questionable

           call upftbv(lunin,"QHDOM",obsqcm(2,1),mxib,ibit,nib)
           if (nib > 0) then
               ibit(1:nib) = ibit(1:nib)-1
               if (any(ibit(1:nib) == 1)) then   ! for T/Td
                  t_qm    = 0 
                  q_qm    = 4 
               endif
               if (any(ibit(1:nib) == 2)) then   ! for uv 
                  uv_qm   = 4 
               endif
               if (any(ibit(1:nib) == 3)) then   ! for SFMR data 
                  wspd_qm = 4 
               endif
               if (any(ibit(1:nib) == 4)) then   ! for T/Td and uv
                  t_qm    = 4 
                  q_qm    = 4 
                  uv_qm   = 4 
               endif
               if (any(ibit(1:nib) == 5)) then   ! for T/Td and SFMR data
                  t_qm    = 4 
                  q_qm    = 4 
                  wspd_qm = 4 
               endif
               if (any(ibit(1:nib) == 6)) then   ! for uv and SFMR data
                  uv_qm   = 4 
                  wspd_qm = 4 
               endif
               if (any(ibit(1:nib) == 9)) then   ! for T/Td, uv, and SFMR data
                  t_qm    = 4 
                  q_qm    = 4 
                  uv_qm   = 4 
                  wspd_qm = 4 
               endif
           else
              write(6,*) 'READ_FL_HDOB: missing QC info'
           endif

           usage = zero                ! will be considered for assimilation
                                       ! subject to further QC in setupt subroutine
           iuse  = icuse(nc)           ! assimilation flag 
           if (iuse <=0) usage = r100  ! will be monitored but not assimilated

!          Read observation location (lat/lon degree) 
           call ufbint(lunin,obsloc,2,1,nlv,locstr)

           if (obsloc(1,1) == missing .or. abs(obsloc(1,1)) >  90.0_r_kind .or. &     
               obsloc(2,1) == missing .or. abs(obsloc(2,1)) > 360.0_r_kind) then 
               write(6,*) 'READ_FL_HDOB: bad lat/lon values: ', obsloc(1,1),obsloc(2,1)              
               cycle loop_readsb2     
           endif
           if (obsloc(2,1) < 0.0_r_kind) obsloc(2,1) = obsloc(2,1) + 360.0_r_kind
           dlon_earth_deg = obsloc(2,1)
           dlat_earth_deg = obsloc(1,1)
           dlon_earth = obsloc(2,1)*deg2rad ! degree to radian
           dlat_earth = obsloc(1,1)*deg2rad ! degree to radian

!          Convert obs lat/lon to rotated coordinate and check 
!          if the obs is outside of the regional domain
           if (regional) then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
              if (diagnostic_reg) then
                 call txy2ll(dlon,dlat,rlon00,rlat00)
                 ntest      = ntest+1
                 cdist      = sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                             (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
                 cdist      = max(-one,min(cdist,one))
                 disterr    = acos(cdist)*rad2deg
                 disterrmax = max(disterrmax,disterr)
              end if
              if(outside) cycle loop_readsb2
           else
              dlon = dlon_earth
              dlat = dlat_earth
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif

!          Read flight-level pressure [Pa] and convert to [cb]
           call ufbint(lunin,obsprs,1,1,nlv,prsstr)

           if (obsprs(1,1) >= missing .or. &
               obsprs(1,1) .gt. 110000.0_r_kind .or. obsprs(1,1) .lt. 5000.0_r_kind) then            
               write(6,*) 'READ_FL_HDOB: bad flight-level pressure [pa] values: ', obsprs(1,1)          
               cycle loop_readsb2     
           endif
           pob_pa = obsprs(1,1)         ! [Pa]
           pob_mb = obsprs(1,1)*r0_01   ! convert {Pa] to [mb]
           pob_cb = obsprs(1,1)*r0_001  ! convert [Pa] to [cb]
           dlnpob = log(pob_cb)         ! [cb] 

!          Read flight-level geopotential height [(m/s)**2] and convert to height [m]       
           call ufbint(lunin,obsg10,1,1,nlv,g10str)
           if (obsg10(1,1) == missing) then                                  
               write(6,*) 'READ_FL_HDOB: bad flight-level geopotential height [(m/s)**2] values: ', obsg10(1,1)
               cycle loop_readsb2 
           endif    
           gob = obsg10(1,1)/grav  !  convert to height [m]     
  
!          Get observation error from error table
           ppb = max(zero,min(pob_mb,r2000))
           if(.not. njqc) then
              if(ppb >= etabl(itype,1,1)) k1 = 1
              do kl = 1,32
                 if(ppb >= etabl(itype,kl+1,1) .and. ppb <= etabl(itype,kl,1)) k1 = kl
              end do
              if(ppb <= etabl(itype,33,1)) k1 = 5
              k2 = k1+1
              ediff = etabl(itype,k2,1)-etabl(itype,k1,1)
              if (abs(ediff) > tiny_r_kind) then
                 del = (ppb-etabl(itype,k1,1))/ediff
              else
                 del = huge_r_kind
              endif
              del    = max(zero,min(del,one))
              obserr = (one-del)*etabl(itype,k1,iecol)+del*etabl(itype,k2,iecol)
              obserr = max(obserr,errmin)
           endif
!         Read extrapolated surface pressure [pa] and convert to [cb]
           dlnpsob = log100         ! default (1000mb)
           if (lpsob) then
              call ufbint(lunin,obspsf,1,1,nlv,psfstr)
              if (obspsf(1,1) >= missing .or. &
                  obspsf(1,1) .gt. 110000.0_r_kind .or. obspsf(1,1) .lt. 5000.0_r_kind) then                   
                  write(6,*) 'READ_FL_HDOB: bad surface pressure [pa] values: ', obspsf(1,1)               
                  cycle loop_readsb2
              endif
              psob_pa = obspsf(1,1)         ! [Pa]
              psob_mb = obspsf(1,1)*r0_01   ! convert {Pa] to [mb]
              psob_cb = obspsf(1,1)*r0_001  ! convert [Pa] to [cb]
              dlnpsob = log(psob_cb)        ! [cb]
!             Get observation error from error table
              if (njqc) then
                 ppb = max(zero,min(pob_mb,r2000))
                 itypey=itype
                 ierr_ps=0
                 do i =1,maxsub_ps
                    if( icsubtype(nc) == isuble_ps(itypey,i) ) then
                       ierr_ps=i+1
                       exit
                    else if( i == maxsub_ps .and. icsubtype(nc)  /= isuble_ps(itypey,i)) then
                       ncount_ps=ncount_ps+1
                       do j=1,maxsub_ps
                          if(isuble_ps(itypey,j) ==0 ) then
                             ierr_ps=j+1
                             exit
                          endif
                       enddo
                       if (ncount_ps ==1) then
                          write(6,*) 'READ_FL_HDOB,WARNING!!psob: cannot find subtype in the error,&
                                      table,itype,iosub=',itypey,icsubtype(nc)
                          write(6,*) 'read error table at colomn subtype as 0, error table column= ',ierr_ps
                       endif
                    endif
                 enddo
                 if(ppb >= etabl_ps(itypey,1,1)) k1 = 1
                 do kl = 1,32
                    if(ppb >= etabl_ps(itypey,kl+1,1) .and. ppb <= etabl_ps(itypey,kl,1)) k1 = kl
                 end do
                 if(ppb <= etabl_ps(itypey,33,1)) k1 = 5
                 k2 = k1+1
                 ediff = etabl_ps(itypey,k2,1)-etabl_ps(itypey,k1,1)
                 if (abs(ediff) > tiny_r_kind) then
                    del = (ppb-etabl_ps(itypey,k1,1))/ediff
                 else
                    del = huge_r_kind
                 endif
                 del    = max(zero,min(del,one))
                 obserr = (one-del)*etabl_ps(itypey,k1,ierr_ps)+del*etabl_ps(itypey,k2,ierr_ps)
                 var_jb=(one-del)*btabl_ps(itypey,k1,ierr_ps)+del*btabl_ps(itypey,k2,ierr_ps)
                 obserr = max(obserr,errmin)
                 var_jb=max(var_jb,jbmin)
              endif
           endif
!          Convert raw temperature data to T or Tv     
!          Read temperature [K],dew pointer temperature [K] and related QC marks 
!          If both T and Td are available and in good condition, then calculate
!          virtual temperature and pass it to setupt.  Otherwise, The dry airs 
!          temperature will be used
           if (ltob) then
              qcm = t_qm 
              if (qcm > 3) usage = r100  
              call ufbint(lunin,obstmp,2,1,nlv,tmpstr)
              call ufbint(lunin,obsmst,3,1,nlv,mststr)
              tob  = obstmp(2,1)  ! airs temperature [K] 
              tdob = obsmst(2,1)  ! dew point temperature [K]
              rhob = obsmst(3,1)  ! relative humidity (%)
              if (tob >= missing .or. tob <= 170.0_r_kind .or. tob >= 320.0_r_kind) then
                 cycle loop_readsb2
              endif
              if (tdob >= missing) then
                 tvflg = one   ! tob is sensible temperature
                 qob   = bmiss 
                 rhob  = bmiss 
                 tob   = obstmp(2,1)
              else
                 tvflg = 0     ! tob is virtual temperature temperature
!                Calculate specific humidity from tob and td
                 if (rhob >= missing) then  
!                   Calculate RH [%] since rhob is missing
                    rhob_calc = exp((one-tob/tdob)*(hvap/rv)/tob) ! e.g. rh=0.98
                    call fpvsx_ad(tob,es,dummy,dummy,.false.)
                    qsat = eps*es/(pob_cb-omeps*es)
                    rhob = rhob_calc   ! calculate RH (%) since rhob is missing
                    qob  = rhob*qsat
                 else
                    call fpvsx_ad(tob,es,dummy,dummy,.false.)
                    qsat      = eps*es/(pob_cb-omeps*es)
                    tdob_calc = tob*(one-tob*log(rhob/100))   ! for comparison
                    qob       = rhob*qsat
                 endif
                 tob = tob*(1.0_r_kind+0.61_r_kind*qob)  ! conver t to tv
              endif
!             Get observation error from error table
              if (njqc) then
                 ppb = max(zero,min(pob_mb,r2000))
                 itypey=itype
                 ierr_t=0
                 do i =1,maxsub_t
                    if( icsubtype(nc)  == isuble_t(itypey,i) ) then
                       ierr_t=i+1
                       exit
                    else if( i == maxsub_t .and. icsubtype(nc)  /= isuble_t(itypey,i)) then
                       ncount_t=ncount_t+1
                       do j=1,maxsub_t
                          if(isuble_t(itypey,j) ==0 ) then
                             ierr_t=j+1
                             exit
                          endif
                       enddo
                       if(ncount_t ==1) then
                          write(6,*) 'READ_FL_HDOB,WARNING!! tob:cannot find subtyep in the error table,&
                                      itype,iosub=',itype,icsubtype(nc) 
                          write(6,*) 'read error table at colomn subtype as 0,error table column=',ierr_t
                       endif
                    endif
                 enddo
                 if(ppb >= etabl_t(itypey,1,1)) k1 = 1
                 do kl = 1,32
                    if(ppb >= etabl_t(itypey,kl+1,1) .and. ppb <= etabl_t(itypey,kl,1)) k1 = kl
                 end do
                 if(ppb <= etabl_t(itypey,33,1)) k1 = 5
                 k2 = k1+1
                 ediff = etabl_t(itypey,k2,1)-etabl_t(itypey,k1,1)
                 if (abs(ediff) > tiny_r_kind) then
                    del = (ppb-etabl_t(itypey,k1,1))/ediff
                 else
                    del = huge_r_kind
                 endif
                 del    = max(zero,min(del,one))
                 obserr = (one-del)*etabl_t(itypey,k1,ierr_t)+del*etabl_t(itypey,k2,ierr_t)
                 var_jb = (one-del)*btabl_t(itypey,k1,ierr_t)+del*btabl_t(itypey,k2,ierr_t)
                 obserr = max(obserr,errmin)
                 var_jb=max(var_jb,jbmin)
              endif
           endif
!          Convert raw moisture data from dew point temperature to specific humidity
!          Read temperature,dew point temperature,relative humidity,              
!          related QC mark and then calculate specific humidity
           if (lqob) then
              qcm = q_qm
              if (qcm > 3) usage = r100 
              call ufbint(lunin,obstmp,2,1,nlv,tmpstr)
              call ufbint(lunin,obsmst,3,1,nlv,mststr)
              tob  = obstmp(2,1)  ! dry airs temperature [K] 
              tdob = obsmst(2,1)  ! dew point temperature [K] 
              rhob = obsmst(3,1)  ! relative humidity (%) 
              tdry = tob       
              if (tob  >= missing .or. tdob >= missing .or. & 
                  tob  <= 170.0_r_kind .or. tob  >= 320.0_r_kind .or. & 
                  tdob <= 170.0_r_kind .or. tdob >= 320.0_r_kind) then
                 cycle loop_readsb2
              endif
!             Calculate specific humidity from relative humidity if abailable
              if (rhob >= missing) then 
                 rhob_calc = exp((one-tob/tdob)*(hvap/rv)/tob) ! e.g. rh=0.98
                 call fpvsx_ad(tob,es,dummy,dummy,.false.)
                 qsat = eps*es/(pob_cb-omeps*es)
                 rhob = rhob_calc   ! calculate RH (%) since rhob is missing          
                 qob  = rhob*qsat
              else
                 call fpvsx_ad(tob,es,dummy,dummy,.false.)
                 qsat      = eps*es/(pob_cb-omeps*es)
                 tdob_calc = tob*(one-tob*log(rhob/100))   ! for comparison
                 qob       = rhob*qsat
              endif 
!             Get observation error from error table
              if (njqc) then
                 ppb = max(zero,min(pob_mb,r2000))
                 itypey=itype
                 ierr_q=0
                 do i =1,maxsub_q
                    if( icsubtype(nc)  == isuble_q(itypey,i) ) then
                       ierr_q=i+1
                       exit
                    else if( i == maxsub_q .and. icsubtype(nc)  /= isuble_q(itypey,i)) then
                       ncount_q=ncount_q+1
                       do j=1,maxsub_q
                          if(isuble_q(itypey,j) ==0 ) then
                             ierr_q=j+1
                             exit
                          endif
                       enddo
                       if( ncount_q ==1 ) then
                          write(6,*) 'READ_FL_HDOB,WARNING!! qob:cannot find subtyep in the error table,&
                                      itype,iosub=',itype,icsubtype(nc) 
                          write(6,*) 'read error table at colomn subtype as 0,error table column=',ierr_q
                       endif
                    endif
                 enddo
                 if(ppb >= etabl_q(itypey,1,1)) k1 = 1
                 do kl = 1,32
                    if(ppb >= etabl_q(itypey,kl+1,1) .and. ppb <= etabl_q(itypey,kl,1)) k1 = kl
                 end do 
                 if(ppb <= etabl_q(itypey,33,1)) k1 = 5
                 k2 = k1+1 
                 ediff = etabl_q(itypey,k2,1)-etabl_q(itypey,k1,1)
                 if (abs(ediff) > tiny_r_kind) then
                    del = (ppb-etabl_q(itypey,k1,1))/ediff
                 else
                    del = huge_r_kind
                 endif
                 del    = max(zero,min(del,one))
                 obserr = (one-del)*etabl_q(itypey,k1,ierr_q)+del*etabl_q(itypey,k2,ierr_q)
                 var_jb = (one-del)*btabl_q(itypey,k1,ierr_q)+del*btabl_q(itypey,k2,ierr_q)
                 obserr = max(obserr,errmin)
                 var_jb=max(var_jb,jbmin)
              endif
           endif
!          Convert raw wind data from wind direction/speed to u & v winds
!          Read wind direction [degree true], speed [m/s] and related QC mark 
!          Convert wind direction and spped to u and v wind components
           if (luvob) then
              qcm = uv_qm
              if (qcm > 3) usage=r100 
              call ufbint(lunin,obswnd,4,1,nlv,wndstr)
              wdir = obswnd(2,1)     ! degree true  
              wspd = obswnd(3,1)     ! m/s 
              if (wdir >= missing .or. wspd >= missing) cycle loop_readsb2 
              uob  = -wspd*sin(wdir*deg2rad) ! u-wind component
              vob  = -wspd*cos(wdir*deg2rad) ! v-wind component
           endif

!          Read surface wind speed [m/s] and total rain rate [mm/hr] from SFMR 
           if (lspdob) then
              qcm = wspd_qm
              if (qcm > 3) usage=r100 
              call ufbint(lunin,obsfmr,2,1,nlv,sfmrstr)
!             print*, 'PKSWSP =  ', obstype,obsfmr(1,1)
!             print*, 'TRRP   =  ', obstype,obsfmr(2,1)
              spdob = obsfmr(1,1) ! surface wind speed 
              rrob  = obsfmr(2,1) ! rain rate 
              if (spdob >= missing .or. rrob >=missing) cycle loop_readsb2
           endif
           if( lspdob .or. luvob) then             
!             Get observation error from error table
              if (njqc) then
                 ppb = max(zero,min(pob_mb,r2000))
                 itypey=itype
                 ierr_uv=0
                 do i =1,maxsub_uv
                    if( icsubtype(nc)  == isuble_uv(itypey,i) ) then
                       ierr_uv=i+1
                       exit
                    else if( i == maxsub_uv .and. icsubtype(nc)  /= isuble_uv(itypey,i)) then
                       ncount_uv=ncount_uv+1
                       do j=1,maxsub_uv
                          if(isuble_uv(itypey,j) ==0 ) then
                             ierr_uv=j+1
                             exit
                          endif
                       enddo
                       if(ncount_uv ==1) then
                          write(6,*) 'READ_FL_HDOB,WARNING!! uvob:cannot find subtyep in the error table,&
                                      itype,iosub=',itype,icsubtype(nc) 
                          write(6,*) 'read error table at colomn subtype 0,error table column=',ierr_uv
                       endif
                    endif
                 enddo
                 if(ppb >= etabl_uv(itypey,1,1)) k1 = 1
                 do kl = 1,32
                    if(ppb >= etabl_uv(itypey,kl+1,1) .and. ppb <= etabl_uv(itypey,kl,1)) k1 = kl
                 end do 
                 if(ppb <= etabl_uv(itypey,33,1)) k1 = 5
                 k2 = k1+1 
                 ediff = etabl_uv(itypey,k2,1)-etabl_uv(itypey,k1,1)
                 if (abs(ediff) > tiny_r_kind) then
                    del = (ppb-etabl_uv(itypey,k1,1))/ediff
                 else
                    del = huge_r_kind
                 endif
                 del    = max(zero,min(del,one))
                 obserr = (one-del)*etabl_uv(itypey,k1,ierr_uv)+del*etabl_uv(itypey,k2,ierr_uv)
                 var_jb = (one-del)*btabl_uv(itypey,k1,ierr_uv)+del*btabl_uv(itypey,k2,ierr_uv)
                 obserr=max(obserr,errmin)
                 var_jb=max(var_jb,jbmin)
              endif
           endif

!          Obtain information necessary for conventional data assimilation 
!          Detect surface type (isfg) and skin temperature (tsavg)
!          isflg - surface flag
!            0     sea
!            1     land
!            2     sea ice
!            3     snow
!            4     mixed
           if ( .not. twodvar_regional) then
              call deter_sfc_type(dlat_earth,dlon_earth,t4dv,isflg,tsavg)
           endif

!          Get information from surface file necessary for conventional data
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)                                                                      
!          Process data thinning procedure on good data   
           if (ithin > 0) then
              if (pflag == 0) then 
!                Interpolate guess pressure profile to observation location
                 klon1 = int(dlon) ;  klat1 = int(dlat)
                 dx    = dlon-klon1;  dy    = dlat-klat1
                 dx1   = one-dx    ;  dy1   = one-dy
                 w00   = dx1*dy1   ;  w10   = dx1*dy 
                 w01   = dx*dy1     ;  w11   = dx*dy
                 klat1 = min(max(1,klat1),nlat)
                 klon1 = min(max(0,klon1),nlon)
                 if (klon1 == 0) klon1 = nlon
                 klatp1= min(nlat,klat1+1)  
                 klonp1= klon1+1
                 if (klonp1 == nlon+1) klonp1 = 1
                 do kk = 1,nsig
                    presl(kk) = w00*prsl_full(klat1 ,klon1 ,kk) + &
                                w10*prsl_full(klatp1,klon1 ,kk) + &
                                w01*prsl_full(klat1 ,klonp1,kk) + &
                                w11*prsl_full(klatp1,klonp1,kk)
                 end do
              endif ! pflag 

!             Set data quality index for thinning
              if (thin4d) then
                 timedif = zero
              else
                 timedif = abs(t4dv-toff)
              endif
              crit1 = timedif/r6+half
              if (pflag == 0) then
                 do kk = 1,nsig
                    presl_thin(kk) = presl(kk)
                 end do
              endif

              call map3grids_m(-1,save_all,pflag,presl_thin,nlevp, &
                     dlat_earth,dlon_earth,pob_cb,crit1,ndata,&
                     luse,maxobs,rthin,.false.,.false.)


              if (.not. luse) cycle loop_readsb2

              if(rthin(ndata))usage=101._r_kind
           else
              ndata        = ndata+1
           endif ! ithin
           iout         = ndata

!-------------------------------------------------------------------------------------------------          
!          Write data into output arrays
           if (var_jb >=10.0_r_kind) var_jb=zero 
           if (qcm == 3) inflate_error = .true.

           if (lpsob) then
              qcm  = ps_qm
              psoe = obserr*one_tenth                   ! convert from mb to cb
              iqm=10
              if (inflate_error) psoe = psoe*r1_2
              if (qcm > lim_qm ) then
                 psoe = psoe*1.0e6_r_kind
              end if
              cdata_all( 1,iout)=psoe                   ! surface pressure error (cb)             
              cdata_all( 2,iout)=dlon                   ! grid relative longitude                  
              cdata_all( 3,iout)=dlat                   ! grid relative latitude          
              cdata_all( 4,iout)=exp(dlnpsob)           ! pressure (in cb)
              cdata_all( 5,iout)=zz                     ! surface height         ! use model terrian elevation from model surface file                   
              cdata_all( 6,iout)=bmiss                  ! surface temperature    ! this is not provided                                    
              cdata_all( 7,iout)=rstation_id            ! station id
              cdata_all( 8,iout)=t4dv                   ! time
              cdata_all( 9,iout)=nc                     ! type
              cdata_all(10,iout)=qcm                    ! quality mark
              cdata_all(11,iout)=obserr*one_tenth       ! original obs error (cb)          
              cdata_all(12,iout)=usage                  ! usage parameter
              cdata_all(13,iout)=idomsfc                ! dominate surface type    
              cdata_all(14,iout)=tsavg                  ! skin temperature
              cdata_all(15,iout)=ff10                   ! 10 meter wind factor   
              cdata_all(16,iout)=sfcr                   ! surface roughness
              cdata_all(17,iout)=dlon_earth_deg         ! earth relative longitude (degree)                   
              cdata_all(18,iout)=dlat_earth_deg         ! earth relative latitude (degree)                       
              cdata_all(19,iout)=gob                    ! station elevation (m)   
              cdata_all(20,iout)=zz                     ! terrain height at ob location                    
              cdata_all(21,iout)=r_prvstg(1,1)          ! provider name
              cdata_all(22,iout)=r_sprvstg(1,1)         ! subprovider name
              cdata_all(23,iout)=var_jb                 ! non linear qc b
              if(perturb_obs)cdata_all(24,iout)=ran01dom()*perturb_fact ! ps perturbation              
           endif

!          Winds --- u, v components 
           if (luvob) then
              woe = obserr
              iqm = 12
              if (pob_mb < r50)  woe = woe*r1_2
              if (inflate_error) woe = woe*r1_2
              if (qcm > lim_qm ) then
                 woe = woe*1.0e6_r_kind
              end if
              if(regional .and. .not. fv3_regional)then
                 u0 = uob
                 v0 = vob
                 call rotate_wind_ll2xy(u0,v0,uob,vob,dlon_earth,dlon,dlat)
                 if(diagnostic_reg) then
                    call rotate_wind_xy2ll(uob,vob,u00,v00,dlon_earth,dlon,dlat)
                    nvtest      = nvtest+1
                    disterr     = sqrt((u0-u00)**2+(v0-v00)**2)
                    vdisterrmax = max(vdisterrmax,disterr)
                 end if
              endif
              cdata_all( 1,iout)=woe                    ! wind error
              cdata_all( 2,iout)=dlon                   ! grid relative longitude                       
              cdata_all( 3,iout)=dlat                   ! grid relative latitude
              cdata_all( 4,iout)=dlnpob                 ! ln(pressure in cb)
              cdata_all( 5,iout)=gob                    ! height of observation (m)   
              cdata_all( 6,iout)=uob                    ! u obs
              cdata_all( 7,iout)=vob                    ! v obs
              cdata_all( 8,iout)=rstation_id            ! station id
              cdata_all( 9,iout)=t4dv                   ! time
              cdata_all(10,iout)=nc                     ! index of type in convinfo                        
              cdata_all(11,iout)=gob                    ! station elevation (m)  
              cdata_all(12,iout)=qcm                    ! quality mark
              cdata_all(13,iout)=obserr                 ! original obs error
              cdata_all(14,iout)=usage                  ! usage parameter
              cdata_all(15,iout)=idomsfc                ! dominate surface
              cdata_all(16,iout)=tsavg                  ! skin temperature
              cdata_all(17,iout)=ff10                   ! 10 meter wind
              cdata_all(18,iout)=sfcr                   ! surface roughness
              cdata_all(19,iout)=dlon_earth_deg         ! earth relative longitude (degree)                
              cdata_all(20,iout)=dlat_earth_deg         ! earth relative latitude (degree)                    
              cdata_all(21,iout)=zz                     ! terrain height at ob location             
              cdata_all(22,iout)=r_prvstg(1,1)          ! provider name
              cdata_all(23,iout)=r_sprvstg(1,1)         ! subprovider name
              cdata_all(24,iout)=qcm                    ! cat
              cdata_all(25,iout)=var_jb                 ! non linear qc 
              cdata_all(26,iout)=one
              if(perturb_obs)then
                 cdata_all(26,iout)=ran01dom()*perturb_fact ! u perturbation
                 cdata_all(27,iout)=ran01dom()*perturb_fact ! v perturbation
              endif
           endif 

!          Temperature
           if(ltob) then
              toe = obserr
              iqm = 10
              if (pob_mb < r100) toe = toe*r1_2
              if (inflate_error) toe = toe*r1_2
              if (qcm > lim_qm ) then
                 toe = toe*1.0e6_r_kind
              end if
              cdata_all( 1,iout)=toe                    ! temperature error
              cdata_all( 2,iout)=dlon                   ! grid relative longitude       
              cdata_all( 3,iout)=dlat                   ! grid relative latitude                       
              cdata_all( 4,iout)=dlnpob                 ! ln(pressure in cb)
              cdata_all( 5,iout)=tob                    ! temperature ob.                        
              cdata_all( 6,iout)=rstation_id            ! station id
              cdata_all( 7,iout)=t4dv                   ! time
              cdata_all( 8,iout)=nc                     ! ob type
              cdata_all( 9,iout)=tvflg                  ! qtflg (0=virtual temperature 1=sensible temperature)   
              cdata_all(10,iout)=qcm                    ! quality mark
              cdata_all(11,iout)=obserr                 ! original obs error
              cdata_all(12,iout)=usage                  ! usage parameter
              cdata_all(13,iout)=idomsfc                ! dominate surface type   
              cdata_all(14,iout)=tsavg                  ! skin temperature
              cdata_all(15,iout)=ff10                   ! 10 meter wind factor
              cdata_all(16,iout)=sfcr                   ! surface roughness
              cdata_all(17,iout)=dlon_earth_deg         ! earth relative longitude (degrees)   
              cdata_all(18,iout)=dlat_earth_deg         ! earth relative latitude (degrees)     
              cdata_all(19,iout)=gob                    ! station elevation (m)   
              cdata_all(20,iout)=gob                    ! observation height (m)   
              cdata_all(21,iout)=zz                     ! terrain height at ob location             
              cdata_all(22,iout)=r_prvstg(1,1)          ! provider name
              cdata_all(23,iout)=r_sprvstg(1,1)         ! subprovider name
              cdata_all(24,iout)=qcm                    ! cat
              cdata_all(25,iout)=var_jb                 ! non linear qc
              if(perturb_obs) &
                 cdata_all(26,iout)=ran01dom()*perturb_fact  ! t perturbation             
           endif 
!          Specific humidity 
           if(lqob) then
              qoe     = obserr*one_tenth  ! RH (e.g. 0.98)
              qmaxerr = emerr
              iqm = 11
              if (inflate_error) then
                 qmaxerr = emerr*r0_7 
                 qoe     = qoe*r1_2
              end if
              if (qcm > lim_qm ) then
                 qoe = qoe*1.0e6_r_kind
              end if
              cdata_all( 1,iout)=qoe                    ! q error (RH e.g. 0.98)
              cdata_all( 2,iout)=dlon                   ! grid relative longitude                    
              cdata_all( 3,iout)=dlat                   ! grid relative latitude          
              cdata_all( 4,iout)=dlnpob                 ! ln(pressure in cb)
              cdata_all( 5,iout)=qob                    ! q ob (specific humidity)             
              cdata_all( 6,iout)=rstation_id            ! station id
              cdata_all( 7,iout)=t4dv                   ! time
              cdata_all( 8,iout)=nc                     ! type
              cdata_all( 9,iout)=qmaxerr                ! q max error (RH e.g. 0.2)    
              cdata_all(10,iout)=tdry                   ! dry temperature (obsis tv)     
              cdata_all(11,iout)=qcm                    ! quality mark
              cdata_all(12,iout)=obserr*one_tenth       ! original obs error (RH e.g. 0.98)       
              cdata_all(13,iout)=usage                  ! usage parameter
              cdata_all(14,iout)=idomsfc                ! dominate surface type    
              cdata_all(15,iout)=dlon_earth_deg         ! earth relative longitude (degree)        
              cdata_all(16,iout)=dlat_earth_deg         ! earth relative latitude (degree)
              cdata_all(17,iout)=gob                    ! station elevation (m)    
              cdata_all(18,iout)=gob                    ! observation height (m)   
              cdata_all(19,iout)=zz                     ! terrain height at ob location             
              cdata_all(20,iout)=r_prvstg(1,1)          ! provider name
              cdata_all(21,iout)=r_sprvstg(1,1)         ! subprovider name
              cdata_all(22,iout)=qcm                    ! cat
              cdata_all(26,iout)=var_jb                 ! non linear qc
              if(perturb_obs) &
                 cdata_all(27,iout)=ran01dom()*perturb_fact ! q perturbation         
           endif 

!          Winds --- surface wind speed 
           if (lspdob) then
              woe = obserr
              iqm = 11
              if (inflate_error) woe = woe*r1_2
              if (qcm > lim_qm ) then
                 woe = woe*1.0e6_r_kind
              end if
              cdata_all( 1,iout)=woe                    ! wind error
              cdata_all( 2,iout)=dlon                   ! grid relative longitude             
              cdata_all( 3,iout)=dlat                   ! grid relative latitude                  
              cdata_all( 4,iout)=r_missing                ! ln(surface pressure in cb) !Since dlnpsob is not provided by SFMR, force it to be r_missing. Not used in setupspd.f90
              cdata_all( 5,iout)=spdob*sqrt(two)*half   ! u obs
              cdata_all( 6,iout)=spdob*sqrt(two)*half   ! v obs
              cdata_all( 7,iout)=rstation_id            ! station id
              cdata_all( 8,iout)=t4dv                   ! time
              cdata_all( 9,iout)=nc                     ! type
              cdata_all(10,iout)=r10                    !  elevation of observation ! 10-m wind       
              cdata_all(11,iout)=qcm                    !  quality mark 
              cdata_all(12,iout)=obserr                 !  original obs error 
              cdata_all(13,iout)=usage                  ! usage parameter 
              cdata_all(14,iout)=idomsfc                !  dominate surface type        
              cdata_all(15,iout)=tsavg                  ! skin temperature 
              cdata_all(16,iout)=ff10                   ! 10 meter wind factor     
              cdata_all(17,iout)=sfcr                   ! surface roughness 
              cdata_all(18,iout)=dlon_earth_deg         ! earth relative longitude (degree)                
              cdata_all(19,iout)=dlat_earth_deg         ! earth relative latitude (degree)                  
              cdata_all(20,iout)=gob                    !  station elevation (m)    
              cdata_all(21,iout)=zz                     !  terrain height at ob location        
              cdata_all(22,iout)=r_prvstg(1,1)          !  provider name 
              cdata_all(23,iout)=r_sprvstg(1,1)         !  subprovider name 
           endif 
           if(usage >= r100)rusage(ndata)=.false.

        end do loop_readsb2
     end do loop_msg2

!    Close unit to bufr file
     call closbf(lunin)
     close(lunin)
!    Deallocate arrays used for thinning data
     if (.not.use_all) then
        deallocate(presl_thin)
        call del3grids
     endif

     nxdata=ndata
     ndata=0
     if(nxdata > 0)then
!       numthin=0
!       numqc=0
!       numrem=0
!       do i=1,ndata
!         if(.not. rusage(i))then
!            numqc=numqc+1
!         else if(rthin(i))then
!            numthin=numthin+1
!         else
!            numrem=numrem+1
!         end if
!       end do
!       write(6,*) ' fl ',trim(ioctype(nc)),ictype(nc),icsubtype(nc),numall,&
!              numrem,numqc,numthin
!   If thinned data set quality mark to 14
        if (ithin > 0 .and. ithin <5) then
          do i=1,nxdata
             if(rthin(i))cdata_all(iqm,i)=14
          end do
        end if

!     If flag to not save thinned data is set - compress data
        if(pmot /= 1)then
          do i=1,nxdata

!         pmot=0 - all obs - thin obs
!         pmot=1 - all obs
!         pmot=2 - use obs
!         pmot=3 - use obs + thin obs
             if((pmot == 0 .and. .not. rthin(i)) .or. &
                (pmot == 2 .and. (rusage(i) .and. .not. rthin(i)))  .or. &
                (pmot == 3 .and. rusage(i))) then

                ndata=ndata+1
                if(i > ndata)then
                   do k=1,nreal
                      cdata_all(k,ndata)=cdata_all(k,i)
                   end do
                end if
             end if
           end do
        end if
      end if
      if(luvob)then
         nodata=nodata+2*ndata
      else
         nodata=nodata+nxdata
      end if

!    Write header record and data to output file for further processing
!     deallocate(etabl)

     call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)
     deallocate(cdata_all,rusage,rthin)

     if(diagnostic_reg .and. ntest>0)  write(6,*)'READ_FL_HDOB:  ',&
        'ntest,  disterrmax=', ntest,disterrmax
     if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_FL_HDOB:  ',&
        'nvtest,vdisterrmax=',ntest,vdisterrmax

     if (ndata == 0) then
        write(6,*)'READ_FL_HDOB: no data to process',obstype
     endif
     write(6,*)'READ_FL_HDOB: nreal=',nreal,obstype
     write(6,*)'READ_FL_HDOB: ntb,nread,ndata,nodata=',ntb,nread,ndata,nodata


!    End of routine
     return

end subroutine read_fl_hdob

