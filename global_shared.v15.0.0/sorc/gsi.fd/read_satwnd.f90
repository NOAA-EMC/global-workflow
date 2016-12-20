subroutine read_satwnd(nread,ndata,nodata,infile,obstype,lunout,gstime,twind,sis,&
     prsl_full,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_satwnd                    read satellite winds  
!   prgmmr: su, xiujuan      org: np23                date: 2010-10-13
!
! abstract:  This routine reads satellite winds from satellite wind dump.  
!            it also has options to thin the data by using conventional 
!            thinning programs 
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!            For the satellite ID type: 240: GOES short wave winds, 
!            241: India, 242:JMA Visible,243: EUMETSAT visible,244: AVHRR winds
!            245: GOES IR. 246: GOES WV cloud top, 247: GOES WV deep layer
!            250: JMA WV deep layer. 251:GOES visible, 252: JMA IR winds
!            253: EUMETSAT IR winds, 254: EUMETSAT WV deep layer winds
!            257,258,259: MODIS IR,WV cloud top, WV deep layer winds
!            260: VIIR IR winds
!            respectively
!            For satellite subtype: 50-70 from EUMETSAT geostationary satellites(METEOSAT) 
!                                   100-199 from JMA geostationary satellites(MTSAT)
!                                   250-299 from NESDIS geostationary satellites(GOES)
!                                   700-799 from NASA Terra and Aqua satellites
!                                   <10, 200-223 from NOAA-15, 16, 17, 18, polar
!                                   orbit and EUMESAT MetOp satellites 
!           The quality mark:  QM, the values range from 0 to 15, 0-7 used, 8-15
!                              monitored, 0 is best, when the value greater than
!                              3, the observation error needed to be enflated. 
!           THe quality markers from producer:  qifn:  QI values without
!           forecast considered, qify: QI values with forecast considered, ee:
!           Expected error      

! program history log:
!   2010-10-13 su, x.  
!   2011-08-09 pondeca - add support for twodvar option
!   2011-08-27 todling - bypass this routine when SATWND from prepbufr are used
!   2011-10-03 su      - read AVHRR wind into system and modify satellite id range 
!                        and put subset as screen criterie since AVHRR from 
!                        different satellites
!   2011-10-24         -add reading observation error in this subroutine and stop
!                       statement if no prepbufr error table available.
!   2011-12-08 Su      -modify GOES reading program for new bufrtab.005 new format, reading        
!                       SDM quality mark 
!   2011-12-20 Su      -modify to read deep layer WV winds as monitor with qm=9,considering short 
!                       wave winds as subset 1 0f 245         
!   2012-07-18 Sienkiewicz - fix for infrad IR winds monitoring north of 20N
!   2012-10-13 Su      -modify the code to assimilate GOES hourly wind, changed the error and quality control
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-02-13  parrish - set pflag=0 outside loopd to prevent runtime fatal error in debug mode.
!   2013-08-26 McCarty -modified to remove automatic rejection of AVHRR winds
!   2013-09-20  Su      - set satellite ID as satellite wind subtype
!   2014-07-16  Su      - read VIIRS winds 
!   2014-10-16  Su      -add optione for 4d thinning and option to keep thinned data  
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-02-26  su      - add njqc as an option to choose new non linear qc 
!   2015-03-23  Su      -fix array size with maximum message and subset number from fixed number to
!                        dynamic allocated array 
!   2015-02-26  Genkova - read GOES-R like winds from ASCII files & apply Sharon Nebuda's changes for GOES-R
!   2015-05-12  Genkova - reading from ASCII files removed, read GOES-R from new BUFR, keep Nebuda's GOES-R related changes 
!   2015-03-14  Nebuda  - add QC for clear air WV AMV (WVCS) from GOES type 247, removed PCT1 check not applicable to 247
!   2016-03-15  Su      - modified the code so that the program won't stop when
!                         no subtype is found in non linear qc error table and b table !                         table
!
!   input argument list:
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of satellite winds read 
!     ndata    - number of satellite winds retained for further processing
!     nodata   - number of satellite winds retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind,r_single
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
       tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
       rlats,rlons,twodvar_regional
  use qcmod, only: errormod,noiqc,njqc
  use convthin, only: make3grids,map3grids,map3grids_m,del3grids,use_all
  use convthin_time, only: make3grids_tm,map3grids_tm,map3grids_m_tm,del3grids_tm,use_all_tm
  use constants, only: deg2rad,zero,rad2deg,one_tenth,&
        tiny_r_kind,huge_r_kind,r60inv,one_tenth,&
        one,two,three,four,five,half,quarter,r60inv,r100,r2000
  use converr,only: etabl
  use converr_uv,only: etabl_uv,ptabl_uv,isuble_uv,maxsub_uv
  use convb_uv,only: btabl_uv,isuble_buv
  use obsmod, only: iadate,oberrflg,perturb_obs,perturb_fact,ran01dom,bmiss
  use convinfo, only: nconvtype,ctwind, &
       ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype, &
       ithin_conv,rmesh_conv,pmesh_conv,pmot_conv,ptime_conv, &
       id_bias_ps,id_bias_t,conv_bias_ps,conv_bias_t,use_prepb_satwnd

  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,time_4dvar,thin4d
  use deter_sfc_mod, only: deter_sfc_type,deter_sfc2
  use mpimod, only: npe
  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=20)                     ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe)        ,intent(inout) :: nobs
  real(r_kind)                          ,intent(in   ) :: twind
  real(r_kind),dimension(nlat,nlon,nsig),intent(in   ) :: prsl_full

! Declare local parameters
  real(r_kind),parameter:: r1_2= 1.2_r_kind
  real(r_kind),parameter:: r3_33= 3.33_r_kind
  real(r_kind),parameter:: r6= 6.0_r_kind
  real(r_kind),parameter:: r50= 50.0_r_kind
  real(r_kind),parameter:: r70= 70.0_r_kind
  real(r_kind),parameter:: r90= 90.0_r_kind
  real(r_kind),parameter:: r105= 105.0_r_kind
  real(r_kind),parameter:: r110= 110.0_r_kind
  real(r_kind),parameter:: r125=125.0_r_kind
  real(r_kind),parameter:: r200=200.0_r_kind
  real(r_kind),parameter:: r250=250.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r600=600.0_r_kind
  real(r_kind),parameter:: r700=700.0_r_kind
  real(r_kind),parameter:: r850=850.0_r_kind
  real(r_kind),parameter:: r199=199.0_r_kind
  real(r_kind),parameter:: r299=299.0_r_kind
  real(r_kind),parameter:: r799=799.0_r_kind
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: r10000= 10000.0_r_kind
  
  

! Declare local variables
  logical outside,inflate_error
  logical luse,ithinp
  logical,allocatable,dimension(:,:):: lmsg     ! set true when convinfo entry id found in a message

  character(70) obstr,hdrtr
  character(50) qcstr
  character(8) subset
! character(20) derdwtr,heightr
  character(8) c_prvstg,c_sprvstg
  character(8) c_station_id,stationid
  
  integer(i_kind) mxtb,nmsgmax
  integer(i_kind) ireadmg,ireadsb,iuse
  integer(i_kind) i,maxobs,idomsfc,nsattype,ncount
  integer(i_kind) nc,nx,isflg,itx,j,nchanl
  integer(i_kind) ntb,ntmatch,ncx,ncsave,ntread
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) nmind,lunin,idate,ilat,ilon,iret,k
  integer(i_kind) nreal,ithin,iout,ntmp,icount,iiout,ii
  integer(i_kind) itype,iosub,ixsub,isubsub,iobsub,itypey,ierr
  integer(i_kind) qm
  integer(i_kind) nlevp         ! vertical level for thinning
  integer(i_kind) pflag
  integer(i_kind) ntest,nvtest
  integer(i_kind) kl,k1,k2
  integer(i_kind) nmsg                ! message index
 
  integer(i_kind),dimension(nconvtype) :: ntxall 
  integer(i_kind),dimension(nconvtype+1) :: ntx  
  
  integer(i_kind),dimension(5):: idate5 
  integer(i_kind),allocatable,dimension(:):: nrep,isort,iloc
  integer(i_kind),allocatable,dimension(:,:):: tab


  integer(i_kind) ntime,itime

  real(r_kind) toff,t4dv
  real(r_kind) rmesh,ediff,usage,tdiff
  real(r_kind) u0,v0,uob,vob,dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) dlnpob,ppb,ppb2,qifn,qify,ee,ree
  real(r_kind) woe,dlat,dlon,dlat_earth,dlon_earth
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterrmax,u00,v00,uob1,vob1
  real(r_kind) del,werrmin,obserr,ppb1,var_jb,wjbmin,wjbmax
  real(r_kind) tsavg,ff10,sfcr,sstime,gstime,zz
  real(r_kind) crit1,timedif,xmesh,pmesh,pmot,ptime
  real(r_kind),dimension(nsig):: presl
  
  real(r_double),dimension(13):: hdrdat
  real(r_double),dimension(4):: obsdat
  real(r_double),dimension(3,5) :: heightdat
  real(r_double),dimension(6,4) :: derdwdat
  real(r_double),dimension(3,12) :: qcdat
  real(r_double),dimension(1,1):: r_prvstg,r_sprvstg
  real(r_kind),allocatable,dimension(:):: presl_thin
  real(r_kind),allocatable,dimension(:):: rusage 
  real(r_kind),allocatable,dimension(:,:):: cdata_all,cdata_out

! GOES-R new BUFR related variables
  character(70)               :: eham_str,prlc_str,wdir_str,wspd_str,pccf_str,solc_str,cvwd_str,cloud1_str,cloud2_str
  real(r_double),dimension(4) :: eham_dat 
  real(r_double),dimension(4) :: prlc_dat 
  real(r_double),dimension(4) :: wspd_dat 
  real(r_double),dimension(3) :: wdir_dat 
  real(r_double),dimension(2) :: pccf_dat 
  real(r_double),dimension(2) :: solc_dat,cvwd_dat
  real(r_double),dimension(4) :: cloud1_dat
  real(r_double),dimension(2,3)::cloud2_dat
  real(r_double)                 experr_norm,pct1

  real(r_double) rstation_id

! equivalence to handle character names
  equivalence(r_prvstg(1,1),c_prvstg)
  equivalence(r_sprvstg(1,1),c_sprvstg)
  equivalence(rstation_id,c_station_id)

  data hdrtr /'SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SWCM SAZA OGCE SCCF SWQM'/ ! OGCE replaces GCLONG, OGCE exists in old and new BUFR
                                                                                  ! SWQM doesn't exist in new BUFR, so qm is initialized to '2' manually
  data obstr/'HAMD PRLC WDIR WSPD'/ 
! data heightr/'MDPT '/ 
! data derdwtr/'TWIND'/
  data qcstr /' OGCE GNAP PCCF'/

! GOES-R new BUFR related variables
  data cloud1_str/'SSNX SSNY CLDP COPT'/
  data cloud2_str/'CLDMNT CLDT'/
! substitute for obstr
  data eham_str /'EHAM'/ 
  data prlc_str /'PRLC'/
  data wdir_str /'WDIR'/
  data wspd_str /'WSPD'/
! no substitute for qcstr: 1)OGCE is already in goesr_str 2)GNAP not provided in new BUFR 3)PCCF is read directly
  data pccf_str /'PCCF'/
  data solc_str /'SOLC'/
  data cvwd_str /'CVWD'/  
  
  data ithin / -9 /
  data lunin / 11 /
  data rmesh / -99.999_r_kind /

!**************************************************************************

! Return when SATWND are coming from prepbufr file
  if(use_prepb_satwnd) return

! read observation error table

  disterrmax=zero
  vdisterrmax=zero
  wjbmin=zero
  wjbmax=5.0_r_kind
  pflag=0
  var_jb=zero

! allocate(etabl(302,33,6)) ! add 2 ObsErr profiles for GOES-R IR(itype=301) and WV(itype=300) (not used yet, 2015-07-08, Genkova) 
  
! Set lower limits for observation errors
  werrmin=one
  nsattype=0
  nreal=25
  if(perturb_obs ) nreal=nreal+2
  ntread=1
  ntmatch=0
  ntx(ntread)=0
  ntxall=0
  do nc=1,nconvtype
     if( (trim(ioctype(nc)) == 'uv' .or. trim(ioctype(nc)) == 'wspd10m') .and.  ictype(nc) >=240 &
             .and. ictype(nc) <=265) then
        ntmatch=ntmatch+1
        ntxall(ntmatch)=nc
        ithin=ithin_conv(nc)
        if(ithin > 0)then
           ntread=ntread+1
           ntx(ntread)=nc
        end if
     end if
  end do
  if(ntmatch == 0)then
     write(6,*) ' READ_SATWND: no matching obstype found in obsinfo ',obstype
     return
  end if
      
!!  go through the satedump to find out how many subset to process

!! get message and subset counts

  call getcount_bufr(infile,nmsgmax,mxtb)

  allocate(lmsg(nmsgmax,ntread),tab(mxtb,3),nrep(nmsgmax))

 
  lmsg = .false.
  maxobs=0
  tab=0
  nmsg=0
  nrep=0
  ntb =0
  call closbf(lunin)
  open(lunin,file=trim(infile),form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)
  
  msg_report: do while (ireadmg(lunin,subset,idate) == 0)
!    if(trim(subset) == 'NC005012') cycle msg_report 

!    Time offset
     if(nmsg == 0) call time_4dvar(idate,toff)
     nmsg=nmsg+1
     if (nmsg>nmsgmax) then
        write(6,*)'READ_SATWND: messages exceed maximum ',nmsgmax
        call stop2(49)
     endif
     loop_report: do while (ireadsb(lunin) == 0)
        ntb = ntb+1
        maxobs=maxobs+1
        nrep(nmsg)=nrep(nmsg)+1
        if (ntb>mxtb) then
           write(6,*)'READ_SATWND: reports exceed maximum ',mxtb   
           call stop2(49)
        endif

            
           call ufbint(lunin,hdrdat,13,1,iret,hdrtr) 
          ! SWQM doesn't exist for GOES-R/new BUFR/ hence hdrdat(13)=MISSING.
          ! qm=2, instead of using hdrdat(13)(2015-07-16, Genkova)

        iobsub=0
        itype=-1
        iobsub=int(hdrdat(1))
        if(trim(subset) == 'NC005064' .or. trim(subset) == 'NC005065' .or. &
           trim(subset) == 'NC005066') then
           if( hdrdat(1) <r70 .and. hdrdat(1) >= r50) then   !the range of EUMETSAT satellite IDS
              if(hdrdat(9) == one)  then                  ! IR winds
                 itype=253
              else if(hdrdat(9) == two) then              ! visible winds
                 itype=243
              else if(hdrdat(9) == three) then            ! WV cloud top
                 itype=254
              else if(hdrdat(9) >= four) then             ! WV deep layer, monitored
                itype=254
              endif
           endif
        else if(trim(subset) == 'NC005044' .or. trim(subset) == 'NC005045' .or. &
           trim(subset) == 'NC005046') then
           if( hdrdat(1) >=r100 .and. hdrdat(1) <=r199 ) then   ! the range of JMA satellite IDS
              if(hdrdat(9) == one)  then                            ! IR winds
                 itype=252
              else if(hdrdat(9) == two) then                        ! visible winds
                 itype=242
              else if(hdrdat(9) == three) then                      ! WV cloud top
                 itype=250
              else if(hdrdat(9) >= four) then                       ! WV deep layer,monitored
                 itype=250
              endif
           endif
        else if(trim(subset) == 'NC005010' .or. trim(subset) == 'NC005011' .or. &
           trim(subset) == 'NC005012' ) then
           if( hdrdat(1) >=r250 .and. hdrdat(1) <=r299 ) then  ! the range of NESDIS satellite IDS  
              if(hdrdat(9) == one)  then                            ! IR winds
                 if(hdrdat(12) <50000000000000.0_r_kind) then
                    itype=245
                 else
                    itype=240                                       ! short wave IR winds
                 endif
              else if(hdrdat(9) == two  ) then    ! visible winds
                 itype=251
              else if(hdrdat(9) == three ) then   ! WV cloud top
                 itype=246
              else if(hdrdat(9) >= four ) then    ! WV deep layer,monitored
                 itype=247
              endif
           endif
        else if(trim(subset) == 'NC005070' .or. trim(subset) == 'NC005071'  ) then
           if( hdrdat(1) >=r700 .and. hdrdat(1) <= r799 ) then    ! the range of NASA Terra and Aqua satellite IDs
              if(hdrdat(9) == one)  then                            ! IR winds
                 itype=257
              else if(hdrdat(9) == three) then                      ! WV cloud top
                 itype=258
              else if(hdrdat(9) >= four) then                       ! WV deep layer
                 itype=259
              endif
           endif
        else if( trim(subset) == 'NC005080') then                    
           if( hdrdat(1) <10.0_r_kind .or. (hdrdat(1) >= 200.0_r_kind .and. &
               hdrdat(1) <=223.0_r_kind) ) then      ! the range of EUMETSAT and NOAA polar orbit satellite IDs  
              if(hdrdat(9) == one)  then                            ! IR winds
                 itype=244
              else
                 write(6,*) 'READ_SATWND: wrong derived method value'
              endif
           endif
        else if( trim(subset) == 'NC005019') then                   ! GOES shortwave winds
           if(hdrdat(1) >=r250 .and. hdrdat(1) <=r299 ) then   ! The range of NESDIS satellite IDS
              if(hdrdat(9) == one)  then                            ! short wave IR winds
                 itype=240
              endif
           endif
        else if( trim(subset) == 'NC005090') then                   ! VIIRS winds 
           if(hdrdat(1) >=r200 .and. hdrdat(1) <=r250 ) then   ! The range of satellite IDS
              if(hdrdat(9) == one)  then                            ! VIIRS IR winds
                 itype=260
              endif
           endif
        !GOES-R section of the 'if' statement over 'subsets' 
        else if(trim(subset) == 'NC005030' .or. trim(subset) == 'NC005031' .or. trim(subset) == 'NC005032' .or. &
                trim(subset) == 'NC005034' .or. trim(subset) == 'NC005039') then
! Commented out, because we need clarification for SWCM/hdrdat(9) from Yi Song
! NOTE: Once it is confirmed that SWCM values are sensible, apply this logic and replace lines 685-702
!                 if(hdrdat(9) == one)  then
!                    if(hdrdat(12) <50000000000000.0_r_kind) then
!                     itype=245                                      ! GOES-R IR(LW) winds
!                    else
!                     itype=240                                      ! GOES-R IR(SW) winds
!                    endif
!                 else if(hdrdat(9) == two  ) then
!                    itype=251                                       !  GOES-R VIS    winds
!                 else if(hdrdat(9) == three ) then
!                    itype=246                                       !  GOES-R CT WV  winds
!                 else if(hdrdat(9) >= four ) then 
!                    itype=247                                       !  GOES-R CS WV  winds
!                 endif

!Temporary solution replacing the commented code above
                 if(trim(subset) == 'NC005030')  then                 ! IR LW winds
                    itype=245
                 else if(trim(subset) == 'NC005039')  then            ! IR SW winds
                    itype=240                                      
                 else if(trim(subset) == 'NC005032')  then            ! VIS winds
                    itype=251
                 else if(trim(subset) == 'NC005034')  then            ! WV cloud top
                    itype=246
                 else if(trim(subset) == 'NC005031')  then            ! WV clear sky/deep layer
                    itype=247
                 endif

         endif

!  Match ob to proper convinfo type
        ncsave=0
        matchloop:do ncx=1,ntmatch
           nc=ntxall(ncx)
           if (itype /= ictype(nc)) cycle matchloop
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
           maxobs=maxobs+1
           nx=1
           if(ithin_conv(ncsave) > 0)then
              do ii=2,ntread
                 if(ntx(ii) == ncsave)nx=ii
              end do
           end if
           tab(ntb,1)=ncsave
           tab(ntb,2)=nx
           tab(ntb,3)=1
           lmsg(nmsg,nx) = .true.
        end if
     enddo loop_report
  enddo msg_report


  allocate(cdata_all(nreal,maxobs),isort(maxobs),rusage(maxobs))
  isort = 0
  cdata_all=zero
  nread=0
  ntest=0
  nvtest=0
  nchanl=0
  ilon=2
  ilat=3
  rusage=101.0_r_kind

! Open, then read date from bufr data
!!  read satellite winds one type a time

  loop_convinfo: do nx=1,ntread 
     use_all = .true.
     use_all_tm = .true.
     ithin=0
     if(nx >1) then
        nc=ntx(nx)
        ithin=ithin_conv(nc)
        if (ithin > 0 ) then
           rmesh=rmesh_conv(nc)
           pmesh=pmesh_conv(nc)
           pmot=pmot_conv(nc)
           ptime=ptime_conv(nc)
           if(pmesh > zero) then
              pflag=1
              nlevp=r1200/pmesh
           else
              pflag=0
              nlevp=nsig
           endif
           xmesh=rmesh
           if( ptime >zero) then
              use_all_tm = .false.
              ntime=6.0_r_kind/ptime                   !!  6 hour winddow
              call make3grids_tm(xmesh,nlevp,ntime)
           else
              use_all = .false.
              call make3grids(xmesh,nlevp)
           endif
           if (.not.use_all .or. .not.use_all_tm) then
              allocate(presl_thin(nlevp))
              if (pflag==1) then
                 do k=1,nlevp
                    presl_thin(k)=(r1200-(k-1)*pmesh)*one_tenth
                 enddo
              endif
           endif
           write(6,*)'READ_SATWND: ictype(nc),rmesh,pflag,nlevp,pmesh,nc ',&
                   ioctype(nc),ictype(nc),rmesh,pflag,nlevp,pmesh,nc,pmot,ptime
        endif
     endif

     call closbf(lunin)
     open(lunin,file=trim(infile),form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     ntb = 0
     nmsg = 0
     ncount=0
     loop_msg:  do while(IREADMG(lunin,subset,idate) == 0)
        nmsg = nmsg+1
        if(.not.lmsg(nmsg,nx)) then
           ntb=ntb+nrep(nmsg)
           cycle loop_msg ! no useable reports this mesage, skip ahead report count
        end if
        loop_readsb: do while(ireadsb(lunin) == 0)
           ntb = ntb+1
           nc=tab(ntb,1)
           if(nc <= 0 .or. tab(ntb,2) /= nx) cycle loop_readsb
           hdrdat=bmiss
           obsdat=bmiss
           heightdat=bmiss
           derdwdat=bmiss
           qcdat=bmiss
           eham_dat=bmiss
           prlc_dat=bmiss
           wspd_dat=bmiss
           wdir_dat=bmiss
           pccf_dat=bmiss
           solc_dat=bmiss
           cvwd_dat=bmiss
           cloud1_dat=bmiss
           cloud2_dat=bmiss
           iobsub=0
           itype=-1
           uob=bmiss
           vob=bmiss
           ppb=bmiss
           ppb1=bmiss
           ppb2=bmiss
           uob1=bmiss
           vob1=bmiss
           ee=r110
           qifn=r110
           qify=r110

           call ufbint(lunin,hdrdat,13,1,iret,hdrtr) 
           call ufbint(lunin,obsdat,4,1,iret,obstr)
        
           ppb=obsdat(2)
           if (ppb > 100000000.0_r_kind .or. hdrdat(3) >100000000.0_r_kind &
            .or. obsdat(4) > 100000000.0_r_kind) cycle loop_readsb
           if(ppb >r10000) ppb=ppb/r100
           if (ppb <r125) cycle loop_readsb    !  reject data above 125mb
           if (twodvar_regional .and. ppb <r850) cycle loop_readsb
!   reject the data with bad quality mark from SDM
           if(hdrdat(13) == 12.0_r_kind .or. hdrdat(13) == 14.0_r_kind) cycle loop_readsb      
!   Compare relative obs time with window.  If obs 
!   falls outside of window, don't use this obs
           idate5(1) = hdrdat(4)     !year
           idate5(2) = hdrdat(5)     ! month
           idate5(3) = hdrdat(6)     ! day
           idate5(4) = hdrdat(7)     ! hours
           idate5(5) = hdrdat(8)     ! minutes
           call w3fs21(idate5,nmind)
           t4dv = real((nmind-iwinbgn),r_kind)*r60inv
           sstime = real(nmind,r_kind) 
           tdiff=(sstime-gstime)*r60inv
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle loop_readsb 
           else
              if (abs(tdiff)>twind) cycle loop_readsb 
           endif
           iosub=0
           if(abs(hdrdat(2)) >r90 ) cycle loop_readsb 
           if(hdrdat(3) <zero) hdrdat(3)=hdrdat(3)+r360
           if(hdrdat(3) == r360) hdrdat(3)=hdrdat(3)-r360
           if(hdrdat(3) >r360) cycle loop_readsb 
           qm=2
           iobsub=int(hdrdat(1))
           write(stationid,'(i3)') iobsub

           ! assign types and get quality info : start
           if(trim(subset) == 'NC005064' .or. trim(subset) == 'NC005065' .or. &  
              trim(subset) == 'NC005066') then
              if( hdrdat(1) <r70 .and. hdrdat(1) >= r50) then    ! the range of EUMETSAT satellite IDs      
                 c_prvstg='EUMETSAT'
                 if(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
                 if(hdrdat(9) == one)  then                  ! IR winds
                    itype=253
                    c_station_id='IR'//stationid
                    c_sprvstg='IR'
                 else if(hdrdat(9) == two) then              ! visible winds
                    itype=243
                    c_station_id='VI'//stationid
                    c_sprvstg='VI'
                 else if(hdrdat(9) == three) then            ! WV cloud top, try to assimilate
                    itype=254                                
                    c_station_id='WV'//stationid
                    c_sprvstg='WV'
                 else if(hdrdat(9) >= four) then             ! WV deep layer,monitoring
                    itype=254
                    qm=9                                     !  quality mark as 9, means the observation error needed to be set
                    c_station_id='WV'//stationid
                    c_sprvstg='WV'
                 endif
!  get quality information
                 call ufbrep(lunin,qcdat,3,12,iret,qcstr) 
                 do j=4,9
                    if( qify <r105 .and. qifn <r105 .and. ee <r105) exit
                    if(qcdat(2,j) < r10000 .and. qcdat(3,j) <r10000) then
                       if(qcdat(2,j) == one .and. qify >r105) then
                          qify=qcdat(3,j)
                       else if(qcdat(2,j) == two .and. qifn >r105) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) ==  three .and. ee >r105) then
                          ee=qcdat(3,j)
                       endif
                    endif
                 enddo
                 if(qifn <85.0_r_kind )  then    !  qifn, QI without forecast
                    qm=15
                 endif 
              endif
           else if(trim(subset) == 'NC005044' .or. trim(subset) == 'NC005045' .or. &   ! JMA
                   trim(subset) == 'NC005046') then           
              if(hdrdat(1) >=r100 .and. hdrdat(1) <=r199 ) then   !  the range of JMA satellite IDS 
                 c_prvstg='JMA'
                 if(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
                 if(hdrdat(9) == one)  then                      ! IR winds
                    itype=252
                    c_station_id='IR'//stationid
                    c_sprvstg='IR'
                 else if(hdrdat(9) == two) then                  ! visible winds
                    itype=242
                    c_station_id='VI'//stationid
                    c_sprvstg='VI'
                 else if(hdrdat(9) == three) then                ! WV cloud top 
                    itype=250
                    c_station_id='WV'//stationid
                    c_sprvstg='WV'
                 else if(hdrdat(9) >=four) then                  ! WV deep layer,as monitoring
                    itype=250
                    qm=9
                    c_station_id='WV'//stationid
                    c_sprvstg='WV'
                 endif
! get quality information
                 call ufbrep(lunin,qcdat,3,12,iret,qcstr)
                 do j=4,9
                    if( qify <=r105 .and. qifn <r105 .and. ee <r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000) then
                       if(qcdat(2,j) == 101.0_r_kind .and. qify >r105 ) then
                          qify=qcdat(3,j)
                       else if(qcdat(2,j) == 102.0_r_kind .and. qifn >r105 ) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) == 103.0_r_kind .and. ee >r105) then
                          ee=qcdat(3,j)
                       endif   
                    endif
                 enddo 

                 if(qifn <85.0_r_kind )  then     ! qifn: QI value without forecast 
                    qm=15
                 endif 
              endif
           else if(trim(subset) == 'NC005010' .or. trim(subset) == 'NC005011' .or. &  ! NESDIS GOES 
                   trim(subset) == 'NC005012' ) then
              if(hdrdat(1) >=r250 .and. hdrdat(1) <=r299 ) then  ! the range of NESDIS satellite IDS 
                 c_prvstg='NESDIS'
                 if(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
                 if(hdrdat(9) == one)  then                            ! IR winds
                    if(hdrdat(12) <50000000000000.0_r_kind) then       ! for channel 4
                       itype=245
                       c_station_id='IR'//stationid
                       c_sprvstg='IR'
                    else
                       itype=240                                       ! short wave winds
                       c_station_id='IR'//stationid
                       c_sprvstg='IR'
                    endif
                 else if(hdrdat(9) == two ) then                       ! visible winds
                    itype=251
                    c_station_id='VI'//stationid
                    c_sprvstg='VI'
                 else if(hdrdat(9) == three) then                      ! WV cloud top
                    itype=246
                    c_station_id='WV'//stationid
                    c_sprvstg='WV'
                 else if(hdrdat(9) >= four) then                       ! WV deep layer.mornitored set in convinfo file
                     itype=247
                    c_station_id='WV'//stationid
                    c_sprvstg='WV'
                 endif
                 call ufbrep(lunin,qcdat,3,8,iret,qcstr)
! get quality information
                 do j=1,8
                    if( qify <=r105 .and. qifn <r105 .and. ee < r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000) then
                       if( qcdat(2,j) == one .and. qifn >r105 ) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) == three .and. qify >r105) then
                          qify=qcdat(3,j)
                       else if( qcdat(2,j) == four .and. ee >r105) then
                          ee=qcdat(3,j) 
                       endif  
                    endif
                 enddo
!QI not applied to CAWV for now - may in the future
                 if(qifn <85.0_r_kind .and. itype /= 247)  then
                    qm=15
                 endif
! Minimum speed requirement for CAWV of 10m/s
                 if(itype == 247 .and. obsdat(4) < 10.0_r_kind)  then
                   qm=15
                endif
              endif
           else if(trim(subset) == 'NC005070' .or. trim(subset) == 'NC005071') then  ! MODIS  
              if(hdrdat(1) >=r700 .and. hdrdat(1) <= r799 ) then
                 c_prvstg='MODIS'
                 if(hdrdat(9) == one)  then                            ! IR winds
                    itype=257
                    c_station_id='IR'//stationid
                    c_sprvstg='IR'
                 else if(hdrdat(9) == three) then                      ! WV cloud top
                    itype=258
                    c_station_id='WV'//stationid
                    c_sprvstg='WVCLOP'
                 else if(hdrdat(9) >= four) then                       ! WV deep layer
                    itype=259 
                    c_station_id='WV'//stationid
                    c_sprvstg='WVDLAYER'
                 endif
!  get quality information
                 call ufbrep(lunin,qcdat,3,8,iret,qcstr) 
                 do j=1,8
                    if( qify <=r105 .and. qifn <r105 .and. ee < r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000) then
                       if(qcdat(2,j) == one .and. qifn >r105) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) == three .and. qify >r105) then
                          qify=qcdat(3,j)
                       else if( qcdat(2,j) == four .and. ee >r105 ) then
                          ee=qcdat(3,j) 
                       endif  
                    endif
                 enddo
              endif
           else if( trim(subset) == 'NC005080') then                   ! AVHRR 
              if(hdrdat(1) <10.0_r_kind .or. (hdrdat(1) >= 200.0_r_kind .and. &
                 hdrdat(1) <=223.0_r_kind) ) then      
                 c_prvstg='AVHRR'
                 if(hdrdat(9) == one)  then                            ! IR winds
                    itype=244
                 else
                    write(6,*) 'READ_SATWND: wrong derived method value'
                 endif
! get quality information
                 call ufbrep(lunin,qcdat,3,8,iret,qcstr)
                 do j=1,6
                    if( qify <=r105 .and. qifn <r105 .and. ee <r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000 ) then
                       if(qcdat(2,j) ==  one  .and. qifn >r105) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) ==  three .and. qify >r105) then
                          qify=qcdat(3,j)
                       else if( qcdat(2,j) == four .and. ee >r105) then
                          ee=qcdat(3,j)
                       endif 
                    endif
                 enddo
              endif
           else if( trim(subset) == 'NC005019') then                   ! GOES shortwave winds 
              if(hdrdat(1) >=r250 .and. hdrdat(1) <=r299 ) then   ! The range of NESDIS satellite IDS
                 c_prvstg='NESDIS'
                 if(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
                 if(hdrdat(9) == one)  then                            ! short wave IR winds
                    itype=240
                    c_station_id='IR'//stationid
                    c_sprvstg='IR'
                 endif
! get quality information
                 call ufbrep(lunin,qcdat,3,8,iret,qcstr)
                 do j=1,6
                    if( qify <=r105 .and. qifn <r105 .and. ee <r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000 ) then
                       if(qcdat(2,j) ==  one  .and. qifn >r105) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) ==  three .and. qify >r105) then
                          qify=qcdat(3,j)
                       else if( qcdat(2,j) == four .and. ee >r105) then
                          ee=qcdat(3,j)
                       endif
                    endif
                 enddo
              endif
           else if( trim(subset) == 'NC005090') then                   ! VIIRS IR winds 
               if(hdrdat(1) >=r200 .and. hdrdat(1) <=r250 ) then   ! The range of satellite IDS
                 c_prvstg='VIIRS'
                 if(hdrdat(9) == one)  then                            ! VIIRS IR winds
                    itype=260
                    c_station_id='IR'//stationid
                    c_sprvstg='IR'
                 endif
! get quality information
                 call ufbrep(lunin,qcdat,3,8,iret,qcstr)
                 do j=1,6
                    if( qify <=r105 .and. qifn <r105 .and. ee <r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000 ) then
                       if(qcdat(2,j) ==  one  .and. qifn >r105) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) ==  three .and. qify >r105) then
                          qify=qcdat(3,j)
                       else if( qcdat(2,j) == four .and. ee >r105) then
                          ee=qcdat(3,j)
                       endif
                    endif
                 enddo
               endif
! Extra block for GOES-R winds: Start
           else if(trim(subset) == 'NC005030' .or. trim(subset) == 'NC005031' .or. trim(subset) == 'NC005032' .or. &  !IR(LW) / CS WV / VIS  GOES-R like winds        
                   trim(subset) == 'NC005034' .or. trim(subset) == 'NC005039' ) then                                  !CT WV  / IR(SW) GOES-R like winds        
              !read additional variables from new BUFR
              call ufbint(lunin,cloud1_dat,4,1,iret,cloud1_str)
              call ufbrep(lunin,cloud2_dat,2,3,iret,cloud2_str)
              call ufbrep(lunin,eham_dat,1,4,iret,eham_str)
              call ufbrep(lunin,prlc_dat,1,6,iret,prlc_str)
              call ufbrep(lunin,wdir_dat,1,6,iret,wdir_str)
              call ufbrep(lunin,wspd_dat,1,4,iret,wspd_str)
              call ufbrep(lunin,pccf_dat,1,2,iret,pccf_str)
              call ufbrep(lunin,solc_dat,1,2,iret,solc_str)
              call ufbrep(lunin,cvwd_dat,1,2,iret,cvwd_str)
              !fix obstr/'HAMD PRLC WDIR WSPD'/ (EHAM in new BUFR replaces HAMD in old BUFR)
              obsdat(1)=eham_dat(1)
              if(hdrdat(1) >=r250 .and. hdrdat(1) <=r299 ) then  ! the range of NESDIS satellite IDs
                                                                 ! The sample newBUFR has SAID=259 (GOES-15)
                                                                 ! When GOES-R SAID is assigned, pls check
                                                                 ! if this range is still valid (Genkova)) 
                 c_prvstg='NESDIS'
                 if(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
                 if(trim(subset) == 'NC005030')  then                 ! IR LW winds
                    itype=245
                    c_station_id='IR'//stationid
                    c_sprvstg='IR'
                    !write(6,*)'itype= ',itype
                 else if(trim(subset) == 'NC005039')  then            ! IR SW winds
                    itype=240                                      
                    c_station_id='IR'//stationid
                    c_sprvstg='IR'
                    !write(6,*)'itype= ',itype
                 else if(trim(subset) == 'NC005032')  then            ! VIS winds
                    itype=251
                    c_station_id='VI'//stationid
                    c_sprvstg='VI'
                    !write(6,*)'itype= ',itype
                 else if(trim(subset) == 'NC005034')  then            ! WV cloud top
                    itype=246
                    c_station_id='WV'//stationid
                    c_sprvstg='WV'
                    !write(6,*)'itype= ',itype
                 else if(trim(subset) == 'NC005031')  then            ! WV clear sky/deep layer
                    itype=247
                    c_station_id='WV'//stationid
                    c_sprvstg='WV'
                    !write(6,*)'itype= ',itype
                 endif
! get quality information
                 qify=pccf_dat(1)
                 ee=wspd_dat(2) ! NOTE: GOES-R's ee is in [m/s]
! Additional QC introduced by Sharon Nebuda (for GOES-R winds from MSG proxy images)
                 if (qifn < 80_r_kind)   qm=15 !reject data with low QI
                 if (ppb < 12500_r_kind) qm=15 !reject data above 125hPa: Trop check in setup.f90
                 experr_norm = 10.0_r_double - 0.1_r_double*ee   ! introduced by Santek/Nebuda 
                 if (wspd_dat(1) > 0.1_r_double) then  ! wspd_dat(1) is the AMV speed
                    experr_norm = experr_norm/wspd_dat(1)
                 else
                    experr_norm = 100.0_r_double
                 end if
                 if (experr_norm > 0.9_r_double) qm=15 ! reject data with EE/SPD>0.9
                 pct1=cvwd_dat(1)             ! use of pct1 (a new variable in the BUFR) is introduced by Nebuda/Genkova
                 if(itype==240 .or. itype==245 .or. itype==246 .or. itype==251) then 
                ! types 245 and 246 have been used to determine the acceptable pct1 range, but that pct1 range is applied to all GOES-R winds
           	    if (pct1 < 0.04_r_double) qm=15  
		    if (pct1 > 0.50_r_double) qm=15
		 endif
                ! winds rejected by qc dont get used
                if (qm == 15) usage=r100
                if (qm == 3 .or. qm ==7) woe=woe*r1_2
                ! set strings for diagnostic output
                if(itype==240 )  then;  c_prvstg='GOESR' ; c_sprvstg='IRSW'  ; endif
                if(itype==245 )  then;  c_prvstg='GOESR' ; c_sprvstg='IR'  ; endif
                if(itype==246 )  then;  c_prvstg='GOESR' ; c_sprvstg='WVCT'  ; endif
                if(itype==247 )  then;  c_prvstg='GOESR' ; c_sprvstg='WVCS'  ; endif
                if(itype==251 )  then;  c_prvstg='GOESR' ; c_sprvstg='VIS'  ; endif
              endif
! Extra block for GOES-R winds: End
           endif
           ! assign types and get quality info : end

           if ( qify == zero) qify=r110
           if ( qifn == zero) qifn=r110
           if ( ee == zero)   ee=r110

           nread=nread+1
           dlon_earth=hdrdat(3)*deg2rad
           dlat_earth=hdrdat(2)*deg2rad
                              
!       If regional, map obs lat,lon to rotated grid.
           if(regional)then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
              if(diagnostic_reg) then
                 call txy2ll(dlon,dlat,rlon00,rlat00)
                 ntest=ntest+1
                 cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                       (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
                 cdist=max(-one,min(cdist,one))
                 disterr=acos(cdist)*rad2deg
                 disterrmax=max(disterrmax,disterr)
              end if
              if(outside) cycle loop_readsb 
           else
              dlon=dlon_earth
              dlat=dlat_earth
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif

       

!!   detect surface type for  IR winds monitoring over land for lat greter than 20N
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
           if( .not. twodvar_regional) then
              if(itype ==245 .or. itype ==252 .or. itype ==253 .or. itype ==240) then 
                 if(hdrdat(2) >20.0_r_kind) then 
                    call deter_sfc_type(dlat_earth,dlon_earth,t4dv,isflg,tsavg)
                    if(isflg /= 0) cycle loop_readsb 
                 endif
              endif
           endif
       
!!    convert from wind direction and speed to u,v component
           uob=-obsdat(4)*sin(obsdat(3)*deg2rad)
           vob=-obsdat(4)*cos(obsdat(3)*deg2rad)
!!!  some information only has in NESDIS satellite winds
!          if(hdrdat(1) >=r200 .and. hdrdat(1) <= r299 ) then
!             call ufbseq(lunin,heightdat,3,5,iret,heightr)         
!             call ufbseq(lunin,derdwdat,6,4,iret,derdwtr)         
!             write(99,*) 'heightdat ',itype
!             write(99,101) heightdat(2,1),heightdat(2,2),heightdat(2,3),heightdat(2,4),heightdat(2,5)
!101 format(5e10.2)
!             uob1=-derdwdat(6,2)*sin(derdwdat(5,2)*deg2rad)    ! get originial wind info
!             vob1=-derdwdat(6,2)*cos(derdwdat(5,2)*deg2rad)    ! get originial wind info
!             if(itype == 245 ) then
!                ppb1=heightdat(2,1)/r100                        ! window height assignment value
!                ppb2=heightdat(2,4)/r100                        ! co2 height assignment value  
!             else if(itype == 246) then
!                ppb1=heightdat(2,3)/r100                        !  H2O height assignment value
!                ppb2=heightdat(2,4)/r100                        ! co2 height assignment value
!             endif
!          endif

!  first to get observation error from PREPBUFR observation error table
           ppb=max(zero,min(ppb,r2000))
           if (njqc) then
              itypey=itype
              ierr=0
              do i =1,maxsub_uv
                 if( icsubtype(nc) == isuble_uv(itypey,i) ) then
                    ierr=i+1
                    exit
                 else if( i == maxsub_uv .and. icsubtype(nc) /= isuble_uv(itypey,i)) then
                    ncount=ncount+1
                    do j=1,maxsub_uv
                       if(isuble_uv(itypey,j) ==0 ) then
                          ierr=j+1
                          exit
                       endif
                    enddo
                    if (ncount ==1) then
                       write(6,*) 'READ_SATWND,WARNING cannot find subtyep in the error table,&
                                   itype,iobsub=',itypey,icsubtype(nc)
                       write(6,*) 'read error table at colomn subtype as 0,error table column=',ierr
                    endif
                 endif
              enddo
              if(ppb>=etabl_uv(itypey,1,1)) k1=1
              do kl=1,32
                 if(ppb>=etabl_uv(itypey,kl+1,1).and.ppb<=etabl_uv(itypey,kl,1)) then
                    k1=kl
                    exit
                 endif
              end do
              k2=k1+1
              if(ppb<=etabl_uv(itypey,33,1)) then
                 k1=33
                 k2=33
              endif
              ediff = etabl_uv(itypey,k2,1)-etabl_uv(itypey,k1,1)
              if (abs(ediff) > tiny_r_kind) then
                 del = (ppb-etabl_uv(itypey,k1,1))/ediff
              else
                 del = huge_r_kind
              endif
              del=max(zero,min(del,one))
              obserr=(one-del)*etabl_uv(itypey,k1,ierr)+del*etabl_uv(itypey,k2,ierr)
              obserr=max(obserr,werrmin)
!  get non linear qc parameter from b table
              var_jb=(one-del)*btabl_uv(itypey,k1,ierr)+del*btabl_uv(itypey,k2,ierr)
              var_jb=max(var_jb,wjbmin)
              if (var_jb >=10.0_r_kind) var_jb=zero
!              if (itype ==245 ) then
!                write(6,*)
!                'READ_SATWND:obserr,var_jb,ppb,del,one,etabl_uv,btabl_uv=',&
!                obserr,var_jb,ppb,del,one,etabl_uv(itypey,k1,ierr),btabl_uv(itypey,k1,ierr),wjbmin,werrmin
!           endif
           else                         ! else use the ONE error table
              if(ppb>=etabl(itype,1,1)) k1=1
              do kl=1,32
                 if(ppb>=etabl(itype,kl+1,1).and.ppb<=etabl(itype,kl,1)) k1=kl
              end do
              if(ppb<=etabl(itype,33,1)) k1=33
              k2=k1+1
              ediff = etabl(itype,k2,1)-etabl(itype,k1,1)
              if (abs(ediff) > tiny_r_kind) then
                 del = (ppb-etabl(itype,k1,1))/ediff
              else
                 del = huge_r_kind
              endif
              del=max(zero,min(del,one))
              obserr=(one-del)*etabl(itype,k1,4)+del*etabl(itype,k2,4)
              obserr=max(obserr,werrmin)
           endif                    ! end of njqc

           if((itype==245 .or. itype==246) &
              .and. (trim(subset) == 'NC005010' .or. trim(subset) == 'NC005011' .or. trim(subset) == 'NC005012' )) then !only applies to AMVs from legacy algorithm (pre GOES-R)
!  using Santek quality control method,calculate the original ee value:
!  NOTE: Up until GOES-R winds algorithm, EE (expected error, ee) is reported as percent 0-100% (the higher the ee, the better the wind quality)
!  NOTE: In the new GOES-R BUFR, EE (expected error, ee) is reported in m/s (the smaller the ee, the better the wind quality)
              if(ee <r105) then
                 ree=(ee-r100)/(-10.0_r_kind)
                 if(obsdat(4) >zero) then
                    ree=ree/obsdat(4)
                 else
                    ree=two
                 endif
              else
                 ree=0.2_r_kind
              endif
              if( ppb >= 800.0_r_kind .and. ree >0.55_r_kind) then
                  qm=15
              else if (ree >0.8_r_kind) then
                  qm=15
              endif
           endif

! Reduce OE for the GOES-R winds by half following Sharon Nebuda's work
! GOES-R wind are identified/recognised here by subset, but it could be done by itype or SAID
! After completing the evaluation of GOES-R winds, REVISE this section!!!
            if(trim(subset) == 'NC005030' .or. trim(subset) == 'NC005031' .or. trim(subset) == 'NC005032' .or. &  
               trim(subset) == 'NC005034' .or. trim(subset) == 'NC005039' ) then  
               obserr=obserr/two
            endif

!         Set usage variable
           usage = 0 
           iuse=icuse(nc)
           if(iuse <= 0)usage=r100
           if(qm == 15 .or. qm == 12 .or. qm == 9)usage=r100
!           if(itype==240) then;  c_prvstg='NESDIS'   ;  c_sprvstg='IR'       ; endif
!           if(itype==242) then;  c_prvstg='JMA'      ;  c_sprvstg='VI'       ; endif
!           if(itype==243) then;  c_prvstg='EUMETSAT' ;  c_sprvstg='VI'       ; endif
!           if(itype==244) then;  c_prvstg='AVHRR'    ;  c_sprvstg='IR'       ; endif
!           if(itype==245) then;  c_prvstg='NESDIS'   ;  c_sprvstg='IR'       ; endif
!           if(itype==246) then;  c_prvstg='NESDIS'   ;  c_sprvstg='WV'       ; endif
!           if(itype==250) then;  c_prvstg='JMA'      ;  c_sprvstg='WV'       ; endif
!           if(itype==251) then;  c_prvstg='NESDIS'   ;  c_sprvstg='VI'       ; endif
!           if(itype==252) then;  c_prvstg='JMA'      ;  c_sprvstg='IR'       ; endif
!           if(itype==253) then;  c_prvstg='EUMETSAT' ;  c_sprvstg='IR'       ; endif
!           if(itype==254) then;  c_prvstg='EUMETSAT' ;  c_sprvstg='WV'       ; endif
!           if(itype==257) then;  c_prvstg='MODIS'    ;  c_sprvstg='IR'       ; endif
!           if(itype==258) then;  c_prvstg='MODIS'    ;  c_sprvstg='WVCTOP'   ; endif
!           if(itype==259) then;  c_prvstg='MODIS'    ;  c_sprvstg='WVDLAYER' ; endif
!
!           c_station_id='SATWND'

! Get information from surface file necessary for conventional data here
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)
 
!!    process the thining procedure
                
           ithin=ithin_conv(nc)
           ithinp = ithin > 0 .and. pflag /= 0
!          if(ithinp  .and. iuse >=0 )then
           if(ithinp   )then
!          Interpolate guess pressure profile to observation location
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
 
 !         Compute depth of guess pressure layersat observation location
           end if
           dlnpob=log(one_tenth*ppb)  ! ln(pressure in cb)
           ppb=one_tenth*ppb         ! from mb to cb
 !         Special block for data thinning - if requested
           if (ithin > 0 .and. iuse >=0 .and. qm <4) then
              ntmp=ndata  ! counting moved to map3gridS
 !         Set data quality index for thinning
              if (thin4d) then
                 timedif = zero
              else
                 timedif=abs(t4dv-toff)
              endif
              if(itype == 243 .or. itype == 253 .or. itype == 254) then
                 if(qifn <r105) then
                    crit1 = timedif/r6+half + four*(one-qifn/r100)*r3_33
                 else
                    crit1 = timedif/r6+half
                 endif
              else if(itype == 245 .or. itype == 246) then
                 if(qifn <r105 .and. ee <r105) then
                    crit1 = timedif/r6+half + four*(one-qifn/r100)*r3_33+(one-ee/r100)*r3_33
                 else
                    crit1 = timedif/r6+half
                 endif
              else
                 crit1 = timedif/r6+half
              endif
              if (pflag==0) then
                 do kk=1,nsig
                    presl_thin(kk)=presl(kk)
                 end do
              endif
              if (ptime >zero ) then
                 itime=int((tdiff+three)/ptime)+1
                 if(pmot <one) then
!                    call map3grids_tm(-1,pflag,presl_thin,nlevp,ntime,dlat_earth,dlon_earth,&
                    call map3grids_tm(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                                        ppb,itime,crit1,ndata,iout,ntb,iiout,luse,.false.,.false.)
                    if (.not. luse) cycle loop_readsb
                    if(iiout > 0) isort(iiout)=0
                    if (ndata > ntmp) then
                       nodata=nodata+2
                    endif
                    rusage(iout)=usage
                    isort(ntb)=iout
                 else
!                    call map3grids_m_tm(-1,pflag,presl_thin,nlevp,ntime,dlat_earth,dlon_earth,&
                    call map3grids_m_tm(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                              ppb,itime,crit1,ndata,iout,ntb,iiout,luse,maxobs,usage,rusage,.false.,.false.)
                    if (ndata > ntmp) then
                       nodata=nodata+2
                    endif
                    isort(ntb)=iout
                 endif
              else 
                 if(pmot <one) then
                    call map3grids(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                                 ppb,crit1,ndata,iout,ntb,iiout,luse,.false.,.false.)
                    if (.not. luse) cycle loop_readsb
                    if(iiout > 0) isort(iiout)=0
                    if (ndata > ntmp) then
                       nodata=nodata+2
                    endif
                    isort(ntb)=iout
                    rusage(iout)=usage
                 else
                    call map3grids_m(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                              ppb,crit1,ndata,iout,ntb,iiout,luse,maxobs,usage,rusage,.false.,.false.)
                    if (ndata > ntmp) then
                       nodata=nodata+2
                    endif
                    isort(ntb)=iout
                 endif
              endif
           else
              ndata=ndata+1
              nodata=nodata+2
              iout=ndata
              isort(ntb)=iout
              rusage(iout)=usage
           endif
           inflate_error=.false.
           if (qm==3 .or. qm==7) inflate_error=.true.
           woe=obserr
           if (inflate_error) woe=woe*r1_2
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
           cdata_all(1,iout)=woe                  ! wind error
           cdata_all(2,iout)=dlon                 ! grid relative longitude
           cdata_all(3,iout)=dlat                 ! grid relative latitude
           cdata_all(4,iout)=dlnpob               ! ln(pressure in cb)
           cdata_all(5,iout)=ee                   !  quality information 
           cdata_all(6,iout)=uob                  ! u obs
           cdata_all(7,iout)=vob                  ! v obs 
           cdata_all(8,iout)=rstation_id          ! station id 
           cdata_all(9,iout)=t4dv                 ! time
           cdata_all(10,iout)=nc                  ! index of type in convinfo file
           cdata_all(11,iout)=qifn +1000.0_r_kind*qify   ! quality mark infor  
           cdata_all(12,iout)=qm                  ! quality mark
           cdata_all(13,iout)=obserr              ! original obs error
           cdata_all(14,iout)=usage               ! usage parameter
           cdata_all(15,iout)=idomsfc             ! dominate surface type
           cdata_all(16,iout)=tsavg               ! skin temperature
           cdata_all(17,iout)=ff10                ! 10 meter wind factor
           cdata_all(18,iout)=sfcr                ! surface roughness
           cdata_all(19,iout)=dlon_earth*rad2deg  ! earth relative longitude (degrees)
           cdata_all(20,iout)=dlat_earth*rad2deg  ! earth relative latitude (degrees)
           cdata_all(21,iout)=zz                  ! terrain height at ob location
           cdata_all(22,iout)=r_prvstg(1,1)       ! provider name
           cdata_all(23,iout)=r_sprvstg(1,1)      ! subprovider name
           cdata_all(25,iout)=var_jb              ! non linear qc parameter

           if(perturb_obs)then
              cdata_all(26,iout)=ran01dom()*perturb_fact ! u perturbation
              cdata_all(27,iout)=ran01dom()*perturb_fact ! v perturbation
           endif

        enddo  loop_readsb
 !   End of bufr read loop
     enddo loop_msg
!    Close unit to bufr file
     call closbf(lunin)
!    Deallocate arrays used for thinning data
     if (.not.use_all) then
        deallocate(presl_thin)
        call del3grids
     endif
     if (.not.use_all_tm) then
       deallocate(presl_thin)
        call del3grids_tm
     endif

! Normal exit

  enddo loop_convinfo! loops over convinfo entry matches
  deallocate(lmsg,tab,nrep)
 

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
     write(6,*) ' READ_SATWND: mix up in read_satwnd ,ndata,icount ',ndata,icount
     call stop2(49)
  end if

  allocate(cdata_out(nreal,ndata))
  do i=1,ndata
     itx=iloc(i)
     do k=1,13
        cdata_out(k,i)=cdata_all(k,itx)
     end do
     cdata_out(14,i)=rusage(itx)
     do k=15,nreal
        cdata_out(k,i)=cdata_all(k,itx)
     end do
  end do
  deallocate(iloc,isort,cdata_all,rusage)
  
  call count_obs(ndata,nreal,ilat,ilon,cdata_out,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) cdata_out

  deallocate(cdata_out)
900 continue
  if(diagnostic_reg .and. ntest>0) write(6,*)'READ_SATWND:  ',&
       'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_SATWND:  ',&
       'nvtest,vdisterrmax=',ntest,vdisterrmax



  if (ndata == 0) then
     call closbf(lunin)
     write(6,*)'READ_SATWND:  closbf(',lunin,')'
  endif
  
  write(6,*) 'READ_SATWND,nread,ndata,nreal,nodata=',nread,ndata,nreal,nodata

  close(lunin)

! End of routine
  return



end subroutine read_satwnd
