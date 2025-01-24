subroutine read_wcpbufr(nread,ndata,nodata,infile,obstype,lunout,twindin,sis,&
     prsl_full,nobs,nrec_start)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_wcpbufr:  read obs from wcpbufr file
!   prgmmr: parrish          org: np22                date: 1990-10-07
!
! abstract:  This routine reads retrieved hydrometeor (water content) data in the wcpbufr file.  
!            Specific observation types read by this routine include: 
!            solid-water content path and liquid-water content path
!            derived from Hurricane GPROF (see Wu et al. 2016, Brown et al. 2016)
!            (they are called integrated solid-water content and integrated liquid-water content 
!             in Wu et al. 2016) 
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2017-12-18  T.-C. Wu - adapted from read_prepbufr

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
  use constants, only: zero,one_tenth,one,deg2rad,half,&
      rad2deg,tiny_r_kind,huge_r_kind,huge_i_kind,&
      r60inv,r2000
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
      tll2xy,txy2ll, rlats,rlons
  use convinfo, only: nconvtype,ctwind, &
      ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype, &
      ithin_conv,rmesh_conv,pmesh_conv,pmot_conv
  use converr,only: etabl
  use obsmod, only: iadate, offtime_data, oberrflg,reduce_diag
  use gsi_4dvar, only: l4dvar,l4densvar,time_4dvar,winlen,thin4d
  use convthin, only: make3grids,map3grids_m,del3grids,use_all
  use mpimod, only: npe

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
  real(r_kind),parameter:: r6   = 6.0_r_kind
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: convert= 1.0e-3_r_kind ! from g m^-2 to kg m^-2

! Declare local variables
  logical swcpob, lwcpob
  logical outside
  logical luse,ithinp
  logical,allocatable,dimension(:,:):: lmsg           ! set true when convinfo entry id found in a message

  character(40) hdstr,qcstr,oestr,levstr,hdstr2
  character(80) obstr
  character(10) date
  character(8) subset
  character(8) c_station_id
  character(1) sidchr(8)

  integer(i_kind) ireadmg,ireadsb,icntpnt,icntpnt2
  integer(i_kind) lunin,i,maxobs,nmsgmax,mxtb
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) nc,nx,ntread,ii,ncsave
  integer(i_kind) ihh,idd,idate,iret,im,iy,k,levs
  integer(i_kind) kx,nreal,nchanl,ilat,ilon,ithin
  integer(i_kind) qm, swcpq, lwcpq
  integer(i_kind) nlevp         ! vertical level for thinning
  integer(i_kind) iout
  integer(i_kind) pflag,irec
  integer(i_kind) ntest,nvtest,iosub,ixsub,isubsub,iobsub
  integer(i_kind) kl,k1,k2
  integer(i_kind) itypex
  integer(i_kind) minobs,minan
  integer(i_kind) ntb,ntmatch,ncx
  integer(i_kind) nmsg                ! message index
  integer(i_kind),dimension(5):: idate5
  integer(i_kind),dimension(255):: pqm
  integer(i_kind),dimension(nconvtype)::ntxall
  integer(i_kind),dimension(nconvtype+1)::ntx
  integer(i_kind),allocatable,dimension(:):: nrep
  integer(i_kind),allocatable,dimension(:,:):: tab
  real(r_kind) time,timex,timeobs,toff,t4dv,zeps
  real(r_kind) rmesh,ediff,usage
  real(r_kind) dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) dlnpob,ppb
  real(r_kind) swcpoe, swcpmerr, lwcpoe, lwcpmerr
  real(r_kind) dlat,dlon,dlat_earth,dlon_earth
  real(r_kind) dlat_earth_deg,dlon_earth_deg
  real(r_kind) stnelev
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterrmax
  real(r_kind) del, swcperrmin, lwcperrmin
  real(r_kind) crit1,timedif,xmesh,pmesh
  real(r_kind) time_correction
  real(r_kind) perrmin
  real(r_kind),dimension(nsig):: presl
  real(r_kind),dimension(nsig-1):: dpres
  real(r_kind),dimension(255)::plevs
  real(r_kind),allocatable,dimension(:):: presl_thin
  real(r_kind),allocatable,dimension(:,:):: cdata_all
  logical,allocatable,dimension(:)::rthin,rusage
  logical save_all
! integer(i_kind) numthin,numqc,numrem
  integer(i_kind) nxdata,pmot,numall


  real(r_double) rstation_id,qcmark_huge
  real(r_double),dimension(8):: hdr
  real(r_double),dimension(4,255):: qcmark,obserr
  real(r_double),dimension(5,255):: obsdat
  real(r_double),dimension(1,255):: levdat
  equivalence(rstation_id,c_station_id)
  equivalence(rstation_id,sidchr)

!  data statements
  data hdstr  /'SID XOB YOB DHR TYP ELV SAID T29'/
  data hdstr2 /'TYP SAID T29 SID'/
  data obstr  /'POB ZOB CWIO CWLO PRSS' /
  data qcstr  /'PQM ZQM CWIQ CWLQ     '/
  data oestr  /'POE NUL CWIE CWLE     '/
  data levstr  /'POB'/

  data lunin / 15 /
  data ithin / -9 /
  data rmesh / -99.999_r_kind /

  
!------------------------------------------------------------------------
! Initialize variables

  vdisterrmax=zero
  pflag=0                  !  dparrish debug compile run flags pflag as not defined ???????????

  swcpob = obstype == 'swcp'
  lwcpob = obstype == 'lwcp'
  if(swcpob) then
     nreal=16
  else if(lwcpob) then
     nreal=16
  else 
     write(6,*) ' illegal obs type in READ_WCPBUFR ',obstype
     call stop2(94)
  end if

  qcmark_huge = huge_i_kind

  perrmin=0.3_r_kind
  swcperrmin=one
  lwcperrmin=one
  
!------------------------------------------------------------------------
  ntread=1
  ntmatch=0
  ntx(ntread)=0
  ntxall=0
  do nc=1,nconvtype
     if(trim(ioctype(nc)) == trim(obstype))then
          ntmatch=ntmatch+1
          ntxall(ntmatch)=nc
     end if
     if(trim(ioctype(nc)) == trim(obstype) .and. abs(icuse(nc)) <= 1)then
           ithin=ithin_conv(nc)
           if(ithin > 0)then
              ntread=ntread+1
              ntx(ntread)=nc
           end if
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

! Open, then read date from bufr data
  open(lunin,file=trim(infile),form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)

  msg_report: do while (ireadmg(lunin,subset,idate) == 0)
     irec = irec + 1
     if(irec < nrec_start) cycle msg_report

!    Time offset
     if(nmsg == 0) call time_4dvar(idate,toff)
     nmsg=nmsg+1
     if (nmsg>nmsgmax) then
        write(6,*)'READ_WCPBUFR: messages exceed maximum ',nmsgmax
        call stop2(50)
     endif
     loop_report: do while (ireadsb(lunin) == 0)
        ntb = ntb+1
        nrep(nmsg)=nrep(nmsg)+1
        if (ntb>mxtb) then
           write(6,*)'READ_WCPBUFR: reports exceed maximum ',mxtb
           call stop2(50)
        endif

!       Extract type information
        call ufbint(lunin,hdr,4,1,iret,hdstr2)
        kx=hdr(1)

! temporary specify iobsub until put in bufr file
        iobsub = 0                                                  

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
  if (nmsg==0) then
     write(6,*)'READ_WCPBUFR: no messages/reports '
     call closbf(lunin)
     close(lunin)
     return
  end if
  write(6,*)'READ_WCPBUFR: messages/reports = ',nmsg,'/',ntb,' ntread = ',ntread
!------------------------------------------------------------------------

! loop over convinfo file entries; operate on matches
  
  allocate(cdata_all(nreal,maxobs),rusage(maxobs),rthin(maxobs))
  nread=0
  ntest=0
  nvtest=0
  nchanl=0
  ilon=2
  ilat=3
  rusage = .true.
  rthin = .false.
  loop_convinfo: do nx=1, ntread

     use_all = .true.
     ithin=0
     pmot=0

     if(nx > 1) then
        nc=ntx(nx)
        ithin=ithin_conv(nc)
        if (ithin > 0 ) then
           rmesh=rmesh_conv(nc)
           pmesh=pmesh_conv(nc)
           pmot=nint(pmot_conv(nc))
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
     
           write(6,*)'READ_WCPBUFR: obstype,ictype(nc),rmesh,pflag,nlevp,pmesh=',&
              trim(ioctype(nc)),ictype(nc),rmesh,pflag,nlevp,pmesh
        endif
     endif
     if(reduce_diag .and. pmot < 2)pmot=pmot+2
     save_all=.false.
     if(pmot /= 2 .and. pmot /= 0) save_all=.true.
       

     call closbf(lunin)
     close(lunin)
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

           if(abs(hdr(3))>r90 .or. abs(hdr(2))>r360) cycle loop_readsb
           if(hdr(2)== r360)hdr(2)=hdr(2)-r360
           if(hdr(2) < zero)hdr(2)=hdr(2)+r360
           dlon_earth_deg=hdr(2)
           dlat_earth_deg=hdr(3)
           dlon_earth=hdr(2)*deg2rad
           dlat_earth=hdr(3)*deg2rad

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
 
              time_correction=real(minobs-minan,r_kind)*r60inv

           else
              time_correction=zero
           end if

           timeobs=real(real(hdr(4),r_single),r_double)
           t4dv=timeobs + toff
           zeps=1.0e-8_r_kind
           if (t4dv<zero  .and.t4dv>      -zeps) t4dv=zero
           if (t4dv>winlen.and.t4dv<winlen+zeps) t4dv=winlen
           t4dv=t4dv + time_correction
           time=timeobs + time_correction

 
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero.OR.t4dv>winlen) cycle loop_readsb ! outside time window
           else
              if((real(abs(time)) > real(ctwind(nc)) .or. real(abs(time)) > real(twindin)))cycle loop_readsb ! outside time window
           endif

           timex=time
     
!          Extract data information on levels
           call ufbint(lunin,obsdat,5,255,levs,obstr)
           call ufbint(lunin,qcmark,4,255,levs,qcstr)
           call ufbint(lunin,obserr,4,255,levs,oestr)

           if(oberrflg)then

!             Set lower limits for observation errors
              swcperrmin=one_tenth
              lwcperrmin=one_tenth
                do k=1,levs
                   itypex=kx
                   ppb=obsdat(1,k)
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
                   obserr(1,k)=(one-del)*etabl(itypex,k1,5)+del*etabl(itypex,k2,5)
                   obserr(1,k)=max(obserr(1,k),perrmin)
                   obserr(3,k)=max(obserr(3,k),swcperrmin)
                   obserr(4,k)=max(obserr(3,k),lwcperrmin)
                enddo
           endif        ! endif for oberrflg

           nread=nread+1

!          Set station ID
           rstation_id=hdr(1)

!          Loop over levels
           do k=1,levs
              do i=1,4
                 qcmark(i,k) = min(qcmark(i,k),qcmark_huge)
              end do

!              if (kx == id_bias_ps) then
!                 plevs(k)=one_tenth*obsdat(1,k)+conv_bias_ps   ! convert mb to cb
!              else
                 plevs(k)=one_tenth*obsdat(1,k)   ! convert mb to cb
!              endif
              pqm(k)=nint(qcmark(1,k))
           end do

           stnelev=hdr(6)
           ithin=ithin_conv(nc)
           ithinp = ithin > 0 .and. pflag /= 0
           if(levs > 1 .or. ithinp)then
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
              if (levs > 1) then
                 do kk=1,nsig-1
                    dpres(kk)=presl(kk)-presl(kk+1)
                 end do
              endif
           end if
           LOOP_K_LEVS: do k=1,levs
              icntpnt=icntpnt+1

!             Extract quality marks
              if(swcpob) then
                 swcpq=nint(qcmark(3,k))
                 qm=swcpq
              else if(lwcpob) then
                 lwcpq=nint(qcmark(4,k))
                 qm=lwcpq
             end if

!             Check qc marks to see if obs should be processed or skipped

              if(qm > 15 .or. qm < 0) cycle loop_k_levs

!             Set usage variable              
              usage = zero

!             Special block for data thinning - if requested
              if (ithin > 0) then
           
!                Set data quality index for thinning
                 if (thin4d) then
                    timedif = zero
                 else
                    timedif=abs(t4dv-toff)
                 endif
                    crit1 = timedif/r6+half

                 if (pflag==0) then
                    do kk=1,nsig
                       presl_thin(kk)=presl(kk)
                    end do
                 endif

                 call map3grids_m(-1,save_all,pflag,presl_thin,nlevp, &
                     dlat_earth,dlon_earth,plevs(k),crit1,ndata,&
                     luse,maxobs,rthin,.false.,.false.)


                 if(rthin(ndata))usage=101._r_kind
                 if (.not. luse) then
                    if(k==levs) then
                       cycle loop_readsb
                    else
                       cycle LOOP_K_LEVS
                    endif
                 endif

              else
                 ndata=ndata+1
              endif
              iout=ndata

              if(ndata > maxobs) then
                 write(6,*)'READ_WCPBUFR:  ***WARNING*** ndata > maxobs for ',obstype
                 ndata = maxobs
              end if



              if(icuse(nc) <= 0)usage=100._r_kind
              if(qm == 15 .or. qm == 12 .or. qm == 9)usage=100._r_kind
              if(plevs(k) < 0.0001_r_kind) then
                 write(*,*) 'warning: obs pressure is too small:',kx,k,plevs(k)
                 cycle
              endif

              if(ncnumgrp(nc)>0 )then                 ! default cross validation on
                 if(mod(ndata+1,ncnumgrp(nc))== ncgroup(nc)-1)usage=ncmiter(nc)
              end if
              if(icuse(nc) <= 0 .or. qm >= 8) rusage(iout) = .false.

!             Extract pressure level and quality marks
              dlnpob=log(plevs(k))  ! ln(pressure in cb)
 
!             solid-water content path (Hurricane GPROF: TMI and GMI)
              if(swcpob) then

                 swcpoe=obserr(3,k)*convert
                 swcpmerr=swcpoe
                 cdata_all(1,iout)=swcpoe                  ! swcp error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=obsdat(3,k)*convert     ! swcp obs
                 cdata_all(5,iout)=rstation_id             ! station id
                 cdata_all(6,iout)=t4dv                    ! time
                 cdata_all(7,iout)=nc                      ! type
                 cdata_all(8,iout)=swcpmerr                ! swcp max error
                 cdata_all(9,iout)=swcpq                   ! quality mark
                 cdata_all(10,iout)=swcpoe                 ! original obs error
                 cdata_all(11,iout)=usage                  ! usage parameter
                 cdata_all(12,iout)=dlon_earth_deg         ! earth relative longitude (degrees)
                 cdata_all(13,iout)=dlat_earth_deg         ! earth relative latitude (degrees)
                 cdata_all(14,iout)=stnelev                ! station elevation (m)
                 cdata_all(15,iout)=obsdat(1,k)            ! observation pressure (hPa)
                 cdata_all(16,iout)=obsdat(2,k)            ! observation height (m)
 
!             liquid-water content path (Hurricane GPROF: TMI and GMI)
              else if(lwcpob) then

                 lwcpoe=obserr(4,k)*convert
                 lwcpmerr=lwcpoe
                 cdata_all(1,iout)=lwcpoe                  ! lwcp error
                 cdata_all(2,iout)=dlon                    ! grid relative longitude
                 cdata_all(3,iout)=dlat                    ! grid relative latitude
                 cdata_all(4,iout)=obsdat(4,k)*convert     ! lwcp obs
                 cdata_all(5,iout)=rstation_id             ! station id
                 cdata_all(6,iout)=t4dv                    ! time
                 cdata_all(7,iout)=nc                      ! type
                 cdata_all(8,iout)=lwcpmerr                ! lwcp max error
                 cdata_all(9,iout)=lwcpq                   ! quality mark
                 cdata_all(10,iout)=lwcpoe                 ! original obs error
                 cdata_all(11,iout)=usage                  ! usage parameter
                 cdata_all(12,iout)=dlon_earth_deg         ! earth relative longitude (degrees)
                 cdata_all(13,iout)=dlat_earth_deg         ! earth relative latitude (degrees)
                 cdata_all(14,iout)=stnelev                ! station elevation (m)
                 cdata_all(15,iout)=obsdat(1,k)            ! observation pressure (hPa)
                 cdata_all(16,iout)=obsdat(2,k)            ! observation height (m)

              end if

           end do  LOOP_K_LEVS   !    End k loop over levs
        end do loop_readsb   !   End of bufr read loop
     enddo loop_msg


!    Deallocate arrays used for thinning data
     if (.not.use_all) then
        deallocate(presl_thin)
        call del3grids
     endif
! Normal exit

  enddo loop_convinfo! loops over convinfo entry matches
! Close unit to bufr file
  call closbf(lunin)
  close(lunin)
  deallocate(lmsg,tab,nrep)

  nxdata=ndata
  nodata=0
  if(nxdata > 0)then
!    numthin=0
!    numqc=0
!    numrem=0
!    do i=1,nxdata
!       if(.not. rusage(i))then
!          numqc=numqc+1
!       else if(rthin(i))then
!          numthin=numthin+1
!       else
!          numrem=numrem+1
!       end if
!    end do
!    write(6,*) ' wcp ',trim(ioctype(nc)),ictype(nc),icsubtype(nc),numall,numrem,numqc,numthin
!   If thinned data set usage
     do i=1,nxdata
        if(rthin(i))cdata_all(11,i)=100._r_kind
     end do
!  If flag to not save thinned data is set - compress data
     do i=1,nxdata
!   pmot=0 - all obs - thin obs
!   pmot=1 - all obs
!   pmot=2 - use obs
!   pmot=3 - use obs + thin obs
        if((pmot == 0 .and. .not. rthin(i)) .or. &
           (pmot == 1) .or. &
           (pmot == 2 .and. (rusage(i) .and. .not. rthin(i)))  .or. &
           (pmot == 3 .and. rusage(i))) then

           ndata=ndata+1
           do k=1,nreal
              cdata_all(k,ndata)=cdata_all(k,i)
           end do
        end if
     end do
     nodata=nodata+ndata
  end if

  deallocate(rusage,rthin)

! Write header record and data to output file for further processing

  call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)

  deallocate(cdata_all)

  if(diagnostic_reg .and. ntest>0) write(6,*)'READ_WCPBUFR:  ',&
     'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_WCPBUFR:  ',&
     'nvtest,vdisterrmax=',ntest,vdisterrmax

  if (ndata == 0) then 
     write(6,*)'READ_WCPBUFR:  closbf(',lunin,') no data'
  endif

  close(lunin)

! End of routine
  return

end subroutine read_wcpbufr
