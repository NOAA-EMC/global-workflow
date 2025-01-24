subroutine read_rapidscat(nread,ndata,nodata,infile,obstype,lunout,gstime,twind,sis,&
     prsl_full,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_rapidscat                    read scatterometer winds
!   prgmmr: Ling Liu                               date: 2015-04-03
!
! abstract:  This routine reads RapidScat scatterometer winds from BUFR dump.        
!            It also has options to thin the data by using conventional 
!            thinning programs 
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2015-04-03 Ling Liu    
!   2015-09-17 Thomas  - add l4densvar and thin4d to data selection procedure
!   2016-03-11 j. guo  - Fixed {dlat,dlon}_earth_deg in the obs data stream
!   2020-05-04  wu   - no rotate_wind for fv3_regional
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
       rlats,rlons,fv3_regional
  use qcmod, only: errormod
  use convthin, only: make3grids,map3grids_m,del3grids,use_all
  use constants, only: deg2rad,zero,rad2deg,one_tenth,&
        tiny_r_kind,huge_r_kind,r60inv,one_tenth,&
        one,two,three,four,five,half,quarter,r60inv,r10,r100,r2000
!  use converr,only: etabl
  use obsmod, only: ran01dom,bmiss,reduce_diag
  use convinfo, only: nconvtype, &
       icuse,ictype,icsubtype,ioctype, &
       ithin_conv,rmesh_conv,pmesh_conv,pmot_conv
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen,time_4dvar,l4densvar,thin4d
  use deter_sfc_mod, only: deter_sfc_type,deter_sfc2
  use mpimod, only: npe
  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=*)                      ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  real(r_kind)                          ,intent(in   ) :: twind
  real(r_kind),dimension(nlat,nlon,nsig),intent(in   ) :: prsl_full
  integer(i_kind),dimension(npe)  ,intent(inout) :: nobs

! Declare local parameters

  real(r_kind),parameter:: r1_2= 1.2_r_kind
  real(r_kind),parameter:: r3_33= 3.33_r_kind
  real(r_kind),parameter:: r6= 6.0_r_kind
  real(r_kind),parameter:: r50= 50.0_r_kind
  real(r_kind),parameter:: r54= 54.0_r_kind
  real(r_kind),parameter:: r55= 55.0_r_kind
  real(r_kind),parameter:: r56= 56.0_r_kind
  real(r_kind),parameter:: r70= 70.0_r_kind
  real(r_kind),parameter:: r85= 85.0_r_kind
  real(r_kind),parameter:: r90= 90.0_r_kind
  real(r_kind),parameter:: r105= 105.0_r_kind
  real(r_kind),parameter:: r110= 110.0_r_kind
  real(r_kind),parameter:: r125=125.0_r_kind
  real(r_kind),parameter:: r200=200.0_r_kind
  real(r_kind),parameter:: r250=250.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r700=700.0_r_kind
  real(r_kind),parameter:: r199=199.0_r_kind
  real(r_kind),parameter:: r299=299.0_r_kind
  real(r_kind),parameter:: r421=421.0_r_kind
  real(r_kind),parameter:: r296=296.0_r_kind
  real(r_kind),parameter:: r799=799.0_r_kind
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: r10000= 10000.0_r_kind
  
  

! Declare local variables
  logical outside
  logical luse,ithinp
  logical,allocatable,dimension(:,:):: lmsg     ! set true when convinfo entry id found in a message

  character(70) obstr,hdrtr,wndstr
  character(8) subset
  character(8) c_prvstg,c_sprvstg

  integer(i_kind) ireadmg,ireadsb,iuse,mxtb,nmsgmax
  integer(i_kind) i,maxobs,idomsfc,nsattype
  integer(i_kind) nc,nx,isflg,nchanl
  integer(i_kind) ntb,ntmatch,ncx,ncsave,ntread
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) nmind,lunin,idate,ilat,ilon,iret,k
  integer(i_kind) nreal,ithin,iout,ii
  integer(i_kind) itype,iosub,ixsub,isubsub,iobsub 
  integer(i_kind) nlevp         ! vertical level for thinning
  integer(i_kind) pflag
  integer(i_kind) ntest,nvtest
  integer(i_kind) kl,k1,k2
  integer(i_kind) nmsg                ! message index
  integer(i_kind) qc1
  
  
 
  integer(i_kind),dimension(nconvtype) :: ntxall 
  integer(i_kind),dimension(nconvtype+1) :: ntx  
  
  integer(i_kind),dimension(5):: idate5 
  integer(i_kind),allocatable,dimension(:):: nrep
  integer(i_kind),allocatable,dimension(:,:)::tab

  integer(i_kind) ietabl,itypex,lcount,iflag,m

  real(r_single),allocatable,dimension(:,:,:) :: etabl

  real(r_kind) toff,t4dv
  real(r_kind) rmesh,ediff,tdiff
  real(r_kind) u0,v0,uob,vob,dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) dlnpob,ppb,ppb2,qifn,qify,ee
  real(r_kind) woe,dlat,dlon,dlat_earth,dlon_earth,oelev
  real(r_kind) dlat_earth_deg,dlon_earth_deg
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterrmax,u00,v00,uob1,vob1
  real(r_kind) del,werrmin,obserr,ppb1,usage
  real(r_kind) tsavg,ff10,sfcr,sstime,gstime,zz
  real(r_kind) crit1,timedif,xmesh,pmesh
  real(r_kind),dimension(nsig):: presl
  real(r_kind) uob_1,uob_2,uob_3,uob_4,vob_1,vob_2,vob_3,vob_4
  real(r_kind) lkcs_1,lkcs_2,lkcs_3,lkcs_4
  
  real(r_double),dimension(9):: hdrdat
  real(r_double),dimension(2):: satqc
  real(r_double),dimension(5):: obsdat
  real(r_double),dimension(3,4):: wnddat
  real(r_double),dimension(1,1):: r_prvstg,r_sprvstg
  real(r_kind),allocatable,dimension(:):: presl_thin
  real(r_kind),allocatable,dimension(:,:):: cdata_all

  logical,allocatable,dimension(:)::rthin,rusage
  logical save_all
! integer(i_kind) numthin,numqc,numrem,numall
  integer(i_kind) nxdata,pmot

! equivalence to handle character names
  equivalence(r_prvstg(1,1),c_prvstg)
  equivalence(r_sprvstg(1,1),c_sprvstg)


!******** Modify below from the bufrtable: 
  data hdrtr /'SAID YEAR MNTH DAYS HOUR MINU SECO WS10 WD10'/ 
  data obstr/'CLAT CLON WS10 WD10 SWVQ'/ 
  data wndstr/'WS10 WD10 SWVQ'/ 
  
  
  data ithin / -9 /
  data lunin / 21 /
  data rmesh / -99.999_r_kind /

!**************************************************************************

! Return when SATWND are coming from prepbufr file
!  if(use_prepb_satwnd) return

! Read observation error table
! itype 291 has been modified in the error table 

  allocate(etabl(300,33,6))
  etabl=1.e9_r_kind
  ietabl=19
  open(ietabl,file='errtable',form='formatted')
  rewind ietabl
  etabl=1.e9_r_kind
  lcount=0
loopd : do
     read(ietabl,100,IOSTAT=iflag) itypex
     if( iflag /= 0 ) exit loopd
     lcount=lcount+1
     do k=1,33
        read(ietabl,110)(etabl(itypex,k,m),m=1,6)
     end do
  end do   loopd
100     format(1x,i3)
110     format(1x,6e12.5)
  if(lcount<=0 ) then
     write(6,*)'READ_RAPIDSCAT: obs error table not available to 3dvar. the program will stop'
     call stop2(49) 
  else
     write(6,*)'READ_RAPIDSCAT: observation errors provided by local file errtable'
  endif

  close(ietabl)

! Set lower limits for observation errors
! ** keep this way for now
! nreal keep the dimension of cdata_all 
  werrmin=one
  nsattype=0
  nreal=23

! ** read convtype from convinfo file 
! ** only read in rapidsat 296 for now ** 
  ntread=1
  ntmatch=0
  ntx(ntread)=0
  ntxall=0
  do nc=1,nconvtype
     if(trim(ioctype(nc)) == 'uv' .and. ictype(nc) ==296) then
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
     write(6,*) ' READ_RAPIDSCAT: no matching obstype found in convinfo ',obstype
     return
  end if
      
!!  go through the satedump to find out how many subset to process
!** Open and read data from bufr data file

  open(lunin,file=trim(infile),form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)

!! get message and subset counts

  call getcount_bufr(infile,nmsgmax,mxtb)

  allocate(lmsg(nmsgmax,ntread),tab(mxtb,2),nrep(nmsgmax))

  lmsg = .false.
  maxobs=0
  tab=0
  nmsg=0
  nrep=0
  ntb =0
  
  msg_report: do while (ireadmg(lunin,subset,idate) == 0)
!    Time offset
     if(nmsg == 0) call time_4dvar(idate,toff)
     nmsg=nmsg+1
     if (nmsg>nmsgmax) then
        write(6,*)'READ_RAPIDSCAT: messages exceed maximum ',nmsgmax
        call stop2(49)
     endif
     loop_report: do while (ireadsb(lunin) == 0)
        ntb = ntb+1
        maxobs=maxobs+1
        nrep(nmsg)=nrep(nmsg)+1
        if (ntb>mxtb) then
           write(6,*)'READ_RAPIDSCAT: reports exceed maximum ',mxtb   
           call stop2(49)
        endif

!** Extract sat ID information from BUFR and assign type 
!   This part will extract information from the bufrtable
!   bufrtable need to be modified including the rapidscat data entry
!   iobsub is the prepbufr subtype, still part of the convinfo file
!********* iobsub=0 for rapidscat*

        call ufbint(lunin,hdrdat,9,1,iret,hdrtr)
!       determine the satellite wind type 
!       296: rapidscat data                                              
        iobsub=0
        itype=-1
        if(trim(subset) == 'NC012255') then
           if( hdrdat(1) == r296 ) then           !   rapidscat data
             itype=296
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
           lmsg(nmsg,nx) = .true.
        end if
     enddo loop_report
  enddo msg_report

! Loop over convinfo file entries; operate on matches
  allocate(cdata_all(nreal,maxobs),rusage(maxobs),rthin(maxobs))
  nread=0
  ntest=0
  nvtest=0
  nchanl=0
  ilon=2
  ilat=3

!!  read satellite winds one type a time
!   same as in the read_prepbufr.f90 file

  rusage = .true.
  rthin = .false.
  loop_convinfo: do nx=1,ntread 
     use_all = .true.
     ithin=0
     if(nx >1) then
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

           write(6,*)'READ_RAPIDSCAT: ictype(nc),rmesh,pflag,nlevp,pmesh,nc ',&
                   ioctype(nc),ictype(nc),rmesh,pflag,nlevp,pmesh,nc
        endif
     endif

     call closbf(lunin)
     close(lunin)
     open(lunin,file=infile,form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)

! Big loop over BUFR file

     ntb = 0
     nmsg = 0
     if(nx == 1)then
       pmot=0
     else
       nc=ntx(nx)
       pmot=nint(pmot_conv(nc))
     end if
     if(pmot < 2 .and. reduce_diag)pmot=pmot+2
     save_all=.false.
     if(pmot /= 2 .and. pmot /= 0) save_all=.true.
     use_all=.true.

     loop_msg:  do while(ireadmg(lunin,subset,idate) == 0)
        nmsg = nmsg+1
        if(.not.lmsg(nmsg,nx)) then
           ntb=ntb+nrep(nmsg)
           cycle loop_msg ! no useable reports this mesage, skip ahead report count
        end if

        loop_readsb: do while(ireadsb(lunin) == 0)
!          use msg lookup table to decide which messages to skip
!          use report id lookup table to only process matching reports
           ntb = ntb+1
           nc=tab(ntb,1)
           if(nc <= 0 .or. tab(ntb,2) /= nx) cycle loop_readsb

           hdrdat=bmiss
           obsdat=bmiss
           wnddat=bmiss
           satqc=bmiss
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
           uob_1=bmiss
           vob_1=bmiss
           uob_2=bmiss
           vob_2=bmiss
           uob_3=bmiss
           vob_3=bmiss
           uob_4=bmiss
           vob_4=bmiss
           lkcs_1=bmiss
           lkcs_2=bmiss
           lkcs_3=bmiss
           lkcs_4=bmiss
 

! Extract type, date, and location information
           call ufbint(lunin,hdrdat,9,1,iret,hdrtr) 
           call ufbint(lunin,obsdat,5,1,iret,obstr)
           call ufbrep(lunin,wnddat,3,4,iret,wndstr)
!** potential can reject bad cell, etc, place holder for now
!   reject the data with bad quality mark from SDM
!** cycle loop means skip to the next record     

           if(hdrdat(1) /= r296) cycle loop_readsb

!       Compare relative obs time with window.  If obs 
!       falls outside of window, don't use this obs
           idate5(1) = hdrdat(2)     !year
           idate5(2) = hdrdat(3)     ! month
           idate5(3) = hdrdat(4)     ! day
           idate5(4) = hdrdat(5)     ! hours
           idate5(5) = hdrdat(6)     ! minutes
           call w3fs21(idate5,nmind)
           t4dv = real((nmind-iwinbgn),r_kind)*r60inv
           sstime = real(nmind,r_kind) 
           tdiff=(sstime-gstime)*r60inv
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle loop_readsb 
           else
              if (abs(tdiff)>twind) cycle loop_readsb 
           endif


!       determine the satellite wind type as in prepbufr
!       296: rapidscat winds                              

           iosub=0
           if(abs(obsdat(1)) >r90 ) cycle loop_readsb 
           if(obsdat(2) <zero) obsdat(2)=obsdat(2)+r360
           if(obsdat(2) == r360) obsdat(2)=obsdat(2)-r360
           if(obsdat(2) >r360) cycle loop_readsb 
           if(obsdat(5) >1) cycle loop_readsb

           if(trim(subset) == 'NC012255') then    ! rapidscat wind
              if( hdrdat(1) == r296) then          
                   itype=296
              endif
           endif


           nread=nread+2
           dlon_earth_deg = obsdat(2)
           dlat_earth_deg = obsdat(1)
           dlon_earth=obsdat(2)*deg2rad
           dlat_earth=obsdat(1)*deg2rad
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
              if(outside) cycle loop_readsb ! check to see if outside regional 
           else
              dlon=dlon_earth
              dlat=dlat_earth
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif

!     If rapidscat data, determine primary surface type.  If not open sea,
!     skip this observation.  This check must be done before thinning.
!     isflg    - surface flag 0:sea 1:land 2:sea ice 3:snow 4:mixed

           if (itype==296) then                              
              call deter_sfc_type(dlat_earth,dlon_earth,t4dv,isflg,tsavg)
              if (isflg /= 0) cycle loop_readsb
              if (tsavg <= 273.0_r_kind) cycle loop_readsb
           endif

       
!!    convert from wind direction and speed to u,v component
           uob=obsdat(3)*sin(obsdat(4)*deg2rad)
           vob=obsdat(3)*cos(obsdat(4)*deg2rad)
           qc1=obsdat(5)

!!  Get observation error from PREPBUFR observation error table
!   only need read the 4th column for type 291 from the right
 
           ppb=max(zero,min(ppb,r2000))
           if(ppb>=etabl(itype,1,1)) k1=1          
           do kl=1,32
              if(ppb>=etabl(itype,kl+1,1).and.ppb<=etabl(itype,kl,1)) k1=kl
           end do
           if(ppb<=etabl(itype,33,1)) k1=5
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
!         Set usage variable
           usage = zero 
           iuse=icuse(nc)
           if(iuse <= 0)usage=r100

! Get information from surface file necessary for conventional data here
! This is different from the previous sfc_type call
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)
 
!!    process the thining procedure
                
           ithin=ithin_conv(nc)
           ithinp = ithin > 0 .and. pflag /= 0
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
           end if

! The following two lines are new in SATWND:
           dlnpob=log(one_tenth*ppb)  ! ln(pressure in cb)
           ppb=one_tenth*ppb         ! from mb to cb

 !         Special block for data thinning - if requested
           if (ithin > 0 .and. iuse >=0) then

 !         Set data quality index for thinning
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
                  dlat_earth,dlon_earth,ppb,crit1,ndata,&
                  luse,maxobs,rthin,.false.,.false.)

              if (.not. luse) cycle loop_readsb

           else
              ndata=ndata+1
           endif
           iout=ndata

           woe=obserr
           oelev=r10

           if(regional .and. .not. fv3_regional)then
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

! Output result to array, need restruct from here. 


           cdata_all(1,iout)=woe                  ! wind error
           cdata_all(2,iout)=dlon                 ! grid relative longitude
           cdata_all(3,iout)=dlat                 ! grid relative latitude
           cdata_all(4,iout)=dlnpob               ! ln(pressure in cb)
           cdata_all(5,iout)=oelev                 ! index of height         
           cdata_all(6,iout)=uob                  ! u obs
           cdata_all(7,iout)=vob                  ! v obs 
           cdata_all(8,iout)=ndata                ! station id 
           cdata_all(9,iout)=t4dv                 ! time
           cdata_all(10,iout)=nc                  ! index of type in convinfo file
           cdata_all(11,iout)=0                   ! index of station elevation
           cdata_all(12,iout)=qc1                 ! index of quality mark
           cdata_all(13,iout)=obserr              ! original obs error
           cdata_all(14,iout)=usage               ! usage parameter
           cdata_all(15,iout)=idomsfc             ! dominate surface type
           cdata_all(16,iout)=9999999               ! tsavg skin temperature
           cdata_all(17,iout)=ff10                ! 10 meter wind factor
           cdata_all(18,iout)=sfcr                ! surface roughness
           cdata_all(19,iout)=dlon_earth_deg      ! earth relative longitude (degrees)
           cdata_all(20,iout)=dlat_earth_deg      ! earth relative latitude (degrees)
           cdata_all(21,iout)=zz                  ! terrain height at ob location
           cdata_all(22,iout)=r_prvstg(1,1)       ! provider name
           cdata_all(23,iout)=r_sprvstg(1,1)      ! subprovider name
           if(usage >= r100)rusage(ndata)=.false.

        enddo  loop_readsb

     enddo loop_msg
!    Deallocate arrays used for thinning data
     if (.not.use_all) then
        deallocate(presl_thin)
        call del3grids
     endif
! Normal exit

  enddo loop_convinfo! loops over convinfo entry matches
  deallocate(lmsg,tab,nrep)
  call closbf(lunin)
 
! Write header record and data to output file for further processing
  deallocate(etabl)
  nxdata=ndata
  ndata=0
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
!    write(6,*) ' rapid ',trim(ioctype(nc)),ictype(nc),icsubtype(nc),numall,numrem,numqc,numthin
!   If thinned data set usage
     do i=1,nxdata
        if(rthin(i))then
           cdata_all(14,i)=100._r_kind
           cdata_all(12,i)=14
        end if
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
  end if
  nodata=nodata+ndata
  deallocate(rusage,rthin)

  call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)

  deallocate(cdata_all)

  if(diagnostic_reg .and. ntest>0) write(6,*)'READ_RAPIDSCAT:  ',&
       'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_RAPIDSCAT:  ',&
       'nvtest,vdisterrmax=',ntest,vdisterrmax

  write(6,*)'READ_RAPIDSCAT:  closbf(',lunin,')'
  
  write(6,*) 'READ_RAPIDSCAT,nread,ndata,nreal,nodata=',nread,ndata,nreal,nodata

  close(lunin)
  
! End of routine
  return



end subroutine read_rapidscat
