subroutine read_anowbufr(nread,ndata,nodata,gstime,&
      infile,obstype,lunout,twindin,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_anowbufr                read pm2_5 obs from AIRNow prepbufr file (based on other bufr readers)
!   prgmmr: pagowski                date: 2010-09-13
!
! abstract:  This routine reads conventional pm2_5 data from AIRNow 
! prepbufr can be done similarly for ozone
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2010-09-13  pagowski adopted prepbufr reader code for 
!  AIRNow bufr for pm2_5
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-11-01 pagowski - make code compatible with ncep/mhu airnow bufr
!   2015-02-23  Rancic/Thomas - add l4densvar to time window logical
!   2015-10-01  guo     - consolidate use of ob location (in deg)
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     twindin  - input group time window (hours)
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     nodata   - number of individual "obstype" observations retained for !further processing
!     sis      - satellite/instrument/sensor indicator
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,one,half,deg2rad,&
       rad2deg,r60inv
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,&
       tll2xy,txy2ll,rlats,rlons,region_dx
  use convinfo, only: nconvtype,ctwind, &
       icuse,ioctype,ictype,cermin,cermax
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use chemmod, only : obs2model_anowbufr_pm,&
        iconc,ierror,ilat,ilon,itime,iid,ielev,isite,iikx,ilate,ilone,&
        elev_missing,site_scale,tunable_error,&
        code_pm25_ncbufr,code_pm25_anowbufr,&
        code_pm10_ncbufr,code_pm10_anowbufr,&
        anowbufr_ext,pm2_5_teom_max,pm10_teom_max

  use mpimod, only: npe

  implicit none
  
! declare passed variables
  character(len=*),intent(in   ) :: infile,obstype
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  real(r_kind)    ,intent(in   ) :: gstime,twindin
  character(len=*),intent(in   ) :: sis
  
  
! declare local parameters

  integer(i_kind), parameter :: maxobs=1e5
  integer(i_kind), parameter :: nsid=1,nxob=2,&
        nyob=3,ndhr=4,ntyp=5,ncopopm=6
!see headr input format below
  integer(i_kind), parameter :: nfields=6
  integer(i_kind), parameter :: nfields_b=12
!output format parameters
  integer(i_kind), parameter:: nchanl=0,nreal=ilone

  real(r_kind),parameter :: r360 = 360.0_r_kind
  real(r_kind),parameter :: r90 = 90.0_r_kind
  real(r_kind),parameter :: percent=1.e-2_r_kind

  real(r_kind), parameter :: anow_missing=1.0e11_r_kind,&
        conc_missing = anow_missing-1

! declare local variables
  logical outside
  
  integer(i_kind) lunin,i
  integer(i_kind) idate,iret,k
  integer(i_kind) kx,ikx
  integer(i_kind) nmind
  integer(i_kind) :: iy,im,id,ih,imin,site_char,site_id
!           (site_char=1,2,3,4: unknown,urban,suburban,rural)  

  integer(i_kind) :: ireadmg,ireadsb
  integer(i_kind), dimension(5) :: idate5
  integer(i_kind), dimension(8) :: idate8
  real(r_kind), dimension(5)  :: rinc
  character(len=8) :: subset
  character(len=80) :: headr
  character(len=80) :: obstr

  real(r_double), dimension(nfields) :: indata
  real(r_double), dimension(nfields_b) :: indata_a,indata_b

  real(r_kind) :: tdiff,obstime,t4dv
  real(r_kind) :: dlat,dlon,error_1,error_2,obserror,dlat_earth,dlon_earth
  real(r_kind) :: dlat_earth_deg,dlon_earth_deg
  
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  integer(i_kind) ntest,ios
  

  real(r_kind) :: conc,site_elev
  real(r_kind), dimension(nreal,maxobs):: cdata_all
  character(len=8):: sid
  character(len=10) :: cdate

  logical :: ncbufr,anowbufr

  equivalence (sid,indata(nsid))

  data lunin / 10 /

  ncbufr=.false.
  anowbufr=.false.

  site_char=1 ! set unknown site character
  site_elev=elev_missing ! set unknown site elevation

!**************************************************************************
! initialize variables
  disterrmax=zero
  ntest=0
  nread=0
  ndata = 0
  nodata = 0

! open, then read date from bufr data
  open(lunin,file=trim(infile),form='unformatted')

  call openbf(lunin,'IN',lunin)
  call datelen(10)

! reading each report from bufr

  do while (ireadmg(lunin,subset,idate) == 0)
     if (trim(obstype)=='pm2_5') then
        
        if ( (subset == 'NC008031') .or. (subset == 'NC008032' ) ) then
           headr='PTID CLONH CLATH TPHR TYPO COPOPM'
           ncbufr=.true.
           write(6,*)'READ_PM2_5:  AIRNOW data type, subset=',subset
        else if (subset == 'ANOWPM') then
           if (anowbufr_ext) then
              headr='SID XOB YOB DHR TYP T29 SQN PROCN RPT CAT TYPO TSIG'
              obstr='TPHR QCIND COPOPM ELV COPOPM10 COPOCO'
              anowbufr=.true.
              write(6,*)'READ_PM2_5_BUFR_EXT:  AIRNOW data type, subset=',subset 
           else ! default ANOWBUFR Table
              headr='SID XOB YOB DHR TYP COPOPM'
              anowbufr=.true.
              write(6,*)'READ_PM2_5:  AIRNOW data type, subset=',subset
           end if
        else
           cycle
        endif

     else if (trim(obstype)=='pm10') then
        
        if (subset == 'NC008033') then
           headr='PTID CLONH CLATH TPHR TYPO COPOPM'
           ncbufr=.true.
           write(6,*)'READ_PM10:  AIRNOW data type, subset=',subset
        else if (subset == 'ANOWPM') then
           if (anowbufr_ext) then
              headr='SID XOB YOB DHR TYP T29 SQN PROCN RPT CAT TYPO TSIG'
              obstr='TPHR QCIND COPOPM ELV COPOPM10 COPOCO'
              anowbufr=.true.
              write(6,*)'READ_PM10_BUFR_EXT:  AIRNOW data type, subset=',subset
           else
              headr='SID XOB YOB DHR TYP COPOPM'
              anowbufr=.true.
              write(6,*)'READ_PM10:  AIRNOW data type, subset=',subset
           end if
        else
           cycle
        endif
        
     else !keep option for ozone
        cycle
!     subset='AIRNOW' for ozone
     endif

     write(cdate,'(i10)') idate
     read (cdate,'(i4,3i2)') iy,im,id,ih
     imin=0

     do while (ireadsb(lunin) == 0)
        if (anowbufr_ext) then
           call ufbint(lunin,indata_a,nfields_b,1,iret,headr)
           indata(1:5) = indata_a(1:5)
           call ufbint(lunin,indata_b,nfields_b,1,iret,obstr)
           if (trim(obstype)=='pm2_5') indata(ncopopm)=indata_b(3)
           if (trim(obstype)=='pm10')  indata(ncopopm)=indata_b(5)
           site_elev = indata_b(4)
        else
           call ufbint(lunin,indata,nfields,1,iret,headr)
        end if

        if (anowbufr) then
           kx=indata(ntyp)
           read(sid,'(Z8)')site_id
        else if (ncbufr) then
           kx=indata(ntyp)
           if (kx/=code_pm25_ncbufr .or. kx/=code_pm10_ncbufr) then
              cycle
           else
              if (trim(obstype)=='pm2_5') then 
                 kx=code_pm25_anowbufr
              else 
                 kx=code_pm10_anowbufr
              endif
           endif
           read(sid,'(Z8)',iostat=ios)site_id
           if (ios/=0) site_id=nint(indata(nsid))
        endif
        
        nread = nread + 1
        conc=indata(ncopopm)
        if ( iret > 0 .and. (conc < conc_missing ) .and. &
               (conc >= zero)) then
           
           if(indata(nxob) >= r360)  indata(nxob) = indata(nxob) - r360
           if(indata(nxob) <  zero)  indata(nxob) = indata(nxob) + r360

           if(indata(nxob) > r360)cycle
           if(indata(nyob) > r90)cycle

           dlon_earth_deg=indata(nxob)
           dlat_earth_deg=indata(nyob)

           dlon_earth=indata(nxob)*deg2rad
           dlat_earth=indata(nyob)*deg2rad


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

              if(outside) cycle ! check to see if outside regional domain

           else
              dlat = dlat_earth
              dlon = dlon_earth
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif
           
!  extract date information.  if time outside window, skip this obs
           

           idate5(1) = iy !year
           idate5(2) = im !month
           idate5(3) = id !day
           idate5(4) = ih !hour
           idate5(5) = imin !minute
           
           idate8(1:3)=idate5(1:3)
           idate8(4)=0
           idate8(5:6)=idate5(4:5)
           idate8(7:8)=0
           
           rinc(1)=zero
! half below accounts for the fact that in bufr file measurement
! at 5:30 (ie. average from 5:00 to 6:00) is assigned time 6:00 
           rinc(2)=indata(ndhr)-half 
           rinc(3:5)=zero

           call w3movdat(rinc,idate8,idate8)
           
           idate5(1:3)=idate8(1:3)
           idate5(4:5)=idate8(5:6)

           ikx = 0
           do i = 1, nconvtype
              if (obstype == ioctype(i) .and. kx == ictype(i) .and. &
                    abs(icuse(i))== 1) ikx=i
           end do
           if(ikx == 0) cycle             ! not ob type used
           
           call w3fs21(idate5,nmind)
           t4dv=real((nmind-iwinbgn),r_kind)*r60inv
           obstime=real(nmind,r_kind)
           tdiff=(obstime-gstime)*r60inv

           if (l4dvar.or.l4densvar) then
              if (t4dv < zero .or. t4dv > winlen) cycle
           else
              if(abs(tdiff) > twindin .or. &
                    abs(tdiff) > ctwind(ikx)) cycle  ! outside time window
           endif

!at a later stage search for site character and elevation 
!based on sid when reference table available
!for now assign default sitecharacter and unknown elevation 
           
!calculate pm2_5 obs error
!obs error for pm2_5/pm10 is calculated as
!obserror=sqrt(error_1^2+error_2^2)
!measurement error: 
!error_1=cermax+cermin*conc
!(1.5ug/m3+0.0075*concentration)
!cermin, cermax in convinfo.txt are nonstandard            
!cermax is a constant part, cermin is a percentage part           
!representativeness error: 
!error_2=tunable_error*error_1*sqrt(dx/site_scale)
!similar for ozone

           
           conc=conc*obs2model_anowbufr_pm()

           error_1=cermax(ikx)+cermin(ikx)*percent*conc
           error_2=tunable_error*error_1*&
                 sqrt(region_dx(nint(dlat),nint(dlon))/&
                 site_scale(site_char))
           obserror=sqrt(error_1**2+error_2**2)

           ndata=ndata+1
           nodata=nodata+1
           if(ndata>maxobs) exit

           cdata_all(iconc,ndata)  = conc                    ! pm2_5 obs     
           cdata_all(ierror,ndata) = obserror                ! pm2_5 obs error
           cdata_all(ilat,ndata)   = dlat                    ! grid relative latitude 

           cdata_all(ilon,ndata)   = dlon                    ! grid relative longitude 
           cdata_all(itime,ndata)  = tdiff                   ! time of obs
           cdata_all(iid,ndata)    = site_id                 ! site id 
           cdata_all(ielev,ndata)  = site_elev               ! elevation
           cdata_all(isite,ndata)  = site_char               ! site character
           cdata_all(iikx,ndata)   = ikx                     ! ordered number in convinfo table
           cdata_all(ilate,ndata)  = dlat_earth_deg          ! earth relative latitude (degrees)
           cdata_all(ilone,ndata)  = dlon_earth_deg          ! earth relative longitude (degrees)


        end if
     enddo
  enddo
  
1000 continue

  call closbf(lunin)
  close(lunin)
  
  if(diagnostic_reg .and. &
       ntest > 0) write(6,*)'read_airnow_bufr: ',&
       'ntest,disterrmax=',ntest,disterrmax

  if (nodata == 0) then 
     write(6,*)'did not find pm2_5 in airnow_bufr '
     write(6,*)'check input airnow_bufr file'
  endif

  
! write header record and data to output file for further processing
  call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)

  return
  
end subroutine read_anowbufr
