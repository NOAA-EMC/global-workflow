subroutine read_goesglm(nread,ndata,nodata,infile,obstype,lunout,twindin,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_goesglm                reads lightning obs from a BUFR file
!   prgmmr:      k apodaca <karina.apodaca@colostate.edu>
!      org:      CSU/CIRA, Data Assimilation Group
!     date:      2015-03-12
!
! abstract:  This routine reads lightning data (Earth-relative location, frequency) from 
!            file and prepares it for assimiliation in the form of lightning flash rate 
!            (#hits km-2 hr-1).  
!
!            Note: when running GSI in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2015-06-12  zupanski  - include the convert_to_flash_rate subroutine to convert 
!                           lightning strike observations into lightning flash rate 
!                           (#hits km-2 hr-1). 
!   2015-11-18  apodaca   - include the convert_time subroutine to deal with computer 
!                           precision dependencies.
!   2018-02-07  apodaca   - add further documentation
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     nodata   - number of individual "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     twindin  - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:  
!
!$$$
  use kinds, only: r_single,r_kind,r_double,i_kind
  use constants, only: zero,one_tenth,one,deg2rad,&
      three,rad2deg,&
      r60inv,ten
  use gridmod, only: diagnostic_reg,wrf_mass_regional,regional,nlon,nlat,&
      tll2xy,txy2ll,&
      rlats,rlons
  use lightinfo, only: iuse_light,nlighttype
  use obsmod, only: iadate
  use obsmod, only: offtime_data
  use gsi_4dvar, only: l4dvar,l4densvar,time_4dvar,winlen

  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=*)                      ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout
  integer(i_kind)                       ,intent(inout) :: nread,ndata
  real(r_kind)                          ,intent(in   ) :: twindin
  integer(i_kind)                                      :: nodata

! Declare local parameters
  real(r_kind),parameter:: r6   = 6.0_r_kind
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r180 = 180.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind

!--- Declare local variables
  logical lob
  logical outside

  character(40) hdstr,oestr,qcstr
  character(10) date
  character(8)  subset
  character(1)  sidchr(8)

  integer(i_kind) ireadmg,ireadsb,icntpnt
  integer(i_kind) lunin,i
  integer(i_kind) ihh,idd,idate,iret,im,iy,k
  integer(i_kind) nchanl,nreal,ilat,ilon
  integer(i_kind) lqm
  integer(i_kind) iout
  integer(i_kind) ntest,nvtest
  integer(i_kind) minobs,minan
  integer(i_kind) ntb
  integer(i_kind) nmsg                ! message index
  integer(i_kind),parameter :: maxobs=2000000
  integer(i_kind),dimension(5):: idate5

  real(r_kind) time
  real(r_kind) usage
  real(r_kind) loe,lmerr
  real(r_kind) time_correction
  real(r_kind) dlat,dlon,dlat_earth,dlon_earth
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterrmax
  real(r_kind) timex,timeobs,toff,t4dv,zeps
  real(r_kind),allocatable,dimension(:,:):: cdata_all
!--- flash rate
  real(r_kind),allocatable,dimension(:,:):: cdata_flash,cdata_flash_h
  integer(i_kind)                        :: ndata_flash,ndata_flash_h

  real(r_double) rstation_id

  real(r_double),dimension(3):: hdr
  real(r_double),dimension(1,1):: qcmark,obserr

!  equivalence to handle character names
  equivalence(rstation_id,sidchr)

!--- data statements
  data hdstr  /'XOB YOB DHR'/
  data oestr  /'LOE'/ 
  data qcstr  /'LQM'/   

  data lunin / 13 /


  nreal=13
  lob = obstype == 'goes_glm'
  if(.not.lob) then
    write(6,*) 'mix-up reading goes_glm ',obstype
    return
  end if

!                .      .    .                                       .

! Open, then read date from BUFR file


  open(lunin,file=infile,form='unformatted') 
  call openbf(lunin,'IN',lunin) 
  call datelen(10) 


! Initialization

  ntb = 0 
  nmsg = 0 
  disterrmax=-9999.0_r_kind 

  allocate(cdata_all(nreal,maxobs))
  cdata_all=zero
  nread=0
  ntest=0
  nvtest=0
  nchanl=0
  ilon=2
  ilat=3
  icntpnt=0

!                .      .    .                                       .
! Big loop over glmbufr file : READING THE BUFR FILE
     
  loop_msg: do while (ireadmg(lunin,subset,idate)== 0) 
     nmsg = nmsg+1 
     write(*,'(3a,i10)') 'subset=',subset,' cycle time =',idate 


     loop_readsb: do while(ireadsb(lunin) == 0)  
!       use msg lookup table to decide which messages to skip
!       use report id lookup table to only process matching reports
        ntb = ntb+1 

          
!       Extract location and date information
        call ufbint(lunin,hdr,3,1,iret,hdstr)

        if(abs(hdr(2))>r90 .or. abs(hdr(1))>r360) cycle loop_readsb
        if(hdr(1) > r180)hdr(1)=hdr(1)-r360
        if(hdr(1) < zero)hdr(1)=hdr(1)+r360
        dlon_earth=hdr(1)*deg2rad
        dlat_earth=hdr(2)*deg2rad
                
        if (regional) then

!-- WRF-ARW

           if (wrf_mass_regional) then

              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinates
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

           endif ! wrf_mass_regional

        endif !if (regional) then

! Global

        if (.not. regional) then
            dlat = dlat_earth
            dlon = dlon_earth
            call grdcrd1(dlat,rlats,nlat,1)
            call grdcrd1(dlon,rlons,nlon,1)
        endif !  end global block 

        if (offtime_data) then
 
!           in time correction for observations to account for analysis
!           time being different from obs file time.
            write(date,'( i10)') idate
            read (date,'(i4,3i2)') iy,im,idd,ihh
            idate5(1)=iy
            idate5(2)=im
            idate5(3)=idd
            idate5(4)=ihh
            idate5(5)=0
            call w3fs21(idate5,minobs)    !  obs ref time in seconds relative to historic date
            idate5(1)=iadate(1)
            idate5(2)=iadate(2)
            idate5(3)=iadate(3)
            idate5(4)=iadate(4)
            idate5(5)=0
            call w3fs21(idate5,minan)    !  analysis ref time in seconds relative to historic date
           
!           Add obs reference time, then subtract analysis time to get obs time relative to analysis
            time_correction=real(minobs-minan,r_kind)*r60inv
        else
            time_correction=zero
        end if

        timeobs=real(real(hdr(3),r_single),r_double)
        t4dv=timeobs + toff
        zeps=1.0e-8_r_kind
        if (t4dv<zero  .and.t4dv>      -zeps) t4dv=zero
        if (t4dv>winlen.and.t4dv<winlen+zeps) t4dv=winlen
        t4dv=t4dv + time_correction
        time=timeobs + time_correction


        if (l4dvar.or.l4densvar) then
           if (t4dv<zero.or.t4dv>winlen) cycle loop_readsb ! outside time window
        else
           if((real(abs(time)) > real(twindin)))cycle loop_readsb ! outside time window
        endif

        timex=time


!       Note: An assessment of the GLM detection error is still a work in
!       progress and at present, there are no clear metrics to assign a 
!       "quality mark" for an effective quality control procedure for
!       lightning observations from this sensor. Nonetheless, the infrastructure
!       for passing observations errors and quality control information to other 
!       routines within the GSI source code has been left in place. In addition, a 
!       temporarty undefined value (-.9999) has been assigned to the mnemonics
!       corresponding to these variables in the lightning BUFR file. Developments 
!       for the calculation of observation errors and quality control (sanity checks) are 
!       expected in future upgrades to the "GOES/GLM variational lightning 
!       assimilation package."

!       Extract observation error informatiom
        call ufbint(lunin,obserr,1,1,iret,oestr)

        loe=obserr(1,1)
        lmerr=loe
 
!       Extract quality control information
        call ufbint(lunin,qcmark,1,1,iret,qcstr)
 
        lqm=qcmark(1,1)

!       Data counter

        nread=nread+1
        icntpnt=icntpnt+1

        ndata=ndata+1
        if(ndata>maxobs) exit
        nodata=nodata+1
        iout=ndata

        if (ndata > maxobs) then
           write(6,*)'READ_GOESGLM:  ***WARNING*** ndata > maxobs for ',obstype
           ndata = maxobs
        end if

             
!       Set usage variable              
        usage = zero

        if (iuse_light(nlighttype) <= 0)usage=100._r_kind
        cdata_all(1,iout) =loe                  ! lightning observation error
        cdata_all(2,iout) =dlon                 ! grid relative longitude
        cdata_all(3,iout) =dlat                 ! grid relative latitude
        cdata_all(4,iout) =iout                 ! lightning obs
        cdata_all(5,iout) =rstation_id          ! station id
        cdata_all(6,iout) =t4dv                 ! analysis time
        cdata_all(7,iout) =nlighttype           ! type
        cdata_all(8,iout) =lmerr                ! lightning max error
        cdata_all(9,iout) =lqm                  ! quality mark
        cdata_all(10,iout)=loe                  ! original lightning obs error loe 
        cdata_all(11,iout)=usage                ! usage parameter
        cdata_all(12,iout)=dlon_earth*rad2deg   ! earth relative lon (degrees)
        cdata_all(13,iout)=dlat_earth*rad2deg   ! earth relative lat (degrees)


! end loop on read line BUFR

     end do loop_readsb

! end of BUFR read loop

  enddo loop_msg   !Uncomment if reading messeges in a loop

!                .      .    .                                       .

! Close unit to bufr file

  call closbf(lunin)

! Write header record and data to output file for further processing
 

! Call to the subroutine that transforms lightning strikes into lightning flash rate


  if(ndata /= 0) then

      !! count flash rate data and alocate temporary domain 
      !! begin with the current number of strikes as the theoretical upper limit
   
     ndata_flash_h=ndata
  
     allocate(cdata_flash_h(nreal,ndata_flash_h))

     call convert_to_flash_rate   &
              (nreal,ndata,cdata_all,ndata_flash_h,cdata_flash_h,ndata_flash)
 
     deallocate(cdata_all)
     ndata=ndata_flash
     allocate(cdata_flash(nreal,ndata))

     do i=1,ndata
        do k=1,nreal
           cdata_flash(k,i)=cdata_flash_h(k,i)
        end do
     end do

     deallocate(cdata_flash_h)

     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) cdata_flash

!!!!!!!!!
!!! Write lat, lon, time, lightning flash rate "superobs" into a file: 
!!! cdata_flash_h(2,iout),cdata_flash_h(3,iout),cdata_flash_h(4,iout), &
!!! cdata_flash_h(6,iout)cdata_flash(4,:)

     deallocate(cdata_flash)

  else  ! ndata=0

     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)
     deallocate(cdata_all)

  end if  !!  if(ndata =/ 0) then


900 continue
  if(diagnostic_reg .and. ntest>0) write(6,*)'READ_GOESGLM:  ',&
     'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_GOESGLM:  ',&
     'nvtest,vdisterrmax=',ntest,vdisterrmax

  if (ndata == 0) then 
     write(6,*)'READ_GOESGLM:  closbf(',lunin,') no data'
  endif


  close(lunin)

! End of routine
  return

end subroutine read_goesglm


!!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

subroutine convert_to_flash_rate   &
   (nreal,ndata_strike,cdata_strike,ndata_flash_h,cdata_flash_h,ndata_flash)

!$$$  documentation block
!                .      .    .                                       .
! subroutine:  convert_to_flash_rate      converts geo-located lightning strikes into
!                                         lightning flash rate (#hits km-2 hr-1)

!   prgmmr: zupanski <milija.zupanski@colostate.edu>
!      org: CSU/CIRA, Data Assimilation group
!     date: 2015-06-12
!
! abstract:  This subroutine does the following: 
!----
!- 1- counts the number of hits surrounding a GSI analysis grid point
!- 2- calculates the flash rate averaged over time and area (hr*km**2)
!- 3- finds the center of mass in terms of lon,lat and glon,glat
!- 4- assigns a center of mass to be the flash rate observation point
!---- 
!
! program history log:
!   2015-07-01  apodaca  - several updates in the calculation of lightning flashrate

  use kinds, only: r_single,r_kind,r_double,i_kind
  use constants, only: zero,one_tenth,one,deg2rad,&
      three,half,zero,&
      r10,r100,ten, r1000, rearth
  use gridmod, only: wrf_mass_regional,regional,nlon,nlat,&
      tll2xy,txy2ll
  use gridmod, only: lat2, lon2
  use wrf_mass_guess_mod, only: ges_xlon, ges_xlat
  use gsi_4dvar, only: nhr_assimilation

  implicit none

  integer(i_kind),intent(in   )               :: nreal,ndata_strike,ndata_flash_h
  integer(i_kind),intent(inout)               :: ndata_flash
  real(r_kind),intent(inout),dimension(nreal,ndata_strike)  :: cdata_strike
  real(r_kind),intent(inout),dimension(nreal,ndata_flash_h) :: cdata_flash_h

  real(r_kind),allocatable,dimension(:) :: gtim_central
  real(r_kind),allocatable,dimension(:) :: glon_central
  real(r_kind),allocatable,dimension(:) :: glat_central
  real(r_kind),allocatable,dimension(:) ::  lon_central
  real(r_kind),allocatable,dimension(:) ::  lat_central
  integer(i_kind),allocatable,dimension(:) ::  ind_min
  integer(i_kind),allocatable,dimension(:) ::  lcount
  
  real(r_kind)      :: rearth2
  real(r_kind)      :: dtime,darea,cosine
  real(r_kind)      :: darea_sum
  real(r_kind)      :: delta_lon,delta_lat
  real(r_kind)      :: lat_ref
  real(r_kind)      :: xx,yy
  real(r_kind)      :: dist2,dist_min
  integer(i_kind)   :: ii0,jj0
  integer(i_kind)   :: ngridh
  integer(i_kind)   :: index
  integer(i_kind)   :: iobs,usage 
  logical           :: xflag,yflag

  real(r_kind)      :: xbound,ybound
  integer(i_kind)   :: nxdim,nydim
  integer(i_kind)   :: icount
  
  !  Output files
!----


!- unit grid lat-lon distances (Earth difference divided by grid difference)
!- calculated from all averaged grid areas
!!- note: this can be relaxed if the unit grid area (km) is known (i.e. darea)
!! darea = (r*cos(lat)*dlon)*(dlat)


  rearth2=(rearth/r1000)**2  !! squared earth radius in km (need for darea calculation)

  if (ndata_strike>0) then
 
     darea_sum=0._r_kind
     do iobs=1,ndata_strike

        ii0=INT(cdata_strike(2,iobs))
        jj0=INT(cdata_strike(3,iobs))

        delta_lon=ges_xlon(jj0,ii0+1,1)-ges_xlon(jj0,ii0,1)
        delta_lat=ges_xlat(jj0+1,ii0,1)-ges_xlat(jj0,ii0,1)
  
        lat_ref  =half*(cdata_strike(13,iobs)+cdata_strike(13,iobs-1))

        cosine=cos(lat_ref*deg2rad)

        darea_sum=darea_sum+rearth2*cos(lat_ref*deg2rad)*(abs(delta_lon)*deg2rad)*&
                  (abs(delta_lat)*deg2rad)

     end do  !! do iobs=2,ndata_strike

     darea=darea_sum/real(ndata_strike,r_kind)
  else   !! ndata_strike=0
     darea=zero

  end if  !! if(ndata_strike>0) then

  dtime=real(nhr_assimilation,r_kind)

  ! Regional

  if (regional) then

!-- WRF-ARW

     if (wrf_mass_regional) then
      
        nxdim=lon2
        nydim=lat2

     endif ! wrf_mass_regional

  endif !if (regional) then

! Global

  if (.not. regional) then

     nxdim=nlon
     nydim=nlat

  endif !  end global block 

!!! Allocate new var for flash rate here (nxdim,nydim)
! asign zero to all points
! update the relevant points in the loop

  ngridh=nxdim*nydim


  allocate(gtim_central(1:ngridh))
  allocate(glon_central(1:ngridh))
  allocate(glat_central(1:ngridh))
  allocate( lon_central(1:ngridh))
  allocate( lat_central(1:ngridh))
  allocate(      lcount(1:ngridh))

  lcount(:)=0
  glon_central(:)=zero
  glat_central(:)=zero
  lon_central(:) =zero
  lat_central(:) =zero
  gtim_central(:)=zero
     
  do iobs=1,ndata_strike

     xx=cdata_strike(2,iobs)   !! glon
     yy=cdata_strike(3,iobs)   !! glat
     ii0=INT(cdata_strike(2,iobs))
     jj0=INT(cdata_strike(3,iobs))

!!   find lightning strikes near the (ii0,jj0) point

     xbound=real(ii0,r_kind)
     ybound=real(jj0,r_kind)

     xflag=(xx>xbound) .AND. (xx<xbound+1.)
     yflag=(yy>ybound) .AND. (yy<ybound+1.)

     if (xflag .AND. yflag ) then

        index=(jj0-1)*nxdim+ii0
        lcount(index)=lcount(index)+1
       
        glon_central(index)=glon_central(index)+cdata_strike(2,iobs)
        glat_central(index)=glat_central(index)+cdata_strike(3,iobs)
        lon_central(index)= lon_central(index)+cdata_strike(12,iobs) 
        lat_central(index)= lat_central(index)+cdata_strike(13,iobs)

        if (lcount(index)<2) then
            gtim_central(index)=cdata_strike(6,iobs)
        else
            call convert_time (gtim_central(index),cdata_strike(6,iobs),lcount(index))
        end if 

     end if  !! if(xflag .AND. yflag ) 
      
  enddo !! do iobs=1,ndata_strike



!-- find the center of mass

  do index=1,ngridh
     if (lcount(index)>0) then
        glon_central(index)=glon_central(index)/real(lcount(index),r_kind)
        glat_central(index)=glat_central(index)/real(lcount(index),r_kind)
        lon_central(index)= lon_central(index)/real(lcount(index),r_kind)
        lat_central(index)= lat_central(index)/real(lcount(index),r_kind)
     endif  !! if(lcount(index)>0) then
  enddo  !! do index=1,ngridh

!-- find the original index of the nearest strike (need for transfer of input obs)

  allocate(ind_min(1:ngridh))
  ind_min(:)=-99

  dist_min=1.e10_r_kind
  do iobs=1,ndata_strike

     xx=cdata_strike(2,iobs)   !! glon
     yy=cdata_strike(3,iobs)   !! glat
     ii0=INT(cdata_strike(2,iobs))
     jj0=INT(cdata_strike(3,iobs))
     index=(jj0-1)*nxdim+ii0

     if (lcount(index)>0) then
        dist2=(xx-glon_central(index))**2+(yy-glat_central(index))**2
        if (dist2<dist_min) then
           dist_min=dist2
           ind_min(index)=iobs
        end if
     end if  !! if(lcount(index)>0) then

  enddo  !! do iobs=1,ndata_strike

!----
!---- Output
!----

!-- count the non-zero flash rates and assign a temporary domain cdata_flash_h
!-- Note: it is assumed that only non-zero flash rates are true observations

  icount=0
  do index=1,ngridh
     if (lcount(index)>0) then
        icount=icount+1
        cdata_flash_h( 1,icount)=cdata_strike( 1,ind_min(index))
        cdata_flash_h( 2,icount)=glon_central(index)
        cdata_flash_h( 3,icount)=glat_central(index)

        if (darea>0._r_kind) then
           cdata_flash_h( 4,icount)=real(lcount(index),r_kind)/(darea*dtime)
        else 
           cdata_flash_h( 4,icount)=0. 
        end if

        cdata_flash_h( 5,icount)=cdata_strike( 5,ind_min(index))
        cdata_flash_h( 6,icount)=gtim_central(index)
        cdata_flash_h( 7,icount)=cdata_strike( 7,ind_min(index))
        cdata_flash_h( 8,icount)=cdata_strike( 8,ind_min(index))
        cdata_flash_h( 9,icount)=cdata_strike( 9,ind_min(index))
        cdata_flash_h(10,icount)=cdata_strike(10,ind_min(index))
        cdata_flash_h(11,icount)=usage                 
        cdata_flash_h(12,icount)=lon_central(index)
        cdata_flash_h(13,icount)=lat_central(index)

     endif  !! if(lcount(index)>0) then

  enddo  !! do index=1,ngridh

  ndata_flash=icount

  deallocate(ind_min)
  deallocate(lcount)
  deallocate(gtim_central)
  deallocate(glon_central) 
  deallocate(glat_central)
  deallocate(lon_central) 
  deallocate(lat_central)

!-----
! End of routine
  return

end subroutine convert_to_flash_rate

!-----
subroutine convert_time (date_old,date_new,nmax)

!$$$  documentation block
!                .      .    .                                       .
! subroutine:  convert_time      
! prgmmr: k apodaca               date: 2015-11-18 
!
! abstract:  This subroutine performs a date conversion to deal with
!            computer precission dependencies associated with a 10-digit 
!            float for the analysis date/time.
!-- 
  use kinds, only: r_kind,i_kind

  implicit none

  integer(i_kind), intent(in) :: nmax
  real(r_kind), intent(inout) :: date_old
  real(r_kind), intent(in)  :: date_new
  integer(i_kind) :: i,sumidd
  integer(i_kind) :: idd,jdd,kdd
  real(r_kind), allocatable :: xdate(:) 
  real(r_kind) :: dd,hh,ysumidd,xsumidd
  real(r_kind) :: xdd,xhh,ydate
  real(r_kind) :: xccyy

  allocate(xdate(1:nmax))

  xdate(1:nmax-1) = date_old
  xdate(nmax) = date_new
    
  sumidd=0._r_kind
  do i=1,nmax
     xccyy = INT(1.0e-8_r_kind*xdate(i))*1.0e8_r_kind 
     xdate(i) = INT(xdate(i))-xccyy   

     jdd=INT(0.0001_r_kind*xdate(i))
     idd=INT(xdate(i))-jdd*10000

     ysumidd=real(idd,r_kind)
     dd=real(INT(0.01_r_kind*ysumidd),r_kind)
     hh=ysumidd-dd*100._r_kind

     sumidd=sumidd+dd*24._r_kind+hh

  enddo  !! do i=1,nmax

  xsumidd=real(sumidd,r_kind)/nmax
  ysumidd=real(INT(xsumidd),r_kind)

  kdd=INT(xsumidd/24._r_kind)
  xdd=real(kdd,r_kind)
  xhh=ysumidd-real(kdd,r_kind)*24._r_kind

  ydate=real(jdd,r_kind)*10000._r_kind+xdd*100._r_kind+xhh+xccyy
      
  date_old=ydate 
      
  deallocate(xdate)

end subroutine convert_time

