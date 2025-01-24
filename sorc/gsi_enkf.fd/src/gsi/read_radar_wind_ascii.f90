subroutine read_radar_wind_ascii(nread,ndata,nodata,infile,lunout,obstype,sis,hgtl_full,nobs)
!$$$   subprogram documentation block
!                .      .    .                                       .
!   subprogram: read_dbz        read level2 raw QC'd radial velocity data
!   
!   prgmmr: carley          org: np22                date: 2011-05-24
!
! abstract: Reads and processes level 2 horizontal radial velocity (m/s) by 
!                radar site.  Data are on radar scan surafces.  Processing includes
!                finding the lat/lon and height of each observation. .
!
! program history log:
!   2011-08-12  carley - fix ob error to 2 m/s
!   2011-08-23  carley - use deter_sfc_mod
!   2011-12-08  carley - add wind rotation (earth to grid)
!   2020-05-04  wu   - no rotate_wind for fv3_regional
!
!   input argument list:
!     infile   - file from which to read data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!
!   output argument list:
!     nread    - number of radar reflectivity observations read
!     ndata    - number of radar reflectivity observations retained for further processing
!     nodata   - number of radar reflectivity observations retained for further processing
!     sis      - satellite/instrument/sensor indicator
!
! Variable Definitions:
!
!  a43 - real - (4/3)*(earth radius)   
!  a,b,c,ha,epsh,h,aactual - real - used in computing radar observation height 
!  cdata_all - real - dim(maxdat,maxobs) - array holding all data for assimilation
!  celev0,selev0 - real- cos and sin of elevation angle (raw)
!  celev,selev - real - corrected cos and sin of elevation angle
!  clat0 - real - cos of radar station latitude
!  cstaid - char - radar station ide
!  dbzerr - real - observation error (obtained from convinfo - dBZ)
!  dlat - real - grid relative latitude of observation (grid units)
!  dlon - real - grid relative longitude of observation (grid units)
!  gamma - real - used in finding observation latlon
!  lunrad - int - unit number for reading radar data from file
!  maxobs - int - max number of obs converted to no precip observations
!  num_m2nopcp -int - number of missing obs 
!  num_missing - int - number of missing observations
!  num_noise - int - number of rejected noise observations
!  num_nopcp - int - number of noise obs converted to no precip observations
!  numbadtime - int - number of elevations outside time window
!  num_badtilt - int - number of elevations outside specified interval
!  num_badrange - int - number of obs outside specified range distance
!  obdate - int - dim(5) - yyyy,mm,dd,hh,minmin of observation
!  outside - logical - if observations are outside the domain -> true
!  radartwindow - real - time window for radar observations (minutes)
!  rlatglob - real - earth relative latitude of observation (radians)
!  rlatloc - real - latitude of observation on radar-relative projection
!  rlonglob - real - earth relative longitude of observation (radians)
!  rlonloc - real - longitude of observation on radar-relative projection
!  rlon0 - real - radar station longitude (radians)
!  rmins_an - real - analysis time from reference date (minutes)
!  rmins_ob - real -  observation time from reference date (minutes)
!  rstation_id - real - radar station id
!  slat0 - real - sin of radar station latitude
!  thisazimuthr - real - 90deg minues the actual azimuth and converted to radians
!  thiserr - real - observation error
!  thislat - real - latitude of observation
!  thislon - real - longitude of observation
!  thisrange - real - range of observation from radar
!  thishgt - real - observation height
!  this_stahgt - real - radar station height (meters about sea level)
!  this_staid - char - radar station id
!  thistilt - real - radar tilt angle (degrees)
!  thistiltr - real- radar tilt angle (radians)
!  timeb - real - obs time (analyis relative minutes)
!
!  
!
! Derived data types
!
!  radar - derived data type for containing volume scan information
!     nelv- int - number of elevation angles 
!     radid - char*4 - radar ID (e.g. KAMA)
!     vcpnum - int - volume coverage pattern number
!     year - int - UTC
!     day - int - UTC
!     month - int - UTC
!     hour - in - UTC
!     minute - int - UTC
!     second - int - UTC
!     radhgt - real - elevation of the radar above sea level in meters (I believe
!              this includes the height of the antenna as well)
!     radlat - real - latitude location of the radar
!     radlon - real - longitude location of the radar
!     fstgatdis - real - first gate distance (meters)
!     gatewidth - real - gate width (meters)
!     elev_angle - real - radar elevation angle (degrees)
!     num_beam - int - number of beams
!     num_gate - int - number of gates
!     nyq_vel - real - nyquist velocity 
!     azim - real - azimuth angles
!     field - real - radar data variable (reflectivity or velocity)
!
! Defined radar types:
!  strct_in_vel - radar - contains volume scan information related to 
!                         radial velocity
!  strct_in_dbz - radar - contains volume scan information related to
!                         radar reflectivity
!  strct_in_rawvel - radar - contains volume scan information related to
!                            raw radial velocity    
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,half,one,two,deg2rad,rearth,rad2deg, &
                       one_tenth,r1000,r60,r60inv,r100,r400,grav_equator, &
                        eccentricity,somigliana,grav_ratio,grav,semi_major_axis,flattening 
  use gridmod, only: regional,tll2xy,rotate_wind_ll2xy,nsig,nlat,nlon,&
                     fv3_regional
  use obsmod, only: iadate, &
    mintiltvr,maxtiltvr,minobrangevr,maxobrangevr,rmesh_vr,zmesh_vr,pmot_vr,&
    doradaroneob,oneoblat,oneoblon,oneobheight,oneobradid
  use obsmod,only: radar_no_thinning,reduce_diag
  use gsi_4dvar, only: l4dvar,time_4dvar
  use convinfo, only: nconvtype,ctwind,icuse,ioctype
  use convthin, only: make3grids,map3grids_m,del3grids,use_all
  use read_l2bufr_mod, only: invtllv
  use qcmod, only: erradar_inflate
  use deter_sfc_mod, only: deter_sfc2,deter_zsfc_model   
  use mpimod, only: npe
       
  implicit none
  
! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=*),intent(in   ) :: sis
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata
  real(r_kind),dimension(nlat,nlon,nsig),intent(in):: hgtl_full
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs

! Declare local parameters
  real(r_kind),parameter :: four_thirds = 4.0_r_kind / 3.0_r_kind
  real(r_kind),parameter :: r8     = 8.0_r_kind
  real(r_kind),parameter:: r6 = 6.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  integer(i_kind),parameter:: maxdat=22         ! Used in generating cdata array
  
!--Derived data type declaration

  type :: radar
     character(4) :: radid
     integer(i_kind) :: vcpnum
     integer(i_kind) :: year           
     integer(i_kind) :: month          
     integer(i_kind) :: day            
     integer(i_kind) :: hour           
     integer(i_kind) :: minute         
     integer(i_kind) :: second
     real(r_kind) :: radlat
     real(r_kind) :: radlon
     real(r_kind) :: radhgt
     real(r_kind) :: fstgatdis    
     real(r_kind) :: gateWidth
     real(r_kind) :: elev_angle
     integer(i_kind) :: num_beam       
     integer(i_kind) :: num_gate
     real(r_kind) :: nyq_vel
     real(r_kind),allocatable :: azim(:)      !has dimension (num_beam)
     real(r_kind),allocatable :: field(:,:)   !has dimension (num_gate,num_beam)
  end type radar

!--Counters for diagnostics
  integer(i_kind) :: num_missing=0,numbadtime=0, &   !counts
                     num_badtilt=0,num_badrange=0, &
                     ibadazm=0 

  integer(i_kind) :: ithin,zflag,nlevz,icntpnt,klon1,klat1,kk,klatp1,klonp1
  real(r_kind) :: rmesh,xmesh,zmesh,dx,dy,dx1,dy1,w00,w01,w10,w11
  real(r_kind), allocatable, dimension(:) :: zl_thin
  real(r_kind),dimension(nsig):: hges,zges
  real(r_kind) sin2,termg,termr,termrg,zobs,height
  integer(i_kind) iout,ntdrvr_thin2
  real(r_kind) crit1,timedif
  real(r_kind),parameter:: r16000 = 16000.0_r_kind
  logical :: luse

!--General declarations
  integer(i_kind) :: ierror,lunrad,i,j,k,v,na,nb,nelv,nvol, &
                     ikx,mins_an,mins_ob
  integer(i_kind) :: maxobs,nchanl,ilat,ilon,idomsfc
  
  integer(i_kind),dimension(5) :: obdate
  
  real(r_kind) :: b,c,ha,epsh,h,aactual,a43,thistilt,ff10,sfcr,skint,zsges, &
                  radar_lon,radar_lat,dlon_radar,dlat_radar,errmax,errmin,error                             
  real(r_kind) :: thistiltr,selev0,celev0,thisrange,this_stahgt,thishgt                           
  real(r_kind) :: celev,selev,gamma,thisazimuthr,rlon0,t4dv, &
                  clat0,slat0,dlat,dlon,thiserr,thislon,thislat, &
                  rlonloc,rlatloc,rlonglob,rlatglob,timeb,rad_per_meter
  real(r_kind) :: azm,cosazm_earth,sinazm_earth,cosazm,sinazm  
  real(r_kind) :: radartwindow,usage
  real(r_kind) :: rmins_an,rmins_ob                                                     
  real(r_kind),allocatable,dimension(:,:):: cdata_all
  real(r_double) rstation_id
  logical, allocatable,dimension(:)     :: rusage,rthin
  logical save_all
! integer(i_kind)  numthin,numqc,numrem,numall
  integer(i_kind) nxdata,pmot
  
  character(8) cstaid
  character(4) this_staid
  equivalence (this_staid,cstaid)
  equivalence (cstaid,rstation_id)

  logical      :: outside

  type(radar),allocatable :: strct_in_vel(:,:)

  real(r_kind) :: mintilt,maxtilt,maxobrange,minobrange

  integer(i_kind) :: thin_freq=1

  mintilt=mintiltvr
  maxtilt=maxtiltvr
  minobrange=minobrangevr
  maxobrange=maxobrangevr

  !-Check if radial velocity is in the convinfo file and extract necessary attributes 
  
  ithin=1 !number of obs to keep per grid box
  if(radar_no_thinning) then
    ithin=-1
  endif

  errmax=-huge(errmax)
  errmin=huge(errmin)
  
  ikx=0
  do i=1,nconvtype
     if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== 1) then
        ikx=i 
        radartwindow=ctwind(ikx)*r60         !Time window units converted to minutes 
                                             !  (default setting for dbz within convinfo is 0.05 hours)
        thiserr= 2_r_kind !1.75_r_kind !2_r_kind                     !Ob error (m/s) to use for radial velocity
        exit                                 !Exit loop when finished with initial convinfo fields     
     else if ( i==nconvtype ) then
        write(6,*) 'READ_RADAR_WIND_ASCII: ERROR - OBSERVATION TYPE IS NOT PRESENT IN CONVINFO OR USE FLAG IS ZERO'
        write(6,*) 'READ_RADAR_WIND_ASCII: ABORTING read_radar_wind_ascii.f90 - NO VELOCITY OBS READ!'
        return   
     endif
  end do   
 
    
  if (minobrange >= maxobrange) then
    write(6,*) 'MININMUM OB RANGE >= MAXIMUM OB RANGE FOR READING RADIAL VELOCITY - PROGRAM STOPPING FROM READ_RADAR_WIND_ASCII.F90'
    call stop2(400)
  end if
        
  !-next three values are dummy values for now
  nchanl=0
  ilon=2
  ilat=3
  
  maxobs=50000000    !value taken from read_radar.f90 

  !--Allocate cdata_all array

  allocate(cdata_all(maxdat,maxobs),rusage(maxobs),rthin(maxobs))


  rmesh=rmesh_vr
  zmesh=zmesh_vr

  ntdrvr_thin2=0
  icntpnt=0
  zflag=0

  use_all=.true.
  if (ithin == 1) then
     write(6,*)'READ_RADAR: rmesh :',rmesh
     use_all=.false.
     if(zflag == 0)then
        nlevz=nsig
     else
        nlevz=r16000/zmesh
     endif
     xmesh=rmesh
     call make3grids(xmesh,nlevz)

     allocate(zl_thin(nlevz))
     if (zflag == 1) then
        do k=1,nlevz
           zl_thin(k)=k*zmesh
        enddo
     endif
     write(6,*)'READ_RADAR: xmesh, zflag, nlevz =', xmesh, zflag, nlevz
  endif

       
  lunrad=31
  open(lunrad,file=trim(infile),status='old',action='read', &
       iostat=ierror,form='formatted')

  
  fileopen: if (ierror == 0) then    
         read(lunrad,'(2i8)') nelv,nvol               !read number of elevations and number of volumes
    
     
     !*************************IMPORTANT***************************!
     !                                                             !
     !    All data = 999.0 correspond to missing or bad data       !       
     !                                                             !
     !*************************************************************!
     
         
 !------Begin processing--------------------------!  

          rusage = .true.
          rthin = .false.
          use_all=.true.


         !-Obtain analysis time in minutes since reference date

          call w3fs21(iadate,mins_an)  !mins_an -integer number of mins snce 01/01/1978
          rmins_an=mins_an             !convert to real number
          
          volumes: do v=1,nvol 
           
            read(lunrad,'(i8)') nelv 
            allocate(strct_in_vel(1,nelv))
            tilts: do k=1,nelv

               read(lunrad,'(a4)') strct_in_vel(1,k)%radid
               read(lunrad,'(i8)') strct_in_vel(1,k)%vcpnum
               read(lunrad,'(6i8)') strct_in_vel(1,k)%year              &
                                   ,strct_in_vel(1,k)%month                &
                                   ,strct_in_vel(1,k)%day                  &
                                   ,strct_in_vel(1,k)%hour                 &
                                   ,strct_in_vel(1,k)%minute               &
                                   ,strct_in_vel(1,k)%second
               read(lunrad,'(2f10.3,f10.1)') strct_in_vel(1,k)%radlat   &
                                            ,strct_in_vel(1,k)%radlon      &
                                            ,strct_in_vel(1,k)%radhgt
               read(lunrad,'(2f8.1)') strct_in_vel(1,k)%fstgatdis       &
                                     ,strct_in_vel(1,k)%gateWidth
               read(lunrad,'(f8.3)') strct_in_vel(1,k)%elev_angle
               read(lunrad,'(2i8)') strct_in_vel(1,k)%num_beam          &
                                   ,strct_in_vel(1,k)%num_gate
               na=strct_in_vel(1,k)%num_beam
               nb=strct_in_vel(1,k)%num_gate
             
                !******allocate arrays within radar data type**********!
               allocate(strct_in_vel(1,k)%azim(na))
               allocate(strct_in_vel(1,k)%field(nb,na))
                !******************************************************!
                  
               read(lunrad,'(f8.3)') strct_in_vel(1,k)%nyq_vel
               read(lunrad,'(15f6.1)') (strct_in_vel(1,k)%azim(j),j=1,na)
               read(lunrad,'(20f6.1)') ((strct_in_vel(1,k)%field(i,j),i=1,nb),j=1,na)


               obdate(1)=strct_in_vel(1,k)%year
               obdate(2)=strct_in_vel(1,k)%month  
               obdate(3)=strct_in_vel(1,k)%day 
               obdate(4)=strct_in_vel(1,k)%hour   
               obdate(5)=strct_in_vel(1,k)%minute 
               call w3fs21(obdate,mins_ob)                             !mins_ob -integer number of mins snce 01/01/1978
               rmins_ob=mins_ob                                        !convert to real number
               rmins_ob=rmins_ob+(strct_in_vel(1,k)%second*r60inv)     !convert seconds to minutes and add to ob time
         
              !-Comparison is done in units of minutes
              
               timeb = rmins_ob-rmins_an


               if(doradaroneob .and. (oneobradid /= strct_in_vel(1,k)%radid)) cycle tilts

               if(abs(timeb) > abs(radartwindow)) then
                  numbadtime=numbadtime+1  
                  cycle tilts                           !If not in time window, cycle the loop
               end if                  
              !--Time window check complete--!

               thistilt=strct_in_vel(1,k)%elev_angle
               if (thistilt <= maxtilt .and. thistilt >= mintilt) then 
             
                  gates: do i=1,strct_in_vel(1,k)%num_gate,thin_freq  
                      thisrange=strct_in_vel(1,k)%fstgatdis + real(i-1,r_kind)*strct_in_vel(1,k)%gateWidth
                     
                     !-Check to make sure observations are within specified range 

                      if (thisrange <= maxobrange .and. thisrange >= minobrange) then    
             
                      azms: do j=1,strct_in_vel(1,k)%num_beam
           
                           !-Check to see if this is a missing observation)
                            nread=nread+1
                            if ( strct_in_vel(1,k)%field(i,j) >= 999.0_r_kind ) then
                               num_missing=num_missing+1
                               cycle azms                        !No reason to process the ob if it is missing      	       
                            end if
                                
                           !--Find observation height using method from read_l2bufr_mod.f90										       
                 
                            this_stahgt=strct_in_vel(1,k)%radhgt
                            aactual=rearth+this_stahgt                    
                            a43=four_thirds*aactual
                    thistiltr=thistilt*deg2rad
                    selev0=sin(thistiltr)
                    celev0=cos(thistiltr)      
                    b=thisrange*(thisrange+two*aactual*selev0)
                    c=sqrt(aactual*aactual+b)
                    ha=b/(aactual+c)
                    epsh=(thisrange*thisrange-ha*ha)/(r8*aactual)
                    h=ha-epsh
                    thishgt=this_stahgt+h 
                    height=thishgt
                   !--Find observation location using method from read_l2bufr_mod.f90

                   !-Get corrected tilt angle
                    celev=celev0
                    selev=selev0
                    celev=a43*celev0/(a43+h)
                    selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
          
                    gamma=half*thisrange*(celev0+celev)
         
                   !-Get earth lat lon of observation
          
                    rlon0=deg2rad*strct_in_vel(1,k)%radlon
                    clat0=cos(deg2rad*strct_in_vel(1,k)%radlat)
                    slat0=sin(deg2rad*strct_in_vel(1,k)%radlat)
                    thisazimuthr=(90.0_r_kind-strct_in_vel(1,k)%azim(j))*deg2rad   !Storing as 90-azm to
                                                                                   ! be consistent with 
                                                                                   ! read_l2bufr_mod.f90
                    rad_per_meter=one/rearth
                    rlonloc=rad_per_meter*gamma*cos(thisazimuthr)
                    rlatloc=rad_per_meter*gamma*sin(thisazimuthr)
                  
                    call invtllv(rlonloc,rlatloc,rlon0,clat0,slat0,rlonglob,rlatglob)
                 
                    thislat=rlatglob*rad2deg
                    thislon=rlonglob*rad2deg 

                 if(doradaroneob) then
                    thislat=oneoblat
                    thislon=oneoblon
                    thishgt=oneobheight
                 endif


                 if(thislon>=r360) thislon=thislon-r360
                 if(thislon<zero ) thislon=thislon+r360
                 
            !-Convert back to radians                 
              
                 thislat = thislat*deg2rad
                 thislon = thislon*deg2rad
                
            !find grid relative lat lon locations of earth lat lon
                 
                 call tll2xy(thislon,thislat,dlon,dlat,outside)
                 if (outside) cycle azms             !If observation is outside the domain
                                                     ! then cycle, but don't increase range right away.
                                                     ! Domain could be rectangular, so ob may be out of
                                                     ! range at one end, but not the other.
                
                 if(regional .and. .not. fv3_regional) then
                    cosazm_earth=cos(thisazimuthr)
                    sinazm_earth=sin(thisazimuthr)
                    call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,thislon,dlon,dlat)
                    azm=atan2(sinazm,cosazm)
                 else
                    azm=thisazimuthr
                 end if      
   
           !--Do limited QC from read_radar.f90--!
                 error = erradar_inflate*thiserr
                 errmax=max(error,errmax)
                 if(thiserr>zero) errmin=min(error,errmin)                    
                 if(abs(azm)>r400) then
                    ibadazm=ibadazm+1
                    cycle azms
                 end if
    
                 this_staid=strct_in_vel(1,k)%radid      !Via equivalence in declaration, value is propagated
                                                         !  to rstation_id used below. 	    
    
             ! Get model terrain at radar station location
             ! If radar station is outside of grid, does not mean the 
             !    radar obs are outside the grid - therefore no need to
             !    cycle azms.
    
                 radar_lon=deg2rad*strct_in_vel(1,k)%radlon
                 radar_lat=deg2rad*strct_in_vel(1,k)%radlat
                 call tll2xy(radar_lon,radar_lat,dlon_radar,dlat_radar,outside)
                 call deter_zsfc_model(dlat_radar,dlon_radar,zsges)
    
             !  Determines land surface type based on surrounding land
             !    surface types
                     
                 t4dv=timeb*r60inv
     
                 call deter_sfc2(thislat,thislon,t4dv,idomsfc,skint,ff10,sfcr)

!####################       Data thinning       ###################

                 icntpnt=icntpnt+1
                 if(icntpnt>maxobs) exit
                 pmot=pmot_vr
                 if(reduce_diag .and. pmot < 2)pmot=pmot+2
                 save_all=.false.
                 if(pmot /= 2 .and. pmot /= 0) save_all=.true.

                 usage = zero
                 if(abs(icuse(ikx)) /= 1)usage=r100
 
                 if(ithin == 1)then
                   if(zflag == 0)then
                     klon1= int(dlon);  klat1= int(dlat)
                     dx   = dlon-klon1; dy   = dlat-klat1
                     dx1  = one-dx;     dy1  = one-dy
                     w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy
  
                     klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
                     if (klon1==0) klon1=nlon
                     klatp1=min(nlat,klat1+1); klonp1=klon1+1
                     if (klonp1==nlon+1) klonp1=1
                     do kk=1,nsig
                       hges(kk)=w00*hgtl_full(klat1 ,klon1 ,kk) +  &
                                w10*hgtl_full(klatp1,klon1 ,kk) + &
                                w01*hgtl_full(klat1 ,klonp1,kk) + &
                                w11*hgtl_full(klatp1,klonp1,kk)
                     end do
                     sin2  = sin(thislat)*sin(thislat)
                     termg = grav_equator * &
                     ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
                     termr = semi_major_axis /(one + flattening + grav_ratio -  &
                        two*flattening*sin2)
                     termrg = (termg/grav)*termr
                     do kk=1,nsig
                       zges(kk) = (termr*hges(kk)) / (termrg-hges(kk))
                       zl_thin(kk)=zges(kk)
                     end do
                   endif

                   zobs = height

                   if (l4dvar) then
                      timedif = zero
                   else
!                 timedif=abs(t4dv-toff)
                      timedif=abs(t4dv) !don't know about this
                   endif
                   crit1 = timedif/r6+half
 
                   call map3grids_m(1,save_all,zflag,zl_thin,nlevz, &
                     thislat,thislon,zobs,crit1,ndata,&
                     luse,maxobs,rthin,.false.,.false.)

                   if (.not. luse) then
                      ntdrvr_thin2=ntdrvr_thin2+1
                      cycle
                   endif

                 else
                   ndata =ndata+1
                 endif
                 iout=ndata

                 cdata_all(1,iout) = error                         ! wind obs error (m/s)
                 cdata_all(2,iout) = dlon                          ! grid relative longitude
                 cdata_all(3,iout) = dlat                          ! grid relative latitude
                 cdata_all(4,iout) = thishgt                       ! obs absolute height (m)
                 cdata_all(5,iout) = strct_in_vel(1,k)%field(i,j)  ! wind obs (m/s)
                 cdata_all(6,iout) = azm                           ! azimuth angle (radians)
                 cdata_all(7,iout) = t4dv                          ! obs time (hour) - analysis relative
                 cdata_all(8,iout) = ikx                           ! type 	      
                 cdata_all(9,iout) = thistiltr                     ! tilt angle (radians)
                 cdata_all(10,iout)= this_stahgt                   ! station elevation (m)
                 cdata_all(11,iout)= rstation_id                   ! station id
                 cdata_all(12,iout)= icuse(ikx)                    ! usage parameter
                 cdata_all(13,iout)= idomsfc                       ! dominate surface type
                 cdata_all(14,iout)= skint                         ! skin temperature
                 cdata_all(15,iout)= ff10                          ! 10 meter wind factor
                 cdata_all(16,iout)= sfcr                          ! surface roughness
                 cdata_all(17,iout)=thislon*rad2deg                ! earth relative longitude (degrees)
                 cdata_all(18,iout)=thislat*rad2deg                ! earth relative latitude (degrees)
                 cdata_all(19,iout)=thisrange/1000._r_kind         ! range from radar in km (used to estimate beam spread)
                 cdata_all(20,iout)=zsges                          ! model elevation at radar site
                 cdata_all(21,iout)=thiserr
                 cdata_all(22,iout)=two                            ! Level 2 data
 
                 if(doradaroneob .and. (cdata_all(5,iout) > -99_r_kind) ) exit volumes
                 if(usage >= r100)rusage(iout)=.false.

               end do azms  !j
            else
               num_badrange=num_badrange+1      !If outside acceptable range, increment
            end if   !Range check	

         end do gates    !i
     
       else
         num_badtilt=num_badtilt+1           !If outside acceptable tilts, increment
       end if         !Tilt check
  
    end do tilts       !k

    do k=1,nelv
       deallocate(strct_in_vel(1,k)%azim)
       deallocate(strct_in_vel(1,k)%field)
    enddo
    deallocate(strct_in_vel)
  end do volumes      !v 
      
  close(lunrad) !modified to do one scan at a time 

  if (.not. use_all) then
     deallocate(zl_thin) 
     call del3grids
  endif
!end modified for thinning


  nxdata=ndata
  ndata=0
  if(nxdata > 0)then
!    numthin=0
!    numqc=0
!    numrem=0
!    do i=1,nxdata
!      if(.not. rusage(i))then
!         numqc=numqc+1
!      else if(rthin(i))then
!         numthin=numthin+1
!      else
!         numrem=numrem+1
!      end if
!    end do
!    write(6,*) ' asciiradar ',trim(ioctype(ikx)),ikx,numall,&
!           numrem,numqc,numthin
!   If thinned data set quality mark to 14
     if (ithin == 1 ) then
        do i=1,nxdata
          if(rthin(i))cdata_all(12,i)=101._r_kind
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
                do k=1,maxdat
                   cdata_all(k,ndata)=cdata_all(k,i)
                end do
             end if
          end if
        end do
     end if
   end if
   nodata=nodata+ndata

!---all looping done now print diagnostic output

  write(6,*)'READ_RADAR_WIND_ASCII: Reached eof on radar wind ascii file'
  write(6,*)'READ_RADAR_WIND_ASCII: # volumes in input file             =',nvol
  write(6,*)'READ_RADAR_WIND_ASCII: # elevations per volume             =',nelv
  write(6,*)'READ_RADAR_WIND_ASCII: # elevations outside time window    =',numbadtime
  write(6,*)'READ_RADAR_WIND_ASCII: # of missing data                   =',num_missing
  write(6,*)'READ_RADAR_WIND_ASCII: # outside specif. range             =',num_badrange
  write(6,*)'READ_RADAR_WIND_ASCII: # outside specif. tilts             =',num_badtilt
  write(6,*)'READ_RADAR_WIND_ASCII: # bad azimuths                      =',ibadazm
!---Write observation to scratch file---!
  
  call count_obs(ndata,maxdat,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,maxdat,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)
 
  
  !---------------DEALLOCATE ARRAYS-------------!
 

 else  !fileopen
    write(6,*) 'READ_RADAR_WIND_ASCII: ERROR OPENING RADIAL VELOCITY FILE: ',trim(infile),' IOSTAT ERROR: ',ierror, ' SKIPPING...'
 end if fileopen

 deallocate(cdata_all,rusage,rthin)


end subroutine read_radar_wind_ascii

