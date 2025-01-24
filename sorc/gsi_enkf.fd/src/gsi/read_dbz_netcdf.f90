subroutine read_dbz_mrms_netcdf(nread,ndata,nodata,infile,obstype,lunout,sis,nobs)
!$$$   subprogram documentation block
!                .      .    .                                       .
!   subprogram: read_dbz        read level2 raw QC'd radar reflectivity files
!   
!   prgmmr: carley          org: np22                date: 2011-04-04
!
! abstract: Reads and processes level 2 horizontal radar reflectivity (dBZ) by 
!                radar site.  Data are on radar scan surafces. Also reads, but does
!                not process unfolded radial velocities.  Processing includes
!                finding the lat/lon and height of each observation. 
!                This formulation is not outfitted for 4dvar, but will
!                work with 3dvar and hybrid ensemble.
!
! program history log:
!   2011-08-12  carley - Fix dBZ oberror to be 3dBZ and add optional
!                        upper bound limit to observed dBZ to account
!                        for representativeness error.
!   2011-12-08  carley - Fix dBZ oberror to 5 dBZ 
!           
!   2015        Lei  -- modify from read_dbz to read_dbz_mrms_netcdf
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
  use netcdf
  use kinds, only: r_kind,r_double,i_kind,r_single
  use constants, only: zero,half,one,two,deg2rad,rearth,rad2deg, &
                       one_tenth,r1000,r60,r60inv,r100,r400
  use gridmod, only: tll2xy
  use obsmod, only: iadate
  use convinfo, only: nconvtype,ctwind,icuse,ioctype
  use mpimod, only: npe
  use read_l2bufr_mod, only : invtllv
       
  implicit none
  
! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=*),intent(in   ) :: sis
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe),intent(inout) :: nobs

! Declare local parameters
  real(r_kind),parameter :: four_thirds = 4.0_r_kind / 3.0_r_kind
  real(r_kind),parameter :: r8     = 8.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  integer(i_kind),parameter:: maxdat=17         ! Used in generating cdata array
  integer (i_kind):: iyear,imon,iday,ihour,imin,isec
  
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
 integer(i_kind) :: num_missing=0,num_nopcp=0, &      !counts 
                    numbadtime=0,num_badtilt=0, &    
                    num_badrange=0,num_m2nopcp=0, &
                    num_noise=0,num_limmax=0 ,num_limmin=0    
     
                                                          

!--General declarations
  integer(i_kind) :: ierror,lunrad,i,j,k,v,na,nb,nelv,nvol, &
                     ikx,mins_an,mins_ob
  integer(i_kind) :: maxobs,nchanl,ilat,ilon,scount
  
  integer(i_kind),dimension(5) :: obdate
  
  real(r_kind) :: b,c,ha,epsh,h,aactual,a43,thistilt                             
  real(r_kind) :: thistiltr,selev0,celev0,thisrange,this_stahgt,thishgt                           
  real(r_kind) :: celev,selev,gamma,thisazimuthr,rlon0, &
                  clat0,slat0,dlat,dlon,thiserr,thislon,thislat, &
                  rlonloc,rlatloc,rlonglob,rlatglob,timeb,rad_per_meter  
  real(r_kind) :: radartwindow
  real(r_kind) :: dbzerr,rmins_an,rmins_ob                                                     
  real(r_kind),allocatable,dimension(:,:):: cdata_all
  real(r_double) rstation_id
  
  character(8) cstaid
  character(4) this_staid
  equivalence (this_staid,cstaid)
  equivalence (cstaid,rstation_id)

  logical      :: outside
    
  type(radar),allocatable :: strct_in_dbz(:,:)

  !---------SETTINGS FOR FUTURE NAMELIST---------!
  integer(i_kind) :: maxobrange=999000 ! Range (m) *within* which to use observations - obs *outside* this range are not used
  integer(i_kind) :: minobrange=-999 ! Range (m) *outside* of which to use observatons - obs *inside* this range are not used
  real(r_kind)    :: mintilt=0.0_r_kind          ! Only use tilt(elevation) angles (deg) >= this number 
  real(r_kind)    :: maxtilt=20.0_r_kind         ! Do no use tilt(elevation) angles (deg) >= this number
  logical         :: missing_to_nopcp=.false.    ! Set missing observations to 'no precipitation' observations -> dbznoise (See Aksoy et al. 2009, MWR) 
  real(r_kind)    :: dbznoise=2.0_r_kind           ! dBZ obs must be >= dbznoise for assimilation
  logical         :: l_limmax=.true.             ! If true, observations > 60 dBZ are limited to be 60 dBZ.  This is
  logical         :: l_limmin=.true.             ! If true, observations <0 dBZ are limited to be 0 dBZ.  This is

 character (len=4) :: radarsite_nc
 character (len=256) vcpstr_nc


!following the treatment on the precision issue for netcdf like in 
!wrf_netcdf_interface.F90
integer(i_kind) :: ncid,ierr,dimid1,dimid2
integer(i_kind) :: varid1,varid2,varid3,varid4,varid6
integer(i_kind) :: numazim_nc,numgate_nc,vcp_nc
real(r_single) :: elev_nc,firstgate_nc,lat_nc,lon_nc,height_nc


real(r_single), allocatable :: azimuth_nc(:),beamwidth_nc(:),azimspacing_nc(:),gatewidth_nc(:)
real(r_single), allocatable :: obdata_nc(:,:)
!clg
  !                                              !  due to representativeness error associated with the model
  !----------------------------------------------!

!--------------------------------------------------------------------------------------!
!                            END OF ALL DECLARATIONS                                   !
!--------------------------------------------------------------------------------------!
   
  !-Check if reflectivity is in the convinfo file and extract necessary attributes 
  scount=0
  ikx=0
  do i=1,nconvtype
     if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== 1) then
        ikx=i 
        radartwindow=ctwind(ikx)*r60         !Time window units converted to minutes 
                                             !  (default setting for dbz within convinfo is 0.05 hours)
        dbzerr=5_r_kind                      !Ob error (dB) to use for radar reflectivity factor
        exit                                 !Exit loop when finished with initial convinfo fields     
     else if ( i==nconvtype ) then
        write(6,*) 'READ_dBZ: ERROR - OBSERVATION TYPE IS NOT PRESENT IN CONVINFO OR USE FLAG IS ZERO'
        write(6,*) 'READ_dBZ: ABORTTING read_dbz.f90 - NO REFLECTIVITY OBS READ!'
        return   
     endif
  end do     
    
  if (minobrange >= maxobrange) then
     write(6,*) 'MININMUM OB RANGE >= MAXIMUM OB RANGE FOR READING dBZ - PROGRAM STOPPING FROM READ_DBZ.F90'
     call stop2(400)
  end if
        

  !-next three values are dummy values for now
  nchanl=0
  ilon=2
  ilat=3
  
  maxobs=2000000    !value taken from read_radar.f90 

  !--Allocate cdata_all array

  allocate(cdata_all(maxdat,maxobs))
       
  lunrad=31
! get the time from the file name

!read the QC radar data in titls in netcdf format from K. Cooper.
   nvol=1
   nelv=1
 v=1;k=1
   
  allocate(strct_in_dbz(nvol,nelv))

!!READ RADAR DATA
ierr =  NF90_OPEN(trim(infile),0,ncid)

if (ierr /= nf90_noerr) call handle_err(ierr,"open")

ierr = NF90_INQ_DIMID(ncid,'Azimuth',dimid1)
if (ierr /= nf90_noerr) call handle_err(ierr,"Azimuth")
ierr = NF90_INQ_DIMID(ncid,'Gate',dimid2)
if (ierr /= nf90_noerr) call handle_err(ierr,"Gate")



ierr = NF90_INQ_VARID(ncid,'Azimuth',varid1)
if (ierr /= nf90_noerr) call handle_err(ierr,"Azimuth")
ierr = NF90_INQ_VARID(ncid,'BeamWidth',varid2)
if (ierr /= nf90_noerr) call handle_err(ierr,"BeamWidth")
ierr = NF90_INQ_VARID(ncid,'AzimuthalSpacing',varid3)
if (ierr /= nf90_noerr) call handle_err(ierr,"azimuthalspacing")
ierr = NF90_INQ_VARID(ncid,'GateWidth',varid4)
if (ierr /= nf90_noerr) call handle_err(ierr,"gatewidth")
ierr = NF90_INQ_VARID(ncid,'ReflectivityQC',varid6)
if (ierr /= nf90_noerr) call handle_err(ierr,"ReflectivityQC")


ierr = nf90_inquire_dimension(ncid, dimid1, len = numazim_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"numazim data")
ierr = nf90_inquire_dimension(ncid, dimid2, len = numgate_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"numgate data")


ierr = NF90_GET_ATT(ncid,nf90_global,'Elevation',elev_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"get elev")
ierr = NF90_GET_ATT(ncid,nf90_global,'RangeToFirstGate',firstgate_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"get firstgate")
ierr = NF90_GET_ATT(ncid,nf90_global,'Latitude',lat_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"get lat")
ierr = NF90_GET_ATT(ncid,nf90_global,'Longitude',lon_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"get lon")
ierr = NF90_GET_ATT(ncid,nf90_global,'radarName-value',radarsite_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"radarsite")
ierr = NF90_GET_ATT(ncid,nf90_global,'vcp-value',vcpstr_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"vcp")
read(vcpstr_nc,*) vcp_nc
ierr = NF90_GET_ATT(ncid,nf90_global,'Height',height_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"height")


!reverse order of dimensions as stated in ncdump:
allocate(azimuth_nc(numazim_nc),beamwidth_nc(numazim_nc),azimspacing_nc(numazim_nc),gatewidth_nc(numazim_nc))
allocate(obdata_nc(numgate_nc,numazim_nc))

ierr = NF90_GET_VAR(ncid,varid1,azimuth_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"azimuth data")
ierr = NF90_GET_VAR(ncid,varid2,beamwidth_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"beamwidth data")
ierr = NF90_GET_VAR(ncid,varid3,azimspacing_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"azimspacing data")
ierr = NF90_GET_VAR(ncid,varid4,gatewidth_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"gatewidth data")
ierr = NF90_GET_VAR(ncid,varid6,obdata_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"obdata data")

ierr = NF90_CLOSE(ncid)
if (ierr /= nf90_noerr) call handle_err(ierr,"close")

do i=1,numazim_nc
  if ( (beamwidth_nc(i) /= beamwidth_nc(1)) .or. (gatewidth_nc(i) /= gatewidth_nc(1)) )then
    print *, "stopping: non-uniform scan"
  endif
enddo
read(infile(21:24),'(I4.4)')iyear
read(infile(25:26),'(I2.2)')imon
read(infile(27:28),'(I2.2)')iday
read(infile(30:31),'(I2.2)')ihour
read(infile(32:33),'(I2.2)')imin
read(infile(34:35),'(I2.2)')isec
do i=1,numgate_nc
  do j=1,numazim_nc
    if(obdata_nc(i,j) <= -999_r_kind) obdata_nc(i,j)=-999_r_kind
  enddo
enddo


!transform the read-in ob to the intermidate  obs variables( radar obs  to be used in GSI

strct_in_dbz(v,k)%radid=radarsite_nc
strct_in_dbz(v,k)%vcpnum=vcp_nc
strct_in_dbz(v,k)%year=iyear  !  to be defind from infile name              
strct_in_dbz(v,k)%month=imon                
strct_in_dbz(v,k)%day=iday                  
strct_in_dbz(v,k)%hour=ihour                 
strct_in_dbz(v,k)%minute=imin               
strct_in_dbz(v,k)%second=isec
strct_in_dbz(v,k)%radlat=lat_nc
strct_in_dbz(v,k)%radlon=lon_nc   
strct_in_dbz(v,k)%radhgt=height_nc
strct_in_dbz(v,k)%fstgatdis =firstgate_nc      
strct_in_dbz(v,k)%gateWidth=gatewidth_nc(1) ! always the same ??)
strct_in_dbz(v,k)%elev_angle=elev_nc
strct_in_dbz(v,k)%num_beam=numazim_nc          
strct_in_dbz(v,k)%num_gate=numgate_nc
na=strct_in_dbz(v,k)%num_beam
nb=strct_in_dbz(v,k)%num_gate
     
!******allocate arrays within radar data type**********!
allocate(strct_in_dbz(v,k)%azim(na))
allocate(strct_in_dbz(v,k)%field(nb,na))
!******************************************************!
          
strct_in_dbz(v,k)%azim(:)=azimuth_nc(:)
strct_in_dbz(v,k)%field(:,:)=obdata_nc(:,:)
 ierror=0
 fileopen: if (ierror == 0) then           !Check to make sure file is open - will also fail if file does not exist. Closing endif at end of subroutine.
     
     !*************************IMPORTANT***************************!
     !                                                             !
     !    All data = 999.0 correspond to missing or bad data       !       
     !                                                             !
     !*************************************************************!
     
         
 !------Begin processing--------------------------!  


 !-Obtain analysis time in minutes since reference date
  if(ndata/=0) then ! for further thinking
   write(6,*)'ndata is not 0 in read_dbz_netcdf, its impact needs to be considered ,stop' 
  endif
 
  call w3fs21(iadate,mins_an)  !mins_an -integer number of mins snce 01/01/1978
! w3movedat to help get a date from the time difference
  rmins_an=mins_an             !convert to real number
  
  volumes: do v=1,nvol 
   
    tilts: do k=1,nelv

     !--Check if observation fits within specified time window--!
      !-Find reference time of observation
     
        obdate(1)=strct_in_dbz(v,k)%year
        obdate(2)=strct_in_dbz(v,k)%month  
        obdate(3)=strct_in_dbz(v,k)%day 
        obdate(4)=strct_in_dbz(v,k)%hour   
        obdate(5)=strct_in_dbz(v,k)%minute 
        call w3fs21(obdate,mins_ob)                             !mins_ob -integer number of mins snce 01/01/1978
        rmins_ob=mins_ob                                        !convert to real number
        rmins_ob=rmins_ob+(strct_in_dbz(v,k)%second*r60inv)     !convert seconds to minutes and add to ob time
 
      !-Comparison is done in units of minutes
      
        timeb = rmins_ob-rmins_an
        if(abs(timeb) > 100_r_kind) cycle 
        
        write(6,*) 'Processing obdate:',obdate,strct_in_dbz(v,k)%second                 
      !--Time window check complete--!
      
        thistilt=strct_in_dbz(v,k)%elev_angle
        if (thistilt <= maxtilt .and. thistilt >= mintilt) then 
     
          gates: do i=1,strct_in_dbz(v,k)%num_gate
             
             thisrange=strct_in_dbz(v,k)%fstgatdis + real(i-1,r_kind)*strct_in_dbz(v,k)%gateWidth
       
             !-Check to make sure observations are within specified range 

              if (thisrange <= maxobrange .and. thisrange >= minobrange) then    
                azms: do j=1,strct_in_dbz(v,k)%num_beam
   
                    !-Check to see if this is a missing observation 
    
                    nread=nread+1
 
                    if ( abs(strct_in_dbz(v,k)%field(i,j)) >= 99.0_r_kind ) then
                     
                      !--Extend no precip observations to missing data fields?
                      !  May help suppress spurious convection if a problem.
     
                       if (missing_to_nopcp) then
                          strct_in_dbz(v,k)%field(i,j) = dbznoise
                          num_m2nopcp = num_m2nopcp+1
                       else  
                          num_missing=num_missing+1
                          cycle azms                        !No reason to process the ob if it is missing 
                       end if
                       
                    end if
 
    
                    if (l_limmax) then
                       if ( strct_in_dbz(v,k)%field(i,j) > 60_r_kind ) then
                          strct_in_dbz(v,k)%field(i,j) = 60_r_kind
                          num_limmax=num_limmax+1
                       end if    
                    end if
                    if (l_limmin) then
                       if ( strct_in_dbz(v,k)%field(i,j) < 0_r_kind ) then
                          strct_in_dbz(v,k)%field(i,j) = 0_r_kind
                          num_limmin=num_limmin+1
                       end if    
                    end if
       
                    !--Find observation height using method from read_l2bufr_mod.f90										       
         
                    this_stahgt=strct_in_dbz(v,k)%radhgt
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

                    !--Find observation location using method from read_l2bufr_mod.f90
 
                    !-Get corrected tilt angle
                    celev=celev0
                    selev=selev0
                    celev=a43*celev0/(a43+h)
                    selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
          
                    gamma=half*thisrange*(celev0+celev)
         
                   !-Get earth lat lon of observation
          
                    rlon0=deg2rad*strct_in_dbz(v,k)%radlon
                    clat0=cos(deg2rad*strct_in_dbz(v,k)%radlat)
                    slat0=sin(deg2rad*strct_in_dbz(v,k)%radlat)  
                    thisazimuthr=(90.0_r_kind-strct_in_dbz(v,k)%azim(j))*deg2rad   !Storing as 90-azm to
                                                                                   ! be consistent with 
                                                                                   ! read_l2bufr_mod.f90
                    rad_per_meter=one/rearth
                    rlonloc=rad_per_meter*gamma*cos(thisazimuthr)
                    rlatloc=rad_per_meter*gamma*sin(thisazimuthr)
                  
                    call invtllv(rlonloc,rlatloc,rlon0,clat0,slat0,rlonglob,rlatglob)
                 
                    thislat=rlatglob*rad2deg
                    thislon=rlonglob*rad2deg 
  
                   !-Check format of longitude and correct if necessary
                 
                    if(thislon>=r360) thislon=thislon-r360
                    if(thislon<zero ) thislon=thislon+r360
                    if(thislon>=r360 .or. thislat >90.0_r_kind) cycle
 
                   !-Convert back to radians                 
                      
                    thislat = thislat*deg2rad
                    thislon = thislon*deg2rad
                  
                   !find grid relative lat lon locations of earth lat lon
                 
                    call tll2xy(thislon,thislat,dlon,dlat,outside)
                    if (outside) cycle azms             !If observation is outside the domain
                                                        ! then cycle, but don't increase range right away.
                                                        ! Domain could be rectangular, so ob may be out of
                                                        ! range at one end, but not the other.		     					                   		   		   
                    thiserr=dbzerr
                
         
                    ndata  = min(ndata+1,maxobs)     
                    nodata = min(nodata+1,maxobs)  !number of obs not used (no meaning here)
    
                    this_staid=strct_in_dbz(v,k)%radid      !Via equivalence in declaration, value is propagated
                                                            !  to rstation_id used below.
   
                    cdata_all(1,ndata) = thiserr                      ! reflectivity obs error (dB) - inflated/adjusted
                    cdata_all(2,ndata) = dlon                         ! grid relative longitude
                    cdata_all(3,ndata) = dlat                         ! grid relative latitude
                    cdata_all(4,ndata) = thishgt                      ! obs absolute height (m)
                    cdata_all(5,ndata) = strct_in_dbz(v,k)%field(i,j) ! radar reflectivity factor 
                    cdata_all(6,ndata) = thisazimuthr                 ! 90deg-azimuth angle (radians)
                    cdata_all(7,ndata) = timeb*r60inv                 ! obs time (analyis relative hour)
                    cdata_all(8,ndata) = ikx                          ! type double check with the convinfo txt	   
                    cdata_all(9,ndata) = thistiltr                    ! tilt angle (radians)
                    cdata_all(10,ndata)= this_stahgt                  ! station elevation (m)
                    cdata_all(11,ndata)= rstation_id                  ! station id
                    cdata_all(12,ndata)= icuse(ikx)                   ! usage parameter
                    cdata_all(13,ndata)= thislon*rad2deg              ! earth relative longitude (degrees)
                    cdata_all(14,ndata)= thislat*rad2deg              ! earth relative latitude (degrees)
                    cdata_all(15,ndata)= thisrange                    ! range from radar in m 
                    cdata_all(16,ndata)= dbzerr                       ! orginal error from convinfo file
                    cdata_all(17,ndata)= dbznoise                     ! noise threshold for reflectivity (dBZ)

                  end do azms  !j
              else
                  num_badrange=num_badrange+1      !If outside acceptable range, increment
              end if   !Range check	

           end do gates    !i
     
        else
           num_badtilt=num_badtilt+1           !If outside acceptable tilts, increment
        end if         !Tilt check
  
     end do tilts       !k
  end do volumes      !v 
      

!---all looping done now print diagnostic output

  write(6,*)'READ_dBZ: Reached eof on radar reflectivity file'
  write(6,*)'READ_dBZ: # volumes in input file             =',nvol
  write(6,*)'READ_dBZ: # elevations per volume             =',nelv
  write(6,*)'READ_dBZ: # elevations outside time window    =',numbadtime
  write(6,*)'READ_dBZ: # of noise obs to no precip obs     =',num_nopcp
  write(6,*)'READ_dBZ: # of missing data to no precip obs  =',num_m2nopcp
  write(6,*)'READ_dBZ: # of rejected noise obs             =',num_noise
  write(6,*)'READ_dBZ: # of missing data                   =',num_missing
  write(6,*)'READ_dBZ: # outside specif. range             =',num_badrange
  write(6,*)'READ_dBZ: # outside specif. tilts             =',num_badtilt
  write(6,*)'READ_dBZ: # restricted to 60dBZ limit         =',num_limmax
  write(6,*)'READ_dBZ: # restricted to 0dBZ limit         =',num_limmin
  write(6,*)'READ_dBZ: # ndata         =',ndata

!---Write observation to scratch file---!
  
  call count_obs(ndata,maxdat,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,maxdat,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)
 
  
  !---------------DEALLOCATE ARRAYS-------------!
 

 else  !fileopen
  write(6,*) 'READ_dBZ: ERROR OPENING RADAR REFLECTIVITY FILE: ',trim(infile),' IOSTAT ERROR: ',ierror, ' SKIPPING...'
 end if fileopen
 deallocate(cdata_all)
 do v=1,nvol
    do k=1,nelv
       deallocate(strct_in_dbz(v,k)%azim)
       deallocate(strct_in_dbz(v,k)%field)
    end do
 end do
 deallocate(strct_in_dbz)
 deallocate(obdata_nc,azimuth_nc)
 deallocate(beamwidth_nc,azimspacing_nc,gatewidth_nc)

end subroutine read_dbz_mrms_netcdf


subroutine read_dbz_mrms_sparse_netcdf(nread,ndata,nodata,infile,obstype,lunout,sis,nobs)
!$$$   subprogram documentation block
!                .      .    .                                       .
!   subprogram: read_dbz        read level2 raw QC'd radar reflectivity files
!   
!   prgmmr: carley          org: np22                date: 2011-04-04
!
! abstract: Reads and processes level 2 horizontal radar reflectivity (dBZ) by 
!                radar site.  Data are on radar scan surafces. Also reads, but does
!                not process unfolded radial velocities.  Processing includes
!                finding the lat/lon and height of each observation. 
!                This formulation is not outfitted for 4dvar, but will
!                work with 3dvar and hybrid ensemble.
!
! program history log:
!   2011-08-12  carley - Fix dBZ oberror to be 3dBZ and add optional
!                        upper bound limit to observed dBZ to account
!                        for representativeness error.
!   2011-12-08  carley - Fix dBZ oberror to 5 dBZ 
!   2015        Lei    - modified from read_dbz to read_dbz_mrms_spase_netcdf
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
  use netcdf
  use kinds, only: r_kind,r_double,i_kind,i_short,r_single
  use constants, only: zero,half,one,two,deg2rad,rearth,rad2deg, &
                       one_tenth,r1000,r60,r60inv,r100,r400
  use gridmod, only: tll2xy
  use obsmod, only: iadate
  use convinfo, only: nconvtype,ctwind,icuse,ioctype
  use mpimod, only: npe
  use read_l2bufr_mod, only : invtllv
       
  implicit none
  
! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=*),intent(in   ) :: sis
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe),intent(inout) :: nobs

! Declare local parameters
  real(r_kind),parameter :: four_thirds = 4.0_r_kind / 3.0_r_kind
  real(r_kind),parameter :: r8     = 8.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  integer(i_kind),parameter:: maxdat=17         ! Used in generating cdata array
  integer (i_kind):: iyear,imon,iday,ihour,imin,isec
  
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
 integer(i_kind) :: num_missing=0,num_nopcp=0, &      !counts 
                    numbadtime=0,num_badtilt=0, &    
                    num_badrange=0,num_m2nopcp=0, &
                    num_noise=0,num_limmax=0 ,num_limmin=0   
    
                                                          

!--General declarations
  integer(i_kind) :: ierror,lunrad,i,j,k,v,na,nb,nelv,nvol, &
                     ikx,mins_an,mins_ob
  integer(i_kind) :: maxobs,nchanl,ilat,ilon,scount
  
  integer(i_kind),dimension(5) :: obdate
  
  real(r_kind) :: b,c,ha,epsh,h,aactual,a43,thistilt                             
  real(r_kind) :: thistiltr,selev0,celev0,thisrange,this_stahgt,thishgt                           
  real(r_kind) :: celev,selev,gamma,thisazimuthr,rlon0, &
                  clat0,slat0,dlat,dlon,thiserr,thislon,thislat, &
                  rlonloc,rlatloc,rlonglob,rlatglob,timeb,rad_per_meter  
  real(r_kind) :: radartwindow
  real(r_kind) :: dbzerr,rmins_an,rmins_ob                                                     
  real(r_kind),allocatable,dimension(:,:):: cdata_all
  real(r_double) rstation_id
  
  character(8) cstaid
  character(4) this_staid
  equivalence (this_staid,cstaid)
  equivalence (cstaid,rstation_id)

  logical      :: outside
    
  type(radar),allocatable :: strct_in_dbz(:,:)

  !---------SETTINGS FOR FUTURE NAMELIST---------!
  integer(i_kind) :: maxobrange=99900000 ! Range (m) *within* which to use observations - obs *outside* this range are not used
  integer(i_kind) :: minobrange=-999 ! Range (m) *outside* of which to use observatons - obs *inside* this range are not used
  real(r_kind)    :: mintilt=0.0_r_kind          ! Only use tilt(elevation) angles (deg) >= this number 
  real(r_kind)    :: maxtilt=20.0_r_kind         ! Do no use tilt(elevation) angles (deg) >= this number
  logical         :: missing_to_nopcp=.false.    ! Set missing observations to 'no precipitation' observations -> dbznoise (See Aksoy et al. 2009, MWR) 
  real(r_kind)    :: dbznoise=2_r_kind           ! dBZ obs must be >= dbznoise for assimilation
  logical         :: l_limmax=.true.             ! If true, observations > 60 dBZ are limited to be 60 dBZ.  This is
  logical         :: l_limmin=.true.             ! If true, observations <0  dBZ are limited to be 0 dBZ.  This is

 character (len=4) :: radarsite_nc
 character (len=256) vcpstr_nc


!following the treatment on the precision issue for netcdf like in 
!wrf_netcdf_interface.F90
integer(i_kind) :: ncid,ierr,dimid1,dimid2,dimid3
integer(i_kind) :: varid1,varid2,varid3,varid4,varid6
integer(i_kind) :: pixel_x_varid,pixel_y_varid 
integer(i_kind) :: numazim_nc,numgate_nc,num_pixel_nc,real_num_pixel,vcp_nc
real(r_single) :: elev_nc,firstgate_nc,lat_nc,lon_nc,height_nc
integer(i_short),allocatable :: pixel_x_nc(:),pixel_y_nc(:)


real(r_single), allocatable :: azimuth_nc(:),beamwidth_nc(:),azimspacing_nc(:),gatewidth_nc(:)
real(r_single), allocatable :: obdata_pixel_nc(:)
logical l_pixel_unlimited
integer(i_kind):: ipix
integer(i_kind)::real_numpixel,start_nc(1),count_nc(1)
   
  !-Check if reflectivity is in the convinfo file and extract necessary attributes 
  scount=0
  ikx=0
  do i=1,nconvtype
     if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== 1) then
        ikx=i 
        radartwindow=ctwind(ikx)*r60         !Time window units converted to minutes 
                                             !  (default setting for dbz within convinfo is 0.05 hours)
        dbzerr=5_r_kind                      !Ob error (dB) to use for radar reflectivity factor
        exit                                 !Exit loop when finished with initial convinfo fields     
     else if ( i==nconvtype ) then
        write(6,*) 'READ_dBZ: ERROR - OBSERVATION TYPE IS NOT PRESENT IN CONVINFO OR USE FLAG IS ZERO'
        write(6,*) 'READ_dBZ: ABORTTING read_dbz.f90 - NO REFLECTIVITY OBS READ!'
        return   
     endif
  end do     
    
  if (minobrange >= maxobrange) then
  write(6,*) 'MININMUM OB RANGE >= MAXIMUM OB RANGE FOR READING dBZ - PROGRAM STOPPING FROM READ_DBZ.F90'
  call stop2(400)
  end if
        

  !-next three values are dummy values for now
  nchanl=0
  ilon=2
  ilat=3
  
  maxobs=2000000    !value taken from read_radar.f90 

  !--Allocate cdata_all array

  allocate(cdata_all(maxdat,maxobs))
       
  lunrad=31

   nvol=1
   nelv=1
   v=1;k=1
   
  allocate(strct_in_dbz(nvol,nelv))

!!READ RADAR DATA
ierr =  NF90_OPEN(trim(infile),0,ncid)

if (ierr /= nf90_noerr) call handle_err(ierr,"open")

ierr = NF90_INQ_DIMID(ncid,'Azimuth',dimid1)
if (ierr /= nf90_noerr) call handle_err(ierr,"Azimuth")
ierr = NF90_INQ_DIMID(ncid,'Gate',dimid2)
if (ierr /= nf90_noerr) call handle_err(ierr,"Gate")
ierr = NF90_INQ_DIMID(ncid,'pixel',dimid3)
if (ierr /= nf90_noerr) call handle_err(ierr,"Pixel number")



ierr = NF90_INQ_VARID(ncid,'Azimuth',varid1)
if (ierr /= nf90_noerr) call handle_err(ierr,"Azimuth")
ierr = NF90_INQ_VARID(ncid,'BeamWidth',varid2)
if (ierr /= nf90_noerr) call handle_err(ierr,"BeamWidth")
ierr = NF90_INQ_VARID(ncid,'AzimuthalSpacing',varid3)
if (ierr /= nf90_noerr) call handle_err(ierr,"azimuthalspacing")
ierr = NF90_INQ_VARID(ncid,'GateWidth',varid4)
if (ierr /= nf90_noerr) call handle_err(ierr,"gatewidth")
ierr = NF90_INQ_VARID(ncid,'pixel_x',pixel_x_varid)
ierr = NF90_INQ_VARID(ncid,'pixel_y',pixel_y_varid)
ierr = NF90_INQ_VARID(ncid,'ReflectivityQC',varid6)
if (ierr /= nf90_noerr) call handle_err(ierr,"ReflectivityQC")


ierr = nf90_inquire_dimension(ncid, dimid1, len = numazim_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"numazim data")
ierr = nf90_inquire_dimension(ncid, dimid2, len = numgate_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"numgate data")
ierr = nf90_inquire_dimension(ncid, dimid3, len = num_pixel_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"num_pixel_ncdata")
if(num_pixel_nc<=0 ) then !unlimited size
  num_pixel_nc=numazim_nc*numgate_nc
  l_pixel_unlimited=.true.
else
  real_num_pixel=num_pixel_nc
  l_pixel_unlimited=.false.
endif


ierr = NF90_GET_ATT(ncid,nf90_global,'Elevation',elev_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"get elev")
ierr = NF90_GET_ATT(ncid,nf90_global,'RangeToFirstGate',firstgate_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"get firstgate")
ierr = NF90_GET_ATT(ncid,nf90_global,'Latitude',lat_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"get lat")
ierr = NF90_GET_ATT(ncid,nf90_global,'Longitude',lon_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"get lon")
ierr = NF90_GET_ATT(ncid,nf90_global,'radarName-value',radarsite_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"radarsite")
ierr = NF90_GET_ATT(ncid,nf90_global,'vcp-value',vcpstr_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"vcp")
read(vcpstr_nc,*) vcp_nc
ierr = NF90_GET_ATT(ncid,nf90_global,'Height',height_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"height")


!reverse order of dimensions as stated in ncdump:
allocate(azimuth_nc(numazim_nc),beamwidth_nc(numazim_nc),azimspacing_nc(numazim_nc),gatewidth_nc(numazim_nc))
allocate(obdata_pixel_nc(num_pixel_nc))
allocate(pixel_x_nc(num_pixel_nc))
allocate(pixel_y_nc(num_pixel_nc))

ierr = NF90_GET_VAR(ncid,varid1,azimuth_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"azimuth data")
ierr = NF90_GET_VAR(ncid,varid2,beamwidth_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"beamwidth data")
ierr = NF90_GET_VAR(ncid,varid3,azimspacing_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"azimspacing data")
ierr = NF90_GET_VAR(ncid,varid4,gatewidth_nc)
if (ierr /= nf90_noerr) call handle_err(ierr,"gatewidth data")
if(.not.l_pixel_unlimited) then
  ierr = NF90_GET_VAR(ncid,varid6,obdata_pixel_nc)
  real_numpixel=num_pixel_nc
if (ierr /= nf90_noerr) call handle_err(ierr,"obdata_pixel data")
  ierr = NF90_GET_VAR(ncid,pixel_x_varid,pixel_x_nc)
  ierr = NF90_GET_VAR(ncid,pixel_y_varid,pixel_y_nc)
else
  ierr=nf90_noerr
  ipix=1
  start_nc=(/1/)
  count_nc=(/1/)
  ipix=1
  do 255, while (ierr == nf90_noerr)
    start_nc(1)=ipix
    ierr = NF90_GET_VAR(ncid,varid6,obdata_pixel_nc(ipix:ipix),start=start_nc,count=count_nc)
    ierr = NF90_GET_VAR(ncid,pixel_x_varid,pixel_x_nc(ipix:ipix),start=start_nc,count=count_nc)
    ierr = NF90_GET_VAR(ncid,pixel_y_varid,pixel_y_nc(ipix:ipix),start=start_nc,count=count_nc)
    ipix=ipix+1
255 continue
    real_numpixel=ipix-2

endif

ierr = NF90_CLOSE(ncid)
if (ierr /= nf90_noerr) call handle_err(ierr,"close")

do i=1,numazim_nc
  if ( (beamwidth_nc(i) /= beamwidth_nc(1)) .or. (gatewidth_nc(i) /= gatewidth_nc(1)) )then
     print *, "stopping: non-uniform scan"
  endif
enddo
read(infile(21:24),'(I4.4)')iyear
read(infile(25:26),'(I2.2)')imon
read(infile(27:28),'(I2.2)')iday
read(infile(30:31),'(I2.2)')ihour
read(infile(32:33),'(I2.2)')imin
read(infile(34:35),'(I2.2)')isec
do j=1,real_numpixel
  if(obdata_pixel_nc(j) < -999_r_kind) obdata_pixel_nc(j)=-999_r_kind
enddo


! transform the read-in ob to the intermidate  obs variables( radar obs  to be used in GSI

        strct_in_dbz(v,k)%radid=radarsite_nc
        strct_in_dbz(v,k)%vcpnum=vcp_nc
        strct_in_dbz(v,k)%year=iyear  !  to be defind from infile name              
        strct_in_dbz(v,k)%month=imon                
        strct_in_dbz(v,k)%day=iday                  
        strct_in_dbz(v,k)%hour=ihour                 
        strct_in_dbz(v,k)%minute=imin               
        strct_in_dbz(v,k)%second=isec
        strct_in_dbz(v,k)%radlat=lat_nc
        strct_in_dbz(v,k)%radlon=lon_nc   
        strct_in_dbz(v,k)%radhgt=height_nc
        strct_in_dbz(v,k)%fstgatdis =firstgate_nc      
        strct_in_dbz(v,k)%gateWidth=gatewidth_nc(1) ! always the same ??)
        strct_in_dbz(v,k)%elev_angle=elev_nc
        strct_in_dbz(v,k)%num_beam=numazim_nc          
        strct_in_dbz(v,k)%num_gate=numgate_nc
        na=strct_in_dbz(v,k)%num_beam
        nb=strct_in_dbz(v,k)%num_gate
     
        !******allocate arrays within radar data type**********!
           allocate(strct_in_dbz(v,k)%azim(na))
        !******************************************************!
          
         strct_in_dbz(v,k)%azim(:)=azimuth_nc(:)
  ierror=0
 fileopen: if (ierror == 0) then           !Check to make sure file is open - will also fail if file does not exist. Closing endif at end of subroutine.


 !-Obtain analysis time in minutes since reference date
 
  call w3fs21(iadate,mins_an)  !mins_an -integer number of mins snce 01/01/1978
! w3movedat to help get a date from the time difference
  rmins_an=mins_an             !convert to real number
  
  volumes: do v=1,nvol 
   
    tilts: do k=1,nelv

     !--Check if observation fits within specified time window--!
      !-Find reference time of observation
     
        obdate(1)=strct_in_dbz(v,k)%year
        obdate(2)=strct_in_dbz(v,k)%month  
        obdate(3)=strct_in_dbz(v,k)%day	 
        obdate(4)=strct_in_dbz(v,k)%hour   
        obdate(5)=strct_in_dbz(v,k)%minute 
        call w3fs21(obdate,mins_ob)                             !mins_ob -integer number of mins snce 01/01/1978
       
        rmins_ob=mins_ob                                        !convert to real number
        rmins_ob=rmins_ob+(strct_in_dbz(v,k)%second*r60inv)     !convert seconds to minutes and add to ob time
 
      !-Comparison is done in units of minutes
      
        timeb = rmins_ob-rmins_an
! now the window is controled by the preprocessing script starting from
!5/14/2015
!        if(abs(timeb) > abs(radartwindow)) then
!  numbadtime=numbadtime+1	  
!	  cycle tilts                           !If not in time window, cycle the loop
!	end if
        
        write(6,*) 'Processing obdate:',obdate,strct_in_dbz(v,k)%second                 
        if(abs(timeb) > 99999_r_kind) cycle
      !--Time window check complete--!
      
        thistilt=strct_in_dbz(v,k)%elev_angle
        if (thistilt <= maxtilt .and. thistilt >= mintilt) then 
     
          pixel: do ipix=1,real_numpixel
              j=pixel_x_nc(ipix)+1
              i=pixel_y_nc(ipix)+1
             
              thisrange=strct_in_dbz(v,k)%fstgatdis + real(i-1,r_kind)*strct_in_dbz(v,k)%gateWidth
       
             !-Check to make sure observations are within specified range 

              if (thisrange <= maxobrange .and. thisrange >= minobrange) then    
     
    
                    nread=nread+1
                    if ( abs(obdata_pixel_nc(ipix)) >= 999.0_r_kind ) then

                      !--Extend no precip observations to missing data fields?
                      !  May help suppress spurious convection if a problem.
     
                       if (missing_to_nopcp) then
                          obdata_pixel_nc(ipix) = dbznoise
                          num_m2nopcp = num_m2nopcp+1
                       else  
                          num_missing=num_missing+1
                          cycle pixel                       !No reason to process the ob if it is missing 
                       end if

                    end if


                    if (l_limmax) then
                       if ( obdata_pixel_nc(ipix) > 60_r_kind ) then
                          obdata_pixel_nc(ipix) = 60_r_kind
                          num_limmax=num_limmax+1
                       end if    
                    end if
                    if (l_limmin) then
                       if ( obdata_pixel_nc(ipix) < 0_r_kind ) then
                          obdata_pixel_nc(ipix) = 0_r_kind
                          num_limmin=num_limmin+1
                       end if    
                    end if

                   !-Special treatment for no-precip obs?	       			       


                   !--Find observation height using method from read_l2bufr_mod.f90										       

                    this_stahgt=strct_in_dbz(v,k)%radhgt
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

                   !--Find observation location using method from read_l2bufr_mod.f90
 
                   !-Get corrected tilt angle
                    celev=celev0
                    selev=selev0
                    celev=a43*celev0/(a43+h)
                    selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
          
                    gamma=half*thisrange*(celev0+celev)
         
                   !-Get earth lat lon of observation
          
                    rlon0=deg2rad*strct_in_dbz(v,k)%radlon
                    clat0=cos(deg2rad*strct_in_dbz(v,k)%radlat)
                    slat0=sin(deg2rad*strct_in_dbz(v,k)%radlat)  
                    thisazimuthr=(90.0_r_kind-strct_in_dbz(v,k)%azim(j))*deg2rad   !Storing as 90-azm to
                                                                                   ! be consistent with 
                                                                                   ! read_l2bufr_mod.f90
                    rad_per_meter=one/rearth
                    rlonloc=rad_per_meter*gamma*cos(thisazimuthr)
                    rlatloc=rad_per_meter*gamma*sin(thisazimuthr)
                  
                    call invtllv(rlonloc,rlatloc,rlon0,clat0,slat0,rlonglob,rlatglob)
                 
                    thislat=rlatglob*rad2deg
                    thislon=rlonglob*rad2deg 
  
                   !-Check format of longitude and correct if necessary
                 
                    if(thislon>=r360) thislon=thislon-r360
                    if(thislon<zero ) thislon=thislon+r360
                 
                   !-Convert back to radians                 
                      
                    thislat = thislat*deg2rad
                    thislon = thislon*deg2rad
                 
                   !find grid relative lat lon locations of earth lat lon
                 
                    call tll2xy(thislon,thislat,dlon,dlat,outside)
                    if (outside) cycle pixel             !If observation is outside the domain
                    thiserr=dbzerr
                




                   !-Load good data into output array
         
                    ndata  = min(ndata+1,maxobs)     
                    nodata = min(nodata+1,maxobs)  !number of obs not used (no meaning here)
    
                    this_staid=strct_in_dbz(v,k)%radid      !Via equivalence in declaration, value is propagated
                                                            !  to rstation_id used below.

                    cdata_all(1,ndata) = thiserr                      ! reflectivity obs error (dB) - inflated/adjusted
                    cdata_all(2,ndata) = dlon                         ! grid relative longitude
                    cdata_all(3,ndata) = dlat                         ! grid relative latitude
                    cdata_all(4,ndata) = thishgt                      ! obs absolute height (m)
                    cdata_all(5,ndata) = obdata_pixel_nc(ipix)  !strct_in_dbz(v,k)%field(i,j) ! radar reflectivity factor 
                    cdata_all(6,ndata) = thisazimuthr                 ! 90deg-azimuth angle (radians)
                    cdata_all(7,ndata) = timeb*r60inv                 ! obs time (analyis relative hour)
                    cdata_all(8,ndata) = ikx                          ! type		   
                    cdata_all(9,ndata) = thistiltr                    ! tilt angle (radians)
                    cdata_all(10,ndata)= this_stahgt                  ! station elevation (m)
                    cdata_all(11,ndata)= rstation_id                  ! station id
                    cdata_all(12,ndata)= icuse(ikx)                   ! usage parameter
                    cdata_all(13,ndata)= thislon*rad2deg              ! earth relative longitude (degrees)
                    cdata_all(14,ndata)= thislat*rad2deg              ! earth relative latitude (degrees)
                    cdata_all(15,ndata)= thisrange                    ! range from radar in m 
                    cdata_all(16,ndata)= dbzerr                       ! orginal error from convinfo file
                    cdata_all(17,ndata)= dbznoise                     ! noise threshold for reflectivity (dBZ)

              else
                   num_badrange=num_badrange+1      !If outside acceptable range, increment
              end if   !Range check	

     
              end do pixel    !i
        else
           num_badtilt=num_badtilt+1           !If outside acceptable tilts, increment
        end if         !Tilt check
  
     end do tilts       !k
  end do volumes      !v 
      

!---all looping done now print diagnostic output

  write(6,*)'READ_dBZ: Reached eof on radar reflectivity file'
  write(6,*)'READ_dBZ: # volumes in input file             =',nvol
  write(6,*)'READ_dBZ: # elevations per volume             =',nelv
  write(6,*)'READ_dBZ: # elevations outside time window    =',numbadtime
  write(6,*)'READ_dBZ: # of noise obs to no precip obs     =',num_nopcp
  write(6,*)'READ_dBZ: # of missing data to no precip obs  =',num_m2nopcp
  write(6,*)'READ_dBZ: # of rejected noise obs             =',num_noise
  write(6,*)'READ_dBZ: # of missing data                   =',num_missing
  write(6,*)'READ_dBZ: # outside specif. range             =',num_badrange
  write(6,*)'READ_dBZ: # outside specif. tilts             =',num_badtilt
  write(6,*)'READ_dBZ: # restricted to 60dBZ limit         =',num_limmax
  write(6,*)'READ_dBZ: # restricted to 0dBZ limit         =',num_limmin
  write(6,*)'READ_dBZ: # ndata         =',ndata

!---Write observation to scratch file---!
  call count_obs(ndata,maxdat,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,maxdat,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)
 
  
  !---------------DEALLOCATE ARRAYS-------------!
 
  deallocate(cdata_all)
  do v=1,nvol
     do k=1,nelv
        deallocate(strct_in_dbz(v,k)%azim)
     end do
  end do
  deallocate(strct_in_dbz)
  deallocate(azimuth_nc,beamwidth_nc,azimspacing_nc,gatewidth_nc)
  deallocate(pixel_x_nc)
  deallocate(pixel_y_nc)

 else  !fileopen
  write(6,*) 'READ_dBZ: ERROR OPENING RADAR REFLECTIVITY FILE: ',trim(infile),' IOSTAT ERROR: ',ierror, ' SKIPPING...'
 end if fileopen

end subroutine read_dbz_mrms_sparse_netcdf

subroutine read_dbz_mrms_detect_format(infile,l_sparse_netcdf)
! to detect if it is sparse or not netcdf format by MRMS
!$$$   subprogram documentation block
!                .      .    .                                       .
!   
!$$$ end documentation block
  use netcdf
  use kinds, only: r_kind,r_double,i_kind,i_short,r_single
       
  implicit none
  
! Declare passed variables
  character(len=*),intent(in   ) :: infile
! Declare local parameters
  real(r_kind),parameter :: four_thirds = 4.0_r_kind / 3.0_r_kind
  real(r_kind),parameter :: r8     = 8.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  integer(i_kind),parameter:: maxdat=17         ! Used in generating cdata array
  logical l_sparse_netcdf
  
!--General declarations

integer(i_kind) :: ncid,ierr,dimid3

!--------------------------------------------------------------------------------------!
!                            END OF ALL DECLARATIONS                                   !
!--------------------------------------------------------------------------------------!
   
!!READ RADAR DATA
ierr =  NF90_OPEN(trim(infile),0,ncid)

if (ierr /= nf90_noerr) call handle_err(ierr,"open")

ierr = NF90_INQ_DIMID(ncid,'pixel',dimid3)
if (ierr /= nf90_noerr) then 
  l_sparse_netcdf=.false.
else
  l_sparse_netcdf=.true.
endif
ierr = NF90_CLOSE(ncid)
end subroutine read_dbz_mrms_detect_format



subroutine handle_err(ierr,istring)
use netcdf
use kinds, only: i_kind
implicit none
integer(i_kind) :: ierr
character (len=*) :: istring

print *, ierr,trim(istring)
print *, trim(nf90_strerror(ierr))
stop

end subroutine handle_err

