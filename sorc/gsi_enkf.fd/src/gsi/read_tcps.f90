subroutine read_tcps(nread,ndata,nodata,infile,obstype,lunout,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_tcps                   read tcvitals ascii file
!   prgmmr: kleist             org: np20                date: 2009-02-02
!
! abstract:  This routine reads the ascii TC psmin data file 
!
!
! program history log:
!   2009-02-02  kleist
!   2010-09-08  treadon - add station_id and destroy_tcv_card; increase
!                         maxdat to 10; remove i_kind suffix from integer 
!                         constants; 
!   2013-01-26  parrish - change from grdcrd to grdcrd1
!   2015-10-01  guo     - consolidate use of ob location (in deg)
!
!   input argument list:
!     infile   - unit from which to read ascii file
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!
!   output argument list:
!     nread    - number of bogus data read
!     ndata    - number of bogus data retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_double
  use gridmod, only: nlat,nlon,rlats,rlons,regional,tll2xy
  use constants, only: deg2rad,zero,one_tenth,one
  use convinfo, only: nconvtype,ictype,icuse
  use obsmod, only: ianldate
  use tcv_mod, only: get_storminfo,numstorms,stormlat,stormlon,stormpsmin,stormdattim,&
       centerid,stormid,destroy_tcv_card,tcp_refps,tcp_width,tcp_ermin,tcp_ermax
  use gsi_4dvar, only: time_4dvar
  use mpimod, only:npe
  implicit none

! Declare passed variables
  character(len=*),intent(in  ) :: obstype,infile
  character(20)  ,intent(in   ) :: sis
  integer(i_kind),intent(in   ) :: lunout
  integer(i_kind),intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe),intent(inout) :: nobs

! Declare local parameters
  real(r_kind),parameter:: r360=360.0_r_kind
  integer(i_kind),parameter:: maxobs=2e6
  integer(i_kind),parameter:: maxdat=10

! Declare local variables
  real(r_double) rstation_id
  character(8) station_id
  equivalence(rstation_id,station_id)

  real(r_kind) dlat,dlon,dlat_earth,dlon_earth
  real(r_kind) dlat_earth_deg,dlon_earth_deg
  real(r_kind),allocatable,dimension(:,:):: cdata_all
  real(r_kind) ohr,olat,olon,psob,pob,oberr,usage,toff
  real(r_kind) psdif,alpha

  integer(i_kind) i,k,lunin,nc
  integer(i_kind) ilat,ilon,ikx,nreal,nchanl,noutside,nmrecs

  logical endfile, outside

  data lunin / 10 /

!**************************************************************************
! Initialize variables
  nmrecs=0
  nreal=maxdat
  nchanl=0
  noutside=0
  ilon=2
  ilat=3
  endfile=.false.

  allocate(cdata_all(maxdat,maxobs))

  open(lunin,file=trim(infile),form='formatted',action='read')
  call datelen(10)

! Big loop over binary file

  call get_storminfo(lunin)

  write(6,*) 'READ_TCPS:  IANLDATE = ',ianldate

  do i=1,numstorms
     nmrecs=nmrecs+1
     nread=nread+1

! Set ikx here...pseudo-mslp tc_vitals obs are assumed to be type 111
     do nc=1,nconvtype
        if (ictype(nc)==112) ikx=nc
     end do

! Set usage variable
     usage = zero
     if(icuse(ikx) <= 0)usage=100._r_kind

     if (stormdattim(i)/=ianldate) then
        write(6,*) 'READ_TCPS:  IGNORE TC_VITALS ENTRY # ',i
        write(6,*) 'READ_TCPS:  MISMATCHED FROM ANALYSIS TIME, OBS / ANL DATES = ',stormdattim(i),ianldate
        cycle
     end if

! Set center and storm id (only used in diagnostic file)
  station_id = centerid(i)(1:4) // '_' // stormid(i)(1:3)

! Observation occurs at analysis time as per date check above
! Set observation lat, lon, mslp, and default obs-error
     call time_4dvar(ianldate,toff)
!    write(6,*)'READ_TCPS: bufr file date is ',ianldate
!    write(6,*)'READ_TCPS: time offset is ',toff,' hours.'
     ohr=toff
     olat=stormlat(i)
     olon=stormlon(i)
     psob=stormpsmin(i)      ! in mb
     psdif=tcp_refps-psob    ! in mb
     alpha=max(min(psdif/tcp_width,one),zero)
     oberr=tcp_ermin+(tcp_ermax-tcp_ermin)*alpha

! Make sure the psob is reasonable
     if ( (psob<850._r_kind) .or. (psob>1025._r_kind) )then
        usage=100._r_kind
     end if

     if (olon >= r360) olon=olon-r360
     if (olon < zero)  olon=olon+r360
     dlat_earth_deg = olat
     dlon_earth_deg = olon
     dlat_earth = olat * deg2rad
     dlon_earth = olon * deg2rad
     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) then
           noutside=noutside+1
           cycle
        endif
     else 
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     end if

! Extract observation.
     ndata=min(ndata+1,maxobs)
     nodata=min(nodata+1,maxobs)

! convert pressure (mb) to log(pres(cb))
     pob=one_tenth*psob
 
     cdata_all(1,ndata)=oberr*one_tenth       ! obs error converted to cb
     cdata_all(2,ndata)=dlon                  ! grid relative longitude
     cdata_all(3,ndata)=dlat                  ! grid relative latitude
     cdata_all(4,ndata)=pob                   ! pressure in cb 
     cdata_all(5,ndata)=toff                  ! obs time (analyis relative hour)
     cdata_all(6,ndata)=ikx                   ! obs type
     cdata_all(7,ndata)=dlon_earth_deg        ! earth relative longitude (degrees)
     cdata_all(8,ndata)=dlat_earth_deg        ! earth relative latitude (degrees)
     cdata_all(9,ndata)=usage                 ! usage parameter
     cdata_all(10,ndata)=rstation_id          ! storm name (centerid_stormid)

! End of loop over number of storms
  end do

  write(6,*) 'READ_TCPS:  NUMBER OF OBS READ IN = ', ndata
  write(6,*) 'READ_TCPS: # out of domain =', noutside

! Write observations to scratch file

  call count_obs(ndata,maxdat,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)

! Close unit
  deallocate(cdata_all)
  call destroy_tcv_card
  close(lunin)

! End of routine
  return
end subroutine read_tcps
