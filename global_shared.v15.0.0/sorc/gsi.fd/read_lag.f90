subroutine read_lag(nread,ndata,nodata,infile,lunout, &
           obstype,twind,gstime,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_lag                      read lagrangian data
!   prgmmr: meunier          org: 610                 date: 2009-03-05
!
! abstract:  This routine read positions of lagrangian tracers 
!            (primarely balloons launch during the VORCORE campaign).
!            An ASCII file is taken as input :
!            number year month day hour min lon lat pressure
!
! program history log:
!   2009-03-05  meunier
!   2011-08-01  lueken  - changed F90 to f90 (no machine logic)
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2015-02-23  Rancic/Thomas - add l4densvar to time window logical
!
!   input argument list:
!     infile   - unit from which to read ozone data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     gstime   - analysis time in minutes from reference date
!     twind    - input group time window (hours)
!     sis      - sensor indicator
!
!   output argument list:
!     nread    - number of lagrandian observations read
!     ndata    - number of lagrandian observations for further processing
!     nodata   - number of lagrandian observations for further processing
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,regional,tll2xy,rlats,rlons
  use constants, only: deg2rad,zero,r60inv
  use convinfo, only: nconvtype,ctwind, &
      icuse,ictype,ioctype
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen

  use lag_fields, only: ntotal_orig_lag,orig_lag_num

  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=20),intent(in  ) :: sis
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind) ,intent(inout) :: ndata,nodata
  real(r_kind)    ,intent(in   ) :: gstime,twind

! Declare local parameters
  real(r_kind),parameter:: r360 = 360.0_r_kind
  integer(i_kind),parameter:: nmaxobs = 1e5
  integer(i_kind),parameter:: npara   = 10
  integer(i_kind),parameter:: idnum = 1
  integer(i_kind),parameter:: idlon = 3
  integer(i_kind),parameter:: idlat = 4
  integer(i_kind),parameter:: lunin = 10
  real(r_kind),parameter:: pmin=10_r_kind
  real(r_kind),parameter:: pmax=1000_r_kind
  real(r_kind),parameter:: lonmin=-180_r_kind
  real(r_kind),parameter:: lonmax=360_r_kind
  real(r_kind),parameter:: latmin=-90_r_kind
  real(r_kind),parameter:: latmax=90_r_kind
  integer(i_kind),parameter:: nreal=npara
  integer(i_kind),parameter:: nchanl=0

! Declare local variables
  real(r_kind),allocatable,dimension(:,:):: lagdata
  integer(i_kind):: ctype
  integer(i_kind):: iferror
  integer(i_kind):: inum,iyear,imonth,iday,ihour,imin,ikx
  integer(i_kind):: internalnum
  integer(i_kind):: nmind
  integer(i_kind),dimension(5):: idate5
  real(r_kind):: rlon,rlat,rpress
  real(r_kind):: rlonrad,rlatrad,rlongrid,rlatgrid
  real(r_kind):: sstime,r4dtime,r3dtime
  logical:: lerror,leof,lmax,lok
  integer(i_kind):: i,k

! Things for convinfo

  ! type definition depending on sis
  if (trim(sis) == 'lagP') then
     ctype=902;
  else
     ctype=901; !Default
  end if

  write(6,*) ' READLAG: sis=',trim(sis),' ctype=',ctype

  ikx = 0
  do i=1,nconvtype
     if(trim(obstype)==trim(ioctype(i)) .and. &
        ctype==ictype(i)) ikx=i
  end do
  if(ikx == 0)then
     write(6,*) ' READLAG: NO DATA REQUESTED'
     return
  end if

! If data have not to be used (convinfo)
  if (icuse(ikx)==0) then
     write(6,*) ' READLAG: DATA DISABLED IN CONVINFO'
     return
  end if


! Allocate the array to store data,
  allocate(lagdata(npara,nmaxobs))

! Opening file for reading
  open(lunin,file=trim(infile),form='formatted',iostat=iferror)
  lerror = (iferror/=0)

! Read the first line of the data file
  if (.not.lerror) then
     read(lunin,fmt=*,iostat=iferror) &
        inum,iyear,imonth,iday,ihour,imin,rlon,rlat,rpress
     lerror=(iferror>0)
     leof  =(iferror<0)
     lmax  =.false.
  end if

! Loop on the file
  do while (.not.(lerror .or. leof .or. lmax))
   
     nread = nread + 1 

     !Obs ok ?
     lok=(rlon>=lonmin .and. rlon<=lonmax .and. &
          rlat>=latmin .and. rlat<=latmax .and. &
          rpress>=pmin .and. rpress<=pmax)
     !Is there a starting point for this balloon
     internalnum=0
     findnum: do i=1,ntotal_orig_lag
        if (inum==orig_lag_num(i,1)) then
           internalnum=i
           exit findnum
        end if
     end do findnum
     lok=lok .and. (internalnum/=0)
     if (internalnum==0) &
        write(6,*)'READ_LAG: no reference for balloon',inum

     if (lok) then

        !Convert position in radians, and in grid relative
        if (rlon<zero) rlon = rlon + r360
        rlonrad= rlon * deg2rad
        rlatrad= rlat * deg2rad
        if(regional) then
           call tll2xy(rlonrad,rlatrad,rlongrid,rlatgrid,lok)
        else
           rlongrid=rlonrad
           rlatgrid=rlatrad
           call grdcrd1(rlongrid,rlons,nlon,1)
           call grdcrd1(rlatgrid,rlats,nlat,1)
        end if

        ! In the regional grid ?
        if (lok) then
        
           idate5(1) = iyear   !year
           idate5(2) = imonth  !month
           idate5(3) = iday    !day
           idate5(4) = ihour   !hour
           idate5(5) = imin    !minute
           call w3fs21(idate5,nmind)
           r4dtime=real(nmind-iwinbgn,r_kind)*r60inv
           if (l4dvar.or.l4densvar) then
              if (r4dtime<zero .OR. r4dtime>winlen) then
                 write(6,*)'READ_LAG: obs time idate5=',idate5,', t4dv=',&
                     r4dtime,' is outside time window'
                 lok=.false.
              end if
           else
              sstime=real(nmind,r_kind)
              r3dtime=(sstime-gstime)*r60inv
              if(abs(r3dtime) > twind .or. abs(r3dtime) > ctwind(ikx)) then
                 write(6,*)'READ_LAG: obs time idate5=',idate5,', t3dv=',&
                     r3dtime,' is outside time window, twind=',min(twind,ctwind(ikx))
                 lok=.false.
              end if
           end if

           ! Time ok ?
           if (lok) then
          
              ndata  = ndata+1
              nodata = nodata + 1
            
              lagdata(idnum,ndata)=real(internalnum,r_kind)   ! balloon number
              lagdata(2,ndata)=r4dtime
              lagdata(idlon,ndata)=rlongrid           ! grid relative longitude
              lagdata(idlat,ndata)=rlatgrid           ! grid relative lattitude
              lagdata(5,ndata)=rlonrad                ! longitude (radians)
              lagdata(6,ndata)=rlatrad                ! lattitude (radians)
              lagdata(7,ndata)=rpress                 ! Pressure (hPa)
              lagdata(8,ndata)=real(ikx,r_kind)       ! ob type (convinfo)
              lagdata(9,ndata)=zero                   ! localization error (meters)
                                                      ! 0 if not in the file
           end if
        end if
     end if

    ! read the next one
     read(lunin,fmt=*,iostat=iferror) &
        inum,iyear,imonth,iday,ihour,imin,rlon,rlat,rpress
     lerror=(iferror>0)
     leof  =(iferror<0)
     lmax  =nreal>=nmaxobs
    
  end do

  if (lmax) then
     write(6,*) ' READ_LAG:   Number of lagrangian obs reached maxobs = ', &
        nmaxobs
  end if

  if (.not.lerror) then
     ! Write header record and data to output file for further processing
     write(lunout) obstype,sis,nreal,nchanl,idnum
     write(lunout) ((lagdata(k,i),k=1,npara),i=1,ndata)
     write(6,*) ' READ_LAG:   Number of lagrangian obs read = ', &
        nread
     write(6,*) ' READ_LAG:   Number of lagrangian obs to be use = ', &
        ndata
  else
     write(6,*) ' READ_LAG:   I/O error while processing obs file'
  end if

  ! Close unit to input data file
  close(lunin)

  deallocate(lagdata)
  
end subroutine read_lag
