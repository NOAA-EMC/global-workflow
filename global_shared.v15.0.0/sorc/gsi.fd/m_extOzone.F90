module m_extOzone
!#define NO_EXTRA_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_extOzone
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2013-09-27
!
! abstract: a module for reading extra ozone observation data
!
! program history log:
!   2012-09-10  wargan  - add OMI text option for OMI with efficiency factors
!   2012-12-06  jin     - read MLS o3lev data from bufr files.
!                         keep's Kris's ascii reading and rename the type to 'o3levtext'.
!   2012-12-14  jin/Wargan - fixed mflg error under "o3levtext" handling block.
!   2013-01-18  Wargan  - read omieffnc from a NetCDF file, and removed omieff text.
!   2013-02-05  guo     - STOP statements were replaced with die() to signal an _abort_.
!                       - dfile_format() is used to handle formats of "omieff" and "o3lev".
!                       - code is slightly cleaned up for possible restructuring
!                         with alternative format handling.
!   2013-09-27  guo   - added this document block.
!   2014-02-03  guo     - restructured from GMAO extended read_ozone() routine
!                         as a separated module for extended ozone obstypes, in
!                         particular, if not in BUFR format.
!                       - removed "text" option of "o3lev", for it is not been
!                         used anymore.
!                       - Moved history log messages here from read_ozone.
!   2015-09-17  Thomas  - add l4densvar and thin4d to data selection procedure
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

  use kinds, only: i_kind,r_kind,r_double
  implicit none
  private     ! except

  public:: is_extOzone
  public:: extOzone_read

!!  public:: extOzone_setupoz
!!  public:: extOzone_setupozlev
!!  public:: extOzone_setupoztot

!!  public:: extOzone_intoz
!!  public:: extOzone_intozlev
!!  public:: extOzone_intoztot

  interface is_extOzone; module procedure is_extOzone_; end interface
  interface extOzone_read; module procedure read_; end interface


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*) , parameter:: myname='m_extOzone'

  integer(kind=i_kind), parameter:: MAXOBS_ = 1000000

  real   (kind=r_kind), parameter:: RMISS = -9999.9_r_kind
  real   (kind=r_kind), parameter:: badoz = 10000.0_r_kind
  real   (kind=r_kind), parameter:: r6    =     6.0_r_kind
  real   (kind=r_kind), parameter:: r360  =   360.0_r_kind

contains
function is_extOzone_(dfile,dtype,dplat,class)

  use obsmod   , only: dfile_format
  use mpeu_util, only: die,perr
  implicit none
  logical:: is_extozone_              ! this is the function return variable
  character(len=*),intent(in):: dfile   ! observation input filename (see gsiparm.anl:&OBSINPUT/)
  character(len=*),intent(in):: dtype   ! observation type (see gsiparm.anl:&OBSINPUT/)
  character(len=*),intent(in):: dplat   ! platform (see gsiparm.anl:&OBSINPUT/)
  character(len=*),optional,intent(in):: class  ! specify either "level" or "layer" for sub-class

! Use Case 1: (in read_obs())
!
!   elseif(is_extOzone(dfile,obstype,dplat)) then
!     call read_ozone(obstype,dplat,dsis,dfile,...)
!
!
! Use Case 2: (in setuprhsall())
!
!   elseif(is_extOzone(dfile,obstype,dplat,class='level')) then
!     call setupozlev(obstype,dplat,dsis,...)
!   elseif(is_extOzone(dfile,obstype,dplat,class='layer')) then
!     call setupozlay(obstype,dplat,dsis,...)
!   elseif(is_extOzone(dfile,obstype,dplat,class='total')) then
!     call setupozlay(obstype,dplat,dsis,...)
!
! Use Case 3: (where intoz() or stpoz() are called)
!
!   elseif(is_extOzone(dfile,obstype,dplat)) then
!     call intoz(obstype,dplat,dsis,...)

  character(len=*), parameter:: myname_=myname//'::is_extOzone_'

  integer(kind=i_kind),parameter:: iANY    = 0
  integer(kind=i_kind),parameter:: iUNKNOWN=-1

  integer(kind=i_kind),parameter:: iLEVEL  = 1
  integer(kind=i_kind),parameter:: iLAYER  = 2
  integer(kind=i_kind),parameter:: iTOTAL  = 3

  integer(kind=i_kind),parameter:: iTEXT   = 1
  integer(kind=i_kind),parameter:: iBUFR   = 2
  integer(kind=i_kind),parameter:: iNC     = 3

  integer(kind=i_kind):: class_,ifile_

  ifile_=iUNKNOWN
  select case(dfile_format(dfile))
  case("text")
    ifile_=iTEXT
  case("bufr")
    ifile_=iBUFR
  case("nc")
    ifile_=iNC
  end select

  class_=iANY
  if(present(class)) then
    select case(class)
    case('level')
      class_=iLEVEL
    case('layer')
      class_=iLAYER
    case('total')
      class_=iTOTAL
    case default
      class_=iUNKNOWN
      call perr(myname_,'unknown ozone class, class =',class)
      call perr(myname_,'                     dfile =',dfile)
      call perr(myname_,'                     dtype =',dtype)
      call perr(myname_,'                     dplat =',dplat)
      call  die(myname_)
    end select
  endif
    
  is_extOzone_= .false.
#ifndef NO_EXTRA_
  select case(class_)
  case(iANY)
    is_extOzone_= &
      ifile_==iBUFR .and. dtype == 'o3lev'   .or. &
      ifile_==iNC   .and. dtype == 'mls55'   .or. &
      ifile_==iNC   .and. dtype == 'omieff'  .or. &
      ifile_==iNC   .and. dtype == 'tomseff'

  case(iLEVEL)
    is_extOzone_= &
      ifile_==iBUFR .and. dtype == 'o3lev'   .or. &
      ifile_==iNC   .and. dtype == 'mls55'

  case(iLAYER)
    is_extOzone_= .false.

  case(iTOTAL)
    is_extOzone_= &
      ifile_==iNC   .and. dtype == 'omieff'  .or. &
      ifile_==iNC   .and. dtype == 'tomseff'

  case default
    is_extOzone_= .false.
  end select
#endif

end function is_extOzone_

subroutine read_(dfile,dtype,dplat,dsis, &      ! intent(in), keys for type managing
  nread,npuse,nouse, &                          ! intent(out), beside other implicit output variables
  jsatid,gstime,lunout,twind,ithin,rmesh,nobs)       ! intent(in), beside other implicit input variables

  use obsmod   , only: dfile_format
  use constants, only: zero
  use mpeu_util, only: die,perr,tell
  use mpimod, only: npe
!  use mpeu_util, only: mprefix,stdout
!     nobs     - array of observations on each subdomain for each processor

  implicit none
  character(len=*), intent(in):: dfile   ! obs_input filename
  character(len=*), intent(in):: dtype   ! observation type (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dplat   ! platform (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dsis    ! sensor/instrument/satellite tag (see gsiparm.anl:&OBSINPUT/ and ozinfo)

! Use Case: (in read_ozone())
!
!   elseif(is_extOzone(dfile,dtype,dplat)) then
!     call extOzone_read(dtype,dplat,dsis,dfile,...)
!

  character(len=*), parameter:: myname_=myname//'::read_'

  integer(kind=i_kind), intent(out):: nread     ! number of obs record reads in this call
  integer(kind=i_kind),dimension(npe), intent(inout):: nobs     ! number of obs record reads in this call
  integer(kind=i_kind), intent(out):: npuse     ! nnmber of "preofiles" retained in this call
  integer(kind=i_kind), intent(out):: nouse     ! nnmber of obs retained in this call

  ! jsatid,gstime,lunout,twind,ithin,rmesh
  character(len=*)    , intent(in ):: jsatid    ! platform ID (verification)
  real   (kind=r_kind), intent(in ):: gstime    ! analysis time (minute) from reference date
  integer(kind=i_kind), intent(in ):: lunout    ! logical unit to send obs output.
  real   (kind=r_kind), intent(in ):: twind     ! input group time window (hour)

  integer(kind=i_kind), intent(in ):: ithin     ! flag to thin data
  real   (kind=r_kind), intent(in ):: rmesh     ! thining mesh size (km)

  real(kind=r_kind),pointer,dimension(:,:):: p_out

  integer(kind=i_kind):: nreal,nchan,ilat,ilon
  integer(kind=i_kind):: maxobs
  integer(kind=i_kind):: i,k

  nread=-1
  npuse=-1
  nouse=-1

  if(.not.is_extOzone(dfile,dtype,dplat)) then
    call perr(myname_,'unexpected use, dfile =',dfile)
    call perr(myname_,'                dtype =',dtype)
    call perr(myname_,'                dplat =',dplat)
    call die (myname_)
  endif

  select case(dtype)
  case('omieff','tomseff')       ! layer-ozone or total-ozone types
     select case(dfile_format(dfile))
     case('nc')
        call oztot_ncInquire_(dfile,dtype,dplat, &
                              nreal,nchan,ilat,ilon, &
                              ithin,rmesh,maxobs)

        allocate(p_out(nreal+nchan,maxobs))
        p_out(:,:)=RMISS

        call oztot_ncRead_(dfile,dtype,dplat,dsis, p_out,nread,npuse,nouse, &
                           gstime, twind, nreal,nchan,ilat,ilon, ithin,rmesh)

                ! Skip all "thinned" data records, and reset values of
                ! nread, npuse, and nouse, as they are required by
                ! upper level read_obs().

        k=0
        do i=1,maxobs
           if(p_out(1,i)>zero) then
             k=k+1
             if(i>k) p_out(:,k)=p_out(:,i)
           endif
        enddo
        nouse=k
        npuse=k
     end select

  case('o3lev')         ! level-ozone types
     select case(dfile_format(dfile))
!     case('text')
!       call ozlev_textInquire(dfile,dtype,dplat,  &
!                               nreal,nchan,ilat,ilon, maxobs)
!
!       allocate(p_out(nreal+nchan,maxobs))
!        p_out(:,:)=RMISS
!
!        call ozlev_textRead_(dfile,dtype,dplat,dsis, p_out,nread,npuse,nouse, &
!                             gstime,twind, nreal,nchan,ilat,ilon)
!
     case('bufr')
        call ozlev_bufrInquire_(dfile,dtype,dplat,  &
                                nreal,nchan,ilat,ilon,maxobs)

        allocate(p_out(nreal+nchan,maxobs))
        p_out(:,:)=RMISS

        call ozlev_bufrRead_(dfile,dtype,dplat,dsis, p_out,nread,npuse,nouse, &
                             jsatid, gstime,twind, nreal,nchan,ilat,ilon)
     end select

  case('mls55')
     select case(dfile_format(dfile))
     case('nc')
        call ozlev_ncInquire_(dfile,dtype,dplat,  &
                              nreal,nchan,ilat,ilon,maxobs)

        allocate(p_out(nreal+nchan,maxobs))
        p_out(:,:)=RMISS

        call ozlev_ncRead_(dfile,dtype,dplat,dsis, p_out,nread,npuse,nouse, &
                             gstime,twind, nreal,nchan,ilat,ilon)
     end select

  end select

  if(nouse<0 .or. .not.associated(p_out)) then
     call perr(myname_,'can not process, dtype =',trim(dtype))
     call perr(myname_,'                 dplat =',trim(dplat))
     call perr(myname_,'                  dsis =',trim(dsis))
     call perr(myname_,'                 dfile =',trim(dfile))
     call perr(myname_,'     associated(p_out) =',associated(p_out))
     if(associated(p_out)) then
       call perr(myname_,'  actual size(p_out,1) =',size(p_out,1))
       call perr(myname_,'  actual size(p_out,2) =',size(p_out,2))
       call perr(myname_,'                 nread =',nread)
       call perr(myname_,'                 npuse =',npuse)
       call perr(myname_,'                 nouse =',nouse)
     endif
     call  die(myname_)
  endif

  if(nreal+nchan/=size(p_out,1)) then
    call perr(myname_,'unexpected size(p_out,1), nreal+nchan =',nreal+nchan)
    call perr(myname_,'                                nreal =',nreal)
    call perr(myname_,'                                nchan =',nchan)
    call perr(myname_,'                 actual size(p_out,1) =',size(p_out,1))
    call perr(myname_,'                 actual size(p_out,2) =',size(p_out,2))
    call  die(myname_)
  endif

! Output candidate observations from _dfile_ to _obsfile_ (pre-opened in lunout).

!  write(stdout,'(3a,3i8,f8.2)') mprefix('read_ozone'), &
!     ' obstype,nread,npuse,nouse,no/npuse = ',dtype,nread,npuse,nouse,real(nouse)/npuse

        ! While nreal+nchan defines the leading dimension of p_out(:,:), it is
        ! obviously a bad idea that n_out has been missing from the header for
        ! its second dimension.

  call count_obs(npuse,nreal,ilat,ilon,p_out,nobs)
  write(lunout) dtype, dsis, nreal, nchan, ilat, ilon
  write(lunout) p_out(:,1:npuse)

  deallocate(p_out)

end subroutine read_

subroutine oztot_ncInquire_(dfile,dtype,dplat, nreal,nchan,ilat,ilon, ithin,rmesh,maxrec)
  use satthin, only: satthin_itxmax    => itxmax
  use satthin, only: satthin_makegrids => makegrids
  implicit none
  character(len=*), intent(in):: dfile   ! obs_input filename
  character(len=*), intent(in):: dtype   ! observation type (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dplat   ! platform (see gsiparm.anl:&OBSINPUT/)

  integer(kind=i_kind), intent(out):: nreal  ! number of real parameters per record
  integer(kind=i_kind), intent(out):: nchan  ! number of channels or levels per record
  integer(kind=i_kind), intent(out):: ilat   ! index to latitude in nreal parameters.
  integer(kind=i_kind), intent(out):: ilon   ! index to longitude in nreal parameters.

  integer(kind=i_kind), intent(in ):: ithin     ! flag to thin data
  real   (kind=r_kind), intent(in ):: rmesh     ! size (km) of the thinning mesh

  integer(kind=i_kind), intent(out):: maxrec    ! extimated input record count

  character(len=*), parameter:: myname_=myname//'::oztot_ncInquire_'

!    Configure the record buffer for this obs class
     nreal=36
     nchan=1
     ilat=4
     ilon=3

!    Make thinning grids, and to define the total record size.
     call satthin_makegrids(rmesh,ithin)
     maxrec=satthin_itxmax

end subroutine oztot_ncInquire_

!..................................................................................
subroutine oztot_ncread_(dfile,dtype,dplat,dsis, ozout,nmrecs,ndata,nodata, &
                         gstime,twind, nreal,nchan,ilat,ilon,ithin,rmesh)
!..................................................................................

  use netcdf, only: nf90_open,nf90_close
  use netcdf, only: nf90_nowrite,nf90_noerr
  use netcdf, only: nf90_inq_dimid,nf90_inquire_dimension
  use netcdf, only: nf90_inq_varid,nf90_get_var

  use satthin, only: satthin_itxmax       => itxmax
  use satthin, only: satthin_makegrids    => makegrids
  use satthin, only: satthin_map2tgrid    => map2tgrid
  use satthin, only: satthin_finalcheck   => finalcheck
  use satthin, only: satthin_destroygrids => destroygrids

  use gridmod, only: nlat,nlon,regional,tll2xy,rlats,rlons
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen,l4densvar,thin4d
  use obsmod, only: nloz_omi

  use constants, only: deg2rad,zero,rad2deg,r60inv

  use netcdf_mod, only: nc_check
!  use mpeu_util, only: mprefix,stdout

  implicit none
  character(len=*), intent(in):: dfile   ! obs_input filename
  character(len=*), intent(in):: dtype   ! observation type (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dplat   ! platform (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dsis    ! sensor/instrument/satellite tag (see gsiparm.anl:&OBSINPUT/ and ozinfo)

  real   (kind=r_kind), dimension(:,:), intent(out):: ozout
  integer(kind=i_kind), intent(out):: nmrecs ! read count in ozout
  integer(kind=i_kind), intent(out):: ndata  ! "good" profile count in ozout
  integer(kind=i_kind), intent(out):: nodata ! "good" data count in ozout

  real   (kind=r_kind), intent(in):: gstime ! analysis time (minute) from reference date
  real   (kind=r_kind), intent(in):: twind  ! input group time window (hour)

  integer(kind=i_kind), intent(in):: nreal  ! number of real parameters per record
  integer(kind=i_kind), intent(in):: nchan  ! number of channels or levels per record
  integer(kind=i_kind), intent(in):: ilat   ! index to latitude in nreal parameters.
  integer(kind=i_kind), intent(in):: ilon   ! index to longitude in nreal parameters.

  integer(kind=i_kind), intent(in ):: ithin     ! flag to thin data
  real   (kind=r_kind), intent(in ):: rmesh     ! size (km) of the thinning mesh

  character(len=*), parameter:: myname_=myname//'::oztot_ncRead_'

! parameters for output bookkeeping
  integer(kind=i_kind):: i, irec
  integer(kind=i_kind):: itx,itt

! parameters for NetCDF arrays
  integer(kind=i_kind), allocatable :: iya(:),ima(:),idda(:),ihha(:),imina(:),fovna(:)
  integer(kind=i_kind), allocatable :: toqfa(:),alqfa(:)
  real   (kind=r_kind), allocatable :: rseca(:),slatsa(:),slonsa(:),totoza(:),szaa(:)
  real   (kind=r_kind), allocatable :: aprioria(:,:),efficiencya(:,:)
  integer(kind=i_kind)  nrecDimId,nomilevsDimID,lonVarID,latVarID,yyVarID,mmVarID
  integer(kind=i_kind)  ddVarID,hhVarID,minVarID,ssVarID,fovnVarID,toqfVarID,alqfVarID
  integer(kind=i_kind)  szaVarID,totozVarID,aprioriVarID,efficiencyVarID
  integer(kind=i_kind)  ier, ncid, nomilevs
  integer(kind=i_kind):: iy,im,idd,ihh,imin,nmind
  integer(kind=i_kind):: toqf,alqf,fovn
  real   (kind=r_kind):: rsec,slats,slons
  real   (kind=r_kind),dimension(nloz_omi):: apriori, efficiency
  integer(kind=i_kind):: binary(17)

  real   (kind=r_kind):: dlon,dlon_earth
  real   (kind=r_kind):: dlat,dlat_earth
  real   (kind=r_kind):: tdiff,sstime,t4dv,timedif,crit1,dist1,rsat
  integer(kind=i_kind):: idate5(5)

  real   (kind=r_double):: totoz, sza

  logical:: outside, removeScans, iuse
  integer(kind=i_kind):: maxobs


  removeScans = .true. ! remove OMI scan numbers >= 25?
  maxobs=size(ozout,2)
  rsat=999._r_kind

! Using OMI/TOMS with efficience factors in NetCDF format
     nodata = 0
     ndata  = 0

     call nc_check(nf90_open(trim(dfile),nf90_nowrite,ncid),&
         myname_,'open '//trim(dfile),stat=ier)

     ! ignore if the file is not actually present.
     if(ier/=nf90_noerr) go to 136

     ! Get dimensions from OMI input file
     call nc_check(nf90_inq_dimid(ncid, "nrec", nrecDimId),&
         myname_,'inq_dimid nrec '//trim(dfile),stat=ier)

     ! ignore if the file header is empty
     if(ier/=nf90_noerr) then
        call nc_check(nf90_close(ncid),&
            myname_,'close '//trim(dfile),stat=ier)
        go to 136
     endif

     ! Get dimensions from OMI/TOMS input file
!!!!     nmrecs=0
     call nc_check(nf90_inquire_dimension(ncid, nrecDimId, len = nmrecs),&
         myname_,'inquire_dimension nrec '//trim(dfile),stat=ier)

     ! ignore if the file header is empty
     if(ier/=nf90_noerr .or. nmrecs==0) then
        call nc_check(nf90_close(ncid),&
            myname_,'close '//trim(dfile),stat=ier)
        go to 136
     endif

     ! Continue the input
     call nc_check(nf90_inq_dimid(ncid, "nlevs", nomilevsDimId),&
         myname_,'inq_dimid nlevs '//trim(dfile))
     call nc_check(nf90_inquire_dimension(ncid, nomilevsDimId, len = nomilevs),&
         myname_,'inquire_dimension nlevs '//trim(dfile))
     
     ! We have dimensions so we can allocate arrays
     allocate(iya(nmrecs),ima(nmrecs),idda(nmrecs),ihha(nmrecs),imina(nmrecs), &
          rseca(nmrecs),fovna(nmrecs),slatsa(nmrecs),slonsa(nmrecs),totoza(nmrecs), &
          toqfa(nmrecs),alqfa(nmrecs),szaa(nmrecs))
     allocate(aprioria(nomilevs,nmrecs),efficiencya(nomilevs,nmrecs))

     ! Read variables and store them in these arrays
     call nc_check(nf90_inq_varid(ncid, "lon", lonVarId),&
         myname_,'inq_varid lon '//trim(dfile))
     call nc_check(nf90_get_var(ncid, lonVarId, slonsa),&
         myname_,'get_var lon '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "lat", latVarId),&
         myname_,'inq_varid lat '//trim(dfile))
     call nc_check(nf90_get_var(ncid, latVarId, slatsa),&
         myname_,'get_var lat '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "yy", yyVarId),&
         myname_,'inq_varid yy '//trim(dfile))
     call nc_check(nf90_get_var(ncid, yyVarId, iya),&
         myname_,'get_var yy '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "mm", mmVarId),&
         myname_,'inq_varid mm '//trim(dfile))
     call nc_check(nf90_get_var(ncid, mmVarId, ima),&
         myname_,'get_var mm '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "dd", ddVarId),&
         myname_,'inq_varid dd '//trim(dfile))
     call nc_check(nf90_get_var(ncid, ddVarId, idda),&
         myname_,'get_var dd '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "hh", hhVarId),&
         myname_,'inq_varid hh '//trim(dfile))
     call nc_check(nf90_get_var(ncid, hhVarId, ihha),&
         myname_,'get_var hh '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "min", minVarId),&
         myname_,'inq_varid min '//trim(dfile))
     call nc_check(nf90_get_var(ncid, minVarId, imina),&
         myname_,'get_var min '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "ss", ssVarId),&
         myname_,'inq_varid ss '//trim(dfile))
     call nc_check(nf90_get_var(ncid, ssVarId, rseca),&
         myname_,'get_var ss '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "fov", fovnVarId),&
         myname_,'inq_varid fov '//trim(dfile))
     call nc_check(nf90_get_var(ncid, fovnVarId, fovna),&
         myname_,'get_var fov '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "sza", szaVarId),&
         myname_,'inq_varid sza '//trim(dfile))
     call nc_check(nf90_get_var(ncid, szaVarId, szaa),&
         myname_,'get_var sza '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "toqf", toqfVarId),&
         myname_,'inq_varid toqf '//trim(dfile))
     call nc_check(nf90_get_var(ncid, toqfVarId, toqfa),&
         myname_,'get_var toqf '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "alqf", alqfVarId),&
         myname_,'inq_varid alqf '//trim(dfile))
     call nc_check(nf90_get_var(ncid, alqfVarId, alqfa),&
         myname_,'get_var alqf '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "toz", totozVarId),&
         myname_,'inq_varid toz '//trim(dfile))
     call nc_check(nf90_get_var(ncid, totozVarId, totoza),&
         myname_,'get_var toz '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "apriori", aprioriVarId),&
         myname_,'inq_varid apriori '//trim(dfile))
     call nc_check(nf90_get_var(ncid, aprioriVarId, aprioria),&
         myname_,'get_var apriori '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "efficiency", efficiencyVarId),&
         myname_,'inq_varid efficiency '//trim(dfile))
     call nc_check(nf90_get_var(ncid, efficiencyVarId, efficiencya),&
         myname_,'get_var efficiency '//trim(dfile))

     ! close the data file
     call nc_check(nf90_close(ncid),&
         myname_,'close '//trim(dfile))
     
     ! now screen the data and put them into the right places
     do irec = 1, nmrecs
        iy = iya(irec)
        im = ima(irec)
        idd = idda(irec)
        ihh = ihha(irec)
        imin = imina(irec)
        rsec = rseca(irec)
        fovn = fovna(irec)
        slats = slatsa(irec)
        slons = slonsa(irec)
        totoz = totoza(irec)
        toqf = toqfa(irec)
        alqf = alqfa(irec)
        sza = szaa(irec)
        do i = 1, nomilevs
           apriori(i) = aprioria(i, irec)
           ! Reported efficiencies from layer 4 up (counting from the surface
           ! so from 1 - 8 here) are incorrect (too large) for high SZA
           ! Setting them all to 1.0 per PK Bhartia''s recommendation
           if (i .le. 8) then
              efficiency(i) = 1._r_kind
           else
              efficiency(i) = efficiencya(i, irec) 
           endif
        end do
        
!!!!        nmrecs=nmrecs+1
        !      Convert observation location to radians
        if(abs(slats)>90._r_kind .or. abs(slons)>r360) go to 135  
        if(slons< zero) slons=slons+r360
        if(slons==r360) slons=zero
        dlat_earth = slats * deg2rad
        dlon_earth = slons * deg2rad

        if(regional)then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           if(outside) go to 135
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        endif

!    convert observation time to relative time
        idate5(1) = iy    !year 
        idate5(2) = im    !month
        idate5(3) = idd   !day
        idate5(4) = ihh   !hour
        idate5(5) = imin  !minute
        call w3fs21(idate5,nmind)

        t4dv=real((nmind-iwinbgn),r_kind)*r60inv
        sstime=real(nmind,r_kind)
        tdiff=(sstime-gstime)*r60inv
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) go to 135
        else
           if(abs(tdiff) > twind) go to 135
        end if

        if (totoz > badoz ) goto 135
        
! Apply data screening based on quality flags
! Bit 10 (from the left) in TOQF represents row anomaly.  All 17 bits in toqf is
! supposed to converted into array elements of binary(:), either for "tomseff" or
! "omieff".
        binary(:)=0
        select case(dtype)
        case('omieff')

           if (toqf .ne. 0 .and. toqf .ne. 1) go to 135 
 
!       Remove obs at high solar zenith angles
           if (sza > 84.0_r_kind) goto 135

!       remove the bad scan position data: fovn beyond 25
           if (removeScans) then
              if (fovn >=25_i_kind) goto 135
           endif
           if (fovn <=2_i_kind .or. fovn >=58_i_kind) goto 135

!       remove the data in which the C-pair algorithm ((331 and 360 nm) is used. 
           if (alqf == 3_i_kind .or. alqf == 13_i_kind) goto 135

        case('tomseff')
        ! The meaning of quality flags for TOMS version 8 is similar to that
        ! for SBUV:
        ! 0 - good data, 1 - good data with SZA > 84 deg
           if (toqf /= 0) goto 135

        case default
        end select

!       thin OMI/TOMS data

        if (thin4d) then 
           timedif = zero 
        else 
           timedif = r6*abs(tdiff)        ! range:  0 to 18 
        endif 
        crit1 = 0.01_r_kind+timedif
        if (ithin /= 0) then
           call satthin_map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,dsis)
           if(.not. iuse)go to 135 
           call satthin_finalcheck(dist1,crit1,itx,iuse)
           if(.not. iuse)go to 135
           ndata=ndata+1
           nodata=ndata
        else
           ndata=ndata+1
           nodata=ndata
           itx= ndata
        end if

!!        ASSERT_(size(ozout,2)>=itx)

        if(itx <= maxobs) then
           ozout(1,itx)=rsat
           ozout(2,itx)=t4dv
           ozout(3,itx)=dlon               ! grid relative longitude
           ozout(4,itx)=dlat               ! grid relative latitude
           ozout(5,itx)=dlon_earth*rad2deg ! earth relative longitude (degrees)
           ozout(6,itx)=dlat_earth*rad2deg ! earth relative latitude (degrees)
           ozout(7,itx)=real(toqf)         ! - total ozone quality code (not used)
           ozout(8,itx)=real(sza)          ! solar zenith angle
           ozout(9,itx)=binary(10)         ! row anomaly flag, is actually fixed to 0
           ozout(10,itx)=0.                ! - cloud amount (not used)
           ozout(11,itx)=0.                ! - vzan (not used)
           ozout(12,itx)=0.                ! - aerosol index (not used)
           ozout(13,itx)=0.                ! - ascending/descending (not used)
           ozout(14,itx)=real(fovn)        ! scan position
                                           ! "(not used)" flags above imply that they
                                           ! are not used in setupozlay().

! Added apriori and efficiency profiles 
          ozout(15:25,itx)=apriori        
          ozout(26:36,itx)=efficiency
          ozout(37,itx)=totoz 
        endif

!!        ASSERT_(size(ozout,1)==37)

135     continue
     end do

     deallocate(iya,ima,idda,ihha,imina, &
          rseca,fovna,slatsa,slonsa,totoza, &
          toqfa,alqfa,szaa,aprioria,efficiencya)
136  continue
! end
!    End of loop over observations
! End of OMI block with efficiency factors in NetCDF format

      call satthin_destroygrids()

      nodata=min(ndata,maxobs)
!      write(stdout,'(3a,3i8,f8.2)') mprefix('read_ozone'), &
!         ' obstype,nmrecs,ndata,nodata,no/ndata = ',dtype,nmrecs,ndata,nodata,real(nodata)/ndata

end subroutine oztot_ncread_
!..................................................................................


subroutine ozlev_ncInquire_(dfile,dtype,dplat, nreal,nchan,ilat,ilon, maxrec)
  implicit none
  character(len=*), intent(in):: dfile   ! obs_input filename
  character(len=*), intent(in):: dtype   ! observation type (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dplat   ! platform (see gsiparm.anl:&OBSINPUT/)

  integer(kind=i_kind), intent(out):: nreal  ! number of real parameters per record
  integer(kind=i_kind), intent(out):: nchan  ! number of channels or levels per record
  integer(kind=i_kind), intent(out):: ilat   ! index to latitude in nreal parameters.
  integer(kind=i_kind), intent(out):: ilon   ! index to longitude in nreal parameters.

  integer(kind=i_kind), intent(out):: maxrec    ! extimated input record count

  character(len=*), parameter:: myname_=myname//'::ozlev_ncInquire_'

!    Configure the record, they are not (dfile,dtype,dplat) dependent in this case.
     nreal = 12
     nchan =  1   ! There are 'mlslevs' levels but each is treated 
                  ! as a separate observation so that nchanl = 1
     ilat=4
     ilon=3

     maxrec = MAXOBS_
end subroutine ozlev_ncInquire_

!..................................................................................
subroutine ozlev_ncread_(dfile,dtype,dplat,dsis, ozout,nmrecs,ndata,nodata, gstime,twind, nreal,nchan,ilat,ilon)
!..................................................................................
  use netcdf, only: nf90_open,nf90_close
  use netcdf, only: nf90_nowrite,nf90_noerr
  use netcdf, only: nf90_inq_dimid,nf90_inquire_dimension
  use netcdf, only: nf90_inq_varid,nf90_get_var

  use gridmod, only: nlat,nlon,regional,tll2xy,rlats,rlons
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen,l4densvar

  use constants, only: deg2rad,zero,rad2deg,one_tenth,r60inv
  use ozinfo, only: jpch_oz,nusis_oz,iuse_oz
  use mpeu_util, only: perr,die
  use netcdf_mod, only: nc_check
!  use mpeu_util, only: mprefix,stdout

  implicit none
  character(len=*), intent(in):: dfile   ! obs_input filename
  character(len=*), intent(in):: dtype   ! observation type (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dplat   ! platform (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dsis    ! sensor/instrument/satellite tag (see gsiparm.anl:&OBSINPUT/ and ozinfo)

  real   (kind=r_kind), dimension(:,:), intent(out):: ozout
  integer(kind=i_kind), intent(out):: nmrecs ! count of records read
  integer(kind=i_kind), intent(out):: ndata  ! count of processed
  integer(kind=i_kind), intent(out):: nodata ! count of retained

  real   (kind=r_kind), intent(in):: gstime ! analysis time (minute) from reference date
  real   (kind=r_kind), intent(in):: twind  ! input group time window (hour)

  integer(kind=i_kind), intent(in):: nreal  ! number of real parameters per record
  integer(kind=i_kind), intent(in):: nchan  ! number of channels or levels per record
  integer(kind=i_kind), intent(in):: ilat   ! index to latitude in nreal parameters.
  integer(kind=i_kind), intent(in):: ilon   ! index to longitude in nreal parameters.

  character(len=*), parameter:: myname_=myname//'::ozlev_ncRead_'

  integer(kind=i_kind):: ier, iprof, nprofs, maxobs
  integer(kind=i_kind):: i, ilev, ikx, ncid
!  integer(kind=i_kind)  ier, ncid, nomilevs
  integer(kind=i_kind),allocatable,dimension(:):: ipos

  integer(kind=i_kind):: nrecDimId,lonVarID,latVarID,yyVarID,mmVarID
  integer(kind=i_kind):: ddVarID,hhVarID,minVarID,ssVarID
  integer(kind=i_kind):: pressVarID
  integer(kind=i_kind):: convVarID, qualVarID, mlserrVarID, mlsozoneVarID
  integer(kind=i_kind):: mlslevsDimID,mlslevs

  integer(kind=i_kind), allocatable :: iya(:),ima(:),idda(:),ihha(:),imina(:),iseca(:)
  real   (kind=r_kind), allocatable :: slatsa(:),slonsa(:)
  real   (kind=r_kind), allocatable :: mlspress(:), mlsozone(:,:), mlsqual(:)
  real   (kind=r_kind), allocatable :: mlsconv(:), mlserr(:,:)

  integer(kind=i_kind):: nmind

  real   (kind=r_kind):: slons0,slats0
  real   (kind=r_kind):: ppmv, pres, pob, obserr, usage
  !real(kind=r_kind) tdiff,sstime,slons,slats,dlon,dlat,t4dv,timedif,crit1,dist1
  !real(kind=r_kind) !slons0,slats0,rsat,solzen,solzenp,dlat_earth,dlon_earth
  !real(kind=r_kind) !rsec, ppmv, prec, pres, pob, obserr, usage

  real   (kind=r_kind):: dlon,dlon_earth
  real   (kind=r_kind):: dlat,dlat_earth
  real   (kind=r_kind):: tdiff,sstime,t4dv,rsat
  integer(kind=i_kind):: idate5(5)

  logical:: outside

  maxobs=size(ozout,2)
  rsat=999._r_kind
  ndata = 0
  nmrecs=0
!..................................................................................
     ! ---------------MLS NRT NetCDF -----------------------------
     ! Process MLS o3lev in NetCDF format
     ! Open file and read dimensions
     call nc_check(nf90_open(trim(dfile),nf90_nowrite,ncid),&
         myname_,'open '//trim(dfile),stat=ier)

     ! ignore if the file is not actually present.
     if(ier/=nf90_noerr) return ! go to 170
   
     ! Get dimensions from OMI input file
     call nc_check(nf90_inq_dimid(ncid, "nprofiles", nrecDimId),&
         myname_,'inq_dimid nprofiles '//trim(dfile),stat=ier)

     ! ignore if the file header is empty
     if(ier/=nf90_noerr) then
        call nc_check(nf90_close(ncid),&
            myname_,'close '//trim(dfile),stat=ier)
        return ! go to 170
     endif

     ! Get dimensions from MLS input file: # of profiles and # of levels
     nprofs=0
     call nc_check(nf90_inquire_dimension(ncid, nrecDimId, len = nprofs),&
         myname_,'inquire_dimension nprofiles '//trim(dfile),stat=ier)

     ! ignore if the file header is empty
     if(ier/=nf90_noerr .or. nprofs==0) then
        call nc_check(nf90_close(ncid),&
            myname_,'close '//trim(dfile),stat=ier)
        return ! go to 170
     endif

     ! Continue the input
     call nc_check(nf90_inq_dimid(ncid, "nlevs", mlslevsDimId),&
         myname_,'inq_dimid nlevs '//trim(dfile))
     call nc_check(nf90_inquire_dimension(ncid, mlslevsDimId, len = mlslevs),&
         myname_,'inquire_dimension nlevs '//trim(dfile))

     !  NOTE: Make sure that 'ozinfo' has the same number of levels
     ! for NRT it is 55
     allocate(ipos(mlslevs))

     ikx = 0 
     do i=1,jpch_oz
        if(index(nusis_oz(i),'mls55')/=0) then  ! MLS .nc data
           ikx=ikx+1
           ipos(ikx)=i
        end if
     end do

     nmrecs=0
     ! Allocate space and read data

     allocate(iya(nprofs),ima(nprofs),idda(nprofs),ihha(nprofs),imina(nprofs), &
          iseca(nprofs),slatsa(nprofs),slonsa(nprofs), mlsozone(mlslevs,nprofs), &
          mlserr(mlslevs,nprofs),mlsqual(nprofs), mlsconv(nprofs), mlspress(mlslevs))

     ! Read variables and store them in these arrays
     call nc_check(nf90_inq_varid(ncid, "lon", lonVarId),&
         myname_,'inq_varid lon '//trim(dfile))
     call nc_check(nf90_get_var(ncid, lonVarId, slonsa),&
         myname_,'get_var lon '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "lat", latVarId),&
         myname_,'inq_varid lat '//trim(dfile))
     call nc_check(nf90_get_var(ncid, latVarId, slatsa),&
         myname_,'get_var lat '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "yy", yyVarId),&
         myname_,'inq_varid yy '//trim(dfile))
     call nc_check(nf90_get_var(ncid, yyVarId, iya),&
         myname_,'get_var yy '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "mm", mmVarId),&
         myname_,'inq_varid mm '//trim(dfile))
     call nc_check(nf90_get_var(ncid, mmVarId, ima),&
         myname_,'get_var mm '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "dd", ddVarId),&
         myname_,'inq_varid dd '//trim(dfile))
     call nc_check(nf90_get_var(ncid, ddVarId, idda),&
         myname_,'get_var dd '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "hh", hhVarId),&
         myname_,'inq_varid hh '//trim(dfile))
     call nc_check(nf90_get_var(ncid, hhVarId, ihha),&
         myname_,'get_var hh '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "min", minVarId),&
         myname_,'inq_varid min '//trim(dfile))
     call nc_check(nf90_get_var(ncid, minVarId, imina),&
         myname_,'get_var min '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "ss", ssVarId),&
         myname_,'inq_varid ss '//trim(dfile))
     call nc_check(nf90_get_var(ncid, ssVarId, iseca),&
         myname_,'get_var ss '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "press", pressVarId),&
         myname_,'inq_varid press '//trim(dfile))
     call nc_check(nf90_get_var(ncid, pressVarId, mlspress),&
         myname_,'get_var press '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "conv", convVarId),&
         myname_,'inq_varid conv '//trim(dfile))
     call nc_check(nf90_get_var(ncid, convVarId, mlsconv),&
         myname_,'get_var conv '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "qual", qualVarId),&
         myname_,'inq_varid qual '//trim(dfile))
     call nc_check(nf90_get_var(ncid, qualVarId, mlsqual),&
         myname_,'get_var qual '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "oberr", mlserrVarId),&
         myname_,'inq_varid oberr '//trim(dfile))
     call nc_check(nf90_get_var(ncid, mlserrVarId, mlserr),&
         myname_,'get_var oberr '//trim(dfile))

     call nc_check(nf90_inq_varid(ncid, "ozone", mlsozoneVarId),&
         myname_,'inq_varid ozone '//trim(dfile))
     call nc_check(nf90_get_var(ncid, mlsozoneVarId, mlsozone),&
         myname_,'get_var ozone '//trim(dfile))

     ! close the data file
     call nc_check(nf90_close(ncid),&
         myname_,'close '//trim(dfile))
     
     ! 'Unpack' the data
     nmrecs = 0
     nodata = 0
     do iprof = 1, nprofs
        do ilev = 1, mlslevs
           ! note that most of the quality control is done at the 
           ! pre-processing stage
           if (mlspress(ilev) .gt. 262.0 .or. mlspress(ilev) .lt. 0.1 ) cycle ! goto 145
           if (mlsozone(ilev, iprof) .lt. -900.0) cycle ! goto 145 ! undefined
           if (mlserr(ilev, iprof) .lt. -900.0) cycle ! goto 145 ! undefined
!           if (ndata >= maxobs) then 
!              write(6,*) ' read_ozone:   Number of MLS obs reached maxobs = ', &
!                   maxobs
!              return ! goto 150
!           endif
           if (iuse_oz(ipos(ilev)) < 0) then
              usage = 100._r_kind
           else
              usage = zero
           endif
           nmrecs=nmrecs+1

           !       convert observation location to radians
           slons0=slonsa(iprof)
           slats0=slatsa(iprof)
           if(abs(slats0)>90._r_kind .or. abs(slons0)>r360) cycle
           if(slons0< zero) slons0=slons0+r360
           if(slons0==r360) slons0=zero
           dlat_earth = slats0 * deg2rad
           dlon_earth = slons0 * deg2rad
           
           if(regional)then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
              if(outside) cycle    
           else
              dlat = dlat_earth
              dlon = dlon_earth
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
             ! call grdcrd(dlat,1,rlats,nlat,1)
             ! call grdcrd(dlon,1,rlons,nlon,1)
           endif

           idate5(1) = iya(iprof) !year
           idate5(2) = ima(iprof) !month
           idate5(3) = idda(iprof) !day
           idate5(4) = ihha(iprof) !hour
           idate5(5) = imina(iprof) !minute
           call w3fs21(idate5,nmind)
           t4dv=real((nmind-iwinbgn),r_kind)*r60inv
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) then
                 write(6,*)'read_ozone: mls obs time idate5=',idate5,', t4dv=',&
                      t4dv,' is outside time window, sstime=',sstime*r60inv
                 cycle
              end if
           else
              sstime=real(nmind,r_kind)
              tdiff=(sstime-gstime)*r60inv
              if(abs(tdiff) > twind)then
                 write(6,*)'read_ozone: mls obs time idate5=',idate5,', tdiff=',&
                      tdiff,' is outside time window=',twind
                 cycle
              end if
           end if
         
           obserr = mlserr(ilev, iprof)
           ppmv = mlsozone(ilev, iprof)
           pres = mlspress(ilev) 
           pob = log(pres * one_tenth)
           
           ndata  = ndata+1
           if(ndata<=maxobs) then
              nodata = nodata + 1
              ozout(1,ndata)=rsat
              ozout(2,ndata)=t4dv
              ozout(3,ndata)=dlon                 ! grid relative longitude
              ozout(4,ndata)=dlat                 ! grid relative latitude
              ozout(5,ndata)=dlon_earth*rad2deg   ! earth relative longitude (degrees)
              ozout(6,ndata)=dlat_earth*rad2deg   ! earth relative latitude (degrees)
           
              ozout(7,ndata)=rmiss                ! used to be solar zenith angle
              ozout(8,ndata)=usage
              ozout(9,ndata)=pob                  ! pressure 
              ozout(10,ndata)=obserr              ! ozone mixing ratio precision in ppmv
              ozout(11,ndata)=float(ipos(ilev))   ! pointer of obs level index in ozinfo.txt
              ozout(12,ndata)=mlslevs             ! # of mls vertical levels
              ozout(13,ndata)=ppmv                ! ozone mixing ratio in ppmv
           endif
           
! 145        continue
        end do
     end do


     deallocate(iya,ima,idda,ihha,imina,iseca,slatsa,slonsa, mlsozone, &
          mlserr,mlsqual, mlsconv, mlspress)
     deallocate(ipos)

!     write(stdout,'(3a,3i8,f8.2)') mprefix('read_ozone'), &
!        ' obstype,nmrecs,ndata,nodata,no/ndata = ',dtype,nmrecs,ndata,nodata,real(nodata)/ndata

           if (ndata > maxobs) then 
              call perr('read_ozone','Number of MLS obs reached maxobs = ', maxobs)
              call perr(myname_,'Number of MLS obs reached maxobs = ', maxobs)
              call perr(myname_,'                           ndata = ', ndata)
              call perr(myname_,'                          nodata = ', nodata)
              call die(myname_)
           endif
  !---------------END MLS NRT NetCDF---------------------------
!!end subroutine read_mlsnc_
end subroutine ozlev_ncread_

subroutine ozlev_bufrInquire_(dfile,dtype,dplat, nreal,nchan,ilat,ilon,maxrec)
  implicit none
  character(len=*), intent(in):: dfile   ! obs_input filename
  character(len=*), intent(in):: dtype   ! observation type (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dplat   ! platform (see gsiparm.anl:&OBSINPUT/)

  integer(kind=i_kind), intent(out):: nreal  ! number of real parameters per record
  integer(kind=i_kind), intent(out):: nchan  ! number of channels or levels per record
  integer(kind=i_kind), intent(out):: ilat   ! index to latitude in nreal parameters.
  integer(kind=i_kind), intent(out):: ilon   ! index to longitude in nreal parameters.

  integer(kind=i_kind), intent(out):: maxrec    ! extimated input record count

  character(len=*), parameter:: myname_=myname//'::ozlev_bufrInquire_'

!    Configure the record, they are not (dfile,dtype,dplat) dependent in this case.
     nreal = 12
     nchan =  1
     ilat=4
     ilon=3

     maxrec = MAXOBS_
end subroutine ozlev_bufrInquire_

subroutine ozlev_bufrread_(dfile,dtype,dplat,dsis, ozout,nmrecs,ndata,nodata, &
                           jsatid, gstime,twind, nreal,nchan,ilat,ilon)

  use gridmod, only: nlat,nlon,regional,tll2xy,rlats,rlons
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen,l4densvar

  use constants, only: deg2rad,zero,rad2deg,r60inv
  use ozinfo, only: jpch_oz,nusis_oz,iuse_oz

  use mpeu_util, only: warn,tell
!  use mpeu_util, only: mprefix,stdout

  implicit none
  character(len=*), intent(in):: dfile   ! obs_input filename
  character(len=*), intent(in):: dtype   ! observation type (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dplat   ! platform (see gsiparm.anl:&OBSINPUT/)
  character(len=*), intent(in):: dsis    ! sensor/instrument/satellite tag (see gsiparm.anl:&OBSINPUT/ and ozinfo)

  real   (kind=r_kind), dimension(:,:), intent(out):: ozout
  integer(kind=i_kind), intent(out):: nmrecs ! count of actual records read
  integer(kind=i_kind), intent(out):: ndata  ! count of processed data
  integer(kind=i_kind), intent(out):: nodata ! count of retained data

  character(len=*)    , intent(in):: jsatid ! platform ID (verification)
  real   (kind=r_kind), intent(in):: gstime ! analysis time (minute) from reference date
  real   (kind=r_kind), intent(in):: twind  ! input group time window (hour)

  integer(kind=i_kind), intent(in):: nreal  ! number of real parameters per record
  integer(kind=i_kind), intent(in):: nchan  ! number of channels or levels per record
  integer(kind=i_kind), intent(in):: ilat   ! index to latitude in nreal parameters.
  integer(kind=i_kind), intent(in):: ilon   ! index to longitude in nreal parameters.


  character(len=*),parameter:: myname_=myname//'::ozlev_bufrread_'

  integer(kind=i_kind),parameter:: nloz =37
  integer(kind=i_kind),parameter:: lunin=10

  character(len=*), parameter:: mlstr  ='SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SECO SOZA CONV MLST PCCF'
  character(len=*), parameter:: mlstrl2='PRLC OZMX OZMP OZME'

  integer(kind=i_kind):: idate,jdate,ksatid,iy,iret,im,ihh,idd
  integer(kind=i_kind):: mpos
  character(len= 8):: subset

  integer(kind=i_kind):: maxobs
  integer(kind=i_kind):: nmind
  integer(kind=i_kind):: k
  integer(kind=i_kind):: kidsat
  integer(kind=i_kind):: idate5(5)
  integer(kind=i_kind):: ikx
  integer(kind=i_kind):: decimal,binary_mls(18)

  real(kind=r_kind):: tdiff,sstime,dlon,dlat,t4dv
  real(kind=r_kind):: slons0,slats0,rsat,dlat_earth,dlon_earth
  real(kind=r_double),dimension(13):: hdrmls
  real(kind=r_double),dimension(4,37):: hdrmlsl2
  real(kind=r_double):: hdrmls13
  real(kind=r_kind),allocatable,dimension(:):: mlspres,mlsoz,mlsozpc,usage1
  integer(kind=i_kind),allocatable,dimension(:):: ipos
  integer(kind=i_kind):: iprofs,nprofs
  logical:: outside

  maxobs=size(ozout,2)
  rsat=999._r_kind

     open(lunin,file=dfile,form='unformatted')

     call openbf(lunin,'IN',lunin)
     call datelen(10)
     call readmg(lunin,subset,idate,iret)

     ! If it has failed at the first read(), ...
     if (iret/=0 .or. subset /= 'GM008015') then
        call closbf(lunin)
        close(lunin)

        call warn(myname_,'Failed at reading BUFR file, dfile =',trim(dfile))
        call warn(myname_,'                             dtype =',trim(dtype))
        call warn(myname_,'                            jsatid =',trim(jsatid))
        call warn(myname_,'                             lunin =',lunin)
        call warn(myname_,'                              iret =',iret)
        call warn(myname_,'                            subset =',trim(subset))

        nprofs = 0
        nodata = 0

        return
     endif

     call tell(myname_,'MLS o3lev data type, subset =',trim(subset))
     write(6,*)'read_ozone:  GMAO MLS o3lev data type, subset=',subset

     !    Q: o3lev data has 37 levels. Then, why is size(ipos) of 44? - j.jin 
     !    A: Because there are 44 entries in ozinfo.txt at the time - j.guo

     mpos=max(nloz,jpch_oz)
     allocate (ipos(mpos))      ! 44? 37?
     allocate (usage1(nloz))

     nmrecs=0
     nprofs=0
     nodata=0

!    Set dependent variables and allocate arrays

     allocate (mlspres(nloz))
     allocate (mlsoz(nloz))
     allocate (mlsozpc(nloz))

     ikx=0
     do k=1,jpch_oz
        if(index(nusis_oz(k),'o3lev')/=0) then  ! all "o3lev" in ozinfo.txt
           ikx=ikx+1
           ipos(ikx)=k
        end if
     end do

     iy=0
     im=0
     idd=0
     ihh=0

!! This is the top of the profile loop
     ndata=0
     iprofs=0
139  continue
     call readsb(lunin,iret)
     if (iret/=0) then           !JJJ, end of the subset
        call readmg(lunin,subset,jdate,iret)  !JJJ  open a new mg
        if (iret/=0) goto 150    !JJJ, no more  mg,  EOF      
        goto 139
     endif

     do k=1,nloz
        if (iuse_oz(ipos(k)) < 0) then
           usage1(k) = 100._r_kind
        else
           usage1(k) = zero
        endif
     end do

!    extract header information
     call ufbint(lunin,hdrmls,13,1,iret,mlstr)
     rsat = hdrmls(1); ksatid=rsat

     if(jsatid == 'aura')kidsat = 785
     if (ksatid /= kidsat) go to 139

     nmrecs=nmrecs+nloz

!    Convert observation location to radians
     slats0= hdrmls(2)
     slons0= hdrmls(3)
     if(abs(slats0)>90._r_kind .or. abs(slons0)>r360) go to 139
     if(slons0< zero) slons0=slons0+r360
     if(slons0==r360) slons0=zero
     dlat_earth = slats0 * deg2rad
     dlon_earth = slons0 * deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if(outside) go to 139
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif

! convert observation time to relative time
     idate5(1) = hdrmls(4)  !year
     idate5(2) = hdrmls(5)  !month
     idate5(3) = hdrmls(6)  !day
     idate5(4) = hdrmls(7)  !hour
     idate5(5) = hdrmls(8)  !minute
     call w3fs21(idate5,nmind)

     t4dv=real((nmind-iwinbgn),r_kind)*r60inv
     if (l4dvar.or.l4densvar) then
        if (t4dv<zero .OR. t4dv>winlen) then
           write(6,*)'read_ozone: mls obs time idate5=',idate5,', t4dv=',&
              t4dv,' is outside time window, sstime=',sstime*r60inv
           go to 139
        endif
     else
        sstime=real(nmind,r_kind)
        tdiff=(sstime-gstime)*r60inv
        if (abs(tdiff) > twind) then
           write(6,*)'read_ozone: mls obs time idate5=',idate5,', tdiff=',&
              tdiff,' is outside time window=',twind
           go to 139
        endif
     end if

!    v2.2 data screening, only accept:
!    Pressure range:       215-0.02mb
!    Precision:            positive OZMP;    
!    Status flag:          only use even number
!    Quality(PCCF):        use >1.2 for data at 215-100mb & low latitude, 
!                          use >0.4 for data elsewhere
!    Convergence:          use <1.8

!    Bit 1 in MLST represents data should not be used
!    Note: in BUFR bits are defined from left to right as: 123456789...
!    whereas in HDF5 (and the nasa document) bits are defined from right to left as: ...876543210

     decimal=int(hdrmls(12))
     call dec2bin(decimal,binary_mls,18)
     if (binary_mls(1) == 1 ) goto 139

     if(hdrmls(11) >= 1.8_r_kind) go to 139

!    extract pressure, ozone mixing ratio and precision
     call ufbrep(lunin,hdrmlsl2,4,nloz,iret,mlstrl2)

     iprofs=iprofs+1    ! counting the profiles
     do k=1,nloz
        mlspres(k)=log(hdrmlsl2(1,k)*0.001_r_kind)    ! mls pressure in Pa, coverted to log(cb)
        mlsoz(k)=hdrmlsl2(2,k)                     ! ozone mixing ratio in ppmv
        mlsozpc(k)=hdrmlsl2(3,k)                   ! ozone mixing ratio precision in ppmv
        if (dsis /= 'mls_aura_ozpc') mlsozpc(k)=hdrmlsl2(4,k)   ! use obserr if (dsis /= 'mls_aura_ozpc')
     end do

     do k=1,nloz
        if(hdrmlsl2(1,k)>21600._r_kind .or. hdrmlsl2(1,k)<2._r_kind) usage1(k)=1000._r_kind
        if(hdrmlsl2(3,k)<=0._r_kind) usage1(k)=1000._r_kind
     end do

     hdrmls13=hdrmls(13)*0.1_r_kind
     if (abs(slats0)<30._r_kind) then
        do k=1,nloz
           if(hdrmlsl2(1,k)>10000._r_kind .and. hdrmlsl2(1,k)<21600._r_kind) then
              if(hdrmls13 <= 1.2_r_kind) usage1(k)=1000._r_kind
           else
              if(hdrmls13 <= 0.4_r_kind) usage1(k)=1000._r_kind
           endif
        end do
     else
        if(hdrmls13 <= 0.4_r_kind) then
           do k=1,nloz
              usage1(k)=1000._r_kind
           end do
        end if
     end if

     do k=1,nloz

        ndata=ndata+1

        if(ndata <= maxobs) then
           ozout( 1,ndata)=rsat
           ozout( 2,ndata)=t4dv
           ozout( 3,ndata)=dlon               ! grid relative longitude
           ozout( 4,ndata)=dlat               ! grid relative latitude
           ozout( 5,ndata)=dlon_earth*rad2deg ! earth relative longitude (degrees)
           ozout( 6,ndata)=dlat_earth*rad2deg ! earth relative latitude (degrees)
           ozout( 7,ndata)=hdrmls(10)         ! solar zenith angle

           ozout( 8,ndata)=usage1(k)          ! 
           ozout( 9,ndata)=mlspres(k)          ! mls pressure in log(cb)
           ozout(10,ndata)=mlsozpc(k)   ! ozone mixing ratio precision in ppmv
           ozout(11,ndata)=float(ipos(k))       ! pointer of obs level index in ozinfo.txt
           ozout(12,ndata)=nloz         ! # of mls vertical levels
           ozout(13,ndata)=mlsoz(k)     ! ozone mixing ratio in ppmv
        endif
     end do

     go to 139
150  continue   ! this is the bottom of the profile loop
!    End of o3lev bufr loop

     nodata=min(ndata,maxobs)   ! count of retained data
     if(nodata < ndata) then    ! warning if all are not properly retained
        call warn(myname_,'insufficient buffer space, expecting =',ndata)
        call warn(myname_,'                     actual retained =',nodata)
        call warn(myname_,'                       size(ozout,2) =',maxobs)
     endif

!     write(stdout,'(3a,3i8,f8.2)') mprefix('read_ozone'), &
!        ' obstype,nmrecs,ndata,nodata,no/ndata = ',dtype,nmrecs,ndata,nodata,real(nodata)/ndata

end subroutine ozlev_bufrread_

end module m_extOzone
