subroutine gesinfo
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gesinfo                  get information from model guess files
!   prgmmr: treadon          org: np23                date: 2006-01-10
!
! abstract: This subroutine gets date/time, vertical coordinate, and other
!           information from model guess file(s)
!
! program history log:
!   2006-01-10  treadon
!   2006-04-14  treadon - remove sigi,sigl; add ntracer,ncloud,ck5
!   2007-03-16  moorthi - replace gfsatm_head%ak,%bk with %vcoord
!   2007-05-08  kleist  - add capability to handle fully generalized coordinate
!   2008-06-04  safford - rm unused use one
!   2009-01-07  todling - add logics to determine begin/end of analysis
!   2009-01-28  todling - remove original GMAO interface
!   2009-10-09  wu      - replace nhr_offset with min_offset since it's 1.5 hr for regional
!   2010-03-31  treadon - move jcap_b to gridmod
!   2010-09-09  pagowski - add cmaq
!   2010-12-03  Huang   - add use_gfs_nemsio if input files is in NEMSIO format
!                         make use of nemsio_module to obtain header information including time
!                         vertical corrdiates, ...etc.
!   2011-08-01  lueken  - changed F90 to f90 (no machine logic)
!   2011-10-27  Huang   - (1) no gfshead%nvcoord info in NEMS header. Add code to determine the
!                             value of gfshead%nvcoord from gfsheadv%vcoord.
!                         (2) no idvm info in NEMS header. The output data type of temp and pres
!                             is fixed as dry temperature (in kelvin) and pressure (pascal).
!                             The read/write in ncepnems_io.f90 automatically recognizes this
!                             setting.  Therefore, when use_gfs_nemsio = .true.
!                             (1) remove idvm(5) and derivation of idpsfc5 and idthrm5
!                             (2) remove cpi, NEMSIO input always is dry tempersture (no
!                                 conversion from enthalpy w/ cpi is needed)
!   2017-05-12 Y. Wang and X. Wang - forecast length in minute unit is included in analysis time calculation
!                                    for subhourly DA, POC: xuguang.wang@ou.edu
!   2017-10-10  wu,w    - setup for FV3
!   2019-09-24  martin  - add use_gfs_ncio if input files are in netCDF format
!
!   input argument list:
!
!   comments:
!     The difference of time Info between operational GFS IO (gfshead%, sfc_head%),
!      analysis time (iadate), and NEMSIO (idate=)
!
!       gfshead & sfc_head            NEMSIO Header           Analysis time (obsmod)
!       ===================   ============================  ==========================
!         %idate(1)  Hour     idate(1)  Year                iadate(1)  Year
!         %idate(2)  Month    idate(2)  Month               iadate(2)  Month
!         %idate(3)  Day      idate(3)  Day                 iadate(3)  Day
!         %idate(4)  Year     idate(4)  Hour                iadate(4)  Hour
!                             idate(5)  Minute              iadate(5)  Minute
!                             idate(6)  Scaled seconds
!                             idate(7)  Seconds multiplier
!
!     The difference of header forecasting hour Info bewteen operational GFS IO
!      (gfshead%, sfc_head%) and NEMSIO
!
!           gfshead & sfc_head                NEMSIO Header
!       ==========================     ============================
!       %fhour  FCST Hour (r_kind)     nfhour     FCST Hour (i_kind)
!                                      nfminute   FCST Mins (i_kind)
!                                      nfsecondn  FCST Secs (i_kind) numerator
!                                      nfsecondd  FCST Secs (i_kind) denominator
!
!       %fhour = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
!                real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind,r_single
  use obsmod, only: iadate,ianldate,time_offset,iadatemn
  use gsi_4dvar, only: ibdate, iedate, iadatebgn, iadateend, iwinbgn,time_4dvar
  use gsi_4dvar, only: nhr_assimilation,min_offset
  use mpimod, only: npe,mype
  use gridmod, only: idvc5,ak5,bk5,ck5,tref5,&
      regional,nsig,regional_fhr,regional_time,fv3_regional,&
      wrf_nmm_regional,wrf_mass_regional,twodvar_regional,nems_nmmb_regional,cmaq_regional,&
      ntracer,ncloud,idvm5,&
      ncepgfs_head,ncepgfs_headv,idpsfc5,idthrm5,idsl5,cp5,jcap_b, use_gfs_nemsio, &
      regional_fmin, use_gfs_ncio
  use sigio_module, only: sigio_head,sigio_srhead,sigio_sclose,&
      sigio_sropen
  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_getheadvar
  use module_ncio, only: Dimension, Dataset, open_dataset, get_dim, &
                                read_vardata, get_idate_from_time_units,&
                                read_attribute, close_dataset 

  use constants, only: zero,h300,r60,r3600,i_missing

  use gsi_rfv3io_mod, only: read_fv3_files
  use read_wrf_mass_files_mod, only: read_wrf_mass_files_class
  use read_wrf_nmm_files_mod, only: read_wrf_nmm_files_class
  use gsi_io, only: verbose
  implicit none

! Declare passed variables

! Declare local parameters
  integer(i_kind),parameter:: lunges=11
  real(r_kind),parameter::  zero_001=0.001_r_kind
  type(read_wrf_nmm_files_class):: wrf_nmm_files
  type(read_wrf_mass_files_class):: wrf_mass_files

! Declare local variables

  logical fexist
  character(6) filename,sfilename
  character(8) filetype, mdlname

  integer(i_kind) iyr,ihourg,k,kr
  integer(i_kind) mype_out,iret,iret2,intype
  integer(i_kind),dimension(5):: idate4
  integer(i_kind),dimension(8):: ida,jda
  integer(i_kind) :: nmin_an
  integer(i_kind),dimension(7):: idate
  integer(i_kind),dimension(6):: idate2
  integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
  integer(i_kind),allocatable,dimension(:) :: ntrac,ncld

  real(r_kind) hourg, minuteg
  real(r_kind),dimension(5) :: fha
  real(r_single),allocatable,dimension(:,:,:) :: nems_vcoord
  real(r_single),allocatable,dimension(:) :: aknc, bknc, fhour

  type(sigio_head):: sighead
  type(ncepgfs_head):: gfshead
  type(ncepgfs_headv):: gfsheadv
  type(nemsio_gfile) :: gfile2
  type(Dataset) :: atmges,sfcges
  type(Dimension) :: ncdim
  logical :: print_verbose
  logical :: fatal = .false.

!---------------------------------------------------------------------
! Get guess date and vertical coordinate structure from atmospheric
! guess file

  mype_out=npe/2


  print_verbose=.false.
  if(verbose)print_verbose=.true.
! Handle non-GMAO interface (ie, NCEP interface)
  if(.not. fv3_regional) then
     write(filename,'("sigf",i2.2)')nhr_assimilation
     inquire(file=filename,exist=fexist)
     if(.not.fexist) then
        write(6,*)' GESINFO:  ***FATAL ERROR*** ',trim(filename),' NOT AVAILABLE: PROGRAM STOPS'
        call stop2(99)
        stop
     end if
  end if

! Handle NCEP regional case
  if(regional) then
     idate4(1)=regional_time(4)  !  hour
     idate4(2)=regional_time(2)  !  month
     idate4(3)=regional_time(3)  !  day
     idate4(4)=regional_time(1)  !  year
     idate4(5)=regional_time(5)  ! minutes
     hourg=regional_fhr          !  fcst hour
     minuteg=regional_fmin       !  fcst minute
! Handle RURTMA date:  get iadatemn
     iadatemn(1)=regional_time(1)  !  year
     iadatemn(2)=regional_time(2)  !  month
     iadatemn(3)=regional_time(3)  !  day
     iadatemn(4)=regional_time(4)  !  hour
     iadatemn(5)=regional_time(5)  !  minute
     if(print_verbose)write (6,*) 'in gesinfo: iadatemn with minutes', iadatemn
! Handle NCEP global cases
  else

!    Determine NCEP atmospheric guess file format
     intype = 0
     if ( (.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio) ) then

        call sigio_sropen(lunges,filename,iret)
        call sigio_srhead(lunges,sighead,iret2)
        if (iret/=0 .or. iret2/=0) then
           write(6,*)' GESINFO:  UNKNOWN FORMAT FOR NCEP ATM GUESS FILE ',filename
           call stop2(99)
           stop
        endif
        if (mype==mype_out) &
             write(6,*)'GESINFO:  Read NCEP sigio format file, ',filename

!       Extract information from NCEP atmospheric guess using sigio
!       Fill structure with NCEP sigio header information
        gfshead%fhour=sighead%fhour
        gfshead%idate=sighead%idate
        gfshead%latb=sighead%latb
        gfshead%lonb=sighead%lonb
        gfshead%levs=sighead%levs
        gfshead%jcap=sighead%jcap
        gfshead%ntrac=sighead%ntrac
        gfshead%idvc=sighead%idvc
        gfshead%idvm=sighead%idvm
        gfshead%idsl=sighead%idsl
        gfshead%ncldt=sighead%ncldt
        gfshead%nvcoord=sighead%nvcoord

        allocate(gfsheadv%vcoord(gfshead%levs+1,gfshead%nvcoord))
        gfsheadv%vcoord=sighead%vcoord

        allocate(gfsheadv%cpi(gfshead%ntrac+1))
        if (mod(gfshead%idvm/10,10) == 3) then
           do k=1,gfshead%ntrac+1
              gfsheadv%cpi(k)=sighead%cpi(k)
           end do
        else
           do k=1,gfshead%ntrac+1
              gfsheadv%cpi(k)=zero
           end do
        endif

        call sigio_sclose(lunges,iret)

!          Check for consistency:  jcap, levs
        if (gfshead%jcap/=jcap_b .or. gfshead%levs/=nsig) then
           write(6,*)'GESINFO:  ***ERROR*** sigio (jcap_b,levs)=',&
                gfshead%jcap,gfshead%levs, ' do not equal ',&
                ' user (jcap_b,nsig)=',jcap_b,nsig
           call stop2(85)
        endif


!    Extract information from NCEP atmospheric guess using NEMSIO
     else if ( use_gfs_nemsio ) then
        call nemsio_init(iret=iret2)
        if ( iret2 /= 0 ) then
           write(6,*)' GESINFO:  ***ERROR*** problem nemsio_init file = ', &
              trim(filename),', Status = ',iret2
           call stop2(99)
        end if
        call nemsio_open(gfile2,filename,'READ',iret=iret2)
        if ( iret2 /= 0 ) then
           write(6,*)' GESINFO:  ***ERROR*** problem opening file = ', &
              trim(filename),', Status = ',iret2
           call stop2(99)
        end if

        idate         = i_missing
        nfhour        = i_missing
        nfminute      = i_missing
        nfsecondn     = i_missing
        nfsecondd     = i_missing
        gfshead%idsl  = i_missing
        call nemsio_getfilehead(gfile2, idate=idate, gtype=filetype,  &
           modelname=mdlname, nfhour=nfhour, nfminute=nfminute,       &
           nfsecondn=nfsecondn, nfsecondd=nfsecondd,                  &
           dimx=gfshead%lonb, dimy=gfshead%latb,   dimz=gfshead%levs, &
           jcap=gfshead%jcap, ntrac=gfshead%ntrac, idvc=gfshead%idvc, &
           idsl=gfshead%idsl,   ncldt=gfshead%ncldt, iret=iret2)

        if ( iret2 /= 0 .or. TRIM(filetype) /= 'NEMSIO' ) then
           write(6,*)' GESINFO:  UNKNOWN FORMAT FOR GFSATM file = ', &
              trim(filename),' Status = ',iret2
           write(6,*)' GESINFO:  reding filetype = ',trim(filetype), &
              ' modelname = ', trim(mdlname)
           call stop2(99)
        else
           if (mype==mype_out) write(6,*)'GESINFO:  Read NCEP nemsio ', &
              'format file, ',trim(filename), ' model name = ', trim(mdlname)
        endif

!       Extract vertical coordinate descriptions nems_vcoord.
!       nems_vcoord(gfshead%levs+1,3,2) dimension is hardwired here.
!       Present NEMSIO modules do not allow flexibility of 2nd and 3rd
!       array dimension for nems_vcoord, for now, it is hardwired as
!       (levs,3,2) If NEMS changes the setting of vcoord dimension,
!       GSI needs to update its setting of nems_vcoord accordingly.

        if (allocated(nems_vcoord))     deallocate(nems_vcoord)
        allocate(nems_vcoord(gfshead%levs+1,3,2))
        call nemsio_getfilehead(gfile2,iret=iret2,vcoord=nems_vcoord)
        if ( iret2 /= 0 ) then
           write(6,*)' GESINFO:  ***ERROR*** problem reading header ', &
              'vcoord, Status = ',iret2
           call stop2(99)
        endif

!       Determine the type of vertical coordinate used by model because that
!       gfshead%nvcoord is no longer part of NEMSIO header output.
        gfshead%nvcoord=3
        if(maxval(nems_vcoord(:,3,1))==zero .and. &
           minval(nems_vcoord(:,3,1))==zero ) then
           gfshead%nvcoord=2
           if(maxval(nems_vcoord(:,2,1))==zero .and. &
              minval(nems_vcoord(:,2,1))==zero ) then
              gfshead%nvcoord=1
           end if
        end if
        if ( gfshead%idsl==i_missing .or. gfshead%idsl < 1 ) then
           gfshead%idsl=1
           if ( gfshead%nvcoord == 3 ) gfshead%idsl=2
        end if
!
        if (allocated(gfsheadv%vcoord)) deallocate(gfsheadv%vcoord)
        allocate(gfsheadv%vcoord(gfshead%levs+1,gfshead%nvcoord))
        gfsheadv%vcoord(:,1:gfshead%nvcoord) = nems_vcoord(:,1:gfshead%nvcoord,1)
!
!       obtain gfs%head time info from readin nemsio header info (w/ different
!       definition and variables)
        if ( nfhour == i_missing .or. nfminute == i_missing .or. &
             nfsecondn == i_missing .or. nfsecondd == i_missing ) then
           write(6,*)'GESINFO:  ***ERROR*** some forecast hour info are not ', &
              'defined in ', trim(filename)
           write(6,*)'       :  nfhour, nfminute, nfsecondn, and nfsecondd = ', &
              nfhour, nfminute, nfsecondn, nfsecondd
           call stop2(99)
        endif
        gfshead%fhour = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
                        real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600

        gfshead%idate(1) = idate(4)  !hour
        gfshead%idate(3) = idate(3)  !day
        gfshead%idate(2) = idate(2)  !month
        gfshead%idate(4) = idate(1)  !year

        call nemsio_close(gfile2,iret=iret2)
        if ( iret2 /= 0 ) then
           write(6,*)'GESINFO:  problem closing file = ',trim(filename),', Status = ',iret2
           call stop2(99)
        end if

!       Check for consistency:  levs, latb,lonb
!       if (gfshead%latb+2/=nlat .or. gfshead%lonb/=nlon .or. &
!           gfshead%levs/=nsig ) then
!          write(6,*)'GESINFO:  ***ERROR*** gfshead (latb+2,lonb,levs)=',&
!             gfshead%latb+2,gfshead%lonb,gfshead%levs, ' do not equal ',&
!             ' user (nlat,nlon,nsig)=',nlat,nlon,nsig
!          call stop2(99)
!       endif
     else ! use_gfs_ncio and get this information
        write(sfilename,'("sfcf",i2.2)')nhr_assimilation
        ! open the netCDF file
        atmges = open_dataset(filename,errcode=iret)
        if (iret /=0) then
           write(6,*)'GESINFO:  ***FATAL ERROR*** ',trim(filename),' NOT AVAILABLE: PROGRAM STOPS'
           call stop2(99)
        endif
        sfcges = open_dataset(sfilename,errcode=iret)
        if (iret /=0) then
           write(6,*)'GESINFO:  ***FATAL ERROR*** ',trim(sfilename),' NOT AVAILABLE: PROGRAM STOPS'
           call stop2(99)
        endif
        ! get dimension sizes
        ncdim = get_dim(atmges, 'grid_xt'); gfshead%lonb = ncdim%len
        ncdim = get_dim(atmges, 'grid_yt'); gfshead%latb = ncdim%len
        ncdim = get_dim(atmges, 'pfull') ; gfshead%levs = ncdim%len 
        ! hard code jcap,idsl,idvc 
        gfshead%jcap = -9999
        gfshead%idsl= 1
        gfshead%idvc = 2
        call read_attribute(atmges, 'ncnsto', ntrac)
        gfshead%ntrac = ntrac(1)
        call read_attribute(sfcges, 'ncld', ncld)
        gfshead%ncldt = ncld(1)
        call close_dataset(sfcges)
        if (mype==mype_out) write(6,*)'GESINFO:  Read NCEP FV3GFS netCDF ', &
           'format file, ',trim(filename)
        ! hard code nvcoord to be 2
        gfshead%nvcoord=2 ! ak and bk
        if (allocated(gfsheadv%vcoord)) deallocate(gfsheadv%vcoord)
        allocate(gfsheadv%vcoord(gfshead%levs+1,gfshead%nvcoord))
        call read_attribute(atmges, 'ak', aknc)
        call read_attribute(atmges, 'bk', bknc)
        do k=1,gfshead%levs+1
           kr = gfshead%levs+2-k
           gfsheadv%vcoord(k,1) = aknc(kr)
           gfsheadv%vcoord(k,2) = bknc(kr)
        end do
        deallocate(aknc,bknc)

        ! get time information
        idate2 = get_idate_from_time_units(atmges)
        gfshead%idate(1) = idate2(4)  !hour
        gfshead%idate(2) = idate2(2)  !month
        gfshead%idate(3) = idate2(3)  !day
        gfshead%idate(4) = idate2(1)  !year
        call read_vardata(atmges, 'time', fhour) ! might need to change this to attribute later
                                               ! depends on model changes from Jeff Whitaker
        gfshead%fhour = fhour(1)

        call close_dataset(atmges)

     endif

!    Extract header information
     hourg    = gfshead%fhour
     idate4(1)= gfshead%idate(1)
     idate4(2)= gfshead%idate(2)
     idate4(3)= gfshead%idate(3)
     idate4(4)= gfshead%idate(4)
     idate4(5)= zero
     ntracer  = gfshead%ntrac
     ncloud   = gfshead%ncldt


!    Load vertical coordinate structure
     idvc5=gfshead%idvc
     idsl5=gfshead%idsl
     do k=1,nsig+1
        ak5(k)=zero
        bk5(k)=zero
        ck5(k)=zero
     end do

     if (gfshead%nvcoord == 1) then
        do k=1,nsig+1
           bk5(k) = gfsheadv%vcoord(k,1)
        end do
     elseif (gfshead%nvcoord == 2) then
        do k = 1,nsig+1
           ak5(k) = gfsheadv%vcoord(k,1)*zero_001
           bk5(k) = gfsheadv%vcoord(k,2)
        end do
     elseif (gfshead%nvcoord == 3) then
        do k = 1,nsig+1
           ak5(k) = gfsheadv%vcoord(k,1)*zero_001
           bk5(k) = gfsheadv%vcoord(k,2)
           ck5(k) = gfsheadv%vcoord(k,3)*zero_001
        end do
     else
        write(6,*)'GESINFO:  ***ERROR*** INVALID value for nvcoord=',gfshead%nvcoord
        call stop2(85)
     endif

!    Load reference temperature array (used by general coordinate)
     do k=1,nsig
        tref5(k)=h300
     end do

     if ( (.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio) ) then
!       Load surface pressure and thermodynamic variable ids
        idvm5   = gfshead%idvm
        idpsfc5 = mod ( gfshead%idvm,10 )
        idthrm5 = mod ( gfshead%idvm/10,10 )

!       Load specific heat for tracers
        if (allocated(cp5)) deallocate(cp5)
        allocate(cp5(gfshead%ntrac+1))
        do k=1,gfshead%ntrac+1
           cp5(k)=gfsheadv%cpi(k)
        end do
     end if

!    Check for consistency with namelist settings
     if (gfshead%jcap/=jcap_b.and..not.regional .or. gfshead%levs/=nsig) then
        if (gfshead%levs/=nsig) then
           write(6,*)'GESINFO:  ***FATAL ERROR*** guess levels inconsistent with namelist'
           write(6,*)'      guess nsig=',gfshead%levs
           write(6,*)'   namelist nsig=',nsig
           fatal = .true.
        endif
        if (gfshead%jcap/=jcap_b.and..not.regional ) then
           if (gfshead%jcap < 0) then
              ! FV3GFS write component does not write JCAP to the NEMSIO file
              if ( mype == mype_out ) then
                 write(6,*)'GESINFO:  ***WARNING*** guess jcap inconsistent with namelist'
                 write(6,*)'GESINFO:  ***WARNING*** this is a FV3GFS NEMSIO/NetCDF file'
              endif
              fatal = .false.
           else
              if ( mype == mype_out ) &
                 write(6,*)'GESINFO:  ***FATAL ERROR*** guess jcap inconsistent with namelist'
              fatal = .true.
           endif
           if ( mype == mype_out ) &
              write(6,*)'GESINFO:  guess jcap_b, namelist jcap_b = ',gfshead%jcap, jcap_b
        endif
        if ( fatal ) call stop2(85)
     endif


!    Echo select header information to stdout
     if(mype==mype_out .and. print_verbose) then
        if ( (.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio) ) then
           write(6,100) gfshead%jcap,gfshead%levs,gfshead%latb,gfshead%lonb,&
                gfshead%ntrac,gfshead%ncldt,idvc5,gfshead%nvcoord,&
                idvm5,idsl5,idpsfc5,idthrm5
100        format('GESINFO:  jcap_b=',i4,', levs=',i3,', latb=',i5,&
                ', lonb=',i5,', ntrac=',i3,', ncldt=',i3,', idvc=',i3,&
                ', nvcoord=',i3,', idvm=',i3,', idsl=',i3,', idpsfc=',i3,&
                ', idthrm=',i3)
        else
           write(6,200) gfshead%jcap,gfshead%levs,gfshead%latb,gfshead%lonb,&
                gfshead%ntrac,gfshead%ncldt,idvc5,gfshead%nvcoord,idsl5
200        format('GESINFO:  jcap_b=',i5,', levs=',i3,', latb=',i5,&
                ', lonb=',i5,', ntrac=',i3,', ncldt=',i3,', idvc=',i3,&
                ', nvcoord=',i3,', idsl=',i3)
        end if
        do k=1,nsig
           write(6,110) k,ak5(k),bk5(k),ck5(k),tref5(k)
        end do
        k=nsig+1
        write(6,110) k,ak5(k),bk5(k),ck5(k)
110     format(3x,'k,ak,bk,ck,tref=',i3,1x,4(g19.12,1x))
     endif



! End of NCEP global block
  endif


! Compute grid latitude, longitude, factors, and weights.
  call gengrid_vars



! Compute analysis time from guess date and forecast length.
  iyr=idate4(4)
  ihourg=hourg
  if(iyr>=0.and.iyr<=99) then
     if(iyr>51) then
        iyr=iyr+1900
     else
        iyr=iyr+2000
     end if
  end if
  fha=zero; ida=0; jda=0
  fha(2)=ihourg    ! relative time interval in hours
#ifdef RR_CLOUDANALYSIS
  fha(3)=minuteg   ! relative time interval in minutes
#endif
  ida(1)=iyr       ! year
  ida(2)=idate4(2) ! month
  ida(3)=idate4(3) ! day
  ida(4)=0         ! time zone
  ida(5)=idate4(1) ! hour
#ifdef RR_CLOUDANALYSIS
  ida(6)=idate4(5) ! minute
#endif
  call w3movdat(fha,ida,jda)
  iadate(1)=jda(1) ! year
  iadate(2)=jda(2) ! mon
  iadate(3)=jda(3) ! day
  iadate(4)=jda(5) ! hour
#ifdef RR_CLOUDANALYSIS
  iadate(5)=jda(6) !regional_time(5)      ! minute
#else
  iadate(5)=0      ! minute
#endif
  ianldate =jda(1)*1000000+jda(2)*10000+jda(3)*100+jda(5)

! Determine date and time at start of assimilation window
  ida(:)=0
  jda(:)=0
  fha(:)=zero
  fha(2)=-real(int(min_offset/60),r_kind)
  fha(3)=-(min_offset+fha(2)*r60)
  ida(1:3)=iadate(1:3)
  ida(5:6)=iadate(4:5)
  call w3movdat(fha,ida,jda)

  ibdate(1:5)=(/jda(1),jda(2),jda(3),jda(5),jda(6)/)
  iadatebgn=jda(1)*1000000+jda(2)*10000+jda(3)*100+jda(5)

! Set the analysis time - this is output info...
! w3fs21(NCEP-w3) converts analysis time to minutes relative to a fixed date.
  call w3fs21(ibdate,nmin_an)
  iwinbgn = nmin_an

! Determine date and time at end of assimilation window
  ida(:)=jda(:)
  jda(:)=0
  fha(:)=zero
  if ( min_offset == 0 ) then
     fha(2)=zero
  else
     fha(2)=nhr_assimilation
  endif
  call w3movdat(fha,ida,jda)

  iedate(1:5)=(/jda(1),jda(2),jda(3),jda(5),jda(6)/)
  iadateend=jda(1)*1000000+jda(2)*10000+jda(3)*100+jda(5)

! Get time offset
  call time_4dvar(ianldate,time_offset)
#ifdef RR_CLOUDANALYSIS
  fha(2)=real(int(min_offset/60),r_kind)
  fha(3)=(min_offset-fha(2)*r60)
  time_offset=time_offset+fha(3)/r60
#endif

! Get information about date/time and number of guess files
  if (regional) then
     if(wrf_nmm_regional) then
        call wrf_nmm_files%read_wrf_nmm_files(mype)
     else if(nems_nmmb_regional) then
        call wrf_nmm_files%read_nems_nmmb_files(mype)
     else if(wrf_mass_regional) then
        call wrf_mass_files%read_wrf_mass_files(mype)
     else if(fv3_regional) then
        call read_fv3_files(mype)
     else if(twodvar_regional) then
        call read_2d_files(mype)
     else if(cmaq_regional) then
        call read_cmaq_files(mype)
     end if
  else
     call read_files(mype)
  endif


  if(mype==mype_out) then
     if (twodvar_regional) then
        write(6,*)'GESINFO: 2dvar-Guess-date is',regional_time
        write(6,*)'GESINFO: Analysis date with minute: ',iadatemn
     else
        write(6,*)'GESINFO:  Guess    date is ',idate4,hourg
        write(6,*)'GESINFO:  Analysis date is ',iadate,ianldate,time_offset
     endif
  endif

  if (allocated(nems_vcoord))     deallocate(nems_vcoord)
  if (allocated(gfsheadv%vcoord)) deallocate(gfsheadv%vcoord)
  if (allocated(gfsheadv%cpi))    deallocate(gfsheadv%cpi)
  if (allocated(cp5)) deallocate(cp5)

  return
end subroutine gesinfo
