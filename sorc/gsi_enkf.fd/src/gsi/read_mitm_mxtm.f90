subroutine read_mitm_mxtm(nread,ndata,nodata,infile,obstype,lunout,gstime,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_mitm_mxtm.f90
!   prgmmr: pondeca                               date: 2015-08-05
!
! abstract:  read in min/maxT obs from a text file
!
! program history log:
!   2015-08-05 pondeca 
!   2016-03-11 j. guo   - Fixed {dlat,dlon}_earth_deg in the obs data stream
!
!   input argument list:
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of obs read 
!     ndata    - number of obs retained for further processing
!     nodata   - number of obs retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f95/2003
!   machine:  WCOSS
!
!$$$

  use kinds, only: r_single,r_kind,r_double,i_kind
  use constants, only: zero,one_tenth,one,deg2rad,half,&
      three,four,r60inv,r10,r100,r2000,t0c

  use convinfo, only: nconvtype, &
      icuse,ictype,ioctype
  use gridmod, only: regional,nlon,nlat,tll2xy,txy2ll,&
      rlats,rlons,twodvar_regional
  use deter_sfc_mod, only: deter_sfc2
  use obsmod, only: ianldate,bmiss,hilbert_curve
  use gsi_4dvar, only: iwinbgn,time_4dvar
  use sfcobsqc,only: init_rjlists,get_usagerj,destroy_rjlists
  use ndfdgrids,only: init_ndfdgrid,destroy_ndfdgrid,relocsfcob,adjust_error
  use hilbertcurve,only: init_hilbertcurve, accum_hilbertcurve, &
                         apply_hilbertcurve,destroy_hilbertcurve
  use mpimod, only:npe
  use gsi_io, only: verbose
  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=20)                     ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  real(r_kind)                          ,intent(in   ) :: gstime
  integer(i_kind),dimension(npe),intent(inout) :: nobs

! Declare local parameters
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r0_1_bmiss=one_tenth*bmiss
  real(r_kind),parameter:: r0_01_bmiss=r0_01*bmiss
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: r6= 6.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_single),parameter:: rmiss=9999.
  real(r_single),parameter:: r0_1_rmiss=one_tenth*rmiss
  character(8),parameter:: cspval= '88888888'

! Declare local variables
  character(len=14) :: myname
  character(len=8) :: c_station_id,c_prvstg,c_sprvstg


  integer(i_kind) :: nreal,i,lunin
  integer(i_kind) cat,mxtmqm,mitmqm
  integer(i_kind) :: kx,idomsfc
  integer(i_kind) :: nc,k,ilat,ilon,nchanl
  integer(i_kind) :: idate,iout,maxobs,icount,ierr
  real(r_kind) :: dlat,dlon,dlat_earth,dlon_earth,toff,t4dv
  real(r_kind) :: dlat_earth_deg,dlon_earth_deg
  real(r_kind) :: rminobs,pob,dlnpob,obval
  real(r_kind) :: stnelev
  real(r_kind) :: usage,tsavg,ff10,sfcr,zz
  real(r_kind) :: mxtmoe,mitmoe,oberr,qtflg
  real(r_kind),allocatable,dimension(:,:):: cdata_all

  integer(i_kind) :: ikx(100:199) !order number of report type in convinfo file
  integer(i_kind) :: kxall(100:199)
  integer(i_kind) :: kx_min, kx_max
  integer(i_kind) :: itimeshift, thisobtype_usage
  integer(i_kind) :: invalidkx, invalidob, n_outside
  real(r_single)  :: rkx,tank,rlon4,rlat4,stnelev4,obval4
  real(r_single)  :: maxtmint_oberrors(100:199)

  real(r_double) :: udbl,vdbl
  logical mxtmob,mitmob
  logical :: outside
  logical lhilbert
  logical  linvalidkx, linvalidob
  logical  fexist
  logical print_verbose

  real(r_double) :: rstation_id
  real(r_double) :: r_prvstg
  real(r_double) :: r_sprvstg


!  equivalence to handle character names
  equivalence(rstation_id,c_station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)

  namelist/oberrors_for_maxtmint/maxtmint_oberrors

! data statements
! data (maxtmint_oberrors(k),k=100,170) / 0.10000E+10_r_single /
  data maxtmint_oberrors(100:179) / 80*0.10000E+10_r_single /
  data maxtmint_oberrors(180)     / 1._r_single /
  data maxtmint_oberrors(181)     / 1._r_single /
  data maxtmint_oberrors(182)     / 1._r_single /
  data maxtmint_oberrors(183)     / 1.2_r_single /
  data maxtmint_oberrors(184)     / 4.0_r_single /
  data maxtmint_oberrors(185)     / 4.0_r_single /
  data maxtmint_oberrors(186)     / 4.0_r_single /
  data maxtmint_oberrors(187)     / 1._r_single /
  data maxtmint_oberrors(188)     / 1.2_r_single /
  data maxtmint_oberrors(189)     / 4.0_r_single /
  data maxtmint_oberrors(190)     / 0.10000E+10_r_single /
  data maxtmint_oberrors(191)     / 0.10000E+10_r_single /
  data maxtmint_oberrors(192)     / 1._r_single /
  data maxtmint_oberrors(193)     / 1._r_single /
  data maxtmint_oberrors(194)     / 1.2_r_single /
  data maxtmint_oberrors(195)     / 1.2_r_single /
  data maxtmint_oberrors(196)     / 1.2_r_single /
  data maxtmint_oberrors(197)     / 1.2_r_single /
  data maxtmint_oberrors(198)     / 1.2_r_single /
  data maxtmint_oberrors(199)     / 1.2_r_single /

  print_verbose=.false.
  if(verbose)print_verbose=.true.
  mxtmob = obstype == 'mxtm'
  mitmob = obstype == 'mitm'

  lunin=11_i_kind
  myname='READ_MITM_MXTM'
  nreal=24

  nchanl=0
  ilon=2
  ilat=3

  ikx=-1
  nc=0
  conv: do i=1,nconvtype
     if(trim(obstype) == trim(ioctype(i))) then
        nc=nc+1
        ikx(ictype(i))=i
     end if
  end do conv

  if(nc > 0)then
     write(6,*) myname,' found ',nc, ' matching obstypes in convinfo. proceed  with ob processing'
   else
     write(6,*) myname,' no matching obstype found in convinfo ',obstype
     return
  end if


  ! Try opening text file, if unable print error to the screen
  !  and return to read_obs.F90
  open(lunin,file=trim(infile),form='formatted',iostat=ierr)
  if (ierr/=0) then
     write(6,*)myname,':ERROR: Trouble opening input file: ',trim(infile),' returning to read_obs.F90...'
     return
  end if

90 format(3(A8,3X),3(F8.3,3X),F8.3,3X,F8.3,3X,I3,3X,F8.3)

  ! Find number of reports
  maxobs = 0
  readloop: do 
      read(lunin,90,end=101) c_station_id,c_prvstg,c_sprvstg, & 
          rkx,tank,rlat4,rlon4,stnelev4,itimeshift,obval4
      maxobs=maxobs+1
   end do readloop
101 continue
   if(print_verbose)write(6,*)myname,': maxobs=',maxobs

  if (maxobs == 0) then
     write(6,*)myname,': No reports found.  returning to read_obs.F90...'
     return
  end if

  lhilbert = twodvar_regional .and. hilbert_curve

  call init_rjlists
  if (lhilbert) call init_hilbertcurve(maxobs)
  if (twodvar_regional) call init_ndfdgrid

  if (twodvar_regional) then
     kx_min=180
     kx_max=199 
   else
     kx_min=100
     kx_max=199 
  endif

  inquire(file='oberrors_for_maxtmint_input',exist=fexist)
  if(fexist) then
    open (55,file='oberrors_for_maxtmint_input',form='formatted')
    read (55,oberrors_for_maxtmint)
    close(55)
  endif

  if(print_verbose)then
     write(6,*)myname,': ------ observation errors ------'
     do k=kx_min,kx_max
        write(6,*) 'maxtmint_oberrors(',k,')=',maxtmint_oberrors(k)
     enddo
     write(6,*)myname,': --------------------------------'
  end if

  allocate(cdata_all(nreal,maxobs))

  kxall=0
  iout=0
  invalidkx=0
  invalidob=0
  n_outside=0
  rewind(lunin)
  loop_readobs: do icount=1,maxobs
      read(lunin,90) c_station_id,c_prvstg,c_sprvstg, & 
          rkx,tank,rlat4,rlon4,stnelev4,itimeshift,obval4

      kx=int(rkx)

      if (kx < kx_min .or. kx > kx_max) then
         linvalidkx=.true.
         invalidkx=invalidkx+1
         write(6,*)myname,': Invalid report type: icount,kx ',icount,kx
        else
         linvalidkx=.false.
      endif

      if (obval4 > r0_1_rmiss) then
         linvalidob=.true.
         invalidob=invalidob+1
         write(6,*)myname,': Invalid ob value: icount,obval4 ',icount,obval4
        else
         linvalidob=.false.
      endif

      dlat_earth_deg=real(rlat4,kind=r_kind)
      dlon_earth_deg=real(rlon4,kind=r_kind)
      stnelev=real(stnelev4,kind=r_kind)

      if(abs(dlat_earth_deg)>r90 .or. abs(dlon_earth_deg)>r360) cycle loop_readobs
      if (dlon_earth_deg == r360) dlon_earth_deg=dlon_earth_deg-r360
      if (dlon_earth_deg < zero)  dlon_earth_deg=dlon_earth_deg+r360
      dlon_earth=dlon_earth_deg*deg2rad
      dlat_earth=dlat_earth_deg*deg2rad

      outside=.false. !need this on account of global gsi
      if(regional)then
         call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
         if(outside) n_outside=n_outside+1
      else
         dlat = dlat_earth
         dlon = dlon_earth
         call grdcrd1(dlat,rlats,nlat,1)
         call grdcrd1(dlon,rlons,nlon,1)
      endif
      if (linvalidkx .or. linvalidob .or. outside)  cycle loop_readobs

      rminobs=gstime !assuming ob time to be the same as the analysis time 
      t4dv = (rminobs-real(iwinbgn,r_kind))*r60inv

      !note: no time window check needed for min/maxT

!     if(oberrflg) oberr=real(etabl(kx,1,2),kind=r_kind)    !get error from error table
      oberr=real(maxtmint_oberrors(kx),kind=r_kind)

      kxall(kx)=kxall(kx)+1   !kxall used for diagnostic purposes only

      obval=real(obval4,kind=r_kind)+t0c
      iout=iout+1
      nc=ikx(kx)

      cat=0
      mxtmqm=0
      mitmqm=0

! Set usage variable
      usage = zero

      if(icuse(nc) <= 0)usage=100._r_kind

      call time_4dvar(ianldate,toff)

      if (mod(iout-1,100)==0 .and. print_verbose) then
         write(6,*)myname,': t4dv is ',t4dv
         write(6,*)myname,': file date is ',ianldate
         write(6,*)myname,': time offset is ',toff,' hours'
      endif


      udbl=0._r_double
      vdbl=0._r_double

      idate=ianldate   !assume ob report time to be at analysis time

      call get_usagerj(kx,obstype,c_station_id,c_prvstg,c_sprvstg, &
                              dlon_earth,dlat_earth,idate,t4dv-toff, &
                              udbl,vdbl,usage)


! Get information from surface file necessary for conventional data here
      call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)

      if(lhilbert) &
      call accum_hilbertcurve(usage,c_station_id,c_prvstg,c_sprvstg, &
           dlat_earth,dlon_earth,dlat,dlon,t4dv,toff,nc,kx,iout)

      pob=101.0_r_kind  ! Assume 1010 mb = 101.0 cb
      dlnpob=log(pob)   ! ln(pressure in cb)

!  Maximum temperature
      if(mxtmob) then
         mxtmoe=oberr
         qtflg=one
         cdata_all(1,iout)=mxtmoe                  ! maximum temperature error
         cdata_all(2,iout)=dlon                    ! grid relative longitude
         cdata_all(3,iout)=dlat                    ! grid relative latitude
         cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
         cdata_all(5,iout)=obval                   ! maximum temperature ob.
         cdata_all(6,iout)=rstation_id             ! station id
         cdata_all(7,iout)=t4dv                    ! time
         cdata_all(8,iout)=nc                      ! type
         cdata_all(9,iout)=qtflg                   ! qtflg (virtual temperature flag)
         cdata_all(10,iout)=mxtmqm                 ! quality mark
         cdata_all(11,iout)=oberr                  ! original obs error
         cdata_all(12,iout)=usage                  ! usage parameter
         if (lhilbert) thisobtype_usage=12         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
         cdata_all(13,iout)=idomsfc                ! dominate surface type
         cdata_all(14,iout)=tsavg                  ! skin temperature
         cdata_all(15,iout)=ff10                   ! 10 meter wind factor
         cdata_all(16,iout)=sfcr                   ! surface roughness
         cdata_all(17,iout)=dlon_earth_deg         ! earth relative longitude (degrees)
         cdata_all(18,iout)=dlat_earth_deg         ! earth relative latitude (degrees)
         cdata_all(19,iout)=stnelev                ! station elevation (m)
         cdata_all(20,iout)=stnelev                ! observation height (m)
         cdata_all(21,iout)=zz                     ! terrain height at ob location
         cdata_all(22,iout)=r_prvstg               ! provider name
         cdata_all(23,iout)=r_sprvstg              ! subprovider name
         cdata_all(24,iout)=cat                    ! cat

!   Minimum temperature
      else if(mitmob) then
         mitmoe=oberr
         qtflg=one
         cdata_all(1,iout)=mitmoe                  ! minimum temperature error
         cdata_all(2,iout)=dlon                    ! grid relative longitude
         cdata_all(3,iout)=dlat                    ! grid relative latitude
         cdata_all(4,iout)=dlnpob                  ! ln(pressure in cb)
         cdata_all(5,iout)=obval                   ! minimum temperature ob.
         cdata_all(6,iout)=rstation_id             ! station id
         cdata_all(7,iout)=t4dv                    ! time
         cdata_all(8,iout)=nc                      ! type
         cdata_all(9,iout)=qtflg                   ! qtflg (virtual temperature flag)
         cdata_all(10,iout)=mitmqm                 ! quality mark
         cdata_all(11,iout)=oberr                  ! original obs error
         cdata_all(12,iout)=usage                  ! usage parameter
         if (lhilbert) thisobtype_usage=12         ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
         cdata_all(13,iout)=idomsfc                ! dominate surface type
         cdata_all(14,iout)=tsavg                  ! skin temperature
         cdata_all(15,iout)=ff10                   ! 10 meter wind factor
         cdata_all(16,iout)=sfcr                   ! surface roughness
         cdata_all(17,iout)=dlon_earth_deg         ! earth relative longitude (degrees)
         cdata_all(18,iout)=dlat_earth_deg         ! earth relative latitude (degrees)
         cdata_all(19,iout)=stnelev                ! station elevation (m)
         cdata_all(20,iout)=stnelev                ! observation height (m)
         cdata_all(21,iout)=zz                     ! terrain height at ob location
         cdata_all(22,iout)=r_prvstg               ! provider name
         cdata_all(23,iout)=r_sprvstg              ! subprovider name
         cdata_all(24,iout)=cat                    ! cat
      end if
  end do loop_readobs

  write(6,*)myname,': ob counts by report type'
  do kx=100,199
     if (ikx(kx) /= -1) then
        write(6,*) '     kx, number of obs: ',kx,kxall(kx)
     endif
  enddo

  write(6,*)myname,': Total invalid report types: ',invalidkx
  write(6,*)myname,': Total invalid observation values: ',invalidob
  write(6,*)myname,': Observations outside of the domain: ',n_outside
! Apply hilbert curve for cross validation if requested

  if(lhilbert) &
     call apply_hilbertcurve(maxobs,obstype,cdata_all(thisobtype_usage,1:maxobs))
 
  nread=maxobs
  ndata=iout
  nodata=iout

  call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)

  deallocate(cdata_all)

  call destroy_rjlists
  if (lhilbert) call destroy_hilbertcurve
  if (twodvar_regional) call destroy_ndfdgrid

  close(lunin)

  return

end subroutine read_mitm_mxtm
