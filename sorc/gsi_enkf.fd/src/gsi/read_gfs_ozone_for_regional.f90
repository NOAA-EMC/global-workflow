subroutine read_gfs_ozone_for_regional
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfs_ozone_for_regionl  read gfs ozone for regional
!   prgmmr: parrish          org: np22                date: 2010-03-06
!
! abstract: read gfs ozone and interpolate to regional analysis grid.
!
!
! program history log:
!   2010-03-06  parrish, initial documentation
!   2010-03-19  parrish - correct for extrapolation error in subroutine intp_spl
!   2010-03-19  parrish - correct diagnostic output of reg_ozmin, reg_ozmax
!   2010-04-15  wu - set ges_oz to a small number if the gfs input negative
!   2010-10-15  parrish - add uv_hyb_ens to arg list for call to general_read_gfsatm.
!                          This was added to allow retrieval of u,v or psi,chi in
!                           subroutine get_gefs_ensperts_dualres.f90.
!   2011-07-05  todling - treatment of oz and prsl looks suspicious: fix to fit 
!                         new interface to general_sub2grid (but equally suspicious)
!   2013-02-20  wu      - add call to general_destroy_spec_vars to deallocate large arrays in sp_gfs.
!                           Also deallocate other locally allocated arrays.
!   2013-10-19  todling - metguess now holds background
!   2013-12-06  eliu    - add FGAT capability 
!   2014-06-30  wu      - bug fix for undefined variable "proceed" in check_vars_
!   2014-08-18  tong    - modified to allow gfs/gdas spectral coefficients to be
!                         transformed to a coarser resolution grid
!   2014-11-30  todling - update interface to general_read_gfs routines
!   2014-12-03  derber  - modify call to general_read_gfsatm
!   2015-05-13  wu      - read in just one GFS for ozone even when nfldsig > 1 
!                         use the same ges_oz in all time levels
!   2016-12-12  tong    - add code to get nemsio meta data, if use_gfs_nemsio=True
!   2012-07-02  bi      - add code to get netcdf data, if use_gfs_ncio=.true.
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use gridmod, only: nlat,nlon,lat2,lon2,nsig,region_lat,region_lon,check_gfs_ozone_date
  use gridmod, only: jcap_gfs,nlat_gfs,nlon_gfs,wrf_nmm_regional,use_gfs_nemsio, &
                     use_gfs_ncio,ncepgfs_head,ncepgfs_headv
  use constants,only: zero,half,rd_over_cp,one,h300,r60,r3600
  use constants, only: max_varname_length
  use mpimod, only: mpi_comm_world,ierror,mype,mpi_rtype,mpi_min,mpi_max,npe
  use mpeu_util, only: die
  use kinds, only: r_kind,i_kind,r_single
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_grid2sub,general_sub2grid
  use general_sub2grid_mod, only: general_sub2grid_destroy_info
  use general_specmod, only: spec_vars,general_init_spec_vars,general_destroy_spec_vars
  use egrid2agrid_mod, only: g_create_egrid2points_slow,egrid2agrid_parm,g_egrid2points_faster
  use sigio_module, only: sigio_intkind,sigio_head,sigio_srhead
  use guess_grids, only: ges_prsl,ntguessig,nfldsig,ifilesig
  use aniso_ens_util, only: intp_spl
  use obsmod, only: iadate
  use gsi_bundlemod, only: gsi_bundle,gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundlecreate,gsi_bundledestroy
  use gsi_bundlemod, only: gsi_grid,gsi_gridcreate
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use gsi_4dvar, only: nhr_assimilation
  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
  use ncepnems_io, only: error_msg
  use nemsio_module, only: nemsio_gfile,nemsio_getfilehead
  use module_ncio, only: Dataset, Dimension, get_dim, read_vardata,&
                                 open_dataset, close_dataset, read_attribute,&
                                 get_idate_from_time_units

  implicit none

  type(sub2grid_info) grd_gfs,grd_mix,grd_gfst
  type(spec_vars) sp_gfs,sp_b
  real(r_kind),allocatable,dimension(:,:,:) :: pri,prsl
  real(r_kind),pointer,dimension(:,:,:) :: vor =>null()
  real(r_kind),pointer,dimension(:,:,:) :: div =>null()
  real(r_kind),pointer,dimension(:,:,:) :: u   =>null()
  real(r_kind),pointer,dimension(:,:,:) :: v   =>null()
  real(r_kind),pointer,dimension(:,:,:) :: tv  =>null()
  real(r_kind),pointer,dimension(:,:,:) :: q   =>null()
  real(r_kind),pointer,dimension(:,:,:) :: cwmr=>null()
  real(r_kind),pointer,dimension(:,:,:) :: oz  =>null()
  real(r_kind),pointer,dimension(:,:)   :: z =>null()
  real(r_kind),pointer,dimension(:,:)   :: ps=>null()
  real(r_kind),allocatable,dimension(:) :: ak5,bk5,ck5,tref5
  real(r_kind),allocatable :: work_sub(:,:,:,:),work(:,:,:,:),work_reg(:,:,:,:)

  character(len=*),parameter::myname='read_gfs_ozone_for_regional'
  real(r_kind) kapr,kap1,trk
  integer(i_kind) iret,i,j,k,k2,ndim
  integer(i_kind) it,it_beg,it_end   
  integer(i_kind) istatus
  integer(i_kind) kr
  character(24) filename
  character(255),allocatable,dimension(:)::infiles
  logical uv_hyb_ens
  integer(sigio_intkind):: lunges = 11
  type(sigio_head):: sighead
  type(egrid2agrid_parm) :: p_g2r
  integer(i_kind) inner_vars,num_fields,num_fieldst,nsig_gfs,jcap_gfs_test
  integer(i_kind) nord_g2r,jcap_org,nlon_b
  logical,allocatable :: vector(:)
  logical vector0
  logical hires
  real(r_kind) ozmin,ozmax
  real(r_kind) ozmin0,ozmax0
  real(r_kind),parameter::  zero_001=0.001_r_kind
  logical regional,proceed
  real(r_kind),allocatable,dimension(:) :: xspli,yspli,xsplo,ysplo
  integer(i_kind) iyr,ihourg
  integer(i_kind),dimension(4):: idate4
  integer(i_kind),dimension(6):: idate6
  integer(i_kind),dimension(8) :: ida,jda 
  integer(i_kind),dimension(5) :: iadate_gfs
  real(r_kind) hourg
  real(r_kind),dimension(5):: fha
  character(len=120) :: my_name = 'READ_GFS_OZONE_FOR_REGIONAL'
  integer(i_kind) :: latb, lonb, levs, nframe
  integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
  integer(i_kind) :: njcap, idvc, idsl
  integer(i_kind) :: istop = 101
  integer(i_kind),dimension(7):: idate
  real(r_kind) :: fhour
  type(nemsio_gfile) :: gfile
  integer(i_kind) :: nvcoord
  real(r_single),allocatable:: nems_vcoord(:,:,:)
  real(r_single),allocatable:: vcoord(:,:)
  real(r_kind),allocatable,dimension(:)::glb_ozmin,glb_ozmax,reg_ozmin,reg_ozmax
  real(r_kind),allocatable,dimension(:)::glb_ozmin0,glb_ozmax0,reg_ozmin0,reg_ozmax0
  real(r_kind),allocatable,dimension(:,:,:,:)::ges_oz
  type(Dataset) :: atmges
  type(ncepgfs_head):: gfshead
  type(ncepgfs_headv):: gfsheadv
  type(Dimension) :: ncdim
  real(r_kind), allocatable, dimension(:) :: fhour2,aknc,bknc

  type(gsi_bundle) :: atm_bundle
  type(gsi_grid)   :: atm_grid
  integer(i_kind),parameter :: n2d=2
  integer(i_kind),parameter :: n3d=8
  character(len=max_varname_length), parameter :: vars2d(n2d) = (/ 'z   ', 'ps  ' /)
  character(len=max_varname_length), parameter :: vars3d(n3d) = (/ 'u   ', 'v   ', &
                                                  'vor ', 'div ', &
                                                  'tv  ', 'q   ', &
                                                  'cw  ', 'oz  '  /)


!     figure out what are acceptable dimensions for global grid, based on resolution of input spectral coefs
!   need to inquire from file what is spectral truncation, then setup general spectral structure variable

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  regional=.true.
  uv_hyb_ens=.false.      !  can be true or false, since these fields are discarded anyway.

! Determine input GFS filenames
  it_beg=1
!!  for now use just one time level for global ozone input
  it_end=1
  allocate(infiles(nfldsig))
  do it=it_beg,it_end
     write(filename,'("gfs_sigf",i2.2)')nhr_assimilation
     infiles(it)=filename
     if(mype==0) then
        write(6,*) 'read_gfs_ozone_for_regional: gfs file required: nfldsig = ',nfldsig                           
        write(6,*) 'read_gfs_ozone_for_regional: gfs file required: ifilesig(it)= ',ifilesig(it)          
        write(6,*) 'read_gfs_ozone_for_regional: gfs file required: infiles(it) = ',trim(infiles(it))
        write(6,*) 'read_gfs_ozone_for_regional: gfs file required: ntguessig   = ',ntguessig                       
     endif
  enddo
 
! Loop through input GFS files
  it_loop: do it = it_beg,it_end
 
  filename=infiles(it)
  if (mype==0) write(6,*)'read_gfs_ozone_for_regional: reading in gfs file:',trim(filename)                  

  if ((.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio))then
     open(lunges,file=trim(filename),form='unformatted')
     call sigio_srhead(lunges,sighead,iret)
     close(lunges)
     if(mype == 0) then
        write(6,*) ' sighead%fhour,sighead%idate=',sighead%fhour,sighead%idate
        write(6,*) ' iadate(y,m,d,hr,min)=',iadate
        write(6,*) ' sighead%jcap,sighead%levs=',sighead%jcap,sighead%levs
        write(6,*) ' sighead%latf,sighead%lonf=',sighead%latf,sighead%lonf
        write(6,*) ' sighead%idvc,sighead%nvcoord=',sighead%idvc,sighead%nvcoord
        write(6,*) ' sighead%idsl=',sighead%idsl
        do k=1,sighead%levs+1
           write(6,*)' k,vcoord=',k,sighead%vcoord(k,:)
        end do
     end if

     nsig_gfs=sighead%levs
     jcap_org=sighead%jcap
     idvc=sighead%idvc
     idsl=sighead%idsl
! Extract header information
     hourg    = sighead%fhour
     idate4(1)= sighead%idate(1)
     idate4(2)= sighead%idate(2)
     idate4(3)= sighead%idate(3)
     idate4(4)= sighead%idate(4)

!  add netcdf code here
   else if (use_gfs_ncio) then
      atmges = open_dataset(filename)
      ! get dimension sizes
      ncdim = get_dim(atmges, 'grid_xt'); gfshead%lonb = ncdim%len
      ncdim = get_dim(atmges, 'grid_yt'); gfshead%latb = ncdim%len
      ncdim = get_dim(atmges, 'pfull'); gfshead%levs = ncdim%len
      nsig_gfs=gfshead%levs
      ! hard code jcap,idsl,idvc
      gfshead%jcap = -9999
      gfshead%idsl= 1
      gfshead%idvc = 2

      ! FV3GFS write component does not include JCAP, infer from DIMY-2
      njcap=latb-2
      
      nlat_gfs=gfshead%latb+2
      nlon_gfs=gfshead%lonb
      nsig_gfs=gfshead%levs
      
      jcap_gfs=gfshead%latb-2

      ! get time information
      idate6 = get_idate_from_time_units(atmges)
      gfshead%idate(1) = idate6(4)  !hour
      gfshead%idate(2) = idate6(2)  !month
      gfshead%idate(3) = idate6(3)  !day
      gfshead%idate(4) = idate6(1)  !year
      call read_vardata(atmges, 'time', fhour2) ! might need to change this to attribute later
                                               ! depends on model changes from Jeff Whitaker
      gfshead%fhour = fhour2(1)

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

      call close_dataset(atmges)

      if(mype == 0) then
        write(6,*) ' reozone:fhour,idate=',fhour2,idate6
        write(6,*) ' reozone:iadate(y,m,d,hr,min)=',iadate
        write(6,*) ' reozone: jcap,levs=',gfshead%levs
        write(6,*) ' reozone: latb,lonb=',gfshead%latb,gfshead%lonb
        write(6,*) ' reozone: nvcoord=',gfshead%nvcoord
        write(6,*) ' reozone: idvc,idsl=',gfshead%idvc,gfshead%idsl
      endif

       hourg = fhour2(1)
       idate4(1) = idate6(4)
       idate4(2) = idate6(2)
       idate4(3) = idate6(3)
       idate4(4) = idate6(1)

  else
     call nemsio_init(iret=iret)
     if (iret /= 0) call error_msg(trim(my_name),trim(filename),' ','init',istop,iret)

     call nemsio_open(gfile,filename,'READ',iret=iret)
     if (iret /= 0) call error_msg(trim(my_name),trim(filename),' ','open',istop,iret)

     call nemsio_getfilehead(gfile,iret=iret,nframe=nframe, &
          nfhour=nfhour,nfminute=nfminute,nfsecondn=nfsecondn,nfsecondd=nfsecondd, &
          idate=idate,dimx=lonb,dimy=latb,dimz=levs,jcap=njcap,idvc=idvc,idsl=idsl)

     ! FV3GFS write component does not include JCAP, infer from DIMY-2
     if (njcap<0) njcap=latb-2
 
     if (nframe /= 0) call error_msg(trim(my_name),trim(filename),'nframe', &
                                     'getfilehead',istop,nframe)

     fhour = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
             real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600

     nsig_gfs=levs
     jcap_org=njcap

     allocate(nems_vcoord(levs+1,3,2))
     call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord)
     if ( iret /= 0 ) call error_msg(trim(my_name),trim(filename),' ', &
                                     'getfilehead',istop,iret)

!    Determine the type of vertical coordinate used by model because that
!    gfshead%nvcoord is no longer part of NEMSIO header output.
     nvcoord=3
     if(maxval(nems_vcoord(:,3,1))==zero .and. &
        minval(nems_vcoord(:,3,1))==zero ) then
        nvcoord=2
        if(maxval(nems_vcoord(:,2,1))==zero .and. &
           minval(nems_vcoord(:,2,1))==zero ) then
           nvcoord=1
        end if
     end if
     if (nvcoord > 2) then
        write(6,*)' READ_GFS_OZONE_FOR_REGIONAL: NOT READY YET FOR ak5,bk5,ck5 vert &
                    coordinate'
        call stop2(85)
     endif


     call nemsio_close(gfile,iret=iret)
     if ( iret /= 0 ) call error_msg(trim(my_name),trim(filename),' ', &
                                     'close',istop,iret)

     hourg = fhour
     idate4(1) = idate(4)  !hour
     idate4(2) = idate(2)  !month
     idate4(3) = idate(3)  !day
     idate4(4) = idate(1)  !year

     if(mype == 0) then
        write(6,*) ' nemsio: fhour,idate=',fhour,idate
        write(6,*) ' iadate(y,m,d,hr,min)=',iadate
        write(6,*) ' nemsio: jcap,levs=',njcap,levs
        write(6,*) ' nemsio: latb,lonb=',latb,lonb
        write(6,*) ' nemsio: idvc,nvcoord=',idvc,nvcoord
        write(6,*) ' nemsio: idsl=',idsl
     end if

  end if

! Compute valid time from guess date and forecast length and compare to iadate, the analysis time
  iyr=idate4(4)
  ihourg=hourg
  if(iyr>=0.and.iyr<=99) then
     if(iyr>51) then
        iyr=iyr+1900
     else
        iyr=iyr+2000
     end if
  end if
  fha=zero ; ida=0; jda=0
  fha(2)=ihourg    ! relative time interval in hours
  ida(1)=iyr       ! year
  ida(2)=idate4(2) ! month
  ida(3)=idate4(3) ! day
  ida(4)=0         ! time zone
  ida(5)=idate4(1) ! hour
  call w3movdat(fha,ida,jda)
  iadate_gfs(1)=jda(1) ! year
  iadate_gfs(2)=jda(2) ! mon
  iadate_gfs(3)=jda(3) ! day
  iadate_gfs(4)=jda(5) ! hour
  iadate_gfs(5)=0      ! minute
  if(mype == 0) then
     write(6,*)' in read_gfs_ozone_for_regional, iadate_gfs=',iadate_gfs
     write(6,*)' in read_gfs_ozone_for_regional, iadate    =',iadate
  end if

  if((iadate_gfs(1)/=iadate(1).or.iadate_gfs(2)/=iadate(2).or.iadate_gfs(3)/=iadate(3).or.&
      iadate_gfs(4)/=iadate(4).or.iadate_gfs(5)/=iadate(5)) .and. .not. wrf_nmm_regional ) then
     if(mype == 0) write(6,*)' WARNING: GFS OZONE FIELD DATE NOT EQUAL TO ANALYSIS DATE'
     if(check_gfs_ozone_date) then
        if(mype == 0) write(6,*)' CHECK_GFS_OZONE_DATE = .true., PROGRAM STOPS DUE TO OZONE DATE MISMATCH'
        call stop2(85)
     end if
  end if
     

!         set up ak5,bk5,ck5 for use in computing 3d pressure field (needed for vertical interp to regional)
!                            following is code segment from gesinfo.F90
  allocate(ak5(nsig_gfs+1))
  allocate(bk5(nsig_gfs+1))
  allocate(ck5(nsig_gfs+1))
  allocate(tref5(nsig_gfs))

  idvc=gfshead%idvc
  idsl=gfshead%idsl
  do k=1,nsig_gfs+1
     ak5(k)=zero
     bk5(k)=zero
     ck5(k)=zero
  end do
  if((.not. use_gfs_nemsio).and.(.not. use_gfs_ncio))then
     if (sighead%nvcoord == 1) then
        do k=1,sighead%levs+1
           bk5(k) = sighead%vcoord(k,1)
        end do
     elseif (sighead%nvcoord == 2) then
        do k = 1,sighead%levs+1
           ak5(k) = sighead%vcoord(k,1)*zero_001
           bk5(k) = sighead%vcoord(k,2)
        end do
     elseif (sighead%nvcoord == 3) then
        do k = 1,sighead%levs+1
           ak5(k) = sighead%vcoord(k,1)*zero_001
           bk5(k) = sighead%vcoord(k,2)
           ck5(k) = sighead%vcoord(k,3)*zero_001
        end do
     else
        write(6,*)'READ_GFS_OZONE_FOR_REGIONAL:  ***ERROR*** INVALID value for nvcoord=',sighead%nvcoord,filename
        call stop2(85)
     endif
  else if ( use_gfs_ncio ) then 
     if (gfshead%nvcoord == 1) then
        do k=1,nsig_gfs+1
           bk5(k) = gfsheadv%vcoord(k,1)
        end do
     elseif (gfshead%nvcoord == 2) then
        do k = 1,nsig_gfs+1
           ak5(k) = gfsheadv%vcoord(k,1)*zero_001
           bk5(k) = gfsheadv%vcoord(k,2)
        end do
     elseif (gfshead%nvcoord == 3) then
        do k = 1,nsig_gfs+1
           ak5(k) = gfsheadv%vcoord(k,1)*zero_001
           bk5(k) = gfsheadv%vcoord(k,2)
           ck5(k) = gfsheadv%vcoord(k,3)*zero_001
        end do
     else
        write(6,*)'***ERROR*** INVALID value nvcoord=',gfshead%nvcoord
        call stop2(85)
     endif
  else
     allocate(vcoord(levs+1,nvcoord))
     vcoord(:,1:nvcoord) = nems_vcoord(:,1:nvcoord,1)
     if (nvcoord == 1) then
        do k=1,nsig_gfs+1
           bk5(k) = vcoord(k,1)
        end do
     elseif (nvcoord == 2) then
        do k = 1,nsig_gfs+1
           ak5(k) = vcoord(k,1)*zero_001
           bk5(k) = vcoord(k,2)
        end do
     elseif (nvcoord == 3) then
        do k = 1,nsig_gfs+1
           ak5(k) = vcoord(k,1)*zero_001
           bk5(k) = vcoord(k,2)
           ck5(k) = vcoord(k,3)*zero_001
        end do
     else
        write(6,*)'GET_GEFS_FOR_REGIONAL:  ***ERROR*** INVALID value for nvcoord=',nvcoord
        call stop2(85)
     endif
     deallocate(vcoord,nems_vcoord)
  end if

! Load reference temperature array (used by general coordinate)
  do k=1,nsig_gfs
     tref5(k)=h300
  end do


  inner_vars=1
  num_fields=6*nsig_gfs+1
  num_fieldst=min(num_fields,npe)

  hires=.false.
  nlon_b=((2*jcap_org+1)/nlon_gfs+1)*nlon_gfs
  if ( nlon_b > nlon_gfs ) then
     hires=.true.
  else
     hires=.false.
     if((.not. use_gfs_nemsio).and.(.not. use_gfs_ncio))then
        jcap_gfs=sighead%jcap
        nlat_gfs=sighead%latf+2
        nlon_gfs=sighead%lonf
     else
        jcap_gfs=njcap
        nlat_gfs=latb+2
        nlon_gfs=lonb
     endif
  end if

  if(mype==0) write(6,*)'read_gfs_ozone_for_regional: jcap_org, jcap_gfs= ', &
              jcap_org, jcap_gfs
  if(mype==0) write(6,*)'read_gfs_ozone_for_regional: nlon_b, nlon_gfs, hires=', &
                         nlon_b, nlon_gfs, hires

  call general_sub2grid_create_info(grd_gfst,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fieldst, &
                                  .not.regional)
  num_fields=2*nsig_gfs
  allocate(vector(num_fields))
  vector=.false.
  call general_sub2grid_create_info(grd_gfs,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fields, &
                                  .not.regional,vector)
  jcap_gfs_test=jcap_gfs
  call general_init_spec_vars(sp_gfs,jcap_gfs,jcap_gfs_test,grd_gfs%nlat,grd_gfs%nlon)
  if (hires .and. .not. use_gfs_nemsio .and. .not. use_gfs_ncio) then
      call general_init_spec_vars(sp_b,jcap_org,jcap_org,nlat_gfs,nlon_b)
  end if

!  also want to set up regional grid structure variable grd_mix, which still has number of
!   vertical levels set to nsig_gfs, but horizontal dimensions set to regional domain.

  call general_sub2grid_create_info(grd_mix,inner_vars,nlat,nlon,nsig_gfs,num_fields,regional,vector)
  deallocate(vector)


! allocate bundle for reading required fields
  call gsi_gridcreate(atm_grid,grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig)
  call gsi_bundlecreate(atm_bundle,atm_grid,'aux-atm-read',istatus,names2d=vars2d,names3d=vars3d)
  if(istatus/=0) then
    write(6,*)myname,': trouble creating atm_bundle'
    call stop2(999)
  endif

  allocate( pri(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig+1))
  allocate(prsl(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
  if(use_gfs_nemsio)then
     call general_read_gfsatm_nems(grd_gfst,sp_gfs,filename,uv_hyb_ens,.false.,.false., &
                              atm_bundle,.true.,iret)
  else if (use_gfs_ncio) then
     call general_read_gfsatm_nc(grd_gfst,sp_gfs,filename,uv_hyb_ens,.false.,.true., &
                              atm_bundle,.true.,iret)
  else
     if (hires) then
        call general_read_gfsatm(grd_gfst,sp_gfs,sp_b,filename,uv_hyb_ens,.false.,.false., &
                              atm_bundle,.true.,iret)
     else
        call general_read_gfsatm(grd_gfst,sp_gfs,sp_gfs,filename,uv_hyb_ens,.false.,.false., &
                              atm_bundle,.true.,iret)
     end if
  end if

  ierror = 0
  call gsi_bundlegetpointer(atm_bundle,'vor' ,vor ,istatus) ; ierror = ierror + istatus
  call gsi_bundlegetpointer(atm_bundle,'div' ,div ,istatus) ; ierror = ierror + istatus
  call gsi_bundlegetpointer(atm_bundle,'u'   ,u   ,istatus) ; ierror = ierror + istatus
  call gsi_bundlegetpointer(atm_bundle,'v'   ,v   ,istatus) ; ierror = ierror + istatus
  call gsi_bundlegetpointer(atm_bundle,'tv'  ,tv  ,istatus) ; ierror = ierror + istatus
  call gsi_bundlegetpointer(atm_bundle,'q'   ,q   ,istatus) ; ierror = ierror + istatus
  call gsi_bundlegetpointer(atm_bundle,'oz'  ,oz  ,istatus) ; ierror = ierror + istatus
  call gsi_bundlegetpointer(atm_bundle,'cw'  ,cwmr,istatus) ; ierror = ierror + istatus
  call gsi_bundlegetpointer(atm_bundle,'z'   ,z   ,istatus) ; ierror = ierror + istatus
  call gsi_bundlegetpointer(atm_bundle,'ps'  ,ps  ,istatus) ; ierror = ierror + istatus
  if ( ierror /= 0 ) call die(myname,': missing atm_bundle vars, aborting ...',ierror)

  do k=1,grd_gfs%nsig
     ozmin=minval(oz(:,:,k))
     ozmax=maxval(oz(:,:,k))
     call mpi_allreduce(ozmin,ozmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(ozmax,ozmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
     if(mype == 0) write(6,'(" k,min,max gfs oz = ",i3,2e15.4)')k,ozmin0,ozmax0
  end do

! compute 3d pressure on interfaces
  kap1=rd_over_cp+one
  kapr=one/rd_over_cp
  pri=zero
  k=1
  k2=grd_gfs%nsig+1
  do j=1,grd_gfs%lon2
     do i=1,grd_gfs%lat2
        pri(i,j,k)=ps(i,j)
        pri(i,j,k2)=zero
     end do
  end do
  if (idvc /= 3) then
     do k=2,grd_gfs%nsig
        do j=1,grd_gfs%lon2
           do i=1,grd_gfs%lat2
              pri(i,j,k)=ak5(k)+bk5(k)*ps(i,j)
           end do
        end do
     end do
  else
     do k=2,grd_gfs%nsig
        do j=1,grd_gfs%lon2
           do i=1,grd_gfs%lat2
              trk=(half*(tv(i,j,k-1)+tv(i,j,k))/tref5(k))**kapr
              pri(i,j,k)=ak5(k)+(bk5(k)*ps(i,j))+(ck5(k)*trk)
           end do
        end do
     end do
  end if
  deallocate(ak5,bk5,ck5,tref5)

  do k=1,grd_gfs%nsig+1
     ozmin=minval(pri(:,:,k))
     ozmax=maxval(pri(:,:,k))
     call mpi_allreduce(ozmin,ozmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(ozmax,ozmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
     if(mype == 0) write(6,'(" k,min,max gfs pri = ",i3,2e15.4)')k,ozmin0,ozmax0
  end do

! next get pressure at layer midpoints.
  if (idsl/=2) then
     do j=1,grd_gfs%lon2
        do i=1,grd_gfs%lat2
           do k=1,grd_gfs%nsig
              prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+1)**kap1)/&
                           (kap1*(pri(i,j,k)-pri(i,j,k+1))))**kapr
           end do
        end do
     end do
  else
     do j=1,grd_gfs%lon2
        do i=1,grd_gfs%lat2
           do k=1,grd_gfs%nsig
              prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+1))*half
           end do
        end do
     end do
  end if
  deallocate(pri)
  do k=1,grd_gfs%nsig
     ozmin=minval(prsl(:,:,k))
     ozmax=maxval(prsl(:,:,k))
     call mpi_allreduce(ozmin,ozmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(ozmax,ozmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
     if(mype == 0) write(6,'(" k,min,max gfs prsl = ",i3,2e15.4)')k,ozmin0,ozmax0
  end do

! use general_sub2grid to go to horizontal slabs 

  allocate(work_sub(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig*2,1))
  ndim=grd_gfs%nsig
  do k=1,grd_gfs%nsig
     do j=1,grd_gfs%lon2
        do i=1,grd_gfs%lat2
           work_sub(i,j,k,1)=prsl(i,j,k)
           work_sub(i,j,k+ndim,1)=oz(i,j,k)
        end do
     end do
  end do
  deallocate(prsl)
  allocate(work(grd_gfs%nlat,grd_gfs%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc,1))
  call general_sub2grid(grd_gfs,work_sub,work)
  deallocate(work_sub)

  call gsi_bundledestroy(atm_bundle,istatus)

! then interpolate to regional analysis grid
  nord_g2r=4
  call g_create_egrid2points_slow(nlat*nlon,region_lat,region_lon, &
                    grd_gfs%nlat,sp_gfs%rlats,grd_gfs%nlon,sp_gfs%rlons,nord_g2r,p_g2r)
  vector0=.false.
  allocate(work_reg(grd_mix%nlat,grd_mix%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc,1))
  do k=grd_gfs%kbegin_loc,grd_gfs%kend_loc
     call g_egrid2points_faster(p_g2r,work(:,:,k,1),work_reg(:,:,k,1),vector0)
  end do
  deallocate(work)

! next general_grid2sub to go to regional grid subdomains.
  allocate(work_sub(grd_mix%lat2,grd_mix%lon2,grd_gfs%nsig*2,1))
  call general_grid2sub(grd_mix,work_reg,work_sub)
  deallocate(work_reg)
  do k=1,grd_gfs%nsig
     ozmin=minval(work_sub(:,:,k,1))
     ozmax=maxval(work_sub(:,:,k,1))
     call mpi_allreduce(ozmin,ozmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(ozmax,ozmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  end do
  do k=1,grd_gfs%nsig
     ozmin=minval(work_sub(:,:,k+ndim,1))
     ozmax=maxval(work_sub(:,:,k+ndim,1))
     call mpi_allreduce(ozmin,ozmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(ozmax,ozmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  end do

! finally, interpolate/extrapolate in vertical using yoshi's spline code.  deposit result in ges_oz.
  allocate(xspli(grd_mix%nsig),yspli(grd_mix%nsig),xsplo(nsig),ysplo(nsig))
  allocate(glb_ozmin(grd_mix%nsig),glb_ozmax(grd_mix%nsig))
  allocate(reg_ozmin(        nsig),reg_ozmax(        nsig))
  allocate(glb_ozmin0(grd_mix%nsig),glb_ozmax0(grd_mix%nsig))
  allocate(reg_ozmin0(        nsig),reg_ozmax0(        nsig))
  glb_ozmin= huge(glb_ozmin)
  glb_ozmax=-huge(glb_ozmax)
  reg_ozmin= huge(reg_ozmin)
  reg_ozmax=-huge(reg_ozmax)
  do j=1,lon2
     do i=1,lat2
        do k=1,grd_mix%nsig
           xspli(k)=log(work_sub(i,j,k,1)*10.0_r_kind)
           yspli(k)=work_sub(i,j,k+ndim,1)
           glb_ozmax(k)=max(yspli(k),glb_ozmax(k))
           glb_ozmin(k)=min(yspli(k),glb_ozmin(k))
                 
        end do
        do k=1,nsig
!          xsplo(k)=log(ges_prsl(i,j,k,ntguessig)*10._r_kind)
           xsplo(k)=log(ges_prsl(i,j,k,it)*10._r_kind)
        end do
        call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,nsig)
!               following is to correct for bug in intp_spl
        do k=1,nsig
           if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
           if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
        end do
        do k=1,nsig
           if(ysplo(k) < zero)ysplo(k)=1.e-10_r_kind
           ges_oz(i,j,k,it)=ysplo(k)   !  for now, only read in ges at analysis time and copy to time levels
           reg_ozmax(k)=max(ysplo(k),reg_ozmax(k))
           reg_ozmin(k)=min(ysplo(k),reg_ozmin(k))
        end do
     end do
  end do
  deallocate(work_sub)
  call mpi_allreduce(reg_ozmax,reg_ozmax0,nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  call mpi_allreduce(reg_ozmin,reg_ozmin0,nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
  call mpi_allreduce(glb_ozmax,glb_ozmax0,grd_mix%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  call mpi_allreduce(glb_ozmin,glb_ozmin0,grd_mix%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
  if(mype == 0) then
     do k=1,grd_mix%nsig
        write(6,'(" k,glb_ozmin,max=",i4,2e15.4)') k,glb_ozmin0(k),glb_ozmax0(k)
     end do
     do k=1,nsig
        write(6,'(" k,reg_ozmin,max=",i4,2e15.4)') k,reg_ozmin0(k),reg_ozmax0(k)
     end do
  end if
  call general_destroy_spec_vars(sp_gfs)
  if ( hires .and. .not. use_gfs_nemsio .and. .not. use_gfs_ncio) call general_destroy_spec_vars(sp_b)
  deallocate(xspli,yspli,xsplo,ysplo,glb_ozmin,glb_ozmax,reg_ozmin,reg_ozmax,&
             glb_ozmin0,glb_ozmax0,reg_ozmin0,reg_ozmax0)

  enddo it_loop

!!  for now use just one time level for global ozone input
  do it=2,nfldsig
     ges_oz(:,:,:,it)=ges_oz(:,:,:,1)
  enddo

! copy ges_oz to met-bundle ...
  call copy_vars_
  call final_vars_
  call general_sub2grid_destroy_info(grd_gfs)
  call general_sub2grid_destroy_info(grd_mix)
  call general_sub2grid_destroy_info(grd_gfst)
  deallocate(infiles)

  return

  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer istatus,ivar
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::oz' , ivar, istatus )
  proceed=ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  character(len=*),parameter::myname_=myname//'init_vars_'
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld,istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get oz ...
     varname='oz'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_oz))then
            write(6,*) trim(myname_), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_oz(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_oz(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_oz(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname_),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname_), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine copy_vars_

  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld,istatus

! get oz ...
  varname='oz'
  do ifld=1,nfldsig
     call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
     if (istatus==0) rank3=ges_oz(:,:,:,ifld)
  enddo

  end subroutine copy_vars_

  subroutine final_vars_
    if(allocated(ges_oz)) deallocate(ges_oz)
  end subroutine final_vars_

end subroutine read_gfs_ozone_for_regional
