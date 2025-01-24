subroutine get_gefs_for_regional
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_gefs_for_regionl  read gefsozone for regional
!   prgmmr: parrish          org: np22                date: 2010-09-26
!
! abstract: read gefs and interpolate to regional ensemble grid.
!          (adaptation of get_gefs_ensperts_dualres)
!
!
! program history log:
!   2010-09-26  parrish, initial documentation
!   2012-01-17  wu, clean up, add/setup option "full_ensemble"
!   2012-02-08  parrish - a little more cleanup
!   2012-10-11  wu      - dual resolution for options of regional hybens
!   2013-02-21  wu      - add call to general_destroy_spec_vars to fix memory problem
!   2013-10-19  todling - all guess variables in met-guess
!   2014-11-30  todling - update interface to general_read_gfs routines
!   2014-12-03  derber - changes to call for general_read_gfsatm
!   2015-05-12  wu      - changes to read in multiple ensemble for 4DEnVar
!   2015-09-20  s.liu   - use general sub2grid in grads1a
!   2016-05-19  Carley/s.liu   - prevent the GSI from printing out erroneous error  
!                               when using ensembles from different time
!   2016-12-12  tong    - add code to get nemsio meta data, if use_gfs_nemsio=True
!   2020-07-01  Bi   - add code to get netCDF data, if use_gfs_ncio=.true.
!   2020-05-04  wu   - no rotate_wind for fv3_regional
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

  use gridmod, only: idsl5,regional,use_gfs_nemsio,use_gfs_ncio,&
                     ncepgfs_head,ncepgfs_headv
  use gridmod, only: nlon,nlat,lat2,lon2,nsig,rotate_wind_ll2xy,&
                     fv3_regional
  use hybrid_ensemble_parameters, only: region_lat_ens,region_lon_ens
  use hybrid_ensemble_parameters, only: en_perts,ps_bar,nelen
  use hybrid_ensemble_parameters, only: n_ens_gfs,weight_ens_gfs,grd_ens,grd_a1,grd_e1,p_e2a,uv_hyb_ens,dual_res
  use hybrid_ensemble_parameters, only: full_ensemble,q_hyb_ens,l_ens_in_diff_time,write_ens_sprd
  use hybrid_ensemble_parameters, only: ntlevs_ens,ensemble_path,jcap_ens
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use constants,only: zero,half,fv,rd_over_cp,one,h300,r60,r3600
  use constants, only: rd,grav
  use mpimod, only: mpi_comm_world,ierror,mype,mpi_rtype,mpi_min,mpi_max
  use kinds, only: r_kind,i_kind,r_single
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_grid2sub,general_sub2grid
  use general_sub2grid_mod, only: general_suba2sube,general_sube2suba
  use general_sub2grid_mod, only: general_sub2grid_destroy_info
  use general_specmod, only: spec_vars,general_init_spec_vars,general_destroy_spec_vars
  use egrid2agrid_mod, only: g_create_egrid2points_slow,egrid2agrid_parm,g_egrid2points_faster
  use sigio_module, only: sigio_intkind,sigio_head,sigio_srhead
  use guess_grids, only: ges_prsl,ntguessig
  use guess_grids, only: ges_tsen,ifilesig,hrdifsig
  use aniso_ens_util, only: intp_spl
  use obsmod, only: iadate
  use mpimod, only: npe
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_metguess_mod, only: GSI_MetGuess_Bundle
  use mpeu_util, only: die
  use gsi_4dvar, only: nhr_assimilation
  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
  use ncepnems_io, only: error_msg
  use nemsio_module, only: nemsio_gfile,nemsio_getfilehead
  use module_ncio, only: Dimension, Dataset, open_dataset, get_dim, &
                                read_vardata, get_idate_from_time_units,&
                                read_attribute, close_dataset
  use get_wrf_mass_ensperts_mod, only: get_wrf_mass_ensperts_class
  use gsi_io, only: verbose
  use obsmod, only: l_wcp_cwm
  implicit none

  type(sub2grid_info) grd_gfs,grd_mix,grd_gfst
  type(get_wrf_mass_ensperts_class) :: wrf_mass_ensperts
  type(spec_vars) sp_gfs
  real(r_kind),allocatable,dimension(:,:,:) :: pri,prsl,prsl1000
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
  real(r_kind),allocatable :: tmp_ens(:,:,:,:),tmp_anl(:,:,:,:),tmp_ens2(:,:,:,:)
  real(r_kind),allocatable,dimension(:,:,:)::stbar,vpbar,tbar,rhbar,ozbar,cwbar
  real(r_kind),allocatable,dimension(:,:)::  pbar_nmmb
  real(r_kind),allocatable,dimension(:,:,:,:)::st_eg,vp_eg,t_eg,rh_eg,oz_eg,cw_eg
  real(r_kind),allocatable,dimension(:,:,:):: p_eg_nmmb
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsl_e
  real(r_kind),allocatable,dimension(:,:,:)::tsen,qs
  real(r_kind),allocatable,dimension(:,:,:)::ut,vt,tt,rht,ozt,cwt
  real(r_single),pointer,dimension(:,:,:):: w3
  real(r_single),pointer,dimension(:,:):: w2

  character(len=*),parameter::myname='get_gefs_for_regional'
  real(r_kind) bar_norm,sig_norm,kapr,kap1,trk
  integer(i_kind) iret,i,j,k,k2,n,mm1,iderivative
  integer(i_kind) ic2,ic3,it
  integer(i_kind) ku,kv,kt,kq,koz,kcw,kz,kps
  character(255) filename,filelists(ntlevs_ens)
  character(6) sfilename

  logical ice
  integer(sigio_intkind):: lunges = 11
  type(sigio_head):: sighead
  type(egrid2agrid_parm) :: p_g2r
  integer(i_kind) inner_vars,num_fields,nlat_gfs,nlon_gfs,nsig_gfs,jcap_gfs,jcap_gfs_test
  integer(i_kind) nord_g2r,num_fieldst
  logical,allocatable :: vector(:)
  real(r_kind),parameter::  zero_001=0.001_r_kind
  real(r_kind),allocatable,dimension(:) :: xspli,yspli,xsplo,ysplo
  integer(i_kind) iyr,ihourg,kr
  integer(i_kind),dimension(4):: idate4
  integer(i_kind),dimension(8) :: ida,jda 
  integer(i_kind),dimension(5) :: iadate_gfs
  integer(i_kind),dimension(6):: idate6
  real(r_kind) hourg
  real(r_kind),dimension(5):: fha
  integer(i_kind) istatus
  character(len=120) :: my_name = 'GET_GEFS_FOR_REGIONAL'
  integer(i_kind) :: latb, lonb, levs, nframe
  integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
  integer(i_kind) :: njcap, idvc, idsl
  integer(i_kind) :: istop = 101
  integer(i_kind),dimension(7):: idate
  real(r_kind) :: fhour
  type(nemsio_gfile) :: gfile
  type(Dataset) :: atmges      
  type(ncepgfs_head):: gfshead
  type(ncepgfs_headv):: gfsheadv
  real(r_single),allocatable,dimension(:) :: aknc, bknc, fhour1


  type(Dimension) :: ncdim
  integer(i_kind) :: nvcoord
  real(r_single),allocatable:: nems_vcoord(:,:,:)
  real(r_single),allocatable:: vcoord(:,:)
  real(r_kind) rdog,h,dz
  real(r_kind),allocatable::height(:),zbarl(:,:,:)
  logical add_bias_perturbation,inithead
  integer(i_kind) n_ens_temp
  real(r_kind),allocatable::psfc_out(:,:)
  integer(i_kind) ilook,jlook,ier

  real(r_kind) dlon,dlat,uob,vob,dlon_ens,dlat_ens
  integer(i_kind) ii,jj,n1
  integer(i_kind) iimax,iimin,jjmax,jjmin
  integer(i_kind) nming1,nming2
  integer(i_kind) its,ite
  real(r_kind) ratio_x,ratio_y

  type(gsi_bundle) :: atm_bundle
  type(gsi_grid)   :: atm_grid
  integer(i_kind),parameter :: n2d=2
  integer(i_kind),parameter :: n3d=8
  character(len=4), parameter :: vars2d(n2d) = (/ 'z   ', 'ps  ' /)
  character(len=4), parameter :: vars3d(n3d) = (/ 'u   ', 'v   ', &
                                                  'vor ', 'div ', &
                                                  'tv  ', 'q   ', &
                                                  'cw  ', 'oz  '  /)

  real(r_kind), pointer :: ges_ps(:,:  )=>NULL()
  real(r_kind), pointer :: ges_z (:,:  )=>NULL()
  real(r_kind), pointer :: ges_u (:,:,:)=>NULL()
  real(r_kind), pointer :: ges_v (:,:,:)=>NULL()
  real(r_kind), pointer :: ges_tv(:,:,:)=>NULL()
  real(r_kind), pointer :: ges_q (:,:,:)=>NULL()
  logical :: print_verbose
  real(r_kind), allocatable :: ges_z_ens(:,:)

  print_verbose=.false.
  if(verbose)print_verbose=.true.
  add_bias_perturbation=.false.  !  not fully activated yet--testing new adjustment of ps perturbions 1st

  if(ntlevs_ens > 1) then
     do i=1,ntlevs_ens
        write(filelists(i),'("filelist",i2.2)')ifilesig(i)
     enddo
     its=1
     ite=ntlevs_ens
  else
     write(filelists(1),'("filelist",i2.2)')nhr_assimilation
     its=ntguessig
     ite=ntguessig
  endif

  do it=its,ite
! get pointers for typical meteorological fields
  ier=0
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps',ges_ps,istatus );ier=ier+istatus
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z', ges_z, istatus );ier=ier+istatus
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u', ges_u, istatus );ier=ier+istatus
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v', ges_v, istatus );ier=ier+istatus
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv',ges_tv,istatus );ier=ier+istatus
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q' ,ges_q, istatus );ier=ier+istatus
  if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)

!     figure out what are acceptable dimensions for global grid, based on resolution of input spectral coefs
!   need to inquire from file what is spectral truncation, then setup general spectral structure variable

!  filename='sigf06_ens_mem001'
  if(ntlevs_ens > 1) then
     open(10,file=trim(filelists(it)),form='formatted',err=30)
  else
     open(10,file=trim(filelists(1)),form='formatted',err=30)
  endif
  rewind (10) 
  do n=1,200
     read(10,'(a)',err=20,end=40)filename 
  enddo
40 n_ens_temp=n-1
  if(n_ens_gfs/=n_ens_temp) then
     n_ens_gfs=n_ens_temp
     if(mype == 0) then
         write(6,*)'the n_ens_gfs is adjusted to the actual number of ensemble members ',n_ens_temp
     endif
  endif

!    set n_ens_temp depending on if we want to add bias perturbation to the ensemble

  if(add_bias_perturbation) then
     n_ens_temp=n_ens_gfs+1
  else
     n_ens_temp=n_ens_gfs
  end if

  rewind (10) 
  read(10,'(a)',err=20,end=20)filename 
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
        if(print_verbose)then
           do k=1,sighead%levs+1
              write(6,*)' k,vcoord=',k,sighead%vcoord(k,:)
           end do
        end if
     end if

     nlat_gfs=sighead%latf+2
     nlon_gfs=sighead%lonf
     nsig_gfs=sighead%levs
     if(sighead%jcap > 0)then
        jcap_gfs=sighead%jcap
     else if(jcap_ens > 0)then
        jcap_gfs=jcap_ens
     else
        write(6,*)'get_gefs_for_regional:ERROR jcap is undefined'
        call stop2(555)
     endif




     idvc=sighead%idvc
     idsl=sighead%idsl
! Extract header information
     hourg    = sighead%fhour
     idate4(1)= sighead%idate(1)
     idate4(2)= sighead%idate(2)
     idate4(3)= sighead%idate(3)
     idate4(4)= sighead%idate(4)
  else if ( use_gfs_nemsio ) then
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

     nlat_gfs=latb+2
     nlon_gfs=lonb
     nsig_gfs=levs
     if(njcap > 0)then
        jcap_gfs=njcap
     else if(jcap_ens > 0)then
        jcap_gfs=jcap_ens
     else
        write(6,*)'get_gefs_for_regional:ERROR jcap is undefined'
        call stop2(555)
     endif
     if(allocated(nems_vcoord)) deallocate(nems_vcoord)
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
        write(6,*)' GET_GEFS_FOR_REGIONAL: NOT READY YET FOR ak5,bk5,ck5 vert &
                    coordinate'
        call stop2(85)
     endif

     if(allocated(vcoord)) deallocate(vcoord)
     allocate(vcoord(levs+1,nvcoord))
     vcoord(:,1:nvcoord) = nems_vcoord(:,1:nvcoord,1)
     deallocate(nems_vcoord)

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
        write(6,*) ' nemsio: jcap,levs=',levs
        write(6,*) ' nemsio: latb,lonb=',latb,lonb
        write(6,*) ' nemsio: idvc,nvcoord=',idvc,nvcoord
        write(6,*) ' nemsio: idsl=',idsl
     end if

!  add netCDF header information: 
     else ! use_gfs_ncio and get this information
        write(sfilename,'("sfcf",i2.2)')nhr_assimilation
        ! open the netCDF file
        atmges = open_dataset(filename)
        ! get dimension sizes
        ncdim = get_dim(atmges, 'grid_xt'); gfshead%lonb = ncdim%len
        ncdim = get_dim(atmges, 'grid_yt'); gfshead%latb = ncdim%len
        ncdim = get_dim(atmges, 'pfull') ; gfshead%levs = ncdim%len
        nsig_gfs = gfshead%levs
        ! hard code jcap,idsl,idvc
        gfshead%jcap = -9999
        gfshead%idsl= 1
        gfshead%idvc = 2


        nlat_gfs=gfshead%latb+2
        nlon_gfs=gfshead%lonb
        nsig_gfs=gfshead%levs

        jcap_gfs=gfshead%latb-2

        if (mype==0) write(6,*)'GESINFO:  Read NCEP FV3GFS netCDF ', &
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
        idate6 = get_idate_from_time_units(atmges)
        gfshead%idate(1) = idate6(4)  !hour
        gfshead%idate(2) = idate6(2)  !month
        gfshead%idate(3) = idate6(3)  !day
        gfshead%idate(4) = idate6(1)  !year
        call read_vardata(atmges, 'time', fhour1) ! might need to change this to attribute later
                                               ! depends on model changes from Jeff Whitaker
        gfshead%fhour = fhour1(1)

        call close_dataset(atmges)

        if(mype == 0) then
          write(6,*) ' netCDF:fhour,idate=',fhour1,idate6
          write(6,*) ' netCDF:iadate(y,m,d,hr,min)=',iadate
          write(6,*) ' netCDF: jcap,levs=',gfshead%levs
          write(6,*) ' netCDF: latb,lonb=',gfshead%latb,gfshead%lonb
          write(6,*) ' netCDF: nvcoord=',gfshead%nvcoord
          write(6,*) ' netCDF: idvc,idsl=',gfshead%idvc,gfshead%idsl
        endif

        hourg = fhour1(1)
        idate4(1) = idate6(4)
        idate4(2) = idate6(2)
        idate4(3) = idate6(3)
        idate4(4) = idate6(1)

  end if

! Compute valid time from ensemble date and forecast length and compare to iadate, the analysis time
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
  if(ntlevs_ens > 1) then
     iadate_gfs(4)=jda(5)+hrdifsig(ntguessig)-hrdifsig(it) ! hour
  else
     iadate_gfs(4)=jda(5) ! hour
  endif
  iadate_gfs(5)=0      ! minute


  if(mype == 0) then
     write(6,*)' in get_gefs_for_regional, iadate_gefs=',iadate_gfs
     write(6,*)' in get_gefs_for_regional, iadate    =',iadate
  end if

     call w3fs21(iadate,nming1)
     call w3fs21(iadate_gfs,nming2)

  if( (nming1/=nming2) .and. (.not.l_ens_in_diff_time) ) then
     if(mype == 0) write(6,*)' GEFS ENSEMBLE MEMBER DATE NOT EQUAL TO ANALYSIS DATE, PROGRAM STOPS'
     call stop2(85)
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
  if ((.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio))then
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
        write(6,*)'GET_GEFS_FOR_REGIONAL:  ***ERROR*** INVALID value for nvcoord=',sighead%nvcoord
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
        write(6,*)'GET_GEFS_FOR_REGIONAL netCDF:  ***ERROR*** INVALID value for nvcoord=',gfshead%nvcoord
        call stop2(85)
     endif
  else
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
  end if


  if(mype == 0 .and. print_verbose)then
     do k=1,nsig_gfs+1
        write(6,*)' ak5,bk5,ck5=',ak5(k),bk5(k),ck5(k)
     end do
  end if

! Load reference temperature array (used by general coordinate)
  do k=1,nsig_gfs
     tref5(k)=h300
  end do


  inner_vars=1
  num_fields=6*nsig_gfs+2      !  want to transfer u,v,t,q,oz,cw,ps,z from gfs subdomain to slab
                            !  later go through this code, adapting gsibundlemod, since currently 
                            !   hardwired.
  num_fieldst=min(num_fields,npe)
  allocate(vector(num_fields))
  vector=.false.
  vector(1:2*nsig_gfs)=uv_hyb_ens
  call general_sub2grid_create_info(grd_gfst,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fieldst, &
                                  .not.regional)
  call general_sub2grid_create_info(grd_gfs,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fields, &
                                  .not.regional,vector)
  jcap_gfs_test=jcap_gfs
  call general_init_spec_vars(sp_gfs,jcap_gfs,jcap_gfs_test,grd_gfs%nlat,grd_gfs%nlon)

!  also want to set up regional grid structure variable grd_mix, which still has number of
!   vertical levels set to nsig_gfs, but horizontal dimensions set to regional domain.

  call general_sub2grid_create_info(grd_mix,inner_vars,grd_ens%nlat,grd_ens%nlon,nsig_gfs, &
                                    num_fields,regional,vector)

!  create interpolation information for global grid to regional ensemble grid

  nord_g2r=4
  call g_create_egrid2points_slow(grd_ens%nlat*grd_ens%nlon,region_lat_ens,region_lon_ens, &
                    grd_gfs%nlat,sp_gfs%rlats,grd_gfs%nlon,sp_gfs%rlons,nord_g2r,p_g2r)

!  allocate mix ensemble space--horizontal on regional domain, vertical still gefs 
  allocate(st_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens_gfs))
  allocate(vp_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens_gfs))
  allocate( t_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens_gfs))
  allocate(rh_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens_gfs))
  allocate(oz_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens_gfs))
  allocate(cw_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens_gfs))
  allocate( p_eg_nmmb(grd_mix%lat2,grd_mix%lon2,n_ens_gfs))
  st_eg=zero ; vp_eg=zero ; t_eg=zero ; rh_eg=zero ; oz_eg=zero ; cw_eg=zero 
  p_eg_nmmb=zero

!
! prepare terrain height
!
  allocate(ges_z_ens(grd_mix%lat2,grd_mix%lon2))
  if (dual_res) then
     allocate ( tmp_ens(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,1) )
     allocate ( tmp_anl(lat2,lon2,nsig,1) )
     tmp_anl=0.0_r_kind
     tmp_anl(:,:,1,1)=ges_z(:,:)
     call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
     ges_z_ens(:,:)=tmp_ens(:,:,1,1)
     deallocate(tmp_ens)
     deallocate(tmp_anl)
  else
     ges_z_ens(:,:)=ges_z(:,:)
  endif

!                begin loop over ensemble members

  rewind(10)
  inithead=.true.
  do n=1,n_ens_gfs
     read(10,'(a)',err=20,end=20)filename 
     filename=trim(ensemble_path) // trim(filename)
!     write(filename,100) n
!100  format('sigf06_ens_mem',i3.3)



!    allocate necessary space on global grid
     call gsi_gridcreate(atm_grid,grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig)
     call gsi_bundlecreate(atm_bundle,atm_grid,'aux-atm-read',istatus,names2d=vars2d,names3d=vars3d)
     if(istatus/=0) then
       write(6,*)myname,': trouble creating atm_bundle'
       call stop2(999)
     endif

     if(use_gfs_nemsio)then
        call general_read_gfsatm_nems(grd_gfst,sp_gfs,filename,uv_hyb_ens,.false.,.true., &
               atm_bundle,.true.,iret)
     else if (use_gfs_ncio) then
        call general_read_gfsatm_nc(grd_gfst,sp_gfs,filename,uv_hyb_ens,.false.,.true., &
               atm_bundle,.true.,iret)
     else
        call general_read_gfsatm(grd_gfst,sp_gfs,sp_gfs,filename,uv_hyb_ens,.false.,.true., &
               atm_bundle,inithead,iret)
     end if
     inithead = .false.

     ier = 0
     call gsi_bundlegetpointer(atm_bundle,'vor' ,vor ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'div' ,div ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'u'   ,u   ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'v'   ,v   ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'tv'  ,tv  ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'q'   ,q   ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'oz'  ,oz  ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'cw'  ,cwmr,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'z'   ,z   ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'ps'  ,ps  ,istatus) ; ier = ier + istatus
     if ( ier /= 0 ) call die(myname,': missing atm_bundle vars, aborting ...',ier)

     allocate(work_sub(grd_gfs%inner_vars,grd_gfs%lat2,grd_gfs%lon2,num_fields))
     do k=1,grd_gfs%nsig
        ku=k ; kv=k+grd_gfs%nsig ; kt=k+2*grd_gfs%nsig ; kq=k+3*grd_gfs%nsig ; koz=k+4*grd_gfs%nsig
        kcw=k+5*grd_gfs%nsig
        do j=1,grd_gfs%lon2
           do i=1,grd_gfs%lat2
              work_sub(1,i,j,ku)=u(i,j,k)
              work_sub(1,i,j,kv)=v(i,j,k)
              work_sub(1,i,j,kt)=tv(i,j,k)
              work_sub(1,i,j,kq)=q(i,j,k)
              work_sub(1,i,j,koz)=oz(i,j,k)
              work_sub(1,i,j,kcw)=cwmr(i,j,k)
           end do
        end do
     end do

     
     kz=num_fields ; kps=kz-1
     do j=1,grd_gfs%lon2
        do i=1,grd_gfs%lat2
           work_sub(1,i,j,kz)=z(i,j)
           work_sub(1,i,j,kps)=ps(i,j)
        end do
     end do

     call gsi_bundledestroy(atm_bundle,istatus)

     allocate(work(grd_gfs%inner_vars,grd_gfs%nlat,grd_gfs%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc))
     call general_sub2grid(grd_gfs,work_sub,work)
     deallocate(work_sub)

!    then interpolate to regional analysis grid
     allocate(work_reg(grd_mix%inner_vars,grd_mix%nlat,grd_mix%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc))
     do k=grd_gfs%kbegin_loc,grd_gfs%kend_loc
        call g_egrid2points_faster(p_g2r,work(1,1,1,k),work_reg(1,1,1,k),vector(k))
     end do
     deallocate(work)

!    next general_grid2sub to go to regional grid subdomains.
     allocate(work_sub(grd_mix%inner_vars,grd_mix%lat2,grd_mix%lon2,num_fields))
     call general_grid2sub(grd_mix,work_reg,work_sub)
     deallocate(work_reg)
     allocate(pri(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig+1))
     kz=num_fields ; kps=kz-1
!    compute 3d pressure on interfaces
     kap1=rd_over_cp+one
     kapr=one/rd_over_cp
     pri=zero
     k=1
     k2=grd_mix%nsig+1
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           pri(i,j,k)=work_sub(1,i,j,kps)
           pri(i,j,k2)=zero
        end do
     end do
     if (idvc /= 3) then
        do k=2,grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 pri(i,j,k)=ak5(k)+bk5(k)*work_sub(1,i,j,kps)
              end do
           end do
        end do
     else
        do k=2,grd_mix%nsig
           kt=k+2*grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 trk=(half*(work_sub(1,i,j,kt-1)+work_sub(1,i,j,kt))/tref5(k))**kapr
                 pri(i,j,k)=ak5(k)+(bk5(k)*work_sub(1,i,j,kps))+(ck5(k)*trk)
              end do
           end do
        end do
     end if

!    Get 3d pressure field now on interfaces
     allocate(prsl(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     if (idsl5/=2) then
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              do k=1,grd_mix%nsig
                 prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+1)**kap1)/&
                           (kap1*(pri(i,j,k)-pri(i,j,k+1))))**kapr
              end do
           end do
        end do
     else
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              do k=1,grd_mix%nsig
                 prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+1))*half
              end do
           end do
        end do
     end if
!  !Compute geopotential height at interface between layers
     allocate(zbarl(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     allocate(height(grd_mix%nsig))
     rdog=rd/grav
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           k  = 1
           kt=k+2*grd_mix%nsig
           h  = rdog * work_sub(1,i,j,kt)
           dz = h * log(pri(i,j,k)/prsl(i,j,k))
           height(k) = work_sub(1,i,j,kz)+dz

           do k=2,grd_mix%nsig
              kt=k+2*grd_mix%nsig
              h  = rdog * half * (work_sub(1,i,j,kt-1)+work_sub(1,i,j,kt))
              dz = h * log(prsl(i,j,k-1)/prsl(i,j,k))
              height(k) = height(k-1) + dz
           end do
           do k=1,grd_mix%nsig
              zbarl(i,j,k)=height(k)
           end do
        end do
     end do
     deallocate(pri,height)
!! recompute pbar using routine Wan-Shu obtained from Matt Pyle:

     allocate(tt(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     allocate(psfc_out(grd_mix%lat2,grd_mix%lon2))
     do k=1,grd_mix%nsig
        kt=k+2*grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              tt(i,j,k)=work_sub(1,i,j,kt)
           end do
        end do
     end do
     mm1=mype+1
     ! !ilook=ide/2
     ! !jlook=jde/2
     ! !ilook=29
     ! !jlook=41
     ilook=-1 ; jlook=-1
     allocate(prsl1000(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     prsl1000=1000._r_kind*prsl
     call compute_nmm_surfacep ( ges_z_ens(:,:), zbarl,prsl1000, &
                                 psfc_out,grd_mix%nsig,grd_mix%lat2,grd_mix%lon2, &
                                 ilook,jlook)
     deallocate(tt,zbarl,prsl1000)
     psfc_out=.001_r_kind*psfc_out
     !   psfc_out=ges_ps(:,:)
     !   write(6,*)' min,max ges_ps-psfc_out=',&
     !        minval(ges_ps(:,:)-psfc_out),maxval(ges_ps(:,:)-psfc_out)
     !               pdiffmax=-huge(pdiffmax)
     !               pdiffmin= huge(pdiffmin)
     !   !  do j=2,grd_mix%lon2-1
     !   !     do i=2,grd_mix%lat2-1
     !      do j=1,grd_mix%lon2
     !         do i=1,grd_mix%lat2
     !            pdiffmax=max(ges_ps(i,j)-psfc_out(i,j),pdiffmax)
     !            pdiffmin=min(ges_ps(i,j)-psfc_out(i,j),pdiffmin)
     !            if(ges_ps(i,j)<10._r_kind) &
     !               write(6,*)' small ges_ps,i,j,lat2,lon2,ig,jg,ide,jde=',i,j,grd_mix%lat2,grd_mix%lon2,&
     !                   grd_mix%istart(mm1)-2+i,grd_mix%jstart(mm1)-2+j,grd_mix%nlat,grd_mix%nlon
     !            if(psfc_out(i,j)<10._r_kind) &
     !               write(6,*)' small ens ps,i,j,lat2,lon2,ig,jg,ide,jde=',i,j,grd_mix%lat2,grd_mix%lon2,&
     !                   grd_mix%istart(mm1)-2+i,grd_mix%jstart(mm1)-2+j,grd_mix%nlat,grd_mix%nlon
     !         end do
     !      end do
     !      call mpi_allreduce(pdiffmax,pdiffmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
     !      call mpi_allreduce(pdiffmin,pdiffmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     !             if(mype==0) write(6,*)' min,max ges_ps - matt ps =',pdiffmin0,pdiffmax0

     !                                                write(fname,'("matt_pbar_corrected")')
     !                                                call grads1a(psfc_out,1,mype,trim(fname))
     !                                                write(fname,'("ges_ps")')
     !                                                call grads1a(ges_ps(:,:),1,mype,trim(fname))


! If not using Q perturbations, convert to RH
     if (.not.q_hyb_ens) then
        allocate(tsen(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
        allocate(qs(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
!    Compute RH and potential virtual temp
!    First step is go get sensible temperature and 3d pressure
        do k=1,grd_mix%nsig
           kt=k+2*grd_mix%nsig ; kq=k+3*grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 tsen(i,j,k)= work_sub(1,i,j,kt)/(one+fv*max(zero,work_sub(1,i,j,kq)))
              end do
           end do 
        end do

        ice=.true.
        iderivative=0
        call genqsat(qs,tsen,prsl,grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,ice,iderivative)

        do k=1,grd_mix%nsig
           kt=k+2*grd_mix%nsig ; kq=k+3*grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 work_sub(1,i,j,kq) = work_sub(1,i,j,kq)/qs(i,j,k)
              end do
           end do
        end do
        deallocate(qs,tsen)
     end if
     do k=1,grd_mix%nsig
        kt=k+2*grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              work_sub(1,i,j,kt)=work_sub(1,i,j,kt)/(0.01_r_kind*prsl(i,j,k))**rd_over_cp
           end do
        end do
     end do

     deallocate(prsl)

     iimax=0
     iimin=grd_mix%nlat
     jjmax=0
     jjmin=grd_mix%nlon
     ratio_x=(nlon-one)/(grd_mix%nlon-one)
     ratio_y=(nlat-one)/(grd_mix%nlat-one)
     if(.not. fv3_regional)then
        do k=1,grd_mix%nsig
           ku=k ; kv=ku+grd_mix%nsig ; kt=kv+grd_mix%nsig ; kq=kt+grd_mix%nsig ; koz=kq+grd_mix%nsig
           kcw=koz+grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2

                 ii=i+grd_mix%istart(mm1)-2
                 jj=j+grd_mix%jstart(mm1)-2
                 ii=min(grd_mix%nlat,max(1,ii))
                 jj=min(grd_mix%nlon,max(1,jj))
                 iimax=max(ii,iimax)
                 iimin=min(ii,iimin)
                 jjmax=max(jj,jjmax)
                 jjmin=min(jj,jjmin)
                 dlon_ens=real(jj,r_kind)
                 dlat_ens=real(ii,r_kind)
                 dlon=one+(dlon_ens-one)*ratio_x
                 dlat=one+(dlat_ens-one)*ratio_y
                 call rotate_wind_ll2xy(work_sub(1,i,j,ku),work_sub(1,i,j,kv), &
                                        uob,vob,region_lon_ens(ii,jj),dlon,dlat)
                 st_eg(i,j,k,n)=uob
                 vp_eg(i,j,k,n)=vob
                  t_eg(i,j,k,n)=work_sub(1,i,j,kt)     !  now pot virtual temp
                 rh_eg(i,j,k,n)=work_sub(1,i,j,kq)     !  now rh
                 oz_eg(i,j,k,n)=work_sub(1,i,j,koz)
                 cw_eg(i,j,k,n)=work_sub(1,i,j,kcw)
              end do
           end do
        end do
     else
        do k=1,grd_mix%nsig
           ku=k ; kv=ku+grd_mix%nsig ; kt=kv+grd_mix%nsig ; kq=kt+grd_mix%nsig ; koz=kq+grd_mix%nsig
           kcw=koz+grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 st_eg(i,j,k,n)=work_sub(1,i,j,ku)
                 vp_eg(i,j,k,n)=work_sub(1,i,j,kv)
                  t_eg(i,j,k,n)=work_sub(1,i,j,kt)     !  now pot virtual temp
                 rh_eg(i,j,k,n)=work_sub(1,i,j,kq)     !  now rh
                 oz_eg(i,j,k,n)=work_sub(1,i,j,koz)
                 cw_eg(i,j,k,n)=work_sub(1,i,j,kcw)
              end do
           end do
        end do
     endif
     kz=num_fields ; kps=kz-1
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           p_eg_nmmb(i,j,n)=psfc_out(i,j)
        end do
     end do
     deallocate(work_sub,psfc_out)

!                    pdiffmax=-huge(pdiffmax)
!                    pdiffmin= huge(pdiffmin)
!           do j=1,grd_mix%lon2
!              do i=1,grd_mix%lat2
!                 pdiffmax=max(ges_ps(i,j)-p_eg_nmmb(i,j,n),pdiffmax)
!                 pdiffmin=min(ges_ps(i,j)-p_eg_nmmb(i,j,n),pdiffmin)
!                  if(ges_ps(i,j)<10._r_kind) &
!                     write(6,*)' small ges_ps,i,j,lat2,lon2,ig,jg,ide,jde=',i,j,grd_mix%lat2,grd_mix%lon2,&
!                         grd_mix%istart(mm1)-1+i,grd_mix%jstart(mm1)-1+j,grd_mix%nlat,grd_mix%nlon
!                  if(p_eg_nmmb(i,j,n)<10._r_kind) &
!                     write(6,*)' small ens ps,i,j,lat2,lon2,ig,jg,ide,jde=',i,j,grd_mix%lat2,grd_mix%lon2,&
!                         grd_mix%istart(mm1)-1+i,grd_mix%jstart(mm1)-1+j,grd_mix%nlat,grd_mix%nlon
!              end do
!           end do
!           call mpi_allreduce(pdiffmax,pdiffmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!           call mpi_allreduce(pdiffmin,pdiffmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!                   if(mype==0) write(6,*)' with halo, n,min,max ges_ps - matt ps =',n,pdiffmin0,pdiffmax0

  end do   !  end loop over ensemble members.

  deallocate(ges_z_ens)

!   next, compute mean of ensembles.

  allocate(stbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(vpbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate( tbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(rhbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(ozbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(cwbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(pbar_nmmb(grd_mix%lat2,grd_mix%lon2))

!   compute mean state
  stbar=zero ; vpbar=zero ; tbar=zero ; rhbar=zero ; ozbar=zero ; cwbar=zero 
  pbar_nmmb=zero
  do n=1,n_ens_gfs
     do k=1,grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              stbar(i,j,k)=stbar(i,j,k)+st_eg(i,j,k,n)
              vpbar(i,j,k)=vpbar(i,j,k)+vp_eg(i,j,k,n)
               tbar(i,j,k)= tbar(i,j,k)+ t_eg(i,j,k,n)
              rhbar(i,j,k)=rhbar(i,j,k)+rh_eg(i,j,k,n)
              ozbar(i,j,k)=ozbar(i,j,k)+oz_eg(i,j,k,n)
              cwbar(i,j,k)=cwbar(i,j,k)+cw_eg(i,j,k,n)
           end do
        end do
     end do
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           pbar_nmmb(i,j)=pbar_nmmb(i,j)+p_eg_nmmb(i,j,n)
        end do
     end do
  end do

! Convert to mean
  bar_norm = one/real(n_ens_gfs,r_kind)
  do k=1,grd_mix%nsig
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           stbar(i,j,k)=stbar(i,j,k)*bar_norm
           vpbar(i,j,k)=vpbar(i,j,k)*bar_norm
            tbar(i,j,k)= tbar(i,j,k)*bar_norm
           rhbar(i,j,k)=rhbar(i,j,k)*bar_norm
           ozbar(i,j,k)=ozbar(i,j,k)*bar_norm
           cwbar(i,j,k)=cwbar(i,j,k)*bar_norm
        end do
     end do
  end do
  do j=1,grd_mix%lon2
     do i=1,grd_mix%lat2
        pbar_nmmb(i,j)=pbar_nmmb(i,j)*bar_norm
!   also save pbar to module array ps_bar for possible use in vertical localization
!                                                    in terms of scale heights/normalized p/p
        ps_bar(i,j,1)=pbar_nmmb(i,j)
     end do
  end do
!                                                 write(fname,'("test_pbar_uncorrected")')
!                                                 call grads1a(pbar,1,mype,trim(fname))
!                                                 write(fname,'("test_ges_ps")')
!                                                 call grads1a(ges_ps,1,mype,trim(fname))
!                                                 write(fname,'("test_ges_z")')
!                                                 call grads1a(ges_z,1,mype,trim(fname))

! Subtract mean from ensemble members, but save scaling by sqrt(1/(nens-1)) until after vertical interpolation
  n1=1
!www  ensemble perturbation for all but the first member if full_ensemble
  if(full_ensemble)n1=2

  do n=n1,n_ens_gfs
     do k=1,grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              st_eg(i,j,k,n)=st_eg(i,j,k,n)-stbar(i,j,k)
              vp_eg(i,j,k,n)=vp_eg(i,j,k,n)-vpbar(i,j,k)
               t_eg(i,j,k,n)= t_eg(i,j,k,n)- tbar(i,j,k)
              rh_eg(i,j,k,n)=rh_eg(i,j,k,n)-rhbar(i,j,k)
              oz_eg(i,j,k,n)=oz_eg(i,j,k,n)-ozbar(i,j,k)
              cw_eg(i,j,k,n)=cw_eg(i,j,k,n)-cwbar(i,j,k)
           end do
        end do
     end do
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           p_eg_nmmb(i,j,n)=p_eg_nmmb(i,j,n)-pbar_nmmb(i,j)
        end do
     end do
  end do
  deallocate(stbar,vpbar,rhbar,ozbar,cwbar)

! now obtain mean pressure prsl
! compute 3d pressure on interfaces
  kap1=rd_over_cp+one
  kapr=one/rd_over_cp
  allocate(pri(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig+1))
  pri=zero
  k=1
  k2=grd_mix%nsig+1
  do j=1,grd_mix%lon2
     do i=1,grd_mix%lat2
        pri(i,j,k)=pbar_nmmb(i,j)
        pri(i,j,k2)=zero
     end do
  end do
  if (idvc /= 3) then
     do k=2,grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              pri(i,j,k)=ak5(k)+bk5(k)*pbar_nmmb(i,j)
           end do
        end do
     end do
  else
     do k=2,grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              trk=(half*(tbar(i,j,k-1)+tbar(i,j,k))/tref5(k))**kapr
              pri(i,j,k)=ak5(k)+(bk5(k)*pbar_nmmb(i,j))+(ck5(k)*trk)
           end do
        end do
     end do
  end if

! Get 3d pressure field now at layer midpoints
  allocate(prsl(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  if (idsl/=2) then
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           do k=1,grd_mix%nsig
              prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+1)**kap1)/&
                        (kap1*(pri(i,j,k)-pri(i,j,k+1))))**kapr
           end do
        end do
     end do
  else
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           do k=1,grd_mix%nsig
              prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+1))*half
           end do
        end do
     end do
  end if
  deallocate(pri,pbar_nmmb,tbar)
  deallocate(ak5,bk5,ck5,tref5)

! interpolate/extrapolate in vertical using yoshi's spline code.

!  first need ges_prsl_e, the 3d pressure on the ensemble grid.

  allocate(ges_prsl_e(grd_ens%inner_vars,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  if(dual_res) then
     call general_suba2sube(grd_a1,grd_e1,p_e2a,ges_prsl(:,1,1,it),ges_prsl_e(1,:,1,1),regional) ! x?
  else
     ges_prsl_e(1,:,:,:)=ges_prsl(:,:,:,it)
  end if

  allocate(xspli(grd_mix%nsig),yspli(grd_mix%nsig),xsplo(grd_ens%nsig),ysplo(grd_ens%nsig))

  allocate(ut(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(vt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(tt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(rht(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(ozt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(cwt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  do n=1,n_ens_gfs
     do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
           do k=1,grd_mix%nsig
              xspli(k)=log(prsl(i,j,k)*10.0_r_kind)
           end do
           do k=1,grd_ens%nsig
              xsplo(k)=log(ges_prsl_e(1,i,j,k)*10._r_kind)
           end do

!    u
           do k=1,grd_mix%nsig
              yspli(k)=st_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              ut(i,j,k)=ysplo(k)
           end do
!    v
           do k=1,grd_mix%nsig
              yspli(k)=vp_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              vt(i,j,k)=ysplo(k)
           end do
!    t
           do k=1,grd_mix%nsig
              yspli(k)=t_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              ysplo(k)=ysplo(k)*(0.01_r_kind*ges_prsl_e(1,i,j,k))**rd_over_cp  ! converting from pot Tv to Tv
              tt(i,j,k)=ysplo(k)
           end do
!    rh
           do k=1,grd_mix%nsig
              yspli(k)=rh_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              rht(i,j,k)=ysplo(k)
           end do
!       oz
           do k=1,grd_mix%nsig
              yspli(k)=oz_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              ozt(i,j,k)=ysplo(k)
           end do
!    cw
           do k=1,grd_mix%nsig
              yspli(k)=cw_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              cwt(i,j,k)=ysplo(k)
           end do

        end do
     end do

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
     if(n==1 .and. full_ensemble)then

        allocate(qs(lat2,lon2,nsig))
        ice=.true.
        iderivative=0
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 qs(i,j,k)=ges_q(i,j,k)
              end do
           end do
        end do
        call genqsat(qs,ges_tsen(:,:,:,it),ges_prsl(:,:,:,it),lat2,lon2,nsig,ice,iderivative)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!! The first member is full perturbation based on regional first guess !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! put fist guess in ensemble grid & Subtract guess from 1st ensemble member (ensemble mean)

        if (dual_res) then
           allocate ( tmp_ens(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,1) )
           allocate ( tmp_ens2(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,1) )
           allocate ( tmp_anl(lat2,lon2,nsig,1) )

           if (.not.q_hyb_ens) then
              tmp_anl(:,:,:,1)=qs(:,:,:)
              call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
              tmp_anl(:,:,:,1)=ges_q(:,:,:)
              call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens2,regional)
              rht(:,:,:) = rht(:,:,:)-tmp_ens2(:,:,:,1)/tmp_ens(:,:,:,1)
           else
              tmp_anl(:,:,:,1)=ges_q(:,:,:)
              call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens2,regional)
              rht(:,:,:) = rht(:,:,:)-tmp_ens2(:,:,:,1)
           end if

           tmp_anl(:,:,:,1)=ges_u(:,:,:)
           call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
           ut(:,:,:) = ut(:,:,:)-tmp_ens(:,:,:,1)
           tmp_anl(:,:,:,1)=ges_v(:,:,:)
           call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
           vt(:,:,:) = vt(:,:,:)-tmp_ens(:,:,:,1)
           tmp_anl(:,:,:,1)=ges_tv(:,:,:)
           call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
           tt(:,:,:) = tt(:,:,:)-tmp_ens(:,:,:,1)
           tmp_anl(:,:,1,1)=ges_ps(:,:)
           call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
           p_eg_nmmb(:,:,n) = p_eg_nmmb(:,:,n)-tmp_ens(:,:,1,1)
           deallocate(tmp_anl,tmp_ens,tmp_ens2)
        else
           do k=1,grd_ens%nsig
              do j=1,grd_ens%lon2
                 do i=1,grd_ens%lat2
                    ut(i,j,k) = ut(i,j,k)-ges_u(i,j,k)
                    vt(i,j,k) = vt(i,j,k)-ges_v(i,j,k)
                    tt(i,j,k) = tt(i,j,k)-ges_tv(i,j,k)
                 end do
              end do
           end do

           if (.not.q_hyb_ens) then
              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       rht(i,j,k) = rht(i,j,k)-ges_q(i,j,k)/qs(i,j,k)
                    end do
                 end do
              end do
           else
              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       rht(i,j,k) = rht(i,j,k)-ges_q(i,j,k)
                    end do
                 end do
              end do
           end if

           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 p_eg_nmmb(i,j,n) = p_eg_nmmb(i,j,n)-ges_ps(i,j)
              end do
           end do
        endif
        deallocate(qs)

     endif   ! n==1 .and. full_ensemble

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

!   transfer from temporary arrays to perturbation arrays and normalize by sig_norm

! sig_norm from the following
! 2*J_b = x^T * (beta1*B + beta2*P_ens)^(-1) * x
! where  P_ens is the ensemble covariance which is the sum of outer products of the
! ensemble perturbations (unnormalized) divided by n_ens-1  (or n_ens, depending on who you read).
     sig_norm=sqrt(weight_ens_gfs/max(one,n_ens_temp-one))

!     if(n_ens_temp==n_ens.and.n==n_ens+1) sig_norm=one
!                                                  if(n==1 .or. n==2 .or. n==50) then
!                                                      write(fname,'("test_pp_",i2.2)')n
!                                                      call grads1a(p_eg_nmmb(1,1,n),1,mype,trim(fname))
!                                                      write(fname,'("test_up_",i2.2)')n
!                                                      call grads1a(ut,grd_ens%nsig,mype,trim(fname))
!                                                      write(fname,'("test_vp_",i2.2)')n
!                                                      call grads1a(vt,grd_ens%nsig,mype,trim(fname))
!                                                      write(fname,'("test_tp_",i2.2)')n
!                                                      call grads1a(tt,grd_ens%nsig,mype,trim(fname))
!                                                      write(fname,'("test_rhp_",i2.2)')n
!                                                      call grads1a(rht,grd_ens%nsig,mype,trim(fname))
!!                                                      write(fname,'("test_ozp_",i2.2)')n
!!                                                      call grads1a(ozt,grd_ens%nsig,mype,trim(fname))
!!                                                      write(fname,'("test_cwp_",i2.2)')n
!!                                                      call grads1a(cwt,grd_ens%nsig,mype,trim(fname))
!                                                  end if
     do ic3=1,nc3d

        if(ntlevs_ens > 1) then
           call gsi_bundlegetpointer(en_perts(n,1,it),trim(cvars3d(ic3)),w3,istatus)
        else
           call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars3d(ic3)),w3,istatus)
        endif
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
           call stop2(999)
        end if

        select case (trim(cvars3d(ic3)))

           case('sf','SF')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = ut(i,j,k)*sig_norm
                    end do
                 end do
              end do

           case('vp','VP')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = vt(i,j,k)*sig_norm
                    end do
                 end do
              end do

           case('t','T')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = tt(i,j,k)*sig_norm
                    end do
                 end do
              end do

           case('q','Q')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = rht(i,j,k)*sig_norm
                    end do
                 end do
              end do

           case('oz','OZ')
!          temporarily ignore ozone perturbations

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                   !   w3(i,j,k) = ozt(i,j,k)*sig_norm
                       w3(i,j,k) = zero
                    end do
                 end do
              end do

           case('cw','CW')
!          open cloud water perturbations for regional analysis

              if(l_wcp_cwm) then
                 do k=1,grd_ens%nsig
                    do j=1,grd_ens%lon2
                       do i=1,grd_ens%lat2
                          w3(i,j,k) = cwt(i,j,k)*sig_norm
                       end do
                    end do
                 end do
              else
                 do k=1,grd_ens%nsig
                    do j=1,grd_ens%lon2
                       do i=1,grd_ens%lat2
                          w3(i,j,k) = zero
                       end do
                    end do
                 end do
              endif

        end select
     end do
     do ic2=1,nc2d

        if(ntlevs_ens > 1) then
           call gsi_bundlegetpointer(en_perts(n,1,it),trim(cvars2d(ic2)),w2,istatus)
        else
           call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars2d(ic2)),w2,istatus)
        endif
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
           call stop2(999)
        end if

        select case (trim(cvars2d(ic2)))

           case('ps','PS')

              do j=1,grd_ens%lon2
                 do i=1,grd_ens%lat2
                    w2(i,j) = p_eg_nmmb(i,j,n)*sig_norm
                 end do
              end do

           case('sst','SST')

! dtk: temporarily ignore sst perturbations in hybrid
              do j=1,grd_ens%lon2
                 do i=1,grd_ens%lat2
                    w2(i,j) = zero
                 end do
              end do

        end select
     end do
  end do

  call general_sub2grid_destroy_info(grd_gfs)
  call general_sub2grid_destroy_info(grd_mix)
  call general_sub2grid_destroy_info(grd_gfst)
!
!
! CALCULATE ENSEMBLE SPREAD
  if(write_ens_sprd)then
     call mpi_barrier(mpi_comm_world,ierror)
     call wrf_mass_ensperts%ens_spread_dualres_regional(mype,en_perts,nelen)
     call mpi_barrier(mpi_comm_world,ierror)
  end if

  call general_destroy_spec_vars(sp_gfs)
  deallocate(vector)
  deallocate(st_eg,vp_eg,t_eg,rh_eg)
  deallocate(oz_eg,cw_eg,p_eg_nmmb)
  deallocate(ges_prsl_e)
  deallocate(xspli,yspli,xsplo,ysplo)
  deallocate(prsl)
  deallocate(ut,vt,tt,rht,ozt,cwt)

  enddo ! it=1,ntlevs_ens
  return

30 write(6,*) 'GET_GEFS+FOR_REGIONAL open filelist failed '
   call stop2(555)
20 write(6,*) 'GET_GEFS+FOR_REGIONAL read gfs ens failed ',n
   call stop2(555)
end subroutine get_gefs_for_regional

  SUBROUTINE compute_nmm_surfacep ( TERRAIN_HGT_T, Z3D_IN, PRESS3D_IN,   &
                                    psfc_out,generic,IME,JME, Ilook,Jlook )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compute_nmm_surfacep  obtain nmm surface pressure
!   prgmmr: pyle             org: np22                date: 2010-09-26
!
! abstract: using model terrain height and 3d fields of height, pressure and temperature,
!             compute pressure at the model terrain height.
!
!
! program history log:
!   2010-09-26  pyle
!   2013-02-15  parrish -- change DO L=generic,2,-1 to DO L=generic-1,2,-1 to prevent
!                            out of bounds array reference in array Z3D_IN.
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


       use kinds, only: r_kind,i_kind
       IMPLICIT NONE

       integer(i_kind),intent(in) :: IME,JME
       integer(i_kind),intent(in) :: Ilook,Jlook
       integer(i_kind),intent(in) :: generic

       real(r_kind),intent(in) :: TERRAIN_HGT_T(IME,JME)
       real(r_kind),intent(in) :: Z3D_IN(IME,JME,generic)
       real(r_kind),intent(in) :: PRESS3D_IN(IME,JME,generic)
       real(r_kind),intent(out) :: psfc_out(IME,JME)

       integer(i_kind) :: I,J,L,LL
       integer(i_kind) :: loopinc,iloopinc

       real(r_kind) :: PSFC_IN(IME,JME),TOPO_IN(IME,JME)
       real(r_kind) :: dlnpdz,BOT_INPUT_HGT,BOT_INPUT_PRESS

       real(r_kind), allocatable:: dum2d(:,:),DUM2DB(:,:)

       logical :: DEFINED_PSFC(IME,JME), DEFINED_PSFCB(IME,JME)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!	write(6,*) 'size(TERRAIN_HGT_T):: ', size(TERRAIN_HGT_T,dim=1),size(TERRAIN_HGT_T,dim=2)
!	write(6,*) 'what is JME here??? : ', JME
!	write(6,*) 'JmE: ', JmE

       DO J=1,JME
          DO I=1,IME
             DEFINED_PSFC(I,J)=.FALSE.
             DEFINED_PSFCB(I,J)=.FALSE.
             IF (PRESS3D_IN(I,J,1) /= 200100._r_kind) THEN
                PSFC_IN(I,J)=PRESS3D_IN(I,J,1)
                TOPO_IN(I,J)=Z3D_IN(I,J,1)
             ELSE
                PSFC_IN(I,J)=PRESS3D_IN(I,J,2)
                TOPO_IN(I,J)=Z3D_IN(I,J,2)
             ENDIF
          ENDDO
       ENDDO

!       write(6,*) 'terrain_hgt_t in surfacep compute ', IME,JME
!       do J=JME,1,min(-(JME-1)/20,-1)
!          write(6,535) J,(TERRAIN_HGT_T(I,J),I=1,IME,max(1,(IME-1)/12))
!       enddo

!       write(6,*) 'z3d_in(3) at same points:'
!       do J=JME,1,min(-(JME-1)/20,-1)
!          write(6,535) J,(Z3D_IN(I,J,3),I=1,IME,max(1,(IME-1)/12))
!       enddo
! 535	format(I4,' ::: ',18(f5.0,1x))

       ALLOCATE(DUM2D(IME,JME))
     
       DO J=1,JME
          DO I=1,IME
             DUM2D(I,J)=-9._r_kind
          END DO
       END DO

       DO J=1,JmE
          I_loop: DO I=1,ImE

             IF (PSFC_IN(I,J) == 0._r_kind) THEN
                write(6,*) 'QUITTING BECAUSE I,J, PSFC_IN: ', I,J,PSFC_IN(I,J)

                STOP
             ENDIF

             BOT_INPUT_PRESS=PSFC_IN(I,J)
             BOT_INPUT_HGT=TOPO_IN(I,J)


             IF (I == Ilook .AND. J == Jlook) THEN

!	         write(6,*) ' TERRAIN_HGT_T: ', I,J, TERRAIN_HGT_T(I,J)
                write(6,*) ' PSFC_IN, TOPO_IN: ', &
                   I, J, PSFC_IN(I,J),TOPO_IN(I,J)

                DO L=1,generic
                   write(6,*) ' L,PRESS3D_IN, Z3D_IN: ', &
                      I,J,L, PRESS3D_IN(I,J,L),Z3D_IN(I,J,L)
                END DO
             ENDIF

!             do L=2,generic
             DO L=generic-1,2,-1

                IF ( PRESS3D_IN(i,j,L) > PSFC_IN(I,J) .AND.  &
                         Z3D_IN(I,J,L) < TERRAIN_HGT_T(I,J) .AND. &
                         Z3D_IN(I,J,L+1) > TERRAIN_HGT_T(I,J) ) THEN

                   BOT_INPUT_PRESS=PRESS3D_IN(i,j,L)
                   BOT_INPUT_HGT=Z3D_IN(I,J,L)
 
                   IF (I == Ilook .and. J == Jlook) THEN
                      write(6,*) 'BOT_INPUT_PRESS, BOT_INPUT_HGT NOW : ', &
                         Ilook,Jlook, BOT_INPUT_PRESS, BOT_INPUT_HGT
                   ENDIF

                ENDIF 
             END DO	

             IF (I == Ilook .and. J == Jlook) THEN
                write(6,*) 'enter this section, TERRAIN_HGT_T, BOT_INPUT_HGT: ', TERRAIN_HGT_T(I,J), BOT_INPUT_HGT
             ENDIF

             IF (TERRAIN_HGT_T(I,J) == BOT_INPUT_HGT ) THEN
                dum2d(I,J)=BOT_INPUT_PRESS
                DEFINED_PSFC(I,J)=.TRUE.
                IF (I == Ilook .and. J == Jlook) THEN
                   write(6,*) 'TERRAIN_HGT_T == BOT_INPUT_HGT, set dum2d to: ', I,J, dum2d(I,J)
                ENDIF

	!        IF (BOT_INPUT_HGT /= 0._r_kind .and. (BOT_INPUT_HGT-INT(BOT_INPUT_HGT) /= 0._r_kind) ) THEN
	!           write(6,*) 'with BOT_INPUT_HGT: ', BOT_INPUT_HGT, &
        !              'set dum2d to bot_input_pres: ', I,J,dum2d(I,J)
        !        ENDIF

             ELSEIF (TERRAIN_HGT_T(I,J) < BOT_INPUT_HGT ) THEN

!         target is below lowest possible input...extrapolate

                IF ( BOT_INPUT_PRESS-PRESS3D_IN(I,J,2) > 500._r_kind ) THEN
                   dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,2)) ) / &
                      (BOT_INPUT_HGT-Z3D_IN(i,j,2))
                   IF (I == Ilook .and. J == Jlook) THEN
                      write(6,*) 'I,J,dlnpdz(a): ', I,J,dlnpdz
                   ENDIF

                ELSE

!! thin layer and/or just have lowest level - difference with 3rd level data
                   IF ( abs(BOT_INPUT_PRESS - PRESS3D_IN(i,j,3)) > 290._r_kind ) THEN

                      dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,3)) ) / &
                         (BOT_INPUT_HGT-Z3D_IN(i,j,3))

                      IF (I == Ilook .and. J == Jlook) then
                         write(6,*) 'p diff: ', BOT_INPUT_PRESS, PRESS3D_IN(i,j,3)
                         write(6,*) 'z diff: ', BOT_INPUT_HGT, Z3D_IN(i,j,3)
                      ENDIF
	
                   ELSE

!! Loop up to level 7 looking for a sufficiently thick layer

                      FIND_THICK:  DO LL=4,7
                         IF( abs(BOT_INPUT_PRESS - PRESS3D_IN(i,j,LL)) > 290._r_kind) THEN
                            dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,LL)) ) / &
                               (BOT_INPUT_HGT-Z3D_IN(i,j,LL))
                            EXIT FIND_THICK
                         ENDIF 
                      END DO FIND_THICK

                   ENDIF
        
                ENDIF

                dum2d(I,J)= exp(log(BOT_INPUT_PRESS) + dlnpdz * &
                    (TERRAIN_HGT_T(I,J) - BOT_INPUT_HGT) )

                DEFINED_PSFC(I,J)=.TRUE.

                IF (I == Ilook .and. J == Jlook) THEN
	           write(6,*) 'here(b) set dum2d to: ', I,J, dum2d(I,J)
                ENDIF

             ELSE ! target level bounded by input levels

                DO L=2,generic-1
                   IF (TERRAIN_HGT_T(I,J) > Z3D_IN(i,j,L) .AND. &
                       TERRAIN_HGT_T(I,J) < Z3D_IN(i,j,L+1) ) THEN
                      dlnpdz= (log(PRESS3D_IN(i,j,l))-log(PRESS3D_IN(i,j,L+1)) ) / &
                         (Z3D_IN(i,j,l)-Z3D_IN(i,j,L+1))
                      dum2d(I,J)= log(PRESS3D_IN(i,j,l)) +   &
                         dlnpdz * (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,L) )
                      dum2d(i,j)=exp(dum2d(i,j))
                      DEFINED_PSFC(I,J)=.TRUE.
                      IF (I == Ilook .and. J == Jlook) THEN
	                 write(6,*) 'here(c) set dum2d to: ', I,J, Dum2d(I,J)
                      ENDIF
                   ENDIF
                ENDDO

!!! account for situation where BOT_INPUT_HGT < TERRAIN_HGT_T < Z3D_IN(:,2,:)
                IF (dum2d(I,J) == -9._r_kind .AND. BOT_INPUT_HGT < TERRAIN_HGT_T(I,J) &
                   .AND. TERRAIN_HGT_T(I,J) < Z3D_IN(I,J,2)) then

          !         IF (mod(I,50) == 0 .AND. mod(J,50) == 0) THEN
          !            write(6,*) 'I,J,BOT_INPUT_HGT, bot_pres, TERRAIN_HGT_T: ',  &
          !               I,J,BOT_INPUT_HGT, BOT_INPUT_PRESS, TERRAIN_HGT_T(I,J)
          !         ENDIF

                   dlnpdz= (log(PSFC_IN(i,j))-log(PRESS3D_IN(i,j,2)) ) / &
                      (TOPO_IN(i,j)-Z3D_IN(i,j,2))
                   dum2d(I,J)= log(PSFC_IN(i,j)) +   &
                      dlnpdz * (TERRAIN_HGT_T(I,J) - TOPO_IN(i,j) )
                   dum2d(i,j)= exp(dum2d(i,j))
                   DEFINED_PSFC(I,J)=.TRUE.
                   IF (I == Ilook .and. J == Jlook) THEN
	              write(6,*) 'here(d) set dum2d to: ', I,J, Dum2d(I,J)
                   ENDIF
                ENDIF

                IF (dum2d(I,J) == -9._r_kind) THEN
                   write(6,*) 'must have flukey situation in new ', I,J
                   write(6,*) 'I,J,BOT_INPUT_HGT, bot_pres, TERRAIN_HGT_T: ',  &
                      I,J,BOT_INPUT_HGT, BOT_INPUT_PRESS, TERRAIN_HGT_T(I,J)

                   DO L=1,generic-1
                      IF ( TERRAIN_HGT_T(I,J) == Z3D_IN(i,j,L) ) THEN
! problematic with HGT_M substitution for "input" surface height?
                         dum2d(i,j)=PRESS3D_IN(I,J,L)
                         DEFINED_PSFC(I,J)=.TRUE.
                         IF (I == Ilook .and. J == Jlook) THEN
                            write(6,*) 'here(e) set dum2d to: ', I,J, Dum2d(I,J)
                         ENDIF
                      ENDIF
                   ENDDO

                   IF ( TERRAIN_HGT_T(I,J) == TOPO_IN(I,J)) THEN
                      dum2d(I,J)=PSFC_IN(I,J)
                      DEFINED_PSFC(I,J)=.TRUE.
                      IF (I == Ilook .and. J == Jlook) THEN
	                 write(6,*) 'here(f) set dum2d to: ', I,J, Dum2d(I,J)
                      ENDIF
         !             write(6,*) 'matched input topo, psfc: ', I,J,TOPO_IN(I,J),PSFC_IN(I,J)
                   ENDIF

!                   IF (dum2d(I,J) == -9._r_kind) THEN
!                   ENDIF 

                ENDIF

                if (.not. defined_psfc(i,J)) then
!                   write(6,*) 'switching to true here'
                   DEFINED_PSFC(I,J)=.TRUE.
                endif

                IF (I == Ilook .AND. J == Jlook) THEN
                   write(6,*) 'newstyle psfc: ', I,J,dum2d(I,J)
                ENDIF

             ENDIF 

             if (.not. DEFINED_PSFC(I,J)) then
!                write(6,*) 'new style undefined at: ', I,J
             endif

          ENDDO I_loop
       ENDDO

      !write(6,*) 'psfc points (new style)'
       loopinc=max( (JmE-1)/20,1)
       iloopinc=max( (ImE-1)/10,1)

       DO J=JmE,1,-loopinc
       !   write(6,633) (dum2d(I,J)/100.,I=1,min(ImE,ImE),iloopinc)
       END DO

  633  format(35(f5.0,1x))

!       write(6,*) 'PSFC extremes (new style): ',  minval(dum2d,MASK=DEFINED_PSFC),maxval(dum2d,MASK=DEFINED_PSFC)

!       IF (minval(dum2d,MASK=DEFINED_PSFC) < 40000._r_kind .or. maxval(dum2d,MASK=DEFINED_PSFC) > 110000._r_kind) THEN
!       ENDIF

!! "traditional" isobaric only approach ------------------------------------------------

       ALLOCATE (DUM2DB(IME,JME))
       DO J=1,JME
          DO I=1,IME
             DUM2DB(I,J)=-9._r_kind
          END DO
       END DO

       DO J=1,JmE
          DO I=1,ImE

             IF (TERRAIN_HGT_T(I,J) < Z3D_IN(i,j,2)) THEN ! targ below lowest

                IF ( abs(PRESS3D_IN(i,j,2)-PRESS3D_IN(i,j,3)) > 290._r_kind) THEN
                   dlnpdz= (log(PRESS3D_IN(i,j,2))-log(PRESS3D_IN(i,j,3)) ) / &
                      (Z3D_IN(i,j,2)-Z3D_IN(i,j,3))
                ELSE
                   dlnpdz= (log(PRESS3D_IN(i,j,2))-log(PRESS3D_IN(i,j,4)) ) / &
                      (Z3D_IN(i,j,2)-Z3D_IN(i,j,4))
                ENDIF

                DUM2DB(I,J)= exp( log(PRESS3D_IN(i,j,2)) + dlnpdz * &
                   (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,2)) )

                IF (I == Ilook .and. J == Jlook) THEN
                   write(6,*) 'I,K, trad: dlnpdz, press_in(2), terrain_t, Z3D_IN(2): ', I,J,dlnpdz, &
                      PRESS3D_IN(i,j,2), TERRAIN_HGT_T(I,J), Z3D_IN(i,j,2)
                ENDIF

                DEFINED_PSFCB(i,j)=.true.

             ELSEIF (TERRAIN_HGT_T(I,J) > Z3D_IN(i,j,2)) THEN ! target level bounded by input levels

                DO L=2,generic-1
                   IF (TERRAIN_HGT_T(I,J) > Z3D_IN(i,j,L) .AND. &
                       TERRAIN_HGT_T(I,J) < Z3D_IN(i,j,L+1) ) THEN

                      dlnpdz= (log(PRESS3D_IN(i,j,l))-log(PRESS3D_IN(i,j,L+1)) ) / &
                         (Z3D_IN(i,j,l)-Z3D_IN(i,j,L+1))

                      DUM2DB(I,J)= log(PRESS3D_IN(i,j,l)) +   &
                         dlnpdz * (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,L) )
                      DUM2DB(i,j)=exp(DUM2DB(i,j))
 
                      IF (I == Ilook .and. J == Jlook) THEN
                         write(6,*) 'L, L+1, p3d_in(L), p3d_in(L+1), z3d_in(L), z3d_in(L+1): ', &
                                   L, L+1, PRESS3D_IN(i,j,l), PRESS3D_IN(i,j,L+1), &
                                   Z3D_IN(i,j,l), Z3D_IN(i,j,L+1)
                         write(6,*) 'TERRAIN_HGT_T(I,J) , Z3D_IN(i,j,L): ', TERRAIN_HGT_T(I,J) , Z3D_IN(i,j,L)
                         write(6,*) 'here(2b) set dum2db to: ', I,J, Dum2db(I,J)
                      ENDIF

                      DEFINED_PSFCB(i,j)=.true.

                      IF (DUM2DB(I,J) < 13000._r_kind) THEN
           !              write(6,*) 'I,J,L,terrain,Z3d(L),z3d(L+1),p3d(L),p3d(l+1): ', I,J,L, &
           !                 TERRAIN_HGT_T(I,J),Z3D_IN(I,J,L),Z3D_IN(I,J,L+1),PRESS3D_IN(I,J,L), &
           !                 PRESS3D_IN(I,J,L+1)
                      ENDIF
                   ENDIF
                ENDDO

             ELSEIF (TERRAIN_HGT_T(I,J) == Z3D_IN(i,j,2)) THEN
                DUM2DB(i,j)=PRESS3D_IN(I,J,2)
                IF (I == Ilook .and. J == Jlook) THEN
                   write(6,*) 'here(2c) set dum2db to: ', I,J, Dum2db(I,J)
                ENDIF
                DEFINED_PSFCB(i,j)=.true.
             ENDIF

             IF (DUM2DB(I,J) == -9._r_kind) THEN
         !       write(6,*) 'must have flukey situation in trad ', I,J
                DO L=1,generic-1
                   IF ( TERRAIN_HGT_T(I,J) == Z3D_IN(i,j,L) ) THEN
                      DUM2DB(i,j)=PRESS3D_IN(I,J,L)
                      IF (I == Ilook .and. J == Jlook) THEN
	                 write(6,*) 'here(2d) set dum2db to: ', I,J, Dum2db(I,J)
                      ENDIF
                      DEFINED_PSFCB(i,j)=.true.
                   ENDIF
                ENDDO
             ENDIF

             IF (DUM2DB(I,J) == -9._r_kind) THEN
                write(6,*) 'HOPELESS PSFC, I QUIT'
             ENDIF

             if (I == Ilook .and. J == Jlook) THEN
                write(6,*) ' traditional psfc: ', I,J,DUM2DB(I,J)
             ENDIF

          ENDDO
       ENDDO

!       write(6,*) 'psfc points (traditional)'
!       DO J=JmE,1,-loopinc
!          write(6,633) (DUM2DB(I,J)/100.,I=1,ime,iloopinc)
!       ENDDO

!       write(6,*) 'PSFC extremes (traditional): ', minval(DUM2DB,MASK=DEFINED_PSFCB),maxval(DUM2DB,MASK=DEFINED_PSFCB)

!       IF (minval(DUM2DB,MASK=DEFINED_PSFCB) < 40000._r_kind .or. maxval(DUM2DB,MASK=DEFINED_PSFCB) > 108000._r_kind) THEN
!       ENDIF

!!!!! end traditional

       DO J=1,JmE
          DO I=1,ImE
             IF (DEFINED_PSFCB(I,J) .and. DEFINED_PSFC(I,J)) THEN

                IF (  abs(dum2d(I,J)-DUM2DB(I,J)) > 400._r_kind) THEN
	!          write(6,*) 'BIG DIFF I,J, dum2d, DUM2DB: ', I,J,dum2d(I,J),DUM2DB(I,J)
                ENDIF

!! do we have enough confidence in new style to give it more than 50% weight?
                psfc_out(I,J)=0.5_r_kind*(dum2d(I,J)+DUM2DB(I,J))
             ELSEIF (DEFINED_PSFC(I,J)) THEN
                psfc_out(I,J)=dum2d(I,J)
             ELSEIF (DEFINED_PSFCB(I,J)) THEN
                psfc_out(I,J)=DUM2DB(I,J)
             ELSE
         !       write(6,*) 'I,J,dum2d,DUM2DB: ', I,J,dum2d(I,J),DUM2DB(I,J)
	 !       write(6,*) 'I,J,DEFINED_PSFC(I,J),DEFINED_PSFCB(I,J): ', I,J,DEFINED_PSFC(I,J),DEFINED_PSFCB(I,J)
             ENDIF

             IF (I == Ilook .AND. J == Jlook) THEN
                write(6,*) ' combined psfc: ', I,J,psfc_out(I,J)
             ENDIF

             IF (psfc_out(I,J) < 50000._r_kind .or. psfc_out(I,J) > 108000._r_kind) THEN
 !               write(6,*) 'strange combo on psfc_out, terrain_hgt_t: ', I,J, psfc_out(I,J), terrain_hgt_t(I,J)
 !               write(6,*) 'DEFINED_PSFC, dum2d: ', DEFINED_PSFC(I,J),dum2d(I,J)
 !               write(6,*) 'DEFINED_PSFCB, DUM2DB: ', DEFINED_PSFCB(I,J),DUM2DB(I,J)

!                if (terrain_hgt_t(I,J) > 0._r_kind .and. terrain_hgt_t(I,J) < 5000._r_kind) then
!                else
!                   write(6,*) 'will let strange psfc pass because surface topo is: ', terrain_hgt_t(I,J)
!                endif

             ENDIF

          ENDDO
       ENDDO

      ! write(6,*) 'psfc points (final combined)'
       DO J=JmE,1,-loopinc
      !    write(6,633) (psfc_out(I,J)/100.,I=1,ime,iloopinc)
       ENDDO

       deallocate(dum2d,dum2db)

  END SUBROUTINE compute_nmm_surfacep


subroutine grads1a(f,nvert,mype,fname)

  use kinds, only: r_single,r_kind,i_kind
  use gridmod, only: nlat,nlon,lon2,lat2,rlats,rlons,regional
  use constants, only: rad2deg
  use general_sub2grid_mod, only: sub2grid_info,general_gather2grid,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_sub2grid_destroy_info
  implicit none

  integer(i_kind),intent(in):: nvert,mype
  character(*),intent(in):: fname
  real(r_single),intent(in):: f(1,lat2,lon2,nvert)

  real(r_single),dimension(1,nlat,nlon)::work
  real(r_single) outfield(nlon,nlat)

  character(50) dsname,title,filename
! data dsname/'test.dat'/
  data title/'inmi'/
  character(112) datdes(50000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33/

  integer(i_kind) i,k,kend,kstart,next,np,ioutdes,ioutdat
  integer(i_kind) last,j,koutmax
  real(r_single) undef
  real(r_single) startp,pinc
  real(r_single) rlons_deg(nlon)
  real(r_single) rlats_deg(nlat)
  type(sub2grid_info) s

  if(mype==0) then
     if(regional) then
        rlons_deg=rlons
        rlats_deg=rlats
     else
        rlons_deg=rad2deg*rlons
        rlats_deg=rad2deg*rlats
     end if
     np=nvert
     startp=1._r_single
     pinc=1._r_single
     ioutdes=98550
     ioutdat=98551
     write(filename,'(a,".des")')trim(fname)
     write(dsname,'(a,".dat")')trim(fname)
     open(unit=ioutdes,file=trim(filename),form='formatted')
     open(unit=ioutdat,file=trim(dsname),form='unformatted')
     rewind ioutdes
     rewind ioutdat
     do i=1,50000
        write(datdes(i),'(112a1)')(blank,k=1,112)
     end do
     write(datdes(1),'("DSET ",a50)')dsname
     write(datdes(2),'("options big_endian sequential")')
     write(datdes(3),'("TITLE ",a50)')title
     write(datdes(4),'("UNDEF ",e11.2)')undef
     next=5
     write(datdes(next),'("XDEF ",i5," LEVELS")')nlon
     kend=0
     do
        kstart=kend+1
        kend=min(kstart+9,nlon)
        if(kstart>nlon) exit
        next=next+1
        write(datdes(next),'(10f11.4)')(rlons_deg(k),k=kstart,kend)
     end do
     next=next+1
     write(datdes(next),'("YDEF ",i5," LEVELS")')nlat
     kend=0
     do
        kstart=kend+1
        kend=min(kstart+9,nlat)
        if(kstart>nlat) exit
        next=next+1
        write(datdes(next),'(10f11.4)')(rlats_deg(k),k=kstart,kend)
     end do
     next=next+1
     write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
     next=next+1
     koutmax=1
     write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
     next=next+1
     write(datdes(next),'("VARS 1")')
     next=next+1
     write(datdes(next),'("f   ",i5," 99 f   ")')nvert
     next=next+1
     write(datdes(next),'("ENDVARS")')
     last=next
     write(ioutdes,'(a112)')(datdes(i),i=1,last)

  end if

  call general_sub2grid_create_info(s,1,nlat,nlon,1,1,.true.)
  do k=1,nvert
     call general_gather2grid(s,f(:,:,:,k),work,0)
     if(mype==0) then
        do j=1,nlon ; do i=1,nlat
           outfield(j,i)=work(1,i,j)
        end do ; end do
        write(ioutdat)outfield
     end if
  end do

  if(mype==0) then
     close(ioutdes)
     close(ioutdat)
  end if
  call general_sub2grid_destroy_info(s)

end subroutine grads1a

subroutine sub2grid_1a(sub,grid,gridpe,mype)

!     straightforward, but inefficient code to convert a single variable on subdomains to complete
!      slab on one processor.
!  2013-10-24 todling - revist strip interface
!                     - reposition ltosi and others to commvar

  use kinds, only: r_kind,i_kind,r_single
  use constants, only: zero
  use gridmod, only: nlat,nlon,lat2,lon2,lat1,lon1,&
         iglobal,ijn,displs_g,itotsub,strip
  use general_commvars_mod, only: ltosi,ltosj
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind), intent(in)::gridpe,mype
  real(r_single),dimension(lat2,lon2),intent(in):: sub
  real(r_single),dimension(nlat,nlon),intent(out)::grid

  real(r_single),dimension(lat1*lon1):: zsm
  real(r_single),dimension(itotsub):: work1
  integer(i_kind) mm1,i,j,k

  mm1=mype+1

  do j=1,lon1*lat1
     zsm(j)=zero
  end do
  call strip(sub,zsm)
  call mpi_gatherv(zsm,ijn(mm1),mpi_rtype, &
                 work1,ijn,displs_g,mpi_rtype, &
                 gridpe,mpi_comm_world,ierror)
  if(mype==gridpe) then
     do k=1,iglobal
        i=ltosi(k) ; j=ltosj(k)
        grid(i,j)=work1(k)
     end do
  end if

end subroutine sub2grid_1a
