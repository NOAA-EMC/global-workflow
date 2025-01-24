module get_pseudo_ensperts_mod
use abstract_get_pseudo_ensperts_mod
  type, extends(abstract_get_pseudo_ensperts_class) :: get_pseudo_ensperts_class
  contains
    procedure, pass(this) :: get_pseudo_ensperts => get_pseudo_ensperts_wrf
    procedure, nopass :: read_wrf_nmm_tclib
    procedure, nopass :: get_bgtc_center
    procedure, nopass :: create_pseudo_enpert_blend
    procedure, nopass :: pseudo_ens_e2a
  end type get_pseudo_ensperts_class

contains
  subroutine get_pseudo_ensperts_wrf(this,en_perts,nelen)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_pseudo_ensperts
!   prgmmr: mtong           org: np22                date: 2011-09-06
!
! abstract: read pseudo ensemble members from TC library, and interpolate to analysis
!           grid. Calculate pseudo ensemble perturbations and replace global ensemble
!           perturbations in vortex region (300 km from storm center)   
!
! program history log:
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

    use gridmod, only: half_grid,filled_grid,regional
    use constants,only: zero,one,two
    use mpimod, only: mpi_comm_world,ierror,mype,mpi_rtype
    use kinds, only: r_kind,i_kind,r_single
    use gsi_4dvar, only: nhr_assimilation 
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,uv_hyb_ens, &
                                          grid_ratio_ens,write_ens_sprd
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid,general_grid2sub, &
                                    general_sub2grid_create_info
    use gsi_io, only: lendian_in
    use hybrid_ensemble_parameters, only: n_ens,grd_ens
    use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
    use gsi_bundlemod, only: gsi_bundlecreate
    use gsi_bundlemod, only: gsi_grid
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_bundledestroy
    use gsi_bundlemod, only: gsi_gridcreate
    use get_wrf_nmm_ensperts_mod, only: get_wrf_nmm_ensperts_class
    use get_wrf_mass_ensperts_mod, only: get_wrf_mass_ensperts_class
  
  
    implicit none
!   Declare passed variables
    class(get_pseudo_ensperts_class),  intent(inout) :: this
    type(gsi_bundle),allocatable, intent(in   ) :: en_perts(:,:,:)
    integer(i_kind),                   intent(in   ) :: nelen
    

    type(sub2grid_info) grd_lib,grd_tmp
    real(r_kind),allocatable,dimension(:,:,:) :: u,v,tv,rh
    real(r_kind),allocatable,dimension(:,:)   :: ps
    real(r_kind) bar_norm,sig_norm
  
    integer(i_kind):: ic2,ic3
    integer(i_kind) ipc2d(nc2d),ipc3d(nc3d)
    real(r_single),pointer,dimension(:,:,:):: w3
    real(r_single),pointer,dimension(:,:):: w2
    real(r_kind),pointer,dimension(:,:,:):: x3
    real(r_kind),pointer,dimension(:,:):: x2
    type(gsi_bundle):: en_bar
    type(gsi_bundle),allocatable :: lib_perts(:)
    type(gsi_grid):: grid_ens
    type(get_wrf_nmm_ensperts_class):: nmm_enspert
    type(get_wrf_mass_ensperts_class):: mass_enspert
  
    integer(i_kind) :: nord_blend,nn_ens1,nn_ens2
    real(r_kind),allocatable,dimension(:,:) :: blend,wgt
    real(r_single),allocatable,dimension(:,:) :: outwork
    real(r_kind),allocatable::temp(:)
  
    integer(i_kind) i,j,k,n,istatus,mm1
    character(120) filename,infofile
  ! character(24) fileout
    logical(4) fexist
  
    integer(i_kind) regional_time(6)
    integer(i_kind) nlon_regional,nlat_regional
    real(r_single) dlmd,dphd,dlmd_ens,dphd_ens,dlmd_lib,dphd_lib
  
    real(r_kind) dlmd2,dphd2
    integer(i_kind) nlon_regional_lib,nlat_regional_lib,nsig,nlon_lib,nlat_lib
    integer(i_kind) inner_vars,num_fields
    logical,allocatable::vector(:)
  
    integer(i_kind) ku,kv,kt,kq,kps
    real(r_kind),allocatable,dimension(:,:,:,:) :: work,work_sub,work_reg
  
    real(r_kind) bc_lon,bc_lat,lclon,lclat,lc_lon,lc_lat,lc_lonm,lc_latm
    integer(i_kind) lon_bc,lat_bc,lon_lc,lat_lc,ratio_lon,ratio_lat
    logical :: test

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
  
  ! create ensemble perturbation and ensemble mean bundle
    allocate(lib_perts(n_ens))
    call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
    call gsi_bundlecreate(en_bar,grid_ens,'ensemble mean',istatus,names2d=cvars2d, &
                          names3d=cvars3d,bundle_kind=r_kind)
    if(istatus/=0) then
       write(6,*)' get_pseudo_ensperts: trouble creating en_bar bundle'
       call stop2(999)
    endif
    do n=1,n_ens
        call gsi_bundlecreate(lib_perts(n),grid_ens,'library perts',istatus, &
                              names2d=cvars2d,names3d=cvars3d,bundle_kind=r_single)
       if(istatus/=0) then
          write(6,*)'get_pseudo_enperts: trouble creating lib_perts bundle'
          call stop2(999)
       endif
    end do
  
  !
  ! INITIALIZE ENSEMBLE MEAN ACCUMULATORS
    en_bar%values=zero
    do n=1,n_ens
       lib_perts(n)%valuesr4=zero
    end do 
  
  ! read dlmd and dphd from guess
    write(filename,'("sigf",i2.2)')nhr_assimilation
    open(lendian_in,file=filename,form='unformatted')
    rewind lendian_in
    read(lendian_in) regional_time,nlon_regional,nlat_regional,nsig, &
                dlmd,dphd
    close(lendian_in)
  
  
    open(100,file='filelistp',form='formatted',err=30)
    open(200,file='infolist',form='formatted',err=30)
    rewind (100)
    rewind (200)
    do n=1,200
    read(100,'(a120)',err=20,end=40)filename
    enddo
  40  nn_ens1=n-1
  
    do n=1,200
    read(200,'(a120)',err=20,end=50)infofile
    enddo
  50  nn_ens2=n-1
  
    if(nn_ens1 < n_ens .or. nn_ens2 < n_ens)then
       print *,'missing ensemble members or infofile'
       call stop2(401)
    end if
  
    rewind (100)
    read(100,'(a120)',err=20,end=60)filename
    open(lendian_in,file=trim(filename),form='unformatted')
    rewind lendian_in
    read(lendian_in) nlon_regional_lib,nlat_regional_lib,nsig,dlmd_lib,dphd_lib
  !  if(mype == 0)print *,'nlon_lib, nlat_lib, nsig=', nlon_regional_lib,nlat_regional_lib,nsig
  !  if(mype == 0)print *,'dlmd_lib,dphd_lib=', dlmd_lib,dphd_lib
    if(filled_grid) then
       nlon_lib=2*nlon_regional_lib-1
       nlat_lib=nlat_regional_lib
    endif
    if(half_grid) then
       nlon_lib=nlon_regional_lib
       nlat_lib=1+nlat_regional_lib/2
    endif
    close(lendian_in)
  !  if(mype == 0)print *,'nlon_lib, nlat_lib, nsig=', nlon_lib, nlat_lib, nsig
    num_fields=1+4*nsig 
  !  if(mype == 0)print *,'num_fields=', num_fields
    inner_vars=1
    allocate(vector(num_fields))
    vector=.false.
    vector(1:2*nsig)=uv_hyb_ens
    call general_sub2grid_create_info(grd_lib,inner_vars,nlat_lib,nlon_lib,nsig,num_fields,regional,vector)
  
    call general_sub2grid_create_info(grd_tmp,inner_vars,grd_ens%nlat,grd_ens%nlon,nsig,num_fields,regional,vector)
    deallocate(vector)
  
  ! read background TC center longitude and latide and convert to analysis
  ! grid index
  
    call get_bgtc_center(bc_lon,bc_lat)
  
  !  if(mype == 0) print *,'bc_lon,bc_lat=', bc_lon,bc_lat
  
    if(filled_grid)then
       dlmd2=dlmd
       dphd2=dphd
    end if
    if(half_grid)then
       dlmd2=two*dlmd
       dphd2=two*dphd
    end if
  
    dlmd2=dlmd2*grid_ratio_ens
    dphd2=dlmd2*grid_ratio_ens
    dlmd_ens=dlmd*grid_ratio_ens
    dphd_ens=dphd*grid_ratio_ens
  
    allocate(blend(grd_ens%nlat,grd_ens%nlon))
  
    blend=zero
    nord_blend=4
    call create_pseudo_enpert_blend(bc_lon,bc_lat,dlmd2,dphd2,nord_blend,blend)
  
    if(mype==0)then
       allocate(outwork(grd_ens%nlon,grd_ens%nlat))
       outwork=zero
       do j=1,grd_ens%nlon
          do i=1,grd_ens%nlat
             outwork(j,i)=blend(i,j)
          end do
      end do
      call outgrads1(outwork,grd_ens%nlon,grd_ens%nlat,'blend')
      deallocate(outwork)
    end if
  
  ! Reorganize for eventual distribution to local domains
    allocate(temp(grd_ens%itotsub))
    if (mype==0) then
       do k=1,grd_ens%itotsub
          i=grd_ens%ltosi_s(k) ; j=grd_ens%ltosj_s(k)
          temp(k)=blend(i,j)
       end do
    end if
  
    allocate(wgt(grd_ens%lat2,grd_ens%lon2))
    wgt=zero
    mm1=mype+1
    call mpi_scatterv(temp,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype,&
         wgt,grd_ens%ijn_s(mm1),mpi_rtype,0,mpi_comm_world,ierror)
  
    call nmm_enspert%grads3d(grd_ens,wgt,1,mype,'wgt')
  
    deallocate(temp)
  
    rewind(100)
    rewind(200)
    do n=1,n_ens
       read(100,'(a120)',err=20,end=60)filename
       filename=trim(filename)
       if(mype == 0)print *,'get_pseudo_ensperts: filename=', filename
       inquire(file=filename,exist=fexist)
       if(.not. fexist) call stop2(401)
  
       read(200,'(a120)',err=20,end=60)infofile
       infofile=trim(infofile)
       if(mype == 0)print *,'get_pseudo_ensperts: infofile=', infofile
       inquire(file=infofile,exist=fexist)
       if(.not. fexist) call stop2(401) 
  
  !    read library tc center index from info file and find the index of the 
  !    point that match the background TC center
       open(30,file=infofile,form='formatted')
       read(30,*)lclon,lclat,lc_lon,lc_lat
       close(30)
  
  !     if(mype == 0)print *,'n,lc_lon,lc_lat=', n,lc_lon, lc_lat
  
       if(filled_grid)then
          lc_lon=(2*lc_lon)-1
       end if  
  
       if(half_grid)then
          lc_lat=1+lc_lat/2
       end if
  
  !     if(mype == 0)print *,'n,lc_lon,lc_lat=', n,lc_lon, lc_lat
  
       lc_lonm=lc_lon-(bc_lon-NINT(bc_lon))*dlmd_ens/dlmd_lib
       lc_latm=lc_lat-(bc_lat-NINT(bc_lat))*dphd_ens/dphd_lib
  
  !     if(mype == 0)print *,'n,lc_lonm,lc_latm=', n,lc_lonm, lc_latm
  
       lon_bc=NINT(bc_lon) 
       lat_bc=NINT(bc_lat)
  
       lon_lc=NINT(lc_lonm)
       lat_lc=NINT(lc_latm)
  
       ratio_lon=INT(dlmd_ens/dlmd_lib)
       ratio_lat=INT(dphd_ens/dphd_lib)
  
  ! test
  !     ratio_lon=1 
  !     ratio_lat=1
  ! test
  
  !     if(mype == 0)print *,'lon_bc,lat_bc=',lon_bc,lat_bc
  !     if(mype == 0)print *,'lon_lc,lat_lc=',lon_lc,lat_lc
  !     if(mype == 0)print *,'ratio_lon,ratio_lat=',ratio_lon,ratio_lat
  
       allocate(   u(grd_lib%lat2,grd_lib%lon2,grd_lib%nsig))
       allocate(   v(grd_lib%lat2,grd_lib%lon2,grd_lib%nsig))
       allocate(  tv(grd_lib%lat2,grd_lib%lon2,grd_lib%nsig))
       allocate(  rh(grd_lib%lat2,grd_lib%lon2,grd_lib%nsig))
       allocate(  ps(grd_lib%lat2,grd_lib%lon2))
       u=zero ; v=zero ; tv=zero ; rh=zero ; ps=zero
  
  !    read fields from TC library
  
       call read_wrf_nmm_tclib(grd_lib,filename,mype,ps,u,v,tv,rh)
  
  !     write(fileout,'("tclib",i3.3)') n
  !     call grads3a(grd_lib,u,v,tv,rh,ps,grd_lib%nsig,mype,fileout)
  
       allocate(work_sub(grd_lib%inner_vars,grd_lib%lat2,grd_lib%lon2,num_fields))
       work_sub=zero
       do k=1,grd_lib%nsig
          ku=k ; kv=k+grd_lib%nsig ; kt=k+2*grd_lib%nsig ; kq=k+3*grd_lib%nsig 
          do j=1,grd_lib%lon2
             do i=1,grd_lib%lat2
                work_sub(1,i,j,ku)=u(i,j,k)
                work_sub(1,i,j,kv)=v(i,j,k)
                work_sub(1,i,j,kt)=tv(i,j,k)
                work_sub(1,i,j,kq)=rh(i,j,k)
             end do
          end do
       end do
       deallocate(u,v,tv,rh)
       kps=num_fields
       do j=1,grd_lib%lon2
          do i=1,grd_lib%lat2
             work_sub(1,i,j,kps)=ps(i,j)
          end do
       end do
       deallocate(ps)
  
       allocate(work(grd_lib%inner_vars,grd_lib%nlat,grd_lib%nlon,grd_lib%kbegin_loc:grd_lib%kend_alloc))
       work=zero
       call general_sub2grid(grd_lib,work_sub,work)
       deallocate(work_sub)
  
  !    then match to regional analysis grid
       allocate(work_reg(grd_tmp%inner_vars,grd_tmp%nlat,grd_tmp%nlon,grd_tmp%kbegin_loc:grd_tmp%kend_alloc))
       work_reg=zero
  
       do k=grd_lib%kbegin_loc,grd_lib%kend_loc
          call pseudo_ens_e2a(lon_bc,lat_bc,lon_lc,lat_lc,grd_lib%nlon,grd_lib%nlat,work(1,1,1,k), &
                              blend,ratio_lon,ratio_lat,grd_ens%nlon,grd_ens%nlat,work_reg(1,1,1,k))
  
       end do
       deallocate(work)
  
  !    next general_grid2sub to go to regional grid subdomains.
       allocate(work_sub(grd_tmp%inner_vars,grd_tmp%lat2,grd_tmp%lon2,grd_tmp%num_fields))
       work_sub=zero
       call general_grid2sub(grd_tmp,work_reg,work_sub)
       deallocate(work_reg)
  
       allocate(   u(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(   v(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(  tv(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(  rh(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(  ps(grd_ens%lat2,grd_ens%lon2))
       do k=1,grd_ens%nsig
          ku=k ; kv=ku+grd_ens%nsig ; kt=kv+grd_ens%nsig ; kq=kt+grd_ens%nsig 
          do j=1,grd_ens%lon2
             do i=1,grd_ens%lat2
                u(i,j,k)=work_sub(1,i,j,ku)
                v(i,j,k)=work_sub(1,i,j,kv)
                tv(i,j,k)=work_sub(1,i,j,kt)     !  now pot virtual temp
                rh(i,j,k)=work_sub(1,i,j,kq)     !  now rh
             end do
          end do
       end do
       kps=num_fields
       do j=1,grd_ens%lon2
          do i=1,grd_ens%lat2
             ps(i,j)=work_sub(1,i,j,kps)
          end do
       end do
       deallocate(work_sub)
  
  !     write(fileout,'("tclibe2a",i3.3)') n
  !     call grads3a(grd_ens,u,v,tv,rh,ps,grd_ens%nsig,mype,fileout)
  !
  ! SAVE ENSEMBLE MEMBER DATA IN COLUMN VECTOR
       do ic3=1,nc3d
  
          call gsi_bundlegetpointer(lib_perts(n),trim(cvars3d(ic3)),w3,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
             call stop2(999)
          end if
          call gsi_bundlegetpointer(en_bar,trim(cvars3d(ic3)),x3,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for en_bar'
             call stop2(999)
          end if
  
          select case (trim(cvars3d(ic3)))
  
             case('sf','SF')
  
                do k=1,grd_ens%nsig
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         w3(i,j,k) = u(i,j,k)
                         x3(i,j,k) = x3(i,j,k)+u(i,j,k)
                      end do
                   end do
                end do
   
             case('vp','VP')
   
                do k=1,grd_ens%nsig
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         w3(i,j,k) = v(i,j,k)
                         x3(i,j,k) = x3(i,j,k)+v(i,j,k)
                      end do
                   end do
                end do
   
             case('t','T')
   
                do k=1,grd_ens%nsig
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         w3(i,j,k) = tv(i,j,k)
                         x3(i,j,k) = x3(i,j,k)+tv(i,j,k)
                      end do
                   end do
                end do
   
             case('q','Q')
   
                do k=1,grd_ens%nsig
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         w3(i,j,k) = rh(i,j,k)
                         x3(i,j,k) = x3(i,j,k)+rh(i,j,k)
                      end do
                   end do
                end do
  
          end select
       end do
       deallocate(u,v,tv,rh)
  
       do ic2=1,nc2d
  
          call gsi_bundlegetpointer(lib_perts(n),trim(cvars2d(ic2)),w2,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
             call stop2(999)
          end if
          call gsi_bundlegetpointer(en_bar,trim(cvars2d(ic2)),x2,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar'
             call stop2(999)
          end if
  
          select case (trim(cvars2d(ic2)))
  
             case('ps','PS')
  
                do j=1,grd_ens%lon2
                   do i=1,grd_ens%lat2
                      w2(i,j) = ps(i,j)
                      x2(i,j) = x2(i,j)+ps(i,j)
                   end do
                end do
  
          end select
       end do
       deallocate(ps)
  
    end do ! end do over ensemble
   
    close(100)
    close(200)
  
    deallocate(blend)
   
  ! Convert to mean
    bar_norm = one/real(n_ens,r_kind)
    en_bar%values=en_bar%values*bar_norm
  
    if(write_ens_sprd)then
       call mpi_barrier(mpi_comm_world,ierror)
       call mass_enspert%ens_spread_dualres_regional(mype,en_perts,nelen,en_bar)
       call mpi_barrier(mpi_comm_world,ierror)
    end if
  
    test=.false.
    if(test)then
       allocate(u(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(v(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(tv(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(rh(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(ps(grd_ens%lat2,grd_ens%lon2))
  
       u=zero; v=zero; tv=zero; rh=zero; ps=zero
  
       call gsi_bundlegetpointer(en_bar,cvars3d,ipc3d,istatus)
       if(istatus/=0) then
         write(6,*) 'mtong: cannot find 3d pointers'
         call stop2(999)
       endif
       call gsi_bundlegetpointer(en_bar,cvars2d,ipc2d,istatus)
       if(istatus/=0) then
         write(6,*) 'mtong: cannot find 2d pointers'
         call stop2(999)
       endif
   
       do ic3=1,nc3d
          select case (trim(cvars3d(ic3)))
  
             case('sf','SF')
  
                do k=1,grd_ens%nsig
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         u(i,j,k) = en_bar%r3(ipc3d(ic3))%q(i,j,k)
                      end do
                   end do
                end do
  
             case('vp','VP')
  
                do k=1,grd_ens%nsig
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         v(i,j,k) = en_bar%r3(ipc3d(ic3))%q(i,j,k)
                      end do
                   end do
                end do
  
             case('t','T')
                do k=1,grd_ens%nsig
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         tv(i,j,k) = en_bar%r3(ipc3d(ic3))%q(i,j,k)
                      end do
                   end do
                end do
  
             case('q','Q')
  
                do k=1,grd_ens%nsig
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         rh(i,j,k) = en_bar%r3(ipc3d(ic3))%q(i,j,k)
                      end do
                   end do
                end do
  
          end select
       end do
  
       do ic2=1,nc2d
          select case (trim(cvars2d(ic2)))
  
             case('ps','PS')
  
                do j=1,grd_ens%lon2
                   do i=1,grd_ens%lat2
                      ps(i,j) = en_bar%r2(ipc2d(ic2))%q(i,j)
                   end do
                end do
  
          end select
       end do
  
       call nmm_enspert%grads3a(grd_ens,u,v,tv,rh,ps,grd_ens%nsig,mype,'en_bar')
       deallocate(u,v,tv,rh,ps)
    end if
  
  ! Convert ensemble members to perturbations
    sig_norm=sqrt(one/max(one,n_ens-one))
    do n=1,n_ens
       do i=1,nelen
          lib_perts(n)%valuesr4(i)=(lib_perts(n)%valuesr4(i)-en_bar%values(i))*sig_norm
       end do
    end do
    call mpi_barrier(mpi_comm_world,ierror)
  
    call gsi_bundledestroy(en_bar,istatus)
    if(istatus/=0) then
       write(6,*)' in get_pseudo_ensperts: trouble destroying en_bar bundle'
       call stop2(999)
    endif
  
  ! merge pseudo ensemble perturbation to global ensemble perturbation
  
  ! NOTE:  because library perturbation bundle structure is same as ensemble perturbation, 
  !        use same ipc3d and ipc2d indices for lib_perts and en_perts bundles.
  
    call gsi_bundlegetpointer (en_perts(1,1,1),cvars3d,ipc3d,istatus)
    if(istatus/=0) then
       write(6,*) 'get_pseudo_enperts: cannot find 3d pointers'
       call stop2(999)
    endif
    call gsi_bundlegetpointer (en_perts(1,1,1),cvars2d,ipc2d,istatus)
    if(istatus/=0) then
       write(6,*) 'get_pseudo_enperts: cannot find 2d pointers'
       call stop2(999)
    endif
  
    do n=1,n_ens
  
       do ic3=1,nc3d
  
          do k=1,grd_ens%nsig
             do j=1,grd_ens%lon2
                do i=1,grd_ens%lat2
                   en_perts(n,1,1)%r3(ipc3d(ic3))%qr4(i,j,k) = wgt(i,j) * &
                                lib_perts(n)%r3(ipc3d(ic3))%qr4(i,j,k)  + &
                                (one-wgt(i,j))*en_perts(n,1,1)%r3(ipc3d(ic3))%qr4(i,j,k)
                end do
             end do
          end do
  
       end do
  
       do ic2=1,nc2d
  
          do j=1,grd_ens%lon2
             do i=1,grd_ens%lat2
                en_perts(n,1,1)%r2(ipc2d(ic2))%qr4(i,j) = wgt(i,j)  * &
                               lib_perts(n)%r2(ipc2d(ic2))%qr4(i,j) + &
                               (one-wgt(i,j))*en_perts(n,1,1)%r2(ipc2d(ic2))%qr4(i,j)
             end do
          end do
  
       end do
  
    end do
    
    do n=1,n_ens
       call gsi_bundledestroy(lib_perts(n),istatus)
       if(istatus/=0) then
          write(6,*)' in get_pseudo_ensperts: trouble destroying lib_perts bundle'
          call stop2(999)
       endif
    end do
  
    deallocate(wgt,lib_perts)
  
  ! output perturbations
  
    test=.false.
    if(test)then
       allocate(u(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(v(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(tv(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(rh(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
       allocate(ps(grd_ens%lat2,grd_ens%lon2))
  
       u=zero; v=zero; tv=zero; rh=zero; ps=zero
  
       do n=1,n_ens
  
          write(filename,"('enpert',i3.3)") n
  
          call gsi_bundlegetpointer(en_perts(n,1,1),cvars3d,ipc3d,istatus)
          if(istatus/=0) then
            write(6,*) 'mtong: cannot find 3d pointers'
            call stop2(999)
          endif
          call gsi_bundlegetpointer(en_perts(n,1,1),cvars2d,ipc2d,istatus)
          if(istatus/=0) then
            write(6,*) 'mtong: cannot find 2d pointers'
            call stop2(999)
          endif
  
          do ic3=1,nc3d
             select case (trim(cvars3d(ic3)))
  
                case('sf','SF')
  
                   do k=1,grd_ens%nsig
                      do j=1,grd_ens%lon2
                         do i=1,grd_ens%lat2
                            u(i,j,k) = en_perts(n,1,1)%r3(ipc3d(ic3))%qr4(i,j,k)
                         end do
                      end do
                   end do
  
                case('vp','VP')
  
                   do k=1,grd_ens%nsig
                      do j=1,grd_ens%lon2
                         do i=1,grd_ens%lat2
                            v(i,j,k) = en_perts(n,1,1)%r3(ipc3d(ic3))%qr4(i,j,k)
                         end do
                      end do
                   end do
  
                case('t','T')
  
                   do k=1,grd_ens%nsig
                      do j=1,grd_ens%lon2
                         do i=1,grd_ens%lat2
                            tv(i,j,k) = en_perts(n,1,1)%r3(ipc3d(ic3))%qr4(i,j,k)
                         end do
                      end do
                   end do
  
                case('q','Q')
  
                   do k=1,grd_ens%nsig
                      do j=1,grd_ens%lon2
                         do i=1,grd_ens%lat2
                            rh(i,j,k) = en_perts(n,1,1)%r3(ipc3d(ic3))%qr4(i,j,k)
                         end do
                      end do
                   end do
  
             end select
          end do
  
          do ic2=1,nc2d
             select case (trim(cvars2d(ic2)))
  
                case('ps','PS')
  
                   do j=1,grd_ens%lon2
                      do i=1,grd_ens%lat2
                         ps(i,j) = en_perts(n,1,1)%r2(ipc2d(ic2))%qr4(i,j)
                      end do
                   end do
  
             end select
          end do
  
          call nmm_enspert%grads3a(grd_ens,u,v,tv,rh,ps,grd_ens%nsig,mype,filename)
  
       end do
  
       deallocate(u,v,tv,rh,ps)
    end if ! if(test)then
  
  
    return
  30 write(6,*) 'GET_PSEUDO_ENSPERTS open filelist failed '
     call stop2(400)
  20 write(6,*) 'GET_PSEUDO_NMM_ENSPERTS read file list failed ',n
     call stop2(400)
  60 write(6,*) 'GET_PSEUDO_NMM_ENSPERTS reach end of file list ',n
     call stop2(400)
  end subroutine get_pseudo_ensperts_wrf
  
  subroutine read_wrf_nmm_tclib(grd,filename,mype,ps,u,v,tv,rh)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_wrf_nmm_tclib        read wrf_nmm TC library
  !   prgmmr: tong          org: np22                date: 2011-09-06
  !
  ! program history log:
  !
  !   input argument list:
  !     grd      - structure variable containing information about grid
  !                (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
  !     filename - input sigma file name
  !     mype     - mpi task id
  !
  !   output argument list:
  !     *        - library fields
  !
  ! language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: r_kind,r_single,i_kind
    use mpimod, only: ierror,mpi_real4,mpi_comm_world,npe
    use gridmod, only: half_grid,filled_grid
    use constants, only: one,ten,one_tenth,half,fv
    use gsi_io, only: lendian_in
    use general_sub2grid_mod, only: sub2grid_info
    use hybrid_ensemble_parameters, only: q_hyb_ens
    use get_wrf_nmm_ensperts_mod, only: get_wrf_nmm_ensperts_class 
    implicit none
  
  ! Declare passed variables here
    type(sub2grid_info)                   ,intent(in   ) :: grd
    character(120)                        ,intent(in   ) :: filename
    integer(i_kind)                       ,intent(in   ) :: mype
    real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  out) :: ps
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: u,v,tv,rh
  
  ! Declare local parameters
    real(r_kind),parameter:: r0_01=0.01_r_kind
    real(r_kind),allocatable,dimension(:,:,:) :: tsen,q,prsl
    real(r_kind),allocatable,dimension(:,:) :: g_pd
    type(get_wrf_nmm_ensperts_class) :: wrf_nmm
  
  ! Declare local variables
    integer(i_kind) kt,kq,ku,kv
  
  ! NMM variable names stuck in here
  
  ! other internal variables
    integer(i_kind) nlon_regional,nlat_regional,nsig
    real(r_single) dlmd,dphd
    real(r_single)pt,pdtop
    real(r_single),allocatable:: aeta1(:),aeta2(:)
    real(r_kind) pdtop_ll,pt_ll
    real(r_kind),allocatable:: aeta1_ll(:),aeta2_ll(:)
  
    real(r_single),allocatable::tempa(:)
    real(r_single),allocatable::temp1(:,:)
    real(r_single),allocatable::all_loc(:,:,:)
    integer(i_kind),allocatable::igtype(:),jsiskip(:)
    integer(i_kind) irc_s_reg(npe),ird_s_reg(npe)
    integer(i_kind) ifld,im,jm,lm,num_nmm_fields
    integer(i_kind) num_all_fields,num_loc_groups,num_all_pad
    integer(i_kind) i,icount,icount_prev,j,k
    integer(i_kind) i_0,i_pd,i_t,i_q,i_u,i_v
    integer(i_kind) iderivative
    real(r_kind) pd,psfc_this
    integer(i_kind) ireturn
    logical ice
  ! character(24) fileout
  
    lm=grd%nsig
  
    num_nmm_fields=1+4*lm
  
    num_all_fields=num_nmm_fields*1
    num_loc_groups=num_all_fields/npe
  !  if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_tclib, lm            =",i6)')lm
  !  if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_tclib, num_nmm_fields=",i6)')num_nmm_fields
  !  if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_tclib, num_all_fields=",i6)')num_all_fields
  !  if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_tclib, npe           =",i6)')npe
  !  if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_tclib, num_loc_groups=",i6)')num_loc_groups
    do
       num_all_pad=num_loc_groups*npe
       if(num_all_pad >= num_all_fields) exit
       num_loc_groups=num_loc_groups+1
    end do
    if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_tclib, num_all_pad   =",i6)')num_all_pad
    if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_tclib, num_loc_groups=",i6)')num_loc_groups
  
    allocate(all_loc(grd%lat2,grd%lon2,num_all_pad))
    allocate(jsiskip(num_nmm_fields))
    allocate(igtype(num_nmm_fields))
  
    i=0
    i=i+1 ; i_pd=i                                                    ! pd
    jsiskip(i)=2
    igtype(i)=1
  
    i_t=i+1
    do k=1,lm
       i=i+1                                                          ! t(k)
       jsiskip(i)=0
       igtype(i)=1
    end do
    i_q=i+1
    do k=1,lm
       i=i+1                                                          ! q(k)
       jsiskip(i)=0 ; igtype(i)=1
    end do
    i_u=i+1
    do k=1,lm
       i=i+1                                                          ! u(k)
       jsiskip(i)=0 ; igtype(i)=2
    end do
    i_v=i+1
    do k=1,lm
       i=i+1                                                          ! v(k)
       jsiskip(i)=0 ; igtype(i)=2
    end do
  
    do i=1,npe
       irc_s_reg(i)=grd%ijn_s(mype+1)
    end do
    ird_s_reg(1)=0
    do i=1,npe
       if(i /= 1) ird_s_reg(i)=ird_s_reg(i-1)+irc_s_reg(i-1)
    end do
  
    icount=0
    icount_prev=1
    open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
    read(lendian_in) nlon_regional,nlat_regional,nsig,dlmd,dphd,pt,pdtop
  !  if(mype == 0)print *,'nlon_regional,nlat_regional,nsig,dlmd,dphd,pt,pdtop'
  !  if(mype == 0)print *,nlon_regional,nlat_regional,nsig,dlmd,dphd,pt,pdtop
    im=nlon_regional
    jm=nlat_regional
    allocate(aeta1(nsig),aeta2(nsig))
    allocate(aeta1_ll(nsig),aeta2_ll(nsig))
    allocate(temp1(im,jm))
    allocate(tempa(grd%itotsub))
  
  !    Read, interpolate, and distribute NMM restart fields
    do ifld=1,num_nmm_fields
       icount=icount+1
       if(jsiskip(ifld) > 0) then 
          do i=1,jsiskip(ifld)
             if(i == 1)then
                read(lendian_in)aeta1
             else if(i == 2)then
                read(lendian_in)aeta2
             else
                read(lendian_in)
             end if
          end do
       end if
       if(mype == mod(icount-1,npe)) then
          read(lendian_in)((temp1(i,j),i=1,im),j=1,jm)
          if(filled_grid) call wrf_nmm%general_fill_nmm_grid2(grd,temp1,im,jm,tempa,abs(igtype(ifld)),1,ireturn)
          if(half_grid)   call wrf_nmm%general_half_nmm_grid2(grd,temp1,im,jm,tempa,abs(igtype(ifld)),1,ireturn)
          if(ireturn == 1)call stop2(500)
       else
          read(lendian_in)
       end if
  
  !    Distribute to local domains everytime we have npe fields
       if(mod(icount,npe) == 0.or.icount == num_all_fields) then
          call mpi_alltoallv(tempa,grd%ijn_s,grd%displs_s,mpi_real4, &
               all_loc(1,1,icount_prev),irc_s_reg,ird_s_reg,mpi_real4,mpi_comm_world,ierror)
          icount_prev=icount+1
       end if
  
    end do
    close(lendian_in)
  
    allocate(tsen(grd%lat2,grd%lon2,grd%nsig),q(grd%lat2,grd%lon2,grd%nsig))
    allocate(prsl(grd%lat2,grd%lon2,grd%nsig))
    allocate(g_pd(grd%lat2,grd%lon2))
  
    aeta1_ll=aeta1
    aeta2_ll=aeta2
    pdtop_ll=r0_01*pdtop
    pt_ll=r0_01*pt
    i_0=0
    kt=i_0+i_t-1
    kq=i_0+i_q-1
    ku=i_0+i_u-1
    kv=i_0+i_v-1
  
    do k=1,grd%nsig
       kt=kt+1
       kq=kq+1
       ku=ku+1
       kv=kv+1
  
       do i=1,grd%lon2
          do j=1,grd%lat2
             u(j,i,k) = real(all_loc(j,i,ku),r_kind)
             v(j,i,k) = real(all_loc(j,i,kv),r_kind)
             q(j,i,k) = real(all_loc(j,i,kq),r_kind)
             tsen(j,i,k) = real(all_loc(j,i,kt),r_kind)! actually holds sensible temperature
          end do
       end do
    end do
  
    do i=1,grd%lon2
       do j=1,grd%lat2
          g_pd(j,i)=real(all_loc(j,i,i_pd),r_kind)
       end do
    end do
  
  !  call grads3d(grd,u,nsig,mype,'u')
  
  ! convert wrf nmm pd variable to psfc in mb, and then to log(psfc) in cb
    do i=1,grd%lon2
       do j=1,grd%lat2
          pd=r0_01*real(all_loc(j,i,i_0+i_pd),r_kind)
          psfc_this=pd+pdtop_ll+pt_ll
          ps(j,i)=one_tenth*psfc_this   ! convert from mb to cb
       end do
    end do
    do k=1,grd%nsig
       do i=1,grd%lon2
          do j=1,grd%lat2
             tv(j,i,k) = tsen(j,i,k) * (one+fv*q(j,i,k))
             prsl(j,i,k)=one_tenth* &
                        (aeta1_ll(k)*pdtop_ll + &
                         aeta2_ll(k)*(ten*ps(j,i)-pdtop_ll-pt_ll) + &
                         pt_ll)
          end do
       end do
    end do
  
    if (.not.q_hyb_ens) then
       ice=.true.
       iderivative=0
       call genqsat(rh,tsen,prsl,grd%lat2,grd%lon2,grd%nsig,ice,iderivative)
  
       do k=1,grd%nsig
          do i=1,grd%lon2
             do j=1,grd%lat2
                rh(j,i,k)=q(j,i,k)/rh(j,i,k)
             end do
          end do
       end do
    else
       do k=1,grd%nsig
          do i=1,grd%lon2
             do j=1,grd%lat2
                rh(j,i,k)=q(j,i,k)
             end do
          end do
       end do
    end if
  
  
    deallocate(aeta1,aeta2,aeta1_ll,aeta2_ll)
    deallocate(all_loc,jsiskip,igtype)
    deallocate(temp1,tempa)
    deallocate(tsen,q,prsl,g_pd)
  
  end subroutine read_wrf_nmm_tclib
  
  subroutine get_bgtc_center(bc_lon,bc_lat)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    get_bgtc_center  
  !                
  !   prgmmr: mtong          org: np22                date: 2011-09-08
  !
  ! abstract:  read background TC center longitude and latitude and convert to 
  !            analysis grid index
  !
  ! program history log:
  !
  !   input argument list:
  !
  !   output argument list:
  !     bc_lon    - TC center grid index in x direction
  !     bc_lat    - TC center grid index in y direction
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: r_kind,r_single,i_kind
    use constants, only: zero,one,deg2rad
    use gridmod, only: tll2xy
  
    implicit none
  
    real(r_kind)                          ,intent(inout) :: bc_lon,bc_lat
    integer(i_kind) iclat,iclon
    character*1 sn,ew
    real(r_kind),parameter:: r360=360.0_r_kind
    real(r_kind) clat,clon
    logical outside
  
    clon=zero
    clat=zero
  
    open(32,file='trak.hwrf06',form='formatted')
    read(32,'(33x,I3,A1,1x,I4,A1)') iclat,sn,iclon,ew
    close(32)
  
    if(sn == 'S')then
      clat=- one * real(iclat,r_kind)/10.0_r_kind
    else
      clat=real(iclat,r_kind)/10.0_r_kind
    endif
  
    if(ew == 'W')then
       clon=360._r_kind - real(iclon,r_kind)/10.0_r_kind
    else
       clon=real(iclon,r_kind)/10.0_r_kind
    endif
  
  
    if (clon >= r360) clon=clon-r360
    if (clon < zero)  clon=clon+r360
    clat = clat * deg2rad
    clon = clon * deg2rad
    call tll2xy(clon,clat,bc_lon,bc_lat,outside)
    if (outside) then
       print *,'tc center is outside domain'
       call stop2(402)
    end if
  
  end subroutine get_bgtc_center 
  
  subroutine create_pseudo_enpert_blend(bc_lon,bc_lat,dlmd,dphd,nord_blend,blndmsk)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    get_pseudo_enpert_blend 
  !   prgmmr: mtong          org: np22                date: 2011-09-12
  !
  ! abstract: create blend mask for pseudo ensemble perturbations 
  !           The blending function gradually change from 1 to 0
  !           witin the blending zone 150 km to 300 km from the 
  !           TC center.
  !
  ! program history log:
  !   2011-09-12  mtong, initial documentation
  !
  !   input argument list:
  !     bc_lon         - background TC center x index
  !     bc_lat         - background TC center y index
  !     dlmd           - grid resolution in x direction
  !     dphd           - grid resolution in y direction
  !     nord_blend     - variable used to create variable blend mask
  !
  !   output argument list:
  !     blndmsk        - blend mask
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    use constants, only: zero,one
    use kinds, only: r_kind,i_kind,r_single
    use hybrid_ensemble_parameters, only: grd_ens
    use blendmod, only: blend
    implicit none
  
    real(r_kind)                          ,intent(in   ) :: bc_lon,bc_lat 
    real(r_kind  )                        ,intent(in   ) :: dlmd,dphd
    integer(i_kind)                       ,intent(in   ) :: nord_blend
    real(r_kind)                          ,intent(  out) :: blndmsk(grd_ens%nlat,grd_ens%nlon)
  
    real(r_kind),parameter:: r300=300_r_kind
    real(r_kind),parameter:: r150=150_r_kind
    real(r_kind),parameter:: r111=111.12_r_kind
    real(r_kind) dx,dy,dr,x,y
    integer(i_kind) i,j,k,imin,jmin,imax,jmax
    integer(i_kind) mm
    integer(i_kind),dimension(0:40):: iblend
  
    blndmsk=zero
  
    dx=r111*dlmd
    dy=r111*dphd
  
    imin=max(1,INT(bc_lat-r300/dy))
    jmin=max(1,INT(bc_lon-r300/dx))
    imax=min(grd_ens%nlat,NINT(bc_lat+r300/dy))
    jmax=min(grd_ens%nlon,NINT(bc_lon+r300/dx)) 
  
  ! set up blend function
    mm=nord_blend
    call blend(mm,iblend)
    do j=jmin,jmax
       do i=imin,imax
          dr=sqrt((dy*(i-bc_lat))*(dy*(i-bc_lat))+(dx*(j-bc_lon))*(dx*(j-bc_lon)))
          if(dr <= r150)then
             blndmsk(i,j)=one
          else if(dr <= r300)then
             x=abs(dr-r300)/r150
             y=iblend(mm)
             do k=mm-1,0,-1
                y=x*y+iblend(k)
             end do
             y=y*x**(mm+1)
             blndmsk(i,j)=y
          end if
       end do
    end do
  
  end subroutine create_pseudo_enpert_blend
  
  subroutine pseudo_ens_e2a(lon_bc,lat_bc,lon_lc,lat_lc,nlone,nlate,e,blend,ratio_lon, &
                            ratio_lat,nlona,nlata,a)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    pseudo_ens_e2a move TC library ensemble grid to analysis grid
  !   prgmmr: mtong          org: np22                date: 2011-09-12
  !
  ! abstract: Move TC library ensemble grid to analysis grid, so that the 
  !           ensemble TC centers match the background TC center
  !
  ! program history log:
  !   2011-09-12  mtong, initial documentation
  !
  !   input argument list:
  !     lon_bc         - background TC center x index
  !     lat_bc         - background TC center y index
  !     lon_lc         - ensemble member TC center x index
  !     lat_lc         - ensemble member TC center y index
  !     nlone          - ensemble grid x dimension
  !     nlate          - ensemble grid y dimension 
  !     e              - ensemble member values on full ensemble global domain
  !     blend          - blending mask
  !     ratio_lon      - ensemble to analysis grid ratio in x direction
  !     ratio_lat      - ensemble to analysis grid ratio in y direction
  !     nlona          - analysis grid x dimension
  !     nlata          - analysis grid y dimension
  !
  !   output argument list:
  !     a              - ensemble member values on analysis grid
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    use kinds, only: r_kind,i_kind,r_single
    use constants, only: zero
    implicit none
  
    integer(i_kind)                       ,intent(in   ) :: lon_bc,lat_bc,lon_lc,lat_lc
    integer(i_kind)                       ,intent(in   ) :: nlone,nlate,nlona,nlata
    integer(i_kind)                       ,intent(in   ) :: ratio_lon,ratio_lat
    real(r_kind)                          ,intent(in   ) :: e(nlate,nlone)
    real(r_kind)                          ,intent(in   ) :: blend(nlata,nlona)
    real(r_kind)                          ,intent(  out) :: a(nlata,nlona)
  
    integer(i_kind) i,j,ii,jj
  
    do j=1,nlona
       jj=lon_lc+ratio_lon*(j-lon_bc)
       if( jj > 0 .and. jj <= nlone )then
          do i=1,nlata 
             ii=lat_lc+ratio_lat*(i-lat_bc)
             if(blend(i,j) > zero .and. ii > 0 .and. ii <= nlate)then
                a(i,j)=e(ii,jj)
             end if
          end do
       end if
    end do
                
  end subroutine pseudo_ens_e2a
end module get_pseudo_ensperts_mod 
