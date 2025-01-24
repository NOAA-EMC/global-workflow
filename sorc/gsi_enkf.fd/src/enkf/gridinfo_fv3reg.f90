module gridinfo
!$$$  module documentation block
!
! module: gridinfo                     read horizontal (lons, lats) and
!                                      vertical (pressure) information from
!                                      ensemble mean first guess file.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: This module reads the ensemble mean background file and
! extracts information about the analysis grid, including the
! longitudes and latitudes of the analysis grid points and
! the pressure on each grid point/vertical level.
!
! Public Subroutines:
!   getgridinfo: read latitudes, longitudes, pressures and orography for analysis grid,
!    broadcast to each task. Compute spherical cartesian coordinate values
!    for each analysis horizontal grid point.
!   gridinfo_cleanup: deallocate allocated module variables.
!
! Public Variables:
!   npts: number of analysis grid points in the horizontal (from module params).
!   nlevs: number of analysis vertical levels (from module params).
!    specific humidity, ozone and cloud condensate).
!   ptop: (real scalar) pressure (hPa) at top model layer interface.
!   lonsgrd(npts): real array of analysis grid longitudes (radians).
!   latsgrd(npts): real array of analysis grid latitudes (radians).
!   logp(npts,ndim):  -log(press) for all 2d analysis grids. Assumed invariant
!   in assimilation window, computed fro ensemble mean at middle of window.
!   gridloc(3,npts): spherical cartesian coordinates (x,y,z) for analysis grid.
!
! Modules Used: mpisetup, params, kinds
!
! program history log:
!   2009-02-23  Initial version.
!   2016-05-02: shlyaeva: Modification for reading state vector from table
!   2016-04-20  Modify to handle the updated nemsio sig file (P, DP & DPDT removed)
!   2021-02-08  CAPS(J. Park) - Modified 'vars3d_supported' for direct reflectivity DA capability
!   2022-06-    Ting   --  Implement paranc=.true. for fv3-lam
!
! attributes:
!   language: f95
!
!$$$

use mpisetup, only: nproc, mpi_integer, mpi_real4,mpi_status
use mpimod, only: mpi_comm_world
use params, only: datapath,nlevs,nlons,nlats,use_gfs_nemsio, fgfileprefixes, &
                  fv3fixpath, nx_res,ny_res, ntiles,l_fv3reg_filecombined,paranc, &
                  fv3_io_layout_nx,fv3_io_layout_ny,taperanalperts,taperanalperts_akbot, &
                  taperanalperts_aktop
          
use kinds, only: r_kind, i_kind, r_double, r_single
use constants, only: one,zero,pi,cp,rd,grav,rearth,max_varname_length
use constants, only: half
use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
use netcdf, only: nf90_inq_dimid,nf90_inq_varid
use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
use netcdf_mod, only: nc_check
use read_fv3regional_restarts,only:read_fv3_restart_data1d,read_fv3_restart_data2d
use read_fv3regional_restarts,only:read_fv3_restart_data3d,read_fv3_restart_data4d

implicit none
private
public :: getgridinfo, gridinfo_cleanup
public :: ak,bk,eta1_ll,eta2_ll
real(r_single),public :: ptop
real(r_single),public, allocatable, dimension(:) :: lonsgrd, latsgrd
real(r_single),public, allocatable, dimension(:) :: taper_vert
! arrays passed to kdtree2 routines must be single
real(r_single),public, allocatable, dimension(:,:) :: gridloc
real(r_single),public, allocatable, dimension(:,:) :: logp
integer(i_kind),                                  public     :: nlevs_pres
integer(i_kind),public :: npts
integer(i_kind),public :: ntrunc
! supported variable names in anavinfo
character(len=max_varname_length),public, dimension(16) :: &
  vars3d_supported = [character(len=max_varname_length) :: &
    'u', 'v', 'w', 't', 'q', 'oz', 'cw', 'tsen', 'prse', &
    'ql', 'qi', 'qr', 'qs', 'qg', 'qnr','dbz']
character(len=max_varname_length),public, dimension(3) :: &
  vars2d_supported = [character(len=max_varname_length) :: &
    'ps', 'pst', 'sst']
character(len=max_varname_length),public, dimension(8)  :: &
  vars2d_landonly = (/'', '', '', '', '', '', '', '' /)
real(r_single), allocatable, dimension(:) :: ak,bk,eta1_ll,eta2_ll
integer (i_kind),public,allocatable,dimension(:,:):: nxlocgroup,nylocgroup
integer(i_kind):: numproc_io_sub
integer (i_kind),public,allocatable,dimension(:):: recvcounts2d,displs2d
integer (i_kind),public,allocatable,dimension(:,:):: ij_pe,myneb_table
contains

subroutine getgridinfo(fileprefix, reducedgrid)
! read latitudes, longitudes and pressures for analysis grid,
! broadcast to each task.
use read_fv3regional_restarts, only: read_fv3_restart_data2d
implicit none

character(len=120), intent(in) :: fileprefix
logical, intent(in)            :: reducedgrid

integer(i_kind) ierr,  k, nn 
character(len=500) filename
integer(i_kind) i,j,ij
real(r_kind), allocatable, dimension(:) :: spressmn
real(r_kind), allocatable, dimension(:,:) :: pressimn,presslmn
real(r_kind) kap,kapr,kap1

integer(i_kind) :: file_id,var_id,dim_id,nlevsp1,nx_tile,ny_tile,ntile
integer (i_kind):: nn_tile0
integer(i_kind) :: nlevsp1n
real(r_single), allocatable, dimension(:,:) :: lat_tile,lon_tile,ps
real(r_single), allocatable, dimension(:,:) :: loclat_tile,loclon_tile
real(r_single), allocatable, dimension(:,:,:) :: delp,g_prsi
real(r_single), allocatable, dimension(:,:,:) :: delploc
real(r_single) ptop
character(len=4) char_nxres
character(len=4) char_nyres
character(len=1) char_tile
character(len=24),parameter :: myname_ = 'fv3: getgridinfo'
integer (i_kind),allocatable,dimension(:):: mpi_id_group
integer(i_kind):: world_group,pe_group
integer (i_kind):: mpi_comm_userread,ierror,iope,nxloc,nyloc,ipe,jpe
character(len=24) strsubid
integer(i_kind)::ii,nx_res1,ny_res1
integer(i_kind):: iocomtest,iret,nproc1
write (6,*)"The input fileprefix, reducedgrid are not used in the current implementation", &
           fileprefix, reducedgrid
nlevsp1 = nlevs + 1
nlevs_pres = nlevsp1
npts = ntiles*nx_res*ny_res
kap = rd/cp
kapr = cp/rd
kap1 = kap + one

!when paranc=.false, fv3_io_layout_nx=fv3_io_layout_ny=1
! read data on root task
if (nproc == 0) then

   !  read ak,bk from ensmean fv_core.res.nc
   !  read nx,ny and nz from fv_core.res.nc
   filename = 'fv3sar_tile1_akbk.nc'
   call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
   myname_,'open: '//trim(adjustl(filename)) )
   call nc_check( nf90_inq_dimid(file_id,'xaxis_1',dim_id),&
       myname_,'inq_dimid xaxis_1 '//trim(filename) )
   call nc_check( nf90_inquire_dimension(file_id,dim_id,len=nlevsp1n),&
       myname_,'inquire_dimension xaxis_1 '//trim(filename) )
   if(nlevsp1n.ne.nlevsp1) then
     write(6,*)'the configure nlevsp1 is not consistent with the parameter &
                  read from the data files, stop'
     call stop2(25)
   endif



   allocate(ak(nlevsp1),bk(nlevsp1))
   allocate(eta1_ll(nlevsp1),eta2_ll(nlevsp1))
   call read_fv3_restart_data1d('ak',filename,file_id,ak)
   call read_fv3_restart_data1d('bk',filename,file_id,bk)

!!!!! change unit of ak,also reverse the 
     
   do i=1,nlevsp1
       eta1_ll(i)=ak(i)*0.01_r_kind
       eta2_ll(i)=bk(i)
   enddo

   ptop = eta1_ll(nlevsp1)
   call nc_check( nf90_close(file_id),&
   myname_,'close '//trim(filename) )

   ! vertical taper function for ens perts
   allocate(taper_vert(nlevs))
   if (taperanalperts) then
      do k=1,nlevs
         if (k < nlevs/2 .and. (ak(k) <= taperanalperts_akbot .and. ak(k) >= taperanalperts_aktop)) then
            taper_vert(nlevs-k+1)= log(ak(k) - taperanalperts_aktop)/log(taperanalperts_akbot - taperanalperts_aktop)
         else if (bk(k) == zero .and. ak(k) < taperanalperts_aktop) then
            taper_vert(nlevs-k+1) = zero
         endif
      enddo
      print *,'vertical taper for anal perts:'
      do k=1,nlevs
         print *,k,ak(nlevs-k+1),bk(nlevs-k+1),taper_vert(k)
      enddo
   else
      taper_vert = one
   endif

   deallocate(ak,bk)
endif ! root task

allocate(nxlocgroup(fv3_io_layout_nx,fv3_io_layout_ny))
allocate(nylocgroup(fv3_io_layout_nx,fv3_io_layout_ny))

if(nproc == 0) then 
  ii=0
  do j=1,fv3_io_layout_ny
    do i=1,fv3_io_layout_nx
      if(paranc) then
        write(strsubid,'(a,I4.4)')'.',ii
      else
        strsubid=""
      endif
      filename = 'fv3sar_tile1_grid_spec.nc'//trim(strsubid)
      call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
       myname_,'open: '//trim(adjustl(filename)) )
      call nc_check( nf90_inq_dimid(file_id,'grid_xt',dim_id),&
                     myname_,'inq_dimid grid_xt '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,dim_id,len=nxlocgroup(i,j)),&
                     myname_,'inquire_dimension grid_xt '//trim(filename) )
      call nc_check( nf90_inq_dimid(file_id,'grid_yt',dim_id),&
                     myname_,'inq_dimid grid_yt '//trim(filename) )
      call nc_check( nf90_inquire_dimension(file_id,dim_id,len=nylocgroup(i,j)),&
                     myname_,'inquire_dimension grid_yt '//trim(filename) )
      ii=ii+1
    enddo
  enddo

endif !nproc=0 

call mpi_bcast(nxlocgroup,size(nxlocgroup),mpi_integer,0,MPI_COMM_WORLD,ierr) ! 
call mpi_bcast(nylocgroup,size(nxlocgroup),mpi_integer,0,MPI_COMM_WORLD,ierr) !
numproc_io_sub=fv3_io_layout_ny*fv3_io_layout_nx
allocate(recvcounts2d(numproc_io_sub),displs2d(numproc_io_sub))
allocate(mpi_id_group(numproc_io_sub))
allocate(ij_pe(2,0:numproc_io_sub-1))
allocate(myneb_table(4,0:numproc_io_sub-1))
ij=1
do j=1,fv3_io_layout_ny
  do i=1,fv3_io_layout_nx
     recvcounts2d(ij) = nxlocgroup(i,j)*nylocgroup(i,j)
     if(ij == 1) then
       displs2d(ij) =0 
     else
       displs2d(ij) =displs2d(ij-1)+recvcounts2d(ij-1) 
     endif
     ij=ij+1
  end do
enddo 

nx_res1=sum(nxlocgroup(:,1))
ny_res1=sum(nylocgroup(1,:))
if(nx_res1.ne.nx_res.or.ny_res1.ne.ny_res) then
  write(6,*)'something wroing with dimensions of the whole domain and dimensions in subdomains, stop '
  call stop2(25)
endif


ij=0
do j=1,fv3_io_layout_ny
  do i=1,fv3_io_layout_nx
     ij_pe(1,ij)=i
     ij_pe(2,ij)=j
     !for Northen boundary ,the MPI ID on this direction , if no, =-1
     if(j.lt.fv3_io_layout_ny) then
        myneb_table(1,ij)=j*fv3_io_layout_nx+i-1
     else
        myneb_table(1,ij)=-1
     endif
     !for Easten boundary, 
     if(i.lt.fv3_io_layout_nx) then
        myneb_table(2,ij)=(j-1)*fv3_io_layout_nx+i
     else
        myneb_table(2,ij)=-1
     endif
     !for Sourthern boundary
     if(j.gt.1) then
        myneb_table(3,ij)=(j-2)*fv3_io_layout_nx+i-1
     else
        myneb_table(3,ij)=-1
     endif
     !for West boundary
     if(i.gt.1) then
        myneb_table(4,ij)=(j-1)*fv3_io_layout_nx+i-2
     else
        myneb_table(4,ij)=-1
     endif

     ij=ij+1
  enddo
enddo 
    


mpi_id_group=(/(i,i=0,numproc_io_sub-1)/)
call MPI_COMM_GROUP(MPI_COMM_WORLD, world_group, ierror)
if(any (mpi_id_group == nproc))  then ! if paranc=.fales., this equal to "nproc== 00 
  call MPI_GROUP_INCL(world_group, numproc_io_sub, mpi_id_group, pe_group, ierror)
  call MPI_COMM_CREATE_GROUP(MPI_COMM_WORLD, pe_group, 0, mpi_comm_userread, ierror)
endif
  write(char_nxres, '(i4)') nx_res
  write(char_nyres, '(i4)') ny_res
if(nproc==0) then 
   allocate(lat_tile(nx_res,ny_res),lon_tile(nx_res,ny_res))
else
   allocate(lat_tile(1,1),lon_tile(1,1))
endif

if(nproc == 0)  then
  nn = 0
  npts=nx_res*ny_res
  allocate(latsgrd(npts),lonsgrd(npts))
endif
if(any (mpi_id_group == nproc))  then ! if paranc=.fales., this equal to "nproc== 00 
  call mpi_comm_rank(mpi_comm_userread,iope, ierror)
  if(iope.ne.nproc) then
     write(6,*) "it is assumed the so generated subgroup of mpi should & 
      match the first N (size of the fomer) MPI process of the total MPI group, if not, & 
      something wrong, stop" 
     call stop2(25)
  endif
        




  ipe=ij_pe(1,nproc)
  jpe=ij_pe(2,nproc)
  nxloc=nxlocgroup(ipe,jpe)
  nyloc=nylocgroup(ipe,jpe)

  allocate(loclat_tile(nxloc,nyloc),loclon_tile(nxloc,nyloc))
  do ntile=1,ntiles
     nn_tile0=(ntile-1)*nx_res*ny_res
     write(char_tile, '(i1)') ntile
     i=ij_pe(1,nproc)
     j=ij_pe(2,nproc)
     ii=(j-1)*fv3_io_layout_nx+i-1 
     if(paranc) then
       write(strsubid,'(a,I4.4)')'.',ii
     else
       strsubid=""
     endif
     filename='fv3sar_tile'//char_tile//'_grid_spec.nc'//trim(strsubid)
     
     call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
     myname_,'open: '//trim(adjustl(filename)) )
     call read_fv3_restart_data2d('grid_lont',filename,file_id,loclon_tile)
     call read_fv3_restart_data2d('grid_latt',filename,file_id,loclat_tile)
     call nc_check( nf90_close(file_id),&
     myname_,'close '//trim(filename) )
     if(paranc) then
        call mpi_gatherv(loclat_tile, recvcounts2d(nproc+1), mpi_real4, lat_tile, recvcounts2d, displs2d,&
                mpi_real4, 0,mpi_comm_userread ,ierror)
        call mpi_gatherv(loclon_tile, recvcounts2d(nproc+1), mpi_real4, lon_tile, recvcounts2d, displs2d,&
                mpi_real4, 0,mpi_comm_userread ,ierror)
     else
        if(nproc==0) then
          lon_tile=loclon_tile
          lat_tile=loclat_tile
        endif
     endif
     if (nproc == 0) then 
        nn = nn_tile0
        do j=1,ny_res
           do i=1,nx_res
              nn=nn+1
              latsgrd(nn) = lat_tile(i,j)
              lonsgrd(nn) = lon_tile(i,j)
           enddo
        enddo
     endif
  enddo  !loop for ntilet
  if(nproc == 0) then
    latsgrd = pi*latsgrd/180._r_single
    lonsgrd = pi*lonsgrd/180._r_single
  endif 
endif ! nproc in mpi_id_group

call mpi_bcast(npts,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

if(nproc==0 )then
  allocate(delp(nx_res,ny_res,nlevs),ps(nx_res,ny_res))
  allocate(g_prsi(nx_res,ny_res,nlevsp1))
  allocate(pressimn(npts,nlevsp1),presslmn(npts,nlevs))
  allocate(spressmn(npts))
else
  allocate(delp(1,1,nlevs),ps(1,1))
  allocate(g_prsi(1,1,nlevs))
  allocate(pressimn(1,1),presslmn(1,1))
  allocate(spressmn(1))
endif

if(any (mpi_id_group == nproc))  then ! if paranc=.fales., this equal to "nproc== 00 
  nn = 0
  do ntile=1,ntiles
     nn_tile0=(ntile-1)*nx_res*ny_res
     nn=nn_tile0
     write(char_tile, '(i1)') ntile
     i=ij_pe(1,nproc)
     j=ij_pe(2,nproc)
     nxloc=nxlocgroup(i,j) 
     nyloc=nylocgroup(i,j) 
     allocate(delploc(nxloc,nyloc,nlevs))
     ii=(j-1)*fv3_io_layout_nx+i-1 

     if (paranc ) then
          write(strsubid,'(a,I4.4)')'.',ii
     else
          strsubid=""
     endif
     if(l_fv3reg_filecombined) then
       filename = 'fv3sar_tile'//char_tile//"_ensmean_dynvartracer"//trim(strsubid)
     else
       filename = 'fv3sar_tile'//char_tile//"_ensmean_dynvars"//trim(strsubid)
     endif 
     call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
     myname_,'open: '//trim(adjustl(filename)) )
     call read_fv3_restart_data3d('delp',filename,file_id,delploc)
     call nc_check( nf90_close(file_id),&
     myname_,'close '//trim(filename) )
     if(paranc) then
       do k=1,nlevs
          call mpi_gatherv(delploc(:,:,k), recvcounts2d(nproc+1), mpi_real4, delp(:,:,k), recvcounts2d, displs2d,&
                      mpi_real4, 0,mpi_comm_userread ,ierror)
       enddo
     else 
        delp=delploc  
     endif
     deallocate(delploc)

     if(nproc == 0) then 
        g_prsi(:,:,nlevsp1)=eta1_ll(nlevsp1) !etal_ll is needed
        do i=nlevs,1,-1
          g_prsi(:,:,i)=delp(:,:,i)*0.01_r_kind+g_prsi(:,:,i+1)
        enddo

       ps = g_prsi(:,:,1)
       nn=nn_tile0
       do j=1,ny_res
          do i=1,nx_res
             nn=nn+1
             spressmn(nn) = ps(i,j)
          enddo
       enddo



    ! pressure at interfaces
       do k=1,nlevsp1
          nn=nn_tile0
          do j=1,ny_res
            do i=1,nx_res  
              nn=nn+1
              pressimn(nn,k) = g_prsi(i,j,k)
            enddo
          enddo
       enddo
       do k=1,nlevs
          nn=nn_tile0
          do j=1,ny_res
            do i=1,nx_res  
              nn=nn+1
              presslmn(nn,k) = (pressimn(nn,k)+pressimn(nn,k+1)) *half
            enddo
          enddo
       end do
    endif !nproc==0
  enddo !ntile

  if(nproc ==0 ) then
    print *,'ensemble mean first guess surface pressure:'
    print *,minval(spressmn),maxval(spressmn)
   ! logp holds log(pressure) or pseudo-height on grid, for each level/variable.
    allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
    do k=1,nlevs
       ! all variables to be updated are on mid-layers, not layer interfaces.
       logp(:,k) = -log(presslmn(:,k))
       print *,'min/max presslmn',k,minval(presslmn(:,k)),maxval(presslmn(:,k)),minval(logp(:,k)),maxval(logp(:,k))
    end do
    logp(:,nlevs_pres) = -log(spressmn(:))
  endif
  deallocate(spressmn,presslmn,pressimn)
  deallocate(ps)
  deallocate(g_prsi,delp)
  deallocate(lat_tile,lon_tile)
endif ! nproc in IO group 

allocate(gridloc(3,npts))
if (nproc .ne. 0) then
   ! allocate arrays on other (non-root) tasks
   allocate(latsgrd(npts),lonsgrd(npts),taper_vert(nlevs))
   allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
   allocate(eta1_ll(nlevsp1),eta2_ll(nlevsp1))
endif
if(nproc==0) write(6,*)logp(1,:)
do k=1,nlevs_pres
  call mpi_bcast(logp(1,k),npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
enddo
call mpi_bcast(lonsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(latsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(taper_vert,nlevs,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(eta1_ll,nlevsp1,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(eta2_ll,nlevsp1,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(ptop,1,mpi_real4,0,MPI_COMM_WORLD,ierr)
  
!==> precompute cartesian coords of analysis grid points.
do nn=1,npts
   gridloc(1,nn) = cos(latsgrd(nn))*cos(lonsgrd(nn))
   gridloc(2,nn) = cos(latsgrd(nn))*sin(lonsgrd(nn))
   gridloc(3,nn) = sin(latsgrd(nn))
end do
deallocate (mpi_id_group)
end subroutine getgridinfo

subroutine gridinfo_cleanup()
if (allocated(lonsgrd)) deallocate(lonsgrd)
if (allocated(latsgrd)) deallocate(latsgrd)
if (allocated(taper_vert)) deallocate(taper_vert)
if (allocated(logp)) deallocate(logp)
if (allocated(gridloc)) deallocate(gridloc)
end subroutine gridinfo_cleanup

end module gridinfo
