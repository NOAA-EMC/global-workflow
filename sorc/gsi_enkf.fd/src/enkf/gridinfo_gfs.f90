module gridinfo
!$$$  module documentation block
!
! module: gridinfo                     read horizontal (lons, lats) and
!                                      vertical (pressure) information from
!                                      ensemble mean first guess file.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: This module reads gfg_YYYYMMDDHH_fhr06_ensmean, and
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
!   ntrac: number of 'tracer' model state variables (3 for GFS,
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
!   2019-03-13  Add precipitation components
!
! attributes:
!   language: f95
!
!$$$

use mpisetup, only: nproc, mpi_integer, mpi_real4
use mpimod, only: mpi_comm_world
use params, only: datapath,nlevs,nlons,nlats,use_gfs_nemsio,use_gfs_ncio,fgfileprefixes,&
                  taperanalperts,taperanalperts_aktop,taperanalperts_akbot
use kinds, only: r_kind, i_kind, r_double, r_single
use constants, only: one,zero,pi,cp,rd,grav,rearth,max_varname_length
use specmod, only: sptezv_s, sptez_s, init_spec_vars, isinitialized, asin_gaulats, &
    ndimspec => nc
use reducedgrid_mod, only: reducedgrid_init, regtoreduced, reducedtoreg,&
                           nptsred, lonsred, latsred
implicit none
private
public :: getgridinfo, gridinfo_cleanup
integer(i_kind),public :: nlevs_pres, idvc
real(r_single),public :: ptop
real(r_single),public, allocatable, dimension(:) :: lonsgrd, latsgrd, taper_vert
! arrays passed to kdtree2 routines must be single
real(r_single),public, allocatable, dimension(:,:) :: gridloc
real(r_single),public, allocatable, dimension(:,:) :: logp
integer,public :: npts
integer,public :: ntrunc
! supported variable names in anavinfo
character(len=max_varname_length),public, dimension(13) :: vars3d_supported = (/'u   ', 'v   ', 'tv  ', 'q   ', 'oz  ', 'cw  ', 'tsen', 'prse', &
                                                                                'ql  ', 'qi  ', 'qr  ', 'qs  ', 'qg  '/) 
character(len=max_varname_length),public, dimension(13)  :: vars2d_supported = (/'ps ', 'pst', 'sst', 't2m', 'q2m', 'st1', 'st2', 'st3', 'st4', 'sl1', 'sl2', 'sl3', 'sl4' /)
character(len=max_varname_length),public, dimension(8)  :: vars2d_landonly = (/'st1', 'st2', 'st3', 'st4', 'sl1', 'sl2', 'sl3', 'sl4' /)
! supported variable names in anavinfo
contains

subroutine getgridinfo(fileprefix, reducedgrid)
! read latitudes, longitudes and pressures for analysis grid,
! broadcast to each task.
use sigio_module, only: sigio_head, sigio_data, sigio_sclose, sigio_sropen, &
                        sigio_srohdc, sigio_sclose, sigio_srhead, sigio_axdata
use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                         nemsio_getfilehead,nemsio_getheadvar,&
                         nemsio_readrecv,nemsio_init, nemsio_realkind
use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                       read_attribute, close_dataset, get_dim, read_vardata 
implicit none

type(Dataset) :: dset
type(Dimension) :: londim,latdim,levdim
character(len=120), intent(in) :: fileprefix
logical, intent(in)            :: reducedgrid

integer(i_kind) nlevsin, ierr, iunit, k, nn, idvc 
character(len=500) filename
integer(i_kind) iret,i,j,nlonsin,nlatsin
real(r_kind), allocatable, dimension(:) :: ak,bk,spressmn,tmpspec
real(r_kind), allocatable, dimension(:,:) :: pressimn,presslmn,values_2d
real(r_single),allocatable,dimension(:,:,:) :: nems_vcoord
real(r_kind) kap,kapr,kap1
real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk
type(sigio_data) sigdata
type(sigio_head) sighead
type(nemsio_gfile) :: gfile

iunit = 77
kap = rd/cp
kapr = cp/rd
kap1 = kap + one
nlevs_pres=nlevs+1
if (nproc == 0) then
filename = trim(adjustl(datapath))//trim(adjustl(fileprefix))//"ensmean"
if (use_gfs_nemsio) then
     call nemsio_init(iret=iret)
     if(iret/=0) then
        write(6,*)'grdinfo: gfs model: problem with nemsio_init, iret=',iret, ', file: ', trim(filename)
        call stop2(23)
     end if
     call nemsio_open(gfile,filename,'READ',iret=iret)
     if (iret/=0) then
        write(6,*)'grdinfo: gfs model: problem with nemsio_open, iret=',iret, ', file: ', trim(filename)
        call stop2(23)
     endif
     call nemsio_getfilehead(gfile,iret=iret, dimx=nlonsin, dimy=nlatsin,&
                             dimz=nlevsin,jcap=ntrunc,idvc=idvc)
     ! set ntrunc to nlats if missing
     ! (only used for inflation smoothing and mass balance adjustment if use_gfsnemsio = T)
     ! FV3GFS write component does not include JCAP, infer from nlatsin
     if (ntrunc < 0) ntrunc = nlatsin-2
     if (iret/=0) then
        write(6,*)'grdinfo: gfs model: problem with nemsio_getfilehead, iret=',iret, ', file: ', trim(filename)
        call stop2(23)
     endif
     print *,'ntrunc = ',ntrunc
     if (nlons /= nlonsin .or. nlats /= nlatsin .or. nlevs /= nlevsin) then
       print *,'incorrect dims in nemsio file'
       print *,'expected',nlons,nlats,nlevs
       print *,'got',nlonsin,nlatsin,nlevsin
       call stop2(23)
     end if
else if (use_gfs_ncio) then
     dset = open_dataset(filename)
     londim = get_dim(dset,'grid_xt'); nlonsin = londim%len
     latdim = get_dim(dset,'grid_yt'); nlatsin = latdim%len
     levdim = get_dim(dset,'pfull');   nlevsin = levdim%len
     idvc = 2; ntrunc = nlatsin-2
     if (nlons /= nlonsin .or. nlats /= nlatsin .or. nlevs /= nlevsin) then
       print *,'incorrect dims in netcdf file'
       print *,'expected',nlons,nlats,nlevs
       print *,'got',nlonsin,nlatsin,nlevsin
       call stop2(23)
     end if
else
     ! define sighead on all tasks.
     call sigio_sropen(iunit,trim(filename),iret)
     if (iret /= 0) then
        print *,'error reading file in gridinfo ',trim(filename),' on task',nproc
        call stop2(24)
     end if
     call sigio_srhead(iunit,sighead,iret)
     if (iret /= 0) then
        print *,'error reading file in gridinfo ',trim(filename),' on task',nproc
        call stop2(24)
     end if
     call sigio_sclose(iunit,iret)
     ntrunc = sighead%jcap
endif
endif
call mpi_bcast(ntrunc,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

! initialize spectral module on all tasks.
if (.not. isinitialized) call init_spec_vars(nlons,nlats,ntrunc,4)

if (nproc == 0) then
   ! get pressure, lat/lon information from ensemble mean file.
   allocate(presslmn(nlons*nlats,nlevs))
   allocate(pressimn(nlons*nlats,nlevs+1))
   allocate(spressmn(nlons*nlats))
   allocate(taper_vert(nlevs))
   taper_vert=one
   if (use_gfs_nemsio) then
      call nemsio_readrecv(gfile,'pres','sfc',1,nems_wrk,iret=iret)
      if (iret/=0) then
          write(6,*)'grdinfo: gfs model: problem with nemsio_readrecv(ps), iret=',iret
          call stop2(23)
      endif

!     Extract vertical coordinate descriptions nems_vcoord.
!     nems_vcoord(gfshead%levs+1,3,2) dimension is hardwired here.
!     Present NEMSIO modules do not allow flexibility of 2nd and 3rd
!     array dimension for nems_vcoord, for now, it is hardwired as
!     (levs,3,2) If NEMS changes the setting of vcoord dimension,
!     GSI needs to update its setting of nems_vcoord accordingly.

      if (allocated(nems_vcoord))     deallocate(nems_vcoord)
      allocate(nems_vcoord(nlevs_pres,3,2))
      call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord)
      if ( iret /= 0 ) then
         write(6,*)' gridinfo:  ***ERROR*** problem reading header ', &
            'vcoord, Status = ',iret
         call stop2(99)
      endif

      spressmn = 0.01_r_kind*nems_wrk ! convert ps to millibars.
      !print *,'min/max spressmn = ',minval(spressmn),maxval(spressmn)

      allocate(ak(nlevs+1),bk(nlevs+1))

      if ( idvc == 0 ) then                         ! sigma coordinate, old file format.
         ak = zero
         bk = nems_vcoord(1:nlevs+1,1,1)
      elseif ( idvc == 1 ) then                     ! sigma coordinate
         ak = zero
         bk = nems_vcoord(1:nlevs+1,2,1)
      elseif ( idvc == 2 .or. idvc == 3 ) then      ! hybrid coordinate
         ak = 0.01_r_kind*nems_vcoord(1:nlevs+1,1,1) ! convert to mb
         bk = nems_vcoord(1:nlevs+1,2,1)
      else
         write(6,*)'gridinfo:  ***ERROR*** INVALID value for idvc=',idvc
         call stop2(85)
      endif

      ! pressure at interfaces
      do k=1,nlevs+1
         pressimn(:,k) = ak(k)+bk(k)*spressmn(:)
      enddo
      call nemsio_close(gfile, iret=iret)
      ptop = ak(nlevs+1)
   else if (use_gfs_ncio) then
      call read_vardata(dset, 'pressfc', values_2d,errcode=iret)
      if (iret /= 0) then
         print *,'error reading ps in gridinfo_gfs'
         call stop2(11)
      endif
      ! convert to 1d array, units to millibars, flip so lats go N to S.
      spressmn = 0.01_r_kind*reshape(values_2d,(/nlons*nlats/))
      call read_attribute(dset, 'ak', ak)
      call read_attribute(dset, 'bk', bk)
      call close_dataset(dset)
      ! pressure at interfaces
      do k=1,nlevs+1
         pressimn(:,k) = 0.01_r_kind*ak(nlevs-k+2)+bk(nlevs-k+2)*spressmn(:)
      enddo
      ptop = 0.01_r_kind*ak(1)
      deallocate(values_2d)
   else
! get pressure from ensemble mean,
! distribute to all processors.
      call sigio_srohdc(iunit,trim(filename), &
                       sighead,sigdata,iret)
      if (iret /= 0) then
         print *,'error reading file in gridinfo',trim(filename)
         call stop2(24)
      end if
      nlevsin = sighead%levs
      if (nlevs .ne. nlevsin) then
        print *,'error reading input file in gridinfo - nlevs != ',nlevsin,nlevs
        call stop2(24)
      end if
      allocate(ak(nlevs+1),bk(nlevs+1))
      if (sighead%idvc == 0) then ! sigma coordinate, old file format.
         ak = zero
         bk = sighead%si(1:nlevs+1)
      else if (sighead%idvc == 1) then ! sigma coordinate
         ak = zero
         bk = sighead%vcoord(1:nlevs+1,2)
      else if (sighead%idvc == 2 .or. sighead%idvc == 3) then ! hybrid coordinate
         ak = 0.01_r_kind*sighead%vcoord(1:nlevs+1,1)          ! convert to mb
         bk = sighead%vcoord(1:nlevs+1,2)
      else
         print *,'unknown vertical coordinate type',sighead%idvc
         call stop2(24)
      end if
      allocate(tmpspec(ndimspec))
      tmpspec = sigdata%ps
      call sptez_s(tmpspec,spressmn,1)
      deallocate(tmpspec)
      spressmn = 10._r_kind*exp(spressmn)
      ! pressure at interfaces
      do k=1,nlevs+1
         pressimn(:,k) = ak(k)+bk(k)*spressmn(:)
      enddo
      call sigio_axdata(sigdata,iret)
      ptop = ak(nlevs+1)
   endif
   if (reducedgrid) then
      call reducedgrid_init(nlons,nlats,asin_gaulats)
      npts = nptsred
   else
      npts = nlons*nlats
   end if
   allocate(latsgrd(npts),lonsgrd(npts))
   allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
   allocate(gridloc(3,npts))
   !==> pressure at interfaces.
   if (reducedgrid) then
      lonsgrd(:) = lonsred(:)
      latsgrd(:) = latsred(:)
   else
      nn = 0
      do j=1,nlats
         do i=1,nlons
            nn = nn + 1
            lonsgrd(nn) = 2._r_single*pi*float(i-1)/nlons
            latsgrd(nn) = asin_gaulats(j)
         enddo
      enddo
   endif
   do k=1,nlevs
     ! layer pressure from Phillips vertical interpolation.
     presslmn(:,k) = ((pressimn(:,k)**kap1-pressimn(:,k+1)**kap1)/&
                      (kap1*(pressimn(:,k)-pressimn(:,k+1))))**kapr
   end do
   print *,'ensemble mean first guess surface pressure:'
   print *,minval(spressmn),maxval(spressmn)
   !do k=1,nlevs
   !   print *,'min/max ens mean press level',&
   !   k,'=',minval(presslmn(:,k)),maxval(presslmn(:,k))
   !   print *,'min/max ens mean press interface',&
   !   k,'=',minval(pressimn(:,k)),maxval(pressimn(:,k))
   !enddo
   ! logp holds log(pressure) or pseudo-height on grid, for each level/variable.
   do k=1,nlevs
      ! all variables to be updated are on mid-layers, not layer interfaces.
      if (reducedgrid) then
         call regtoreduced(presslmn(:,k),logp(:,k))
         logp(:,k) = -log(logp(:,k))
      else
         logp(:,k) = -log(presslmn(:,k))
      endif
      !print *,'min/max presslmn',k,minval(presslmn(:,k)),maxval(presslmn(:,k)),minval(logp(:,k)),maxval(logp(:,k))
   end do
   if (reducedgrid) then
      call regtoreduced(spressmn,logp(:,nlevs_pres))
      logp(:,nlevs_pres) = -log(logp(:,nlevs_pres))
   else
      logp(:,nlevs_pres) = -log(spressmn(:))
   endif
   deallocate(spressmn,presslmn,pressimn)
  ! vertical taper function for ens perts
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
  endif
  if (allocated(ak)) deallocate(ak)
  if (allocated(bk)) deallocate(bk)
end if
call mpi_bcast(npts,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
if (nproc .ne. 0) then
   ! allocate arrays on other (non-root) tasks
   allocate(latsgrd(npts),lonsgrd(npts))
   allocate(taper_vert(nlevs))
   allocate(logp(npts,nlevs_pres)) ! log(ens mean first guess press) on mid-layers
   allocate(gridloc(3,npts))
   ! initialize reducedgrid_mod on other tasks.
   if (reducedgrid) then
      call reducedgrid_init(nlons,nlats,asin_gaulats)
   end if
endif
!call mpi_bcast(logp,npts*nlevs_pres,mpi_real4,0,MPI_COMM_WORLD,ierr)
do k=1,nlevs_pres
  call mpi_bcast(logp(1,k),npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
enddo
call mpi_bcast(lonsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(latsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(taper_vert,nlevs,mpi_real4,0,MPI_COMM_WORLD,ierr)
call mpi_bcast(ptop,1,mpi_real4,0,MPI_COMM_WORLD,ierr)
!==> precompute cartesian coords of analysis grid points.
do nn=1,npts
   gridloc(1,nn) = cos(latsgrd(nn))*cos(lonsgrd(nn))
   gridloc(2,nn) = cos(latsgrd(nn))*sin(lonsgrd(nn))
   gridloc(3,nn) = sin(latsgrd(nn))
end do

end subroutine getgridinfo

subroutine gridinfo_cleanup()
if (allocated(lonsgrd)) deallocate(lonsgrd)
if (allocated(latsgrd)) deallocate(latsgrd)
if (allocated(taper_vert)) deallocate(taper_vert)
if (allocated(logp)) deallocate(logp)
if (allocated(gridloc)) deallocate(gridloc)
end subroutine gridinfo_cleanup

end module gridinfo
