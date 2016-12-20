module reducedgrid_mod

! Module for transforming between reg (or gaussian) lat/lon
! grid to a reduced grid with a variable number on longitudes
! per latitude.  The number of longitudes is chosen so that
! the zonal grid spacing is approximately the same as at the equator.
! The reduced grid has approximately 33% fewer grid points.
! FFT interpolation is used to transform 
! to/from the reduced and full grids (using fftpack).

! reducedgrid_init: call first to initialize reduced grid data type.
! regtoreduced: transform data on full grid to reduced grid.
! reducedtoreg: inverse transform, reduced to full grid.
! reducedgrid_destroy: deallocate memory.

! Jeff Whitaker <jeffrey.s.whitaker@noaa.gov>
! 20100217

use kinds, only: r_kind,r_single
implicit none

private
public :: reducedgrid_init, reducedgrid_destroy, reducedtoreg, &
          regtoreduced

! number of lons on full grid.
integer,public :: nlonsfull
! number of lats on full grid.
integer,public :: nlatsfull
! latitudes on full grid
real(r_single), allocatable, public, dimension(:) :: latsfull
! number of lons for each latitude on reduced grid
integer, allocatable, public, dimension(:) :: lonsperlat
! total number of points on reduced grid.
integer,public :: nptsred
! lons and lats on reduced grid
real(r_single), allocatable, public, dimension(:) :: lonsred, latsred
logical, public :: is_initialized = .false.
! private data (fft work arrays)
real, allocatable, dimension(:) :: fftwork1
real, allocatable, dimension(:,:) :: fftwork2

contains

subroutine reducedgrid_init(nlons,nlats,lats)
 ! nlonsfull is (integer) number of lons on full grid.
 ! nlatsfull is (integer) number of lons on full grid.
 ! latsfull is a real array (size nlatsfull) containing latitudes
 ! of full grid in radians.
 integer, intent(in) :: nlons, nlats
 real(r_kind), intent(in) :: lats(nlats)
 integer nlonsred, nlat, nlon, n
 real(r_single) coslat, pi
 pi = 4.*atan(1.0)
 nlonsfull = nlons
 nlatsfull = nlats
 allocate(latsfull(nlatsfull))
 allocate(lonsperlat(nlatsfull))
 latsfull(:) = lats(:)
 nptsred = 0
 do nlat=1,nlatsfull
    coslat = cos(latsfull(nlat))
    nlon = ceiling(nlonsfull*coslat)
    lonsperlat(nlat) = nlon
    nptsred = nptsred + nlon
 enddo
 allocate(lonsred(nptsred))
 allocate(latsred(nptsred))
 n = 0
 allocate(fftwork1(2*nlonsfull+15))
 allocate(fftwork2(2*nlonsfull+15,nlatsfull))
 do nlat=1,nlatsfull
    nlonsred = lonsperlat(nlat)
    do nlon=1,nlonsred
       n = n + 1
       lonsred(n) = 2.*pi*float(nlon-1)/float(nlonsred)
       latsred(n) = latsfull(nlat)
    enddo
    call fftpack_rffti(nlonsred, fftwork2(1,nlat))
 enddo
 call fftpack_rffti(nlonsfull, fftwork1)
 is_initialized = .true.
end subroutine reducedgrid_init

subroutine reducedgrid_destroy()
 if (allocated(lonsperlat)) deallocate(lonsperlat)
 if (allocated(latsfull)) deallocate(latsfull)
 if (allocated(latsred)) deallocate(latsred)
 if (allocated(lonsred)) deallocate(lonsred)
 if (allocated(fftwork1)) deallocate(fftwork1)
 if (allocated(fftwork2)) deallocate(fftwork2)
 is_initialized = .false.
end subroutine reducedgrid_destroy

subroutine regtoreduced(datareg,datared)
 ! interpolate from regular (full) to reduced grid.
 ! datareg (size nlonsfull*nlatsfull) has data on 
 ! full grid.
 ! datared (size nptspred) is returned with data on
 ! reduced grid. 
 real(r_kind), intent(in), dimension(nlonsfull*nlatsfull):: datareg
 real(r_single), intent(out), dimension(nptsred) :: datared
 integer nlon, nlat, n, nlonsred
 real datareg_lon(nlonsfull)
 real fftwork(2*nlonsfull+15)

 n = 0
 do nlat=1,nlatsfull
    nlonsred = lonsperlat(nlat)
    datareg_lon(1:nlonsfull) = datareg((nlat-1)*nlonsfull+1:nlat*nlonsfull)
    fftwork = fftwork1
    call fftpack_rfftf(nlonsfull,datareg_lon,fftwork)
    fftwork = fftwork2(:,nlat)
    call fftpack_rfftb(nlonsred,datareg_lon,fftwork)
    do nlon=1,nlonsred
       n = n + 1
       datared(n) = datareg_lon(nlon)/real(nlonsfull)
    enddo    
 enddo   

end subroutine regtoreduced

subroutine reducedtoreg(datared,datareg)
 ! interpolate from reduced to regular (full) grid.
 ! datared (size nptspred) is data on reduced grid.
 ! datareg (size nlonsfull*nlatsfull) is returned with
 ! data on full grid.
 real(r_kind), intent(out), dimension(nlonsfull*nlatsfull) :: datareg
 real(r_single), intent(in), dimension(nptsred) :: datared
 integer nlon, nlat, nlonsred, n
 real datared_lon(nlonsfull)
 real fftwork(2*nlonsfull+15)

 n = 0
 do nlat=1,nlatsfull
    nlonsred = lonsperlat(nlat)
    datared_lon=0.
    do nlon=1,nlonsred
       n = n + 1
       datared_lon(nlon) = datared(n)
    enddo
    fftwork = fftwork2(:,nlat)
    call fftpack_rfftf(nlonsred,datared_lon,fftwork)
    fftwork = fftwork1
    call fftpack_rfftb(nlonsfull,datared_lon,fftwork)
    datareg((nlat-1)*nlonsfull+1:nlat*nlonsfull) = &
    datared_lon(1:nlonsfull)/real(nlonsred)
 enddo

end subroutine reducedtoreg

end module reducedgrid_mod
