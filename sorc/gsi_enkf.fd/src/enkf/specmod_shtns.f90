      module specmod

! drop-in replacement specmod.f90 that uses shtns insted of sp library (shtns is
! *much* faster). All transform calcs done in double precision,
! regardless of precision of input arrays.

! public subroutines:
!    init_spec_vars
!    destroy_spec_vars
!    sptez_s
!    sptezv_s

! public variable definitions below
!   def jcap          - spectral (assumed triangular) truncation
!   def nc            - (N+1)*(N+2); N=truncation==jcap
!   def ncd2          - [(N+1)*(N+2)]/2; N=truncation==jcap
!   def idrt          - integer grid identifier
!                       (idrt=4 for gaussian grid,
!                        idrt=0 for equally-spaced grid including poles,
!                        idrt=256 for equally-spaced grid excluding poles)
!   def imax          - integer even number of longitudes for transform
!   def jmax          - integer number of latitudes for transform
!   def gaulats       - sin of latitudes on grid.
!   def gauwts        - gaussian weights (only relevant for idrt=4). 
!                       if idrt !=. 4, set to cos(sin(gaulats)).
!   def rerth         - radius of earth used in sptezv_s
!   def isinitialized - true if module has been initialized by a call to init_spec_vars.

      use kinds, only: r_kind, r_double
      implicit none 
      private 
      public :: init_spec_vars,sptez_s,sptezv_s,destroy_spec_vars
      public :: gaulats, gauwts, nc, ncd2, asin_gaulats,&
                isinitialized, imax, jmax, jcap, rerth
      INTEGER, PARAMETER :: SHT_NATIVE_LAYOUT=0
      INTEGER, PARAMETER :: SHT_THETA_CONTIGUOUS=256
      INTEGER, PARAMETER :: SHT_PHI_CONTIGUOUS=512
      INTEGER, PARAMETER :: SHT_NO_CS_PHASE=1024
      INTEGER, PARAMETER :: SHT_REAL_NORM=2048
      INTEGER, PARAMETER :: SHT_ORTHONORMAL=0
      INTEGER, PARAMETER :: SHT_FOURPI=1
      INTEGER, PARAMETER :: SHT_SCHMIDT=2
      INTEGER, PARAMETER :: SHT_GAUSS=0
      INTEGER, PARAMETER :: SHT_AUTO=1
      INTEGER, PARAMETER :: SHT_REG_FAST=2
      INTEGER, PARAMETER :: SHT_REG_DCT=3
      INTEGER, PARAMETER :: SHT_QUICK_INIT=4
      INTEGER, PARAMETER :: SHT_REG_POLES=5
      INTEGER, PARAMETER :: SHT_GAUSS_FLY=6
      REAL(r_double), PARAMETER :: SHT_DEFAULT_POLAR_OPT=1.d-10
      REAL(r_double), parameter :: rerth=6.3712E6
      REAL(r_double), parameter :: sqrt2=1.41421356237309504880
      INTEGER            :: imax   
      INTEGER            :: jmax   
      INTEGER            :: ijmax   
      INTEGER            :: jcap
      INTEGER            :: nc
      INTEGER            :: ncd2
      INTEGER            :: idrt
      LOGICAL            :: isinitialized=.false.
      REAL(r_kind), DIMENSION(:), ALLOCATABLE :: gauwts, gaulats, &
         asin_gaulats
      REAL(r_double), DIMENSION(:), ALLOCATABLE :: lap, invlap

      contains

      subroutine init_spec_vars(nlons,nlats,mtrunc,igrid)

! initialize library, allocate arrays.
! nlons: number of longitude points.
! nlats: number of latitude points.
! ntrunc: spectral truncation wavenumber.
! idir: grid (4=gaussian,0=reg including poles,256=reg not including
! poles

      integer, intent(in) :: nlons,nlats,mtrunc
      integer, intent(in):: igrid
      real(r_double), dimension(:), allocatable :: gauwts1,gaulats1
      integer m,n,j,nlm

      call destroy_spec_vars()
      call shtns_use_threads(1) ! number of openmp threads.
      call shtns_set_size(mtrunc,mtrunc,1,SHT_FOURPI+SHT_NO_CS_PHASE)

      if (igrid .eq. 4) then ! gaussian grid
         call shtns_precompute(SHT_GAUSS_FLY,SHT_PHI_CONTIGUOUS,SHT_DEFAULT_POLAR_OPT,nlats,nlons)
      else if (igrid .eq. 0) then ! reg grid including poles
         call shtns_precompute(SHT_REG_POLES,SHT_PHI_CONTIGUOUS,SHT_DEFAULT_POLAR_OPT,nlats,nlons)
      else if (igrid .eq. 256) then ! reg grid not including poles
         call shtns_precompute(SHT_REG_DCT,SHT_PHI_CONTIGUOUS,SHT_DEFAULT_POLAR_OPT,nlats,nlons)
      else
         print *,'igrid must be 4,0 or 256'
         call stop2(-99)
      end if

      idrt = igrid; jmax = nlats; imax = nlons; jcap = mtrunc
      ijmax = imax*jmax

      call shtns_calc_nlm(nlm,jcap,jcap,1)
      nc = 2*nlm
      ncd2 = nlm
      if (ncd2 .ne. (jcap+1)*(jcap+2)/2) then
         print *,'error: ncd2 not what expected',ncd2,jcap
         call stop2(-99)
      endif

      allocate(gaulats(jmax))
      allocate(asin_gaulats(jmax))
      allocate(gauwts(jmax))

      allocate(gaulats1(jmax))
      call shtns_cos_array(gaulats1)
      gaulats = gaulats1
      asin_gaulats = asin(gaulats)
      deallocate(gaulats1)

      if (idrt .eq. 4) then
         allocate(gauwts1(jmax/2))
         call shtns_gauss_wts(gauwts1)
         do j=1,jmax/2
            gauwts(j) = gauwts1(j)
            gauwts(jmax-j+1) = gauwts1(j)
         enddo
         deallocate(gauwts1)
      else
         gauwts = cos(asin(gaulats))
      endif

      allocate(lap(nlm))
      allocate(invlap(nlm))
      ! laplacian operator * sqrt2 * rerth
      j = 1
      do m=0,jcap
         do n=m,jcap
            lap(j) = real(n)
            j = j + 1
         enddo
      enddo
      lap = -sqrt2*lap*(lap+1.0)/rerth
      invlap = 0
      invlap(2:) = 1./lap(2:)

      isinitialized = .true.

      end subroutine init_spec_vars

      subroutine destroy_spec_vars()

! deallocate arrays.

      call shtns_reset()

      if (allocated(gaulats)) deallocate(gaulats)
      if (allocated(asin_gaulats)) deallocate(asin_gaulats)
      if (allocated(gauwts)) deallocate(gauwts)
      if (allocated(lap)) deallocate(lap)
      if (allocated(invlap)) deallocate(invlap)

      isinitialized = .false.

      end subroutine destroy_spec_vars

      subroutine sptez_s(dataspec,datagrid,idir)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_s       perform a simple scalar spherical transform
!
! absract: this subprogram performs a spherical transform
!           between spectral coefficients of a scalar quantity
!           and a field on a global cylindrical grid.
!           the wave-space is triangular.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave field is in sequential 'ibm order'.
!           the grid field is indexed east to west, then north to south.
!
!   input arguments:
!     dataspec - real (2*mx) wave field if idir>0
!                where mx=(jcap+1)*(jcap+2)/2
!     datagrid - real (imax,jmax) grid field (e->w,n->s) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     dataspec - real (2*mx) wave field if idir<0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     datagrid - real (imax,jmax) grid field (e->w,n->s) if idir>0
!
!$$$

      real(r_kind), dimension(ijmax), intent(inout)  :: datagrid
      real(r_kind), dimension(nc), intent(inout)     :: dataspec
      real(r_double), dimension(:,:),  allocatable   :: datagrid_tmp
      complex(r_double), dimension(:), allocatable   :: dataspec_tmp

      integer, intent(in) :: idir
      integer nm,nn,i,j
      allocate(datagrid_tmp(imax,jmax))
      allocate(dataspec_tmp(ncd2))

      if (idir .eq. -1) then ! grid to spec

         nn = 0
         do j=1,jmax
         do i=1,imax
            nn = nn + 1
            datagrid_tmp(i,j) = datagrid(nn)
         enddo
         enddo
         call shtns_spat_to_sh(datagrid_tmp, dataspec_tmp)
         nn = 1
         do nm=1,ncd2
            dataspec(nn) = real(dataspec_tmp(nm))
            dataspec(nn+1) = imag(dataspec_tmp(nm))
            nn = nn + 2
         enddo
         dataspec = sqrt2*dataspec

      else if (idir .eq. 1) then ! spec to grid

         nn = 1
         do nm=1,ncd2
            dataspec_tmp(nm) = cmplx(dataspec(nn),dataspec(nn+1))
            nn = nn + 2
         enddo
         dataspec_tmp = dataspec_tmp/sqrt2
         call shtns_sh_to_spat(dataspec_tmp, datagrid_tmp)
         nn = 0
         do j=1,jmax
         do i=1,imax
            nn = nn + 1
            datagrid(nn) = datagrid_tmp(i,j)
         enddo
         enddo

      else
         print *,'idir must be 1 or -1'
         call stop2(-99)

      endif

      deallocate(datagrid_tmp,dataspec_tmp)

      end subroutine sptez_s

      subroutine sptezv_s(divspec,vrtspec,ugrid,vgrid,idir)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sptez_v       perform a simple vector spherical transform
!
! abstract: this subprogram performs a spherical transform
!           between spectral coefficients of divergence and curl
!           and a vector field on a global cylindrical grid.
!           the wave-space is triangular.
!           the grid-space can be either an equally-spaced grid
!           (with or without pole points) or a gaussian grid.
!           the wave field is in sequential 'ibm order'.
!           the grid field is indexed east to west, then north to south.
!
!   input arguments:
!     vrtspec  - real (2*mx) wave divergence field if idir>0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     divspec  - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     ugrid    - real (imax*jmax) grid u-wind (e->w,n->s) if idir<0
!     vgrid    - real (imax*jmax) grid v-wind (e->w,n->s) if idir<0
!     idir     - integer transform flag
!                (idir>0 for wave to grid, idir<0 for grid to wave)
!
!   output arguments:
!     divspec  - real (2*mx) wave divergence field if idir<0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     vrtspec  - real (2*mx) wave vorticity field if idir>0
!                where mx=(maxwv+1)*(maxwv+2)/2
!     ugrid    - real (imax*jmax) grid u-wind (e->w,n->s) if idir>0
!     vgrid    - real (imax*jmax) grid v-wind (e->w,n->s) if idir>0
!
!$$$
      integer, intent(in) :: idir
      real(r_kind), dimension(ijmax), intent(inout) :: ugrid,vgrid
      real(r_kind), dimension(nc), intent(inout)    :: vrtspec,divspec
      complex(r_double), allocatable, dimension(:)  :: vrtspec_tmp,divspec_tmp
      real(r_double), allocatable, dimension(:,:)   :: ugrid_tmp,vgrid_tmp
      integer i,j,nn,nm

      allocate(ugrid_tmp(imax,jmax))
      allocate(vgrid_tmp(imax,jmax))
      allocate(vrtspec_tmp(ncd2))
      allocate(divspec_tmp(ncd2))

      if (idir .eq. -1) then ! grid to spec

         nn = 0
         do j=1,jmax
         do i=1,imax
            nn = nn + 1
            ugrid_tmp(i,j) = ugrid(nn)
            vgrid_tmp(i,j) = vgrid(nn)
         enddo
         enddo
         call shtns_spat_to_sphtor(ugrid_tmp, vgrid_tmp, vrtspec_tmp, divspec_tmp)
         vrtspec_tmp = lap*vrtspec_tmp; divspec_tmp = lap*divspec_tmp
         nn = 1
         do nm=1,ncd2
            vrtspec(nn) = real(vrtspec_tmp(nm))
            vrtspec(nn+1) = imag(vrtspec_tmp(nm))
            divspec(nn) = real(divspec_tmp(nm))
            divspec(nn+1) = imag(divspec_tmp(nm))
            nn = nn + 2
         enddo

      else if (idir .eq. 1) then ! spec to grid

         nn = 1
         do nm=1,ncd2
            vrtspec_tmp(nm) = cmplx(vrtspec(nn),vrtspec(nn+1))
            divspec_tmp(nm) = cmplx(divspec(nn),divspec(nn+1))
            nn = nn + 2
         enddo
         vrtspec_tmp = invlap*vrtspec_tmp
         divspec_tmp = invlap*divspec_tmp
         call shtns_sphtor_to_spat(vrtspec_tmp,divspec_tmp,ugrid_tmp,vgrid_tmp)
         nn = 0
         do j=1,jmax
         do i=1,imax
            nn = nn + 1
            ugrid(nn) = ugrid_tmp(i,j)
            vgrid(nn) = vgrid_tmp(i,j)
         enddo
         enddo

      else

         print *,'idir must be 1 or -1'
         call stop2(-99)

      endif

      deallocate(ugrid_tmp,vgrid_tmp,vrtspec_tmp,divspec_tmp)

      end subroutine sptezv_s

      end module specmod
