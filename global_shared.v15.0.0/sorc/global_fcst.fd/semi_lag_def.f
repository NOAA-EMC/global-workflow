      module semi_lag_def
!     use resol_def
      use machine
      implicit none
      save
      integer pmap                ! dimension of artificial array
      parameter(pmap=2000)
!
      integer
     &     kdpmpf(pmap), ! mapping from artificial array to model levels
     &     kdpmph(pmap)  ! mapping from artificial array to interfaces
!
!  lbasdz  weights for lagrange cubic derivative estimates on the
!          unequally spaced vertical grid (corresponding to model
!          full levels).
      real(kind=kind_evod) ,allocatable :: lbasdz(:,:,:)
!  lbasiz  weights for lagrange cubic interpolation on the
!          unequally spaced vertical grid (corresponding to model
!          full levels).
      real(kind=kind_evod) ,allocatable :: lbasiz(:,:,:)
! delta eta at interfaces
      real(kind=kind_evod) ,allocatable :: detai(:)
! delta eta at levels
      real(kind=kind_evod) ,allocatable :: detam(:)
! eta at levels
      real(kind=kind_evod) ,allocatable :: etamid(:)
! eta at interfaces
      real(kind=kind_evod) ,allocatable :: etaint(:)
!  sinlam  sine of longitudes in global grid (no extension points).
!  coslam  cosine of longitudes in global grid (no extension points).
      real(kind=kind_evod) ,allocatable :: sinlamg(:,:),coslamg(:,:)
      end module semi_lag_def

