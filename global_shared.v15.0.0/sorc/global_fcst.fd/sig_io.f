      module sig_io
      use resol_def
      implicit none
      save
      integer              icen
      integer              icen2
      integer              ienst
      integer              iensi
      integer              itrun
!     integer              igen
!
!     integer lonb, latb, iens(5), idpp, idvt, idrun
      integer lonb, latb, iens(5), idpp, idrun
     &,       idusr, ncldt, irealf, iorder
!!
!     real(kind=kind_evod) runid
!     real(kind=kind_evod) usrid
      real(kind=kind_ior), allocatable ::  z_r(:)
      real(kind=kind_io4), allocatable ::  z(:)
      real(kind=kind_io4), allocatable :: fmm(:,:)
      logical(1), allocatable ::  lbmm(:,:)
      real(kind=kind_io4), allocatable ::  rbufm(:,:)
      integer, allocatable ::  ibufm(:,:)

!!
      end module sig_io
