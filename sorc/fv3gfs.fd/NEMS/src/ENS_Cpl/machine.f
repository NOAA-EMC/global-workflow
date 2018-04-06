      MODULE MACHINE

      IMPLICIT NONE
      SAVE
!  Machine dependant constants
      integer, parameter :: kind_io4  = 4, kind_io8  = 8 , kind_ior = 8
     &,                     kind_evod = 8, kind_dbl_prec = 8
     &,                     kind_rad  = selected_real_kind(13,60) ! the '60' maps to 64-bit real
     &,                     kind_phys = selected_real_kind(13,60) ! the '60' maps to 64-bit real
!
      real(kind=kind_evod), parameter :: mprec = 1.e-12           ! machine precision to restrict dep

      END MODULE MACHINE
