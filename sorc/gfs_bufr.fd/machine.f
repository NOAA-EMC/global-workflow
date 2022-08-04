      MODULE MACHINE

      IMPLICIT NONE
      SAVE
!  Machine dependant constants
      integer kind_io4,kind_io8,kind_phys,kind_rad
      parameter (kind_rad = selected_real_kind(13,60)) ! the '60' maps to 64-bit real
      parameter (kind_phys = selected_real_kind(13,60)) ! the '60' maps to 64-bit real
      parameter (kind_io4 = 4)
!      parameter (kind_io8 = 8)
      parameter (kind_io8 = 4)
      integer kint_mpi
      parameter (kint_mpi = 4)

      END MODULE MACHINE
