      module machine

      implicit none
      save
!  machine dependant constants
      integer, parameter :: kind_io4  = 4, kind_io8  = 8 , kind_ior = 8
     &,                     kind_evod = 8, kind_dbl_prec = 8
     &,                     kind_qdt_prec = selected_real_kind(30,90)
     &,                     kind_rad  = selected_real_kind(13,60) ! the '60' maps to 64-bit real
     &,                     kind_phys = selected_real_kind(13,60) ! the '60' maps to 64-bit real
     &,                     kind_real = 8                         ! used in cmp_comm
     &,                     kind_integer = 4                      ! -,,-
!
      real(kind=kind_evod), parameter :: mprec = 1.e-12           ! machine precision to restrict dep

      end module machine
