module jp_pkind
!$$$  module documentation block
!                .      .    .                                       .
! module:   jp_pkind
!
! abstract:  Kinds for single- and double-precision
!
! module history log:
!
! Subroutines Included:
!
! Functions Included:
!
! remarks:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use mpi
integer,parameter:: spi=selected_int_kind(6),&
                    dpi=selected_int_kind(12),&
                    sp =selected_real_kind(6,30),&
                    dp =selected_real_kind(15,300),&
                    spc=sp,dpc=dp
!private:: one_dpi; integer(8),parameter:: one_dpi=1
!integer,parameter:: dpi=kind(one_dpi)
!integer,parameter:: sp=kind(1.0)
!integer,parameter:: dp=kind(1.0d0)
!integer,parameter:: spc=kind((1.0,1.0))
!integer,parameter:: dpc=kind((1.0d0,1.0d0))
end module jp_pkind
