module jp_pkind2
!$$$  module documentation block
!                .      .    .                                       .
! module:   jp_pkind2
!
! abstract:  Integer kinds for helf- and fourth-precision integers
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
integer,parameter:: hpi=selected_int_kind(3),&
                    fpi=selected_int_kind(2)
end module jp_pkind2
