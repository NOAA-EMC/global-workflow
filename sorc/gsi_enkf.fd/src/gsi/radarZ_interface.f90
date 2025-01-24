MODULE radarz_iface
!
! !DESCRIPTION: initialize variables required for radarz library
!               required for direct reflectivity DA capabilities
!
! !REVISION HISTORY:
!   2019-xx-xx  CAPS - init commit
!   2021-05-17  J. Park(CAPS) - radaremul renamed to radarz
!                             - deleted unnecessary mphyopt options
!
  use kinds, only: r_kind,r_single,r_double,i_kind

  IMPLICIT NONE
  SAVE
  PRIVATE

  PUBLIC :: init_mphyopt

CONTAINS
SUBROUTINE init_mphyopt(mype,iret)

  use radarz_cst, only: mphyopt
  use radarz_cst, only: hail_ON, graupel_ON
  use radarz_cst, only: nscalar
  use radarz_cst, only: P_qc, P_qr, P_qi, P_qs, P_qh, P_qg
  use radarz_cst, only: P_nc, P_nr, P_ni, P_ns, P_nh, P_ng
  use radarz_cst, only:       P_zr, P_zi, P_zs, P_zh, P_zg
  use radarz_cst, only: n0rain, n0snow, n0hail, n0grpl
  use radarz_cst, only: rhosnow, rhohail, rhogrpl
  use radarz_cst, only: alpharain, alphasnow, alphagrpl, alphahail, alphaice
  use radarz_cst, only: rfopt, wavelen
  use radarz_module, only: qgh_opt, grpl_ON, hl_ON, lambda, calcConstants, get_qgh_opt

  IMPLICIT NONE

  integer(i_kind), intent(in   ) :: mype
  integer(i_kind), intent(inout) :: iret

  IF (mype == 0) &
    WRITE(6,*) "INIT_MPHYOPT: Initializing radar emulator ...  (mphyopt=",mphyopt,")"

  P_qc = 0; P_qr = 0; P_qi = 0; P_qs = 0; P_qh = 0; P_qg = 0
  P_nc = 0; P_nr = 0; P_ni = 0; P_ns = 0; P_nh = 0; P_ng = 0
            P_zr = 0; P_zi = 0; P_zs = 0; P_zh = 0; P_zg = 0

  alpharain = 0.0_r_kind
  alphasnow = 0.0_r_kind
  alphagrpl = 0.0_r_kind
  alphahail = 0.0_r_kind
  alphaice = 0.0_r_kind

  IF ( mphyopt == 2 .OR. mphyopt == 3 .OR. mphyopt == 4 ) THEN
    nscalar = 5
    P_qc = 1; P_qr = 2; P_qi = 3; P_qs = 4; P_qh = 5

    graupel_ON = 0
    hail_ON = 1
  ELSE IF ( mphyopt == 5 .OR. mphyopt == 6 .OR. mphyopt == 7 ) THEN
    nscalar = 5
    P_qc = 1; P_qr = 2; P_qi = 3; P_qs = 4; P_qg = 5

    graupel_ON = 1
    hail_ON = 0
  ELSE IF (mphyopt == 102 .OR. mphyopt == 106) THEN  ! linscheme, wsm6scheme
    P_qc = 1; P_qr = 2; P_qi = 3; P_qs = 4; P_qg = 5
    nscalar  = 5

    graupel_ON = 1
    hail_ON = 0
  ELSE IF (mphyopt == 108) THEN                      ! thompson
    P_qc = 1; P_qr = 2; P_qi = 3; P_qs = 4; P_qg = 5
              P_nr = 6; P_ni = 7
    nscalar = 7

    graupel_ON = 1
    hail_ON = 0
  END IF

  grpl_ON = graupel_ON
  hl_ON = hail_ON
  qgh_opt = get_qgh_opt(graupel_ON, hail_ON)

! assign default values for radarz library for TM for EnKF
  IF (mphyopt == 108 ) then
    n0rain=8.0e6_r_kind
    n0snow=3.0e6_r_kind
    n0hail=4.0e4_r_kind
    n0grpl=4.0e5_r_kind
    rhosnow=100.0_r_kind
    rhohail=913.0_r_kind
    rhogrpl=500.0_r_kind
    rfopt=1
    wavelen=107.0_r_kind
  END IF

  lambda = wavelen
  call calcConstants()

  iret = -1
  SELECT CASE (mphyopt)
    CASE(2,3,4)             ! Lin
      iret = 0
    CASE(5,6,7)             ! WSM6
      iret = 0
    CASE(108)               ! Thompson
      iret = 0
    CASE DEFAULT            ! not ready for dbz operator
      iret = -1
  END SELECT



END SUBROUTINE init_mphyopt

END MODULE
