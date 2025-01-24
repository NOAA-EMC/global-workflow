! Adopted from RADREMUL library of ARPS system
! Used for direct reflectity DA capability
!
! !DESCRIPTION: define variables 
!               required for direct reflectivity DA capabilities
!
! !REVISION HISTORY:
!   2021-xx-xx  CAPS - init commit
!   2021-05-17  J. Park(CAPS) - radaremul renamed to radarz
!                             - deleted unnecessary subprograms
!
!########################################################################
!########################################################################
!#########                                                      #########
!#########                 module radarz_cst                    #########
!#########                                                      #########
!#########                     Developed by                     #########
!#########     Center for Analysis and Prediction of Storms     #########
!#########                University of Oklahoma                #########
!#########                                                      #########
!########################################################################
!########################################################################

MODULE radarz_cst

  use kinds, only: r_kind,i_kind

  IMPLICIT NONE
  SAVE
  PRIVATE

  PUBLIC :: mphyopt
  PUBLIC :: hail_ON, graupel_ON
  PUBLIC :: nscalar
  PUBLIC :: MFflg
  PUBLIC :: P_qc, P_qr, P_qi, P_qs, P_qh, P_qg
  PUBLIC :: P_nc, P_nr, P_ni, P_ns, P_nh, P_ng
  PUBLIC ::       P_zr, P_zi, P_zs, P_zh, P_zg

  PUBLIC :: n0rain, n0snow, n0hail, n0grpl
  PUBLIC :: rhosnow, rhohail, rhogrpl
  PUBLIC :: alpharain,alphasnow,alphagrpl,alphahail,alphaice
  PUBLIC :: alpha_dsd
  PUBLIC :: dsdparaopt
  PUBLIC :: nen
  PUBLIC :: rfopt
  PUBLIC :: wavelen


  INTEGER(i_kind) :: mphyopt
  INTEGER(i_kind) :: hail_ON, graupel_ON
  INTEGER(i_kind) :: nscalar = 1
  INTEGER(i_kind) :: MFflg = 3

  INTEGER(i_kind) :: P_qc, P_qr, P_qi, P_qs, P_qh, P_qg
  INTEGER(i_kind) :: P_nc, P_nr, P_ni, P_ns, P_nh, P_ng
  INTEGER(i_kind) ::       P_zr, P_zi, P_zs, P_zh, P_zg

  REAL(r_kind) :: n0rain, n0snow, n0hail, n0grpl
  REAL(r_kind) :: rhosnow, rhohail, rhogrpl
  REAL(r_kind) :: alpharain,alphasnow,alphagrpl,alphahail,alphaice
  REAL(r_kind) :: alpha_dsd(6)

  INTEGER(i_kind) :: dsdparaopt = 0

  INTEGER(i_kind) :: nen = 1

  INTEGER(i_kind) :: rfopt
  REAL(r_kind)    :: wavelen
END MODULE radarz_cst
