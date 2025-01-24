!                                                   **************************
!                                                   *   MODULE pvqc_tables   *
!                                                   *   R. J. Purser         *
!                                                   *   NOAA/NCEP/EMC 2017   *
!                                                   **************************
!  Parameters and tables defining the three-shape-parameter variational
!  quality control (VQC) scheme of the super-logictic kind (NOAA/NCEP Office 
!  Note 468) together with corresponding analogs of the Huber-like type
!  (see Tavolato and Isaksen, 2015).
!
!  DIRECT DEPENDENCIES
!  Modules: kind
!
!=============================================================================
module pvqc_tables
!=============================================================================
! sgt: super-logistic standardized log-probability table (but unnormalized)
! swt: super-logistic standardized weight-factor table
! x1t,x2t: Tables of the negative and positive transition pts in the 
!          Huber-like analogs of the standardized probability models
! xat,yat: translation parameter tables for centering the Huber-like models
! npx: Resolution, points per unit x of tables sgt and swt
! npa: Resolution, points per unit alpha in all tables
! npb: Nominal resolution, points per unit beta, but independent of the tables
! npk: Resolution, points per unit kappa in all tables
! nx: size [-nx:nx] of centered table in x
! na: size [0:na] of one-sided table in alpha
! linitvqc: logical flag, true only when tables are initialized
!=============================================================================
use kinds, only: r_kind,dp,i_kind
implicit none
public
real(dp),allocatable,dimension(:,:,:):: sgt,swt
real(dp),allocatable,dimension(:,:)  :: x1t,x2t,xat,yat
real(dp)                             :: dx,da,db,dk
integer(i_kind)                              :: npx,npa,npb,npk,nx,na,npb2
logical                              :: linitvqc
data linitvqc/.false./
end module pvqc_tables

