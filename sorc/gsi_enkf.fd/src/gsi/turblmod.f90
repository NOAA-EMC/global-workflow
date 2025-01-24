module turblmod
!$$$   module documentation block
!             .      .    .
! module:     turblmod
!  prgmmr:
!
! abstract:
!
! program history log:
!   2008-04-01  safford - added doc blocks
!
! subroutines included:
!   init_turbl          --
!   create_turblvars    --
!   destroy_turblvars   --
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig_hlf
  use constants, only: one
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_turbl
  public :: create_turblvars
  public :: destroy_turblvars
! set passed variables to public
  public :: use_pbl,f5my20,f4my20,f7my20,f6my20,f3my20, &
            d0my20,c0my20,f2my20,f1my20,f8my20,smcmy20, &
            shcmy20,ri_int,eps_m,rfcmy20,karmy20,b1my20, &
            ricmy20,l0my20,km,zi,sm,kh,rf,dvdz,dudz,ri, &
            dodz,sh,kar0my20,rdzl,b0my20,a0my20,rdzi, &
            dudtm,lmix,dtdtm,dvdtm,fsh_my20,fsm_my20

  logical use_pbl
  real(r_kind),allocatable,dimension(:,:,:):: dudz
  real(r_kind),allocatable,dimension(:,:,:):: dvdz
  real(r_kind),allocatable,dimension(:,:,:):: dodz
  real(r_kind),allocatable,dimension(:,:,:):: dudtm
  real(r_kind),allocatable,dimension(:,:,:):: dvdtm
  real(r_kind),allocatable,dimension(:,:,:):: dtdtm
  real(r_kind),allocatable,dimension(:,:,:):: rdzi
  real(r_kind),allocatable,dimension(:,:,:):: rdzl
  real(r_kind),allocatable,dimension(:,:,:):: ri
  real(r_kind),allocatable,dimension(:,:,:):: rf
  real(r_kind),allocatable,dimension(:,:,:):: zi
  real(r_kind),allocatable,dimension(:,:,:):: km
  real(r_kind),allocatable,dimension(:,:,:):: kh
  real(r_kind),allocatable,dimension(:,:,:):: sm
  real(r_kind),allocatable,dimension(:,:,:):: sh
  real(r_kind),allocatable,dimension(:,:,:):: lmix

  real(r_kind),allocatable,dimension(:,:):: kar0my20

  integer(i_kind),allocatable,dimension(:,:,:):: ri_int

! Constants used in MY20 PBL parameterization  

  real(r_kind) a0my20,b0my20,c0my20,d0my20,f1my20,f2my20, &
               f3my20,f4my20,f5my20,f6my20,f7my20,f8my20,b1my20, &
               karmy20,l0my20, &
               f85my20,f76my20
  real(r_kind) ricmy20,rfcmy20,shcmy20,smcmy20,eps_m
  real(r_kind) fsm_my20,fsh_my20

!>>> These are original coefficients from MY(1974)

  parameter( ricmy20= 0.2338021249_r_kind )
  parameter( a0my20= 0.7162162662_r_kind )
  parameter( b0my20= 0.1886792481_r_kind )
  parameter( c0my20= 0.3197414279_r_kind )
  parameter( d0my20= 0.03559985757_r_kind )
  parameter( f1my20= 2.339999914_r_kind )
  parameter( f2my20= 0.2293333411_r_kind )
  parameter( f3my20= 1.074666739_r_kind )
  parameter( f4my20= 1.000000000_r_kind )
  parameter( f5my20= 2.600000143_r_kind )
  parameter( f6my20= 9.619999886_r_kind )
  parameter( f7my20= 3.440000057_r_kind )
  parameter( f8my20= 13.78000069_r_kind )
  parameter( f85my20=f8my20*f5my20)
  parameter( f76my20=f7my20*f6my20)
  parameter( b1my20=15.0_r_kind )
 
!>>> These coefficients recalculated from Eta model based
!>>> Janjic (1990) implementation

!  parameter( ricmy20=0.5046048164_r_kind ) 
!  parameter( a0my20=0.6892600656_r_kind )
!  parameter( b0my20=0.2228188217_r_kind )
!  parameter( c0my20=0.2009073496_r_kind )
!  parameter( d0my20=0.04964822531_r_kind )
!  parameter( f1my20=1.972262979_r_kind )
!m  parameter( f1my20=.1972262979_r_kind )
!  parameter( f2my20=0.2222222388_r_kind )
!  parameter( f3my20=1.163989186_r_kind )
!  parameter( f4my20=1.003753304_r_kind )
!  parameter( f5my20=2.629684210_r_kind )
!  parameter( f6my20=8.561278343_r_kind )
!  parameter( f7my20=2.639554262_r_kind )
!  parameter( f8my20=11.84619045_r_kind )
!  parameter( b1my20=11.87799326209552761_r_kind )

  parameter( karmy20=0.4_r_kind )
  parameter( l0my20=80._r_kind )    
!m  parameter( eps_m   = 0.00000002_r_kind )
  parameter( eps_m   = 0.02_r_kind )

contains

  subroutine init_turbl
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_turbl
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-01  safford -- add subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$

    implicit none

    use_pbl=.false.                      ! set to true to turn on effect of pbl
    rfcmy20=a0my20*(ricmy20+b0my20-sqrt(ricmy20**2-c0my20*ricmy20+d0my20))
    shcmy20=f1my20*(f2my20-f3my20*rfcmy20)/(one-rfcmy20)
    smcmy20=f4my20*(f5my20-f6my20*rfcmy20)/(f7my20-f8my20*rfcmy20)*shcmy20
    fsh_my20=f1my20*(f2my20-f3my20)
    fsm_my20=f4my20*(f85my20-f76my20)
    return
  end subroutine init_turbl

  subroutine create_turblvars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    create_turblvars
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-01  safford -- add subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
    implicit none
    
    if(.not. use_pbl)return
    allocate(dudz (lat2,lon2,nsig_hlf) )
    allocate(dvdz (lat2,lon2,nsig_hlf) )
    allocate(dodz (lat2,lon2,nsig_hlf) )
    allocate(dudtm(lat2,lon2,nsig_hlf) )
    allocate(dvdtm(lat2,lon2,nsig_hlf) )
    allocate(dtdtm(lat2,lon2,nsig_hlf) )
    allocate(rdzi (lat2,lon2,nsig_hlf) )
    allocate(rdzl (lat2,lon2,nsig_hlf) )
    allocate(ri   (lat2,lon2,nsig_hlf) )
    allocate(rf   (lat2,lon2,nsig_hlf) )
    allocate(km   (lat2,lon2,nsig_hlf+1) )
    allocate(kh   (lat2,lon2,nsig_hlf+1) )
    allocate(zi   (lat2,lon2,nsig_hlf+1) )
    allocate(sm   (lat2,lon2,nsig_hlf) )
    allocate(sh   (lat2,lon2,nsig_hlf) )
    allocate(lmix (lat2,lon2,nsig_hlf) )
    allocate(ri_int(lat2,lon2,nsig_hlf) )
    allocate(kar0my20(lat2,lon2) )
    
    return
  end subroutine create_turblvars

  subroutine destroy_turblvars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    destroy_turblvars
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-01  safford -- add subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
    implicit none

    if(.not. use_pbl)return
    deallocate(dudz  )
    deallocate(dvdz  )
    deallocate(dodz  )
    deallocate(dudtm )
    deallocate(dvdtm )
    deallocate(dtdtm )
    deallocate(rdzi  )
    deallocate(rdzl  )
    deallocate(ri    )
    deallocate(rf    )
    deallocate(km    )
    deallocate(kh    )
    deallocate(zi    )
    deallocate(sm    )
    deallocate(sh    )
    deallocate(lmix  )
    deallocate(kar0my20 )
    deallocate(ri_int )

    return
  end subroutine destroy_turblvars

end module turblmod 
