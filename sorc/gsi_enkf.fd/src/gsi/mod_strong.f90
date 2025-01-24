module mod_strong
!$$$   module documentation block
!                .      .    .                                       .
! module:  mod_strong
! prgmmr:  parrish            org: np23               date: 2007-02-15
!
! abstract: high level module for carrying all parameters used for various
!           flavors of strong constraint.
!           Implement implicit normal mod initialization routines for
!           use with analysis increment and analysis increment tendencies.
!           reference: Temperton, C., 1989:  "Implicit Normal Mode Initialization
!                           for Spectral Models", .  MWR, 117, 436-451.
!
! program history log:
!   2007-02-15 parrish
!   2012-02-08 kleist - add option tlnmc_option to control how TLNMC is applied
!   2013-07-02 parrish - change tlnmc_type to reg_tlnmc_type.  tlnmc_type no
!                          longer used for global application of tlnmc.
!   2014-12-03  derber  - remove unused variables
!
! Subroutines Included:
!   sub init_strongvars  - set default namelist variable values
!   sub gproj            - project input u,v,mass variable to gravity modes for
!   update
!   sub gproj_diag       - project input u,v,mass variable to gravity modes plus
!   diagnostics
!   sub gproj_diag_update- project input u,v,mass variable to gravity modes plus
!   diagnostic and update
!   sub gproj0           -
!   sub gproj_ad         -
!   sub dinmi            - obtain balance increment from input tendencies
!   sub dinmi_ad         - adjoint of dinmi
!   sub dinmi0           - lower level--balance increment from input tendencies
!   sub balm_1           - compute balance diagnostic variable
!   sub getbcf           - compute matrices B,C,F as defined in above reference
!   sub scale_vars       - scale variables as defined in reference
!   sub scale_vars_ad    - adjoint of scale variables
!   sub unscale_vars     - unscale variables
!   sub unscale_vars_ad  - adjoint of unscale variables
!   sub f_mult           - multiply by F matrix
!   sub c_mult           - multiply by C matrix
!   sub i_mult           - multiply by sqrt(-1)
!   sub solve_f2c2       - solve (F*F+C*C)*x = y
!
! Variable Definitions:
!   def l_tlnmc          - Logical for TLNMC (set to true if namelist option tlnmc_option
!                          is 1, 2, or 3
!   def reg_tlnmc_type   - =1 for regional 1st version of strong constraint
!                          =2 for regional 2nd version of strong constraint
!   def nstrong          - number of iterations of strong constraint initialization
!   def scheme           - which scheme (B, C or D) is being used (see reference above)
!   def period_max       - max period (hours) of gravity modes to be balanced
!   def period_width     - width of smooth transition (hours, centered on period_max)
!                          from balanced to unbalanced gravity modes
!   def baldiag_full     - flag to toggle balance diagnostics for the full fields
!   def baldiag_inc      - flag to toggle balance diagnostics for the analysis increment
!   def tlnmc_option - Integer option for Incremental Normal Mode Constraint (inmc) / TLNMC
!                            when in hybrid ensemble mode:
!                          =0: no constraint at all
!                          =1: TLNMC on static contribution to increment (or if non-hybrid)
!                          =2: TLNMC on total increment (single time level only, or 3D mode)
!                          =3: TLNMC on total increment over all nobs_bins (if 4D mode)
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

use kinds,only: r_kind,i_kind
use constants, only: zero,half,one,two,four,r3600,omega,pi,rearth,one_tenth
implicit none

! set default to private
  private
! set subroutines to public
  public :: init_strongvars
  public :: gproj
  public :: gproj_diag
  public :: gproj_diag_update
  public :: gproj0
  public :: gproj_ad
  public :: dinmi
  public :: dinmi_ad
  public :: dinmi0
  public :: balm_1
  public :: getbcf
  public :: scale_vars
  public :: scale_vars_ad
  public :: unscale_vars
  public :: unscale_vars_ad
  public :: f_mult
  public :: c_mult
  public :: i_mult
  public :: solve_f2c2

! set passed variables to public
  public :: nstrong,baldiag_full,l_tlnmc,baldiag_inc,period_width,period_max,scheme
  public :: reg_tlnmc_type
  public :: tlnmc_option

  integer(i_kind) nstrong
  integer(i_kind) reg_tlnmc_type,tlnmc_option
  real(r_kind) period_max,period_width
  logical l_tlnmc,baldiag_full,baldiag_inc
  character(1) scheme

contains


  subroutine init_strongvars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_strongvars
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-05  safford -- add subprogram doc block
!   2013-07-02 parrish - change tlnmc_type to reg_tlnmc_type.  Set default for reg_tlnmc_type = 1.
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    implicit none

    l_tlnmc=.false.
    reg_tlnmc_type=1
    tlnmc_option=0
    nstrong=0
    period_max=1000000._r_kind
    period_width=one_tenth
    scheme='B'
    baldiag_full=.false.
    baldiag_inc =.false.

  end subroutine init_strongvars
          

  subroutine gproj_diag_update(vort,div,phi,vort_g,div_g,phi_g,rmstend,rmstend_g,rmstend_f,rmstend_fg, &
         m,mmax,gspeed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gproj
!
!   prgrmmr:
!
! abstract:    
!         for gravity wave projection:    vort, div, phi --> vort_g, div_g, phi_g
!
!         scale:      vort,div,phi --> vort_hat,div_hat,phi_hat
!
!         solve:     (F*F+C*C)*x = F*vort_hat + C*phi_hat
!         then:
!               phi_hat_g = C*x
!              vort_hat_g = F*x
!               div_hat_g = div_hat
!
!         unscale:    vort_hat_g, div_hat_g, phi_hat_g --> vort_g, div_g, phi_g
!
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     vort,div,phi
!     rmstend,rmstend_g
!     filtered
!
!   output argument list:
!     vort_g,div_g,phi_g
!     rmstend,rmstend_g
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(2,m:mmax):: vort,div,phi
    real(r_kind),intent(  out),dimension(2,m:mmax):: vort_g,div_g,phi_g
    real(r_kind),intent(in   ) :: gspeed
    real(r_kind),intent(inout) :: rmstend,rmstend_g,rmstend_f,rmstend_fg
    integer(i_kind),intent(in) :: m,mmax

    real(r_kind),dimension(2,m:mmax):: vort_hat,div_hat,phi_hat,vort_hat_g,phi_hat_g
    real(r_kind),dimension(m:mmax):: b,c,f,c2,c3

    call getbcf(b,c,f,c2,c3,m,mmax,gspeed)
    call scale_vars(vort,div,phi,vort_hat,div_hat,phi_hat,.false.,c2,c3,m,mmax,gspeed)
    call balm_1(vort_hat,div_hat,phi_hat,rmstend,m,mmax)

    call gproj0(vort_hat,phi_hat,vort_hat_g,phi_hat_g,c,f,m,mmax)
    call balm_1(vort_hat_g,div_hat,phi_hat_g,rmstend_g,m,mmax)

    call scale_vars(vort,div,phi,vort_hat,div_hat,phi_hat,.true.,c2,c3,m,mmax,gspeed)
    call balm_1(vort_hat,div_hat,phi_hat,rmstend_f,m,mmax)

    call gproj0(vort_hat,phi_hat,vort_hat_g,phi_hat_g,c,f,m,mmax)
    call balm_1(vort_hat_g,div_hat,phi_hat_g,rmstend_fg,m,mmax)

    call unscale_vars(vort_hat_g,div_hat,phi_hat_g,vort_g,div_g,phi_g,c2,c3,m,mmax)

  end subroutine gproj_diag_update
  subroutine gproj_diag(vort,div,phi,rmstend,rmstend_g,rmstend_f,rmstend_fg,&
         m,mmax,gspeed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gproj
!
!   prgrmmr:
!
! abstract:    
!         for gravity wave projection:    vort, div, phi --> vort_g, div_g, phi_g
!
!         scale:      vort,div,phi --> vort_hat,div_hat,phi_hat
!
!         solve:     (F*F+C*C)*x = F*vort_hat + C*phi_hat
!         then:
!               phi_hat_g = C*x
!              vort_hat_g = F*x
!               div_hat_g = div_hat
!
!         unscale:    vort_hat_g, div_hat_g, phi_hat_g --> vort_g, div_g, phi_g
!
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     vort,div,phi
!     rmstend,rmstend_g
!     filtered
!
!   output argument list:
!     vort_g,div_g,phi_g
!     rmstend,rmstend_g
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(2,m:mmax):: vort,div,phi
    real(r_kind),intent(in   ) :: gspeed
    real(r_kind),intent(inout) :: rmstend,rmstend_g,rmstend_f,rmstend_fg
    integer(i_kind),intent(in) :: m,mmax

    real(r_kind),dimension(2,m:mmax):: vort_hat,div_hat,phi_hat,vort_hat_g,phi_hat_g
    real(r_kind),dimension(m:mmax):: b,c,f,c2,c3

    call getbcf(b,c,f,c2,c3,m,mmax,gspeed)
    call scale_vars(vort,div,phi,vort_hat,div_hat,phi_hat,.false.,c2,c3,m,mmax,gspeed)
    call balm_1(vort_hat,div_hat,phi_hat,rmstend,m,mmax)

    call gproj0(vort_hat,phi_hat,vort_hat_g,phi_hat_g,c,f,m,mmax)
    call balm_1(vort_hat_g,div_hat,phi_hat_g,rmstend_g,m,mmax)

    call scale_vars(vort,div,phi,vort_hat,div_hat,phi_hat,.true.,c2,c3,m,mmax,gspeed)
    call balm_1(vort_hat,div_hat,phi_hat,rmstend_f,m,mmax)

    call gproj0(vort_hat,phi_hat,vort_hat_g,phi_hat_g,c,f,m,mmax)
    call balm_1(vort_hat_g,div_hat,phi_hat_g,rmstend_fg,m,mmax)

  end subroutine gproj_diag
  subroutine gproj(vort,div,phi,vort_g,div_g,phi_g,m,mmax,gspeed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gproj
!
!   prgrmmr:
!
! abstract:    
!         for gravity wave projection:    vort, div, phi --> vort_g, div_g, phi_g
!
!         scale:      vort,div,phi --> vort_hat,div_hat,phi_hat
!
!         solve:     (F*F+C*C)*x = F*vort_hat + C*phi_hat
!         then:
!               phi_hat_g = C*x
!              vort_hat_g = F*x
!               div_hat_g = div_hat
!
!         unscale:    vort_hat_g, div_hat_g, phi_hat_g --> vort_g, div_g, phi_g
!
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     vort,div,phi
!     rmstend,rmstend_g
!     filtered
!
!   output argument list:
!     vort_g,div_g,phi_g
!     rmstend,rmstend_g
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(2,m:mmax):: vort,div,phi
    real(r_kind),intent(  out),dimension(2,m:mmax):: vort_g,div_g,phi_g
    real(r_kind),intent(in   ) :: gspeed
    integer(i_kind),intent(in) :: m,mmax

    real(r_kind),dimension(2,m:mmax):: vort_hat,div_hat,phi_hat,vort_hat_g,phi_hat_g
    real(r_kind),dimension(m:mmax):: b,c,f,c2,c3

    call getbcf(b,c,f,c2,c3,m,mmax,gspeed)
    call scale_vars(vort,div,phi,vort_hat,div_hat,phi_hat,.true.,c2,c3,m,mmax,gspeed)

    call gproj0(vort_hat,phi_hat,vort_hat_g,phi_hat_g,c,f,m,mmax)

    call unscale_vars(vort_hat_g,div_hat,phi_hat_g,vort_g,div_g,phi_g,c2,c3,m,mmax)

  end subroutine gproj


  subroutine gproj0(vort_hat,phi_hat,vort_hat_g,phi_hat_g,c,f,m,mmax)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gproj0
!
!   prgrmmr:
!
! abstract:
!  for gravity wave projection: vort,div,phi --> vort_g,div_g,phi_g
!  -----------------------------------------------------------------------------
!
!    solve:  (F*F+C*C)*x = F*vort_hat + C*phi_hat
!
!    then:
!          phi_hat_g  = C*x
!         vort_hat_g  = F*x
!          div_hat_g  = div_hat
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!     vort_hat,div_hat,phi_hat
!
!   output argument list:
!     vort_hat_g,div_hat_g,phi_hat_g
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),dimension(2,m:mmax),intent(in   ) :: vort_hat,phi_hat
    real(r_kind),dimension(  m:mmax),intent(in   ) :: c,f
    real(r_kind),dimension(2,m:mmax),intent(  out) :: vort_hat_g,phi_hat_g
    integer(i_kind),intent(in) :: m,mmax

    real(r_kind),dimension(2,m:mmax):: x,y


    call f_mult(y,vort_hat,f,m,mmax)
    call c_mult(x,phi_hat,c,m,mmax)
    y=y+x
    call solve_f2c2(x,y,f,c,m,mmax)
    call c_mult(phi_hat_g,x,c,m,mmax)
    call f_mult(vort_hat_g,x,f,m,mmax)

  end subroutine gproj0


  subroutine gproj_ad(vort,div,phi,vort_g,div_g,phi_g,m,mmax,gspeed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gproj_ad
!
!   prgrmmr:
!
! abstract:
!    for gravity wave projection: vort,div,phi --> vort_g,div_g,phi_g
!
!    scale:      vort,div,phi --> vort_hat,div_hat,phi_hat
!
!    solve:  (F*F+C*C)*x = F*vort_hat + C*phi_hat
!
!    then:
!               phi_hat_g = C*x
!              vort_hat_g = F*x
!               div_hat_g = div_hat
!
!      unscale:    vort_hat_g, div_hat_g, phi_hat_g --> vort_g, div_g, phi_g
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!     vort,div,phi
!     vort_g,div_g,phi_g
!
!   output argument list:
!     vort,div,phi
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(inout),dimension(2,m:mmax) :: vort,div,phi
    real(r_kind),intent(in   ),dimension(2,m:mmax) :: vort_g,div_g,phi_g
    real(r_kind),intent(in   )                     :: gspeed
    integer(i_kind),intent(in) :: m,mmax

    real(r_kind),dimension(2,m:mmax)::vort_hat,div_hat,phi_hat,vort_hat_g,phi_hat_g
    real(r_kind),dimension(m:mmax):: b,c,f,c2,c3

    call getbcf(b,c,f,c2,c3,m,mmax,gspeed)
    call unscale_vars_ad(vort_hat_g,div_hat,phi_hat_g,vort_g,div_g,phi_g,c2,c3,m,mmax)
    call gproj0(vort_hat_g,phi_hat_g,vort_hat,phi_hat,c,f,m,mmax)
    call scale_vars_ad(vort,div,phi,vort_hat,div_hat,phi_hat,c2,c3,m,mmax,gspeed)

  end subroutine gproj_ad


  subroutine dinmi(vort_t,div_t,phi_t,del_vort,del_div,del_phi,m,mmax,gspeed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dinmi
!
!   prgrmmr:
!
! abstract:
!      for implicit nmi correction: vort_t,div_t,phi_t --> del_vort,del_div,del_phi
!
!      scale:      vort_t,div_t,phi_t --> vort_t_hat,div_t_hat,phi_t_hat
!
!      solve:  (F*F+C*C)*del_div_hat = sqrt(-1)*(F*vort_t_hat + C*phi_t_hat)
!
!      solve:  (F*F+C*C)*x = sqrt(-1)*div_t_hat - B*del_div_hat
!
!      then:
!            del_phi_hat  = C*x
!            del_vort_hat = F*x
!
!      unscale: del_vort_hat,del_div_hat,del_phi_hat --> del_vort,del_div,del_phi
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!     vort_t,div_t,phi_t
!
!   output argument list:
!     del_vort,del_div,del_phi
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(2,m:mmax):: vort_t,div_t,phi_t
    real(r_kind),intent(  out),dimension(2,m:mmax):: del_vort,del_div,del_phi
    real(r_kind),intent(in   ):: gspeed
    integer(i_kind),intent(in) :: m,mmax

    real(r_kind),dimension(2,m:mmax):: vort_t_hat,div_t_hat,phi_t_hat
    real(r_kind),dimension(2,m:mmax):: del_vort_hat,del_div_hat,del_phi_hat
    real(r_kind),dimension(m:mmax):: b,c,f,c2,c3

    call getbcf(b,c,f,c2,c3,m,mmax,gspeed)
    call scale_vars(vort_t,div_t,phi_t,vort_t_hat,div_t_hat,phi_t_hat,.true.,c2,c3,m,mmax,gspeed)
    call dinmi0(vort_t_hat,div_t_hat,phi_t_hat,del_vort_hat,del_div_hat,del_phi_hat,b,c,f,m,mmax)

    call unscale_vars(del_vort_hat,del_div_hat,del_phi_hat,del_vort,del_div,del_phi,c2,c3,m,mmax)

  end subroutine dinmi


  subroutine dinmi_ad(vort_t,div_t,phi_t,del_vort,del_div,del_phi,m,mmax,gspeed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dinmi_ad
!
!   prgrmmr:
!
! abstract:
!       for implicit nmi correction: vort_t,div_t,phi_t --> del_vort,del_div,del_phi
!
!       scale:      vort_t,div_t,phi_t --> vort_t_hat,div_t_hat,phi_t_hat
!
!       solve:  (F*F+C*C)*del_div_hat = sqrt(-1)*(F*vort_t_hat + C*phi_t_hat)
!
!       solve:  (F*F+C*C)*x = sqrt(-1)*div_t_hat - B*del_div_hat
!
!       then:
!          del_phi_hat  = C*x
!          del_vort_hat = F*x
!
!       unscale: del_vort_hat,del_div_hat,del_phi_hat --> del_vort,del_div,del_phi
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!    del_vort,del_div,del_phi
!    vort_t,div_t,phi_t
!
!   output argument list:
!    vort_t,div_t,phi_t
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block
    implicit none

    real(r_kind),intent(inout),dimension(2,m:mmax):: vort_t,div_t,phi_t
    real(r_kind),intent(in   ),dimension(2,m:mmax):: del_vort,del_div,del_phi
    real(r_kind),intent(in   ):: gspeed
    integer(i_kind),intent(in) :: m,mmax

    real(r_kind),dimension(2,m:mmax):: vort_t_hat,div_t_hat,phi_t_hat
    real(r_kind),dimension(2,m:mmax):: del_vort_hat,del_div_hat,del_phi_hat
    real(r_kind),dimension(m:mmax):: b,c,f,c2,c3
    integer(i_kind) n

    call getbcf(b,c,f,c2,c3,m,mmax,gspeed)
    call unscale_vars_ad(del_vort_hat,del_div_hat,del_phi_hat,del_vort,del_div,del_phi,c2,c3,m,mmax)
    call dinmi0(del_vort_hat,del_div_hat,del_phi_hat,vort_t_hat,div_t_hat,phi_t_hat,b,c,f,m,mmax)
    do n=m,mmax
       vort_t_hat(1,n)=-vort_t_hat(1,n)
       vort_t_hat(2,n)=-vort_t_hat(2,n)
       div_t_hat(1,n)=-div_t_hat(1,n)
       div_t_hat(2,n)=-div_t_hat(2,n)
       phi_t_hat(1,n)=-phi_t_hat(1,n)
       phi_t_hat(2,n)=-phi_t_hat(2,n)
    end do
    call scale_vars_ad(vort_t,div_t,phi_t,vort_t_hat,div_t_hat,phi_t_hat,c2,c3,m,mmax,gspeed)

  end subroutine dinmi_ad


  subroutine dinmi0(vort_t_hat,div_t_hat,phi_t_hat,del_vort_hat,del_div_hat,del_phi_hat,b,c,f,&
         m,mmax)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dinmi0
!
!   prgrmmr:
!
! abstract:
!      for implicit nmi correction: vort_t,div_t,phi_t --> del_vort,del_div,del_phi
!
!      solve:  (F*F+C*C)*del_div_hat = sqrt(-1)*(F*vort_t_hat + C*phi_t_hat)
!
!      solve:  (F*F+C*C)*x = sqrt(-1)*div_t_hat - B*del_div_hat
!
!      then:
!          del_phi_hat  = C*x
!          del_vort_hat = F*x
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!    vort_t_hat,div_t_hat,phi_t_hat
!
!   output argument list:
!    del_vort_hat,del_div_hat,del_phi_hat
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),dimension(2,m:mmax),intent(in   ) :: vort_t_hat,div_t_hat,phi_t_hat
    real(r_kind),dimension(  m:mmax),intent(in   ) :: b,c,f
    real(r_kind),dimension(2,m:mmax),intent(  out) :: del_vort_hat,del_div_hat,del_phi_hat
    integer(i_kind),intent(in)::m,mmax

    real(r_kind),dimension(2,m:mmax):: x,y


    call f_mult(y,vort_t_hat,f,m,mmax)
    call c_mult(x,phi_t_hat,c,m,mmax)
    x=y+x
    call i_mult(y,x,m,mmax)
    call solve_f2c2(del_div_hat,y,f,c,m,mmax)
    call c_mult(x,del_div_hat,b,m,mmax)       !  actually multiplying by b
    call i_mult(y,div_t_hat,m,mmax)
    y=y-x
    call solve_f2c2(x,y,f,c,m,mmax)
    call c_mult(del_phi_hat,x,c,m,mmax)
    call f_mult(del_vort_hat,x,f,m,mmax)

  end subroutine dinmi0


  subroutine balm_1(vort_t_hat,div_t_hat,phi_t_hat,balnm1,m,mmax)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gproj
!
!   prgrmmr:
!
! abstract:      obtain balance diagnostic for each wave number n,m using 
!                method 1 (eq 4.23 of Temperton,1989)
!
!                balnm1 = abs(vort_t_hat)(n,m)**2 + abs(div_t_hat)(n,m)**2 + 
!                         abs(phi_t_hat)(n,m)**2
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!   2011-07-01  todling -- balm1 must not be zeroed out
!               
!   input argument list:
!    vort_t_hat,div_t_hat,phi_t_hat
!
!   output argument list:
!    balnm1
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(2,m:mmax) :: vort_t_hat,div_t_hat,phi_t_hat
    real(r_kind),intent(inout) :: balnm1
    integer(i_kind),intent(in)::m,mmax

    integer(i_kind) n

    do n=m,mmax
       balnm1=balnm1+vort_t_hat(1,n)**2+vort_t_hat(2,n)**2 &
                     +div_t_hat(1,n)**2+ div_t_hat(2,n)**2 &
                     +phi_t_hat(1,n)**2+ phi_t_hat(2,n)**2
    end do

  end subroutine balm_1


  subroutine getbcf(b,c,f,c2,c3,m,mmax,gspeed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    getbcf
!
!   prgrmmr:
!
! abstract:      compute operators needed to do gravity wave projection
!                and implicit normal mode initialization in spectral space
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!
!   output argument list:
!     b,c,f,c2,c3
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(  out),dimension(m:mmax) :: b,c,f,c2,c3
    real(r_kind),intent(in   ) ::gspeed
    integer(i_kind),intent(in)::m,mmax

    integer(i_kind) n,nstart
    real(r_kind) eps,rn,rm,rn1

!   scheme B:   b = 2*omega*m/(n*(n+1))
!               f = 2*omega*sqrt(n*n-1)*eps/n
!               c = gspeed*sqrt(n*(n+1))/erad

!   scheme C:   b = 0
!               f = 2*omega*sqrt(n*n-1)*eps/n
!               c = gspeed*sqrt(n*(n+1))/erad

!   scheme D:   b = 0
!               f = 2*omega*eps
!               c = gspeed*sqrt(n*(n+1))/erad

!     in the above, eps = sqrt((n*n-m*m)/(4*n*n-1))

    nstart=max(m,1)
    rm=m
    do n=nstart,mmax
       rn=n
       eps=sqrt((rn*rn-rm*rm )/(four*rn*rn-one))
       rn1=sqrt(rn*(rn+one))
       if(scheme=='B') then
          b(n)=two*omega*rm/(rn*(rn+one))
          f(n)=two*omega*sqrt(rn*rn-one)*eps/rn
          c(n)=gspeed*rn1/rearth
          c2(n)=rearth/rn1
          c3(n)=one/gspeed
       else if(scheme=='C') then
          b(n)=zero
          f(n)=two*omega*sqrt(rn*rn-one)*eps/rn
          c(n)=gspeed*rn1/rearth
          c2(n)=rearth/rn1
          c3(n)=one/gspeed
       else if(scheme=='D') then
          b(n)=zero
          f(n)=two*omega*eps
          c(n)=gspeed*rn1/rearth
          c2(n)=rearth
          c3(n)=rn1/gspeed
       else
          write(6,*)' scheme = ',scheme,' incorrect, must be = B, C, or D'
       end if
    end do

  end subroutine getbcf


  subroutine scale_vars(vort,div,phi,vort_hat,div_hat,phi_hat,filtered,c2,c3,&
             m,mmax,gspeed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    scale_vars
!
!   prgrmmr:
!
! abstract:
!        input scaling:
!
!           for schemes B, C:
!               vort_hat(n) = erad*vort(n)/sqrt(n*(n+1))
!               div_hat(n)  = sqrt(-1)*erad*div(n)/sqrt(n*(n+1))
!               phi_hat(n)  = phi(n)/gspeed
!
!           for scheme D:
!               vort_hat(n) = erad*vort(n)
!               div_hat(n)  = sqrt(-1)*erad*div(n)
!               phi_hat(n)  = phi(n)*sqrt(n*(n+1))/gspeed
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!     vort,div,phi
!
!   output argument list:
!     vort_hat,div_hat,phi_hat
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(2,m:mmax):: vort,div,phi
    real(r_kind),intent(in   ),dimension(m:mmax):: c2,c3
    real(r_kind),intent(in   )                  :: gspeed
    real(r_kind),intent(  out),dimension(2,m:mmax):: vort_hat,div_hat,phi_hat
    integer(i_kind),intent(in)::m,mmax
    logical,intent(in)::filtered

    real(r_kind) pmask,c1,pmax
    integer(i_kind) n,nstart

!   following is to account for 0,0 term being zero

    vort_hat(1,m)=zero
    vort_hat(2,m)=zero
    div_hat(1,m)=zero
    div_hat(2,m)=zero
    phi_hat(1,m)=zero
    phi_hat(2,m)=zero
    nstart=max(m,1)

    c1=two*pi*rearth/(gspeed*r3600*period_width)
    pmax=period_max/period_width
    do n=nstart,mmax
       pmask=one
       if(filtered)pmask=half*(one-tanh(c1/real(n,r_kind)-pmax))
       vort_hat(1,n)=pmask*c2(n)*vort(1,n)
       vort_hat(2,n)=pmask*c2(n)*vort(2,n)
       div_hat(2,n) =pmask*c2(n)*div (1,n)
       div_hat(1,n) =-pmask*c2(n)*div(2,n)
       phi_hat(1,n) =pmask*c3(n)*phi(1,n)
       phi_hat(2,n) =pmask*c3(n)*phi(2,n)
    end do

  end subroutine scale_vars


  subroutine scale_vars_ad(vort,div,phi,vort_hat,div_hat,phi_hat,c2,c3,&
               m,mmax,gspeed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    scale_vars_ad
!
!   prgrmmr:
!
! abstract:
!        input scaling:
!               for schemes B, C:
!                 vort_hat(n) = erad*vort(n)/sqrt(n*(n+1))
!                 div_hat(n)  = sqrt(-1)*erad*div(n)/sqrt(n*(n+1))
!                 phi_hat(n)  = phi(n)/gspeed
!
!              for scheme D:
!                 vort_hat(n) = erad*vort(n)
!                 div_hat(n)  = sqrt(-1)*erad*div(n)
!                 phi_hat(n)  = phi(n)*sqrt(n*(n+1))/gspeed
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!     vort,div,phi
!     vort_hat,div_hat,phi_hat
!
!   output argument list:
!     vort,div,phi
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(inout),dimension(2,m:mmax):: vort,div,phi
    real(r_kind),intent(in   ),dimension(m:mmax):: c2,c3
    real(r_kind),intent(in   )                  :: gspeed
    real(r_kind),intent(in   ),dimension(2,m:mmax):: vort_hat,div_hat,phi_hat
    integer(i_kind),intent(in)::m,mmax

    integer(i_kind) n,nstart
    real(r_kind) pmask,pmax,c1

    nstart=max(m,1)
    c1=two*pi*rearth/(gspeed*r3600*period_width)
    pmax=period_max/period_width
    do n=nstart,mmax
       pmask=half*(one-tanh(c1/real(n,r_kind)-pmax))
       vort(1,n)=vort(1,n)+pmask*c2(n)*vort_hat(1,n)
       vort(2,n)=vort(2,n)+pmask*c2(n)*vort_hat(2,n)
       div(1,n) =div(1,n) +pmask*c2(n)*div_hat (2,n)
       div(2,n) =div(2,n) -pmask*c2(n)*div_hat (1,n)
       phi(1,n) =phi(1,n) +pmask*c3(n)*phi_hat (1,n)
       phi(2,n) =phi(2,n) +pmask*c3(n)*phi_hat (2,n)
    end do

  end subroutine scale_vars_ad


  subroutine unscale_vars(vort_hat,div_hat,phi_hat,vort,div,phi,c2,c3,m,mmax)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    unscale_vars
!
!   prgrmmr:
!
! abstract:
!      output scaling:
!            for schemes B, C:
!                vort(n,m) = sqrt(n*(n+1))*vort_hat(n,m)/erad
!                div(n,m)  = -sqrt(-1)*sqrt(n*(n+1))*div_hat(n,m)/erad
!                phi(n,m)  = gspeed*phi_hat(n,m)
!            for scheme C:
!                vort(n,m) = vort_hat(n,m)/erad
!                div(n,m)  = -sqrt(-1)*div_hat(n,m)/erad
!                phi(n,m)  = gspeed*phi_hat(n,m)/sqrt(n*(n+1))
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!    vort_hat,div_hat,phi_hat
!
!   output argument list:
!    vort,div,phi
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(2,m:mmax):: vort_hat,div_hat,phi_hat
    real(r_kind),intent(in   ),dimension(m:mmax):: c2,c3
    real(r_kind),intent(  out),dimension(2,m:mmax):: vort,div,phi
    integer(i_kind),intent(in)::m,mmax

    integer(i_kind) n,nstart

!   following is to account for 0,0 term being zero

    vort(1,m)=zero
    vort(2,m)=zero
    div(1,m)=zero
    div(2,m)=zero
    phi(1,m)=zero
    phi(2,m)=zero
    nstart=max(m,1)
    do n=nstart,mmax
       vort(1,n)=vort_hat(1,n)/c2(n)
       vort(2,n)=vort_hat(2,n)/c2(n)
       div (1,n)=div_hat (2,n)/c2(n)
       div (2,n)=-div_hat(1,n)/c2(n)
       phi (1,n)=phi_hat (1,n)/c3(n)
       phi (2,n)=phi_hat (2,n)/c3(n)
    end do

  end subroutine unscale_vars


  subroutine unscale_vars_ad(vort_hat,div_hat,phi_hat,vort,div,phi,c2,c3,m,mmax)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    unscale_vars_ad
!
!   prgrmmr:
!
! abstract:
!      output scaling:
!          for schemes B, C:
!              vort(n,m) = sqrt(n*(n+1))*vort_hat(n,m)/erad
!              div(n,m)  = -sqrt(-1)*sqrt(n*(n+1))*div_hat(n,m)/erad
!              phi(n,m)  = gspeed*phi_hat(n,m)
!          for scheme C:
!              vort(n,m) = vort_hat(n,m)/erad
!              div(n,m)  = -sqrt(-1)*div_hat(n,m)/erad
!              phi(n,m)  = gspeed*phi_hat(n,m)/sqrt(n*(n+1))
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!    vort,div,phi
!    vort_hat,div_hat,phi_hat
!
!   output argument list:
!    vort_hat,div_hat,phi_hat
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(inout),dimension(2,m:mmax):: vort_hat,div_hat,phi_hat
    real(r_kind),intent(in   ),dimension(2,m:mmax):: vort,div,phi
    real(r_kind),intent(in   ),dimension(m:mmax):: c2,c3
    integer(i_kind),intent(in)::m,mmax

    integer(i_kind) n,nstart

!   following is to account for 0,0 term being zero

    vort_hat(1,m)=zero
    vort_hat(2,m)=zero
    div_hat(1,m)=zero
    div_hat(2,m)=zero
    phi_hat(1,m)=zero
    phi_hat(2,m)=zero
    nstart=max(m,1)
    do n=nstart,mmax
       vort_hat(1,n)=vort(1,n)/c2(n)
       vort_hat(2,n)=vort(2,n)/c2(n)
       div_hat (1,n)=-div (2,n)/c2(n)
       div_hat (2,n)=div (1,n)/c2(n)
       phi_hat (1,n)=phi (1,n)/c3(n)
       phi_hat (2,n)=phi (2,n)/c3(n)
    end do

  end subroutine unscale_vars_ad


  subroutine f_mult(x,y,f,m,mmax)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    f_mult
!
!   prgrmmr:
!
! abstract:      x = F*y
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!     y,f
!
!   output argument list:
!     x
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(m:mmax):: f
    real(r_kind),intent(in   ),dimension(2,m:mmax):: y
    real(r_kind),intent(  out),dimension(2,m:mmax):: x
    integer(i_kind),intent(in)::m,mmax

    integer(i_kind) n,nstart

    if(m==mmax) then
       x=zero
       return
    end if

!   following is to account for 0,0 term being zero

    x(1,m)=zero
    x(2,m)=zero
    nstart=max(m,1)

    x(1,mmax)=zero
    x(2,mmax)=zero
    if(nstart<mmax) then

       do n=nstart,mmax-1
          x(1,n)=f(n+1)*y(1,n+1)
          x(2,n)=f(n+1)*y(2,n+1)
       end do
       do n=nstart+1,mmax
          x(1,n)=x(1,n)+f(n)*y(1,n-1)
          x(2,n)=x(2,n)+f(n)*y(2,n-1)
       end do
    end if

  end subroutine f_mult


  subroutine c_mult(x,y,c,m,mmax)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    c_mult
!
!   prgrmmr:
!
! abstract:      x = C*y
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!     y,c
!
!   output argument list:
!     x
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(m:mmax):: c
    real(r_kind),intent(in   ),dimension(2,m:mmax):: y
    real(r_kind),intent(  out),dimension(2,m:mmax):: x
    integer(i_kind),intent(in)::m,mmax

    integer(i_kind) n,nstart


!   following is to account for 0,0 term being zero

    x(1,m)=zero
    x(2,m)=zero
    nstart=max(m,1)

    do n=nstart,mmax
       x(1,n)=c(n)*y(1,n)
       x(2,n)=c(n)*y(2,n)
    end do

  end subroutine c_mult


  subroutine i_mult(x,y,m,mmax)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    i_mult
!
!   prgrmmr:
!
! abstract:      x = sqrt(-1)*y
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!     y
!
!   output argument list:
!     x
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(2,m:mmax):: y
    real(r_kind),intent(  out),dimension(2,m:mmax):: x
    integer(i_kind),intent(in)::m,mmax

    integer(i_kind) n,nstart


!   following is to account for 0,0 term being zero

    x(1,m)=zero
    x(2,m)=zero
    nstart=max(m,1)

    do n=nstart,mmax
       x(1,n)=-y(2,n)
       x(2,n)=y(1,n)
    end do

  end subroutine i_mult


  subroutine solve_f2c2(x,y,f,c,m,mmax)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    solve_f2c2
!
!   prgrmmr:
!
! abstract:      solve (F*F+C*C)*x = y
!         
! program history log:
!   2008-05-05  safford -- add subprogram doc block, rm unused uses
!               
!   input argument list:
!     y,f,c
!
!   output argument list:
!     x
!   
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   
!$$$ end documentation block

    implicit none

    real(r_kind),intent(in   ),dimension(m:mmax):: f,c
    real(r_kind),intent(in   ),dimension(2,m:mmax):: y
    real(r_kind),intent(  out),dimension(2,m:mmax):: x
    integer(i_kind),intent(in)::m,mmax

    integer(i_kind) n,nstart
    real(r_kind),dimension(m:mmax):: a,b
    real(r_kind),dimension(2,m:mmax):: z


!   following is to account for 0,0 term being zero

    x(1,m)=zero
    x(2,m)=zero
    nstart=max(m,1)

!     copy forcing y to internal array

    do n=nstart,mmax
       z(1,n)=y(1,n)
       z(2,n)=y(2,n)
    end do

!     if nstart==mmax, then trivial solution

    if(nstart==mmax) then
       a(  nstart)=c(  nstart)*c(nstart)
       x(1,nstart)=z(1,nstart)/a(nstart)
       x(2,nstart)=z(2,nstart)/a(nstart)
    else

!       compute main diagonal of F*F + C*C

       a(nstart)=f(nstart+1)*f(nstart+1)+c(nstart)*c(nstart)
       if(nstart+1 < mmax) then
          do n=nstart+1,mmax-1
             a(n)=f(n)*f(n)+f(n+1)*f(n+1)+c(n)*c(n)
          end do
       end if
       a(mmax)=f(mmax)*f(mmax)+c(mmax)*c(mmax)

!       compute only non-zero off-diagonal of F*F + C*C

       if(nstart+2<=mmax) then
          do n=nstart+2,mmax
             b(n)=f(n-1)*f(n)
             a(n)=a(n)-b(n)*b(n)/a(n-2)
          end do

!        forward elimination:

          do n=nstart,mmax-2
             z(1,n+2)=z(1,n+2)-b(n+2)*z(1,n)/a(n)
             z(2,n+2)=z(2,n+2)-b(n+2)*z(2,n)/a(n)
          end do

       end if

!        backward substitution:

       x(1,mmax  )=z(1,mmax  )/a(mmax  )
       x(2,mmax  )=z(2,mmax  )/a(mmax  )
       x(1,mmax-1)=z(1,mmax-1)/a(mmax-1)
       x(2,mmax-1)=z(2,mmax-1)/a(mmax-1)
       if(nstart+2 <= mmax) then
          do n=mmax-2,nstart,-1
             x(1,n)=(z(1,n) - b(n+2)*x(1,n+2))/a(n)
             x(2,n)=(z(2,n) - b(n+2)*x(2,n+2))/a(n)
          end do
       end if

    end if

  end subroutine solve_f2c2
          
end module mod_strong
