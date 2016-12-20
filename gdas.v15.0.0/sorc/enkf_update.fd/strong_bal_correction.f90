subroutine strong_bal_correction(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps,bal_diagnostic,fullfield,update,uvflag)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_bal_correction  strong balance correction
!   prgmmr: parrish          org: np23                date: 2007-02-15
!
! abstract: given input perturbation tendencies of u,v,t,ps from TLM,
!           and input perturbation u,v,t,ps, compute balance adjustment to u,v,t,ps
!           which zeroes out input gravity component of perturbation tendencies.
!           also output, for later use, input tendencies projected onto gravity modes.
!           this is higher level routine, which calls more specific routines, based
!           on the value of parameters regional and reg_tlnmc_type, passed through module mod_strong
!
!           If .not. regional then call global application
!           If regional, then
!               reg_tlnmc_type = 1 for 1st version of regional application
!                              = 2 for 2nd version of regional application
!           
!
! program history log:
!   2007-02-15  parrish
!   2008-08-10  derber - update to output correction to psi and chi for global
!   2012-02-08  kleist - add uvflag to argument list
!   2013-07-02  parrish - changes to eliminate tlnmc_type for global tlnmc and
!                          add new variable reg_tlnmc_type for two versions of
!                          regional tlnmc.
!
!   input argument list:
!     u_t      - input perturbation u tendency (subdomains)
!     v_t      - input perturbation v tendency (subdomains)
!     t_t      - input perturbation T tendency (subdomains)
!     ps_t     - input perturbation surface pressure tendency (subdomains)
!     mype     - current processor
!     psi
!     chi
!     t        - input perturbation T (subdomains)
!     ps       - input perturbation surface pressure (subdomains)
!     bal_diagnostic - if true, then compute BAL diagnostic, a measure of amplitude
!                      of balanced gravity mode tendencies
!     fullfield - if true, the BAL diagnostics are full field, if false, they are 
!                 incremental.
!     update   - if false, then do not update u,v,t,ps with balance increment
!
!   output argument list:
!     u_t      - output perturbation u tendency (subdomains)
!     v_t      - output perturbation v tendency (subdomains)
!     t_t      - output perturbation T tendency (subdomains)
!     ps_t     - output perturbation surface pressure tendency (subdomains)
!     psi
!     chi
!     t        - output balanced perturbation T (subdomains)
!     ps       - output balanced perturbation surface pressure (subdomains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use mod_strong, only: reg_tlnmc_type
  use zrnmi_mod, only: zrnmi_strong_bal_correction
  use strong_fast_global_mod, only: strong_bal_correction_fast_global
  use gridmod, only: lat2,lon2,nsig,regional
  implicit none

  integer(i_kind)                       ,intent(in   ) :: mype
  logical                               ,intent(in   ) :: bal_diagnostic,update,fullfield,uvflag
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: psi,chi,t
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps

  if(.not.regional) then

!    global option:

     call strong_bal_correction_fast_global(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps,bal_diagnostic,fullfield,update,uvflag)

  else

     if(reg_tlnmc_type==1) then

!       regional option 1:

        call zrnmi_strong_bal_correction(u_t,v_t,t_t,ps_t,psi,chi,t,ps,bal_diagnostic,fullfield,update,mype)

     elseif(reg_tlnmc_type==2) then

!       regional option 2:

        !call fmg_strong_bal_correction_ad_test(u_t,v_t,t_t,ps_t,psi,chi,t,ps,mype)
        !call zrnmi_filter_uvm_ad_test(mype)

        call fmg_strong_bal_correction(u_t,v_t,t_t,ps_t,psi,chi,t,ps,bal_diagnostic,fullfield,update,mype)

     end if

  end if

end subroutine strong_bal_correction

subroutine strong_bal_correction_ad(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps,uvflag)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_bal_correction_ad  adjoint strong balance correction
!   prgmmr: parrish          org: np23                date: 2007-02-15
!
! abstract: given input perturbation tendencies of u,v,t,ps from TLM,
!           and input perturbation u,v,t,ps, compute balance adjustment to u,v,t,ps
!           which zeroes out input gravity component of perturbation tendencies.
!           also output, for later use, input tendencies projected onto gravity modes.
!           this is higher level routine, which calls more specific routines, based
!           on the value of parameters regional and reg_tlnmc_type, passed through module mod_strong
!
!           If .not. regional then call global application
!           If regional, then
!               reg_tlnmc_type = 1 for 1st version of regional application
!                              = 2 for 2nd version of regional application
!           
!
! program history log:
!   2007-02-15  parrish
!   2008-08-10  derber - update to output correction to psi and chi for global
!   2012-02-08  kleist - add uvflag to argument list
!
!   input argument list:
!     u_t      - input perturbation u tendency (subdomains)
!     v_t      - input perturbation v tendency (subdomains)
!     t_t      - input perturbation T tendency (subdomains)
!     ps_t     - input perturbation surface pressure tendency (subdomains)
!     mype     - current processor
!     psi
!     chi
!     t        - input perturbation T (subdomains)
!     ps       - input perturbation surface pressure (subdomains)
!
!   output argument list:
!     u_t      - output adjusted perturbation u_t (subdomains)
!     v_t      - output adjusted perturbation v_t (subdomains)
!     t_t      - output adjusted perturbation T_t (subdomains)
!     ps_t     - output adjusted perturbation ps_t (subdomains)
!     psi
!     chi
!     t        - output perturbation T (subdomains)
!     ps       - output perturbation surface pressure (subdomains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use mod_strong, only: reg_tlnmc_type
  use zrnmi_mod, only: zrnmi_strong_bal_correction_ad
  use strong_fast_global_mod, only: strong_bal_correction_fast_global_ad
  use gridmod, only: lat2,lon2,nsig,regional
  implicit none

  integer(i_kind)                       ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: psi,chi,t
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps
  logical,intent(in):: uvflag

  logical update

  if(.not.regional) then

!    global option:

     call strong_bal_correction_fast_global_ad(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps,uvflag)

  else

     if(reg_tlnmc_type==1) then

!       regional option 1:

        update=.true.
        call zrnmi_strong_bal_correction_ad(u_t,v_t,t_t,ps_t,psi,chi,t,ps,update,mype)

     elseif(reg_tlnmc_type==2) then

!       regional option 2:

        update=.true.
        call fmg_strong_bal_correction_ad(u_t,v_t,t_t,ps_t,psi,chi,t,ps,update,mype)
     end if

  end if

end subroutine strong_bal_correction_ad

