module strong_fast_global_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    strong_fast_global_mod
!
! abstract: contains all routines for fast strong balance constraint
!

! program history log:
!   2008-04-04  safford - add moddule doc block and missing subroutine doc blocks
!   2012-02-08  kleist  - add uvflag in place of uv_hyb_ens
!   2012-11-23  parrish - Replace calls to spanaly_ns and spsynth_ns with inline code.
!                          Remove subroutines spanaly_ns and spsynth_ns.
!   2013-07-02  parrish - change name of init_strongvars_2 to init_strongvars.
!
! subroutines included:
!    init_strongvars          --
!    strong_bal_correction_fast_global
!    strong_bal_correction_fast_global_ad -- adjoint of strong_bal_correction
!    gather_rmstends0         -- get bal diagnostics
!    gather_rmstends          -- get bal diagnostics
!    inmi_coupler_sd2ew0      --
!    inmi_coupler_sd2ew1      --
!    inmi_coupler_sd2ew       --
!    inmi_coupler_ew2sd1      --
!    inmi_coupler_ew2sd       --
!    inmi_ew_trans            --
!    inmi_ew_invtrans_ad      --
!    inmi_ew_invtrans         --
!    inmi_ew_trans_ad         --
!    inmi_coupler_ew2ns0      --
!    inmi_coupler_ew2ns1      --
!    inmi_coupler_ew2ns       --
!    inmi_coupler_ns2ew       --
!    inmi_nsuvm2zdm           --
!    spdz2uv_ns               -- compute winds from div and vort for 1 zonal wave number
!    spuv2dz_ns               -- compute div,vort from winds for one zonal wave number
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!$$$ end documentation block

  use kinds, only: i_kind,r_kind
  use gridmod, only: nlat,nlon,lat2,lon2,nsig,sp_a,jstart,istart
  use gridmod, only: ilat1,jlon1
  use constants, only: zero,one,two,rearth
  use mpimod, only: ierror,mpi_comm_world,mpi_integer4,mpi_rtype,mpi_sum,npe
  use mod_vtrans, only: speeds,nvmodes_keep,vtrans,vtrans_inv,vtrans_ad,vtrans_inv_ad
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_strongvars
  public :: strong_bal_correction_fast_global
  public :: strong_bal_correction_fast_global_ad
  public :: gather_rmstends0
  public :: gather_rmstends
  public :: inmi_coupler_sd2ew0
  public :: inmi_coupler_sd2ew1
  public :: inmi_coupler_sd2ew
  public :: inmi_coupler_ew2sd1
  public :: inmi_coupler_ew2sd
  public :: inmi_ew_trans
  public :: inmi_ew_invtrans_ad
  public :: inmi_ew_invtrans
  public :: inmi_ew_trans_ad
  public :: inmi_coupler_ew2ns0
  public :: inmi_coupler_ew2ns1
  public :: inmi_coupler_ew2ns
  public :: inmi_coupler_ns2ew
  public :: inmi_nsuvm2zdm
  public :: spdz2uv_ns
  public :: spuv2dz_ns

  integer(i_kind),allocatable:: mode_list(:,:)
                                                   !  mode_list(1,j) = lat index for ew strip j
                                                   !  mode_list(2,j) = vert mode number for ew strip j
                                                   !  mode_list(3,j) = pe of this lat/vert mode strip 
  integer(i_kind),allocatable:: mmode_list(:,:)
                                                   !  mmode_list(1,j) = m1 (zonal wave number 1) for ns strip
                                                   !  mmode_list(2,j) = m2 (zonal wave number 2) for ns strip
                                                   !  mmode_list(3,j) = vert mode number 1 for ns strip j
                                                   !  mmode_list(4,j) = vert mode number 2 for ns strip j
                                                   !  mmode_list(5,j) = pe for ns strip j
  integer(i_kind) nlatm_0,nlatm_1,m_0,m_1
  integer(i_kind) mthis
  integer(i_kind) nallsend_ew2sd,nallrecv_ew2sd
  integer(i_kind) nallsend,nallrecv
  integer(i_kind) nallsend_sd2ew,nallrecv_sd2ew
  integer(i_kind),allocatable,dimension(:)::mthis0,ndisp,indexglob
  integer(i_kind),allocatable,dimension(:)::nsend_sd2ew,nrecv_sd2ew
  integer(i_kind),allocatable,dimension(:)::ndsend_sd2ew,ndrecv_sd2ew
  integer(i_kind),allocatable,dimension(:)::nsend_ew2sd,nrecv_ew2sd
  integer(i_kind),allocatable,dimension(:)::ndsend_ew2sd,ndrecv_ew2sd
  integer(i_kind),allocatable,dimension(:)::nsend,nrecv,ndsend,ndrecv
  integer(i_kind),allocatable,dimension(:,:)::info_send_sd2ew,info_recv_sd2ew
  integer(i_kind),allocatable,dimension(:,:)::info_send_ew2sd,info_recv_ew2sd
  integer(i_kind),allocatable,dimension(:,:)::info_send,info_recv

contains

subroutine init_strongvars(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_strongvars
!
!   prgrmmr: parrish
!
! abstract: initialize strong fast global initialization
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block
!   2010-03-31  treadon - replace specmod jcap with sp_a structure
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  integer(i_kind),intent(in   ) :: mype

  allocate(mode_list(3,nlat*nvmodes_keep),mmode_list(5,(sp_a%jcap+1)*nvmodes_keep))
  call inmi_coupler_sd2ew0(mype)
  call inmi_coupler_ew2ns0(mype)
  call gather_rmstends0
  call inmi_coupler_sd2ew1(mype)
  call inmi_coupler_ew2ns1(mype)
  call inmi_coupler_ew2sd1(mype)

  return
end subroutine init_strongvars


subroutine strong_bal_correction_fast_global(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps, &
                                              bal_diagnostic,fullfield,update,uvflag)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_bal_correction  strong balance correction
!   prgmmr: parrish          org: np23                date: 2006-07-15
!
! abstract: given input perturbation tendencies of u,v,t,ps from tlm on gaussian grid,
!           and input perturbation u,v,t,ps, compute balance adjustment to u,v,t,ps
!           which zeroes out input gravity component of perturbation tendencies.
!           also output, for later use, input tendencies projected onto gravity modes.
!
! program history log:
!   2006-07-15  parrish
!   2007-04-16  kleist  - modified for full field or incremental diagnostics
!   2008-10-08  parrish/derber - modify to output streamfunction and vel. pot. and not update time derivatives
!   2009-11-27  parrish - add uvflag.  if uvflag=true, then
!                          input/output variables psi=u, chi=v.
!   2010-03-31  treadon - replace specmod jcap with sp_a structure
!   2012-02-08  kleist  - add uvflag in place of uv_hyb_ens
!   2013-10-26  todling - prevent division by zero when rms's are zero
!
!   input argument list:
!     u_t      - input perturbation u tendency on gaussian grid (subdomains)
!     v_t      - input perturbation v tendency on gaussian grid (subdomains)
!     t_t      - input perturbation t tendency on gaussian grid (subdomains)
!     ps_t     - input perturbation surface pressure tendency on gaussian grid (subdomains)
!     mype     - current processor
!     psi      - input perturbation psi on gaussian grid (subdomains)
!     chi      - input perturbation chi on gaussian grid (subdomains)
!     t        - input perturbation t on gaussian grid (subdomains)
!     ps       - input perturbation surface pressure on gaussian grid (subdomains)
!     bal_diagnostic - if true, then compute bal diagnostic, a measure of amplitude
!                      of balanced gravity mode tendencies
!     fullfield - if true, full field diagnostics
!                 if false, incremental 
!     update   - if false, then do not update u,v,t,ps with balance increment
!
!   output argument list:
!     psi      - output balanced perturbation u on gaussian grid (subdomains)
!     chi      - output balanced perturbation v on gaussian grid (subdomains)
!     t        - output balanced perturbation t on gaussian grid (subdomains)
!     ps       - output balanced perturbation surface pressure on gaussian grid (subdomains)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use mod_strong, only: dinmi,gproj,gproj_diag,gproj_diag_update
  use constants, only: tiny_r_kind
  implicit none

  integer(i_kind)                       ,intent(in   ) :: mype
  logical                               ,intent(in   ) :: bal_diagnostic,update,fullfield,uvflag
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: psi,chi,t
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps

  real(r_kind),dimension(nvmodes_keep)::rmstend_uf,rmstend_g_uf
  real(r_kind),dimension(nvmodes_keep)::rmstend_f,rmstend_g_f

  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde_t,vtilde_t,mtilde_t
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::delutilde,delvtilde,delmtilde
  real(r_kind),dimension(2,0:sp_a%jcap)::divhat,vorthat,mhat,deldivhat,delvorthat,delmhat
  real(r_kind),allocatable,dimension(:,:)::rmstend_loc_uf,rmstend_g_loc_uf
  real(r_kind),allocatable,dimension(:,:)::rmstend_loc_f,rmstend_g_loc_f
  real(r_kind),allocatable,dimension(:,:,:,:)::uvm_ew
  real(r_kind),allocatable,dimension(:,:,:,:,:)::uvm_ewtrans,uvm_ns,zdm_hat
  real(r_kind) rmstend_all_uf,rmstend_all_g_uf,rmstend_all_f,rmstend_all_g_f
  real(r_kind) del2inv,rn,gspeed
  real(r_kind) diff1,diffi

  integer(i_kind) i,ipair,kk,mode,n,mmax,m

  if(.not. (update .or. bal_diagnostic))return

  mmax=sp_a%jcap

!   1.  u,v,t,ps   -->    utilde,vtilde,mtilde  (vertical mode transform)
!       (subdomains)         (subdomains)

  call vtrans(u_t,v_t,t_t,ps_t,utilde_t,vtilde_t,mtilde_t)

  allocate(uvm_ew(nlon,2,3,nlatm_0:nlatm_1),uvm_ewtrans(2,0:sp_a%jcap,2,3,nlatm_0:nlatm_1))
  allocate(uvm_ns(3,2,nlat,2,m_0:m_1),zdm_hat(3,2,nlat,2,m_0:m_1))
  call inmi_coupler_sd2ew(utilde_t,vtilde_t,mtilde_t,utilde_t,vtilde_t,mtilde_t, &
                          uvm_ew,mype)
  call inmi_ew_trans(uvm_ew,uvm_ewtrans)
  call inmi_coupler_ew2ns(uvm_ewtrans,uvm_ns)
  call inmi_nsuvm2zdm(uvm_ns,zdm_hat)
  if(bal_diagnostic)then
    allocate(rmstend_loc_f(2,m_0:m_1))
    allocate(rmstend_g_loc_f(2,m_0:m_1))
    rmstend_loc_f=zero
    rmstend_g_loc_f=zero
    allocate(rmstend_loc_uf(2,m_0:m_1))
    allocate(rmstend_g_loc_uf(2,m_0:m_1))
    rmstend_g_loc_uf=zero
    rmstend_loc_uf=zero
  end if
  if(update)then
!$omp parallel do  schedule(dynamic,1) private(kk,ipair,m,mode,gspeed,i,n) &
!$omp private(vorthat,divhat,delvorthat,deldivhat,delmhat,mhat,rn,del2inv)
    do kk=m_0,m_1
       do ipair=1,2
          m=mmode_list(ipair,kk)
          mode=mmode_list(ipair+2,kk)
          gspeed=speeds(abs(mode))
          i=0
          do n=m,sp_a%jcap
             i=i+1
             vorthat(1,n)=zdm_hat(1,1,i,ipair,kk)
             vorthat(2,n)=zdm_hat(1,2,i,ipair,kk)
             divhat( 1,n)=zdm_hat(2,1,i,ipair,kk)
             divhat( 2,n)=zdm_hat(2,2,i,ipair,kk)
             mhat(   1,n)=zdm_hat(3,1,i,ipair,kk)
             mhat(   2,n)=zdm_hat(3,2,i,ipair,kk)
          end do

!   4.  divhat,vorthat,mhat --> deldivhat,delvorthat,delmhat   (inmi correction)
!          (slabs)                        (slabs)

          if(mode >  0) then
!               here, delvorthat, etc contain field corrections necessary to zero out gravity component
!                                         of tendencies
             call dinmi(vorthat(1,m),divhat(1,m),mhat(1,m),delvorthat(1,m),deldivhat(1,m),delmhat(1,m),&
                      m,mmax,gspeed)
          else
!               here, delvorthat, etc contain gravity component of tendencies
             if(bal_diagnostic) then   ! bal_diagnostic and update
                call gproj_diag_update(vorthat(1,m),divhat(1,m),mhat(1,m),delvorthat(1,m),deldivhat(1,m),delmhat(1,m), &
                          rmstend_loc_uf(ipair,kk),rmstend_g_loc_uf(ipair,kk),rmstend_loc_f(ipair,kk), &
                          rmstend_g_loc_f(ipair,kk),m,mmax,gspeed)
             else              !  update only
                call gproj(vorthat(1,m),divhat(1,m),mhat(1,m),delvorthat(1,m),deldivhat(1,m),delmhat(1,m), &
                          m,mmax,gspeed)
             end if
          end if

          if(.not. uvflag)then
             do n=m,sp_a%jcap
                if(n >  0) then
                   rn=real(n,r_kind)
                   del2inv=-rearth**2/(rn*(rn+one))
                else
                   del2inv=zero
                end if
                delvorthat(1,n)=delvorthat(1,n)*del2inv
                delvorthat(2,n)=delvorthat(2,n)*del2inv
                deldivhat(1,n)=deldivhat(1,n)*del2inv
                deldivhat(2,n)=deldivhat(2,n)*del2inv
             end do
          end if
             
          i=0
          do n=m,sp_a%jcap
             i=i+1
             zdm_hat(1,1,i,ipair,kk)=delvorthat(1,n)
             zdm_hat(1,2,i,ipair,kk)=delvorthat(2,n)
             zdm_hat(2,1,i,ipair,kk)=deldivhat(1,n)
             zdm_hat(2,2,i,ipair,kk)=deldivhat(2,n)
             zdm_hat(3,1,i,ipair,kk)=delmhat(1,n)
             zdm_hat(3,2,i,ipair,kk)=delmhat(2,n)
          end do
        end do
     end do
!   7.  delutilde,delvtilde,delmtilde  -->  psi,chi,t,ps   (vertical mode inverse transform)
!       (subdomains)                      (subdomains)

!  update u,v,t,ps

     if(uvflag) then
        call inmi_nszdm2uvm(uvm_ns,zdm_hat)
     else
        call inmi_nspcm_hat2pcm(uvm_ns,zdm_hat)
     end if
     call inmi_coupler_ns2ew(uvm_ewtrans,uvm_ns)
     call inmi_ew_invtrans(uvm_ew,uvm_ewtrans)
     call inmi_coupler_ew2sd(delutilde,delvtilde,delmtilde,utilde_t,vtilde_t,mtilde_t,uvm_ew,mype)
     call vtrans_inv(delutilde,delvtilde,delmtilde,psi,chi,t,ps)
!    u_t_g=zero;v_t_g=zero;t_t_g=zero;ps_t_g=zero
!    call vtrans_inv(utilde_t,vtilde_t,mtilde_t,u_t_g,v_t_g,t_t_g,ps_t_g)
  else            ! bal_diagnostic only no update
!$omp parallel do  schedule(dynamic,1) private(kk,ipair,m,mode,gspeed,i,n) &
!$omp private(vorthat,divhat,mhat)
     do kk=m_0,m_1
        do ipair=1,2
           mode=mmode_list(ipair+2,kk)
           if(mode > 0)cycle
           m=mmode_list(ipair,kk)
           gspeed=speeds(abs(mode))
           i=0
           do n=m,sp_a%jcap
              i=i+1
              vorthat(1,n)=zdm_hat(1,1,i,ipair,kk)
              vorthat(2,n)=zdm_hat(1,2,i,ipair,kk)
              divhat( 1,n)=zdm_hat(2,1,i,ipair,kk)
              divhat( 2,n)=zdm_hat(2,2,i,ipair,kk)
              mhat(   1,n)=zdm_hat(3,1,i,ipair,kk)
              mhat(   2,n)=zdm_hat(3,2,i,ipair,kk)
           end do

           call gproj_diag(vorthat(1,m),divhat(1,m),mhat(1,m), &
                     rmstend_loc_uf(ipair,kk),rmstend_g_loc_uf(ipair,kk),rmstend_loc_f(ipair,kk), &
                     rmstend_g_loc_f(ipair,kk),m,mmax,gspeed)
        end do
     end do
  end if

  deallocate(uvm_ew,uvm_ewtrans,uvm_ns,zdm_hat)


  if(bal_diagnostic) then
     call gather_rmstends(rmstend_loc_uf,  rmstend_uf)
     call gather_rmstends(rmstend_g_loc_uf,rmstend_g_uf)
     call gather_rmstends(rmstend_loc_f,   rmstend_f)
     call gather_rmstends(rmstend_g_loc_f, rmstend_g_f)
     if(mype == 0) then
        rmstend_all_uf=zero
        rmstend_all_g_uf=zero

        if (fullfield) then
           write(6,*) 'strong_fast_global:   full field balance diagnostics --  '
        else
           write(6,*) 'strong_fast_global:   incremental balance diagnostics --  '
        end if

        do i=1,nvmodes_keep
           rmstend_all_uf=rmstend_all_uf+rmstend_uf(i)
           rmstend_all_g_uf=rmstend_all_g_uf+rmstend_g_uf(i)
           diffi = rmstend_uf(i)-rmstend_g_uf(i) 
           diff1 = rmstend_uf(1)-rmstend_g_uf(1)
           if(abs(diffi)<tiny_r_kind.or.abs(diff1)<tiny_r_kind) then
             write(6,'(" mode,rmstend_uf,rmstend_g_uf,rat = ",i5,2e28.18)') &
                              i,rmstend_uf(i),rmstend_g_uf(i)
           else
             write(6,'(" mode,rmstend_uf,rmstend_g_uf,rat = ",i5,2e28.18,2f24.18)') &
                              i,rmstend_uf(i),rmstend_g_uf(i),&
                              rmstend_g_uf(i)/diffi, &
                              rmstend_g_uf(i)/diff1
           endif
        end do
        rmstend_all_f=zero
        rmstend_all_g_f=zero
        do i=1,nvmodes_keep
           rmstend_all_f=rmstend_all_f+rmstend_f(i)
           rmstend_all_g_f=rmstend_all_g_f+rmstend_g_f(i)
           diffi = rmstend_f(i)-rmstend_g_f(i)
           diff1 = rmstend_f(1)-rmstend_g_f(1)
           if(abs(diffi)<tiny_r_kind.or.abs(diff1)<tiny_r_kind) then
             write(6,'(" mode,rmstend_f,rmstend_g_f = ",i5,2e28.18)') &
                              i,rmstend_f(i),rmstend_g_f(i)
           else
             write(6,'(" mode,rmstend_f,rmstend_g_f,rat = ",i5,2e28.18,2f24.18)') &
                              i,rmstend_f(i),rmstend_g_f(i),&
                              rmstend_g_f(i)/diffi, &
                              rmstend_g_f(i)/diff1
           endif
        end do
        diffi = rmstend_all_uf-rmstend_all_g_uf
        diff1 = rmstend_all_f-rmstend_all_g_f
        if(abs(diffi)<tiny_r_kind.or.abs(diff1)<tiny_r_kind) then
          write(6,'(" rmstend_all_uf,g_uf,rat = ",2e28.18)') rmstend_all_uf,rmstend_all_g_uf
          write(6,'(" rmstend_all_f,g_f,rat = ",2e28.18)') rmstend_all_f,rmstend_all_g_f
        else
          write(6,'(" rmstend_all_uf,g_uf,rat = ",2e28.18,f24.18)') rmstend_all_uf,rmstend_all_g_uf, &
                                                   diffi
          write(6,'(" rmstend_all_f,g_f,rat = ",2e28.18,f24.18)') rmstend_all_f,rmstend_all_g_f, &
                                                   diff1
        endif
     end if
     deallocate(rmstend_loc_uf,rmstend_g_loc_uf)
     deallocate(rmstend_loc_f,rmstend_g_loc_f)
  end if
  return

end subroutine strong_bal_correction_fast_global

subroutine strong_bal_correction_fast_global_ad(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps,uvflag)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_bal_correction_ad adjoint of strong_bal_correction
!   prgmmr: parrish          org: np23                date: 2006-07-15
!
! abstract: adjoint of strong_bal_correction
!
! program history log:
!   2006-07-15  parrish
!   2008-10-08  parrish/derber - modify to output streamfunction and vel. pot. and not update time derivatives
!   2009-11-27  parrish - add uvflag.  if present and true, then
!                          input/output variables psi=u, chi=v.
!   2010-03-31  treadon - replace specmod jcap with sp_a structure
!   2012-02-08  kleist  - add uvflag in place of uv_hyb_ens
!
!   input argument list:
!     u_t      - input perturbation u tendency on gaussian grid (subdomains)
!     v_t      - input perturbation v tendency on gaussian grid (subdomains)
!     t_t      - input perturbation t tendency on gaussian grid (subdomains)
!     ps_t     - input perturbation surface pressure tendency on gaussian grid (subdomains)
!     mype     - current processor
!     psi      - input perturbation psi on gaussian grid (subdomains)
!     chi      - input perturbation chi on gaussian grid (subdomains)
!     t        - input perturbation t on gaussian grid (subdomains)
!     ps       - input perturbation surface pressure on gaussian grid (subdomains)
!
!   output argument list:
!     u_t      - output perturbation u tendency on gaussian grid (subdomains)
!     v_t      - output perturbation v tendency on gaussian grid (subdomains)
!     t_t      - output perturbation t tendency on gaussian grid (subdomains)
!     ps_t     - output perturbation surface pressure tendency on gaussian grid (subdomains)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use mod_strong, only: dinmi_ad,gproj_ad
  implicit none

  integer(i_kind)                       ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: psi,chi,t
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: ps
  logical, intent(in)                                  :: uvflag

  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde_t,vtilde_t,mtilde_t
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::delutilde,delvtilde,delmtilde
  real(r_kind),dimension(2,0:sp_a%jcap)::divhat,vorthat,mhat,deldivhat,delvorthat,delmhat
  real(r_kind),allocatable,dimension(:,:,:,:)::uvm_ew
  real(r_kind),allocatable,dimension(:,:,:,:,:)::uvm_ewtrans,uvm_ns,zdm_hat
  real(r_kind) del2inv,rn,gspeed

  integer(i_kind) i,ipair,kk,mode,n,m,mmax

  mmax=sp_a%jcap

!  adjoint of update u,v,t,ps

!   7.  adjoint of delutilde,delvtilde,delmtilde  -->  psi,chi,t,ps  (vert mode inverse transform)
!       (subdomains)                      (subdomains)

  delutilde=zero ; delvtilde=zero ; delmtilde=zero
  call vtrans_inv_ad(delutilde,delvtilde,delmtilde,psi,chi,t,ps)

  allocate(uvm_ew(nlon,2,3,nlatm_0:nlatm_1),uvm_ewtrans(2,0:sp_a%jcap,2,3,nlatm_0:nlatm_1))
  allocate(uvm_ns(3,2,nlat,2,m_0:m_1),zdm_hat(3,2,nlat,2,m_0:m_1))
  utilde_t=zero ; vtilde_t=zero ; mtilde_t=zero
  call inmi_coupler_sd2ew(delutilde,delvtilde,delmtilde,utilde_t,vtilde_t,mtilde_t,uvm_ew,mype)
  call inmi_ew_invtrans_ad(uvm_ew,uvm_ewtrans)
  call inmi_coupler_ew2ns(uvm_ewtrans,uvm_ns)
  if(uvflag) then
     call inmi_nszdm2uvm_ad(uvm_ns,zdm_hat)
  else
     call inmi_nspcm_hat2pcm_ad(uvm_ns,zdm_hat)
  end if
!$omp parallel do  schedule(dynamic,1) private(kk,ipair,m,mode,gspeed,i,n) &
!$omp private(vorthat,divhat,delvorthat,deldivhat,delmhat,mhat,rn,del2inv)
  do kk=m_0,m_1
     do ipair=1,2
        m=mmode_list(ipair,kk)
        mode=mmode_list(ipair+2,kk)
        gspeed=speeds(abs(mode))
        do n=m,sp_a%jcap
           vorthat(1,n)=zero
           vorthat(2,n)=zero
           divhat( 1,n)=zero
           divhat( 2,n)=zero
           mhat(   1,n)=zero
           mhat(   2,n)=zero
        end do
        i=0
        do n=m,sp_a%jcap
           i=i+1
           delvorthat(1,n)=zdm_hat(1,1,i,ipair,kk)
           delvorthat(2,n)=zdm_hat(1,2,i,ipair,kk)
           deldivhat(1,n)=zdm_hat(2,1,i,ipair,kk)
           deldivhat(2,n)=zdm_hat(2,2,i,ipair,kk)
           delmhat(1,n)=zdm_hat(3,1,i,ipair,kk)
           delmhat(2,n)=zdm_hat(3,2,i,ipair,kk)
        end do
        if(.not. uvflag) then
           do n=m,sp_a%jcap
              if(n >  0) then
                 rn=real(n,r_kind) 
                 del2inv=-rearth**2/(rn*(rn+one))
              else
                 del2inv=zero
              end if
              delvorthat(1,n)=delvorthat(1,n)*del2inv
              delvorthat(2,n)=delvorthat(2,n)*del2inv
              deldivhat(1,n)=deldivhat(1,n)*del2inv
              deldivhat(2,n)=deldivhat(2,n)*del2inv
           end do
        end if
        if(mode >  0) then
           call dinmi_ad(vorthat(1,m),divhat(1,m),mhat(1,m),&
                       delvorthat(1,m)   ,   deldivhat(1,m),   delmhat(1,m),m,mmax,gspeed)
        else
           call gproj_ad(vorthat(1,m),divhat(1,m),mhat(1,m),delvorthat(1,m),deldivhat(1,m), &
                 delmhat(1,m),m,mmax,gspeed)
        end if

        i=0
        do n=m,sp_a%jcap
           i=i+1
           zdm_hat(1,1,i,ipair,kk)=vorthat(1,n)
           zdm_hat(1,2,i,ipair,kk)=vorthat(2,n)
           zdm_hat(2,1,i,ipair,kk)=divhat(1,n)
           zdm_hat(2,2,i,ipair,kk)=divhat(2,n)
           zdm_hat(3,1,i,ipair,kk)=mhat(1,n)
           zdm_hat(3,2,i,ipair,kk)=mhat(2,n)
        end do

     end do
  end do

  call inmi_nsuvm2zdm_ad(uvm_ns,zdm_hat)
  call inmi_coupler_ns2ew(uvm_ewtrans,uvm_ns)
  call inmi_ew_trans_ad(uvm_ew,uvm_ewtrans)
  call inmi_coupler_ew2sd(utilde_t,vtilde_t,mtilde_t,delutilde,delvtilde,delmtilde,uvm_ew,mype)

  utilde_t=utilde_t+delutilde
  vtilde_t=vtilde_t+delvtilde
  mtilde_t=mtilde_t+delmtilde
!
!    1.  adjoint of u,v,t,ps   -->    utilde,vtilde,mtilde  (vertical mode transform)
!                      (subdomains)         (subdomains)

  call vtrans_ad(u_t,v_t,t_t,ps_t,utilde_t,vtilde_t,mtilde_t)
  deallocate(uvm_ew,uvm_ewtrans,uvm_ns,zdm_hat)

end subroutine strong_bal_correction_fast_global_ad

subroutine gather_rmstends0

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gather_rmstends0  get bal diagnostics
!   prgmmr: parrish          org: np23                date: 2006-08-03
!
! abstract: compute bal diagnostics which give amplitude of 
!           gravity projection of energy norm of tendencies
!
! program history log:
!   2006-08-03  parrish
!   2008-04-04  safford  - rm unused uses
!   2010-03-31  treadon - replace specmod jcap with sp_a structure
!
!   input argument list:
!
!   output argument list:
!     rmstend  -  all vertical modes of rmstend assembled across all processors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  implicit none

  integer(i_kind),dimension(m_0:m_1):: indexloc
  integer(i_kind) i
  
  allocate(mthis0(npe),ndisp(npe+1),indexglob((sp_a%jcap+1)*nvmodes_keep))
  do i=m_0,m_1
     indexloc(i)=i
  end do
  mthis=m_1-m_0+1
  call mpi_allgather(mthis,1,mpi_integer4,mthis0,1,mpi_integer4,mpi_comm_world,ierror)
  ndisp(1)=0
  do i=2,npe+1
     ndisp(i)=ndisp(i-1)+mthis0(i-1)
  end do
  call mpi_allgatherv(indexloc,mthis,mpi_integer4, &
                      indexglob,mthis0,ndisp,mpi_integer4,mpi_comm_world,ierror)

end subroutine gather_rmstends0

subroutine gather_rmstends(rmstend_loc,rmstend)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gather_rmstends  get bal diagnostics
!   prgmmr: parrish          org: np23                date: 2006-08-03
!
! abstract: compute bal diagnostics which give amplitude of 
!           gravity projection of energy norm of tendencies
!
! program history log:
!   2006-08-03  parrish
!   2008-04-04  safford  - rm unused uses
!
!   input argument list:
!     rmstend_loc - previously computed energy norms of vertical modes
!                   as distributed on local processors
!
!   output argument list:
!     rmstend  -  all vertical modes of rmstend assembled across all processors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  implicit none

  real(r_kind),intent(in   ) :: rmstend_loc(2,m_0:m_1)
  real(r_kind),intent(  out) :: rmstend(nvmodes_keep)

  real(r_kind),dimension(2,(sp_a%jcap+1)*nvmodes_keep)::work
  integer(i_kind) i,ii,mode,mpi_string1
  
  call mpi_type_contiguous(2,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_allgatherv(rmstend_loc,mthis,mpi_string1, &
                     work,mthis0,ndisp,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  rmstend=zero
  do i=1,ndisp(npe+1)
     ii=indexglob(i)
     mode=mmode_list(3,ii)
     if(mode <  0) rmstend(-mode)=work(1,ii)+rmstend(-mode)
     mode=mmode_list(4,ii)
     if(mode <  0) rmstend(-mode)=work(2,ii)+rmstend(-mode)
  end do

end subroutine gather_rmstends

subroutine inmi_coupler_sd2ew0(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_coupler_sd2ew0
!   prgmmr:
!
! abstract:  create ew (lat strips) subdivision for use with inmi
!
! program history log:
!   2008-04-04  safford  - add doc block
!
!   input argument list:
!     mype        - current processor number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  implicit none

  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) i,k,kchk,kk,n,nn,nlatm_this

  nlatm_this=nlat*nvmodes_keep/npe
  if(mod(nlat*nvmodes_keep,npe)==0) then
     kchk=npe
  else
     nlatm_this=nlatm_this+1
     kchk=mod(nlat*nvmodes_keep,npe)
  end if

  nn=0
  do k=1,nvmodes_keep
     do i=1,nlat
        nn=nn+1
        mode_list(1,nn)=i
        mode_list(2,nn)=k
        mode_list(3,nn)=-1
     end do
  end do
  
  nlatm_0=-1
  nlatm_1=-2
  nn=0
  do n=1,npe
     if(n <= kchk) then
        kk=nlatm_this
     else
        kk=nlatm_this-1
     end if
     if(kk >  0) then
        if(mype+1 == n) then
           nlatm_0=nn+1
           nlatm_1=nn+kk
        end if
        do k=1,kk
           nn=nn+1
           mode_list(3,nn)=n
        end do
     end if
  end do
end subroutine inmi_coupler_sd2ew0

subroutine inmi_coupler_sd2ew1(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_sd2ew1
!
!   prgrmmr:
!
! abstract: use mpi_alltoallv to move u_sd,v_sd,m_sd (subdomains) to uvm_ew (lat strips)
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none


  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) i,ii,ii0,ilat,imode,j,mm1,nlonloc,ipe,ilatm,ilon,mpi_string1
  real(r_kind),dimension(nlat,nvmodes_keep):: mode2_list

  allocate(nsend_sd2ew(npe),nrecv_sd2ew(npe))
  allocate(ndsend_sd2ew(npe+1),ndrecv_sd2ew(npe+1))
  mm1=mype+1

  mode2_list=0
  do i=1,nlat*nvmodes_keep
     ilat=mode_list(1,i)
     imode=mode_list(2,i)
     if(mode2_list(ilat,imode) /= 0) then
        if(mype == 0) write(6,*)' problem in inmi_coupler_sd2ew0'
        call mpi_finalize(i)
        stop
     end if
     mode2_list(ilat,imode)=i
  end do
  do imode=1,nvmodes_keep
     do ilat=1,nlat
        if(mode2_list(ilat,imode) == 0) then
           if(mype == 0) write(6,*)' problem in inmi_coupler_sd2ew0'
           call mpi_finalize(i)
           stop
        end if
     end do
  end do



!  obtain counts of points to send to each pe from this pe

  nsend_sd2ew=0
  nlonloc=lon2-2
  do imode=1,nvmodes_keep
     if(imode == 0) cycle
     do i=2,lat2-1
        ilat=i+istart(mm1)-2
        j=mode2_list(ilat,imode)
        ipe=mode_list(3,j)
        nsend_sd2ew(ipe)=nsend_sd2ew(ipe)+nlonloc
     end do
  end do

  ndsend_sd2ew(1)=0
  do i=2,npe+1
     ndsend_sd2ew(i)=ndsend_sd2ew(i-1)+nsend_sd2ew(i-1)
  end do
  nallsend_sd2ew=ndsend_sd2ew(npe+1)
  allocate(info_send_sd2ew(2,nallsend_sd2ew))
  nsend_sd2ew=0
  do imode=1,nvmodes_keep
     if(imode == 0) cycle
     do i=2,lat2-1
        ilat=i+istart(mm1)-2
        ilatm=mode2_list(ilat,imode)
        ipe=mode_list(3,ilatm)
        do ii=2,lon2-1
           ilon=ii+jstart(mm1)-2
           nsend_sd2ew(ipe)=nsend_sd2ew(ipe)+1
           ii0=ndsend_sd2ew(ipe)+nsend_sd2ew(ipe)
           info_send_sd2ew(1,ii0)=ilon
           info_send_sd2ew(2,ii0)=ilatm
        end do
     end do
  end do

  call mpi_alltoall(nsend_sd2ew,1,mpi_integer4,nrecv_sd2ew,1,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_sd2ew(1)=0
  do i=2,npe+1
     ndrecv_sd2ew(i)=ndrecv_sd2ew(i-1)+nrecv_sd2ew(i-1)
  end do
  nallrecv_sd2ew=ndrecv_sd2ew(npe+1)
  allocate(info_recv_sd2ew(2,nallrecv_sd2ew))
  call mpi_type_contiguous(2,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_sd2ew,nsend_sd2ew,ndsend_sd2ew,mpi_string1, &
                     info_recv_sd2ew,nrecv_sd2ew,ndrecv_sd2ew,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
    
end subroutine inmi_coupler_sd2ew1

subroutine inmi_coupler_sd2ew(u_sd1,v_sd1,m_sd1,u_sd2,v_sd2,m_sd2,uvm_ew,mype)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_coupler_sd2ew
!
!   prgrmmr:
!
! abstract: use mpi_alltoallv to move u_sd,v_sd,m_sd (subdomains) to uvm_ew (lat strips)
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     mype     - mpi task id
!     u_sd1    -
!     v_sd1    -
!     m_sd1    -
!     u_sd2    -
!     v_sd2    -
!     m_sd2    -
!
!   output argument list:
!     uvm_ew   -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none


  integer(i_kind)                                 ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)  ,intent(in   ) :: u_sd1,v_sd1,m_sd1
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)  ,intent(in   ) :: u_sd2,v_sd2,m_sd2
  real(r_kind),dimension(nlon,2,3,nlatm_0:nlatm_1),intent(  out) :: uvm_ew

  real(r_kind),allocatable,dimension(:,:,:)::sendbuf,recvbuf
  integer(i_kind) ilat,imode,j,mm1,ilatm,ilon,mpi_string1

  mm1=mype+1

  allocate(sendbuf(2,3,nallsend_sd2ew))
  do j=1,nallsend_sd2ew
     ilon=info_send_sd2ew(1,j)-jstart(mm1)+2
     ilatm=info_send_sd2ew(2,j)
     ilat=mode_list(1,ilatm)-istart(mm1)+2
     imode=mode_list(2,ilatm)
     sendbuf(1,1,j)=u_sd1(ilat,ilon,imode)
     sendbuf(1,2,j)=v_sd1(ilat,ilon,imode)
     sendbuf(1,3,j)=m_sd1(ilat,ilon,imode)
     sendbuf(2,1,j)=u_sd2(ilat,ilon,imode)
     sendbuf(2,2,j)=v_sd2(ilat,ilon,imode)
     sendbuf(2,3,j)=m_sd2(ilat,ilon,imode)
  end do
  allocate(recvbuf(2,3,nallrecv_sd2ew))
  call mpi_type_contiguous(6,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_sd2ew,ndsend_sd2ew,mpi_string1, &
                     recvbuf,nrecv_sd2ew,ndrecv_sd2ew,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)
    
  do j=1,nallrecv_sd2ew
     ilon=info_recv_sd2ew(1,j)
     ilatm=info_recv_sd2ew(2,j)
     uvm_ew(ilon,1,1,ilatm)=recvbuf(1,1,j)
     uvm_ew(ilon,1,2,ilatm)=recvbuf(1,2,j)
     uvm_ew(ilon,1,3,ilatm)=recvbuf(1,3,j)
     uvm_ew(ilon,2,1,ilatm)=recvbuf(2,1,j)
     uvm_ew(ilon,2,2,ilatm)=recvbuf(2,2,j)
     uvm_ew(ilon,2,3,ilatm)=recvbuf(2,3,j)
  end do
  deallocate(recvbuf)
    
end subroutine inmi_coupler_sd2ew

subroutine inmi_coupler_ew2sd1(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ew2sd1
!
!   prgrmmr:
!
! abstract: use mpi_alltoallv to move uvm_ew (lat strips) to u_sd,v_sd,m_sd (subdomains)
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars and uses
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none


  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) i,ilat,imode,j,k,mm1,ipe,ilon,mpi_string1,nn

  allocate(nsend_ew2sd(npe),nrecv_ew2sd(npe))
  allocate(ndsend_ew2sd(npe+1),ndrecv_ew2sd(npe+1))
  mm1=mype+1

!      1.  for each pe, gather up list of points from this set of lat strips destined
!             for subdomain of pe
  do ipe=1,npe
     nn=0
     do k=nlatm_0,nlatm_1
        ilat=mode_list(1,k)
        imode=mode_list(2,k)
        i=ilat-istart(ipe)+2
        if(i <  1.or.i >  ilat1(ipe)+2) cycle
        do j=1,jlon1(ipe)+2
           ilon=j+jstart(ipe)-2
           if(ilon <  1) ilon=ilon+nlon
           if(ilon >  nlon) ilon=ilon-nlon
           nn=nn+1
        end do
     end do
     nsend_ew2sd(ipe)=nn
  end do

  ndsend_ew2sd(1)=0
  do i=2,npe+1
     ndsend_ew2sd(i)=ndsend_ew2sd(i-1)+nsend_ew2sd(i-1)
  end do
  nallsend_ew2sd=ndsend_ew2sd(npe+1)
  allocate(info_send_ew2sd(3,nallsend_ew2sd))
  nn=0
  do ipe=1,npe
     do k=nlatm_0,nlatm_1
        ilat=mode_list(1,k)
        imode=mode_list(2,k)
        i=ilat-istart(ipe)+2
        if(i <  1.or.i >  ilat1(ipe)+2) cycle
        do j=1,jlon1(ipe)+2
           ilon=j+jstart(ipe)-2
           if(ilon <  1) ilon=ilon+nlon
           if(ilon >  nlon) ilon=ilon-nlon
           nn=nn+1
           info_send_ew2sd(1,nn)=ilon
           info_send_ew2sd(2,nn)=j
           info_send_ew2sd(3,nn)=k
        end do
     end do
  end do

  call mpi_alltoall(nsend_ew2sd,1,mpi_integer4,nrecv_ew2sd,1,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_ew2sd(1)=0
  do i=2,npe+1
     ndrecv_ew2sd(i)=ndrecv_ew2sd(i-1)+nrecv_ew2sd(i-1)
  end do
  nallrecv_ew2sd=ndrecv_ew2sd(npe+1)
  allocate(info_recv_ew2sd(3,nallrecv_ew2sd))
  call mpi_type_contiguous(3,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_ew2sd,nsend_ew2sd,ndsend_ew2sd,mpi_string1, &
                     info_recv_ew2sd,nrecv_ew2sd,ndrecv_ew2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine inmi_coupler_ew2sd1

subroutine inmi_coupler_ew2sd(u_sd1,v_sd1,m_sd1,u_sd2,v_sd2,m_sd2,uvm_ew,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ew2sd
!
!   prgrmmr:
!
! abstract: use mpi_alltoallv to move uvm_ew (lat strips) to u_sd,v_sd,m_sd (subdomains)
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     mype     - mpi task id
!     uvm_ew   -
!
!   output argument list:
!     u_sd1    -
!     v_sd1    -
!     m_sd1    -
!     u_sd2    -
!     v_sd2    -
!     m_sd2    -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none


  integer(i_kind)                                 ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)  ,intent(  out) :: u_sd1,v_sd1,m_sd1
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)  ,intent(  out) :: u_sd2,v_sd2,m_sd2
  real(r_kind),dimension(nlon,2,3,nlatm_0:nlatm_1),intent(in   ) :: uvm_ew

  real(r_kind),allocatable,dimension(:,:,:)::sendbuf,recvbuf
  integer(i_kind) ilat,imode,j,mm1,ilatm,ilon,mpi_string1,ilonloc

  mm1=mype+1

  allocate(sendbuf(2,3,nallsend_ew2sd))
  do j=1,nallsend_ew2sd
     ilon=info_send_ew2sd(1,j)
     ilatm=info_send_ew2sd(3,j)
     sendbuf(1,1,j)=uvm_ew(ilon,1,1,ilatm)
     sendbuf(1,2,j)=uvm_ew(ilon,1,2,ilatm)
     sendbuf(1,3,j)=uvm_ew(ilon,1,3,ilatm)
     sendbuf(2,1,j)=uvm_ew(ilon,2,1,ilatm)
     sendbuf(2,2,j)=uvm_ew(ilon,2,2,ilatm)
     sendbuf(2,3,j)=uvm_ew(ilon,2,3,ilatm)
  end do
  allocate(recvbuf(2,3,nallrecv_ew2sd))
  call mpi_type_contiguous(6,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_ew2sd,ndsend_ew2sd,mpi_string1, &
                     recvbuf,nrecv_ew2sd,ndrecv_ew2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ew2sd
     ilonloc=info_recv_ew2sd(2,j)
     ilatm=info_recv_ew2sd(3,j)
     ilat=mode_list(1,ilatm)-istart(mm1)+2
     imode=mode_list(2,ilatm)
     u_sd1(ilat,ilonloc,imode)=recvbuf(1,1,j)
     v_sd1(ilat,ilonloc,imode)=recvbuf(1,2,j)
     m_sd1(ilat,ilonloc,imode)=recvbuf(1,3,j)
     u_sd2(ilat,ilonloc,imode)=recvbuf(2,1,j)
     v_sd2(ilat,ilonloc,imode)=recvbuf(2,2,j)
     m_sd2(ilat,ilonloc,imode)=recvbuf(2,3,j)
!--------------check for north or south pole
     ilat=-1
     if(mode_list(1,ilatm) == nlat) ilat=nlat-istart(mm1)+3
     if(mode_list(1,ilatm) == 1) ilat=2-istart(mm1)
     if(ilat == -1) cycle
!-----------------do repeat rows for north/south pole
     u_sd1(ilat,ilonloc,imode)=recvbuf(1,1,j)
     v_sd1(ilat,ilonloc,imode)=recvbuf(1,2,j)
     m_sd1(ilat,ilonloc,imode)=recvbuf(1,3,j)
     u_sd2(ilat,ilonloc,imode)=recvbuf(2,1,j)
     v_sd2(ilat,ilonloc,imode)=recvbuf(2,2,j)
     m_sd2(ilat,ilonloc,imode)=recvbuf(2,3,j)
  end do
  deallocate(recvbuf)

end subroutine inmi_coupler_ew2sd

subroutine inmi_ew_trans(uvm_ew,uvm_ewtrans)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_ew_trans
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!   2010-03-31  treadon - replace specmod components with sp_a structure
!
!   input argument list:
!     uvm_ew   -
!
!   output argument list:
!     uvm_ewtrans -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  real(r_kind),dimension(nlon,2,3,nlatm_0:nlatm_1)    ,intent(in   ) :: uvm_ew
  real(r_kind),dimension(2,0:sp_a%jcap,2,3,nlatm_0:nlatm_1),intent(  out) :: uvm_ewtrans

  integer(i_kind) i,j,k
  real(r_kind),dimension(2,0:nlon/2,2)::halfwave
  real(r_kind),dimension(50000+4*sp_a%imax)::tmpafft

!$omp parallel do  schedule(dynamic,1) private(k,j,i,tmpafft,halfwave)
  do k=nlatm_0,nlatm_1
     tmpafft=sp_a%afft
     do j=1,3
        call spffte(nlon,1+nlon/2,nlon,2,halfwave,uvm_ew(1,1,j,k),-1,tmpafft)
        do i=0,sp_a%jcap
           uvm_ewtrans(1,i,1,j,k)=halfwave(1,i,1)
           uvm_ewtrans(2,i,1,j,k)=halfwave(2,i,1)
           uvm_ewtrans(1,i,2,j,k)=halfwave(1,i,2)
           uvm_ewtrans(2,i,2,j,k)=halfwave(2,i,2)
        end do
     end do
  end do

end subroutine inmi_ew_trans

subroutine inmi_ew_invtrans_ad(uvm_ew,uvm_ewtrans)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_ew_invtrans_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!   2010-03-31  treadon - replace specmod components with sp_a structure
!
!   input argument list:
!     uvm_ew   -
!
!   output argument list:
!     uvm_ewtrans -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  real(r_kind),dimension(nlon,2,3,nlatm_0:nlatm_1)    ,intent(in   ) :: uvm_ew
  real(r_kind),dimension(2,0:sp_a%jcap,2,3,nlatm_0:nlatm_1),intent(  out) :: uvm_ewtrans

  integer(i_kind) i,j,k
  real(r_kind) fnlon,fnlon2
  real(r_kind),dimension(2,0:nlon/2,2)::halfwave
  real(r_kind),dimension(50000+4*sp_a%imax)::tmpafft

  fnlon=real(nlon,r_kind)
  fnlon2=two*fnlon

!$omp parallel do  schedule(dynamic,1) private(k,j,i,tmpafft,halfwave)
  do k=nlatm_0,nlatm_1
     tmpafft=sp_a%afft
     do j=1,3
        call spffte(nlon,1+nlon/2,nlon,2,halfwave,uvm_ew(1,1,j,k),-1,tmpafft)
        uvm_ewtrans(1,0,1,j,k)=halfwave(1,0,1)*fnlon
        uvm_ewtrans(2,0,1,j,k)=halfwave(2,0,1)*fnlon
        uvm_ewtrans(1,0,2,j,k)=halfwave(1,0,2)*fnlon
        uvm_ewtrans(2,0,2,j,k)=halfwave(2,0,2)*fnlon
        do i=1,sp_a%jcap
           uvm_ewtrans(1,i,1,j,k)=halfwave(1,i,1)*fnlon2
           uvm_ewtrans(2,i,1,j,k)=halfwave(2,i,1)*fnlon2
           uvm_ewtrans(1,i,2,j,k)=halfwave(1,i,2)*fnlon2
           uvm_ewtrans(2,i,2,j,k)=halfwave(2,i,2)*fnlon2
        end do
     end do
  end do

end subroutine inmi_ew_invtrans_ad

subroutine inmi_ew_invtrans(uvm_ew,uvm_ewtrans)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_ew_invtrans
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars
!   2010-03-31  treadon - replace specmod components with sp_a structure
!
!   input argument list:
!     uvm_ewtrans -
!
!   output argument list:
!     uvm_ew      -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  real(r_kind),dimension(nlon,2,3,nlatm_0:nlatm_1)    ,intent(  out) :: uvm_ew
  real(r_kind),dimension(2,0:sp_a%jcap,2,3,nlatm_0:nlatm_1),intent(in   ) :: uvm_ewtrans

  integer(i_kind) i,j,k
  real(r_kind),dimension(2,0:nlon/2,2)::halfwave
  real(r_kind),dimension(50000+4*sp_a%imax)::tmpafft

!$omp parallel do  schedule(dynamic,1) private(k,j,i,tmpafft,halfwave)
  do k=nlatm_0,nlatm_1
     tmpafft=sp_a%afft
     do j=1,3
        do i=0,sp_a%jcap
           halfwave(1,i,1)=uvm_ewtrans(1,i,1,j,k)
           halfwave(2,i,1)=uvm_ewtrans(2,i,1,j,k)
           halfwave(1,i,2)=uvm_ewtrans(1,i,2,j,k)
           halfwave(2,i,2)=uvm_ewtrans(2,i,2,j,k)
        end do
        do i=sp_a%jcap+1,nlon/2
           halfwave(1,i,1)=zero
           halfwave(2,i,1)=zero
           halfwave(1,i,2)=zero
           halfwave(2,i,2)=zero
        end do
        call spffte(nlon,1+nlon/2,nlon,2,halfwave,uvm_ew(1,1,j,k),1,tmpafft)
     end do
  end do

end subroutine inmi_ew_invtrans

subroutine inmi_ew_trans_ad(uvm_ew,uvm_ewtrans)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_ew_trans_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars
!   2010-03-31  treadon - replace specmod components with sp_a structure
!
!   input argument list:
!     uvm_ewtrans -
!
!   output argument list:
!   output argument list:
!     uvm_ew      -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$


  implicit none

  real(r_kind),dimension(nlon,2,3,nlatm_0:nlatm_1)    ,intent(  out) :: uvm_ew
  real(r_kind),dimension(2,0:sp_a%jcap,2,3,nlatm_0:nlatm_1),intent(in   ) :: uvm_ewtrans

  integer(i_kind) i,j,k
  real(r_kind) invnlon,invnlon2
  real(r_kind),dimension(2,0:nlon/2,2):: halfwave
  real(r_kind),dimension(50000+4*sp_a%imax)::tmpafft

  invnlon=one/real(nlon,r_kind)
  invnlon2=one/(two*real(nlon,r_kind))
!$omp parallel do  schedule(dynamic,1) private(k,j,i,tmpafft,halfwave)
  do k=nlatm_0,nlatm_1
     tmpafft=sp_a%afft
     do j=1,3
        halfwave(1,0,1)=uvm_ewtrans(1,0,1,j,k)*invnlon
        halfwave(2,0,1)=zero
        halfwave(1,0,2)=uvm_ewtrans(1,0,2,j,k)*invnlon
        halfwave(2,0,2)=zero
        do i=1,sp_a%jcap
           halfwave(1,i,1)=uvm_ewtrans(1,i,1,j,k)*invnlon2
           halfwave(2,i,1)=uvm_ewtrans(2,i,1,j,k)*invnlon2
           halfwave(1,i,2)=uvm_ewtrans(1,i,2,j,k)*invnlon2
           halfwave(2,i,2)=uvm_ewtrans(2,i,2,j,k)*invnlon2
        end do
        do i=sp_a%jcap+1,nlon/2
           halfwave(1,i,1)=zero
           halfwave(2,i,1)=zero
           halfwave(1,i,2)=zero
           halfwave(2,i,2)=zero
        end do
        call spffte(nlon,1+nlon/2,nlon,2,halfwave,uvm_ew(1,1,j,k),1,tmpafft)
     end do
  end do

end subroutine inmi_ew_trans_ad

subroutine inmi_coupler_ew2ns0(mype)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ew2ns0
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!   2010-03-31  treadon - replace specmod jcap with sp_a structure
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) k,kk,m,n,num_per_pe,total_groups,nn,kchk

!   in laying out by zonal wave number/vertical mode, have two types of groupings:

!   1.  jcap odd: 

!     group zonal wave numbers in pairs as follows:

!      (0,(jcap+1)/2)    then    ( m,jcap+1-m)  for m=1,(jcap-1)/2

!   2.  jcap even:

!     0  (+,- mode pair together),  then   (m,jcap+1-m ) for m=1,jcap/2  single modes

  total_groups=(sp_a%jcap+1)*nvmodes_keep
  num_per_pe=total_groups/npe
  if(mod(total_groups,npe)/=0) num_per_pe=num_per_pe+1
  if(mod(total_groups,npe)==0) then
     kchk=npe
  else
     kchk=mod(total_groups,npe)
  end if

  if(mod(sp_a%jcap,2) /= 0) then

!    case  jcap odd:
   
     nn=0
     do k=1,nvmodes_keep
        nn=nn+1
        mmode_list(1,nn)=0
        mmode_list(2,nn)=(sp_a%jcap+1)/2
        mmode_list(3,nn)=k
        mmode_list(4,nn)=k
        mmode_list(5,nn)=-1
        do m=1,(sp_a%jcap-1)/2
           nn=nn+1
           mmode_list(1,nn)=m
           mmode_list(2,nn)=sp_a%jcap+1-m
           mmode_list(3,nn)=k
           mmode_list(4,nn)=k
           mmode_list(5,nn)=-1
        end do
     end do
     do k=1,nvmodes_keep
        nn=nn+1
        mmode_list(1,nn)=0
        mmode_list(2,nn)=(sp_a%jcap+1)/2
        mmode_list(3,nn)=-k
        mmode_list(4,nn)=-k
        mmode_list(5,nn)=-1
        do m=1,(sp_a%jcap-1)/2
           nn=nn+1
           mmode_list(1,nn)=m
           mmode_list(2,nn)=sp_a%jcap+1-m
           mmode_list(3,nn)=-k
           mmode_list(4,nn)=-k
           mmode_list(5,nn)=-1
        end do
     end do

  else

!    case  jcap even:
   
     nn=0
     do k=1,nvmodes_keep
        nn=nn+1
        mmode_list(1,nn)=0
        mmode_list(2,nn)=0
        mmode_list(3,nn)=k
        mmode_list(4,nn)=-k
        mmode_list(5,nn)=-1
        do m=1,sp_a%jcap/2
           nn=nn+1
           mmode_list(1,nn)=m
           mmode_list(2,nn)=sp_a%jcap+1-m
           mmode_list(3,nn)=k
           mmode_list(4,nn)=k
           mmode_list(5,nn)=-1
        end do
     end do
     do k=1,nvmodes_keep
        do m=1,sp_a%jcap/2
           nn=nn+1
           mmode_list(1,nn)=m
           mmode_list(2,nn)=sp_a%jcap+1-m
           mmode_list(3,nn)=-k
           mmode_list(4,nn)=-k
           mmode_list(5,nn)=-1
        end do
     end do

  end if

  m_0=-1
  m_1=-2
  nn=0
  do n=1,npe
     if(n <= kchk) then
        kk=num_per_pe
     else
        kk=num_per_pe-1
     end if
     if(kk >  0) then
        if(mype+1 == n) then
           m_0=nn+1
           m_1=nn+kk
        end if
        do k=1,kk
           nn=nn+1
           mmode_list(5,nn)=n
        end do
     end if
  end do

end subroutine inmi_coupler_ew2ns0

subroutine inmi_coupler_ew2ns1(mype)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ew2ns1
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block
!   2010-03-31  treadon - replace specmod jcap with sp_a structure
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  integer(i_kind),intent(in   )::mype

  integer(i_kind) i,ip12,ipe,j,k,m,nn,m1,m2,ilat,imode,imode1,imode2
  integer(i_kind) mpi_string1,ibad,ibad0,loop
  real(r_kind),dimension(0:sp_a%jcap,-nvmodes_keep:nvmodes_keep)::mmode2_list

  allocate(nsend(npe),nrecv(npe),ndsend(npe+1),ndrecv(npe+1))
  nn=0
                
  mmode2_list=0
  do j=1,(sp_a%jcap+1)*nvmodes_keep
     m1=mmode_list(1,j)
     m2=mmode_list(2,j)
     imode1=mmode_list(3,j)
     imode2=mmode_list(4,j)
     if(imode1 == imode2) then
        if(mmode2_list(m1,imode1) /= 0.or.mmode2_list(m2,imode1) /= 0) then
           if(mype == 0) write(6,*)' problem in inmi_coupler_ew2ns'
           call mpi_finalize(i)
           stop
        end if
        mmode2_list(m1,imode1)=j
        mmode2_list(m2,imode1)=j
     end if
     if(m1 == m2) then
        if(mmode2_list(m1,imode1) /= 0.or.mmode2_list(m1,imode2) /= 0) then
           if(mype == 0) write(6,*)' problem in inmi_coupler_ew2ns'
           call mpi_finalize(i)
           stop
        end if
        mmode2_list(m1,imode1)=j
        mmode2_list(m1,imode2)=j
     end if
  end do
  do imode=-nvmodes_keep,nvmodes_keep
     if(imode == 0) cycle
     do m=0,sp_a%jcap
        if(mmode2_list(m,imode) == 0) then
           if(mype == 0) write(6,*)' problem in inmi_coupler_ew2ns'
           call mpi_finalize(i)
           stop
        end if
     end do
  end do

!  obtain counts of points to send to each pe from this pe

  nsend=0
  do k=nlatm_0,nlatm_1
     imode=mode_list(2,k)
     do m=0,sp_a%jcap
        j=mmode2_list(m,imode)
        ipe=mmode_list(5,j)
        nsend(ipe)=nsend(ipe)+1
        j=mmode2_list(m,-imode)
        ipe=mmode_list(5,j)
        nsend(ipe)=nsend(ipe)+1
     end do
  end do

  ndsend(1)=0
  do i=2,npe+1
     ndsend(i)=ndsend(i-1)+nsend(i-1)
  end do
  nallsend=ndsend(npe+1)
  allocate(info_send(6,nallsend))
  nsend=0
  ibad =0
  do k=nlatm_0,nlatm_1
     ilat=mode_list(1,k)
     do loop=1,2
        imode=mode_list(2,k)
        if(loop == 2) imode=-mode_list(2,k)
        do m=0,sp_a%jcap
           j=mmode2_list(m,imode)
           m1=mmode_list(1,j)
           m2=mmode_list(2,j)
           imode1=mmode_list(3,j)
           imode2=mmode_list(4,j)
           ipe=mmode_list(5,j)
           ip12=0
           if(m1 == m2.and.imode == imode1) ip12=1
           if(m1 == m2.and.imode == imode2) ip12=2
           if(imode1 == imode2.and.m == m1) ip12=1
           if(imode1 == imode2.and.m == m2) ip12=2
           if(ip12 == 0) ibad=ibad+1
           ipe=mmode_list(5,j)
           nsend(ipe)=nsend(ipe)+1
           info_send(1,ndsend(ipe)+nsend(ipe))=k
           info_send(2,ndsend(ipe)+nsend(ipe))=ilat
           info_send(3,ndsend(ipe)+nsend(ipe))=imode
           info_send(4,ndsend(ipe)+nsend(ipe))=m
           info_send(5,ndsend(ipe)+nsend(ipe))=j
           info_send(6,ndsend(ipe)+nsend(ipe))=ip12
        end do
     end do
  end do

  call mpi_allreduce(ibad,ibad0,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
  if(ibad0 >  0) then
     if(mype == 0) write(0,*)' ibad = ',ibad0,'  inconsistency in inmi_coupler_ew2ns1'
     call mpi_finalize(ierror)
     stop
  end if

  call mpi_alltoall(nsend,1,mpi_integer4,nrecv,1,mpi_integer4,mpi_comm_world,ierror)
  ndrecv(1)=0
  do i=2,npe+1
     ndrecv(i)=ndrecv(i-1)+nrecv(i-1)
  end do
  nallrecv=ndrecv(npe+1)
  allocate(info_recv(6,nallrecv))
  call mpi_type_contiguous(6,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send,nsend,ndsend,mpi_string1, &
                     info_recv,nrecv,ndrecv,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine inmi_coupler_ew2ns1

subroutine inmi_coupler_ew2ns(uvm_ewtrans,uvm_ns)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ew2ns
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars and uses
!   2010-03-31  treadon - replace specmod components with sp_a structure
!
!   input argument list:
!     uvm_ewtrans -
!
!   output argument list:
!     uvm_ns      -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  real(r_kind),dimension(2,0:sp_a%jcap,2,3,nlatm_0:nlatm_1),intent(in   ) :: uvm_ewtrans
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1)          ,intent(  out) :: uvm_ns

  integer(i_kind) ip12,j,m,mm,ilat,ilatm,imode,mpi_string1,loop
  real(r_kind),allocatable,dimension(:,:,:)::sendbuf,recvbuf

  allocate(sendbuf(3,2,nallsend))
  do j=1,nallsend
     ilatm=info_send(1,j)
     imode=info_send(3,j)
     m=info_send(4,j)
     loop=1
     if(imode <  0) loop=2
     sendbuf(1,1,j)=uvm_ewtrans(1,m,loop,1,ilatm)
     sendbuf(2,1,j)=uvm_ewtrans(1,m,loop,2,ilatm)
     sendbuf(3,1,j)=uvm_ewtrans(1,m,loop,3,ilatm)
     sendbuf(1,2,j)=uvm_ewtrans(2,m,loop,1,ilatm)
     sendbuf(2,2,j)=uvm_ewtrans(2,m,loop,2,ilatm)
     sendbuf(3,2,j)=uvm_ewtrans(2,m,loop,3,ilatm)
  end do
  allocate(recvbuf(3,2,nallrecv))
  call mpi_type_contiguous(6,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend,ndsend,mpi_string1, &
                     recvbuf,nrecv,ndrecv,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv
     ilat=info_recv(2,j)
     mm=info_recv(5,j)
     ip12=info_recv(6,j)
     uvm_ns(1,1,ilat,ip12,mm)=recvbuf(1,1,j)
     uvm_ns(2,1,ilat,ip12,mm)=recvbuf(2,1,j)
     uvm_ns(3,1,ilat,ip12,mm)=recvbuf(3,1,j)
     uvm_ns(1,2,ilat,ip12,mm)=recvbuf(1,2,j)
     uvm_ns(2,2,ilat,ip12,mm)=recvbuf(2,2,j)
     uvm_ns(3,2,ilat,ip12,mm)=recvbuf(3,2,j)
  end do
  deallocate(recvbuf)

end subroutine inmi_coupler_ew2ns

subroutine inmi_coupler_ns2ew(uvm_ewtrans,uvm_ns)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ns2ew
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars and uses
!   2010-03-31  treadon - replace specmod jcap with sp_a structure
!
!   input argument list:
!     uvm_ns   -
!
!   output argument list:
!     uvm_ewtrans -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  real(r_kind),dimension(2,0:sp_a%jcap,2,3,nlatm_0:nlatm_1),intent(  out) :: uvm_ewtrans
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1)          ,intent(in   ) :: uvm_ns

  integer(i_kind) ip12,j,m,mm,ilat,ilatm,imode,mpi_string1,loop
  real(r_kind),allocatable,dimension(:,:,:)::sendbuf,recvbuf


  allocate(recvbuf(3,2,nallrecv))
  do j=1,nallrecv
     ilat=info_recv(2,j)
     mm=info_recv(5,j)
     ip12=info_recv(6,j)
     recvbuf(1,1,j)=uvm_ns(1,1,ilat,ip12,mm)
     recvbuf(2,1,j)=uvm_ns(2,1,ilat,ip12,mm)
     recvbuf(3,1,j)=uvm_ns(3,1,ilat,ip12,mm)
     recvbuf(1,2,j)=uvm_ns(1,2,ilat,ip12,mm)
     recvbuf(2,2,j)=uvm_ns(2,2,ilat,ip12,mm)
     recvbuf(3,2,j)=uvm_ns(3,2,ilat,ip12,mm)
  end do
  allocate(sendbuf(3,2,nallsend))
  call mpi_type_contiguous(6,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(recvbuf,nrecv,ndrecv,mpi_string1, &
                     sendbuf,nsend,ndsend,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(recvbuf)
  do j=1,nallsend
     ilatm=info_send(1,j)
     imode=info_send(3,j)
     m=info_send(4,j)
     loop=1
     if(imode <  0) loop=2
     uvm_ewtrans(1,m,loop,1,ilatm)=sendbuf(1,1,j)
     uvm_ewtrans(1,m,loop,2,ilatm)=sendbuf(2,1,j)
     uvm_ewtrans(1,m,loop,3,ilatm)=sendbuf(3,1,j)
     uvm_ewtrans(2,m,loop,1,ilatm)=sendbuf(1,2,j)
     uvm_ewtrans(2,m,loop,2,ilatm)=sendbuf(2,2,j)
     uvm_ewtrans(2,m,loop,3,ilatm)=sendbuf(3,2,j)
  end do
  deallocate(sendbuf)

end subroutine inmi_coupler_ns2ew

subroutine inmi_nsuvm2zdm(uvm_ns,zdm_hat)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_nsuv2zdm
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!   2010-03-31  treadon - replace specmod components with sp_a structure
!   2012-11-23  parrish - replace calls to spanaly_ns with inline code
!
!   input argument list:
!     uvm_ns   -
!
!   output argument list:
!     zdm_had  -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in   ) :: uvm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(  out) :: zdm_hat

  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind),dimension(2,0:sp_a%jcap):: spcz,spcd,spcp
  real(r_kind),dimension(2,0:sp_a%jcap+1):: spcu,spcv
  real(r_kind),dimension(0:sp_a%jcap+1):: plnloc
  real(r_kind),dimension(2,2):: fu,fv,fp
  real(r_kind) f11u,f21u,f12u,f22u
  real(r_kind) f11v,f21v,f12v,f22v
  real(r_kind) f11p,f21p,f12p,f22p
  real(r_kind):: c1,c2

!$omp parallel do  schedule(dynamic,1) private(mm,ipair,m,ics,i,n) &
!$omp private(spcz,spcd,spcp,spcu,spcv,j,jnorth,jsouth,plnloc,fu,fv,fp,c1,c2) &
!$omp private(f11u,f21u,f12u,f22u,f11v,f21v,f12v,f22v,f11p,f21p,f12p,f22p)
  do mm=m_0,m_1
     do ipair=1,2
        m=mmode_list(ipair,mm)
        ics=1+m*(2*sp_a%jcap+3-m)/2-m

        do n=m,sp_a%jcap
           spcp(1,n)=zero
           spcp(2,n)=zero
           spcu(1,n)=zero
           spcu(2,n)=zero
           spcv(1,n)=zero
           spcv(2,n)=zero
        end do
        spcu(1,sp_a%jcap+1)=zero
        spcu(2,sp_a%jcap+1)=zero
        spcv(1,sp_a%jcap+1)=zero
        spcv(2,sp_a%jcap+1)=zero

        do j=sp_a%jb,sp_a%je
           jsouth=1+j
           jnorth=nlat-j

           c1=sp_a%wlat(j)
           c2=c1/sp_a%clat(j)
           fu(1,1)=uvm_ns(1,1,jnorth,ipair,mm)*c2
           fu(2,1)=uvm_ns(1,2,jnorth,ipair,mm)*c2
           fu(1,2)=uvm_ns(1,1,jsouth,ipair,mm)*c2
           fu(2,2)=uvm_ns(1,2,jsouth,ipair,mm)*c2
           fv(1,1)=uvm_ns(2,1,jnorth,ipair,mm)*c2
           fv(2,1)=uvm_ns(2,2,jnorth,ipair,mm)*c2
           fv(1,2)=uvm_ns(2,1,jsouth,ipair,mm)*c2
           fv(2,2)=uvm_ns(2,2,jsouth,ipair,mm)*c2
           fp(1,1)=uvm_ns(3,1,jnorth,ipair,mm)*c1
           fp(2,1)=uvm_ns(3,2,jnorth,ipair,mm)*c1
           fp(1,2)=uvm_ns(3,1,jsouth,ipair,mm)*c1
           fp(2,2)=uvm_ns(3,2,jsouth,ipair,mm)*c1
!           create plnloc

   
           do n=m,sp_a%jcap
              plnloc(n)=sp_a%pln(ics+n,j)
           end do
           plnloc(sp_a%jcap+1)=sp_a%plntop(m+1,j)
 
           f11u=fu(1,1)+fu(1,2)
           f21u=fu(2,1)+fu(2,2)
           f12u=fu(1,1)-fu(1,2)
           f22u=fu(2,1)-fu(2,2)
           f11v=fv(1,1)+fv(1,2)
           f21v=fv(2,1)+fv(2,2)
           f12v=fv(1,1)-fv(1,2)
           f22v=fv(2,1)-fv(2,2)
           f11p=fp(1,1)+fp(1,2)
           f21p=fp(2,1)+fp(2,2)
           f12p=fp(1,1)-fp(1,2)
           f22p=fp(2,1)-fp(2,2)
           do n=m,sp_a%jcap+1,2
              spcu(1,n)=spcu(1,n)+plnloc(n)*f11u
              spcu(2,n)=spcu(2,n)+plnloc(n)*f21u
              spcv(1,n)=spcv(1,n)+plnloc(n)*f11v
              spcv(2,n)=spcv(2,n)+plnloc(n)*f21v
           end do
           do n=m+1,sp_a%jcap+1,2
              spcu(1,n)=spcu(1,n)+plnloc(n)*f12u
              spcu(2,n)=spcu(2,n)+plnloc(n)*f22u
              spcv(1,n)=spcv(1,n)+plnloc(n)*f12v
              spcv(2,n)=spcv(2,n)+plnloc(n)*f22v
           end do
           do n=m,sp_a%jcap,2
              spcp(1,n)=spcp(1,n)+plnloc(n)*f11p
              spcp(2,n)=spcp(2,n)+plnloc(n)*f21p
           end do
           do n=m+1,sp_a%jcap,2
              spcp(1,n)=spcp(1,n)+plnloc(n)*f12p
              spcp(2,n)=spcp(2,n)+plnloc(n)*f22p
           end do

        end do

        call spuv2dz_ns(sp_a%jcap,m,ics, &
                spcu(1,m),spcv(1,m),spcu(1,sp_a%jcap+1),spcv(1,sp_a%jcap+1),spcd(1,m),spcz(1,m))

        i=0
        do n=m,sp_a%jcap
           i=i+1
           zdm_hat(1,1,i,ipair,mm)=spcz(1,n)*sp_a%enn1(ics+n)
           zdm_hat(1,2,i,ipair,mm)=spcz(2,n)*sp_a%enn1(ics+n)
           zdm_hat(2,1,i,ipair,mm)=spcd(1,n)*sp_a%enn1(ics+n)
           zdm_hat(2,2,i,ipair,mm)=spcd(2,n)*sp_a%enn1(ics+n)
           zdm_hat(3,1,i,ipair,mm)=spcp(1,n)
           zdm_hat(3,2,i,ipair,mm)=spcp(2,n)
        end do

     end do

  end do

end subroutine inmi_nsuvm2zdm

subroutine inmi_nszdm2uvm_ad(uvm_ns,zdm_hat)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_nszdm2uvm_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!   2010-03-31  treadon - replace specmod components with sp_a structure
!   2012-11-23  parrish - replace calls to spanaly_ns with inline code
!
!   input argument list:
!     uvm_ns   -
!
!   output argument list:
!     zdm_had  -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in   ) :: uvm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(  out) :: zdm_hat

  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind),dimension(3,2,nlat)::uvm_ns_temp
  real(r_kind),dimension(2,0:sp_a%jcap):: spcz,spcd,spcp
  real(r_kind),dimension(2,0:sp_a%jcap+1):: spcu,spcv
  real(r_kind),dimension(0:sp_a%jcap+1):: plnloc
  real(r_kind),dimension(2,2):: fu,fv,fp
  real(r_kind) f11u,f21u,f12u,f22u
  real(r_kind) f11v,f21v,f12v,f22v
  real(r_kind) f11p,f21p,f12p,f22p
  real(r_kind):: c1

!$omp parallel do  schedule(dynamic,1) private(mm,ipair,m,ics,i,n) &
!$omp private(spcz,spcd,spcp,spcu,spcv,j,jnorth,jsouth,plnloc,fu,fv,fp,c1,uvm_ns_temp) &
!$omp private(f11u,f21u,f12u,f22u,f11v,f21v,f12v,f22v,f11p,f21p,f12p,f22p)
  do mm=m_0,m_1
     do ipair=1,2
        m=mmode_list(ipair,mm)
        ics=1+m*(2*sp_a%jcap+3-m)/2-m

        do n=m,sp_a%jcap
           spcp(1,n)=zero
           spcp(2,n)=zero
           spcu(1,n)=zero
           spcu(2,n)=zero
           spcv(1,n)=zero
           spcv(2,n)=zero
        end do
        spcu(1,sp_a%jcap+1)=zero
        spcu(2,sp_a%jcap+1)=zero
        spcv(1,sp_a%jcap+1)=zero
        spcv(2,sp_a%jcap+1)=zero

!  adjoint of set pole values
        uvm_ns_temp(:,:,:)=uvm_ns(:,:,:,ipair,mm)
        if(m == 0) then
           uvm_ns_temp(3,1,     2)=uvm_ns_temp(3,1,   1)+uvm_ns_temp(3,1,     2)
           uvm_ns_temp(3,1,nlat-1)=uvm_ns_temp(3,1,nlat)+uvm_ns_temp(3,1,nlat-1)
        else if(m == 1) then
           uvm_ns_temp(1,1,     2)=uvm_ns_temp(1,1,   1)+uvm_ns_temp(1,1,     2)
           uvm_ns_temp(1,2,     2)=uvm_ns_temp(1,2,   1)+uvm_ns_temp(1,2,     2)
           uvm_ns_temp(2,1,     2)=uvm_ns_temp(2,1,   1)+uvm_ns_temp(2,1,     2)
           uvm_ns_temp(2,2,     2)=uvm_ns_temp(2,2,   1)+uvm_ns_temp(2,2,     2)
           uvm_ns_temp(1,1,nlat-1)=uvm_ns_temp(1,1,nlat)+uvm_ns_temp(1,1,nlat-1)
           uvm_ns_temp(1,2,nlat-1)=uvm_ns_temp(1,2,nlat)+uvm_ns_temp(1,2,nlat-1)
           uvm_ns_temp(2,1,nlat-1)=uvm_ns_temp(2,1,nlat)+uvm_ns_temp(2,1,nlat-1)
           uvm_ns_temp(2,2,nlat-1)=uvm_ns_temp(2,2,nlat)+uvm_ns_temp(2,2,nlat-1)
        end if

        do j=sp_a%jb,sp_a%je
           jsouth=1+j
           jnorth=nlat-j

           c1=one/sp_a%clat(j)
           fu(1,1)=uvm_ns_temp(1,1,jnorth)*c1
           fu(2,1)=uvm_ns_temp(1,2,jnorth)*c1
           fu(1,2)=uvm_ns_temp(1,1,jsouth)*c1
           fu(2,2)=uvm_ns_temp(1,2,jsouth)*c1
           fv(1,1)=uvm_ns_temp(2,1,jnorth)*c1
           fv(2,1)=uvm_ns_temp(2,2,jnorth)*c1
           fv(1,2)=uvm_ns_temp(2,1,jsouth)*c1
           fv(2,2)=uvm_ns_temp(2,2,jsouth)*c1
           fp(1,1)=uvm_ns_temp(3,1,jnorth)
           fp(2,1)=uvm_ns_temp(3,2,jnorth)
           fp(1,2)=uvm_ns_temp(3,1,jsouth)
           fp(2,2)=uvm_ns_temp(3,2,jsouth)

!           create plnloc

   
           do n=m,sp_a%jcap
              plnloc(n)=sp_a%pln(ics+n,j)
           end do
           plnloc(sp_a%jcap+1)=sp_a%plntop(m+1,j)

           f11u=fu(1,1)+fu(1,2)
           f21u=fu(2,1)+fu(2,2)
           f12u=fu(1,1)-fu(1,2)
           f22u=fu(2,1)-fu(2,2)
           f11v=fv(1,1)+fv(1,2)
           f21v=fv(2,1)+fv(2,2)
           f12v=fv(1,1)-fv(1,2)
           f22v=fv(2,1)-fv(2,2)
           f11p=fp(1,1)+fp(1,2)
           f21p=fp(2,1)+fp(2,2)
           f12p=fp(1,1)-fp(1,2)
           f22p=fp(2,1)-fp(2,2)
           do n=m,sp_a%jcap+1,2
              spcu(1,n)=spcu(1,n)+plnloc(n)*f11u
              spcu(2,n)=spcu(2,n)+plnloc(n)*f21u
              spcv(1,n)=spcv(1,n)+plnloc(n)*f11v
              spcv(2,n)=spcv(2,n)+plnloc(n)*f21v
           end do
           do n=m+1,sp_a%jcap+1,2
              spcu(1,n)=spcu(1,n)+plnloc(n)*f12u
              spcu(2,n)=spcu(2,n)+plnloc(n)*f22u
              spcv(1,n)=spcv(1,n)+plnloc(n)*f12v
              spcv(2,n)=spcv(2,n)+plnloc(n)*f22v
           end do
           do n=m,sp_a%jcap,2
              spcp(1,n)=spcp(1,n)+plnloc(n)*f11p
              spcp(2,n)=spcp(2,n)+plnloc(n)*f21p
           end do
           do n=m+1,sp_a%jcap,2
              spcp(1,n)=spcp(1,n)+plnloc(n)*f12p
              spcp(2,n)=spcp(2,n)+plnloc(n)*f22p
           end do

        end do

        call spuv2dz_ns(sp_a%jcap,m,ics, &
                spcu(1,m),spcv(1,m),spcu(1,sp_a%jcap+1),spcv(1,sp_a%jcap+1),spcd(1,m),spcz(1,m))

        i=0
        if(m == 0) then
           i=i+1
           zdm_hat(1,1,i,ipair,mm)=zero
           zdm_hat(1,2,i,ipair,mm)=zero
           zdm_hat(2,1,i,ipair,mm)=zero
           zdm_hat(2,2,i,ipair,mm)=zero
           zdm_hat(3,1,i,ipair,mm)=spcp(1,0)
           zdm_hat(3,2,i,ipair,mm)=spcp(2,0)
        end if
        do n=max(1,m),sp_a%jcap
           i=i+1
           zdm_hat(1,1,i,ipair,mm)=spcz(1,n)
           zdm_hat(1,2,i,ipair,mm)=spcz(2,n)
           zdm_hat(2,1,i,ipair,mm)=spcd(1,n)
           zdm_hat(2,2,i,ipair,mm)=spcd(2,n)
           zdm_hat(3,1,i,ipair,mm)=spcp(1,n)
           zdm_hat(3,2,i,ipair,mm)=spcp(2,n)
        end do

     end do

  end do

end subroutine inmi_nszdm2uvm_ad

subroutine inmi_nszdm2uvm(uvm_ns,zdm_hat)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_szdm2uvm
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!   2010-03-31  treadon - replace specmod components with sp_a structure
!   2012-11-23  parrish - replace calls to spsynth_ns with inline code
!
!   input argument list:
!     uvm_ns   -
!
!   output argument list:
!     zdm_had  -
!
! attributes:
!   language:  f90
!   machine:   ibm rs/6000 sp
!
!$$$

  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(  out) :: uvm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in   ) :: zdm_hat

  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind),dimension(2,0:sp_a%jcap):: spcz,spcd,spcp
  real(r_kind),dimension(2,0:sp_a%jcap+1):: spcu,spcv
  real(r_kind),dimension(0:sp_a%jcap+1):: plnloc
  real(r_kind),dimension(2,2):: fu,fv,fp
  real(r_kind) f1ur,f1ui,f2ur,f2ui
  real(r_kind) f1vr,f1vi,f2vr,f2vi
  real(r_kind) f1pr,f1pi,f2pr,f2pi

  real(r_kind):: c1

!$omp parallel do  schedule(dynamic,1) private(mm,ipair,m,ics,i,n) &
!$omp private(spcz,spcd,spcp,spcu,spcv,j,jnorth,jsouth,plnloc,fu,fv,fp,c1) &
!$omp private(f1ur,f1ui,f2ur,f2ui,f1vr,f1vi,f2vr,f2vi,f1pr,f1pi,f2pr,f2pi)
  do mm=m_0,m_1
     do ipair=1,2
        m=mmode_list(ipair,mm)
        ics=1+m*(2*sp_a%jcap+3-m)/2-m

!           gather up spcz, spcd, spcp

        i=0
        do n=m,sp_a%jcap
           i=i+1
           spcz(1,n)=zdm_hat(1,1,i,ipair,mm)
           spcz(2,n)=zdm_hat(1,2,i,ipair,mm)
           spcd(1,n)=zdm_hat(2,1,i,ipair,mm)
           spcd(2,n)=zdm_hat(2,2,i,ipair,mm)
           spcp(1,n)=zdm_hat(3,1,i,ipair,mm)
           spcp(2,n)=zdm_hat(3,2,i,ipair,mm)
        end do

!           convert to spcu, spcv

        call spdz2uv_ns(sp_a%jcap,m,ics, &
                spcd(1,m),spcz(1,m),spcu(1,m),spcv(1,m),spcu(1,sp_a%jcap+1),spcv(1,sp_a%jcap+1))

        do j=sp_a%jb,sp_a%je
           jsouth=1+j
           jnorth=nlat-j

!           create plnloc

           do n=m,sp_a%jcap
              plnloc(n)=sp_a%pln(ics+n,j)
           end do
           plnloc(sp_a%jcap+1)=sp_a%plntop(m+1,j)
 
!          obtain f

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  zero out fourier coefficients.
           f1ur=zero
           f1ui=zero
           f2ur=zero
           f2ui=zero
           f1vr=zero
           f1vi=zero
           f2vr=zero
           f2vi=zero
           f1pr=zero
           f1pi=zero
           f2pr=zero
           f2pi=zero
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  synthesis over finite latitude.
!  for each zonal wavenumber, synthesize terms over total wavenumber.
!  synthesize even and odd polynomials separately.
           do n=m,sp_a%jcap+1,2
              f1ur=f1ur+plnloc(n)*spcu(1,n)
              f1ui=f1ui+plnloc(n)*spcu(2,n)
              f1vr=f1vr+plnloc(n)*spcv(1,n)
              f1vi=f1vi+plnloc(n)*spcv(2,n)
           enddo
           do n=m+1,sp_a%jcap+1,2
              f2ur=f2ur+plnloc(n)*spcu(1,n)
              f2ui=f2ui+plnloc(n)*spcu(2,n)
              f2vr=f2vr+plnloc(n)*spcv(1,n)
              f2vi=f2vi+plnloc(n)*spcv(2,n)
           enddo
           do n=m,sp_a%jcap,2
              f1pr=f1pr+plnloc(n)*spcp(1,n)
              f1pi=f1pi+plnloc(n)*spcp(2,n)
           enddo
           do n=m+1,sp_a%jcap,2
              f2pr=f2pr+plnloc(n)*spcp(1,n)
              f2pi=f2pi+plnloc(n)*spcp(2,n)
           enddo
!  separate fourier coefficients from each hemisphere.
!  odd polynomials contribute negatively to the southern hemisphere.
           fu(1,1)=f1ur+f2ur
           fu(2,1)=f1ui+f2ui
           fu(1,2)=f1ur-f2ur
           fu(2,2)=f1ui-f2ui
           fv(1,1)=f1vr+f2vr
           fv(2,1)=f1vi+f2vi
           fv(1,2)=f1vr-f2vr
           fv(2,2)=f1vi-f2vi
           fp(1,1)=f1pr+f2pr
           fp(2,1)=f1pi+f2pi
           fp(1,2)=f1pr-f2pr
           fp(2,2)=f1pi-f2pi
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!          scatter back to output pairs of lats
 
           c1=one/sp_a%clat(j)
           uvm_ns(1,1,jnorth,ipair,mm)=fu(1,1)*c1
           uvm_ns(1,2,jnorth,ipair,mm)=fu(2,1)*c1
           uvm_ns(1,1,jsouth,ipair,mm)=fu(1,2)*c1
           uvm_ns(1,2,jsouth,ipair,mm)=fu(2,2)*c1
           uvm_ns(2,1,jnorth,ipair,mm)=fv(1,1)*c1
           uvm_ns(2,2,jnorth,ipair,mm)=fv(2,1)*c1
           uvm_ns(2,1,jsouth,ipair,mm)=fv(1,2)*c1
           uvm_ns(2,2,jsouth,ipair,mm)=fv(2,2)*c1
           uvm_ns(3,1,jnorth,ipair,mm)=fp(1,1)
           uvm_ns(3,2,jnorth,ipair,mm)=fp(2,1)
           uvm_ns(3,1,jsouth,ipair,mm)=fp(1,2)
           uvm_ns(3,2,jsouth,ipair,mm)=fp(2,2)

        end do

!  set pole values
        if(m == 0) then
           uvm_ns(1,1,1,ipair,mm)=zero
           uvm_ns(1,2,1,ipair,mm)=zero
           uvm_ns(2,1,1,ipair,mm)=zero
           uvm_ns(2,2,1,ipair,mm)=zero
           uvm_ns(3,1,1,ipair,mm)=uvm_ns(3,1,2,ipair,mm)
           uvm_ns(3,2,1,ipair,mm)=zero
           uvm_ns(1,1,nlat,ipair,mm)=zero
           uvm_ns(1,2,nlat,ipair,mm)=zero
           uvm_ns(2,1,nlat,ipair,mm)=zero
           uvm_ns(2,2,nlat,ipair,mm)=zero
           uvm_ns(3,1,nlat,ipair,mm)=uvm_ns(3,1,nlat-1,ipair,mm)
           uvm_ns(3,2,nlat,ipair,mm)=zero
        else if(m == 1) then
           uvm_ns(1,1,1,ipair,mm)=uvm_ns(1,1,2,ipair,mm)
           uvm_ns(1,2,1,ipair,mm)=uvm_ns(1,2,2,ipair,mm)
           uvm_ns(2,1,1,ipair,mm)=uvm_ns(2,1,2,ipair,mm)
           uvm_ns(2,2,1,ipair,mm)=uvm_ns(2,2,2,ipair,mm)
           uvm_ns(3,1,1,ipair,mm)=zero
           uvm_ns(3,2,1,ipair,mm)=zero
           uvm_ns(1,1,nlat,ipair,mm)=uvm_ns(1,1,nlat-1,ipair,mm)
           uvm_ns(1,2,nlat,ipair,mm)=uvm_ns(1,2,nlat-1,ipair,mm)
           uvm_ns(2,1,nlat,ipair,mm)=uvm_ns(2,1,nlat-1,ipair,mm)
           uvm_ns(2,2,nlat,ipair,mm)=uvm_ns(2,2,nlat-1,ipair,mm)
           uvm_ns(3,1,nlat,ipair,mm)=zero
           uvm_ns(3,2,nlat,ipair,mm)=zero
        else
           uvm_ns(1,1,1,ipair,mm)=zero
           uvm_ns(1,2,1,ipair,mm)=zero
           uvm_ns(2,1,1,ipair,mm)=zero
           uvm_ns(2,2,1,ipair,mm)=zero
           uvm_ns(3,1,1,ipair,mm)=zero
           uvm_ns(3,2,1,ipair,mm)=zero
           uvm_ns(1,1,nlat,ipair,mm)=zero
           uvm_ns(1,2,nlat,ipair,mm)=zero
           uvm_ns(2,1,nlat,ipair,mm)=zero
           uvm_ns(2,2,nlat,ipair,mm)=zero
           uvm_ns(3,1,nlat,ipair,mm)=zero
           uvm_ns(3,2,nlat,ipair,mm)=zero
        end if

     end do

  end do

end subroutine inmi_nszdm2uvm

subroutine inmi_nspcm_hat2pcm(pcm_ns,pcm_hat)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_nspcm_hat2pcm
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-13  lueken - added subprogram doc block
!   2010-03-31  treadon - replace specmod components with sp_a structure
!   2012-11-23  parrish - replace calls to spsynth_ns with inline code
!
!   input argument list:
!    pcm_hat
!
!   output argument list:
!    pcm_ns
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(  out) :: pcm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in   ) :: pcm_hat

  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind),dimension(2,0:sp_a%jcap):: spcp,spcc,spcm
  real(r_kind),dimension(0:sp_a%jcap+1):: plnloc
  real(r_kind),dimension(2,2):: fp,fc,fm
  real(r_kind) f1pr,f1pi,f2pr,f2pi
  real(r_kind) f1cr,f1ci,f2cr,f2ci
  real(r_kind) f1mr,f1mi,f2mr,f2mi

!$omp parallel do  schedule(dynamic,1) private(mm,ipair,m,ics,i,n) &
!$omp private(spcc,spcm,spcp,j,jnorth,jsouth,plnloc,fc,fm,fp) &
!$omp private(f1pr,f1pi,f2pr,f2pi,f1cr,f1ci,f2cr,f2ci,f1mr,f1mi,f2mr,f2mi)
  do mm=m_0,m_1
     do ipair=1,2
        m=mmode_list(ipair,mm)
        ics=1+m*(2*sp_a%jcap+3-m)/2-m

!           gather up spcp, spcc, spcm

        i=0
        do n=m,sp_a%jcap
           i=i+1
           spcp(1,n)=pcm_hat(1,1,i,ipair,mm)
           spcp(2,n)=pcm_hat(1,2,i,ipair,mm)
           spcc(1,n)=pcm_hat(2,1,i,ipair,mm)
           spcc(2,n)=pcm_hat(2,2,i,ipair,mm)
           spcm(1,n)=pcm_hat(3,1,i,ipair,mm)
           spcm(2,n)=pcm_hat(3,2,i,ipair,mm)
        end do

        do j=sp_a%jb,sp_a%je
           jsouth=1+j
           jnorth=nlat-j

!           create plnloc

           do n=m,sp_a%jcap
              plnloc(n)=sp_a%pln(ics+n,j)
           end do

!          obtain f

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  zero out fourier coefficients.
           f1pr=zero
           f1pi=zero
           f2pr=zero
           f2pi=zero
           f1cr=zero
           f1ci=zero
           f2cr=zero
           f2ci=zero
           f1mr=zero
           f1mi=zero
           f2mr=zero
           f2mi=zero
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  synthesis over finite latitude.
!  for each zonal wavenumber, synthesize terms over total wavenumber.
!  synthesize even and odd polynomials separately.
           do n=m,sp_a%jcap,2
              f1pr=f1pr+plnloc(n)*spcp(1,n)
              f1pi=f1pi+plnloc(n)*spcp(2,n)
              f1cr=f1cr+plnloc(n)*spcc(1,n)
              f1ci=f1ci+plnloc(n)*spcc(2,n)
              f1mr=f1mr+plnloc(n)*spcm(1,n)
              f1mi=f1mi+plnloc(n)*spcm(2,n)
           enddo
           do n=m+1,sp_a%jcap,2
              f2pr=f2pr+plnloc(n)*spcp(1,n)
              f2pi=f2pi+plnloc(n)*spcp(2,n)
              f2cr=f2cr+plnloc(n)*spcc(1,n)
              f2ci=f2ci+plnloc(n)*spcc(2,n)
              f2mr=f2mr+plnloc(n)*spcm(1,n)
              f2mi=f2mi+plnloc(n)*spcm(2,n)
           enddo
!  separate fourier coefficients from each hemisphere.
!  odd polynomials contribute negatively to the southern hemisphere.
           fp(1,1)=f1pr+f2pr
           fp(2,1)=f1pi+f2pi
           fp(1,2)=f1pr-f2pr
           fp(2,2)=f1pi-f2pi
           fc(1,1)=f1cr+f2cr
           fc(2,1)=f1ci+f2ci
           fc(1,2)=f1cr-f2cr
           fc(2,2)=f1ci-f2ci
           fm(1,1)=f1mr+f2mr
           fm(2,1)=f1mi+f2mi
           fm(1,2)=f1mr-f2mr
           fm(2,2)=f1mi-f2mi

!          scatter back to output pairs of lats

           pcm_ns(1,1,jnorth,ipair,mm)=fp(1,1)
           pcm_ns(1,2,jnorth,ipair,mm)=fp(2,1)
           pcm_ns(1,1,jsouth,ipair,mm)=fp(1,2)
           pcm_ns(1,2,jsouth,ipair,mm)=fp(2,2)
           pcm_ns(2,1,jnorth,ipair,mm)=fc(1,1)
           pcm_ns(2,2,jnorth,ipair,mm)=fc(2,1)
           pcm_ns(2,1,jsouth,ipair,mm)=fc(1,2)
           pcm_ns(2,2,jsouth,ipair,mm)=fc(2,2)
           pcm_ns(3,1,jnorth,ipair,mm)=fm(1,1)
           pcm_ns(3,2,jnorth,ipair,mm)=fm(2,1)
           pcm_ns(3,1,jsouth,ipair,mm)=fm(1,2)
           pcm_ns(3,2,jsouth,ipair,mm)=fm(2,2)
 
        end do

!  set pole values
        if(m == 0) then
           pcm_ns(1,1,1,ipair,mm)=pcm_ns(1,1,2,ipair,mm)
           pcm_ns(1,2,1,ipair,mm)=zero
           pcm_ns(1,1,nlat,ipair,mm)=pcm_ns(1,1,nlat-1,ipair,mm)
           pcm_ns(1,2,nlat,ipair,mm)=zero
           pcm_ns(2,1,1,ipair,mm)=pcm_ns(2,1,2,ipair,mm)
           pcm_ns(2,2,1,ipair,mm)=zero
           pcm_ns(2,1,nlat,ipair,mm)=pcm_ns(2,1,nlat-1,ipair,mm)
           pcm_ns(2,2,nlat,ipair,mm)=zero
           pcm_ns(3,1,1,ipair,mm)=pcm_ns(3,1,2,ipair,mm)
           pcm_ns(3,2,1,ipair,mm)=zero
           pcm_ns(3,1,nlat,ipair,mm)=pcm_ns(3,1,nlat-1,ipair,mm)
           pcm_ns(3,2,nlat,ipair,mm)=zero
        else
           pcm_ns(1,1,1,ipair,mm)=zero
           pcm_ns(1,2,1,ipair,mm)=zero
           pcm_ns(2,1,1,ipair,mm)=zero
           pcm_ns(2,2,1,ipair,mm)=zero
           pcm_ns(3,1,1,ipair,mm)=zero
           pcm_ns(3,2,1,ipair,mm)=zero
           pcm_ns(1,1,nlat,ipair,mm)=zero
           pcm_ns(1,2,nlat,ipair,mm)=zero
           pcm_ns(2,1,nlat,ipair,mm)=zero
           pcm_ns(2,2,nlat,ipair,mm)=zero
           pcm_ns(3,1,nlat,ipair,mm)=zero
           pcm_ns(3,2,nlat,ipair,mm)=zero
        end if

     end do

  end do

end subroutine inmi_nspcm_hat2pcm

subroutine inmi_nspcm_hat2pcm_ad(pcm_ns,pcm_hat)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_nspcm_hat2pcm_ad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-13  lueken - added subprogram doc block
!   2010-03-31  treadon - replace specmod components with sp_a structure
!   2012-11-23  parrish - replace calls to spanaly_ns with inline code
!
!   input argument list:
!    pcm_ns
!
!   output argument list:
!    pcm_hat
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in   ) :: pcm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(  out) :: pcm_hat

  real(r_kind),dimension(3,2,nlat)::pcm_ns_temp
  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind),dimension(2,0:sp_a%jcap):: spcp,spcc,spcm
  real(r_kind),dimension(0:sp_a%jcap+1):: plnloc
  real(r_kind),dimension(2,2):: fp,fc,fm
  real(r_kind) f11p,f21p,f12p,f22p
  real(r_kind) f11c,f21c,f12c,f22c
  real(r_kind) f11m,f21m,f12m,f22m

  pcm_hat=zero
!$omp parallel do  schedule(dynamic,1) private(mm,ipair,m,ics,i,n) &
!$omp private(spcc,spcm,spcp,j,jnorth,jsouth,plnloc,fc,fm,fp,pcm_ns_temp) &
!$omp private(f11p,f21p,f12p,f22p,f11c,f21c,f12c,f22c,f11m,f21m,f12m,f22m)
  do mm=m_0,m_1
     do ipair=1,2
        m=mmode_list(ipair,mm)
        ics=1+m*(2*sp_a%jcap+3-m)/2-m
 
        do n=m,sp_a%jcap
           spcp(1,n)=zero
           spcp(2,n)=zero
           spcc(1,n)=zero
           spcc(2,n)=zero
           spcm(1,n)=zero
           spcm(2,n)=zero
        end do

!  adjoint of set pole values

        pcm_ns_temp(:,:,:)=pcm_ns(:,:,:,ipair,mm)
        if(m == 0) then
           pcm_ns_temp(1,1,     2)=pcm_ns_temp(1,1,   1)+pcm_ns_temp(1,1,     2)
           pcm_ns_temp(1,1,nlat-1)=pcm_ns_temp(1,1,nlat)+pcm_ns_temp(1,1,nlat-1)
           pcm_ns_temp(2,1,     2)=pcm_ns_temp(2,1,   1)+pcm_ns_temp(2,1,     2)
           pcm_ns_temp(2,1,nlat-1)=pcm_ns_temp(2,1,nlat)+pcm_ns_temp(2,1,nlat-1)
           pcm_ns_temp(3,1,     2)=pcm_ns_temp(3,1,   1)+pcm_ns_temp(3,1,     2)
           pcm_ns_temp(3,1,nlat-1)=pcm_ns_temp(3,1,nlat)+pcm_ns_temp(3,1,nlat-1)
        end if

        do j=sp_a%jb,sp_a%je
           jsouth=1+j
           jnorth=nlat-j
 
!          adjoint of scatter back to output pairs of lats

           fp(1,1)=pcm_ns_temp(1,1,jnorth)
           fp(2,1)=pcm_ns_temp(1,2,jnorth)
           fp(1,2)=pcm_ns_temp(1,1,jsouth)
           fp(2,2)=pcm_ns_temp(1,2,jsouth)
           fc(1,1)=pcm_ns_temp(2,1,jnorth)
           fc(2,1)=pcm_ns_temp(2,2,jnorth)
           fc(1,2)=pcm_ns_temp(2,1,jsouth)
           fc(2,2)=pcm_ns_temp(2,2,jsouth)
           fm(1,1)=pcm_ns_temp(3,1,jnorth)
           fm(2,1)=pcm_ns_temp(3,2,jnorth)
           fm(1,2)=pcm_ns_temp(3,1,jsouth)
           fm(2,2)=pcm_ns_temp(3,2,jsouth)
 
!           create plnloc

           do n=m,sp_a%jcap
              plnloc(n)=sp_a%pln(ics+n,j)
           end do

!          adjoint of obtain f

           f11p=fp(1,1)+fp(1,2)
           f21p=fp(2,1)+fp(2,2)
           f12p=fp(1,1)-fp(1,2)
           f22p=fp(2,1)-fp(2,2)
           f11c=fc(1,1)+fc(1,2)
           f21c=fc(2,1)+fc(2,2)
           f12c=fc(1,1)-fc(1,2)
           f22c=fc(2,1)-fc(2,2)
           f11m=fm(1,1)+fm(1,2)
           f21m=fm(2,1)+fm(2,2)
           f12m=fm(1,1)-fm(1,2)
           f22m=fm(2,1)-fm(2,2)
           do n=m,sp_a%jcap,2
              spcp(1,n)=spcp(1,n)+plnloc(n)*f11p
              spcp(2,n)=spcp(2,n)+plnloc(n)*f21p
              spcc(1,n)=spcc(1,n)+plnloc(n)*f11c
              spcc(2,n)=spcc(2,n)+plnloc(n)*f21c
              spcm(1,n)=spcm(1,n)+plnloc(n)*f11m
              spcm(2,n)=spcm(2,n)+plnloc(n)*f21m
           end do
           do n=m+1,sp_a%jcap,2
              spcp(1,n)=spcp(1,n)+plnloc(n)*f12p
              spcp(2,n)=spcp(2,n)+plnloc(n)*f22p
              spcc(1,n)=spcc(1,n)+plnloc(n)*f12c
              spcc(2,n)=spcc(2,n)+plnloc(n)*f22c
              spcm(1,n)=spcm(1,n)+plnloc(n)*f12m
              spcm(2,n)=spcm(2,n)+plnloc(n)*f22m
           end do
 
        end do

!       adjoint of gather up spcp, spcc, spcm

        i=0
        do n=m,sp_a%jcap
           i=i+1
           pcm_hat(1,1,i,ipair,mm)=spcp(1,n)
           pcm_hat(1,2,i,ipair,mm)=spcp(2,n)
           pcm_hat(2,1,i,ipair,mm)=spcc(1,n)
           pcm_hat(2,2,i,ipair,mm)=spcc(2,n)
           pcm_hat(3,1,i,ipair,mm)=spcm(1,n)
           pcm_hat(3,2,i,ipair,mm)=spcm(2,n)
        end do

     end do

  end do

end subroutine inmi_nspcm_hat2pcm_ad

subroutine inmi_nsuvm2zdm_ad(uvm_ns,zdm_hat)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_nsuvm2zdm_ad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-13  lueken - added subprogram doc block
!   2010-03-31  treadon - replace specmod components with sp_a structure
!   2012-11-23  parrish - replace calls to spsynth_ns with inline code
!
!   input argument list:
!    uvm_ns
!    zdm_hat
!
!   output argument list:
!    uvm_ns
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(inout) :: uvm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in   ) :: zdm_hat

  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind),dimension(2,0:sp_a%jcap):: spcz,spcd,spcp
  real(r_kind),dimension(2,0:sp_a%jcap+1):: spcu,spcv
  real(r_kind),dimension(0:sp_a%jcap+1):: plnloc
  real(r_kind),dimension(2,2):: fu,fv,fp
  real(r_kind) f1ur,f1ui,f2ur,f2ui
  real(r_kind) f1vr,f1vi,f2vr,f2vi
  real(r_kind) f1pr,f1pi,f2pr,f2pi

  real(r_kind):: c1,c2

!$omp parallel do  schedule(dynamic,1) private(mm,ipair,m,ics,i,n) &
!$omp private(spcz,spcd,spcp,spcu,spcv,j,jnorth,jsouth,plnloc,fu,fv,fp,c1,c2) &
!$omp private(f1ur,f1ui,f2ur,f2ui,f1vr,f1vi,f2vr,f2vi,f1pr,f1pi,f2pr,f2pi)
  do mm=m_0,m_1
     do ipair=1,2
        m=mmode_list(ipair,mm)
        ics=1+m*(2*sp_a%jcap+3-m)/2-m

!           gather up spcz, spcd, spcp

        i=0
        do n=m,sp_a%jcap
           i=i+1
           spcz(1,n)=zdm_hat(1,1,i,ipair,mm)*sp_a%enn1(ics+n)
           spcz(2,n)=zdm_hat(1,2,i,ipair,mm)*sp_a%enn1(ics+n)
           spcd(1,n)=zdm_hat(2,1,i,ipair,mm)*sp_a%enn1(ics+n)
           spcd(2,n)=zdm_hat(2,2,i,ipair,mm)*sp_a%enn1(ics+n)
           spcp(1,n)=zdm_hat(3,1,i,ipair,mm)
           spcp(2,n)=zdm_hat(3,2,i,ipair,mm)
        end do

!           convert to spcu, spcv

        call spdz2uv_ns(sp_a%jcap,m,ics, &
                spcd(1,m),spcz(1,m),spcu(1,m),spcv(1,m),spcu(1,sp_a%jcap+1),spcv(1,sp_a%jcap+1))

        do j=sp_a%jb,sp_a%je
           jsouth=1+j
           jnorth=nlat-j
 
!           create plnloc

           do n=m,sp_a%jcap
              plnloc(n)=sp_a%pln(ics+n,j)
           end do
           plnloc(sp_a%jcap+1)=sp_a%plntop(m+1,j)

!          obtain f

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  zero out fourier coefficients.
           f1ur=zero
           f1ui=zero
           f2ur=zero
           f2ui=zero
           f1vr=zero
           f1vi=zero
           f2vr=zero
           f2vi=zero
           f1pr=zero
           f1pi=zero
           f2pr=zero
           f2pi=zero
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  synthesis over finite latitude.
!  for each zonal wavenumber, synthesize terms over total wavenumber.
!  synthesize even and odd polynomials separately.
           do n=m,sp_a%jcap+1,2
              f1ur=f1ur+plnloc(n)*spcu(1,n)
              f1ui=f1ui+plnloc(n)*spcu(2,n)
              f1vr=f1vr+plnloc(n)*spcv(1,n)
              f1vi=f1vi+plnloc(n)*spcv(2,n)
           enddo
           do n=m+1,sp_a%jcap+1,2
              f2ur=f2ur+plnloc(n)*spcu(1,n)
              f2ui=f2ui+plnloc(n)*spcu(2,n)
              f2vr=f2vr+plnloc(n)*spcv(1,n)
              f2vi=f2vi+plnloc(n)*spcv(2,n)
           enddo
           do n=m,sp_a%jcap,2
              f1pr=f1pr+plnloc(n)*spcp(1,n)
              f1pi=f1pi+plnloc(n)*spcp(2,n)
           enddo
           do n=m+1,sp_a%jcap,2
              f2pr=f2pr+plnloc(n)*spcp(1,n)
              f2pi=f2pi+plnloc(n)*spcp(2,n)
           enddo
!  separate fourier coefficients from each hemisphere.
!  odd polynomials contribute negatively to the southern hemisphere.
           fu(1,1)=f1ur+f2ur
           fu(2,1)=f1ui+f2ui
           fu(1,2)=f1ur-f2ur
           fu(2,2)=f1ui-f2ui
           fv(1,1)=f1vr+f2vr
           fv(2,1)=f1vi+f2vi
           fv(1,2)=f1vr-f2vr
           fv(2,2)=f1vi-f2vi
           fp(1,1)=f1pr+f2pr
           fp(2,1)=f1pi+f2pi
           fp(1,2)=f1pr-f2pr
           fp(2,2)=f1pi-f2pi

!          scatter back to output pairs of lats

           c1=sp_a%wlat(j)
           c2=c1/sp_a%clat(j)
           uvm_ns(1,1,jnorth,ipair,mm)=fu(1,1)*c2
           uvm_ns(1,2,jnorth,ipair,mm)=fu(2,1)*c2
           uvm_ns(1,1,jsouth,ipair,mm)=fu(1,2)*c2
           uvm_ns(1,2,jsouth,ipair,mm)=fu(2,2)*c2
           uvm_ns(2,1,jnorth,ipair,mm)=fv(1,1)*c2
           uvm_ns(2,2,jnorth,ipair,mm)=fv(2,1)*c2
           uvm_ns(2,1,jsouth,ipair,mm)=fv(1,2)*c2
           uvm_ns(2,2,jsouth,ipair,mm)=fv(2,2)*c2
           uvm_ns(3,1,jnorth,ipair,mm)=fp(1,1)*c1
           uvm_ns(3,2,jnorth,ipair,mm)=fp(2,1)*c1
           uvm_ns(3,1,jsouth,ipair,mm)=fp(1,2)*c1
           uvm_ns(3,2,jsouth,ipair,mm)=fp(2,2)*c1

        end do

     end do

  end do

end subroutine inmi_nsuvm2zdm_ad
      subroutine spdz2uv_ns(m,l,ics,d,z,u,v,utop,vtop)
!$$$  subprogram documentation block
!
! subprogram:    spdz2uv_ns  compute winds from div and vort for one zonal wave number
!   prgmmr: iredell          org: w/nmc23     date: 92-10-31
!
! abstract: computes the wind components from divergence and vorticity
!           in spectral space.
!           subprogram speps should be called already.
!           if l is the zonal wavenumber, n is the total wavenumber,
!           eps(l,n)=sqrt((n**2-l**2)/(4*n**2-1)) and a is earth radius,
!           then the zonal wind component u is computed as
!             u(l,n)=-i*l/(n*(n+1))*a*d(l,n)
!                    +eps(l,n+1)/(n+1)*a*z(l,n+1)-eps(l,n)/n*a*z(l,n-1)
!           and the meridional wind component v is computed as
!             v(l,n)=-i*l/(n*(n+1))*a*z(l,n)
!                    -eps(l,n+1)/(n+1)*a*d(l,n+1)+eps(l,n)/n*a*d(l,n-1)
!           where d is divergence and z is vorticity.
!           u and v are weighted by the cosine of latitude.
!           extra terms are computed over top of the spectral domain.
!           advantage is taken of the fact that eps(l,l)=0
!           in order to vectorize over the entire spectral domain.
!           triangular truncation only
!
! program history log:
!   91-10-31  mark iredell
!   2006-09-05 parrish -- modify to do one zonal wave number only for parallel
!                         computation across processors by zonal wave number.
!   2010-05-14 derber  - make triangular truncation only
!
! usage:    call spdz2uv_ns(m,l,elonn1,eon,eontop,d,z,u,v,utop,vtop)
!
!   input argument list:
!     m        - integer spectral truncation
!     l        - zonal wave number
!     ics      - starting point is full spectral array
!     d        - real (2,l:m) divergence for zonal wave number l
!     z        - real (2,l:m) vorticity for zonal wave number l
!
!   output argument list:
!     u        - real (2,l:m) zonal wind (times coslat) for zonal wave number l
!     v        - real (2,l:m) merid wind (times coslat) for zonal wave number l
!     utop     - real (2) zonal wind (times coslat) over top for zonal wave number l
!     vtop     - real (2) merid wind (times coslat) over top for zonal wave number l
!
! attributes:
!   language: cray fortran
!
!$$$
      implicit none

      integer(i_kind),intent(in   ) :: m,l,ics
      real(r_kind)   ,intent(in   ) :: d(2,l:m),z(2,l:m)
      real(r_kind)   ,intent(  out) :: u(2,l:m),v(2,l:m)
      real(r_kind)   ,intent(  out) :: utop(2),vtop(2)

      integer(i_kind) n
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute winds in the spectral domain

      do n=l,m
         u(1,n)= sp_a%elonn1(ics+n)*d(2,n)
         u(2,n)=-sp_a%elonn1(ics+n)*d(1,n)
         v(1,n)= sp_a%elonn1(ics+n)*z(2,n)
         v(2,n)=-sp_a%elonn1(ics+n)*z(1,n)
      end do
      do n=l,m-1
         u(1,n)=u(1,n)+sp_a%eon(ics+n+1)*z(1,n+1)
         u(2,n)=u(2,n)+sp_a%eon(ics+n+1)*z(2,n+1)
         v(1,n)=v(1,n)-sp_a%eon(ics+n+1)*d(1,n+1)
         v(2,n)=v(2,n)-sp_a%eon(ics+n+1)*d(2,n+1)
      end do
      do n=l+1,m
         u(1,n)=u(1,n)-sp_a%eon(ics+n)*z(1,n-1)
         u(2,n)=u(2,n)-sp_a%eon(ics+n)*z(2,n-1)
         v(1,n)=v(1,n)+sp_a%eon(ics+n)*d(1,n-1)
         v(2,n)=v(2,n)+sp_a%eon(ics+n)*d(2,n-1)
      end do
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute winds over top of the spectral domain
      utop(1)=-sp_a%eontop(l+1)*z(1,m)
      utop(2)=-sp_a%eontop(l+1)*z(2,m)
      vtop(1)= sp_a%eontop(l+1)*d(1,m)
      vtop(2)= sp_a%eontop(l+1)*d(2,m)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end subroutine spdz2uv_ns
!-----------------------------------------------------------------------
      subroutine spuv2dz_ns(m,l,ics,u,v,utop,vtop,d,z)
!$$$  subprogram documentation block
!
! subprogram:    spuv2dz_ns  compute div,vort from winds for one zonal wave number
!   prgmmr: iredell          org: w/nmc23     date: 92-10-31
!
! abstract: computes the divergence and vorticity from wind components
!           in spectral space.
!           subprogram speps should be called already.
!           if l is the zonal wavenumber, n is the total wavenumber,
!           eps(l,n)=sqrt((n**2-l**2)/(4*n**2-1)) and a is earth radius,
!           then the divergence d is computed as
!             d(l,n)=i*l*a*u(l,n)
!                    +eps(l,n+1)*n*a*v(l,n+1)-eps(l,n)*(n+1)*a*v(l,n-1)
!           and the vorticity z is computed as
!             z(l,n)=i*l*a*v(l,n)
!                    -eps(l,n+1)*n*a*u(l,n+1)+eps(l,n)*(n+1)*a*u(l,n-1)
!           where u is the zonal wind and v is the meridional wind.
!           u and v are weighted by the secant of latitude.
!           extra terms are used over top of the spectral domain.
!           advantage is taken of the fact that eps(l,l)=0
!           in order to vectorize over the entire spectral domain.
!           triangular truncation only
!
! program history log:
!   91-10-31  mark iredell
!   2006-09-05 parrish -- modify to do one zonal wave number only for parallel
!                         computation across processors by zonal wave number.
!   2010-05-14 derber  - triangular truncation only
!
! usage:    call spuv2dz_ns(m,l,ics,u,v,utop,vtop,d,z)
!
!   input argument list:
!     l        - zonal wave number
!     m        - integer spectral truncation
!     ics      - starting point is full spectral array
!     u        - real (2,l:m) zonal wind (over coslat)
!     v        - real (2,l:m) merid wind (over coslat)
!     utop     - real (2) zonal wind (over coslat) over top
!     vtop     - real (2) merid wind (over coslat) over top
!
!   output argument list:
!     d        - real (2,l:m) divergence
!     z        - real (2,l:m) vorticity
!
! attributes:
!   language: cray fortran
!
!$$$
      implicit none

      integer(i_kind),intent(in   ) :: m,l,ics
      real(r_kind)   ,intent(in   ) :: u(2,l:m),v(2,l:m)
      real(r_kind)   ,intent(in   ) :: utop(2),vtop(2)
      real(r_kind)   ,intent(  out) :: d(2,l:m),z(2,l:m)

      integer(i_kind) n
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute terms from the spectral domain

      do n=l,m
         d(1,n)=-sp_a%elonn1(ics+n)*u(2,n)
         d(2,n)= sp_a%elonn1(ics+n)*u(1,n)
         z(1,n)=-sp_a%elonn1(ics+n)*v(2,n)
         z(2,n)= sp_a%elonn1(ics+n)*v(1,n)
      end do
      do n=l,m-1
         d(1,n)=d(1,n)+sp_a%eon(ics+n+1)*v(1,n+1)
         d(2,n)=d(2,n)+sp_a%eon(ics+n+1)*v(2,n+1)
         z(1,n)=z(1,n)-sp_a%eon(ics+n+1)*u(1,n+1)
         z(2,n)=z(2,n)-sp_a%eon(ics+n+1)*u(2,n+1)
      end do
      do n=l+1,m
         d(1,n)=d(1,n)-sp_a%eon(ics+n)*v(1,n-1)
         d(2,n)=d(2,n)-sp_a%eon(ics+n)*v(2,n-1)
         z(1,n)=z(1,n)+sp_a%eon(ics+n)*u(1,n-1)
         z(2,n)=z(2,n)+sp_a%eon(ics+n)*u(2,n-1)
      end do

!  compute terms from over top of the spectral domain
      n=m
      d(1,n)=d(1,n)+sp_a%eontop(l+1)*vtop(1)
      d(2,n)=d(2,n)+sp_a%eontop(l+1)*vtop(2)
      z(1,n)=z(1,n)-sp_a%eontop(l+1)*utop(1)
      z(2,n)=z(2,n)-sp_a%eontop(l+1)*utop(2)

      return
      end subroutine spuv2dz_ns

end module strong_fast_global_mod
