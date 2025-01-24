subroutine turbl_tl(pges,tges,oges,u,v,prs,t,termu,termv,termt,jstart,jstop)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    turbl_tl
!
!   prgrmmr:
!
! abstract:      Tangent linear of turbl
!
! program history log:
!   2008-04-01  safford - add subprogram doc block, rm unused vars and uses
!   2008-11-01  guo     - remove reference to dzl_tl(1); out-of-bounds
!
!   input argument list:
!     pges     -
!     tges     -
!     oges     -
!     prs      -
!     t        -
!     u        -
!     v        -
!     termu    -
!     termv    -
!     termt    -
!
!   output argument list:
!     termu    -
!     termv    -
!     termt    -
!
! attributes:
!   language:  f90
!   machine:
!
!$$$

  use constants,only: rd_over_cp,two,rd_over_g,half,zero,one,three,grav
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,nsig_hlf
  use turblmod, only: dudz,dvdz,dodz,ri,rf,kar0my20,zi,km,kh,sm,sh
  use turblmod, only: lmix,dudtm,dvdtm,dtdtm,rdzi,rdzl
  use turblmod, only: a0my20,c0my20,d0my20,f7my20,f8my20,karmy20
  use turblmod, only: eps_m,fsm_my20,fsh_my20,ri_int

  implicit none


! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: pges
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(in   ) :: tges,oges
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: prs
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(in   ) :: t,u,v
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: termu,termv,termt
  integer(i_kind)                         ,intent(in   ) :: jstart,jstop  

! Declare local variables
  real(r_kind),dimension(nsig_hlf):: dzi_tl,t_bck,o_bck
  real(r_kind),dimension(nsig_hlf):: dudz_bck,dvdz_bck,dodz_bck,ri_bck,rf_bck
  real(r_kind),dimension(nsig_hlf):: rdudz_bck,rdvdz_bck,sm_bck,sh_bck,rdzl_bck,rdzi_bck
  real(r_kind),dimension(nsig_hlf):: u_tl,v_tl,o_tl,zl_tl,t_tl,rssq,rofbck,rshbck
  real(r_kind),dimension(nsig_hlf+1):: km_bck,kh_bck,p_bck,zi_bck,km_tl,kh_tl,zi_tl,p_tl
  real(r_kind),dimension(2:nsig_hlf):: dzl_tl,dodz_tl,dudz_tl,dvdz_tl,ri_tl
  real(r_kind),dimension(2:nsig_hlf):: rf_tl,sh_tl,sm_tl,lmix_tl
  real(r_kind):: a1,a2,ax,bx,px,rpx,zx,ssq
  real(r_kind):: alph,beta,gamm,gam1,gam2,alph1,alph2,delt,bet
  real(r_kind):: rdzibk,rdzlbk,kmbk,khbk,rpbck,hrdzbk,ardzbk
  real(r_kind):: kmaz_bck,khaz_bck,kmaz_tl,khaz_tl,rtbck

  integer(i_kind) i,j,k
  integer(i_kind),dimension(nsig):: lssq

  
  do j=jstart,jstop
     do i=1,lat2

! background fields
        do k=1,nsig_hlf
           t_bck(k)=tges(i,j,k)
           p_bck(k)=pges(i,j,k)
           o_bck(k)=oges(i,j,k)
           rdzi_bck(k)=rdzi(i,j,k)
           rdzl_bck(k)=rdzl(i,j,k)
           zi_bck(k)=zi(i,j,k)
        end do
        p_bck(nsig_hlf+1) =pges(i,j,nsig_hlf+1)
        zi_bck(nsig_hlf+1)=zi  (i,j,nsig_hlf+1)

        do k=1,nsig_hlf
           dodz_bck(k)=dodz(i,j,k)
           dudz_bck(k)=dudz(i,j,k)
           dvdz_bck(k)=dvdz(i,j,k)
           ri_bck(k)=ri(i,j,k)
           rf_bck(k)=rf(i,j,k)
           sm_bck(k)=sm(i,j,k)
           sh_bck(k)=sh(i,j,k)
           km_bck(k)=km(i,j,k)
           kh_bck(k)=kh(i,j,k)
        end do
        km_bck(nsig_hlf+1)=zero
        kh_bck(nsig_hlf+1)=zero
      
        do k=2,nsig_hlf
           ssq=dudz_bck(k)**2+dvdz_bck(k)**2
           lssq(k)=1
           if(ssq < eps_m) then
              lssq(k)=0
              ssq = eps_m
           end if
           rssq(k)=one/ssq
           rdudz_bck(k)=dudz_bck(k)*rssq(k)
           rdvdz_bck(k)=dvdz_bck(k)*rssq(k)
           rofbck(k)=one/(one-rf_bck(k))
           rshbck(k)=one/sh_bck(k)
        end do 


! perturbation fields

        do k=1,nsig_hlf
           t_tl(k)=t(i,j,k)
           u_tl(k)=u(i,j,k)
           v_tl(k)=v(i,j,k)
           p_tl(k)=prs(i,j,k)
        end do
        p_tl (nsig_hlf+1) =prs (i,j,nsig_hlf+1)

! perturbations of potential temperature and heigh increment

        do k=1,nsig_hlf
           rpbck=one/(p_bck(k)+p_bck(k+1))
           px=rd_over_cp*rpbck
           alph1=o_bck(k)/t_bck(k)
           alph2=-o_bck(k)*px
           o_tl(k)=alph1* t_tl(k)+&
                   alph2*(p_tl(k)+&
                          p_tl(k+1))
           rpx=two*rpbck
           a1=rd_over_g*rpx
           a2=a1*t_bck(k)*rpx
           alph= a2*p_bck(k+1)
           beta=-a2*p_bck(k)
           gamm=-a1*(p_bck(k+1)-p_bck(k))
           dzi_tl(k)=alph*p_tl(k)+&
                     beta*p_tl(k+1)+&
                     gamm*t_tl(k)
        end do

! perturbation of heights

        zi_tl(1)=zero
        do k=1,nsig_hlf
           zi_tl(k+1)=zi_tl(k)+dzi_tl(k)
        end do

        do k=1,nsig_hlf
           zl_tl(k)=half*(zi_tl(k)+zi_tl(k+1))
        end do

        do k=2,nsig_hlf
           dzl_tl(k)=zl_tl(k)-zl_tl(k-1)
        end do

! perturbation of vertical derivatives

        do k=2,nsig_hlf
           rdzlbk=rdzl_bck(k)
           zx=dzl_tl(k)*rdzlbk
           dudz_tl(k)=(u_tl(k)-u_tl(k-1))*rdzlbk - & 
                      dudz_bck(k)*zx
           dvdz_tl(k)=(v_tl(k)-v_tl(k-1))*rdzlbk - & 
                      dvdz_bck(k)*zx
           dodz_tl(k)=(o_tl(k)-o_tl(k-1))*rdzlbk - & 
                      dodz_bck(k)*zx
        end do

! perturbation of Richardson number

        do k=2,nsig_hlf
           if(ri_int(i,j,k)==1) then
              ri_tl(k) = zero
           else
              rtbck = one/(t_bck(k)+t_bck(k-1))
              alph=-ri_bck(k)*two*lssq(k)
              alph1=alph*rdudz_bck(k)
              alph2=alph*rdvdz_bck(k)
              beta= grav*rssq(k)*rtbck*two
              gamm=-ri_bck(k)*rtbck
              ri_tl(k)=alph1*dudz_tl(k)+&
                       alph2*dvdz_tl(k)+&
                       beta* dodz_tl(k)+&
                       gamm*(t_tl(k)+t_tl(k-1))
           end if
        end do


! perturbation of flux Richardson number, sh, sm and lmix

        do k=2,nsig_hlf
           if(ri_int(i,j,k)==1) then
              rf_tl(k)=zero
              sh_tl(k)=zero
              sm_tl(k)=zero
           else
              ax=one/(f7my20-f8my20*rf_bck(k))
              rf_tl(k)=a0my20*(one-(two*ri_bck(k)-c0my20)/ &
                 (two*sqrt(ri_bck(k)*(ri_bck(k)-c0my20)+d0my20))) *ri_tl(k)
              sh_tl(k)=fsh_my20*rofbck(k)**2    *rf_tl(k)
              sm_tl(k)=fsm_my20*sh_bck(k)*ax**2 *rf_tl(k) +&
                       sm_bck(k)*rshbck(k)      *sh_tl(k)
           end if
           lmix_tl(k)=karmy20/(one+kar0my20(i,j)*(zi_bck(k)-zi_bck(1)))**2 *zi_tl(k)
        end do


! perturbation of km and kh

        do k=2,nsig_hlf
           kmbk=km_bck(k)
           khbk=kh_bck(k)
           if(ri_int(i,j,k)==1) then
              km_tl(k)=zero
              kh_tl(k)=zero
           else
              ax=two/lmix(i,j,k)
              bet=-half*rofbck(k)
              bx=half/sm_bck(k)
              alph= kmbk*three*bx
              beta= kmbk*bet
              gam1= kmbk*rdudz_bck(k)
              gam2= kmbk*rdvdz_bck(k)
              delt= kmbk*ax
              km_tl(k)=alph*sm_tl(k) + &
                       beta*rf_tl(k) + &
                       gam1*dudz_tl(k)+&
                       gam2*dvdz_tl(k)+&
                       delt*lmix_tl(k) 
              alph= khbk*bx
              alph2=khbk*rshbck(k)
              beta= khbk*bet
              gam1= khbk*rdudz_bck(k)
              gam2= khbk*rdvdz_bck(k)
              delt= khbk*ax
              kh_tl(k)=alph*sm_tl(k) + &
                       alph2*sh_tl(k)+ &
                       beta*rf_tl(k) + &
                       gam1*dudz_tl(k)+&
                       gam2*dvdz_tl(k)+&
                       delt*lmix_tl(k)
           end if
        end do

        km_tl(1)=zero; kh_tl(1)=zero

! update perturbation tendencies


        do k=1,nsig_hlf
           rdzibk=rdzi_bck(k)
           ax=t_bck(k)/o_bck(k)
           hrdzbk=half*rdzibk
           ardzbk=ax*hrdzbk
           zx= dzi_tl(k)*rdzibk
           kmaz_bck=(km_bck(k)+km_bck(k+1))*hrdzbk
           khaz_bck=(kh_bck(k)+kh_bck(k+1))*ardzbk
   
           termu(i,j,k)=termu(i,j,k)-zx*dudtm(i,j,k)
           termv(i,j,k)=termv(i,j,k)-zx*dvdtm(i,j,k)
           termt(i,j,k)=termt(i,j,k)-zx*dtdtm(i,j,k)
           kmaz_tl=zero
           khaz_tl=zero
           if(k<nsig_hlf) then
              kmaz_tl =kmaz_tl+km_tl (k+1)*hrdzbk
              khaz_tl =khaz_tl+kh_tl (k+1)*ardzbk
           end if
           if(k>1) then
              kmaz_tl= kmaz_tl+km_tl (k)*hrdzbk
              khaz_tl= khaz_tl+kh_tl (k)*ardzbk
           end if
           if(k<nsig_hlf) then
              termu(i,j,k)=termu(i,j,k)+&
                 kmaz_bck*dudz_tl(k+1) +&
                 kmaz_tl*dudz_bck(k+1)
              termv(i,j,k)=termv(i,j,k)+&
                 kmaz_bck*dvdz_tl(k+1) +&
                 kmaz_tl*dvdz_bck(k+1)
              termt(i,j,k)=termt(i,j,k)+&
                 khaz_bck*dodz_tl(k+1) +&
                 khaz_tl*dodz_bck(k+1)
           end if
           if(k>1) then
              termu(i,j,k)=termu(i,j,k)-&
                   kmaz_bck*dudz_tl(k) -&
                   kmaz_tl*dudz_bck(k)
              termv(i,j,k)=termv(i,j,k)-&
                   kmaz_bck*dvdz_tl(k) -&
                   kmaz_tl*dvdz_bck(k)
              termt(i,j,k)=termt(i,j,k)-&
                   khaz_bck*dodz_tl(k) -&
                   khaz_tl*dodz_bck(k)
           end if
        end do 
     end do
  end do

  return
end subroutine turbl_tl
