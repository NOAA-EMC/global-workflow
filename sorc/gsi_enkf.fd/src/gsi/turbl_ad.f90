subroutine turbl_ad(pges,tges,oges,u,v,prs,t,termu,termv,termt,jstart,jstop)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    turbl_ad
!
!   prgrmmr:
!
! abstract:      Adjoint of turbl_tl
!
! program history log:
!   2008-04-02  safford -- add subprogram doc block, rm unused vars and uses
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
!     prs      -
!     t        -
!     u        -
!     v        -
!     termu    -
!     termv    -
!     termt    -
!
! attributes:
!   language:  f90
!   machine:
!
!$$$

  use constants, only: rd_over_cp,two,rd_over_g,half,zero,one,three,grav
  use kinds, only: r_kind,i_kind 
  use gridmod, only: lat2,lon2,nsig,nsig_hlf
  use turblmod, only: dudz,dvdz,dodz,ri,rf,kar0my20,zi,km,kh,sm,sh
  use turblmod, only: lmix,dudtm,dvdtm,dtdtm,rdzi,rdzl
  use turblmod, only: a0my20,c0my20,d0my20, &
                      f7my20,f8my20,karmy20
  use turblmod, only: eps_m
  use turblmod, only: fsm_my20,fsh_my20
  use turblmod, only: ri_int
  implicit none


! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: pges
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(in   ) :: tges,oges
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: prs
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t,u,v
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: termu,termv,termt
  integer(i_kind)                         ,intent(in   ) :: jstart,jstop

! Declare local variables
  real(r_kind),dimension(nsig_hlf):: t_bck,o_bck
  real(r_kind),dimension(nsig_hlf):: dudz_bck,dvdz_bck,dodz_bck,ri_bck,rf_bck
  real(r_kind),dimension(nsig_hlf):: rdudz_bck,rdvdz_bck,sm_bck,sh_bck
  real(r_kind),dimension(nsig_hlf):: rdzl_bck,rdzi_bck,t_tl,u_tl,v_tl,o_tl,dzi_tl,zl_tl
  real(r_kind),dimension(nsig_hlf):: rssq,rofbck,rshbck
  real(r_kind),dimension(nsig_hlf+1):: km_bck,kh_bck,p_bck,zi_bck
  real(r_kind),dimension(nsig_hlf+1):: km_tl,kh_tl,p_tl,zi_tl
  real(r_kind),dimension(2:nsig_hlf):: dzl_tl,dodz_tl,dudz_tl,dvdz_tl,ri_tl
  real(r_kind),dimension(2:nsig_hlf):: rf_tl,sh_tl,sm_tl,lmix_tl
  integer(i_kind),dimension(nsig_hlf):: lssq
  real(r_kind):: px,rpx,a1,a2,ax,bx,ssq,rtbck
  real(r_kind):: alph,beta,gamm,gam1,gam2,alph1,alph2,delt,bet
  real(r_kind):: rdzibk,rdzlbk,kmbk,khbk,rpbck,hrdzbk,ardzbk
  real(r_kind):: kmaz_bck,khaz_bck,kmaz_tl,khaz_tl
  integer(i_kind) i,j,k

  do i=1,lat2
     do j=jstart,jstop

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

! initialize perturbations

        do k=nsig_hlf,2,-1
           dzl_tl(k)=zero
           lmix_tl(k)=zero
           sm_tl(k)=zero; sh_tl(k)=zero
           ri_tl(k)=zero; rf_tl(k)=zero
           dudz_tl(k)=zero; dvdz_tl(k)=zero; dodz_tl(k)=zero
        enddo
        do k=nsig_hlf,1,-1
           dzi_tl(k)=zero
           zi_tl(k)=zero;   zl_tl(k)=zero
           km_tl(k)=zero; kh_tl(k)=zero
           t_tl(k)=zero
           o_tl(k)=zero; u_tl(k)=zero; v_tl(k)=zero; p_tl(k)=zero
        end do
        p_tl(nsig_hlf+1)=zero; zi_tl(nsig_hlf+1)=zero
        km_tl(nsig_hlf+1)=zero; kh_tl(nsig_hlf+1)=zero

        
! adjoint of update of perturbation tendencies

        do k=nsig_hlf,1,-1
           kmaz_tl=zero
           khaz_tl=zero
           rdzibk=rdzi_bck(k)
           ax=t_bck(k)/o_bck(k)
           hrdzbk=half*rdzibk
           ardzbk=ax*hrdzbk
           kmaz_bck=(km_bck(k)+km_bck(k+1))*hrdzbk
           khaz_bck=(kh_bck(k)+kh_bck(k+1))*ardzbk
           if(k>1) then
              kmaz_tl=-dudz_bck(k)*termu(i,j,k)-dvdz_bck(k)*termv(i,j,k)
              khaz_tl=-dodz_bck(k)*termt(i,j,k)
              dudz_tl(k)=-kmaz_bck*termu(i,j,k)
              dvdz_tl(k)=-kmaz_bck*termv(i,j,k)
              dodz_tl(k)=-khaz_bck*termt(i,j,k)
           end if
           if(k<nsig_hlf) then
              kmaz_tl= dudz_bck(k+1)*termu(i,j,k)+dvdz_bck(k+1)*termv(i,j,k)+kmaz_tl
              khaz_tl= dodz_bck(k+1)*termt(i,j,k)+khaz_tl
              dudz_tl(k+1)= kmaz_bck*termu(i,j,k)+dudz_tl(k+1)
              dvdz_tl(k+1)= kmaz_bck*termv(i,j,k)+dvdz_tl(k+1)
              dodz_tl(k+1)= khaz_bck*termt(i,j,k)+dodz_tl(k+1)
           end if
           dzi_tl(k)= -rdzibk*(dudtm(i,j,k)*termu(i,j,k)+&
                               dvdtm(i,j,k)*termv(i,j,k)+&
                               dtdtm(i,j,k)*termt(i,j,k) )
           if(k>1) then
              km_tl(k)  =hrdzbk*kmaz_tl
              kh_tl(k)  =ardzbk*khaz_tl
           end if
           if(k<nsig_hlf) then
              km_tl(k+1)=hrdzbk*kmaz_tl+km_tl(k+1)
              kh_tl(k+1)=ardzbk*khaz_tl+kh_tl(k+1)
           end if
        end do


! adjoint of perturbation of km and kh

        do k=nsig_hlf,2,-1
           kmbk=km_bck(k)
           khbk=kh_bck(k)
           if(ri_int(i,j,k)==1) then
              km_tl(k)=zero
              kh_tl(k)=zero
           end if
           bx=half/sm_bck(k)
           bet=-half*rofbck(k)
           ax=two/lmix(i,j,k)
           delt= kmbk*ax
           gam1= kmbk*rdudz_bck(k)
           gam2= kmbk*rdvdz_bck(k)
           beta= kmbk*bet
           alph= kmbk*three*bx
           sm_tl(k)  =km_tl(k)*alph  
           rf_tl(k)  =km_tl(k)*beta  
           dudz_tl(k)=km_tl(k)*gam1+dudz_tl(k)
           dvdz_tl(k)=km_tl(k)*gam2+dvdz_tl(k)
           lmix_tl(k)=km_tl(k)*delt          
 
           delt= khbk*ax
           gam1= khbk*rdudz_bck(k)
           gam2= khbk*rdvdz_bck(k)
           beta= khbk*bet
           alph2=khbk*rshbck(k)
           alph= khbk *bx
           sh_tl(k)  =kh_tl(k)*alph2  
           sm_tl(k)  =kh_tl(k)*alph+sm_tl(k)
           rf_tl(k)  =kh_tl(k)*beta+rf_tl(k)
           dudz_tl(k)=kh_tl(k)*gam1+dudz_tl(k)
           dvdz_tl(k)=kh_tl(k)*gam2+dvdz_tl(k)
           lmix_tl(k)=kh_tl(k)*delt+lmix_tl(k)      
        end do


! adjoint of perturbation of flux Richardson number, sh, sm and lmix

        do k=nsig_hlf,2,-1
           zi_tl(k)=karmy20/(one+kar0my20(i,j)*(zi_bck(k)-zi_bck(1)))**2 *lmix_tl(k)  
           if(ri_int(i,j,k)==1) then
              sh_tl(k)=zero
              rf_tl(k)=zero
              sm_tl(k)=zero
           end if 
           sh_tl(k)=sh_tl(k)+sm_bck(k)*rshbck(k)           *sm_tl(k) 
           rf_tl(k)=rf_tl(k)+fsm_my20*sh_bck(k)   &     
                            /(f7my20-f8my20*rf_bck(k))**2  *sm_tl(k) &
                            +fsh_my20*rofbck(k)**2         *sh_tl(k)
           ri_tl(k)=        a0my20*(one-(two*ri_bck(k)-c0my20)/ &
                   (two*sqrt(ri_bck(k)*(ri_bck(k)-c0my20)+d0my20))) *rf_tl(k)
        end do


! adjoint of perturbation of Richardson number

        do k=nsig_hlf,2,-1
           if(ri_int(i,j,k)==1) ri_tl(k) = zero
           rtbck=one/(t_bck(k)+t_bck(k-1))
           alph=-ri_bck(k)*two*lssq(k)
           alph1=alph*rdudz_bck(k)
           alph2=alph*rdvdz_bck(k)
           beta= grav*rssq(k)*rtbck*two
           gamm=-ri_bck(k)*rtbck
           dudz_tl(k)=ri_tl(k)*alph1 + dudz_tl(k)
           dvdz_tl(k)=ri_tl(k)*alph2 + dvdz_tl(k)
           dodz_tl(k)=ri_tl(k)*beta  + dodz_tl(k)
           t_tl(k)   =ri_tl(k)*gamm  + t_tl(k)
           t_tl(k-1) =ri_tl(k)*gamm 
        end do


! adjoint of perturbation of vertical derivatives


        do k=nsig_hlf,2,-1
           rdzlbk=rdzl_bck(k)
           o_tl(k  )=o_tl(k  )+rdzlbk*dodz_tl(k)
           o_tl(k-1)=         -rdzlbk*dodz_tl(k)
           v_tl(k  )=v_tl(k  )+rdzlbk*dvdz_tl(k)
           v_tl(k-1)=         -rdzlbk*dvdz_tl(k)
           u_tl(k  )=u_tl(k  )+rdzlbk*dudz_tl(k)
           u_tl(k-1)=         -rdzlbk*dudz_tl(k)
           dzl_tl(k)=         -rdzlbk*dvdz_bck(k)*dvdz_tl(k) &
                              -rdzlbk*dodz_bck(k)*dodz_tl(k) &
                              -rdzlbk*dudz_bck(k)*dudz_tl(k)
        end do


! adjoint of perturbation of heights

        do k=nsig_hlf,2,-1
           zl_tl(k  )=+dzl_tl(k)+zl_tl(k  )
           zl_tl(k-1)=-dzl_tl(k) !+zl_tl(k-1)
        end do

        do k=nsig_hlf,1,-1
           zi_tl(k+1)=zl_tl(k)*half  + zi_tl(k+1)
           zi_tl(k     )=zl_tl(k)*half  + zi_tl(k  )
        end do

        do k=nsig_hlf,1,-1
           zi_tl(k) =zi_tl(k+1)+zi_tl(k)
           dzi_tl(k)=zi_tl(k+1)+dzi_tl(k)
        end do
        zi_tl(1)=zero    


! adjoint of perturbation fields

        do k=nsig_hlf,1,-1
           rpbck=one/(p_bck(k)+p_bck(k+1))
           rpx = two*rpbck
           a1 = rd_over_g*rpx
           a2 = a1*t_bck(k)*rpx
           gamm=-a1*(p_bck(k+1)-p_bck(k))
           beta=-a2*p_bck(k)
           alph= a2*p_bck(k+1)
           px=rd_over_cp*rpbck
           alph1=o_bck(k)/t_bck(k)
           alph2=-o_bck(k)*px
           t_tl(k  )= gamm*dzi_tl(k)+o_tl(k)*alph1 + t_tl(k  )
           p_tl(k+1)= beta*dzi_tl(k)+o_tl(k)*alph2 + p_tl(k+1)
           p_tl(k     )= alph*dzi_tl(k)+o_tl(k)*alph2 
        end do


        do k=nsig_hlf,1,-1
           t(i,j,k)=t_tl(k)+t(i,j,k)
           u(i,j,k)=u_tl(k)+u(i,j,k)
           v(i,j,k)=v_tl(k)+v(i,j,k)
           prs(i,j,k)=p_tl(k)+prs(i,j,k)
        end do
        prs(i,j,nsig_hlf+1)=p_tl(nsig_hlf+1)+prs(i,j,nsig_hlf+1)

     end do 
  end do 

  return
end subroutine turbl_ad
