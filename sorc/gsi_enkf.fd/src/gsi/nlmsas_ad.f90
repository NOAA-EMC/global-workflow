module nlmsas_ad_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    nlmsas_ad_mod   module wrapper around subroutine nlmsas_ad
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract: This module has been added as a wrapper around subroutine nlmsas_ad
!            to eliminate type mismatch compile errors when using the debug
!            compile option on WCOSS.
!
! program history log:
!   2012-01-26  parrish
!
! subroutines included:
!  nlmsas_ad
!  nlmsas_ad_1_1_
!  nlmsas_ad_im_ix_

  implicit none

! set default to private
  private
! set subroutines to public
  public :: nlmsas_ad

  interface nlmsas_ad
     module procedure nlmsas_ad_1_1_
     module procedure nlmsas_ad_im_ix_
  end interface

contains

subroutine nlmsas_ad_1_1_(im,ix,km,jcap,delt,del_,sl_,rcs_,&
     slimsk_,xkt2_,ncloud,ps_, &
     t0_,q0_,cwm0_,u0_,v0_,dot0_, &
     t1_,q1_,cwm1_,u1_,v1_,rn1_, &
     cldwrk_,kbot_,ktop_,jmin_,kuo_,kb_, &
     t0_ad_,q0_ad_,cwm0_ad_,u0_ad_,v0_ad_,dot0_ad_, &
     t1_ad_,q1_ad_,cwm1_ad_,u1_ad_,v1_ad_,rn1_ad_, &
     adjoint)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nlmsas_ad_1_1_
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract:  interface for nlmsas_ad, where im=1,ix=1, and calling routine has
!              no dimension index corresponding to im and ix.
!
! program history log:
!   2013-01-26  parrish - initial documentation
!
!   input argument list:
!     im,ix,km,jcap,ncloud,adjoint,delt
!     del_(km),sl_(km),rcs_,slimsk_,xkt2_,ps_,t0_(km),q0_(km),cwm0_(km),u0_(km),v0_(km),dot0_(km)
!     t1_(km),q1_(km),cwm1_(km),u1_(km),v1_(km),rn1_,cldwrk_,kbot_,ktop_,jmin_,kuo_,kb_
!     t0_ad_(km),q0_ad_(km),cwm0_ad_(km),u0_ad_(km),v0_ad_(km),dot0_ad_(km)
!     t1_ad_(km),q1_ad_(km),cwm1_ad_(km),u1_ad_(km),v1_ad_(km),rn1_ad_
!
!   output argument list:
!     t1_(km),q1_(km),cwm1_(km),u1_(km),v1_(km),rn1_,cldwrk_,kbot_,ktop_,jmin_,kuo_,kb_
!     t0_ad_(km),q0_ad_(km),cwm0_ad_(km),u0_ad_(km),v0_ad_(km),dot0_ad_(km)
!     t1_ad_(km),q1_ad_(km),cwm1_ad_(km),u1_ad_(km),v1_ad_(km),rn1_ad_

  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind), intent(in   ) :: im,ix,km,jcap,ncloud
  logical,         intent(in   ) :: adjoint
  real(r_kind),    intent(in   ) :: delt

  real(r_kind),    intent(in   ) :: del_(km),sl_(km),rcs_,slimsk_
  real(r_kind),    intent(in   ) :: xkt2_,ps_,t0_(km),q0_(km),cwm0_(km),u0_(km),v0_(km)
  real(r_kind),    intent(in   ) :: dot0_(km)
  real(r_kind),    intent(inout) :: t1_(km),q1_(km),cwm1_(km)
  real(r_kind),    intent(inout) :: u1_(km),v1_(km),rn1_,cldwrk_
  integer(i_kind), intent(inout) :: kbot_,ktop_,jmin_,kuo_
  integer(i_kind), intent(inout) :: kb_
  real(r_kind),    intent(inout) :: t0_ad_(km),q0_ad_(km),cwm0_ad_(km)
  real(r_kind),    intent(inout) :: u0_ad_(km),v0_ad_(km),dot0_ad_(km)
  real(r_kind),    intent(inout) :: t1_ad_(km),q1_ad_(km),cwm1_ad_(km),u1_ad_(km),v1_ad_(km)
  real(r_kind),    intent(inout) :: rn1_ad_


  real(r_kind)     :: del(km,ix),sl(km,ix),rcs(ix),slimsk(ix)
  real(r_kind)     :: xkt2(ix),ps(ix),t0(km,ix),q0(km,ix),cwm0(km,ix),u0(km,ix),v0(km,ix)
  real(r_kind)     :: dot0(km,ix)
  real(r_kind)     :: t1(km,ix),q1(km,ix),cwm1(km,ix)
  real(r_kind)     :: u1(km,ix),v1(km,ix),rn1(ix),cldwrk(ix)
  integer(i_kind)  :: kbot(ix),ktop(ix),jmin(ix),kuo(ix)
  integer(i_kind)  :: kb(ix)
  real(r_kind)     :: t0_ad(km,ix),q0_ad(km,ix),cwm0_ad(km,ix)
  real(r_kind)     :: u0_ad(km,ix),v0_ad(km,ix),dot0_ad(km,ix)
  real(r_kind)     :: t1_ad(km,ix),q1_ad(km,ix),cwm1_ad(km,ix),u1_ad(km,ix),v1_ad(km,ix)
  real(r_kind)     :: rn1_ad(ix)

  integer(i_kind) k

  if( im /= 1 .or. ix /= 1 ) then
     write(6,*)' NLMSAS_AD_1_1_, IM,IX=',IM,IX,' -- BOTH MUST BE 1.  PROGRAM FAILS'
     stop
  end if

  rcs(1)=rcs_
  slimsk(1)=slimsk_
  xkt2(1)=xkt2_
  ps(1)=ps_
  rn1(1)=rn1_
  cldwrk(1)=cldwrk_
  kbot(1)=kbot_
  ktop(1)=ktop_
  jmin(1)=jmin_
  kuo(1)=kuo_
  kb(1)=kb_
  rn1_ad(1)=rn1_ad_
  do k=1,km
     del(k,1)=del_(k)
     sl(k,1)=sl_(k)
     t0(k,1)=t0_(k)
     q0(k,1)=q0_(k)
     cwm0(k,1)=cwm0_(k)
     u0(k,1)=u0_(k)
     v0(k,1)=v0_(k)
     dot0(k,1)=dot0_(k)
     t1(k,1)=t1_(k)
     q1(k,1)=q1_(k)
     cwm1(k,1)=cwm1_(k)
     u1(k,1)=u1_(k)
     v1(k,1)=v1_(k)
     t0_ad(k,1)=t0_ad_(k)
     q0_ad(k,1)=q0_ad_(k)
     cwm0_ad(k,1)=cwm0_ad_(k)
     u0_ad(k,1)=u0_ad_(k)
     v0_ad(k,1)=v0_ad_(k)
     dot0_ad(k,1)=dot0_ad_(k)
     t1_ad(k,1)=t1_ad_(k)
     q1_ad(k,1)=q1_ad_(k)
     cwm1_ad(k,1)=cwm1_ad_(k)
     u1_ad(k,1)=u1_ad_(k)
     v1_ad(k,1)=v1_ad_(k)
  end do

  call nlmsas_ad_im_ix_(im,ix,km,jcap,delt,del,sl,rcs,&
     slimsk,xkt2,ncloud,ps, &
     t0,q0,cwm0,u0,v0,dot0, &
     t1,q1,cwm1,u1,v1,rn1, &
     cldwrk,kbot,ktop,jmin,kuo,kb, &
     t0_ad,q0_ad,cwm0_ad,u0_ad,v0_ad,dot0_ad, &
     t1_ad,q1_ad,cwm1_ad,u1_ad,v1_ad,rn1_ad, &
     adjoint)

  rn1_=rn1(1)
  cldwrk_=cldwrk(1)
  kbot_=kbot(1)
  ktop_=ktop(1)
  jmin_=jmin(1)
  kuo_=kuo(1)
  kb_=kb(1)
  rn1_ad_=rn1_ad(1)
  do k=1,km
     t1_(k)=t1(k,1)
     q1_(k)=q1(k,1)
     cwm1_(k)=cwm1(k,1)
     u1_(k)=u1(k,1)
     v1_(k)=v1(k,1)
     t0_ad_(k)=t0_ad(k,1)
     q0_ad_(k)=q0_ad(k,1)
     cwm0_ad_(k)=cwm0_ad(k,1)
     u0_ad_(k)=u0_ad(k,1)
     v0_ad_(k)=v0_ad(k,1)
     dot0_ad_(k)=dot0_ad(k,1)
     t1_ad_(k)=t1_ad(k,1)
     q1_ad_(k)=q1_ad(k,1)
     cwm1_ad_(k)=cwm1_ad(k,1)
     u1_ad_(k)=u1_ad(k,1)
     v1_ad_(k)=v1_ad(k,1)
  end do

end subroutine nlmsas_ad_1_1_

subroutine nlmsas_ad_im_ix_(im,ix,km,jcap,delt,del,sl,rcs,&
     slimsk,xkt2,ncloud,ps, &
     t0,q0,cwm0,u0,v0,dot0, &
     t1,q1,cwm1,u1,v1,rn1, &
     cldwrk,kbot,ktop,jmin,kuo,kb, &
     t0_ad,q0_ad,cwm0_ad,u0_ad,v0_ad,dot0_ad, &
     t1_ad,q1_ad,cwm1_ad,u1_ad,v1_ad,rn1_ad, &
     adjoint)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nlmsas_ad    forward and adjoint model for GFS SASCNV
!     prgmmr:    treadon     org: np23                date: 2003-12-18
!
! abstract:  This routine computes convective heating and moistening using a 
!   one cloud type Arakawa-Schubert convection scheme originally developed
!   by George Grell.  The scheme includes updraft and downdraft effects.
!   The closure is the cloud work function. both updraft and downdraft
!   are assumed to be saturated and the heating and moistening are
!   accomplished by the compensating environment. The name, SASCNV, comes
!   from "simplified arakawa-schubert convection parameterization".
!
!   This routine also includes the adjoint of SASCNV
!
!
! program history log:
!   1992-03-01  Hua-Lu Pan   - initial SASCNV (forward model only)
!   2004-06-14  Russ Treadon - reformat documentation
!   2004-10-28  treadon - replace parameters "tiny" and "qsmall"
!                         with global constant "tiny_r_kind"
!   2006-01-06  treadon - fix cnvflg bug
!   2006-04-12  treadon - change del and sl from 1d to 2d arrays
!   2008-04-29  safford - rm unused vars
!   2008-10-29  min-jeong kim - make consistent with global_fcst
!   2013-01-26  parrish - module added as a wrapper around subroutine nlmsas_ad
!                            to eliminate type mismatch compile errors when using the debug
!                            compile option on WCOSS.
!
!  input argument list:
!     im       - integer number of points
!     ix       - leading dimension of qn,tn,q1,t0,spd
!     km       - integer number of levels
!     jcap     - integer spectral truncation
!     delt     - time step in seconds
!     del      - sigma layer thickness
!     sl       - sigma values
!     rcs      - 1/sin(lat)**2
!     slimsk   - sea (=0), land (=1), ice (=2) flag
!     xkt2     - random number used to select cloud top
!     ncloud   - parameter to turn on cloud water adjustment
!     ps       - surface pressure in kilopascals (cb)
!     t0       - temperature
!     q0       - specific humidity
!     cwm0     - cloud condensate mixing ratio
!     u0       - zonal wind component
!     v0       - meridional wind component
!     dot0     - vertical velocity
!     t1_ad    - temperature perturbation
!     q1_ad    - specific humidity perturbation
!     cwm1_ad  - cloud condensate mixing ratio perturbation
!     u1_ad    - zonal wind perturbation
!     v1_ad    - meridional wind perturbation
!     rn1_ad   - convective rain rate perturbation
!     adjoint  - logical flag (.false.=forward model only, .true.=forward and ajoint)
!
!   output argument list:
!     t1      - temperature adjusted by convective processes
!     q1      - specific humidity adjusted by convective processes
!     cwm1    - cloud condensate mixing ratio adjusted by convective processes
!     u1      - zonal wind adjusted by convective processes
!     v1      - meridional wind adjusted by convective processes
!     rn1     - convective rain rate
!     cldwrk  - cloud work function
!     kbot    - integer model level of cloud base 
!     ktop    - integer model level of cloud top
!     jmin    - integer model level of downdraft origin
!     kuo     - bit flag indicating presence (=1) of convection (0=no convection)
!     kb      - integer model level of updraft origin
!     t0_ad   - partial derivative of convective rain rate with respect to t
!     q0_ad   - partial derivative of convective rain rate with respect to q
!     cwm0_ad - partial derivative of convective rain rate with respect to cwm
!     u0_ad   - partial derivative of convective rain rate with respect to u
!     v0_ad   - partial derivative of convective rain rate with respect to v
!     dot0_ad - partial derivative of convective rain rate with respect to vertical velocity
!     
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: c0,tiny_r_kind,el2orc,factor1,factor2,pcpeff3,h300,&
       elocp,pcpeff2,pcpeff0,pcpeff1,epsm1,delta,eps,zero,one_tenth,one,two,three,cp,hvap,&
       half,rd,grav,r1000,r3600
  implicit none  

  integer(i_kind), intent(in   ) :: im,ix,km,jcap,ncloud
  logical adjoint
  real(r_kind),    intent(in   ) :: delt
  real(r_kind),    intent(in   ) :: del(km,ix),sl(km,ix),rcs(ix),slimsk(ix)
  real(r_kind),    intent(in   ) :: xkt2(ix),ps(ix),t0(km,ix),q0(km,ix),cwm0(km,ix),u0(km,ix),v0(km,ix)
  real(r_kind),    intent(in   ) :: dot0(km,ix)
  real(r_kind),    intent(inout) :: t1(km,ix),q1(km,ix),cwm1(km,ix)
  real(r_kind),    intent(inout) :: u1(km,ix),v1(km,ix),rn1(ix),cldwrk(ix)
  integer(i_kind), intent(inout) :: kbot(ix),ktop(ix),jmin(ix),kuo(ix)
  integer(i_kind), intent(inout) :: kb(ix)
  real(r_kind),    intent(inout) :: t0_ad(km,ix),q0_ad(km,ix),cwm0_ad(km,ix)
  real(r_kind),    intent(inout) :: u0_ad(km,ix),v0_ad(km,ix),dot0_ad(km,ix)
  real(r_kind),    intent(inout) :: t1_ad(km,ix),q1_ad(km,ix),cwm1_ad(km,ix),u1_ad(km,ix),v1_ad(km,ix)
  real(r_kind),    intent(inout) :: rn1_ad(ix)

! Local variables and arrays
  logical,dimension(ix):: cnvflg,dwnflg,dwnflg2,flg
  logical,dimension(km,ix):: flgk

  integer(i_kind) kbcon(ix),lmin(ix),ktcon(ix),kbdtr(ix)
 
  real(r_kind) term,r1200
  real(r_kind) to2(km,ix),qo2(km,ix)
  real(r_kind) to3(km,ix),qo3(km,ix)
  real(r_kind) p(km,ix),pdot(ix),acrtfct(ix),acrtfct1(ix)
  real(r_kind) acrtfct0(ix)
  real(r_kind) to(km,ix),qo(km,ix),uo(km,ix),vo(km,ix)
  real(r_kind) qeso(km,ix)
  real(r_kind) tvo(km,ix),dbyo(km,ix)
  real(r_kind) zo(km,ix)
  real(r_kind) heo(km,ix),heso(km,ix)
  real(r_kind) qrcd(km,ix),dellah(km,ix),dellaq(km,ix)
  real(r_kind) hcko(km,ix)
  real(r_kind) qcko(km,ix),eta(km,ix)
  real(r_kind) etad(km,ix)
  real(r_kind) qrcdo(km,ix),dtconv0(ix)
  real(r_kind) pwo(km,ix),pwdo(km,ix),dtconv(ix),acrt(ix)
  real(r_kind) psfc(ix),hmax(ix)
  real(r_kind) hkbo(ix),qkbo(ix),pbcdif(ix)
  real(r_kind) hmin(ix),pwavo(ix)
  real(r_kind) aa1(ix),vshear(ix)
  real(r_kind) edt(ix)
  real(r_kind) edto(ix),pwevo(ix)
  real(r_kind) hcdo(ix),qcdo(ix)
  real(r_kind) xhkb(ix),xqkb(ix),xpwav(ix),xpwev(ix),xhcd(ix)
  real(r_kind) xaa0(ix),f(ix),xk(ix),xmb(ix)
  real(r_kind) edtx(ix),xqcd(ix)
  real(r_kind) hsbar(ix),xmbmax(ix),xlamb(ix),xlamd(ix)
  real(r_kind) xlamdet(ix),xlamdet0(ix),xlamdet1(ix)
  real(r_kind) pcrit(15), acritt(15), acrit(15)
  real(r_kind) mbdt
  real(r_kind) rntot(ix)
  
  integer(i_kind) kt2(ix)
      
!new
  real(r_kind) qol(km,ix),uol(km,ix),vol(km,ix),tol(km,ix)
  real(r_kind) heol(km,ix),hesol(km,ix),qesol(km,ix),qol0(km,ix)
  real(r_kind) xqo(km,ix),xto(km,ix),xtvo(km,ix),xzo(km,ix),xqo0(km,ix)
  real(r_kind) xheo(km,ix),xheso(km,ix),xqeso(km,ix),xqrcd(km,ix)
  real(r_kind) xhcko(km,ix),xqcko(km,ix),xtol(km,ix),xqol(km,ix)
  real(r_kind) xqesol(km,ix),aa10(ix),xqcko0(km,ix),xqrch0(km,ix)
  real(r_kind) dpscl(ix),tdpscl(ix),dpcld(ix),tdpcld(ix)
  real(r_kind) tcwfup(ix),tcwfdn(ix),tdetrn(ix),tdpdwn(ix)
  real(r_kind) eta0(km,ix),hcko0(km,ix),hckod(km,ix),hcko1(km,ix)
  real(r_kind) qcko0(km,ix),dqk(km,ix),edt0(ix),qcko00(km,ix)
  real(r_kind) xtemp0(km,ix),xmb0(ix),rn11(ix)
  real(r_kind) qeso2(km,ix),rn0k(km,ix),delqevk(km,ix)
  real(r_kind) edto1(ix),edto10(ix),edtx1(ix),edtx10(ix)
!new

!ajm
  real(r_kind) to_ad(km,ix),qo_ad(km,ix),qeso_ad(km,ix),tvo_ad(km,ix)
  real(r_kind) zo_ad(km,ix),heo_ad(km,ix),heso_ad(km,ix)
  real(r_kind) tol_ad(km,ix),qol_ad(km,ix),hesol_ad(km,ix),heol_ad(km,ix)
  real(r_kind) qesol_ad(km,ix),hkbo_ad(ix),xlamb_ad(ix),eta_ad(km,ix)
  real(r_kind) hcko_ad(km,ix),qcko_ad(km,ix),xlamdet_ad(ix)
  real(r_kind) hcko1_ad(km,ix),dbyo_ad(km,ix),hckod_ad(km,ix)
  real(r_kind) pwo_ad(km,ix),aa1_ad(ix),pwavo_ad(ix),tcwfup_ad(ix)
  real(r_kind) tcwfdn_ad(ix),vshear_ad(ix),edt_ad(ix),edto_ad(ix)
  real(r_kind) edtx_ad(ix),uo_ad(km,ix),vo_ad(km,ix),uol_ad(km,ix)
  real(r_kind) vol_ad(km,ix),xlamd_ad(ix),etad_ad(km,ix),qrcdo_ad(km,ix)
  real(r_kind) pwdo_ad(km,ix),pwevo_ad(ix),qcdo_ad(ix),hcdo_ad(ix)
  real(r_kind) dellah_ad(km,ix),dellaq_ad(km,ix),xto_ad(km,ix)
  real(r_kind) xqo_ad(km,ix),xqeso_ad(km,ix),xtvo_ad(km,ix)
  real(r_kind) xzo_ad(km,ix),xtol_ad(km,ix),xqol_ad(km,ix)
  real(r_kind) xqesol_ad(km,ix),xheo_ad(km,ix),xheso_ad(km,ix)
  real(r_kind) xhcko_ad(km,ix),xqcko_ad(km,ix),xaa0_ad(ix),xpwav_ad(ix)
  real(r_kind) xpwev_ad(ix),xqrcd_ad(km,ix),xhcd_ad(ix),pdot_ad(ix)
  real(r_kind) acrtfct1_ad(ix),acrtfct_ad(ix),dtconv_ad(ix)
  real(r_kind) f_ad(ix),xk_ad(ix),xmb_ad(ix),to2_ad(km,ix)
  real(r_kind) qo2_ad(km,ix),qeso2_ad(km,ix),rntot_ad(ix)
  real(r_kind) to3_ad(km,ix),qo3_ad(km,ix),edto1_ad(ix),edtx1_ad(ix)
!ajm

!new
  integer(i_kind) ktcon0(ix)
  real(r_kind) sumz(km,ix),sumh(km,ix),cwmo_ad(km,ix)
  real(r_kind) ucko(km,ix),vcko(km,ix),etau(km,ix),etau0(km,ix)
  real(r_kind) ucko0(km,ix),vcko0(km,ix),ucdo(ix),vcdo(ix)
  real(r_kind) uckod(km,ix),vckod(km,ix),dellalk_ad(km,ix)
  real(r_kind) ukbo(ix),vkbo(ix),qlko_ktcon(ix),dellal(ix),dellal1(ix)
  real(r_kind) dellalk(km,ix),cwmo2_ad(km,ix),dellal1_ad(ix)
  real(r_kind) dellau(km,ix),dellav(km,ix),uo2(km,ix),vo2(km,ix)
  real(r_kind) cwmo(km,ix),cwmo2(km,ix)
  real(r_kind) qo0(km,ix),qo0_ad(km,ix),dellal_ad(ix),xqol0_ad(km,ix)
  real(r_kind) uo2_ad(km,ix),vo2_ad(km,ix),xqol0(km,ix),xqo0_ad(km,ix)
  real(r_kind) ucko_ad(km,ix),vcko_ad(km,ix),ukbo_ad(ix),vkbo_ad(ix)
  real(r_kind) ucdo_ad(ix),vcdo_ad(ix),ukcod_ad(km,ix),vckod_ad(km,ix)
  real(r_kind) dellau_ad(km,ix),dellav_ad(km,ix),qlko_ktcon_ad(ix)
  real(r_kind) etau_ad(km,ix),xlamdet1_ad(ix),xlamdet0_ad(ix)
  real(r_kind) sumz_ad(km,ix),sumh_ad(km,ix),qol0_ad(km,ix)
  
  integer(i_kind) k,indx,jmn,i
  integer(i_kind),dimension(ix):: kbmax,kbm,kmax
  
  real(r_kind) rterm,fjcap
  real(r_kind) etah_ad,esl_ad,dqsdp_ad,xpw_ad,xqrch_ad,qlk_ad,dz_ad,dz1_ad
  real(r_kind) dvv1_ad,dvu1_ad,dvq1_ad,dv1_ad,pprime_ad,es_ad,desdt_ad,qs_ad
  real(r_kind) termq_ad,term1_ad,term2_ad,term3_ad,term4_ad,dv3v_ad,dv2v_ad
  real(r_kind) dv1v_ad,dv3u_ad,dv2u_ad,dv1u_ad,dv1q_ad
  real(r_kind) dv2_ad,dv3_ad,detad_ad,detau_ad,termu_ad,termv_ad
  real(r_kind) shear_ad,rain_ad,dellat_ad,ratio_ad,dt_ad,dqs_ad,es0_ad
  real(r_kind) evef_ad,qevap_ad,delqev_ad,rn0_ad,term5_ad,dhh_ad,term_ad
  real(r_kind) dv2q_ad,e1_ad,temp_ad,qrch_ad,dv3q_ad,onemf_ad,xtemp_ad
  real(r_kind) factor_ad,dqsdt_ad,xqc_ad,vol2_ad,uol2_ad,tem1_ad,qc_ad
  real(r_kind) qcond_ad,fuv_ad,onemfu_ad,xdby_ad,xpwd_ad,dq_ad,rfact_ad
  real(r_kind) heol2_ad,dh_ad,dg_ad,term6_ad,tem2_ad,gamma_ad
  
  real(r_kind) dv1,dv2,dv3,dv1q,dv2q,dv3q,dv1u,dv2u,dv3u,dv1v,dv2v,dv3v,dp
  real(r_kind) detau,detad,term1,term2,term3,term4,termq,shear,termu,termv
  real(r_kind) betas,beta,betal,xtemp,aup,adw,rain,dvu1,dvq1,dhh,dg,dh,gamma
  real(r_kind) factor,onemf,xdby,xqrch,dq,qc,qlk,qrch,temp,etah,dellat,edtmax
  real(r_kind) edtmaxl,edtmaxs,dz1,rfact,term6,qevap0,qevap1,delq2,qcond,term5
  real(r_kind) w1l,w2l,w3l,w4l,w1s,w2s,w3s,w4s,w1,w2,w3,w4,ratio,xpwd,evef,rn0
  real(r_kind) xpw,xqc,dqs,heol2,fuv,onemfu,uol2,vol2,dvv1,fixed,fkm,dtmin,dtmax
  real(r_kind) dt2,alphal,alphas,adt,es0,po,esl,alpha,pdetrn,pdpdwn,xlambu
  real(r_kind) dz,qevap,desdt,dqsdt,dqsdp,dt,pprime,qs,tem1,tem2,e1,dif,val,es
  real(r_kind) delqev,dlnsig,evfact
  
  save pcrit, acritt
  data pcrit/850._r_kind,800._r_kind,750._r_kind,700._r_kind,&
       650._r_kind,600._r_kind,550._r_kind,500._r_kind,&
       450._r_kind,400._r_kind,350._r_kind,h300,&
       250._r_kind,200._r_kind,150._r_kind/
  data acritt/.0633_r_kind,.0445_r_kind,.0553_r_kind,.0664_r_kind,&
       .075_r_kind,.1082_r_kind,.1521_r_kind,.2216_r_kind,&
       .3151_r_kind,.3677_r_kind,.41_r_kind,.5255_r_kind,&
       .7663_r_kind,1.1686_r_kind,1.6851_r_kind/

!-----------------------------------------------------------------------
  r1200=1200._r_kind
  do i = 1,ix
     do k = 1,km
        cwmo_ad(k,i)=zero
        cwmo2_ad(k,i)=zero
        dellalk_ad(k,i)=zero
        t0_ad(k,i) = zero
        q0_ad(k,i) = zero
        u0_ad(k,i) = zero
        v0_ad(k,i) = zero
        cwm0_ad(k,i)=zero
        dot0_ad(k,i) = zero
        tvo_ad(k,i) = zero
        qeso_ad(k,i) = zero
        qo_ad(k,i) = zero
        to_ad(k,i) = zero
        uo_ad(k,i) = zero
        vo_ad(k,i) = zero
        zo_ad(k,i) = zero
        heso_ad(k,i) = zero
        heo_ad(k,i) = zero
        tol_ad(k,i) = zero
        qol_ad(k,i) = zero
        uol_ad(k,i) = zero
        vol_ad(k,i) = zero
        heol_ad(k,i) = zero
        hesol_ad(k,i) = zero
        qesol_ad(k,i) = zero
        eta_ad(k,i) = zero
        hcko_ad(k,i) = zero
        qcko_ad(k,i) = zero
        hcko1_ad(k,i) = zero
        dbyo_ad(k,i) = zero
        hckod_ad(k,i) = zero
        qcko_ad(k,i) = zero
        pwo_ad(k,i) = zero
        etad_ad(k,i) = zero
        qrcdo_ad(k,i) = zero
        pwdo_ad(k,i) = zero
        dellah_ad(k,i) = zero
        dellaq_ad(k,i) = zero
        xto_ad(k,i) = zero
        xqo_ad(k,i) = zero
        xqeso_ad(k,i) = zero
        xtvo_ad(k,i) = zero
        xzo_ad(k,i) = zero
        xtol_ad(k,i) = zero
        xqol_ad(k,i) = zero
        xqesol_ad(k,i) = zero
        xheo_ad(k,i) = zero
        xheso_ad(k,i) = zero
        xhcko_ad(k,i) = zero
        xqcko_ad(k,i) = zero
        xqrcd_ad(k,i) = zero
        to2_ad(k,i) = zero
        qo2_ad(k,i) = zero
        qeso2_ad(k,i) = zero
        to3_ad(k,i) = zero
        qo3_ad(k,i) = zero
        qo0_ad(k,i)=zero
        xqol0_ad(k,i)=zero
        uo2_ad(k,i)=zero
        vo2_ad(k,i)=zero
        xqo0_ad(k,i)=zero
        ucko_ad(k,i)=zero
        vcko_ad(k,i)=zero
        ukcod_ad(k,i)=zero
        vckod_ad(k,i)=zero
        dellau_ad(k,i)=zero
        dellav_ad(k,i)=zero
        etau_ad(k,i)=zero
        sumz_ad(k,i)=zero
        sumh_ad(k,i)=zero
        qol0_ad(k,i)=zero
!
        p(k,i) = zero
        to(k,i) = zero
        qo(k,i) = zero
        to2(k,i) = zero
        qo2(k,i) = zero
        qeso(k,i) = zero
        qeso2(k,i) = zero
        to3(k,i) = zero
        qo3(k,i) = zero
        tvo(k,i) = zero
        dbyo(k,i) = zero
        zo(k,i) = zero
        heo(k,i) = zero
        heso(k,i) = zero
        xqo(k,i) = zero
        xqeso(k,i) = zero
        xto(k,i) = zero
        xtvo(k,i) = zero
        xzo(k,i) = zero
        xheo(k,i) = zero
        xheso(k,i) = zero
        tol(k,i) = zero
        qol(k,i) = zero
        qesol(k,i) = zero
        heol(k,i) = zero
        hesol(k,i) = zero
        uol(k,i) = zero
        vol(k,i) = zero
        xtol(k,i) = zero
        xqol(k,i) = zero
        xqesol(k,i) = zero
        qrcd(k,i) = zero
        dellah(k,i) = zero
        dellaq(k,i) = zero
        hcko(k,i) = zero
        qcko(k,i) = zero
        eta(k,i) = zero
        hcko0(k,i) = zero
        qcko0(k,i) = zero
        qcko00(k,i)= zero
        dqk(k,i) = zero
        eta0(k,i) = zero
        etad(k,i) = zero
        xhcko(k,i) = zero
        xqcko(k,i) = zero
        xqcko0(k,i) = zero
        xtemp0(k,i) = zero
        xqrch0(k,i) = zero
        qrcdo(k,i) = zero
        pwo(k,i) = zero
        pwdo(k,i) = zero
        hcko1(k,i) = zero
        hckod(k,i) = zero
        xqrcd(k,i) = zero
        rn0k(k,i) = zero
        delqevk(k,i) = zero
        flgk(k,i) = .true.
!new
        sumz(k,i) = zero
        sumh(k,i) = zero
        ucko(k,i) = zero
        vcko(k,i) = zero
        etau(k,i) = zero
        uckod(k,i)= zero
        vckod(k,i)=zero
        hckod(k,i)=zero
        
     end do
  end do
  do i = 1,ix
     xlamdet1_ad(i)=zero
     xlamdet0_ad(i)=zero
     ukbo_ad(i)=zero
     vkbo_ad(i)=zero
     ucdo_ad(i)=zero
     vcdo_ad(i)=zero
     qlko_ktcon_ad(i)=zero
     dellal_ad(i)=zero
     dellal1_ad(i)=zero
     hkbo_ad(i) = zero
     xlamb_ad(i) = zero
     xlamdet_ad(i) = zero
     aa1_ad(i) = zero
     pwavo_ad(i) = zero
     tcwfup_ad(i) = zero
     tcwfdn_ad(i) = zero
     vshear_ad(i) = zero
     edt_ad(i) = zero
     edto_ad(i) = zero
     edtx_ad(i) = zero
     edto1_ad(i) = zero
     edtx1_ad(i) = zero
     xlamd_ad(i) = zero
     pwevo_ad(i) = zero
     qcdo_ad(i) = zero
     hcdo_ad(i) = zero
     xaa0_ad(i) = zero
     xpwav_ad(i) = zero
     xpwev_ad(i) = zero
     xhcd_ad(i) = zero
     pdot_ad(i) = zero
     acrtfct1_ad(i) = zero
     acrtfct_ad(i) = zero
     dtconv_ad(i) = zero
     f_ad(i) = zero
     xk_ad(i) = zero
     xmb_ad(i) = zero
     rntot_ad(i) = zero
     
     dpscl(i) = zero
     dpcld(i) = zero
     tdpscl(i) = zero
     tdpcld(i) = zero
     tdetrn(i) = zero
     tcwfup(i) = zero
     tcwfdn(i) = zero
     acrtfct(i) = zero
     acrtfct0(i) = zero
     acrtfct1(i) = zero
     acrt(i) = zero
     psfc(i) = zero
     hmax(i) = zero
     kb(i) = 0
     hkbo(i) = zero
     qkbo(i) = zero
     kbcon(i) = 0
     pbcdif(i) = zero
     hmin(i) = zero
     lmin(i) = 0
     jmin(i) = 0
     pwavo(i) = zero
     aa1(i) = zero
     vshear(i) = zero
     edt(i) = zero
     edt0(i) = zero
     edto(i) = zero
     edto1(i) = zero
     edto10(i) = zero
     pwevo(i) = zero
     hcdo(i) = zero
     qcdo(i) = zero
     xhkb(i) = zero
     xqkb(i) = zero
     xpwav(i) = zero
     xpwev(i) = zero
     xhcd(i) = zero
     xaa0(i) = zero
     f(i) = zero
     xk(i) = zero
     xmb(i) = zero
     xmb0(i) = zero
     ktcon(i) = 0
     edtx(i) = zero
     edtx1(i) = zero
     edtx10(i) = zero
     xqcd(i) = zero
     hsbar(i) = zero
     xmbmax(i) = zero
     xlamb(i) = zero
     xlamdet(i) = zero
     xlamdet0(i)=zero
     xlamdet1(i)=zero
     xlamd(i) = zero
     kbdtr(i) = 0
     pdot(i) = zero
     cldwrk(i) = zero
     kbot(i) = km+1
     ktop(i) = 0
     kuo(i) = 0
     rn1(i) = zero
     aa10(i) = zero
     rn11(i) = zero
     qlko_ktcon(i)=zero
  end do


! Initialize arrays
  do i = 1,im
     cnvflg(i)  = .true.
     dwnflg(i)  = .true.
     dwnflg2(i) = .true.
     flg(i)     = .true.
     dtconv(i)  = r3600
     dtconv0(i) = dtconv(i)
     xmbmax(i)  = one_tenth
  enddo
  do k = 1, 15
     acrit(k) = acritt(k) * (975._r_kind - pcrit(k))
  enddo
  dt2   = delt
  dtmin = max(dt2,r1200)
  dtmax = max(dt2,r3600)

! Model tunable parameters are all here
  mbdt    = 10._r_kind
  edtmaxl = 0.3_r_kind
  edtmaxs = 0.3_r_kind
  alphal  = half
  alphas  = half
  betal   = 0.15_r_kind
  betas   = 0.15_r_kind
  betal   = 0.05_r_kind
  betas   = 0.05_r_kind
  evfact = 0.3_r_kind
  pdpdwn  = zero
  pdetrn  = 200._r_kind
  xlambu  = 1.e-4_r_kind
  fjcap = (real(jcap,r_kind) / 126._r_kind) ** 2
  val   =  one
  fjcap = max(fjcap,val)
  fkm = (real(km,r_kind) / 28._r_kind) ** 2
  fkm = max(fkm,one)
  w1l = -8.e-3_r_kind
  w2l = -4.e-2_r_kind
  w3l = -5.e-3_r_kind
  w4l = -5.e-4_r_kind
  w1s = -2.e-4_r_kind
  w2s = -2.e-3_r_kind
  w3s = -1.e-3_r_kind
  w4s = -2.e-5_r_kind

!  Define top layer for search of the downdraft originating layer
!  and the maximum thetae for updraft

  do i=1,im
     kbmax(i) = km
     kbm(i)   = km
     kmax(i)  = km
     do k = 1, km
        if (sl(k,i)  >  0.45_r_kind) kbmax(i) = k + 1
        if (sl(k,i)  >  0.7_r_kind)  kbm(i)   = k + 1
        if (sl(k,i)  >  0.04_r_kind) kmax(i)  = k + 1
     enddo
  end do
! kbm=10, kbmax=14, kmax=25 (=23 if sl(k) > 06)


! Convert surface pressure to mb from cb
  do i = 1, im
     psfc(i) = ps(i) * 10._r_kind
  enddo
  do i = 1,im
     do k = 1,km
        p(k,i)  = psfc(i) * sl(k,i)
        to(k,i) = t0(k,i)
!       qo(k,i) = q0(k,i)
        if (q0(k,i)>zero) then
           qo(k,i) = q0(k,i)
        else
           qo(k,i) = tiny_r_kind
        endif
        uo(k,i) = u0(k,i)
        vo(k,i) = v0(k,i)
        cwmo(k,i)= cwm0(k,i)
        
        qo0(k,i) = q0(k,i)
        
     enddo
  enddo

! Column variables
! p is pressure of the layer (mb)
! t is temperature at t-dt (k)..tn
! q is specific humidity at t-dt (kg/kg)..qn
! to is temperature at t+dt (k)... this is after advection and turbulan
! qo is specific humidity at t+dt (kg/kg)..q1
  do i = 1,im
     do k = 1, kmax(i)
        call fpvsx_ad(to(k,i),es0,adt,es0_ad,.false.)
        es = 10.0_r_kind*es0
        qeso(k,i) = eps*es / (p(k,i) + epsm1*es)
        
!new code bounds qeso to be >= 1.e-8, qo>=1.e-10

        tvo(k,i)  = to(k,i) + delta * to(k,i) * qo(k,i)
     enddo
  enddo

! Hydrostatic height assume zero terr
  do i = 1, im
     dlnsig = log(sl(1,i))
     zo(1,i) = zero - dlnsig * rd / grav * tvo(1,i)
  enddo

!round
!     reverse order of this loop leads to slightly different 
!     final results.  change order of loop in sascnv2 to
!     restore similar results.
  do i = 1,im
     do k = 2, kmax(i)
        dlnsig  = log(sl(k,i) / sl(k-1,i))
        term1   = dlnsig * rd / grav
        term2   = half * (tvo(k,i) + tvo(k-1,i))
        zo(k,i) = zo(k-1,i) - term1*term2
     enddo
  enddo

! Compute moist static energy
  do i = 1,im
     do k = 1, kmax(i)
        heo(k,i)  = grav * zo(k,i) + cp * to(k,i) + hvap * qo(k,i)
        heso(k,i) = grav * zo(k,i) + cp * to(k,i) + hvap * qeso(k,i)
     enddo
  enddo

! Determine level with largest moist static energy
! this is the level where updraft starts
  do i = 1, im
     hmax(i) = heo(1,i)
     kb(i) = 1
  enddo
  do i = 1,im
     do k = 2, kbm(i)
        if (heo(k,i) > hmax(i)) then
           kb(i) = k
           hmax(i) = heo(k,i)
        endif
     enddo
  enddo

! Search for downdraft originating level above theta-e minimum
!      do i = 1, im
!         hmin(i) = heso(1,i)
!         lmin(i) = kbmax(i)
!         jmin(i) = kbmax(i)
!      enddo
!      do i = 1,im
!         do k = 2, kbmax(i)
!            if (heso(k,i) < hmin(i)) then
!               lmin(i) = k + 1
!               hmin(i) = heso(k,i)
!            endif
!         enddo
!      enddo
  do i = 1,im
     do k = 1, kmax(i) - 1
        dz = half * (zo(k+1,i) - zo(k,i))
        dp = half * (p(k+1,i) - p(k,i))
        call fpvsx_ad(to(k+1,i),es0,adt,es0_ad,.false.)
        es = 10._r_kind*es0
        pprime = p(k+1,i) + epsm1 * es
        qs     = eps * es / pprime
        dqsdp  = - qs / pprime
        desdt = es * (factor1 / to(k+1,i) + factor2 / (to(k+1,i)**2))
        dqsdt = qs * p(k+1,i) * desdt / (es * pprime)
        gamma = el2orc * qeso(k+1,i) / (to(k+1,i)**2)
        dt    = (grav * dz + hvap * dqsdp * dp) / (cp * (one + gamma))
        dq    = dqsdt * dt + dqsdp * dp
        tol(k,i) = to(k+1,i) + dt
        qol0(k,i) = qo(k+1,i) + dq
        if (qol0(k,i)>zero) then
           qol(k,i) = qol0(k,i)
        else
           qol(k,i) = tiny_r_kind   
        endif
     end do
  end do

  do i = 1,im
     do k = 1,kmax(i)-1
        po    = half * (p(k,i) + p(k+1,i))
        call fpvsx_ad(tol(k,i),es0,adt,es0_ad,.false.)
        esl   = 10.0_r_kind*es0
        qesol(k,i) = eps*esl / (po + epsm1*esl)
        heol(k,i)  = half * grav * (zo(k,i) + zo(k+1,i)) + &
             cp * tol(k,i) + hvap * qol(k,i)
        hesol(k,i) = half * grav * (zo(k,i) + zo(k+1,i)) + &
             cp * tol(k,i) + hvap * qesol(k,i)
        uol(k,i)   = half * (uo(k,i) + uo(k+1,i))
        vol(k,i)   = half * (vo(k,i) + vo(k+1,i))
     enddo
  enddo
  do i = 1, im
     k = kmax(i)
     heol(k,i) = heo(k,i)
     hesol(k,i) = heso(k,i)
  enddo

! Look for convective cloud base as the level of free convection
  do i = 1, im
     indx     = kb(i)
     hkbo(i)  = heol(indx,i)
     ukbo(i)  = uol(indx,i)
     vkbo(i)  = vol(indx,i)
     kbcon(i) = kmax(i)
     flg(i)   = .true.
  enddo
  do i = 1,im
     do k = 1, kbmax(i)
        if (flg(i) .and. k > kb(i)) then
           hsbar(i) = hesol(k,i)
           if (hkbo(i) > hsbar(i)) then
              flg(i)   = .false.
              kbcon(i) = k
           endif
        endif
     enddo
  enddo
  do i = 1, im
     dpscl(i) = -p(kbcon(i),i) + p(kb(i),i)
!         if (abs(dpscl(i)) > tiny_r_kind) then
!            tdpscl(i) = funsqr(150.,dpscl(i),1)
!         else
!            tdpscl(i) = one
!         endif
     tdpscl(i) = one
     pdot(i) = 10._r_kind* dot0(kbcon(i),i)
     if (dpscl(i) > 150._r_kind) tdpscl(i) = zero
     if (kbcon(i)==kmax(i)) tdpscl(i) = zero
  enddo

! Found LFC, can define rest of variables
! determine entrainment rate between kb and kbcon
  do i = 1, im
     alpha = alphas
     if (nint(slimsk(i)) == 1) alpha = alphal
     if (kb(i)==1) then
        dz = half * (zo(kbcon(i),i) + zo(kbcon(i)-1,i)) - zo(1,i)
     else
        dz = half * (zo(kbcon(i),i) + zo(kbcon(i)-1,i)) &
             - half * (zo(kb(i),i) + zo(kb(i)-1,i))
     endif
     if (kbcon(i)/=kb(i)) then
        xlamb(i) = -log(alpha) / dz
     else
        xlamb(i) = zero
     endif
  enddo

! Determine updraft mass flux
  do i = 1,im
     do k = 1, kmax(i)
        eta(k,i) = one
        etau(k,i)= one
     enddo
  enddo
  do i = 1,im
     do k = kbmax(i), 2, -1
        if (k < kbcon(i) .and. k >= kb(i)) then
           dz       = half * (zo(k+1,i) - zo(k-1,i))
           eta(k,i) = eta(k+1,i) * exp(-xlamb(i) * dz)
           etau(k,i)= eta(k,i)
        endif
     enddo
  enddo
  do i = 1, im
     if (kb(i)==1 .and. kbcon(i) > 1) then
        dz = half * (zo(2,i) - zo(1,i))
        eta(1,i) = eta(2,i) * exp(-xlamb(i) * dz)
        etau(1,i)= eta(1,i)
     endif
  enddo

! Work up updraft cloud properties
  do i = 1, im
     indx = kb(i)
     hcko(indx,i) = hkbo(i)
     ucko(indx,i) = ukbo(i)
     vcko(indx,i) = vkbo(i)
  enddo

! Cloud property below cloud base is modified by the entrainment proces
  do i = 1,im
     do k = 2, kmax(i) - 1
        if (k > kb(i) .and. k <= kbcon(i)) then
           factor = eta(k-1,i) / eta(k,i)
           onemf = one - factor
           hcko(k,i) = factor * hcko(k-1,i) + onemf * &
                half * (heol(k,i) + heol(k+1,i))
           ucko(k,i) = factor * ucko(k-1,i) + onemf * &
                half * (uol(k,i) + uol(k+1,i))
           vcko(k,i) = factor * vcko(k-1,i) + onemf * &
                half * (vol(k,i) + vol(k+1,i))
        endif
        if (k > kbcon(i)) then
           hcko(k,i) = hcko(kbcon(i),i)
           ucko(k,i) = ucko(kbcon(i),i)
           vcko(k,i) = vcko(kbcon(i),i)
        endif
     enddo
  enddo

! Load eta into new work array to preserve previous value
  do i = 1,im
     do k = 1,km
        eta0(k,i) = eta(k,i)
        hcko0(k,i) = hcko(k,i)
        ucko0(k,i) = ucko(k,i)
        vcko0(k,i) = vcko(k,i)
        etau0(k,i) = etau(k,i)
     end do
  end do

! Determine cloud top
  do i = 1, im
     flg(i) = .true.
     ktcon(i) = 1
  enddo
  do i = 1,im
     do k = 2, kmax(i)
        dif = hcko(k,i) - hesol(k,i)
        if (dif < zero .and. flg(i) .and. k > kbcon(i)) then
           ktcon(i) = k
           flg(i) = .false.
        endif
     enddo
  enddo


! Make sure that jmin is within the cloud
!new
! Search for downdraft origin level above theta-e minimum
  do i=1,im
     hmin(i) = heol(kbcon(i),i)
     lmin(i) = kbmax(i)
     jmin(i) = kbmax(i)
     if(cnvflg(i)) then
        do k = kbcon(i), kbmax(i)
           if(heol(k,i) < hmin(i)) then
              lmin(i) = k + 1
              hmin(i) = heol(k,i)
           endif
        enddo
     endif
  end do
  
! Make sure jmin is within the cloud      
  do i = 1, im
     jmin(i) = min(lmin(i),ktcon(i)-1)
     jmin(i) = max(jmin(i),kbcon(i)+1)
     if (ktcon(i) < kbcon(i)) jmin(i) = 1
  enddo

  do i = 1, im
     dpcld(i)  = p(kbcon(i),i) - p(ktcon(i),i)
!        tdpcld(i) = funsqr(dpcld(i),150.,1)
     tdpcld(i) = one
     if (dpcld(i) < 150._r_kind) tdpcld(i) = zero
  enddo

!new
!  KTCON is model level of deepest cloud in ensemble.
!  Now randomly select cloud from ensemble of clouds
!  ranging from one level above the downdraft origin to
!  to level of neutral buoyancy (ktcon)

  do i=1,im
     do k=2,kmax(i)-1
        if (k > jmin(i) .and. k <= ktcon(i)) then
           sumz(k,i) = sumz(k-1,i) + half * (zo(k+1,i) - zo(k-1,i))
           sumh(k,i) = sumh(k-1,i) + half * (zo(k+1,i) - zo(k-1,i)) &
                * heol(k,i)
        endif
     end do
  end do

! Select cloud from ensemble
  do i=1,im
     kt2(i) = nint(xkt2(i)*real(ktcon(i)-jmin(i),r_kind)-half) + jmin(i) + 1
     tem1 = hcko(jmin(i),i) - hesol(kt2(i),i)
     tem2 = sumz(kt2(i),i) * hesol(kt2(i),i) - sumh(kt2(i),i)
     if (abs(tem2)  >  0.000001_r_kind) then
        xlamdet0(i) = tem1 / tem2
     else
        xlamdet0(i) = zero
        cnvflg(i) = .false.
     endif
     if (xlamdet0(i) >= zero) then
        xlamdet1(i) = xlamdet0(i)
     else
        xlamdet1(i) = zero
     endif
     if (sumz(kt2(i),i)  >  0.000001_r_kind) then
        term = 2.3_r_kind/sumz(kt2(i),i)
        if (xlamdet1(i) < term) then
           xlamdet(i) = xlamdet1(i)
        else
           xlamdet(i) = term
        endif
     else
        xlamdet(i) = xlamdet1(i)
     endif
  end do

! Apply logic to determine if convection is possible
  do i=1,im
     if (kt2(i) >= ktcon(i)) dwnflg(i) = .false.
     if(xlamdet(i) <= 1.e-30_r_kind .or. &
          hcko(jmin(i),i)-hesol(kt2(i),i) <= 1.e-30_r_kind) &
          dwnflg(i) = .false.
     do k = jmin(i), kt2(i)
        if(dwnflg(i) .and. &
             heol(k,i) > hesol(kt2(i),i)) dwnflg(i)=.false.
     enddo
  end do
         
         


! Detraining cloud
  do i = 1, im
!        if (abs(dpcld(i)) > tiny_r_kind) then
!           tdetrn(i) = funsqr(pdetrn,dpcld(i),1)
!        else
!           tdetrn(i) = one
!        endif
     tdetrn(i) = one
     if (dpcld(i) < pdpdwn) dwnflg2(i)=.false.
!        if (abs(pdpdwn) > tiny_r_kind) then
!           tdpdwn(i) = funsqr(dpcld(i),pdpdwn,1)
!        else
!           tdpdwn(i) = one
!        endif
     tdpdwn(i) = one
     if (dwnflg2(i) .and. jmin(i) <= kbcon(i)) then
        dwnflg2(i) = .false.
        tdetrn(i)  = zero
        tdpdwn(i)  = zero
     endif
     
  enddo

! Cloud property above cloud top is modified by the detrainment process
  do i=1,im
     if (dwnflg(i)) then
        do k=2,kmax(i)-1
           if (k > jmin(i) .and. k <= kt2(i)) then
              dz = half * (zo(k+1,i) - zo(k-1,i))
!   To simplify matters, we will take the linear approach here
              eta(k,i)  = eta(k-1,i)  * (one + xlamdet(i) * dz)
              etau(k,i) = etau(k-1,i) * (one +(xlamdet(i)+xlambu)* dz)
           endif
        end do
     else
        do k=2,kmax(i)-1
           if (k > jmin(i) .and. k <= ktcon(i)) then
              dz = half * (zo(k+1,i) - zo(k-1,i))
              etau(k,i) = etau(k-1,i) * (one + xlambu * dz)
           endif
        end do
     endif
     
  enddo
  do i=1,im
     if (dwnflg(i)) then
        ktcon0(i) = ktcon(i)
        ktcon(i) = kt2(i)
     else
        ktcon0(i) = ktcon(i)
     endif
  end do

! Load hcko at cloud base into hckod
  do i = 1,im
     if (dwnflg(i)) then
        indx = kbcon(i)
        hckod(indx,i) = hcko(indx,i)
        uckod(indx,i) = ucko(indx,i)
        vckod(indx,i) = vcko(indx,i)
     endif
  end do

! Adjustment to hckod by detraining cloud
  do i = 1,im
     do k = 2,kmax(i)-1
        if (k > kbcon(i) .and. k <= ktcon(i)) then
           factor = eta(k-1,i) / eta(k,i)
           onemf  = one - factor
           fuv    = etau(k-1,i)/etau(k,i)
           onemfu = one - fuv
           heol2  = half*(heol(k,i) + heol(k+1,i))
           hckod(k,i) = factor*hckod(k-1,i) + onemf*heol2
           
           uol2       = half*(uol(k,i)+uol(k+1,i))
           uckod(k,i) = fuv * uckod(k-1,i) + onemfu*uol2
           
           vol2       = half*(vol(k,i)+vol(k+1,i))
           vckod(k,i) = fuv * vckod(k-1,i) + onemfu*vol2
           
        endif
     end do
  end do

! Load hckod and hcko into hcko1 and calculate dbyo
  do i=1,im
     do k=2,kmax(i)-1
        if (k > kb(i).and.k <= kbcon(i)) then
           hcko1(k,i) = hcko(k,i)
        elseif (k > kbcon(i).and.k <= ktcon(i)) then
           if (.not.dwnflg(i)) then
              hcko1(k,i) = hcko(k,i)
           else
              hcko1(k,i) = hckod(k,i)
           endif
        elseif (k > ktcon(i)) then
           hcko1(k,i)  = hcko(k,i)
        endif
        if (k > kb(i)) then
           dbyo(k,i) = hcko1(k,i) - hesol(k,i)
        endif
     end do
  end do


! Compute cloud moisture property and precipitation
  do i = 1, im
     indx = kb(i)
     qcko(indx,i) = qol(indx,i)
  enddo
  do k = 1,km
     do i = 1,im
        qcko0(k,i) = qcko(k,i)
     end do
  end do
  do i = 1,im
     do k = 1, kmax(i)
        if (k > kb(i) .and. k < ktcon(i)) then
           factor = eta(k-1,i) / eta(k,i)
           onemf  = one - factor
           temp   = factor * qcko(k-1,i) + onemf * &
                half * (qol(k,i) + qol(k+1,i))
           qcko0(k,i) = temp
           gamma  = el2orc * qesol(k,i) / (tol(k,i)**2)
           qrch   = qesol(k,i) &
                + gamma * dbyo(k,i) / (hvap * (one + gamma))
           dq = eta(k,i) * (temp - qrch)
           dqk(k,i) = dq

!          Below LFC check if there is excess moisture to release latent heat
           if (dq > zero) then
              dz   = half * (zo(k+1,i) - zo(k-1,i))
              dz1  = (zo(k,i) - zo(k-1,i))
              etah = half * (eta(k,i) + eta(k-1,i))
              qlk  = dq / (eta(k,i) + etah * c0 * dz)
              aa1(i)    = aa1(i) - dz1 * grav * qlk
              pwo(k,i)  = etah * c0 * dz * qlk
              pwavo(i)  = pwavo(i) + pwo(k,i)
              qc        = qlk + qrch
              qcko(k,i) = qc
           else
              qcko(k,i) = temp   
           endif
        endif
     enddo
  enddo

! This section is for cloud liquid water
  if (ncloud > 0) then
     do i=1,im
        k = ktcon(i)
        if (cnvflg(i)) then
           gamma = el2orc * qesol(k,i) / (tol(k,i)**2)
           qrch = qesol(k,i) &
                + gamma * dbyo(k,i) / (hvap * (one + gamma))
           dq = qcko(k-1,i) - qrch
           qcko00(k-1,i) = qcko(k-1,i)

!          Check if there is excess moisture to release latent heat
           if(dq > zero) then
              qlko_ktcon(i) = dq
              qcko(k-1,i) = qrch
           endif
        endif
     end do
  endif
               

! Calculate cloud work function at t+dt
  do i = 1,im
     do k = 1, kmax(i)
        if (k > kbcon(i) .and. k <= ktcon(i)) then
           dz1    = zo(k,i) - zo(k-1,i)
           gamma  = el2orc * qesol(k-1,i) / (tol(k-1,i)**2)
           rfact  =  one + delta * cp * gamma &
                * tol(k-1,i) / hvap
           term1 = dz1*grav/cp
           term2 = one/tol(k-1,i)
           term3 = dbyo(k-1,i)
           term4 = one/(one+gamma)
           term5 = rfact
           aa1(i) = aa1(i) + term1*term2*term3*term4*term5
           
           dqs    = qesol(k-1,i) - qol(k-1,i)
           if (dqs > zero) then
              aa1(i) = aa1(i)+ dz1 * grav * delta * dqs
           endif
        endif
     enddo
  enddo

! Set cloud mass flux threshold function based on updaft cwf.
  do i = 1,im
     term    = 1.e3_r_kind*aa1(i)
     aa10(i) = aa1(i)
     if (abs(term)  <  100._r_kind) then
        tcwfup(i) = ftanh(term)
     else
        if (term  <  -100._r_kind) then
           tcwfup(i) = zero
        else
           tcwfup(i) = one
        endif
     endif
  end do


! ----Downdraft calculations follow----

! Determine downdraft strength in terms of windshear
  do i = 1,im
     do k = 1, kmax(i)
        if (k >= kb(i) .and. k <= ktcon(i)) then
           termu = (uol(k+1,i) - uol(k,i))**2
           termv = (vol(k+1,i) - vol(k,i))**2
           if (termu+termv > tiny_r_kind) then
              shear = rcs(i) * sqrt(termu+termv)
           else
              shear = zero
           endif
           vshear(i)= vshear(i) + shear
        endif
     enddo
  enddo
  do i = 1, im
     dz = zo(ktcon(i),i)-zo(kb(i),i)
     if (abs(dz) > tiny_r_kind) then
        term   = 1.e3_r_kind * vshear(i) / dz
        e1     = pcpeff0 + pcpeff1*term + pcpeff2*term**2 + pcpeff3*term**3
        edt(i) = one_tenth
     else
        edt(i) = zero
     endif
     edt0(i) = edt(i)
  end do

! Impose upper and lower bounds on precipitation efficiency
  do i = 1,im
     if (edt(i) > 0.9_r_kind) then
        edt(i) = 0.9_r_kind
     elseif (edt(i) < zero) then
        edt(i) = zero
     endif
     edto(i) = edt(i)
     edtx(i) = edt(i)
  enddo

! Determine detrainment rate between 1 and kbdtr
  do i = 1, im
     kbdtr(i) = kbcon(i)
     beta = betas
     if (nint(slimsk(i)) == 1) beta = betal
     kbdtr(i) = kbcon(i)
     kbdtr(i) = max(kbdtr(i),1)
     xlamd(i) = zero
     if (kbdtr(i) > 1) then
        dz = half * zo(kbdtr(i),i) + half * zo(kbdtr(i)-1,i) &
             - zo(1,i)
        xlamd(i) = log(beta) / dz
     endif
  enddo

! Determine downdraft mass flux
  do i = 1,im
     do k = 1, kmax(i)
        etad(k,i) = one
     enddo
  enddo
  do i = 1,im
     do k = kbmax(i), 2, -1
        if (k < kbdtr(i)) then
           dz = half * (zo(k+1,i) - zo(k-1,i))
           etad(k,i) = etad(k+1,i) * exp(xlamd(i) * dz)
        endif
     enddo
  enddo
  k = 1
  do i = 1, im
     if (kbdtr(i) > 1) then
        dz = half * (zo(2,i) - zo(1,i))
        etad(k,i) = etad(k+1,i) * exp(xlamd(i) * dz)
     endif
  enddo

! Downdraft moisture properties
  do i = 1,im
     do k = 1,km
        qrcdo(k,i) = zero
        pwdo(k,i) = zero
     end do
  end do
  do i = 1, im
     pwevo(i) = zero
     jmn     = jmin(i)
     hcdo(i) = heol(jmn,i)
     qrcdo(jmn,i) = qesol(jmn,i)
     ucdo(i) = uol(jmn,i)
     vcdo(i) = vol(jmn,i)
  enddo
  do i = 1,im
     do k = kmax(i)-1, 1, -1
        if (k < jmin(i)) then
           dq    = qesol(k,i)
           dt    = tol(k,i)
           gamma = el2orc * dq / dt**2
           dh    = hcdo(i) - hesol(k,i)
           term5 = (one/hvap) * gamma
           term6 = one/(one+gamma)
           qrcdo(k,i) = dq + term5*term6*dh
           detad = etad(k+1,i) - etad(k,i)
           if (k < jmin(i)-1) then
              term1 = etad(k+1,i)*qcdo(i)
              term2 = etad(k,i) * qrcdo(k,i)
              term3 = detad*half*(qrcdo(k,i)+qrcdo(k+1,i))
              pwdo(k,i) = term1 - term2 - term3
           endif
           pwevo(i) = pwevo(i) + pwdo(k,i)
        endif
     enddo
  enddo
  do i = 1,im
     qcdo(i) = qrcdo(1,i)
  end do

! Final downdraft strength dependent on precip
! efficiency (edt), normalized condensate (pwav), and
! evaporate (pwev)
  do i = 1, im
     edtmax = edtmaxl
     if (nint(slimsk(i)) == 0) edtmax = edtmaxs
     if (dwnflg2(i)) then
        if (pwevo(i) < zero) then
           edto1(i) = -edto(i) * pwavo(i) / pwevo(i)
           edto10(i) = edto1(i)
           if (edto1(i) > edtmax) then
              edto1(i) = edtmax
           endif
        else
           edto1(i) = zero
        endif
     else
        edto1(i)   = zero
     endif
  enddo


! Downdraft cloudwork functions
!round reverse order of loop --> get slightly different result
  do i = 1,im
     do k = kmax(i)-1, 1, -1
        if (dwnflg2(i) .and. k < jmin(i)) then
           gamma = el2orc * qesol(k+1,i) / tol(k+1,i)**2
           dhh   = hcdo(i)
           dt    = tol(k+1,i)
           dg    = gamma
           dh    = hesol(k+1,i)
           dz    = zo(k,i) - zo(k+1,i)
           
           term1 = edto1(i)
           term2 = dz
           term3 = grav/(cp*dt)
           term4 = dhh-dh
           term5 = one/(one+dg)
           term6 = one + delta*cp*dg*dt/hvap
           aa1(i) = aa1(i) &
                + term1*term2*term3*term4*term5*term6
           
           dqs   = qesol(k+1,i)-qol(k+1,i)
           if (dqs > zero) then
              aa1(i)= aa1(i) + edto1(i)*dz*grav*delta*dqs
           endif
        endif
     enddo
  enddo

! Compute threshold function based on final cloud work function
  do i = 1,im
     term = 1.e3_r_kind*aa1(i)
     if (abs(term)  <  100._r_kind) then
        tcwfdn(i) = ftanh(term)
     else
        if (term  <  -100._r_kind) then
           tcwfdn(i) = zero
        else
           tcwfdn(i) = one
        endif
     endif
  end do


! What would the change be, that a cloud with unit mass
! will do to the environment?
  do i = 1,im
     do k = 1, kmax(i)
        dellah(k,i) = zero
        dellaq(k,i) = zero
        dellau(k,i) = zero
        dellav(k,i) = zero
     enddo
  enddo
  do i = 1, im
     if (ktcon(i)/=1) then
        dp = 100.0_r_kind * psfc(i) * del(1,i)
        dellah(1,i) = edto1(i) * etad(1,i) * (hcdo(i) &
             - heol(1,i)) * grav / dp
        dellaq(1,i) = edto1(i) * etad(1,i) * (qcdo(i) &
             - qol(1,i)) * grav / dp
        dellau(1,i) = edto1(i) * etad(1,i) * (ucdo(i) &
             - uol(1,i))  * grav / dp
        dellav(1,i) = edto1(i) * etad(1,i) * (vcdo(i) &
             - vol(1,i))  * grav / dp
     endif
  enddo

! Changed due to subsidence and entrainment
!round reverse order of loop yields different results
  do i = 1,im
     do k = 2, kmax(i)-1
        if ( k < ktcon(i) .and. ktcon(i)/=1 ) then
           aup = one
           if (k <= kb(i)) aup = zero
           adw = one
           if (k > jmin(i)) adw = zero
           dv1   = heol(k,i)
           dv2   = half * (heol(k,i) + heol(k+1,i))
           dv3   = heol(k-1,i)
           dv1q  = qol(k,i)
           dv2q  = half * (qol(k,i) + qol(k+1,i))
           dv3q  = qol(k-1,i)
           
           dv1u  = uol(k,i)
           dv2u  = half * (uol(k,i) + uol(k+1,i))
           dv3u  = uol(k-1,i)
           
           dv1v  = vol(k,i)
           dv2v  = half * (vol(k,i) + vol(k+1,i))
           dv3v  = vol(k-1,i)
           
           
           dp    = 100.0_r_kind * psfc(i) * del(k,i)
           detau = eta(k,i) - eta(k-1,i)
           detad = etad(k,i) - etad(k-1,i)
           
           term1 = aup*eta(k,i) - adw*edto1(i)*etad(k,i)
           term2 = aup*detau
           term3 = aup*eta(k-1,i) - adw*edto1(i)*etad(k-1,i)
           term4 = adw*edto1(i)*detad
           termq = half*(qrcdo(k,i)+qrcdo(k-1,i))
           
           dellah(k,i) = dellah(k,i) + (grav/dp) * &
                (term1*dv1 - term3*dv3 - term2*dv2 + term4*hcdo(i))
           dellaq(k,i) = dellaq(k,i) + (grav/dp) * &
                ( term1*dv1q - term3*dv3q - term2*dv2q + term4*termq )
           
           dellau(k,i) = dellau(k,i) + (grav/dp) * &
                (term1*dv1u - term3*dv3u - term2*dv2u + term4*ucdo(i))
           dellav(k,i) = dellav(k,i) + (grav/dp) * &
                (term1*dv1v - term3*dv3v - term2*dv2v + term4*vcdo(i))
           
        endif
     enddo
  enddo

! Cloud top
  do i = 1, im
     if (ktcon(i)/=1) then
        indx = ktcon(i)
        dp   = 100.0_r_kind * psfc(i) * del(indx,i)
        dv1  = heol(indx-1,i)
        dellah(indx,i) = eta(indx-1,i) * &
             (hcko1(indx-1,i) - dv1) * grav / dp
        dvq1 = qol(indx-1,i)
        dellaq(indx,i) = eta(indx-1,i) * &
             (qcko(indx-1,i) - dvq1) * grav / dp
        
        dvu1 = uol(indx-1,i)
        dellau(indx,i) = eta(indx-1,i) * &
             (ucko(indx-1,i) - dvu1) * grav / dp
        dvv1 = vol(indx-1,i)
        dellav(indx,i) = eta(indx-1,i) * &
             (vcko(indx-1,i) - dvv1) * grav / dp
!cloud liquid water
        dellal(i) = eta(indx-1,i) * qlko_ktcon(i) *grav/dp
        
     endif
  enddo

! Final changed variable per unit mass flux
  do i = 1,im
     do k = 1, kmax(i)
        if ( k <= ktcon(i) .and. ktcon(i)/=1 ) then
           xqo0(k,i) = dellaq(k,i) * mbdt + qo0(k,i)
           if (xqo0(k,i)>zero) then
              xqo(k,i) = xqo0(k,i)
           else
              xqo(k,i) = tiny_r_kind
           endif
           dellat   = (dellah(k,i) - hvap * dellaq(k,i)) / cp
           xto(k,i) = dellat * mbdt + to(k,i)
        else
           xqo(k,i) = qo0(k,i)
           xto(k,i) = to(k,i)
        endif
     enddo
  enddo


! The above changed environment is now used to calulate the
! effect the arbitrary cloud (with unit mass flux) would have
! on the stability, which then is used to calculate the real 
! mass flux, necessary to keep this change in balance with !
! the large-scale destabilization.
!
! Environmental conditions again, first heights
  do i = 1,im
     do k = 1, kmax(i)
        call fpvsx_ad(xto(k,i),es0,adt,es0_ad,.false.)
        es = 10.0_r_kind*es0
        xqeso(k,i) = eps*es / (p(k,i) + epsm1*es)
        xtvo(k,i)  = xto(k,i) + delta * xto(k,i) * xqo(k,i)
     enddo
  enddo

! Hydrostatic height assume zero terr
  do i = 1, im
     dlnsig = log(sl(1,i))
     xzo(1,i) = zero - dlnsig * rd / grav * xtvo(1,i)
  enddo
  do i = 1,im
     do k = 2, kmax(i)
        dlnsig = log(sl(k,i) / sl(k-1,i))
        term1 = dlnsig * rd / grav
        term2 = half * (xtvo(k,i) + xtvo(k-1,i))
        xzo(k,i) = xzo(k-1,i) - term1*term2
     enddo
  enddo

! Moist static energy
  do i = 1,im
     do k = 1, kmax(i) - 1
        dz     = half * (xzo(k+1,i) - xzo(k,i))
        dp     = half * (p(k+1,i) - p(k,i))
        call fpvsx_ad(xto(k+1,i),es0,adt,es0_ad,.false.)
        es     = 10.0_r_kind*es0
        pprime = p(k+1,i) + epsm1 * es
        qs     = eps * es / pprime
        dqsdp  = - qs / pprime
        desdt = es * (factor1 / xto(k+1,i) + factor2 / (xto(k+1,i)**2))
        dqsdt = qs * p(k+1,i) * desdt / (es * pprime)
        gamma = el2orc * xqeso(k+1,i) / (xto(k+1,i)**2)
        dt    = (grav * dz + hvap * dqsdp * dp) / (cp * (one + gamma))
        dq    = dqsdt * dt + dqsdp * dp
        xtol(k,i) = xto(k+1,i) + dt
        xqol0(k,i) = xqo(k+1,i) + dq
        if (xqol0(k,i)>zero) then
           xqol(k,i) = xqol0(k,i)
        else
           xqol(k,i) = tiny_r_kind
        endif
     end do
  end do
  do i = 1,im
     do k = 1,kmax(i)-1
        po    = half * (p(k,i) + p(k+1,i))
        call fpvsx_ad(xtol(k,i),es0,adt,es0_ad,.false.)
        esl   = 10.0_r_kind*es0
        xqesol(k,i) = eps*esl / (po + epsm1*esl)
        xheo(k,i)   = half * grav * (xzo(k,i) + xzo(k+1,i)) + &
             cp * xtol(k,i) + hvap * xqol(k,i)
        xheso(k,i)  = half * grav * (xzo(k,i) + xzo(k+1,i)) + &
             cp * xtol(k,i) + hvap * xqesol(k,i)
     enddo
  enddo
  do i = 1, im
     k = kmax(i)
     xheo(k,i)  = grav*xzo(k,i) + cp*xtol(k,i) + hvap*xqol(k,i)
     xheso(k,i) = grav*xzo(k,i) + cp*xtol(k,i) + hvap*xqesol(k,i)
  enddo
  do i = 1, im
     indx = kb(i)
     xhcko(indx,i) = xheo(indx,i)
     xqcko(indx,i) = xqol(indx,i)
  enddo


! Static control

! Moisture and cloud work functions
  do i = 1,im
     do k = 2, kmax(i) - 1
        if (k > kb(i) .and. k <= ktcon(i)) then
           factor = eta(k-1,i) / eta(k,i)
           onemf  = one - factor
           xhcko(k,i) = factor * xhcko(k-1,i) + onemf * &
                half * (xheo(k,i) + xheo(k+1,i))
        endif
     enddo
  enddo

  do i = 1,im
     xaa0(i) = zero
     xpwav(i) = zero
  end do
  do i = 1,im
     do k = 2,kmax(i)-1
        if (k > kb(i) .and. k < ktcon(i)) then

!          xtemp calculation
           factor = eta(k-1,i)/eta(k,i)
           onemf  = one - factor
           termq  = half*(xqol(k,i) + xqol(k+1,i))
           xtemp  = factor*xqcko(k-1,i) + onemf*termq
           xtemp0(k,i) = xtemp

!          xqrch calculation
           xdby = xhcko(k,i) - xheso(k,i)
           if (xdby > zero) then
              gamma = el2orc*xqesol(k,i)/xtol(k,i)**2
              xqrch = xqesol(k,i) + gamma*xdby/(hvap*(one+gamma))
           else
              xqrch = xqesol(k,i)
           endif
           xqrch0(k,i) = xqrch

!          dq calculation
           dq = eta(k,i)*(xtemp-xqrch)

!          actions dependent upon sign of dq
           if (dq > zero) then
              dz   = half * (xzo(k+1,i) - xzo(k-1,i))
              dz1  = xzo(k,i) - xzo(k-1,i)
              etah = half * (eta(k,i) + eta(k-1,i))
              qlk  = dq / (eta(k,i) + etah * c0 * dz)
              xqc  = qlk + xqrch
              xqcko(k,i) = xqc
              xaa0(i)  = xaa0(i) - dz1*grav*qlk
              xpw      = etah * c0 * dz * qlk
              xpwav(i) = xpwav(i) + xpw
           else
              xqcko(k,i) = xtemp
           endif
!
        endif
     end do
  end do
!
  do i = 1,im
     do k = 2,kmax(i)-1
        if (k > kbcon(i) .and. k <= ktcon(i)) then
           dz1   = xzo(k,i) - xzo(k-1,i)
           gamma = el2orc * xqesol(k-1,i) / (xtol(k-1,i)**2)
           rfact =  one + delta * cp * gamma &
                * xtol(k-1,i) / hvap
           xdby  = xhcko(k-1,i) - xheso(k-1,i)
           term1 = dz1 * (grav/cp)
           term2 = one/xtol(k-1,i)
           term3 = xdby
           term4 = one/(one+gamma)
           term5 = rfact
           xaa0(i) = xaa0(i) &
                + term1*term2*term3*term4*term5
           dqs = xqesol(k-1,i) - xqol(k-1,i)
           if (dqs > zero) then
              xaa0(i) = xaa0(i)+ dz1 * grav * delta * dqs
           endif
        endif
     enddo
  enddo

! Downdraft calculations
! Downdraft moisture properties
  do i = 1, im
     xpwev(i) = zero
  enddo
  do i = 1, im
     if (dwnflg2(i)) then
        jmn     = jmin(i)
        xhcd(i) = xheo(jmn,i)
        xqrcd(jmn,i) = xqesol(jmn,i)
     endif
  enddo
  do i = 1,im
     do k = kmax(i)-1, 1, -1
        if (dwnflg2(i) .and. k < jmin(i)) then
           dq = xqesol(k,i)
           dt = xtol(k,i)
           gamma = el2orc*dq/dt**2
           dh = xhcd(i) - xheso(k,i)
           term5 = (one/hvap) * gamma
           term6 = one/(one+gamma)
           
           xqrcd(k,i) = dq + term5*term6*dh
           
           detad = etad(k+1,i) - etad(k,i)
           term1 = etad(k+1,i)*xqrcd(k+1,i)
           term2 = etad(k,i)*xqrcd(k,i)
           term3 = detad * half * (xqrcd(k,i) + xqrcd(k+1,i))
           xpwd = term1 - term2 - term3
           xpwev(i) = xpwev(i) + xpwd
        endif
     enddo
  enddo

  do i = 1, im
     edtmax = edtmaxl
     if (nint(slimsk(i)) == 0) edtmax = edtmaxs
     if (dwnflg2(i)) then
        if (xpwev(i) >= zero) then
           edtx1(i) = zero
        else
           edtx1(i) = -edtx(i) * xpwav(i) / xpwev(i)
           edtx10(i) = edtx1(i)
           if (edtx1(i) > edtmax) then
              edtx1(i) = edtmax
           endif
        endif
     else
        edtx1(i) = zero
     endif
  enddo

! Downdraft cloudwork functions
!round reverse order of loop yields slightly difference results
!      difference only in q and rn, not t
  do i = 1,im
     do k = kmax(i)-1, 1, -1
        if (dwnflg2(i) .and. k < jmin(i)) then
           gamma   = el2orc * xqesol(k+1,i) / xtol(k+1,i)**2
           dhh     = xhcd(i)
           dt      = xtol(k+1,i)
           dg      = gamma
           dh      = xheso(k+1,i)
           dz      = xzo(k,i) - xzo(k+1,i)
           
           term1 = edtx1(i)
           term2 = dz
           term3 = grav/(cp*dt)
           term4 = dhh-dh
           term5 = one/(one+dg)
           term6 = one + delta*cp*dg*dt/hvap
           dqs = xqesol(k+1,i)-xqol(k+1,i)
           
           xaa0(i) = xaa0(i) &
                + term1*term2*term3*term4*term5*term6
           
           if (dqs > zero) then
              xaa0(i) = xaa0(i) + edtx1(i)*dz*grav*delta*dqs
           endif
        endif
     enddo
  enddo

! Calculate critical cloud work function
  do i = 1, im
     acrt(i) = zero
     if (p(ktcon(i),i) < pcrit(15))then
        acrt(i) = acrit(15)*(975.0_r_kind-p(ktcon(i),i)) &
             /(975.0_r_kind-pcrit(15))
     elseif (p(ktcon(i),i) > pcrit(1))then
        acrt(i) = acrit(1)
     else
        k =  int((850.0_r_kind - p(ktcon(i),i))/50.0_r_kind) + 2
        k = min(k,15)
        k = max(k,2)
        acrt(i) = acrit(k)+(acrit(k-1)-acrit(k))* &
             (p(ktcon(i),i)-pcrit(k))/(pcrit(k-1)-pcrit(k))
     endif
  enddo
  do i = 1, im
     acrtfct(i) = one
     if (nint(slimsk(i)) == 1) then
        w1 = w1l
        w2 = w2l
        w3 = w3l
        w4 = w4l
     else
        w1 = w1s
        w2 = w2s
        w3 = w3s
        w4 = w4s
     endif
     if (pdot(i) <= w4) then
        acrtfct(i) = (pdot(i) - w4) / (w3 - w4)
     elseif (pdot(i) >= -w4) then
        acrtfct(i) = -one*(pdot(i) + w4) / (w4 - w3)
     else
        acrtfct(i) = zero
     endif
     acrtfct0(i) = acrtfct(i)

     if (acrtfct(i) < -one) then
        acrtfct(i) = -one
     endif
     if (acrtfct(i) > one) then
        acrtfct(i) = one
     endif

     acrtfct1(i) = one - acrtfct(i)

     dtconv(i)  = dt2 + (1800.0_r_kind - dt2) * &
          (pdot(i) - w2) / (w1 - w2)
     dtconv0(i) = dtconv(i)
     if (dtconv(i) < dtmin) then
        dtconv(i) = dtmin
     endif
     if (dtconv(i) > dtmax) then
        dtconv(i) = dtmax
     endif
  enddo

! Large scale forcing
  do i= 1, im
     f(i)  = (aa1(i) - acrt(i) * acrtfct1(i)) / dtconv(i)
     xk(i) = (xaa0(i) - aa1(i)) / mbdt

!    Kernel, cloud base mass flux
     if (abs(xk(i)) > tiny_r_kind) then
        ratio = -f(i)/xk(i)
     else
        ratio = zero
     endif
     if ( (f(i) > zero) .and. (xk(i) < zero) ) then
        fixed = tdpscl(i)*tdpcld(i)*tdetrn(i)*tdpdwn(i)
        xmb(i)  = fixed * ratio*tcwfup(i)*tcwfdn(i)
     else
        xmb(i) = zero
     endif
     xmb0(i) = xmb(i)
     if (xmb(i) > xmbmax(i)) then
        xmb(i) = xmbmax(i)
     endif
  enddo


! Feedback: simply the changes from the cloud with unit mass flux
!           multiplied by  the mass flux necessary to keep the
!           equilibrium with the larger-scale.
!round - reverse order of loop, different q and rn
  do i = 1,im
     do k = 1,km
        dellal1(i)   = zero
        dellalk(k,i) = zero
        dellalk(k,i) = dellal(i)
        if (k <= ktcon(i) .and. ktcon(i)/=1) then
           dellat     = (dellah(k,i)-hvap*dellaq(k,i))/cp
           to2(k,i)   = to(k,i) + dellat*xmb(i)*dt2
           qo2(k,i)   = qo0(k,i) + dellaq(k,i)*xmb(i)*dt2
           uo2(k,i)   = uo(k,i) + dellau(k,i)*xmb(i)*dt2
           vo2(k,i)   = vo(k,i) + dellav(k,i)*xmb(i)*dt2
           call fpvsx_ad(to2(k,i),es0,adt,es0_ad,.false.)
           es         = 10.0_r_kind*es0
           qeso2(k,i) = eps*es / (p(k,i)+epsm1*es)
           if (k==ktcon(i) .and. ncloud > 0 .and. cnvflg(i)) then
              dp = 100.0_r_kind * psfc(i) * del(k,i)
              cwmo2(k,i) = cwmo(k,i) + dellalk(k,i) * xmb(i) * dt2
              dellal1(i) = dellalk(k,i)*xmb(i)*dp/grav
           else
              cwmo2(k,i) = cwmo(k,i)
              dellal1(i) = dellalk(k,i)
           endif
        else
           to2(k,i) = to(k,i)
           qo2(k,i) = qo(k,i)
           uo2(k,i) = uo(k,i)
           vo2(k,i) = vo(k,i)
           cwmo2(k,i)= cwmo(k,i)
           call fpvsx_ad(to2(k,i),es0,adt,es0_ad,.false.)
           es         = 10._r_kind*es0
           qeso2(k,i) = eps*es / (p(k,i)+epsm1*es)
           dellal1(i) = dellalk(k,i)
        endif
        dellal(i) = dellal1(i)
        dellal1(i)= zero
     enddo
  enddo

  do i = 1, im
     rntot(i)  = zero
  enddo
!round different results when reverse loop
  do i = 1,im
     do k = kmax(i), 1, -1
        if (k <= ktcon(i) .and. ktcon(i)/=1) then
           aup = one
           if (k <= kb(i)) aup = zero
           adw = one
           if (k > jmin(i)) adw = zero
           rain =  aup * pwo(k,i) + adw * edto1(i) * pwdo(k,i)
           rntot(i) = rntot(i) + rain * xmb(i) * .001_r_kind * dt2
        endif
     enddo
  enddo

!round very slight difference in q and r
  do i = 1,im
     rn0    = zero
     delqev = zero
     flg(i) = .true.
!
     do k = km,1,-1
        qevap = zero
        qcond = zero
        delq2 = zero

!       Accumulate precipitation from cloud level
        if (k <= ktcon(i) .and. ktcon(i)/=1) then
           aup = one
           if (k <= kb(i)) aup = zero
           adw = one
           if (k > jmin(i)) adw = zero
           rain = aup*pwo(k,i) + adw*edto1(i)*pwdo(k,i)
           rn0  = rn0 + rain*xmb(i)*.001_r_kind*dt2
        endif

!       Save layer specific quantities for use in adjoint
        rn0k(k,i) = rn0
        flgk(k,i) = flg(i)
        delqevk(k,i) = delqev

!       Check if any falling precipitation will evaporate
        if (flg(i) .and. k <= ktcon(i)) then
!               if (nint(slimsk(i)) == 1) then
!                  evef = 0.07_r_kind
!               else
!                  evef = edt(i)*evfact
!               endif
           evef = edt(i)*evfact
           term1 = evef
           term2 = qo2(k,i) - qeso2(k,i)
           term3 = el2orc*qeso2(k,i)/to2(k,i)**2
           term4 = one / (one+term3)
           qcond = term1*term2*term4
           dp = 100.0_r_kind * psfc(i) * del(k,i)
!
!          Compute amount of precipitation subject to evaporation
           if (rn0  >  zero .and. qcond <  zero) then
              qevap = -qcond * (one-exp(-.32_r_kind*sqrt(dt2*rn0)))
              
              if ( qevap  >  rn0*r1000*grav/dp ) then
                 qevap = rn0*r1000*grav/dp
              endif
              
              delq2 = delqev + .001_r_kind * qevap * dp / grav
              if (delq2 > rntot(i)) then
                 qevap  = r1000* grav * (rntot(i)-delqev) / dp
                 flg(i) = .false.
              endif
           endif

!          Adjust t and q profiles plus net rain in response
!          to evaporation of falling precipitation.
           if (rn0  >  zero .and. qevap >  zero) then
              qo3(k,i) = qo2(k,i) + qevap
              to3(k,i) = to2(k,i) - elocp * qevap
              rn0      = rn0  - .001_r_kind * qevap * dp / grav
              
              delqev   = delqev + .001_r_kind*dp*qevap/grav

!          Otherwise, no adjustment to t and q profiles
           else
              to3(k,i) = to2(k,i)
              qo3(k,i) = qo2(k,i)
           endif

!       Above cloud top (or not cloud present). 
!       Therefore, no changes to t and q.
        else
           to3(k,i) = to2(k,i)
           qo3(k,i) = qo2(k,i)
        endif

     enddo

!    Save net surface precipitation.
     rn1(i) = rn0
  enddo


! Load output arrays.
  do i = 1,im

!    If total precipitation is negative, restore initial profiles.
     rn11(i) = rn1(i)
     if (rn1(i) < zero) then
        rn1(i) = zero
        do k = 1,km
           t1(k,i) = to(k,i)
           q1(k,i) = qo0(k,i)
        end do

!    Otherwise, load modified t,q profile to output arrays
     else
        do k = 1,km
           t1(k,i) = to3(k,i)
           q1(k,i) = qo3(k,i)
        end do
     endif

!    Load various diagostic output arrays
     ktop(i)   = ktcon(i)
     kbot(i)   = kbcon(i)
     kuo(i)    = 1
     cldwrk(i) = aa1(i)
  end do


! Transfer input u,v,cwm to ouptut arrays
  do i = 1,im
     do k=1,km
        u1(k,i)  = uo2(k,i)
        v1(k,i)  = vo2(k,i)
        cwm1(k,i)= cwmo2(k,i)
     end do
  end do
  
  if (.not.adjoint) return

!
!
!
!=====================================================================
!
!
! Begin adjoint code here

! Adjoint of u,v,cwm transfer
  do i = 1,im
     do k=1,km
        uo2_ad(k,i)   = uo2_ad(k,i) + u1_ad(k,i)
        vo2_ad(k,i)   = vo2_ad(k,i) + v1_ad(k,i)
        cwmo2_ad(k,i) = cwmo2_ad(k,i) + cwm1_ad(k,i)
        u1_ad(k,i) = zero
        v1_ad(k,i) = zero
        cwm1_ad(k,i) = zero
     end do
  end do

!
! Adjoint of loading of output arrays
  do i = 1,im

!    zero or negative precip case.
     if (rn11(i) < zero) then
        rn1_ad(i) = zero
        do k = km,1,-1
           qo0_ad(k,i)= qo0_ad(k,i)+ q1_ad(k,i)
           to_ad(k,i) = to_ad(k,i) + t1_ad(k,i)
           q1_ad(k,i) = zero
           t1_ad(k,i) = zero
        end do

!    precipitation greater than zero case
     else
        do k = km,1,-1
           qo3_ad(k,i) = qo3_ad(k,i) + q1_ad(k,i)
           to3_ad(k,i) = to3_ad(k,i) + t1_ad(k,i)
           q1_ad(k,i) = zero
           t1_ad(k,i) = zero
        end do
     endif
  end do


! Adjoint of final precipitation calculation (including
! evaporation of falling precipitation)
  do i = 1,im

!    Zero local adjoint variables
     rn0_ad    = zero
     delqev_ad = zero

!    Adjoint of rn0 to rn1 transfer
     rn0_ad = rn0_ad + rn1_ad(i)
!
     do k = 1,km
!
!       Restore saved values from nlm
        rn0    = rn0k(k,i)
        flg(i) = flgk(k,i)
        delqev = delqevk(k,i)
        qevap  = zero
        qcond  = zero
        delq2  = zero

!       Zero local adjoint variables
        qevap_ad = zero
        qcond_ad = zero
!
!       Adjoint of code for evaporation of falling precipitation
        if (flg(i) .and. k <= ktcon(i)) then
!
!           Redo nlm
!           if (nint(slimsk(i)) == 1) then
!              evef = 0.07_r_kind
!           else
!              evef = edt(i)*evfact
!           endif
           evef = edt(i)*evfact
           term1 = evef
           term2 = qo2(k,i) - qeso2(k,i)
           term3 = el2orc*qeso2(k,i)/to2(k,i)**2
           term4 = one / (one+term3)
           qcond = term1*term2*term4
           dp = 100._r_kind * psfc(i) * del(k,i)

           if (rn0 >  zero .and. qcond < zero) then
              qevap  = -qcond * (one-exp(-.32_r_kind*sqrt(dt2*rn0)))
              qevap0 = qevap
              if ( qevap  >  (rn0*r1000*grav/dp) ) then
                 qevap = rn0*r1000*grav/dp
              endif
              delq2 = delqev + 0.001_r_kind * qevap * dp / grav
              qevap1 = qevap
              if (delq2 > rntot(i)) then
                 qevap  = r1000* grav * (rntot(i)-delqev) / dp
              endif
           endif
!
!          Adjoint of t,q,rn0 adjustments due to evaporation
           if ( rn0 > zero .and. qevap > zero ) then
              qevap_ad = qevap_ad + 0.001_r_kind*delqev_ad*dp/grav

              qevap_ad = qevap_ad - 0.001_r_kind*rn0_ad*dp/grav

              qevap_ad = qevap_ad - elocp*to3_ad(k,i)
              to2_ad(k,i) = to2_ad(k,i) + to3_ad(k,i)

              qevap_ad = qevap_ad + qo3_ad(k,i)
              qo2_ad(k,i) = qo2_ad(k,i) + qo3_ad(k,i)
!
!             Otherwise adjoint of no adjustment case
           else
              qo2_ad(k,i) = qo2_ad(k,i) + qo3_ad(k,i)
              to2_ad(k,i) = to2_ad(k,i) + to3_ad(k,i)
           endif
!
!          Adjoint of qevap calculation
           if (rn0 > zero .and. qcond < zero) then
              if (delq2 > rntot(i)) then
                 delqev_ad = delqev_ad - r1000*grav*qevap_ad/dp
                 rntot_ad(i) = rntot_ad(i) + r1000*grav*qevap_ad/dp
              endif
                  
              if (qevap0  >  rn0*r1000*grav/dp) then
                 rn0_ad = rn0_ad + qevap_ad*r1000*grav/dp
              endif

              rn0_ad = rn0_ad &
                   + qcond * (exp(-0.32_r_kind*sqrt(dt2*rn0)) * &
                   (-0.32_r_kind*half/sqrt(dt2*rn0)) * dt2*qevap_ad)
              qcond_ad = qcond_ad &
                   - qevap_ad * (one-exp(-0.32_r_kind*sqrt(dt2*rn0)))
           endif
!     
!          Adjoint of qcond calculation
           term4_ad = zero
           term3_ad = zero
           term2_ad = zero
           term1_ad = zero
           evef_ad  = zero

           term4_ad = term4_ad + term1*term2*qcond_ad
           term2_ad = term2_ad + term1*term4*qcond_ad
           term1_ad = term1_ad + term2*term4*qcond_ad

           term3_ad = term3_ad - one/(one+term3)**2 * term4_ad
               
           to2_ad(k,i) = to2_ad(k,i) &
                - two*el2orc*qeso2(k,i)/to2(k,i)**3 * term3_ad
           qeso2_ad(k,i) = qeso2_ad(k,i) &
                + el2orc*term3_ad/to2(k,i)**2

           qeso2_ad(k,i) = qeso2_ad(k,i) - term2_ad
           qo2_ad(k,i) = qo2_ad(k,i) + term2_ad

           evef_ad = evef_ad + term1_ad
!           if (nint(slimsk(i)) == 1) then
!              evef_ad = zero
!           else
!              edt_ad(i) = edt_ad(i) + evef_ad*evfact
!           endif
              edt_ad(i) = edt_ad(i) + evef_ad*evfact
!
!              
!       Adjoint of "2" to "3" transfer.  This is the case
!       when above cloud top or no cloud present.
        else
           to2_ad(k,i) = to2_ad(k,i) + to3_ad(k,i)
           qo2_ad(k,i) = qo2_ad(k,i) + qo3_ad(k,i)
        endif
!
!       Adjoint of layer rain calculation
        if (k <= ktcon(i) .and. ktcon(i)/=1) then
!
!          Redo nlm
           aup = one
           if (k <= kb(i)) aup = zero
           adw = one
           if (k > jmin(i)) adw = zero
           rain = aup*pwo(k,i) + adw*edto1(i)*pwdo(k,i)
!
!          Adjoint of tlm code.
           rain_ad   = zero
           xmb_ad(i) = xmb_ad(i) + rain*rn0_ad*0.001_r_kind*dt2
           rain_ad   = rain_ad + rn0_ad*xmb(i)*0.001_r_kind*dt2

           pwdo_ad(k,i) = pwdo_ad(k,i) + adw*edto1(i)*rain_ad
           edto1_ad(i)   = edto1_ad(i) + adw*rain_ad*pwdo(k,i)
           pwo_ad(k,i)  = pwo_ad(k,i) + aup*rain_ad
        endif
!
!       Zero local adjoint variables
        qevap_ad = zero
        qcond_ad = zero
!
!    End of loop over vertical coordinate
     end do
!
!    Zero local adjoint variables
     rn0_ad    = zero
     delqev_ad = zero
!
! End of loop over profiles
  end do

!
!
! Adjoint of intial total precipitation calculation
  do i = 1,im
     do k = 1,kmax(i)
        if (k <= ktcon(i) .and. ktcon(i)/=1) then
!
!          Redo nlm
           aup = one
           if (k <= kb(i)) aup = zero
           adw = one
           if (k > jmin(i)) adw = zero
           rain =  AUP * PWO(k,i) + ADW * EDTO1(I) * PWDO(k,i)
!
!          Adjoint of tlm code
           rain_ad   = zero
           xmb_ad(i) = xmb_ad(i) + rain*0.001_r_kind*dt2 * rntot_ad(i)
           rain_ad   = rain_ad + 0.001_r_kind*xmb(i)*dt2 * rntot_ad(i)
               
           pwdo_ad(k,i) = pwdo_ad(k,i) + adw*edto1(i)*rain_ad
           edto1_ad(i)   = edto1_ad(i) + adw*pwdo(k,i)*rain_ad
           pwo_ad(k,i)  = pwo_ad(k,i) + aup*rain_ad
        endif
     end do
  end do
!
! Adjoint of to2 and qo2 calculation (prior to evaportation)
  do i = 1,im
     do k = km,1,-1
        dellal1_ad(i) = zero
        dellal1_ad(i) = dellal1_ad(i) + dellal_ad(i)
        if (k <= ktcon(i) .and. ktcon(i)/=1) then
!
!          Redo nlm
           dellat = (dellah(k,i)-hvap*dellaq(k,i))/cp
           call fpvsx_ad(to2(k,i),es0,adt,es0_ad,.false.)
           es = 10.0_r_kind*es0
!
!          Adjoint of tlm code
           dellat_ad = zero
           es_ad     = zero
           es0_ad    = zero

           if (k==ktcon(i) .and. ncloud > 0 .and. cnvflg(i)) then
              dp = 100.0_r_kind*psfc(i)*del(k,i)

              dellalk_ad(k,i) = dellalk_ad(k,i) + &
                   dellal1_ad(i)*xmb(i)*dp/grav
              xmb_ad(i) = xmb_ad(i) + &
                   dellalk(k,i)*dellal1_ad(i)*dp/grav

              dellalk_ad(k,i) = dellalk_ad(k,i) + &
                   cwmo2_ad(k,i)*xmb(i)*dt2
              xmb_ad(i) = xmb_ad(i) + &
                   dellalk(k,i)*cwmo2_ad(k,i)*dt2
              cwmo_ad(k,i)  = cwmo_ad(k,i) + cwmo2_ad(k,i)

              cwmo2_ad(k,i) = zero
           else
              cwmo_ad(k,i) = cwmo_ad(k,i) + cwmo2_ad(k,i)
              dellalk_ad(k,i) = dellalk_ad(k,i) + dellal1_ad(i)

              cwmo2_ad(k,i) = zero
           endif
!
           es_ad = es_ad - eps*es/(p(k,i)+epsm1*es)**2 * &
                epsm1*qeso2_ad(k,i)
           es_ad = es_ad + eps*qeso2_ad(k,i)/(p(k,i)+epsm1*es)

           es0_ad = es0_ad + 10*es_ad

           adt=zero
           call fpvsx_ad(to2(k,i),es0,adt,es0_ad,adjoint)
           to2_ad(k,i) = to2_ad(k,i) + adt
               

           vo_ad(k,i) = vo_ad(k,i) + vo2_ad(k,i)
           xmb_ad(i)  = xmb_ad(i)  + &
                vo2_ad(k,i)*dellav(k,i)*dt2
           dellav_ad(k,i) = dellav_ad(k,i) + &
                vo2_ad(k,i)*xmb(i)*dt2

           uo_ad(k,i) = uo_ad(k,i) + uo2_ad(k,i)
           xmb_ad(i)  = xmb_ad(i)  + &
                uo2_ad(k,i)*dellau(k,i)*dt2
           dellau_ad(k,i) = dellau_ad(k,i) + &
                uo2_ad(k,i)*xmb(i)*dt2

           qo_ad(k,i) = qo_ad(k,i) + qo2_ad(k,i)
           xmb_ad(i)  = xmb_ad(i)  + &
                qo2_ad(k,i)*dellaq(k,i)*dt2
           dellaq_ad(k,i) = dellaq_ad(k,i) + &
                qo2_ad(k,i)*xmb(i)*dt2

           to_ad(k,i) = to_ad(k,i) + to2_ad(k,i)
           xmb_ad(i)  = xmb_ad(i)  + to2_ad(k,i)*dellat*dt2
           dellat_ad  = dellat_ad  + to2_ad(k,i)*xmb(i)*dt2

           dellaq_ad(k,i) = dellaq_ad(k,i) - hvap*dellat_ad/cp
           dellah_ad(k,i) = dellah_ad(k,i) + dellat_ad/cp

        else
!
!          Redo nlm
           call fpvsx_ad(to2(k,i),es0,adt,es0_ad,.false.)
           es = 10.0_r_kind*es0

!
!          Adjoint of tlm code
           es_ad = zero
           es0_ad= zero

           dellalk_ad(k,i) = dellalk_ad(k,i) + dellal1_ad(i)

           es_ad = es_ad - eps*es/(p(k,i)+epsm1*es)**2 * &
                epsm1*qeso2_ad(k,i)
           es_ad = es_ad + eps*qeso2_ad(k,i)/(p(k,i)+epsm1*es)

           es0_ad = es0_ad + 10.0_r_kind*es_ad
           adt = zero
           call fpvsx_ad(to2(k,i),es0,adt,es0_ad,adjoint)
           to2_ad(k,i) = to2_ad(k,i) + adt

           cwmo_ad(k,i) = cwmo_ad(k,i) + cwmo2_ad(k,i)
           vo_ad(k,i)   = vo_ad(k,i) + vo2_ad(k,i)
           uo_ad(k,i)   = uo_ad(k,i) + uo2_ad(k,i)
           qo_ad(k,i)   = qo_ad(k,i) + qo2_ad(k,i)
           to_ad(k,i)   = to_ad(k,i) + to2_ad(k,i)

        endif
        dellal_ad(i)    = dellal_ad(i) + dellalk_ad(k,i)
        dellalk_ad(k,i) = zero
        dellal1_ad(i)   = zero
!
     end do
  end do
!
! Adjoint of kernel (cloud mass flux) calculation
  do i = 1,im
!
!    Redo nlm
     if (abs(xk(i)) > tiny_r_kind) then
        ratio = -f(i)/xk(i)
     else
        ratio = zero
     endif
     fixed = tdpscl(i)*tdpcld(i)*tdetrn(i)*tdpdwn(i)
!
!    Adjoint of tlm
     if (xmb0(i) > xmbmax(i)) then
!       do nothing since tlm = zero
        xmb_ad(i) = zero
     endif

     ratio_ad = zero
     if ( (f(i) > zero) .and. (xk(i) < zero) ) then
        tcwfdn_ad(i) = tcwfdn_ad(i) &
             + fixed*ratio*tcwfup(i)*xmb_ad(i)
        tcwfup_ad(i) = tcwfup_ad(i) &
             + fixed*ratio*xmb_ad(i)*tcwfdn(i)
        ratio_ad = ratio_ad + fixed*xmb_ad(i)*tcwfup(i)*tcwfdn(i)
     else
!       do nothing since tlm = zero
        xmb_ad(i) = zero
     endif

     if (abs(xk(i)) > tiny_r_kind) then
        xk_ad(i) = xk_ad(i) + f(i)/xk(i)**2 * ratio_ad
        f_ad(i)  = f_ad(i) - ratio_ad/xk(i)
     else
!       do nothing since tlm = zero
        ratio_ad = zero
     endif
!
!    Adjoint of xk tlm
     aa1_ad(i)  = aa1_ad(i)  - xk_ad(i)/mbdt
     xaa0_ad(i) = xaa0_ad(i) + xk_ad(i)/mbdt
!
!    Adjoint of f tlm
     dtconv_ad(i) = dtconv_ad(i) &
          - (aa1(i) - acrt(i)*acrtfct1(i))/dtconv(i)**2 &
          * f_ad(i)
     acrtfct1_ad(i) = acrtfct1_ad(i) &
          - acrt(i)*f_ad(i) / dtconv(i)
     aa1_ad(i) = aa1_ad(i) + f_ad(i) / dtconv(i)
!
  end do
!
! Adjoint of acrtfct1 and dtconv calculations
!
  do i = 1,im
!
!    Redo nlm 
     if (nint(slimsk(i)) == 1) then
        w1 = w1l
        w2 = w2l
        w3 = w3l
        w4 = w4l
     else
        w1 = w1s
        w2 = w2s
        w3 = w3s
        w4 = w4s
     endif
!
!    Adjoint of dtconv calculation
     if (dtconv0(i) > dtmax) then
!       do nothing since tlm = zero
        dtconv_ad(i) = zero
     endif
     if (dtconv0(i) < dtmin) then
!       do nothing since tlm = zero
        dtconv_ad(i) = zero
     endif
     pdot_ad(i) = pdot_ad(i) + (1800.0_r_kind-dt2) &
          * dtconv_ad(i)/(w1-w2)
!
!    Adjoint of acrtfct calculation
     acrtfct_ad(i) = acrtfct_ad(i) - acrtfct1_ad(i)
     if (acrtfct0(i) > one) then
!       do nothing since tlm = zero
        acrtfct_ad(i) = zero
     endif
     if (acrtfct0(i) < -one) then
!       do nothing since tlm = zero
        acrtfct_ad(i) = zero
     endif
     if (pdot(i) <= w4) then
        pdot_ad(i) = pdot_ad(i) + acrtfct_ad(i)/(w3-w4)
     elseif (pdot(i) >= -w4) then
        pdot_ad(i) = pdot_ad(i) - acrtfct_ad(i)/(w4-w3)
     else
!       do nothing since tlm = zero
        acrtfct_ad(i) = zero
     endif
!
  end do
!
! Adjoint of xaa0 downdraft cloudwork function calculation
  do i = 1,im
     do k = 1,kmax(i)-1
        if (dwnflg2(i) .and. k < jmin(i)) then
!
!          Redo nlm calculations
           GAMMA = EL2ORC * XQESOL(k+1,i) / XTOL(k+1,i)**2
           DHH   = XHCD(I)
           DT    = XTOL(k+1,i)
           DG    = GAMMA
           DH    = XHESO(k+1,i)
           dz    = xzo(k,i) - xzo(k+1,i)
!
           term1 = edtx1(i)
           term2 = dz
           term3 = grav/(cp*dt)
           term4 = (dhh-dh)
           term5 = one/(one+dg)
           term6 = one + delta*cp*dg*dt/hvap
           dqs = xqesol(k+1,i) - xqol(k+1,i)

!
!          Adjoint of tlm code
           dqs_ad = zero
           dz_ad  = zero
           dt_ad  = zero
           dg_ad  = zero
           dh_ad  = zero
           dhh_ad = zero
           if (dqs > zero) then
              dqs_ad = dqs_ad + edtx1(i)*dz*grav*delta  * xaa0_ad(i)
              dz_ad  = dz_ad  + edtx1(i)*grav*delta*dqs * xaa0_ad(i)
              edtx1_ad(i) = edtx1_ad(i) + dz*grav*delta*dqs &
                   * xaa0_ad(i)
           endif
!     
           term1_ad = term2*term3*term4*term5*term6*xaa0_ad(i)
           term2_ad = term1*term3*term4*term5*term6*xaa0_ad(i)
           term3_ad = term1*term2*term4*term5*term6*xaa0_ad(i)
           term4_ad = term1*term2*term3*term5*term6*xaa0_ad(i)
           term5_ad = term1*term2*term3*term4*term6*xaa0_ad(i)
           term6_ad = term1*term2*term3*term4*term5*xaa0_ad(i)

           xqol_ad(k+1,i)   = xqol_ad(k+1,i)   - dqs_ad
           xqesol_ad(k+1,i) = xqesol_ad(k+1,i) + dqs_ad

!     
           dt_ad  = (delta*cp/hvap) * dg*term6_ad
           dg_ad  = (delta*cp/hvap) * term6_ad*dt
           dg_ad  = dg_ad  - one/(one+dg)**2 * term5_ad
           dh_ad  = dh_ad  - term4_ad
           dhh_ad = dhh_ad + term4_ad
           dt_ad  = dt_ad  - grav/(cp*dt*dt) * term3_ad
           dz_ad  = dz_ad  + term2_ad
           edtx1_ad(i) = edtx1_ad(i) + term1_ad 

           xzo_ad(k,i)    = xzo_ad(k,i)   + dz_ad
           xzo_ad(k+1,i)  = xzo_ad(k+1,i) - dz_ad
           xheso_ad(k+1,i)= xheso_ad(k+1,i) + dh_ad
           gamma_ad       = dg_ad
           xtol_ad(k+1,i) = xtol_ad(k+1,i) + dt_ad
           xhcd_ad(i)     = xhcd_ad(i)   + dhh_ad
           xtol_ad(k+1,i) = xtol_ad(k+1,i) &
                - two*el2orc*xqesol(k+1,i)/xtol(k+1,i)**3 &
                * gamma_ad
           xqesol_ad(k+1,i) = xqesol_ad(k+1,i) + &
                el2orc*gamma_ad/xtol(k+1,i)**2
        endif
     end do
  end do
!
! Adjoint of final edtx1 calculation
  do i = 1,im
     edtmax = edtmaxl
     if (nint(slimsk(i)) == 0) edtmax = edtmaxs
     if (dwnflg2(i)) then
        if (xpwev(i) >= zero) then
!          do nothing since tlm is zero
           edtx1_ad(i) = zero
        else
           if (edtx10(i) > edtmax) then
!             do nothing since tlm is zero
              edtx1_ad(i) = zero
           else
              xpwev_ad(i) = xpwev_ad(i) &
                   + edtx(i)*xpwav(i)/xpwev(i)**2 * edtx1_ad(i)
              xpwav_ad(i) = xpwav_ad(i) &
                   - edtx(i)*edtx1_ad(i)/xpwev(i)
              edtx_ad(i) = edtx_ad(i) &
                   - edtx1_ad(i)*xpwav(i)/xpwev(i)
           endif
        endif
     else
!       do nothing since tlm is zero             
        edtx1_ad(i) = zero
     endif
  enddo
!
! Adjoint of downdraft xpwev calculation
  do i = 1,im
     do k = 1,kmax(i)-1
        if (dwnflg2(i) .and. k < jmin(i)) then
!
!          Redo nlm
           dq    = xqesol(k,i)
           dt    = xtol(k,i)
           gamma = el2orc*dq/dt**2
           dh = xhcd(i) - xheso(k,i)
           term5 = (one/hvap) * gamma
           term6 = one/(one+gamma)
           detad = etad(k+1,i) - etad(k,i)
           term1 = etad(k+1,i)*xqrcd(k+1,i)
           term2 = etad(k,i)*xqrcd(k,i)
           term3 = detad * half * (xqrcd(k,i) + xqrcd(k+1,i))
!
!          Adjoint of tlm code
           xpwd_ad  = zero
           detad_ad = zero
           dh_ad    = zero
           gamma_ad = zero
           dt_ad    = zero
           dq_ad    = zero
           term1_ad = zero
           term2_ad = zero
           term3_ad = zero
           term5_ad = zero
           term6_ad = zero

           xpwd_ad = xpwd_ad + xpwev_ad(i)

           term1_ad = term1_ad + xpwd_ad
           term2_ad = term2_ad - xpwd_ad
           term3_ad = term3_ad - xpwd_ad

           xqrcd_ad(k+1,i) = xqrcd_ad(k+1,i) + etad(k+1,i)*term1_ad
           etad_ad(k+1,i)  = etad_ad(k+1,i)  + term1_ad*xqrcd(k+1,i)

           xqrcd_ad(k,i)   = xqrcd_ad(k,i) + etad(k,i)*term2_ad
           etad_ad(k,i)    = etad_ad(k,i)  + term2_ad*xqrcd(k,i)

           detad_ad        = detad_ad + term3_ad*half*(xqrcd(k,i)+xqrcd(k+1,i))
           xqrcd_ad(k,i)   = xqrcd_ad(k,i) + detad*half*term3_ad
           xqrcd_ad(k+1,i) = xqrcd_ad(k+1,i) + detad*half*term3_ad

           etad_ad(k,i)    = etad_ad(k,i) - detad_ad
           etad_ad(k+1,i)  = etad_ad(k+1,i) + detad_ad

           dh_ad    = dh_ad    + term5*term6*xqrcd_ad(k,i)
           term6_ad = term6_ad + term5*dh*xqrcd_ad(k,i)
           term5_ad = term5_ad + term6*dh*xqrcd_ad(k,i)
           dq_ad = dq_ad + xqrcd_ad(k,i)

           gamma_ad = gamma_ad -one/(one+gamma)**2 * term6_ad
           gamma_ad = gamma_ad + (one/hvap)*term5_ad

           xheso_ad(k,i) = xheso_ad(k,i) - dh_ad
           xhcd_ad(i)  = xhcd_ad(i) + dh_ad

           dt_ad = dt_ad - two*el2orc*dq/dt**3 * gamma_ad
           dq_ad = dq_ad + el2orc*gamma_ad/dt**2

           xtol_ad(k,i) = xtol_ad(k,i) + dt_ad
           xqesol_ad(k,i) = xqesol_ad(k,i) + dq_ad

        endif
     end do
  end do
!
! Adjoint of xheo,xqesol --> xhcd,xqrcd transfers
  do i = 1,im
     if (dwnflg2(i)) then
        jmn = jmin(i)
        xqesol_ad(jmn,i) = xqesol_ad(jmn,i) + xqrcd_ad(jmn,i)
        xheo_ad(jmn,i)   = xheo_ad(jmn,i) + xhcd_ad(i)
     endif
  end do
!
! Adjoint of xaa0 calculations
!
  do i = 1,im
     do k = kmax(i)-1,2,-1
        if (k > kbcon(i).and.k <= ktcon(i)) then
!
!          Recalculate nlm 
           dz1   = xzo(k,i) - xzo(k-1,i)
           gamma = el2orc*xqesol(k-1,i)/xtol(k-1,i)**2
           rfact =  one + delta*cp*gamma*xtol(k-1,i)/hvap
           xdby  = xhcko(k-1,i) - xheso(k-1,i)
           term1 = dz1 * (grav/cp)
           term2 = one/xtol(k-1,i)
           term3 = xdby
           term4 = one/(one+gamma)
           term5 = rfact
           dqs   = xqesol(k-1,i) - xqol(k-1,i)
!
!          Zero local adjoint variables
           dqs_ad = zero
           dz1_ad = zero
           term1_ad = zero
           term2_ad = zero
           term3_ad = zero
           term4_ad = zero
           term5_ad = zero
           rfact_ad = zero
           gamma_ad = zero
           xdby_ad  = zero
!
!          Adjoint of forward model tlm
           if (dqs > zero) then
              dqs_ad   = dqs_ad + grav*delta * dz1*xaa0_ad(i)
              dz1_ad   = dz1_ad + grav*delta * dqs*xaa0_ad(i)
           endif
           xqol_ad(k-1,i)   = xqol_ad(k-1,i)   - dqs_ad
           xqesol_ad(k-1,i) = xqesol_ad(k-1,i) + dqs_ad

           term5_ad = term5_ad + term1*term2*term3*term4*xaa0_ad(i)
           term4_ad = term4_ad + term1*term2*term3*xaa0_ad(i)*term5
           term3_ad = term3_ad + term1*term2*xaa0_ad(i)*term4*term5
           term2_ad = term2_ad + term1*xaa0_ad(i)*term3*term4*term5
           term1_ad = term1_ad + xaa0_ad(i)*term2*term3*term4*term5

           rfact_ad = rfact_ad + term5_ad
           gamma_ad = gamma_ad - one/(one+gamma)**2 * term4_ad
           xdby_ad  = xdby_ad + term3_ad
           xtol_ad(k-1,i) = xtol_ad(k-1,i) &
                - one/(xtol(k-1,i)**2) * term2_ad
           dz1_ad   = dz1_ad + term1_ad * (grav/cp)
           
           xheso_ad(k-1,i) = xheso_ad(k-1,i) - xdby_ad
           xhcko_ad(k-1,i) = xhcko_ad(k-1,i) + xdby_ad

           xtol_ad(k-1,i) = xtol_ad(k-1,i) &
                + (delta*cp/hvap)*gamma*rfact_ad
           gamma_ad = gamma_ad &
                + (delta*cp/hvap)*rfact_ad*xtol(k-1,i)

           xtol_ad(k-1,i) = xtol_ad(k-1,i) &
                - two*el2orc*xqesol(k-1,i)/(xtol(k-1,i)**3) &
                * gamma_ad
           xqesol_ad(k-1,i) = xqesol_ad(k-1,i) &
                + el2orc*gamma_ad/xtol(k-1,i)**2

           xzo_ad(k-1,i) = xzo_ad(k-1,i) - dz1_ad
           xzo_ad(k,i   )   = xzo_ad(k,i)      + dz1_ad

        endif
     end do
  end do
!
! Adjoint of xpwav calculation and initial xaa0 calculation
!
  do i = 1,im
     do k = kmax(i)-1,2,-1
        if (k > kb(i) .and. k < ktcon(i)) then
!
!          Redo nlm
           factor = eta(k-1,i)/eta(k,i)
           onemf  = one - factor
           termq  = half*(xqol(k,i) + xqol(k+1,i))
           gamma  = el2orc*xqesol(k,i)/xtol(k,i)**2
           xdby   = xhcko(k,i) - xheso(k,i)
           xtemp  = xtemp0(k,i)
           xqrch  = xqrch0(k,i)
           dq     = eta(k,i)*(xtemp-xqrch)
!
!          Adjoint code
           xtemp_ad  = zero
           termq_ad  = zero
           onemf_ad  = zero
           factor_ad = zero
           xqrch_ad  = zero
           gamma_ad  = zero
           xdby_ad   = zero
           xqc_ad    = zero
           qlk_ad    = zero
           xpw_ad    = zero
           dz_ad     = zero
           etah_ad   = zero
           dz1_ad    = zero
           dq_ad     = zero
!
!          Adjoint of actions dependent on sign of dq
           if (dq > zero) then
!
!             Redo nlm calculations
              dz   = half * (xzo(k+1,i) - xzo(k-1,i))
              dz1  = xzo(k,i) - xzo(k-1,i)
              etah = half * (eta(k,i) + eta(k-1,i))
              qlk  = dq / (eta(k,i) + etah * c0 * dz)
!
!             Adjoint code
              xpw_ad   = xpw_ad  + xpwav_ad(i)
              qlk_ad   = qlk_ad  + etah*c0*dz*xpw_ad
              dz_ad    = dz_ad   + etah*c0*xpw_ad*qlk
              etah_ad  = etah_ad + xpw_ad*c0*dz*qlk

              qlk_ad   = qlk_ad  - dz1*grav*xaa0_ad(i)
              dz1_ad   = dz1_ad  - xaa0_ad(i)*grav*qlk

              xqc_ad   = xqc_ad + xqcko_ad(k,i)
              xqrch_ad = xqrch_ad + xqc_ad
              qlk_ad   = qlk_ad + xqc_ad

              dz_ad    = dz_ad &
                   - (dq/(eta(k,i)+etah*c0*dz)**2) &
                   * etah*c0*qlk_ad
              etah_ad  = etah_ad &
                   - (dq/(eta(k,i)+etah*c0*dz)**2) &
                   * qlk_ad*c0*dz
              eta_ad(k,i) = eta_ad(k,i) &
                   - (dq/(eta(k,i)+etah*c0*dz)**2) &
                   * qlk_ad
              dq_ad = dq_ad + qlk_ad/(eta(k,i)+etah*c0*dz)

              eta_ad(k-1,i) = eta_ad(k-1,i) + half*etah_ad
              eta_ad(k,i)   = eta_ad(k,i)      + half*etah_ad
              xzo_ad(k-1,i) = xzo_ad(k-1,i) - dz1_ad
              xzo_ad(k,i)   = xzo_ad(k,i)      + dz1_ad
              xzo_ad(k-1,i) = xzo_ad(k-1,i) - half*dz_ad
              xzo_ad(k+1,i) = xzo_ad(k+1,i) + half*dz_ad
           else
              xtemp_ad = xtemp_ad + xqcko_ad(k,i)
           endif
!
!          Adjoint of dq calculation
           xqrch_ad = xqrch_ad - eta(k,i)*dq_ad
           xtemp_ad = xtemp_ad + eta(k,i)*dq_ad
           eta_ad(k,i) = eta_ad(k,i) + dq_ad*(xtemp-xqrch)
!
!          Adjoint of xqrch calculation
           xdby = xhcko(k,i) - xheso(k,i)
           if (xdby > zero) then
              xqesol_ad(k,i) = xqesol_ad(k,i) + xqrch_ad
              gamma_ad = gamma_ad &
                   - gamma*xdby/((hvap*(one+gamma))**2) &
                   * hvap*xqrch_ad
              xdby_ad  = xdby_ad + gamma*xqrch_ad/(hvap*(one+gamma))
              gamma_ad = gamma_ad + xqrch_ad*xdby/(hvap*(one+gamma))
              xtol_ad(k,i) = xtol_ad(k,i) &
                   - two*el2orc*xqesol(k,i)/xtol(k,i)**3 &
                   * gamma_ad
              xqesol_ad(k,i) = xqesol_ad(k,i) &
                   + el2orc*gamma_ad/xtol(k,i)**2
           else
              xqesol_ad(k,i) = xqesol_ad(k,i) + xqrch_ad
           endif
           xheso_ad(k,i) = xheso_ad(k,i) - xdby_ad
           xhcko_ad(k,i) = xhcko_ad(k,i) + xdby_ad
!
!          Adjoint of xtemp calculation
           termq_ad = termq_ad + onemf*xtemp_ad
           onemf_ad = onemf_ad + xtemp_ad*termq
           xqcko_ad(k-1,i) = xqcko_ad(k-1,i) + factor*xtemp_ad
           factor_ad = factor_ad + xtemp_ad*xqcko(k-1,i)

           xqol_ad(k+1,i) = xqol_ad(k+1,i) + half*termq_ad
           xqol_ad(k,i)   = xqol_ad(k,i)      + half*termq_ad

           factor_ad = factor_ad - onemf_ad

           eta_ad(k,i) = eta_ad(k,i) &
                - eta(k-1,i)/eta(k,i)**2 * factor_ad
           eta_ad(k-1,i) = eta_ad(k-1,i) + factor_ad/eta(k,i)
!
        endif
     end do
  end do
!
! Adjoint of xhcko calculation
  do i = 1,im
     do k = kmax(i)-1,2,-1
        if (k > kb(i).and.k <= ktcon(i)) then
!
!          Recalculate nlm
           factor = eta(k-1,i)/eta(k,i)
           onemf  = one - factor
!
!          Adjoint of tlm code
           onemf_ad  = zero
           factor_ad = zero

           xheo_ad(k+1,i) = xheo_ad(k+1,i) + onemf*half*xhcko_ad(k,i)
           xheo_ad(k,i)   = xheo_ad(k,i)   + onemf*half*xhcko_ad(k,i)
           onemf_ad = onemf_ad + &
                xhcko_ad(k,i) * half*(xheo(k,i)+xheo(k+1,i))
           xhcko_ad(k-1,i) = xhcko_ad(k-1,i) + factor*xhcko_ad(k,i)
           factor_ad = factor_ad + xhcko_ad(k,i)*xhcko(k-1,i)

           factor_ad = factor_ad - onemf_ad

           eta_ad(k,i) = eta_ad(k,i) - &
                (eta(k-1,i)/eta(k,i)**2) * factor_ad
           eta_ad(k-1,i) = eta_ad(k-1,i) + factor_ad/eta(k,i)
        endif
     end do
  end do
!
! Adjoint of xheo,xqol --> xhcko,xqcko transfer
  do i = 1,im
     indx = kb(i)
     xheo_ad(indx,i) = xheo_ad(indx,i) + xhcko_ad(indx,i)
     xqol_ad(indx,i) = xqol_ad(indx,i) + xqcko_ad(indx,i)
  end do
!
! Adjoint of xheo and xheso calculation at k=kmax(i)
  do i = 1,im
     k=kmax(i)
     xqesol_ad(k,i) = xqesol_ad(k,i) + hvap*xheso_ad(k,i)
     xtol_ad(k,i)   = xtol_ad(k,i)   + cp  *xheso_ad(k,i)
     xzo_ad(k,i)    = xzo_ad(k,i)    + grav   *xheso_ad(k,i)

     xqol_ad(k,i) = xqol_ad(k,i) + hvap*xheo_ad(k,i)
     xtol_ad(k,i) = xtol_ad(k,i) + cp  *xheo_ad(k,i)
     xzo_ad(k,i)  = xzo_ad(k,i)  + grav   *xheo_ad(k,i)
  enddo
!
! Adjoint of xheo and xheso calculation below k=kmax
  do i = 1,im
     do k = kmax(i)-1,1,-1
!
!       Redo nlm calculation
        po    = half * (p(k,i) + p(k+1,i))
        call fpvsx_ad(xtol(k,i),es0,adt,es0_ad,.false.)
        esl = 10.0_r_kind*es0
!
!       Adjoint of tlm code
        esl_ad = zero
        es0_ad = zero

        xqesol_ad(k,i) = xqesol_ad(k,i) + hvap *xheso_ad(k,i)
        xtol_ad(k,i)   = xtol_ad(k,i)   + cp   *xheso_ad(k,i)
        xzo_ad(k+1,i)  = xzo_ad(k+1,i)  + half*grav*xheso_ad(k,i)
        xzo_ad(k,i)    = xzo_ad(k,i)    + half*grav*xheso_ad(k,i)

        xqol_ad(k,i)   = xqol_ad(k,i)   + hvap *xheo_ad(k,i)
        xtol_ad(k,i)   = xtol_ad(k,i)   + cp   *xheo_ad(k,i)
        xzo_ad(k+1,i)  = xzo_ad(k+1,i)  + half*grav*xheo_ad(k,i)
        xzo_ad(k,i)    = xzo_ad(k,i)    + half*grav*xheo_ad(k,i)

        esl_ad = esl_ad - eps*esl/((po+epsm1*esl)**2) &
             *epsm1*xqesol_ad(k,i)
        esl_ad = esl_ad &
             + eps*xqesol_ad(k,i)/(po+epsm1*esl)

        es0_ad = es0_ad + 10*esl_ad

        adt = zero
        call fpvsx_ad(xtol(k,i),es0,adt,es0_ad,adjoint)
        xtol_ad(k,i) = xtol_ad(k,i) + adt
     end do
  end do
!
! Adjoint of xtol and xqol calculations
  do i = 1,im
     do k = kmax(i)-1,1,-1
!
!       Redo nlm calculations
        dz     = half * (xzo(k+1,i) - xzo(k,i))
        dp     = half * (p(k+1,i) - p(k,i))
        call fpvsx_ad(xto(k+1,i),es0,adt,es0_ad,.false.)
        es     = 10.0_r_kind*es0
        pprime = p(k+1,i) + epsm1 * es
        qs     = eps * es / pprime
        dqsdp  = - qs / pprime
        desdt  = ES * (FACTOR1 / xto(k+1,i) + FACTOR2 / (xto(k+1,i)**2))
        dqsdt  = qs * p(k+1,i) * desdt / (es * pprime)
        gamma  = el2orc * xqeso(k+1,i) / (xto(k+1,i)**2)
        dt     = (grav * dz + hvap*dqsdp*dp) / (cp*(one+gamma))
        dq     = dqsdt * dt + dqsdp * dp
!           
!       Initialize local ajm variables to zero
        dq_ad     = zero
        dt_ad     = zero
        dqsdp_ad  = zero
        dqsdt_ad  = zero
        gamma_ad  = zero
        dz_ad     = zero
        pprime_ad = zero
        es_ad     = zero
        desdt_ad  = zero
        qs_ad     = zero
        es0_ad    = zero
!
!       Adjoint of tlm code
        if (xqol0(k,i)>zero) then
           xqol0_ad(k,i) = xqol0_ad(k,i) + xqol_ad(k,i)
        else
           xqol0_ad(k,i) = zero
        endif
        xqo_ad(k+1,i) = xqo_ad(k+1,i) + xqol0_ad(k,i)
        dq_ad = dq_ad + xqol0_ad(k,i)

        xto_ad(k+1,i) = xto_ad(k+1,i) + xtol_ad(k,i)
        dt_ad = dt_ad + xtol_ad(k,i)

        dqsdp_ad = dqsdp_ad + dq_ad*dp
        dt_ad    = dt_ad + dqsdt*dq_ad
        dqsdt_ad = dqsdt_ad + dq_ad*dt

        gamma_ad = gamma_ad - &
             (grav*dz+hvap*dqsdp*dp)/((cp*(one+gamma))**2) * &
             cp*dt_ad
        dqsdp_ad = dqsdp_ad + hvap*dt_ad*dp/(cp*(one+gamma))
        dz_ad    = dz_ad + grav*dt_ad/(cp*(one+gamma))

        xto_ad(k+1,i) = xto_ad(k+1,i) - &
             two*el2orc*xqeso(k+1,i)/(xto(k+1,i)**3) * gamma_ad
        xqeso_ad(k+1,i) = xqeso_ad(k,i) + &
             el2orc*gamma_ad/(xto(k+1,i)**2)

        rterm = one/(es*pprime)
        pprime_ad = pprime_ad - (qs*p(k+1,i)*desdt)*rterm**2 * &
             es*dqsdt_ad
        es_ad     = es_ad - (qs*p(k+1,i)*desdt)*rterm**2 * &
             dqsdt_ad*pprime
        desdt_ad  = desdt_ad + p(k+1,i)*qs*dqsdt_ad*rterm
        qs_ad     = qs_ad + p(k+1,i)*dqsdt_ad*desdt*rterm

        es_ad = es_ad + &
             desdt_ad*(factor1/xto(k+1,i) + factor2/(xto(k+1,i)**2))
        xto_ad(k+1,i) = xto_ad(k+1,i) - desdt_ad * &
             (es/(xto(k+1,i)**2)) * (factor1+two*factor2/xto(k+1,i))

        pprime_ad = pprime_ad + (qs/(pprime**2))*dqsdp_ad
        qs_ad     = qs_ad - dqsdp_ad/pprime

        pprime_ad = pprime_ad - (eps*es/(pprime**2))*qs_ad
        es_ad     = es_ad + eps*qs_ad/pprime
            
        es_ad = es_ad + epsm1*pprime_ad

        es0_ad = es0_ad + 10*es_ad

        adt = zero
        call fpvsx_ad(xto(k+1,i),es0,adt,es0_ad,adjoint)
        xto_ad(k+1,i) = xto_ad(k+1,i) + adt

        xzo_ad(k,i)   = xzo_ad(k,i)   - half*dz_ad
        xzo_ad(k+1,i) = xzo_ad(k+1,i) + half*dz_ad
            
     end do
  end do

! Adjoint of integration of hydrostatic equation to get xzo
  do i = 1,im
     do k = kmax(i),2,-1
!          
!       Redo nlm calculations
        DLNSIG = LOG(SL(K,i) / SL(K-1,i))
        term1  = dlnsig * rd / grav
!
!       Adjoint of tlm code
        term2_ad = zero

        term2_ad = term2_ad - term1*xzo_ad(k,i)
        xzo_ad(k-1,i) = xzo_ad(k-1,i) + xzo_ad(k,i)

        xtvo_ad(k-1,i) = xtvo_ad(k-1,i) + half*term2_ad
        xtvo_ad(k,i)      = xtvo_ad(k,i)      + half*term2_ad
     end do
  end do
  do i = 1,im
     dlnsig = log(sl(1,i))
     xtvo_ad(1,i) = xtvo_ad(1,i) - dlnsig*rd/grav * xzo_ad(1,i)
  end do
!
! Adjoint of xqeso and xtvo calculations
  do i = 1,im
     do k = kmax(i),1,-1
        call fpvsx_ad(xto(k,i),es0,adt,es0_ad,.false.)
        es = 10.0_r_kind*es0

        es_ad  = zero
        es0_ad = zero

        xqo_ad(k,i) = xqo_ad(k,i) + delta*xto(k,i)*xtvo_ad(k,i)
        xto_ad(k,i) = xto_ad(k,i) + delta*xqo(k,i)*xtvo_ad(k,i)
        xto_ad(k,i) = xto_ad(k,i) + xtvo_ad(k,i)

        es_ad = es_ad &
             - eps*es/((p(k,i)+epsm1*es)**2) * &
             epsm1*xqeso_ad(k,i)
        es_ad = es_ad &
             + eps*xqeso_ad(k,i)/(p(k,i)+epsm1*es)

        es0_ad = es0_ad + 10*es_ad
        adt = zero
        call fpvsx_ad(xto(k,i),es0,adt,es0_ad,adjoint)
        xto_ad(k,i) = xto_ad(k,i) + adt
     end do
  end do

!
! Adjoint of xto,xqo calculation
  do i = 1,im
     do k = kmax(i),1,-1
        dellat_ad = zero

        if ( k <= ktcon(i) .and. ktcon(i)/=1 )  then
           to_ad(k,i) = to_ad(k,i) + xto_ad(k,i)
           dellat_ad  = dellat_ad + xto_ad(k,i)*mbdt

           dellaq_ad(k,i) = dellaq_ad(k,i) - hvap*dellat_ad/cp
           dellah_ad(k,i) = dellah_ad(k,i) + dellat_ad/cp
               
           if (xqo0(k,i)>zero) then
              xqo0_ad(k,i) = xqo0_ad(k,i) + xqo_ad(k,i)
           else
              xqo0_ad(k,i) = zero
           endif
           qo0_ad(k,i)    = qo0_ad(k,i) + xqo0_ad(k,i)
           dellaq_ad(k,i) = dellaq_ad(k,i) + xqo0_ad(k,i)*mbdt
        else
           to_ad(k,i) = to_ad(k,i) + xto_ad(k,i)
           qo0_ad(k,i)= qo0_ad(k,i)+ xqo_ad(k,i)
        endif
     end do
  end do
!
! Adjoint of dellah and dellaq calculations at cloud top
  do i = 1,im
     if (ktcon(i)/=1) then
        indx = ktcon(i)
!
!       Redo nlm calculations
        dp   = 100.0_r_kind * psfc(i)*del(indx,i)
        dv1  = heol(indx-1,i)
        dvq1 = qol(indx-1,i)
        dvu1 = uol(indx-1,i)
        dvv1 = vol(indx-1,i)
!
!       Adjoint of tlm code
        dvv1_ad = zero
        dvu1_ad = zero
        dvq1_ad = zero
        dv1_ad  = zero

        qlko_ktcon_ad(i) = qlko_ktcon_ad(i)       + &
             eta(indx-1,i)*dellal_ad(i) *grav/dp
        eta_ad(indx-1,i) = eta_ad(indx-1,i) + &
             dellal_ad(i)    *qlko_ktcon(i)*grav/dp


        dvv1_ad = dvv1_ad &
             - eta(indx-1,i) * dellav_ad(indx,i) * (grav/dp)
        vcko_ad(indx-1,i) = vcko_ad(indx-1,i) &
             + eta(indx-1,i) * dellav_ad(indx,i) * (grav/dp)
        eta_ad (indx-1,i) = eta_ad (indx-1,i) &
             + dellav_ad(indx,i)*(vcko(indx-1,i)-dvv1)*(grav/dp)
        vol_ad (indx-1,i) = vol_ad (indx-1,i) + dvv1_ad

        dvu1_ad = dvu1_ad &
             - eta(indx-1,i) * dellau_ad(indx,i) * (grav/dp)
        ucko_ad(indx-1,i) = ucko_ad(indx-1,i) &
             + eta(indx-1,i) * dellau_ad(indx,i) * (grav/dp)
        eta_ad (indx-1,i) = eta_ad (indx-1,i) &
             + dellau_ad(indx,i)*(ucko(indx-1,i)-dvu1)*(grav/dp)
        uol_ad (indx-1,i) = uol_ad (indx-1,i) + dvu1_ad

        dvq1_ad = dvq1_ad &
             - eta(indx-1,i) * dellaq_ad(indx,i) * (grav/dp) 
        qcko_ad(indx-1,i) = qcko_ad(indx-1,i) &
             + eta(indx-1,i) * dellaq_ad(indx,i) * (grav/dp)
        eta_ad (indx-1,i) = eta_ad (indx-1,i) &
             + dellaq_ad(indx,i)*(qcko(indx-1,i)-dvq1)*(grav/dp)
        qol_ad (indx-1,i) = qol_ad (indx-1,i) + dvq1_ad

        dv1_ad = dv1_ad &
             - eta(indx-1,i) * dellah_ad(indx,i) * (grav/dp)
        hcko1_ad(indx-1,i) = hcko1_ad(indx-1,i) &
             + eta(indx-1,i) * dellah_ad(indx,i) * (grav/dp)
        eta_ad  (indx-1,i) = eta_ad  (indx-1,i) &
             + dellah_ad(indx,i)*(hcko1(indx-1,i)-dv1)*(grav/dp)
        heol_ad (indx-1,i) = heol_ad (indx-1,i) + dv1_ad

     endif
  end do
!
! Adjoint of dellah and dellaq calculations within cloud
!
  do i = 1,im
     do k = kmax(i)-1,2,-1
        if ( k < ktcon(i) .and. ktcon(i)/=1 ) then
!
!          Redo nlm calculations
           aup = one
           if (k <= kb(i)) aup = zero
           adw = one
           if (k > jmin(i)) adw = zero
!
           dv1   = heol(k,i)
           dv2   = half * (heol(k,i) + heol(k+1,i))
           dv3   = heol(k-1,i)
           dv1q  = qol(k,i)
           dv2q  = half * (qol(k,i) + qol(k+1,i))
           dv3q  = qol(k-1,i)

           DV1u  = uOL(k,i)
           DV2u  = half * (uOL(k,i) + uOL(k+1,i))
           DV3u  = uOL(k-1,i)
           DV1v  = vOL(k,i)
           DV2v  = half * (vOL(k,i) + vOL(k+1,i))
           DV3v  = vOL(k-1,i)

           dp    = 100.0_r_kind * psfc(i) * del(k,i)
           detau = eta(k,i) - eta(k-1,i)
           detad = etad(k,i) - etad(k-1,i)              
!
           term1 = aup*eta(k,i) - adw*edto1(i)*etad(k,i)
           term2 = aup*detau
           term3 = aup*eta(k-1,i) - adw*edto1(i)*etad(k-1,i)
           term4 = adw*edto1(i)*detad
           termq = half*(qrcdo(k,i)+qrcdo(k-1,i))
!
!          Initialize loop local adjoint variables to zero
           termq_ad = zero
           term4_ad = zero
           term3_ad = zero
           term2_ad = zero
           term1_ad = zero
           dv3v_ad  = zero
           dv2v_ad  = zero
           dv1v_ad  = zero
           dv3u_ad  = zero
           dv2u_ad  = zero
           dv1u_ad  = zero
           dv3q_ad  = zero
           dv2q_ad  = zero
           dv1q_ad  = zero
           dv3_ad   = zero
           dv2_ad   = zero
           dv1_ad   = zero
           detad_ad = zero
           detau_ad = zero
!
!          Adjoint of tlm code.
           vcdo_ad(i) = vcdo_ad(i) + (grav/dp)*term4*dellav_ad(k,i)
           term4_ad   = term4_ad   + (grav/dp)*dellav_ad(k,i)*vcdo(i)
           dv2v_ad    = dv2v_ad    - (grav/dp)*term2*dellav_ad(k,i)
           term2_ad   = term2_ad   - (grav/dp)*dellav_ad(k,i)*dv2v
           dv3v_ad    = dv3v_ad    - (grav/dp)*term3*dellav_ad(k,i)
           term3_ad   = term3_ad   - (grav/dp)*dellav_ad(k,i)*dv3v
           dv1v_ad    = dv1v_ad    + (grav/dp)*term1*dellav_ad(k,i)
           term1_ad   = term1_ad   + (grav/dp)*dellav_ad(k,i)*dv1v

           ucdo_ad(i) = ucdo_ad(i) + (grav/dp)*term4*dellau_ad(k,i)
           term4_ad   = term4_ad   + (grav/dp)*dellau_ad(k,i)*ucdo(i)
           dv2u_ad    = dv2u_ad    - (grav/dp)*term2*dellau_ad(k,i)
           term2_ad   = term2_ad   - (grav/dp)*dellau_ad(k,i)*dv2u
           dv3u_ad    = dv3u_ad    - (grav/dp)*term3*dellau_ad(k,i)
           term3_ad   = term3_ad   - (grav/dp)*dellau_ad(k,i)*dv3u
           dv1u_ad    = dv1u_ad    + (grav/dp)*term1*dellau_ad(k,i)
           term1_ad   = term1_ad   + (grav/dp)*dellau_ad(k,i)*dv1u
               
           termq_ad   = termq_ad   + (grav/dp)*term4*dellaq_ad(k,i)
           term4_ad   = term4_ad   + (grav/dp)*termq*dellaq_ad(k,i)
           dv2q_ad    = dv2q_ad    - (grav/dp)*term2*dellaq_ad(k,i)
           term2_ad   = term2_ad   - (grav/dp)*dv2q *dellaq_ad(k,i)
           dv3q_ad    = dv3q_ad    - (grav/dp)*term3*dellaq_ad(k,i)
           term3_ad   = term3_ad   - (grav/dp)*dv3q *dellaq_ad(k,i)
           dv1q_ad    = dv1q_ad    + (grav/dp)*term1*dellaq_ad(k,i)
           term1_ad   = term1_ad   + (grav/dp)*dv1q *dellaq_ad(k,i)
!
           hcdo_ad(i) = hcdo_ad(i) + (grav/dp)*term4  *dellah_ad(k,i)
           term4_ad   = term4_ad   + (grav/dp)*hcdo(i)*dellah_ad(k,i)
           dv2_ad     = dv2_ad     - (grav/dp)*term2*dellah_ad(k,i)
           term2_ad   = term2_ad   - (grav/dp)*dv2  *dellah_ad(k,i)
           dv3_ad     = dv3_ad     - (grav/dp)*term3*dellah_ad(k,i)
           term3_ad   = term3_ad   - (grav/dp)*dv3  *dellah_ad(k,i)
           dv1_ad     = dv1_ad     + (grav/dp)*term1*dellah_ad(k,i)
           term1_ad   = term1_ad   + (grav/dp)*dv1  *dellah_ad(k,i)
!
           qrcdo_ad(k-1,i) = qrcdo_ad(k-1,i) + half*termq_ad
           qrcdo_ad(k,i)   = qrcdo_ad(k,i)      + half*termq_ad

           detad_ad    = detad_ad + adw*edto1(i)*term4_ad
           edto1_ad(i) = edto1_ad(i) + adw*term4_ad*detad

           etad_ad(k-1,i)  = etad_ad(k-1,i) - adw*edto1(i)*term3_ad
           edto1_ad(i)     = edto1_ad(i) - adw*term3_ad*etad(k-1,i)
           eta_ad(k-1,i)   = eta_ad(k-1,i) + aup*term3_ad
           
           detau_ad = detau_ad + aup*term2_ad

           etad_ad(k,i) = etad_ad(k,i) - adw*edto1(i)*term1_ad
           edto1_ad(i)  = edto1_ad(i)  - adw*term1_ad*etad(k,i)
           eta_ad(k,i)  = eta_ad(k,i) + aup*term1_ad

           etad_ad(k-1,i) = etad_ad(k-1,i) - detad_ad
           etad_ad(k,i)   = etad_ad(k,i)   + detad_ad

           eta_ad(k-1,i) = eta_ad(k-1,i) - detau_ad
           eta_ad(k,i)   = eta_ad(k,i)   + detau_ad

           uol_ad(k-1,i) = uol_ad(k-1,i) + dv3u_ad
           uol_ad(k+1,i) = uol_ad(k+1,i) + half*dv2u_ad
           uol_ad(k,i)   = uol_ad(k,i)   + half*dv2u_ad
           uol_ad(k,i)   = uol_ad(k,i)   + dv1u_ad

           vol_ad(k-1,i) = vol_ad(k-1,i) + dv3v_ad
           vol_ad(k+1,i) = vol_ad(k+1,i) + half*dv2v_ad
           vol_ad(k,i)   = vol_ad(k,i)   + half*dv2v_ad
           vol_ad(k,i)   = vol_ad(k,i)   + dv1v_ad

           qol_ad(k-1,i) = qol_ad(k-1,i) + dv3q_ad
           qol_ad(k+1,i) = qol_ad(k+1,i) + half*dv2q_ad
           qol_ad(k,i)   = qol_ad(k,i)   + half*dv2q_ad
           qol_ad(k,i)   = qol_ad(k,i)   + dv1q_ad

           heol_ad(k-1,i) = heol_ad(k-1,i) + dv3_ad
           heol_ad(k+1,i) = heol_ad(k+1,i) + half*dv2_ad
           heol_ad(k,i)   = heol_ad(k,i)   + half*dv2_ad
           heol_ad(k,i)   = heol_ad(k,i)   + dv1_ad
!
        endif
     end do
  end do
!     
! Adjoint of dellah and dellaq calculations at k=1
  do i = 1,im
     if (ktcon(i)/=1) then
        dp = 100.0_r_kind * psfc(i) * del(1,i)

        vol_ad(1,i) = vol_ad(1,i) &
             - edto1(i)*etad(1,i)*dellav_ad(1,i)*grav/dp
        vcdo_ad(i) = vcdo_ad(i) &
             + edto1(i)*etad(1,i)*dellav_ad(1,i)*grav/dp
        etad_ad(1,i) = etad_ad(1,i) &
             + edto1(i)*dellav_ad(1,i)*(vcdo(i)-vol(1,i))*grav/dp
        edto1_ad(i) = edto1_ad(i) &
             + dellav_ad(1,i)*etad(1,i)*(vcdo(i)-vol(1,i))*grav/dp

        uol_ad(1,i) = uol_ad(1,i) &
             - edto1(i)*etad(1,i)*dellau_ad(1,i)*grav/dp
        ucdo_ad(i) = ucdo_ad(i) &
             + edto1(i)*etad(1,i)*dellau_ad(1,i)*grav/dp
        etad_ad(1,i) = etad_ad(1,i) &
             + edto1(i)*dellau_ad(1,i)*(ucdo(i)-uol(1,i))*grav/dp
        edto1_ad(i) = edto1_ad(i) &
             + dellau_ad(1,i)*etad(1,i)*(ucdo(i)-uol(1,i))*grav/dp

        qol_ad(1,i) = qol_ad(1,i) &
             - edto1(i)*etad(1,i)*dellaq_ad(1,i)*grav/dp
        qcdo_ad(i) = qcdo_ad(i) &
             + edto1(i)*etad(1,i)*dellaq_ad(1,i)*grav/dp
        etad_ad(1,i) = etad_ad(1,i) &
             + edto1(i)*dellaq_ad(1,i)*(qcdo(i)-qol(1,i))*grav/dp
        edto1_ad(i) = edto1_ad(i) &
             + dellaq_ad(1,i)*etad(1,i)*(qcdo(i)-qol(1,i))*grav/dp
            
        heol_ad(1,i) = heol_ad(1,i) & 
             - edto1(i)*etad(1,i)*dellah_ad(1,i)*grav/dp
        hcdo_ad(i) = hcdo_ad(i) &
             + edto1(i)*etad(1,i)*dellah_ad(1,i)*grav/dp
        etad_ad(1,i) = etad_ad(1,i) &
             + edto1(i)*dellah_ad(1,i)*(hcdo(i)-heol(1,i))*grav/dp
        edto1_ad(i) = edto1_ad(i) &
             + dellah_ad(1,i)*etad(1,i)*(hcdo(i)-heol(1,i))*grav/dp
     endif
  end do
!
! Adjoint of cloud mass flux threshold function based
! on cloud work function after downdraft
  do i = 1,im
     term_ad = zero
     term = 1.e3_r_kind*aa1(i)
     if (abs(term) < 100._r_kind) then
        term_ad = dftanh(term)*tcwfdn_ad(i)
     else
        if (term < -100._r_kind) then
           term_ad = zero
        else
           term_ad = zero
        endif
     endif
     aa1_ad(i) = aa1_ad(i) + 1.e3_r_kind*term_ad
  end do
!
! Adjoint of downdraft cloudwork function calculation
  do i = 1,im
     do k = 1,kmax(i)-1
        if (dwnflg2(i) .and. k < jmin(i)) then
!
!          Redo nlm calculations
           GAMMA = EL2ORC * QESOL(k+1,i) / TOL(k+1,i)**2
           DHH   = HCDO(I)
           DT    = TOL(k+1,i)
           DG    = GAMMA
           DH    = HESOL(k+1,i)
           dz    = zo(k,i) - zo(k+1,i)
!
           term1 = edto1(i)
           term2 = dz
           term3 = grav/(cp*dt)
           term4 = (dhh-dh)
           term5 = one/(one+dg)
           term6 = one + delta*cp*dg*dt/hvap
!     
           dqs = qesol(k+1,i) - qol(k+1,i)
!
!          Adjoint of tlm code
           dqs_ad = zero
           dz_ad  = zero
           dt_ad  = zero
           dg_ad  = zero
           dh_ad  = zero
           dhh_ad = zero
           if (dqs > zero) then
              dqs_ad = dqs_ad + edto1(i)*dz*grav*delta * aa1_ad(i)
              dz_ad  = dz_ad + edto1(i)*grav*delta*dqs * aa1_ad(i)
              edto1_ad(i) = edto1_ad(i) + dz*grav*delta*dqs * aa1_ad(i)
           endif
                  
           qol_ad(k+1,i)   = qol_ad  (k+1,i) - dqs_ad
           qesol_ad(k+1,i) = qesol_ad(k+1,i) + dqs_ad
! 
           term1_ad = term2*term3*term4*term5*term6 * aa1_ad(i)
           term2_ad = term1*term3*term4*term5*term6 * aa1_ad(i)
           term3_ad = term1*term2*term4*term5*term6 * aa1_ad(i)
           term4_ad = term1*term2*term3*term5*term6 * aa1_ad(i)
           term5_ad = term1*term2*term3*term4*term6 * aa1_ad(i)
           term6_ad = term1*term2*term3*term4*term5 * aa1_ad(i)
! 
           dt_ad  = (delta*cp/hvap) * dg*term6_ad
           dg_ad  = (delta*cp/hvap) * term6_ad*dt
           dg_ad  = dg_ad - one/(one+dg)**2 * term5_ad
           dh_ad  = dh_ad - term4_ad
           dhh_ad = dhh_ad + term4_ad
           dt_ad  = dt_ad - one*grav/(cp*dt*dt) * term3_ad
           dz_ad  = dz_ad + term2_ad
           edto1_ad(i) = edto1_ad(i) + term1_ad
!
           zo_ad(k,i)     = zo_ad(k,i)   + dz_ad
           zo_ad(k+1,i)   = zo_ad(k+1,i) - dz_ad
           hesol_ad(k+1,i)= hesol_ad(k+1,i) + dh_ad
           gamma_ad       = dg_ad
           tol_ad(k+1,i)  = tol_ad(k+1,i) + dt_ad
           hcdo_ad(i)     = hcdo_ad(i) + dhh_ad
           tol_ad(k+1,i)  = tol_ad(k+1,i) &
                - two*el2orc*qesol(k+1,i)/tol(k+1,i)**3 * gamma_ad
           qesol_ad(k+1,i)= qesol_ad(k+1,i) + &
                el2orc*gamma_ad/tol(k+1,i)**2
        endif
     end do
  end do
!
! Adjoint of final edto1 calculation
  do i = 1,im
     edtmax = edtmaxl
     if (nint(slimsk(i)) == 0) edtmax = edtmaxs
     if (dwnflg2(i)) then
        if (pwevo(i) < zero) then
           if (edto10(i) > edtmax) then
!             do nothing since tlm is zero
              edto1_ad(i) = zero
           endif
           pwevo_ad(i) = pwevo_ad(i) &
                + edto(i)*pwavo(i)/pwevo(i)**2 * edto1_ad(i)
           pwavo_ad(i) = pwavo_ad(i) &
                - edto(i)*edto1_ad(i)/pwevo(i)
           edto_ad(i) = edto_ad(i) &
                - edto1_ad(i)*pwavo(i)/pwevo(i)
        else
!          do nothing since tlm is zero
           edto1_ad(i) = zero
        endif
     else
!       do nothing since tlm is zero             
        edto1_ad(i) = zero
     endif
  enddo
!
! Adjoint of qrcdo --> qcdo transfer
  do i = 1,im
     qrcdo_ad(1,i) = qrcdo_ad(1,i) + qcdo_ad(i)
  end do
!
! Adjoint of pwevo, pwdo, qrcdo calculation
  do i = 1,im
     do k = 1,kmax(i)-1
        if (k < jmin(i)) then
!
!          Recalculate part of foward model
           dq    = qesol(k,i)
           dt    = tol(k,i)
           gamma = el2orc * dq / dt**2
           dh    = hcdo(i) - hesol(k,i)
           term5 = (one/hvap) * gamma
           term6 = one/(one+gamma)
           detad = etad(k+1,i) - etad(k,i)               
!
!          Adjoint of forward tlm
           term1_ad = zero
           term2_ad = zero
           term3_ad = zero
           pwdo_ad(k,i) = pwdo_ad(k,i) + pwevo_ad(i)

           if (k < jmin(i)-1) then
              term1_ad = term1_ad + pwdo_ad(k,i)
              term2_ad = term2_ad - pwdo_ad(k,i)
              term3_ad = term3_ad - pwdo_ad(k,i)

              qrcdo_ad(k+1,i) = qrcdo_ad(k+1,i) + detad*half*term3_ad
              qrcdo_ad(k,i) = qrcdo_ad(k,i) + detad*half*term3_ad
              detad_ad = +half*(qrcdo(k,i)+qrcdo(k+1,i))*term3_ad
              qrcdo_ad(k,i) = qrcdo_ad(k,i) + etad(k,i)*term2_ad
              etad_ad(k,i) = etad_ad(k,i) + qrcdo(k,i)*term2_ad
              qcdo_ad(i) =qcdo_ad(i) +term1_ad*etad(k+1,i)
              etad_ad(k+1,i) =etad_ad(k+1,i) +term1_ad*qcdo(i)
           endif

           etad_ad(k,i)   = etad_ad(k,i)      - detad_ad
           etad_ad(k+1,i) = etad_ad(k+1,i) + detad_ad

           dh_ad    = term5 * term6 * qrcdo_ad(k,i)
           term6_ad = term5 * qrcdo_ad(k,i) * dh
           term5_ad = qrcdo_ad(k,i) * term6 * dh
           dq_ad    = qrcdo_ad(k,i)

           gamma_ad = -one/(one+gamma)**2 * term6_ad
           gamma_ad = gamma_ad + (one/hvap) * term5_ad
           term5_ad = zero
           term6_ad = zero

           hesol_ad(k,i) = hesol_ad(k,i) - dh_ad
           hcdo_ad(i) = hcdo_ad(i) + dh_ad
           dh_ad = zero

           dt_ad = -two*el2orc*dq/dt**3 * gamma_ad
           dq_ad = dq_ad + el2orc*gamma_ad/dt**2
           gamma_ad = zero

           tol_ad(k,i) = tol_ad(k,i) + dt_ad
           qesol_ad(k,i) = qesol_ad(k,i) + dq_ad
           dt_ad = zero
           dq_ad = zero

        endif
     end do
  end do

!
! Adjoint of hcdo and qrcdo transfer
  do i = 1,im
     jmn = jmin(i)
     heol_ad(jmn,i)  = heol_ad(jmn,i) + hcdo_ad(i)
     qesol_ad(jmn,i) = qesol_ad(jmn,i) + qrcdo_ad(jmn,i)
     uol_ad(jmn,i)   = uol_ad(jmn,i) + ucdo_ad(i)
     vol_ad(jmn,i)   = vol_ad(jmn,i) + vcdo_ad(i)
  end do
!
! Adjoint of etad (downdraft entrainment) calculation at k=1
  k = 1
  do i = 1,im
     dz_ad = zero
     if (kbdtr(i) > 1) then
!
!       Redo nlm
        dz = half * (zo(2,i)-zo(1,i))
!
!       Adjoint of tlm code.
        dz_ad = dz_ad + &
             etad(k+1,i)*exp(xlamd(i)*dz) * xlamd(i)*etad_ad(k,i)
        xlamd_ad(i) = xlamd_ad(i) + &
             etad(k+1,i)*exp(xlamd(i)*dz) * dz*etad_ad(k,i)
        etad_ad(k+1,i) = etad_ad(k+1,i) + &
             exp(xlamd(i)*dz) * etad_ad(k,i)

        zo_ad(1,i) = zo_ad(1,i) - half*dz_ad
        zo_ad(2,i) = zo_ad(2,i) + half*dz_ad
     endif
  end do
!
! Adjoint of etad (downdraft entrainment) calculation above k=1
  do i = 1,im
     do k = 2,kbmax(i)
        dz_ad = zero
        if (k < kbdtr(i)) then
!
!          Redo nlm
           dz = half * (zo(k+1,i)-zo(k-1,i))
!
!          Adjoint of tlm code.
           dz_ad = dz_ad + &
                etad(k+1,i)*exp(xlamd(i)*dz) * xlamd(i)*etad_ad(k,i)
           xlamd_ad(i) = xlamd_ad(i) + &
                etad(k+1,i)*exp(xlamd(i)*dz) * dz*etad_ad(k,i)
           etad_ad(k+1,i) = etad_ad(k+1,i) + &
                exp(xlamd(i)*dz) * etad_ad(k,i)
           
           zo_ad(k-1,i) = zo_ad(k-1,i) - half*dz_ad
           zo_ad(k+1,i) = zo_ad(k+1,i) + half*dz_ad
        endif
     end do
  end do
!
! Adjoint of xlamd calculation
  do i = 1,im
     dz_ad = zero
     beta = betas
     if (nint(slimsk(i)) == 1) beta = betal
     if (kbdtr(i) > 1) then
!
!       Redo nlm calculations
        dz = half * (zo(kbdtr(i),i) + zo(kbdtr(i)-1,i)) &
             - zo(1,i)
!
!       Adjoint code
        dz_ad = dz_ad - log(beta)/(dz*dz)*xlamd_ad(i)
        zo_ad(1,i) = zo_ad(1,i) - dz_ad
        zo_ad(kbdtr(i)-1,i) = zo_ad(kbdtr(i)-1,i) + half*dz_ad
        zo_ad(kbdtr(i),i)      = zo_ad(kbdtr(i),i)      + half*dz_ad
     endif
  end do
!
! Adjoint of upper and lower bounds on precipitation efficiency.
  do i = 1,im
     edt_ad(i) = edt_ad(i) + edtx_ad(i)
     edt_ad(i) = edt_ad(i) + edto_ad(i)
     if (edt0(i) > 0.9_r_kind) then
        edt_ad(i) = zero
     elseif (edt0(i) < zero) then
        edt_ad(i) = zero
     endif
  end do
!
! Adjoint for precipitation efficiency (edt) calculation
  do i = 1,im
     e1_ad   = zero
     term_ad = zero
     dz_ad   = zero
!
!    Redo nlm calculations
     dz = zo(ktcon(i),i)-zo(kb(i),i)
     if (abs(dz) > tiny_r_kind) then
        term = 1.e3_r_kind * vshear(i)/dz
!
!       Adjoint code
        e1_ad   = e1_ad - edt_ad(i)

        term_ad = term_ad + pcpeff3*three*term**2 * e1_ad
        term_ad = term_ad + pcpeff2*two*term * e1_ad
        term_ad = term_ad + pcpeff1 * e1_ad

        dz_ad = dz_ad - 1.e3_r_kind*vshear(i)/(dz*dz) * term_ad
        vshear_ad(i) = vshear_ad(i) + 1.e3_r_kind*term_ad/dz
     else
        term_ad = zero
        dz_ad = zero
     endif

     zo_ad(kb(i),i) = zo_ad(kb(i),i) - dz_ad
     zo_ad(ktcon(i),i) = zo_ad(ktcon(i),i) + dz_ad

  end do
!
! Adjoint of windshear calculation
  do i = 1,im
     do k = kmax(i),1,-1
        shear_ad = zero
        termv_ad = zero
        termu_ad = zero
        if (k >= kb(i) .and. k <= ktcon(i)) then
!
!          Adjoint code for vshear
           shear_ad = shear_ad + vshear_ad(i)
!
!          Redo nlm calculations
           termu = (uol(k+1,i) - uol(k,i))**2
           termv = (vol(k+1,i) - vol(k,i))**2
!
!          Adjoint code for uol and vol
           if (termu+termv > tiny_r_kind) then
              termv_ad = termv_ad + &
                   rcs(i)*(half/sqrt(termu+termv)) * shear_ad
              termu_ad = termu_ad + &
                   rcs(i)*(half/sqrt(termu+termv)) * shear_ad
           else
              shear_ad = zero
              termu_ad = zero
              termv_ad = zero
           endif

           vol_ad(k,i)      = vol_ad(k,i)      &
                - two*(vol(k+1,i)-vol(k,i))*termv_ad
           vol_ad(k+1,i) = vol_ad(k+1,i) &  
                + two*(vol(k+1,i)-vol(k,i))*termv_ad

           uol_ad(k,i)      = uol_ad(k,i)      &
                - two*(uol(k+1,i)-uol(k,i))*termu_ad
           uol_ad(k+1,i) = uol_ad(k+1,i) &  
                + two*(uol(k+1,i)-uol(k,i))*termu_ad

        endif
     end do
  end do
!
! Adjoint of cloud mass flux threshold function based
! on updraft cloud work function
  do i = 1,im
     term_ad = zero
     term = 1.e3_r_kind*aa10(i)
     if (abs(term) < 100._r_kind) then
        term_ad = dftanh(term)*tcwfup_ad(i)
     else
        if (term < -100.0_r_kind) then
           term_ad = zero
        else
           term_ad = zero
        endif
     endif
     aa1_ad(i) = aa1_ad(i) + 1.e3_r_kind*term_ad
  end do
!
!
! Adjoint of updraft cwf calculation
  do i=1,im
     do k=kmax(i),1,-1
        if (k > kbcon(i).and.k <= ktcon(i)) then
!
!          Recalculate part of forward model
           dz1   = zo(k,i) - zo(k-1,i)
           gamma = el2orc * qesol(k-1,i) / (tol(k-1,i)**2)
           rfact =  one + delta*cp*gamma*tol(k-1,i)/hvap
           term1 = dz1*grav/cp
           term2 = one/tol(k-1,i)
           term3 = dbyo(k-1,i)
           term4 = one/(one+gamma)
           term5 = rfact
           dqs = qesol(k-1,i) - qol(k-1,i)
!
!          Adjoint of forward model tlm
           dqs_ad = zero
           dz1_ad = zero
           if (dqs > zero) then
              dqs_ad   = dqs_ad + dz1*grav*delta * aa1_ad(i)
              dz1_ad   = dz1_ad + aa1_ad(i) * grav*delta*dqs
           endif

           qol_ad(k-1,i)   = qol_ad  (k-1,i) - dqs_ad
           qesol_ad(k-1,i) = qesol_ad(k-1,i) + dqs_ad

           term5_ad = term1*term2*term3*term4*aa1_ad(i)
           term4_ad = term1*term2*term3*term5*aa1_ad(i)
           term3_ad = term1*term2*term4*term5*aa1_ad(i)
           term2_ad = term1*term3*term4*term5*aa1_ad(i)
           term1_ad = term2*term3*term4*term5*aa1_ad(i)

           rfact_ad = term5_ad
           gamma_ad = -one/((one+gamma)**2) * term4_ad
           dbyo_ad(k-1,i) = dbyo_ad(k-1,i) + term3_ad
           tol_ad(k-1,i) = tol_ad(k-1,i) - &
                (one/(tol(k-1,i)**2)) * term2_ad
           dz1_ad = dz1_ad + term1_ad*grav/cp

           tol_ad(k-1,i) = tol_ad(k-1,i) + &
                delta*cp*gamma*rfact_ad/hvap
           gamma_ad = gamma_ad + &
                delta*cp*rfact_ad*tol(k-1,i)/hvap

           tol_ad(k-1,i) = tol_ad(k-1,i) - &
                two*el2orc*qesol(k-1,i)/(tol(k-1,i)**3)* &
                gamma_ad
           qesol_ad(k-1,i) = qesol_ad(k-1,i) + &
                el2orc*gamma_ad/(tol(k-1,i)**2)

           zo_ad(k,i)   = zo_ad(k,i)   + dz1_ad
           zo_ad(k-1,i) = zo_ad(k-1,i) - dz1_ad

        endif
     end do
  end do

! Adjoint of section is for cloud liquid water
  if (ncloud > 0) then
     do i=1,im
        k = ktcon(i)
        if (cnvflg(i)) then

!          Recompute nlm
           gamma = EL2ORC * QESOl(K,i) / (TOl(K,i)**2)
           QRCH = QESOl(K,i) &
                + GAMMA * DBYO(K,i) / (HVAP * (one + GAMMA))
           DQ = qcko00(K-1,i) - QRCH

!          Adjoint of tangent linear model
           dq_ad   = zero
           qrch_ad = zero
           gamma_ad= zero
           
           if (dq > zero) then
              qrch_ad = qrch_ad + qcko_ad(k-1,i)
              dq_ad   = dq_ad + qlko_ktcon_ad(i)
           endif

           qrch_ad = qrch_ad - dq_ad
           qcko_ad(k-1,i) = qcko_ad(k-1,i) + dq_ad

           gamma_ad = gamma_ad + &
                qrch_ad*dbyo(k,i)/(hvap*(one+gamma)*(one+gamma))
           dbyo_ad(k,i) = dbyo_ad(k,i) + &
                gamma*qrch_ad/(hvap*(one+gamma))
           qesol_ad(k,i) = qesol_ad(k,i) + qrch_ad

           tol_ad(k,i) = tol_ad(k,i) - gamma_ad * &
                two*el2orc*qesol(k,i)/(tol(k,i)**3)
           qesol_ad(k,i) = qesol_ad(k,i) + &
                el2orc*gamma_ad/(tol(k,i)**2)
        endif
     end do
  endif

!
! Adjoint of pwavo, pwo, qcko, and aa1 calculation
!
  do i = 1,im
     do k = kmax(i),1,-1
        if (k > kb(i).and.k < ktcon(i)) then
!
!          Recalculate part of forward model
           factor = eta(k-1,i)/eta(k,i)
           onemf  = one - factor
           temp   = qcko0(k,i)
           gamma  = el2orc*qesol(k,i)/(tol(k,i)**2) 
           qrch   = qesol(k,i) + &
                gamma*dbyo(k,i)/(hvap*(one+gamma))
           dq = dqk(k,i)
!
           if (dq > zero) then
!
!             Redo nlm
              dz   = half*(zo(k+1,i)-zo(k-1,i))
              dz1  = zo(k,i) - zo(k-1,i)
              etah = half*(eta(k,i)+eta(k-1,i))
              qlk  = dq/(eta(k,i) + etah*c0*dz)
!
!             Adjoint of tlm
              temp_ad = zero
              qc_ad = qcko_ad(k,i)
              qrch_ad = qc_ad
              qlk_ad = qc_ad

              pwo_ad(k,i) = pwo_ad(k,i) + pwavo_ad(i)
                   
              qlk_ad  = qlk_ad + etah*c0*dz*pwo_ad(k,i)
              dz_ad   = etah*c0*pwo_ad(k,i)*qlk
              etah_ad = pwo_ad(k,i)*c0*dz*qlk

              qlk_ad = qlk_ad  - dz1*grav*aa1_ad(i)
              dz1_ad = -one*aa1_ad(i)*grav*qlk
                   
              dz_ad  = dz_ad - &
                   (dq/((eta(k,i)+etah*c0*dz)**2))* &
                   etah*c0*qlk_ad
              etah_ad = etah_ad - &
                   (dq/((eta(k,i)+etah*c0*dz)**2))* &
                   qlk_ad*c0*dz
              eta_ad(k,i) = eta_ad(k,i) - &
                   (dq/((eta(k,i)+etah*c0*dz)**2))* &
                   qlk_ad
              dq_ad = qlk_ad/(eta(k,i)+etah*c0*dz)
              
              eta_ad(k-1,i) = eta_ad(k-1,i) + half*etah_ad
              eta_ad(k,i)   = eta_ad(k,i)      + half*etah_ad
                   
              zo_ad(k-1,i) = zo_ad(k-1,i) - dz1_ad
              zo_ad(k,i)   = zo_ad(k,i)      + dz1_ad
              zo_ad(k-1,i) = zo_ad(k-1,i) - half*dz_ad
              zo_ad(k+1,i) = zo_ad(k+1,i) + half*dz_ad

           else
              qc_ad   = zero
              qrch_ad = zero
              qlk_ad  = zero
              dz_ad   = zero
              etah_ad = zero
              dz1_ad  = zero
              dq_ad   = zero
              temp_ad = qcko_ad(k,i)
           endif
              
           qrch_ad = qrch_ad - eta(k,i)*dq_ad
           temp_ad = temp_ad + eta(k,i)*dq_ad
           eta_ad(k,i) = eta_ad(k,i) + dq_ad*(temp-qrch)
!
           gamma_ad = -gamma*dbyo(k,i)/((hvap*(one+gamma))**2)* &
                hvap*qrch_ad
           dbyo_ad(k,i) = dbyo_ad(k,i) + &
                gamma*qrch_ad/(hvap*(one+gamma))
           gamma_ad = gamma_ad + &
                qrch_ad*dbyo(k,i)/(hvap*(one+gamma))
           qesol_ad(k,i) = qesol_ad(k,i) + qrch_ad
           
           tol_ad(k,i) = tol_ad(k,i) - &
                el2orc*2*qesol(k,i)/(tol(k,i)**3)*gamma_ad
           qesol_ad(k,i) = qesol_ad(k,i) + &
                el2orc/(tol(k,i)**2)*gamma_ad
              
           qol_ad(k+1,i) = qol_ad(k+1,i) + onemf*half*temp_ad
           qol_ad(k,i)   = qol_ad(k,i)   + onemf*half*temp_ad
           onemf_ad = temp_ad*half*(qol(k,i)+qol(k+1,i))
           qcko_ad(k-1,i) = qcko_ad(k-1,i) + factor*temp_ad
           factor_ad = temp_ad*qcko(k-1,i)
              
           factor_ad = factor_ad - onemf_ad
                   
           eta_ad(k,i)      = eta_ad(k,i)      - &
                (eta(k-1,i)/(eta(k,i)**2))*factor_ad
           eta_ad(k-1,i) = eta_ad(k-1,i) + (one/eta(k,i))*factor_ad

        endif
     end do
  end do
!
! Adjoint of qol to qcko transfer
  do i = 1,im
     indx = kb(i)
     qol_ad(indx,i) = qol_ad(indx,i) + qcko_ad(indx,i)
  end do

!
! Adjoint of hcko,hckod transfer to hcko1 and dbyo calculation
  do i = 1,im
     do k = kmax(i)-1,2,-1
        if (k > kb(i)) then
           hesol_ad(k,i) = hesol_ad(k,i) - dbyo_ad(k,i)
           hcko1_ad(k,i) = hcko1_ad(k,i) + dbyo_ad(k,i)
        endif
        if (k > kb(i).and.k <= kbcon(i)) then
           hcko_ad(k,i) = hcko_ad(k,i) + hcko1_ad(k,i)
        elseif (k > kbcon(i).and.k <= ktcon(i)) then
           if (.not.dwnflg(i)) then
              hcko_ad(k,i) = hcko_ad(k,i) + hcko1_ad(k,i)
           else
              hckod_ad(k,i) = hckod_ad(k,i) + hcko1_ad(k,i)
           endif
        elseif (k > ktcon(i)) then
           hcko_ad(k,i) = hcko_ad(k,i) + hcko1_ad(k,i)
        endif
     end do
  end do

!
! Adjoint of modification of hcko1 by detrainment process
  do i = 1,im
     do k = kmax(i)-1,2,-1
        if (k > kbcon(i) .and. k <= ktcon(i)) then
!
!          Redo nlm calculations
           factor = eta(k-1,i)/eta(k,i)
           onemf  = one - factor
           fuv    = etau(k-1,i)/etau(k,i)
           onemfu = one - fuv
           heol2  = half*(heol(k,i) + heol(k+1,i))
           uol2   = half*(uol(k,i)+uol(k+1,i))
           vol2   = half*(vol(k,i)+vol(k+1,i))
!
!          Adjoint code
           fuv_ad    = zero
           onemfu_ad = zero
           onemf_ad  = zero
           factor_ad = zero
           heol2_ad  = zero
           vol2_ad   = zero
           uol2_ad   = zero

           vol2_ad   = vol2_ad + onemfu*vckod_ad(k,i)
           onemfu_ad = onemfu_ad + vckod_ad(k,i)*vol2
           vckod_ad(k-1,i) = vckod_ad(k-1,i) + fuv*vckod_ad(k,i)
           fuv_ad = fuv_ad + vckod_ad(k,i)*vckod(k-1,i)

           uol2_ad   = uol2_ad + onemfu*ukcod_ad(k,i)
           onemfu_ad = onemfu_ad + ukcod_ad(k,i)*uol2
           ukcod_ad(k-1,i) = ukcod_ad(k-1,i) + fuv*ukcod_ad(k,i)
           fuv_ad = fuv_ad + ukcod_ad(k,i)*uckod(k-1,i)
           
           heol2_ad = heol2_ad + onemf*hckod_ad(k,i)
           onemf_ad = onemf_ad + heol2*hckod_ad(k,i)
           hckod_ad(k-1,i) = hckod_ad(k-1,i) + factor*hckod_ad(k,i)
           factor_ad = factor_ad + hckod(k-1,i)*hckod_ad(k,i)
               
           heol_ad(k,i)   = heol_ad(k,i)   + half*heol2_ad
           heol_ad(k+1,i) = heol_ad(k+1,i) + half*heol2_ad

           fuv_ad = fuv_ad - onemfu_ad
           etau_ad(k-1,i) = etau_ad(k-1,i) + fuv_ad/etau(k,i)
           etau_ad(k,i) = etau_ad(k,i) -  fuv_ad * &
                etau(k-1,i)/(etau(k,i)**2)

           factor_ad = factor_ad - onemf_ad

           eta_ad(k,i) = eta_ad(k,i) - &
                factor_ad*(eta(k-1,i)/(eta(k,i)**2))
           eta_ad(k-1,i) = eta_ad(k-1,i) + factor_ad/eta(k,i)
        endif
     end do
  end do

!
! Adjoint of hcko to hckod transfer at kbcon
  do i = 1,im
     if (dwnflg(i)) then
        indx = kbcon(i)
        hcko_ad(indx,i) = hcko_ad(indx,i) + hckod_ad(indx,i)
        ucko_ad(indx,i) = ucko_ad(indx,i) + ukcod_ad(indx,i)
        vcko_ad(indx,i) = vcko_ad(indx,i) + vckod_ad(indx,i)
     endif
  end do
!
! Adjoint of eta calculation
  do i = 1,im
     if (dwnflg(i)) then
        do k = kmax(i)-1,2,-1
           if (k > jmin(i) .and. k <= kt2(i)) then

!             Redo nlm calculation                  
              dz = half*(zo(k+1,i)-zo(k-1,i))

!             Adjoint code
              dz_ad = zero

              dz_ad = dz_ad + etau(k-1,i) * &
                   (xlamdet(i)+xlambu)*etau_ad(k,i)

              xlamdet_ad(i) = xlamdet_ad(i) + & 
                   etau(k-1,i)*etau_ad(k,i)*dz

              etau_ad(k-1,i) = etau_ad(k-1,i) + & 
                   etau_ad(k,i)*(one+(xlamdet(i)+xlambu)*dz)

              dz_ad = dz_ad + eta(k-1,i)*xlamdet(i)*eta_ad(k,i)

              xlamdet_ad(i) = xlamdet_ad(i) + &
                   eta(k-1,i)*eta_ad(k,i)*dz

              eta_ad(k-1,i) = eta_ad(k-1,i) + &
                   eta_ad(k,i)*(one+xlamdet(i)*dz)

              zo_ad(k-1,i) = zo_ad(k-1,i) - half*dz_ad
              zo_ad(k+1,i) = zo_ad(k+1,i) + half*dz_ad
           endif
        end do
     else
        do k=kmax(i)-1,2,-1
           if (k > jmin(i) .and. k <= ktcon0(i)) then

!             Redo nlm calculation                  
              DZ = half * (ZO(k+1,i) - ZO(k-1,i))

!             Adjoint code
              dz_ad = zero

              dz_ad = dz_ad + etau(k-1,i)*xlambu*etau_ad(k,i)
              etau_ad(k-1,i) = etau_ad(k-1,i) + &
                   etau_ad(k,i)*(one+xlambu*dz)
           endif
        end do
     endif
  end do


!
!
! Adjoint of modification of cloud properties below cloud
! base via the entrainment process
  do i = 1,im
     tem1 = hcko(jmin(i),i) - hesol(kt2(i),i)
     tem2 = sumz(kt2(i),i) * hesol(kt2(i),i) - sumh(kt2(i),i)

     tem1_ad = zero
     tem2_ad = zero

     if (sumz(kt2(i),i)  >  0.000001_r_kind) then
        term = 2.3_r_kind/sumz(kt2(i),i)
        if (xlamdet1(i) < term) then
           xlamdet1_ad(i) = xlamdet1_ad(i) + xlamdet_ad(i)
        else
           term_ad = zero
           term_ad = term_ad + xlamdet_ad(i)
           sumz_ad(kt2(i),i) = sumz_ad(kt2(i),i) - term_ad * &
                2.3_r_kind/(sumz(kt2(i),i)**2)
        endif
     else
        xlamdet1_ad(i) = xlamdet1_ad(i) + xlamdet_ad(i)
     endif

     if (xlamdet0(i) >= zero) then
        xlamdet0_ad(i) = xlamdet0_ad(i) + xlamdet1_ad(i)
     else
        xlamdet0_ad(i) = zero
     endif

     if (abs(tem2)  >  0.000001_r_kind) then
        tem1_ad = zero
        tem2_ad = zero
        tem1_ad = tem1_ad + xlamdet0_ad(i)/tem2
        tem2_ad = tem2_ad - tem1/(tem2**2) * xlamdet0_ad(i)
     else
        tem1_ad = zero
        tem2_ad = zero
     endif

     sumz_ad(kt2(i),i) =sumz_ad(kt2(i),i) + tem2_ad*hesol(kt2(i),i)
     hesol_ad(kt2(i),i)=hesol_ad(kt2(i),i)+ tem2_ad*sumz(kt2(i),i)
     sumh_ad(kt2(i),i) =sumh_ad(kt2(i),i) - tem2_ad
         
     hesol_ad(kt2(i),i) = hesol_ad(kt2(i),i) - tem1_ad
     hcko_ad(jmin(i),i)  = hcko_ad(jmin(i),i) + tem1_ad

  end do

  do i=1,im
     do k=kmax(i)-1,2,-1
        if (k > jmin(i) .and. k <= ktcon0(i)) then

           heol_ad(k,i)      = heol_ad(k,i) + &
                half * (zo(k+1,i) - zo(k-1,i)) * sumh_ad(k,i)
           zo_ad(k-1,i)   = zo_ad(k-1,i)   - half*sumh_ad(k,i)*heol(k,i)
           zo_ad(k+1,i)   = zo_ad(k+1,i)   + half*sumh_ad(k,i)*heol(k,i)
           sumh_ad(k-1,i) = sumh_ad(k-1,i) + sumh_ad(k,i)
           
           zo_ad(k-1,i)   = zo_ad(k-1,i)   - half*sumz_ad(k,i)
           zo_ad(k+1,i)   = zo_ad(k+1,i)   + half*sumz_ad(k,i)
           sumz_ad(k-1,i) = sumz_ad(k-1,i) + sumz_ad(k,i)
        endif
     end do
  end do

! Adjoint of cloud property below cloud base is modified by the 
! entrainment process
  do i=1,im
     do k=kmax(i)-1,2,-1

!       Adjoint of last if-then block in tlm
        if (k > kbcon(i)) then
           hcko_ad(kbcon(i),i) = hcko_ad(kbcon(i),i) + hcko_ad(k,i)
           ucko_ad(kbcon(i),i) = ucko_ad(kbcon(i),i) + ucko_ad(k,i)
           vcko_ad(kbcon(i),i) = vcko_ad(kbcon(i),i) + vcko_ad(k,i)
        endif
!
!       Adjoint of first if-then block in tlm
        if (k > kb(i).and.k <= kbcon(i)) then
!
!          Redo nlm calculations
           factor = eta0(k-1,i)/eta0(k,i)
           onemf  = one - factor
!
!          Adjoint code
           factor_ad = zero
           onemf_ad = zero

           vol_ad(k+1,i) = vol_ad(k+1,i) + onemf*half*vcko_ad(k,i)
           vol_ad(k,i)   = vol_ad(k,i)      + onemf*half*vcko_ad(k,i)
           onemf_ad = onemf_ad + vcko_ad(k,i)*half* (vol(k,i)+vol(k+1,i))
           vcko_ad(k-1,i) = vcko_ad(k-1,i) + factor*vcko_ad(k,i)
           factor_ad = factor_ad + vcko_ad(k,i)*vcko0(k-1,i)

           uol_ad(k+1,i) = uol_ad(k+1,i) + onemf*half*ucko_ad(k,i)
           uol_ad(k,i)   = uol_ad(k,i)   + onemf*half*ucko_ad(k,i)
           onemf_ad = onemf_ad + ucko_ad(k,i)*half* (uol(k,i)+uol(k+1,i))
           ucko_ad(k-1,i) = ucko_ad(k-1,i) + factor*ucko_ad(k,i)
           factor_ad = factor_ad + ucko_ad(k,i)*ucko0(k-1,i)

           heol_ad(k+1,i) = heol_ad(k+1,i) + onemf*half*hcko_ad(k,i)
           heol_ad(k,i)   = heol_ad(k,i)      + onemf*half*hcko_ad(k,i)
           onemf_ad = onemf_ad + hcko_ad(k,i)*half*(heol(k,i)+heol(k+1,i))
           hcko_ad(k-1,i) = hcko_ad(k-1,i) + factor*hcko_ad(k,i)
           factor_ad = factor_ad + hcko_ad(k,i)*hcko0(k-1,i)
               

           factor_ad = factor_ad - onemf_ad
               
           eta_ad(k,i) = eta_ad(k,i) - factor_ad* &
                eta0(k-1,i)/(eta0(k,i)*eta0(k,i))
           eta_ad(k-1,i) = eta_ad(k-1,i) + factor_ad/eta0(k,i)
        endif
     end do
  end do
!
! Adjoint of hkbo --> hcko transfer
  do i = 1,im
     indx = kb(i)
     hkbo_ad(i) = hkbo_ad(i) + hcko_ad(indx,i)
     ukbo_ad(i) = ukbo_ad(i) + ucko_ad(indx,i)
     vkbo_ad(i) = vkbo_ad(i) + vcko_ad(indx,i)
  end do
!
! Adjoint of updraft mass flux calculation
  do i = 1,im
     dz_ad = zero
     if (kb(i)==1 .and. kbcon(i) > 1) then
        dz = half*(zo(2,i)-zo(1,i))
        eta_ad(1,i) = eta_ad(1,i) + etau_ad(1,i)
        dz_ad = dz_ad - &
             eta0(2,i)*exp(-xlamb(i)*dz)*xlamb(i)* &
             eta_ad(1,i)
        xlamb_ad(i) = xlamb_ad(i) - &
             eta0(2,i)*exp(-xlamb(i)*dz)*dz*eta_ad(1,i)
        eta_ad(2,i) = eta_ad(2,i) + eta_ad(1,i)* &
             exp(-xlamb(i)*dz)
        zo_ad(1,i) = zo_ad(1,i) - half*dz_ad
        zo_ad(2,i) = zo_ad(2,i) + half*dz_ad
     endif
  end do
  do i = 1,im
     do k = 2,kbmax(i)
        dz_ad = zero
        if (k < kbcon(i).and.k >= kb(i)) then
           dz = half * (zo(k+1,i) - zo(k-1,i))
           eta_ad(k,i) = eta_ad(k,i) + etau_ad(k,i)
           dz_ad = dz_ad - eta0(k+1,i)*exp(-xlamb(i)*dz) * &
                xlamb(i)*eta_ad(k,i)
           xlamb_ad(i) = xlamb_ad(i) - &
                eta0(k+1,i)*exp(-xlamb(i)*dz)*dz*eta_ad(k,i)
           eta_ad(k+1,i) = eta_ad(k+1,i) + eta_ad(k,i)* &
                exp(-xlamb(i)*dz)
           zo_ad(k-1,i) = zo_ad(k-1,i) - half*dz_ad
           zo_ad(k+1,i) = zo_ad(k+1,i) + half*dz_ad
        endif
     end do
  end do
!
! Adjoint of determination of entrainment rate between kb and kbcon
  do i = 1,im
!
!    Recalculate nlm
     alpha = alphas
     if (nint(slimsk(i)) == 1) alpha = alphal
     if (kb(i)==1) then
        dz = half * (zo(kbcon(i),i)+zo(kbcon(i)-1,i)) - zo(1,i)
     else
        dz = half*(zo(kbcon(i),i)+zo(kbcon(i)-1,i)) - &
             half*(zo(kb(i),i)  +zo(kb(i)-1,i))
     endif
!
!    Adjoint of tlm code.
     dz_ad = zero
     if (kbcon(i)/=kb(i)) then
        dz_ad = dz_ad + two*log(alpha)/(dz*dz) * xlamb_ad(i)
     else
        dz_ad = zero
     endif
     if (kb(i)==1) then
        zo_ad(1,i)          = zo_ad(1,i)             - dz_ad
        zo_ad(kbcon(i)-1,i) = zo_ad(kbcon(i)-1,i) + half*dz_ad
        zo_ad(kbcon(i),i)   = zo_ad(kbcon(i),i)      + half*dz_ad
     else
        zo_ad(kb(i)-1,i)    = zo_ad(kb(i)-1,i)    - half*dz_ad
        zo_ad(kb(i),i)      = zo_ad(kb(i),i)         - half*dz_ad
        zo_ad(kbcon(i),i)   = zo_ad(kbcon(i),i)      + half*dz_ad
        zo_ad(kbcon(i)-1,i) = zo_ad(kbcon(i)-1,i) + half*dz_ad
     endif
  end do
!
! Adjoint of dot to pdot transfer at kbcon
  do i = 1,im
     dot0_ad(kbcon(i),i) = dot0_ad(kbcon(i),i) + 10.0_r_kind*pdot_ad(i)
  end do
!
! Adjoint of heol --> hkbo transfer
  do i = 1,im
     indx = kb(i)
     heol_ad(indx,i) = heol_ad(indx,i) + hkbo_ad(i)
     uol_ad(indx,i)  = uol_ad(indx,i) + ukbo_ad(i)
     vol_ad(indx,i)  = vol_ad(indx,i) + vkbo_ad(i)
  end do
!
! Adjoint of heol and hesol calculation at kmax(i)
  do i = 1,im
     k=kmax(i)
     heo_ad(k,i)  = heo_ad(k,i) + heol_ad(k,i)
     heso_ad(k,i) = heso_ad(k,i) + hesol_ad(k,i)
  end do
!
! Adjoint of heol and hesol calculation below kmax      
  do i = 1,im

     do k = kmax(i)-1,1,-1
!
!       Make necessary nlm calculations
        PO  = half * (P(k,i) + P(k+1,i))
        call fpvsx_ad(tol(k,i),es0,adt,es0_ad,.false.)
        esl = 10.0_r_kind*es0

!
!       Zero local adjoint variables
        es0_ad = zero
        esl_ad = zero
!
!       Adjoint of tlm code
        vo_ad(k+1,i)  = vo_ad(k+1,i) + half*vol_ad(k,i)
        vo_ad(k,i)    = vo_ad(k,i)   + half*vol_ad(k,i)

        uo_ad(k+1,i)  = uo_ad(k+1,i) + half*uol_ad(k,i)
        uo_ad(k,i)    = uo_ad(k,i)   + half*uol_ad(k,i)

        qesol_ad(k,i) = qesol_ad(k,i)+ hvap*hesol_ad(k,i)
        tol_ad(k,i)   = tol_ad(k,i)  + cp*hesol_ad(k,i)
        zo_ad(k+1,i)  = zo_ad(k+1,i) + half*grav*hesol_ad(k,i)
        zo_ad(k,i)    = zo_ad(k,i)   + half*grav*hesol_ad(k,i)

        qol_ad(k,i)   = qol_ad(k,i)  + hvap*heol_ad(k,i)
        tol_ad(k,i)   = tol_ad(k,i)  + cp*heol_ad(k,i)
        zo_ad(k+1,i)  = zo_ad(k+1,i) + half*grav*heol_ad(k,i)
        zo_ad(k,i)    = zo_ad(k,i)   + half*grav*heol_ad(k,i)

        esl_ad = esl_ad - eps*esl/((po+epsm1*esl)**2) * &
             epsm1 * qesol_ad(k,i)
        esl_ad = esl_ad + eps*qesol_ad(k,i)/(po+epsm1*esl)

        es0_ad = es0_ad + 10*esl_ad

        adt = zero
        call fpvsx_ad(tol(k,i),es0,adt,es0_ad,adjoint)
        tol_ad(k,i) = tol_ad(k,i) + adt

     end do
  end do


!
! Adjoint of tol and qol calculation
  do i = 1,im
     do k = kmax(i)-1,1,-1
!
!       Make necessary nlm calculations
        DZ    = half * (ZO(k+1,i) - ZO(k,i))
        DP    = half * (P(k+1,i) - P(k,i))
!       ES    = 10.0_r_kind * FESB(TO(k+1,i))
        call fpvsx_ad(to(k+1,i),es0,adt,es0_ad,.false.)
        es = 10.0_r_kind*es0
        PPRIME= P(k+1,i) + EPSM1 * ES
        QS    = EPS * ES / PPRIME
        DQSDP = - QS / PPRIME
        DESDT = ES * (FACTOR1 / TO(k+1,i) + FACTOR2 / (TO(k+1,i)**2))
        DQSDT = QS * P(k+1,i) * DESDT / (ES * PPRIME)
        GAMMA = EL2ORC * QESO(k+1,i) / (TO(k+1,i)**2)
        DT    = (GRAV * DZ + HVAP * DQSDP * DP) / (CP * (one + GAMMA))
!
!       Initialize local ajm variables to zero
        es0_ad    = zero
        dq_ad     = zero
        dt_ad     = zero
        dqsdp_ad  = zero
        dqsdt_ad  = zero
        gamma_ad  = zero
        dz_ad     = zero
        pprime_ad = zero
        es_ad     = zero
        desdt_ad  = zero
        qs_ad     = zero
!
!       Adjoint of tlm code
        if (qol0(k,i)>zero) then
           qol0_ad(k,i) = qol0_ad(k,i) + qol_ad(k,i)
        else
           qol0_ad(k,i) = zero
        endif

        qo_ad(k+1,i) = qo_ad(k+1,i) + qol0_ad(k,i)
        dq_ad = dq_ad + qol0_ad(k,i)

        to_ad(k+1,i) = to_ad(k+1,i) + tol_ad(k,i)
        dt_ad = dt_ad + tol_ad(k,i)

        dqsdp_ad = dqsdp_ad + dq_ad*dp
        dt_ad    = dt_ad + dqsdt*dq_ad
        dqsdt_ad = dqsdt_ad + dq_ad*dt

        gamma_ad = gamma_ad - &
             (grav*dz+hvap*dqsdp*dp)/((cp*(one+gamma))**2) * &
             cp*dt_ad
        dqsdp_ad = dqsdp_ad + hvap*dt_ad*dp/(cp*(one+gamma))
        dz_ad    = dz_ad + grav*dt_ad/(cp*(one+gamma))

        to_ad(k+1,i) = to_ad(k+1,i) - &
             two*el2orc*qeso(k+1,i)/(to(k+1,i)**3) * gamma_ad
        qeso_ad(k+1,i) = qeso_ad(k,i) + & 
             el2orc*gamma_ad/(to(k+1,i)**2)

        rterm = one/(es*pprime)
        pprime_ad = pprime_ad - (qs*p(k+1,i)*desdt)*rterm**2 * es*dqsdt_ad
        es_ad     = es_ad - (qs*p(k+1,i)*desdt)*rterm**2 * dqsdt_ad*pprime
        desdt_ad  = desdt_ad + p(k+1,i)*qs*dqsdt_ad*rterm
        qs_ad     = qs_ad + p(k+1,i)*dqsdt_ad*desdt*rterm

        to_ad(k+1,i) = to_ad(k+1,i) + desdt_ad * es * &
             (-one*factor1/(to(k+1,i)**2) - two*factor2/(to(k+1,i)**3))
        es_ad = es_ad + desdt_ad * &
             (factor1/to(k+1,i) + factor2/(to(k+1,i)**2))

        pprime_ad = pprime_ad + (qs/(pprime**2))*dqsdp_ad
        qs_ad     = qs_ad - dqsdp_ad/pprime

        pprime_ad = pprime_ad - (eps*es/(pprime**2))*qs_ad
        es_ad     = es_ad + eps*qs_ad/pprime
 
        es_ad = es_ad + epsm1*pprime_ad

        es0_ad = es0_ad + 10.0_r_kind*es_ad
        adt=zero
        call fpvsx_ad(to(k+1,i),es0,adt,es0_ad,.false.)
        to_ad(k+1,i) = to_ad(k+1,i) + adt

        zo_ad(k,i)   = zo_ad(k,i)      - half*dz_ad
        zo_ad(k+1,i) = zo_ad(k+1,i) + half*dz_ad
 
     end do
  end do
!            
!
! Adjoint of moist static energy calculation
  do i = 1,im
     do k = 1,kmax(i)
        qeso_ad(k,i) = qeso_ad(k,i) + hvap*heso_ad(k,i)
        to_ad(k,i)   = to_ad(k,i) + cp*heso_ad(k,i)
        zo_ad(k,i)   = zo_ad(k,i) + grav*heso_ad(k,i)
        qo_ad(k,i)   = qo_ad(k,i) + hvap*heo_ad(k,i)
        to_ad(k,i)   = to_ad(k,i) + cp*heo_ad(k,i)
        zo_ad(k,i)   = zo_ad(k,i) + grav*heo_ad(k,i)
     end do
  end do
!
!
! Adjoint of zo calculation
  do i = 1,im
     do k = kmax(i),2,-1
        dlnsig = log(sl(k,i)/sl(k-1,i))
        term1  = dlnsig * rd / grav
        zo_ad(k-1,i) = zo_ad(k-1,i) + zo_ad(k,i)
        term2_ad = -one*term1*zo_ad(k,i)
        tvo_ad(k-1,i) = tvo_ad(k-1,i) + half*term2_ad
        tvo_ad(k,i)   = tvo_ad(k,i)      + half*term2_ad
     end do
  end do
!
  do i = 1,im
     dlnsig = log(sl(1,i))
     tvo_ad(1,i) = tvo_ad(1,i) - (dlnsig*rd/grav)*zo_ad(1,i)
  end do
!
!
! Adjoint of initial qeso and tvo calculation.
  do i = 1,im
     do k = 1,kmax(i)
        call fpvsx_ad(to(k,i),es0,adt,es0_ad,.false.)
        es = 10.0_r_kind*es0

        qo_ad(k,i) = qo_ad(k,i) + delta*to(k,i)*tvo_ad(k,i)
        to_ad(k,i) = to_ad(k,i) + delta*qo(k,i)*tvo_ad(k,i)
        to_ad(k,i) = to_ad(k,i) + tvo_ad(k,i)

        es_ad = zero
        es_ad = es_ad + qeso_ad(k,i) * &
             eps*p(k,i)/(p(k,i)+epsm1*es)**2

        es0_ad = zero
        es0_ad = 10.0_r_kind*es_ad

        adt=zero
        call fpvsx_ad(to(k,i),es0,adt,es0_ad,adjoint)
        to_ad(k,i) = to_ad(k,i) + adt

     end do
  end do
!
! Adjoint of initial 0 --> o transfer 


  do i = 1,im
     do k = 1,km
        q0_ad(k,i)   = q0_ad(k,i) + qo0_ad(k,i)
        cwm0_ad(k,i) = cwm0_ad(k,i) + cwmo_ad(k,i)
        v0_ad(k,i)   = v0_ad(k,i) + vo_ad(k,i)
        u0_ad(k,i)   = u0_ad(k,i) + uo_ad(k,i)

        if (q0(k,i)>zero) then
           q0_ad(k,i) = q0_ad(k,i) + qo_ad(k,i)
        else
           q0_ad(k,i) = zero
        endif
        t0_ad(k,i)   = t0_ad(k,i) + to_ad(k,i)
     end do
  end do

!
! End of routine
  return

! Define internal functions
contains

  real(r_kind) function ftanh(x)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ftanh
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-07  lueken - added subprogram doc block
!
!   input argument list:
!    x
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),intent(in   ) :: x

    ftanh = half*(one + tanh(x))
  end function ftanh

  real(r_kind) function dftanh(x)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dftanh
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-07  lueken - added subprogram doc block
!
!   input argument list:
!    x
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),intent(in   ) :: x

    dftanh = half/(cosh(x)**2)
  end function dftanh

end subroutine nlmsas_ad_im_ix_

end module nlmsas_ad_mod
