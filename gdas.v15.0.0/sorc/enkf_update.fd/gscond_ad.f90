module gscond_ad_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    gscond_ad_mod   module wrapper around subroutine gscond_ad
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract: This module has been added as a wrapper around subroutine gscond_ad
!            to eliminate type mismatch compile errors when using the debug
!            compile option on WCOSS.
!
! program history log:
!   2012-01-26  parrish
!
! subroutines included:
!  gscond_ad
!  gscond_ad_1_1_
!  gscond_ad_im_ix_

  implicit none

! set default to private
   private
! set subroutines to public
  public :: gscond_ad

  interface gscond_ad
     module procedure gscond_ad_1_1_
     module procedure gscond_ad_im_ix_
  end interface

contains

subroutine gscond_ad_1_1_( im, ix, km, dt, sl_, ps_, rhc_, advt_, advq_, &
     advp_, q_in_, cwm_in_, t_in_, q_out_, cwm_out_, t_out_, advt_ad_, advq_ad_, &
     advp_ad_, q_in_ad_, cwm_in_ad_, t_in_ad_, q_out_ad_, cwm_out_ad_, t_out_ad_, &
     adjoint )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gscond_ad_1_1_
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract:  interface for gscond_ad, where im=1,ix=1, and calling routine has
!              no dimension index corresponding to im and ix.
!
! program history log:
!   2013-01-26  parrish - initial documentation
!
!   input argument list:
!     im,ix,km
!     dt,adjoint
!     sl_(km),ps_,rhc_(km),advt_(km),advq_(km),advp_(km),q_in_(km),cwm_in_(km),t_in_(km)
!     q_out_(km),cwm_out_(km),t_out_(km),advt_ad_(km),advq_ad_(km),advp_ad_(km)
!     q_in_ad_(km),cwm_in_ad_(km),t_in_ad_(km),q_out_ad_(km),cwm_out_ad_(km),t_out_ad_(km)
!
!   output argument list:
!     q_out_(km),cwm_out_(km),t_out_(km),advt_ad_(km),advq_ad_(km),advp_ad_(km)
!     q_in_ad_(km),cwm_in_ad_(km),t_in_ad_(km),q_out_ad_(km),cwm_out_ad_(km),t_out_ad_(km)

  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind), intent(in   ) :: im,ix,km
  real(r_kind),    intent(in   ) :: dt
  logical,         intent(in   ) :: adjoint

  real(r_kind),    intent(in   ) :: sl_(km),ps_,rhc_(km) 
  real(r_kind),    intent(in   ) :: advt_(km),advq_(km),advp_(km)
  real(r_kind),    intent(in   ) :: q_in_(km),cwm_in_(km),t_in_(km)

  real(r_kind),    intent(inout) :: q_out_(km),cwm_out_(km),t_out_(km)
  real(r_kind),    intent(inout) :: advt_ad_(km),advq_ad_(km),advp_ad_(km)
  real(r_kind),    intent(inout) :: q_in_ad_(km),cwm_in_ad_(km),t_in_ad_(km)
  real(r_kind),    intent(inout) :: q_out_ad_(km),cwm_out_ad_(km),t_out_ad_(km)


  real(r_kind)                   :: sl(km,ix),ps(im),rhc(km,ix)
  real(r_kind)                   :: advt(km,im),advq(km,im),advp(km,im)
  real(r_kind)                   :: q_in(km,ix),cwm_in(km,ix),t_in(km,ix)
  real(r_kind)                   :: q_out(km,ix),cwm_out(km,ix),t_out(km,ix)
  real(r_kind)                   :: advt_ad(km,im),advq_ad(km,im),advp_ad(km,im)
  real(r_kind)                   :: q_in_ad(km,ix),cwm_in_ad(km,ix),t_in_ad(km,ix)
  real(r_kind)                   :: q_out_ad(km,ix),cwm_out_ad(km,ix),t_out_ad(km,ix)
  integer(i_kind) k

  if( im /= 1 .or. ix /= 1 ) then
     write(6,*)' GSCOND_AD_1_1_, IM,IX=',IM,IX,' -- BOTH MUST BE 1.  PROGRAM FAILS'
     stop
  end if

  ps(1)=ps_
  do k=1,km
     sl(k,1)=sl_(k)
     rhc(k,1)=rhc_(k)
     advt(k,1)=advt_(k)
     advq(k,1)=advq_(k)
     advp(k,1)=advp_(k)
     q_in(k,1)=q_in_(k)
     cwm_in(k,1)=cwm_in_(k)
     t_in(k,1)=t_in_(k)
     q_out(k,1)=q_out_(k)
     cwm_out(k,1)=cwm_out_(k)
     t_out(k,1)=t_out_(k)
     advt_ad(k,1)=advt_ad_(k)
     advq_ad(k,1)=advq_ad_(k)
     advp_ad(k,1)=advp_ad_(k)
     q_in_ad(k,1)=q_in_ad_(k)
     cwm_in_ad(k,1)=cwm_in_ad_(k)
     t_in_ad(k,1)=t_in_ad_(k)
     q_out_ad(k,1)=q_out_ad_(k)
     cwm_out_ad(k,1)=cwm_out_ad_(k)
     t_out_ad(k,1)=t_out_ad_(k)
  end do

  call gscond_ad_im_ix_( im, ix, km, dt, sl, ps, rhc, advt, advq, &
     advp, q_in, cwm_in, t_in, q_out, cwm_out, t_out, advt_ad, advq_ad, &
     advp_ad, q_in_ad, cwm_in_ad, t_in_ad, q_out_ad, cwm_out_ad, t_out_ad, &
     adjoint )

  do k=1,km
     q_out_(k)=q_out(k,1)
     cwm_out_(k)=cwm_out(k,1)
     t_out_(k)=t_out(k,1)
     advt_ad_(k)=advt_ad(k,1)
     advq_ad_(k)=advq_ad(k,1)
     advp_ad_(k)=advp_ad(k,1)
     q_in_ad_(k)=q_in_ad(k,1)
     cwm_in_ad_(k)=cwm_in_ad(k,1)
     t_in_ad_(k)=t_in_ad(k,1)
     q_out_ad_(k)=q_out_ad(k,1)
     cwm_out_ad_(k)=cwm_out_ad(k,1)
     t_out_ad_(k)=t_out_ad(k,1)
  end do

end subroutine gscond_ad_1_1_

subroutine gscond_ad_im_ix_( im, ix, km, dt, sl, ps, rhc, advt, advq, &
     advp, q_in, cwm_in, t_in, q_out, cwm_out, t_out, advt_ad, advq_ad, &
     advp_ad, q_in_ad, cwm_in_ad, t_in_ad, q_out_ad, cwm_out_ad, t_out_ad, &
     adjoint )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gscond_ad    forward & adjoint model for GFS gridscale condensation
!     prgmmr:    treadon     org: np23                date: 2003-12-18
!
! abstract:  This subroutine contains the forward and ajoint models for the
!            GFS gridscale condensation algorithm
!
! program history log:
!   2003-12-18  treadon - initial routine
!   2004-06-14  treadon - reformat documenation
!   2006-04-12  treadon - change sl from 1d to 2d array
!   2008-06-02  safford - rm unused var
!   2013-01-26  parrish - module added as a wrapper around subroutine gscond_ad
!                            to eliminate type mismatch compile errors when using the debug
!                            compile option on WCOSS.
!
!   input argument list:
!     im       - actual number of profiles to be processed
!     ix       - maximum number of profiles to process (array dimension)
!     km       - number of levels in vertical profile
!     dt       - physics timestep
!     sl       - "sigma" value at layer midpoints
!     ps       - surface pressure
!     rhc      - critcal relative humidity thresholds
!     advt     - temperature advection
!     advq     - moisture advection
!     advp     - pressure advection
!     q_in     - specific humidity
!     cwm_in   - cloud condensate mixing ratio
!     t_in     - temperature
!     advt_ad   - temperature advection perturbation
!     advq_ad   - moisture advection perturbation
!     advp_ad   - pressure advection perturbation
!     q_out_ad  - moisture perturbation
!     cwm_out_ad- cloud condesate mixing ratio perturbation
!     t_out_ad  - temperature perturbation
!     adjoint  - logical flag (.false.=forward model only, .true.=forward and ajoint)
!
!   output argument list:
!     q_out    - moisture following gridscale condensation
!     cwm_out  - cloud condensation mixing ratio following gridscale condensation
!     t_out    - temperature following gridscale condensation
!     q_in_adt  - change in condensation with respect to moisture
!     cwm_in_ad - change in condensation with respect to cloud condensation mixing ratio
!     t_in_ad   - change in condensation with respect to temperature
!
! remarks:
!    The adjoint portion of this routine was generated by the
!    Tangent linear and Adjoint Model Compiler,  TAMC 5.3.0
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!==============================================
! all entries are defined explicitly
!==============================================
  use kinds, only: r_kind,i_kind
  use constants, only: zero, half, one, two, el2orc, cclimit, elocp, rcp, &
       cp, cpr, h1000, eps, climit, epsq, epsm1, hsub, hvap, ttp
  implicit none

!==============================================
! define arguments
!==============================================
  logical        ,intent(in   ) :: adjoint
  integer(i_kind),intent(in   ) :: im
  integer(i_kind),intent(in   ) :: km
  real(r_kind)   ,intent(inout) :: advp_ad(km,im)
  real(r_kind)   ,intent(inout) :: advq_ad(km,im)
  real(r_kind)   ,intent(inout) :: advt_ad(km,im)
  integer(i_kind),intent(in   ) :: ix
  real(r_kind)   ,intent(  out) :: cwm_in_ad(km,ix)
  real(r_kind)   ,intent(inout) :: cwm_out_ad(km,ix)
  real(r_kind)   ,intent(  out) :: q_in_ad(km,ix)
  real(r_kind)   ,intent(inout) :: q_out_ad(km,ix)
  real(r_kind)   ,intent(  out) :: t_in_ad(km,ix)
  real(r_kind)   ,intent(inout) :: t_out_ad(km,ix)
  real(r_kind)   ,intent(in   ) :: advp(km,im)
  real(r_kind)   ,intent(in   ) :: advq(km,im)
  real(r_kind)   ,intent(in   ) :: advt(km,im)
  real(r_kind)   ,intent(in   ) :: cwm_in(km,ix)
  real(r_kind)   ,intent(  out) :: cwm_out(km,ix)
  real(r_kind)   ,intent(in   ) :: dt
  real(r_kind)   ,intent(in   ) :: ps(im)
  real(r_kind)   ,intent(in   ) :: q_in(km,ix)
  real(r_kind)   ,intent(  out) :: q_out(km,ix)
  real(r_kind)   ,intent(in   ) :: rhc(km,ix)
  real(r_kind)   ,intent(in   ) :: sl(km,ix)
  real(r_kind)   ,intent(in   ) :: t_in(km,ix)
  real(r_kind)   ,intent(  out) :: t_out(km,ix)

!==============================================
! define local variables
!==============================================
  real(r_kind) aa
  real(r_kind) ab
  real(r_kind) ac
  real(r_kind) ad
  real(r_kind) aa_ad
  real(r_kind) ab_ad
  real(r_kind) ac_ad
  real(r_kind) ad_ad
  real(r_kind) ae_ad
  real(r_kind) af_ad
  real(r_kind) ag_ad
  real(r_kind) ai_ad
  real(r_kind) ap_ad
  real(r_kind) aq_ad
  real(r_kind) at_ad
  real(r_kind) ccrik_ad
  real(r_kind) ccrik1_ad
  real(r_kind) cond_ad
  real(r_kind) cond1_ad
  real(r_kind) cond2_ad
  real(r_kind) cond3_ad
  real(r_kind) condi_ad
  real(r_kind) cone0_ad
  real(r_kind) cwmik_ad
  real(r_kind) cwmin_ad
  real(r_kind) delq_1_ad
  real(r_kind) delq_2_ad
  real(r_kind) delq_3_ad
  real(r_kind) e0_ad
  real(r_kind) e1_ad
  real(r_kind) e2_ad
  real(r_kind) e3_ad
  real(r_kind) e4_ad
  real(r_kind) es_1_ad
  real(r_kind) es_2_ad
  real(r_kind) es_3_ad
  real(r_kind) esk_ad
  real(r_kind) qik_ad
  real(r_kind) qin_ad
  real(r_kind) qs_1_ad
  real(r_kind) qs_2_ad
  real(r_kind) qs_3_ad
  real(r_kind) qsik_ad
  real(r_kind) qsk_ad
  real(r_kind) rqik_ad
  real(r_kind) rqikk_ad
  real(r_kind) tik_ad
  real(r_kind) tmt0_ad
  real(r_kind) tsq_1_ad
  real(r_kind) tsq_2_ad
  real(r_kind) tsq_3_ad
  real(r_kind) tx1_1_ad
  real(r_kind) tx1_2_ad
  real(r_kind) tx1_3_ad
  real(r_kind) tx2_2_ad
  real(r_kind) tx2_3_ad
  real(r_kind) tx2_4_ad
  real(r_kind) tx3_1_ad
  real(r_kind) tx3_2_ad
  real(r_kind) tx3_3_ad
  real(r_kind) ae
  real(r_kind) af
  real(r_kind) ag
  real(r_kind) ai
  real(r_kind) ap
  real(r_kind) aq
  real(r_kind) at
  real(r_kind) ccrik
  real(r_kind) ccrik1
  real(r_kind) cond
  real(r_kind) cond1
  real(r_kind) cond2
  real(r_kind) cond3
  real(r_kind) condi
  real(r_kind) cone0
  real(r_kind) cwmik
  real(r_kind) cwmin
  real(r_kind) delq_1
  real(r_kind) delq_2
  real(r_kind) delq_3
  real(r_kind) dum1
  real(r_kind) dum2
  real(r_kind) e0
  real(r_kind) e1
  real(r_kind) e2
  real(r_kind) e3
  real(r_kind) e4
  real(r_kind) elv,elvk(km,ix)
  real(r_kind) es_1,es_1k(km,ix)
  real(r_kind) es_2,es_2k(km,ix)
  real(r_kind) es_3,es_3k(km,ix)
  real(r_kind) esk,eski(km,ix)
  integer(i_kind) i
  integer(i_kind) iw0,iw0k(km,ix)
  integer(i_kind) iw1,iw1k(km,ix)
  integer(i_kind) k
  real(r_kind) pres
  real(r_kind) prsk
  real(r_kind) qik
  real(r_kind) qin
  real(r_kind) qs_1
  real(r_kind) qs_2
  real(r_kind) qs_3
  real(r_kind) qsik
  real(r_kind) qsk,qski(km,ix)
  real(r_kind) rdt
  real(r_kind) rqik
  real(r_kind) rqikk
  real(r_kind) tik
  real(r_kind) tmt0
  real(r_kind) tsq_1
  real(r_kind) tsq_2
  real(r_kind) tsq_3
  real(r_kind) tx1_1
  real(r_kind) tx1_2
  real(r_kind) tx1_3
  real(r_kind) tx2_2
  real(r_kind) tx2_3
  real(r_kind) tx2_4
  real(r_kind) tx3_1
  real(r_kind) tx3_2
  real(r_kind) tx3_3
  real(r_kind) u00ik
  real(r_kind) us00


!----------------------------------------------
! RESET LOCAL ADJOINT VARIABLES
!----------------------------------------------
  aa_ad = zero
  ab_ad = zero
  ac_ad = zero
  ad_ad = zero
  ae_ad = zero
  af_ad = zero
  ag_ad = zero
  ai_ad = zero
  ap_ad = zero
  aq_ad = zero
  at_ad = zero
  ccrik_ad = zero
  ccrik1_ad = zero
  cond_ad = zero
  cond1_ad = zero
  cond2_ad = zero
  cond3_ad = zero
  condi_ad = zero
  cone0_ad = zero
  cwmik_ad = zero
  cwmin_ad = zero
  delq_1_ad = zero
  delq_2_ad = zero
  delq_3_ad = zero
  e0_ad = zero
  e1_ad = zero
  e2_ad = zero
  e3_ad = zero
  e4_ad = zero
  es_1_ad = zero
  es_2_ad = zero
  es_3_ad = zero
  esk_ad = zero
  qik_ad = zero
  qin_ad = zero
  qs_1_ad = zero
  qs_2_ad = zero
  qs_3_ad = zero
  qsik_ad = zero
  qsk_ad = zero
  rqik_ad = zero
  rqikk_ad = zero
  tik_ad = zero
  tmt0_ad = zero
  tsq_1_ad = zero
  tsq_2_ad = zero
  tsq_3_ad = zero
  tx1_1_ad = zero
  tx1_2_ad = zero
  tx1_3_ad = zero
  tx2_2_ad = zero
  tx2_3_ad = zero
  tx2_4_ad = zero
  tx3_1_ad = zero
  tx3_2_ad = zero
  tx3_3_ad = zero

!----------------------------------------------
! ROUTINE BODY
!----------------------------------------------
!----------------------------------------------
! FUNCTION AND TAPE COMPUTATIONS
!----------------------------------------------
  rdt = one/dt
  do i = 1, im
     iw0 = 0
     iw1 = iw0
     iw0k(km,i) = iw0
     iw1k(km,i) = iw0
     do k = km, 1, -1
        cwmin = cwm_in(k,i)
        tik = t_in(k,i)
        qin = q_in(k,i)
        if (qin > epsq) then
           qik = qin
        else
           qik = epsq
        endif
        if (cwmin > climit) then
           cwmik = cwmin
        else
           cwmik = climit
        endif
        prsk = ps(i)*sl(k,i)
        call fpvsx_ad(tik,esk,dum1,dum2,.false.)
        qsk = eps*esk/(prsk+epsm1*esk)
        if (qsk > epsq) then
           qsik = qsk
        else
           qsik = epsq
        endif
        

!       Save forward model calculation for use in adjoint model
        eski(k,i) = esk
        qski(k,i) = qsk
!
        
        tmt0 = tik-ttp
        if (tmt0 < (-15._r_kind)) then
           u00ik = rhc(k,i)
           if (qik-u00ik*qsik > zero .or. cwmik > climit) then
              iw0 = 1
           else
              iw0 = 0
           endif
        endif
        if (tmt0 >= zero) then
           iw0 = 0
        endif
        if (tmt0 < zero .and. tmt0 >= (-15._r_kind)) then
           iw0 = 0
           if (k < km) then
              if (iw1 == 1 .and. cwmik > climit) then
                 iw0 = 1
              endif
           endif
        endif
        if (iw0 == 0) then
           elv = hvap
        else
           elv = hsub
        endif
        iw1 = iw0

!       Save forward model values for adjoint model
        iw0k(k,i) = iw0
        iw1k(k,i) = iw1
        elvk(k,i) = elv
        

        u00ik = rhc(k,i)
        pres = prsk*h1000
        at = advt(k,i)
        aq = advq(k,i)
        ap = advp(k,i)
        if (qsik <= 1.e-10_r_kind) then
           rqik = zero
        else
           rqik = qik/qsik
        endif
        if (rqik < u00ik) then
           ccrik = zero
        else if (rqik >= one) then
           ccrik = one
        else
           if (rqik < one) then
              rqikk = rqik
           else
              rqikk = one
           endif
           ccrik = one-sqrt((one-rqikk)/(one-u00ik))
        endif
        if (ccrik <= cclimit .and. cwmik > climit) then
           tx1_1 = tik
           tx3_1 = qik
           call fpvsx_ad(tx1_1,es_1,dum1,dum2,.false.)
           es_1k(k,i) = es_1
           qs_1 = u00ik*eps*es_1/(prsk+epsm1*es_1)
           tsq_1 = tx1_1*tx1_1
           delq_1 = half*(qs_1-tx3_1)*tsq_1/(tsq_1+el2orc*qs_1)
           tx2_2 = delq_1
           tx1_2 = tx1_1-delq_1*elocp
           tx3_2 = tx3_1+delq_1
           call fpvsx_ad(tx1_2,es_2,dum1,dum2,.false.)
           es_2k(k,i) = es_2
           qs_2 = u00ik*eps*es_2/(prsk+epsm1*es_2)
           tsq_2 = tx1_2*tx1_2
           delq_2 = (qs_2-tx3_2)*tsq_2/(tsq_2+el2orc*qs_2)
           tx2_3 = tx2_2+delq_2
           tx1_3 = tx1_2-delq_2*elocp
           tx3_3 = tx3_2+delq_2
           call fpvsx_ad(tx1_3,es_3,dum1,dum2,.false.)
           es_3k(k,i) = es_3
           qs_3 = u00ik*eps*es_3/(prsk+epsm1*es_3)
           tsq_3 = tx1_3*tx1_3
           delq_3 = (qs_3-tx3_3)*tsq_3/(tsq_3+el2orc*qs_3)
           tx2_4 = tx2_3+delq_3
           e4 = tx2_4*rdt
           if (e4 < zero) then
              e3 = zero
           else
              e3 = e4
           endif
           if (cwmik*rdt < e3) then
              e2 = cwmik*rdt
           else
              e2 = e3
           endif
           if (e2 < zero) then
              e1 = zero
           else
              e1 = e2
           endif
           e0 = e1
        else
           e0 = zero
        endif
        if (ccrik > cclimit .and. qsik > epsq) then
           us00 = one-u00ik
           ccrik1 = one-ccrik
           aa = eps*elv*pres*qik
           ab = ccrik*ccrik1*qsik*us00
           ac = ab+half*cwmik
           ad = ab*ccrik1
           ae = cpr*tik*tik
           af = ae*pres
           ag = aa*elv
           ai = cp*aa
           cond1 = (ac-ad)*(af*aq-ai*at+ae*qik*ap)/(ac*(af+ag))
           condi = (qik-u00ik*qsik*one)*rdt
           if (cond1 < condi) then
              cond2 = cond1
           else
              cond2 = condi
           endif
           if (cond2 > zero) then
              cond3 = cond2
           else
              cond3 = zero
           endif
           cond = cond3
        else
           cond = zero
        endif
        cone0 = (cond-e0)*dt
        cwm_out(k,i) = cwmin+cone0
        t_out(k,i) = tik+elv*rcp*cone0
        q_out(k,i) = qin-cone0
     end do
  end do
  
  if (.not.adjoint) return

!----------------------------------------------
! ADJOINT COMPUTATIONS
!----------------------------------------------
  do i = im, 1, -1
     do k = 1, km
        iw0 = 0
        iw1 = iw0
        
!       Load values saved from forward model.
        iw0 = iw0k(k,i)
        iw1 = iw1k(k,i)
        elv = elvk(k,i)
        esk = eski(k,i)
        qsk = qski(k,i)

!
!       Redo some forward model calculations
        cwmin = cwm_in(k,i)
        tik = t_in(k,i)
        qin = q_in(k,i)
        if (qin > epsq) then
           qik = qin
        else
           qik = epsq
        endif
        if (cwmin > climit) then
           cwmik = cwmin
        else
           cwmik = climit
        endif
        prsk = ps(i)*sl(k,i)
        if (qsk > epsq) then
           qsik = qsk
        else
           qsik = epsq
        endif
        u00ik = rhc(k,i)
        pres = prsk*h1000
        at = advt(k,i)
        aq = advq(k,i)
        ap = advp(k,i)
        if (qsik <= 1.e-10_r_kind) then
           rqik = zero
        else
           rqik = qik/qsik
        endif
        if (rqik < u00ik) then
           ccrik = zero
        else if (rqik >= one) then
           ccrik = one
        else
           if (rqik < one) then
              rqikk = rqik
           else
              rqikk = one
           endif
           ccrik = one-sqrt((one-rqikk)/(one-u00ik))
        endif

!       Adjoint model
        cone0_ad        = cone0_ad-q_out_ad  (k,i)
        qin_ad          = qin_ad  +q_out_ad  (k,i)
        q_out_ad(k,i)   = zero
        cone0_ad        = cone0_ad+t_out_ad  (k,i)*elv*rcp
        tik_ad          = tik_ad  +t_out_ad  (k,i)
        t_out_ad(k,i)   = zero
        cone0_ad        = cone0_ad+cwm_out_ad(k,i)
        cwmin_ad        = cwmin_ad+cwm_out_ad(k,i)
        cwm_out_ad(k,i) = zero
        cond_ad         = cond_ad +cone0_ad*dt
        e0_ad           = e0_ad   -cone0_ad*dt
        cone0_ad        = zero
        
        if (ccrik > cclimit .and. qsik > epsq) then

!          Redo forward model calculations
           us00   = one-u00ik
           ccrik1 = one-ccrik
           aa = eps*elv*pres*qik
           ab = ccrik*ccrik1*qsik*us00
           ac = ab+half*cwmik
           ad = ab*ccrik1
           ae = cpr*tik*tik
           af = ae*pres
           ag = aa*elv
           ai = cp*aa
           cond1 = (ac-ad)*(af*aq-ai*at+ae*qik*ap)/(ac*(af+ag))
           condi = (qik-u00ik*qsik*one)*rdt
           if (cond1 < condi) then
              cond2 = cond1
           else
              cond2 = condi
           endif
           
!          Adjoint model
           cond3_ad = cond3_ad+cond_ad
           cond_ad  = zero
           if (cond2 > zero) then
              cond2_ad = cond2_ad+cond3_ad
              cond3_ad = zero
           else
              cond3_ad = zero
           endif
           if (cond1 < condi) then
              cond1_ad = cond1_ad+cond2_ad
              cond2_ad = zero
           else
              condi_ad = condi_ad+cond2_ad
              cond2_ad = zero
           endif
           qik_ad   = qik_ad +    condi_ad      *rdt
           qsik_ad  = qsik_ad-one*condi_ad*u00ik*rdt
           condi_ad = zero
           ac_ad = ac_ad+cond1_ad*((af*aq-ai*at+ae*qik*ap)/(ac*(af+ag))- &
                (ac-ad)*(af*aq-ai*at+ae*qik*ap)*(af+ag)/(ac*(af+ag)*ac*(af+ag)))
           ad_ad = ad_ad-cond1_ad*((af*aq-ai*at+ae*qik*ap)/(ac*(af+ag)))
           ae_ad = ae_ad+cond1_ad*((ac-ad)*qik*ap/(ac*(af+ag)))
           af_ad = af_ad+cond1_ad*((ac-ad)*aq/(ac*(af+ag))-(ac-ad)*(af*aq- &
                ai*at+ae*qik*ap)*ac/(ac*(af+ag)*ac*(af+ag)))
           ag_ad = ag_ad-cond1_ad*((ac-ad)*(af*aq-ai*at+ae*qik*ap)*ac/(ac* &
                (af+ag)*ac*(af+ag)))
           ai_ad = ai_ad-cond1_ad*((ac-ad)*at/(ac*(af+ag)))
           ap_ad = ap_ad+cond1_ad*((ac-ad)*ae*qik/(ac*(af+ag)))
           aq_ad = aq_ad+cond1_ad*((ac-ad)*af/(ac*(af+ag)))
           at_ad = at_ad-cond1_ad*((ac-ad)*ai/(ac*(af+ag)))
           qik_ad = qik_ad+cond1_ad*((ac-ad)*ae*ap/(ac*(af+ag)))
           cond1_ad = zero
           aa_ad = aa_ad+ai_ad*cp
           ai_ad = zero
           aa_ad = aa_ad+ag_ad*elv
           ag_ad = zero
           ae_ad = ae_ad+af_ad*pres
           af_ad = zero
           tik_ad = tik_ad+2*ae_ad*cpr*tik
           ae_ad = zero
           ab_ad = ab_ad+ad_ad*ccrik1
           ccrik1_ad = ccrik1_ad+ad_ad*ab
           ad_ad = zero
           ab_ad = ab_ad+ac_ad
           cwmik_ad = cwmik_ad+half*ac_ad
           ac_ad = zero
           ccrik_ad = ccrik_ad+ab_ad*ccrik1*qsik*us00
           ccrik1_ad = ccrik1_ad+ab_ad*ccrik*qsik*us00
           qsik_ad = qsik_ad+ab_ad*ccrik*ccrik1*us00
           ab_ad = zero
           qik_ad = qik_ad+aa_ad*eps*elv*pres
           aa_ad = zero
           ccrik_ad = ccrik_ad-ccrik1_ad
           ccrik1_ad = zero
        else
           cond_ad = zero
        endif
        if (ccrik <= cclimit .and. cwmik > climit) then

!          Redo forward model calculations.  Vapor pressure calculations
!          are saved from the forward model
           tx1_1  = tik
           tx3_1  = qik
           es_1   = es_1k(k,i)
           qs_1   = u00ik*eps*es_1/(prsk+epsm1*es_1)
           tsq_1  = tx1_1*tx1_1
           delq_1 = half*(qs_1-tx3_1)*tsq_1/(tsq_1+el2orc*qs_1)
           tx2_2  = delq_1
           tx1_2  = tx1_1-delq_1*elocp
           tx3_2  = tx3_1+delq_1
           es_2   = es_2k(k,i)
           qs_2   = u00ik*eps*es_2/(prsk+epsm1*es_2)
           tsq_2  = tx1_2*tx1_2
           delq_2 = (qs_2-tx3_2)*tsq_2/(tsq_2+el2orc*qs_2)
           tx2_3  = tx2_2+delq_2
           tx1_3  = tx1_2-delq_2*elocp
           tx3_3  = tx3_2+delq_2
           es_3   = es_3k(k,i)
           qs_3   = u00ik*eps*es_3/(prsk+epsm1*es_3)
           tsq_3  = tx1_3*tx1_3
           delq_3 = (qs_3-tx3_3)*tsq_3/(tsq_3+el2orc*qs_3)
           tx2_4  = tx2_3+delq_3
           e4     = tx2_4*rdt
           if (e4 < zero) then
              e3 = zero
           else
              e3 = e4
           endif
           if (cwmik*rdt < e3) then
              e2 = cwmik*rdt
           else
              e2 = e3
           endif
           
!          Adjoint model
           e1_ad = e1_ad+e0_ad
           e0_ad = zero
           if (e2 < zero) then
              e1_ad = zero
           else
              e2_ad = e2_ad+e1_ad
              e1_ad = zero
           endif
           if (cwmik*rdt < e3) then
              cwmik_ad = cwmik_ad+e2_ad*rdt
              e2_ad = zero
           else
              e3_ad = e3_ad+e2_ad
              e2_ad = zero
           endif
           if (e4 < zero) then
              e3_ad = zero
           else
              e4_ad = e4_ad+e3_ad
              e3_ad = zero
           endif
           tx2_4_ad  = tx2_4_ad+e4_ad*rdt
           e4_ad     = zero
           delq_3_ad = delq_3_ad+tx2_4_ad
           tx2_3_ad  = tx2_3_ad+tx2_4_ad
           tx2_4_ad  = zero
           qs_3_ad   = qs_3_ad+delq_3_ad*(tsq_3/(tsq_3+el2orc*qs_3)-(qs_3- &
                tx3_3)*tsq_3*el2orc/((tsq_3+el2orc*qs_3)*(tsq_3+el2orc*qs_3)))
           tsq_3_ad  = tsq_3_ad+delq_3_ad*((qs_3-tx3_3)/(tsq_3+el2orc*qs_3) &
                -(qs_3-tx3_3)*tsq_3/((tsq_3+el2orc*qs_3)*(tsq_3+el2orc*qs_3)))
           tx3_3_ad  = tx3_3_ad-delq_3_ad*(tsq_3/(tsq_3+el2orc*qs_3))
           delq_3_ad = zero
           tx1_3_ad  = tx1_3_ad+2*tsq_3_ad*tx1_3
           tsq_3_ad  = zero
           es_3_ad   = es_3_ad+qs_3_ad*(u00ik*eps/(prsk+epsm1*es_3)-u00ik* &
                eps*es_3*epsm1/((prsk+epsm1*es_3)*(prsk+epsm1*es_3)))
           qs_3_ad   = zero
           call fpvsx_ad( tx1_3,es_3,tx1_3_ad,es_3_ad,adjoint )
           es_3_ad   = zero
           delq_2_ad = delq_2_ad+tx3_3_ad
           tx3_2_ad  = tx3_2_ad+tx3_3_ad
           tx3_3_ad  = zero
           delq_2_ad = delq_2_ad-tx1_3_ad*elocp
           tx1_2_ad  = tx1_2_ad+tx1_3_ad
           tx1_3_ad  = zero
           delq_2_ad = delq_2_ad+tx2_3_ad
           tx2_2_ad  = tx2_2_ad+tx2_3_ad
           tx2_3_ad  = zero
           qs_2_ad   = qs_2_ad+delq_2_ad*(tsq_2/(tsq_2+el2orc*qs_2)-(qs_2- &
                tx3_2)*tsq_2*el2orc/((tsq_2+el2orc*qs_2)*(tsq_2+el2orc*qs_2)))
           tsq_2_ad  = tsq_2_ad+delq_2_ad*((qs_2-tx3_2)/(tsq_2+el2orc*qs_2) &
                -(qs_2-tx3_2)*tsq_2/((tsq_2+el2orc*qs_2)*(tsq_2+el2orc*qs_2)))
           tx3_2_ad  = tx3_2_ad-delq_2_ad*(tsq_2/(tsq_2+el2orc*qs_2))
           delq_2_ad = zero
           tx1_2_ad  = tx1_2_ad+2*tsq_2_ad*tx1_2
           tsq_2_ad  = zero
           es_2_ad   = es_2_ad+qs_2_ad*(u00ik*eps/(prsk+epsm1*es_2)-u00ik* &
                eps*es_2*epsm1/((prsk+epsm1*es_2)*(prsk+epsm1*es_2)))
           qs_2_ad   = zero
           call fpvsx_ad( tx1_2,es_2,tx1_2_ad,es_2_ad,adjoint )
           es_2_ad   = zero
           delq_1_ad = delq_1_ad+tx3_2_ad
           tx3_1_ad  = tx3_1_ad+tx3_2_ad
           tx3_2_ad  = zero
           delq_1_ad = delq_1_ad-tx1_2_ad*elocp
           tx1_1_ad  = tx1_1_ad+tx1_2_ad
           tx1_2_ad  = zero
           delq_1_ad = delq_1_ad+tx2_2_ad
           tx2_2_ad  = zero
           qs_1_ad   = qs_1_ad+delq_1_ad*(half*tsq_1/(tsq_1+el2orc*qs_1)-half* &
                (qs_1-tx3_1)*tsq_1*el2orc/((tsq_1+el2orc*qs_1)* &
                (tsq_1+el2orc*qs_1) ))
           tsq_1_ad  = tsq_1_ad+delq_1_ad*(half*(qs_1-tx3_1)/(tsq_1+el2orc* &
                qs_1)-half*(qs_1-tx3_1)*tsq_1/((tsq_1+el2orc*qs_1)* &
                (tsq_1+el2orc*qs_1)))
           tx3_1_ad  = tx3_1_ad+delq_1_ad*((-half)*tsq_1/(tsq_1+el2orc*qs_1))
           delq_1_ad = zero
           tx1_1_ad  = tx1_1_ad+2*tsq_1_ad*tx1_1
           tsq_1_ad  = zero
           es_1_ad   = es_1_ad+qs_1_ad*(u00ik*eps/(prsk+epsm1*es_1)-u00ik* &
                eps*es_1*epsm1/((prsk+epsm1*es_1)*(prsk+epsm1*es_1)))
           qs_1_ad   = zero
           call fpvsx_ad( tx1_1,es_1,tx1_1_ad,es_1_ad,adjoint )
           es_1_ad   = zero
           qik_ad    = qik_ad+tx3_1_ad
           tx3_1_ad  = zero
           tik_ad    = tik_ad+tx1_1_ad
           tx1_1_ad  = zero
        else
           e0_ad = zero
        endif
        if (rqik < u00ik) then
           ccrik_ad = zero
        else if (rqik >= one) then
           ccrik_ad = zero
        else
           rqikk_ad = rqikk_ad+ccrik_ad*(one/(two*sqrt((one-rqikk)/ &
                (one-u00ik)))/(one-u00ik))
           ccrik_ad = zero
           if (rqik < one) then
              rqik_ad  = rqik_ad+rqikk_ad
              rqikk_ad = zero
           else
              rqikk_ad = zero
           endif
        endif
        if (qsik <= 1.e-10_r_kind) then
           rqik_ad = zero
        else
           qik_ad  = qik_ad+rqik_ad/qsik
           qsik_ad = qsik_ad-rqik_ad*(qik/(qsik*qsik))
           rqik_ad = zero
        endif
        advp_ad(k,i) = advp_ad(k,i)+ap_ad
        ap_ad        = zero
        advq_ad(k,i) = advq_ad(k,i)+aq_ad
        aq_ad        = zero
        advt_ad(k,i) = advt_ad(k,i)+at_ad
        at_ad        = zero
        tik_ad       = tik_ad+tmt0_ad
        tmt0_ad      = zero
        if (qsk > epsq) then
           qsk_ad  = qsk_ad+qsik_ad
           qsik_ad = zero
        else
           qsik_ad = zero
        endif
        esk_ad = esk_ad+qsk_ad*(eps/(prsk+epsm1*esk)-eps*esk*epsm1/ &
             ((prsk+epsm1*esk)*(prsk+epsm1*esk)))
        qsk_ad = zero
        call fpvsx_ad( tik,esk,tik_ad,esk_ad,adjoint )
        esk_ad = zero
        if (cwmin > climit) then
           cwmin_ad = cwmin_ad+cwmik_ad
           cwmik_ad = zero
        else
           cwmik_ad = zero
        endif
        if (qin > epsq) then
           qin_ad = qin_ad+qik_ad
           qik_ad = zero
        else
           qik_ad = zero
        endif
        q_in_ad(k,i)   = q_in_ad(k,i)+qin_ad
        qin_ad         = zero
        t_in_ad(k,i)   = t_in_ad(k,i)+tik_ad
        tik_ad         = zero
        cwm_in_ad(k,i) = cwm_in_ad(k,i)+cwmin_ad
        cwmin_ad       = zero
     end do
  end do
  
  return
end subroutine gscond_ad_im_ix_

end module gscond_ad_mod
