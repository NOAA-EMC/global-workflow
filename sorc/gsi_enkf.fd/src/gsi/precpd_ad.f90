subroutine precpd_ad( km, dt, del, sl, ps, rhc, q_in, &
     cwm_in, t_in, q_out, cwm_out, t_out, rn_out, q_in_ad, cwm_in_ad, &
     t_in_ad, q_out_ad, cwm_out_ad, t_out_ad, rn_out_ad, &
     adjoint)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    precpd_ad    compute precipitation processes from suspended cloud water/ice
!     prgmmr:    treadon     org: np23                date: 2003-12-18
!
! abstract:  This routine parameterizes the effect of precipitation processes from
!            suspended cloud water and ice on temperature, moisture, and cloud 
!            condensate.  For more details regarding the algorithm, please refer
!            to
!               Zhao and Carr (1997), Monthly Weather Review (August)
!               Sundqvist et al., (1989) Monthly Weather review. (August)
!
!
! program history log:
!   1995-01-01  zhao - original routine
!   1998-10-10  moorthi,pan - modified and rewritten for GFS application
!   2003-12-18  treadon - add adjoint code
!   2004-06-14  treadon - reformat documenation
!   2004-08-04  treadon - add only on use declarations; add intent in/out
!   2006-04-12  treadon - change del and sl from 1d to 2d arrays
!   2008-04-24  safford - rm unused vars
!
!   input argument list:
!       km        - number of vertical levels
!       dt        - time step in seconds
!       del       - pressure layer thickness (bottom to top)
!       sl        - sigma level
!       ps        - surface pressure (centibars)
!       rhc       - critical relative humidity threshold
!       q_in      - specific humidity
!       cwm_in    - condensate mixing ratio
!       t_in      - temperature
!       q_out_ad   - specific humidity perturbation
!       cwm_out_ad - cloud condensate mixing perturbation
!       t_out_ad   - temperature perturbation
!       rn_out_ad  - precipitation perturbation
!       adjoint   - logical flag (.false.=forward model only, .true.=forward and ajoint)
!
!   output argument list:
!       q_out     - q following grid scale precipitation
!       cwm_out   - cloud condensate mixing ratio following precipitation
!       t_out     - temperature following precipitation
!       rn_out    - precipitation over one time step, dt
!       q_in_ad    - change in rain rate with respect to moisture
!       cwm_in_ad  - change in rain rate with respect to cloud condensate mixing ratio
!       t_in_ad    - change in rain rate with respect to temperature
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!
!***************************************************************
!==============================================
! all entries are defined explicitly
!==============================================
  use kinds, only: r_kind,i_kind
  use constants, only: cmr,cws,half,epsm1,h300,rrow,two,&
       hfus,hvap,hsub,rcp,h1000,ke2,zero,one,ttp,eps,epsq,&
       grav,climit
  implicit none
!==============================================
! define arguments
!==============================================
  logical        ,intent(in   ):: adjoint
  integer(i_kind),intent(in   ):: km
  real(r_kind)   ,intent(inout):: cwm_in_ad(km)
  real(r_kind)   ,intent(inout):: cwm_out_ad(km)
  real(r_kind)   ,intent(inout):: q_in_ad(km)
  real(r_kind)   ,intent(inout):: q_out_ad(km)
  real(r_kind)   ,intent(inout):: rn_out_ad
  real(r_kind)   ,intent(inout):: t_in_ad(km)
  real(r_kind)   ,intent(inout):: t_out_ad(km)
  real(r_kind)   ,intent(in   ):: cwm_in(km)
  real(r_kind)   ,intent(  out):: cwm_out(km)
  real(r_kind)   ,intent(in   ):: del(km)
  real(r_kind)   ,intent(in   ):: dt
  real(r_kind)   ,intent(in   ):: ps
  real(r_kind)   ,intent(in   ):: q_in(km)
  real(r_kind)   ,intent(  out):: q_out(km)
  real(r_kind)   ,intent(in   ):: rhc(km)
  real(r_kind)   ,intent(  out):: rn_out
  real(r_kind)   ,intent(in   ):: sl(km)
  real(r_kind)   ,intent(in   ):: t_in(km)
  real(r_kind)   ,intent(  out):: t_out(km)
!==============================================
! define local variables
!==============================================
  real(r_kind) aa2
  real(r_kind) amaxcmr_ad
  real(r_kind) amaxcms_ad
  real(r_kind) amaxps_ad
  real(r_kind) amaxrq_ad
  real(r_kind) ccr_ad
  real(r_kind) cwmin_ad
  real(r_kind) cwmk_ad
  real(r_kind) cwmks_ad
  real(r_kind) erk_ad
  real(r_kind) err_ad
  real(r_kind) ers_ad
  real(r_kind) es_ad
  real(r_kind) expf_ad
  real(r_kind) fi_ad
  real(r_kind) ppr0_ad
  real(r_kind) ppr1_ad
  real(r_kind) ppr2_ad
  real(r_kind) pps0_ad
  real(r_kind) pps1_ad
  real(r_kind) pps2_ad
  real(r_kind) praut_ad
  real(r_kind) praut0_ad
  real(r_kind) precrk0_ad
  real(r_kind) precrk1_ad
  real(r_kind) precrl1k_ad
  real(r_kind) precrl_0_ad
  real(r_kind) precrl_1_ad
  real(r_kind) precrl_2_ad
  real(r_kind) precrl_3_ad
  real(r_kind) precsk0_ad
  real(r_kind) precsk1_ad
  real(r_kind) precsl1k_ad
  real(r_kind) precsl_0_ad
  real(r_kind) precsl_1_ad
  real(r_kind) precsl_2_ad
  real(r_kind) precsl_3_ad
  real(r_kind) psaci_ad
  real(r_kind) psaut_ad
  real(r_kind) psm1_ad
  real(r_kind) psm2_ad
  real(r_kind) psm3_ad
  real(r_kind) q2_ad
  real(r_kind) qin_ad
  real(r_kind) qk_ad
  real(r_kind) qs_ad
  real(r_kind) qs0_ad
  real(r_kind) rprs_ad
  real(r_kind) rq_ad
  real(r_kind) rqkll_ad
  real(r_kind) t2_ad
  real(r_kind) tem1_ad
  real(r_kind) tem2_ad
  real(r_kind) tem3_ad
  real(r_kind) tem4_ad
  real(r_kind) tem5_ad
  real(r_kind) term_ad
  real(r_kind) term1_ad
  real(r_kind) term2r_ad
  real(r_kind) term2s_ad
  real(r_kind) term3r_ad
  real(r_kind) term3s_ad
  real(r_kind) term4_ad
  real(r_kind) term5_ad
  real(r_kind) term6_ad
  real(r_kind) tin_ad
  real(r_kind) tmt0_ad
  real(r_kind) tmt0k_ad
  real(r_kind) ww0_ad
  real(r_kind) ww1_ad
  real(r_kind) ww2_ad
  real(r_kind) wwn_ad
  real(r_kind) amaxcmr
  real(r_kind) amaxcms
  real(r_kind) amaxps
  real(r_kind) amaxrq
  real(r_kind) c00
  real(r_kind) ccr
  logical comput,comput0
  real(r_kind) conde
  real(r_kind) condt
  real(r_kind) const
  real(r_kind) cr
  real(r_kind) crs1
  real(r_kind) crs2
  real(r_kind) csm1
  real(r_kind) cwmin
  real(r_kind) cwmk
  real(r_kind) cwmks
  real(r_kind) dtcp
  real(r_kind) dum1
  real(r_kind) dum2
  real(r_kind) erk
  real(r_kind) err
  real(r_kind) ers
  real(r_kind) es
  real(r_kind) expf
  real(r_kind) fi
  integer(i_kind) iwl
  integer(i_kind) iwl1,iwl1k(km+1)
  integer(i_kind) k
  real(r_kind) ppr0
  real(r_kind) ppr1
  real(r_kind) ppr2
  real(r_kind) pps0
  real(r_kind) pps1
  real(r_kind) pps2
  real(r_kind) praut
  real(r_kind) praut0
  real(r_kind) precrk0
  real(r_kind) precrk1
  real(r_kind) precrl1k,precrl1ki(km+1)
  real(r_kind) precrl_0
  real(r_kind) precrl_1
  real(r_kind) precrl_2
  real(r_kind) precrl_3
  real(r_kind) precsk0
  real(r_kind) precsk1
  real(r_kind) precsl1k,precsl1ki(km+1)
  real(r_kind) precsl_0
  real(r_kind) precsl_1
  real(r_kind) precsl_2
  real(r_kind) precsl_3
  real(r_kind) pres
  real(r_kind) psaci
  real(r_kind) psaut
  real(r_kind) psm1
  real(r_kind) psm2
  real(r_kind) psm3
  real(r_kind) q2
  real(r_kind) qin
  real(r_kind) qk
  real(r_kind) qs
  real(r_kind) qs0
  real(r_kind) rconde
  real(r_kind) rdt
  real(r_kind) rhci(km)
  real(r_kind) rke2
  real(r_kind) rprs
  real(r_kind) rq
  real(r_kind) rqkll
  real(r_kind) t2
  real(r_kind) tem
  real(r_kind) tem1
  real(r_kind) tem2
  real(r_kind) tem3
  real(r_kind) tem4
  real(r_kind) tem5
  real(r_kind) term
  real(r_kind) term1
  real(r_kind) term2r
  real(r_kind) term2s
  real(r_kind) term3r
  real(r_kind) term3s
  real(r_kind) term4
  real(r_kind) term5
  real(r_kind) term6
  real(r_kind) tin
  real(r_kind) tmt0
  real(r_kind) tmt0k
  real(r_kind) wmin
  real(r_kind) ww0
  real(r_kind) ww1
  real(r_kind) ww2
  real(r_kind) wwn
  real(r_kind) zaodt
  real(r_kind) onem10,onem20

!----------------------------------------------
! RESET LOCAL ADJOINT VARIABLES
!----------------------------------------------
  onem10 = 1.0e-10_r_kind
  onem20 = 1.0e-20_r_kind
  amaxcmr_ad = zero
  amaxcms_ad = zero
  amaxps_ad = zero
  amaxrq_ad = zero
  ccr_ad = zero
  cwmin_ad = zero
  cwmk_ad = zero
  cwmks_ad = zero
  erk_ad = zero
  err_ad = zero
  ers_ad = zero
  es_ad = zero
  expf_ad = zero
  fi_ad = zero
  ppr0_ad = zero
  ppr1_ad = zero
  ppr2_ad = zero
  pps0_ad = zero
  pps1_ad = zero
  pps2_ad = zero
  praut_ad = zero
  praut0_ad = zero
  precrk0_ad = zero
  precrk1_ad = zero
  precrl1k_ad = zero
  precrl_0_ad = zero
  precrl_1_ad = zero
  precrl_2_ad = zero
  precrl_3_ad = zero
  precsk0_ad = zero
  precsk1_ad = zero
  precsl1k_ad = zero
  precsl_0_ad = zero
  precsl_1_ad = zero
  precsl_2_ad = zero
  precsl_3_ad = zero
  psaci_ad = zero
  psaut_ad = zero
  psm1_ad = zero
  psm2_ad = zero
  psm3_ad = zero
  q2_ad = zero
  qin_ad = zero
  qk_ad = zero
  qs_ad = zero
  qs0_ad = zero
  rprs_ad = zero
  rq_ad = zero
  rqkll_ad = zero
  t2_ad = zero
  tem1_ad = zero
  tem2_ad = zero
  tem3_ad = zero
  tem4_ad = zero
  tem5_ad = zero
  term_ad = zero
  term1_ad = zero
  term2r_ad = zero
  term2s_ad = zero
  term3r_ad = zero
  term3s_ad = zero
  term4_ad = zero
  term5_ad = zero
  term6_ad = zero
  tin_ad = zero
  tmt0_ad = zero
  tmt0k_ad = zero
  ww0_ad = zero
  ww1_ad = zero
  ww2_ad = zero
  wwn_ad = zero

!----------------------------------------------
! ROUTINE BODY
!----------------------------------------------
!----------------------------------------------
! FUNCTION AND TAPE COMPUTATIONS
!----------------------------------------------
  rdt = one/dt
  zaodt = 800._r_kind*rdt
  csm1 = 5.e-8_r_kind*zaodt
  crs1 = 5.e-6_r_kind*zaodt
  crs2 = 6.666e-10_r_kind*zaodt
  cr = 0.0005_r_kind*zaodt
  aa2 = 0.00125_r_kind*zaodt
  rke2 = ke2*sqrt(rdt)
  dtcp = dt*rcp
  c00 = 0.0001_r_kind*dt
  do k = 1, km
     rhci(k) = one/rhc(k)
  end do

!new
  comput0 =  .false.
  do k = 1, km
     tem = 0.00001_r_kind*sl(k)*ps*0.01_r_kind
     if (cwm_in(k) > tem) then
        comput0 =  .true.
     endif
  end do
!new

  iwl1      = 0
  precrl1k = zero
  precsl1k = zero
  rn_out   = zero
  const    = ps*(h1000*dt/grav)
!new
  if (comput0) then
!n
     do k = km, 1, -1
        wmin     = 0.00001_r_kind*sl(k)*ps*0.01_r_kind
        precrl_0 = precrl1k
        precsl_0 = precsl1k
        
!       Save forward model values for use in adjoint model
        precrl1ki(k) = precrl_0
        precsl1ki(k) = precsl_0
        iwl1k(k)     = iwl1

        iwl   = 0
        tin   = t_in(k)
        qin   = q_in(k)
        cwmin = cwm_in(k)
        pres  = ps*sl(k)
        if (precrl_0 > zero) then
           precrk0 = precrl_0
        else
           precrk0 = zero
        endif
        if (precsl_0 > zero) then
           precsk0 = precsl_0
        else
           precsk0 = zero
        endif
        if (cwmin > climit) then
           wwn = cwmin
        else
           wwn = climit
        endif
        if (wwn > climit .or. precrk0+precsk0 > zero) then
           comput =  .true. 
        else
           comput =  .false. 
        endif
        if (comput) then
           conde = const*del(k)
           condt = conde*rdt
           rconde = one/conde
           if (qin > epsq) then
              qk = qin
           else
              qk = epsq
           endif
           tmt0 = tin-ttp
           call fpvsx_ad(tin,es,dum1,dum2,.false.)
           qs0  = eps*es/(pres+epsm1*es)
           if (qs0 > epsq) then
              qs = qs0
           else
              qs = epsq
           endif
           if (tmt0 < (-15._r_kind)) then
              fi = qk-rhc(k)*qs
              if (fi > zero .or. wwn > climit) then
                 iwl = 1
              else
                 iwl = 0
              endif
           else if (tmt0 >= zero) then
              iwl = 0
           else
              iwl = 0
              if (iwl1 == 1 .and. wwn > climit) then
                 iwl = 1
              endif
           endif
           if (qs <= onem10) then
              rq = zero
           else
              rq = qk/qs
           endif
           if (rq < rhc(k)) then
              ccr = zero
           else if (rq >= one) then
              ccr = one
           else
              if (rq < one) then
                 rqkll = rq
              else
                 rqkll = one
              endif
              ccr = one-sqrt((one-rqkll)/(one-rhc(k)))
           endif
           if (ccr > zero) then
              ww0 = cwmin
              if (ww0 > zero) then
                 cwmk = ww0
              else
                 cwmk = zero
              endif
              if (iwl == 1) then
                 term1 = cwmk-wmin
                 if (term1 > zero) then
                    amaxcms = term1
                 else
                    amaxcms = zero
                 endif
                 expf   = dt*exp(0.025_r_kind*tmt0)
                 term2s = 0.0004_r_kind*expf*amaxcms
                 if (term2s < cwmk) then
                    psaut = term2s
                 else
                    psaut = cwmk
                 endif
                 ww1 = ww0-psaut
                 if (ww1 > zero) then
                    cwmks = ww1
                 else
                    cwmks = zero
                 endif
                 term3s = aa2*expf*precsl_0*cwmks
                 if (term3s < cwmks) then
                    psaci = term3s
                 else
                    psaci = cwmks
                 endif
                 ww2      = ww1-psaci
                 precsl_1 = precsl_0+(ww0-ww2)*condt
                 precrl_1 = precrl_0
              else
                 amaxcmr = cwmk
                 tem1    = precsl_0+precrl_0
                 term2r  = 268._r_kind-tin
                 if (term2r > zero) then
                    term3r = term2r
                 else
                    term3r = zero
                 endif
                 if (term3r < 20._r_kind) then
                    term4 = term3r
                 else
                    term4 = 20._r_kind
                 endif
                 tem2 = term4
                 tem3 = (one+h300*sqrt(tem1*rdt))*(one+half*sqrt(tem2))
                 if (ccr > 0.01_r_kind) then
                    term5 = ccr
                 else
                    term5 = 0.01_r_kind
                 endif
                 tem4  = amaxcmr*cmr*tem3/term5
                 term6 = tem4*tem4
                 if (term6 < 50._r_kind) then
                    tem5 = term6
                 else
                    tem5 = 50._r_kind
                 endif
                 praut = c00*tem3*amaxcmr*(one-exp(-tem5))
                 if (praut < cwmk) then
                    praut0 = praut
                 else
                    praut0 = cwmk
                 endif
                 ww2      = ww0-praut0
                 precrl_1 = precrl_0+(ww0-ww2)*condt
                 precsl_1 = precsl_0
              endif
           else
              ww2      = cwmin
              precrl_1 = precrl_0
              precsl_1 = precsl_0
           endif
           if (tmt0 > (-30._r_kind)) then
              tmt0k = tmt0
           else
              tmt0k = -30._r_kind
           endif
           if (precrl_1 > zero) then
              precrk1 = precrl_1
           else
              precrk1 = zero
           endif
           if (precsl_1 > zero) then
              precsk1 = precsl_1
           else
              precsk1 = zero
           endif
           term = rhc(k)-rq
           if (term > zero) then
              amaxrq = term*conde
           else
              amaxrq = zero
           endif
           ppr0 = rke2*amaxrq*sqrt(precrk1)
           if (tmt0 >= zero) then
              pps0 = zero
           else
              pps0 = (crs1+crs2*tmt0k)*amaxrq*precsk1*rhci(k)
           endif
           erk = precrk1+precsk1
           if (rq >= onem10) then
              erk = amaxrq*qk*rdt/rq
           endif
           if (ppr0+pps0 > abs(erk)) then
              rprs = erk/(precrk1+precsk1)
              ppr1 = precrk1*rprs
              pps1 = precsk1*rprs
           else
              ppr1 = ppr0
              pps1 = pps0
           endif
           if (ppr1 < precrk1) then
              ppr2 = ppr1
           else
              ppr2 = precrk1
           endif
           if (pps1 < precsk1) then
              pps2 = pps1
           else
              pps2 = precsk1
           endif
           err      = ppr2*rconde
           ers      = pps2*rconde
           precrl_2 = precrl_1-ppr2
           precsl_2 = precsl_1-pps2
           if (tmt0 > zero) then
              if (precsl_2 > zero) then
                 amaxps = precsl_2
              else
                 amaxps = zero
              endif
              psm1 = csm1*tmt0*tmt0*amaxps
              if (ww2 > zero) then
                 psm2 = cws*cr*ww2*amaxps
              else
                 psm2 = zero
              endif
              ppr0 = (psm1+psm2)*conde
              if (ppr0 > amaxps) then
                 ppr1 = amaxps
                 psm3 = amaxps*rconde
              else
                 ppr1 = ppr0
                 psm3 = psm1
              endif
              precrl_3 = precrl_2+ppr1
              precsl_3 = precsl_2-ppr1
           else
              psm3     = zero
              precrl_3 = precrl_2
              precsl_3 = precsl_2
           endif
           t2       = tin-dtcp*(hvap*err+hsub*ers+hfus*psm3)
           q2       = qin+dt*(err+ers)
        else
           t2       = tin
           q2       = qin
           precrl_3 = precrl_0
           precsl_3 = precsl_0
           ww2      = cwmin
        endif
        iwl1 = iwl
        if (precrl_3 > zero) then
           precrl1k = precrl_3
        else
           precrl1k = zero
        endif
        if (precsl_3 > zero) then
           precsl1k = precsl_3
        else
           precsl1k = zero
        endif
        if (ww2 < zero) then
           q_out(k)   = q2+ww2
           t_out(k)   = t2-hvap*rcp*ww2
           cwm_out(k) = zero
        else
           q_out(k)   = q2
           t_out(k)   = t2
           cwm_out(k) = ww2
        endif
     end do
     rn_out = (precrl1k+precsl1k)*rrow

!new - no compute case
  else
     do k = km, 1, -1
        if (cwm_in(k) < zero) then
           q_out(k)   = q_in(k)+cwm_in(k)
           t_out(k)   = t_in(k)-hvap*rcp*cwm_in(k)
           cwm_out(k) = zero
        else
           q_out(k)   = q_in(k)
           t_out(k)   = t_in(k)
           cwm_out(k) = cwm_in(k)
        endif
     end do
     rn_out = zero
  endif
     
  
  if (.not.adjoint) return

!----------------------------------------------
! ADJOINT COMPUTATIONS
!----------------------------------------------

  if (comput0) then
     
     const        = ps*(h1000*dt/grav)
     precrl1k_ad  = precrl1k_ad+rn_out_ad*rrow
     precsl1k_ad  = precsl1k_ad+rn_out_ad*rrow
     rn_out_ad = zero
     do k = 1, km

!       Load saved forward model values
        precrl1k = precrl1ki(k)
        precsl1k = precsl1ki(k)
        iwl1     = iwl1k(k)
        
!       Recompute forward model values
        wmin     = 0.00001_r_kind*sl(k)*ps*0.01_r_kind
        precrl_0 = precrl1k
        precsl_0 = precsl1k
        tin      = t_in(k)
        qin      = q_in(k)
        cwmin    = cwm_in(k)
        pres     = ps*sl(k)
        if (precrl_0 > zero) then
           precrk0 = precrl_0
        else
           precrk0 = zero
        endif
        if (precsl_0 > zero) then
           precsk0 = precsl_0
        else
           precsk0 = zero
        endif
        if (cwmin > climit) then
           wwn = cwmin
        else
           wwn = climit
        endif
        if (wwn > climit .or. precrk0+precsk0 > zero) then
           comput =  .true. 
        else
           comput =  .false. 
        endif
        if (comput) then
           conde = const*del(k)
           condt = conde*rdt
           if (qin > epsq) then
              qk = qin
           else
              qk = epsq
           endif
           tmt0 = tin-ttp
           call fpvsx_ad(tin,es,dum1,dum2,.false.)
           qs0  = eps*es/(pres+epsm1*es)
           if (qs0 > epsq) then
              qs = qs0
           else
              qs = epsq
           endif
           if (tmt0 < (-15._r_kind)) then
              fi = qk-rhc(k)*qs
              if (fi > zero .or. wwn > climit) then
                 iwl = 1
              else
                 iwl = 0
              endif
           else if (tmt0 >= zero) then
              iwl = 0
           else
              iwl = 0
              if (iwl1 == 1 .and. wwn > climit) then
                 iwl = 1
              endif
           endif
           if (qs <= onem10) then
              rq = zero
           else
              rq = qk/qs
           endif
           if (rq < rhc(k)) then
              ccr = zero
           else if (rq >= one) then
              ccr = one
           else
              if (rq < one) then
                 rqkll = rq
              else
                 rqkll = one
              endif
              ccr = one-sqrt((one-rqkll)/(one-rhc(k)))
           endif
           if (ccr > zero) then
              ww0 = cwmin
              if (ww0 > zero) then
                 cwmk = ww0
              else
                 cwmk = zero
              endif
              if (iwl == 1) then
                 term1 = cwmk-wmin
                 if (term1 > zero) then
                    amaxcms = term1
                 else
                    amaxcms = zero
                 endif
                 expf   = dt*exp(0.025_r_kind*tmt0)
                 term2s = 0.0004_r_kind*expf*amaxcms
                 if (term2s < cwmk) then
                    psaut = term2s
                 else
                    psaut = cwmk
                 endif
                 ww1 = ww0-psaut
                 if (ww1 > zero) then
                    cwmks = ww1
                 else
                    cwmks = zero
                 endif
                 term3s = aa2*expf*precsl_0*cwmks
                 if (term3s < cwmks) then
                    psaci = term3s
                 else
                    psaci = cwmks
                 endif
                 ww2      = ww1-psaci
                 precsl_1 = precsl_0+(ww0-ww2)*condt
                 precrl_1 = precrl_0
              else
                 amaxcmr  = cwmk
                 tem1     = precsl_0+precrl_0
                 term2r   = 268._r_kind-tin
                 if (term2r > zero) then
                    term3r = term2r
                 else
                    term3r = zero
                 endif
                 if (term3r < 20._r_kind) then
                    term4 = term3r
                 else
                    term4 = 20._r_kind
                 endif
                 tem2 = term4
                 tem3 = (one+h300*sqrt(tem1*rdt))*(one+half*sqrt(tem2))
                 if (ccr > 0.01_r_kind) then
                    term5 = ccr
                 else
                    term5 = 0.01_r_kind
                 endif
                 tem4  = amaxcmr*cmr*tem3/term5
                 term6 = tem4*tem4
                 if (term6 < 50._r_kind) then
                    tem5 = term6
                 else
                    tem5 = 50._r_kind
                 endif
                 praut = c00*tem3*amaxcmr*(one-exp(-tem5))
                 if (praut < cwmk) then
                    praut0 = praut
                 else
                    praut0 = cwmk
                 endif
                 ww2      = ww0-praut0
                 precrl_1 = precrl_0+(ww0-ww2)*condt
                 precsl_1 = precsl_0
              endif
           else
              ww2      = cwmin
              precrl_1 = precrl_0
              precsl_1 = precsl_0
           endif
           if (tmt0 > (-30._r_kind)) then
              tmt0k = tmt0
           else
              tmt0k = -30._r_kind
           endif
           if (precrl_1 > zero) then
              precrk1 = precrl_1
           else
              precrk1 = zero
           endif
           if (precsl_1 > zero) then
              precsk1 = precsl_1
           else
              precsk1 = zero
           endif
           term = rhc(k)-rq
           if (term > zero) then
              amaxrq = term*conde
           else
              amaxrq = zero
           endif
!         ppr0 = rke2*amaxrq*sqrt(precrk1)
           if (precrk1>zero) then
              ppr0 = rke2*amaxrq*sqrt(precrk1)
           else
              ppr0 = zero
           endif
           
           if (tmt0 >= zero) then
              pps0 = zero
           else
              pps0 = (crs1+crs2*tmt0k)*amaxrq*precsk1*rhci(k)
           endif
           erk = precrk1+precsk1
           if (rq >= onem10) then
              erk = amaxrq*qk*rdt/rq
           endif
           if (ppr0+pps0 > abs(erk)) then
              rprs = erk/(precrk1+precsk1)
              ppr1 = precrk1*rprs
              pps1 = precsk1*rprs
           else
              ppr1 = ppr0
              pps1 = pps0
           endif
           if (ppr1 < precrk1) then
              ppr2 = ppr1
           else
              ppr2 = precrk1
           endif
           if (pps1 < precsk1) then
              pps2 = pps1
           else
              pps2 = precsk1
           endif
           precrl_2 = precrl_1-ppr2
           precsl_2 = precsl_1-pps2
           if (tmt0 > zero) then
              if (precsl_2 > zero) then
                 amaxps = precsl_2
              else
                 amaxps = zero
              endif
              psm1 = csm1*tmt0*tmt0*amaxps
              if (ww2 > zero) then
                 psm2 = cws*cr*ww2*amaxps
              else
                 psm2 = zero
              endif
              ppr0 = (psm1+psm2)*conde
              if (ppr0 > amaxps) then
                 ppr1 = amaxps
              else
                 ppr1 = ppr0
              endif
              precrl_3 = precrl_2+ppr1
              precsl_3 = precsl_2-ppr1
           else
              precrl_3 = precrl_2
              precsl_3 = precsl_2
           endif
        else
           precrl_3 = precrl_0
           precsl_3 = precsl_0
           ww2      = cwmin
        endif
     
!       Adjoint model
        if (ww2 < zero) then
           cwm_out_ad(k) = zero
           t2_ad         = t2_ad+t_out_ad(k)
           ww2_ad        = ww2_ad-t_out_ad(k)*hvap*rcp
           t_out_ad(k)   = zero
           q2_ad         = q2_ad+q_out_ad(k)
           ww2_ad        = ww2_ad+q_out_ad(k)
           q_out_ad(k)   = zero
        else
           ww2_ad        = ww2_ad+cwm_out_ad(k)
           cwm_out_ad(k) = zero
           t2_ad         = t2_ad+t_out_ad(k)
           t_out_ad(k)   = zero
           q2_ad         = q2_ad+q_out_ad(k)
           q_out_ad(k)   = zero
        endif
        if (precsl_3 > zero) then
           precsl_3_ad = precsl_3_ad+precsl1k_ad
           precsl1k_ad = zero
        else
           precsl1k_ad = zero
        endif
        if (precrl_3 > zero) then
           precrl_3_ad = precrl_3_ad+precrl1k_ad
           precrl1k_ad = zero
        else
           precrl1k_ad = zero
        endif
        if (comput) then
           rconde  = one/conde
           err_ad  = err_ad+q2_ad*dt
           ers_ad  = ers_ad+q2_ad*dt
           qin_ad  = qin_ad+q2_ad
           q2_ad   = zero
           err_ad  = err_ad-t2_ad*dtcp*hvap
           ers_ad  = ers_ad-t2_ad*dtcp*hsub
           psm3_ad = psm3_ad-t2_ad*dtcp*hfus
           tin_ad  = tin_ad+t2_ad
           t2_ad   = zero
           if (tmt0 > zero) then
              ppr1_ad     = ppr1_ad-precsl_3_ad
              precsl_2_ad = precsl_2_ad+precsl_3_ad
              precsl_3_ad = zero
              ppr1_ad     = ppr1_ad+precrl_3_ad
              precrl_2_ad = precrl_2_ad+precrl_3_ad
              precrl_3_ad = zero
              if (ppr0 > amaxps) then
                 amaxps_ad = amaxps_ad+psm3_ad*rconde
                 psm3_ad   = zero
                 amaxps_ad = amaxps_ad+ppr1_ad
                 ppr1_ad   = zero
              else
                 psm1_ad   = psm1_ad+psm3_ad
                 psm3_ad   = zero
                 ppr0_ad   = ppr0_ad+ppr1_ad
                 ppr1_ad   = zero
              endif
              psm1_ad = psm1_ad+ppr0_ad*conde
              psm2_ad = psm2_ad+ppr0_ad*conde
              ppr0_ad = zero
              if (ww2 > zero) then
                 amaxps_ad = amaxps_ad+psm2_ad*cws*cr*ww2
                 ww2_ad    = ww2_ad+psm2_ad*cws*cr*amaxps
                 psm2_ad   = zero
              else
                 psm2_ad   = zero
              endif
              amaxps_ad = amaxps_ad+psm1_ad*csm1*tmt0*tmt0
              tmt0_ad   = tmt0_ad+2*psm1_ad*csm1*tmt0*amaxps
              psm1_ad   = zero
              if (precsl_2 > zero) then
                 precsl_2_ad = precsl_2_ad+amaxps_ad
                 amaxps_ad   = zero
              else
                 amaxps_ad   = zero
              endif
           else
              precsl_2_ad = precsl_2_ad+precsl_3_ad
              precsl_3_ad = zero
              precrl_2_ad = precrl_2_ad+precrl_3_ad
              precrl_3_ad = zero
              psm3_ad     = zero
           endif
           pps2_ad     = pps2_ad-precsl_2_ad
           precsl_1_ad = precsl_1_ad+precsl_2_ad
           precsl_2_ad = zero
           ppr2_ad     = ppr2_ad-precrl_2_ad
           precrl_1_ad = precrl_1_ad+precrl_2_ad
           precrl_2_ad = zero
           pps2_ad     = pps2_ad+ers_ad*rconde
           ers_ad      = zero
           ppr2_ad     = ppr2_ad+err_ad*rconde
           err_ad      = zero
           if (pps1 < precsk1) then
              pps1_ad    = pps1_ad+pps2_ad
              pps2_ad    = zero
           else
              precsk1_ad = precsk1_ad+pps2_ad
              pps2_ad    = zero
           endif
           ppr0 = rke2*amaxrq*sqrt(precrk1)
           if (ppr0+pps0 > abs(erk)) then
              ppr1 = precrk1*rprs
           else
              ppr1 = ppr0
           endif
           if (ppr1 < precrk1) then
              ppr1_ad    = ppr1_ad+ppr2_ad
              ppr2_ad    = zero
           else
              precrk1_ad = precrk1_ad+ppr2_ad
              ppr2_ad    = zero
           endif
           ppr0 = rke2*amaxrq*sqrt(precrk1)
           if (ppr0+pps0 > abs(erk)) then
              precsk1_ad = precsk1_ad+pps1_ad*rprs
              rprs_ad    = rprs_ad+pps1_ad*precsk1
              pps1_ad    = zero
              precrk1_ad = precrk1_ad+ppr1_ad*rprs
              rprs_ad    = rprs_ad+ppr1_ad*precrk1
              ppr1_ad    = zero
              erk_ad     = erk_ad+rprs_ad/(precrk1+precsk1)
              precrk1_ad = precrk1_ad-rprs_ad*(erk/((precrk1+precsk1)* &
                   (precrk1+precsk1)))
              precsk1_ad = precsk1_ad-rprs_ad*(erk/((precrk1+precsk1)* &
                   (precrk1+precsk1)))
              rprs_ad    = zero
           else
              pps0_ad    = pps0_ad+pps1_ad
              pps1_ad    = zero
              ppr0_ad    = ppr0_ad+ppr1_ad
              ppr1_ad    = zero
           endif
           if (rq >= onem10) then
              amaxrq_ad = amaxrq_ad+erk_ad*(qk*rdt/rq)
              qk_ad     = qk_ad+erk_ad*(amaxrq*rdt/rq)
              rq_ad     = rq_ad-erk_ad*(amaxrq*qk*rdt/(rq*rq))
              erk_ad    = zero
           endif
           precrk1_ad = precrk1_ad+erk_ad
           precsk1_ad = precsk1_ad+erk_ad
           erk_ad     = zero
           if (tmt0 >= zero) then
              pps0_ad    = zero
           else
              amaxrq_ad  = amaxrq_ad+pps0_ad*(crs1+crs2*tmt0k)*precsk1* &
                   rhci(k)
              precsk1_ad = precsk1_ad+pps0_ad*(crs1+crs2*tmt0k)*amaxrq* &
                   rhci(k)
              tmt0k_ad   = tmt0k_ad+pps0_ad*crs2*amaxrq*precsk1*rhci(k)
              pps0_ad    = zero
           endif
!         amaxrq_ad  = amaxrq_ad+ppr0_ad*rke2*sqrt(precrk1)
!         precrk1_ad = precrk1_ad+ppr0_ad*rke2*amaxrq*one/(two* &
!              sqrt(precrk1))
!         ppr0_ad    = zero
           if (precrk1>zero) then
              amaxrq_ad  = amaxrq_ad+ppr0_ad*rke2*sqrt(precrk1)
              precrk1_ad = precrk1_ad+ppr0_ad*rke2*amaxrq*one/(two* &
                   sqrt(precrk1))
              ppr0_ad    = zero
           else
              ppr0_ad    = zero
           endif
           
           if (term > zero) then
              term_ad   = term_ad+amaxrq_ad*conde
              amaxrq_ad = zero
           else
              amaxrq_ad = zero
           endif
           rq_ad   = rq_ad-term_ad
           term_ad = zero
           if (precsl_1 > zero) then
              precsl_1_ad = precsl_1_ad+precsk1_ad
              precsk1_ad  = zero
           else
              precsk1_ad  = zero
           endif
           if (precrl_1 > zero) then
              precrl_1_ad = precrl_1_ad+precrk1_ad
              precrk1_ad  = zero
           else
              precrk1_ad  = zero
           endif
           if (tmt0 > (-30._r_kind)) then
              tmt0_ad  = tmt0_ad+tmt0k_ad
              tmt0k_ad = zero
           else
              tmt0k_ad = zero
           endif
           if (ccr > zero) then
              if (iwl == 1) then
                 precrl_0_ad = precrl_0_ad+precrl_1_ad
                 precrl_1_ad = zero
                 precsl_0_ad = precsl_0_ad+precsl_1_ad
                 ww0_ad      = ww0_ad+precsl_1_ad*condt
                 ww2_ad      = ww2_ad-precsl_1_ad*condt
                 precsl_1_ad = zero
                 psaci_ad    = psaci_ad-ww2_ad
                 ww1_ad      = ww1_ad+ww2_ad
                 ww2_ad      = zero
                 if (term3s < cwmks) then
                    term3s_ad = term3s_ad+psaci_ad
                    psaci_ad  = zero
                 else
                    cwmks_ad  = cwmks_ad+psaci_ad
                    psaci_ad  = zero
                 endif
                 cwmks_ad    = cwmks_ad+term3s_ad*aa2*expf*precsl_0
                 expf_ad     = expf_ad+term3s_ad*aa2*precsl_0*cwmks
                 precsl_0_ad = precsl_0_ad+term3s_ad*aa2*expf*cwmks
                 term3s_ad   = zero
                 if (ww1 > zero) then
                    ww1_ad   = ww1_ad+cwmks_ad
                    cwmks_ad = zero
                 else
                    cwmks_ad = zero
                 endif
                 psaut_ad = psaut_ad-ww1_ad
                 ww0_ad   = ww0_ad+ww1_ad
                 ww1_ad   = zero
                 if (term2s < cwmk) then
                    term2s_ad = term2s_ad+psaut_ad
                    psaut_ad  = zero
                 else
                    cwmk_ad   = cwmk_ad+psaut_ad
                    psaut_ad  = zero
                 endif
                 amaxcms_ad = amaxcms_ad+0.0004_r_kind*term2s_ad*expf
                 expf_ad    = expf_ad+0.0004_r_kind*term2s_ad*amaxcms
                 term2s_ad  = zero
                 tmt0_ad    = tmt0_ad+0.025_r_kind*expf_ad*dt*exp(0.025_r_kind*tmt0)
                 expf_ad    = zero
                 if (term1 > zero) then
                    term1_ad   = term1_ad+amaxcms_ad
                    amaxcms_ad = zero
                 else
                    amaxcms_ad = zero
                 endif
                 cwmk_ad     = cwmk_ad+term1_ad
                 term1_ad    = zero
              else
                 precsl_0_ad = precsl_0_ad+precsl_1_ad
                 precsl_1_ad = zero
                 precrl_0_ad = precrl_0_ad+precrl_1_ad
                 ww0_ad      = ww0_ad+precrl_1_ad*condt
                 ww2_ad      = ww2_ad-precrl_1_ad*condt
                 precrl_1_ad = zero
                 praut0_ad   = praut0_ad-ww2_ad
                 ww0_ad      = ww0_ad+ww2_ad
                 ww2_ad      = zero
                 if (praut < cwmk) then
                    praut_ad  = praut_ad+praut0_ad
                    praut0_ad = zero
                 else
                    cwmk_ad   = cwmk_ad+praut0_ad
                    praut0_ad = zero
                 endif
                 amaxcmr_ad = amaxcmr_ad+praut_ad*c00*tem3*(one-exp(-tem5))
                 tem3_ad    = tem3_ad+praut_ad*c00*amaxcmr*(one-exp(-tem5))
                 tem5_ad    = tem5_ad+praut_ad*c00*tem3*amaxcmr*exp(-tem5)
                 praut_ad   = zero
                 if (term6 < 50._r_kind) then
                    term6_ad = term6_ad+tem5_ad
                    tem5_ad  = zero
                 else
                    tem5_ad  = zero
                 endif
                 tem4_ad    = tem4_ad+2*term6_ad*tem4
                 term6_ad   = zero
                 amaxcmr_ad = amaxcmr_ad+tem4_ad*(cmr*tem3/term5)
                 tem3_ad    = tem3_ad+tem4_ad*(amaxcmr*cmr/term5)
                 term5_ad   = term5_ad-tem4_ad*(amaxcmr*cmr*tem3/(term5*term5))
                 tem4_ad    = zero
                 if (ccr > 0.01_r_kind) then
                    ccr_ad   = ccr_ad+term5_ad
                    term5_ad = zero
                 else
                    term5_ad = zero
                 endif

!old - code below can result in divide by zero
!             tem1_ad = tem1_ad+tem3_ad*h300*one/(two*sqrt(tem1*rdt))* &
!                  rdt*(one+half*sqrt(tem2))
!             tem2_ad = tem2_ad+tem3_ad*(one+h300*sqrt(tem1*rdt))*half* &
!                  (one/(two*sqrt(tem2)))
                 if (abs(tem1)>onem20) then 
                    tem1_ad = tem1_ad+tem3_ad*h300*one/ &
                         (two*sqrt(tem1*rdt))*rdt*(one+half*sqrt(tem2))
                 else
                    tem1_ad = tem1_ad+tem3_ad*h300*one/ &
                         (two*sqrt(onem20*rdt))*rdt*(one+half*sqrt(tem2))
                 endif
                 if (abs(tem2)>onem20) then
                    tem2_ad = tem2_ad+tem3_ad*(one+h300*sqrt(tem1*rdt))* &
                         half*(one/(two*sqrt(tem2)))
                 else
                    tem2_ad = tem2_ad+tem3_ad*(one+h300*sqrt(tem1*rdt))* &
                         half*(one/(two*sqrt(onem20)))
                 endif
!new
                 
                 tem3_ad  = zero
                 term4_ad = term4_ad+tem2_ad
                 tem2_ad  = zero
                 if (term3r < 20._r_kind) then
                    term3r_ad = term3r_ad+term4_ad
                    term4_ad  = zero
                 else
                    term4_ad  = zero
                 endif
                 if (term2r > zero) then
                    term2r_ad = term2r_ad+term3r_ad
                    term3r_ad = zero
                 else
                    term3r_ad = zero
                 endif
                 tin_ad      = tin_ad-term2r_ad
                 term2r_ad   = zero
                 precrl_0_ad = precrl_0_ad+tem1_ad
                 precsl_0_ad = precsl_0_ad+tem1_ad
                 tem1_ad     = zero
                 cwmk_ad     = cwmk_ad+amaxcmr_ad
                 amaxcmr_ad  = zero
              endif
              if (ww0 > zero) then
                 ww0_ad  = ww0_ad+cwmk_ad
                 cwmk_ad = zero
              else
                 cwmk_ad = zero
              endif
              cwmin_ad    = cwmin_ad+ww0_ad
              ww0_ad      = zero
           else
              precsl_0_ad = precsl_0_ad+precsl_1_ad
              precsl_1_ad = zero
              precrl_0_ad = precrl_0_ad+precrl_1_ad
              precrl_1_ad = zero
              cwmin_ad    = cwmin_ad+ww2_ad
              ww2_ad      = zero
           endif
           if (rq < rhc(k)) then
              ccr_ad = zero
           else if (rq >= one) then
              ccr_ad = zero
           else
              rqkll_ad = rqkll_ad+ccr_ad*(one/(two*sqrt((one-rqkll)/ &
                   (one-rhc(k))))/(one-rhc(k)))
              ccr_ad   = zero
              if (rq < one) then
                 rq_ad    = rq_ad+rqkll_ad
                 rqkll_ad = zero
              else
                 rqkll_ad = zero
              endif
           endif
           if (qs <= onem10) then
              rq_ad = zero
           else
              qk_ad = qk_ad+rq_ad/qs
              qs_ad = qs_ad-rq_ad*(qk/(qs*qs))
              rq_ad = zero
           endif
           if (tmt0 < (-15._r_kind)) then
              qk_ad = qk_ad+fi_ad
              qs_ad = qs_ad-fi_ad*rhc(k)
              fi_ad = zero
           endif
           if (qs0 > epsq) then
              qs0_ad = qs0_ad+qs_ad
              qs_ad  = zero
           else
              qs_ad  = zero
           endif
           es_ad   = es_ad+qs0_ad*(eps/(pres+epsm1*es)-eps*es*epsm1/ &
                ((pres+epsm1*es)*(pres+epsm1*es)))
           qs0_ad  = zero
           call fpvsx_ad( tin,es,tin_ad,es_ad,adjoint )
           es_ad   = zero
           tin_ad  = tin_ad+tmt0_ad
           tmt0_ad = zero
           if (qin > epsq) then
              qin_ad = qin_ad+qk_ad
              qk_ad  = zero
           else
              qk_ad  = zero
           endif
        else
           cwmin_ad    = cwmin_ad+ww2_ad
           ww2_ad      = zero
           precsl_0_ad = precsl_0_ad+precsl_3_ad
           precsl_3_ad = zero
           precrl_0_ad = precrl_0_ad+precrl_3_ad
           precrl_3_ad = zero
           qin_ad      = qin_ad+q2_ad
           q2_ad       = zero
           tin_ad      = tin_ad+t2_ad
           t2_ad       = zero
        endif
        if (cwmin > climit) then
           cwmin_ad = cwmin_ad+wwn_ad
           wwn_ad   = zero
        else
           wwn_ad   = zero
        endif
        if (precsl_0 > zero) then
           precsl_0_ad = precsl_0_ad+precsk0_ad
           precsk0_ad  = zero
        else
           precsk0_ad  = zero
        endif
        if (precrl_0 > zero) then
           precrl_0_ad = precrl_0_ad+precrk0_ad
           precrk0_ad  = zero
        else
           precrk0_ad  = zero
        endif
        cwm_in_ad(k) = cwm_in_ad(k)+cwmin_ad
        cwmin_ad     = zero
        q_in_ad(k)   = q_in_ad(k)+qin_ad
        qin_ad       = zero
        t_in_ad(k)   = t_in_ad(k)+tin_ad
        tin_ad       = zero
        precsl1k_ad  = precsl1k_ad+precsl_0_ad
        precsl_0_ad    = zero
        precrl1k_ad    = precrl1k_ad+precrl_0_ad
        precrl_0_ad    = zero
     end do
        
  else
     rn_out_ad = zero
     do k = km, 1, -1
        if (cwm_in(k) < zero) then
           cwm_out_ad(k) = zero
           cwm_in_ad(k)  = cwm_in_ad(k)-t_out_ad(k)*hvap*rcp
           t_in_ad(k)    = t_in_ad(k)+t_out_ad(k)
           t_out_ad(k)   = zero
           cwm_in_ad(k)  = cwm_in_ad(k)+q_out_ad(k)
           q_in_ad(k)    = q_in_ad(k)+q_out_ad(k)
           q_out_ad(k)   = zero
        else
           cwm_in_ad(k)  = cwm_in_ad(k)+cwm_out_ad(k)
           cwm_out_ad(k) = zero
           t_in_ad(k)    = t_in_ad(k)+t_out_ad(k)
           t_out_ad(k)   = zero
           q_in_ad(k)    = q_in_ad(k)+q_out_ad(k)
           q_out_ad(k)   = zero
        endif
     end do
  endif
  rn_out_ad    = zero
  precsl1k_ad  = zero
  precrl1k_ad  = zero
  
  return
end subroutine precpd_ad


