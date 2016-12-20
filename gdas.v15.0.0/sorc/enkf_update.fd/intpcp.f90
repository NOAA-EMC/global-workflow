module intpcpmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intpcpmod    module for intpcp and its tangent linear intpcp_tl
!   prgmmr:
!
! abstract: module for intpcp and its tangent linear intpcp_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intpcp and its tangent linear intpcp_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intpcp_tl
!   2009-08-13  lueken - update documentation
!
! subroutines included:
!   sub intpcp_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intpcp

interface intpcp; module procedure &
          intpcp_
end interface

contains

subroutine intpcp_(pcphead,rval,sval)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpcp      precip rate nonlin qc obs operator
!   prgmmr: treadon          org: np23                date: 2003-09-13
!
! abstract: apply precipitation rate operator and adjoint with
!            addition of nonlinear qc operator
!
! program history log:
!   2003-12-18  treadon - initial routine
!   2004-06-15  treadon - reformat documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intpcp and intpcp_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-01-19  derber  - limit pcp_ges* > zero
!   2007-02-15  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-06-02  safford - rm unused vars
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-28  todling  - adapt Tremolet linearization of inner loop to May-2008 version
!                        - remove quad precision; mpi_allgather is reproducible
!                        - turn FOTO optional; changed ptr%time handle
!                        - internal copy of pred's to avoid reshape in calling program
!   2009-01-26 todling   - bug fix in linearlization 
!   2010-03-25 zhu       - use state_vector in the interface for generalizing control variable
!                        - add treatment when cw is not control variable
!                        - use pointer_state
!   2010-05-13 todling   - update to use gsi_bundle
!                        - BUG FIX: foto was using TV instead of TSENS
!                        - on-the-spot handling of non-essential vars
!   2011-11-01 eliu      - add handling for ql and qi increments  
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     pcphead  - obs type pointer to obs structure
!     st       - input temperature correction field
!     sq       - input q correction field
!     su       - input zonal wind correction field
!     sv       - input meridional wind correction field
!     sql      - input cloud liquid water mixing ratio correction field
!     sqi      - input cloud ice water mixing ratio correction field
!     rt
!     rq
!     ru
!     rql
!     rqi
!
!   output argument list:
!     rt       - output  t vector after inclusion of pcp. info.
!     rq       - output  q vector after inclusion of pcp. info.
!     ru       - output  u vector after inclusion of pcp. info.
!     rv       - output  v vector after inclusion of pcp. info.
!     rql      - output ql vector after inclusion of pcp. info.
!     rqi      - output qi vector after inclusion of pcp. info.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: pcp_ob_type,lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use pcpinfo, only: npcptype,npredp,b_pcp,pg_pcp,tinym1_obs
  use constants, only: zero,one,half,tiny_r_kind,cg_term,r3600
  use gridmod, only: nsig,latlon11,latlon1n
  use gsi_4dvar, only: ltlint
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt  
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(pcp_ob_type),pointer,intent(in) :: pcphead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  logical:: lcld
  integer(i_kind) j1,j2,j3,j4,nq,nu,nv,ncwm,n,nt,kx,ier,istatus,icw,iql,iqi
  real(r_kind) dt,dq,du,dv,dcwm,dcwm_ad,termges_ad,w1,w2,w3,w4
  real(r_kind) pcp_ges_ad,dq_ad,dt_ad,dv_ad,du_ad,pcp_ges
  real(r_kind) obsges,termges,time_pcp,termges_tl,pcp_ges_tl,pcp_cur,termcur
  real(r_kind) cg_pcp,p0,wnotgross,wgross
  type(pcp_ob_type), pointer :: pcpptr

  real(r_kind),pointer,dimension(:):: st,sq,su,sv
  real(r_kind),pointer,dimension(:):: sql,sqi,scwm   
  real(r_kind),pointer,dimension(:):: rql,rqi,rcwm  
  real(r_kind),pointer,dimension(:):: rt,rq,ru,rv
  real(r_kind),pointer,dimension(:):: xhat_dt_tsen,xhat_dt_q,xhat_dt_u,xhat_dt_v,xhat_dt_cw
  real(r_kind),pointer,dimension(:):: dhat_dt_tsen,dhat_dt_q,dhat_dt_u,dhat_dt_v,dhat_dt_cw
 
! If no pcp obs return
  if(.not. associated(pcphead))return

! Retrieve pointers
  ier=0
  call gsi_bundlegetpointer(sval,'u',    su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',    sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'tsen' ,st,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'q',    sq,istatus);ier=istatus+ier
  if(ier/=0)return

  call gsi_bundlegetpointer(rval,'u',    ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',    rv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'tsen' ,rt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q',    rq,istatus);ier=istatus+ier
  if(ier/=0)return

! Non-essentials:
  icw=0; iql=0; iqi=0
  call gsi_bundlegetpointer(sval,'cw', scwm,istatus);icw=istatus+icw
  call gsi_bundlegetpointer(rval,'cw', rcwm,istatus);icw=istatus+icw
  call gsi_bundlegetpointer(sval,'ql', sql,istatus) ;iql=istatus+iql
  call gsi_bundlegetpointer(sval,'qi', sqi,istatus) ;iqi=istatus+iqi
  call gsi_bundlegetpointer(rval,'ql', rql,istatus) ;iql=istatus+iql
  call gsi_bundlegetpointer(rval,'qi', rqi,istatus) ;iqi=istatus+iqi

  if(l_foto) then
     call gsi_bundlegetpointer(xhat_dt,'u',      xhat_dt_u,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'v',      xhat_dt_v,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'tsen',xhat_dt_tsen,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'q',      xhat_dt_q,istatus);ier=istatus+ier

     call gsi_bundlegetpointer(dhat_dt,'u',      dhat_dt_u,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'v',      dhat_dt_v,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'tsen',dhat_dt_tsen,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'q',      dhat_dt_q,istatus);ier=istatus+ier
     if(ier/=0)return

! Non-essentials:
     call gsi_bundlegetpointer(xhat_dt,'cw',    xhat_dt_cw,istatus);icw=istatus+icw
     call gsi_bundlegetpointer(dhat_dt,'cw',    dhat_dt_cw,istatus);icw=istatus+icw
  endif

  lcld = (icw==0 .or. (iql+iqi)==0)

  pcpptr => pcphead
  do while(associated(pcpptr))
     j1=pcpptr%ij(1)
     j2=pcpptr%ij(2)
     j3=pcpptr%ij(3)
     j4=pcpptr%ij(4)
     w1=pcpptr%wij(1)
     w2=pcpptr%wij(2)
     w3=pcpptr%wij(3)
     w4=pcpptr%wij(4)
     pcp_ges = pcpptr%ges 
     pcp_ges_tl = zero


     if(l_foto) time_pcp=pcpptr%time*r3600
!    Compute updated simulated rain rate based on changes in t,q,u,v,cwm
     do n = 1,nsig
        dt = w1* st(j1)+w2* st(j2)+ w3* st(j3)+w4* st(j4)
        dq = w1* sq(j1)+w2* sq(j2)+ w3* sq(j3)+w4* sq(j4)
        du = w1* su(j1)+w2* su(j2)+ w3* su(j3)+w4* su(j4)
        dv = w1* sv(j1)+w2* sv(j2)+ w3* sv(j3)+w4* sv(j4)
        if (lcld) then
           if (icw==0) then
              dcwm=w1* scwm(j1)+w2* scwm(j2)+  &
                   w3* scwm(j3)+w4* scwm(j4)
           else
              dcwm=w1* sql(j1)+w1* sqi(j1)+  &
                   w2* sql(j2)+w2* sqi(j2)+  &
                   w3* sql(j3)+w3* sqi(j3)+  &
                   w4* sql(j4)+w4* sqi(j4)
           endif
        else 
           dcwm=zero
        endif

        if (l_foto) then
           dt = dt+&
               (w1*xhat_dt_tsen(j1)+w2*xhat_dt_tsen(j2)+ &
                w3*xhat_dt_tsen(j3)+w4*xhat_dt_tsen(j4))*time_pcp
           dq = dq+&
               (w1*xhat_dt_q(j1)+w2*xhat_dt_q(j2)+ &
                w3*xhat_dt_q(j3)+w4*xhat_dt_q(j4))*time_pcp
           du = du+&
               (w1*xhat_dt_u(j1)+w2*xhat_dt_u(j2)+ &
                w3*xhat_dt_u(j3)+w4*xhat_dt_u(j4))*time_pcp
           dv = dv+&
               (w1*xhat_dt_v(j1)+w2*xhat_dt_v(j2)+ &
                w3*xhat_dt_v(j3)+w4*xhat_dt_v(j4))*time_pcp
           if (icw==0) then
              dcwm=dcwm+&
                  (w1*xhat_dt_cw(j1)+w2*xhat_dt_cw(j2)+  &
                   w3*xhat_dt_cw(j3)+w4*xhat_dt_cw(j4))*time_pcp
           end if
        endif
        
        nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig
        pcp_ges_tl = pcp_ges_tl +&
                     pcpptr%dpcp_dvar(nt)*dt + &
                     pcpptr%dpcp_dvar(nq)*dq + &
                     pcpptr%dpcp_dvar(nu)*du + &
                     pcpptr%dpcp_dvar(nv)*dv + &
                     pcpptr%dpcp_dvar(ncwm)*dcwm
        
        j1=j1+latlon11
        j2=j2+latlon11
        j3=j3+latlon11
        j4=j4+latlon11
        
     end do
     pcp_cur = pcp_ges + pcp_ges_tl

!    Ensure rain rate is greater than a small zero
     pcp_ges = max(pcp_ges,zero)
     termges = log(one+pcp_ges)

!    Compute o-g 
     if (ltlint) then
        if ( pcp_ges > tinym1_obs ) then
           termges_tl = pcp_ges_tl/(one+pcp_ges)
        else
           termges_tl = zero
        endif
        obsges = termges + termges_tl - pcpptr%obs 
     else
        pcp_cur = max(pcp_cur,zero)
        termcur = log(one+pcp_cur)
        termges_tl = termcur - termges
        obsges = termcur - pcpptr%obs 
     endif

     if(luse_obsdiag)then
        if (lsaveobsens) then
           termges_ad = termges_tl*pcpptr%err2*pcpptr%raterr2
           pcpptr%diags%obssen(jiter) = termges_ad
        else
           if (pcpptr%luse) pcpptr%diags%tldepart(jiter)=termges_tl
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
!          Adjoint model
           kx=pcpptr%icxp
           if (nlnqc_iter .and. pg_pcp(kx) > tiny_r_kind .and.  &
                                b_pcp(kx)  > tiny_r_kind) then
              cg_pcp=cg_term/b_pcp(kx)
              wnotgross= one-pg_pcp(kx)*varqc_iter
              wgross = varqc_iter*pg_pcp(kx)*cg_pcp/wnotgross
              p0   = wgross/(wgross+exp(-half*pcpptr%err2*obsges**2))
              obsges = obsges*(one-p0)
           endif

           termges_ad  = obsges*pcpptr%err2*pcpptr%raterr2
        endif

!       Adjoint for logrithmic forumulation
        if (ltlint) then
           if ( pcp_ges > tinym1_obs ) then
              pcp_ges_ad = termges_ad/(one+pcp_ges)
           else
              pcp_ges_ad = zero
           endif
        else
           pcp_ges_ad = termges_ad/(one+pcp_cur)
        endif

!       Adjoint of pcp_ges update

        j1=pcpptr%ij(1)
        j2=pcpptr%ij(2)
        j3=pcpptr%ij(3)
        j4=pcpptr%ij(4)
        do n=1,nsig
           nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig

           if (lcld) dcwm_ad = pcpptr%dpcp_dvar(ncwm)*pcp_ges_ad
           dv_ad   = pcpptr%dpcp_dvar(nv)*pcp_ges_ad
           du_ad   = pcpptr%dpcp_dvar(nu)*pcp_ges_ad
           dq_ad   = pcpptr%dpcp_dvar(nq)*pcp_ges_ad
           dt_ad   = pcpptr%dpcp_dvar(nt)*pcp_ges_ad

           if (lcld) then
              if (icw==0) then
                 rcwm(j4) = rcwm(j4) + w4*dcwm_ad   
                 rcwm(j3) = rcwm(j3) + w3*dcwm_ad   
                 rcwm(j2) = rcwm(j2) + w2*dcwm_ad   
                 rcwm(j1) = rcwm(j1) + w1*dcwm_ad    
              else
                 rql(j4) = rql(j4)+w4*dcwm_ad       
                 rqi(j4) = rqi(j4)+w4*dcwm_ad       
                 rql(j3) = rql(j3)+w3*dcwm_ad       
                 rqi(j3) = rqi(j3)+w3*dcwm_ad       
                 rql(j2) = rql(j2)+w2*dcwm_ad        
                 rqi(j2) = rqi(j2)+w2*dcwm_ad       
                 rql(j1) = rql(j1)+w1*dcwm_ad      
                 rqi(j1) = rqi(j1)+w1*dcwm_ad       
              end if
           end if

           rv(j4) = rv(j4) + w4*dv_ad
           rv(j3) = rv(j3) + w3*dv_ad
           rv(j2) = rv(j2) + w2*dv_ad
           rv(j1) = rv(j1) + w1*dv_ad
 
           ru(j4) = ru(j4) + w4*du_ad
           ru(j3) = ru(j3) + w3*du_ad
           ru(j2) = ru(j2) + w2*du_ad
           ru(j1) = ru(j1) + w1*du_ad

           rq(j4) = rq(j4) + w4*dq_ad
           rq(j3) = rq(j3) + w3*dq_ad
           rq(j2) = rq(j2) + w2*dq_ad
           rq(j1) = rq(j1) + w1*dq_ad

           rt(j4) = rt(j4) + w4*dt_ad
           rt(j3) = rt(j3) + w3*dt_ad
           rt(j2) = rt(j2) + w2*dt_ad
           rt(j1) = rt(j1) + w1*dt_ad

           if (l_foto) then
              if (icw==0) dcwm_ad = time_pcp*dcwm_ad
              dv_ad   = time_pcp*dv_ad
              du_ad   = time_pcp*du_ad
              dq_ad   = time_pcp*dq_ad
              dt_ad   = time_pcp*dt_ad

              if (icw==0) then
                 dhat_dt_cw(j4) = dhat_dt_cw(j4) + w4*dcwm_ad
                 dhat_dt_cw(j3) = dhat_dt_cw(j3) + w3*dcwm_ad
                 dhat_dt_cw(j2) = dhat_dt_cw(j2) + w2*dcwm_ad
                 dhat_dt_cw(j1) = dhat_dt_cw(j1) + w1*dcwm_ad
              end if

              dhat_dt_v(j4) = dhat_dt_v(j4) + w4*dv_ad
              dhat_dt_v(j3) = dhat_dt_v(j3) + w3*dv_ad
              dhat_dt_v(j2) = dhat_dt_v(j2) + w2*dv_ad
              dhat_dt_v(j1) = dhat_dt_v(j1) + w1*dv_ad

              dhat_dt_u(j4) = dhat_dt_u(j4) + w4*du_ad
              dhat_dt_u(j3) = dhat_dt_u(j3) + w3*du_ad
              dhat_dt_u(j2) = dhat_dt_u(j2) + w2*du_ad
              dhat_dt_u(j1) = dhat_dt_u(j1) + w1*du_ad
 
              dhat_dt_q(j4) = dhat_dt_q(j4) + w4*dq_ad
              dhat_dt_q(j3) = dhat_dt_q(j3) + w3*dq_ad
              dhat_dt_q(j2) = dhat_dt_q(j2) + w2*dq_ad
              dhat_dt_q(j1) = dhat_dt_q(j1) + w1*dq_ad

              dhat_dt_tsen(j4) = dhat_dt_tsen(j4) + w4*dt_ad
              dhat_dt_tsen(j3) = dhat_dt_tsen(j3) + w3*dt_ad
              dhat_dt_tsen(j2) = dhat_dt_tsen(j2) + w2*dt_ad
              dhat_dt_tsen(j1) = dhat_dt_tsen(j1) + w1*dt_ad
           endif

           j1=j1+latlon11
           j2=j2+latlon11
           j3=j3+latlon11
           j4=j4+latlon11

        end do
     endif
     pcpptr => pcpptr%llpoint 
  end do

  return
end subroutine intpcp_


end module intpcpmod
