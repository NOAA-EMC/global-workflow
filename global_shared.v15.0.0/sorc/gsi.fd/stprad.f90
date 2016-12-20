module stpradmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpradmod    module for stprad and its tangent linear stprad_tl
!  prgmmr:
!
! abstract: module for stprad and its tangent linear stprad_tl
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stprad and its tangent linear stprad_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stprad_tl
!   2009-08-12  lueken - update documentation
!   2011-05-17  todling - add internal routine set_
!
! subroutines included:
!   sub stprad
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind
implicit none

PRIVATE
PUBLIC stprad


contains

subroutine stprad(radhead,dval,xval,rpred,spred,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprad compute contribution to penalty and stepsize
!                from rad, using nonlinear qc.
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: compute contribution to penalty and stepsize from radiances.
!
! program history log:
!   1990-10-11  parrish
!   1992-07-21
!   1995-07-17  derber
!   1997-03-10  wu       
!   1998-02-02  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-30  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-01-20  okamoto - add wind components
!   2005-04-11  treadon - merge stprad and stprad_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-04-09  safford - rm unused vars and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-03-25  zhu     - use state_vector in the interface;
!                       - add handlings of sst,oz cases; add pointer_state
!   2010-05-13  todling - update to use gsi_bundle
!                       - on-the-spot handling of non-essential vars
!   2010-07-10  todling - remove omp directives (per merge w/ r8741; Derber?)
!   2011-05-04  todling - merge in Min-Jeong Kim's cloud clear assimilation (connect to Metguess)
!   2011-05-16  todling - generalize entries in radiance jacobian
!   2011-05-17  augline/todling - add hydrometeors
!   2016-07-19  kbathmann- adjustment to bias correction when using correlated obs
!
!   input argument list:
!     radhead
!     rt       - search direction for temperature
!     rq       - search direction for moisture 
!     roz      - search direction for ozone
!     ru       - search direction for zonal wind
!     rv       - search direction for meridional wind
!     rst      - search direction for skin temperature
!     st       - input temperature correction field        
!     sq       - input q correction field        
!     soz      - input ozone correction field        
!     su       - input u correction field
!     sv       - input v correction field
!     sst      - input skin temp. vector 
!     rpred    - search direction for predictors
!     spred    - input predictor values
!     sges     - step size estimates(nstep)
!     nstep    - number of stepsizes (==0 means use outer iteration value)
!
!   output argument list:
!     out(1:nstep)   - penalty for radiance data sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use radinfo, only: npred,jpch_rad,b_rad,pg_rad
  use radinfo, only: radjacnames,radjacindxs,nsigradjac
  use obsmod, only: rad_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,r3600,zero_quad,one_quad
  use gridmod, only: nsig,latlon11,latlon1n
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_get
  use mpeu_util, only: getindex
  use intradmod, only: luseu,lusev,luset,luseq,lusecw,luseoz,luseqg,luseqh,luseqi,luseql, &
          luseqr,luseqs,lusesst
  use intradmod, only: itv,iqv,ioz,icw,ius,ivs,isst,iqg,iqh,iqi,iql,iqr,iqs,lgoback
  implicit none
  
! Declare passed variables
  type(rad_ob_type),pointer              ,intent(in   ) :: radhead
  integer(i_kind)                        ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep))   ,intent(inout) :: out
  real(r_kind),dimension(npred,jpch_rad) ,intent(in   ) :: rpred,spred
  real(r_kind),dimension(max(1,nstep))   ,intent(in   ) :: sges
  type(gsi_bundle),intent(in) :: dval
  type(gsi_bundle),intent(in) :: xval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) nn,n,ic,k,nx,j1,j2,j3,j4,kk, mm, ic1
  real(r_kind) val2,val,w1,w2,w3,w4
  real(r_kind),dimension(nsigradjac):: tdir,rdir
  real(r_kind) cg_rad,wgross,wnotgross
  real(r_kind) time_rad
  integer(i_kind),dimension(nsig) :: j1n,j2n,j3n,j4n
  real(r_kind),dimension(max(1,nstep)) :: term,rad
  type(rad_ob_type), pointer :: radptr
  real(r_kind), dimension(:,:), allocatable:: rsqrtinv
  integer(i_kind) :: chan_count, ii, jj
  real(r_kind),pointer,dimension(:) :: rt,rq,rcw,roz,ru,rv,rqg,rqh,rqi,rql,rqr,rqs
  real(r_kind),pointer,dimension(:) :: st,sq,scw,soz,su,sv,sqg,sqh,sqi,sql,sqr,sqs
  real(r_kind),pointer,dimension(:) :: rst,sst
  real(r_kind),pointer,dimension(:) :: xhat_dt_t,xhat_dt_q,xhat_dt_oz,xhat_dt_u,xhat_dt_v
  real(r_kind),pointer,dimension(:) :: dhat_dt_t,dhat_dt_q,dhat_dt_oz,dhat_dt_u,dhat_dt_v

  out=zero_quad

!  If no rad data return
  if(.not. associated(radhead))return

  if(lgoback)return

! Retrieve pointers
  call gsi_bundlegetpointer(xval,'u',  su, istatus)
  call gsi_bundlegetpointer(xval,'v',  sv, istatus)
  call gsi_bundlegetpointer(xval,'tv' ,st, istatus)
  call gsi_bundlegetpointer(xval,'q',  sq, istatus)
  call gsi_bundlegetpointer(xval,'cw' ,scw,istatus)
  call gsi_bundlegetpointer(xval,'oz' ,soz,istatus)
  call gsi_bundlegetpointer(xval,'sst',sst,istatus)
  call gsi_bundlegetpointer(xval,'qg' ,sqg,istatus)
  call gsi_bundlegetpointer(xval,'qh' ,sqh,istatus)
  call gsi_bundlegetpointer(xval,'qi' ,sqi,istatus)
  call gsi_bundlegetpointer(xval,'ql' ,sql,istatus)
  call gsi_bundlegetpointer(xval,'qr' ,sqr,istatus)
  call gsi_bundlegetpointer(xval,'qs' ,sqs,istatus)

  call gsi_bundlegetpointer(dval,'u',  ru, istatus)
  call gsi_bundlegetpointer(dval,'v',  rv, istatus)
  call gsi_bundlegetpointer(dval,'tv' ,rt, istatus)
  call gsi_bundlegetpointer(dval,'q',  rq, istatus)
  call gsi_bundlegetpointer(dval,'cw' ,rcw,istatus)
  call gsi_bundlegetpointer(dval,'oz' ,roz,istatus)
  call gsi_bundlegetpointer(dval,'sst',rst,istatus)
  call gsi_bundlegetpointer(dval,'qg' ,rqg,istatus)
  call gsi_bundlegetpointer(dval,'qh' ,rqh,istatus)
  call gsi_bundlegetpointer(dval,'qi' ,rqi,istatus)
  call gsi_bundlegetpointer(dval,'ql' ,rql,istatus)
  call gsi_bundlegetpointer(dval,'qr' ,rqr,istatus)
  call gsi_bundlegetpointer(dval,'qs' ,rqs,istatus)

  if(l_foto) then
     call gsi_bundlegetpointer(xhat_dt,'u',  xhat_dt_u, istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'v',  xhat_dt_v, istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'tv' ,xhat_dt_t, istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'q',  xhat_dt_q, istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'oz' ,xhat_dt_oz,istatus);ioz=istatus+ioz
     if(ier/=0)return

     call gsi_bundlegetpointer(dhat_dt,'u',  dhat_dt_u, istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'v',  dhat_dt_v, istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'tv' ,dhat_dt_t, istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'q',  dhat_dt_q, istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'oz' ,dhat_dt_oz,istatus);ioz=istatus+ioz
     if(ier/=0)return
  endif


  tdir=zero
  rdir=zero

  radptr=>radhead
  do while(associated(radptr))
     if(radptr%luse)then
        if(nstep > 0)then
           j1=radptr%ij(1)
           j2=radptr%ij(2)
           j3=radptr%ij(3)
           j4=radptr%ij(4)
           w1=radptr%wij(1)
           w2=radptr%wij(2)
           w3=radptr%wij(3)
           w4=radptr%wij(4)
           if(luseu)then
           if (radptr%use_corr_obs) then
              allocate(rsqrtinv(radptr%nchan,radptr%nchan))
              chan_count=0
              do ii=1,radptr%nchan
                 do jj=ii,radptr%nchan
                    chan_count=chan_count+1
                    rsqrtinv(ii,jj)=radptr%rsqrtinv(chan_count)
                    rsqrtinv(jj,ii)=radptr%rsqrtinv(chan_count)
                 end do
              end do
           end if
              tdir(ius+1)=w1* su(j1) + w2* su(j2) + w3* su(j3) + w4* su(j4)
              rdir(ius+1)=w1* ru(j1) + w2* ru(j2) + w3* ru(j3) + w4* ru(j4)
           endif
           if(lusev)then
              tdir(ivs+1)=w1* sv(j1) + w2* sv(j2) + w3* sv(j3) + w4* sv(j4)
              rdir(ivs+1)=w1* rv(j1) + w2* rv(j2) + w3* rv(j3) + w4* rv(j4)
           endif
           if (isst>=0) then
              tdir(isst+1)=w1*sst(j1) + w2*sst(j2) + w3*sst(j3) + w4*sst(j4)   
              rdir(isst+1)=w1*rst(j1) + w2*rst(j2) + w3*rst(j3) + w4*rst(j4)   
           end if

           j1n(1) = j1
           j2n(1) = j2
           j3n(1) = j3
           j4n(1) = j4
           do n=2,nsig
              j1n(n) = j1n(n-1)+latlon11
              j2n(n) = j2n(n-1)+latlon11
              j3n(n) = j3n(n-1)+latlon11
              j4n(n) = j4n(n-1)+latlon11
           enddo
           do n=1,nsig
              j1 = j1n(n)
              j2 = j2n(n)
              j3 = j3n(n)
              j4 = j4n(n)
  
!             Input state vector
!             Input search direction vector
              if(luset)then
                 tdir(itv+n)=w1* st(j1) +w2* st(j2) + w3* st(j3) +w4*  st(j4)
                 rdir(itv+n)=w1* rt(j1) +w2* rt(j2) + w3* rt(j3) +w4*  rt(j4)
              endif
              if(luseq)then
                 tdir(iqv+n)=w1* sq(j1) +w2* sq(j2) + w3* sq(j3) +w4*  sq(j4)
                 rdir(iqv+n)=w1* rq(j1) +w2* rq(j2) + w3* rq(j3) +w4*  rq(j4)
              endif
              if (luseoz) then
                 tdir(ioz+n)=w1*soz(j1)+w2*soz(j2)+ w3*soz(j3)+w4*soz(j4)
                 rdir(ioz+n)=w1*roz(j1)+w2*roz(j2)+ w3*roz(j3)+w4*roz(j4)
              end if
              if (lusecw) then
                 tdir(icw+n)=w1*scw(j1)+w2*scw(j2)+ w3*scw(j3)+w4*scw(j4)
                 rdir(icw+n)=w1*rcw(j1)+w2*rcw(j2)+ w3*rcw(j3)+w4*rcw(j4)
              end if
              if (luseqg) then
                 tdir(iqg+n)=w1*sqg(j1)+w2*sqg(j2)+ w3*sqg(j3)+w4*sqg(j4)
                 rdir(iqg+n)=w1*rqg(j1)+w2*rqg(j2)+ w3*rqg(j3)+w4*rqg(j4)
              end if
              if (luseqh) then
                 tdir(iqh+n)=w1*sqh(j1)+w2*sqh(j2)+ w3*sqh(j3)+w4*sqh(j4)
                 rdir(iqh+n)=w1*rqh(j1)+w2*rqh(j2)+ w3*rqh(j3)+w4*rqh(j4)
              end if
              if (luseqi) then
                 tdir(iqi+n)=w1*sqi(j1)+w2*sqi(j2)+ w3*sqi(j3)+w4*sqi(j4)
                 rdir(iqi+n)=w1*rqi(j1)+w2*rqi(j2)+ w3*rqi(j3)+w4*rqi(j4)
              end if
              if (luseql) then
                 tdir(iql+n)=w1*sql(j1)+w2*sql(j2)+ w3*sql(j3)+w4*sql(j4)
                 rdir(iql+n)=w1*rql(j1)+w2*rql(j2)+ w3*rql(j3)+w4*rql(j4)
              end if
              if (luseqr) then
                 tdir(iqr+n)=w1*sqr(j1)+w2*sqr(j2)+ w3*sqr(j3)+w4*sqr(j4)
                 rdir(iqr+n)=w1*rqr(j1)+w2*rqr(j2)+ w3*rqr(j3)+w4*rqr(j4)
              end if
              if (luseqs) then
                 tdir(iqs+n)=w1*sqs(j1)+w2*sqs(j2)+ w3*sqs(j3)+w4*sqs(j4)
                 rdir(iqs+n)=w1*rqs(j1)+w2*rqs(j2)+ w3*rqs(j3)+w4*rqs(j4)
              end if


           end do
           if(l_foto)then
              time_rad=radptr%time*r3600
              if(luseu)then
                 tdir(ius+1)=tdir(ius+1)+ &
                    (w1*xhat_dt_u(j1) + w2*xhat_dt_u(j2) + &
                     w3*xhat_dt_u(j3) + w4*xhat_dt_u(j4))*time_rad
                 rdir(ius+1)=rdir(ius+1)+ &
                    (w1*dhat_dt_u(j1) + w2*dhat_dt_u(j2) + &
                     w3*dhat_dt_u(j3) + w4*dhat_dt_u(j4))*time_rad
              endif
              if(lusev)then
                 tdir(ivs+1)=tdir(ivs+1)+ &
                    (w1*xhat_dt_v(j1) + w2*xhat_dt_v(j2) + &
                     w3*xhat_dt_v(j3) + w4*xhat_dt_v(j4))*time_rad
                 rdir(ivs+1)=rdir(ivs+1)+ &
                    (w1*dhat_dt_v(j1) + w2*dhat_dt_v(j2) + &
                     w3*dhat_dt_v(j3) + w4*dhat_dt_v(j4))*time_rad
              endif
              do n=1,nsig
                 j1 = j1n(n)
                 j2 = j2n(n)
                 j3 = j3n(n)
                 j4 = j4n(n)

!                Input state vector
!                Input search direction vector
                 if(luset)then
                    tdir(itv+n)=  tdir(itv+n)+                      &
                       (w1*xhat_dt_t(j1) +w2*xhat_dt_t(j2) +        &
                        w3*xhat_dt_t(j3) +w4*xhat_dt_t(j4))*time_rad
                    rdir(itv+n)=  rdir(itv+n)+                      &
                       (w1*dhat_dt_t(j1) +w2*dhat_dt_t(j2) +        &
                        w3*dhat_dt_t(j3) +w4*dhat_dt_t(j4))*time_rad
                 endif
                 if(luseq)then
                    tdir(iqv+n)= tdir(iqv+n)+                       &
                       (w1*xhat_dt_q(j1) +w2*xhat_dt_q(j2) +        &
                        w3*xhat_dt_q(j3) +w4*xhat_dt_q(j4))*time_rad
                    rdir(iqv+n)= rdir(iqv+n)+                       &
                       (w1*dhat_dt_q(j1) +w2*dhat_dt_q(j2) +        &
                        w3*dhat_dt_q(j3) +w4*dhat_dt_q(j4))*time_rad
                 endif
                 if (luseoz) then
                    tdir(ioz+n)=tdir(ioz+n)+                        &
                       (w1*xhat_dt_oz(j1)+w2*xhat_dt_oz(j2)+        &
                        w3*xhat_dt_oz(j3)+w4*xhat_dt_oz(j4))*time_rad
                    rdir(ioz+n)=rdir(ioz+n)+                        &
                       (w1*dhat_dt_oz(j1)+w2*dhat_dt_oz(j2)+        &
                        w3*dhat_dt_oz(j3)+w4*dhat_dt_oz(j4))*time_rad
                 end if
 

              end do
           end if
        end if
        do nn=1,radptr%nchan

           val2=-radptr%res(nn)

           if(nstep > 0)then
              val = zero
!             contribution from bias corection
              ic=radptr%icx(nn)
              do nx=1,npred
                 if (radptr%use_corr_obs) then
                    do mm=1,radptr%nchan
                       ic1=radptr%icx(mm)
                       val2=val2+spred(nx,ic1)*rsqrtinv(nn,mm)*radptr%pred(nx,mm)
                       val=val+rpred(nx,ic1)*rsqrtinv(nn,mm)*radptr%pred(nx,mm)
                    end do
                 else
                    val2=val2+spred(nx,ic)*radptr%pred(nx,nn)
                    val =val +rpred(nx,ic)*radptr%pred(nx,nn)
                 end if
              end do
 
!             contribution from atmosphere
              do k=1,nsigradjac
                 val2=val2+tdir(k)*radptr%dtb_dvar(k,nn)
                 val =val +rdir(k)*radptr%dtb_dvar(k,nn)
              end do

!             calculate radiances for each stepsize
              do kk=1,nstep
                 rad(kk)=val2+sges(kk)*val
              end do
           else
              rad(kk)= val2
           end if
        
!          calculate contribution to J
           do kk=1,max(1,nstep)
              term(kk)  = radptr%err2(nn)*rad(kk)*rad(kk)
           end do

!          Modify penalty term if nonlinear QC
           if(nlnqc_iter .and. pg_rad(ic) > tiny_r_kind .and. &
                               b_rad(ic)  > tiny_r_kind)then
              cg_rad=cg_term/b_rad(ic)
              wnotgross= one-pg_rad(ic)*varqc_iter
              wgross = varqc_iter*pg_rad(ic)*cg_rad/wnotgross
              do kk=1,max(1,nstep)
                 term(kk)  = -two*log((exp(-half*term(kk) ) + wgross)/(one+wgross))
              end do
           endif

           out(1) = out(1) + term(1)*radptr%raterr2(nn)
           do kk=2,nstep
              out(kk) = out(kk) + (term(kk)-term(1))*radptr%raterr2(nn)
           end do

        end do
        if (radptr%use_corr_obs) deallocate(rsqrtinv)
     end if  !luse

     radptr => radptr%llpoint
  end do
  return
end subroutine stprad

end module stpradmod
