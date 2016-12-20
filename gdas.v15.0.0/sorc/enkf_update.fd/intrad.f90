module intradmod

!$$$ module documentation block
!                .      .    .                                       .
! module:    intradmod    module for intrad and its tangent linear intrad_tl
!   prgmmr:
!
! abstract: module for intrad and its tangent linear intrad_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intrad and its tangent linear intrad_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intrad_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2011-05-17  todling - add internal routine set_
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off and
!                         add threading
!
! subroutines included:
!   sub intrad_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind
implicit none

PRIVATE
PUBLIC intrad,setrad
PUBLIC itv,iqv,ioz,icw,ius,ivs,isst,iqg,iqh,iqi,iql,iqr,iqs,lgoback
PUBLIC luseu,lusev,luset,luseq,lusecw,luseoz,luseqg,luseqh,luseqi,luseql, &
       luseqr,luseqs,lusesst

interface intrad; module procedure &
          intrad_
end interface

integer(i_kind) :: itv,iqv,ioz,icw,ius,ivs,isst
integer(i_kind) :: iqg,iqh,iqi,iql,iqr,iqs
logical :: done_setting = .false.
logical :: lgoback
logical luseu,lusev,luset,luseq,lusecw,luseoz,luseqg,luseqh,luseqi,luseql, &
        luseqr,luseqs,lusesst

contains

subroutine setrad(sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_  sat radiance operator setting
!   prgmmr: todling          org: np22                date: 2011-05-18
!
! abstract: sets parameters required for intrad.
!           This routine is NEVER to be make public.
!
! program history log:
!   2011-05-18  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use radinfo, only: radjacnames,radjacindxs,nsigradjac
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_get
  use mpeu_util, only: getindex
  implicit none

! Declare passed variables
  type(gsi_bundle), intent(in   ) :: sval

! Declare local variables
  integer(i_kind) ier,istatus,indx
  logical         look

  real(r_kind),pointer,dimension(:) :: st,sq,scw,soz,su,sv,sqg,sqh,sqi,sql,sqr,sqs
  real(r_kind),pointer,dimension(:) :: sst

  if(done_setting) return

! Retrieve pointers; return when not found (except in case of non-essentials)
  ier=0; itv=0; iqv=0; ius=0; ivs=0; isst=0; ioz=0; icw=0
  iqg=0; iqh=0; iqi=0; iql=0; iqr=0; iqs=0
  call gsi_bundlegetpointer(sval,'u',  su, istatus);ius=istatus+ius
  call gsi_bundlegetpointer(sval,'v',  sv, istatus);ivs=istatus+ivs
  call gsi_bundlegetpointer(sval,'tv' ,st, istatus);itv=istatus+itv
  call gsi_bundlegetpointer(sval,'q',  sq, istatus);iqv=istatus+iqv
  call gsi_bundlegetpointer(sval,'cw' ,scw,istatus);icw=istatus+icw
  call gsi_bundlegetpointer(sval,'oz' ,soz,istatus);ioz=istatus+ioz
  call gsi_bundlegetpointer(sval,'sst',sst,istatus);isst=istatus+isst
  call gsi_bundlegetpointer(sval,'qg' ,sqg,istatus);iqg=istatus+iqg
  call gsi_bundlegetpointer(sval,'qh' ,sqh,istatus);iqh=istatus+iqh
  call gsi_bundlegetpointer(sval,'qi' ,sqi,istatus);iqi=istatus+iqi
  call gsi_bundlegetpointer(sval,'ql' ,sql,istatus);iql=istatus+iql
  call gsi_bundlegetpointer(sval,'qr' ,sqr,istatus);iqr=istatus+iqr
  call gsi_bundlegetpointer(sval,'qs' ,sqs,istatus);iqs=istatus+iqs
  lgoback=(ius/=0).and.(ivs/=0).and.(itv/=0).and.(iqv/=0).and.(ioz/=0).and.(icw/=0).and.(isst/=0)
  lgoback=lgoback .and.(iqg/=0).and.(iqh/=0).and.(iqi/=0).and.(iql/=0).and.(iqr/=0).and.(iqs/=0)
  if(lgoback)return

! check to see if variable participates in forward operator
! tv
  indx=getindex(radjacnames,'tv')
  look=(itv==0.and.indx>0)
  itv=-1
  if(look) itv=radjacindxs(indx)
! q
  indx=getindex(radjacnames,'q')
  look=(iqv==0.and.indx>0)
  iqv=-1
  if(look) iqv=radjacindxs(indx)
! oz
  indx=getindex(radjacnames,'oz')
  look=(ioz ==0.and.indx>0)
  ioz=-1
  if(look) ioz =radjacindxs(indx)
! cw
  indx=getindex(radjacnames,'cw')
  look=(icw ==0.and.indx>0)
  icw=-1
  if(look) icw =radjacindxs(indx)
! sst
  indx=getindex(radjacnames,'sst')
  look=(isst==0.and.indx>0)
  isst=-1
  if(look) isst=radjacindxs(indx)
! us & vs
  indx=getindex(radjacnames,'u')
  look=(ius==0.and.indx>0)
  ius=-1
  if(look) ius=radjacindxs(indx)
  indx=getindex(radjacnames,'v')
  look=(ivs==0.and.indx>0)
  ivs=-1
  if(look) ivs=radjacindxs(indx)
! qg
  indx=getindex(radjacnames,'qg')
  look=(iqg ==0.and.indx>0)
  iqg=-1
  if(look) iqg =radjacindxs(indx)
! qh
  indx=getindex(radjacnames,'qh')
  look=(iqh ==0.and.indx>0)
  iqh=-1
  if(look) iqh =radjacindxs(indx)
! qi
  indx=getindex(radjacnames,'qi')
  look=(iqi ==0.and.indx>0)
  iqi=-1
  if(look) iqi =radjacindxs(indx)
! ql
  indx=getindex(radjacnames,'ql')
  look=(iql ==0.and.indx>0)
  iql=-1
  if(look) iql =radjacindxs(indx)
! qr
  indx=getindex(radjacnames,'qr')
  look=(iqr ==0.and.indx>0)
  iqr=-1
  if(look) iqr =radjacindxs(indx)
! qs
  indx=getindex(radjacnames,'qs')
  look=(iqs ==0.and.indx>0)
  iqs=-1
  if(look) iqs =radjacindxs(indx)

  luseu=ius>=0
  lusev=ivs>=0
  luset=itv>=0
  luseq=iqv>=0
  luseoz=ioz>=0
  lusecw=icw>=0
  luseql=iql>=0
  luseqi=iqi>=0
  luseqh=iqh>=0
  luseqg=iqg>=0
  luseqr=iqr>=0
  luseqs=iqs>=0
  lusesst=isst>=0

  done_setting = .true.

  return
end subroutine setrad

subroutine intrad_(radhead,rval,sval,rpred,spred)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrad      sat radiance nonlin qc obs operator
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: apply satellite radiance operator and adjoint with
!            addition of nonlinear qc operator.
!
! program history log:
!   1990-10-11  parrish
!   1992-07-21
!   1995-07-17  derber
!   1997-03-10  wu
!   1997-12-22  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-01-20  okamoto - add wind components
!   2005-04-11  treadon - merge intrad and intrad_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2006-04-03  derber  - clean up code
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-03-19  tremolet - binning of observations
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-05-31  safford - rm unused vars and uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!   2008-10-10  derber  - flip indices for spred and rpred
!   2008-11-28  todling  - remove quad precision; mpi_allgather is reproducible
!                        - turn FOTO optional; changed ptr%time handle
!                        - internal copy of pred's to avoid reshape in calling program
!   2010-03-25  zhu - use state_vector in the interface for generalizing control variable
!                   - add treatment when sst and oz are not control variables
!                   - add pointer_state
!   2010-05-05  derber - omp commands removed
!   2010-05-13  todling - update to use gsi_bundle; 
!                       - on-the-spot handling of non-essential vars
!   2011-05-04  todling - merge in Min-Jeong Kim's cloud clear assimilation (connect to Metguess)
!   2011-05-16  todling - generalize entries in radiance jacobian
!   2011-05-17  auligne/todling - add hydrometeors
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2015-04-01  W. Gu   - scale the bias correction term to handle the
!                       - inter-channel correlated obs errors.
!   2016-07-19  kbathmann - move decomposition of correlated R to outer loop.
!
!   input argument list:
!     radhead  - obs type pointer to obs structure
!     st       - input temperature correction field
!     sq       - input q correction field
!     soz      - input ozone correction field
!     su       - input u correction field
!     sv       - input v correction field
!     spred    - input predictor values
!     sst      - input skin temp. vector
!     rt
!     rq
!     roz
!     ru
!     rv
!     rpred
!     rst
!
!   output argument list:
!     rt       - output t vector after inclusion of radiance info.
!     rq       - output q vector after inclusion of radiance info.
!     roz      - output ozone vector after inclusion of radiance info.
!     ru       - output u vector after inclusion of radiance info.
!     rv       - output v vector after inclusion of radiance info.
!     rpred    - output predictor vector after inclusion of radiance info.
!     rst      - output skin temp. vector after inclusion of radiance info.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use radinfo, only: npred,jpch_rad,pg_rad,b_rad
  use radinfo, only: radjacnames,radjacindxs,nsigradjac
  use obsmod, only: rad_ob_type,lsaveobsens,l_do_adjoint,luse_obsdiag
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  use gridmod, only: latlon11,latlon1n,nsig
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,tiny_r_kind,cg_term,r3600
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_get
  use mpeu_util, only: getindex
  use gsi_4dvar, only: ladtest_obs


  implicit none

! Declare passed variables
  type(rad_ob_type),pointer,intent(in) :: radhead
  type(gsi_bundle), intent(in   ) :: sval
  type(gsi_bundle), intent(inout) :: rval
  real(r_kind),dimension(npred*jpch_rad),intent(in   ) :: spred
  real(r_quad),dimension(npred*jpch_rad),intent(inout) :: rpred

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,i1,i2,i3,i4,n,k,ic,ix,nn,mm
  integer(i_kind) ier,istatus
  integer(i_kind),dimension(nsig) :: i1n,i2n,i3n,i4n
  real(r_kind),allocatable,dimension(:):: val
  real(r_kind) w1,w2,w3,w4
  real(r_kind),dimension(nsigradjac):: tval,tdir
  real(r_kind) cg_rad,p0,wnotgross,wgross,time_rad
  type(rad_ob_type), pointer :: radptr
  real(r_kind), dimension(:,:), allocatable:: rsqrtinv
  integer(i_kind) :: ic1,ix1
  integer(i_kind) :: chan_count, ii, jj
  real(r_kind),pointer,dimension(:) :: st,sq,scw,soz,su,sv,sqg,sqh,sqi,sql,sqr,sqs
  real(r_kind),pointer,dimension(:) :: sst
  real(r_kind),pointer,dimension(:) :: rt,rq,rcw,roz,ru,rv,rqg,rqh,rqi,rql,rqr,rqs
  real(r_kind),pointer,dimension(:) :: rst
  real(r_kind),pointer,dimension(:) :: xhat_dt_t,xhat_dt_q,xhat_dt_oz,xhat_dt_u,xhat_dt_v
  real(r_kind),pointer,dimension(:) :: dhat_dt_t,dhat_dt_q,dhat_dt_oz,dhat_dt_u,dhat_dt_v

!  If no rad observations return
  if(.not.associated(radhead)) return
! Set required parameters
  if(lgoback) return


! Retrieve pointers; return when not found (except in case of non-essentials)
  ier=0
  if(luseu)then
    call gsi_bundlegetpointer(sval,'u',  su, istatus)
    call gsi_bundlegetpointer(rval,'u',  ru, istatus)
  end if
  if(lusev)then
    call gsi_bundlegetpointer(sval,'v',  sv, istatus)
    call gsi_bundlegetpointer(rval,'v',  rv, istatus)
  end if
  if(luset)then
    call gsi_bundlegetpointer(sval,'tv' ,st, istatus)
    call gsi_bundlegetpointer(rval,'tv' ,rt, istatus)
  end if
  if(luseq)then
    call gsi_bundlegetpointer(sval,'q',  sq, istatus)
    call gsi_bundlegetpointer(rval,'q',  rq, istatus)
  end if
  if(lusecw)then
    call gsi_bundlegetpointer(sval,'cw' ,scw,istatus)
    call gsi_bundlegetpointer(rval,'cw' ,rcw,istatus)
  end if
  if(luseoz)then
    call gsi_bundlegetpointer(sval,'oz' ,soz,istatus)
    call gsi_bundlegetpointer(rval,'oz' ,roz,istatus)
  end if
  if(lusesst)then
    call gsi_bundlegetpointer(sval,'sst',sst,istatus)
    call gsi_bundlegetpointer(rval,'sst',rst,istatus)
  end if
  if(luseqg)then
    call gsi_bundlegetpointer(sval,'qg' ,sqg,istatus)
    call gsi_bundlegetpointer(rval,'qg' ,rqg,istatus)
  end if
  if(luseqh)then
    call gsi_bundlegetpointer(sval,'qh' ,sqh,istatus)
    call gsi_bundlegetpointer(rval,'qh' ,rqh,istatus)
  end if
  if(luseqi)then
    call gsi_bundlegetpointer(sval,'qi' ,sqi,istatus)
    call gsi_bundlegetpointer(rval,'qi' ,rqi,istatus)
  end if
  if(luseql)then
    call gsi_bundlegetpointer(sval,'ql' ,sql,istatus)
    call gsi_bundlegetpointer(rval,'ql' ,rql,istatus)
  end if
  if(luseqr)then
    call gsi_bundlegetpointer(sval,'qr' ,sqr,istatus)
    call gsi_bundlegetpointer(rval,'qr' ,rqr,istatus)
  end if
  if(luseqs)then
    call gsi_bundlegetpointer(sval,'qs' ,sqs,istatus)
    call gsi_bundlegetpointer(rval,'qs' ,rqs,istatus)
  end if

  if(l_foto) then
     ier=0
     if(luseu)then
       call gsi_bundlegetpointer(xhat_dt,'u',  xhat_dt_u, istatus);ier=istatus+ier
       call gsi_bundlegetpointer(dhat_dt,'u',  dhat_dt_u, istatus);ier=istatus+ier
     end if
     if(lusev)then
       call gsi_bundlegetpointer(xhat_dt,'v',  xhat_dt_v, istatus);ier=istatus+ier
       call gsi_bundlegetpointer(dhat_dt,'v',  dhat_dt_v, istatus);ier=istatus+ier
     end if
     if(luset)then
       call gsi_bundlegetpointer(xhat_dt,'tv' ,xhat_dt_t, istatus);ier=istatus+ier
       call gsi_bundlegetpointer(dhat_dt,'tv' ,dhat_dt_t, istatus);ier=istatus+ier
     end if
     if(luseq)then
       call gsi_bundlegetpointer(xhat_dt,'q',  xhat_dt_q, istatus);ier=istatus+ier
       call gsi_bundlegetpointer(dhat_dt,'q',  dhat_dt_q, istatus);ier=istatus+ier
     end if
     if(luseoz)then
       call gsi_bundlegetpointer(xhat_dt,'oz' ,xhat_dt_oz,istatus);ier=istatus+ier
       call gsi_bundlegetpointer(dhat_dt,'oz' ,dhat_dt_oz,istatus);ier=istatus+ier
     end if

     if(ier/=0)return
  endif

  radptr => radhead
  do while (associated(radptr))
     j1=radptr%ij(1)
     j2=radptr%ij(2)
     j3=radptr%ij(3)
     j4=radptr%ij(4)
     w1=radptr%wij(1)
     w2=radptr%wij(2)
     w3=radptr%wij(3)
     w4=radptr%wij(4)
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
!  Begin Forward model
!  calculate temperature, q, ozone, sst vector at observation location
     i1n(1) = j1
     i2n(1) = j2
     i3n(1) = j3
     i4n(1) = j4
     do k=2,nsig
        i1n(k) = i1n(k-1)+latlon11
        i2n(k) = i2n(k-1)+latlon11
        i3n(k) = i3n(k-1)+latlon11
        i4n(k) = i4n(k-1)+latlon11
     enddo

     do k=1,nsig
        i1 = i1n(k)
        i2 = i2n(k)
        i3 = i3n(k)
        i4 = i4n(k)
        if(luset)then
           tdir(itv+k)=  w1*  st(i1)+w2*  st(i2)+ &
                         w3*  st(i3)+w4*  st(i4)
        endif
        if(luseq)then
           tdir(iqv+k)= w1*  sq(i1)+w2*  sq(i2)+ &
                        w3*  sq(i3)+w4*  sq(i4)
        endif
        if(luseoz)then
          tdir(ioz+k)=w1* soz(i1)+w2* soz(i2)+ &
                      w3* soz(i3)+w4* soz(i4)
        end if
        if(lusecw)then
           tdir(icw+k)=w1* scw(i1)+w2* scw(i2)+ &
                       w3* scw(i3)+w4* scw(i4)
        end if
        if(luseql)then
           tdir(iql+k)=w1* sql(i1)+w2* sql(i2)+ &
                       w3* sql(i3)+w4* sql(i4)
        end if
        if(luseqi)then
           tdir(iqi+k)=w1* sqi(i1)+w2* sqi(i2)+ &
                       w3* sqi(i3)+w4* sqi(i4)
        end if
        if(luseqh)then
           tdir(iqh+k)=w1* sqh(i1)+w2* sqh(i2)+ &
                       w3* sqh(i3)+w4* sqh(i4)
        end if
        if(luseqg)then
           tdir(iqg+k)=w1* sqg(i1)+w2* sqg(i2)+ &
                       w3* sqg(i3)+w4* sqg(i4)
        end if
        if(luseqr)then
           tdir(iqr+k)=w1* sqr(i1)+w2* sqr(i2)+ &
                       w3* sqr(i3)+w4* sqr(i4)
        end if
        if(luseqs)then
           tdir(iqs+k)=w1* sqs(i1)+w2* sqs(i2)+ &
                       w3* sqs(i3)+w4* sqs(i4)
        end if
     end do
     if(luseu)then
        tdir(ius+1)=   w1* su(j1) +w2* su(j2)+ &
                       w3* su(j3) +w4* su(j4)
     endif
     if(lusev)then
        tdir(ivs+1)=   w1* sv(j1) +w2* sv(j2)+ &
                       w3* sv(j3) +w4* sv(j4)
     endif
     if(lusesst)then
        tdir(isst+1)=w1*sst(j1) +w2*sst(j2)+ &
                           w3*sst(j3) +w4*sst(j4)
     end if



     if (l_foto) then
        time_rad=radptr%time*r3600
        do k=1,nsig
           i1 = i1n(k)
           i2 = i2n(k)
           i3 = i3n(k)
           i4 = i4n(k)
           if(luset)then 
              tdir(itv+k)= tdir(itv+k)+&
                           (w1* xhat_dt_t(i1)+w2*xhat_dt_t(i2)+ &
                            w3* xhat_dt_t(i3)+w4*xhat_dt_t(i4))*time_rad
           endif
           if(luseq)then 
              tdir(iqv+k)= tdir(iqv+k)+&
                           (w1* xhat_dt_q(i1)+w2*xhat_dt_q(i2)+ &
                            w3* xhat_dt_q(i3)+w4*xhat_dt_q(i4))*time_rad
           endif
           if(luseoz)then 
              tdir(ioz+k)= tdir(ioz+k)+&
                          (w1*xhat_dt_oz(i1)+w2*xhat_dt_oz(i2)+ &
                           w3*xhat_dt_oz(i3)+w4*xhat_dt_oz(i4))*time_rad
           endif
        end do
        if(luseu)then 
           tdir(ius+1)=   tdir(ius+1)+&
                          (w1*xhat_dt_u(j1) +w2*xhat_dt_u(j2)+ &
                           w3*xhat_dt_u(j3) +w4*xhat_dt_u(j4))*time_rad
        endif
        if(lusev)then 
           tdir(ivs+1)=   tdir(ivs+1)+&
                          (w1*xhat_dt_v(j1) +w2*xhat_dt_v(j2)+ &
                           w3*xhat_dt_v(j3) +w4*xhat_dt_v(j4))*time_rad
        endif
 
     endif

!  For all other configurations
!  begin channel specific calculations
     allocate(val(radptr%nchan))
     do nn=1,radptr%nchan
        ic=radptr%icx(nn)
        ix=(ic-1)*npred

!       include observation increment and lapse rate contributions to bias correction
        val(nn)=zero

!       Include contributions from atmospheric jacobian
        do k=1,nsigradjac
           val(nn)=val(nn)+tdir(k)*radptr%dtb_dvar(k,nn)
        end do

!       Include contributions from remaining bias correction terms
        if( .not. ladtest_obs) then
           if(radptr%use_corr_obs)then
              do n=1,npred
                 do mm=1,radptr%nchan
                    ic1=radptr%icx(mm)
                    ix1=(ic1-1)*npred
                    val(nn)=val(nn)+rsqrtinv(nn,mm)*spred(ix1+n)*radptr%pred(n,mm)
                 enddo
              enddo
           else
              do n=1,npred
                 val(nn)=val(nn)+spred(ix+n)*radptr%pred(n,nn)
              end do
           endif
        end if


        if(luse_obsdiag)then
           if (lsaveobsens) then
              val(nn) = val(nn)*radptr%err2(nn)*radptr%raterr2(nn)
              radptr%diags(nn)%ptr%obssen(jiter) = val(nn)
           else
              if (radptr%luse) radptr%diags(nn)%ptr%tldepart(jiter) = val(nn)
           endif
        endif

        if (l_do_adjoint) then
           if (.not. lsaveobsens) then
              if( .not. ladtest_obs)   val(nn)=val(nn)-radptr%res(nn)

!             Multiply by variance.
              if (nlnqc_iter .and. pg_rad(ic) > tiny_r_kind .and. &
                                   b_rad(ic)  > tiny_r_kind) then
                 cg_rad=cg_term/b_rad(ic)
                 wnotgross= one-pg_rad(ic)*varqc_iter
                 wgross = varqc_iter*pg_rad(ic)*cg_rad/wnotgross
                 p0   = wgross/(wgross+exp(-half*radptr%err2(nn)*val(nn)*val(nn)))
                 val(nn) = val(nn)*(one-p0)
              endif

              if(.not. ladtest_obs )val(nn) = val(nn)*radptr%err2(nn)*radptr%raterr2(nn)
           endif

!          Extract contributions from bias correction terms
!          use compensated summation
           if( .not. ladtest_obs) then
              if(radptr%luse)then
                 if(radptr%use_corr_obs)then
                    do n=1,npred
                       do mm=1,radptr%nchan
                          ic1=radptr%icx(mm)
                          ix1=(ic1-1)*npred
                          rpred(ix1+n)=rpred(ix1+n)+rsqrtinv(nn,mm)*radptr%pred(n,mm)*val(nn)
                       enddo
                    enddo
                 else
                    do n=1,npred
                       rpred(ix+n)=rpred(ix+n)+radptr%pred(n,nn)*val(nn)
                    end do
                 end if
              end if
           end if ! not ladtest_obs
        end if
     end do

!          Begin adjoint
     if (radptr%use_corr_obs) deallocate(rsqrtinv)
     if (l_do_adjoint) then
        do k=1,nsigradjac
           tval(k)=zero

           do nn=1,radptr%nchan
!          Extract contributions from atmospheric jacobian
              tval(k)=tval(k)+radptr%dtb_dvar(k,nn)*val(nn)
           end do

        end do

!    Distribute adjoint contributions over surrounding grid points
 
        if(luseu) then
           ru(j1)=ru(j1)+w1*tval(ius+1)
           ru(j2)=ru(j2)+w2*tval(ius+1)
           ru(j3)=ru(j3)+w3*tval(ius+1)
           ru(j4)=ru(j4)+w4*tval(ius+1)
        endif
        if(lusev) then
           rv(j1)=rv(j1)+w1*tval(ivs+1)
           rv(j2)=rv(j2)+w2*tval(ivs+1)
           rv(j3)=rv(j3)+w3*tval(ivs+1)
           rv(j4)=rv(j4)+w4*tval(ivs+1)
        endif

        if (lusesst) then
           rst(j1)=rst(j1)+w1*tval(isst+1)
           rst(j2)=rst(j2)+w2*tval(isst+1)
           rst(j3)=rst(j3)+w3*tval(isst+1)
           rst(j4)=rst(j4)+w4*tval(isst+1)
        end if
        do k=1,nsig
           i1 = i1n(k)
           i2 = i2n(k)
           i3 = i3n(k)
           i4 = i4n(k)

 
           if(luset)then
              mm=itv+k
              rt(i1)=rt(i1)+w1*tval(mm)
              rt(i2)=rt(i2)+w2*tval(mm)
              rt(i3)=rt(i3)+w3*tval(mm)
              rt(i4)=rt(i4)+w4*tval(mm)
           endif
           if(luseq)then
              mm=iqv+k
              rq(i1)=rq(i1)+w1*tval(mm)
              rq(i2)=rq(i2)+w2*tval(mm)
              rq(i3)=rq(i3)+w3*tval(mm)
              rq(i4)=rq(i4)+w4*tval(mm)
           endif
           if(luseoz)then
              mm=ioz+k
              roz(i1)=roz(i1)+w1*tval(mm)
              roz(i2)=roz(i2)+w2*tval(mm)
              roz(i3)=roz(i3)+w3*tval(mm)
              roz(i4)=roz(i4)+w4*tval(mm)
           end if
           if(lusecw)then
              mm=icw+k
              rcw(i1)=rcw(i1)+w1*tval(mm)
              rcw(i2)=rcw(i2)+w2*tval(mm)
              rcw(i3)=rcw(i3)+w3*tval(mm)
              rcw(i4)=rcw(i4)+w4*tval(mm)
           end if
           if(luseqg)then
              mm=iqg+k
              rqg(i1)=rqg(i1)+w1*tval(mm)
              rqg(i2)=rqg(i2)+w2*tval(mm)
              rqg(i3)=rqg(i3)+w3*tval(mm)
              rqg(i4)=rqg(i4)+w4*tval(mm)
           end if
           if(luseqh)then
              mm=iqh+k
              rqh(i1)=rqh(i1)+w1*tval(mm)
              rqh(i2)=rqh(i2)+w2*tval(mm)
              rqh(i3)=rqh(i3)+w3*tval(mm)
              rqh(i4)=rqh(i4)+w4*tval(mm)
           end if
           if(luseqi)then
              mm=iqi+k
              rqi(i1)=rqi(i1)+w1*tval(mm)
              rqi(i2)=rqi(i2)+w2*tval(mm)
              rqi(i3)=rqi(i3)+w3*tval(mm)
              rqi(i4)=rqi(i4)+w4*tval(mm)
           end if
           if(luseql)then
              mm=iql+k
              rql(i1)=rql(i1)+w1*tval(mm)
              rql(i2)=rql(i2)+w2*tval(mm)
              rql(i3)=rql(i3)+w3*tval(mm)
              rql(i4)=rql(i4)+w4*tval(mm)
           end if
           if(luseqr)then
              mm=iqr+k
              rqr(i1)=rqr(i1)+w1*tval(mm)
              rqr(i2)=rqr(i2)+w2*tval(mm)
              rqr(i3)=rqr(i3)+w3*tval(mm)
              rqr(i4)=rqr(i4)+w4*tval(mm)
           end if
           if(luseqs)then
              mm=iqs+k
              rqs(i1)=rqs(i1)+w1*tval(mm)
              rqs(i2)=rqs(i2)+w2*tval(mm)
              rqs(i3)=rqs(i3)+w3*tval(mm)
              rqs(i4)=rqs(i4)+w4*tval(mm)
           end if
        end do
        if (l_foto) then
           if(luseu) then
              dhat_dt_u(j1)=dhat_dt_u(j1)+w1*tval(ius+1)*time_rad
              dhat_dt_u(j2)=dhat_dt_u(j2)+w2*tval(ius+1)*time_rad
              dhat_dt_u(j3)=dhat_dt_u(j3)+w3*tval(ius+1)*time_rad
              dhat_dt_u(j4)=dhat_dt_u(j4)+w4*tval(ius+1)*time_rad
           endif
           if(lusev) then
              dhat_dt_v(j1)=dhat_dt_v(j1)+w1*tval(ivs+1)*time_rad
              dhat_dt_v(j2)=dhat_dt_v(j2)+w2*tval(ivs+1)*time_rad
              dhat_dt_v(j3)=dhat_dt_v(j3)+w3*tval(ivs+1)*time_rad
              dhat_dt_v(j4)=dhat_dt_v(j4)+w4*tval(ivs+1)*time_rad
           endif
           do k=1,nsig
              i1 = i1n(k)
              i2 = i2n(k)
              i3 = i3n(k)
              i4 = i4n(k)
              if(luset)then
                 mm=itv+k
                 dhat_dt_t(i1)=dhat_dt_t(i1)+w1*tval(mm)*time_rad
                 dhat_dt_t(i2)=dhat_dt_t(i2)+w2*tval(mm)*time_rad
                 dhat_dt_t(i3)=dhat_dt_t(i3)+w3*tval(mm)*time_rad
                 dhat_dt_t(i4)=dhat_dt_t(i4)+w4*tval(mm)*time_rad
              endif
              if(luseq)then
                 mm=iqv+k
                 dhat_dt_q(i1)=dhat_dt_q(i1)+w1*tval(mm)*time_rad
                 dhat_dt_q(i2)=dhat_dt_q(i2)+w2*tval(mm)*time_rad
                 dhat_dt_q(i3)=dhat_dt_q(i3)+w3*tval(mm)*time_rad
                 dhat_dt_q(i4)=dhat_dt_q(i4)+w4*tval(mm)*time_rad
              endif
              if(luseoz)then
                 mm=ioz+k
                 dhat_dt_oz(i1)=dhat_dt_oz(i1)+w1*tval(mm)*time_rad
                 dhat_dt_oz(i2)=dhat_dt_oz(i2)+w2*tval(mm)*time_rad
                 dhat_dt_oz(i3)=dhat_dt_oz(i3)+w3*tval(mm)*time_rad
                 dhat_dt_oz(i4)=dhat_dt_oz(i4)+w4*tval(mm)*time_rad
              end if
           end do
        endif

     endif ! < l_do_adjoint >
     deallocate(val)

     radptr => radptr%llpoint
  end do


  return
end subroutine intrad_

end module intradmod
