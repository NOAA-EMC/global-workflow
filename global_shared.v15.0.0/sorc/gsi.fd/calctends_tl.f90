subroutine calctends_tl(fields,fields_dt,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calctends_tl       tlm of calctends
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: TLM of routine that compute tendencies for pressure, Tv, u, v 
!
! program history log:
!   2005-09-29  kleist
!   2005-10-17  kleist - changes to improve computational efficiency
!   2005-11-21  kleist - add tracer tendencies, use new module
!   2006-04-12  treadon - replace sigi with bk5
!   2006-04-21  kleist - add divergence tendency parts
!   2006-07-31  kleist - changes to use ps instead of ln(ps)
!   2006-09-21  kleist - add rescaling to divergence tendency formulation
!   2006-10-04  rancic - correct bug in tracer advection terms
!   2007-04-16  kleist - move constraint specific items elsewhere
!   2007-05-08  kleist - add bits for fully generalized vertical coordinate
!   2007-06-21  rancic - add pbl 
!   2007-07-26  cucurull - add 3d pressure pri in argument list ;
!                          move getprs_tl outside calctends_tl;
!                          call getprs_horiz_tl;
!                          remove ps from argument list
!   2007-08-08  derber - optimize
!   2008-06-05  safford - rm unused uses
!   2009-08-20  parrish - replace curvfct with curvx, curvy.  this allows tendency computation to
!                          work for any general orthogonal coordinate.
!   2010-11-03  derber - moved threading calculations to gridmod and modified
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!   2011-12-02  zhu     - add safe-guard for the case when there is no entry in the metguess table
!   2012-03-11  tong    - added condition to calcuate cw_t
!   2013-10-19  todling - revamp interface (pass all in bundles); derivatives and
!                         guess fields also in bundles
!   2013-10-28  todling - rename p3d to prse
!   2014-06-05  eliu    - move location to get cw index(icw) from sv table; add condition
!                         to get pointer for cw 
!
! usage:
!   input argument list:
!     fields    - bundle holding relevant fields
!     mype      - task id
!
!   output argument list:
!     fields_dt - bundle holding related time tendencies
!
!   notes:
!     TLM check performed & verified on 2005-09-29 by d. kleist
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,istart,nlat,idvc5,bk5,&
      wrf_nmm_regional,nems_nmmb_regional,eta2_ll,regional,nthreads,jtstart,jtstop
  use constants, only: zero,half,two,rd,rcp
  use derivsmod, only: cwgues
  use tendsmod, only: what9,prsth9,r_prsum9,prdif9,r_prdif9,pr_xsum9,pr_xdif9,&
      pr_ysum9,pr_ydif9,curvx,curvy,coriolis
  use guess_grids, only: ntguessig,ges_teta,ges_prsi
  use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundledup
  use gsi_bundlemod, only: gsi_bundleinquire
  use gsi_bundlemod, only: gsi_bundledestroy
  use mpeu_util, only: die, getindex
  use derivsmod, only: gsi_xderivative_bundle
  use derivsmod, only: gsi_yderivative_bundle
  implicit none

! Declare passed variables
  type(gsi_bundle) :: fields
  type(gsi_bundle) :: fields_dt
  integer(i_kind),intent(in) :: mype

! Declare local variables
  character(len=*),parameter::myname='calctends_tl'
  real(r_kind),dimension(:,:,:),pointer :: u_t,v_t,t_t,q_t,oz_t,cw_t
  real(r_kind),dimension(:,:,:),pointer :: p_t
  real(r_kind),dimension(:,:,:),pointer :: pri
  real(r_kind),dimension(:,:,:),pointer :: u,v,t,q,oz,cw
  real(r_kind),dimension(:,:,:),pointer :: u_x,u_y,v_x,v_y,t_x,t_y
  real(r_kind),dimension(:,:,:),pointer :: q_x,q_y,oz_x,oz_y,cw_x,cw_y
  real(r_kind),dimension(:,:)  ,pointer :: ps_x,ps_y
  real(r_kind),dimension(:,:,:),pointer :: pri_x,pri_y
  real(r_kind),dimension(lat2,lon2,nsig+1):: prsth,what
  real(r_kind),dimension(lat2,lon2,nsig):: prsum,prdif,pr_xsum,pr_xdif,&
       pr_ysum,pr_ydif
  real(r_kind),dimension(lat2,lon2):: sumkm1,sumvkm1,sum2km1,sum2vkm1
  real(r_kind),dimension(lat2,lon2,nsig):: t_thor9

  real(r_kind) tmp,tmp2,tmp3,sumk,sumvk,sum2k,sum2vk,uduvdv
  integer(i_kind) i,j,k,ix,it,kk,istatus,n_actual_clouds,ier,icw
  logical ihave_xtra_derivatives
  character(len=10),allocatable,dimension(:)::fvars2d,fvars3d

  type(gsi_bundle) :: xderivative
  type(gsi_bundle) :: yderivative

  real(r_kind),pointer,dimension(:,:,:) :: ges_u=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_v=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_tv=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_q =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_oz=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_cwmr=>NULL()

  real(r_kind),pointer,dimension(:,:,:) :: ges_u_lon=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_v_lon=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_tv_lon=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_q_lon =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_oz_lon=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_cw_lon=>NULL()

  real(r_kind),pointer,dimension(:,:,:) :: ges_u_lat=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_v_lat=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_tv_lat=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_q_lat =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_oz_lat=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_cw_lat=>NULL()

! linearized about guess solution, so set it flag accordingly
  it=ntguessig

  if(fields%n2d>0) allocate(fvars2d(fields%n2d))
  if(fields%n3d>0) allocate(fvars3d(fields%n3d))
  call gsi_bundleinquire (fields,'shortnames::2d',fvars2d,istatus)
  call gsi_bundleinquire (fields,'shortnames::3d',fvars3d,istatus)
  icw=getindex(fvars3d,'cw')

  ier=0
  call gsi_bundlegetpointer(fields,'u',   u,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields,'v',   v,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields,'tv',  t,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields,'q',   q,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields,'oz' , oz,  istatus);ier=istatus+ier
  if (icw>0) &  
  call gsi_bundlegetpointer(fields,'cw' , cw,  istatus);ier=istatus+ier 
  call gsi_bundlegetpointer(fields,'prse',pri, istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname,': pointers not found on input, ier=', ier
     call stop2(999)
  endif

  ier=0
  call gsi_bundlegetpointer(fields_dt,'u',   u_t, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields_dt,'v',   v_t, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields_dt,'tv',  t_t, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields_dt,'q',   q_t, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields_dt,'oz' , oz_t,istatus);ier=istatus+ier
  if (icw>0) &
  call gsi_bundlegetpointer(fields_dt,'cw' , cw_t,istatus);ier=istatus+ier       
  call gsi_bundlegetpointer(fields_dt,'prse',p_t ,istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname,': pointers not found on tendency, ier=', ier
     call stop2(999)
  endif

  ier=0
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'u' ,ges_u, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'v' ,ges_v, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tv',ges_tv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'q', ges_q ,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'oz',ges_oz,istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname, ': pointers not found in met-guess, ier=', ier
     call stop2(999)
  endif

  ier=0
  call gsi_bundlegetpointer(gsi_xderivative_bundle(it),'u' ,ges_u_lon, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_xderivative_bundle(it),'v' ,ges_v_lon, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_xderivative_bundle(it),'tv',ges_tv_lon,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_xderivative_bundle(it),'q', ges_q_lon ,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_xderivative_bundle(it),'oz',ges_oz_lon,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_xderivative_bundle(it),'cw',ges_cw_lon,istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname, ': pointers not found in lon-derivatives, ier=', ier
     call stop2(999)
  endif

  ier=0
  call gsi_bundlegetpointer(gsi_yderivative_bundle(it),'u' ,ges_u_lat, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_yderivative_bundle(it),'v' ,ges_v_lat, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_yderivative_bundle(it),'tv',ges_tv_lat,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_yderivative_bundle(it),'q', ges_q_lat ,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_yderivative_bundle(it),'oz',ges_oz_lat,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_yderivative_bundle(it),'cw',ges_cw_lat,istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname, ': pointers not found in lat-derivatives, ier=', ier
     call stop2(999)
  endif


! Get pointer to could water mixing ratio
  call gsi_metguess_get('clouds::3d',n_actual_clouds,ier)
  if (n_actual_clouds>0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr,istatus)
     if (istatus/=0) then 
        if (regional) then 
           ges_cwmr => cwgues     ! temporily, revise after moist physics is ready
        else
           call die('setuppcp','cannot get pointer to cwmr, istatus =',istatus)
        end if
     end if
  else
     ges_cwmr => cwgues
  end if

! preliminaries:
  ihave_xtra_derivatives=.false. 
  ier=0
  call gsi_bundledup ( fields, xderivative, 'lon-derivatives', istatus )
  ier=ier+istatus
  call gsi_bundledup ( fields, yderivative, 'lat-derivatives', istatus )
  ier=ier+istatus
  if(ier==0) then
    ihave_xtra_derivatives=.true. 
  else
    call die(myname,': trouble creating temporary space for derivatives',ier)
  endif

  call get_derivatives( fields, xderivative, yderivative )

  call gsi_bundlegetpointer(xderivative,'ps'  , ps_x,istatus)
  call gsi_bundlegetpointer(xderivative,'u'   ,  u_x,istatus)
  call gsi_bundlegetpointer(xderivative,'v'   ,  v_x,istatus)
  call gsi_bundlegetpointer(xderivative,'tv'  ,  t_x,istatus)
  call gsi_bundlegetpointer(xderivative,'q'   ,  q_x,istatus)
  call gsi_bundlegetpointer(xderivative,'oz'  , oz_x,istatus)
  call gsi_bundlegetpointer(xderivative,'cw'  , cw_x,istatus)
  call gsi_bundlegetpointer(xderivative,'prse',pri_x,istatus)

  call gsi_bundlegetpointer(yderivative,'ps'  , ps_y,istatus)
  call gsi_bundlegetpointer(yderivative,'u'   ,  u_y,istatus)
  call gsi_bundlegetpointer(yderivative,'v'   ,  v_y,istatus)
  call gsi_bundlegetpointer(yderivative,'tv'  ,  t_y,istatus)
  call gsi_bundlegetpointer(yderivative,'q'   ,  q_y,istatus)
  call gsi_bundlegetpointer(yderivative,'oz'  , oz_y,istatus)
  call gsi_bundlegetpointer(yderivative,'cw'  , cw_y,istatus)
  call gsi_bundlegetpointer(yderivative,'prse',pri_y,istatus)

  call getprs_horiz_tl(ps_x,ps_y,pri,pri_x,pri_y)

!$omp parallel do private(i,j,k,kk,tmp,tmp2,uduvdv, &
!$omp                  tmp3,sumk,sumvk,sum2k,sum2vk,ix)
  do kk=1,nthreads

    do k=1,nsig
      do j=jtstart(kk),jtstop(kk) 
        do i=1,lat2
           prsum  (i,j,k)=pri  (i,j,k)+pri  (i,j,k+1)
           prdif  (i,j,k)=pri  (i,j,k)-pri  (i,j,k+1)
           pr_xsum(i,j,k)=pri_x(i,j,k)+pri_x(i,j,k+1)
           pr_xdif(i,j,k)=pri_x(i,j,k)-pri_x(i,j,k+1)
           pr_ysum(i,j,k)=pri_y(i,j,k)+pri_y(i,j,k+1)
           pr_ydif(i,j,k)=pri_y(i,j,k)-pri_y(i,j,k+1)
        end do
      end do
    end do

!   Compute horizontal part of tendency for 3d pressure

    do j=jtstart(kk),jtstop(kk) 
      do i=1,lat2
        prsth(i,j,nsig+1)=zero
      end do
    end do
    do k=nsig,1,-1
      do j=jtstart(kk),jtstop(kk) 
        do i=1,lat2
           prsth(i,j,k)=prsth(i,j,k+1) - ( u(i,j,k)*pr_xdif9(i,j,k) + &
              ges_u(i,j,k)*pr_xdif(i,j,k) + v(i,j,k)*pr_ydif9(i,j,k) + &
              ges_v(i,j,k)*pr_ydif(i,j,k) + &
              (u_x(i,j,k) + v_y(i,j,k))*(prdif9(i,j,k)) + &
              (ges_u_lon(i,j,k) + ges_v_lat(i,j,k))*(prdif(i,j,k)) )
        end do
      end do
    end do

!   Get horizontal part of temperature tendency for vertical velocity term

    do k=1,nsig
      do j=jtstart(kk),jtstop(kk) 
        do i=1,lat2
           tmp=rd*ges_tv(i,j,k)*r_prsum9(i,j,k)
           t_t(i,j,k)=-u(i,j,k)*ges_tv_lon(i,j,k) - ges_u(i,j,k)*t_x(i,j,k) - &
              v(i,j,k)*ges_tv_lat(i,j,k) - ges_v(i,j,k)*t_y(i,j,k) +          &
              tmp*rcp*( ges_u(i,j,k)*pr_xsum(i,j,k) + &
              u(i,j,k)*pr_xsum9(i,j,k) + &
              ges_v(i,j,k)*pr_ysum(i,j,k) + &
              v(i,j,k)*pr_ysum9(i,j,k) + &
              prsth(i,j,k) + prsth(i,j,k+1)) + &
              rcp*( ges_u(i,j,k)*pr_xsum9(i,j,k) + &
              ges_v(i,j,k)*pr_ysum9(i,j,k) + &
              prsth9(i,j,k)+prsth9(i,j,k+1) ) * &
              ( rd*t(i,j,k)*r_prsum9(i,j,k) - tmp*prsum(i,j,k)*r_prsum9(i,j,k) )
 
        end do
      end do
    end do

!   calculate vertical velocity term:  z(dp/dz) (zero at top/bottom interfaces)
!   if running global, and there is a c(k) coefficient, we call the vvel subroutine

    if ( (.not.regional) .AND. (idvc5==3)) then

!     Get horizontal part of temperature tendency for vertical velocity term

      do k=1,nsig
        do j=jtstart(kk),jtstop(kk) 
           do i=1,lat2
              tmp=rd*ges_tv(i,j,k)*r_prsum9(i,j,k)
  
              t_thor9(i,j,k)=-ges_u(i,j,k)*ges_tv_lon(i,j,k) - &
                   ges_v(i,j,k)*ges_tv_lat(i,j,k)
              t_thor9(i,j,k)=t_thor9(i,j,k) -tmp*rcp * ( ges_u(i,j,k)*pr_xsum9(i,j,k) + &
                 ges_v(i,j,k)*pr_ysum9(i,j,k) + &
                 prsth9(i,j,k) + prsth9(i,j,k+1) )
           end do
        end do
      end do
      call getvvel_tl(t,t_t,t_thor9,prsth,prdif,what,jtstart(kk),jtstop(kk))
    else
      if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=2,nsig
           do j=jtstart(kk),jtstop(kk) 
              do i=1,lat2
                 what(i,j,k)=prsth(i,j,k)-eta2_ll(k)*prsth(i,j,1)
              end do
           end do
        end do
      else
        do k=2,nsig
           do j=jtstart(kk),jtstop(kk) 
              do i=1,lat2
                 what(i,j,k)=prsth(i,j,k)-bk5    (k)*prsth(i,j,1)
              end do
           end do
        end do
      end if
    end if

!   top/bottom boundary condition:

    do j=jtstart(kk),jtstop(kk) 
      do i=1,lat2
        what(i,j,1)=zero
        what(i,j,nsig+1)=zero
      enddo
    enddo


!   load actual dp/dt

    do k=1,nsig+1
      do j=jtstart(kk),jtstop(kk) 
        do i=1,lat2
           p_t(i,j,k)=prsth(i,j,k)-what(i,j,k)
        end do
      end do
    end do

!   before big k loop, zero out the km1 summation arrays

    do j=jtstart(kk),jtstop(kk) 
      do i=1,lat2
        sumkm1  (i,j)=zero
        sum2km1 (i,j)=zero
        sumvkm1 (i,j)=zero
        sum2vkm1(i,j)=zero
      end do
    end do

!   Compute terms for tendencies of wind components & Temperature

    do k=1,nsig
      do j=jtstart(kk),jtstop(kk) 
        do i=1,lat2
           uduvdv=two*(ges_u(i,j,k)*u(i,j,k) + ges_v(i,j,k)*v(i,j,k))
           u_t(i,j,k)=-u(i,j,k)*ges_u_lon(i,j,k) - ges_u(i,j,k)*u_x(i,j,k) - &
              v(i,j,k)*ges_u_lat(i,j,k) - ges_v(i,j,k)*u_y(i,j,k) + &
              coriolis(i,j)*v(i,j,k) + curvx(i,j)*uduvdv
           v_t(i,j,k)=-u(i,j,k)*ges_v_lon(i,j,k) - ges_u(i,j,k)*v_x(i,j,k) - &
              v(i,j,k)*ges_v_lat(i,j,k) - ges_v(i,j,k)*v_y(i,j,k) - &
              coriolis(i,j)*u(i,j,k) + curvy(i,j)*uduvdv

           tmp=rd*ges_tv(i,j,k)*r_prsum9(i,j,k)
           tmp2=rd*t(i,j,k)*r_prsum9(i,j,k)

           u_t(i,j,k) = u_t(i,j,k)-tmp*( pr_xsum(i,j,k) - &
              (prsum(i,j,k)*pr_xsum9(i,j,k)*r_prsum9(i,j,k)) ) - &
              tmp2*pr_xsum9(i,j,k)
 
           v_t(i,j,k) = v_t(i,j,k)-tmp*( pr_ysum(i,j,k) - &
              (prsum(i,j,k)*pr_ysum9(i,j,k)*r_prsum9(i,j,k)) ) - &
              tmp2*pr_ysum9(i,j,k)

!     vertical flux terms

           if (k > 1) then
              tmp=half*what(i,j,k)*r_prdif9(i,j,k)
              tmp2=half*what9(i,j,k)*r_prdif9(i,j,k)

              u_t(i,j,k) = u_t(i,j,k) - tmp*(ges_u (i,j,k-1)-ges_u (i,j,k)) - &
                 tmp2*( (u(i,j,k-1)-u(i,j,k)) - (ges_u (i,j,k-1)-ges_u (i,j,k))* &
                 prdif(i,j,k)*r_prdif9(i,j,k))          
              v_t(i,j,k) = v_t(i,j,k) - tmp*(ges_v (i,j,k-1)-ges_v (i,j,k)) - &
                 tmp2*( (v(i,j,k-1)-v(i,j,k)) - (ges_v (i,j,k-1)-ges_v (i,j,k))* &
                 prdif(i,j,k)*r_prdif9(i,j,k))
              t_t(i,j,k) = t_t(i,j,k) - tmp*(ges_tv(i,j,k-1)-ges_tv(i,j,k)) - &
                 tmp2*( (t(i,j,k-1)-t(i,j,k)) - (ges_tv(i,j,k-1)-ges_tv(i,j,k))* &
                 prdif(i,j,k)*r_prdif9(i,j,k))
           end if
           if (k < nsig) then
              tmp=half*what(i,j,k+1)*r_prdif9(i,j,k)
              tmp2=half*what9(i,j,k+1)*r_prdif9(i,j,k)
              u_t(i,j,k) = u_t(i,j,k) - tmp*(ges_u (i,j,k)-ges_u (i,j,k+1)) - &
                 tmp2*( (u(i,j,k)-u(i,j,k+1)) - (ges_u (i,j,k)-ges_u (i,j,k+1))* &
                 prdif(i,j,k)*r_prdif9(i,j,k))
              v_t(i,j,k) = v_t(i,j,k) - tmp*(ges_v (i,j,k)-ges_v (i,j,k+1)) - &
                 tmp2*( (v(i,j,k)-v(i,j,k+1)) - (ges_v (i,j,k)-ges_v (i,j,k+1))* &
                 prdif(i,j,k)*r_prdif9(i,j,k))
              t_t(i,j,k) = t_t(i,j,k) - tmp*(ges_tv(i,j,k)-ges_tv(i,j,k+1)) - &
                 tmp2*( (t(i,j,k)-t(i,j,k+1)) - (ges_tv(i,j,k)-ges_tv(i,j,k+1))* &
                 prdif(i,j,k)*r_prdif9(i,j,k))
           end if
         end do   !end do i
      end do     !end do j

!     first sum to level k-1     

      do j=jtstart(kk),jtstop(kk) 
        do i=1,lat2
           tmp=rd*t(i,j,k)*r_prsum9(i,j,k)
           tmp2=rd*ges_tv(i,j,k)*r_prsum9(i,j,k)
           tmp3=prdif9(i,j,k)*r_prsum9(i,j,k)

           sumk = sumkm1(i,j) + ( tmp - tmp2*prsum(i,j,k)*r_prsum9(i,j,k) ) * &
              ( pr_xdif9(i,j,k) - ( pr_xsum9(i,j,k)*tmp3 ) )
           sumk = sumk + tmp2*( pr_xdif(i,j,k) - &
              (tmp3 *pr_xsum(i,j,k) + pr_xsum9(i,j,k)*( ( &
              prdif(i,j,k) - tmp3* prsum(i,j,k) )*r_prsum9(i,j,k) ) ) )

           sumvk = sumvkm1(i,j) + ( tmp - tmp2*prsum(i,j,k)*r_prsum9(i,j,k) ) * &
              ( pr_ydif9(i,j,k) - ( pr_ysum9(i,j,k)*tmp3 ) )
           sumvk = sumvk + tmp2*( pr_ydif(i,j,k) - &
              (tmp3*pr_ysum(i,j,k) + pr_ysum9(i,j,k)*( ( &
              prdif(i,j,k) - tmp3* prsum(i,j,k) )*r_prsum9(i,j,k) ) ) )

           sum2k  = sum2km1 (i,j) + t_x(i,j,k)*tmp3 + &
              ges_tv_lon(i,j,k)*( (prdif(i,j,k) - &
              tmp3*prsum(i,j,k))*r_prsum9(i,j,k))

           sum2vk = sum2vkm1(i,j) + t_y(i,j,k)*tmp3 + &
              ges_tv_lat(i,j,k)*( (prdif(i,j,k) - &
              tmp3*prsum(i,j,k))*r_prsum9(i,j,k))

           u_t(i,j,k) = u_t(i,j,k) - sumkm1 (i,j) - rd*sum2km1 (i,j) - &
              sumk  - rd*sum2k
           v_t(i,j,k) = v_t(i,j,k) - sumvkm1(i,j) - rd*sum2vkm1(i,j) - &
              sumvk - rd*sum2vk
 
! load up the km1 arrays for next k loop

           sumkm1  (i,j)=sumk
           sumvkm1 (i,j)=sumvk
           sum2km1 (i,j)=sum2k
           sum2vkm1(i,j)=sum2vk

        end do !end do i
      end do   !end do j
    end do  !end do k

    call turbl_tl(ges_prsi(1,1,1,it),ges_tv,ges_teta(1,1,1,it),&
               u,v,pri,t,u_t,v_t,t_t,jtstart(kk),jtstop(kk))

    if(.not.wrf_nmm_regional.and..not.nems_nmmb_regional)then
      do k=1,nsig

!       Zero out time derivatives at poles
        do j=jtstart(kk),jtstop(kk) 
           do i=1,lat2
              ix=istart(mype+1)+i-2
              if (ix == 1 .or. ix == nlat) then
                 u_t(i,j,k)=zero
                 v_t(i,j,k)=zero
              end if
           end do
        end do

      end do  !end do k 
    end if
    do k=1,nsig
      do j=jtstart(kk),jtstop(kk) 
        do i=1,lat2
!          tracer advection terms
           q_t (i,j,k) = -u(i,j,k)*ges_q_lon    (i,j,k) - ges_u(i,j,k)*q_x (i,j,k) - &
              v(i,j,k)*ges_q_lat    (i,j,k) - ges_v(i,j,k)*q_y (i,j,k)
           oz_t(i,j,k) = -u(i,j,k)*ges_oz_lon   (i,j,k) - ges_u(i,j,k)*oz_x(i,j,k) - &
              v(i,j,k)*ges_oz_lat   (i,j,k) - ges_v(i,j,k)*oz_y(i,j,k)
           if (icw>0) then
              cw_t(i,j,k) = -u(i,j,k)*ges_cw_lon(i,j,k) - ges_u(i,j,k)*cw_x(i,j,k) - &
                 v(i,j,k)*ges_cw_lat(i,j,k) - ges_v(i,j,k)*cw_y(i,j,k)
           end if
           if(k > 1)then
              tmp=half*what(i,j,k)*r_prdif9(i,j,k)
              tmp2=half*what9(i,j,k)*r_prdif9(i,j,k)
              q_t (i,j,k) = q_t (i,j,k) - tmp*(ges_q   (i,j,k-1)-ges_q   (i,j,k)) - &
                 tmp2*( (q (i,j,k-1)-q (i,j,k)) - (ges_q   (i,j,k-1)-ges_q   (i,j,k))* &
                 prdif(i,j,k)*r_prdif9(i,j,k))
              oz_t(i,j,k) = oz_t(i,j,k) - tmp*(ges_oz  (i,j,k-1)-ges_oz  (i,j,k)) - &
                 tmp2*( (oz(i,j,k-1)-oz(i,j,k)) - (ges_oz  (i,j,k-1)-ges_oz  (i,j,k))* &
                 prdif(i,j,k)*r_prdif9(i,j,k))
              if (icw>0) then 
                 cw_t(i,j,k) = cw_t(i,j,k) - tmp*(ges_cwmr(i,j,k-1)-ges_cwmr(i,j,k)) - &
                    tmp2*( (cw(i,j,k-1)-cw(i,j,k)) - (ges_cwmr(i,j,k-1)-ges_cwmr(i,j,k))* &
                    prdif(i,j,k)*r_prdif9(i,j,k))
              end if
           end if
           if(k < nsig)then
              tmp=half*what(i,j,k+1)*r_prdif9(i,j,k)
              tmp2=half*what9(i,j,k+1)*r_prdif9(i,j,k)
              q_t (i,j,k) = q_t (i,j,k) - tmp*(ges_q   (i,j,k)-ges_q   (i,j,k+1)) - &
                 tmp2*( (q (i,j,k)-q (i,j,k+1)) - (ges_q   (i,j,k)-ges_q   (i,j,k+1))* &
                 prdif(i,j,k)*r_prdif9(i,j,k))
              oz_t(i,j,k) = oz_t(i,j,k) - tmp*(ges_oz  (i,j,k)-ges_oz  (i,j,k+1)) - &
                 tmp2*( (oz(i,j,k)-oz(i,j,k+1)) - (ges_oz  (i,j,k)-ges_oz  (i,j,k+1))* &
                 prdif(i,j,k)*r_prdif9(i,j,k))
              if (icw>0) then
                 cw_t(i,j,k) = cw_t(i,j,k) - tmp*(ges_cwmr(i,j,k)-ges_cwmr(i,j,k+1)) - &
                    tmp2*( (cw(i,j,k)-cw(i,j,k+1)) - (ges_cwmr(i,j,k)-ges_cwmr(i,j,k+1))* &
                    prdif(i,j,k)*r_prdif9(i,j,k))
              end if
           end if
        end do
      end do
    end do

  end do
  if (ihave_xtra_derivatives) then
     deallocate(fvars2d,fvars3d)
     call gsi_bundledestroy(yderivative,ier)
     call gsi_bundledestroy(xderivative,ier)
  endif
! end of threading loop
  return
end subroutine calctends_tl
