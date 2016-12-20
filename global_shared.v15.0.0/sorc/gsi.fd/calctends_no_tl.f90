subroutine calctends_no_tl(st,vp,t,p,mype,u_t,v_t,t_t,p_t,uvflag)
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
!   2009-04-21  derber - remove call to getuv and modify get_derivatives to include uv
!   2009-08-20  parrish - replace curvfct with curvx, curvy.  this allows tendency computation to
!                          work for any general orthogonal coordinate.
!   2010-11-03  derber - moved threading calculations to gridmod and modified
!   2012-02-08  kleist - add uvflag to argument list
!   2013-10-19  todling - derivatives and guess fields now in bundles
!
! usage:
!   input argument list:
!     u        - zonal wind on subdomain
!     v        - meridional wind on subdomain
!     t        - virtual temperature on subdomain
!     mype     - task id
!     uvflag   - logical, set to true for st,vp wind components, instead of stream/potential function
!
!   output argument list:
!     u_t      - time tendency of u
!     v_t      - time tendency of v
!     t_t      - time tendency of t
!     p_t      - time tendency of 2d surface pressure
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
  use tendsmod, only: what9,prsth9,r_prsum9,prdif9,r_prdif9,pr_xsum9,pr_xdif9,&
      pr_ysum9,pr_ydif9,curvx,curvy,coriolis
  use guess_grids, only: ntguessig,ges_teta,ges_prsi
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use derivsmod, only: gsi_xderivative_bundle
  use derivsmod, only: gsi_yderivative_bundle
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: st,vp,t
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: p
  integer(i_kind)                       ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2)     ,intent(  out) :: p_t
  logical                               ,intent(   in) :: uvflag

! Declare local variables
  character(len=*),parameter::myname='calctends_no_tl'
  real(r_kind),dimension(lat2,lon2,nsig+1):: pri
  real(r_kind),dimension(lat2,lon2,nsig):: u,v
  real(r_kind),dimension(lat2,lon2,nsig+1):: pri_x,pri_y
  real(r_kind),dimension(lat2,lon2,nsig+1):: prsth,what
  real(r_kind),dimension(lat2,lon2,nsig):: prsum,prdif,pr_xsum,pr_xdif,&
       pr_ysum,pr_ydif
  real(r_kind),dimension(lat2,lon2):: sumkm1,sumvkm1,sum2km1,sum2vkm1
  real(r_kind),dimension(lat2,lon2,nsig):: t_thor9
  real(r_kind),dimension(lat2,lon2,nsig):: u_x,u_y,v_x,v_y,t_x,t_y

  real(r_kind) tmp,tmp2,tmp3,sumk,sumvk,sum2k,sum2vk,uduvdv
  integer(i_kind) i,j,k,ix,it,kk,ier,istatus

  real(r_kind),pointer,dimension(:,:,:) :: ges_u=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_v=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_tv=>NULL()

  real(r_kind),pointer,dimension(:,:,:) :: ges_u_lon=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_v_lon=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_tv_lon=>NULL()

  real(r_kind),pointer,dimension(:,:,:) :: ges_u_lat=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_v_lat=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_tv_lat=>NULL()

! linearized about guess solution, so set it flag accordingly
  it=ntguessig

  ier=0
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'u' ,ges_u, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'v' ,ges_v, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tv',ges_tv,istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname, ': pointers not found in met-guess, ier=', ier
     call stop2(999)
  endif

  ier=0
  call gsi_bundlegetpointer(gsi_xderivative_bundle(it),'u' ,ges_u_lon, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_xderivative_bundle(it),'v' ,ges_v_lon, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_xderivative_bundle(it),'tv',ges_tv_lon,istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname, ': pointers not found in lon-derivatives, ier=', ier
     call stop2(999)
  endif

  ier=0
  call gsi_bundlegetpointer(gsi_yderivative_bundle(it),'u' ,ges_u_lat, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_yderivative_bundle(it),'v' ,ges_v_lat, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(gsi_yderivative_bundle(it),'tv',ges_tv_lat,istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname, ': pointers not found in lat-derivatives, ier=', ier
     call stop2(999)
  endif

! preliminaries:

! get 3d pressure
  call getprs_tl(p,t,pri)

  if(uvflag)then
    call get_derivatives2uv(st,vp,t,pri,u,v,u_x,v_x,t_x,pri_x, &
                                        u_y,v_y,t_y,pri_y)
  else
    call get_derivatives2(st,vp,t,pri,u,v,u_x,v_x,t_x,pri_x, &
                                        u_y,v_y,t_y,pri_y)
  end if


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
           prsth(i,j,k)=prsth(i,j,k+1) -  ( u(i,j,k)*pr_xdif9(i,j,k) + &
              ges_u(i,j,k)*pr_xdif(i,j,k) + v(i,j,k)*pr_ydif9(i,j,k) + &
              ges_v(i,j,k)*pr_ydif(i,j,k) + &
              (u_x      (i,j,k) + v_y      (i,j,k))*(prdif9(i,j,k)) + &
              (ges_u_lon(i,j,k) + ges_v_lat(i,j,k))*(prdif (i,j,k)) )
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
                 what(i,j,k)=prsth(i,j,k)-bk5(k)*prsth(i,j,1)
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

    do j=jtstart(kk),jtstop(kk)
      do i=1,lat2
        p_t(i,j)=prsth(i,j,1)
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

!          vertical flux terms
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

           sum2k = sum2km1(i,j) + t_x(i,j,k)*tmp3 + &
              ges_tv_lon(i,j,k)*( (prdif(i,j,k) - &
              tmp3*prsum(i,j,k))*r_prsum9(i,j,k))
 
           sum2vk = sum2vkm1(i,j) + t_y(i,j,k)*tmp3 + &
              ges_tv_lat(i,j,k)*( (prdif(i,j,k) - &
              tmp3*prsum(i,j,k))*r_prsum9(i,j,k))
 
           u_t(i,j,k) = u_t(i,j,k) - sumkm1 (i,j) - rd*sum2km1 (i,j) - &
              sumk  - rd*sum2k
           v_t(i,j,k) = v_t(i,j,k) - sumvkm1(i,j) - rd*sum2vkm1(i,j) - &
              sumvk - rd*sum2vk

!          load up the km1 arrays for next k loop
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

  end do
!  end threading loop

  return
end subroutine calctends_no_tl
