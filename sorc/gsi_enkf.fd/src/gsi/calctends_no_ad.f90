subroutine calctends_no_ad(st,vp,t,p,mype,u_t,v_t,t_t,p_t,uvflag)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calctends_ad         adjoint of calctends_tl
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: adjoint of routine that compute tendencies for u,v,Tv,prs
!
! program history log:
!   2005-09-29  kleist
!   2005-10-17  kleist - changes to improve computational efficiency
!   2005-11-21  kleist - add tracer tendencies, use new module
!   2006-01-31  kleist - add indices to sum* variables being initialized in loop
!   2006-04-12  treadon - replace sigi with bk5
!   2006-04-21  kleist - add divergence tendency bits
!   2006-07-31  kleist - changes to use ps instead of ln(ps)
!   2006-09-21  kleist - add rescaling to divergence tendency formulation
!   2007-03-13  kleist - move jcrescale_ad (and related code) into if (divtflg) block
!   2007-04-16  kleist - move constraint specific items elsewhere
!   2007-05-08  kleist - add bits for fully generalized vertical coordinate
!   2007-06-21  rancic - add pbl 
!   2007-07-26  cucurull - add 3d pressure pri in argument list;
!                          move getprs_ad outside calctends_ad;
!                          call getprs_horiz_ad; remove ps from argument
!                          list
!   2007-08-08  derber - optimize
!   2008-06-05  safford - rm unused var "nnn" and unused uses
!   2009-04-21  derber - remove call to getstvp and modify get_derivatives to include uv
!   2009-08-20  parrish - replace curvfct with curvx, curvy.  this allows tendency computation to
!                          work for any general orthogonal coordinate.
!   2010-11-03  derber - moved threading calculations to gridmod and modified
!   2012-02-08  kleist - add uvflag to argument list.
!   2013-01-23  parrish - change t_t from intent(in) to intent(inout) (flagged by WCOSS intel debug compile)
!   2013-10-19  todling - derivatives and guess fields now in bundles
!
! usage:
!   input argument list:
!     u        - zonal wind on subdomain
!     v        - meridional wind on subdomain
!     t        - virtual temperature on subdomain
!     u_t      - time tendency of u
!     v_t      - time tendency of v
!     t_t      - time tendency of t
!     p_t      - time tendency of 2d surface pressure
!     mype     - mpi integer task id
!
!   output argument list:
!     u        - zonal wind on subdomain
!     v        - meridional wind on subdomain
!     t        - virtual temperature on subdomain
!     q        - q on subdomain
!     oz       - ozone on subdomain
!     cw       - cloud water mixing ratio on subdomain
!
!   notes:
!     adjoint check performed succesfully on 2005-09-29 by d. kleist
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,istart,nlat,idvc5,bk5,&
      eta2_ll,wrf_nmm_regional,nems_nmmb_regional,regional,nthreads,jtstart,jtstop
  use constants, only: zero,half,two,rd,rcp
  use tendsmod, only: what9,prsth9,r_prsum9,prdif9,r_prdif9,pr_xsum9,pr_xdif9,&
      pr_ysum9,pr_ydif9,curvx,curvy,coriolis
  use guess_grids, only: ntguessig,&
      ges_teta,ges_prsi
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use derivsmod, only: gsi_xderivative_bundle
  use derivsmod, only: gsi_yderivative_bundle
  use turblmod, only: use_pbl
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u_t,v_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: t_t
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: p_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: st,vp,t
  real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: p
  integer(i_kind)                       ,intent(in   ) :: mype
  logical                               ,intent(in   ) :: uvflag

! Declare local variables
  character(len=*),parameter::myname='calctends_no_ad'
  real(r_kind),dimension(lat2,lon2,nsig):: u,v
  real(r_kind),dimension(lat2,lon2,nsig+1):: pri
  real(r_kind),dimension(lat2,lon2,nsig):: u_x,u_y,v_x,v_y,t_x,t_y
  real(r_kind),dimension(lat2,lon2,nsig+1):: pri_x,pri_y,prsth,what
  real(r_kind),dimension(lat2,lon2,nsig):: prsum,prdif,pr_xsum,pr_xdif,pr_ysum,&
       pr_ydif

  real(r_kind),dimension(lat2,lon2,nsig):: t_thor9
  real(r_kind),dimension(lat2,lon2):: sumkm1,sumvkm1,sum2km1,sum2vkm1
  real(r_kind) tmp,tmp2,tmp3,var,sumk,sumvk,sum2k,sum2vk
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

!$omp parallel do private(i,j,k,kk,tmp,tmp2,ix,&
!$omp                  tmp3,sumk,sumvk,sum2k,sum2vk,var)

  do kk=1,nthreads

!   zero arrays
    do k=1,nsig+1
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           pri(i,j,k)=zero
           pri_x(i,j,k)=zero
           pri_y(i,j,k)=zero
        end do
      end do
    end do

!   Put p_t in prsth and what for adjoint of calculating 
!   full three-dimensional dp/dt

    do j=jtstart(kk),jtstop(kk)
      do i=1,lat2
        prsth(i,j,1)=   p_t(i,j)
        what (i,j,1)= - p_t(i,j)
      end do
    end do
    do k=1,nsig
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           u(i,j,k)=zero
           v(i,j,k)=zero
           u_x(i,j,k)=zero
           u_y(i,j,k)=zero
           v_x(i,j,k)=zero
           v_y(i,j,k)=zero
           t_x(i,j,k)=zero
           t_y(i,j,k)=zero
           prsum(i,j,k)=zero
           prdif(i,j,k)=zero
           pr_xsum(i,j,k)=zero
           pr_xdif(i,j,k)=zero
           pr_ysum(i,j,k)=zero
           pr_ydif(i,j,k)=zero
           what(i,j,k+1)=zero
           prsth(i,j,k+1)=zero
        end do
      end do
    end do
    do j=jtstart(kk),jtstop(kk)
      do i=1,lat2
        sumkm1(i,j)=zero
        sumvkm1(i,j)=zero
        sum2km1(i,j)=zero
        sum2vkm1(i,j)=zero
      end do
    end do

!   adjoint of sum 2d individual terms into 3d tendency arrays
!   because of sum arrays/dependencies, have to go from nsig --> 1

    if(.not.wrf_nmm_regional.and..not.nems_nmmb_regional)then
      do k=1,nsig
        do j=jtstart(kk),jtstop(kk)
           do i=1,lat2
              ix=istart(mype+1)+i-2
              if (ix == 1 .or. ix == nlat) then
                 u_t(i,j,k)=zero ; v_t(i,j,k)=zero
              end if
           end do
        end do
      end do                         
    end if

    if(use_pbl)call turbl_ad(ges_prsi(1,1,1,it),ges_tv,ges_teta(1,1,1,it),&
                u,v,pri,t,u_t,v_t,t_t,jtstart(kk),jtstop(kk))

    do k=nsig,1,-1               


!     vertical summation terms for u,v

      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           sumk  =sumkm1  (i,j) - u_t   (i,j,k)
           sumvk =sumvkm1 (i,j) - v_t   (i,j,k)
           sum2k =sum2km1 (i,j) - rd*u_t(i,j,k) 
           sum2vk=sum2vkm1(i,j) - rd*v_t(i,j,k)
 
           sumkm1(i,j)   = -u_t   (i,j,k)
           sumvkm1(i,j)  = -v_t   (i,j,k)
           sum2km1(i,j)  = -rd*u_t(i,j,k)
           sum2vkm1(i,j) = -rd*v_t(i,j,k)

! arrays tmp2 and tmp3 are from basic state variables

           tmp2=rd*ges_tv(i,j,k)*r_prsum9(i,j,k)
           tmp3=prdif9(i,j,k)*r_prsum9(i,j,k)
           t_y   (i,j,k)= t_y  (i,j,k) + sum2vk*tmp3
           var=sum2vk*r_prsum9(i,j,k)
           prdif (i,j,k)= prdif(i,j,k) + ges_tv_lat(i,j,k)*var
           prsum (i,j,k)= prsum(i,j,k) - ges_tv_lat(i,j,k)*tmp3*var
           sum2vkm1(i,j)=sum2vkm1(i,j) + sum2vk
 
           t_x  (i,j,k) = t_x  (i,j,k) + sum2k*tmp3
           var=sum2k*r_prsum9(i,j,k)
           prdif(i,j,k) = prdif(i,j,k) + ges_tv_lon(i,j,k)*var
           prsum(i,j,k) = prsum(i,j,k) - ges_tv_lon(i,j,k)*tmp3*var
           sum2km1(i,j) = sum2km1(i,j) + sum2k    
 
           pr_ydif(i,j,k) = pr_ydif(i,j,k) + tmp2*sumvk
           pr_ysum(i,j,k) = pr_ysum(i,j,k) - tmp2*tmp3*sumvk
           var=tmp2*sumvk*r_prsum9(i,j,k)
           prsum  (i,j,k) = prsum  (i,j,k) + tmp3*var*pr_ysum9(i,j,k)
           prdif  (i,j,k) = prdif  (i,j,k) - var*pr_ysum9(i,j,k)

! tmp is a reused variable

           tmp = sumvk*( pr_ydif9(i,j,k) - (pr_ysum9(i,j,k)*tmp3) )
           prsum(i,j,k) = prsum(i,j,k) - var* &
              ( pr_ydif9(i,j,k) - (pr_ysum9(i,j,k)*tmp3) )
           sumvkm1(i,j)= sumvkm1(i,j)+ sumvk

           pr_xdif(i,j,k) = pr_xdif(i,j,k) + tmp2*sumk
           pr_xsum(i,j,k) = pr_xsum(i,j,k) - tmp2*tmp3*sumk
           var=tmp2*sumk*r_prsum9(i,j,k)
           prsum  (i,j,k) = prsum  (i,j,k) + tmp3*var*pr_xsum9(i,j,k)
           prdif  (i,j,k) = prdif  (i,j,k) - var*pr_xsum9(i,j,k)
           tmp = tmp + sumk*( pr_xdif9(i,j,k) - &
              (pr_xsum9(i,j,k)*tmp3) )
           prsum  (i,j,k) = prsum(i,j,k) - var* &
              ( pr_xdif9(i,j,k) - (pr_xsum9(i,j,k)*tmp3) )
           sumkm1   (i,j) = sumkm1 (i,j) + sumk

           t(i,j,k) = t(i,j,k) + rd*tmp*r_prsum9(i,j,k)
        end do
      end do

!     adjoint of vertical flux terms

      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           if(k < nsig) then
              tmp2=half*what9(i,j,k+1)*r_prdif9(i,j,k)

              tmp = -u_t(i,j,k)*(ges_u (i,j,k)-ges_u (i,j,k+1)) - &
                     v_t(i,j,k)*(ges_v (i,j,k)-ges_v (i,j,k+1)) - &
                     t_t(i,j,k)*(ges_tv(i,j,k)-ges_tv(i,j,k+1))    

              t(i,j,k  ) = t(i,j,k  ) - t_t(i,j,k)*tmp2
              t(i,j,k+1) = t(i,j,k+1) + t_t(i,j,k)*tmp2
              u(i,j,k  ) = u(i,j,k  ) - u_t(i,j,k)*tmp2
              u(i,j,k+1) = u(i,j,k+1) + u_t(i,j,k)*tmp2
              v(i,j,k  ) = v(i,j,k  ) - v_t(i,j,k)*tmp2
              v(i,j,k+1) = v(i,j,k+1) + v_t(i,j,k)*tmp2
 
              prdif(i,j,k) = prdif(i,j,k) + (tmp2*r_prdif9(i,j,k))* &
                ( ((ges_tv(i,j,k)-ges_tv(i,j,k+1))*t_t(i,j,k)) + &
                  ((ges_u (i,j,k)-ges_u (i,j,k+1))*u_t(i,j,k)) + &
                  ((ges_v (i,j,k)-ges_v (i,j,k+1))*v_t(i,j,k)) )

              what(i,j,k+1) = what(i,j,k+1) + half*tmp*r_prdif9(i,j,k)
           end if
           if(k > 1) then
              tmp2=half*what9(i,j,k)*r_prdif9(i,j,k)

              tmp = - u_t(i,j,k)*(ges_u (i,j,k-1)-ges_u (i,j,k)) - &
                      v_t(i,j,k)*(ges_v (i,j,k-1)-ges_v (i,j,k)) - &
                      t_t(i,j,k)*(ges_tv(i,j,k-1)-ges_tv(i,j,k)) 
 
              t(i,j,k-1) = t(i,j,k-1) - t_t(i,j,k)*tmp2
              t(i,j,k  ) = t(i,j,k  ) + t_t(i,j,k)*tmp2
              u(i,j,k-1) = u(i,j,k-1) - u_t(i,j,k)*tmp2
              u(i,j,k  ) = u(i,j,k  ) + u_t(i,j,k)*tmp2
              v(i,j,k-1) = v(i,j,k-1) - v_t(i,j,k)*tmp2
              v(i,j,k  ) = v(i,j,k  ) + v_t(i,j,k)*tmp2
 
              prdif(i,j,k) = prdif(i,j,k) + (tmp2*r_prdif9(i,j,k))* &
                ( ((ges_tv(i,j,k-1)-ges_tv(i,j,k))*t_t(i,j,k)) + &
                  ((ges_u (i,j,k-1)-ges_u (i,j,k))*u_t(i,j,k)) + &
                  ((ges_v (i,j,k-1)-ges_v (i,j,k))*v_t(i,j,k)) )

              what(i,j,k) = what(i,j,k) + half*tmp*r_prdif9(i,j,k)
           end if

!       Now finish up with adjoint of the rest of the terms
!       tmp is now a basic state variable, whereas tmp2 is used in computation

           tmp=rd*ges_tv(i,j,k)*r_prsum9(i,j,k)

           pr_ysum(i,j,k) = pr_ysum(i,j,k) - tmp*v_t(i,j,k)
           prsum  (i,j,k) = prsum  (i,j,k) + tmp*v_t(i,j,k)*pr_ysum9(i,j,k)* &
              r_prsum9(i,j,k)

           tmp2 = - v_t(i,j,k)*pr_ysum9(i,j,k)
 
           pr_xsum(i,j,k) = pr_xsum(i,j,k) - tmp*u_t(i,j,k)
           prsum  (i,j,k) = prsum  (i,j,k) + tmp*u_t(i,j,k)*pr_xsum9(i,j,k)* &
              r_prsum9(i,j,k)
           tmp2 = tmp2 - u_t(i,j,k)*pr_xsum9(i,j,k)
 
           t(i,j,k) = t(i,j,k) + rd*tmp2*r_prsum9(i,j,k)

           u  (i,j,k) = u  (i,j,k) - v_t(i,j,k)*(ges_v_lon(i,j,k) - two*curvy(i,j)* &
               ges_u(i,j,k) + coriolis(i,j))
           v_x(i,j,k) = v_x(i,j,k) - v_t(i,j,k)*ges_u(i,j,k)
           v  (i,j,k) = v  (i,j,k) - v_t(i,j,k)*(ges_v_lat(i,j,k) - two*curvy(i,j)* &
               ges_v(i,j,k))
           v_y(i,j,k) = v_y(i,j,k) - v_t(i,j,k)*ges_v(i,j,k)
 
           u  (i,j,k) = u  (i,j,k) - u_t(i,j,k)*(ges_u_lon(i,j,k) - two*curvx(i,j)* &
               ges_u(i,j,k))
           u_x(i,j,k) = u_x(i,j,k) - u_t(i,j,k)*ges_u(i,j,k)
           v  (i,j,k) = v  (i,j,k) - u_t(i,j,k)*(ges_u_lat(i,j,k) - two*curvx(i,j)* &
               ges_v(i,j,k) - coriolis(i,j))
           u_y(i,j,k) = u_y(i,j,k) - u_t(i,j,k)*ges_v(i,j,k)

        end do  !end do i
      end do    !end do j
    end do      !end do k


!   adjoint of calculation of vertical velocity

    if ( (.not.regional) .AND. (idvc5==3)) then

!     Basic state horizontal temperature tendency

!     Get horizontal part of temperature tendency for vertical velocity term

      do k=1,nsig
        do j=jtstart(kk),jtstop(kk)
           do i=1,lat2
              tmp=-rd*ges_tv(i,j,k)*r_prsum9(i,j,k)
              t_thor9(i,j,k)=-ges_u(i,j,k)*ges_tv_lon(i,j,k) - &
                   ges_v(i,j,k)*ges_tv_lat(i,j,k)
              t_thor9(i,j,k)=t_thor9(i,j,k) -tmp*rcp * ( ges_u(i,j,k)*pr_xsum9(i,j,k) + &
                 ges_v(i,j,k)*pr_ysum9(i,j,k) + &
                 prsth9(i,j,k) + prsth9(i,j,k+1) )
           end do
        end do
      end do
      call getvvel_ad(t,t_t,t_thor9,prsth,prdif,what,jtstart(kk),jtstop(kk))
    else
      if (wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=2,nsig
           do j=jtstart(kk),jtstop(kk)
              do i=1,lat2
                 prsth(i,j,1) = prsth(i,j,1) - eta2_ll (k)*what(i,j,k)
                 prsth(i,j,k) = prsth(i,j,k) + what(i,j,k)
              end do
           end do
        end do 
      else
        do k=2,nsig
           do j=jtstart(kk),jtstop(kk)
              do i=1,lat2
                 prsth(i,j,1) = prsth(i,j,1) - bk5     (k)*what(i,j,k)
                 prsth(i,j,k) = prsth(i,j,k) + what(i,j,k)
              end do
           end do
        end do 
      end if
    end if

!   Horizontal Part of temperature tendency, now that adjoint of
!   vertical velocity is done

    do k=1,nsig
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           tmp=rd*ges_tv(i,j,k)*r_prsum9(i,j,k)
 
           tmp2 = t_t(i,j,k)*rcp*( ges_u(i,j,k)* &
              pr_xsum9(i,j,k) + &
              ges_v(i,j,k)*pr_ysum9(i,j,k) + &
              prsth9(i,j,k)+prsth9(i,j,k+1) )
           prsum(i,j,k) = prsum(i,j,k) - tmp2*tmp*r_prsum9(i,j,k)

           var=t_t(i,j,k)*tmp*rcp
           pr_xsum(i,j,k) = pr_xsum(i,j,k) + var*ges_u(i,j,k)
           pr_ysum(i,j,k) = pr_ysum(i,j,k) + var*ges_v(i,j,k)
           u      (i,j,k) = u      (i,j,k) + var*pr_xsum9(i,j,k)
           v      (i,j,k) = v      (i,j,k) + var*pr_ysum9(i,j,k)
           prsth  (i,j,k) = prsth  (i,j,k) + var
           prsth(i,j,k+1) = prsth(i,j,k+1) + var

           t(i,j,k) = t(i,j,k) + rd*tmp2*r_prsum9(i,j,k)
 
           u  (i,j,k) = u  (i,j,k) - t_t(i,j,k)*ges_tv_lon(i,j,k)
           t_x(i,j,k) = t_x(i,j,k) - t_t(i,j,k)*ges_u (i,j,k)
           v  (i,j,k) = v  (i,j,k) - t_t(i,j,k)*ges_tv_lat(i,j,k)
           t_y(i,j,k) = t_y(i,j,k) - t_t(i,j,k)*ges_v (i,j,k)
        end do
      end do
    end do

!   adjoint of horizontal portion of pressure tendency

    do k=1,nsig
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           u      (i,j,k) = u      (i,j,k) - prsth(i,j,k)*pr_xdif9(i,j,k)
           pr_xdif(i,j,k) = pr_xdif(i,j,k) - prsth(i,j,k)*ges_u(i,j,k)
           v      (i,j,k) = v      (i,j,k) - prsth(i,j,k)*pr_ydif9(i,j,k)
           pr_ydif(i,j,k) = pr_ydif(i,j,k) - prsth(i,j,k)*ges_v(i,j,k)
           u_x    (i,j,k) = u_x    (i,j,k) - prsth(i,j,k)*(prdif9(i,j,k))
           v_y    (i,j,k) = v_y    (i,j,k) - prsth(i,j,k)*(prdif9(i,j,k))
           prdif  (i,j,k) = prdif  (i,j,k) - prsth(i,j,k)*(ges_u_lon(i,j,k) + &
              ges_v_lat(i,j,k))
           prsth(i,j,k+1) = prsth(i,j,k+1) + prsth(i,j,k)
        end do
      end do
    end do

    do k=1,nsig
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           pri  (i,j,k  )=pri  (i,j,k  ) + (prsum  (i,j,k)+prdif  (i,j,k))
           pri  (i,j,k+1)=pri  (i,j,k+1) + (prsum  (i,j,k)-prdif  (i,j,k))
           pri_x(i,j,k  )=pri_x(i,j,k  ) + (pr_xsum(i,j,k)+pr_xdif(i,j,k))
           pri_x(i,j,k+1)=pri_x(i,j,k+1) + (pr_xsum(i,j,k)-pr_xdif(i,j,k))
           pri_y(i,j,k  )=pri_y(i,j,k  ) + (pr_ysum(i,j,k)+pr_ydif(i,j,k))
           pri_y(i,j,k+1)=pri_y(i,j,k+1) + (pr_ysum(i,j,k)-pr_ydif(i,j,k))
        end do
      end do
    end do

  end do

!  end of threading loop

  if(uvflag)then
     call tget_derivatives2uv(st,vp,t,pri,u,v,u_x,v_x,t_x,pri_x, &
                                         u_y,v_y,t_y,pri_y)
  else
     call tget_derivatives2(st,vp,t,pri,u,v,u_x,v_x,t_x,pri_x, &
                                      u_y,v_y,t_y,pri_y)
  end if


  call getprs_ad(p,t,pri)

  return
end subroutine calctends_no_ad
