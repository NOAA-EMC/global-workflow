subroutine calctends_ad(fields,fields_dt,mype)
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
!   2009-08-20  parrish - replace curvfct with curvx, curvy.  this allows tendency computation to
!                          work for any general orthogonal coordinate.
!   2010-11-03  derber - moved threading calculations to gridmod and modified
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!   2011-12-02  zhu     - add safe-guard for the case when there is no entry in the metguess table
!   2013-01-23  parrish - change t_t from intent(in) to intent(inout) (flagged by WCOSS intel debug compile)
!   2013-10-19  todling - revamp interface (pass all in bundles); derivatives and
!                         guess fields also in bundles
!   2013-10-28  todling - rename p3d to prse
!
! usage:
!   input argument list:
!    fields    - bundle holding relevant fields
!    fields_dt - bundle holding related time tendencies
!     mype     - mpi integer task id
!
!   output argument list:
!    fields    - bundle holding fields
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
  use gridmod, only: lat2,lon2,nsig,istart,rlats,nlat,idvc5,bk5,&
      eta2_ll,wrf_nmm_regional,nems_nmmb_regional,regional,nthreads,jtstart,jtstop
  use constants, only: zero,half,two,rd,rcp
  use derivsmod, only: cwgues
  use tendsmod, only: what9,prsth9,r_prsum9,prdif9,r_prdif9,pr_xsum9,pr_xdif9,&
      pr_ysum9,pr_ydif9,curvx,curvy,coriolis
  use guess_grids, only: ntguessig,&
      ges_teta,ges_prsi
  use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundle
  use mpeu_util, only: die
  use derivsmod, only: gsi_xderivative_bundle
  use derivsmod, only: gsi_yderivative_bundle
  implicit none

! Declare passed variables
  type(gsi_bundle) :: fields
  type(gsi_bundle) :: fields_dt
  type(gsi_bundle) :: derivativex
  type(gsi_bundle) :: derivativey
  integer(i_kind),intent(in) :: mype

! Declare local variables
  character(len=*),parameter::myname='calctends_ad'
  real(r_kind),dimension(:,:,:),pointer :: u_t,v_t,u,v,t,q,oz,cw
  real(r_kind),dimension(:,:,:),pointer :: t_t
  real(r_kind),dimension(:,:,:),pointer :: q_t,oz_t,cw_t
  real(r_kind),dimension(:,:,:),pointer :: p_t
  real(r_kind),dimension(:,:,:),pointer :: pri

  real(r_kind),dimension(lat2,lon2,nsig+1):: prsth,what
  real(r_kind),dimension(lat2,lon2,nsig):: prsum,prdif,pr_xsum,pr_xdif,pr_ysum,&
       pr_ydif

  real(r_kind),dimension(lat2,lon2):: ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig):: t_thor9
  real(r_kind),dimension(lat2,lon2):: sumkm1,sumvkm1,sum2km1,sum2vkm1
  real(r_kind) tmp,tmp2,tmp3,var,sumk,sumvk,sum2k,sum2vk
  integer(i_kind) i,j,k,ix,it,kk,istatus,n_actual_clouds,ier

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

  real(r_kind),pointer,dimension(:,:,:) :: u_x=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: v_x=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: t_x=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: q_x =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: oz_x=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: cw_x=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: pri_x=>NULL()

  real(r_kind),pointer,dimension(:,:,:) :: u_y=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: v_y=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: t_y=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: q_y =>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: oz_y=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: cw_y=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: pri_y=>NULL()

  it=ntguessig

  ier=0
  call gsi_bundlegetpointer(fields,'u',   u,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields,'v',   v,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields,'tv',  t,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields,'q',   q,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields,'oz' , oz,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields,'cw' , cw,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields,'prse',pri, istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname,': pointers not found on input, ier=', ier
     call stop2(999)
  endif

  ier=0
  call gsi_bundlegetpointer(derivativex,'u',   u_x,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativex,'v',   v_x,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativex,'tv',  t_x,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativex,'q',   q_x,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativex,'oz' , oz_x,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativex,'cw' , cw_x,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativex,'prse',pri_x, istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname,': x deriv. pointers not found on input, ier=', ier
     call stop2(999)
  endif
  ier=0
  call gsi_bundlegetpointer(derivativey,'u',   u_y,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativey,'v',   v_y,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativey,'tv',  t_y,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativey,'q',   q_y,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativey,'oz' , oz_y,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativey,'cw' , cw_y,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(derivativey,'prse',pri_y, istatus);ier=istatus+ier
  if(ier/=0) then
     write(6,*) myname,': y deriv. pointers not found on input, ier=', ier
     call stop2(999)
  endif

  call gsi_bundlegetpointer(fields_dt,'u',   u_t, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields_dt,'v',   v_t, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields_dt,'tv',  t_t, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields_dt,'q',   q_t, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields_dt,'oz' , oz_t,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields_dt,'cw' , cw_t,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(fields_dt,'prse', p_t,istatus);ier=istatus+ier
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
           ges_cwmr => cwgues   ! temporily, revise after moist physics is ready
        else
           call die('setuppcp','cannot get pointer to cwmr, istatus =',istatus)
        end if
     end if
  else
     ges_cwmr => cwgues
  end if

!  loop over threads
!$omp parallel do schedule(dynamic,1) private(i,j,k,kk,tmp,tmp2,ix,&
!$omp                  tmp3,sumk,sumvk,sum2k,sum2vk)
  do kk=1,nthreads

!   zero arrays

    do k=1,nsig+1
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           what(i,j,k)=zero
           prsth(i,j,k)=zero
           pri(i,j,k)=zero
        end do
      end do
    end do
    do k=1,nsig
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           prsum(i,j,k)=zero
           prdif(i,j,k)=zero
           pr_xsum(i,j,k)=zero
           pr_xdif(i,j,k)=zero
           pr_ysum(i,j,k)=zero
           pr_ydif(i,j,k)=zero
           u_x(i,j,k)=zero
           u_y(i,j,k)=zero
           v_x(i,j,k)=zero
           v_y(i,j,k)=zero
           t_x(i,j,k)=zero
           t_y(i,j,k)=zero
           q_x(i,j,k)=zero
           q_y(i,j,k)=zero
           cw_x(i,j,k)=zero
           cw_y(i,j,k)=zero
           oz_x(i,j,k)=zero
           oz_y(i,j,k)=zero
        end do
      end do
    end do
    do j=jtstart(kk),jtstop(kk)
      do i=1,lat2
        sumkm1(i,j)=zero
        sumvkm1(i,j)=zero
        sum2km1(i,j)=zero
        sum2vkm1(i,j)=zero
        ps_x(i,j)=zero
        ps_y(i,j)=zero
      end do
    end do

    do k=nsig,1,-1
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           if(k < nsig) then
              tmp2=half*what9(i,j,k+1)*r_prdif9(i,j,k)
              tmp = - q_t (i,j,k)*(ges_q   (i,j,k)-ges_q   (i,j,k+1)) - &
                      oz_t(i,j,k)*(ges_oz  (i,j,k)-ges_oz  (i,j,k+1)) - &
                      cw_t(i,j,k)*(ges_cwmr(i,j,k)-ges_cwmr (i,j,k+1))
              q (i,j,k  ) = q (i,j,k  ) - q_t (i,j,k)*tmp2
              q (i,j,k+1) = q (i,j,k+1) + q_t (i,j,k)*tmp2
              oz(i,j,k  ) = oz(i,j,k  ) - oz_t(i,j,k)*tmp2
              oz(i,j,k+1) = oz(i,j,k+1) + oz_t(i,j,k)*tmp2
              cw(i,j,k  ) = cw(i,j,k  ) - cw_t(i,j,k)*tmp2
              cw(i,j,k+1) = cw(i,j,k+1) + cw_t(i,j,k)*tmp2
              prdif(i,j,k) = prdif(i,j,k)+(tmp2*r_prdif9(i,j,k))* &
                  (((ges_q   (i,j,k)-ges_q   (i,j,k+1))*q_t (i,j,k)) + &
                   ((ges_oz  (i,j,k)-ges_oz  (i,j,k+1))*oz_t(i,j,k)) + &
                   ((ges_cwmr(i,j,k)-ges_cwmr(i,j,k+1))*cw_t(i,j,k)) )
              what(i,j,k+1) = what(i,j,k+1)+(half*tmp*r_prdif9(i,j,k))
           end if

           if(k > 1)then
              tmp2=half*what9(i,j,k)*r_prdif9(i,j,k)
              tmp= - q_t (i,j,k)*(ges_q   (i,j,k-1)-ges_q   (i,j,k)) - &
                     oz_t(i,j,k)*(ges_oz  (i,j,k-1)-ges_oz  (i,j,k)) - &
                     cw_t(i,j,k)*(ges_cwmr(i,j,k-1)-ges_cwmr (i,j,k))
              q (i,j,k-1) = q (i,j,k-1) - q_t (i,j,k)*tmp2
              q (i,j,k  ) = q (i,j,k  ) + q_t (i,j,k)*tmp2
              oz(i,j,k-1) = oz(i,j,k-1) - oz_t(i,j,k)*tmp2
              oz(i,j,k  ) = oz(i,j,k  ) + oz_t(i,j,k)*tmp2
              cw(i,j,k-1) = cw(i,j,k-1) - cw_t(i,j,k)*tmp2
              cw(i,j,k  ) = cw(i,j,k  ) + cw_t(i,j,k)*tmp2

              prdif(i,j,k) = prdif(i,j,k) + (tmp2*r_prdif9(i,j,k))* &
                  (((ges_q   (i,j,k-1)-ges_q   (i,j,k))*q_t (i,j,k)) + &
                   ((ges_oz  (i,j,k-1)-ges_oz  (i,j,k))*oz_t(i,j,k)) + &
                   ((ges_cwmr(i,j,k-1)-ges_cwmr (i,j,k))*cw_t(i,j,k)) )
              what(i,j,k) = what(i,j,k)+(half*tmp*r_prdif9(i,j,k))
           end if

!   adjoint of tracer advective terms

           u(i,j,k) = u(i,j,k) - q_t(i,j,k)*ges_q_lon(i,j,k) -  &
              oz_t(i,j,k)*ges_oz_lon(i,j,k) - cw_t(i,j,k)*ges_cw_lon(i,j,k)
           v(i,j,k) = v(i,j,k) - q_t(i,j,k)*ges_q_lat(i,j,k) -  &
              oz_t(i,j,k)*ges_oz_lat(i,j,k) - cw_t(i,j,k)*ges_cw_lat(i,j,k)
           q_x (i,j,k) = q_x (i,j,k) - q_t (i,j,k)*ges_u(i,j,k)
           q_y (i,j,k) = q_y (i,j,k) - q_t (i,j,k)*ges_v(i,j,k)
           oz_x(i,j,k) = oz_x(i,j,k) - oz_t(i,j,k)*ges_u(i,j,k)
           oz_y(i,j,k) = oz_y(i,j,k) - oz_t(i,j,k)*ges_v(i,j,k)
           cw_x(i,j,k) = cw_x(i,j,k) - cw_t(i,j,k)*ges_u(i,j,k)
           cw_y(i,j,k) = cw_y(i,j,k) - cw_t(i,j,k)*ges_v(i,j,k)
        end do
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

    call turbl_ad(ges_prsi(1,1,1,it),ges_tv,ges_teta(1,1,1,it),&
                u,v,pri,t,u_t,v_t,t_t,jtstart(kk),jtstop(kk))

    do k=nsig,1,-1               


!   vertical summation terms for u,v

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
           t_y(i,j,k) = t_y(i,j,k) + sum2vk*tmp3
           var=sum2vk*r_prsum9(i,j,k)
           prdif(i,j,k) = prdif(i,j,k) + ges_tv_lat(i,j,k)*var
           prsum(i,j,k) = prsum(i,j,k) - ges_tv_lat(i,j,k)*tmp3*var
           sum2vkm1(i,j)=sum2vkm1(i,j)+sum2vk
 
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
           sumvkm1(i,j) = sumvkm1(i,j)+ sumvk

           pr_xdif(i,j,k) = pr_xdif(i,j,k) + tmp2*sumk
           pr_xsum(i,j,k) = pr_xsum(i,j,k) - tmp2*tmp3*sumk
           var=tmp2*sumk*r_prsum9(i,j,k)
           prsum  (i,j,k) = prsum  (i,j,k) + tmp3*var*pr_xsum9(i,j,k)
           prdif  (i,j,k) = prdif  (i,j,k) - var*pr_xsum9(i,j,k)
           tmp = tmp + sumk*( pr_xdif9(i,j,k) - &
              (pr_xsum9(i,j,k)*tmp3) )
           prsum(i,j,k) = prsum(i,j,k) - var* &
              ( pr_xdif9(i,j,k) - (pr_xsum9(i,j,k)*tmp3) )
           sumkm1 (i,j) = sumkm1 (i,j) + sumk

           t(i,j,k) = t(i,j,k) + rd*tmp*r_prsum9(i,j,k)
        end do
      end do

! adjoint of vertical flux terms

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



           u(i,j,k) = u(i,j,k) - v_t(i,j,k)*(ges_v_lon(i,j,k) - two*curvy(i,j)* &
               ges_u(i,j,k) + coriolis(i,j))
           v_x(i,j,k) = v_x(i,j,k) - v_t(i,j,k)*ges_u(i,j,k)
           v(i,j,k) = v(i,j,k) - v_t(i,j,k)*(ges_v_lat(i,j,k) - two*curvy(i,j)* &
               ges_v(i,j,k))
           v_y(i,j,k) = v_y(i,j,k) - v_t(i,j,k)*ges_v(i,j,k)

           u(i,j,k) = u(i,j,k) - u_t(i,j,k)*(ges_u_lon(i,j,k) - two*curvx(i,j)* &
               ges_u(i,j,k))
           u_x(i,j,k) = u_x(i,j,k) - u_t(i,j,k)*ges_u(i,j,k)
           v(i,j,k) = v(i,j,k) - u_t(i,j,k)*(ges_u_lat(i,j,k) - two*curvx(i,j)* &
               ges_v(i,j,k) - coriolis(i,j))
           u_y(i,j,k) = u_y(i,j,k) - u_t(i,j,k)*ges_v(i,j,k)

        end do  !end do i
      end do    !end do j
    end do      !end do k

!   adjoint of calculating full three-dimensional dp/dt

    do k=1,nsig+1
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           prsth(i,j,k)=prsth(i,j,k) + p_t(i,j,k)
           what (i,j,k)=what (i,j,k) - p_t(i,j,k)
        end do
      end do
    end do

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
                   ges_v(i,j,k)*pr_ysum9 (i,j,k) + &
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
             ges_v(i,j,k)*pr_ysum9   (i,j,k) + &
             prsth9  (i,j,k)+prsth9(i,j,k+1) )
           prsum(i,j,k) = prsum(i,j,k) - tmp2*tmp*r_prsum9(i,j,k)

           var=t_t(i,j,k)*tmp*rcp
           pr_xsum(i,j,k) = pr_xsum(i,j,k) + var*ges_u(i,j,k)
           pr_ysum(i,j,k) = pr_ysum(i,j,k) + var*ges_v(i,j,k)
           u(i,j,k) = u(i,j,k) + var*pr_xsum9(i,j,k)
           v(i,j,k) = v(i,j,k) + var*pr_ysum9(i,j,k)
           prsth(i,j,k)      = prsth(i,j,k)      + var
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
           u      (i,j,k) = u      (i,j,k) - prsth(i,j,k)*pr_xdif9  (i,j,k)
           pr_xdif(i,j,k) = pr_xdif(i,j,k) - prsth(i,j,k)*ges_u  (i,j,k)
           v      (i,j,k) = v      (i,j,k) - prsth(i,j,k)*pr_ydif9  (i,j,k)
           pr_ydif(i,j,k) = pr_ydif(i,j,k) - prsth(i,j,k)*ges_v  (i,j,k)
           u_x    (i,j,k) = u_x    (i,j,k) - prsth(i,j,k)*(prdif9  (i,j,k))
           v_y    (i,j,k) = v_y    (i,j,k) - prsth(i,j,k)*(prdif9  (i,j,k))
           prdif  (i,j,k) = prdif  (i,j,k) - prsth(i,j,k)*(ges_u_lon(i,j,k) + &
                         ges_v_lat(i,j,k))
           prsth(i,j,k+1) = prsth(i,j,k+1) + prsth(i,j,k)
        end do
      end do
    end do

!   adjoint of pressure preliminaries

    do k=1,nsig+1
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           pri_x(i,j,k)=zero
           pri_y(i,j,k)=zero
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

! end of loop over threads

  call getprs_horiz_ad(ps_x,ps_y,pri,pri_x,pri_y)

! add contributions from derivatives

  call tget_derivatives( &
         fields,derivativex,derivativey)

  return
end subroutine calctends_ad
