subroutine calctends(mype,teta,pri,guess,xderivative,yderivative,tendency)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calctends       calculate u,v,t,p tendencies
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: compute tendencies for pressure, wind, and virtual 
!           temperature
!
! program history log:
!   2005-09-29  kleist
!   2005-10-17  kleist - changes to improve computational efficiency
!   2005-11-21  kleist - add tracer tendencies, use new module
!   2006-04-12  treadon - replace sigi with bk5
!   2006-04-21  kleist - add divergence tendency
!   2006-07-31  kleist - changes to use ps instead of ln(ps)
!   2007-04-16  kleist - remove divergence tendency bits to outside
!   2007-05-08  kleist - add bits for fully generalized coordinate
!   2007-06-21  rancic - add pbl 
!   2007-07-02  derber - move calculation of z_x, z_y into routine
!   2007-07-26 cucurull - add pri in argument list, call getprs_horiz;
!                         move getprs outside calctends;
!                         remove ps from argument list
!   2007-08-08  derber - optimize, remove calculation of t_over* and dp_over* unless needed.
!   2008-06-05  safford - rm unused vars and uses
!   2009-08-20  parrish - replace curvfct with curvx, curvy.  this allows tendency computation to 
!                          work for any general orthogonal coordinate.
!   2010-11-03  derber - added threading
!   2012-03-11  tong - added condition to calculate cw tendency
!   2013-10-19  todling - revamp interface with fields now in bundles; still
!                         needs generalization
!   2013-10-28  todling - rename p3d to prse
!   2019-03-13  eliu - use derivative var table instead of the control var table  
!   2019-05-09  eliu - point cloud water(cw) to derived value (cwgues) when cw is not in met_guess table  
!
! usage:
!   input argument list:
!     z        - sfc terrain height
!     ps_x     - zonal derivative of ps
!     ps_y     - meridional derivative of ps
!     mype     - task id
!
!   output argument list:
!     p_t      - time tendency of 3d prs
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,istart,rlats,nlat,idvc5,bk5,&
     jstart,region_lat,eta2_ll,wrf_nmm_regional,nems_nmmb_regional,nlon,regional,&
     region_dx,region_dy,nthreads,jtstart,jtstop
  use constants, only: zero,half,one,two,rearth,rd,rcp,omega,grav
  use tendsmod, only: what9,prsth9,r_prsum9,r_prdif9,prdif9,pr_xsum9,pr_xdif9,pr_ysum9,&
     pr_ydif9,curvx,curvy,coriolis
  use derivsmod, only: dvars3d,cwgues  
  use mpeu_util, only: getindex

  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  use mpeu_util, only: die
  use turblmod, only: use_pbl
  implicit none

! Declare passed variables
  integer(i_kind)                         ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: teta
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: pri
  type(gsi_bundle) :: guess
  type(gsi_bundle) :: xderivative
  type(gsi_bundle) :: yderivative
  type(gsi_bundle) :: tendency

! Declare local variables
  character(len=*),parameter::myname='calctends'
  real(r_kind),dimension(lat2,lon2):: z_x,z_y
  real(r_kind),dimension(lat2,lon2,nsig+1):: pri_x,pri_y
  real(r_kind),dimension(lat2,lon2):: sumkm1,sumvkm1,sum2km1,sum2vkm1
  real(r_kind) tmp,tmp2
  integer(i_kind) i,j,k,ix,ixm,ixp,jx,jxm,jxp,kk,icw,iq,ioz,ip3d  
  real(r_kind) sumk,sumvk,sum2k,sum2vk,uuvv

  real(r_kind),dimension(:,:  ),pointer :: z   =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: u   =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: v   =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: t   =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: q   =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: oz  =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: cw  =>NULL()

  real(r_kind),dimension(:,:  ),pointer :: ps_x=>NULL()
  real(r_kind),dimension(:,:,:),pointer :: u_x =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: v_x =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: t_x =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: q_x =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: oz_x=>NULL()
  real(r_kind),dimension(:,:,:),pointer :: cw_x=>NULL()
  real(r_kind),dimension(:,:  ),pointer :: ps_y=>NULL()
  real(r_kind),dimension(:,:,:),pointer :: u_y =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: v_y =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: t_y =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: q_y =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: oz_y=>NULL()
  real(r_kind),dimension(:,:,:),pointer :: cw_y=>NULL()

  real(r_kind),dimension(:,:,:),pointer :: p_t =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: u_t =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: v_t =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: t_t =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: q_t =>NULL()
  real(r_kind),dimension(:,:,:),pointer :: oz_t=>NULL()
  real(r_kind),dimension(:,:,:),pointer :: cw_t=>NULL()

! Get all pointers:

! from guess ...
  call init_vars_('guess',guess)

! from longitudinal derivative ...
  call init_vars_('xderivative',xderivative)

! from latitudinal derivative ...
  call init_vars_('yderivative',yderivative)

! from tendency ...
  call init_vars_('tendency',tendency)

! NOTES:
!  - equations taken from NCEP Office Note 445 (Juang 2005)
!  - this is the nonlinear routine, which currently in the GSI is only
!    called based on the current guess solution.  As such, basic state
!    variables that are needed for the TLM are loaded here (in the *9
!    arrays)

  what9=zero

  call get_zderivs(z,z_x,z_y,mype)
! preliminaries
  call getprs_horiz(ps_x,ps_y,pri,pri_x,pri_y)

!  Loop over threads
!$omp parallel do schedule(dynamic,1) private(i,j,k,kk,jx,jxp,jxm,ixp,ixm,ix, &
!$omp                  tmp,tmp2,uuvv,sumk,sumvk,sum2k,sum2vk)

  do kk=1,nthreads
    if(wrf_nmm_regional.or.nems_nmmb_regional) then
      do j=jtstart(kk),jtstop(kk)
        jx=j+jstart(mype+1)-2
        jx=min(max(1,jx),nlon)
        jxp=jx+1
        jxm=jx-1
        if(jx==1) then
           jxp=2
           jxm=1
        elseif(jx==nlon) then
           jxp=nlon
           jxm=nlon-1
        end if
        do i=1,lat2
           ix=istart(mype+1)+i-2
           ix=min(max(ix,1),nlat)
           ixp=ix+1
           ixm=ix-1
           if(ix==1) then
              ixp=2
              ixm=1
           elseif(ix==nlat) then
              ixp=nlat
              ixm=nlat-1
           end if
           coriolis(i,j)=two*omega*sin(region_lat(ix,jx))
           curvx(i,j)=(region_dy(ix,jxp)-region_dy(ix,jxm))/((jxp-jxm)*region_dx(ix,jx)*region_dy(ix,jx))
           curvy(i,j)=(region_dx(ixp,jx)-region_dx(ixm,jx))/((ixp-ixm)*region_dx(ix,jx)*region_dy(ix,jx))
        end do
      end do
    else
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           ix=istart(mype+1)+i-2
           ix=min(max(ix,2),nlat-1)
           coriolis(i,j)=two*omega*sin(rlats(ix))
           curvx(i,j)=zero
           curvy(i,j)=-tan(rlats(ix))/rearth
        end do
      end do
    end if


    do k=1,nsig
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           r_prsum9(i,j,k)=one/(pri(i,j,k)+pri(i,j,k+1))
           prdif9(i,j,k)=pri(i,j,k)-pri(i,j,k+1)
           r_prdif9(i,j,k)=one/prdif9(i,j,k)
           pr_xsum9(i,j,k)=pri_x(i,j,k)+pri_x(i,j,k+1)
           pr_xdif9(i,j,k)=pri_x(i,j,k)-pri_x(i,j,k+1)
           pr_ysum9(i,j,k)=pri_y(i,j,k)+pri_y(i,j,k+1)
           pr_ydif9(i,j,k)=pri_y(i,j,k)-pri_y(i,j,k+1)
        end do
      end do
    end do

!   Compute horizontal part of tendency for 3d pressure (so dps/dt is the same
!   as prsth9(i,j,1) . . . also note that at the top, dp/dt=0
!   or: prsth9(i,j,nsig+1)=zero

    do j=jtstart(kk),jtstop(kk)
      do i=1,lat2
        prsth9(i,j,nsig+1)=zero
      end do
    end do
    do k=nsig,1,-1
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           prsth9(i,j,k)=prsth9(i,j,k+1) - ( u(i,j,k)*pr_xdif9(i,j,k) + &
                v(i,j,k)*pr_ydif9(i,j,k)    + (u_x(i,j,k) + v_y(i,j,k))* &
                prdif9(i,j,k) )
        end do
      end do
    end do   

!   Compute horizontal part of tendency for T (needed for vertical velocity in hybrid theta coordinates)

    do k=1,nsig
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           tmp=-rd*t(i,j,k)*r_prsum9(i,j,k)
           t_t(i,j,k)=-u(i,j,k)*t_x(i,j,k) - v(i,j,k)*t_y(i,j,k)
           t_t(i,j,k)=t_t(i,j,k) -tmp*rcp * ( u(i,j,k)*pr_xsum9(i,j,k) + &
              v(i,j,k)*pr_ysum9(i,j,k) + &
              prsth9(i,j,k) + prsth9(i,j,k+1) )
        end do  !end do j
      end do    !end do i

    end do  !end do k

!   calculate vertical velocity term:  z(dp/dz) (zero at top/bottom interfaces)
!   if running global, and there is a c(k) coefficient, we call the vvel subroutine

    if ( (.not.regional) .AND. (idvc5==3)) then
      call getvvel(t,t_t,prsth9,prdif9,what9,jtstart(kk),jtstop(kk))
    else
      if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=2,nsig
          do j=jtstart(kk),jtstop(kk)
             do i=1,lat2
                 what9(i,j,k)=prsth9(i,j,k)-eta2_ll(k)*prsth9(i,j,1)
             end do
          end do
        end do
      else
        do k=2,nsig
          do j=jtstart(kk),jtstop(kk)
             do i=1,lat2
                 what9(i,j,k)=prsth9(i,j,k)-bk5(k)*prsth9(i,j,1)
             end do
          end do
        end do
      end if
    end if ! end if on 

!   load actual dp/dt here now, as prsth9 is reused in 
!   what9(i,k,1) & what9(i,j,nsig+1) = zero
!   p_t(i,j,1) is the same as the surface pressure tendency

    ip3d=getindex(dvars3d,'prse')
    if (ip3d>0) then  
       do k=1,nsig+1
         do j=jtstart(kk),jtstop(kk)
           do i=1,lat2
              p_t(i,j,k)=prsth9(i,j,k)-what9(i,j,k)
           end do
         end do
       end do
    endif

!   before big k loop, zero out the km1 summation arrays
    do j=jtstart(kk),jtstop(kk)
      do i=1,lat2
        sumkm1(i,j)=zero
        sum2km1(i,j)=zero
        sumvkm1(i,j)=zero
        sum2vkm1(i,j)=zero
      end do
    end do

!   Compute tendencies for wind components & Temperature

    icw =getindex(dvars3d,'cw')  
    ioz =getindex(dvars3d,'oz')  
    iq  =getindex(dvars3d,'q')   
    ip3d=getindex(dvars3d,'prse')
    do k=1,nsig
      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           tmp=-rd*t(i,j,k)*r_prsum9(i,j,k)
           uuvv=u(i,j,k)*u(i,j,k)+v(i,j,k)*v(i,j,k)
           u_t(i,j,k)=-u(i,j,k)*u_x(i,j,k) - v(i,j,k)*u_y(i,j,k) + &
              coriolis(i,j)*v(i,j,k) + curvx(i,j)*uuvv
           u_t(i,j,k)=u_t(i,j,k) + pr_xsum9(i,j,k)*tmp - grav*z_x(i,j)

           v_t(i,j,k)=-u(i,j,k)*v_x(i,j,k) - v(i,j,k)*v_y(i,j,k) - &
              coriolis(i,j)*u(i,j,k) + curvy(i,j)*uuvv
           v_t(i,j,k)=v_t(i,j,k) + pr_ysum9(i,j,k)*tmp - grav*z_y(i,j)

!   horizontal advection of "tracer" quantities

           if (iq >0) q_t (i,j,k) = -u(i,j,k)*q_x (i,j,k) - v(i,j,k)*q_y (i,j,k)    
           if (ioz>0) oz_t(i,j,k) = -u(i,j,k)*oz_x(i,j,k) - v(i,j,k)*oz_y(i,j,k) 
           if (icw>0) cw_t(i,j,k) = -u(i,j,k)*cw_x(i,j,k) - v(i,j,k)*cw_y(i,j,k) 
 
!   vertical flux terms

           if (k>1) then
              tmp = half*what9(i,j,k)*r_prdif9(i,j,k)
              u_t (i,j,k) = u_t (i,j,k) - tmp*(u (i,j,k-1)-u (i,j,k))
              v_t (i,j,k) = v_t (i,j,k) - tmp*(v (i,j,k-1)-v (i,j,k))
              t_t (i,j,k) = t_t (i,j,k) - tmp*(t (i,j,k-1)-t (i,j,k))
              if (iq >0) q_t (i,j,k) = q_t (i,j,k) - tmp*(q (i,j,k-1)-q (i,j,k)) 
              if (ioz>0) oz_t(i,j,k) = oz_t(i,j,k) - tmp*(oz(i,j,k-1)-oz(i,j,k)) 
              if (icw>0) cw_t(i,j,k) = cw_t(i,j,k) - tmp*(cw(i,j,k-1)-cw(i,j,k)) 
           end if
           if (k<nsig) then
              tmp = half*what9(i,j,k+1)*r_prdif9(i,j,k)
              u_t (i,j,k) = u_t (i,j,k) - tmp*(u (i,j,k)-u (i,j,k+1))
              v_t (i,j,k) = v_t (i,j,k) - tmp*(v (i,j,k)-v (i,j,k+1))
              t_t (i,j,k) = t_t (i,j,k) - tmp*(t (i,j,k)-t (i,j,k+1))
              if (iq >0) q_t (i,j,k) = q_t (i,j,k) - tmp*(q (i,j,k)-q (i,j,k+1))
              if (ioz>0) oz_t(i,j,k) = oz_t(i,j,k) - tmp*(oz(i,j,k)-oz(i,j,k+1)) 
              if (icw>0) cw_t(i,j,k) = cw_t(i,j,k) - tmp*(cw(i,j,k)-cw(i,j,k+1)) 
           end if        
        end do  !end do j
      end do    !end do i

      do j=jtstart(kk),jtstop(kk)
        do i=1,lat2
           tmp = rd*t(i,j,k)*r_prsum9(i,j,k)
           tmp2 = prdif9(i,j,k)*r_prsum9(i,j,k)
           sumk=sumkm1(i,j)     + tmp*( pr_xdif9(i,j,k) - tmp2*pr_xsum9(i,j,k) )
           sumvk=sumvkm1(i,j)   + tmp*( pr_ydif9(i,j,k) - tmp2*pr_ysum9(i,j,k) )
           sum2k=sum2km1(i,j)   + t_x(i,j,k)*tmp2
           sum2vk=sum2vkm1(i,j) + t_y(i,j,k)*tmp2
 
           u_t(i,j,k) = u_t(i,j,k) - sumkm1(i,j) - rd*sum2km1(i,j) - sumk - rd*sum2k 
           v_t(i,j,k) = v_t(i,j,k) - sumvkm1(i,j) - rd*sum2vkm1(i,j) - sumvk - rd*sum2vk 
 

!   load up the km1 arrays for next k loop

           sumkm1(i,j)=sumk
           sumvkm1(i,j)=sumvk
           sum2km1(i,j)=sum2k
           sum2vkm1(i,j)=sum2vk
        end do
      end do
    end do  !end do k

    if(use_pbl)call turbl(u,v,pri,t,teta,z,u_t,v_t,t_t,jtstart(kk),jtstop(kk))

    if(.not.wrf_nmm_regional.and..not.nems_nmmb_regional)then
      do k=1,nsig

        do j=jtstart(kk),jtstop(kk)
           do i=1,lat2
              ix=istart(mype+1)+i-2
              if (ix == 1 .OR. ix == nlat) then
                 u_t(i,j,k)=zero
                 v_t(i,j,k)=zero
              end if
           end do 
        end do

      end do  !end do k
    end if

  end do
!  End of threading loop

  return

  contains
  subroutine init_vars_ (thiscase,bundle)

  type(gsi_bundle) :: bundle
  character(len=*),intent(in) :: thiscase

  character(len=*),parameter::myname_=myname//'*init_vars_'
  integer(i_kind) ier,istatus

!NOTE: this is not general - and in many ways it's wired just as the original
!      code. However, this can be easily generalized by a little re-write
!      of the main code in the subroutine above - TO BE DONE.

  icw =getindex(dvars3d,'cw')  
  ioz =getindex(dvars3d,'oz')  
  iq  =getindex(dvars3d,'q')   
  ip3d=getindex(dvars3d,'prse')

! If require guess vars available, extract from bundle ...
  if (trim(thiscase)=='guess') then
     ier=0
     call gsi_bundlegetpointer(bundle,'z' ,z   ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'u' ,u   ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'v' ,v   ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'tv',t   ,istatus)
     ier=ier+istatus
     if (iq>0) then
        call gsi_bundlegetpointer(bundle,'q' ,q   ,istatus)
        ier=ier+istatus
     endif
     if (ioz>0) then
        call gsi_bundlegetpointer(bundle,'oz',oz  ,istatus)
        ier=ier+istatus
     endif
     if (icw>0) then
        call gsi_bundlegetpointer(bundle,'cw',cw  ,istatus)
        if (istatus /=0) then
            cw => cwgues
            istatus=0
        endif 
        ier=ier+istatus
     endif
  endif
  if (trim(thiscase)=='xderivative') then
     ier=0
     call gsi_bundlegetpointer(bundle,'ps',ps_x,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'u' ,u_x ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'v' ,v_x ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'tv',t_x ,istatus)
     ier=ier+istatus
     if (iq>0) then
        call gsi_bundlegetpointer(bundle,'q' ,q_x ,istatus)
        ier=ier+istatus
     endif
     if (ioz>0) then
        call gsi_bundlegetpointer(bundle,'oz',oz_x,istatus)
        ier=ier+istatus
     endif
     if (icw>0) then
        call gsi_bundlegetpointer(bundle,'cw',cw_x,istatus)
        ier=ier+istatus
     endif
  endif
  if (trim(thiscase)=='yderivative') then
     ier=0
     call gsi_bundlegetpointer(bundle,'ps',ps_y,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'u' ,u_y ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'v' ,v_y ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'tv',t_y ,istatus)
     ier=ier+istatus
     if (iq>0) then
        call gsi_bundlegetpointer(bundle,'q' ,q_y ,istatus)
        ier=ier+istatus
     endif
     if (ioz>0) then
        call gsi_bundlegetpointer(bundle,'oz',oz_y,istatus)
        ier=ier+istatus
     endif
     if (icw>0) then
        call gsi_bundlegetpointer(bundle,'cw',cw_y,istatus)
        ier=ier+istatus
     endif
  endif
  if(trim(thiscase)=='tendency') then
     ier=0
     call gsi_bundlegetpointer(bundle,'u' ,u_t ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'v' ,v_t ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer(bundle,'tv',t_t ,istatus)
     ier=ier+istatus
     if (iq>0) then
        call gsi_bundlegetpointer(bundle,'q' ,q_t ,istatus)
        ier=ier+istatus
     endif
     if (ioz>0) then
        call gsi_bundlegetpointer(bundle,'oz',oz_t,istatus)
        ier=ier+istatus
     endif
     if (icw>0) then
        call gsi_bundlegetpointer(bundle,'cw',cw_t,istatus)
        ier=ier+istatus
     endif
     if (ip3d>0) then
        call gsi_bundlegetpointer(bundle,'prse',p_t,istatus)
        ier=ier+istatus
     endif
  endif
  if (ier/=0) then
      call die(myname_,': missing fields in '//trim(thiscase)//' ier=',ier)
  endif

  end subroutine init_vars_

end subroutine calctends
