subroutine get_derivatives2(st,vp,t,p3d,u,v, &
                 u_x,v_x,t_x,p3d_x, &
                 u_y,v_y,t_y,p3d_y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_derivatives2  compute horizontal derivatives
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: get horizontal derivatives of state vector
!
! program history log:
!   2005-06-06  parrish
!   2005=07-10  kleist, clean up and fix skint
!   2009-04-21  derber - modify from get_derivatives to minimize data movement 
!               and work for mass and momentum only and calculate uv
!   2009-11-27  parrish - add uv_hyb_ens:  uv_hyb_ens=T, then st=u, vp=v
!   2010-01-04  safford - comment out $omp directives that produce inconsistent results 
!   2010-05-23  todling - trying to unwire index assumptions for sf and vp 
!   2012-02-08  kleist  - add uvflag to input arguments, remove ref to uv_hyb_ens parameter.
!   2012-06-12  parrish - significant reorganization to replace sub2grid2/grid2sub2 with
!                         general_sub2grid/general_grid2sub.
!   2014-12-03  derber - changes to reduce data movement (including call to
!                        stvp2uv and tstvp2uv
!
!   input argument list:
!     u        - longitude velocity component
!     v        - latitude velocity component
!     t        - virtual temperature
!     p        - ln(psfc)
!
!   output argument list:
!     u_x      - longitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - longitude derivative of v  (note: in global mode, undefined at pole points)
!     t_x      - longitude derivative of t
!     p_x      - longitude derivative of ln(psfc)
!     u_y      - latitude derivative of u  (note: in global mode, undefined at pole points)
!     v_y      - latitude derivative of v  (note: in global mode, undefined at pole points)
!     t_y      - latitude derivative of t
!     p_y      - latitude derivative of ln(psfc)
!
!   note:  u_x,v_x,u_y,v_y are not evaluated at the poles
!     all other derivatives are
!
!   for u and v, derivatives are following:
!
!     u_x:  (du/dlon)/(a*cos(lat))
!     u_y:  (d(u*cos(lat))/dlat)/(a*cos(lat))
!
!     v_x:  (dv/dlon)/(a*cos(lat))
!     v_y:  (d(v*cos(lat))/dlat)/(a*cos(lat))
!
!  for all other variables, derivatives are:
!
!     f_x:  (df/dlon)/(a*cos(lat))
!     f_y:  (df/dlat)/a
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig
  use compact_diffs, only: compact_dlat,compact_dlon,stvp2uv
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g4

  implicit none

! Passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: p3d
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(in   ) :: t,st,vp
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(  out) :: p3d_x,p3d_y
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(  out) :: t_x,u_x,v_x,u,v
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(  out) :: t_y,u_y,v_y

! Local Variables
  integer(i_kind) k,i,j,kk,k2
  real(r_kind),allocatable,dimension(:,:,:,:) :: hwork_sub,hwork,hwork_x,hwork_y
  real(r_kind),allocatable,dimension(:,:,:,:) :: hwork_subx,hwork_suby
  real(r_kind),dimension(nlat,nlon):: stx,vpx
  logical vector

  allocate(hwork_sub(2,s2g4%lat2,s2g4%lon2,s2g4%num_fields))
!$omp parallel do private(i,j,k,kk,k2)      
  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              hwork_sub(1,i,j,kk)=st(i,j,k)
              hwork_sub(2,i,j,kk)=vp(i,j,k)
           end do
        end do
     else
        k2=s2g4%lnames(2,kk)
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_sub(1,i,j,kk)=p3d(i,j,k)
                 hwork_sub(2,i,j,kk)=zero
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_sub(1,i,j,kk)=p3d(i,j,k)
                 hwork_sub(2,i,j,kk)=t(i,j,k)
              end do
           end do
        end if
     end if
  end do
  allocate(hwork(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))
  call general_sub2grid(s2g4,hwork_sub,hwork)
  allocate(hwork_x(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))
  allocate(hwork_y(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))

! x  and y derivative
  if(regional)then
     do k=s2g4%kbegin_loc,s2g4%kend_loc
        if(trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp') then
           do j=1,nlon
              do i=1,nlat
                 stx(i,j)=hwork(1,i,j,k)
                 vpx(i,j)=hwork(2,i,j,k)
              end do
           end do
           call psichi2uv_reg(stx,vpx,hwork(1,:,:,k),hwork(2,:,:,k))
        end if
     end do
!       !$omp parallel do private (k,vector)     ! ??????????fix this later
     do k=s2g4%kbegin_loc,s2g4%kend_loc
        vector=trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp'
        do j=1,nlon
           do i=1,nlat
              stx(i,j)=hwork(1,i,j,k)
              vpx(i,j)=hwork(2,i,j,k)
           end do
        end do
        call delx_reg(stx,hwork_x(1,:,:,k),vector)
        call dely_reg(stx,hwork_y(1,:,:,k),vector)
        call delx_reg(vpx,hwork_x(2,:,:,k),vector)
        call dely_reg(vpx,hwork_y(2,:,:,k),vector)
     end do
!        !$omp end parallel do                     ! ?????fix later

  else
     do k=s2g4%kbegin_loc,s2g4%kend_loc
        if(trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp') then
           call stvp2uv(hwork(1,1,1,k),s2g4%inner_vars)
        end if
     end do
     do k=s2g4%kbegin_loc,s2g4%kend_loc
        vector=trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp'
!$omp parallel sections
!$omp section
        call compact_dlon(hwork(1,:,:,k),hwork_x(1,:,:,k),vector)
!$omp section
        call compact_dlat(hwork(1,:,:,k),hwork_y(1,:,:,k),vector)
!$omp section
        call compact_dlon(hwork(2,:,:,k),hwork_x(2,:,:,k),vector)
!$omp section
        call compact_dlat(hwork(2,:,:,k),hwork_y(2,:,:,k),vector)
!$omp end parallel sections
     end do
  end if

  call general_grid2sub(s2g4,hwork,hwork_sub)
  deallocate(hwork)
  allocate(hwork_subx(2,s2g4%lat2,s2g4%lon2,s2g4%num_fields))
  call general_grid2sub(s2g4,hwork_x,hwork_subx)
  deallocate(hwork_x)
  allocate(hwork_suby(2,s2g4%lat2,s2g4%lon2,s2g4%num_fields))
  call general_grid2sub(s2g4,hwork_y,hwork_suby)
  deallocate(hwork_y)
!$omp parallel do private(i,j,k,kk,k2)      
  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              u(i,j,k)=hwork_sub(1,i,j,kk)
              v(i,j,k)=hwork_sub(2,i,j,kk)
              u_x(i,j,k)=hwork_subx(1,i,j,kk)
              v_x(i,j,k)=hwork_subx(2,i,j,kk)
              u_y(i,j,k)=hwork_suby(1,i,j,kk)
              v_y(i,j,k)=hwork_suby(2,i,j,kk)
           end do
        end do
     else
        k2=s2g4%lnames(2,kk)
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 p3d_x(i,j,k)=hwork_subx(1,i,j,kk)
                 p3d_y(i,j,k)=hwork_suby(1,i,j,kk)
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 p3d_x(i,j,k)=hwork_subx(1,i,j,kk)
                 t_x(i,j,k)=hwork_subx(2,i,j,kk)
                 p3d_y(i,j,k)=hwork_suby(1,i,j,kk)
                 t_y(i,j,k)=hwork_suby(2,i,j,kk)
              end do
           end do
        end if
     end if
  end do
  deallocate(hwork_sub,hwork_subx,hwork_suby)


  return
end subroutine get_derivatives2

subroutine tget_derivatives2(st,vp,t,p3d,u,v,&
                 u_x,v_x,t_x,p3d_x, &
                 u_y,v_y,t_y,p3d_y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tget_derivatives2  adjoint of get_derivatives
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: adjoint of get_derivatives 
!
! program history log:
!   2005-06-06  parrish
!   2005-07-10  kleist, clean up
!   2009-04-21  derber - modify from get_derivatives to minimize data movement 
!               and work for mass and momentum only and calculate uv
!   2009-11-27  parrish - add uv_hyb_ens:  uv_hyb_ens=T, then st=u, vp=v
!   2010-05-23  todling - trying to unwire index assumptions for sf and vp 
!   2012-02-08  kleist  - add uvflag to input arguments, remove ref to uv_hyb_ens parameter.
!   2012-06-12  parrish - significant reorganization to replace sub2grid2/grid2sub2 with
!                         general_sub2grid/general_grid2sub.
!
!   input argument list:
!     u_x      - longitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - longitude derivative of v  (note: in global mode, undefined at pole points)
!     t_x      - longitude derivative of t
!     p_x      - longitude derivative of ln(psfc)
!     u_y      - latitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - latitude derivative of v  (note: in global mode, undefined at pole points)
!     t_y      - latitude derivative of t
!     p_y      - latitude derivative of ln(psfc)
!
!   output argument list:
!     u        - longitude velocity component
!     v        - latitude velocity component
!     t        - virtual temperature
!     p        - ln(psfc)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!    adjoint of get_derivatives

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig
  use compact_diffs, only: tcompact_dlat,tcompact_dlon,tstvp2uv
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g4
  implicit none

! Passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t,st,vp
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d_x
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t_x,u_x,v_x,u,v
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d_y
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t_y,u_y,v_y

! Local Variables
  integer(i_kind) k,i,j,kk,k2
  real(r_kind),allocatable,dimension(:,:,:,:) :: hwork_sub,hwork,hwork_x,hwork_y
  real(r_kind),allocatable,dimension(:,:,:,:) :: hwork_subx,hwork_suby
  real(r_kind),dimension(nlat,nlon):: ux,vx
  logical vector

  allocate(hwork_sub(2,s2g4%lat2,s2g4%lon2,s2g4%num_fields))
  allocate(hwork_subx(2,s2g4%lat2,s2g4%lon2,s2g4%num_fields))
  allocate(hwork_suby(2,s2g4%lat2,s2g4%lon2,s2g4%num_fields))

  hwork_sub = zero
!$omp parallel do private(i,j,k,kk,k2)      
  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              hwork_sub(1,i,j,kk)=u(i,j,k)
              hwork_sub(2,i,j,kk)=v(i,j,k)
              hwork_subx(1,i,j,kk)=u_x(i,j,k)
              hwork_subx(2,i,j,kk)=v_x(i,j,k)
              hwork_suby(1,i,j,kk)=u_y(i,j,k)
              hwork_suby(2,i,j,kk)=v_y(i,j,k)
           end do
        end do
     else
        k2=s2g4%lnames(2,kk)
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_subx(1,i,j,kk)=p3d_x(i,j,k)
                 hwork_subx(2,i,j,kk)=zero
                 hwork_suby(1,i,j,kk)=p3d_y(i,j,k)
                 hwork_suby(2,i,j,kk)=zero
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 hwork_subx(1,i,j,kk)=p3d_x(i,j,k)
                 hwork_subx(2,i,j,kk)=t_x(i,j,k)
                 hwork_suby(1,i,j,kk)=p3d_y(i,j,k)
                 hwork_suby(2,i,j,kk)=t_y(i,j,k)
              end do
           end do
        end if
     end if
  end do
  allocate(hwork_x(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))
  call general_sub2grid(s2g4,hwork_subx,hwork_x)
  deallocate(hwork_subx)
  allocate(hwork_y(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))
  call general_sub2grid(s2g4,hwork_suby,hwork_y)
  deallocate(hwork_suby)
!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives
  allocate(hwork(s2g4%inner_vars,s2g4%nlat,s2g4%nlon,s2g4%kbegin_loc:s2g4%kend_alloc))
  hwork = zero
  call general_sub2grid(s2g4,hwork_sub,hwork)

  if(regional)then
!       !$omp parallel do private (k,vector)     ! ??????????fix this later
     do k=s2g4%kbegin_loc,s2g4%kend_loc
        vector=trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp'
        if(vector) then
           ux=zero
           vx=zero
           call tdelx_reg(hwork_x(1,:,:,k),ux,vector)
           call tdely_reg(hwork_y(1,:,:,k),ux,vector)
           call tdelx_reg(hwork_x(2,:,:,k),vx,vector)
           call tdely_reg(hwork_y(2,:,:,k),vx,vector)
           call psichi2uvt_reg(ux,vx,hwork(1,:,:,k),hwork(2,:,:,k))
        else
           call tdelx_reg(hwork_x(1,:,:,k),hwork(1,:,:,k),vector)
           call tdely_reg(hwork_y(1,:,:,k),hwork(1,:,:,k),vector)
           call tdelx_reg(hwork_x(2,:,:,k),hwork(2,:,:,k),vector)
           call tdely_reg(hwork_y(2,:,:,k),hwork(2,:,:,k),vector)
        end if
     end do
  else
     do k=s2g4%kbegin_loc,s2g4%kend_loc
        vector=trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp'
!$omp parallel sections
!$omp section
        call tcompact_dlon(hwork(1,:,:,k),hwork_x(1,:,:,k),vector)
        call tcompact_dlat(hwork(1,:,:,k),hwork_y(1,:,:,k),vector)
!$omp section
        call tcompact_dlon(hwork(2,:,:,k),hwork_x(2,:,:,k),vector)
        call tcompact_dlat(hwork(2,:,:,k),hwork_y(2,:,:,k),vector)
!$omp end parallel sections
     end do
!       !$omp end parallel do                        ! ???fix later
     do k=s2g4%kbegin_loc,s2g4%kend_loc
       if(trim(s2g4%names(1,k))=='sf'.and.trim(s2g4%names(2,k))=='vp') then
           call tstvp2uv(hwork(1,1,1,k),s2g4%inner_vars)
        end if
     end do
  end if
  deallocate(hwork_x,hwork_y)

!     use t_x,etc since don't need to save contents
  call general_grid2sub(s2g4,hwork,hwork_sub)
  deallocate(hwork)
!$omp parallel do private(i,j,k,kk,k2)      
  do kk=1,s2g4%num_fields
     k=s2g4%lnames(1,kk)
     if(trim(s2g4%names(1,kk))=='sf'.and.trim(s2g4%names(2,kk))=='vp') then
        do j=1,s2g4%lon2
           do i=1,s2g4%lat2
              st(i,j,k)=st(i,j,k)+hwork_sub(1,i,j,kk)
              vp(i,j,k)=vp(i,j,k)+hwork_sub(2,i,j,kk)
           end do
        end do
     else
        k2=s2g4%lnames(2,kk)
        if(k2==0) then          !  p3d level nsig+1 where there is no corresponding t value
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 p3d(i,j,k)=p3d(i,j,k)+hwork_sub(1,i,j,kk)
              end do
           end do
        else
           do j=1,s2g4%lon2
              do i=1,s2g4%lat2
                 p3d(i,j,k)=p3d(i,j,k)+hwork_sub(1,i,j,kk)
                 t(i,j,k)=t(i,j,k)+hwork_sub(2,i,j,kk)
              end do
           end do
        end if
     end if
  end do
  deallocate(hwork_sub)

end subroutine tget_derivatives2
subroutine get_derivatives2uv(st,vp,t,p3d,u,v, &
                 u_x,v_x,t_x,p3d_x, &
                 u_y,v_y,t_y,p3d_y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_derivatives2uv  compute horizontal derivatives when uv is
! supplied - faster than get_derivatives2
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: get horizontal derivatives of state vector
!
! program history log:
!   2015-12-15  derber - modified from get_derivatives2 to speed up when uv
!   supplied rather than streamfunction and velocity potential
!                        stvp2uv and tstvp2uv
!
!   input argument list:
!     u        - longitude velocity component
!     v        - latitude velocity component
!     t        - virtual temperature
!     p        - ln(psfc)
!
!   output argument list:
!     u_x      - longitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - longitude derivative of v  (note: in global mode, undefined at pole points)
!     t_x      - longitude derivative of t
!     p_x      - longitude derivative of ln(psfc)
!     u_y      - latitude derivative of u  (note: in global mode, undefined at pole points)
!     v_y      - latitude derivative of v  (note: in global mode, undefined at pole points)
!     t_y      - latitude derivative of t
!     p_y      - latitude derivative of ln(psfc)
!
!   note:  u_x,v_x,u_y,v_y are not evaluated at the poles
!     all other derivatives are
!
!   for u and v, derivatives are following:
!
!     u_x:  (du/dlon)/(a*cos(lat))
!     u_y:  (d(u*cos(lat))/dlat)/(a*cos(lat))
!
!     v_x:  (dv/dlon)/(a*cos(lat))
!     v_y:  (d(v*cos(lat))/dlat)/(a*cos(lat))
!
!  for all other variables, derivatives are:
!
!     f_x:  (df/dlon)/(a*cos(lat))
!     f_y:  (df/dlat)/a
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig
  use compact_diffs, only: compact_dlat,compact_dlon
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s1g4

  implicit none

! Passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: p3d
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(in   ) :: t,st,vp
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(  out) :: p3d_x,p3d_y
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(  out) :: t_x,u_x,v_x,u,v
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(  out) :: t_y,u_y,v_y

! Local Variables
  integer(i_kind) k,i,j,kk
  real(r_kind),allocatable,dimension(:,:,:,:) :: hwork_sub,hwork,hwork_x,hwork_y
  real(r_kind),allocatable,dimension(:,:,:,:) :: hwork_suby
  logical vector

  allocate(hwork_sub(1,s1g4%lat2,s1g4%lon2,s1g4%num_fields))
!$omp parallel do private(i,j,k,kk)      
  do kk=1,s1g4%num_fields
     k=s1g4%lnames(1,kk)
     if(trim(s1g4%names(1,kk))=='u') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              hwork_sub(1,i,j,kk)=st(i,j,k)
              u(i,j,k)=st(i,j,k)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='v') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              hwork_sub(1,i,j,kk)=vp(i,j,k)
              v(i,j,k)=vp(i,j,k)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='t') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              hwork_sub(1,i,j,kk)=t(i,j,k)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='prse') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              hwork_sub(1,i,j,kk)=p3d(i,j,k)
           end do
        end do
     end if
  end do
  allocate(hwork(s1g4%inner_vars,s1g4%nlat,s1g4%nlon,s1g4%kbegin_loc:s1g4%kend_alloc))
  call general_sub2grid(s1g4,hwork_sub,hwork)
  allocate(hwork_x(s1g4%inner_vars,s1g4%nlat,s1g4%nlon,s1g4%kbegin_loc:s1g4%kend_alloc))
  allocate(hwork_y(s1g4%inner_vars,s1g4%nlat,s1g4%nlon,s1g4%kbegin_loc:s1g4%kend_alloc))

! x  and y derivative
  if(regional)then
     do k=s1g4%kbegin_loc,s1g4%kend_loc
        vector=trim(s1g4%names(1,k))=='u'.or.trim(s1g4%names(1,k))=='v'
        call delx_reg(hwork(1,:,:,k),hwork_x(1,:,:,k),vector)
        call dely_reg(hwork(1,:,:,k),hwork_y(1,:,:,k),vector)
     end do

  else
     do k=s1g4%kbegin_loc,s1g4%kend_loc
        vector=trim(s1g4%names(1,k))=='u'.or.trim(s1g4%names(1,k))=='v'
!$omp parallel sections
!$omp section
        call compact_dlon(hwork(1,:,:,k),hwork_x(1,:,:,k),vector)
!$omp section
        call compact_dlat(hwork(1,:,:,k),hwork_y(1,:,:,k),vector)
!$omp end parallel sections
     end do
  end if

  deallocate(hwork)
  call general_grid2sub(s1g4,hwork_x,hwork_sub)
  deallocate(hwork_x)
  allocate(hwork_suby(1,s1g4%lat2,s1g4%lon2,s1g4%num_fields))
  call general_grid2sub(s1g4,hwork_y,hwork_suby)
  deallocate(hwork_y)
!$omp parallel do private(i,j,k,kk)      
  do kk=1,s1g4%num_fields
     k=s1g4%lnames(1,kk)
     if(trim(s1g4%names(1,kk))=='u') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              u_x(i,j,k)=hwork_sub(1,i,j,kk)
              u_y(i,j,k)=hwork_suby(1,i,j,kk)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='v') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              v_x(i,j,k)=hwork_sub(1,i,j,kk)
              v_y(i,j,k)=hwork_suby(1,i,j,kk)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='t') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              t_x(i,j,k)=hwork_sub(1,i,j,kk)
              t_y(i,j,k)=hwork_suby(1,i,j,kk)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='prse') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              p3d_x(i,j,k)=hwork_sub(1,i,j,kk)
              p3d_y(i,j,k)=hwork_suby(1,i,j,kk)
           end do
        end do
     end if
  end do
  deallocate(hwork_sub,hwork_suby)


  return
end subroutine get_derivatives2uv

subroutine tget_derivatives2uv(st,vp,t,p3d,u,v,&
                 u_x,v_x,t_x,p3d_x, &
                 u_y,v_y,t_y,p3d_y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tget_derivatives2uv  adjoint of get_derivatives2uv
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: adjoint of get_derivatives 
!
! program history log:
!   2015-02-25  derber - modify from tget_derivatives2 for more efficiency
!
!   input argument list:
!     u_x      - longitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - longitude derivative of v  (note: in global mode, undefined at pole points)
!     t_x      - longitude derivative of t
!     p_x      - longitude derivative of ln(psfc)
!     u_y      - latitude derivative of u  (note: in global mode, undefined at pole points)
!     v_x      - latitude derivative of v  (note: in global mode, undefined at pole points)
!     t_y      - latitude derivative of t
!     p_y      - latitude derivative of ln(psfc)
!
!   output argument list:
!     u        - longitude velocity component
!     v        - latitude velocity component
!     t        - virtual temperature
!     p        - ln(psfc)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!    adjoint of get_derivatives

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,nlat,nlon,lat2,lon2,nsig
  use compact_diffs, only: tcompact_dlat,tcompact_dlon
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s1g4
  implicit none

! Passed variables
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t,st,vp
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d_x
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t_x,u_x,v_x,u,v
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout) :: p3d_y
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: t_y,u_y,v_y

! Local Variables
  integer(i_kind) k,i,j,kk
  real(r_kind),allocatable,dimension(:,:,:,:) :: hwork_sub,hwork,hwork_x,hwork_y
  real(r_kind),allocatable,dimension(:,:,:,:) :: hwork_suby
  real(r_kind),dimension(s1g4%nlat,s1g4%nlon) :: tmp1,tmp2
  logical vector

  allocate(hwork_sub(1,s1g4%lat2,s1g4%lon2,s1g4%num_fields))
  allocate(hwork_suby(1,s1g4%lat2,s1g4%lon2,s1g4%num_fields))

!$omp parallel do private(i,j,k,kk)      
  do kk=1,s1g4%num_fields
     k=s1g4%lnames(1,kk)
     if(trim(s1g4%names(1,kk))=='u') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              hwork_sub(1,i,j,kk)=u_x(i,j,k)
              hwork_suby(1,i,j,kk)=u_y(i,j,k)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='v') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              hwork_sub(1,i,j,kk)=v_x(i,j,k)
              hwork_suby(1,i,j,kk)=v_y(i,j,k)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='t') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              hwork_sub(1,i,j,kk)=t_x(i,j,k)
              hwork_suby(1,i,j,kk)=t_y(i,j,k)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='prse') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              hwork_sub(1,i,j,kk)=p3d_x(i,j,k)
              hwork_suby(1,i,j,kk)=p3d_y(i,j,k)
           end do
        end do
     end if
  end do
  allocate(hwork_x(s1g4%inner_vars,s1g4%nlat,s1g4%nlon,s1g4%kbegin_loc:s1g4%kend_alloc))
  call general_sub2grid(s1g4,hwork_sub,hwork_x)
  allocate(hwork_y(s1g4%inner_vars,s1g4%nlat,s1g4%nlon,s1g4%kbegin_loc:s1g4%kend_alloc))
  call general_sub2grid(s1g4,hwork_suby,hwork_y)
  deallocate(hwork_suby)
  allocate(hwork(s1g4%inner_vars,s1g4%nlat,s1g4%nlon,s1g4%kbegin_loc:s1g4%kend_alloc))
!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives

  if(regional)then
     do k=s1g4%kbegin_loc,s1g4%kend_loc
        vector=trim(s1g4%names(1,k))=='u'.or.trim(s1g4%names(1,k))=='v'
!$omp parallel sections
!$omp section
        tmp1=zero
        call tdelx_reg(hwork_x(1,:,:,k),tmp1,vector)
!$omp section
        tmp2=zero
        call tdely_reg(hwork_y(1,:,:,k),tmp2,vector)
!$omp end parallel sections
        hwork(1,:,:,k)=tmp1(:,:)+tmp2(:,:)
     end do
  else
     do k=s1g4%kbegin_loc,s1g4%kend_loc
        vector=trim(s1g4%names(1,k))=='u'.or.trim(s1g4%names(1,k))=='v'
!$omp parallel sections
!$omp section
        tmp1=zero
        call tcompact_dlon(tmp1,hwork_x(1,:,:,k),vector)
!$omp section
        tmp2=zero
        call tcompact_dlat(tmp2,hwork_y(1,:,:,k),vector)
!$omp end parallel sections
        hwork(1,:,:,k)=tmp1(:,:)+tmp2(:,:)
     end do
  end if
  deallocate(hwork_x,hwork_y)

  call general_grid2sub(s1g4,hwork,hwork_sub)
  deallocate(hwork)
!$omp parallel do private(i,j,k,kk)      
  do kk=1,s1g4%num_fields
     k=s1g4%lnames(1,kk)
     if(trim(s1g4%names(1,kk))=='u') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              st(i,j,k)=st(i,j,k)+hwork_sub(1,i,j,kk)+u(i,j,k)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='v') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              vp(i,j,k)=vp(i,j,k)+hwork_sub(1,i,j,kk)+v(i,j,k)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='t') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              t(i,j,k)=t(i,j,k)+hwork_sub(1,i,j,kk)
           end do
        end do
     else if(trim(s1g4%names(1,kk))=='prse') then
        do j=1,s1g4%lon2
           do i=1,s1g4%lat2
              p3d(i,j,k)=p3d(i,j,k)+hwork_sub(1,i,j,kk)
           end do
        end do
     end if
  end do
  deallocate(hwork_sub)

end subroutine tget_derivatives2uv
