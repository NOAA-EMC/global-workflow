subroutine get_derivatives (guess,xderivative,yderivative)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_derivatives  compute horizontal derivatives
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: get horizontal derivatives of state vector
!
! program history log:
!   2005-06-06  parrish
!   2005=07-10  kleist, clean up and fix skint
!   2008-06-04  safford - rm unused var nbad and uses
!   2010-03-25  zhu     - made changes to interface of sub2grid and grid2sub
!   2010-04-29  todling - update to use gsi_bundle
!   2010-05-22  todling - remove implicit assumption in ordering of nvar_id
!   2010-05-31  todling - no need to do pointer checking
!   2011-07-04  todling - allow run either single or double precision
!   2012-06-12  parrish - make changes to replace sub2grid/grid2sub with general_sub2grid/general_grid2sub.
!                         Remove arrays slndt, sicet, slndt_x, sicet_x, slndt_y, sicet_y,
!                         and variable nsig1o.
!   2013-10-19  todling - derivatives now in bundle
!   2014-12-13  derber  - Switch order of if and do statements to allow eventual
!                         threading and optimization
!
!   input argument list:
!     guess    - bundle holding guess fields
!
!   output argument list:
!     xderivative - longitudinal derivative
!     yderivative - latitudinal  derivative
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
  use constants, only: zero,max_varname_length
  use gridmod, only: regional,lat2,lon2,nsig
  use compact_diffs, only: compact_dlat,compact_dlon
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_bundlemod, only: gsi_bundleputvar
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundleinquire
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g_d
  use mpeu_util, only: die

  implicit none

! Passed variables
  type(gsi_bundle) :: guess
  type(gsi_bundle) :: xderivative
  type(gsi_bundle) :: yderivative

! Local Variables
  character(len=*),parameter::myname='get_derivatives'
  integer(i_kind) k,ic,ier,istatus
  real(r_kind),allocatable,dimension(:,:,:,:):: hwork,hworkd,hworke
  real(r_kind),dimension(:,:,:),pointer :: ptr3dges=>NULL()
  real(r_kind),dimension(:,:  ),pointer :: ptr2dges=>NULL()
  real(r_kind),dimension(:,:,:),pointer :: ptr3ddrv=>NULL()
  real(r_kind),dimension(:,:  ),pointer :: ptr2ddrv=>NULL()
  character(len=max_varname_length),dimension(:),allocatable:: dvars2d
  character(len=max_varname_length),dimension(:),allocatable:: dvars3d
  type(gsi_bundle):: work_bundle
  type(gsi_grid) :: grid

! inquire variable names in x(y)derivative

  if(xderivative%n2d>0) allocate(dvars2d(xderivative%n2d))
  if(xderivative%n3d>0) allocate(dvars3d(xderivative%n3d))
  call gsi_bundleinquire (xderivative,'shortnames::2d',dvars2d,istatus)
  ier=istatus
  call gsi_bundleinquire (xderivative,'shortnames::3d',dvars3d,istatus)
  ier=ier+istatus
  if(ier/=0) call die(myname,': cannot determine field names',ier)

  allocate(hwork (s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))
  allocate(hworkd(s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))
  allocate(hworke(s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))

!        use s2g_d%kend_alloc instead of s2g_d%kend_loc to force hworkd=0 even if not used on this pe

  ier=0
  call gsi_gridcreate(grid,lat2,lon2,nsig)

     call gsi_bundlecreate (work_bundle,grid,'derivatives work',ier, &
                            names2d=dvars2d,names3d=dvars3d,bundle_kind=r_kind)
     if(ier/=0) then
        write(6,*) myname, ': trouble creating work bundle'
        call stop2(999)
     endif

!    copy relevant fields from guess to work_bundle
     do ic=1,size(dvars2d)
        ier=0
        call gsi_bundlegetpointer (guess,      dvars2d(ic),ptr2dges,istatus)
        ier=ier+istatus
        call gsi_bundlegetpointer (work_bundle,dvars2d(ic),ptr2ddrv,istatus)
        ier=ier+istatus
        if(ier==0) ptr2ddrv=ptr2dges
     enddo
     do ic=1,size(dvars3d)
        ier=0
        call gsi_bundlegetpointer (guess,      dvars3d(ic),ptr3dges,istatus)
        ier=ier+istatus
        call gsi_bundlegetpointer (work_bundle,dvars3d(ic),ptr3ddrv,istatus)
        ier=ier+istatus
        if(ier==0) ptr3ddrv=ptr3dges
     enddo

     call general_sub2grid(s2g_d,work_bundle%values,hwork)

!    x derivative
     if(regional) then
        do k=s2g_d%kbegin_loc,s2g_d%kend_loc
!$omp parallel sections 
!$omp section
           call delx_reg(hwork(1,:,:,k),hworkd(1,:,:,k),s2g_d%vector(k))
!$omp section
           call dely_reg(hwork(1,:,:,k),hworke(1,:,:,k),s2g_d%vector(k))
!$omp end parallel sections
        end do
     else
        do k=s2g_d%kbegin_loc,s2g_d%kend_loc
!$omp parallel sections 
!$omp section
           call compact_dlon(hwork(1,:,:,k),hworkd(1,:,:,k),s2g_d%vector(k))
!$omp section
           call compact_dlat(hwork(1,:,:,k),hworke(1,:,:,k),s2g_d%vector(k))
!$omp end parallel sections
        end do
     end if
     call general_grid2sub(s2g_d,hworkd,xderivative%values)
     call general_grid2sub(s2g_d,hworke,yderivative%values)

     
!    clean work space
     call gsi_bundledestroy(work_bundle,ier)
     if(ier/=0) then
        write(6,*) myname, ': trouble destroying work bundle'
        call stop2(999)
     endif

  deallocate(hwork,hworkd,hworke)
  deallocate(dvars2d,dvars3d)

  return
end subroutine get_derivatives

subroutine tget_derivatives(guess,xderivative,yderivative)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tget_derivatives  adjoint of get_derivatives
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: adjoint of get_derivatives 
!
! program history log:
!   2005-06-06  parrish
!   2005-07-10  kleist, clean up
!   2008-06-04  safford - rm unused vars and uses
!   2010-04-29  todling - update to use gsi_bundle
!   2010-05-22  todling - remove implicit assumption in ordering of nvar_id
!   2010-05-31  todling - no need to do pointer checking
!   2012-06-12  parrish - make changes to replace sub2grid/grid2sub with general_sub2grid/general_grid2sub.
!                         Remove arrays slndt, sicet, slndt_x, sicet_x, slndt_y, sicet_y,
!                         and variable nsig1o.
!   2013-10-19  todling - derivatives now in bundle
!   2013-12-18  parrish - change "call delx_reg" to "call tdelx_reg" and "call dely_reg" to "call tdely_reg"
!
!   input argument list:
!     xderivatives - longitude derivatives
!     yderivatives - latitude derivatives
!
!   output argument list:
!     guess    - guess fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: zero,max_varname_length
  use gridmod, only: regional,lat2,lon2,nsig
  use compact_diffs, only: tcompact_dlat,tcompact_dlon
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_bundlemod, only: gsi_bundleputvar
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundleinquire
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g_d
  use mpeu_util, only: die
  implicit none

! Passed variables
  type(gsi_bundle) :: guess
  type(gsi_bundle) :: xderivative
  type(gsi_bundle) :: yderivative

! Local Variables
  character(len=*),parameter::myname='tget_derivatives'
  integer(i_kind) k,ic,ier,istatus
  real(r_kind),allocatable,dimension(:,:,:,:):: hwork,hwork2,hworkd,hworke
  real(r_kind),pointer    ,dimension(:,:,:)  :: ptr3dges=>NULL()
  real(r_kind),pointer    ,dimension(:,:,:)  :: ptr3ddrv=>NULL()
  real(r_kind),pointer    ,dimension(:,:)    :: ptr2dges=>NULL()
  real(r_kind),pointer    ,dimension(:,:)    :: ptr2ddrv=>NULL()
  character(len=max_varname_length),dimension(:),allocatable:: dvars2d
  character(len=max_varname_length),dimension(:),allocatable:: dvars3d
  type(gsi_bundle):: derivative
  type(gsi_grid):: grid

! inquire variable names in x(y)derivative

  if(xderivative%n2d>0) allocate(dvars2d(xderivative%n2d))
  if(xderivative%n3d>0) allocate(dvars3d(xderivative%n3d))
  call gsi_bundleinquire (xderivative,'shortnames::2d',dvars2d,istatus)
  ier=istatus
  call gsi_bundleinquire (xderivative,'shortnames::3d',dvars3d,istatus)
  ier=ier+istatus
  if(ier/=0) call die(myname,': cannot determine field names',ier)

!        use s2g_d%kend_alloc instead of s2g_d%kend_loc to force hworkd=0 even if not used on this pe

  allocate(hwork (s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))
  allocate(hwork2(s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))
  allocate(hworkd(s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))
  allocate(hworke(s2g_d%inner_vars,s2g_d%nlat,s2g_d%nlon,s2g_d%kbegin_loc:s2g_d%kend_alloc))

!             initialize hwork to zero, so can accumulate contribution from
!             all derivatives
  hwork=zero

!   adjoint of x and y derivative

  call general_sub2grid(s2g_d,yderivative%values,hworke)
  call general_sub2grid(s2g_d,xderivative%values,hworkd)
  if(regional) then
     do k=s2g_d%kbegin_loc,s2g_d%kend_loc
!$omp parallel sections 
!$omp section
        call tdely_reg(hworke(1,:,:,k),hwork2(1,:,:,k),s2g_d%vector(k))
!$omp section
        call tdelx_reg(hworkd(1,:,:,k),hwork(1,:,:,k),s2g_d%vector(k))
!$omp end parallel sections
     end do
  else
     do k=s2g_d%kbegin_loc,s2g_d%kend_loc
!$omp parallel sections 
!$omp section
        call tcompact_dlat(hwork2(1,:,:,k),hworke(1,:,:,k),s2g_d%vector(k))
!$omp section
        call tcompact_dlon(hwork(1,:,:,k),hworkd(1,:,:,k),s2g_d%vector(k))
!$omp end parallel sections
     end do
  end if
  hworkd=hworkd+hworke


  ier=0
  call gsi_gridcreate(grid,lat2,lon2,nsig)
  call gsi_bundlecreate (derivative,grid,'AD derivs work',ier, &
                         names2d=dvars2d,names3d=dvars3d,bundle_kind=r_kind)
  if(ier/=0) then
     write(6,*) myname, ': trouble creating work bundle'
     call stop2(999)
  endif

  call general_grid2sub(s2g_d,hworkd,derivative%values)

! Accumulate results
  do ic=1,size(dvars3d)
     ier=0
     call gsi_bundlegetpointer (guess,     dvars3d(ic),ptr3dges,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (derivative,dvars3d(ic),ptr3ddrv,istatus)
     ier=ier+istatus
     if(ier==0) ptr3dges = ptr3dges + ptr3ddrv
  enddo
  do ic=1,size(dvars2d)
     ier=0
     call gsi_bundlegetpointer (guess,     dvars2d(ic),ptr2dges,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (derivative,dvars2d(ic),ptr2ddrv,istatus)
     ier=ier+istatus
     if(ier==0) ptr2dges = ptr2dges + ptr2ddrv
  enddo

  call gsi_bundledestroy(derivative,ier)
  if(ier/=0) then
     write(6,*) myname, ': trouble destroying work bundle'
     call stop2(999)
  endif

  deallocate(hwork,hworkd,hwork2,hworke)
  deallocate(dvars2d,dvars3d)

end subroutine tget_derivatives


subroutine get_zderivs(z,z_x,z_y,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_zderivs    get derivatives of terrain
!   prgmmr: parrish          org: np22                date: 2005-09-29
!
! abstract: get derivatives od terrain field
!
! program history log:
!   2005-09-29  parrish
!   2005-12-05  todling - reorder passed variable declarations
!   2007-07-02  derber - modify for single time level and optimization
!   2008-06-04  safford - complete doc block
!   2010-04-01  treadon - move strip to gridmod
!
!   input argument list:
!     z         - terrain grid
!     mype      - integer task id
!     nfldsig   - number of time periods in terrain grid array
!
!   output argument list:
!     z_x       - zonal derivative of terrain field
!     z_y       - meridional derivative of terrain field
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,lat2,lon2
  use compact_diffs, only: compact_dlat,compact_dlon
  use general_sub2grid_mod, only: general_gather2grid,general_scatter2sub
  use general_commvars_mod, only: g1
  implicit none

! Passed variables
  integer(i_kind)                  ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2),intent(in   ) :: z
  real(r_kind),dimension(lat2,lon2),intent(  out) :: z_x,z_y

! Local variables
  real(r_kind),dimension(:,:,:),allocatable:: workh,workd1,workd2
  real(r_kind),dimension(:),allocatable:: z1,zx1,zy1
  integer(i_kind) i,ii,j
  integer(i_kind) workpe

  workpe=0

  allocate(workh (g1%inner_vars,g1%nlat,g1%nlon))
  allocate(workd1(g1%inner_vars,g1%nlat,g1%nlon))
  allocate(workd2(g1%inner_vars,g1%nlat,g1%nlon))
  allocate(z1(g1%inner_vars*g1%nlat*g1%nlon))
  allocate(zx1(g1%inner_vars*g1%nlat*g1%nlon))
  allocate(zy1(g1%inner_vars*g1%nlat*g1%nlon))

  ii=0
  do j=1,lon2
     do i=1,lat2
        ii=ii+1
        z1(ii)=z(i,j)
     end do
  end do
  call general_gather2grid(g1,z1,workh,workpe)
  deallocate(z1)

  if(mype==workpe) then
     if(regional) then
        call delx_reg(workh,workd1,(.false.))
        call dely_reg(workh,workd2,(.false.))
     else
        call compact_dlon(workh,workd1,(.false.))
        call compact_dlat(workh,workd2,(.false.))
     end if
  end if
  deallocate(workh)

  call general_scatter2sub(g1,workd1,zx1,workpe)
  call general_scatter2sub(g1,workd2,zy1,workpe)
  deallocate(workd1,workd2)
  ii=0
  do j=1,lon2
     do i=1,lat2
        ii=ii+1
        z_x(i,j)=zx1(ii)
        z_y(i,j)=zy1(ii)
     end do
  end do

  deallocate(zx1,zy1)


  return
end subroutine get_zderivs
