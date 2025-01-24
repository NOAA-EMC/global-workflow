! following contains library for version 3 of rtlnmc (regional tangent linear normal mode constraint.
!  
!

module helmholtz_fmg_mod

!  multigrid solver on fixed domain with 5 point stencil and dirichlet bc.

!     TO DO:

!   coded       tested     (NOTE:  pay special attention to boundary treatment--likely several errors to fix)

!  20090215    20090215         grid heirarchy generator
!  20090216    20090216         transfer operators
!  20090216    20090216         bilinear interpolation operator
!  20090216    20090216         bicubic  interpolation operaror
!  20090216    20090217         forward operator  ( f = delsqr(v) - r*v )
!  20090216    20090220         relaxation operator
!  20090216    20090225         fmg algorithm
!  20090227    20090309         fmg_ad

  use kinds, only: r_kind,i_kind
  implicit none
  private
  public :: mgvar
  public :: generate_grids
  public :: check_xbdy_err
  public :: check_ybdy_err
  public :: f2c_full_weight
  public :: c2f_bilinear
  public :: c2f_bicubic
  public :: init_mgvars
  public :: fmg
  public :: fmg_ad

  type mgvar

    sequence
    integer(i_kind) nx                                ! x dimension is 0:nx       east-west
    integer(i_kind) ny                                ! y dimension is 0:ny       north-south
    real(r_kind)    phi0                              ! scale geopotential
    real(r_kind) a_east,b_east,a_east_inv             !  for dirichlet boundary conditions, interpolation
    real(r_kind) a_north,b_north,a_north_inv          !  boundary weights on east and north bdy
    real(r_kind) b_over_a_east,b_over_a_east_neg      !  boundary values (note, with fmg these all = 0 except
    real(r_kind) b_over_a_north,b_over_a_north_neg    !   on finest grid, where solution is desired.
    real(r_kind) half_b_over_a_east_neg               !  but this code is specific to zero bc.
    real(r_kind) half_b_over_a_north_neg              !
    real(r_kind) half_b_over_a_east_north_neg         !  boundary formula (weights are for lin interp):
                                                      !
                                                      !  a_east*v(j,nx)+b_east*v(j,nx-1) = 0
                                                      !               for j=1,ny-1
                                                      !
                                                      !  a_north*v(ny,i)+b_north*v(ny-1,i) = 0
                                                      !               for i=1,nx-1

    real(r_kind),pointer:: f(:,:)      => NULL()    !  residual (forcing on finest grid)
    real(r_kind),pointer:: v(:,:)      => NULL()    !  correction (solution on finest grid)
    real(r_kind),pointer:: w(:,:)      => NULL()    !  scratch space
    real(r_kind),pointer:: w2(:,:)     => NULL()    !  scratch space
    real(r_kind),pointer:: y(:,:)      => NULL()    !  scratch space
    real(r_kind),pointer:: z(:,:)      => NULL()    !  scratch space
    real(r_kind),pointer:: yy(:,:)     => NULL()    !  scratch space
    real(r_kind),pointer:: zz(:,:)     => NULL()    !  scratch space
    real(r_kind),pointer:: x00(:,:)       => NULL() ! increment in x direction (meters)
    real(r_kind),pointer:: y00(:,:)       => NULL() ! increment in y direction (meters)
    real(r_kind),pointer:: h00(:,:)       => NULL() ! all forward
    real(r_kind),pointer:: r00(:,:)       => NULL() !  operator constants
    real(r_kind),pointer:: h0m(:,:)       => NULL() ! all forward
    real(r_kind),pointer:: h0p(:,:)       => NULL() !  operator constants
    real(r_kind),pointer:: hm0(:,:)       => NULL() ! all forward
    real(r_kind),pointer:: hp0(:,:)       => NULL() !  operator constants
    real(r_kind),pointer:: dbarx(:,:)     => NULL() !  x direction  (for poisson problem)
    real(r_kind),pointer:: ubarx(:,:)     => NULL() !   adi
    real(r_kind),pointer:: obarx(:,:)     => NULL() !    coefs
    real(r_kind),pointer:: dbary(:,:)     => NULL() !  y direction
    real(r_kind),pointer:: ubary(:,:)     => NULL() !   adi
    real(r_kind),pointer:: obary(:,:)     => NULL() !    coefs
    real(r_kind),pointer:: dbarxh(:,:)    => NULL() !  x direction  (for helmholtz problem)
    real(r_kind),pointer:: ubarxh(:,:)    => NULL() !   adi
    real(r_kind),pointer:: obarxh(:,:)    => NULL() !    coefs
    real(r_kind),pointer:: dbaryh(:,:)    => NULL() !  y direction
    real(r_kind),pointer:: ubaryh(:,:)    => NULL() !   adi
    real(r_kind),pointer:: obaryh(:,:)    => NULL() !    coefs
    logical,pointer:: bicubic_mask(:,:)   => NULL() !  flags points that are covered by bicubic interpolation
    
  end type mgvar

  integer(i_kind) mgridx,mgridy                     ! number of possible grids in each direction
  integer(i_kind) mgrid                             ! number of grids used in multigrid solution
                                                    !    ( = min(mgridx,mgridy)

  integer(i_kind) nxall(100),nyall(100)             ! dimensions of each grid
  integer(i_kind) nxa,nya                           ! dimensions of analysis grid  (nya,nxa)
                                                    !  nxall(1)
  type(mgvar),save:: mg(100)

contains

  subroutine generate_grids(nxa_in,nya_in)

    use constants, only: half,one
    integer(i_kind),intent(in):: nxa_in,nya_in

!      grids have south and west row in common, but allow north and east row to go one grid point beyond
!      north and east boundary of finest grid.  by doing this, it is not necessary to have any particular
!      number of points, ie 2**p+1, so that all boundaries are part of every grid.  except for the north
!      and east points outside the analysis domain, all interior points of all grids coincide with points
!      of the target analysis grid.

!  w                                                                   e
!  .....................................................................
!  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  .       .       .       .       .       .       .       .       .   x   .
!  .               .               .               .               .   x           . 
!  .                               .                               .   x                           .

    integer(i_kind) k
    real(r_kind) a_end(100),b_end(100)

    nxa=nxa_in
    nya=nya_in
    call generate_1d_grid(nxa-1,nxall,mgridx,a_end,b_end)
    do k=1,mgridx
      mg(k)%nx=nxall(k)
      mg(k)%a_east=a_end(k)
      mg(k)%a_east_inv=one/a_end(k)
      mg(k)%b_east=b_end(k)
      mg(k)%b_over_a_east=b_end(k)/a_end(k)
      mg(k)%b_over_a_east_neg=-b_end(k)/a_end(k)
      mg(k)%half_b_over_a_east_neg=-half*b_end(k)/a_end(k)
    end do
    call generate_1d_grid(nya-1,nyall,mgridy,a_end,b_end)
    do k=1,mgridy
      mg(k)%ny=nyall(k)
      mg(k)%a_north=a_end(k)
      mg(k)%a_north_inv=one/a_end(k)
      mg(k)%b_north=b_end(k)
      mg(k)%b_over_a_north=b_end(k)/a_end(k)
      mg(k)%b_over_a_north_neg=-b_end(k)/a_end(k)
      mg(k)%half_b_over_a_north_neg=-half*b_end(k)/a_end(k)
    end do
    mgrid=min(mgridx,mgridy)
    do k=1,mgrid
      mg(k)%half_b_over_a_east_north_neg=mg(k)%half_b_over_a_east_neg+mg(k)%half_b_over_a_north_neg
    end do
      write(6,*)' in generate_grids, mgridx,mgridy,mgrid=',mgridx,mgridy,mgrid
      do k=1,mgrid
        write(6,'(" k,nx,a_east,a_east_inv,b_east=",i3,i5,3f14.7)') &
                   k,mg(k)%nx,mg(k)%a_east,mg(k)%a_east_inv,mg(k)%b_east
      end do
      do k=1,mgrid
        write(6,'(" k,ny,a_north,a_north_inv,b_north=",i3,i5,3f14.7)') &
                   k,mg(k)%ny,mg(k)%a_north,mg(k)%a_north_inv,mg(k)%b_north
      end do

  end subroutine generate_grids

  subroutine generate_1d_grid(nxf,nxall,mgridx,a_east,b_east)

    use constants, only: zero,one

    integer(i_kind),intent(in):: nxf
    integer(i_kind),intent(out):: mgridx
    integer(i_kind),intent(out):: nxall(100)
    real(r_kind),intent(out)::    a_east(100),b_east(100)

    integer(i_kind) ifactor,k,next
    real(r_kind) end,thisend,factor

    nxall(1)=nxf
    end=nxall(1)
    k=1
    ifactor=1
    a_east(1)=one
    b_east(1)=zero
    do 
      next=nxall(k)/2
      ifactor=2*ifactor
      if(next*ifactor <  nxall(1)) next=next+1
      if(next <  3) exit
      factor=ifactor
      thisend=next*factor
      k=k+1
      nxall(k)=next
      b_east(k)=(thisend-end)/factor
      a_east(k)=one-b_east(k)
    end do
    mgridx=k

  end subroutine generate_1d_grid

  subroutine check_xbdy_err

    use constants, only: zero,one,two

    real(r_kind) factor,xend,errmax,xend_interp
    integer(i_kind) k

    factor=one
    xend=mg(1)%nx
    errmax=zero
    do k=1,mgridx
      xend_interp=factor*mg(k)%nx*mg(k)%a_east+factor*(mg(k)%nx-one)*mg(k)%b_east
      errmax=max(abs(xend_interp-xend),errmax)
      factor=two*factor
    end do
    write(6,*)' for nx=',mg(1)%nx,', mgridx,mgrid,east errmax=',mgridx,mgrid,errmax

  end subroutine check_xbdy_err

  subroutine check_ybdy_err

    use constants, only: zero,one,two

    real(r_kind) factor,yend,errmax,yend_interp
    integer(i_kind) k

    factor=one
    yend=mg(1)%ny
    errmax=zero
    do k=1,mgridy
      yend_interp=factor*mg(k)%ny*mg(k)%a_north+factor*(mg(k)%ny-one)*mg(k)%b_north
      errmax=max(abs(yend_interp-yend),errmax)
      factor=two*factor
    end do
    write(6,*)' for ny=',mg(1)%ny,', mgridy,mgrid,north errmax=',mgridy,mgrid,errmax

  end subroutine check_ybdy_err

  subroutine init_mgvars(region_dx,region_dy,region_lat,phi0)

    use constants, only: zero,half,one,two,omega

    real(r_kind),dimension(0:nya-1,0:nxa-1),intent(in):: region_dx,region_dy,region_lat
    real(r_kind),intent(in):: phi0

    integer(i_kind) i,j,nx,ny
    integer(i_kind) k,jump
    integer(i_kind) ii,im,ip,jj,jm,jp
    real(r_kind) x00,y00,x0p,x0m,xp0,xm0,y0p,y0m,yp0,ym0

    real(r_kind),dimension(0:nya-1,0:nxa-1):: dx_fine,dy_fine,coriolis_fine
    real(r_kind),allocatable:: d(:),dh(:),u(:),o(:)
    real(r_kind),allocatable:: dbar(:),ubar(:),obar(:)
    real(r_kind),allocatable:: dbarh(:),ubarh(:),obarh(:)
    integer(i_kind) ibad,jbad
    integer(i_kind) ic,if,jc,jf

    nx=mg(1)%nx ; ny=mg(1)%ny
    if(nx /= nxa-1.or.ny /= nya-1) then
      write(6,*)' inconsistent args in init_mgvars, nx,nxa-1,ny,nya-1=',nx,nxa-1,ny,nya-1
      stop
    end if
    do i=0,nx
      do j=0,ny
        dx_fine(j,i)=region_dx(j,i)
        dy_fine(j,i)=region_dy(j,i)
        coriolis_fine(j,i)=two*omega*sin(region_lat(j,i))
      end do
    end do

!       allocate grids at all levels

    do k=1,mgrid
      nx=mg(k)%nx ; ny=mg(k)%ny
      mg(k)%phi0=phi0
      allocate(mg(k)%f(0:ny,0:nx),mg(k)%v(0:ny,0:nx),mg(k)%w(0:ny,0:nx))
      allocate(mg(k)%w2(0:ny,0:nx))
      allocate(mg(k)%y(0:ny,0:nx),mg(k)%z(0:ny,0:nx))
      allocate(mg(k)%yy(0:ny,0:nx),mg(k)%zz(0:ny,0:nx))
      allocate(mg(k)%x00(0:ny,0:nx),mg(k)%y00(0:ny,0:nx))
      allocate(mg(k)%h00(0:ny,0:nx),mg(k)%r00(0:ny,0:nx))
      allocate(mg(k)%h0m(0:ny,0:nx),mg(k)%h0p(0:ny,0:nx))
      allocate(mg(k)%hm0(0:ny,0:nx),mg(k)%hp0(0:ny,0:nx))
      allocate(mg(k)%dbarx(0:ny,0:nx),mg(k)%dbary(0:ny,0:nx))
      allocate(mg(k)%ubarx(0:ny,0:nx),mg(k)%ubary(0:ny,0:nx))
      allocate(mg(k)%obarx(0:ny,0:nx),mg(k)%obary(0:ny,0:nx))
      allocate(mg(k)%dbarxh(0:ny,0:nx),mg(k)%dbaryh(0:ny,0:nx))
      allocate(mg(k)%ubarxh(0:ny,0:nx),mg(k)%ubaryh(0:ny,0:nx))
      allocate(mg(k)%obarxh(0:ny,0:nx),mg(k)%obaryh(0:ny,0:nx))
      allocate(mg(k)%bicubic_mask(0:ny,0:nx))
      mg(k)%f=zero ; mg(k)%v=zero ; mg(k)%w=zero ; mg(k)%y=zero ; mg(k)%z=zero
      mg(k)%x00=zero ; mg(k)%y00=zero
      mg(k)%h00=zero ; mg(k)%r00=zero
      mg(k)%h0m=zero ; mg(k)%h0p=zero ; mg(k)%hm0=zero ; mg(k)%hp0=zero
      mg(k)%dbarx=zero ; mg(k)%dbary=zero
      mg(k)%ubarx=zero ; mg(k)%ubary=zero
      mg(k)%obarx=zero ; mg(k)%obary=zero
      mg(k)%dbarxh=zero ; mg(k)%dbaryh=zero
      mg(k)%ubarxh=zero ; mg(k)%ubaryh=zero
      mg(k)%obarxh=zero ; mg(k)%obaryh=zero
      mg(k)%bicubic_mask=.false.
    end do

!      create x00, y00 and r00 on each grid

    do k=1,mgrid
      nx=nxall(k) ; ny=nyall(k)
      jump=2**(k-1)
      ibad=0
      jbad=0
      i=-1
      do ii=0,nxall(1),jump
        i=i+1
        j=-1
        do jj=0,nyall(1),jump
          j=j+1
          mg(k)%x00(j,i)=dx_fine(jj,ii)*jump
          mg(k)%y00(j,i)=dy_fine(jj,ii)*jump
          mg(k)%r00(j,i)=coriolis_fine(jj,ii)**2/mg(k)%phi0
        end do
        if(j <  nyall(k)-1.or.j >  nyall(k)) jbad=jbad+1
        if(j == nyall(k)-1) then
          j=nyall(k)
          jj=nyall(1)
          mg(k)%x00(j,i)=dx_fine(jj,ii)*jump
          mg(k)%y00(j,i)=dy_fine(jj,ii)*jump
          mg(k)%r00(j,i)=coriolis_fine(jj,ii)**2/mg(k)%phi0
        end if
      end do
      if(i <  nxall(k)-1.or.i >  nxall(k)) ibad=ibad+1
      if(i == nxall(k)-1) then
        i=nxall(k)
        ii=nxall(1)
        j=-1
        do jj=0,nyall(1),jump
          j=j+1
          mg(k)%x00(j,i)=dx_fine(jj,ii)*jump
          mg(k)%y00(j,i)=dy_fine(jj,ii)*jump
          mg(k)%r00(j,i)=coriolis_fine(jj,ii)**2/mg(k)%phi0
        end do
        if(j <  nyall(k)-1) jbad=jbad+1
        if(j >  nyall(k))   jbad=jbad+1
        if(j == nyall(k)-1) then
          j=nyall(k)
          jj=nyall(1)
          mg(k)%x00(j,i)=dx_fine(jj,ii)*jump
          mg(k)%y00(j,i)=dy_fine(jj,ii)*jump
          mg(k)%r00(j,i)=coriolis_fine(jj,ii)**2/mg(k)%phi0
        end if
      end if
                        write(6,*)' in init_mgvars, k,ibad,jbad=',k,ibad,jbad
    end do

!      next create forward operators

    do k=1,mgrid
      nx=nxall(k) ; ny=nyall(k)
      do i=0,nx
        ip=min(i+1,nx) ; im=max(i-1,0)
        do j=0,ny
          jp=min(j+1,ny) ; jm=max(j-1,0)
          x00=mg(k)%x00(j,i)
          y00=mg(k)%y00(j,i)
          x0p=half*(x00+mg(k)%x00(j ,ip))
          x0m=half*(x00+mg(k)%x00(j ,im))
          xp0=half*(x00+mg(k)%x00(jp,i ))
          xm0=half*(x00+mg(k)%x00(jm,i ))
          y0p=half*(y00+mg(k)%y00(j ,ip))
          y0m=half*(y00+mg(k)%y00(j ,im))
          yp0=half*(y00+mg(k)%y00(jp,i ))
          ym0=half*(y00+mg(k)%y00(jm,i ))
          mg(k)%h0m(j,i)=y0m/(x0m*x00*y00)
          mg(k)%h0p(j,i)=y0p/(x0p*x00*y00)
          mg(k)%hm0(j,i)=xm0/(ym0*x00*y00)
          mg(k)%hp0(j,i)=xp0/(yp0*x00*y00)
          mg(k)%h00(j,i)=mg(k)%h0m(j,i)+mg(k)%h0p(j,i)+mg(k)%hm0(j,i)+mg(k)%hp0(j,i)
        end do
      end do
    end do

!      next create implicit x operators

    do k=1,mgrid
      nx=nxall(k) ; ny=nyall(k)
      allocate(d(nx-1),dh(nx-1),u(nx-2),o(nx-2))
      allocate(dbar(nx-1),ubar(nx-2),obar(nx-2))
      allocate(dbarh(nx-1),ubarh(nx-2),obarh(nx-2))

!                              dirichlet boundary conditions
      do j=1,ny-1
        ii=0
        do i=1,nx-1
          ii=ii+1
          d(ii)=mg(k)%h00(j,i)
          if(i == nx-1) d(ii)=d(ii)+mg(k)%h0p(j,i)*mg(k)%b_over_a_east
          dh(ii)=d(ii)+mg(k)%r00(j,i)
          if(i <  nx-1) o(ii)=-mg(k)%h0p(j,i)
          if(i >  1) u(ii-1)=-mg(k)%h0m(j,i)
        end do
        call multi_factorize(d,u,o,nx-1,dbar,ubar,obar)
        call multi_factorize(dh,u,o,nx-1,dbarh,ubarh,obarh)
        ii=0
        do i=1,nx-1
          ii=ii+1
          mg(k)%dbarx(j,i)=dbar(ii)
          mg(k)%dbarxh(j,i)=dbarh(ii)
        end do
        ii=0
        do i=1,nx-2
          ii=ii+1
          mg(k)%ubarx(j,i)=ubar(ii)
          mg(k)%obarx(j,i)=obar(ii)
          mg(k)%ubarxh(j,i)=ubarh(ii)
          mg(k)%obarxh(j,i)=obarh(ii)
        end do
      end do

      deallocate(d,dh,u,o,dbar,ubar,obar,dbarh,ubarh,obarh)

    end do

!      next create implicit y operators

    do k=1,mgrid
      nx=nxall(k) ; ny=nyall(k)
      allocate(d(ny-1),dh(ny-1),u(ny-2),o(ny-2))
      allocate(dbar(ny-1),ubar(ny-2),obar(ny-2))
      allocate(dbarh(ny-1),ubarh(ny-2),obarh(ny-2))

!                              dirichlet boundary conditions
      do i=1,nx-1
        jj=0
        do j=1,ny-1
          jj=jj+1
          d(jj)=mg(k)%h00(j,i)
          if(j == ny-1) d(jj)=d(jj)+mg(k)%hp0(j,i)*mg(k)%b_north/mg(k)%a_north
          dh(jj)=d(jj)+mg(k)%r00(j,i)
          if(j <  ny-1) o(jj)=-mg(k)%hp0(j,i)
          if(j >  1) u(jj-1)=-mg(k)%hm0(j,i)
        end do
        call multi_factorize(d,u,o,ny-1,dbar,ubar,obar)
        call multi_factorize(dh,u,o,ny-1,dbarh,ubarh,obarh)
        jj=0
        do j=1,ny-1
          jj=jj+1
          mg(k)%dbary(j,i)=dbar(jj)
          mg(k)%dbaryh(j,i)=dbarh(jj)
        end do
        jj=0
        do j=1,ny-2
          jj=jj+1
          mg(k)%ubary(j,i)=ubar(jj)
          mg(k)%obary(j,i)=obar(jj)
          mg(k)%ubaryh(j,i)=ubarh(jj)
          mg(k)%obaryh(j,i)=obarh(jj)
        end do
      end do

      deallocate(d,dh,u,o,dbar,ubar,obar,dbarh,ubarh,obarh)

    end do

!       set bicubic_mask

    do k=2,mgrid
      do ic=1,mg(k)%nx-1
        if=2*ic
        do jf=3,mg(k-1)%ny-3,2
          mg(k-1)%bicubic_mask(jf,if)=.true.
        end do
      end do
      do if=3,mg(k-1)%nx-3,2
        do jc=1,mg(k)%ny-1
          jf=2*jc
          mg(k-1)%bicubic_mask(jf,if)=.true.
        end do
      end do
      do if=3,mg(k-1)%nx-3,2
        do jf=3,mg(k-1)%ny-3,2
          mg(k-1)%bicubic_mask(jf,if)=.true.
        end do
      end do
    end do

  end subroutine init_mgvars

  subroutine eval_bdys(f,mgk,nx,ny)

!   evaluate boundary point values for field f

    use constants, only: zero,half,one

    integer(i_kind),intent(in):: nx,ny
    real(r_kind),intent(inout):: f(0:ny,0:nx)
    type(mgvar):: mgk

    integer(i_kind) i,j

    if(mgk%nx /= nx.or.mgk%ny /= ny) then
        write(6,*)' argument error in call to eval_ne_bdys, mgk%nx,nx,mgk%ny,ny=',mgk%nx,nx,mgk%ny,ny
        stop
    end if

 
!    north and south boundaries

    do i=1,nx-1
      f(ny,i)=mgk%b_over_a_north_neg*f(ny-1,i)
      f(0,i)=zero
    end do

!    east and west boundaries

    do j=1,ny-1
      f(j,nx)=mgk%b_over_a_east_neg*f(j,nx-1)
      f(j,0) =zero
    end do

!     corners--just average of adjacent points on each boundary

    f( 0, 0)=zero
    f(ny, 0)=mgk%half_b_over_a_north_neg*f(ny-1,1)
    f( 0,nx)=mgk%half_b_over_a_east_neg*f(1,nx-1)
    f(ny,nx)=f(ny-1,nx-1)*mgk%half_b_over_a_east_north_neg

  end subroutine eval_bdys

  subroutine eval_bdys_ad(f,mgk,nx,ny)

!   evaluate boundary point values for field f

    use constants, only: zero,half,one

    integer(i_kind),intent(in):: nx,ny
    real(r_kind),intent(inout):: f(0:ny,0:nx)
    type(mgvar):: mgk

    integer(i_kind) i,j

    if(mgk%nx /= nx.or.mgk%ny /= ny) then
        write(6,*)' argument error in call to eval_ne_bdys, mgk%nx,nx,mgk%ny,ny=',mgk%nx,nx,mgk%ny,ny
        stop
    end if

!     adjoint of corners--just average of adjacent points on each boundary

    f(ny-1,nx-1)=f(ny-1,nx-1)+f(ny,nx)*mgk%half_b_over_a_east_north_neg
    f(ny,nx)=zero
    f(1,nx-1)=f(1,nx-1)+f(0,nx)*mgk%half_b_over_a_east_neg
    f(0,nx)=zero
    f(ny-1,1)=f(ny-1,1)+f(ny,0)*mgk%half_b_over_a_north_neg
    f(ny,0)=zero
    f(0,0)=zero

!    adjoint of east and west boundaries

    do j=1,ny-1
      f(j,0) =zero
      f(j,nx-1)=f(j,nx-1)+f(j,nx)*mgk%b_over_a_east_neg
      f(j,nx)=zero
    end do
 
!    adjoint of north and south boundaries

    do i=1,nx-1
      f(0,i)=zero
      f(ny-1,i)=f(ny-1,i)+f(ny,i)*mgk%b_over_a_north_neg
      f(ny,i)=zero
    end do

  end subroutine eval_bdys_ad

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  following is section containing transfer operators!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! NOTES ABOUT TRANSFER OPERATORS:
!
!  c
! I       transfer operator:  transfer from fine grid to coarse grid using full weighting
!  f
!             implemented by subroutine f2c_full_weight
!
!              the full weighting stencil is
!
!                       1/16     1/8     1/16
!
!                        1/8     1/4     1/8
!
!                       1/16     1/8     1/16
!
!                where the central point coincides with a coarse grid point, and the surrounding
!                points are fine grid points in between the coarse points.
!
!  f
! I       transfer operator:  transfer from coarse grid to fine grid using bilinear interpolation
!  c
!             implemented by subroutine c2f_bilinear
!
!   f
! II      transfer operator:  transfer from coarse grid to fine grid using bicubic interpolation
!   c
!             implemented by subroutine c2f_bicubic
!
!
!      NOTE 1:  all grids in this multigrid system have common point (0,0).
!                the grid dimensions are f(0:nyf,0:nxf), c(0:nyc,0:nxc)
!
!
!      NOTE 2:  the coarse grid increment is exactly twice the fine grid increment, so
!                every other point of the fine grid moving away from 0, coincides with
!                a coarse grid point.
!
!      NOTE 3:  the east (nxc) and north (nyc) edges of the coarse grid are defined using
!                boundary interpolation weights, since in general they do not coincide with
!                the boundary of the finest grid (this is because there is no condition on
!                the number of points, so grid coarsening can result in requiring an extra
!                row beyond east/north edges of the domain.
!
!      NOTE 4:  weights used here correspond to adjoint of bilinear interpolation from coarse to fine
!                with the exception that the weights are normalized to preserve area integral.
!
!
!

  subroutine f2c_full_weight(f,c,nxf,nyf,nxc,nyc)

!  full weighting fine to coarse transfer similar to adjoint of coarse to fine bilinear interpolation
!   except coefficients are normalized by their sum and only points interior to fine grid
!   are updated.

    use kinds, only: r_kind,i_kind
    use constants, only: zero
    implicit none

    integer(i_kind),intent(in):: nxc,nyc,nxf,nyf
    real(r_kind),intent(in)::    f(0:nyf,0:nxf)
    real(r_kind),intent(out)::   c(0:nyc,0:nxc)

    real(r_kind),parameter:: one_quarter=1._8/4._8,one_eighth=1._8/8._8,one_sixteenth=1._8/16._8

    integer(i_kind) if,ic,jf,jc,jfp,jfm,ifp,ifm

    c=zero
    do ic=1,nxc-1
      if=2*ic
      ifp=if+1
      ifm=if-1
      do jc=1,nyc-1
        jf=2*jc
        jfp=jf+1
        jfm=jf-1
        c(jc,ic)=one_quarter* f(jf ,if ) &
                          +one_eighth*(f(jf ,ifp)+f(jf ,ifm)+f(jfp, if)+f(jfm, if)) &
                       +one_sixteenth*(f(jfm,ifm)+f(jfm,ifp)+f(jfp,ifm)+f(jfp,ifp))
      end do
    end do

  end subroutine f2c_full_weight

  subroutine f2c_full_weight_ad(f,c,nxf,nyf,nxc,nyc)

!  full weighting fine to coarse transfer similar to adjoint of coarse to fine bilinear interpolation
!   except coefficients are normalized by their sum and only points interior to fine grid
!   are updated.

    use kinds, only: r_kind,i_kind
    use constants, only: zero
    implicit none

    integer(i_kind),intent(in):: nxc,nyc,nxf,nyf
    real(r_kind),intent(out)::    f(0:nyf,0:nxf)
    real(r_kind),intent(in):: c(0:nyc,0:nxc)

    real(r_kind),parameter:: one_quarter=1._8/4._8,one_eighth=1._8/8._8,one_sixteenth=1._8/16._8

    integer(i_kind) if,ic,jf,jc,jfp,jfm,ifp,ifm

    f=zero
    do ic=1,nxc-1
      if=2*ic
      ifp=if+1
      ifm=if-1
      do jc=1,nyc-1
        jf=2*jc
        jfp=jf+1
        jfm=jf-1
        f(jf ,if )=f(jf ,if ) + one_quarter  *c(jc,ic)
        f(jf ,ifp)=f(jf ,ifp) + one_eighth   *c(jc,ic)
        f(jf ,ifm)=f(jf ,ifm) + one_eighth   *c(jc,ic)
        f(jfp,if )=f(jfp,if ) + one_eighth   *c(jc,ic)
        f(jfm,if )=f(jfm,if ) + one_eighth   *c(jc,ic)
        f(jfm,ifm)=f(jfm,ifm) + one_sixteenth*c(jc,ic)
        f(jfm,ifp)=f(jfm,ifp) + one_sixteenth*c(jc,ic)
        f(jfp,ifm)=f(jfp,ifm) + one_sixteenth*c(jc,ic)
        f(jfp,ifp)=f(jfp,ifp) + one_sixteenth*c(jc,ic)
      end do
    end do

  end subroutine f2c_full_weight_ad

  subroutine c2f_bilinear(c,f,nxc,nyc,nxf,nyf)

!  bilinear interpolation from coarse to fine grid
!    note:  c boundary should be filled in to get correct results on 1st interior row of f

    use kinds, only: r_kind,i_kind
    use constants, only: zero,quarter,half
    implicit none

    integer(i_kind),intent(in):: nxc,nyc,nxf,nyf
    real(r_kind),intent(in)::    c(0:nyc,0:nxc)
    real(r_kind),intent(out)::   f(0:nyf,0:nxf)

    integer(i_kind) if,ic,jf,jc,icm,icp,jcm,jcp

!  fill in coincident points:

    f=zero
    do ic=1,nxc-1
      if=2*ic
      do jc=1,nyc-1
        jf=2*jc
        f(jf,if)=c(jc,ic)
      end do
    end do

!  fill in y blanks on coincident x lines

    do ic=1,nxc-1
      if=2*ic
      do jf=1,nyf-1,2
        jcm=(jf-1)/2
        jcp=(jf+1)/2
        f(jf,if)=half*(c(jcm,ic)+c(jcp,ic))
      end do
    end do

!  fill in x blanks on coincident y lines
    do if=1,nxf-1,2
      icm=(if-1)/2
      icp=(if+1)/2
      do jc=1,nyc-1
        jf=2*jc
        f(jf,if)=half*(c(jc,icm)+c(jc,icp))
      end do
    end do

!  fill in remaining holes
    do if=1,nxf-1,2
      icm=(if-1)/2
      icp=(if+1)/2
      do jf=1,nyf-1,2
        jcm=(jf-1)/2
        jcp=(jf+1)/2
        f(jf,if)=quarter*(c(jcm,icm)+c(jcm,icp)+c(jcp,icm)+c(jcp,icp))
      end do
    end do

  end subroutine c2f_bilinear

  subroutine c2f_bilinear_ad(c,f,nxc,nyc,nxf,nyf)

!   adjoint of c2f_bilinear

    use kinds, only: r_kind,i_kind
    use constants, only: zero,quarter,half

    integer(i_kind),intent(in):: nxc,nyc,nxf,nyf
    real(r_kind),intent(out)::    c(0:nyc,0:nxc)
    real(r_kind),intent(in)::   f(0:nyf,0:nxf)

    integer(i_kind) if,ic,jf,jc,icm,icp,jcm,jcp

    c=zero

!  adjoint of fill in remaining holes

    do if=1,nxf-1,2
      icm=(if-1)/2
      icp=(if+1)/2
      do jf=1,nyf-1,2
        jcm=(jf-1)/2
        jcp=(jf+1)/2
        c(jcm,icm)=c(jcm,icm)+quarter*f(jf,if)
        c(jcm,icp)=c(jcm,icp)+quarter*f(jf,if)
        c(jcp,icm)=c(jcp,icm)+quarter*f(jf,if)
        c(jcp,icp)=c(jcp,icp)+quarter*f(jf,if)
      end do
    end do

!  adjoint of fill in x blanks on coincident y lines

    do if=1,nxf-1,2
      icm=(if-1)/2
      icp=(if+1)/2
      do jc=1,nyc-1
        jf=2*jc
        c(jc,icm)=c(jc,icm)+half*f(jf,if)
        c(jc,icp)=c(jc,icp)+half*f(jf,if)
      end do
    end do

!  adjoint of fill in y blanks on coincident x lines

    do ic=1,nxc-1
      if=2*ic
      do jf=1,nyf-1,2
        jcm=(jf-1)/2
        jcp=(jf+1)/2
        c(jcm,ic)=c(jcm,ic)+half*f(jf,if)
        c(jcp,ic)=c(jcp,ic)+half*f(jf,if)
      end do
    end do

!  adjoint of fill in coincident points:

    do ic=1,nxc-1
      if=2*ic
      do jc=1,nyc-1
        jf=2*jc
        c(jc,ic)=c(jc,ic)+f(jf,if)
      end do
    end do

  end subroutine c2f_bilinear_ad

  subroutine c2f_bicubic(c,f,nxc,nyc,nxf,nyf,bicubic_mask)

!  bicubic interpolation from coarse to fine grid

    use kinds, only: r_kind,i_kind
    use constants, only: zero,quarter,half
    implicit none

    integer(i_kind),intent(in):: nxc,nyc,nxf,nyf
    real(r_kind),intent(in)::    c(0:nyc,0:nxc)
    real(r_kind),intent(out)::   f(0:nyf,0:nxf)
    logical,intent(in)::         bicubic_mask(0:nyf,0:nxf)

    real(r_kind),parameter:: one_sixteenth=1._8/16._8,nine_sixteenths=9._8/16._8
    real(r_kind),parameter:: one_sixt2=one_sixteenth**2,onenine_sixt2=one_sixteenth*nine_sixteenths
    real(r_kind),parameter:: nine_sixt2=nine_sixteenths**2

    integer(i_kind) if,ic,jf,jc
    integer(i_kind) icm2,icp,icp2,icm,jcm2,jcm,jcp2,jcp

!    start with coincident points and points closest to boundary that are linearly interpolated:

!  fill in coincident points:

    f=zero
    do ic=1,nxc-1
      if=2*ic
      do jc=1,nyc-1
        jf=2*jc
        if(.not.bicubic_mask(jf,if)) f(jf,if)=c(jc,ic)
      end do
    end do

!  fill in y blanks with linear interpolation on coincident x lines adjacent to y boundarys

    do ic=1,nxc-1
      if=2*ic
      do jf=1,nyf-1,2
        jcm=(jf-1)/2
        jcp=(jf+1)/2
        if(.not.bicubic_mask(jf,if)) f(jf,if)=half*(c(jcm,ic)+c(jcp,ic))
      end do
    end do

!  fill in x blanks on coincident y lines
    do if=1,nxf-1,2
      icm=(if-1)/2
      icp=(if+1)/2
      do jc=1,nyc-1
        jf=2*jc
        if(.not.bicubic_mask(jf,if)) f(jf,if)=half*(c(jc,icm)+c(jc,icp))
      end do
    end do

!  fill in remaining holes
    do if=1,nxf-1,2
      icm=(if-1)/2
      icp=(if+1)/2
      do jf=1,nyf-1,2
        jcm=(jf-1)/2
        jcp=(jf+1)/2
        if(.not.bicubic_mask(jf,if)) f(jf,if)=quarter*(c(jcm,icm)+c(jcm,icp)+c(jcp,icm)+c(jcp,icp))
      end do
    end do

!  fill in y blanks on coincident interior x lines

    do ic=1,nxc-1
      if=2*ic
      do jf=3,nyf-3,2
        jcm=(jf-1)/2 ; jcm2=(jf-3)/2 ; jcp=(jf+1)/2 ; jcp2=(jf+3)/2
        f(jf,if)=nine_sixteenths*(c(jcm,ic)+c(jcp,ic))-one_sixteenth*(c(jcm2,ic)+c(jcp2,ic))
      end do
    end do

!  fill in x blanks on coincident interior y lines

    do if=3,nxf-3,2
      icm=(if-1)/2 ; icm2=(if-3)/2 ; icp=(if+1)/2 ; icp2=(if+3)/2
      do jc=1,nyc-1
        jf=2*jc
        f(jf,if)=nine_sixteenths*(c(jc,icm)+c(jc,icp))-one_sixteenth*(c(jc,icm2)+c(jc,icp2))
      end do
    end do

!  fill in remaining interior holes

    do if=3,nxf-3,2
      icm=(if-1)/2 ; icm2=(if-3)/2 ; icp=(if+1)/2 ; icp2=(if+3)/2
      do jf=3,nyf-3,2
        jcm=(jf-1)/2 ; jcm2=(jf-3)/2 ; jcp=(jf+1)/2 ; jcp2=(jf+3)/2
        f(jf,if)=nine_sixt2*(c(jcm ,icm )+c(jcm ,icp )+c(jcp ,icm )+c(jcp ,icp ))     &
             -onenine_sixt2*(c(jcm ,icm2)+c(jcm ,icp2)+c(jcp ,icm2)+c(jcp ,icp2)      &
                            +c(jcm2,icm )+c(jcp2,icm )+c(jcm2,icp )+c(jcp2,icp ))     &
                 +one_sixt2*(c(jcm2,icm2)+c(jcm2,icp2)+c(jcp2,icm2)+c(jcp2,icp2))
      end do
    end do

  end subroutine c2f_bicubic

  subroutine c2f_bicubic_ad(c,f,nxc,nyc,nxf,nyf,bicubic_mask)

!  bicubic interpolation from coarse to fine grid

    use kinds, only: r_kind,i_kind
    use constants, only: zero,quarter,half
    implicit none

    integer(i_kind),intent(in):: nxc,nyc,nxf,nyf
    real(r_kind),intent(out)::    c(0:nyc,0:nxc)
    real(r_kind),intent(in)::     f(0:nyf,0:nxf)
    logical,intent(in)::          bicubic_mask(0:nyf,0:nxf)

    real(r_kind),parameter:: one_sixteenth=1._8/16._8,nine_sixteenths=9._8/16._8
    real(r_kind),parameter:: one_sixt2=one_sixteenth**2,onenine_sixt2=one_sixteenth*nine_sixteenths
    real(r_kind),parameter:: nine_sixt2=nine_sixteenths**2

    integer(i_kind) if,ic,jf,jc
    integer(i_kind) icm2,icp,icp2,icm,jcm2,jcm,jcp2,jcp

    c=zero

!  adjoint of fill in remaining interior holes

    do if=3,nxf-3,2
      icm=(if-1)/2 ; icm2=(if-3)/2 ; icp=(if+1)/2 ; icp2=(if+3)/2
      do jf=3,nyf-3,2
        jcm=(jf-1)/2 ; jcm2=(jf-3)/2 ; jcp=(jf+1)/2 ; jcp2=(jf+3)/2
        c(jcm ,icm )=c(jcm ,icm )+f(jf,if)*nine_sixt2
        c(jcm ,icp )=c(jcm ,icp )+f(jf,if)*nine_sixt2
        c(jcp ,icm )=c(jcp ,icm )+f(jf,if)*nine_sixt2
        c(jcp ,icp )=c(jcp ,icp )+f(jf,if)*nine_sixt2
        c(jcm ,icm2)=c(jcm ,icm2)-f(jf,if)*onenine_sixt2
        c(jcm ,icp2)=c(jcm ,icp2)-f(jf,if)*onenine_sixt2
        c(jcp ,icm2)=c(jcp ,icm2)-f(jf,if)*onenine_sixt2
        c(jcp ,icp2)=c(jcp ,icp2)-f(jf,if)*onenine_sixt2
        c(jcm2,icm )=c(jcm2,icm )-f(jf,if)*onenine_sixt2
        c(jcp2,icm )=c(jcp2,icm )-f(jf,if)*onenine_sixt2
        c(jcm2,icp )=c(jcm2,icp )-f(jf,if)*onenine_sixt2
        c(jcp2,icp )=c(jcp2,icp )-f(jf,if)*onenine_sixt2
        c(jcm2,icm2)=c(jcm2,icm2)+f(jf,if)*one_sixt2
        c(jcm2,icp2)=c(jcm2,icp2)+f(jf,if)*one_sixt2
        c(jcp2,icm2)=c(jcp2,icm2)+f(jf,if)*one_sixt2
        c(jcp2,icp2)=c(jcp2,icp2)+f(jf,if)*one_sixt2
      end do
    end do

!  adjoint of fill in x blanks on coincident interior y lines

    do if=3,nxf-3,2
      icm=(if-1)/2 ; icm2=(if-3)/2 ; icp=(if+1)/2 ; icp2=(if+3)/2
      do jc=1,nyc-1
        jf=2*jc
        c(jc,icm )=c(jc,icm )+f(jf,if)*nine_sixteenths
        c(jc,icp )=c(jc,icp )+f(jf,if)*nine_sixteenths
        c(jc,icm2)=c(jc,icm2)-f(jf,if)*one_sixteenth
        c(jc,icp2)=c(jc,icp2)-f(jf,if)*one_sixteenth
      end do
    end do

!  adjoint of fill in y blanks on coincident interior x lines

    do ic=1,nxc-1
      if=2*ic
      do jf=3,nyf-3,2
        jcm=(jf-1)/2 ; jcm2=(jf-3)/2 ; jcp=(jf+1)/2 ; jcp2=(jf+3)/2
        c(jcm ,ic)=c(jcm ,ic)+f(jf,if)*nine_sixteenths
        c(jcp ,ic)=c(jcp ,ic)+f(jf,if)*nine_sixteenths
        c(jcm2,ic)=c(jcm2,ic)-f(jf,if)*one_sixteenth
        c(jcp2,ic)=c(jcp2,ic)-f(jf,if)*one_sixteenth
      end do
    end do

!    adjoint of start with coincident points and points closest to boundary that are linearly interpolated:

!  adjoint of fill in remaining holes

    do if=1,nxf-1,2
      icm=(if-1)/2
      icp=(if+1)/2
      do jf=1,nyf-1,2
        jcm=(jf-1)/2
        jcp=(jf+1)/2
        if(.not.bicubic_mask(jf,if)) then
          c(jcm,icm)=c(jcm,icm)+f(jf,if)*quarter
          c(jcm,icp)=c(jcm,icp)+f(jf,if)*quarter
          c(jcp,icm)=c(jcp,icm)+f(jf,if)*quarter
          c(jcp,icp)=c(jcp,icp)+f(jf,if)*quarter
        end if
      end do
    end do

!  adjoint of fill in x blanks on coincident y lines

    do if=1,nxf-1,2
      icm=(if-1)/2
      icp=(if+1)/2
      do jc=1,nyc-1
        jf=2*jc
        if(.not.bicubic_mask(jf,if)) then
          c(jc,icm)=c(jc,icm)+f(jf,if)*half
          c(jc,icp)=c(jc,icp)+f(jf,if)*half
        end if
      end do
    end do

!  adjoint of fill in y blanks with linear interpolation on coincident x lines adjacent to y boundarys

    do ic=1,nxc-1
      if=2*ic
      do jf=1,nyf-1,2
        jcm=(jf-1)/2
        jcp=(jf+1)/2
        if(.not.bicubic_mask(jf,if)) then
          c(jcm,ic)=c(jcm,ic)+f(jf,if)*half
          c(jcp,ic)=c(jcp,ic)+f(jf,if)*half
        end if
      end do
    end do

!  adjoint of fill in coincident points:

    do ic=1,nxc-1
      if=2*ic
      do jc=1,nyc-1
        jf=2*jc
        if(.not.bicubic_mask(jf,if)) then
          c(jc,ic)=c(jc,ic)+f(jf,if)
        end if
      end do
    end do

  end subroutine c2f_bicubic_ad

  subroutine multi_factorize(d,u,o,nx,dbar,ubar,obar)

!    factorize special input matrix into lower times upper triangular matrices (illustrated for nx=5):
!                                       _                                  _      _
! |d(1)   o(1)   0      0      0   |   |d(1)   0      0      0      0   | |d(1)   o(1)   0      0      0   |
! |                                |   |_      _                        | |       _      _                 |
! |u(1)   d(2)   o(2)   0      0   |   |u(1)   d(2)   0      0      0   | |0      d(2)   o(2)   0      0   |
! |                                |   |       _      _                 | |              _      _          |
! |0      u(2)   d(3)   o(3)   0   | = |0      u(2)   d(2)   0      0   |*|0      0      d(3)   o(3)   0   |
! |                                |   |              _      _          | |                     _      _   |
! |0      0      u(3)   d(4)   o(4)|   |0      0      u(2)   d(4)   0   | |0      0      0      d(4)   o(4)|
! |                                |   |                     _      _   | |                            _   |
! |0      0      0      u(4)   d(5)|   |0      0      0      u(4)   d(5)| |0      0      0      0      d(5)|

    integer(i_kind),intent(in):: nx
    real(r_kind),intent(in):: d(nx),u(nx-1),o(nx-1)
    real(r_kind),intent(out):: dbar(nx),ubar(nx-1),obar(nx-1)

    integer(i_kind) i

    dbar(1)=sqrt(d(1))
    obar(1)=o(1)/dbar(1)
    ubar(1)=u(1)/dbar(1)

    do i=2,nx-1
      dbar(i)=sqrt(d(i)-ubar(i-1)*obar(i-1))
      obar(i)=o(i)/dbar(i)
      ubar(i)=u(i)/dbar(i)
    end do
    i=nx
    dbar(i)=sqrt(d(i)-ubar(i-1)*obar(i-1))

  end subroutine multi_factorize

  subroutine forward_op_x1(v,f,d,u,o,nx)

    use constants, only: zero
    integer(i_kind),intent(in)::nx
    real(r_kind),intent(in):: v(nx),d(nx),u(nx-1),o(nx-1)
    real(r_kind),intent(out)::f(nx)

    integer(i_kind) i

    f(1)=d(1)*v(1)+o(1)*v(2)
    do i=2,nx-1
      f(i)=u(i-1)*v(i-1)+d(i)*v(i)+o(i)*v(i+1)
    end do
    f(nx)=u(nx-1)*v(nx-1)+d(nx)*v(nx)

  end subroutine forward_op_x1

  subroutine inverse_op_x1(f,v,dbar,ubar,obar,nx)

    use constants, only: zero
    integer(i_kind),intent(in)::nx
    real(r_kind),intent(in):: f(nx),dbar(nx),ubar(nx-1),obar(nx-1)
    real(r_kind),intent(out)::v(nx)

    integer(i_kind) i
    real(r_kind) temp(nx)

    temp(1)=f(1)/dbar(1)
    do i=2,nx
      temp(i)=(f(i)-ubar(i-1)*temp(i-1))/dbar(i)
    end do

    v(nx)=temp(nx)/dbar(nx)
    do i=nx-1,1,-1
      v(i)=(temp(i)-obar(i)*v(i+1))/dbar(i)
    end do

  end subroutine inverse_op_x1

  subroutine forward_op_x(v,f,nx,ny,j,k,helmholtz)

    use constants, only: zero
    integer(i_kind),intent(in)::nx,ny,j,k
    real(r_kind),intent(in)::helmholtz
    real(r_kind),intent(in):: v(0:ny,0:nx)
    real(r_kind),intent(out)::f(0:ny,0:nx)

    integer(i_kind) i,im1,ip1

    do i=1,nx-1
      im1=i-1 ; ip1=i+1
      f(j,i)=mg(k)%h0m(j,i)*v(j,  im1)+mg(k)%h0p(j,i)*v(j,ip1) &
               -(mg(k)%h00(j,i)+helmholtz*mg(k)%r00(j,i))*v(j,i)
    end do

  end subroutine forward_op_x

  subroutine inverse_luop_x_1(rhs,v,dbarx,ubarx,obarx,nx,ny,j)

    integer(i_kind),intent(in):: nx,ny,j
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    rhs,dbarx,ubarx,obarx
    real(r_kind),dimension(0:ny,0:nx),intent(inout):: v

    integer(i_kind) i,im1,ip1
    real(r_kind) temp(0:ny,0:nx)

!   inverse of lower triangle first:

    temp(j,1)=rhs(j,1)/dbarx(j,1)
    do i=2,nx-1
      im1=i-1
      temp(j,i)=(rhs(j,i)-ubarx(j,im1)*temp(j,im1))/dbarx(j,i)
    end do

!   finish with inverse of upper triangle:

    v(j,nx-1)=temp(j,nx-1)/dbarx(j,nx-1)
    do i=nx-2,1,-1
      ip1=i+1
      v(j,i)=(temp(j,i)-obarx(j,i)*v(j,ip1))/dbarx(j,i)
    end do

  end subroutine inverse_luop_x_1

  subroutine forward_op_y(v,f,nx,ny,i,k,helmholtz)

    use constants, only: zero
    integer(i_kind),intent(in)::nx,ny,i,k
    real(r_kind),intent(in)::helmholtz
    real(r_kind),intent(in):: v(0:ny,0:nx)
    real(r_kind),intent(out)::f(0:ny,0:nx)

    integer(i_kind) j,jm1,jp1

    do j=1,ny-1
      jm1=j-1 ; jp1=j+1
      f(j,i)=mg(k)%hm0(j,i)*v(jm1,i  )+mg(k)%hp0(j,i)*v(jp1,i) &
               -(mg(k)%h00(j,i)+helmholtz*mg(k)%r00(j,i))*v(j,i)
    end do

  end subroutine forward_op_y

  subroutine inverse_luop_y_1(rhs,v,dbary,ubary,obary,nx,ny,i)

    integer(i_kind),intent(in):: nx,ny,i
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    rhs,dbary,ubary,obary
    real(r_kind),dimension(0:ny,0:nx),intent(inout):: v

    integer(i_kind) j
    real(r_kind) temp(0:ny)


!   inverse of lower triangle first:

      temp(1)=rhs(1,i)/dbary(1,i)
      do j=2,ny-1
        temp(j)=(rhs(j,i)-ubary(j-1,i)*temp(j-1))/dbary(j,i)
      end do

!   finish with inverse of upper triangle:

      v(ny-1,i)=temp(ny-1)/dbary(ny-1,i)
      do j=ny-2,1,-1
        v(j,i)=(temp(j)-obary(j,i)*v(j+1,i))/dbary(j,i)
      end do

  end subroutine inverse_luop_y_1

  subroutine forward_op(v,f,nx,ny,k,helmholtz)

    use constants, only: zero
    integer(i_kind),intent(in)::nx,ny,k
    real(r_kind),intent(in)::helmholtz
    real(r_kind),intent(in):: v(0:ny,0:nx)
    real(r_kind),intent(out)::f(0:ny,0:nx)

    integer(i_kind) i,im1,ip1,j

!      check that nx,ny are correct values for level k:
    if(mg(k)%nx /= nx.or.mg(k)%ny /= ny) then
      write(6,*)' inconsistent input nx,ny for fmg_forward_op, program stops'
      stop
    end if

    f=zero
    do i=1,nx-1
      im1=i-1 ; ip1=i+1
      do j=1,ny-1
        f(j,i)=mg(k)%h0m(j,i)*v(j,  im1)+mg(k)%h0p(j,i)*v(j,ip1) &
              +mg(k)%hm0(j,i)*v(j-1,i  )+mg(k)%hp0(j,i)*v(j+1,i) &
               -(mg(k)%h00(j,i)+helmholtz*mg(k)%r00(j,i))*v(j,i)
      end do
    end do

  end subroutine forward_op

  subroutine forward_op_ad(v,f,nx,ny,k,helmholtz)

    use constants, only: zero
    integer(i_kind),intent(in)::nx,ny,k
    real(r_kind),intent(in)::helmholtz
    real(r_kind),intent(out):: v(0:ny,0:nx)
    real(r_kind),intent(in)::f(0:ny,0:nx)

    integer(i_kind) i,im1,ip1,j

!      check that nx,ny are correct values for level k:
    if(mg(k)%nx /= nx.or.mg(k)%ny /= ny) then
      write(6,*)' inconsistent input nx,ny for fmg_forward_op_ad, program stops'
      stop
    end if

    v=zero
    do i=1,nx-1
      im1=i-1 ; ip1=i+1
      do j=1,ny-1
        v(j  ,im1)=v(j  ,im1)+mg(k)%h0m(j,i)*f(j,i)
        v(j  ,ip1)=v(j  ,ip1)+mg(k)%h0p(j,i)*f(j,i)
        v(j-1,i  )=v(j-1,i  )+mg(k)%hm0(j,i)*f(j,i)
        v(j+1,i  )=v(j+1,i  )+mg(k)%hp0(j,i)*f(j,i)
        v(j  ,i  )=v(j  ,i  )-(mg(k)%h00(j,i)+helmholtz*mg(k)%r00(j,i))*f(j,i)
      end do
    end do

  end subroutine forward_op_ad

  subroutine relax(v,f,dbarx,ubarx,obarx,dbary,ubary,obary,h0p,h0m,hp0,hm0,nx,ny, &
                   b_over_a_east_neg,b_over_a_north_neg)
    
    use constants, only: zero

!    do one iteration of adi relaxation, first in x direction, then in y direction

    integer(i_kind),intent(in):: nx,ny
    real(r_kind),dimension(0:ny,0:nx),intent(inout):: v
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    f
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    dbarx,ubarx,obarx
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    dbary,ubary,obary
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    h0p,h0m,hp0,hm0
    real(r_kind),intent(in)::                         b_over_a_east_neg,b_over_a_north_neg

    integer(i_kind) i,ii,im1,ip1,j,jj
    real(r_kind) rhs(0:ny,0:nx)

!      do x lines first:

    ii=1
    do jj=1,2
      do i=1,nx-1
        do j=jj,ny-1,2
          rhs(j,i)=-f(j,i)+hp0(j,i)*v(j+1,i)+hm0(j,i)*v(j-1,i)
        end do
      end do
      call inverse_luop_x(rhs,v,dbarx,ubarx,obarx,nx,ny,jj,ii)
      do j=jj,ny-1,2
        v(j,0)=zero
        v(j,nx)=b_over_a_east_neg*v(j,nx-1)
      end do
    end do

!      now do y lines:

    jj=1
    do ii=1,2
      do i=ii,nx-1,2
        ip1=i+1 ; im1=i-1
        do j=1,ny-1
          rhs(j,i)=-f(j,i)+h0p(j,i)*v(j,ip1)+h0m(j,i)*v(j,im1)
        end do
      end do
      call inverse_luop_y(rhs,v,dbary,ubary,obary,nx,ny,ii,jj)
      do i=ii,nx-1,2
        v(0,i)=zero
        v(ny,i)=b_over_a_north_neg*v(ny-1,i)
      end do
    end do

  end subroutine relax

  subroutine relax_ad(v,f,dbarx,ubarx,obarx,dbary,ubary,obary,h0p,h0m,hp0,hm0,nx,ny, &
                   b_over_a_east_neg,b_over_a_north_neg)
    
    use constants, only: zero

!    do one iteration of adi relaxation, first in x direction, then in y direction

    integer(i_kind),intent(in):: nx,ny
    real(r_kind),dimension(0:ny,0:nx),intent(inout):: v
    real(r_kind),dimension(0:ny,0:nx),intent(inout):: f
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    dbarx,ubarx,obarx
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    dbary,ubary,obary
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    h0p,h0m,hp0,hm0
    real(r_kind),intent(in)::                         b_over_a_east_neg,b_over_a_north_neg

    integer(i_kind) i,ii,im1,ip1,j,jj
    real(r_kind) rhs(0:ny,0:nx)


!      adjoint of now do y lines:

    jj=1
    do ii=2,1,-1
      do i=ii,nx-1,2
        v(ny-1,i)=v(ny-1,i)+b_over_a_north_neg*v(ny,i)
        v(ny,i)=zero
        v(0,i)=zero
      end do
      call inverse_luop_y_ad(rhs,v,dbary,ubary,obary,nx,ny,ii,jj)
      do i=ii,nx-1,2
        ip1=i+1 ; im1=i-1
        do j=ny-1,1,-1
          f(j,i)=f(j,i)-rhs(j,i)
          v(j,ip1)=v(j,ip1)+h0p(j,i)*rhs(j,i)
          v(j,im1)=v(j,im1)+h0m(j,i)*rhs(j,i)
          v(j,i)=zero
        end do
      end do
    end do

!      adjoint of do x lines first:

    ii=1
    do jj=2,1,-1
      do j=jj,ny-1,2
        v(j,nx-1)=v(j,nx-1)+b_over_a_east_neg*v(j,nx)
        v(j,nx)=zero
        v(j,0)=zero
      end do
      call inverse_luop_x_ad(rhs,v,dbarx,ubarx,obarx,nx,ny,jj,ii)
      do i=nx-1,1,-1
        do j=jj,ny-1,2
          f(j,i)=f(j,i)-rhs(j,i)
          v(j+1,i)=v(j+1,i)+hp0(j,i)*rhs(j,i)
          v(j-1,i)=v(j-1,i)+hm0(j,i)*rhs(j,i)
          v(j,i)=zero
        end do
      end do
    end do

  end subroutine relax_ad

  subroutine inverse_luop_x(rhs,v,dbarx,ubarx,obarx,nx,ny,jj,ii)

    integer(i_kind),intent(in):: nx,ny,jj,ii
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    rhs,dbarx,ubarx,obarx
    real(r_kind),dimension(0:ny,0:nx),intent(inout):: v

    integer(i_kind) i,im1,ip1,j
    real(r_kind) temp(0:ny,0:nx)

!   inverse of lower triangle first:

    do j=jj,ny-1,2
      temp(j,ii)=rhs(j,ii)/dbarx(j,ii)
    end do
    do i=ii+1,nx-ii
      im1=i-1
      do j=jj,ny-1,2
        temp(j,i)=(rhs(j,i)-ubarx(j,im1)*temp(j,im1))/dbarx(j,i)
      end do
    end do

!   finish with inverse of upper triangle:

    do j=jj,ny-1,2
      v(j,nx-ii)=temp(j,nx-ii)/dbarx(j,nx-ii)
    end do
    do i=nx-ii-1,ii,-1
      ip1=i+1
      do j=jj,ny-1,2
        v(j,i)=(temp(j,i)-obarx(j,i)*v(j,ip1))/dbarx(j,i)
      end do
    end do

  end subroutine inverse_luop_x

  subroutine inverse_luop_x_ad(rhs,v,dbarx,ubarx,obarx,nx,ny,jj,ii)

    use constants, only: zero

    integer(i_kind),intent(in):: nx,ny,jj,ii
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    dbarx,ubarx,obarx
    real(r_kind),dimension(0:ny,0:nx),intent(inout):: rhs
    real(r_kind),dimension(0:ny,0:nx),intent(inout):: v

    integer(i_kind) i,im1,ip1,j
    real(r_kind) temp(0:ny,0:nx)

!   adjoint of finish with inverse of upper triangle:

    do i=ii,nx-ii-1
      do j=jj,ny-1,2
        temp(j,i)=zero
      end do
    end do
    do j=jj,ny-1,2
      temp(j,nx-ii)=zero
    end do

    do i=ii,nx-ii-1
      ip1=i+1
      do j=jj,ny-1,2
        temp(j,i)=temp(j,i)+v(j,i)/dbarx(j,i)
        v(j,ip1)=v(j,ip1)-obarx(j,i)*v(j,i)/dbarx(j,i)
        v(j,i)=zero                                       !???
      end do
    end do
    do j=jj,ny-1,2
      temp(j,nx-ii)=temp(j,nx-ii)+v(j,nx-ii)/dbarx(j,nx-ii)
    end do

!   adjoint of inverse of lower triangle first:

    do i=nx-ii,ii+1,-1
      do j=jj,ny-1,2
        rhs(j,i)=zero
      end do
    end do
    do j=jj,ny-1,2
      rhs(j,ii)=zero
    end do

    do i=nx-ii,ii+1,-1
      im1=i-1
      do j=jj,ny-1,2
        rhs(j,i)=rhs(j,i)+temp(j,i)/dbarx(j,i)
        temp(j,im1)=temp(j,im1)-ubarx(j,im1)*temp(j,i)/dbarx(j,i)
      end do
    end do
    do j=jj,ny-1,2
      rhs(j,ii)=rhs(j,ii)+temp(j,ii)/dbarx(j,ii)
    end do

  end subroutine inverse_luop_x_ad

  subroutine inverse_luop_y(rhs,v,dbary,ubary,obary,nx,ny,ii,jj)

    integer(i_kind),intent(in):: nx,ny,ii,jj
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    rhs,dbary,ubary,obary
    real(r_kind),dimension(0:ny,0:nx),intent(inout):: v

    integer(i_kind) i,j
    real(r_kind) temp(0:ny)

    do i=ii,nx-1,2

!   inverse of lower triangle first:

      temp(jj)=rhs(jj,i)/dbary(jj,i)
      do j=jj+1,ny-jj
        temp(j)=(rhs(j,i)-ubary(j-1,i)*temp(j-1))/dbary(j,i)
      end do

!   finish with inverse of upper triangle:

      v(ny-jj,i)=temp(ny-jj)/dbary(ny-jj,i)
      do j=ny-jj-1,jj,-1
        v(j,i)=(temp(j)-obary(j,i)*v(j+1,i))/dbary(j,i)
      end do
    end do

  end subroutine inverse_luop_y

  subroutine inverse_luop_y_ad(rhs,v,dbary,ubary,obary,nx,ny,ii,jj)

    use constants, only: zero

    integer(i_kind),intent(in):: nx,ny,ii,jj
    real(r_kind),dimension(0:ny,0:nx),intent(in)::    dbary,ubary,obary
    real(r_kind),dimension(0:ny,0:nx),intent(inout)::    rhs
    real(r_kind),dimension(0:ny,0:nx),intent(inout):: v

    integer(i_kind) i,j
    real(r_kind) temp(0:ny)

    do i=ii,nx-1,2

!   adjoint of finish with inverse of upper triangle:

      do j=jj,ny-jj-1
        temp(j)=zero
      end do
      temp(ny-jj)=zero

      do j=jj,ny-jj-1
        temp(j)=temp(j)+v(j,i)/dbary(j,i)
        v(j+1,i)=v(j+1,i)-obary(j,i)*v(j,i)/dbary(j,i)
        v(j,i)=zero
      end do
      temp(ny-jj)=temp(ny-jj)+v(ny-jj,i)/dbary(ny-jj,i)

!   adjoint of inverse of lower triangle first:

      do j=ny-jj,jj+1,-1
        rhs(j,i)=zero
      end do
      rhs(jj,i)=zero

      do j=ny-jj,jj+1,-1
        rhs(j,i)=rhs(j,i)+temp(j)/dbary(j,i)
        temp(j-1)=temp(j-1)-ubary(j-1,i)*temp(j)/dbary(j,i)
      end do
      rhs(jj,i)=rhs(jj,i)+temp(jj)/dbary(jj,i)

    end do

  end subroutine inverse_luop_y_ad

  subroutine fmg(v,f,helmholtz,nrelax1,nrelax2,nrelax_solve,nx,ny)

!   full multigrid solution on fixed domain with homogeneous dirichlet boundary conditions

!      solve delsqr(v)    = f   helmholtz=.false.
!           (delsqr-R)(v) = f   helmholtz=.true.

    use constants, only: zero,one

    integer(i_kind),intent(in)::nx,ny,nrelax1,nrelax2,nrelax_solve
    logical,intent(in)::        helmholtz
    real(r_kind),intent(in)::   f(0:ny,0:nx)     !  !  in actual use, f is in, and v is out, but
    real(r_kind),intent(out)::  v(0:ny,0:nx)     !   !  make both inout for adjoint test purposes

    integer(i_kind) irelax,k,m
    real(r_kind) helmholtz_factor

    if(nxall(1) /= nx.or.nyall(1) /= ny) then
       write(6,*)' inconsistent input nx,ny in fmg, program stops'
       stop
    end if

    helmholtz_factor=zero
    if(helmholtz) helmholtz_factor=one

            !write(6,'(" at 1 in fmg, min,max f =",2e15.4)')minval(f),maxval(f)
    mg(1)%f=f
    mg(1)%v=zero
    do k=2,mgrid
      call f2c_full_weight(mg(k-1)%f,mg(k)%f,mg(k-1)%nx,mg(k-1)%ny,mg(k)%nx,mg(k)%ny)
      mg(k)%v=zero
    end do
            !write(6,'(" at 2 in fmg, min,max f =",2e15.4)')minval(f),maxval(f)

!      main fmg loop:

    do m=mgrid,1,-1

!      exact solution at level mgrid

      k=mgrid
      do irelax=1,nrelax_solve
        if(helmholtz) then
          call relax(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                     mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        else
          call relax(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                     mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        end if
      end do

      do k=mgrid-1,m,-1

!        update v(k) with v(k+1)

        call eval_bdys(mg(k+1)%v,mg(k+1),mg(k+1)%nx,mg(k+1)%ny)
        call c2f_bilinear(mg(k+1)%v,mg(k)%w,mg(k+1)%nx,mg(k+1)%ny,mg(k)%nx,mg(k)%ny)
        call eval_bdys(mg(k)%w,mg(k),mg(k)%nx,mg(k)%ny)
        mg(k)%v=mg(k)%v+mg(k)%w

!         relax

        do irelax=1,nrelax2
          if(helmholtz) then
            call relax(mg(k)%v,mg(k)%f, &
                       mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                       mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                       mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
          else
            call relax(mg(k)%v,mg(k)%f, &
                       mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                       mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                       mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
          end if
        end do

      end do

      if(m >  1) then
        call eval_bdys(mg(m)%v,mg(m),mg(m)%nx,mg(m)%ny)
        call c2f_bicubic(mg(m)%v,mg(m-1)%v,mg(m)%nx,mg(m)%ny,mg(m-1)%nx,mg(m-1)%ny,mg(m-1)%bicubic_mask)
        call eval_bdys(mg(m-1)%v,mg(m-1),mg(m-1)%nx,mg(m-1)%ny)
      end if

      if(m == 1) exit

      do k=m-1,mgrid-1

!         relax

        do irelax=1,nrelax1
          if(helmholtz) then
            call relax(mg(k)%v,mg(k)%f, &
                       mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                       mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                       mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
          else
            call relax(mg(k)%v,mg(k)%f, &
                       mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                       mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                       mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
          end if
        end do

!        compute residual

        call eval_bdys(mg(k)%v,mg(k),mg(k)%nx,mg(k)%ny)
        mg(k)%w=zero
        call forward_op(mg(k)%v,mg(k)%w,mg(k)%nx,mg(k)%ny,k,helmholtz_factor)

        mg(k)%w=mg(k)%f-mg(k)%w      !  now have residual in mg(k)%w

!        transfer residual to next coarser grid

          !write(6,'(" k, max val new residual=",i4,e14.5)')k, &
          !               maxval(abs(mg(k)%w(1:mg(k)%ny-1,1:mg(k)%nx-1)))
          !write(6,'(" k+1, max val prev residual=",i4,e14.5)')k+1, &
          !             maxval(abs(mg(k+1)%f(1:mg(k+1)%ny-1,1:mg(k+1)%nx-1)))
        call f2c_full_weight(mg(k)%w,mg(k+1)%f,mg(k)%nx,mg(k)%ny,mg(k+1)%nx,mg(k+1)%ny)
          !write(6,'(" k+1, max val transfered residual=",i4,e14.5)')k+1, &
          !             maxval(abs(mg(k+1)%f(1:mg(k+1)%ny-1,1:mg(k+1)%nx-1)))
        mg(k+1)%v=zero

      end do

    end do
            !write(6,'(" at 3 in fmg, min,max f =",2e15.4)')minval(f),maxval(f)

    v=mg(1)%v
            !write(6,'(" at 4 in fmg, min,max f =",2e15.4)')minval(f),maxval(f)

  end subroutine fmg

  subroutine fmg_ad(v,f,helmholtz,nrelax1,nrelax2,nrelax_solve,nx,ny)

!   adjoint of full multigrid solution on fixed domain with homogeneous dirichlet boundary conditions

!      solve delsqr(v)    = f   helmholtz=.false.
!           (delsqr-R)(v) = f   helmholtz=.true.

    use constants, only: zero,one

    integer(i_kind),intent(in)::nx,ny,nrelax1,nrelax2,nrelax_solve
    logical,intent(in)::        helmholtz
    real(r_kind),intent(inout)::   f(0:ny,0:nx)   ! ! in actual practice, v is in and 
    real(r_kind),intent(in)::      v(0:ny,0:nx)   ! !    f is inout, but make both inout for adjoint test

    integer(i_kind) irelax,k,m
    real(r_kind) helmholtz_factor

    if(nxall(1) /= nx.or.nyall(1) /= ny) then
       write(6,*)' inconsistent input nx,ny in fmg, program stops'
       stop
    end if

    helmholtz_factor=zero
    if(helmholtz) helmholtz_factor=one

    mg(1)%v=v
    mg(1)%f=f

!      adjoint of main fmg loop:

    do m=1,mgrid

      if(m >  1) then
        do k=mgrid-1,m-1,-1

!        adjoint of transfer residual to next coarser grid
          call f2c_full_weight_ad(mg(k)%w,mg(k+1)%f,mg(k)%nx,mg(k)%ny,mg(k+1)%nx,mg(k+1)%ny)

!        adjoint of compute residual

         mg(k)%f=mg(k)%w+mg(k)%f
         call forward_op_ad(mg(k)%w2,mg(k)%w,mg(k)%nx,mg(k)%ny,k,helmholtz_factor)
         mg(k)%v=mg(k)%v-mg(k)%w2
         call eval_bdys_ad(mg(k)%v,mg(k),mg(k)%nx,mg(k)%ny)

!           adjoint of relax
          do irelax=1,nrelax1
            if(helmholtz) then
              call relax_ad(mg(k)%v,mg(k)%f, &
                         mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                         mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                         mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                       mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
            else
              call relax_ad(mg(k)%v,mg(k)%f, &
                         mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                         mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                         mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                       mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
            end if
          end do

        end do

        call eval_bdys_ad(mg(m-1)%v,mg(m-1),mg(m-1)%nx,mg(m-1)%ny)
        call c2f_bicubic_ad(mg(m)%v,mg(m-1)%v,mg(m)%nx,mg(m)%ny,mg(m-1)%nx,mg(m-1)%ny,mg(m-1)%bicubic_mask)
        call eval_bdys_ad(mg(m)%v,mg(m),mg(m)%nx,mg(m)%ny)
        mg(m)%f=zero

      end if

      do k=m,mgrid-1

!         adjoint of relax

        do irelax=1,nrelax2
          if(helmholtz) then
            call relax_ad(mg(k)%v,mg(k)%f, &
                       mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                       mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                       mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
          else
            call relax_ad(mg(k)%v,mg(k)%f, &
                       mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                       mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                       mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
          end if
        end do

!        adjoint of update v(k) with v(k+1)

        mg(k)%w=mg(k)%v
        call eval_bdys_ad(mg(k)%w,mg(k),mg(k)%nx,mg(k)%ny)
        call c2f_bilinear_ad(mg(k+1)%v,mg(k)%w,mg(k+1)%nx,mg(k+1)%ny,mg(k)%nx,mg(k)%ny)
        call eval_bdys_ad(mg(k+1)%v,mg(k+1),mg(k+1)%nx,mg(k+1)%ny)
        mg(k+1)%f=zero

      end do

!      adjoint of exact solution at level mgrid

      k=mgrid
      do irelax=1,nrelax_solve
        if(helmholtz) then
          call relax_ad(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                     mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        else
          call relax_ad(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                     mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        end if
      end do

    end do

    do k=mgrid,2,-1
      call f2c_full_weight_ad(mg(k-1)%w,mg(k)%f,mg(k-1)%nx,mg(k-1)%ny,mg(k)%nx,mg(k)%ny)
      mg(k-1)%f=mg(k-1)%f+mg(k-1)%w
    end do
    f=mg(1)%f

  end subroutine fmg_ad

  subroutine vcycle(f,v,nx,ny,mgrid2,nrelax1,nrelax2,nrelax_solve,helmholtz)

!       vcycle over mgrid2 grid levels, using nrelax1 relaxation cycles going from fine to coarse
!        with residuals, nrelax_solve relaxation cycles on coarsest grid, and nrelax2 relaxation
!        cycles going back up from coarse to fine with corrections.

    use constants, only: zero,one

    integer(i_kind),intent(in)::nx,ny,mgrid2,nrelax1,nrelax2,nrelax_solve
    logical,intent(in)::        helmholtz
    real(r_kind),intent(in):: f(0:ny,0:nx)
    real(r_kind),intent(inout):: v(0:ny,0:nx)

    integer(i_kind) irelax,k
    real(r_kind) helmholtz_factor

    helmholtz_factor=zero
    if(helmholtz) helmholtz_factor=one

    mg(1)%f=f
  ! mg(1)%v=zero
    mg(1)%v=v

!                    fine to coarse residuals (descend left side of V cycle)
    do k=1,mgrid2-1

!       relax

      do irelax=1,nrelax1
        if(helmholtz) then
          call relax(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                     mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        else
          call relax(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                     mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        end if
      end do

!        compute residual
    
      call eval_bdys(mg(k)%v,mg(k),mg(k)%nx,mg(k)%ny)
      mg(k)%w=zero
      call forward_op(mg(k)%v,mg(k)%w,mg(k)%nx,mg(k)%ny,k,helmholtz_factor)
    
      mg(k)%w=mg(k)%f-mg(k)%w      !  now have residual in mg(k)%w

!        transfer residual to next coarser grid

      call f2c_full_weight(mg(k)%w,mg(k+1)%f,mg(k)%nx,mg(k)%ny,mg(k+1)%nx,mg(k+1)%ny)
      mg(k+1)%v=zero

    end do

!                 solve (bottom of V cycle)
    k=mgrid2
    do irelax=1,nrelax_solve
      if(helmholtz) then
        call relax(mg(k)%v,mg(k)%f, &
                   mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                   mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                   mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                   mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
      else
        call relax(mg(k)%v,mg(k)%f, &
                   mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                   mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                   mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                   mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
      end if
    end do

!                   coarse to fine corrections (ascend right side of V cycle)

    do k=mgrid2-1,1,-1

!        update v(k) with v(k+1)

      call eval_bdys(mg(k+1)%v,mg(k+1),mg(k+1)%nx,mg(k+1)%ny)
      call c2f_bilinear(mg(k+1)%v,mg(k)%w,mg(k+1)%nx,mg(k+1)%ny,mg(k)%nx,mg(k)%ny)
      call eval_bdys(mg(k)%w,mg(k),mg(k)%nx,mg(k)%ny)
      mg(k)%v=mg(k)%v+mg(k)%w

!         relax

      do irelax=1,nrelax2
        if(helmholtz) then
          call relax(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                     mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                   mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        else
          call relax(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                     mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                   mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        end if
      end do

    end do

    v=mg(1)%v

  end subroutine vcycle

  subroutine vcycle_ad(f,v,nx,ny,mgrid2,nrelax1,nrelax2,nrelax_solve,helmholtz)

!       vcycle over mgrid2 grid levels, using nrelax1 relaxation cycles going from fine to coarse
!        with residuals, nrelax_solve relaxation cycles on coarsest grid, and nrelax2 relaxation
!        cycles going back up from coarse to fine with corrections.

    use constants, only: zero,one

    integer(i_kind),intent(in)::nx,ny,mgrid2,nrelax1,nrelax2,nrelax_solve
    logical,intent(in)::        helmholtz
    real(r_kind),intent(inout):: f(0:ny,0:nx)
    real(r_kind),intent(inout):: v(0:ny,0:nx)

    integer(i_kind) irelax,k
    real(r_kind) helmholtz_factor

    helmholtz_factor=zero
    if(helmholtz) helmholtz_factor=one

    mg(1)%v=v
    mg(1)%f=f

!                   adjoint of coarse to fine corrections (ascend right side of V cycle)

    do k=1,mgrid2-1

!         adjoint of relax

      do irelax=1,nrelax2
        if(helmholtz) then
          call relax_ad(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                     mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                   mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        else
          call relax_ad(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                     mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                   mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        end if
      end do

!        adjoint of update v(k) with v(k+1)

      mg(k)%w=mg(k)%v
      call eval_bdys_ad(mg(k)%w,mg(k),mg(k)%nx,mg(k)%ny)
      call c2f_bilinear_ad(mg(k+1)%v,mg(k)%w,mg(k+1)%nx,mg(k+1)%ny,mg(k)%nx,mg(k)%ny)
      call eval_bdys_ad(mg(k+1)%v,mg(k+1),mg(k+1)%nx,mg(k+1)%ny)
      mg(k+1)%f=zero

    end do

!                 adjoint of solve (bottom of V cycle)
    k=mgrid2
    do irelax=1,nrelax_solve
      if(helmholtz) then
        call relax_ad(mg(k)%v,mg(k)%f, &
                   mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                   mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                   mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                   mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
      else
        call relax_ad(mg(k)%v,mg(k)%f, &
                   mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                   mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                   mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                   mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
      end if
    end do

!                    adjoint of fine to coarse residuals (descend left side of V cycle)
    do k=mgrid2-1,1,-1

!        adjoint of transfer residual to next coarser grid

      call f2c_full_weight_ad(mg(k)%w,mg(k+1)%f,mg(k)%nx,mg(k)%ny,mg(k+1)%nx,mg(k+1)%ny)

!        adjoint of compute residual
    
      call forward_op_ad(mg(k)%w2,mg(k)%w,mg(k)%nx,mg(k)%ny,k,helmholtz_factor)
      mg(k)%v=mg(k)%v-mg(k)%w2
      mg(k)%f=mg(k)%f+mg(k)%w
      call eval_bdys_ad(mg(k)%v,mg(k),mg(k)%nx,mg(k)%ny)

!       adjoint of relax

      do irelax=1,nrelax1
        if(helmholtz) then
          call relax_ad(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarxh,mg(k)%ubarxh,mg(k)%obarxh, &
                     mg(k)%dbaryh,mg(k)%ubaryh,mg(k)%obaryh, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        else
          call relax_ad(mg(k)%v,mg(k)%f, &
                     mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx, &
                     mg(k)%dbary,mg(k)%ubary,mg(k)%obary, &
                     mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,mg(k)%nx,mg(k)%ny, &
                     mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        end if
      end do

    end do

    v=mg(1)%v
    f=mg(1)%f

  end subroutine vcycle_ad

  subroutine test_hop(test_div,nx1,ny1,helmholtz_on)

    use constants, only: zero,one,two

    integer(i_kind),intent(in):: nx1,ny1
    real(r_kind),intent(in):: test_div(0:ny1,0:nx1)
    logical,intent(in):: helmholtz_on

    real(r_kind) helmholtz
    real(r_kind),allocatable:: f(:,:),v(:,:),w(:,:)
!             real(r_kind) time0,timef

!             run tests with test_div_full
    allocate(f(0:ny1,0:nx1))
    allocate(v(0:ny1,0:nx1))
    allocate(w(0:ny1,0:nx1))
    f=test_div
        write(6,'(" min,max f before call fmg=",2e15.4)')minval(f),maxval(f)
        call fmg(v,f,helmholtz_on,1,1,20,mg(1)%nx,mg(1)%ny)
!              write(6,'(" time in fmg =",f15.6," seconds")') .001*(timef()-time0)
        w=zero
        helmholtz=zero
        if(helmholtz_on) helmholtz=one
        call forward_op(v,w,mg(1)%nx,mg(1)%ny,1,helmholtz)
        w=f-w
              write(6,'(" min,max f after  call fmg=",2e15.4)') &
                          minval(f(1:ny1-1,1:nx1-1)), &
                          maxval(f(1:ny1-1,1:nx1-1))
              write(6,'(" min,max r after  call fmg=",2e15.4)') &
                          minval(w(1:ny1-1,1:nx1-1)), &
                          maxval(w(1:ny1-1,1:nx1-1))
             if(helmholtz_on) then
               call outgrad1(v,'vhs_test20x',mg(1)%nx+1,mg(1)%ny+1)
               call outgrad1(f,'fhs_test20x',mg(1)%nx+1,mg(1)%ny+1)
               call outgrad1(w,'rhs_test20x',mg(1)%nx+1,mg(1)%ny+1)
             else
               call outgrad1(v,'v_test20x',mg(1)%nx+1,mg(1)%ny+1)
               call outgrad1(f,'f_test20x',mg(1)%nx+1,mg(1)%ny+1)
               call outgrad1(w,'r_test20x',mg(1)%nx+1,mg(1)%ny+1)
             end if

  end subroutine test_hop

  subroutine test_relax

    use constants, only: zero,one

    integer(i_kind) i,k,nx,ny
    character(50) string

   !do k=1,mgrid
    do k=1,1
      nx=mg(k)%nx
      ny=mg(k)%ny
                write(6,*)' at 1 in test_relax, k,nx,ny=',k,nx,ny
      call random_number(mg(k)%v)
      call eval_bdys(mg(k)%v,mg(k),mg(k)%nx,mg(k)%ny)
      mg(k)%z=mg(k)%v
    ! mg(k)%v=zero
    ! mg(k)%v=mg(k)%r00
    ! mg(k)%z=mg(k)%r00
    ! do j=-ny+1,ny-1
    !!  mg(k)%v=mg(k)%r00
    !!  mg(k)%z=mg(k)%r00
    !   call forward_op_x(mg(k)%v,mg(k)%f,nx,ny,j,k,zero)
    ! end do
    !   mg(k)%w=-mg(k)%f
    ! do j=1,ny-1
    !   mg(k)%w(j,1   )=mg(k)%w(j,    1)+mg(k)%h0m(j,    1)*mg(k)%v(j,  0)
    !   mg(k)%w(j,nx-1)=mg(k)%w(j, nx-1)+mg(k)%h0p(j, nx-1)*mg(k)%v(j, nx)
    !   call inverse_luop_x_1(mg(k)%w,mg(k)%v,mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx,nx,ny,j)
    ! end do
    !   call outgrad1(mg(k)%z,'exact_v_xtest',nx+1,ny+1)
    !   call outgrad1(mg(k)%v,'solve_v_xtest',nx+1,ny+1)
    !        if(1 /= 0) stop

      call eval_bdys(mg(k)%v,mg(k),mg(k)%nx,mg(k)%ny)
      mg(k)%f=zero
      call forward_op(mg(k)%v,mg(k)%f,nx,ny,k,zero)
      mg(k)%v(1:ny-1,1:nx-1)=zero
     !do i=1,1000
      do i=1,60
        call relax(mg(k)%v,mg(k)%f,mg(k)%dbarx,mg(k)%ubarx,mg(k)%obarx,mg(k)%dbary,mg(k)%ubary,mg(k)%obary,&
                        mg(k)%h0p,mg(k)%h0m,mg(k)%hp0,mg(k)%hm0,nx,ny, &
                        mg(k)%b_over_a_east_neg,mg(k)%b_over_a_north_neg)
        call eval_bdys(mg(k)%v,mg(k),mg(k)%nx,mg(k)%ny)
        mg(k)%w=zero
        call forward_op(mg(k)%v,mg(k)%w,nx,ny,k,zero)
        mg(k)%w=mg(k)%f-mg(k)%w
           write(6,'(" iter,max(|r|)/max(|f| = ",i5,e15.4)') i,maxval(abs(mg(k)%w(1:ny-1,1:nx-1)))/ &
                                                               maxval(abs(mg(k)%f(1:ny-1,1:nx-1)))
           write(6,'(" iter,max(|v-v0|)/max(|v0| = ",i5,e15.4)') &
                         i,maxval(abs(mg(k)%v-mg(k)%z))/maxval(abs(mg(k)%z))
        if(i >  20) cycle
        write(string,'("f",i2.2)')i
        call outgrad1(mg(k)%f,trim(string),nx+1,ny+1)
        write(string,'("r_odev_xy",i2.2)')i
        call outgrad1(mg(k)%w,trim(string),nx+1,ny+1)
      end do
        call outgrad1(mg(k)%z,'exact_solution',nx+1,ny+1)
        call outgrad1(mg(k)%v,'relax60_solution',nx+1,ny+1)
    end do

  end subroutine test_relax

  subroutine test_adjoint

    use constants, only: zero,two

    real(r_kind) xtz,yty,helmholtz_factor,errmax
    integer(i_kind) i,ii,j,jj,kc,kf,ihem,nrelax1,nrelax2,nrelax_solve
    logical helmholtz
    integer(i_kind) mgrid2,ivar

!              c2f_bilinear

    errmax=zero
    do kf=1,mgrid-1
      kc=kf+1
      call random_number(mg(kc)%v)
      mg(kc)%w=mg(kc)%v
      call c2f_bilinear(mg(kc)%v,mg(kf)%f,mg(kc)%nx,mg(kc)%ny,mg(kf)%nx,mg(kf)%ny)
      yty=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          yty=yty+mg(kf)%f(j,i)**2
        end do
      end do
      call c2f_bilinear_ad(mg(kc)%v,mg(kf)%f,mg(kc)%nx,mg(kc)%ny,mg(kf)%nx,mg(kf)%ny)
      xtz=zero
      do i=0,mg(kc)%nx
        do j=0,mg(kc)%ny
          xtz=xtz+mg(kc)%v(j,i)*mg(kc)%w(j,i)
        end do
      end do
      write(6,'(" c2f_bilinear_ad (aT*a), kc,kf,xtz,yty=",2i3,2e22.15,e10.3)') &
                     kc,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
    end do

    do kf=1,mgrid-1
      kc=kf+1
      call random_number(mg(kf)%v)
      mg(kf)%w=mg(kf)%v
      call c2f_bilinear_ad(mg(kc)%f,mg(kf)%v,mg(kc)%nx,mg(kc)%ny,mg(kf)%nx,mg(kf)%ny)
      yty=zero
      do i=0,mg(kc)%nx
        do j=0,mg(kc)%ny
          yty=yty+mg(kc)%f(j,i)**2
        end do
      end do
      call c2f_bilinear(mg(kc)%f,mg(kf)%v,mg(kc)%nx,mg(kc)%ny,mg(kf)%nx,mg(kf)%ny)
      xtz=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          xtz=xtz+mg(kf)%v(j,i)*mg(kf)%w(j,i)
        end do
      end do
      write(6,'(" c2f_bilinear_ad (a*aT), kc,kf,xtz,yty=",2i3,2e22.15,e10.3)') &
                     kc,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
    end do
          write(6,'(" max error for c2f_bilinear_ad=",e10.3)') errmax

!              c2f_bicubic

    errmax=zero
    do kf=1,mgrid-1
      kc=kf+1
      call random_number(mg(kc)%v)
      mg(kc)%w=mg(kc)%v
      call c2f_bicubic(mg(kc)%v,mg(kf)%f,mg(kc)%nx,mg(kc)%ny,mg(kf)%nx,mg(kf)%ny,mg(kf)%bicubic_mask)
      yty=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          yty=yty+mg(kf)%f(j,i)**2
        end do
      end do
      call c2f_bicubic_ad(mg(kc)%v,mg(kf)%f,mg(kc)%nx,mg(kc)%ny,mg(kf)%nx,mg(kf)%ny,mg(kf)%bicubic_mask)
      xtz=zero
      do i=0,mg(kc)%nx
        do j=0,mg(kc)%ny
          xtz=xtz+mg(kc)%v(j,i)*mg(kc)%w(j,i)
        end do
      end do
      write(6,'(" c2f_bicubic_ad (aT*a), kc,kf,xtz,yty=",2i3,2e22.15,e10.3)') &
                     kc,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
    end do

    do kf=1,mgrid-1
      kc=kf+1
      call random_number(mg(kf)%v)
      mg(kf)%w=mg(kf)%v
      call c2f_bicubic_ad(mg(kc)%f,mg(kf)%v,mg(kc)%nx,mg(kc)%ny,mg(kf)%nx,mg(kf)%ny,mg(kf)%bicubic_mask)
      yty=zero
      do i=0,mg(kc)%nx
        do j=0,mg(kc)%ny
          yty=yty+mg(kc)%f(j,i)**2
        end do
      end do
      call c2f_bicubic(mg(kc)%f,mg(kf)%v,mg(kc)%nx,mg(kc)%ny,mg(kf)%nx,mg(kf)%ny,mg(kf)%bicubic_mask)
      xtz=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          xtz=xtz+mg(kf)%v(j,i)*mg(kf)%w(j,i)
        end do
      end do
      write(6,'(" c2f_bicubic_ad (a*aT), kc,kf,xtz,yty=",2i3,2e22.15,e10.3)') &
                     kc,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
    end do
          write(6,'(" max error for c2f_bicubic_ad=",e10.3)') errmax

!              eval_bdys

    errmax=zero
    do kf=1,mgrid
      call random_number(mg(kf)%v)
      mg(kf)%w=mg(kf)%v
      call eval_bdys(mg(kf)%v,mg(kf),mg(kf)%nx,mg(kf)%ny)
      yty=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          yty=yty+mg(kf)%v(j,i)**2
        end do
      end do
      call eval_bdys_ad(mg(kf)%v,mg(kf),mg(kf)%nx,mg(kf)%ny)
      xtz=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          xtz=xtz+mg(kf)%v(j,i)*mg(kf)%w(j,i)
        end do
      end do
      write(6,'(" eval_bdys_ad (aT*a), k,xtz,yty=",i3,2e22.15,e10.3)') &
                     kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
    end do

    do kf=1,mgrid
      call random_number(mg(kf)%v)
      mg(kf)%w=mg(kf)%v
      call eval_bdys_ad(mg(kf)%v,mg(kf),mg(kf)%nx,mg(kf)%ny)
      yty=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          yty=yty+mg(kf)%v(j,i)**2
        end do
      end do
      call eval_bdys(mg(kf)%v,mg(kf),mg(kf)%nx,mg(kf)%ny)
      xtz=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          xtz=xtz+mg(kf)%v(j,i)*mg(kf)%w(j,i)
        end do
      end do
      write(6,'(" eval_bdys_ad (a*aT), k,xtz,yty=",i3,2e22.15,e10.3)') &
                     kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
    end do
          write(6,'(" max error for eval_bdys_ad=",e10.3)') errmax

!              f2c_full_weight

    errmax=zero
    do kf=1,mgrid-1
      kc=kf+1
      call random_number(mg(kf)%v)
      mg(kf)%w=mg(kf)%v
      call f2c_full_weight(mg(kf)%v,mg(kc)%f,mg(kf)%nx,mg(kf)%ny,mg(kc)%nx,mg(kc)%ny)
      yty=zero
      do i=0,mg(kc)%nx
        do j=0,mg(kc)%ny
          yty=yty+mg(kc)%f(j,i)**2
        end do
      end do
      call f2c_full_weight_ad(mg(kf)%v,mg(kc)%f,mg(kf)%nx,mg(kf)%ny,mg(kc)%nx,mg(kc)%ny)
      xtz=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          xtz=xtz+mg(kf)%v(j,i)*mg(kf)%w(j,i)
        end do
      end do
      write(6,'(" f2c_full_weight_ad (aT*a), kc,kf,xtz,yty=",2i3,2e22.15,e10.3)') &
                     kc,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
    end do

    do kf=1,mgrid-1
      kc=kf+1
      call random_number(mg(kc)%v)
      mg(kc)%w=mg(kc)%v
      call f2c_full_weight_ad(mg(kf)%f,mg(kc)%v,mg(kf)%nx,mg(kf)%ny,mg(kc)%nx,mg(kc)%ny)
      yty=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          yty=yty+mg(kf)%f(j,i)**2
        end do
      end do
      call f2c_full_weight(mg(kf)%f,mg(kc)%v,mg(kf)%nx,mg(kf)%ny,mg(kc)%nx,mg(kc)%ny)
      xtz=zero
      do i=0,mg(kc)%nx
        do j=0,mg(kc)%ny
          xtz=xtz+mg(kc)%v(j,i)*mg(kc)%w(j,i)
        end do
      end do
      write(6,'(" f2c_full_weight_ad (a*aT), kc,kf,xtz,yty=",2i3,2e22.15,e10.3)') &
                     kc,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
    end do
          write(6,'(" max error for f2c_full_weight_ad=",e10.3)') errmax

!              forward_op

    errmax=zero
    do ihem=0,1
      helmholtz_factor=ihem
      do kf=1,mgrid
        call random_number(mg(kf)%v)
        mg(kf)%w=mg(kf)%v
        call forward_op(mg(kf)%v,mg(kf)%f,mg(kf)%nx,mg(kf)%ny,kf,helmholtz_factor)
        yty=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            yty=yty+mg(kf)%f(j,i)**2
          end do
        end do
        call forward_op_ad(mg(kf)%v,mg(kf)%f,mg(kf)%nx,mg(kf)%ny,kf,helmholtz_factor)
        xtz=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            xtz=xtz+mg(kf)%v(j,i)*mg(kf)%w(j,i)
          end do
        end do
        write(6,'(" forward_op_ad (aT*a), h,k,xtz,yty=",i2,i3,2e22.15,e10.3)') &
                       ihem,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
      end do

      do kf=1,mgrid
        call random_number(mg(kf)%f)
        mg(kf)%w=mg(kf)%f
        call forward_op_ad(mg(kf)%v,mg(kf)%f,mg(kf)%nx,mg(kf)%ny,kf,helmholtz_factor)
        yty=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            yty=yty+mg(kf)%v(j,i)**2
          end do
        end do
        call forward_op(mg(kf)%v,mg(kf)%f,mg(kf)%nx,mg(kf)%ny,kf,helmholtz_factor)
        xtz=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            xtz=xtz+mg(kf)%f(j,i)*mg(kf)%w(j,i)
          end do
        end do
        write(6,'(" forward_op_ad (a*aT), h,k,xtz,yty=",i2,i3,2e22.15,e10.3)') &
                       ihem,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
      end do
    end do
          write(6,'(" max error for forward_op_ad=",e10.3)') errmax

!              inverse_luop_x

    errmax=zero
    do ii=1,1
    do jj=1,2
    do ihem=0,1
      helmholtz_factor=ihem
      do kf=1,mgrid
        call random_number(mg(kf)%v)
        mg(kf)%w=mg(kf)%v
        mg(kf)%f=zero
        if(ihem == 0) then
          call inverse_luop_x(mg(kf)%v,mg(kf)%f,mg(kf)%dbarx,mg(kf)%ubarx,mg(kf)%obarx, &
                               mg(kf)%nx,mg(kf)%ny,jj,ii)
        else
          call inverse_luop_x(mg(kf)%v,mg(kf)%f,mg(kf)%dbarxh,mg(kf)%ubarxh,mg(kf)%obarxh, &
                               mg(kf)%nx,mg(kf)%ny,jj,ii)
        end if
        yty=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            yty=yty+mg(kf)%f(j,i)**2
          end do
        end do
        mg(kf)%v=zero
        if(ihem == 0) then
          call inverse_luop_x_ad(mg(kf)%v,mg(kf)%f,mg(kf)%dbarx,mg(kf)%ubarx,mg(kf)%obarx, &
                               mg(kf)%nx,mg(kf)%ny,jj,ii)
        else
          call inverse_luop_x_ad(mg(kf)%v,mg(kf)%f,mg(kf)%dbarxh,mg(kf)%ubarxh,mg(kf)%obarxh, &
                               mg(kf)%nx,mg(kf)%ny,jj,ii)
        end if
        xtz=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            xtz=xtz+mg(kf)%v(j,i)*mg(kf)%w(j,i)
          end do
        end do
        write(6,'(" inverse_luop_x_ad (aT*a), jj,ii,h,k,xtz,yty=",3i2,i3,2e22.15,e10.3)') &
                       jj,ii,ihem,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
      end do

      do kf=1,mgrid
        call random_number(mg(kf)%f)
        mg(kf)%w=mg(kf)%f
        mg(kf)%v=zero
        if(ihem == 0) then
          call inverse_luop_x_ad(mg(kf)%v,mg(kf)%f,mg(kf)%dbarx,mg(kf)%ubarx,mg(kf)%obarx, &
                               mg(kf)%nx,mg(kf)%ny,jj,ii)
        else
          call inverse_luop_x_ad(mg(kf)%v,mg(kf)%f,mg(kf)%dbarxh,mg(kf)%ubarxh,mg(kf)%obarxh, &
                               mg(kf)%nx,mg(kf)%ny,jj,ii)
        end if
        yty=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            yty=yty+mg(kf)%v(j,i)**2
          end do
        end do
        mg(kf)%f=zero
        if(ihem == 0) then
          call inverse_luop_x(mg(kf)%v,mg(kf)%f,mg(kf)%dbarx,mg(kf)%ubarx,mg(kf)%obarx, &
                               mg(kf)%nx,mg(kf)%ny,jj,ii)
        else
          call inverse_luop_x(mg(kf)%v,mg(kf)%f,mg(kf)%dbarxh,mg(kf)%ubarxh,mg(kf)%obarxh, &
                               mg(kf)%nx,mg(kf)%ny,jj,ii)
        end if
        xtz=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            xtz=xtz+mg(kf)%f(j,i)*mg(kf)%w(j,i)
          end do
        end do
        write(6,'(" inverse_luop_x_ad (a*aT), jj,ii,h,k,xtz,yty=",3i2,i3,2e22.15,e10.3)') &
                       jj,ii,ihem,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
      end do
    end do
    end do
    end do
          write(6,'(" max error for inverse_luop_x_ad=",e10.3)') errmax

!              inverse_luop_y

    errmax=zero
    do ii=1,2
    do jj=1,1
    do ihem=0,1
      helmholtz_factor=ihem
      do kf=1,mgrid
        call random_number(mg(kf)%v)
        mg(kf)%w=mg(kf)%v
        mg(kf)%f=zero
        if(ihem == 0) then
          call inverse_luop_y(mg(kf)%v,mg(kf)%f,mg(kf)%dbary,mg(kf)%ubary,mg(kf)%obary, &
                               mg(kf)%nx,mg(kf)%ny,ii,jj)
        else
          call inverse_luop_y(mg(kf)%v,mg(kf)%f,mg(kf)%dbaryh,mg(kf)%ubaryh,mg(kf)%obaryh, &
                               mg(kf)%nx,mg(kf)%ny,ii,jj)
        end if
        yty=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            yty=yty+mg(kf)%f(j,i)**2
          end do
        end do
        mg(kf)%v=zero
        if(ihem == 0) then
          call inverse_luop_y_ad(mg(kf)%v,mg(kf)%f,mg(kf)%dbary,mg(kf)%ubary,mg(kf)%obary, &
                               mg(kf)%nx,mg(kf)%ny,ii,jj)
        else
          call inverse_luop_y_ad(mg(kf)%v,mg(kf)%f,mg(kf)%dbaryh,mg(kf)%ubaryh,mg(kf)%obaryh, &
                               mg(kf)%nx,mg(kf)%ny,ii,jj)
        end if
        xtz=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            xtz=xtz+mg(kf)%v(j,i)*mg(kf)%w(j,i)
          end do
        end do
        write(6,'(" inverse_luop_y_ad (aT*a), ii,jj,h,k,xtz,yty=",3i2,i3,2e22.15,e10.3)') &
                       ii,jj,ihem,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
      end do

      do kf=1,mgrid
        call random_number(mg(kf)%f)
        mg(kf)%w=mg(kf)%f
        mg(kf)%v=zero
        if(ihem == 0) then
          call inverse_luop_y_ad(mg(kf)%v,mg(kf)%f,mg(kf)%dbary,mg(kf)%ubary,mg(kf)%obary, &
                               mg(kf)%nx,mg(kf)%ny,ii,jj)
        else
          call inverse_luop_y_ad(mg(kf)%v,mg(kf)%f,mg(kf)%dbaryh,mg(kf)%ubaryh,mg(kf)%obaryh, &
                               mg(kf)%nx,mg(kf)%ny,ii,jj)
        end if
        yty=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            yty=yty+mg(kf)%v(j,i)**2
          end do
        end do
        mg(kf)%f=zero
        if(ihem == 0) then
          call inverse_luop_y(mg(kf)%v,mg(kf)%f,mg(kf)%dbary,mg(kf)%ubary,mg(kf)%obary, &
                               mg(kf)%nx,mg(kf)%ny,ii,jj)
        else
          call inverse_luop_y(mg(kf)%v,mg(kf)%f,mg(kf)%dbaryh,mg(kf)%ubaryh,mg(kf)%obaryh, &
                               mg(kf)%nx,mg(kf)%ny,ii,jj)
        end if
        xtz=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            xtz=xtz+mg(kf)%f(j,i)*mg(kf)%w(j,i)
          end do
        end do
        write(6,'(" inverse_luop_y_ad (a*aT), ii,jj,h,k,xtz,yty=",3i2,i3,2e22.15,e10.3)') &
                       ii,jj,ihem,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
      end do
    end do
    end do
    end do
          write(6,'(" max error for inverse_luop_y_ad=",e10.3)') errmax

!              relax_ad

    errmax=zero
    do ihem=0,1
      do kf=1,mgrid
        call random_number(mg(kf)%v)
        call random_number(mg(kf)%f)
        mg(kf)%w=mg(kf)%v
        mg(kf)%y=mg(kf)%f
        if(ihem == 1) then
          call relax(mg(kf)%v,mg(kf)%f, &
                      mg(kf)%dbarxh,mg(kf)%ubarxh,mg(kf)%obarxh, &
                      mg(kf)%dbaryh,mg(kf)%ubaryh,mg(kf)%obaryh, &
                      mg(kf)%h0p,mg(kf)%h0m,mg(kf)%hp0,mg(kf)%hm0, &
                      mg(kf)%nx,mg(kf)%ny,mg(kf)%b_over_a_east_neg,mg(kf)%b_over_a_north_neg)
        else
          call relax(mg(kf)%v,mg(kf)%f, &
                      mg(kf)%dbarx,mg(kf)%ubarx,mg(kf)%obarx, &
                      mg(kf)%dbary,mg(kf)%ubary,mg(kf)%obary, &
                      mg(kf)%h0p,mg(kf)%h0m,mg(kf)%hp0,mg(kf)%hm0, &
                      mg(kf)%nx,mg(kf)%ny,mg(kf)%b_over_a_east_neg,mg(kf)%b_over_a_north_neg)
        end if
        yty=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            yty=yty+mg(kf)%f(j,i)**2+mg(kf)%v(j,i)**2
          end do
        end do
        if(ihem == 1) then
          call relax_ad(mg(kf)%v,mg(kf)%f, &
                      mg(kf)%dbarxh,mg(kf)%ubarxh,mg(kf)%obarxh, &
                      mg(kf)%dbaryh,mg(kf)%ubaryh,mg(kf)%obaryh, &
                      mg(kf)%h0p,mg(kf)%h0m,mg(kf)%hp0,mg(kf)%hm0, &
                      mg(kf)%nx,mg(kf)%ny,mg(kf)%b_over_a_east_neg,mg(kf)%b_over_a_north_neg)
        else
          call relax_ad(mg(kf)%v,mg(kf)%f, &
                      mg(kf)%dbarx,mg(kf)%ubarx,mg(kf)%obarx, &
                      mg(kf)%dbary,mg(kf)%ubary,mg(kf)%obary, &
                      mg(kf)%h0p,mg(kf)%h0m,mg(kf)%hp0,mg(kf)%hm0, &
                      mg(kf)%nx,mg(kf)%ny,mg(kf)%b_over_a_east_neg,mg(kf)%b_over_a_north_neg)
        end if
        xtz=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            xtz=xtz+mg(kf)%v(j,i)*mg(kf)%w(j,i)+mg(kf)%f(j,i)*mg(kf)%y(j,i)
          end do
        end do
        write(6,'(" relax_ad (aT*a), h,k,xtz,yty=",i2,i3,2e22.15,e10.3)') &
                       ihem,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
      end do

      do kf=1,mgrid
        call random_number(mg(kf)%v)
        call random_number(mg(kf)%f)
        mg(kf)%w=mg(kf)%v
        mg(kf)%y=mg(kf)%f
        if(ihem == 1) then
          call relax_ad(mg(kf)%v,mg(kf)%f, &
                      mg(kf)%dbarxh,mg(kf)%ubarxh,mg(kf)%obarxh, &
                      mg(kf)%dbaryh,mg(kf)%ubaryh,mg(kf)%obaryh, &
                      mg(kf)%h0p,mg(kf)%h0m,mg(kf)%hp0,mg(kf)%hm0, &
                      mg(kf)%nx,mg(kf)%ny,mg(kf)%b_over_a_east_neg,mg(kf)%b_over_a_north_neg)
        else
          call relax_ad(mg(kf)%v,mg(kf)%f, &
                      mg(kf)%dbarx,mg(kf)%ubarx,mg(kf)%obarx, &
                      mg(kf)%dbary,mg(kf)%ubary,mg(kf)%obary, &
                      mg(kf)%h0p,mg(kf)%h0m,mg(kf)%hp0,mg(kf)%hm0, &
                      mg(kf)%nx,mg(kf)%ny,mg(kf)%b_over_a_east_neg,mg(kf)%b_over_a_north_neg)
        end if
        yty=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            yty=yty+mg(kf)%f(j,i)**2+mg(kf)%v(j,i)**2
          end do
        end do
        if(ihem == 1) then
          call relax(mg(kf)%v,mg(kf)%f, &
                      mg(kf)%dbarxh,mg(kf)%ubarxh,mg(kf)%obarxh, &
                      mg(kf)%dbaryh,mg(kf)%ubaryh,mg(kf)%obaryh, &
                      mg(kf)%h0p,mg(kf)%h0m,mg(kf)%hp0,mg(kf)%hm0, &
                      mg(kf)%nx,mg(kf)%ny,mg(kf)%b_over_a_east_neg,mg(kf)%b_over_a_north_neg)
        else
          call relax(mg(kf)%v,mg(kf)%f, &
                      mg(kf)%dbarx,mg(kf)%ubarx,mg(kf)%obarx, &
                      mg(kf)%dbary,mg(kf)%ubary,mg(kf)%obary, &
                      mg(kf)%h0p,mg(kf)%h0m,mg(kf)%hp0,mg(kf)%hm0, &
                      mg(kf)%nx,mg(kf)%ny,mg(kf)%b_over_a_east_neg,mg(kf)%b_over_a_north_neg)
        end if
        xtz=zero
        do i=0,mg(kf)%nx
          do j=0,mg(kf)%ny
            xtz=xtz+mg(kf)%v(j,i)*mg(kf)%w(j,i)+mg(kf)%f(j,i)*mg(kf)%y(j,i)
          end do
        end do
        write(6,'(" relax_ad (a*aT), h,k,xtz,yty=",i2,i3,2e22.15,e10.3)') &
                       ihem,kf,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
      end do

    end do
          write(6,'(" max error for relax_ad=",e10.3)') errmax

!              vcycle_ad
   
    errmax=zero
    do ivar=1,2
    do ihem=0,1
     helmholtz=ihem == 1
     do nrelax1=1,1
     do nrelax2=1,1
     do nrelax_solve=1,1
     do mgrid2=2,mgrid
   ! do mgrid2=2,2
      kf=1
      mg(kf)%y=zero ; mg(kf)%z=zero
      if(ivar == 1) call random_number(mg(kf)%y)
      if(ivar == 2) call random_number(mg(kf)%z)
      mg(kf)%yy=mg(kf)%y
      mg(kf)%zz=mg(kf)%z
      call vcycle(mg(kf)%y,mg(kf)%z,mg(kf)%nx,mg(kf)%ny,mgrid2,nrelax1,nrelax2,nrelax_solve,helmholtz)
      yty=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          yty=yty+mg(kf)%y(j,i)**2+mg(kf)%z(j,i)**2
        end do
      end do
      call vcycle_ad(mg(kf)%y,mg(kf)%z,mg(kf)%nx,mg(kf)%ny,mgrid2,nrelax1,nrelax2,nrelax_solve,helmholtz)
      xtz=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          xtz=xtz+mg(kf)%y(j,i)*mg(kf)%yy(j,i)+mg(kf)%z(j,i)*mg(kf)%zz(j,i)
        end do
      end do
        write(6,'(" vcycle_ad (aT*a), vhn12sm2,xz,yy=",6i2,2e22.15,e10.3)') &
               ivar,ihem,nrelax1,nrelax2,nrelax_solve,mgrid2,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
   
      mg(kf)%y=zero ; mg(kf)%z=zero
      if(ivar == 1) call random_number(mg(kf)%y)
      if(ivar == 2) call random_number(mg(kf)%z)
      mg(kf)%yy=mg(kf)%y
      mg(kf)%zz=mg(kf)%z
      call vcycle_ad(mg(kf)%y,mg(kf)%z,mg(kf)%nx,mg(kf)%ny,mgrid2,nrelax1,nrelax2,nrelax_solve,helmholtz)
      yty=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          yty=yty+mg(kf)%y(j,i)**2+mg(kf)%z(j,i)**2
        end do
      end do
      call vcycle(mg(kf)%y,mg(kf)%z,mg(kf)%nx,mg(kf)%ny,mgrid2,nrelax1,nrelax2,nrelax_solve,helmholtz)
      xtz=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          xtz=xtz+mg(kf)%y(j,i)*mg(kf)%yy(j,i)+mg(kf)%z(j,i)*mg(kf)%zz(j,i)
        end do
      end do
        write(6,'(" vcycle_ad (a*aT), vhn12sm2,xz,yy=",6i2,2e22.15,e10.3)') &
               ivar,ihem,nrelax1,nrelax2,nrelax_solve,mgrid2,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
     end do
     end do
     end do
     end do
    end do
    end do
    write(6,'(" max error for vcycle_ad=",e10.3)') errmax

!              fmg_ad
   
    errmax=zero
    do ivar=1,2
    do ihem=0,1
     helmholtz=ihem == 1
     do nrelax1=1,1
     do nrelax2=1,1
     do nrelax_solve=25,25
      kf=1
      mg(kf)%y=zero ; mg(kf)%z=zero
      if(ivar == 1) call random_number(mg(kf)%y)
      if(ivar == 2) call random_number(mg(kf)%z)
      mg(kf)%yy=mg(kf)%y
      mg(kf)%zz=mg(kf)%z
      call fmg(mg(kf)%y,mg(kf)%z,helmholtz,nrelax1,nrelax2,nrelax_solve,mg(kf)%nx,mg(kf)%ny)
      yty=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          yty=yty+mg(kf)%y(j,i)**2+mg(kf)%z(j,i)**2
        end do
      end do
      call fmg_ad(mg(kf)%y,mg(kf)%z,helmholtz,nrelax1,nrelax2,nrelax_solve,mg(kf)%nx,mg(kf)%ny)
      xtz=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          xtz=xtz+mg(kf)%y(j,i)*mg(kf)%yy(j,i)+mg(kf)%z(j,i)*mg(kf)%zz(j,i)
        end do
      end do
        write(6,'(" fmg_ad (aT*a), vhn12s,xz,yy=",5i2,2e22.15,e10.3)') &
               ivar,ihem,nrelax1,nrelax2,nrelax_solve,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
   
      mg(kf)%y=zero ; mg(kf)%z=zero
      if(ivar == 1) call random_number(mg(kf)%y)
      if(ivar == 2) call random_number(mg(kf)%z)
      mg(kf)%yy=mg(kf)%y
      mg(kf)%zz=mg(kf)%z
      call fmg_ad(mg(kf)%y,mg(kf)%z,helmholtz,nrelax1,nrelax2,nrelax_solve,mg(kf)%nx,mg(kf)%ny)
      yty=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          yty=yty+mg(kf)%y(j,i)**2+mg(kf)%z(j,i)**2
        end do
      end do
      call fmg(mg(kf)%y,mg(kf)%z,helmholtz,nrelax1,nrelax2,nrelax_solve,mg(kf)%nx,mg(kf)%ny)
      xtz=zero
      do i=0,mg(kf)%nx
        do j=0,mg(kf)%ny
          xtz=xtz+mg(kf)%y(j,i)*mg(kf)%yy(j,i)+mg(kf)%z(j,i)*mg(kf)%zz(j,i)
        end do
      end do
        write(6,'(" fmg_ad (a*aT), vhn12s,xz,yy=",5i2,2e22.15,e10.3)') &
               ivar,ihem,nrelax1,nrelax2,nrelax_solve,xtz,yty,abs(two*(xtz-yty)/(abs(xtz)+abs(yty)))
                     if(abs(xtz)+abs(yty) /= zero) errmax=max(abs(two*(xtz-yty)/(abs(xtz)+abs(yty))),errmax)
     end do
     end do
     end do
    end do
    end do
          write(6,'(" max error for fmg_ad=",e10.3)') errmax

  end subroutine test_adjoint

end module helmholtz_fmg_mod

subroutine fmg_initialize(mype)

  use kinds, only: r_kind,i_kind,r_single
  use constants, only: zero,half,one
  use gridmod, only: region_lat,region_dx,region_dy,nlon,nlat
  use helmholtz_fmg_mod, only: generate_grids,init_mgvars
  use mod_vtrans, only: nvmodes_keep,depths
  implicit none

  integer(i_kind),intent(in):: mype

  integer(i_kind) mode_number
  real(r_kind) phi0

!                 for this version, make parallel by vertical modes, so only have nvmodes_keep copies
!                     this is most simpleminded approach--later modify to take advantage of all processors
!                     available.

   if(mype+1 >  2*nvmodes_keep) return

   mode_number=mod(mype,nvmodes_keep)+1
   phi0=depths(mode_number)
   write(6,*)' in fmg_initialize, mype,mode_number,phi0=',mype,mode_number,phi0

   call generate_grids(nlon,nlat)
   call init_mgvars(region_dx,region_dy,region_lat,phi0)

end subroutine fmg_initialize

subroutine fmg_initialize_e(mype)

  use kinds, only: r_kind,i_kind,r_single
  use constants, only: zero,half,one
  use gridmod, only: region_lat,region_dx,region_dy,nlon,nlat,txy2ll
  use helmholtz_fmg_mod, only: generate_grids,init_mgvars
  use mod_vtrans, only: nvmodes_keep,depths
  use zrnmi_mod, only: zrnmi_initialize
  implicit none

  integer(i_kind),intent(in):: mype

  integer(i_kind) mode_number
  real(r_kind) phi0
  real(r_kind) region_lat_e(0:nlat+1,0:nlon+1)
  real(r_kind) region_dx_e(0:nlat+1,0:nlon+1),region_dy_e(0:nlat+1,0:nlon+1)
  integer(i_kind) i,j
  real(r_kind) rx,ry,rlon

!                 for this version, make parallel by vertical modes, so only have nvmodes_keep copies
!                     this is most simpleminded approach--later modify to take advantage of all processors
!                     available.

   call zrnmi_initialize(mype)

   if(mype+1 >  2*nvmodes_keep) return

   mode_number=mod(mype,nvmodes_keep)+1
   phi0=depths(mode_number)
   write(6,*)' in fmg_initialize_e, mype,mode_number,phi0=',mype,mode_number,phi0

   do j=1,nlon
     do i=1,nlat
       region_lat_e(i,j)=region_lat(i,j)
       region_dx_e(i,j)=region_dx(i,j)
       region_dy_e(i,j)=region_dy(i,j)
     end do
   end do

   do j=1,nlon
     rx=j
     ry=0
     call txy2ll(rx,ry,rlon,region_lat_e(0,j))
     region_dx_e(0,j)=region_dx_e(1,j)
     region_dy_e(0,j)=region_dy_e(1,j)
     ry=nlat+1
     call txy2ll(rx,ry,rlon,region_lat_e(nlat+1,j))
     region_dx_e(nlat+1,j)=region_dx_e(nlat,j)
     region_dy_e(nlat+1,j)=region_dy_e(nlat,j)
   end do
   do i=0,nlat+1
     ry=i
     rx=0
     call txy2ll(rx,ry,rlon,region_lat_e(i,0))
     region_dx_e(i,0)=region_dx_e(i,1)
     region_dy_e(i,0)=region_dy_e(i,1)
     rx=nlon+1
     call txy2ll(rx,ry,rlon,region_lat_e(i,nlon+1))
     region_dx_e(i,nlon+1)=region_dx_e(i,nlon)
     region_dy_e(i,nlon+1)=region_dy_e(i,nlon)
   end do
              if(mype == 0) call outgrad1(region_lat_e,'region_lat_e',nlon+2,nlat+2)
              if(mype == 0) call outgrad1(region_dx_e,'region_dx_e',nlon+2,nlat+2)
              if(mype == 0) call outgrad1(region_dy_e,'region_dy_e',nlon+2,nlat+2)
   

   call generate_grids(nlon+2,nlat+2)
   call init_mgvars(region_dx_e,region_dy_e,region_lat_e,phi0)

end subroutine fmg_initialize_e

subroutine fmg_strong_bal_correction(u_t,v_t,t_t,ps_t,psi,chi,t,ps,bal_diagnostic,fullfield,update,mype)

  use kinds, only: r_kind,i_kind
  use constants, only: zero,two,omega
  use gridmod, only: lat2,lon2,nsig,nlat,nlon,region_lat
  use mod_vtrans, only: vtrans,vtrans_inv,nvmodes_keep,depths
  use mpimod, only: mpi_comm_world,ierror,mpi_sum,mpi_rtype
  use zrnmi_mod, only: zrnmi_filter_uvm2
  use jfunc,only: jiter
  use hybrid_ensemble_parameters, only: uv_hyb_ens
  implicit none

  integer(i_kind),intent(in)::mype
  logical,intent(in)::bal_diagnostic,update,fullfield
  real(r_kind),dimension(lat2,lon2,nsig),intent(in)::u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2),intent(in)::ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::psi,chi,t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps

  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde,vtilde,mtilde
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::delpsitilde,delchitilde,delmtilde,dummytilde
  real(r_kind),dimension(nlat,nlon)::u0t,v0t,m0t,div0t,vor0t,rhs,mtg,delm,divtg,vortg,mtg_x,mtg_y
  real(r_kind),dimension(nlat,nlon)::delvor,deldiv,delpsi,delchi,delf1,delf2
  integer(i_kind) mode_number_a,mode_number_b
  real(r_kind) bal_a (nvmodes_keep),bal_b (nvmodes_keep)
  real(r_kind) bal_a0(nvmodes_keep),bal_b0(nvmodes_keep)
  real(r_kind) bal(nvmodes_keep)
  character(1) bourke_mcgregor_scheme

  integer(i_kind) i,j

  if(uv_hyb_ens) then
     write(0,*)' STOP IN fmg_strong_bal_correction--NOT ABLE TO ACCEPT uv_hyb_ens=.true. YET'
     call stop2(998)
  end if

 !bourke_mcgregor_scheme='A'
  bourke_mcgregor_scheme='B'

  mode_number_a=0 ; mode_number_b=0
  if(mype+1 <= 2*nvmodes_keep) then
    if(mype+1 <= nvmodes_keep) then
      mode_number_a=mype+1
    else
      mode_number_b=mype+1-nvmodes_keep
    end if
  end if
 !write(6,*)' in fmg_strong_bal_correction, mode_number_a,mode_number_b=',mode_number_a,mode_number_b
          !      if(mype >  -10) then
          !        call mpi_finalize(i)
          !        stop
          !      end if

!      vertical mode transform

    utilde=zero ; vtilde=zero ; mtilde=zero                !!! redundant--remove eventually!!!!
    call vtrans(u_t,v_t,t_t,ps_t,utilde,vtilde,mtilde)

!    filter low frequency scales out of tendencies
    call zrnmi_filter_uvm2(utilde,vtilde,mtilde,mype)

!     layout for new rtlnmc forward code:

!1.  u0t,v0t,m0t input on subdomains  --  output will be delchi, delpsi, delm-->delt,delps

!2.  set up slab areas  A:         0     <= mype <= nvmodes_keep-1
!                       B:  nvmodes_keep <= mype <= 2*nvmodes_keep-1

!        require npe >= 2*nvmodes_keep

!3.  subdomains to slabs:  u0t,v0t,m0t --> A and B

    u0t=zero ; v0t=zero ; m0t=zero             !  may be redundant--consider removing!!
    call special_for_llfmg_sub2grid3(utilde,vtilde,mtilde,utilde,vtilde,mtilde,u0t,v0t,m0t,mype)

!4.  compute div0t on A from u0t,v0t and vort0t on B from u0t,v0t

    if(mode_number_a /= 0) then
      call get_div_reg(u0t,v0t,div0t)
    end if
    if(mode_number_b /= 0) then
      call get_vor_reg(u0t,v0t,vor0t)
    end if

!5.  solve HOP*delm = div0t                    on A
!          HOP*mtg  = delsqr*m0t-f*vor0t  on B

    if(mode_number_a /= 0) then
      call fmg_wrapper_e(delm,div0t,.true.,nlon,nlat)
    end if
    if(mode_number_b /= 0) then
      call get_delsqr_reg(m0t,rhs)
      do j=1,nlon
        do i=1,nlat
          rhs(i,j)=rhs(i,j)-two*omega*sin(region_lat(i,j))*vor0t(i,j)
        end do
      end do
      call fmg_wrapper_e(mtg,rhs,.true.,nlon,nlat)
    end if

!6.  diagnostic BAL computation:

!        BAL = integral( mtg_x**2 + mtg_y**2 + phi0*(vortg**2+divtg**2) )

!       bal_a = integral( phi0*divtg**2 )

!       bal_b = integral( mtg_x**2 + mtg_y**2 + phi0*vortg**2 )

!       BAL = bal_a + bal_b

    if(bal_diagnostic) then

      bal_a=zero ; bal_b=zero
      if(mode_number_a /= 0) then
        divtg=div0t
        bal_a=zero
        do j=1,nlon
          do i=1,nlat
            bal_a(mode_number_a)=bal_a(mode_number_a)+depths(mode_number_a)*divtg(i,j)**2
          end do
        end do
      end if
      if(mode_number_b /= 0) then
!                                  vortg=f*mtg/phi0    ; compute grad(mtg)
        do j=1,nlon
          do i=1,nlat
            vortg(i,j)=two*omega*sin(region_lat(i,j))*mtg(i,j)/depths(mode_number_b)
          end do
        end do
        call delx_reg(mtg,mtg_x,.false.)
        call dely_reg(mtg,mtg_y,.false.)
        bal_b=zero
        do j=1,nlon
          do i=1,nlat
            bal_b(mode_number_b)=bal_b(mode_number_b)+depths(mode_number_b)*vortg(i,j)**2 &
                                  +mtg_x(i,j)**2+mtg_y(i,j)**2
          end do
        end do
      end if
      call mpi_allreduce(bal_a,bal_a0,nvmodes_keep,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
      call mpi_allreduce(bal_b,bal_b0,nvmodes_keep,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
      bal=bal_a0+bal_b0
      if(mype == 0) then
        if(fullfield) then
          write(6,*)'FMG_STRONG_BAL:   FULL FIELD BALANCE DIAGNOSTICS --  '
        else
           write(6,*) 'FMG_STRONG_BAL:   INCREMENTAL BALANCE DIAGNOSTICS --  '
        end if
        do i=1,nvmodes_keep
          write(6,'(" jiter,mode,bal_a,b,bal=",2i3,3e20.9)')jiter,i,bal_a0(i),bal_b0(i),bal(i)
        end do
      end if
    end if


!7.  delvor = f*delm/phi0 --> solve delsqr(delpsi) = delvor    on A
!    deldiv = mtg/phi0    --> solve delsqr(delchi) = deldiv    on B     (for bourke_mcgregor_scheme = B )
!    deldiv = m0t/phi0                                                  (for bourke_mcgregor_scheme = A )

    if(mode_number_a /= 0) then
      do j=1,nlon
        do i=1,nlat
          delvor(i,j)=two*omega*sin(region_lat(i,j))*delm(i,j)/depths(mode_number_a)
        end do
      end do
      call fmg_wrapper_e(delpsi,delvor,.false.,nlon,nlat)
    end if
    if(mode_number_b /= 0) then
      if(bourke_mcgregor_scheme == 'A') then
       !write(6,*)' using bourke_mcgregor_scheme A, so deldiv = m0t/phi0'
        do j=1,nlon
          do i=1,nlat
            deldiv(i,j)=m0t(i,j)/depths(mode_number_b)
          end do
        end do
      elseif(bourke_mcgregor_scheme == 'B') then
       !write(6,*)' using bourke_mcgregor_scheme B, so deldiv = mtg/phi0'
        do j=1,nlon
          do i=1,nlat
            deldiv(i,j)=mtg(i,j)/depths(mode_number_b)
          end do
        end do
      end if
      call fmg_wrapper_e(delchi,deldiv,.false.,nlon,nlat)
    end if


!8.  delpsi,delm    A slab --> subdomains
!    delchi,dummy   B slab --> subdomains

!    if update_uv, u_psi,v_psi  A slab --> subdomains
!    if update_uv, u_chi,v_chi  B slab --> subdomains

    delf1=zero ; delf2=zero
    if(mode_number_a /= 0) then
      do j=1,nlon
        do i=1,nlat
          delf1(i,j)= delpsi(i,j)
          delf2 (i,j)=delm(i,j)
        end do
      end do
    end if
    if(mode_number_b /= 0) then
      do j=1,nlon
        do i=1,nlat
          delf1(i,j)= delchi(i,j)
          delf2 (i,j)=delchi(i,j)
        end do
      end do
    end if
    call special_for_llfmg_grid2sub2(delf1,delf2,delpsitilde,delmtilde,delchitilde,dummytilde,mype)
    if(update) call vtrans_inv(delpsitilde,delchitilde,delmtilde,psi,chi,t,ps)

end subroutine fmg_strong_bal_correction

subroutine fmg_strong_bal_correction_ad(u_t,v_t,t_t,ps_t,psi,chi,t,ps,update,mype)

  use kinds, only: r_kind,i_kind
  use constants, only: zero,two,omega
  use gridmod, only: lat2,lon2,nsig,nlat,nlon,region_lat
  use mod_vtrans, only: vtrans_ad,vtrans_inv_ad,nvmodes_keep,depths
! use helmholtz_fmg_mod, only: mg
  use mpimod, only: mpi_comm_world,mpi_sum,mpi_rtype
  use zrnmi_mod, only: zrnmi_filter_uvm2_ad
  use hybrid_ensemble_parameters, only: uv_hyb_ens
  implicit none

  integer(i_kind),intent(in)::mype
  logical,intent(in)::update
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(in)::psi,chi,t
  real(r_kind),dimension(lat2,lon2),intent(in)::ps

  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde,vtilde,mtilde,utdum,vtdum,mtdum
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::delpsitilde,delchitilde,delmtilde,dummytilde
  real(r_kind),dimension(nlat,nlon)::u0t,v0t,m0t,div0t,vor0t,rhs,mtg,delm
  real(r_kind),dimension(nlat,nlon)::delvor,deldiv,delpsi,delchi,delf1,delf2
  real(r_kind),dimension(lat2,lon2,nsig)::dpsi,dchi,dt
  real(r_kind),dimension(lat2,lon2)::dps
  integer(i_kind) i,j,mode_number_a,mode_number_b
  character(1) bourke_mcgregor_scheme

  if(uv_hyb_ens) then
    write(0,*)' STOP IN fmg_strong_bal_correction--NOT ABLE TO ACCEPT uv_hyb_ens=.true. YET'
    call stop2(998)
  end if

 !bourke_mcgregor_scheme='A'
  bourke_mcgregor_scheme='B'

  mode_number_a=0 ; mode_number_b=0
  if(mype+1 <= 2*nvmodes_keep) then
    if(mype+1 <= nvmodes_keep) then
      mode_number_a=mype+1
    else
      mode_number_b=mype+1-nvmodes_keep
    end if
  end if
 !write(6,*)' in fmg_strong_bal_correction_ad, mode_number_a,mode_number_b=',mode_number_a,mode_number_b

  dpsi=zero ; dchi=zero ; dt=zero ; dps=zero
  if(update) then
    dpsi=psi ; dchi=chi
    dt=t ; dps=ps
  end if
  delpsitilde=zero ; delchitilde=zero ; delmtilde=zero
  call vtrans_inv_ad(delpsitilde,delchitilde,delmtilde,dpsi,dchi,dt,dps)

!8.  delpsi,delm    A slab --> subdomains
!    delchi,dummy   B slab --> subdomains

!    if update_uv, u_psi,v_psi  A slab --> subdomains
!    if update_uv, u_chi,v_chi  B slab --> subdomains

  call special_for_llfmg_sub2grid2(delpsitilde,delmtilde,delchitilde,dummytilde,delf1,delf2,mype)

    delpsi=zero ; delchi=zero ; delm=zero
    if(mode_number_a /= 0) then
      do j=1,nlon
        do i=1,nlat
          delpsi(i,j)=delf1(i,j)
          delm(i,j)=delf2(i,j)
        end do
      end do
    end if
    if(mode_number_b /= 0) then
      do j=1,nlon
        do i=1,nlat
          delchi(i,j)=delf1(i,j)
        end do
      end do
    end if

!7.  delvor = f*delm/phi0 --> solve delsqr(delpsi) = delvor    on A
!    deldiv = mtg/phi0    --> solve delsqr(delchi) = deldiv    on B     (for bourke_mcgregor_scheme = B )
!    deldiv = m0t/phi0                                                  (for bourke_mcgregor_scheme = A )

    if(mode_number_a /= 0) then
      delvor=zero
      call fmg_wrapper_e_ad(delpsi,delvor,.false.,nlon,nlat)
      do j=1,nlon
        do i=1,nlat
          delm(i,j)=delm(i,j)+two*omega*sin(region_lat(i,j))*delvor(i,j)/depths(mode_number_a)
        end do
      end do
    end if
    if(mode_number_b /= 0) then
      deldiv=zero
      call fmg_wrapper_e_ad(delchi,deldiv,.false.,nlon,nlat)
      m0t=zero ; mtg=zero
      if(bourke_mcgregor_scheme == 'A') then
       !write(6,*)' using bourke_mcgregor_scheme A, so deldiv = m0t/phi0'
        do j=1,nlon
          do i=1,nlat
            m0t(i,j)=deldiv(i,j)/depths(mode_number_b)
          end do
        end do
      elseif(bourke_mcgregor_scheme == 'B') then
       !write(6,*)' using bourke_mcgregor_scheme B, so deldiv = mtg/phi0'
        do j=1,nlon
          do i=1,nlat
            mtg(i,j)=deldiv(i,j)/depths(mode_number_b)
          end do
        end do
      end if
    end if

!5.  solve HOP*delm = div0t                    on A
!          HOP*mtg  = delsqr*m0t-f*vor0t  on B

    if(mode_number_a /= 0) then
      div0t=zero
      call fmg_wrapper_e_ad(delm,div0t,.true.,nlon,nlat)
    end if
    if(mode_number_b /= 0) then
      rhs=zero
      call fmg_wrapper_e_ad(mtg,rhs,.true.,nlon,nlat)
      do j=1,nlon
        do i=1,nlat
          vor0t(i,j)= -two*omega*sin(region_lat(i,j))*rhs(i,j)
        end do
      end do
      call tget_delsqr_reg(rhs,m0t)
    end if

!4.  compute div0t on A from u0t,v0t and vort0t on B from u0t,v0t

    if(mode_number_a /= 0) then
      call get_div_reg_ad(u0t,v0t,div0t)
    end if
    if(mode_number_b /= 0) then
      call get_vor_reg_ad(u0t,v0t,vor0t)
    end if

!3.  subdomains to slabs:  u0t,v0t,m0t --> A and B

    utilde=zero ; vtilde=zero ; mtilde=zero
    call special_for_llfmg_grid2sub3(u0t,v0t,m0t,utilde,vtdum,mtdum,utdum,vtilde,mtilde,mype)
    utilde=utilde+utdum ; vtilde=vtilde+vtdum

!      adjoint of filter low frequency scales out of tendencies
    call zrnmi_filter_uvm2_ad(utilde,vtilde,mtilde,mype)

!      adjoint of vertical mode transform

    call vtrans_ad(u_t,v_t,t_t,ps_t,utilde,vtilde,mtilde)

end subroutine fmg_strong_bal_correction_ad

subroutine fmg_strong_bal_correction_ad_test(u_t,v_t,t_t,ps_t,psi,chi,t,ps,mype)

!<><><><<><><><>test of fmg_strong_bal_correction_ad><>><<><><<<<<<<<<<<<<<<<>>>>>>>>>>>>>

           use constants, only: zero,two
           use mpimod, only: mpi_rtype,mpi_sum,mpi_comm_world,ierror
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
           implicit none

  integer(i_kind),intent(in)::mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::psi,chi,t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps
           real(r_kind),dimension(lat2,lon2,nsig)::uu_t,vv_t,tt_t
           real(r_kind),dimension(lat2,lon2)::pps_t
           real(r_kind),dimension(lat2,lon2,nsig)::psi2,chi2,t2
           real(r_kind),dimension(lat2,lon2)::ps2
           integer(i_kind) ivar,i,j,k
           real(r_kind) yty,yty0,xtz,xtz0,errmax

     errmax=zero
     do ivar=0,4
      if(ivar >  0) then
       u_t=zero ; v_t=zero ; t_t=zero ; ps_t=zero
       if(ivar == 1) call random_number(u_t)
       if(ivar == 2) call random_number(v_t)
       if(ivar == 3) call random_number(t_t)
       if(ivar == 4) call random_number(ps_t)
      end if
       uu_t=u_t ; vv_t=v_t ; tt_t=t_t ; pps_t=ps_t
       psi=zero ; chi=zero ; t=zero ; ps=zero
       call fmg_strong_bal_correction(u_t,v_t,t_t,ps_t,psi,chi,t,ps,.false.,.false.,.true.,mype)
       yty=zero
       do k=1,nsig
         do i=2,lon2-1
           do j=2,lat2-1
             yty=yty+psi(j,i,k)**2+chi(j,i,k)**2+t(j,i,k)**2
           end do
         end do
       end do
       do i=2,lon2-1
         do j=2,lat2-1
           yty=yty+ps(j,i)**2
         end do
       end do
       call mpi_allreduce(yty,yty0,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
       u_t=zero ; v_t=zero ; t_t=zero ; ps_t=zero
       call fmg_strong_bal_correction_ad(u_t,v_t,t_t,ps_t,psi,chi,t,ps,.true.,mype)
       xtz=zero
       do k=1,nsig
         do i=2,lon2-1
           do j=2,lat2-1
             xtz=xtz+uu_t(j,i,k)*u_t(j,i,k)+vv_t(j,i,k)*v_t(j,i,k)+tt_t(j,i,k)*t_t(j,i,k)
           end do
         end do
       end do
       do i=2,lon2-1
         do j=2,lat2-1
           xtz=xtz+pps_t(j,i)*ps_t(j,i)
         end do
       end do
       call mpi_allreduce(xtz,xtz0,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
         if(mype == 0) &
         write(6,'(" fmg_strong_bal_correction_ad (aT*a), ivar,xx,yy=",i2,2e22.15,e10.3)') &
                ivar,xtz0,yty0,abs(two*(xtz0-yty0)/(abs(xtz0)+abs(yty0)))
               if(abs(xtz0)+abs(yty0) /= zero) errmax=max(abs(two*(xtz0-yty0)/(abs(xtz0)+abs(yty0))),errmax)
 
       if(ivar == 0) cycle
       psi=zero ; chi=zero ; t=zero ; ps=zero
       if(ivar == 1) call random_number(psi)
       if(ivar == 2) call random_number(chi)
       if(ivar == 3) call random_number(t)
       if(ivar == 4) call random_number(ps)
       psi2=psi ; chi2=chi ; t2=t ; ps2=ps
       u_t=zero ; v_t=zero ; t_t=zero ; ps_t=zero
       call fmg_strong_bal_correction_ad(u_t,v_t,t_t,ps_t,psi,chi,t,ps,.true.,mype)
       yty=zero
       do k=1,nsig
         do i=2,lon2-1
           do j=2,lat2-1
             yty=yty+u_t(j,i,k)*u_t(j,i,k)+v_t(j,i,k)*v_t(j,i,k)+t_t(j,i,k)*t_t(j,i,k)
           end do
         end do
       end do
       do i=2,lon2-1
         do j=2,lat2-1
           yty=yty+ps_t(j,i)*ps_t(j,i)
         end do
       end do
       call mpi_allreduce(yty,yty0,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
       psi=zero ; chi=zero ; t=zero ; ps=zero
       call fmg_strong_bal_correction(u_t,v_t,t_t,ps_t,psi,chi,t,ps,.false.,.false.,.true.,mype)
       xtz=zero
       do k=1,nsig
         do i=2,lon2-1
           do j=2,lat2-1
             xtz=xtz+psi2(j,i,k)*psi(j,i,k)+chi2(j,i,k)*chi(j,i,k)+t2(j,i,k)*t(j,i,k)
           end do
         end do
       end do
       do i=2,lon2-1
         do j=2,lat2-1
           xtz=xtz+ps2(j,i)*ps(j,i)
         end do
       end do
       call mpi_allreduce(xtz,xtz0,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
         if(mype == 0) &
         write(6,'(" fmg_strong_bal_correction_ad (a*aT), ivar,xx,yy=",i2,2e22.15,e10.3)') &
                ivar,xtz0,yty0,abs(two*(xtz0-yty0)/(abs(xtz0)+abs(yty0)))
               if(abs(xtz0)+abs(yty0) /= zero) errmax=max(abs(two*(xtz0-yty0)/(abs(xtz0)+abs(yty0))),errmax)
 
     end do
     if(mype == 0) write(6,'(" max error for fmg_strong_bal_correction_ad=",e10.3)') errmax
        if(mype >  -1000) then
              call mpi_finalize(ierror)
              stop
        end if
 
 !<><><><<><><><><<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>

end subroutine fmg_strong_bal_correction_ad_test

subroutine zrnmi_filter_uvm_ad_test(mype)

!<><><><<><><><>test of zrnmi_filter_uvm_ad_test><>><<><><<<<<<<<<<<<<<<<>>>>>>>>>>>>>

           use constants, only: zero,two
           use mpimod, only: mpi_rtype,mpi_sum,mpi_comm_world,ierror
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use mod_vtrans, only: nvmodes_keep
  use zrnmi_mod, only: zrnmi_filter_uvm2,zrnmi_filter_uvm2_ad
           implicit none

  integer(i_kind),intent(in)::mype
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::u1,u2,v1,v2,m1,m2
           integer(i_kind) ivar,i,j,k
           real(r_kind) yty,yty0,xtz,xtz0,errmax

     errmax=zero
     do ivar=1,3
       u1=zero ; v1=zero ; m1=zero
       if(ivar == 1) call random_number(u1)
       if(ivar == 2) call random_number(v1)
       if(ivar == 3) call random_number(m1)
       u2=u1 ; v2=v1 ; m2=m1
       call zrnmi_filter_uvm2(u1,v1,m1,mype)
       yty=zero
       do k=1,nvmodes_keep
         do i=2,lon2-1
           do j=2,lat2-1
             yty=yty+u1(j,i,k)**2+v1(j,i,k)**2+m1(j,i,k)**2
           end do
         end do
       end do
       call mpi_allreduce(yty,yty0,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
       call zrnmi_filter_uvm2_ad(u1,v1,m1,mype)
       xtz=zero
       do k=1,nvmodes_keep
         do i=2,lon2-1
           do j=2,lat2-1
             xtz=xtz+u1(j,i,k)*u2(j,i,k)+v1(j,i,k)*v2(j,i,k)+m1(j,i,k)*m2(j,i,k)
           end do
         end do
       end do
       call mpi_allreduce(xtz,xtz0,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
         if(mype == 0) &
         write(6,'(" zrnmi_filter_uvm2_ad (aT*a), ivar,xx,yy=",i2,2e22.15,e10.3)') &
                ivar,xtz0,yty0,abs(two*(xtz0-yty0)/(abs(xtz0)+abs(yty0)))
               if(abs(xtz0)+abs(yty0) /= zero) errmax=max(abs(two*(xtz0-yty0)/(abs(xtz0)+abs(yty0))),errmax)
 
       u1=zero ; v1=zero ; m1=zero
       if(ivar == 1) call random_number(u1)
       if(ivar == 2) call random_number(v1)
       if(ivar == 3) call random_number(m1)
       u2=u1 ; v2=v1 ; m2=m1
       call zrnmi_filter_uvm2_ad(u1,v1,m1,mype)
       yty=zero
       do k=1,nvmodes_keep
         do i=2,lon2-1
           do j=2,lat2-1
             yty=yty+u1(j,i,k)**2+v1(j,i,k)**2+m1(j,i,k)**2
           end do
         end do
       end do
       call mpi_allreduce(yty,yty0,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
       call zrnmi_filter_uvm2(u1,v1,m1,mype)
       xtz=zero
       do k=1,nvmodes_keep
         do i=2,lon2-1
           do j=2,lat2-1
             xtz=xtz+u1(j,i,k)*u2(j,i,k)+v1(j,i,k)*v2(j,i,k)+m1(j,i,k)*m2(j,i,k)
           end do
         end do
       end do
       call mpi_allreduce(xtz,xtz0,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
         if(mype == 0) &
         write(6,'(" zrnmi_filter_uvm2_ad (a*aT), ivar,xx,yy=",i2,2e22.15,e10.3)') &
                ivar,xtz0,yty0,abs(two*(xtz0-yty0)/(abs(xtz0)+abs(yty0)))
               if(abs(xtz0)+abs(yty0) /= zero) errmax=max(abs(two*(xtz0-yty0)/(abs(xtz0)+abs(yty0))),errmax)

     end do
     if(mype == 0) write(6,'(" max error for zrnmi_filter_uvm2_ad=",e10.3)') errmax
        if(mype >  -1000) then
              call mpi_finalize(ierror)
              stop
        end if
 
 !<><><><<><><><><<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>

end subroutine zrnmi_filter_uvm_ad_test

subroutine fmg_wrapper(v,f,helmholtz,nlon,nlat)

  use kinds, only: r_kind,i_kind
  use helmholtz_fmg_mod,only: fmg
  implicit none

  integer(i_kind),intent(in):: nlon,nlat
  real(r_kind),intent(in)::    f(0:nlat-1,0:nlon-1)
  real(r_kind),intent(out)::   v(0:nlat-1,0:nlon-1)
  logical,intent(in)::         helmholtz

  call fmg(v,f,helmholtz,1,1,60,nlon-1,nlat-1)

end subroutine fmg_wrapper

subroutine fmg_wrapper_e(v,f,helmholtz,nlon,nlat)

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use helmholtz_fmg_mod,only: fmg
  implicit none

  integer(i_kind),intent(in):: nlon,nlat
  real(r_kind),intent(in)::    f(nlat,nlon)
  real(r_kind),intent(out)::   v(nlat,nlon)
  logical,intent(in)::         helmholtz

  real(r_kind),dimension(0:nlat+1,0:nlon+1)::fe,ve
  integer(i_kind) i,j

  fe=zero
  do j=1,nlon
    do i=1,nlat
      fe(i,j)=f(i,j)
    end do
  end do
  call fmg(ve,fe,helmholtz,1,1,60,nlon+1,nlat+1)
  do j=1,nlon
    do i=1,nlat
      v(i,j)=ve(i,j)
    end do
  end do

end subroutine fmg_wrapper_e

subroutine fmg_wrapper_e_ad(v,f,helmholtz,nlon,nlat)

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use helmholtz_fmg_mod,only: fmg_ad
  implicit none

  integer(i_kind),intent(in):: nlon,nlat
  real(r_kind),intent(inout)::    f(nlat,nlon)
  real(r_kind),intent(in)::   v(nlat,nlon)
  logical,intent(in)::         helmholtz

  real(r_kind),dimension(0:nlat+1,0:nlon+1)::fe,ve
  integer(i_kind) i,j

  ve=zero
  do j=1,nlon
    do i=1,nlat
      ve(i,j)=v(i,j)
      fe(i,j)=f(i,j)
    end do
  end do
  call fmg_ad(ve,fe,helmholtz,1,1,60,nlon+1,nlat+1)
  do j=1,nlon
    do i=1,nlat
      f(i,j)=fe(i,j)
    end do
  end do

end subroutine fmg_wrapper_e_ad

subroutine get_delsqr_reg(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_delsqr_reg compute laplacian  
!   prgmmr: parrish          org: np23               date:  2006-02-13
!
! abstract:  compute laplacian
!
! program history log:
!   2006-02-13  parrish
!
!   input argument list:
!     work1  - array to be differentiated
!
!   output argument list:
!     work2  - array containing laplacian
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  implicit none
  
  integer(i_kind) i,j
  real(r_kind),dimension(nlat,nlon):: work1,work2
  real(r_kind),dimension(nlat,nlon):: grid1,grid2,grid3,grid4

  call delx_reg(work1,grid1,(.false.))
  call delx_reg(grid1,grid2,(.true.))
  call dely_reg(work1,grid3,(.false.))
  call dely_reg(grid3,grid4,(.true.))

  do j=1,nlon
    do i=1,nlat
      work2(i,j)=grid2(i,j)+grid4(i,j)
    end do
  end do
  
  return
end subroutine get_delsqr_reg

subroutine tget_delsqr_reg(work2,work1)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tget_delsqr_reg             adjoint of get_delsqr_reg
!   prgmmr: parrish          org: np23               date:  2006-02-13
!
! abstract:  adjoint of get_delsqr_reg
!
! program history log:
!   2006-02-13  parrish
!
!   input argument list:
!     work2  - array containing delsqr variable
!     work1  - previous contents to be accumulated to
!
!   output argument list:
!     work1  - array containing accumulation of adjoint delsqr
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use constants, only: zero
  implicit none

  integer(i_kind) i,j
  real(r_kind),dimension(nlat,nlon):: work1,work2
  real(r_kind),dimension(nlat,nlon):: grid1,grid2,grid3,grid4

  do j=1,nlon
    do i=1,nlat
      grid2(i,j)=zero
      grid4(i,j)=zero
      grid1(i,j)=zero
      grid3(i,j)=zero
    end do
  end do
  do j=1,nlon
    do i=1,nlat
      grid2(i,j)=grid2(i,j)+work2(i,j)
      grid4(i,j)=grid4(i,j)+work2(i,j)
    end do
  end do
  call tdely_reg(grid4,grid3,(.true.))
  call tdely_reg(grid3,work1,(.false.))
  call tdelx_reg(grid2,grid1,(.true.))
  call tdelx_reg(grid1,work1,(.false.))
  
  return
end subroutine tget_delsqr_reg

subroutine outgrad1(f,label,nx,ny)

         character(*) label
         integer(4) nx,ny
         real(8) f(ny,nx)

         character(80) dsdes,dsdat
         character(80) datdes(1000)
         character(1) blank
         integer np,ioutcor,ioutdat,ntime
         integer i,j,k,next,last,koutmax
         real(4) out(nx,ny)
         real(4) undef,rlonmap0,rlatmap0,dlonmap,dlatmap
         real(4) startp,pinc
         data blank/' '/
         data undef/-9.99e33/


         np=1
         ioutcor=10
         ioutdat=11

         write(dsdes,'(a,".des")')trim(label)
         write(dsdat,'(a,".dat")')trim(label)
         open(unit=ioutcor,file=dsdes,form='formatted')
         open(unit=ioutdat,file=dsdat,form='unformatted')
         ntime=1
         rlonmap0=1.
         dlonmap=1.
         rlatmap0=1.
         dlatmap=1.
         startp=1.
         pinc=1.
         koutmax=1
         do i=1,1000
          write(datdes(i),'(80a1)')(blank,k=1,80)
         end do
         write(datdes(1),'("DSET ",a)')trim(dsdat)
         write(datdes(2),'("options big_endian sequential")')
         write(datdes(3),'("TITLE ",a)')trim(label)
         write(datdes(4),'("UNDEF ",e11.2)')undef
         write(datdes(5),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nx,rlonmap0,dlonmap
         write(datdes(6),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')ny,rlatmap0,dlatmap
         next=7
         write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
         next=next+1
         write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
         next=next+1
         write(datdes(next),'("VARS 1")')
         next=next+1
         write(datdes(next),'("f   ",i5," 99 f   ")')np
         next=next+1
         write(datdes(next),'("ENDVARS")')
         last=next
         write(ioutcor,'(a80)')(datdes(i),i=1,last)
         close(ioutcor)

         do i=1,nx
           do j=1,ny
             out(i,j)=f(j,i)
           end do
         end do
         write(ioutdat)((out(i,j),i=1,nx),j=1,ny)
         close(ioutdat)

return
end subroutine outgrad1

subroutine special_for_llfmg_sub2grid3(a1,a2,a3,b1,b2,b3,f1,f2,f3,mype)

  use kinds, only: i_kind,r_kind
  use mpimod, only: npe
  use gridmod, only: lat1,lon1,lat2,lon2,nlat,nlon,itotsub,iglobal
  use general_commvars_mod, only: ltosi,ltosj
  use mod_vtrans, only: nvmodes_keep
  implicit none

  integer(i_kind),intent(in):: mype
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(in)::a1,a2,a3,b1,b2,b3
  real(r_kind),dimension(nlat,nlon),intent(out):: f1,f2,f3

  real(r_kind) all_loc(lat1,lon1,3,2*nvmodes_keep),tempa(itotsub,3)
  integer(i_kind) i,ip1,j,jp1,k,ka,kb
  integer(i_kind) kbegin(0:npe-1),kend(0:npe-1),numlevs(0:npe-1)

  do k=1,nvmodes_keep
    ka=k
    kb=ka+nvmodes_keep
    do i=1,lon1
      ip1=i+1
      do j=1,lat1
        jp1=j+1
        all_loc(j,i,1,ka)=a1(jp1,ip1,k)
        all_loc(j,i,2,ka)=a2(jp1,ip1,k)
        all_loc(j,i,3,ka)=a3(jp1,ip1,k)
        all_loc(j,i,1,kb)=b1(jp1,ip1,k)
        all_loc(j,i,2,kb)=b2(jp1,ip1,k)
        all_loc(j,i,3,kb)=b3(jp1,ip1,k)
      end do
    end do
  end do

  do k=0,2*nvmodes_keep-1
    numlevs(k)=3
  end do
  do k=2*nvmodes_keep,npe-1
    numlevs(k)=0
  end do
  kbegin(0)=1
  do k=1,npe-1
    kbegin(k)=kbegin(k-1)+numlevs(k-1)
  end do
  do k=0,npe-1
    kend(k)=kbegin(k)+numlevs(k)-1
  end do

  call generic_sub2grid8(all_loc,tempa,kbegin(mype),kend(mype),kbegin,kend,mype,6*nvmodes_keep)
  do k=1,kend(mype)-kbegin(mype)+1
    if(k == 1) then
      do i=1,iglobal
        f1(ltosi(i),ltosj(i))=tempa(i,k)
      end do
    elseif(k == 2) then
      do i=1,iglobal
        f2(ltosi(i),ltosj(i))=tempa(i,k)
      end do
    elseif(k == 3) then
      do i=1,iglobal
        f3(ltosi(i),ltosj(i))=tempa(i,k)
      end do
    end if
  end do

end subroutine special_for_llfmg_sub2grid3

subroutine special_for_llfmg_sub2grid2(a1,a2,b1,b2,f1,f2,mype)

  use kinds, only: i_kind,r_kind
  use mpimod, only: npe
  use gridmod, only: lat1,lon1,lat2,lon2,nlat,nlon,itotsub,iglobal
  use general_commvars_mod, only: ltosi,ltosj
  use mod_vtrans, only: nvmodes_keep
  implicit none

  integer(i_kind),intent(in):: mype
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(in)::a1,a2,b1,b2
  real(r_kind),dimension(nlat,nlon),intent(out):: f1,f2

  real(r_kind) all_loc(lat1,lon1,2,2*nvmodes_keep),tempa(itotsub,2)
  integer(i_kind) i,ip1,j,jp1,k,ka,kb
  integer(i_kind) kbegin(0:npe-1),kend(0:npe-1),numlevs(0:npe-1)

  do k=1,nvmodes_keep
    ka=k
    kb=ka+nvmodes_keep
    do i=1,lon1
      ip1=i+1
      do j=1,lat1
        jp1=j+1
        all_loc(j,i,1,ka)=a1(jp1,ip1,k)
        all_loc(j,i,2,ka)=a2(jp1,ip1,k)
        all_loc(j,i,1,kb)=b1(jp1,ip1,k)
        all_loc(j,i,2,kb)=b2(jp1,ip1,k)
      end do
    end do
  end do

  do k=0,2*nvmodes_keep-1
    numlevs(k)=2
  end do
  do k=2*nvmodes_keep,npe-1
    numlevs(k)=0
  end do
  kbegin(0)=1
  do k=1,npe-1
    kbegin(k)=kbegin(k-1)+numlevs(k-1)
  end do
  do k=0,npe-1
    kend(k)=kbegin(k)+numlevs(k)-1
  end do

  call generic_sub2grid8(all_loc,tempa,kbegin(mype),kend(mype),kbegin,kend,mype,4*nvmodes_keep)
  do k=1,kend(mype)-kbegin(mype)+1
    if(k == 1) then
      do i=1,iglobal
        f1(ltosi(i),ltosj(i))=tempa(i,k)
      end do
    elseif(k == 2) then
      do i=1,iglobal
        f2(ltosi(i),ltosj(i))=tempa(i,k)
      end do
    end if
  end do

end subroutine special_for_llfmg_sub2grid2

subroutine special_for_llfmg_grid2sub2(f1,f2,a1,a2,b1,b2,mype)

  use kinds, only: i_kind,r_kind
  use mpimod, only: npe
  use gridmod, only: lat2,lon2,nlat,nlon,itotsub
  use general_commvars_mod, only: ltosi_s,ltosj_s
  use mod_vtrans, only: nvmodes_keep
  implicit none

  integer(i_kind),intent(in):: mype
  real(r_kind),dimension(nlat,nlon),intent(in):: f1,f2
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(out)::a1,a2,b1,b2

  real(r_kind) all_loc(lat2,lon2,2,2*nvmodes_keep),tempa(itotsub,2)
  integer(i_kind) i,j,k,ka,kb
  integer(i_kind) kbegin(0:npe-1),kend(0:npe-1),numlevs(0:npe-1)

  do k=0,2*nvmodes_keep-1
    numlevs(k)=2
  end do
  do k=2*nvmodes_keep,npe-1
    numlevs(k)=0
  end do
  kbegin(0)=1
  do k=1,npe-1
    kbegin(k)=kbegin(k-1)+numlevs(k-1)
  end do
  do k=0,npe-1
    kend(k)=kbegin(k)+numlevs(k)-1
  end do

  do k=1,kend(mype)-kbegin(mype)+1
    if(k == 1) then
      do i=1,itotsub
        tempa(i,k)=f1(ltosi_s(i),ltosj_s(i))
      end do
    elseif(k == 2) then
      do i=1,itotsub
        tempa(i,k)=f2(ltosi_s(i),ltosj_s(i))
      end do
    end if
  end do

  call generic_grid2sub8(tempa,all_loc,kbegin(mype),kend(mype),kbegin,kend,mype,4*nvmodes_keep)

  do k=1,nvmodes_keep
    ka=k
    kb=ka+nvmodes_keep
    do i=1,lon2
      do j=1,lat2
        a1(j,i,k)=all_loc(j,i,1,ka)
        a2(j,i,k)=all_loc(j,i,2,ka)
        b1(j,i,k)=all_loc(j,i,1,kb)
        b2(j,i,k)=all_loc(j,i,2,kb)
      end do
    end do
  end do

end subroutine special_for_llfmg_grid2sub2

subroutine special_for_llfmg_grid2sub3(f1,f2,f3,a1,a2,a3,b1,b2,b3,mype)

  use kinds, only: i_kind,r_kind
  use mpimod, only: npe
  use gridmod, only: lat2,lon2,nlat,nlon,itotsub
  use general_commvars_mod, only: ltosi_s,ltosj_s
  use mod_vtrans, only: nvmodes_keep
  implicit none

  integer(i_kind),intent(in):: mype
  real(r_kind),dimension(nlat,nlon),intent(in):: f1,f2,f3
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(out)::a1,a2,a3,b1,b2,b3

  real(r_kind) all_loc(lat2,lon2,3,2*nvmodes_keep),tempa(itotsub,3)
  integer(i_kind) i,j,k,ka,kb
  integer(i_kind) kbegin(0:npe-1),kend(0:npe-1),numlevs(0:npe-1)

  do k=0,2*nvmodes_keep-1
    numlevs(k)=3
  end do
  do k=2*nvmodes_keep,npe-1
    numlevs(k)=0
  end do
  kbegin(0)=1
  do k=1,npe-1
    kbegin(k)=kbegin(k-1)+numlevs(k-1)
  end do
  do k=0,npe-1
    kend(k)=kbegin(k)+numlevs(k)-1
  end do

  do k=1,kend(mype)-kbegin(mype)+1
    if(k == 1) then
      do i=1,itotsub
        tempa(i,k)=f1(ltosi_s(i),ltosj_s(i))
      end do
    elseif(k == 2) then
      do i=1,itotsub
        tempa(i,k)=f2(ltosi_s(i),ltosj_s(i))
      end do
    elseif(k == 3) then
      do i=1,itotsub
        tempa(i,k)=f3(ltosi_s(i),ltosj_s(i))
      end do
    end if
  end do

  call generic_grid2sub8(tempa,all_loc,kbegin(mype),kend(mype),kbegin,kend,mype,6*nvmodes_keep)

  do k=1,nvmodes_keep
    ka=k
    kb=ka+nvmodes_keep
    do i=1,lon2
      do j=1,lat2
        a1(j,i,k)=all_loc(j,i,1,ka)
        a2(j,i,k)=all_loc(j,i,2,ka)
        a3(j,i,k)=all_loc(j,i,3,ka)
        b1(j,i,k)=all_loc(j,i,1,kb)
        b2(j,i,k)=all_loc(j,i,2,kb)
        b3(j,i,k)=all_loc(j,i,3,kb)
      end do
    end do
  end do

end subroutine special_for_llfmg_grid2sub3

subroutine generic_sub2grid8(all_loc,tempa,kbegin_loc,kend_loc,kbegin,kend,mype,num_fields)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    generic_sub2grid   converts from subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: variation on subroutine sub2grid, with more general distribution of variables
!              along the k index.
!
! program history log:
!   2004-02-03  kleist, new mpi strategy
!   2004-05-06  derber
!   2004-07-15  treadon - handle periodic subdomains
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!   2004-10-26  kleist - u,v removed; periodicity accounted for only in
!               sub2grid routine if necessary
!   2004-11-29  parrish - adapt sub2grid for related use with mpi io.
!
!   input argument list:
!     all_loc  - input grid values in vertical subdomain mode
!     kbegin_loc - starting k index for tempa on local processor
!     kend_loc   - ending k index for tempa on local processor
!     kbegin     - starting k indices for tempa for all processors
!     kend       - ending k indices for tempa for all processors
!     mype       - local processor number
!     num_fields - total range of k index (1 <= k <= num_fields)
!
!   output argument list:
!     tempa    - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe
  use gridmod, only: ijn,itotsub,lat1,lon1
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none

  integer(i_kind) kbegin_loc,kend_loc,mype,num_fields
  integer(i_kind) kbegin(0:npe),kend(0:npe-1)
  real(r_kind) tempa(itotsub,kbegin_loc:max(kbegin_loc,kend_loc))
  real(r_kind) all_loc(lat1*lon1*num_fields)

  integer(i_kind) k
  integer(i_kind) sendcounts(0:npe-1),sdispls(0:npe),recvcounts(0:npe-1),rdispls(0:npe)

! first get alltoallv indices

  sdispls(0)=0
  do k=0,npe-1
   sendcounts(k)=ijn(k+1)*(kend_loc-kbegin_loc+1)
   sdispls(k+1)=sdispls(k)+sendcounts(k)
  end do
  rdispls(0)=0
  do k=0,npe-1
   recvcounts(k)=ijn(mype+1)*(kend(k)-kbegin(k)+1)
   rdispls(k+1)=rdispls(k)+recvcounts(k)
  end do

  call mpi_alltoallv(all_loc,recvcounts,rdispls,mpi_rtype, &
                tempa,sendcounts,sdispls,mpi_rtype,mpi_comm_world,ierror)

  if(kbegin_loc <= kend_loc) then
    call reorder_s8(tempa,kend_loc-kbegin_loc+1)
  else
    tempa=zero
  end if

end subroutine generic_sub2grid8

  subroutine reorder_s8(work,k_in)

! !USES:

    use mpimod, only: npe
    use kinds, only: r_kind,i_kind
    use constants, only: zero
    use gridmod, only: ijn,itotsub
    implicit none

! !INPUT PARAMETERS:

   integer(i_kind), intent(in) ::  k_in    ! number of levs in work array

! !INPUT/OUTPUT PARAMETERS:

    real(r_kind),dimension(itotsub*k_in),intent(inout):: work ! array to reorder

! !OUTPUT PARAMETERS:

! !DESCRIPTION: adapt reorder to work with single precision
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-complaint prologue
!   2004-11-29  adapt reorder to work with single precision
!
! !REMAKRS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) iloc,iskip,i,k,n
    real(r_kind),dimension(itotsub,k_in):: temp

! Zero out temp array
    do k=1,k_in
       do i=1,itotsub
          temp(i,k)=zero
       end do
    end do

! Load temp array in desired order
    do k=1,k_in
      iskip=0
      iloc=0
      do n=1,npe
        if (n/=1) then
          iskip=iskip+ijn(n-1)*k_in
        end if
        do i=1,ijn(n)
          iloc=iloc+1
          temp(iloc,k)=work(i + iskip + &
                   (k-1)*ijn(n))
        end do
      end do
    end do

! Load the temp array back into work
    iloc=0
    do k=1,k_in
      do i=1,itotsub
        iloc=iloc+1
        work(iloc)=temp(i,k)
      end do
    end do

    return
  end subroutine reorder_s8

subroutine generic_grid2sub8(tempa,all_loc,kbegin_loc,kend_loc,kbegin,kend,mype,num_fields)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    generic_grid2sub   converts from full horizontal grid to subdomains
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: variation on subroutine grid2sub, with more general distribution of variables
!              along the k index.
!
! program history log:
!   2004-02-03  kleist, new mpi strategy
!   2004-05-06  derber
!   2004-07-15  treadon - handle periodic subdomains
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!   2004-10-26  kleist - u,v removed; periodicity accounted for only in
!               sub2grid routine if necessary
!   2004-11-29  parrish - adapt grid2sub for related use with mpi io.
!
!   input argument list:
!     tempa    - input grid values in horizontal slab mode.
!     kbegin_loc - starting k index for tempa on local processor
!     kend_loc   - ending k index for tempa on local processor
!     kbegin     - starting k indices for tempa for all processors
!     kend       - ending k indices for tempa for all processors
!     mype       - local processor number
!     num_fields - total range of k index (1 <= k <= num_fields)
!
!   output argument list:
!     all_loc  - output grid values in vertical subdomain mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe
  use gridmod, only: ijn_s,itotsub,lat2,lon2
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none
  
  integer(i_kind) kbegin_loc,kend_loc,mype,num_fields
  integer(i_kind) kbegin(0:npe),kend(0:npe-1)
  real(r_kind) tempa(itotsub,kbegin_loc:max(kbegin_loc,kend_loc))
  real(r_kind) all_loc(lat2*lon2*num_fields)
  
  integer(i_kind) k
  integer(i_kind) sendcounts(0:npe-1),sdispls(0:npe),recvcounts(0:npe-1),rdispls(0:npe)

! first get alltoallv indices
  
  sdispls(0)=0
  do k=0,npe-1
     sendcounts(k)=ijn_s(k+1)*(kend_loc-kbegin_loc+1) 
     sdispls(k+1)=sdispls(k)+sendcounts(k)
  end do
  rdispls(0)=0
  do k=0,npe-1
     recvcounts(k)=ijn_s(mype+1)*(kend(k)-kbegin(k)+1)
     rdispls(k+1)=rdispls(k)+recvcounts(k)
  end do
  
! then call reorder2

  if(kbegin_loc <= kend_loc) then
    call reorder2_s8(tempa,kend_loc-kbegin_loc+1)
  else
    tempa=zero
  end if

! then alltoallv and i think we are done??

  call mpi_alltoallv(tempa,sendcounts,sdispls,mpi_rtype, &
       all_loc,recvcounts,rdispls,mpi_rtype,mpi_comm_world,ierror)

end subroutine generic_grid2sub8

subroutine reorder2_s8(work,k_in)

! !USES:

  use constants, only: zero
  use mpimod, only: npe
  use gridmod, only: ijn_s,itotsub
  use kinds, only: r_kind,i_kind
  implicit none
  

! !INPUT PARAMETERS:

  integer(i_kind), intent(in) ::  k_in    ! number of levs in work array

! !INPUT/OUTPUT PARAMETERS:

  real(r_kind),dimension(itotsub,k_in),intent(inout):: work

! !OUTPUT PARAMETERS:

! !DESCRIPTION: adapt reorder2 to single precision
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-complaint prologue
!   2004-11-29  parrish, adapt reorder2 to single precision
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) iloc,iskip,i,k,n
  real(r_kind),dimension(itotsub*k_in):: temp

! Zero out temp array
  do k=1,itotsub*k_in
     temp(k)=zero
  end do
  
! Load temp array in order of subdomains
  iloc=0
  iskip=0
  do n=1,npe
     if (n/=1) then
        iskip=iskip+ijn_s(n-1)
     end if
     
     do k=1,k_in
        do i=1,ijn_s(n)
           iloc=iloc+1
           temp(iloc)=work(iskip+i,k)
        end do
     end do
  end do

! Now load the tmp array back into work
  iloc=0
  do k=1,k_in
     do i=1,itotsub
        iloc=iloc+1
        work(i,k)=temp(iloc)
     end do
  end do
  
  return
end subroutine reorder2_s8

subroutine get_div_reg(u,v,div)

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  implicit none

  real(r_kind),dimension(nlat,nlon),intent(in):: u,v
  real(r_kind),dimension(nlat,nlon),intent(out)::div

  integer(i_kind) i,j
  real(r_kind),dimension(nlat,nlon)::ux,vy

  call delx_reg(u,ux,.true.)
  call dely_reg(v,vy,.true.)

  do j=1,nlon
    do i=1,nlat
      div(i,j)=ux(i,j)+vy(i,j)
    end do
  end do

end subroutine get_div_reg

subroutine get_div_reg_ad(u,v,div)

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use constants, only: zero
  implicit none

  real(r_kind),dimension(nlat,nlon),intent(out):: u,v
  real(r_kind),dimension(nlat,nlon),intent(in)::div

  integer(i_kind) i,j
  real(r_kind),dimension(nlat,nlon)::ux,vy

  do j=1,nlon
    do i=1,nlat
      ux(i,j)=div(i,j)
      vy(i,j)=div(i,j)
    end do
  end do

  u=zero ; v=zero
  call tdelx_reg(ux,u,.true.)
  call tdely_reg(vy,v,.true.)

end subroutine get_div_reg_ad

subroutine get_vor_reg(u,v,vor)

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  implicit none

  real(r_kind),dimension(nlat,nlon),intent(in):: u,v
  real(r_kind),dimension(nlat,nlon),intent(out)::vor

  integer(i_kind) i,j
  real(r_kind),dimension(nlat,nlon)::uy,vx

  call delx_reg(v,vx,.true.)
  call dely_reg(u,uy,.true.)

  do j=1,nlon
    do i=1,nlat
      vor(i,j)=vx(i,j)-uy(i,j)
    end do
  end do

end subroutine get_vor_reg

subroutine get_vor_reg_ad(u,v,vor)

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use constants, only: zero
  implicit none

  real(r_kind),dimension(nlat,nlon),intent(out):: u,v
  real(r_kind),dimension(nlat,nlon),intent(in)::vor

  integer(i_kind) i,j
  real(r_kind),dimension(nlat,nlon)::uy,vx


  do j=1,nlon
    do i=1,nlat
      vx(i,j)=vor(i,j)
      uy(i,j)=-vor(i,j)
    end do
  end do
  u=zero ; v=zero
  call tdelx_reg(vx,v,.true.)
  call tdely_reg(uy,u,.true.)

end subroutine get_vor_reg_ad
