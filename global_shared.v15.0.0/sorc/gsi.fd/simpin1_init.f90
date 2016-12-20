subroutine simpin1_init(ixi,tlout,alocalout,blocalout,iord,lbig,x1grid,n1grid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    simpin1_init
!     prgmmr:    parrish     org: np22                date: 2000-01-01
!
! abstract:  This routine computes all Taylor matrices ever needed
!            for 1-d interpolation on grid x1grid.
!
! program history log:
!   2000-01-01  parrish
!   2004-06-22  treadon - update documentation
!
!   input argument list:
!     iord   - order of interpolation (1=linear, 2=quadratic, etc)
!     lbig   - number of interpolating points (=iord+1)
!     x1grid - coordinates of interpolator grid
!                (can be monotonic increasing or decreasing)
!     n1grid - dimension of interpolator grid
!
!   output argument list:
!     ixi    - local coordinate order counter
!     tl     - output taylor matrices
!     alocal,blocal - used to transform to local coordinates around each grid-point
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero, half, one, two, one_tenth
  implicit none

! Declare local parameters
  real(r_kind),parameter:: r0_001=0.001_r_kind

  integer(i_kind),intent(in   ) :: iord,lbig,n1grid
  integer(i_kind),intent(  out) :: ixi(0:iord)
  real(r_kind)   ,intent(  out) :: tlout(lbig,lbig,2*n1grid),&
       alocalout(2*n1grid),blocalout(2*n1grid)
  real(r_kind)   ,intent(in   ) :: x1grid(n1grid)

  real(r_kind) x1in(2*n1grid)
  real(r_kind) dx1gridi(-3:n1grid+3)
  real(r_kind) tl(max(64,2*n1grid/lbig),lbig,lbig)
  integer(i_kind) i1ref(2*n1grid)
  integer(i_kind) ix1sign(2*n1grid)
  real(r_kind) x1temp(2*n1grid),x1p(2*n1grid)
  integer(i_kind) iflag(2*n1grid)
  real(r_kind) alocal(max(64,2*n1grid/lbig)),blocal(max(64,2*n1grid/lbig))
  real(r_kind) xmaxlocal(max(64,2*n1grid/lbig)),xminlocal(max(64,2*n1grid/lbig))
  integer(i_kind), allocatable, dimension(:) :: ix1grid
  
  real(r_kind) delx1,dxa,dxb,dxc,dxd,dxe,dxf,dxg,dxfinei,dxmax,rhigh,rlow,x1ref,xlocal
  real(r_kind) xboundright,xboundleft,dxfine,dxminmin,dxmin,dxthis
  integer(i_kind) i1fine,i1ref0,ih,ii,ip,iximx,j,l,lp,n1fine,nend,np,nstart,nthis,ntl
  integer(i_kind) nmaxright,nn,nin,n,iximax,nminleft,iximn,nm,i
  
  nin=2*n1grid
  do n=1,n1grid
     nm=max(1,n-1)
     np=min(n1grid,n+1)
     delx1=x1grid(np)-x1grid(np-1)
     x1in(n)=x1grid(n)+one_tenth*delx1
     delx1=x1grid(nm+1)-x1grid(nm)
     x1in(n1grid+n)=x1grid(n)-one_tenth*delx1
  end do
  
  ntl=max(64,nin/lbig)
  ! set ixi, the coordinate order counter
 
  do i=0,iord
     ih=(i+1)/2
     if(2*ih==i) then
        ixi(i)=-ih
     else
        ixi(i)=ih
     end if
!     write(6,100)i,ixi(i)
!     100 format(' xi(',i2,')=',f10.0)
  end do

  !  find and mark all points that are outside interval of interpolation

  iximx=maxval(ixi) ; iximn=minval(ixi)
  iximax=max(abs(iximx),abs(iximn))
  nminleft=abs(iximn)+1 ; nmaxright=n1grid-iximax
  if(iord==1) then
     xboundleft=x1grid(1)
     xboundright=x1grid(n1grid)
  else
     xboundleft=x1grid(1+iximax)-.49999_r_kind*(x1grid(1+iximax)-x1grid(iximax))
     xboundright=x1grid(n1grid-iximax)+ &
          .49999_r_kind*(x1grid(n1grid-iximax+1)-x1grid(n1grid-iximax))
  end if
  if(x1grid(n1grid)>x1grid(1)) then
     iflag=1
     do n=1,nin
        if(x1in(n)<=xboundleft.or.x1in(n)>=xboundright) iflag(n)=0
     end do
  end if
  if(x1grid(n1grid)<x1grid(1)) then
     iflag=1
     do n=1,nin
        if(x1in(n)>=xboundleft.or.x1in(n)<=xboundright) iflag(n)=0
     end do
  end if

  !  set up uniform fine grid to use in finding interpolation coordinates
 
  dxmax=-huge(dxmax) ; dxmin=huge(dxmin)
  do i=1,n1grid-1
     dxthis=x1grid(i+1)-x1grid(i)
     dx1gridi(i)=one/dxthis
     dxmax=max(dxthis,dxmax) ; dxmin=min(dxthis,dxmin)
  end do
  dx1gridi(-3:0)=dx1gridi(1)
  dx1gridi(n1grid:n1grid+3)=dx1gridi(n1grid-1)
  if(dxmax*dxmin<=zero) then
     write(6,*)' INTERPOLATION GRID NOT MONOTONIC IN SIMPIN1'
     do i=1,n1grid-1
        write(6,*)' i,x1grid,dx=',i,x1grid(i),x1grid(i+1)-x1grid(i)
     end do
     stop
  end if
  dxminmin=min(abs(dxmax),abs(dxmin))
  dxfine=sign(dxminmin,dxmax)
  dxfinei=one/dxfine
  n1fine=1+ceiling((x1grid(n1grid)-x1grid(1))/dxfine)
!  write(6,*)' in simpin1, n1grid,n1fine=',n1grid,n1fine
  allocate (ix1grid(n1fine))
  ii=1
  do i=1,n1fine
     x1ref=x1grid(1)+(i-1)*dxfine
     if(dxfinei*(x1ref-x1grid(ii+1))>=-r0_001) ii=min(ii+1,n1grid-1)
     ix1grid(i)=ii
!     write(6,)' x1fine,x1grid=',x1ref,x1grid(ix1grid(i))
  end do
 
  ! now get i1ref, index of interval containing point
 
  rlow=-epsilon(rlow)
  rhigh=one+epsilon(rhigh)
  do n=1,nin
     i1fine=1+nint((x1in(n)-x1grid(1))*dxfinei)
     i1fine=max(1,min(i1fine,n1fine))
     i1ref0=max(4,min(ix1grid(i1fine),n1grid-4))
     dxa=(x1in(n)-x1grid(i1ref0-3))*dx1gridi(i1ref0-3)
     dxb=(x1in(n)-x1grid(i1ref0-2))*dx1gridi(i1ref0-2)
     dxc=(x1in(n)-x1grid(i1ref0-1))*dx1gridi(i1ref0-1)
     dxd=(x1in(n)-x1grid(i1ref0  ))*dx1gridi(i1ref0  )
     dxe=(x1in(n)-x1grid(i1ref0+1))*dx1gridi(i1ref0+1)
     dxf=(x1in(n)-x1grid(i1ref0+2))*dx1gridi(i1ref0+2)
     dxg=(x1in(n)-x1grid(i1ref0+3))*dx1gridi(i1ref0+3)
     if(dxa<=rhigh) i1ref(n)=i1ref0-3
     if(dxb>=rlow.and.dxb<=rhigh) i1ref(n)=i1ref0-2
     if(dxc>=rlow.and.dxc<=rhigh) i1ref(n)=i1ref0-1
     if(dxd>=rlow.and.dxd<=rhigh) i1ref(n)=i1ref0
     if(dxe>=rlow.and.dxe<=rhigh) i1ref(n)=i1ref0+1
     if(dxf>=rlow.and.dxf<=rhigh) i1ref(n)=i1ref0+2
     if(dxg>=rlow) i1ref(n)=i1ref0+3
     i1ref(n)=max(nminleft,min(i1ref(n),nmaxright))
  end do
  deallocate (ix1grid)

  !             i1ref now has index of interval containing point
  !               adjust to be closest one to interpolating point
  ix1sign=1
  do n=1,nin
     dxa=(x1in(n)-x1grid(i1ref(n)))*dx1gridi(i1ref(n))
     if(dxa>half) then
        i1ref(n)=min(i1ref(n)+1,nmaxright)
        ix1sign(n)=-1
     end if
  end do

  ! get taylor matrices and invert
  
  nstart=1
  nend=min(nin,ntl)
  do while (nstart<=nend)
     nthis=nend-nstart+1
     
!  compute limits of local x coordinate

     xmaxlocal=-huge(xmaxlocal) ; xminlocal=huge(xminlocal)
     do i=0,iord
        do n=nstart,nend
           if(iflag(n)>0) then
              nn=n-nstart+1
              xlocal=x1grid(i1ref(n)+ix1sign(n)*ixi(i))-x1grid(i1ref(n))
              xmaxlocal(nn)=max(xmaxlocal(nn),xlocal)
              xminlocal(nn)=min(xminlocal(nn),xlocal)
           end if
        end do
     end do
     alocal=zero
     blocal=zero
     do n=nstart,nend
        if(iflag(n)>0) then 
           nn=n-nstart+1
           alocal(nn)=two/(xmaxlocal(nn)-xminlocal(nn))
           blocal(nn)=-one-two*xminlocal(nn)/(xmaxlocal(nn)-xminlocal(nn))
           alocalout(n)=alocal(nn)
           blocalout(n)=blocal(nn)
        end if
     end do
     
     tl=zero
     do i=1,lbig
        tl(1:nthis,i,i)=one
     end do
     l=0
     do i=0,iord
        l=l+1
        x1temp(nstart:nend)=zero
        do n=nstart,nend
           nn=n-nstart+1
           x1temp(n)=alocal(nn)*(x1grid(i1ref(n)+ix1sign(n)*ixi(i))-x1grid(i1ref(n))) &
                +blocal(nn)
        end do
        lp=0
        x1p(nstart:nend)=one
        do ip=0,iord
           lp=lp+1
           do n=nstart,nend
              if(iflag(n)>0) tl(n-nstart+1,l,lp)=x1p(n)
           end do
           x1p(nstart:nend)=x1p(nstart:nend)*x1temp(nstart:nend)
        end do
     end do
     
     call vinvmm(tl,tl,lbig,lbig,lbig,nthis,ntl)
     
     do n=nstart,nend
        nn=n-nstart+1
        do j=1,lbig
           do i=1,lbig
              tlout(i,j,n)=tl(nn,i,j)
           end do
        end do
     end do
     
     nstart=nstart+ntl
     nend=min(nend+ntl,nin)
     
  end do
  
  return
end subroutine simpin1_init
