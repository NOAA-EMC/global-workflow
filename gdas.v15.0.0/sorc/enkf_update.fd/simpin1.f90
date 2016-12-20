subroutine simpin1(wgts,wgtsx1,wgtsx11,iwgts, &
     iflag,x1in,nin,iord,lbig,x1grid,n1grid,nf, &
     ndx1,ndx11,ixi,tl,alocal,blocal)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    simpin1
!     prgmmr:    parrish     org: np22                date: 2000-01-01
!
! abstract:  This routine computes interpolation weights for 1-d
!            interpolation
!
! program history log:
!   2000-01-01  parrish
!   2004-06-22  treadon - update documentation
!   2008-04-12  safford - rm unused vars
!
!   input argument list:
!     x1in    - coordinates of interpolatees
!     nin     - number of interpolatees
!     iord    - order of interpolation (1=linear, 2=quadratic, etc)
!     lbig    - number of interpolating points (=iord+1)
!     x1grid  - coordinates of interpolator grid
!                         (can be monotonic increasing or decreasing)
!     n1grid  - dimension of interpolator grid
!     nf      - =1, then return interpolation weights for function f
!     ndx1    - =1, then return interpolation weights for df/dx1
!     ndx11   - =1, then return interpolation weights for d2f/(dx1*dx1)
!
!   output argument list:
!     wgts    - interpolation weights for function ( wgts(nin,lbig) )
!     wgtsx1  - interpolation weights for df/dx1
!     wgtsx11 - interpolation weights for d2f/(dx1*dx1)
!                 note: if any of these 3 are not asked for, they
!                       can be dummy arguments
!     iwgts   - absolute grid addresses ( iwgts(nin,lbig) )
!     iflag   - flag for each interpolatee (iflag(nin))
!                 =0, then point too close to edge of domain, but weights
!                     are computed anyway, in case it is considered 
!                     OK to extrapolate.
!                 =1, then weights computed
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero, half, one
  implicit none
  
  integer(i_kind),intent(in   ) :: nin,iord,lbig,n1grid,nf,ndx1,ndx11
  real(r_kind)   ,intent(in   ) :: x1in(nin),x1grid(n1grid)
  real(r_kind)   ,intent(  out) :: wgts(nin,lbig)
  integer(i_kind),intent(  out) :: iwgts(nin,lbig)
  real(r_kind)   ,intent(  out) :: wgtsx1(nin,lbig),wgtsx11(nin,lbig)
  integer(i_kind),intent(in   ) :: ixi(0:iord)
  real(r_kind)   ,intent(in   ) :: tl(lbig,lbig,n1grid,2),alocal(n1grid,2),blocal(n1grid,2)

  real(r_kind) dx1gridi(-3:n1grid+3)
  integer(i_kind) i1ref(nin)
  integer(i_kind) ix1sign(nin)
  real(r_kind) x1p(nin)
  integer(i_kind) ix1temp(nin)
  integer(i_kind) iflag(nin)
  real(r_kind) z0(max(64,nin/lbig),lbig)
  integer(i_kind), allocatable, dimension(:) :: ix1grid

  real(r_kind) dxa,dxb,dxc,dxd,dxe,dxf,dxg,x1ref,xboundleft,xboundright
  real(r_kind) dxfine,dxminmin,dxthis,dxfinei,dxmax,dxmin
  integer(i_kind) i1ref0,i1fine,l,iflip,nn,j,k,ip,nend,nstart,lp,&
       nthis,nmaxright,n,nminleft,iximx,ntl
  integer(i_kind) ii,n1fine,iximax,iximn,i
  
  ntl=max(64,nin/lbig)


! Find and mark all points that are outside interval of interpolation
  iximx=maxval(ixi) ; iximn=minval(ixi)
  iximax=max(abs(iximx),abs(iximn))
! write(6,*)' iximx,mn=',iximx,iximn
! write(6,*)' iximax=',iximax
! write(6,*)' ixi=',ixi
  nminleft=abs(iximn)+1 ; nmaxright=n1grid-iximax
  if(iord==1) then
     xboundleft=x1grid(1)
     xboundright=x1grid(n1grid)
  else
     xboundleft=x1grid(1+iximax)-.49999_r_kind*(x1grid(1+iximax)-x1grid(iximax))
     xboundright=x1grid(n1grid-iximax)+ &
          .49999_r_kind*(x1grid(n1grid-iximax+1)-x1grid(n1grid-iximax))
  end if
!  write(6,*) ' xboundleft=',xboundleft
!  write(6,*) ' xboundright=',xboundright
!  do i=1,iximax+1
!     write(6,*) ' x1grid(',i,')=',x1grid(i)
!  end do
!  do i=n1grid,n1grid-iximax,-1
!     write(6,*)' x1grid(',i,')=',x1grid(i)
!  end do
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


! Set up uniform fine grid to use in finding interpolation coordinates
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
     call stop2(999)
  end if
  dxminmin=min(abs(dxmax),abs(dxmin))
  dxfine=sign(dxminmin,dxmax)
  dxfinei=one/dxfine
  n1fine=1+ceiling((x1grid(n1grid)-x1grid(1))/dxfine)
!      write(6,*)' in simpin1, n1grid,n1fine=',n1grid,n1fine
  allocate (ix1grid(n1fine))
  ii=1
  do i=1,n1fine
     x1ref=x1grid(1)+(i-1)*dxfine
     if(dxfinei*(x1ref-x1grid(ii+1))>=-.001_r_kind) ii=min(ii+1,n1grid-1)
     ix1grid(i)=ii
!     write(6,*)' x1fine,x1grid=',x1ref,x1grid(ix1grid(i))
  end do

 
! Now get i1ref, index of interval containing point
 
  i1ref=-1
  do n=1,nin
     i1fine=1+nint((x1in(n)-x1grid(1))*dxfinei)
     i1fine=max(1,min(i1fine,n1fine))
     i1ref0=max(4,min(ix1grid(i1fine),n1grid-4))
     dxa=(x1in(n)-x1grid(i1ref0-3))*dx1gridi(i1ref0-3)
     dxb=(x1in(n)-x1grid(i1ref0-2))*dx1gridi(i1ref0-2)
     dxc=(x1in(n)-x1grid(i1ref0-1 ))*dx1gridi(i1ref0-1)
     dxd=(x1in(n)-x1grid(i1ref0   ))*dx1gridi(i1ref0  )
     dxe=(x1in(n)-x1grid(i1ref0+1 ))*dx1gridi(i1ref0+1)
     dxf=(x1in(n)-x1grid(i1ref0+2))*dx1gridi(i1ref0+2)
     dxg=(x1in(n)-x1grid(i1ref0+3))*dx1gridi(i1ref0+3)
     if(dxa<=one) i1ref(n)=i1ref0-3
     if(dxb>=zero.and.dxb<=one) i1ref(n)=i1ref0-2
     if(dxc>=zero.and.dxc<=one) i1ref(n)=i1ref0-1
     if(dxd>=zero.and.dxd<=one) i1ref(n)=i1ref0
     if(dxe>=zero.and.dxe<=one) i1ref(n)=i1ref0+1
     if(dxf>=zero.and.dxf<=one) i1ref(n)=i1ref0+2
     if(dxg>=zero) i1ref(n)=i1ref0+3
     i1ref(n)=max(nminleft,min(i1ref(n),nmaxright))
  end do
  deallocate (ix1grid)
!  write(6,*)' max,min(i1ref)=',maxval(i1ref),minval(i1ref)


!             i1ref now has index of interval containing point
!               adjust to be closest one to interpolating point

  ix1sign=1
  do n=1,nin
     dxa=(x1in(n)-x1grid(i1ref(n)))*dx1gridi(i1ref(n))
     if(dxa>half.and.i1ref(n)<nmaxright) then
        i1ref(n)=i1ref(n)+1
        ix1sign(n)=-1
     end if
  end do

  ! get interpolation indices

  l=0
  do i=0,iord
     ix1temp=0
     l=l+1
     do n=1,nin
        ix1temp(n)=i1ref(n)+ix1sign(n)*ixi(i)
        iwgts(n,l)=ix1temp(n)
     end do
!     write(6,*)' max,min(ix1temp)=',maxval(ix1temp),minval(ix1temp)
  end do
!  write(6,*)' max,min(iwgts)=',maxval(iwgts),minval(iwgts)
!  write(6,*)' both of above should be >= 1 and <= ',n1grid 
    
 
      ! check that we got all points
!  dx1max=-huge(dx1max)
!  dx1min=huge(dx1min)
!  numinside=0
!  numgtp5=0
!  numoutside=0
!  halfpeps=half+epsilon(x)
!  do n=1,nin
!     if(iflag(n)/=0) then
!        numinside=numinside+1
!        dx1top=x1in(n)-x1grid(i1ref(n))
!        if(dx1top*dx1gridi(i1ref(n))<zero) then
!           dx1this=(x1in(n)-x1grid(i1ref(n)-1)) &
!                     *dx1gridi(i1ref(n)-1)-one
!        else
!           dx1this=dx1top*dx1gridi(i1ref(n))
!        end if
!        if(abs(dx1this)>halfpeps) then
!           numgtp5=numgtp5+1
!        end if
!        dx1max=max(dx1this,dx1max)
!        dx1min=min(dx1this,dx1min)
!     else
!        numoutside=numoutside+1
!     end if
!  end do
!  write(6,*)' numinside,outside=',numinside,numoutside
!  write(6,*)' numgtp5=',numgtp5
!  write(6,*)' dx1min,max=',dx1min,dx1max


! Get taylor matrices and invert
  
  nstart=1
  nend=min(nin,ntl)
  do while (nstart<=nend)
     nthis=nend-nstart+1
     
     if(nf==1) then    ! get weights for interpolating function
 
    ! get taylor vector(s)
 
        lp=0
        x1p(nstart:nend)=one
        do ip=0,iord
           lp=lp+1
           z0(1:nthis,lp)=x1p(nstart:nend)
           do n=1,nthis
              nn=n+nstart-1
              iflip=1
              if(ix1sign(nn)<0) iflip=2
              x1p(nn)=x1p(nn)*(alocal(i1ref(nn),iflip)*  &
                   (x1in(nn)-x1grid(max(1,i1ref(nn)))) &
                   +blocal(i1ref(nn),iflip))
           end do
        end do

    ! get interpolation weights
  
        do k=1,lbig
           wgts(nstart:nend,k)=zero
           do j=1,lbig
              do n=1,nthis
                 nn=n+nstart-1
                 iflip=1
                 if(ix1sign(nn)<0) iflip=2
                 wgts(nn,k)=wgts(nn,k)+z0(n,j)*tl(j,k,i1ref(nn),iflip)
              end do
           end do
        end do

     end if

     if(ndx1==1) then    ! get weights for df/dx1
 
    ! get taylor vector(s)
 
        lp=0
        x1p(nstart:nend)=one
        do ip=0,iord
           lp=lp+1
           do n=1,nthis
              nn=n+nstart-1
              iflip=1
              if(ix1sign(nn)<0) iflip=2
              z0(n,lp)=ip*x1p(nn)*alocal(i1ref(nn),iflip)
           end do
           if(ip>0) then
              do n=1,nthis
                 nn=n+nstart-1
                 iflip=1
                 if(ix1sign(nn)<0) iflip=2
                 x1p(nn)=x1p(nn)*(alocal(i1ref(nn),iflip)*  &
                      (x1in(nn)-x1grid(max(1,i1ref(nn)))) &
                      +blocal(i1ref(nn),iflip))
              end do
           end if
        end do
        
    ! get interpolation weights
  
        do k=1,lbig
           wgtsx1(nstart:nend,k)=zero
           do j=1,lbig
              do n=1,nthis
                 nn=n+nstart-1
                 iflip=1
                 if(ix1sign(nn)<0) iflip=2
                 wgtsx1(nn,k)=wgtsx1(nn,k)+z0(n,j)*tl(j,k,i1ref(nn),iflip)
              end do
           end do
        end do

     end if

     if(ndx11==1) then    ! get weights for d2f/(dx1*dx1)
        
    ! get taylor vector(s)
 
        lp=0
        x1p(nstart:nend)=one
        do ip=0,iord
           lp=lp+1
           do n=1,nthis
              nn=n+nstart-1
              iflip=1
              if(ix1sign(nn)<0) iflip=2
              z0(n,lp)=ip*(ip-1)*x1p(nn)*alocal(i1ref(nn),iflip)**2
           end do
           if(ip>1) then
              do n=1,nthis
                 nn=n+nstart-1
                 iflip=1
                 if(ix1sign(nn)<0) iflip=2
                 x1p(nn)=x1p(nn)*(alocal(i1ref(nn),iflip)*  &
                      (x1in(nn)-x1grid(max(1,i1ref(nn)))) &
                      +blocal(i1ref(nn),iflip))
              end do
           end if
        end do

    ! get interpolation weights
  
        do k=1,lbig
           wgtsx11(nstart:nend,k)=zero
           do j=1,lbig
              do n=1,nthis
                 nn=n+nstart-1
                 iflip=1
                 if(ix1sign(nn)<0) iflip=2
                 wgtsx11(nn,k)=wgtsx11(nn,k)+z0(n,j)*tl(j,k,i1ref(nn),iflip)
              end do
           end do
        end do
        
     end if
     
     nstart=nstart+ntl
     nend=min(nend+ntl,nin)
     
  end do

  return
end subroutine simpin1

subroutine vinvmm(b,a,m,nb,na,ninv,ninv0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    vinvmm
!     prgmmr:    purser      org: np20                date: 1998-01-01
!
! abstract:  This routine is a vector version of purserlib routine 
!            invmm.  The purpose of this routine is to invert a 
!            collection of matrices, possibly in place (a=b), using
!            the l-u decomposition method
!
! program history log:
!   1998-01-01  purser
!   2000-01-01  parrish
!   2004-06-22  treadon - update documentation
!
!   input argument list:
!     b     - input matrices
!     m     - order of matrices
!     nb    - leading dimension of b
!     na    - leading dimension of a
!     ninv  - number of matrices to invert
!     ninv0 - maximum number of matrices
!
!   output argument list:
!     a     - output inverses of b (can have a=b in calling program)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero, one
  implicit none
  
  integer(i_kind),intent(in   ) :: m,nb,na,ninv,ninv0
  real(r_kind)   ,intent(inout) :: a(ninv0,na,*),b(ninv0,nb,*)
  

  integer(i_kind) ipiv(ninv,m)
  real(r_kind) s(ninv),d(ninv)
  integer(i_kind) i,j,k,n
  
  do j=1,m
     do i=1,m
        a(1:ninv,i,j)=b(1:ninv,i,j)
     end do
  end do
  call vlufm(a,ipiv,d,m,na,ninv,ninv0,s)

  !  invert u in place:

  do i=1,m
     a(1:ninv,i,i)=one/a(1:ninv,i,i)
  end do
  do i=1,m-1
     do j=i+1,m
        s=zero
        do k=i,j-1
           s(1:ninv)=s(1:ninv)-a(1:ninv,i,k)*a(1:ninv,k,j)
        end do
        a(1:ninv,i,j)=a(1:ninv,j,j)*s(1:ninv)
     end do
  end do

  !  invert l in place assuming implicitly diagonal elements of unity

  do j=1,m-1
     do i=j+1,m
        s(1:ninv)=-a(1:ninv,i,j)
        do k=j+1,i-1
           s(1:ninv)=s(1:ninv)-a(1:ninv,i,k)*a(1:ninv,k,j)
        end do
        a(1:ninv,i,j)=s(1:ninv)
     end do
  end do

  !  form the product of u**-1 and l**-1 in place

  do j=1,m-1
     do i=1,j
        s(1:ninv)=a(1:ninv,i,j)
        do k=j+1,m
           s(1:ninv)=s(1:ninv)+a(1:ninv,i,k)*a(1:ninv,k,j)
        end do
        a(1:ninv,i,j)=s(1:ninv)
     end do
     do i=j+1,m
        s=zero
        do k=i,m
           s(1:ninv)=s(1:ninv)+a(1:ninv,i,k)*a(1:ninv,k,j)
        end do
        a(1:ninv,i,j)=s(1:ninv)
     end do
  end do
  
  !  permute columns according to ipiv

  do j=m-1,1,-1
     do i=1,m
        do n=1,ninv
           s(n)=a(n,i,j)
           a(n,i,j)=a(n,i,ipiv(n,j))
           a(n,i,ipiv(n,j))=s(n)
        end do
     end do
  end do
  
  return
end subroutine vinvmm

subroutine vlufm(a,ipiv,d,m,na,ninv,ninv0,s)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    vlufm
!     prgmmr:    purser      org: np20                date: 1998-01-01
!
! abstract:  This routine is a vector version of purserlib routine
!            lufm.  This routine is a pivot routine used by matrix
!            inversion routine vinvmm.
!
! program history log:
!   1998-01-01  purser
!   2000-01-01  parrish
!   2004-06-22  treadon - update documentation
!
!   input argument list:
!     a     - original matrices
!     m     - order of matrices
!     na    - leading dimension of matrices
!     ninv  - number of matrices to invert
!     ninv0 - maximum number of matrices
!     s     - work space of dimension ninv
!
!   output argument list:
!     a     - permuted matrices
!     ipiv  - record of permutations
!     d     - signs of determinants of matrices
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero, one
  implicit none
  
  integer(i_kind),intent(in   ) :: m,na,ninv,ninv0
  real(r_kind)   ,intent(inout) :: a(ninv0,na,*)
  integer(i_kind),intent(  out) :: ipiv(ninv,m)
  real(r_kind)   ,intent(  out) :: d(ninv)
  real(r_kind)   ,intent(inout) :: s(ninv)
  
  real(r_kind) ajj(ninv)
  integer(i_kind) i,j,jm,jp,k,n
  real(r_kind) aa
  
  d=one
  ipiv(1:ninv,m)=m
  do j=1,m-1
     jp=j+1
     ajj(1:ninv)=abs(a(1:ninv,j,j))
     ipiv(1:ninv,j)=j
     do i=jp,m
        do n=1,ninv
           aa=abs(a(n,i,j))
           if(aa>ajj(n))then
              ipiv(n,j)=i
              ajj(n)=aa
           end if
        end do
     end do
     
   !  swap rows, recording changed sign of determinant

     do n=1,ninv
        if(ipiv(n,j)/=j) d(n)=-d(n)
     end do
     do k=1,m
        do n=1,ninv
           s(n)=a(n,j,k)
           a(n,j,k)=a(n,ipiv(n,j),k)
           a(n,ipiv(n,j),k)=s(n)
        end do
     end do
     
     do n=1,ninv
        ajj(n)=a(n,j,j)
        if(ajj(n)==zero)then
           jm=j-1
           write(6,100)jm
100        format(' failure in lufact:',/,' matrix singular, rank=',i3)
        endif
     end do
     ajj=one/ajj
     do i=jp,m
        a(1:ninv,i,j)=ajj(1:ninv)*a(1:ninv,i,j)
        do k=jp,m
           a(1:ninv,i,k)=a(1:ninv,i,k)-a(1:ninv,i,j)*a(1:ninv,j,k)
        end do
     end do
     
  end do
  
  return
end subroutine vlufm
