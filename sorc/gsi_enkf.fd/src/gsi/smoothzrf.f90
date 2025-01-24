subroutine frfhvo(p1,iv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    frfhvo      performs vertical smoothing of fields
!   prgmmr: derber           org: np23                date: 2004-05-13
!
! abstract: performs vertical smoothing of fields
!
! program history log:
!   2004-05-13  derber, document
!   2005-01-22  parrish - make use of balmod and rename variables
!   2005-07-14  wu - add max bound to l2
!   2008-04-11  safford - rm unsed vars
!   2010-10-08  derber - optimize and clean up
!
!   input argument list:
!     p1       - input field to be smoothed (lat2,lon2,nsig)
!     iv       - location in alv for smoothing coefficients
!              - iv = 1 streamfunction
!              - iv = 2 velocity potential
!              - iv = 3 temperature        
!              - iv = 4 specific humidity  
!              - iv = 5 ozone           
!              - iv = 6 cloud condensate
!
!   output argument list
!     p1       - output field after vertical smoothing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  use gridmod, only: nsig,regional,lat2,lon2
  use balmod, only: rllat1,llmax
  use berror, only: alv,be,ndeg
  implicit none

! lat2 = number of latitudes (lat2)
! lon2 = number of longitudes (lon2)
! nsig  = number of model levels 
! ndeg  = degree of smoothing (ndeg=4)

  integer(i_kind)                       ,intent(in   ) :: iv
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: p1

  integer(i_kind) j,i,k,kr,ki,l,l2
  real(r_kind) alvl,alvr,alvi
  real(r_kind) gaki,gakr,dekr,deki,bekr,beki
  real(r_kind),dimension(lat2,lon2,nsig):: p2
  real(r_kind),dimension(lat2,lon2,ndeg):: ga,de
  real(r_kind),dimension(lat2,lon2):: dl1,dl2


! Zero output array
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           p2(i,j,k)=p1(i,j,k)
           p1(i,j,k)=zero
        end do
     end do
  end do

! Zero local work arrays
  do k=1,ndeg
     do j=1,lon2
        do i=1,lat2
           ga(i,j,k)=zero
           de(i,j,k)=zero
        end do
     end do
  end do

  if (regional)then
     
     do j=1,lon2
        do k=1,lat2
           l=int(rllat1(k,j))
           l2=min0(l+1,llmax)
           dl2(k,j)=rllat1(k,j)-real(l,r_kind)
           dl1(k,j)=one-dl2(k,j)
        end do
     end do
! Regional mode, odd degree
     if (mod(ndeg,2) == 1) then

!       advancing filter:
        do i=1,nsig
           do j=1,lon2
              do k=1,lat2
                 l=int(rllat1(k,j))
                 l2=min0(l+1,llmax)
                 alvl=dl1(k,j)*alv(l,1,i,iv)+dl2(k,j)*alv(l2,1,i,iv)
                 ga(k,j,1)=alvl*ga(k,j,1)+be(1)*p2(k,j,i)
                 p1(k,j,i)=ga(k,j,1)
              end do
           enddo
!          treat remaining complex roots:
           do kr=2,ndeg,2   ! <-- index of "real" components
              ki=kr+1            ! <-- index of "imag" components
              bekr=be(kr)
              beki=be(ki)
              do j=1,lon2
                 do k=1,lat2
                    l=int(rllat1(k,j))
                    l2=min0(l+1,llmax)
                    alvr=dl1(k,j)*alv(l,kr,i,iv)+dl2(k,j)*alv(l2,kr,i,iv)
                    alvi=dl1(k,j)*alv(l,ki,i,iv)+dl2(k,j)*alv(l2,ki,i,iv)
                    gakr=ga(k,j,kr)
                    gaki=ga(k,j,ki)
                    ga(k,j,kr)=alvr*gakr-alvi*gaki+bekr*p2(k,j,i)
                    ga(k,j,ki)=alvi*gakr+alvr*gaki+beki*p2(k,j,i)
                    p1(k,j,i)=p1(k,j,i)+ga(k,j,kr)
                 end do
              enddo
           enddo
        enddo
 
!       backing filter:
        do i=nsig,1,-1
           do j=1,lon2
              do k=1,lat2
                 p1(k,j,i)=p1(k,j,i)+de(k,j,1)
                 l=int(rllat1(k,j))
                 l2=min0(l+1,llmax)
                 alvl=dl1(k,j)*alv(l,1,i,iv)+dl2(k,j)*alv(l2,1,i,iv)
                 de(k,j,1)=alvl*(de(k,j,1)+be(1)*p2(k,j,i))
              end do
           enddo
!          treat remaining complex roots:
           do kr=2,ndeg,2   ! <-- index of "real" components
              ki=kr+1            ! <-- index of "imag" components
              bekr=be(kr)
              beki=be(ki)
              do j=1,lon2
                 do k=1,lat2
                    p1(k,j,i)=p1(k,j,i)+de(k,j,kr)
                    l=int(rllat1(k,j))
                    l2=min0(l+1,llmax)
                    alvr=dl1(k,j)*alv(l,kr,i,iv)+dl2(k,j)*alv(l2,kr,i,iv)
                    alvi=dl1(k,j)*alv(l,ki,i,iv)+dl2(k,j)*alv(l2,ki,i,iv)
                    dekr=de(k,j,kr)+bekr*p2(k,j,i)
                    deki=de(k,j,ki)+beki*p2(k,j,i)
                    de(k,j,kr)=alvr*dekr-alvi*deki
                    de(k,j,ki)=alvi*dekr+alvr*deki
                 end do
              enddo
           enddo
        enddo
 
! Regional mode, even degree
     else

        ! advancing filter:
        do i=1,nsig
           !       treat remaining complex roots:
           do kr=1,ndeg,2   ! <-- index of "real" components
              ki=kr+1            ! <-- index of "imag" components
              bekr=be(kr)
              beki=be(ki)
              do j=1,lon2
                 do k=1,lat2
                    l=int(rllat1(k,j))
                    l2=min0(l+1,llmax)
                    alvr=dl1(k,j)*alv(l,kr,i,iv)+dl2(k,j)*alv(l2,kr,i,iv)
                    alvi=dl1(k,j)*alv(l,ki,i,iv)+dl2(k,j)*alv(l2,ki,i,iv)
                    gakr=ga(k,j,kr)
                    gaki=ga(k,j,ki)
                    ga(k,j,kr)=alvr*gakr-alvi*gaki+bekr*p2(k,j,i)
                    ga(k,j,ki)=alvi*gakr+alvr*gaki+beki*p2(k,j,i)
                    p1(k,j,i)=p1(k,j,i)+ga(k,j,kr)
                 end do
              enddo
           enddo
        enddo
 
        !    backing filter:
        do i=nsig,1,-1
           !       treat remaining complex roots:
           do kr=1,ndeg,2   ! <-- index of "real" components
              ki=kr+1            ! <-- index of "imag" components
              bekr=be(kr)
              beki=be(ki)
              do j=1,lon2
                 do k=1,lat2
                    p1(k,j,i)=p1(k,j,i)+de(k,j,kr)
                    l=int(rllat1(k,j))
                    l2=min0(l+1,llmax)
                    alvr=dl1(k,j)*alv(l,kr,i,iv)+dl2(k,j)*alv(l2,kr,i,iv)
                    alvi=dl1(k,j)*alv(l,ki,i,iv)+dl2(k,j)*alv(l2,ki,i,iv)
                    dekr=de(k,j,kr)+bekr*p2(k,j,i)
                    deki=de(k,j,ki)+beki*p2(k,j,i)
                    de(k,j,kr)=alvr*dekr-alvi*deki
                    de(k,j,ki)=alvi*dekr+alvr*deki
                 end do
              enddo
           enddo
        enddo
     endif

! Global branch
  else
!    Global mode, odd degree
     if (mod(ndeg,2) == 1) then

!       advancing filter:
        do i=1,nsig
           do j=1,lon2
              do k=1,lat2
                 ga(k,j,1)=alv(k,1,i,iv)*ga(k,j,1)+be(1)*p2(k,j,i)
                 p1(k,j,i)=ga(k,j,1)
              end do
           enddo
!          treat remaining complex roots:
           do kr=2,ndeg,2   ! <-- index of "real" components
              ki=kr+1            ! <-- index of "imag" components
              bekr=be(kr)
              beki=be(ki)
              do j=1,lon2
                do k=1,lat2
                    gakr=ga(k,j,kr)
                    gaki=ga(k,j,ki)
                    ga(k,j,kr)=alv(k,kr,i,iv)*gakr-alv(k,ki,i,iv)*gaki+bekr*p2(k,j,i)
                    ga(k,j,ki)=alv(k,ki,i,iv)*gakr+alv(k,kr,i,iv)*gaki+beki*p2(k,j,i)
                    p1(k,j,i)=p1(k,j,i)+ga(k,j,kr)
                 end do
              enddo
           enddo
        enddo

!       backing filter:
        do i=nsig,1,-1
           do j=1,lon2
              do k=1,lat2
                 p1(k,j,i)=p1(k,j,i)+de(k,j,1)
                 de(k,j,1)=alv(k,1,i,iv)*(de(k,j,1)+be(1)*p2(k,j,i))
              end do
           enddo
!          treat remaining complex roots:
           do kr=2,ndeg,2   ! <-- index of "real" components
              ki=kr+1            ! <-- index of "imag" components
              bekr=be(kr)
              beki=be(ki)
              do j=1,lon2
                 do k=1,lat2
                    p1(k,j,i)=p1(k,j,i)+de(k,j,kr)
                    dekr=de(k,j,kr)+bekr*p2(k,j,i)
                    deki=de(k,j,ki)+beki*p2(k,j,i)
                    de(k,j,kr)=alv(k,kr,i,iv)*dekr-alv(k,ki,i,iv)*deki
                    de(k,j,ki)=alv(k,ki,i,iv)*dekr+alv(k,kr,i,iv)*deki
                 end do
              enddo
           enddo
        enddo

! Global branch, even degree     
     else

        ! advancing filter:
        do i=1,nsig
           !       treat remaining complex roots:
           do kr=1,ndeg,2   ! <-- index of "real" components
              ki=kr+1            ! <-- index of "imag" components
              bekr=be(kr)
              beki=be(ki)
              do j=1,lon2
                 do k=1,lat2
                    gakr=ga(k,j,kr)
                    gaki=ga(k,j,ki)

                    ga(k,j,kr)=alv(k,kr,i,iv)*gakr-alv(k,ki,i,iv)*gaki+bekr*p2(k,j,i)
                    ga(k,j,ki)=alv(k,ki,i,iv)*gakr+alv(k,kr,i,iv)*gaki+beki*p2(k,j,i)
                    p1(k,j,i)= p1(k,j,i)+ga(k,j,kr)
                 end do
              enddo
           enddo
        enddo
     
        !    backing filter:
        do i=nsig,1,-1
           !       treat remaining complex roots:
           do kr=1,ndeg,2   ! <-- index of "real" components
              ki=kr+1            ! <-- index of "imag" components
              bekr=be(kr)
              beki=be(ki)
              do j=1,lon2
                 do k=1,lat2
                    p1(k,j,i)=p1(k,j,i)+de(k,j,kr)
                    dekr=de(k,j,kr)+bekr*p2(k,j,i)
                    deki=de(k,j,ki)+beki*p2(k,j,i)
                    de(k,j,kr)=alv(k,kr,i,iv)*dekr-alv(k,ki,i,iv)*deki
                    de(k,j,ki)=alv(k,ki,i,iv)*dekr+alv(k,kr,i,iv)*deki
                 end do
              enddo
           enddo
        enddo
     endif
  end if
  return
end subroutine frfhvo

subroutine smoothzo(vx,samp,rate,iv,jx,dsv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smoothzo    initializes and renormalizes vertical smoothing coefs.
!   prgmmr: derber           org: np22                date: 2004-05-13
!
! abstract: initializes and renormalizes vertical smoothing coefficients
!           initializes dssv and alv
!
! program history log:
!   2004-05-13  derber, document
!   2004-11-30  treadon - add longitude dimension to variance array dssv
!   2010-03-01  zhu     - decide the location in alv and dsv based on anavinfo
!                       - add dsv in the interface, rm dssv in berror
!   2010-10-08  derber - optimize and clean up
!
!   input argument list:
!     vx       - vertical smoothing scales
!     samp     - parameter for smoothing        
!     rate     - parameter for smoothing       
!     iv       - location in alv and dssv for smoothing coefficients
!     jx       - latitude index
!
!   output argument list
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  use gridmod, only: nsig,lon2
  use berror, only: alv,ndeg
  implicit none
 
  integer(i_kind)             ,intent(in   ) :: jx,iv
  real(r_kind),dimension(nsig),intent(in   ) :: vx
  real(r_kind)                ,intent(in   ) :: samp
  real(r_kind),dimension(ndeg),intent(in   ) :: rate
  real(r_kind),dimension(lon2,nsig),intent(out):: dsv
 
  integer(i_kind) i,k,m
  real(r_kind),dimension(nsig):: dss
  real(r_kind),dimension(nsig,nsig):: p1
  real(r_kind),dimension(nsig,ndeg):: al

  call rfdparv(vx,rate,al,nsig,ndeg)
  do m=1,ndeg
     do k=1,nsig
        alv(jx,m,k,iv)=al(k,m)
     end do
  end do
  p1=zero
  do k=1,nsig
     dss(k)=sqrt(samp*vx(k))
     p1(k,k)=dss(k)
  end do

  call rfhvo(p1,nsig,nsig,al)
  
  call rfhvo(p1,nsig,nsig,al)

  do k=1,nsig
     do i=1,lon2
        dsv(i,k)=sqrt(dss(k)/p1(k,k))
     end do
  end do

  return
end subroutine smoothzo

subroutine rfhvo(p1,nc,n,al)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rfhvo   performs vertical smoothing for renormalization
!   prgmmr: derber           org: np22                date: 2004-05-13
!
! abstract: performs vertical smoothing of identity matrix for use with 
!           renormalization
!
! program history log:
!   2004-05-13  derber, document
!   2010-10-08  derber - optimize and clean up
!
!   input argument list:
!     p1       - input field to be smoothed (nc,n)
!     nc       - first array dimension for p1
!     n        - second array dimension for p1
!     al       - smoothing coefficients
!
!   output argument list
!     p1       - output field after vertical smoothing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use berror, only: be,ndeg
  implicit none
 
  integer(i_kind)               ,intent(in   ) :: n,nc
  real(r_kind),dimension(n,ndeg),intent(in   ) :: al
  real(r_kind),dimension(nc,n)  ,intent(inout) :: p1
 
  integer(i_kind) i,j,kr,ki
  real(r_kind) gaki,dekr,deki,gakr
  real(r_kind),dimension(nc,n):: p2
  real(r_kind),dimension(nc,ndeg):: ga,de


! Zero local work arrays.
  do j=1,ndeg
     do i=1,nc
        ga(i,j)=zero
        de(i,j)=zero
     end do
  end do

  do j=1,n
     do i=1,nc
        p2(i,j)=p1(i,j)
        p1(i,j)=zero
     end do
  end do

  if (mod(ndeg,2) == 1) then
!    advancing filter:
     do i=1,n
        do j=1,nc
           ga(j,1)=al(i,1)*ga(j,1)+be(1)*p2(j,i)
           p1(j,i)=p1(j,i)+ga(j,1)
        enddo
                           ! treat remaining complex roots:
        do kr=2,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           do j=1,nc
              gakr=ga(j,kr)
              gaki=ga(j,ki)
              ga(j,kr)=al(i,kr)*gakr-al(i,ki)*gaki+be(kr)*p2(j,i)
              ga(j,ki)=al(i,ki)*gakr+al(i,kr)*gaki+be(ki)*p2(j,i)
              p1(j,i)=p1(j,i)+ga(j,kr)
           enddo
        enddo
     enddo

!    backing filter:
     do i=n,1,-1
        do j=1,nc
           p1(j,i)=p1(j,i)+de(j,1)
           de(j,1)=al(i,1)*(de(j,1)+be(1)*p2(j,i))
        enddo
                           ! treat remaining complex roots:
        do kr=2,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           do j=1,nc
              p1(j,i)=p1(j,i)+de(j,kr)
              dekr=de(j,kr)+be(kr)*p2(j,i)
              deki=de(j,ki)+be(ki)*p2(j,i)
              de(j,kr)=al(i,kr)*dekr-al(i,ki)*deki
              de(j,ki)=al(i,ki)*dekr+al(i,kr)*deki
           enddo
        enddo
     enddo
     
  else
! advancing filter:
     do i=1,n
                           ! treat remaining complex roots:
        do kr=1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           do j=1,nc
              gakr=ga(j,kr)
              gaki=ga(j,ki)
              ga(j,kr)=al(i,kr)*gakr-al(i,ki)*gaki+be(kr)*p2(j,i)
              ga(j,ki)=al(i,ki)*gakr+al(i,kr)*gaki+be(ki)*p2(j,i)
              p1(j,i)=p1(j,i)+ga(j,kr)
           enddo
        enddo
     enddo

! backing filter:
     do i=n,1,-1
        ! treat remaining complex roots:
        do kr=1,ndeg,2   ! <-- index of "real" components
           ki=kr+1            ! <-- index of "imag" components
           do j=1,nc
              p1(j,i)=p1(j,i)+de(j,kr)
              dekr=de(j,kr)+be(kr)*p2(j,i)
              deki=de(j,ki)+be(ki)*p2(j,i)
              de(j,kr)=al(i,kr)*dekr-al(i,ki)*deki
              de(j,ki)=al(i,ki)*dekr+al(i,kr)*deki
           enddo
        enddo
     enddo
     
  endif
  return
end subroutine rfhvo

subroutine smoothzo1(vx,samp,rate,dsv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smoothzo    initializes and renormalizes vertical smoothing
! coefs.
!   prgmmr: derber           org: np22                date: 2004-05-13
!
! abstract: initializes and renormalizes vertical smoothing coefficients
!           initializes dssv  (no alv)
!
! program history log:
!   2004-05-13  derber, document
!   2004-11-30  treadon - add longitude dimension to variance array dssv
!   2010-03-01  zhu     - decide the location in alv and dsv based on anavinfo
!                       - add dsv in the interface, rm dssv in berror
!   2010-10-08  derber - optimize and clean up
!
!   2017-02-09  CAPS(G. Zhao) - remove initialization of alv (it is defined on
!                               llmin:llmax for regional, might cause problem.)
!
!   input argument list:
!     vx       - vertical smoothing scales
!     samp     - parameter for smoothing
!     rate     - parameter for smoothing
!     iv       - location in alv and dssv for smoothing coefficients
!     jx       - latitude index
!
!   output argument list
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only:  zero
  use gridmod, only: nsig,lon2
  use berror, only: ndeg
  implicit none

  real(r_kind),dimension(nsig),intent(in   ) :: vx
  real(r_kind)                ,intent(in   ) :: samp
  real(r_kind),dimension(ndeg),intent(in   ) :: rate
  real(r_kind),dimension(lon2,nsig),intent(out):: dsv

  integer(i_kind) i,k
  real(r_kind),dimension(nsig):: dss
  real(r_kind),dimension(nsig,nsig):: p1
  real(r_kind),dimension(nsig,ndeg):: al

  call rfdparv(vx,rate,al,nsig,ndeg)
  p1=zero
  do k=1,nsig
     dss(k)=sqrt(samp*vx(k))
     p1(k,k)=dss(k)
  end do

  call rfhvo(p1,nsig,nsig,al)

  call rfhvo(p1,nsig,nsig,al)

  do k=1,nsig
     do i=1,lon2
        dsv(i,k)=sqrt(dss(k)/p1(k,k))
     end do
  end do

  return
end subroutine smoothzo1
