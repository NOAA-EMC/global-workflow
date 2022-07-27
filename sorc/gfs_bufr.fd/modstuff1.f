  subroutine modstuff(km,idvc,idsl,nvcoord,vcoord,ps,psx,psy,d,u,v,&
                      pd,pm,om)
!                     pd,pi,pm,aps,apm,os,om,px,py)
!$$$  Subprogram documentation block
!
! Subprogram: modstuff   Compute model coordinate dependent functions
!   Prgmmr: Iredell      Org: np23        Date: 1999-10-18
!
! Abstract: This subprogram computes fields which depend on the model coordinate
!           such as pressure thickness and vertical velocity.
!
! Program history log:
!   1999-10-18  Mark Iredell
!
! Usage:  call modstuff(km,idvc,idsl,nvcoord,vcoord,ps,psx,psy,d,u,v,&
!                       pd,pi,pm,aps,apm,os,om,px,py)
!   Input argument list:
!     km       integer number of levels
!     idvc     integer vertical coordinate id (1 for sigma and 2 for hybrid)
!     idsl     integer type of sigma structure (1 for phillips or 2 for mean)
!     nvcoord  integer number of vertical coordinates
!     vcoord   real (km+1,nvcoord) vertical coordinates
!     ps       real surface pressure (Pa)
!     psx      real log surface pressure x-gradient (1/m)
!     psy      real log surface pressure y-gradient (1/m)
!     d        real (km) wind divergence (1/s)
!     u        real (km) x-component wind (m/s)
!     v        real (km) y-component wind (m/s)
!   Output argument list:
!     pd       real (km) pressure thickness (Pa)
!     pi       real (km+1) interface pressure (Pa)
!     pm       real (km) mid-layer pressure (Pa)
!     aps      real log surface pressure ()
!     apm      real (km+1) log mid-layer pressure ()
!     os       real (km) surface pressure tendency (Pa/s)
!     om       real (km) vertical velocity (Pa/s)
!     px       real (km) mid-layer pressure x-gradient (Pa/m)
!     py       real (km) mid-layer pressure y-gradient (Pa/m)
!
! Attributes:
!   Language: Fortran 90
!
!$$$
    use sigio_module
    implicit none
    integer,intent(in):: km,idvc,idsl,nvcoord
    real,intent(in):: vcoord(km+1,nvcoord)
    real,intent(in):: ps,psx,psy
    real,intent(in):: u(km),v(km),d(km)
    real,intent(out) :: pd(km),pm(km),om(km)
    real aps,apm(km),os,pi(km+1),px(km),py(km)
    real dpmdps(km),dpddps(km),dpidps(km+1),vgradp
    integer k,iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sigio_modpr(1,1,km,nvcoord,idvc,idsl,vcoord,iret,&
                     ps=(/ps/),&
                     pm=pm,pd=pd,dpmdps=dpmdps,dpddps=dpddps)
    pi(1)=ps
    dpidps(1)=1.
    do k=1,km
      pi(k+1)=pi(k)-pd(k)
      dpidps(k+1)=dpidps(k)-dpddps(k)
    enddo
    aps=log(ps)
    apm=log(pm)
    os=0
    do k=km,1,-1
      vgradp=u(k)*psx+v(k)*psy
      os=os-vgradp*ps*(dpmdps(k)-dpidps(k+1))-d(k)*(pm(k)-pi(k+1))
      om(k)=vgradp*ps*dpmdps(k)+os
      os=os-vgradp*ps*(dpidps(k)-dpmdps(k))-d(k)*(pi(k)-pm(k))
    enddo
    px=ps*dpmdps*psx
    py=ps*dpmdps*psy
  end subroutine
