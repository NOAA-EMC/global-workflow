  subroutine pvetc(km,p,px,py,t,tx,ty,h,u,v,av,hm,s,bvf2,pvn,theta,sigma,pvu)
!$$$  Subprogram documentation block
!
! Subprogram: pvetc      Compute potential vorticity, etc
!   Prgmmr: Iredell      Org: np23        Date: 1999-10-18
!
! Abstract: This subprogram computes
!             the Montgomery streamfunction
!               hm=cp*t+g*z
!             the specific entropy
!               s=cp*log(t/t0)-r*log(p/p0)
!             the Brunt-Vaisala frequency squared
!               bvf2=g/cp*ds/dz
!             the potential vorticity defined as
!               pvn=(av*ds/dz-dv/dz*ds/dx+du/dz*ds/dy)/rho/cp
!             the potential temperature
!               theta=t0*exp(s/cp)
!             the static stability
!               sigma=t/g*bvf2
!             and the potential vorticity in PV units
!               pvu=10**-6*theta*pvn
!
! Program history log:
!   1999-10-18  Mark Iredell
!
! Usage:  call pvetc(km,p,px,py,t,tx,ty,h,u,v,av,s,bvf2,pvn,theta,sigma,pvu)
!   Input argument list:
!     km       integer number of levels
!     p        real (km) pressure (Pa)
!     px       real (km) pressure x-gradient (Pa/m)
!     py       real (km) pressure y-gradient (Pa/m)
!     t        real (km) (virtual) temperature (K)
!     tx       real (km) (virtual) temperature x-gradient (K/m)
!     ty       real (km) (virtual) temperature y-gradient (K/m)
!     h        real (km) height (m)
!     u        real (km) x-component wind (m/s)
!     v        real (km) y-component wind (m/s)
!     av       real (km) absolute vorticity (1/s)
!   Output argument list:
!     hm       real (km) Montgomery streamfunction (m**2/s**2)
!     s        real (km) specific entropy (J/K/kg)
!     bvf2     real (km) Brunt-Vaisala frequency squared (1/s**2)
!     pvn      real (km) potential vorticity (m**2/kg/s)
!     theta    real (km) (virtual) potential temperature (K)
!     sigma    real (km) static stability (K/m)
!     pvu      real (km) potential vorticity (10**-6*K*m**2/kg/s)
!
! Modules used:
!   physcons       Physical constants
!
! Attributes:
!   Language: Fortran 90
!
!$$$
    use physcons, only: con_cp, con_g, con_rd, con_rocp
!
    implicit none
    integer,intent(in):: km
    real,intent(in), dimension(km):: p,px,py,t,tx,ty,h,u,v,av
    real,intent(out),dimension(km):: hm,s,bvf2,pvn,theta,sigma,pvu
!   real,parameter:: hhmin=500.,t0=2.e2,p0=1.e5
    real,parameter:: hhmin=5.,t0=2.e2,p0=1.e5
    integer k,kd,ku,k2(2)
    real cprho,sx,sy,sz,uz,vz
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    do k=1,km
      hm(k) = con_cp*t(k) + con_g*h(k)
      s(k)  = con_cp*log(t(k)/t0) - con_rd*log(p(k)/p0)
    enddo
    do k=1,km
      call rsearch1(km,h,2,(/h(k)-hhmin,h(k)+hhmin/),k2)
!     kd = max(k2(1),1)
!     ku = min(k2(2)+1,km)
!     kd = min(k2(1),km)   ! Chuang: post counts from top down, redefine lower bound
      kd = min(k2(1)+1,km) ! Chuang: post counts from top down,
!     ku = max(k2(2)-1,1)
      ku = max(k2(2),1)
      if(ku==1) kd=2       ! Chuang: make sure ku ne kd at model top
      cprho    = p(k)/(con_rocp*t(k))
      sx       = con_cp*tx(k)  / t(k)-con_rd*px(k)/p(k)
      sy       = con_cp*ty(k)  / t(k)-con_rd*py(k)/p(k)
      sz       = (s(ku)-s(kd)) / (h(ku)-h(kd))
      uz       = (u(ku)-u(kd)) / (h(ku)-h(kd))
      vz       = (v(ku)-v(kd)) / (h(ku)-h(kd))
      bvf2(k)  = con_g/con_cp*sz
      pvn(k)   = (av(k)*sz - vz*sx + uz*sy) / cprho
      theta(k) = t0*exp(s(k)/con_cp)
      sigma(k) = t(k)/con_g*bvf2(k)
      pvu(k)   = 1.e6*theta(k)*pvn(k)
    enddo
  end subroutine
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  subroutine p2th(km,theta,u,v,h,t,pvu,sigma,rh,omga,kth,th &
                 ,lth,uth,vth,hth,tth,zth,sigmath,rhth,oth)
!$$$  Subprogram documentation block
!
! Subprogram: p2th       Interpolate to isentropic level
!   Prgmmr: Iredell      Org: np23        Date: 1999-10-18
!
! Abstract: This subprogram interpolates fields to given isentropic levels.
!   The interpolation is linear in entropy.
!   Outside the domain the bitmap is set to false.
!
! Program history log:
!   1999-10-18  Mark Iredell
!
! Usage:  call p2th(km,theta,u,v,h,t,puv,kth,th,uth,vth,tth)
!   Input argument list:
!     km       integer number of levels
!     theta    real (km) potential temperature (K)
!     u        real (km) x-component wind (m/s)
!     v        real (km) y-component wind (m/s)
!     h        real (km) height (m)
!     t        real (km) temperature (K)
!     pvu      real (km) potential vorticity in PV units (10**-6*K*m**2/kg/s)
!     kth      integer number of isentropic levels
!     th       real (kth) isentropic levels (K)
!   Output argument list:
!     lpv      logical*1 (kth) bitmap
!     uth      real (kth) x-component wind (m/s)
!     vth      real (kth) y-component wind (m/s)
!     hth      real (kth) height (m)
!     tth      real (kth) temperature (K)
!     zth      real (kth) potential vorticity in PV units (10**-6*K*m**2/kg/s)
!
! Subprograms called:
!   rsearch1       search for a surrounding real interval
!
! Attributes:
!   Language: Fortran 90
!
!$$$
    implicit none
    integer,intent(in):: km,kth
    real,intent(in),dimension(km):: theta,u,v,h,t,pvu,sigma,rh,omga
    real,intent(in):: th(kth)
    logical*1,intent(out),dimension(kth):: lth
    real,intent(out),dimension(kth):: uth,vth,hth,tth,zth &
                                     ,sigmath,rhth,oth
    real w
    integer loc(kth),l
    integer k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call rsearch1(km,theta(1),kth,th(1),loc(1))
    do k=1,kth
      l = loc(k)
      lth(k) = l > 0 .and.l < km
      if(lth(k)) then
        w = log(th(k)/theta(l)) / log(theta(l+1)/theta(l))
        uth(k)     = u(l)     + w*(u(l+1)-u(l))
        vth(k)     = v(l)     + w*(v(l+1)-v(l))
        hth(k)     = h(l)     + w*(h(l+1)-h(l))
        tth(k)     = t(l)     + w*(t(l+1)-t(l))
        zth(k)     = pvu(l)   + w*(pvu(l+1)-pvu(l))
        sigmath(k) = sigma(l) + w*(sigma(l+1)-sigma(l))
        rhth(k)    = rh(l)    + w*(rh(l+1)-rh(l))
!	pth(k)     = p(l)     + w*(p(l+1)-p(l))
        oth(k)     = omga(l)  + w*(omga(l+1)-omga(l))
      endif
    enddo
  end subroutine
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  subroutine p2pv(km,pvu,h,t,p,u,v,kpv,pv,pvpt,pvpb,&
                  lpv,upv,vpv,hpv,tpv,ppv,spv)
!$$$  Subprogram documentation block
!
! Subprogram: p2pv       Interpolate to potential vorticity level
!   Prgmmr: Iredell      Org: np23        Date: 1999-10-18
!
! Abstract: This subprogram interpolates fields to given potential vorticity
!   levels within given pressure limits.
!   The output level is the first  encountered from the top pressure limit.
!   If the given potential vorticity level is not found, the outputs are zero
!   and the bitmap is false. The interpolation is linear in potential vorticity.
!
! Program history log:
!   1999-10-18  Mark Iredell
!
! Usage:  call p2pv(km,pvu,h,t,p,u,v,kpv,pv,pvpt,pvpb,&
!                   lpv,upv,vpv,hpv,tpv,ppv,spv)
!   Input argument list:
!     km       integer number of levels
!     pvu      real (km) potential vorticity in PV units (10**-6*K*m**2/kg/s)
!     h        real (km) height (m)
!     t        real (km) temperature (K)
!     p        real (km) pressure (Pa)
!     u        real (km) x-component wind (m/s)
!     v        real (km) y-component wind (m/s)
!     kpv      integer number of potential vorticity levels
!     pv       real (kpv) potential vorticity levels (10**-6*K*m**2/kg/s)
!     pvpt     real (kpv) top pressures for PV search (Pa)
!     pvpb     real (kpv) bottom pressures for PV search (Pa)
!   Output argument list:
!     lpv      logical*1 (kpv) bitmap
!     upv      real (kpv) x-component wind (m/s)
!     vpv      real (kpv) y-component wind (m/s)
!     hpv      real (kpv) temperature (K)
!     tpv      real (kpv) temperature (K)
!     ppv      real (kpv) pressure (Pa)
!     spv      real (kpv) wind speed shear (1/s)
!
! Subprograms called:
!   rsearch1       search for a surrounding real interval
!
! Attributes:
!   Language: Fortran 90
!
!$$$
    use physcons, only: con_rog
    implicit none
    integer,intent(in):: km,kpv
    real,intent(in),dimension(km):: pvu,h,t,p,u,v
    real,intent(in):: pv(kpv),pvpt(kpv),pvpb(kpv)
    logical*1,intent(out),dimension(kpv):: lpv
    real,intent(out),dimension(kpv):: upv,vpv,hpv,tpv,ppv,spv
    real,parameter:: pd=2500.
    real w,spdu,spdd
    integer k,l1,l2,lu,ld,l
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    do k=1,kpv
      call rsearch1(km,p,1,pvpb(k),l1)
      call rsearch1(km,p,1,pvpt(k),l2)
!     l1=l1+1
      l = 0
      if(pv(k) >= 0.) then
!       do lu=l2-1,l1,-1
!       do lu=l2,l1-1 ! Chuang: post counts top down	
        do lu=l2+2,l1 ! Chuang: post counts top down
!         if(pv(k).lt.pvu(lu+1).and.pv(k).ge.pvu(lu)) then
          if(pv(k) >= pvu(lu+1).and.pv(k) < pvu(lu)) then
            call rsearch1(km,p,1,p(lu)+pd,ld)
!           if(all(pv(k).ge.pvu(ld:lu-1))) then
            if(all(pv(k) >= pvu(lu+1:ld))) then
              l = lu
              exit
            endif
          endif
        enddo
      else
!       do lu=l2-1,l1,-1
!       do lu=l2,l1-1 ! Chuang: post counts top down	
        do lu=l2+2,l1 ! Chuang: post counts top down
!         if(pv(k).gt.pvu(lu+1).and.pv(k).le.pvu(lu)) then
          if(pv(k) <= pvu(lu+1).and.pv(k) > pvu(lu)) then
            call rsearch1(km,p,1,p(lu)+pd,ld)
!           if(all(pv(k).le.pvu(ld:lu-1))) then
            if(all(pv(k) <= pvu(lu+1:ld))) then
              l = lu
              exit
            endif
          endif
        enddo
      endif
      lpv(k) = l > 0
      if(lpv(k)) then
        w = (pv(k)-pvu(l))/(pvu(l+1)-pvu(l))
        upv(k) = u(l) + w*(u(l+1)-u(l))
        vpv(k) = v(l) + w*(v(l+1)-v(l))
        hpv(k) = h(l) + w*(h(l+1)-h(l))
        tpv(k) = t(l) + w*(t(l+1)-t(l))
        ppv(k) = p(l)*exp((h(l)-hpv(k))*(1-0.5*(tpv(k)/t(l)-1))/(con_rog*t(l)))

        spdu   = sqrt(u(l+1)*u(l+1) + v(l+1)*v(l+1))
        spdd   = sqrt(u(l)*u(l)     + v(l)*v(l))
        spv(k) = (spdu-spdd) / (h(l+1)-h(l))
      endif
    enddo
  end subroutine
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine rsearch1(km1,z1,km2,z2,l2)
!$$$  subprogram documentation block
!
! subprogram:    rsearch1    search for a surrounding real interval
!   prgmmr: iredell    org: w/nmc23     date: 98-05-01
!
! abstract: this subprogram searches a monotonic sequences of real numbers
!   for intervals that surround a given search set of real numbers.
!   the sequences may be monotonic in either direction; the real numbers
!   may be single or double precision.
!
! program history log:
! 1999-01-05  mark iredell
!
! usage:    call rsearch1(km1,z1,km2,z2,l2)
!   input argument list:
!     km1    integer number of points in the sequence
!     z1     real (km1) sequence values to search
!            (z1 must be monotonic in either direction)
!     km2    integer number of points to search for
!     z2     real (km2) set of values to search for
!            (z2 need not be monotonic)
!
!   output argument list:
!     l2     integer (km2) interval locations from 0 to km1
!            (z2 will be between z1(l2) and z1(l2+1))
!
! subprograms called:
!   sbsrch essl binary search
!   dbsrch essl binary search
!
! remarks:
!   returned values of 0 or km1 indicate that the given search value
!   is outside the range of the sequence.
!
!   if a search value is identical to one of the sequence values
!   then the location returned points to the identical value.
!   if the sequence is not strictly monotonic and a search value is
!   identical to more than one of the sequence values, then the
!   location returned may point to any of the identical values.
!
!   if l2(k)=0, then z2(k) is less than the start point z1(1)
!   for ascending sequences (or greater than for descending sequences).
!   if l2(k)=km1, then z2(k) is greater than or equal to the end point
!   z1(km1) for ascending sequences (or less than or equal to for
!   descending sequences).  otherwise z2(k) is between the values
!   z1(l2(k)) and z1(l2(k+1)) and may equal the former.
!
! attributes:
!   language: fortran
!
!$$$
  implicit none
  integer,intent(in):: km1,km2
  real,intent(in):: z1(km1),z2(km2)
  integer,intent(out):: l2(km2)
  integer k1,k2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  find the surrounding input interval for each output point.
  if(z1(1) <= z1(km1)) then
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  input coordinate is monotonically ascending.
    do k2=1,km2
      if (z1(1) >= z2(k2)) then
        l2(k2) = 1
      else
      l2(k2)=km1
      do k1=1,km1-1
        if(z1(k1) <= z2(k2) .and. z1(k1+1) > z2(k2)) then
          l2(k2) = k1
          exit
        endif
      enddo
     endif
    enddo
  else
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  input coordinate is monotonically descending.
    do k2=1,km2
     if (z1(1) <= z2(k2)) then
        l2(k2) = 1
      else
      l2(k2)=km1
      do k1=km1,2,-1
        if(z2(k2) >= z1(k1) .and. z2(k2) < z1(k1-1)) then
          l2(k2) = k1-1
          exit
        endif
      enddo
     endif
    enddo
  endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
end subroutine
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  subroutine tpause(km,p,u,v,t,h,ptp,utp,vtp,ttp,htp,shrtp)
!$$$  Subprogram documentation block
!
! Subprogram: tpause     Compute tropopause level fields
!   Prgmmr: Iredell      Org: np23        Date: 1999-10-18
!
! Abstract: This subprogram finds the tropopause level and computes fields 
!   at the tropopause level.  The tropopause is defined as the lowest level
!   above 500 mb which has a temperature lapse rate of less than 2 K/km.
!   The lapse rate must average less than 2 K/km over a 2 km depth.
!   If no such level is found below 50 mb, the tropopause is set to 50 mb.
!   The tropopause fields are interpolated linearly in lapse rate.
!   The tropopause pressure is found hydrostatically.
!   The tropopause wind shear is computed as the partial derivative
!   of wind speed with respect to height at the tropopause level.
!
! Program history log:
!   1999-10-18  Mark Iredell
!
! Usage:  call tpause(km,p,u,v,t,h,ptp,utp,vtp,ttp,htp,shrtp)
!   Input argument list:
!     km       integer number of levels
!     p        real (km) pressure (Pa)
!     u        real (km) x-component wind (m/s)
!     v        real (km) y-component wind (m/s)
!     t        real (km) temperature (K)
!     h        real (km) height (m)
!   Output argument list:
!     ptp      real tropopause pressure (Pa)
!     utp      real tropopause x-component wind (m/s)
!     vtp      real tropopause y-component wind (m/s)
!     ttp      real tropopause temperature (K)
!     htp      real tropopause height (m)
!     shrtp    real tropopause wind shear (1/s)
!
! Files included:
!   physcons.h     Physical constants
!
! Subprograms called:
!   rsearch1       search for a surrounding real interval
!
! Attributes:
!   Language: Fortran 90
!
!$$$
    use physcons, only: con_rog
    implicit none
    integer,intent(in):: km
    real,intent(in),dimension(km):: p,u,v,t,h
    real,intent(out):: ptp,utp,vtp,ttp,htp,shrtp
    real,parameter:: ptplim(2)=(/500.e+2,50.e+2/),gamtp=2.e-3,hd=2.e+3
    real gamu,gamd,td,gami,wtp,spdu,spdd
    integer klim(2),k,kd,ktp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  find tropopause level
    call rsearch1(km-2,p(2),2,ptplim(1),klim(1))
    klim(1)=klim(1)+1
    klim(2)=klim(2)+2
    ! klim(1) > klim(2) or loops does not run ; klim(2) has a
    ! minimum value of 3 to insure k-2 != 0 in called subprogram
    gamd=1.e+9
    ktp=klim(2)
    wtp=0
!    do k=klim(1),klim(2)
    do k=klim(1),klim(2),-1
!      gamu=(t(k-1)-t(k+1))/(h(k+1)-h(k-1))
      gamu=(t(k+1)-t(k-1))/(h(k-1)-h(k+1))
      if(gamu.le.gamtp) then
!        call rsearch1(km-k-1,h(k+1),1,h(k)+hd,kd)
	call rsearch1(k-2,h(2),1,h(k)+hd,kd)
!        td=t(k+kd)+(h(k)+hd-h(k+kd))/(h(k+kd+1)-h(k+kd))*(t(k+kd+1)-t(k+kd))
	td=t(kd+2)+(h(k)+hd-h(2+kd))/(h(kd+1)-h(2+kd))*(t(kd+1)-t(2+kd))
        gami=(t(k)-td)/hd
        if(gami.le.gamtp) then
          ktp=k
          wtp=(gamtp-gamu)/(max(gamd,gamtp+0.1e-3)-gamu)
          exit
        endif
      endif
      gamd=gamu
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute tropopause level fields
    utp=u(ktp)-wtp*(u(ktp)-u(ktp-1))
    vtp=v(ktp)-wtp*(v(ktp)-v(ktp-1))
    ttp=t(ktp)-wtp*(t(ktp)-t(ktp-1))
    htp=h(ktp)-wtp*(h(ktp)-h(ktp-1))
    ptp=p(ktp)*exp((h(ktp)-htp)*(1-0.5*(ttp/t(ktp)-1))/(con_rog*t(ktp)))
    spdu=sqrt(u(ktp)**2+v(ktp)**2)
    spdd=sqrt(u(ktp-1)**2+v(ktp-1)**2)
    shrtp=(spdu-spdd)/(h(ktp)-h(ktp-1))
    
    utp=u(ktp)-wtp*(u(ktp)-u(ktp+1))
    vtp=v(ktp)-wtp*(v(ktp)-v(ktp+1))
    ttp=t(ktp)-wtp*(t(ktp)-t(ktp+1))
    htp=h(ktp)-wtp*(h(ktp)-h(ktp+1))
    ptp=p(ktp)*exp((h(ktp)-htp)*(1-0.5*(ttp/t(ktp)-1))/(con_rog*t(ktp)))
    spdu=sqrt(u(ktp)**2+v(ktp)**2)
    spdd=sqrt(u(ktp+1)**2+v(ktp+1)**2)
    shrtp=(spdu-spdd)/(h(ktp)-h(ktp+1))
  end subroutine
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  subroutine mxwind(km,p,u,v,t,h,pmw,umw,vmw,tmw,hmw)
!$$$  Subprogram documentation block
!
! Subprogram: mxwind     Compute maximum wind level fields
!   Prgmmr: Iredell      Org: np23        Date: 1999-10-18
!
! Abstract: This subprogram finds the maximum wind level and computes fields 
!   at the maximum wind level.  The maximum wind level is searched for
!   between 500 mb and 100 mb.  The height and wind speed at the maximum wind
!   speed level is calculated by assuming the wind speed varies quadratically
!   in height in the neighborhood of the maximum wind level.  The other fields
!   are interpolated linearly in height to the maximum wind level.
!   The maximum wind level pressure is found hydrostatically.
!
! Program history log:
!   1999-10-18  Mark Iredell
!   2005-02-02  Mark Iredell  changed upper limit to 100 mb
!
! Usage:  call mxwind(km,p,u,v,t,h,pmw,umw,vmw,tmw,hmw)
!   Input argument list:
!     km       integer number of levels
!     p        real (km) pressure (Pa)
!     u        real (km) x-component wind (m/s)
!     v        real (km) y-component wind (m/s)
!     t        real (km) temperature (K)
!     h        real (km) height (m)
!   Output argument list:
!     pmw      real maximum wind level pressure (Pa)
!     umw      real maximum wind level x-component wind (m/s)
!     vmw      real maximum wind level y-component wind (m/s)
!     tmw      real maximum wind level temperature (K)
!     hmw      real maximum wind level height (m)
!
! Files included:
!   physcons.h     Physical constants
!
! Subprograms called:
!   rsearch1       search for a surrounding real interval
!
! Attributes:
!   Language: Fortran 90
!
!$$$
    use physcons, only: con_rog
    implicit none
    integer,intent(in):: km
    real,intent(in),dimension(km):: p,u,v,t,h
    real,intent(out):: pmw,umw,vmw,tmw,hmw
    real,parameter:: pmwlim(2)=(/500.e+2,100.e+2/)
    integer klim(2),k,kmw
    real spd(km),spdmw,wmw,dhd,dhu,shrd,shru,dhmw,ub,vb,spdb
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  find maximum wind level
    call rsearch1(km,p(1),2,pmwlim(1),klim(1))
!    klim(1)=klim(1)+1
    klim(2)=klim(2)+1
!    spd(klim(1):klim(2))=sqrt(u(klim(1):klim(2))**2+v(klim(1):klim(2))**2)
    spd(klim(2):klim(1))=sqrt(u(klim(2):klim(1))**2+v(klim(2):klim(1))**2)
    spdmw=spd(klim(1))
    kmw=klim(1)
!    do k=klim(1)+1,klim(2)
    do k=klim(1)-1,klim(2),-1
      if(spd(k).gt.spdmw) then
        spdmw=spd(k)
        kmw=k
      endif
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  find speed and height at the maximum wind level
    if(kmw.eq.klim(1).or.kmw.eq.klim(2)) then
      hmw=h(kmw)
      spdmw=spd(kmw)
      wmw=0.
    else
!      dhd=h(kmw)-h(kmw-1)
      dhd=h(kmw)-h(kmw+1) !post counts top down
!      dhu=h(kmw+1)-h(kmw)
      dhu=h(kmw-1)-h(kmw)
!      shrd=(spd(kmw)-spd(kmw-1))/(h(kmw)-h(kmw-1))
      shrd=(spd(kmw)-spd(kmw+1))/(h(kmw)-h(kmw+1))
!      shru=(spd(kmw)-spd(kmw+1))/(h(kmw+1)-h(kmw))
      shru=(spd(kmw)-spd(kmw-1))/(h(kmw-1)-h(kmw))
      dhmw=(shrd*dhu-shru*dhd)/(2*(shrd+shru))
      hmw=h(kmw)+dhmw
      spdmw=spd(kmw)+dhmw**2*(shrd+shru)/(dhd+dhu)
!      if(dhmw.gt.0) kmw=kmw+1
      if(dhmw.gt.0) kmw=kmw-1
!      wmw=(h(kmw)-hmw)/(h(kmw)-h(kmw-1))
      wmw=(h(kmw)-hmw)/(h(kmw)-h(kmw+1))
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute maximum wind level fields
!    ub=u(kmw)-wmw*(u(kmw)-u(kmw-1))
    ub=u(kmw)-wmw*(u(kmw)-u(kmw+1))
!    vb=v(kmw)-wmw*(v(kmw)-v(kmw-1))
    vb=v(kmw)-wmw*(v(kmw)-v(kmw+1))
    spdb=max(sqrt(ub**2+vb**2),1.e-6)
    umw=ub*spdmw/spdb
    vmw=vb*spdmw/spdb
!    tmw=t(kmw)-wmw*(t(kmw)-t(kmw-1))
    tmw=t(kmw)-wmw*(t(kmw)-t(kmw+1))
    pmw=p(kmw)*exp((h(kmw)-hmw)*(1-0.5*(tmw/t(kmw)-1))/(con_rog*t(kmw)))
  end subroutine 
  
! Add Iredells subroutine to read sigma files
!-------------------------------------------------------------------------------
subroutine rtsig(lusig,head,k1,k2,kgds,ijo,levs,ntrac,jcap,lnt2,me,     &
                 h,p,px,py,t,u,v,d,trc,iret)
!$$$  Subprogram documentation block
!
! Subprogram: rtsig      Read and transform sigma file
!   Prgmmr: Iredell      Org: np23        Date: 1999-10-18
!
! Abstract: This subprogram reads a sigma file and transforms
!   the fields to a designated global grid.
!
! Program history log:
!   1999-10-18  Mark Iredell
!   2013-04-19  Jun  Wang:  add option to get tmp and ps(in pascal) 
!                           from enthalpy and ps(cb) option
!   2013-05-06 Shrinivas Moorthi: Initialize midea to 0
!   2013-05-07 Shrinivas Moorthi: Remove mo3, mct, midea and define io3, ict etc
!                                 correctly and get correct cloud condensate.
!   2013-08-02 Shrinivas Moorthi: Rewrote the whole routine to read the sigma
!                                 file differently and to read all tracers
!                                 Addedd sptezj for two 2d fields
!   2014-02-20 Shrinivas Moorthi: Modified conversion from spectral to grid
!                                 taking advantage of threding in SP library.
!                                 This really speeds up the code
!                                 Also threaded loop for Temperature from Tv

!
! Usage:  call rtsig(lusig,head,k1,k2,kgds,ijo,nct,                     &
!                    h,p,px,py,t,tx,ty,u,v,d,z,sh,o3,ct,iret,o,o2)
!   Input argument list:
!     lusig    integer(sigio_intkind) sigma file unit number
!     head     type(sigio_head) sigma file header
!     k1       integer first model level to return
!     k2       integer last model level to return
!     kgds     integer (200) GDS to which to transform
!     ijo      integer dimension of output fields
!     levs     integer number of total vertical levels
!     ntrac    integer number of output tracers
!     jcap     integer number of waves
!     lnt2     integer (jcap+1)*(jcap+2)
!   Output argument list:
!     h        real (ijo) surface orography (m)
!     p        real (ijo) surface pressure (Pa)
!     px       real (ijo) log surface pressure x-gradient (1/m)
!     py       real (ijo) log surface pressure y-gradient (1/m)
!     t        real (ijo,k1:k2) temperature (K)
!     tx       real (ijo,k1:k2) virtual temperature x-gradient (K/m)
!     ty       real (ijo,k1:k2) virtual temperature y-gradient (K/m)
!     u        real (ijo,k1:k2) x-component wind (m/s)
!     v        real (ijo,k1:k2) y-component wind (m/s)
!     d        real (ijo,k1:k2) wind divergence (1/s)
!     trc      real (ijo,k1:k2,ntrac) tracers
!                                1 = specific humidity (kg/kg)
!                                2 = Ozone mixing ratio (kg/kg)
!                                3 = cloud condensate mixing ratio (kg/kg)
!                                .
!                                .
!                                    atomic oxyge, oxygen etc
!
!     iret     integer return code
!
! Modules used:
!   sigio_r_module sigma file I/O
!
! Subprograms called:
!   sigio_rrdati   read sigma single data field
!   sptez          scalar spectral transform
!   sptezd         gradient spectral transform
!   sptezm         multiple scalar spectral transform
!   sptezmv        multiple vector spectral transform
!
! Attributes:
!   Language: Fortran 90
!
!$$$
  use sigio_module,   only : sigio_intkind, sigio_head
  use sigio_r_module, only : sigio_dati, sigio_rrdati
  use physcons,       only : con_omega, con_fvirt
  use omp_lib
  implicit none
  integer(sigio_intkind),intent(in)    :: lusig
  type(sigio_head),      intent(in)    :: head
  integer,intent(in)                   :: k1,k2,kgds(200),ijo,levs,ntrac,jcap,lnt2,me
  real,dimension(ijo),   intent(out)   :: h,p,px,py
  real,dimension(ijo,k1:k2),intent(out):: t,u,v,d
  real,dimension(ijo,k1:k2,ntrac),intent(out),target :: trc
  integer,intent(out) :: iret
!
  integer idrt,io,jo,iidea
! integer idrt,io,jo,mo3,mct,iidea,midea
  integer(sigio_intkind):: irets
! type(sigio_datm):: datm
  type(sigio_dati) dati
! type griddata
! real,dimension(:,:),pointer :: datm
! endtype griddata
! type(griddata),dimension(:),pointer :: datatrc
  real, target ::  trisca(lnt2,k1:k2+1), triscb(lnt2,k1:k2)
  real,dimension(:),  allocatable :: cpi
  real,dimension(:,:),allocatable :: wrk
  integer io3,ict,jct,n,i,k,jc,nt
  integer idvm, klen
  real    pmean,sumq,xcp
! integer, parameter :: latch=20
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Determine output grid
  idrt = kgds(1)
  if(kgds(1) == 0 .and. kgds(4) < 90000) idrt = 256
  io = kgds(2)
  jo = kgds(3)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Read and transform surface fields
  iret = 1
  if (me == 0) then
    print*,'Debug rtsig= ',lusig,k1,k2,ijo,kgds(1:20)
  endif

  idvm  = head%idvm
! jc = omp_get_num_threads()
! write(0,*)' in RTSIG lnt2=',lnt2,' threads=',jc,' latch=',latch,   &
!          ' jcap=',jcap,' io=',io,' jo=',jo,' ijo=',ijo
!
  if (k2 < k1) return

  dati%i = 1                                           ! hs
  dati%f => trisca(:,k1)
  call sigio_rrdati(lusig,head,dati,irets)
  if(irets /= 0) return

! call sptez(0,jcap,idrt,io,jo,trisca(1,k1),h,1)
! call sptez(0,jcap,idrt,io,jo,dats%hs,h,1)
! call sptez(0,jcap,idrt,io,jo,dats%ps,p,1)
! call sptezj(jcap,lnt2,1,idrt,io,jo,jc,trisca,h,latch,1)
!
  dati%i = 2                               ! Surface pressure
  dati%f => trisca(:,k1+1)
  call sigio_rrdati(lusig,head,dati,irets)
  if(irets /= 0) return
!
! call sptez(0,jcap,idrt,io,jo,trisca(1,k1),p,1)
! call sptezj(jcap,lnt2,1,idrt,io,jo,jc,trisca,p,latch,1)
!--
  allocate(wrk(ijo,2))
  call sptezm(0,jcap,idrt,io,jo,2,trisca(1,k1),wrk,1)
  if( mod(idvm,10) < 2) then
!$omp parallel do private(i)
    do i=1,ijo
      h(i) = wrk(i,1)
      p(i) = 1.e3*exp(wrk(i,2))
!     p(i) = 1.e3*exp(p(i))
    enddo
  elseif(mod(idvm,10) == 2) then
!$omp parallel do private(i)
    do i=1,ijo
      h(i) = wrk(i,1)
      p(i) = 1000.*wrk(i,2)
!     p(i) = 1000.*p(i)
    enddo
  endif
  if (allocated(wrk)) deallocate(wrk)

  call sptezd(0,jcap,idrt,io,jo,trisca(1,k1+1),pmean,px,py,1)
  iret = 0

! if (k2 < k1) return

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Read and transform fields on levels k1 through k2
  iret = 2
  if (k2 >= k1) then
    klen = k2-k1+1
    do k=k1,k2
      write(0,*)' retriving T for k=',k,' k1=',k1,' k2=',k2
      dati%i = k + 2                        ! Virtual Temperature or CpT
      dati%f => trisca(:,k)

      call sigio_rrdati(lusig,head,dati,iret)
    enddo
    call sptezm(0,jcap,idrt,io,jo,klen,trisca(1,k1),t(1,k1),1)
!   call sptezm(0,jcap,idrt,io,jo,klen,trisca,t,1)
    do k=k1,k2
      dati%i = levs + 2 + (k-1) * 2 + 1     ! Divergence
      dati%f => trisca(:,k)
      call sigio_rrdati(lusig,head,dati,irets)
      if(irets /= 0) return
      dati%i = levs + 2 + (k-1) * 2 + 2     ! Vorticity
      dati%f => triscb(:,k)
      call sigio_rrdati(lusig,head,dati,irets)
      if(irets /= 0) return
    enddo
    call sptezmv(0,jcap,idrt,io,jo,klen,trisca(1,k1),triscb(1,k1),  &
                 u(1,k1),v(1,k1),1)
    call sptezm(0,jcap,idrt,io,jo,klen,trisca(1,k1),d(1,k1),1)

!   call sptezm(0,jcap,idrt,io,jo,1,triscb,z(1,k),1)
    write(0,*)' retriving d/z for k=',k,' k1=',k1,' k2=',k2
!   datm%z(3,:) = datm%z(3,:)+2*con_omega/sqrt(1.5)
!   call sptezm(0,jcap,idrt,io,jo,klen,datm%z,z,1)
   write(0,*)' start get tracer'
    do nt=1,ntrac
      do k=k1,k2
        dati%i = levs * (2+nt) + 2 + k      ! Tracers starting with q
        dati%f => trisca(:,k)
        call sigio_rrdati(lusig,head,dati,irets)
      enddo
      call sptezm(0,jcap,idrt,io,jo,klen,trisca(1,k1),trc(1,k1,nt),1)
      write(0,*)' retriving d/z for nt=',nt,'ntrac=',ntrac,'k=',k,' k1=',k1,' k2=',k2
    enddo
    !t=t/(1+con_fvirt*sh)
   write(0,*)' end get tracer,idvm=',idvm,'ijo=',ijo,'ntrac=',ntrac
!
!-- get temp 
    if (mod(idvm/10,10) == 3) then ! Enthalpy case
      allocate(cpi(0:ntrac))
!     write(0,*)'aft read sig, cpi=',head%cpi
      cpi(0:ntrac) = head%cpi(1:ntrac+1)
!     write(0,*)'cpi=',cpi(0:ntrac)
!$omp parallel do private(k,i,xcp,sumq,n)
      do k=k1,k2
        do i=1,ijo
          xcp  = 0.0
          sumq = 0.0
          do n=1,ntrac
            if( cpi(n) /= 0.0 ) then
              xcp  = xcp  + cpi(n)*trc(i,k,n)
              sumq = sumq + trc(i,k,n)
            endif
          enddo
          xcp    = (1.-sumq)*cpi(0) + xcp
          t(i,k) = t(i,k) / xcp   ! Now g1 contains T
        enddo
      enddo
      if (allocated(cpi)) deallocate(cpi)
    else
!$omp parallel do private(i,k)
      do k=k1,k2
        do i=1,ijo
          t(i,k) = t(i,k) / (1+con_fvirt*trc(i,k,1)) !get temp from virtual temp
        enddo
      enddo
    endif
  endif
! write(0,*)'end comput t'
  iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
end subroutine  

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  subroutine modstuff(km,idvc,idsl,nvcoord,vcoord,ps,psx,psy,d,u,v,&
                      pi,pm,om)
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
!   2013-04-19  Jun  Wang:  add option to get pi by using 8byte real computation
!
! Usage:  call modstuff(km,idvc,idsl,nvcoord,vcoord,ps,psx,psy,d,u,v,&
!                       pd,pi,pm,os,om,px,py)
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
!     pi       real (km+1) interface pressure (Pa)
!     pm       real (km) mid-layer pressure (Pa)
!     om       real (km) vertical velocity (Pa/s)
!
! Attributes:
!   Language: Fortran 90
!
!$$$
    use sigio_module, only: sigio_modprd
    implicit none
    integer,intent(in):: km,idvc,idsl,nvcoord
    real,intent(in):: vcoord(km+1,nvcoord)
    real,intent(in):: ps,psx,psy
    real,intent(in):: u(km),v(km),d(km)
!   real,intent(out):: pi(km+1),pm(km)
    real*8, intent(out):: pi(km+1),pm(km)
    real,intent(out):: om(km)
    real*8 ps8,pm8(km),pd8(km),vcoord8(km+1,nvcoord)
    real*8 dpmdps(km),dpddps(km),dpidps(km+1),pi8(km+1)
    real vgradp,pd(km),px(km),py(km),os
    integer k,iret,logk
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ps8=ps
    vcoord8=vcoord
    call sigio_modprd(1,1,km,nvcoord,idvc,idsl,vcoord8,iret,&
                     ps=(/ps8/),&
                     pm=pm8,pd=pd8,dpmdps=dpmdps,dpddps=dpddps)
!
!jw: has to be 8 real for wam     
    pi8(1)=ps
    pm=pm8
!   pd=pd8
    dpidps(1)=1.
    do k=1,km
      pi8(k+1)=pi8(k)-pd8(k)
      dpidps(k+1)=dpidps(k)-dpddps(k)
!     if(pi(8)<0.) then
!        print *,'in modstuff,pi8=',pi8(k)
!     endif
    enddo
    pi=pi8
!
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

!-------------------------------------------------------------------------------
  subroutine modstuff2(im,ix,km,idvc,idsl,nvcoord,vcoord,ps,psx,psy,d,u,v,&
                       pi,pm,om,me)
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
!   2013-04-19  Jun  Wang:  add option to get pi by using 8byte real computation
!   2013-08-13  Shrinivas Moorthi - Modified to include im points and thread
!
! Usage:  call modstuff(km,idvc,idsl,nvcoord,vcoord,ps,psx,psy,d,u,v,&
!                       pd,pi,pm,os,om,px,py)
!   Input argument list:
!     im       integer - inner computational domain
!     ix       integer - maximum inner dimension
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
!     pi       real (km+1) interface pressure (Pa)
!     pm       real (km) mid-layer pressure (Pa)
!     om       real (km) vertical velocity (Pa/s)
!
! Attributes:
!   Language: Fortran 90
!
!$$$
    use sigio_module, only : sigio_modprd
    implicit none
    integer,                    intent(in)  :: im,ix,km,idvc,idsl,nvcoord,me
    real,                       intent(in)  :: vcoord(km+1,nvcoord)
    real,   dimension(ix),      intent(in)  :: ps,psx,psy
    real,   dimension(ix,km),   intent(in)  :: u,v,d
    real*8, dimension(ix,km+1), intent(out) :: pi
    real*8, dimension(ix,km),   intent(out) :: pm
    real,   dimension(ix,km),   intent(out) :: om
!   real*8, allocatable :: ps8(:), pm8(:,:), pd8(:,:),dpmdps(:,:),dpddps(:,:), &
!                          dpidps(:,:),pi8(:,:),vcoord8(:,:)
!   real,   allocatable :: os(:)
!   real,   allocatable :: pd(:,:),px(:,:), py(:,:), os(:)

!   real vgradpps

    real*8 ps8(ix),pm8(ix,km),pd8(ix,km),vcoord8(km+1,nvcoord)
    real*8 dpmdps(ix,km),dpddps(ix,km),dpidps(ix,km+1),pi8(ix,km+1)
    real vgradpps,pd(im,km),os(im)
!   real vgradpps,pd(im,km),px(im,km),py(im,km),os(im),tem
    integer i,k,iret,logk
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ps8     = ps
    vcoord8 = vcoord
    call sigio_modprd(im,ix,km,nvcoord,idvc,idsl,vcoord8,iret,                 &
                     ps=ps8,pd=pd8,dpddps=dpddps,pm=pm8,dpmdps=dpmdps)

!
!   if (me == 0) then
!     write(0,*)' pd8=',pd8(1,60:64)
!     write(0,*)' pm8=',pm8(1,60:64)
!    endif
!jw: has to be 8 real for wam     

!$omp parallel do private(i)
    do i=1,im
      pi8(i,1)    = ps(i)
      dpidps(i,1) = 1.
      os(i)       = 0
      pi(i,1)     = pi8(i,1)
    enddo
    do k=1,km
!$omp parallel do private(i)
      do i=1,im
        pi8(i,k+1)    = pi8(i,k)    - pd8(i,k)
        dpidps(i,k+1) = dpidps(i,k) - dpddps(i,k)
!       if(pi(i,8)<0.) then
!          print *,'in modstuff,pi8=',pi8(i,k),' i=',i,' k=',k,' me=',me
!       endif
        pi(i,k+1) = pi8(i,k+1)
        pm(i,k)   = pm8(i,k)
      enddo
    enddo
!
    do k=km,1,-1
!$omp parallel do private(i,vgradpps)
      do i=1,im
        vgradpps = (u(i,k)*psx(i) + v(i,k)*psy(i)) * ps(i)

        os(i)    = os(i) - vgradpps*(dpmdps(i,k)-dpidps(i,k+1))             &
                         - d(i,k)*(pm(i,k)-pi(i,k+1))

        om(i,k)  = os(i) + vgradpps*dpmdps(i,k)

        os(i)    = os(i) - vgradpps*(dpidps(i,k)-dpmdps(i,k))               &
                         - d(i,k)*(pi(i,k)-pm(i,k))
      enddo
    enddo
  end subroutine
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

subroutine mptgen(mpirank,mpisize,nd,jt1,jt2,j1,j2,jx,jm,jn)
!$$$  Subprogram documentation block
!
! Subprogram:    mptgen      Generate grid decomposition dimensions
!   Prgmmr: Iredell    Org: W/NP23      Date: 1999-02-12
!
! Abstract: This subprogram decomposes total dimensions of a problem
!   into smaller domains to be managed on a distributed memory system.
!   The last dimension given is decomposed first.  If more decompositions
!   are possible, the next to last dimension is decomposed next, and so on.
!   The transpositions between decompositions should be done by mptran*.
!
! Program history log:
!   1999-02-12  Mark Iredell
!
! Usage:    call mptgen(mpirank,mpisize,nd,jt1,jt2,j1,j2,jx,jm,jn)
!   Input argument list:
!     mpirank  integer(kint_mpi) rank of the process (from mpi_comm_rank)
!     mpisize  integer(kint_mpi) size of the process (from mpi_comm_size)
!     nd       integer(kint_mpi) number of dimensions to decompose
!     jt1      integer(kint_mpi) (nd) lower bounds of total dimensions
!     jt2      integer(kint_mpi) (nd) upper bounds of total dimensions
!   Output argument list:
!     j1       integer(kint_mpi) (nd) lower bounds of local decompositions
!     j2       integer(kint_mpi) (nd) upper bounds of local decompositions
!     jx       integer(kint_mpi) (nd) local size of decompositions
!     jm       integer(kint_mpi) (nd) maximum size of decompositions
!     jn       integer(kint_mpi) (nd) number of decompositions
!
! Attributes:
!   Language: Fortran 90
!
!$$$
  use machine,only:kint_mpi
  implicit none
  integer(kint_mpi),intent(in):: mpirank,mpisize,nd,jt1(nd),jt2(nd)
  integer(kint_mpi),intent(out):: j1(nd),j2(nd),jx(nd),jm(nd),jn(nd)
  integer msize,mrank,msn,mrn,n
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  msize=mpisize
  mrank=mpirank
  do n=nd,1,-1
    if(jt2(n).ge.jt1(n)) then
      jm(n)=(jt2(n)-jt1(n))/msize+1
      msn=max(msize/(jt2(n)-jt1(n)+1),1)
      if(n.eq.1) msn=1
      jn(n)=msize/msn
      mrn=mrank/msn
      j1(n)=min(jt1(n)+jm(n)*mrn,jt2(n)+1)
      j2(n)=min(jt1(n)+jm(n)*mrn+jm(n)-1,jt2(n))
      jx(n)=j2(n)-j1(n)+1
      msize=msn
      mrank=mod(mrank,msn)
      write(0,*)' mrank=',mrank,' j1=',j1(n),' j2=',j2(n),' jx=',jx(n),' jm=',jm(n)
    else
      jm(n)=0
      jn(n)=1
      j1(n)=jt1(n)
      j2(n)=jt2(n)
      jx(n)=0
    endif
  enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
end subroutine
!-------------------------------------------------------------------------------
subroutine mptranr4(mpicomm,mpisize,im,ida,idb,&
                    jm,jma,jmb,jda,km,kma,kmb,kdb,a,b,ta,tb)
!$$$  Subprogram documentation block
!
! Subprogram:    mptranr4    Transpose grid decompositions
!   Prgmmr: Iredell    Org: W/NP23      Date: 1999-02-12
!
! Abstract: This subprogram transposes an array of data from one
!   grid decomposition to another by using message passing.
!   The grid decompositions should be generated by mptgen.
!
! Program history log:
!   1999-02-12  Mark Iredell
!
! Usage:  call mptranr4(mpicomm,mpisize,im,ida,idb,&
!                       jm,jma,jmb,jda,km,kma,kmb,kdb,a,b)
!   Input argument list:
!     mpicomm  integer(kint_mpi) mpi communicator
!     mpisize  integer(kint_mpi) size of the process (from mpi_comm_size)
!     im       integer(kint_mpi) undecomposed range
!     ida      integer(kint_mpi) undecomposed input dimension
!     idb      integer(kint_mpi) undecomposed output dimension
!     jm       integer(kint_mpi) output grid decomposition size
!     jma      integer(kint_mpi) input grid undecomposed range
!     jmb      integer(kint_mpi) output grid decomposed range
!     jda      integer(kint_mpi) input grid undecomposed dimension
!     km       integer(kint_mpi) input grid decomposition size
!     kma      integer(kint_mpi) input grid decomposed range
!     kmb      integer(kint_mpi) output grid undecomposed range
!     kdb      integer(kint_mpi) output grid undecomposed dimension
!     a        real(4) (ida,jda,kma) input array
!   Output argument list:
!     b        real(4) (idb,kdb,jmb) output array
!     ta,tb    real(4) (im,jm,km,mpisize)  work arrays
!
! Subprograms called:
!   mpi_alltoall  mpi exchange of data between every process pair
!
! Remarks:
!   While this routine serves a wide variety of scalable transpose functions
!   for multidimensional grids,
!     (a) it does not work with nonrectanguloid grids;
!     (b) it does not do any load balancing;
!     (c) it does not do any communication hiding.
!
!   This subprogram must be used rather than mpi_alltoall
!   in any of the following cases:
!     (a) The undecomposed range is less than the respective dimension
!         (either im.lt.ida or im.lt.idb)
!     (b) The decomposition size is greater than one
!         (either km.gt.1 or jm.gt.1)
!     (c) The decomposed range is ever zero
!         (either kma.eq.0 or jmb.eq.0 for any process)
!     (d) The output grid range is not the full extent
!         (either kmb.lt.mpisize or kmb.lt.kda or jma.lt.mpisize or jma.lt.jda)
!   If none of these conditions apply, mpi_alltoall could be used directly
!   rather than this subprogram and would be more efficient.
!
!   Example 1.  Transpose a 1000 x 10000 matrix.
!
!!!   include 'mpif.h'                                     ! use mpi
!!!   parameter(jt=1000,kt=10000)                          ! set problem size
!!!   real,allocatable:: a(:,:),b(:,:)                     ! declare arrays
!!!   call mpi_init(ierr)                                  ! initialize mpi
!!!   call mpi_comm_rank(MPI_COMM_WORLD,mpirank,ierr)      ! get mpi rank
!!!   call mpi_comm_size(MPI_COMM_WORLD,mpisize,ierr)      ! get mpi size
!!!   call mptgen(mpirank,mpisize,1,1,jt,j1,j2,jx,jm,jn)   ! decompose output
!!!   call mptgen(mpirank,mpisize,1,1,kt,k1,k2,kx,km,kn)   ! decompose input
!!!   allocate(a(jt,k1:k2),b(kt,j1:j2))                    ! allocate arrays
!!!   a=reshape((/((j+k,j=1,jt),k=k1,k2)/),(/jt,k2-k1+1/)) ! initialize input
!!!   call mptranr4(MPI_COMM_WORLD,mpisize,1,1,1,          ! transpose arrays
!!!  &              jm,jt,j2-j1+1,jt,km,k2-k1+1,kt,kt,a,b)
!!!   print '(2i8,f16.1)',((k,j,b(k,j),k=2000,kt,2000),    ! print some values
!!!  &                    j=((j1-1)/200+1)*200,j2,200)
!!!   call mpi_finalize(ierr)                              ! finalize mpi
!!!   end
!
!   This transpose took 0.6 seconds on 4 2-way winterhawk nodes.
!   A 20000x10000 transpose took 3.4 seconds on 16 2-way winterhawk nodes.
!   Thus a transpose may take about 1 second for every 16 Mb per node.
!
! Attributes:
!   language: fortran 90
!
!$$$
  use machine,only:kint_mpi
  implicit none
  include 'mpif.h'
  integer(kint_mpi),intent(in):: mpicomm,mpisize
  integer(kint_mpi),intent(in):: im,ida,idb
  integer(kint_mpi),intent(in):: jm,jma,jmb,jda
  integer(kint_mpi),intent(in):: km,kma,kmb,kdb
  real(4),dimension(ida,jda,kma),intent(in):: a
  real(4),dimension(idb,kdb,jmb),intent(out):: b
  real(4),dimension(im,jm,km,mpisize),intent(inout):: ta,tb
  integer(4) jmb1(1),jmbf(mpisize),kma1(1),kmaf(mpisize)
  integer(kint_mpi)::i,j,k,l,ierr,ja,kb
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  jmb1(1) = jmb
  call mpi_allgather(jmb1,1,MPI_INTEGER4,jmbf,1,MPI_INTEGER4,mpicomm,ierr)
  kma1(1) = kma
  call mpi_allgather(kma1,1,MPI_INTEGER4,kmaf,1,MPI_INTEGER4,mpicomm,ierr)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  internally transpose input array
!$omp parallel do private(i,j,k,l,ja)
  do l=1,mpisize
    do k=1,kma
      do j=1,jm
        ja = j + sum(jmbf(1:l-1))
        if(ja <= jma) then
          do i=1,im
            ta(i,j,k,l) = a(i,ja,k)
          enddo
        endif
      enddo
    enddo
  enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  externally transpose data
  call mpi_alltoall(ta,im*jm*km,MPI_REAL4,tb,im*jm*km,MPI_REAL4,mpicomm,ierr)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  internally transpose output array
!$omp parallel do private(i,j,k,l,kb)
  do l=1,mpisize
    do k=1,km
      kb = k + sum(kmaf(1:l-1))
      if(kb <= kmb) then
        do j=1,jmb
          do i=1,im
            b(i,kb,j) = tb(i,j,k,l)
          enddo
        enddo
      endif
    enddo
  enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
end subroutine
!-----------------------------------------------------------------------
      subroutine trssc(jcap,nc,km,ntrac,idvc,idvm,idsl,nvcoord,vcoord,     &
                       cpi,idrt,lonb,latb,ijl,ijn,j1,j2,jc,chgq0,          &
                       szs,sps,st,sd,sz,sq,gfszs,gfsps,gfsp,gfsdp,         &
                       gfst,gfsu,gfsv,gfsq,gfsw)
!$$$  subprogram documentation block
!
! subprogram:    trssc       transform sigma spectral fields to grid
!   prgmmr: iredell          org: w/nmc23     date: 92-10-31
!
! abstract: transforms sigma spectral fields to grid and converts
!   log surface pressure to surface pressure and virtual temperature
!   to temperature.
!
! program history log:
!   91-10-31  mark iredell
!
! usage:    call trssc(jcap,nc,km,ntrac,idvm,
!    &                 idrt,lonb,latb,ijl,j1,j2,jc,
!    &                 szs,sps,st,sd,sz,sq,zs,ps,t,u,v,q)
!   input argument list:
!     jcap         integer spectral truncation
!     nc           integer first dimension (nc>=(jcap+1)*(jcap+2))
!     km           integer number of levels
!     ntrac        integer number of tracers
!     idvm         integer mass variable id
!     idrt         integer data representation type
!     lonb         integer number of longitudes
!     latb         integer number of latitudes
!     ijl          integer horizontal dimension
!     j1           integer first latitude
!     j2           integer last latitude
!     jc           integer number of cpus
!     szs          real (nc) orography
!     sps          real (nc) log surface pressure
!     st           real (nc,levs) virtual temperature
!     sd           real (nc,levs) divergence
!     sz           real (nc,levs) vorticity
!     sq           real (nc,levs*ntrac) tracers
!   output argument list:
!     zs           real (ijl) orography
!     ps           real (ijl) surface pressure
!     t            real (ijl,km) temperature
!     u            real (ijl,km) zonal wind
!     v            real (ijl,km) meridional wind
!     q            real (ijl,km*ntrac) tracers
!
! subprograms called:
!   sptran       perform a scalar spherical transform
!
! attributes:
!   language: fortran
!
!c$$$
      use gfsio_module
!      use gfsio_rst
      implicit none
      integer,intent(in)::jcap,nc,km,ntrac,idvc,idvm,idsl,nvcoord,idrt,lonb,latb
      integer,intent(in)::ijl,ijn,j1,j2,jc,chgq0
      real,intent(in):: szs(nc),sps(nc),st(nc,km),sd(nc,km),sz(nc,km),sq(nc,km*ntrac)
      real,intent(in):: cpi(0:ntrac)
      real*8,intent(in):: vcoord(km+1,nvcoord)
      real,dimension(ijn),intent(inout):: gfszs,gfsps
      real,dimension(ijn,km),intent(inout):: gfsp,gfsdp,gfst,gfsu,gfsv,gfsw
      real,dimension(ijn,km*ntrac),intent(inout):: gfsq
      real zs(ijl),ps(ijl),t(ijl,km),u(ijl,km),v(ijl,km),q(ijl,km*ntrac)
      real wi(ijl,km),pi(ijl,km),dpo(ijl,km)
      real tvcon,xcp,sumq
      integer thermodyn_id,jn,js,is,in
      integer jj,jjm,k,n,j,i,ij,lonb2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  spectral transforms
      if(j1==732)print*,'sample input to trssc= ',jcap,nc,km,ntrac, &
                          idvc,idvm,idsl,nvcoord,    &
                       idrt,lonb,latb,ijl,ijn,j1,j2,jc,chgq0
      lonb2=lonb*2
      ij=lonb2*(j2-j1+1)
      in=1
      is=1+lonb
      call sptran(0,jcap,idrt,lonb,latb,1,1,1,lonb2,lonb2,nc,ijl,           &
                  j1,j2,jc,szs,zs(in),zs(is),1)
      call sptran(0,jcap,idrt,lonb,latb,1,1,1,lonb2,lonb2,nc,ijl,           &
                  j1,j2,jc,sps,ps(in),ps(is),1)
      call sptran(0,jcap,idrt,lonb,latb,km,1,1,lonb2,lonb2,nc,ijl,          &
                  j1,j2,jc,st,t(in,1),t(is,1),1)
      call sptranv(0,jcap,idrt,lonb,latb,km,1,1,lonb2,lonb2,nc,ijl,         &
                   j1,j2,jc,sd,sz,u(in,1),u(is,1),v(in,1),v(is,1),1)
      call sptran(0,jcap,idrt,lonb,latb,km*ntrac,1,1,lonb2,lonb2,nc,ijl,    &
                  j1,j2,jc,sq,q(in,1),q(is,1),1)
      if(j1==732)then
       do k=1,km
        do i=1,ijl
	 if(t(i,k)>400. .or. t(i,k)<100.)print*,'bad T from sptran',i,k,t(i,k)
	 if(q(i,k)>1.)print*,'bad Q  from sptran',i,k,q(i,k)
	 if(q(i,2*k)>1.)print*,'bad Q  from sptran',i,k,q(i,2*k)
	 if(q(i,3*k)>1.)print*,'bad Q  from sptran',i,k,q(i,3*k)
	end do
       end do	 
      end if  
      select case(mod(idvm,10))
      case(0,1)
        do i=1,ij
          ps(i)=1.e3*exp(ps(i))
        enddo
      case(2)
        do i=1,ij
          ps(i)=1.e3*ps(i)
        enddo
      case default
        do i=1,ij
          ps(i)=1.e3*exp(ps(i))
        enddo
      end select
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      thermodyn_id=mod(idvm/10,10)
      if (thermodyn_id == 3) then
        do k=1,km
         do i=1,ij
            t(i,k) = t(i,k)/cpi(0)   ! enthalpy (cpt/cpd)
         end do
        end do
!
      endif

      call getomega(jcap,nc,km,idvc,idvm,idrt,idsl,                    &
        nvcoord,vcoord,lonb,latb,ijl,j1,j2,1,sd,sps,                   &
        ps,t,u,v,wi,pi,dpo)
      if(j1==732)then
       do k=1,km
        do i=1,ijl
	 if(t(i,k)>400. .or. t(i,k)<100.)print*,'bad T after getomega',i,k,t(i,k)
	 if(q(i,k)>1. )print*,'bad Q  after getomega',i,k,q(i,k)
	 if(q(i,2*k)>1. )print*,'bad Q  after getomega',i,2*k,q(i,2*k)
	end do
       end do	 
      end if  
      if(thermodyn_id /= 2)then
!  convert to surface pressure and temperature
         if (thermodyn_id == 3) then
           do k=1,km
            do i=1,ij
               xcp  = 0.0
               sumq = 0.0
               do n=1,ntrac
                 if( cpi(n) .ne. 0.0 ) then
                   xcp  = xcp  + cpi(n)*q(i,k+(n-1)*km)
                   sumq = sumq + q(i,k+(n-1)*km)
                 endif
               enddo
               t(i,k)  = t(i,k)/((1.-sumq)*cpi(0)+xcp)
            end do
           end do

          else 
           tvcon=(461.50/287.05-1.)
           t(:,:) = t(:,:)/(1.+tvcon*q(:,1:km))
          endif
      end if
      if(j1==732)then
       do k=1,km
        do i=1,ijl
	 if(t(i,k)>400. .or. t(i,k)<100.)print*,'bad T after Tv to T',i,k,t(i,k)
	 if(q(i,k)>1.)print*,'bad Q  after Tv to T',i,k,q(i,k)
	 if(q(i,2*k)>1. )print*,'bad Q  after Tv to T',i,k,q(i,2*k)
	end do
       end do	 
      end if  
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!----force tracers to be positive
      if (chgq0 == 1) q = max(q, 0.0)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  pass data to gfsdatao
      do j=1,j2-j1+1
        jn=j+j1-1
        js=latb+1-jn
        jn=(jn-1)*lonb
        js=(js-1)*lonb
        jj=j*lonb
        jjm=(j-1)*lonb
        do i=1,lonb
          gfszs(i+jn) = zs(i+jjm)
          gfsps(i+jn) = ps(i+jjm)
        enddo
        do i=1,lonb
          gfszs(i+js) = zs(i+jj)
          gfsps(i+js) = ps(i+jj)
        enddo
        do k=1,km
         do i=1,lonb
          gfsdp(i+jn,k) = dpo(i+jjm,k)
          gfsp(i+jn,k)  = pi(i+jjm,k)
          gfst(i+jn,k)  = t(i+jjm,k)
          gfsu(i+jn,k)  = u(i+jjm,k)
          gfsv(i+jn,k)  = v(i+jjm,k)
          gfsw(i+jn,k)  = wi(i+jjm,k)
         enddo
         do i=1,lonb
          gfsdp(i+js,k) = dpo(i+jj,k)
          gfsp(i+js,k)  = pi(i+jj,k)
          gfst(i+js,k)  = t(i+jj,k)
          gfsu(i+js,k)  = u(i+jj,k)
          gfsv(i+js,k)  = v(i+jj,k)
          gfsw(i+js,k)  = wi(i+jj,k)
         enddo
        enddo
        do k=1,km*ntrac
          do i=1,lonb
            gfsq(i+jn,k) = q(i+jjm,k)
          enddo
          do i=1,lonb
            gfsq(i+js,k) = q(i+jj,k)
          enddo
        enddo
      enddo
      return
      end
!-----------------------------------------------------------------------
      subroutine getomega(jcap,nc,km,idvc,idvm,idrt,idsl,nvcoord,vcoord,  &
            lonb,latb,ijn,j1,j2,jc,sd,sps,psi,ti,ui,vi,wi,pm,pd)
!!!!!
      use sigio_module, only : sigio_modprd
      implicit none

      integer,intent(in):: jcap,nc,km,idvc,idvm,idrt,idsl,nvcoord
      integer,intent(in):: lonb,latb,j1,j2,jc,ijn
      real*8,intent(in):: vcoord(km+1,nvcoord)
      real,intent(in):: sd(nc,km),sps(nc)
      real,intent(in):: psi(ijn),ti(ijn,km),ui(ijn,km),vi(ijn,km)
      real,intent(out):: wi(ijn,km),pm(ijn,km),pd(ijn,km)
      real :: pi(ijn,km+1)
      real :: os
      real*8 psi8(ijn),ti8(ijn,km),pm8(ijn,km),pd8(ijn,km)
      real*8 dpmdps(ijn,km),dpddps(ijn,km),dpidps(ijn,km+1),vgradp,psmean
      real di(ijn,km),psx(ijn),psy(ijn)
      integer k,i,ij,lonb2,iret,is,in
!----1. spectral transform
      lonb2=lonb*2
      ij=lonb2*(j2-j1+1)
      in=1
      is=1+lonb
      call sptrand(0,jcap,idrt,lonb,latb,1,1,1,lonb2,lonb2,nc,ijn,    &
           j1,j2,jc,sps,psmean,psx(in),psx(is),psy(in),psy(is),1)

      call sptran(0,jcap,idrt,lonb,latb,km,1,1,lonb2,lonb2,nc,ijn,    &
                  j1,j2,jc,sd,di(in,1),di(is,1),1)
      psi8=psi	
      ti8=ti  

      call sigio_modprd(ijn,ijn,km,nvcoord,idvc,idsl,vcoord,iret,     &
                   ps=psi8,t=ti8,pm=pm8,pd=pd8,dpmdps=dpmdps,dpddps=dpddps)
      pm=pm8
      pd=pd8		   

      select case(mod(idvm,10))
      case(0,1)
          continue
      case(2)
          do i=1,ijn
           psx(i)=psx(i)/(psi(i)*1.0e-3)
           psy(i)=psy(i)/(psi(i)*1.0e-3)
          enddo
      case default
          do i=1,ijn
           psx(i)=psx(i)/psi(i)
           psy(i)=psy(i)/psi(i)
          enddo
      end select

!----3.omeda from modstuff
      do i=1,ijn
       pi(i,1)=psi(i)
       dpidps(i,1)=1.
      enddo
      do k=1,km
       do i=1,ijn
         pi(i,k+1)=pi(i,k)-pd(i,k)
         dpidps(i,k+1)=dpidps(i,k)-dpddps(i,k)
       enddo
      enddo
      do i=1,ijn
       os=0.
       do k=km,1,-1
        vgradp=ui(i,k)*psx(i)+vi(i,k)*psy(i)
        os=os-vgradp*psi(i)*(dpmdps(i,k)-dpidps(i,k+1))-                 &
           di(i,k)*(pm(i,k)-pi(i,k+1))
        wi(i,k)=vgradp*psi(i)*dpmdps(i,k)+os
        os=os-vgradp*psi(i)*(dpidps(i,k)-dpmdps(i,k))-                   &
           di(i,k)*(pi(i,k)-pm(i,k))
       enddo
!
      enddo
!---
       return
       end subroutine 
