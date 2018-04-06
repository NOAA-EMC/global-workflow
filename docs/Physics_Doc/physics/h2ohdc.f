      subroutine h2ohdc(ctheta,p0,mmr,grav,mu,h2ohr,lx)
c Subroutine to calculate H2O near-IR heating rates after C.D. Walshaw,
c see Fomichev and Shved (1988).
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c File history
c July 2009: Modified from
c       hh2oh.f
c for discrete differencing, upward pressure grid, making heating
c rate -> 0 between 7 and 12 scale heights (as recommended by Victor in
c July 2007). The heating rate is now calculated in W/kg.
c Apr 06 2012  Henry Juang, initial implement for nems
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Formal arguments
c     IN:
c ctheta - cosine of solar zenith angle
c p0 - model layer pressure (Pa) grid going up
c mmr - H2O MMR (relative units) on model pressure grid going up
c grav - gravity acceleration (m/s^2) on model pressure grid
c mu - atmospheric molecular mass (g/mol) on model pressure grid
c lx - array dimension
      integer,intent(in):: lx
      real,intent(in):: ctheta
      real,dimension(lx),intent(in):: p0,mmr,grav,mu

c     OUT:
c h2ohr - heating rates (W/kg) on model pressure grid
      real,dimension(lx),intent(out):: h2ohr

c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Internal parameters

c Log-pressure of the uppermost extent of the parameterization around
c xtop = 12 sh as recommended by Victor (July 2007), the corresponding
c number of starting model layer, and the fade-off factor between
c xbot = 7 and xtop.
c ***xbot and xtop are assumed to be inside model domain!***
      real,parameter:: xbot=7.,xtop=12.,rdx=1./(xtop-xbot)
      integer:: last
      real:: factor(lx)

c Specific H2O data
c h2omu - molecular mass (g/mol)
      real,parameter:: h2omu=18.015

c Parameterization data after C.D. Walshaw:
c - inverse reference pressure (1/Pa)
      real,parameter:: rpref=1./101325.
c - number of bands
      integer,parameter:: iband=8

c Optical data
c - solar band fluxes times reference bandwidth (W/m^2)
c - l band parameter (m^2/kg)
c - b band parameter (dimensionless)
      real,parameter,dimension(iband)::
     $ wfband=(/
     $     14.3, 14.42, 21.39, 25.84, 9.3526, 6.848, 4.85, 1.3986/),
     $ lband=
     $     (/.00952, .13, .0335, .769, 2.99, 16.7, .138, 29.2/),
c in Fomichev & Shved (1988) lband(7)=.137 but in Victor's code it's
c .138
     $ bband=(/.26, .27, .26, .258, .305, .312, .3, .33/)

c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Work space
      integer:: i,l
      real:: p0dp(lx),rodfac,u(lx),work(lx),zeta,wb(lx,iband)

c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Initialize heating rate
      h2ohr(:)=0.

      if(ctheta.gt.0.) then
c Calculate heating, otherwise do nothing.

c "Rodgers factor" to crudely account for sphericity
         rodfac=35./sqrt(1224.*ctheta**2+1.)

c Scan pressure grid (now every call in case the grid changes),
c find model fade-off layer xbot-xtop (assumed to be fully within model
c domain), where heating rate will be assumed linearly go to 0 and stay
c so above.
      work(:)=log(1e5/p0(:))
      do l=1,lx
         if(work(l).lt.xbot) then
            factor(l)=1.
         else
            i=l
            exit
         endif
      enddo
      do l=i,lx
         if(work(l).lt.xtop) then
            factor(l)=1.-rdx*(work(l)-xbot)
            last=l
         else
            factor(l)=0.
         endif
      enddo

c Begin with vertical-column H2O mass calculation for the entire column
         work(:)=mmr(:)/grav(:)

c Mass above the uppermost model layer assuming hydrostatics (diffusive
c equilibrium
         u(lx)=(mu(lx)/h2omu)*p0(lx)*work(lx)

c Integrate down
         do l=lx-1,1,-1
            u(l)=u(l+1)+.5*(work(l+1)+work(l))*(p0(l)-p0(l+1))
         enddo

c Above l=last the heating is zero
         work(last+1:lx)=0.

c For nonzero-heating layers calculate some more preliminaries
c In the top and bottom layers use one-sided differences
         work(last)=ctheta*grav(last)*factor(last)/
     $        (p0(last-1)-p0(last))
         work(1)=ctheta*grav(1)*factor(1)/(p0(1)-p0(2))

c In other layers use centered differences
         do l=2,last-1
            work(l)=ctheta*grav(l)*factor(l)/(p0(l-1)-p0(l+1))
         enddo

         do l=1,last

c Slant mass
            u(l)=rodfac*u(l)

c Normalize pressure
            p0dp(l)=p0(l)*rpref
         enddo

c Calculate band widths
         do i=1,iband
            do l=1,last
               zeta=(p0dp(l)**bband(i))*sqrt(lband(i)*u(l))
               if(zeta.le.1.) then
                  wb(l,i)=wfband(i)*zeta
               else
                  wb(l,i)=wfband(i)*(1.+log(zeta))
               endif
            enddo
         enddo

c Calculate heating rate (W/kg) where nonzero
         do i=1,iband

c In the top and bottom layers use one-sided differences
            h2ohr(last)=h2ohr(last)+
     $           work(last)*(wb(last-1,i)-wb(last,i))
            h2ohr(1)=h2ohr(1)+work(1)*(wb(1,i)-wb(2,i))

c In other layers use centered differences
            do l=2,last-1
               h2ohr(l)=h2ohr(l)+work(l)*(wb(l-1,i)-wb(l+1,i))
            enddo
         enddo
      endif

      end subroutine h2ohdc
