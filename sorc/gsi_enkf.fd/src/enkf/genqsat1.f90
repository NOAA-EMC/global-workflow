subroutine genqsat1(sph,qsat,ges_prsl,ges_tv,ice,npts,nlevs)
! this subroutine was extracted from the GSI version operational
! at NCEP in Dec. 2007. Only difference is that this version takes
! single precision 2d arrays instead of default real 3d arrays.
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genqsat
!   prgmmr: derber           org: np23                date: 1998-01-14
!
! abstract: obtain saturation specific humidity for given temperature.
!
! program history log:
!   1998-01-14  derber
!   1998-04-05  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1903-10-07  Wei Gu, bug fixes,if qs<0,then set qs=0; merge w/ GSI by R Todling
!   2003-12-23  kleist, use guess pressure, adapt module framework
!   2004-05-13  kleist, documentation
!   2004-06-03  treadon, replace ggrid_g3 array with ges_* arrays
!   2005-02-23  wu, output dlnesdtv
!   2005-11-21  kleist, derber  add dmax array to decouple moisture
!               from temp and pressure for questionable qsat
!   2006-02-02  treadon - rename prsl as ges_prsl
!   2006-09-18  derber - modify to limit saturated values near top
!   2006-11-22  derber - correct bug:  es<esmax should be es<=esmax
!
!   input argument list:
!     ges_prsl  - guess pressure (mb)
!     ges_tv    - guess virtual temp (K)
!     qsat      - guess specific humidity (in), saturation value (out)
!     ice       - logical flag:  T=include ice and ice-water effects,
!                 depending on t, in qsat calcuations.
!                 otherwise, compute qsat with respect to water surface
!
!   output argument list:
!     qsat      - saturation specific humidity (output)
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_double,i_kind,r_single,r_kind
  use constants, only: xai,tmix,xb,omeps,eps,xbi,one,zero,&
       xa,psat,ttp,fv
  implicit none

  logical,intent(in):: ice
  integer(i_kind), intent(in) :: npts,nlevs
  real(r_double),dimension(npts,nlevs),intent(out):: qsat
  real(r_single),intent(in),dimension(npts,nlevs) :: ges_prsl,ges_tv,sph

  integer(i_kind) k,i
  real(r_double) pw,tdry,tr,es
  real(r_double) w,onep2,esmax
  real(r_double),dimension(npts):: mint,estmax
  real(r_double),dimension(npts,nlevs):: ges_tsen
  integer(i_kind),dimension(npts):: lmint

  onep2 = 1.e2_r_double

  ! compute sensible from virtual temp.
  ges_tsen=ges_tv/(one+fv*sph)
  !print &
  !*,'ges_tsen',minval(ges_tsen),maxval(ges_tsen),minval(ges_prsl),maxval(ges_prsl),ice,psat

  lmint=1
  do i=1,npts
    mint(i)=340._r_double
  end do
  do k=1,nlevs
      do i=1,npts
        if((ges_prsl(i,k) < 30._r_double .and.  &
            ges_prsl(i,k) > 2._r_double) .and.  &
            ges_tsen(i,k) < mint(i))then
           lmint(i)=k
           mint(i)=ges_tsen(i,k)
         end if
      end do
  end do
  do i=1,npts
      tdry = mint(i)
      tr = ttp/tdry
      if (tdry >= ttp .or. .not. ice) then
        estmax(i) = psat * (tr**xa) * exp(xb*(one-tr))
      elseif (tdry < tmix) then
        estmax(i) = psat * (tr**xai) * exp(xbi*(one-tr))
      else
        w  = (tdry - tmix) / (ttp - tmix)
        estmax(i) =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
      endif
  end do
  if (ice) then
    do k = 1,nlevs
        do i = 1,npts

          pw = onep2*ges_prsl(i,k) ! convert to Pa from mb.
              
          tdry = ges_tsen(i,k)
          tr = ttp/tdry
          if (tdry >= ttp) then
            es = psat * (tr**xa) * exp(xb*(one-tr))
          elseif (tdry < tmix) then
            es = psat * (tr**xai) * exp(xbi*(one-tr))
          else
            w  = (tdry - tmix) / (ttp - tmix)
            es =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                  + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
          endif

          esmax = es
          if(lmint(i) < k)then
             esmax=0.1*pw
             esmax=min(esmax,estmax(i))
          end if

          if(es > esmax)then
            es = esmax
          end if
          qsat(i,k) = eps * es / (pw - omeps * es)
          qsat(i,k) = max(tiny(qsat(i,k)),qsat(i,k))

        end do
    end do

! Compute saturation values with respect to water surface
  else
    do k = 1,nlevs
        do i = 1,npts
              
          pw = onep2*ges_prsl(i,k)  ! convert to Pa from mb

          tdry = ges_tsen(i,k)
          tr = ttp/tdry
          es = psat * (tr**xa) * exp(xb*(one-tr))
          esmax = es
          if(lmint(i) < k)then
             esmax=0.1*pw
             esmax=min(esmax,estmax(i))
          end if
          if(es > esmax)then
            es = esmax
          end if
          qsat(i,k) = eps * es / (pw - omeps * es)
          qsat(i,k) = max(tiny(qsat(i,k)),qsat(i,k))

        end do
    end do
     
  endif   ! end if ice

  ! enforce min value.
  where (qsat < 1.e-6) qsat = 1.e-6
  
  return
end subroutine genqsat1
