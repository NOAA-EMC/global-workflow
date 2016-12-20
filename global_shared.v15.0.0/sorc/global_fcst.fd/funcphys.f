!-------------------------------------------------------------------------------
module funcphys
!$$$  module documentation block
!
! module:    funcphys        api for basic thermodynamic physics
!   author: iredell          org: w/nx23     date: 1999-03-01
!
! abstract: this module provides an application program interface
!   for computing basic thermodynamic physics functions, in particular
!     (1) saturation vapor pressure as a function of temperature,
!     (2) dewpoint temperature as a function of vapor pressure,
!     (3) equivalent potential temperature as a function of temperature
!         and scaled pressure to the kappa power,
!     (4) temperature and specific humidity along a moist adiabat
!         as functions of equivalent potential temperature and
!         scaled pressure to the kappa power,
!     (5) scaled pressure to the kappa power as a function of pressure, and
!     (6) temperature at the lifting condensation level as a function
!         of temperature and dewpoint depression.
!   the entry points required to set up lookup tables start with a "g".
!   all the other entry points are functions starting with an "f" or
!   are subroutines starting with an "s".  these other functions and
!   subroutines are elemental; that is, they return a scalar if they
!   are passed only scalars, but they return an array if they are passed
!   an array.  these other functions and subroutines can be inlined, too.
!
! program history log:
!   1999-03-01  mark iredell
!   1999-10-15  mark iredell  si unit for pressure (pascals)
!   2001-02-26  mark iredell  ice phase changes of hong and moorthi
!
! public variables:
!   krealfp         integer parameter kind or length of reals (=kind_phys)
!
! public subprograms:
!   gpvsl            compute saturation vapor pressure over liquid table
!
!   fpvsl           elementally compute saturation vapor pressure over liquid
!     function result real(krealfp) saturation vapor pressure in pascals
!     t               real(krealfp) temperature in kelvin
!
!   fpvslq          elementally compute saturation vapor pressure over liquid
!     function result real(krealfp) saturation vapor pressure in pascals
!     t               real(krealfp) temperature in kelvin
!
!   fpvslx          elementally compute saturation vapor pressure over liquid
!     function result real(krealfp) saturation vapor pressure in pascals
!     t               real(krealfp) temperature in kelvin
!
!   gpvsi            compute saturation vapor pressure over ice table
!
!   fpvsi           elementally compute saturation vapor pressure over ice
!     function result real(krealfp) saturation vapor pressure in pascals
!     t               real(krealfp) temperature in kelvin
!
!   fpvsiq          elementally compute saturation vapor pressure over ice
!     function result real(krealfp) saturation vapor pressure in pascals
!     t               real(krealfp) temperature in kelvin
!
!   fpvsix          elementally compute saturation vapor pressure over ice
!     function result real(krealfp) saturation vapor pressure in pascals
!     t               real(krealfp) temperature in kelvin
!
!   gpvs            compute saturation vapor pressure table
!
!   fpvs            elementally compute saturation vapor pressure
!     function result real(krealfp) saturation vapor pressure in pascals
!     t               real(krealfp) temperature in kelvin
!
!   fpvsq           elementally compute saturation vapor pressure
!     function result real(krealfp) saturation vapor pressure in pascals
!     t               real(krealfp) temperature in kelvin
!
!   fpvsx           elementally compute saturation vapor pressure
!     function result real(krealfp) saturation vapor pressure in pascals
!     t               real(krealfp) temperature in kelvin
!
!   gtdpl           compute dewpoint temperature over liquid table
!
!   ftdpl           elementally compute dewpoint temperature over liquid
!     function result real(krealfp) dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   ftdplq          elementally compute dewpoint temperature over liquid
!     function result real(krealfp) dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   ftdplx          elementally compute dewpoint temperature over liquid
!     function result real(krealfp) dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   ftdplxg         elementally compute dewpoint temperature over liquid
!     function result real(krealfp) dewpoint temperature in kelvin
!     t               real(krealfp) guess dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   gtdpi           compute dewpoint temperature table over ice
!
!   ftdpi           elementally compute dewpoint temperature over ice
!     function result real(krealfp) dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   ftdpiq          elementally compute dewpoint temperature over ice
!     function result real(krealfp) dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   ftdpix          elementally compute dewpoint temperature over ice
!     function result real(krealfp) dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   ftdpixg         elementally compute dewpoint temperature over ice
!     function result real(krealfp) dewpoint temperature in kelvin
!     t               real(krealfp) guess dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   gtdp            compute dewpoint temperature table
!
!   ftdp            elementally compute dewpoint temperature
!     function result real(krealfp) dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   ftdpq           elementally compute dewpoint temperature
!     function result real(krealfp) dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   ftdpx           elementally compute dewpoint temperature
!     function result real(krealfp) dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   ftdpxg          elementally compute dewpoint temperature
!     function result real(krealfp) dewpoint temperature in kelvin
!     t               real(krealfp) guess dewpoint temperature in kelvin
!     pv              real(krealfp) vapor pressure in pascals
!
!   gthe            compute equivalent potential temperature table
!
!   fthe            elementally compute equivalent potential temperature
!     function result real(krealfp) equivalent potential temperature in kelvin
!     t               real(krealfp) lcl temperature in kelvin
!     pk              real(krealfp) lcl pressure over 1e5 pa to the kappa power
!
!   ftheq           elementally compute equivalent potential temperature
!     function result real(krealfp) equivalent potential temperature in kelvin
!     t               real(krealfp) lcl temperature in kelvin
!     pk              real(krealfp) lcl pressure over 1e5 pa to the kappa power
!
!   fthex           elementally compute equivalent potential temperature
!     function result real(krealfp) equivalent potential temperature in kelvin
!     t               real(krealfp) lcl temperature in kelvin
!     pk              real(krealfp) lcl pressure over 1e5 pa to the kappa power
!
!   gtma            compute moist adiabat tables
!
!   stma            elementally compute moist adiabat temperature and moisture
!     the             real(krealfp) equivalent potential temperature in kelvin
!     pk              real(krealfp) pressure over 1e5 pa to the kappa power
!     tma             real(krealfp) parcel temperature in kelvin
!     qma             real(krealfp) parcel specific humidity in kg/kg
!
!   stmaq           elementally compute moist adiabat temperature and moisture
!     the             real(krealfp) equivalent potential temperature in kelvin
!     pk              real(krealfp) pressure over 1e5 pa to the kappa power
!     tma             real(krealfp) parcel temperature in kelvin
!     qma             real(krealfp) parcel specific humidity in kg/kg
!
!   stmax           elementally compute moist adiabat temperature and moisture
!     the             real(krealfp) equivalent potential temperature in kelvin
!     pk              real(krealfp) pressure over 1e5 pa to the kappa power
!     tma             real(krealfp) parcel temperature in kelvin
!     qma             real(krealfp) parcel specific humidity in kg/kg
!
!   stmaxg          elementally compute moist adiabat temperature and moisture
!     tg              real(krealfp) guess parcel temperature in kelvin
!     the             real(krealfp) equivalent potential temperature in kelvin
!     pk              real(krealfp) pressure over 1e5 pa to the kappa power
!     tma             real(krealfp) parcel temperature in kelvin
!     qma             real(krealfp) parcel specific humidity in kg/kg
!
!   gpkap           compute pressure to the kappa table
!
!   fpkap           elementally raise pressure to the kappa power.
!     function result real(krealfp) p over 1e5 pa to the kappa power
!     p               real(krealfp) pressure in pascals
!
!   fpkapq          elementally raise pressure to the kappa power.
!     function result real(krealfp) p over 1e5 pa to the kappa power
!     p               real(krealfp) pressure in pascals
!
!   fpkapo          elementally raise pressure to the kappa power.
!     function result real(krealfp) p over 1e5 pa to the kappa power
!     p               real(krealfp) surface pressure in pascals
!
!   fpkapx          elementally raise pressure to the kappa power.
!     function result real(krealfp) p over 1e5 pa to the kappa power
!     p               real(krealfp) pressure in pascals
!
!   grkap           compute pressure to the 1/kappa table
!
!   frkap           elementally raise pressure to the 1/kappa power.
!     function result real(krealfp) pressure in pascals
!     pkap            real(krealfp) p over 1e5 pa to the 1/kappa power
!
!   frkapq          elementally raise pressure to the kappa power.
!     function result real(krealfp) pressure in pascals
!     pkap            real(krealfp) p over 1e5 pa to the kappa power
!
!   frkapx          elementally raise pressure to the kappa power.
!     function result real(krealfp) pressure in pascals
!     pkap            real(krealfp) p over 1e5 pa to the kappa power
!
!   gtlcl           compute lcl temperature table
!
!   ftlcl           elementally compute lcl temperature.
!     function result real(krealfp) temperature at the lcl in kelvin
!     t               real(krealfp) temperature in kelvin
!     tdpd            real(krealfp) dewpoint depression in kelvin
!
!   ftlclq          elementally compute lcl temperature.
!     function result real(krealfp) temperature at the lcl in kelvin
!     t               real(krealfp) temperature in kelvin
!     tdpd            real(krealfp) dewpoint depression in kelvin
!
!   ftlclo          elementally compute lcl temperature.
!     function result real(krealfp) temperature at the lcl in kelvin
!     t               real(krealfp) temperature in kelvin
!     tdpd            real(krealfp) dewpoint depression in kelvin
!
!   ftlclx          elementally compute lcl temperature.
!     function result real(krealfp) temperature at the lcl in kelvin
!     t               real(krealfp) temperature in kelvin
!     tdpd            real(krealfp) dewpoint depression in kelvin
!
!   gfuncphys       compute all physics function tables
!
! attributes:
!   language: fortran 90
!
!$$$
  use machine,only:kind_phys
  use physcons
  implicit none
  private
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! public variables
! integer,public,parameter:: krealfp=selected_real_kind(15,45)
  integer,public,parameter:: krealfp=kind_phys
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! private variables
  real(krealfp),parameter:: psatb=con_psat*1.e-5
  integer,parameter:: nxpvsl=7501
  real(krealfp) c1xpvsl,c2xpvsl,tbpvsl(nxpvsl)
  integer,parameter:: nxpvsi=7501
  real(krealfp) c1xpvsi,c2xpvsi,tbpvsi(nxpvsi)
  integer,parameter:: nxpvs=7501
  real(krealfp) c1xpvs,c2xpvs,tbpvs(nxpvs)
  integer,parameter:: nxtdpl=5001
  real(krealfp) c1xtdpl,c2xtdpl,tbtdpl(nxtdpl)
  integer,parameter:: nxtdpi=5001
  real(krealfp) c1xtdpi,c2xtdpi,tbtdpi(nxtdpi)
  integer,parameter:: nxtdp=5001
  real(krealfp) c1xtdp,c2xtdp,tbtdp(nxtdp)
  integer,parameter:: nxthe=241,nythe=151
  real(krealfp) c1xthe,c2xthe,c1ythe,c2ythe,tbthe(nxthe,nythe)
  integer,parameter:: nxma=151,nyma=121
  real(krealfp) c1xma,c2xma,c1yma,c2yma,tbtma(nxma,nyma),tbqma(nxma,nyma)
! integer,parameter:: nxpkap=5501
  integer,parameter:: nxpkap=11001
  real(krealfp) c1xpkap,c2xpkap,tbpkap(nxpkap)
  integer,parameter:: nxrkap=5501
  real(krealfp) c1xrkap,c2xrkap,tbrkap(nxrkap)
  integer,parameter:: nxtlcl=151,nytlcl=61
  real(krealfp) c1xtlcl,c2xtlcl,c1ytlcl,c2ytlcl,tbtlcl(nxtlcl,nytlcl)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! public subprograms
  public gpvsl,fpvsl,fpvslq,fpvslx
  public gpvsi,fpvsi,fpvsiq,fpvsix
  public gpvs,fpvs,fpvsq,fpvsx
  public gtdpl,ftdpl,ftdplq,ftdplx,ftdplxg
  public gtdpi,ftdpi,ftdpiq,ftdpix,ftdpixg
  public gtdp,ftdp,ftdpq,ftdpx,ftdpxg
  public gthe,fthe,ftheq,fthex
  public gtma,stma,stmaq,stmax,stmaxg
  public gpkap,fpkap,fpkapq,fpkapo,fpkapx
  public grkap,frkap,frkapq,frkapx
  public gtlcl,ftlcl,ftlclq,ftlclo,ftlclx
  public gfuncphys
contains
!-------------------------------------------------------------------------------
  subroutine gpvsl
!$$$     subprogram documentation block
!
! subprogram: gpvsl        compute saturation vapor pressure table over liquid
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: computes saturation vapor pressure table as a function of
!   temperature for the table lookup function fpvsl.
!   exact saturation vapor pressures are calculated in subprogram fpvslx.
!   the current implementation computes a table with a length
!   of 7501 for temperatures ranging from 180. to 330. kelvin.
!
! program history log:
!   91-05-07  iredell
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
!
! usage:  call gpvsl
!
! subprograms called:
!   (fpvslx)   inlinable function to compute saturation vapor pressure
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=180.0_krealfp
    xmax=330.0_krealfp
    xinc=(xmax-xmin)/(nxpvsl-1)
!   c1xpvsl=1.-xmin/xinc
    c2xpvsl=1./xinc
    c1xpvsl=1.-xmin*c2xpvsl
    do jx=1,nxpvsl
      x=xmin+(jx-1)*xinc
      t=x
      tbpvsl(jx)=fpvslx(t)
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental function fpvsl(t)
!$$$     subprogram documentation block
!
! subprogram: fpvsl        compute saturation vapor pressure over liquid
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute saturation vapor pressure from the temperature.
!   a linear interpolation is done between values in a lookup table
!   computed in gpvsl. see documentation for fpvslx for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is almost 6 decimal places.
!   on the cray, fpvsl is about 4 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
!
! usage:   pvsl=fpvsl(t)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!
!   output argument list:
!     fpvsl      real(krealfp) saturation vapor pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsl
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xpvsl+c2xpvsl*t,1._krealfp),real(nxpvsl,krealfp))
    jx=min(xj,nxpvsl-1._krealfp)
    fpvsl=tbpvsl(jx)+(xj-jx)*(tbpvsl(jx+1)-tbpvsl(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function fpvslq(t)
!$$$     subprogram documentation block
!
! subprogram: fpvslq       compute saturation vapor pressure over liquid
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute saturation vapor pressure from the temperature.
!   a quadratic interpolation is done between values in a lookup table
!   computed in gpvsl. see documentation for fpvslx for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is almost 9 decimal places.
!   on the cray, fpvslq is about 3 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             quadratic interpolation
! 1999-03-01  iredell             f90 module
!
! usage:   pvsl=fpvslq(t)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!
!   output argument list:
!     fpvslq     real(krealfp) saturation vapor pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvslq
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xpvsl+c2xpvsl*t,1._krealfp),real(nxpvsl,krealfp))
    jx=min(max(nint(xj),2),nxpvsl-1)
    dxj=xj-jx
    fj1=tbpvsl(jx-1)
    fj2=tbpvsl(jx)
    fj3=tbpvsl(jx+1)
    fpvslq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function fpvslx(t)
!$$$     subprogram documentation block
!
! subprogram: fpvslx       compute saturation vapor pressure over liquid
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute saturation vapor pressure from temperature.
!   the water model assumes a perfect gas, constant specific heats
!   for gas and liquid, and neglects the volume of the liquid.
!   the model does account for the variation of the latent heat
!   of condensation with temperature.  the ice option is not included.
!   the clausius-clapeyron equation is integrated from the triple point
!   to get the formula
!       pvsl=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
!
! usage:   pvsl=fpvslx(t)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!
!   output argument list:
!     fpvslx     real(krealfp) saturation vapor pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvslx
    real(krealfp),intent(in):: t
    real(krealfp),parameter:: dldt=con_cvap-con_cliq
    real(krealfp),parameter:: heat=con_hvap
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) tr
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    tr=con_ttp/t
    fpvslx=con_psat*(tr**xpona)*exp(xponb*(1.-tr))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gpvsi
!$$$     subprogram documentation block
!
! subprogram: gpvsi        compute saturation vapor pressure table over ice
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: computes saturation vapor pressure table as a function of
!   temperature for the table lookup function fpvsi.
!   exact saturation vapor pressures are calculated in subprogram fpvsix.
!   the current implementation computes a table with a length
!   of 7501 for temperatures ranging from 180. to 330. kelvin.
!
! program history log:
!   91-05-07  iredell
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:  call gpvsi
!
! subprograms called:
!   (fpvsix)   inlinable function to compute saturation vapor pressure
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=180.0_krealfp
    xmax=330.0_krealfp
    xinc=(xmax-xmin)/(nxpvsi-1)
!   c1xpvsi=1.-xmin/xinc
    c2xpvsi=1./xinc
    c1xpvsi=1.-xmin*c2xpvsi
    do jx=1,nxpvsi
      x=xmin+(jx-1)*xinc
      t=x
      tbpvsi(jx)=fpvsix(t)
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental function fpvsi(t)
!$$$     subprogram documentation block
!
! subprogram: fpvsi        compute saturation vapor pressure over ice
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute saturation vapor pressure from the temperature.
!   a linear interpolation is done between values in a lookup table
!   computed in gpvsi. see documentation for fpvsix for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is almost 6 decimal places.
!   on the cray, fpvsi is about 4 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   pvsi=fpvsi(t)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!
!   output argument list:
!     fpvsi      real(krealfp) saturation vapor pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsi
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xpvsi+c2xpvsi*t,1._krealfp),real(nxpvsi,krealfp))
    jx=min(xj,nxpvsi-1._krealfp)
    fpvsi=tbpvsi(jx)+(xj-jx)*(tbpvsi(jx+1)-tbpvsi(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function fpvsiq(t)
!$$$     subprogram documentation block
!
! subprogram: fpvsiq       compute saturation vapor pressure over ice
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute saturation vapor pressure from the temperature.
!   a quadratic interpolation is done between values in a lookup table
!   computed in gpvsi. see documentation for fpvsix for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is almost 9 decimal places.
!   on the cray, fpvsiq is about 3 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             quadratic interpolation
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   pvsi=fpvsiq(t)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!
!   output argument list:
!     fpvsiq     real(krealfp) saturation vapor pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsiq
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xpvsi+c2xpvsi*t,1._krealfp),real(nxpvsi,krealfp))
    jx=min(max(nint(xj),2),nxpvsi-1)
    dxj=xj-jx
    fj1=tbpvsi(jx-1)
    fj2=tbpvsi(jx)
    fj3=tbpvsi(jx+1)
    fpvsiq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function fpvsix(t)
!$$$     subprogram documentation block
!
! subprogram: fpvsix       compute saturation vapor pressure over ice
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute saturation vapor pressure from temperature.
!   the water model assumes a perfect gas, constant specific heats
!   for gas and ice, and neglects the volume of the ice.
!   the model does account for the variation of the latent heat
!   of condensation with temperature.  the liquid option is not included.
!   the clausius-clapeyron equation is integrated from the triple point
!   to get the formula
!       pvsi=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   pvsi=fpvsix(t)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!
!   output argument list:
!     fpvsix     real(krealfp) saturation vapor pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsix
    real(krealfp),intent(in):: t
    real(krealfp),parameter:: dldt=con_cvap-con_csol
    real(krealfp),parameter:: heat=con_hvap+con_hfus
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) tr
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    tr=con_ttp/t
    fpvsix=con_psat*(tr**xpona)*exp(xponb*(1.-tr))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gpvs
!$$$     subprogram documentation block
!
! subprogram: gpvs         compute saturation vapor pressure table
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: computes saturation vapor pressure table as a function of
!   temperature for the table lookup function fpvs.
!   exact saturation vapor pressures are calculated in subprogram fpvsx.
!   the current implementation computes a table with a length
!   of 7501 for temperatures ranging from 180. to 330. kelvin.
!
! program history log:
!   91-05-07  iredell
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:  call gpvs
!
! subprograms called:
!   (fpvsx)    inlinable function to compute saturation vapor pressure
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=180.0_krealfp
    xmax=330.0_krealfp
    xinc=(xmax-xmin)/(nxpvs-1)
!   c1xpvs=1.-xmin/xinc
    c2xpvs=1./xinc
    c1xpvs=1.-xmin*c2xpvs
    do jx=1,nxpvs
      x=xmin+(jx-1)*xinc
      t=x
      tbpvs(jx)=fpvsx(t)
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental function fpvs(t)
!$$$     subprogram documentation block
!
! subprogram: fpvs         compute saturation vapor pressure
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute saturation vapor pressure from the temperature.
!   a linear interpolation is done between values in a lookup table
!   computed in gpvs. see documentation for fpvsx for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is almost 6 decimal places.
!   on the cray, fpvs is about 4 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   pvs=fpvs(t)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!
!   output argument list:
!     fpvs       real(krealfp) saturation vapor pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvs
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xpvs+c2xpvs*t,1._krealfp),real(nxpvs,krealfp))
    jx=min(xj,nxpvs-1._krealfp)
    fpvs=tbpvs(jx)+(xj-jx)*(tbpvs(jx+1)-tbpvs(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function fpvsq(t)
!$$$     subprogram documentation block
!
! subprogram: fpvsq        compute saturation vapor pressure
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute saturation vapor pressure from the temperature.
!   a quadratic interpolation is done between values in a lookup table
!   computed in gpvs. see documentation for fpvsx for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is almost 9 decimal places.
!   on the cray, fpvsq is about 3 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             quadratic interpolation
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   pvs=fpvsq(t)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!
!   output argument list:
!     fpvsq      real(krealfp) saturation vapor pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsq
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xpvs+c2xpvs*t,1._krealfp),real(nxpvs,krealfp))
    jx=min(max(nint(xj),2),nxpvs-1)
    dxj=xj-jx
    fj1=tbpvs(jx-1)
    fj2=tbpvs(jx)
    fj3=tbpvs(jx+1)
    fpvsq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function fpvsx(t)
!$$$     subprogram documentation block
!
! subprogram: fpvsx        compute saturation vapor pressure
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute saturation vapor pressure from temperature.
!   the saturation vapor pressure over either liquid and ice is computed
!   over liquid for temperatures above the triple point,
!   over ice for temperatures 20 degress below the triple point,
!   and a linear combination of the two for temperatures in between.
!   the water model assumes a perfect gas, constant specific heats
!   for gas, liquid and ice, and neglects the volume of the condensate.
!   the model does account for the variation of the latent heat
!   of condensation and sublimation with temperature.
!   the clausius-clapeyron equation is integrated from the triple point
!   to get the formula
!       pvsl=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   the reference for this computation is emanuel(1994), pages 116-117.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   pvs=fpvsx(t)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!
!   output argument list:
!     fpvsx      real(krealfp) saturation vapor pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsx
    real(krealfp),intent(in):: t
    real(krealfp),parameter:: tliq=con_ttp
    real(krealfp),parameter:: tice=con_ttp-20.0
    real(krealfp),parameter:: dldtl=con_cvap-con_cliq
    real(krealfp),parameter:: heatl=con_hvap
    real(krealfp),parameter:: xponal=-dldtl/con_rv
    real(krealfp),parameter:: xponbl=-dldtl/con_rv+heatl/(con_rv*con_ttp)
    real(krealfp),parameter:: dldti=con_cvap-con_csol
    real(krealfp),parameter:: heati=con_hvap+con_hfus
    real(krealfp),parameter:: xponai=-dldti/con_rv
    real(krealfp),parameter:: xponbi=-dldti/con_rv+heati/(con_rv*con_ttp)
    real(krealfp) tr,w,pvl,pvi
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    tr=con_ttp/t
    if(t.ge.tliq) then
      fpvsx=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
    elseif(t.lt.tice) then
      fpvsx=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
    else
      w=(t-tice)/(tliq-tice)
      pvl=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
      pvi=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
      fpvsx=w*pvl+(1.-w)*pvi
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gtdpl
!$$$     subprogram documentation block
!
! subprogram: gtdpl        compute dewpoint temperature over liquid table
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute dewpoint temperature table as a function of
!   vapor pressure for inlinable function ftdpl.
!   exact dewpoint temperatures are calculated in subprogram ftdplxg.
!   the current implementation computes a table with a length
!   of 5001 for vapor pressures ranging from 1 to 10001 pascals
!   giving a dewpoint temperature range of 208 to 319 kelvin.
!
! program history log:
!   91-05-07  iredell
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
!
! usage:  call gtdpl
!
! subprograms called:
!   (ftdplxg)  inlinable function to compute dewpoint temperature over liquid
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,t,x,pv
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=1
    xmax=10001
    xinc=(xmax-xmin)/(nxtdpl-1)
    c1xtdpl=1.-xmin/xinc
    c2xtdpl=1./xinc
    t=208.0
    do jx=1,nxtdpl
      x=xmin+(jx-1)*xinc
      pv=x
      t=ftdplxg(t,pv)
      tbtdpl(jx)=t
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental function ftdpl(pv)
!$$$     subprogram documentation block
!
! subprogram: ftdpl        compute dewpoint temperature over liquid
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute dewpoint temperature from vapor pressure.
!   a linear interpolation is done between values in a lookup table
!   computed in gtdpl. see documentation for ftdplxg for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.0005 kelvin
!   for dewpoint temperatures greater than 250 kelvin,
!   but decreases to 0.02 kelvin for a dewpoint around 230 kelvin.
!   on the cray, ftdpl is about 75 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
!
! usage:   tdpl=ftdpl(pv)
!
!   input argument list:
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdpl      real(krealfp) dewpoint temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpl
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xtdpl+c2xtdpl*pv,1._krealfp),real(nxtdpl,krealfp))
    jx=min(xj,nxtdpl-1._krealfp)
    ftdpl=tbtdpl(jx)+(xj-jx)*(tbtdpl(jx+1)-tbtdpl(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftdplq(pv)
!$$$     subprogram documentation block
!
! subprogram: ftdplq       compute dewpoint temperature over liquid
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute dewpoint temperature from vapor pressure.
!   a quadratic interpolation is done between values in a lookup table
!   computed in gtdpl. see documentation for ftdplxg for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.00001 kelvin
!   for dewpoint temperatures greater than 250 kelvin,
!   but decreases to 0.002 kelvin for a dewpoint around 230 kelvin.
!   on the cray, ftdplq is about 60 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             quadratic interpolation
! 1999-03-01  iredell             f90 module
!
! usage:   tdpl=ftdplq(pv)
!
!   input argument list:
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdplq     real(krealfp) dewpoint temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdplq
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xtdpl+c2xtdpl*pv,1._krealfp),real(nxtdpl,krealfp))
    jx=min(max(nint(xj),2),nxtdpl-1)
    dxj=xj-jx
    fj1=tbtdpl(jx-1)
    fj2=tbtdpl(jx)
    fj3=tbtdpl(jx+1)
    ftdplq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftdplx(pv)
!$$$     subprogram documentation block
!
! subprogram: ftdplx       compute dewpoint temperature over liquid
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute dewpoint temperature from vapor pressure.
!   an approximate dewpoint temperature for function ftdplxg
!   is obtained using ftdpl so gtdpl must be already called.
!   see documentation for ftdplxg for details.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
!
! usage:   tdpl=ftdplx(pv)
!
!   input argument list:
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdplx     real(krealfp) dewpoint temperature in kelvin
!
! subprograms called:
!   (ftdpl)    inlinable function to compute dewpoint temperature over liquid
!   (ftdplxg)  inlinable function to compute dewpoint temperature over liquid
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdplx
    real(krealfp),intent(in):: pv
    real(krealfp) tg
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    tg=ftdpl(pv)
    ftdplx=ftdplxg(tg,pv)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftdplxg(tg,pv)
!$$$     subprogram documentation block
!
! subprogram: ftdplxg      compute dewpoint temperature over liquid
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute dewpoint temperature from vapor pressure.
!   a guess dewpoint temperature must be provided.
!   the water model assumes a perfect gas, constant specific heats
!   for gas and liquid, and neglects the volume of the liquid.
!   the model does account for the variation of the latent heat
!   of condensation with temperature.  the ice option is not included.
!   the clausius-clapeyron equation is integrated from the triple point
!   to get the formula
!       pvs=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   the formula is inverted by iterating newtonian approximations
!   for each pvs until t is found to within 1.e-6 kelvin.
!   this function can be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
!
! usage:   tdpl=ftdplxg(tg,pv)
!
!   input argument list:
!     tg         real(krealfp) guess dewpoint temperature in kelvin
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdplxg    real(krealfp) dewpoint temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdplxg
    real(krealfp),intent(in):: tg,pv
    real(krealfp),parameter:: terrm=1.e-6
    real(krealfp),parameter:: dldt=con_cvap-con_cliq
    real(krealfp),parameter:: heat=con_hvap
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) t,tr,pvt,el,dpvt,terr
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    t=tg
    do i=1,100
      tr=con_ttp/t
      pvt=con_psat*(tr**xpona)*exp(xponb*(1.-tr))
      el=heat+dldt*(t-con_ttp)
      dpvt=el*pvt/(con_rv*t**2)
      terr=(pvt-pv)/dpvt
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    ftdplxg=t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gtdpi
!$$$     subprogram documentation block
!
! subprogram: gtdpi        compute dewpoint temperature over ice table
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute dewpoint temperature table as a function of
!   vapor pressure for inlinable function ftdpi.
!   exact dewpoint temperatures are calculated in subprogram ftdpixg.
!   the current implementation computes a table with a length
!   of 5001 for vapor pressures ranging from 0.1 to 1000.1 pascals
!   giving a dewpoint temperature range of 197 to 279 kelvin.
!
! program history log:
!   91-05-07  iredell
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:  call gtdpi
!
! subprograms called:
!   (ftdpixg)  inlinable function to compute dewpoint temperature over ice
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,t,x,pv
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=0.1
    xmax=1000.1
    xinc=(xmax-xmin)/(nxtdpi-1)
    c1xtdpi=1.-xmin/xinc
    c2xtdpi=1./xinc
    t=197.0
    do jx=1,nxtdpi
      x=xmin+(jx-1)*xinc
      pv=x
      t=ftdpixg(t,pv)
      tbtdpi(jx)=t
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental function ftdpi(pv)
!$$$     subprogram documentation block
!
! subprogram: ftdpi        compute dewpoint temperature over ice
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute dewpoint temperature from vapor pressure.
!   a linear interpolation is done between values in a lookup table
!   computed in gtdpi. see documentation for ftdpixg for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.0005 kelvin
!   for dewpoint temperatures greater than 250 kelvin,
!   but decreases to 0.02 kelvin for a dewpoint around 230 kelvin.
!   on the cray, ftdpi is about 75 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   tdpi=ftdpi(pv)
!
!   input argument list:
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdpi      real(krealfp) dewpoint temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpi
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xtdpi+c2xtdpi*pv,1._krealfp),real(nxtdpi,krealfp))
    jx=min(xj,nxtdpi-1._krealfp)
    ftdpi=tbtdpi(jx)+(xj-jx)*(tbtdpi(jx+1)-tbtdpi(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftdpiq(pv)
!$$$     subprogram documentation block
!
! subprogram: ftdpiq       compute dewpoint temperature over ice
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute dewpoint temperature from vapor pressure.
!   a quadratic interpolation is done between values in a lookup table
!   computed in gtdpi. see documentation for ftdpixg for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.00001 kelvin
!   for dewpoint temperatures greater than 250 kelvin,
!   but decreases to 0.002 kelvin for a dewpoint around 230 kelvin.
!   on the cray, ftdpiq is about 60 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             quadratic interpolation
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   tdpi=ftdpiq(pv)
!
!   input argument list:
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdpiq     real(krealfp) dewpoint temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpiq
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xtdpi+c2xtdpi*pv,1._krealfp),real(nxtdpi,krealfp))
    jx=min(max(nint(xj),2),nxtdpi-1)
    dxj=xj-jx
    fj1=tbtdpi(jx-1)
    fj2=tbtdpi(jx)
    fj3=tbtdpi(jx+1)
    ftdpiq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftdpix(pv)
!$$$     subprogram documentation block
!
! subprogram: ftdpix       compute dewpoint temperature over ice
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute dewpoint temperature from vapor pressure.
!   an approximate dewpoint temperature for function ftdpixg
!   is obtained using ftdpi so gtdpi must be already called.
!   see documentation for ftdpixg for details.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   tdpi=ftdpix(pv)
!
!   input argument list:
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdpix     real(krealfp) dewpoint temperature in kelvin
!
! subprograms called:
!   (ftdpi)    inlinable function to compute dewpoint temperature over ice
!   (ftdpixg)  inlinable function to compute dewpoint temperature over ice
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpix
    real(krealfp),intent(in):: pv
    real(krealfp) tg
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    tg=ftdpi(pv)
    ftdpix=ftdpixg(tg,pv)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftdpixg(tg,pv)
!$$$     subprogram documentation block
!
! subprogram: ftdpixg      compute dewpoint temperature over ice
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute dewpoint temperature from vapor pressure.
!   a guess dewpoint temperature must be provided.
!   the water model assumes a perfect gas, constant specific heats
!   for gas and ice, and neglects the volume of the ice.
!   the model does account for the variation of the latent heat
!   of sublimation with temperature.  the liquid option is not included.
!   the clausius-clapeyron equation is integrated from the triple point
!   to get the formula
!       pvs=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   the formula is inverted by iterating newtonian approximations
!   for each pvs until t is found to within 1.e-6 kelvin.
!   this function can be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   tdpi=ftdpixg(tg,pv)
!
!   input argument list:
!     tg         real(krealfp) guess dewpoint temperature in kelvin
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdpixg    real(krealfp) dewpoint temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpixg
    real(krealfp),intent(in):: tg,pv
    real(krealfp),parameter:: terrm=1.e-6
    real(krealfp),parameter:: dldt=con_cvap-con_csol
    real(krealfp),parameter:: heat=con_hvap+con_hfus
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) t,tr,pvt,el,dpvt,terr
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    t=tg
    do i=1,100
      tr=con_ttp/t
      pvt=con_psat*(tr**xpona)*exp(xponb*(1.-tr))
      el=heat+dldt*(t-con_ttp)
      dpvt=el*pvt/(con_rv*t**2)
      terr=(pvt-pv)/dpvt
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    ftdpixg=t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gtdp
!$$$     subprogram documentation block
!
! subprogram: gtdp         compute dewpoint temperature table
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute dewpoint temperature table as a function of
!   vapor pressure for inlinable function ftdp.
!   exact dewpoint temperatures are calculated in subprogram ftdpxg.
!   the current implementation computes a table with a length
!   of 5001 for vapor pressures ranging from 0.5 to 1000.5 pascals
!   giving a dewpoint temperature range of 208 to 319 kelvin.
!
! program history log:
!   91-05-07  iredell
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:  call gtdp
!
! subprograms called:
!   (ftdpxg)   inlinable function to compute dewpoint temperature
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,t,x,pv
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=0.5
    xmax=10000.5
    xinc=(xmax-xmin)/(nxtdp-1)
    c1xtdp=1.-xmin/xinc
    c2xtdp=1./xinc
    t=208.0
    do jx=1,nxtdp
      x=xmin+(jx-1)*xinc
      pv=x
      t=ftdpxg(t,pv)
      tbtdp(jx)=t
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental function ftdp(pv)
!$$$     subprogram documentation block
!
! subprogram: ftdp         compute dewpoint temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute dewpoint temperature from vapor pressure.
!   a linear interpolation is done between values in a lookup table
!   computed in gtdp. see documentation for ftdpxg for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.0005 kelvin
!   for dewpoint temperatures greater than 250 kelvin,
!   but decreases to 0.02 kelvin for a dewpoint around 230 kelvin.
!   on the cray, ftdp is about 75 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   tdp=ftdp(pv)
!
!   input argument list:
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdp       real(krealfp) dewpoint temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdp
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xtdp+c2xtdp*pv,1._krealfp),real(nxtdp,krealfp))
    jx=min(xj,nxtdp-1._krealfp)
    ftdp=tbtdp(jx)+(xj-jx)*(tbtdp(jx+1)-tbtdp(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftdpq(pv)
!$$$     subprogram documentation block
!
! subprogram: ftdpq        compute dewpoint temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute dewpoint temperature from vapor pressure.
!   a quadratic interpolation is done between values in a lookup table
!   computed in gtdp. see documentation for ftdpxg for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.00001 kelvin
!   for dewpoint temperatures greater than 250 kelvin,
!   but decreases to 0.002 kelvin for a dewpoint around 230 kelvin.
!   on the cray, ftdpq is about 60 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             quadratic interpolation
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   tdp=ftdpq(pv)
!
!   input argument list:
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdpq      real(krealfp) dewpoint temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpq
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xtdp+c2xtdp*pv,1._krealfp),real(nxtdp,krealfp))
    jx=min(max(nint(xj),2),nxtdp-1)
    dxj=xj-jx
    fj1=tbtdp(jx-1)
    fj2=tbtdp(jx)
    fj3=tbtdp(jx+1)
    ftdpq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftdpx(pv)
!$$$     subprogram documentation block
!
! subprogram: ftdpx        compute dewpoint temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute dewpoint temperature from vapor pressure.
!   an approximate dewpoint temperature for function ftdpxg
!   is obtained using ftdp so gtdp must be already called.
!   see documentation for ftdpxg for details.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   tdp=ftdpx(pv)
!
!   input argument list:
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdpx      real(krealfp) dewpoint temperature in kelvin
!
! subprograms called:
!   (ftdp)     inlinable function to compute dewpoint temperature
!   (ftdpxg)   inlinable function to compute dewpoint temperature
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpx
    real(krealfp),intent(in):: pv
    real(krealfp) tg
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    tg=ftdp(pv)
    ftdpx=ftdpxg(tg,pv)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftdpxg(tg,pv)
!$$$     subprogram documentation block
!
! subprogram: ftdpxg       compute dewpoint temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute dewpoint temperature from vapor pressure.
!   a guess dewpoint temperature must be provided.
!   the saturation vapor pressure over either liquid and ice is computed
!   over liquid for temperatures above the triple point,
!   over ice for temperatures 20 degress below the triple point,
!   and a linear combination of the two for temperatures in between.
!   the water model assumes a perfect gas, constant specific heats
!   for gas, liquid and ice, and neglects the volume of the condensate.
!   the model does account for the variation of the latent heat
!   of condensation and sublimation with temperature.
!   the clausius-clapeyron equation is integrated from the triple point
!   to get the formula
!       pvsl=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   the reference for this decision is emanuel(1994), pages 116-117.
!   the formula is inverted by iterating newtonian approximations
!   for each pvs until t is found to within 1.e-6 kelvin.
!   this function can be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
! 2001-02-26  iredell             ice phase
!
! usage:   tdp=ftdpxg(tg,pv)
!
!   input argument list:
!     tg         real(krealfp) guess dewpoint temperature in kelvin
!     pv         real(krealfp) vapor pressure in pascals
!
!   output argument list:
!     ftdpxg     real(krealfp) dewpoint temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpxg
    real(krealfp),intent(in):: tg,pv
    real(krealfp),parameter:: terrm=1.e-6
    real(krealfp),parameter:: tliq=con_ttp
    real(krealfp),parameter:: tice=con_ttp-20.0
    real(krealfp),parameter:: dldtl=con_cvap-con_cliq
    real(krealfp),parameter:: heatl=con_hvap
    real(krealfp),parameter:: xponal=-dldtl/con_rv
    real(krealfp),parameter:: xponbl=-dldtl/con_rv+heatl/(con_rv*con_ttp)
    real(krealfp),parameter:: dldti=con_cvap-con_csol
    real(krealfp),parameter:: heati=con_hvap+con_hfus
    real(krealfp),parameter:: xponai=-dldti/con_rv
    real(krealfp),parameter:: xponbi=-dldti/con_rv+heati/(con_rv*con_ttp)
    real(krealfp) t,tr,w,pvtl,pvti,pvt,ell,eli,el,dpvt,terr
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    t=tg
    do i=1,100
      tr=con_ttp/t
      if(t.ge.tliq) then
        pvt=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
        el=heatl+dldtl*(t-con_ttp)
        dpvt=el*pvt/(con_rv*t**2)
      elseif(t.lt.tice) then
        pvt=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
        el=heati+dldti*(t-con_ttp)
        dpvt=el*pvt/(con_rv*t**2)
      else
        w=(t-tice)/(tliq-tice)
        pvtl=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
        pvti=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
        pvt=w*pvtl+(1.-w)*pvti
        ell=heatl+dldtl*(t-con_ttp)
        eli=heati+dldti*(t-con_ttp)
        dpvt=(w*ell*pvtl+(1.-w)*eli*pvti)/(con_rv*t**2)
      endif
      terr=(pvt-pv)/dpvt
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    ftdpxg=t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gthe
!$$$     subprogram documentation block
!
! subprogram: gthe        compute equivalent potential temperature table
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute equivalent potential temperature table
!   as a function of lcl temperature and pressure over 1e5 pa
!   to the kappa power for function fthe.
!   equivalent potential temperatures are calculated in subprogram fthex
!   the current implementation computes a table with a first dimension
!   of 241 for temperatures ranging from 183.16 to 303.16 kelvin
!   and a second dimension of 151 for pressure over 1e5 pa
!   to the kappa power ranging from 0.04**rocp to 1.10**rocp.
!
! program history log:
!   91-05-07  iredell
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
!
! usage:  call gthe
!
! subprograms called:
!   (fthex)    inlinable function to compute equiv. pot. temperature
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx,jy
    real(krealfp) xmin,xmax,ymin,ymax,xinc,yinc,x,y,pk,t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=con_ttp-90._krealfp
    xmax=con_ttp+30._krealfp
    ymin=0.04_krealfp**con_rocp
    ymax=1.10_krealfp**con_rocp
    xinc=(xmax-xmin)/(nxthe-1)
    c1xthe=1.-xmin/xinc
    c2xthe=1./xinc
    yinc=(ymax-ymin)/(nythe-1)
    c1ythe=1.-ymin/yinc
    c2ythe=1./yinc
    do jy=1,nythe
      y=ymin+(jy-1)*yinc
      pk=y
      do jx=1,nxthe
        x=xmin+(jx-1)*xinc
        t=x
        tbthe(jx,jy)=fthex(t,pk)
      enddo
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental function fthe(t,pk)
!$$$     subprogram documentation block
!
! subprogram: fthe         compute equivalent potential temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute equivalent potential temperature at the lcl
!   from temperature and pressure over 1e5 pa to the kappa power.
!   a bilinear interpolation is done between values in a lookup table
!   computed in gthe. see documentation for fthex for details.
!   input values outside table range are reset to table extrema,
!   except zero is returned for too cold or high lcls.
!   the interpolation accuracy is better than 0.01 kelvin.
!   on the cray, fthe is almost 6 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
!
! usage:   the=fthe(t,pk)
!
!   input argument list:
!     t          real(krealfp) lcl temperature in kelvin
!     pk         real(krealfp) lcl pressure over 1e5 pa to the kappa power
!
!   output argument list:
!     fthe       real(krealfp) equivalent potential temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fthe
    real(krealfp),intent(in):: t,pk
    integer jx,jy
    real(krealfp) xj,yj,ftx1,ftx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(c1xthe+c2xthe*t,real(nxthe,krealfp))
    yj=min(c1ythe+c2ythe*pk,real(nythe,krealfp))
    if(xj.ge.1..and.yj.ge.1.) then
      jx=min(xj,nxthe-1._krealfp)
      jy=min(yj,nythe-1._krealfp)
      ftx1=tbthe(jx,jy)+(xj-jx)*(tbthe(jx+1,jy)-tbthe(jx,jy))
      ftx2=tbthe(jx,jy+1)+(xj-jx)*(tbthe(jx+1,jy+1)-tbthe(jx,jy+1))
      fthe=ftx1+(yj-jy)*(ftx2-ftx1)
    else
      fthe=0.
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftheq(t,pk)
!$$$     subprogram documentation block
!
! subprogram: ftheq        compute equivalent potential temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute equivalent potential temperature at the lcl
!   from temperature and pressure over 1e5 pa to the kappa power.
!   a biquadratic interpolation is done between values in a lookup table
!   computed in gthe. see documentation for fthex for details.
!   input values outside table range are reset to table extrema,
!   except zero is returned for too cold or high lcls.
!   the interpolation accuracy is better than 0.0002 kelvin.
!   on the cray, ftheq is almost 3 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             quadratic interpolation
! 1999-03-01  iredell             f90 module
!
! usage:   the=ftheq(t,pk)
!
!   input argument list:
!     t          real(krealfp) lcl temperature in kelvin
!     pk         real(krealfp) lcl pressure over 1e5 pa to the kappa power
!
!   output argument list:
!     ftheq      real(krealfp) equivalent potential temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftheq
    real(krealfp),intent(in):: t,pk
    integer jx,jy
    real(krealfp) xj,yj,dxj,dyj
    real(krealfp) ft11,ft12,ft13,ft21,ft22,ft23,ft31,ft32,ft33
    real(krealfp) ftx1,ftx2,ftx3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(c1xthe+c2xthe*t,real(nxthe,krealfp))
    yj=min(c1ythe+c2ythe*pk,real(nythe,krealfp))
    if(xj.ge.1..and.yj.ge.1.) then
      jx=min(max(nint(xj),2),nxthe-1)
      jy=min(max(nint(yj),2),nythe-1)
      dxj=xj-jx
      dyj=yj-jy
      ft11=tbthe(jx-1,jy-1)
      ft12=tbthe(jx-1,jy)
      ft13=tbthe(jx-1,jy+1)
      ft21=tbthe(jx,jy-1)
      ft22=tbthe(jx,jy)
      ft23=tbthe(jx,jy+1)
      ft31=tbthe(jx+1,jy-1)
      ft32=tbthe(jx+1,jy)
      ft33=tbthe(jx+1,jy+1)
      ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
      ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
      ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
      ftheq=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2
    else
      ftheq=0.
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function fthex(t,pk)
            function fthex(t,pk)
!$$$     subprogram documentation block
!
! subprogram: fthex        compute equivalent potential temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute equivalent potential temperature at the lcl
!   from temperature and pressure over 1e5 pa to the kappa power.
!   equivalent potential temperature is constant for a saturated parcel
!   rising adiabatically up a moist adiabat when the heat and mass
!   of the condensed water are neglected.  ice is also neglected.
!   the formula for equivalent potential temperature (holton) is
!       the=t*(pd**(-rocp))*exp(el*eps*pv/(cp*t*pd))
!   where t is the temperature, pv is the saturated vapor pressure,
!   pd is the dry pressure p-pv, el is the temperature dependent
!   latent heat of condensation hvap+dldt*(t-ttp), and other values
!   are physical constants defined in parameter statements in the code.
!   zero is returned if the input values make saturation impossible.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
!
! usage:   the=fthex(t,pk)
!
!   input argument list:
!     t          real(krealfp) lcl temperature in kelvin
!     pk         real(krealfp) lcl pressure over 1e5 pa to the kappa power
!
!   output argument list:
!     fthex      real(krealfp) equivalent potential temperature in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fthex
    real(krealfp),intent(in):: t,pk
    real(krealfp) p,tr,pv,pd,el,expo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    p=pk**con_cpor
    tr=con_ttp/t
    pv=psatb*(tr**con_xpona)*exp(con_xponb*(1.-tr))
    pd=p-pv
    if(pd.gt.pv) then
      el=con_hvap+con_dldt*(t-con_ttp)
      expo=el*con_eps*pv/(con_cp*t*pd)
      fthex=t*pd**(-con_rocp)*exp(expo)
    else
      fthex=0.
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gtma
!$$$     subprogram documentation block
!
! subprogram: gtma         compute moist adiabat tables
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute temperature and specific humidity tables
!   as a function of equivalent potential temperature and
!   pressure over 1e5 pa to the kappa power for subprogram stma.
!   exact parcel temperatures are calculated in subprogram stmaxg.
!   the current implementation computes a table with a first dimension
!   of 151 for equivalent potential temperatures ranging from 200 to 500
!   kelvin and a second dimension of 121 for pressure over 1e5 pa
!   to the kappa power ranging from 0.01**rocp to 1.10**rocp.
!
! program history log:
!   91-05-07  iredell
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
!
! usage:  call gtma
!
! subprograms called:
!   (stmaxg)   inlinable subprogram to compute parcel temperature
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx,jy
    real(krealfp) xmin,xmax,ymin,ymax,xinc,yinc,x,y,pk,the,t,q,tg
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=200._krealfp
    xmax=500._krealfp
    ymin=0.01_krealfp**con_rocp
    ymax=1.10_krealfp**con_rocp
    xinc=(xmax-xmin)/(nxma-1)
    c1xma=1.-xmin/xinc
    c2xma=1./xinc
    yinc=(ymax-ymin)/(nyma-1)
    c1yma=1.-ymin/yinc
    c2yma=1./yinc
    do jy=1,nyma
      y=ymin+(jy-1)*yinc
      pk=y
      tg=xmin*y
      do jx=1,nxma
        x=xmin+(jx-1)*xinc
        the=x
        call stmaxg(tg,the,pk,t,q)
        tbtma(jx,jy)=t
        tbqma(jx,jy)=q
        tg=t
      enddo
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental subroutine stma(the,pk,tma,qma)
!$$$     subprogram documentation block
!
! subprogram: stma         compute moist adiabat temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute temperature and specific humidity of a parcel
!   lifted up a moist adiabat from equivalent potential temperature
!   at the lcl and pressure over 1e5 pa to the kappa power.
!   bilinear interpolations are done between values in a lookup table
!   computed in gtma. see documentation for stmaxg for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.01 kelvin
!   and 5.e-6 kg/kg for temperature and humidity, respectively.
!   on the cray, stma is about 35 times faster than exact calculation.
!   this subprogram should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             expand table
! 1999-03-01  iredell             f90 module
!
! usage:  call stma(the,pk,tma,qma)
!
!   input argument list:
!     the        real(krealfp) equivalent potential temperature in kelvin
!     pk         real(krealfp) pressure over 1e5 pa to the kappa power
!
!   output argument list:
!     tma        real(krealfp) parcel temperature in kelvin
!     qma        real(krealfp) parcel specific humidity in kg/kg
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp),intent(in):: the,pk
    real(krealfp),intent(out):: tma,qma
    integer jx,jy
    real(krealfp) xj,yj,ftx1,ftx2,qx1,qx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xma+c2xma*the,1._krealfp),real(nxma,krealfp))
    yj=min(max(c1yma+c2yma*pk,1._krealfp),real(nyma,krealfp))
    jx=min(xj,nxma-1._krealfp)
    jy=min(yj,nyma-1._krealfp)
    ftx1=tbtma(jx,jy)+(xj-jx)*(tbtma(jx+1,jy)-tbtma(jx,jy))
    ftx2=tbtma(jx,jy+1)+(xj-jx)*(tbtma(jx+1,jy+1)-tbtma(jx,jy+1))
    tma=ftx1+(yj-jy)*(ftx2-ftx1)
    qx1=tbqma(jx,jy)+(xj-jx)*(tbqma(jx+1,jy)-tbqma(jx,jy))
    qx2=tbqma(jx,jy+1)+(xj-jx)*(tbqma(jx+1,jy+1)-tbqma(jx,jy+1))
    qma=qx1+(yj-jy)*(qx2-qx1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental subroutine stmaq(the,pk,tma,qma)
!$$$     subprogram documentation block
!
! subprogram: stmaq        compute moist adiabat temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute temperature and specific humidity of a parcel
!   lifted up a moist adiabat from equivalent potential temperature
!   at the lcl and pressure over 1e5 pa to the kappa power.
!   biquadratic interpolations are done between values in a lookup table
!   computed in gtma. see documentation for stmaxg for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.0005 kelvin
!   and 1.e-7 kg/kg for temperature and humidity, respectively.
!   on the cray, stmaq is about 25 times faster than exact calculation.
!   this subprogram should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             quadratic interpolation
! 1999-03-01  iredell             f90 module
!
! usage:  call stmaq(the,pk,tma,qma)
!
!   input argument list:
!     the        real(krealfp) equivalent potential temperature in kelvin
!     pk         real(krealfp) pressure over 1e5 pa to the kappa power
!
!   output argument list:
!     tmaq       real(krealfp) parcel temperature in kelvin
!     qma        real(krealfp) parcel specific humidity in kg/kg
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp),intent(in):: the,pk
    real(krealfp),intent(out):: tma,qma
    integer jx,jy
    real(krealfp) xj,yj,dxj,dyj
    real(krealfp) ft11,ft12,ft13,ft21,ft22,ft23,ft31,ft32,ft33
    real(krealfp) ftx1,ftx2,ftx3
    real(krealfp) q11,q12,q13,q21,q22,q23,q31,q32,q33,qx1,qx2,qx3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xma+c2xma*the,1._krealfp),real(nxma,krealfp))
    yj=min(max(c1yma+c2yma*pk,1._krealfp),real(nyma,krealfp))
    jx=min(max(nint(xj),2),nxma-1)
    jy=min(max(nint(yj),2),nyma-1)
    dxj=xj-jx
    dyj=yj-jy
    ft11=tbtma(jx-1,jy-1)
    ft12=tbtma(jx-1,jy)
    ft13=tbtma(jx-1,jy+1)
    ft21=tbtma(jx,jy-1)
    ft22=tbtma(jx,jy)
    ft23=tbtma(jx,jy+1)
    ft31=tbtma(jx+1,jy-1)
    ft32=tbtma(jx+1,jy)
    ft33=tbtma(jx+1,jy+1)
    ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
    ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
    ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
    tma=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2
    q11=tbqma(jx-1,jy-1)
    q12=tbqma(jx-1,jy)
    q13=tbqma(jx-1,jy+1)
    q21=tbqma(jx,jy-1)
    q22=tbqma(jx,jy)
    q23=tbqma(jx,jy+1)
    q31=tbqma(jx+1,jy-1)
    q32=tbqma(jx+1,jy)
    q33=tbqma(jx+1,jy+1)
    qx1=(((q31+q11)/2-q21)*dxj+(q31-q11)/2)*dxj+q21
    qx2=(((q32+q12)/2-q22)*dxj+(q32-q12)/2)*dxj+q22
    qx3=(((q33+q13)/2-q23)*dxj+(q33-q13)/2)*dxj+q23
    qma=(((qx3+qx1)/2-qx2)*dyj+(qx3-qx1)/2)*dyj+qx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental subroutine stmax(the,pk,tma,qma)
!$$$     subprogram documentation block
!
! subprogram: stmax        compute moist adiabat temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute temperature and humidity of a parcel
!   lifted up a moist adiabat from equivalent potential temperature
!   at the lcl and pressure over 1e5 pa to the kappa power.
!   an approximate parcel temperature for subprogram stmaxg
!   is obtained using stma so gtma must be already called.
!   see documentation for stmaxg for details.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
!
! usage:  call stmax(the,pk,tma,qma)
!
!   input argument list:
!     the        real(krealfp) equivalent potential temperature in kelvin
!     pk         real(krealfp) pressure over 1e5 pa to the kappa power
!
!   output argument list:
!     tma        real(krealfp) parcel temperature in kelvin
!     qma        real(krealfp) parcel specific humidity in kg/kg
!
! subprograms called:
!   (stma)     inlinable subprogram to compute parcel temperature
!   (stmaxg)   inlinable subprogram to compute parcel temperature
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp),intent(in):: the,pk
    real(krealfp),intent(out):: tma,qma
    real(krealfp) tg,qg
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call stma(the,pk,tg,qg)
    call stmaxg(tg,the,pk,tma,qma)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental subroutine stmaxg(tg,the,pk,tma,qma)
!$$$     subprogram documentation block
!
! subprogram: stmaxg       compute moist adiabat temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: exactly compute temperature and humidity of a parcel
!   lifted up a moist adiabat from equivalent potential temperature
!   at the lcl and pressure over 1e5 pa to the kappa power.
!   a guess parcel temperature must be provided.
!   equivalent potential temperature is constant for a saturated parcel
!   rising adiabatically up a moist adiabat when the heat and mass
!   of the condensed water are neglected.  ice is also neglected.
!   the formula for equivalent potential temperature (holton) is
!       the=t*(pd**(-rocp))*exp(el*eps*pv/(cp*t*pd))
!   where t is the temperature, pv is the saturated vapor pressure,
!   pd is the dry pressure p-pv, el is the temperature dependent
!   latent heat of condensation hvap+dldt*(t-ttp), and other values
!   are physical constants defined in parameter statements in the code.
!   the formula is inverted by iterating newtonian approximations
!   for each the and p until t is found to within 1.e-4 kelvin.
!   the specific humidity is then computed from pv and pd.
!   this subprogram can be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             exact computation
! 1999-03-01  iredell             f90 module
!
! usage:  call stmaxg(tg,the,pk,tma,qma)
!
!   input argument list:
!     tg         real(krealfp) guess parcel temperature in kelvin
!     the        real(krealfp) equivalent potential temperature in kelvin
!     pk         real(krealfp) pressure over 1e5 pa to the kappa power
!
!   output argument list:
!     tma        real(krealfp) parcel temperature in kelvin
!     qma        real(krealfp) parcel specific humidity in kg/kg
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp),intent(in):: tg,the,pk
    real(krealfp),intent(out):: tma,qma
    real(krealfp),parameter:: terrm=1.e-4
    real(krealfp) t,p,tr,pv,pd,el,expo,thet,dthet,terr
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    t=tg
    p=pk**con_cpor
    do i=1,100
      tr=con_ttp/t
      pv=psatb*(tr**con_xpona)*exp(con_xponb*(1.-tr))
      pd=p-pv
      el=con_hvap+con_dldt*(t-con_ttp)
      expo=el*con_eps*pv/(con_cp*t*pd)
      thet=t*pd**(-con_rocp)*exp(expo)
      dthet=thet/t*(1.+expo*(con_dldt*t/el+el*p/(con_rv*t*pd)))
      terr=(thet-the)/dthet
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    tma=t
    tr=con_ttp/t
    pv=psatb*(tr**con_xpona)*exp(con_xponb*(1.-tr))
    pd=p-pv
    qma=con_eps*pv/(pd+con_eps*pv)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine gpkap
!$$$   subprogram  documentation  block
!
! subprogram: gpkap        compute coefficients for p**kappa
!   author: phillips         org: w/nmc2x2   date: 29 dec 82
!
! abstract: computes pressure to the kappa table as a function of pressure
!   for the table lookup function fpkap.
!   exact pressure to the kappa values are calculated in subprogram fpkapx.
!   the current implementation computes a table with a length
!   of 5501 for pressures ranging up to 110000 pascals.
!
! program history log:
!   94-12-30  iredell
! 1999-03-01  iredell             f90 module
! 1999-03-24  iredell             table lookup
!
! usage:  call gpkap
!
! subprograms called:
!   fpkapx     function to compute exact pressure to the kappa
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,p
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=0._krealfp
    xmax=110000._krealfp
    xinc=(xmax-xmin)/(nxpkap-1)
    c1xpkap=1.-xmin/xinc
    c2xpkap=1./xinc
    do jx=1,nxpkap
      x=xmin+(jx-1)*xinc
      p=x
      tbpkap(jx)=fpkapx(p)
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental function fpkap(p)
!$$$     subprogram documentation block
!
! subprogram: fpkap        raise pressure to the kappa power.
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: raise pressure over 1e5 pa to the kappa power.
!   a linear interpolation is done between values in a lookup table
!   computed in gpkap. see documentation for fpkapx for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy ranges from 9 decimal places
!   at 100000 pascals to 5 decimal places at 1000 pascals.
!   on the cray, fpkap is over 5 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             standardized kappa,
!                                 increased range and accuracy
! 1999-03-01  iredell             f90 module
! 1999-03-24  iredell             table lookup
!
! usage:   pkap=fpkap(p)
!
!   input argument list:
!     p          real(krealfp) pressure in pascals
!
!   output argument list:
!     fpkap      real(krealfp) p over 1e5 pa to the kappa power
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpkap
    real(krealfp),intent(in):: p
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xpkap+c2xpkap*p,1._krealfp),real(nxpkap,krealfp))
    jx=min(xj,nxpkap-1._krealfp)
    fpkap=tbpkap(jx)+(xj-jx)*(tbpkap(jx+1)-tbpkap(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function fpkapq(p)
!$$$     subprogram documentation block
!
! subprogram: fpkapq       raise pressure to the kappa power.
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: raise pressure over 1e5 pa to the kappa power.
!   a quadratic interpolation is done between values in a lookup table
!   computed in gpkap. see documentation for fpkapx for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy ranges from 12 decimal places
!   at 100000 pascals to 7 decimal places at 1000 pascals.
!   on the cray, fpkap is over 4 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             standardized kappa,
!                                 increased range and accuracy
! 1999-03-01  iredell             f90 module
! 1999-03-24  iredell             table lookup
!
! usage:   pkap=fpkapq(p)
!
!   input argument list:
!     p          real(krealfp) pressure in pascals
!
!   output argument list:
!     fpkapq     real(krealfp) p over 1e5 pa to the kappa power
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpkapq
    real(krealfp),intent(in):: p
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xpkap+c2xpkap*p,1._krealfp),real(nxpkap,krealfp))
    jx=min(max(nint(xj),2),nxpkap-1)
    dxj=xj-jx
    fj1=tbpkap(jx-1)
    fj2=tbpkap(jx)
    fj3=tbpkap(jx+1)
    fpkapq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  function fpkapo(p)
!$$$   subprogram  documentation  block
!
! subprogram: fpkapo       raise surface pressure to the kappa power.
!   author: phillips         org: w/nmc2x2   date: 29 dec 82
!
! abstract: raise surface pressure over 1e5 pa to the kappa power
!   using a rational weighted chebyshev approximation.
!   the numerator is of order 2 and the denominator is of order 4.
!   the pressure range is 40000-110000 pa and kappa is defined in fpkapx.
!   the accuracy of this approximation is almost 8 decimal places.
!   on the cray, fpkap is over 10 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             standardized kappa,
!                                 increased range and accuracy
! 1999-03-01  iredell             f90 module
!
! usage:  pkap=fpkapo(p)
!
!   input argument list:
!     p          real(krealfp) surface pressure in pascals
!                p should be in the range 40000 to 110000
!
!   output argument list:
!     fpkapo     real(krealfp) p over 1e5 pa to the kappa power
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpkapo
    real(krealfp),intent(in):: p
    integer,parameter:: nnpk=2,ndpk=4
    real(krealfp):: cnpk(0:nnpk)=(/3.13198449e-1,5.78544829e-2,&
                                         8.35491871e-4/)
    real(krealfp):: cdpk(0:ndpk)=(/1.,8.15968401e-2,5.72839518e-4,&
                                         -4.86959812e-7,5.24459889e-10/)
    integer n
    real(krealfp) pkpa,fnpk,fdpk
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    pkpa=p*1.e-3_krealfp
    fnpk=cnpk(nnpk)
    do n=nnpk-1,0,-1
      fnpk=pkpa*fnpk+cnpk(n)
    enddo
    fdpk=cdpk(ndpk)
    do n=ndpk-1,0,-1
      fdpk=pkpa*fdpk+cdpk(n)
    enddo
    fpkapo=fnpk/fdpk
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function fpkapx(p)
!$$$   subprogram  documentation  block
!
! subprogram: fpkapx       raise pressure to the kappa power.
!   author: phillips         org: w/nmc2x2   date: 29 dec 82
!
! abstract: raise pressure over 1e5 pa to the kappa power.
!   kappa is equal to rd/cp where rd and cp are physical constants.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   94-12-30  iredell             made into inlinable function
! 1999-03-01  iredell             f90 module
!
! usage:  pkap=fpkapx(p)
!
!   input argument list:
!     p          real(krealfp) pressure in pascals
!
!   output argument list:
!     fpkapx     real(krealfp) p over 1e5 pa to the kappa power
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpkapx
    real(krealfp),intent(in):: p
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    fpkapx=(p/1.e5_krealfp)**con_rocp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine grkap
!$$$   subprogram  documentation  block
!
! subprogram: grkap        compute coefficients for p**(1/kappa)
!   author: phillips         org: w/nmc2x2   date: 29 dec 82
!
! abstract: computes pressure to the 1/kappa table as a function of pressure
!   for the table lookup function frkap.
!   exact pressure to the 1/kappa values are calculated in subprogram frkapx.
!   the current implementation computes a table with a length
!   of 5501 for pressures ranging up to 110000 pascals.
!
! program history log:
!   94-12-30  iredell
! 1999-03-01  iredell             f90 module
! 1999-03-24  iredell             table lookup
!
! usage:  call grkap
!
! subprograms called:
!   frkapx     function to compute exact pressure to the 1/kappa
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,p
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=0._krealfp
    xmax=fpkapx(110000._krealfp)
    xinc=(xmax-xmin)/(nxrkap-1)
    c1xrkap=1.-xmin/xinc
    c2xrkap=1./xinc
    do jx=1,nxrkap
      x=xmin+(jx-1)*xinc
      p=x
      tbrkap(jx)=frkapx(p)
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental function frkap(pkap)
!$$$     subprogram documentation block
!
! subprogram: frkap        raise pressure to the 1/kappa power.
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: raise pressure over 1e5 pa to the 1/kappa power.
!   a linear interpolation is done between values in a lookup table
!   computed in grkap. see documentation for frkapx for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 7 decimal places.
!   on the ibm, fpkap is about 4 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             standardized kappa,
!                                 increased range and accuracy
! 1999-03-01  iredell             f90 module
! 1999-03-24  iredell             table lookup
!
! usage:   p=frkap(pkap)
!
!   input argument list:
!     pkap       real(krealfp) p over 1e5 pa to the kappa power
!
!   output argument list:
!     frkap      real(krealfp) pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) frkap
    real(krealfp),intent(in):: pkap
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xrkap+c2xrkap*pkap,1._krealfp),real(nxrkap,krealfp))
    jx=min(xj,nxrkap-1._krealfp)
    frkap=tbrkap(jx)+(xj-jx)*(tbrkap(jx+1)-tbrkap(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function frkapq(pkap)
!$$$     subprogram documentation block
!
! subprogram: frkapq       raise pressure to the 1/kappa power.
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: raise pressure over 1e5 pa to the 1/kappa power.
!   a quadratic interpolation is done between values in a lookup table
!   computed in grkap. see documentation for frkapx for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 11 decimal places.
!   on the ibm, fpkap is almost 4 times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
!   94-12-30  iredell             standardized kappa,
!                                 increased range and accuracy
! 1999-03-01  iredell             f90 module
! 1999-03-24  iredell             table lookup
!
! usage:   p=frkapq(pkap)
!
!   input argument list:
!     pkap       real(krealfp) p over 1e5 pa to the kappa power
!
!   output argument list:
!     frkapq     real(krealfp) pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) frkapq
    real(krealfp),intent(in):: pkap
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xrkap+c2xrkap*pkap,1._krealfp),real(nxrkap,krealfp))
    jx=min(max(nint(xj),2),nxrkap-1)
    dxj=xj-jx
    fj1=tbrkap(jx-1)
    fj2=tbrkap(jx)
    fj3=tbrkap(jx+1)
    frkapq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function frkapx(pkap)
!$$$   subprogram  documentation  block
!
! subprogram: frkapx       raise pressure to the 1/kappa power.
!   author: phillips         org: w/nmc2x2   date: 29 dec 82
!
! abstract: raise pressure over 1e5 pa to the 1/kappa power.
!   kappa is equal to rd/cp where rd and cp are physical constants.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   94-12-30  iredell             made into inlinable function
! 1999-03-01  iredell             f90 module
!
! usage:  p=frkapx(pkap)
!
!   input argument list:
!     pkap       real(krealfp) p over 1e5 pa to the kappa power
!
!   output argument list:
!     frkapx     real(krealfp) pressure in pascals
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) frkapx
    real(krealfp),intent(in):: pkap
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    frkapx=pkap**(1/con_rocp)*1.e5_krealfp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gtlcl
!$$$     subprogram documentation block
!
! subprogram: gtlcl        compute equivalent potential temperature table
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute lifting condensation level temperature table
!   as a function of temperature and dewpoint depression for function ftlcl.
!   lifting condensation level temperature is calculated in subprogram ftlclx
!   the current implementation computes a table with a first dimension
!   of 151 for temperatures ranging from 180.0 to 330.0 kelvin
!   and a second dimension of 61 for dewpoint depression ranging from
!   0 to 60 kelvin.
!
! program history log:
! 1999-03-01  iredell             f90 module
!
! usage:  call gtlcl
!
! subprograms called:
!   (ftlclx)    inlinable function to compute lcl temperature
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    integer jx,jy
    real(krealfp) xmin,xmax,ymin,ymax,xinc,yinc,x,y,tdpd,t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=180._krealfp
    xmax=330._krealfp
    ymin=0._krealfp
    ymax=60._krealfp
    xinc=(xmax-xmin)/(nxtlcl-1)
    c1xtlcl=1.-xmin/xinc
    c2xtlcl=1./xinc
    yinc=(ymax-ymin)/(nytlcl-1)
    c1ytlcl=1.-ymin/yinc
    c2ytlcl=1./yinc
    do jy=1,nytlcl
      y=ymin+(jy-1)*yinc
      tdpd=y
      do jx=1,nxtlcl
        x=xmin+(jx-1)*xinc
        t=x
        tbtlcl(jx,jy)=ftlclx(t,tdpd)
      enddo
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  elemental function ftlcl(t,tdpd)
!$$$     subprogram documentation block
!
! subprogram: ftlcl        compute lcl temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute temperature at the lifting condensation level
!   from temperature and dewpoint depression.
!   a bilinear interpolation is done between values in a lookup table
!   computed in gtlcl. see documentation for ftlclx for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.0005 kelvin.
!   on the cray, ftlcl is ? times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
! 1999-03-01  iredell             f90 module
!
! usage:   tlcl=ftlcl(t,tdpd)
!
!   input argument list:
!     t          real(krealfp) lcl temperature in kelvin
!     tdpd       real(krealfp) dewpoint depression in kelvin
!
!   output argument list:
!     ftlcl      real(krealfp) temperature at the lcl in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftlcl
    real(krealfp),intent(in):: t,tdpd
    integer jx,jy
    real(krealfp) xj,yj,ftx1,ftx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xtlcl+c2xtlcl*t,1._krealfp),real(nxtlcl,krealfp))
    yj=min(max(c1ytlcl+c2ytlcl*tdpd,1._krealfp),real(nytlcl,krealfp))
    jx=min(xj,nxtlcl-1._krealfp)
    jy=min(yj,nytlcl-1._krealfp)
    ftx1=tbtlcl(jx,jy)+(xj-jx)*(tbtlcl(jx+1,jy)-tbtlcl(jx,jy))
    ftx2=tbtlcl(jx,jy+1)+(xj-jx)*(tbtlcl(jx+1,jy+1)-tbtlcl(jx,jy+1))
    ftlcl=ftx1+(yj-jy)*(ftx2-ftx1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftlclq(t,tdpd)
!$$$     subprogram documentation block
!
! subprogram: ftlclq       compute lcl temperature
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute temperature at the lifting condensation level
!   from temperature and dewpoint depression.
!   a biquadratic interpolation is done between values in a lookup table
!   computed in gtlcl. see documentation for ftlclx for details.
!   input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.000003 kelvin.
!   on the cray, ftlclq is ? times faster than exact calculation.
!   this function should be expanded inline in the calling routine.
!
! program history log:
! 1999-03-01  iredell             f90 module
!
! usage:   tlcl=ftlclq(t,tdpd)
!
!   input argument list:
!     t          real(krealfp) lcl temperature in kelvin
!     tdpd       real(krealfp) dewpoint depression in kelvin
!
!   output argument list:
!     ftlcl      real(krealfp) temperature at the lcl in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftlclq
    real(krealfp),intent(in):: t,tdpd
    integer jx,jy
    real(krealfp) xj,yj,dxj,dyj
    real(krealfp) ft11,ft12,ft13,ft21,ft22,ft23,ft31,ft32,ft33
    real(krealfp) ftx1,ftx2,ftx3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xtlcl+c2xtlcl*t,1._krealfp),real(nxtlcl,krealfp))
    yj=min(max(c1ytlcl+c2ytlcl*tdpd,1._krealfp),real(nytlcl,krealfp))
    jx=min(max(nint(xj),2),nxtlcl-1)
    jy=min(max(nint(yj),2),nytlcl-1)
    dxj=xj-jx
    dyj=yj-jy
    ft11=tbtlcl(jx-1,jy-1)
    ft12=tbtlcl(jx-1,jy)
    ft13=tbtlcl(jx-1,jy+1)
    ft21=tbtlcl(jx,jy-1)
    ft22=tbtlcl(jx,jy)
    ft23=tbtlcl(jx,jy+1)
    ft31=tbtlcl(jx+1,jy-1)
    ft32=tbtlcl(jx+1,jy)
    ft33=tbtlcl(jx+1,jy+1)
    ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
    ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
    ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
    ftlclq=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  function ftlclo(t,tdpd)
!$$$   subprogram  documentation  block
!
! subprogram: ftlclo       compute lcl temperature.
!   author: phillips         org: w/nmc2x2   date: 29 dec 82
!
! abstract: compute temperature at the lifting condensation level
!   from temperature and dewpoint depression.  the formula used is
!   a polynomial taken from phillips mstadb routine which empirically
!   approximates the original exact implicit relationship.
!   (this kind of approximation is customary (inman, 1969), but
!   the original source for this particular one is not yet known. -mi)
!   its accuracy is about 0.03 kelvin for a dewpoint depression of 30.
!   this function should be expanded inline in the calling routine.
!
! program history log:
!   91-05-07  iredell             made into inlinable function
! 1999-03-01  iredell             f90 module
!
! usage:  tlcl=ftlclo(t,tdpd)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!     tdpd       real(krealfp) dewpoint depression in kelvin
!
!   output argument list:
!     ftlclo     real(krealfp) temperature at the lcl in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftlclo
    real(krealfp),intent(in):: t,tdpd
    real(krealfp),parameter:: clcl1= 0.954442e+0,clcl2= 0.967772e-3,&
                                    clcl3=-0.710321e-3,clcl4=-0.270742e-5
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ftlclo=t-tdpd*(clcl1+clcl2*t+tdpd*(clcl3+clcl4*t))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  elemental function ftlclx(t,tdpd)
!$$$   subprogram  documentation  block
!
! subprogram: ftlclx       compute lcl temperature.
!   author: iredell          org: w/nmc2x2   date: 25 march 1999
!
! abstract: compute temperature at the lifting condensation level
!   from temperature and dewpoint depression.  a parcel lifted
!   adiabatically becomes saturated at the lifting condensation level.
!   the water model assumes a perfect gas, constant specific heats
!   for gas and liquid, and neglects the volume of the liquid.
!   the model does account for the variation of the latent heat
!   of condensation with temperature.  the ice option is not included.
!   the clausius-clapeyron equation is integrated from the triple point
!   to get the formulas
!       pvlcl=con_psat*(trlcl**xa)*exp(xb*(1.-trlcl))
!       pvdew=con_psat*(trdew**xa)*exp(xb*(1.-trdew))
!   where pvlcl is the saturated parcel vapor pressure at the lcl,
!   pvdew is the unsaturated parcel vapor pressure initially,
!   trlcl is ttp/tlcl and trdew is ttp/tdew.  the adiabatic lifting
!   of the parcel is represented by the following formula
!       pvdew=pvlcl*(t/tlcl)**(1/kappa)
!   this formula is inverted by iterating newtonian approximations
!   until tlcl is found to within 1.e-6 kelvin.  note that the minimum
!   returned temperature is 180 kelvin.
!
! program history log:
! 1999-03-25  iredell
!
! usage:  tlcl=ftlclx(t,tdpd)
!
!   input argument list:
!     t          real(krealfp) temperature in kelvin
!     tdpd       real(krealfp) dewpoint depression in kelvin
!
!   output argument list:
!     ftlclx     real(krealfp) temperature at the lcl in kelvin
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftlclx
    real(krealfp),intent(in):: t,tdpd
    real(krealfp),parameter:: terrm=1.e-4,tlmin=180.,tlminx=tlmin-5.
    real(krealfp) tr,pvdew,tlcl,ta,pvlcl,el,dpvlcl,terr
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    tr=con_ttp/(t-tdpd)
    pvdew=con_psat*(tr**con_xpona)*exp(con_xponb*(1.-tr))
    tlcl=t-tdpd
    do i=1,100
      tr=con_ttp/tlcl
      ta=t/tlcl
      pvlcl=con_psat*(tr**con_xpona)*exp(con_xponb*(1.-tr))*ta**(1/con_rocp)
      el=con_hvap+con_dldt*(tlcl-con_ttp)
      dpvlcl=(el/(con_rv*t**2)+1/(con_rocp*tlcl))*pvlcl
      terr=(pvlcl-pvdew)/dpvlcl
      tlcl=tlcl-terr
      if(abs(terr).le.terrm.or.tlcl.lt.tlminx) exit
    enddo
    ftlclx=max(tlcl,tlmin)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gfuncphys
!$$$     subprogram documentation block
!
! subprogram: gfuncphys    compute all physics function tables
!   author: n phillips            w/nmc2x2   date: 30 dec 82
!
! abstract: compute all physics function tables.  lookup tables are
!   set up for computing saturation vapor pressure, dewpoint temperature,
!   equivalent potential temperature, moist adiabatic temperature and humidity,
!   pressure to the kappa, and lifting condensation level temperature.
!
! program history log:
! 1999-03-01  iredell             f90 module
!
! usage:  call gfuncphys
!
! subprograms called:
!   gpvsl       compute saturation vapor pressure over liquid table
!   gpvsi       compute saturation vapor pressure over ice table
!   gpvs        compute saturation vapor pressure table
!   gtdpl       compute dewpoint temperature over liquid table
!   gtdpi       compute dewpoint temperature over ice table
!   gtdp        compute dewpoint temperature table
!   gthe        compute equivalent potential temperature table
!   gtma        compute moist adiabat tables
!   gpkap       compute pressure to the kappa table
!   grkap       compute pressure to the 1/kappa table
!   gtlcl       compute lcl temperature table
!
! attributes:
!   language: fortran 90.
!
!$$$
    implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call gpvsl
    call gpvsi
    call gpvs
    call gtdpl
    call gtdpi
    call gtdp
    call gthe
    call gtma
    call gpkap
    call grkap
    call gtlcl
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
end module
