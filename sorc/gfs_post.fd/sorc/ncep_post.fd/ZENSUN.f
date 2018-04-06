subroutine zensun(day,time,lat,lon,pi,sun_zenith,sun_azimuth)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  zensun         make sun zenith and sun azimuth angle
!
!   prgmmr: Paul Ricchiazzi org: Earth Space Research Group,UCSB  date: 1992-10-23
!
! abstract: 
!       Compute solar position information as a function of
!      geographic coordinates, date and time.
!
! program history log:
!   2005-10-21  kazumori - reformatted for GSI
!
!   input argument list:
!     day -     Julian day (positive scalar or vector)
!               (spring equinox =  80)
!               (summer solstice= 171)
!               (fall equinox   = 266)
!               (winter solstice= 356)
!     time -    Universal Time in hours (scalar or vector)
!     lat  -    geographic latitude of point on earth's surface (degrees)
!     lon  -    geographic longitude of point on earth's surface (degrees)
!
!   output argument list:
!     sun_zenith  - solar zenith angle
!     sun_azimuth - solar azimuth angle
!
!   comments:
!
!
!     PROCEDURE:
!
!  1. Calculate the subsolar point latitude and longitude, based on
!     DAY and TIME. Since each year is 365.25 days long the exact
!     value of the declination angle changes from year to year.  For
!     precise values consult THE AMERICAN EPHEMERIS AND NAUTICAL
!     ALMANAC published yearly by the U.S. govt. printing office.  The
!     subsolar coordinates used in this code were provided by a
!     program written by Jeff Dozier.
!
!  2. Given the subsolar latitude and longitude, spherical geometry is
!     used to find the solar zenith, azimuth and flux multiplier.
!
!  eqt = equation of time (minutes)  ! solar longitude correction = -15*eqt
!  dec = declination angle (degrees) = solar latitude
!
! LOWTRAN v7 data (25 points)
!     The LOWTRAN solar position data is characterized by only 25 points.
!     This should predict the subsolar angles within one degree.  For
!     increased accuracy add more data points.
!
!nday=[   1.,    9.,   21.,   32.,   44.,   60.,  91.,  121.,  141.,  152.,$
!       160.,  172.,  182.,  190.,  202.,  213., 244.,  274.,  305.,  309.,$
!       325.,  335.,  343.,  355.,  366.]
!
!eqt=[ -3.23, -6.83,-11.17,-13.57,-14.33,-12.63, -4.2,  2.83,  3.57,  2.45,$
!       1.10, -1.42, -3.52, -4.93, -6.25, -6.28,-0.25, 10.02, 16.35, 16.38,$
!       14.3, 11.27,  8.02,  2.32, -3.23]
!
!dec=[-23.07,-22.22,-20.08,-17.32,-13.62, -7.88, 4.23, 14.83, 20.03, 21.95,$
!      22.87, 23.45, 23.17, 22.47, 20.63, 18.23, 8.58, -2.88,-14.18,-15.45,$
!     -19.75,-21.68,-22.75,-23.43,-23.07]
!
! Analemma information from Jeff Dozier
!     This data is characterized by 74 points
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!
  use kinds, only: r_kind,i_kind

  implicit none

  integer(i_kind),intent(in)::   day
  real(r_kind),intent(in) ::  pi,time,lat,lon
  integer(i_kind)   di
  real(r_kind)      z,gaz
  real(r_kind)      ut,noon
  real(r_kind)      y(5),y2(5),x(2,5),Tx(5,2),xTx(2,2),aTx(5,2),det
  real(r_kind)      tt,eqtime,decang,latsun,lonsun
  real(r_kind)      nday(74),eqt(74),dec(74)
  real(r_kind)      beta(2), beta2(2), a(2,2)
  real(r_kind)      t0,t1,p0,p1,zz,xx,yy
  real(r_kind),intent(out) ::     sun_zenith,sun_azimuth

  data   nday/1.0,   6.0,  11.0,  16.0,  21.0,  26.0,  31.0,  36.0,  41.0,  46.0,&
             51.0,  56.0,  61.0,  66.0,  71.0,  76.0,  81.0,  86.0,  91.0,  96.0,&
             101.0, 106.0, 111.0, 116.0, 121.0, 126.0, 131.0, 136.0, 141.0, 146.0,&
             151.0, 156.0, 161.0, 166.0, 171.0, 176.0, 181.0, 186.0, 191.0, 196.0,&
             201.0, 206.0, 211.0, 216.0, 221.0, 226.0, 231.0, 236.0, 241.0, 246.0,&
             251.0, 256.0, 261.0, 266.0, 271.0, 276.0, 281.0, 286.0, 291.0, 296.0,&
             301.0, 306.0, 311.0, 316.0, 321.0, 326.0, 331.0, 336.0, 341.0, 346.0,&
             351.0, 356.0, 361.0, 366.0/

  data  eqt/ -3.23, -5.49, -7.60, -9.48,-11.09,-12.39,-13.34,-13.95,-14.23,-14.19,&
            -13.85,-13.22,-12.35,-11.26,-10.01, -8.64, -7.18, -5.67, -4.16, -2.69,&
             -1.29, -0.02,  1.10,  2.05,  2.80,  3.33,  3.63,  3.68,  3.49,  3.09,&
              2.48,  1.71,  0.79, -0.24, -1.33, -2.41, -3.45, -4.39, -5.20, -5.84,&
             -6.28, -6.49, -6.44, -6.15, -5.60, -4.82, -3.81, -2.60, -1.19,  0.36,&
              2.03,  3.76,  5.54,  7.31,  9.04, 10.69, 12.20, 13.53, 14.65, 15.52,&
             16.12, 16.41, 16.36, 15.95, 15.19, 14.09, 12.67, 10.93,  8.93,  6.70,&
              4.32,  1.86, -0.62, -3.23/

  data dec/ -23.06,-22.57,-21.91,-21.06,-20.05,-18.88,-17.57,-16.13,-14.57,-12.91,&
            -11.16, -9.34, -7.46, -5.54, -3.59, -1.62,  0.36,  2.33,  4.28,  6.19,&
              8.06,  9.88, 11.62, 13.29, 14.87, 16.34, 17.70, 18.94, 20.04, 21.00,&
             21.81, 22.47, 22.95, 23.28, 23.43, 23.40, 23.21, 22.85, 22.32, 21.63,&
             20.79, 19.80, 18.67, 17.42, 16.05, 14.57, 13.00, 11.33,  9.60,  7.80,&
              5.95,  4.06,  2.13,  0.19, -1.75, -3.69, -5.62, -7.51, -9.36,-11.16,&
            -12.88,-14.53,-16.07,-17.50,-18.81,-19.98,-20.99,-21.85,-22.52,-23.02,&
            -23.33,-23.44,-23.35,-23.06/

!
! compute the subsolar coordinates
!

  tt= mod(real((int(day)+time/24.-1.)),365.25) +1.  ! fractional day number
                                                    ! with 12am 1jan = 1.
  do di = 1, 73
    if ((tt .ge. nday(di)) .and. (tt .le. nday(di+1))) exit
  end do

!============== Perform a least squares regression on doy**3 ==============

  x(1,:) = 1.0

  if ((di .ge. 3) .and. (di .le. 72)) then
    y(:) = eqt(di-2:di+2)
    y2(:) = dec(di-2:di+2)

    x(2,:) = nday(di-2:di+2)**3
  end if
  if (di .eq. 2) then
    y(1) = eqt(73)
    y(2:5) = eqt(di-1:di+2)
    y2(1) = dec(73)
    y2(2:5) = dec(di-1:di+2)

    x(2,1) = nday(73)**3
    x(2,2:5) = (365.+nday(di-1:di+2))**3
  end if
  if (di .eq. 1) then
    y(1:2) = eqt(72:73)
    y(3:5) = eqt(di:di+2)
    y2(1:2) = dec(72:73)
    y2(3:5) = dec(di:di+2)

    x(2,1:2) = nday(72:73)**3
    x(2,3:5) = (365.+nday(di:di+2))**3
  end if
  if (di .eq. 73) then
    y(1:4) = eqt(di-2:di+1)
    y(5) = eqt(2)
    y2(1:4) = dec(di-2:di+1)
    y2(5) = dec(2)

    x(2,1:4) = nday(di-2:di+1)**3
    x(2,5) = (365.+nday(2))**3
  end if
  if (di .eq. 74) then
    y(1:3) = eqt(di-2:di)
    y(4:5) = eqt(2:3)
    y2(1:3) = dec(di-2:di)
    y2(4:5) = dec(2:3)

    x(2,1:3) = nday(di-2:di)**3
    x(2,4:5) = (365.+nday(2:3))**3
  end if

!  Tx = transpose(x)
  Tx(1:5,1)=x(1,1:5)
  Tx(1:5,2)=x(2,1:5)
!  xTx = MATMUL(x,Tx)
  xTx(1,1)=x(1,1)*Tx(1,1)+x(1,2)*Tx(2,1)+x(1,3)*Tx(3,1)+x(1,4)*Tx(4,1)+x(1,5)*Tx(5,1)
  xTx(1,2)=x(1,1)*Tx(1,2)+x(1,2)*Tx(2,2)+x(1,3)*Tx(3,2)+x(1,4)*Tx(4,2)+x(1,5)*Tx(5,2)
  xTx(2,1)=x(2,1)*Tx(1,1)+x(2,2)*Tx(2,1)+x(2,3)*Tx(3,1)+x(2,4)*Tx(4,1)+x(2,5)*Tx(5,1)
  xTx(2,2)=x(2,1)*Tx(1,2)+x(2,2)*Tx(2,2)+x(2,3)*Tx(3,2)+x(2,4)*Tx(4,2)+x(2,5)*Tx(5,2)

  det = xTx(1,1)*xTx(2,2) - xTx(1,2)*xTx(2,1)
  a(1,1) = xTx(2,2)/det
  a(1,2) = -xTx(1,2)/det
  a(2,1) = -xTx(2,1)/det
  a(2,2) = xTx(1,1)/det

!  aTx = MATMUL(Tx,a)
  aTx(1,1)=Tx(1,1)*a(1,1)+Tx(1,2)*a(2,1) 
  aTx(2,1)=Tx(2,1)*a(1,1)+Tx(2,2)*a(2,1) 
  aTx(3,1)=Tx(3,1)*a(1,1)+Tx(3,2)*a(2,1) 
  aTx(4,1)=Tx(4,1)*a(1,1)+Tx(4,2)*a(2,1) 
  aTx(5,1)=Tx(5,1)*a(1,1)+Tx(5,2)*a(2,1) 
  aTx(1,2)=Tx(1,1)*a(1,2)+Tx(1,2)*a(2,2) 
  aTx(2,2)=Tx(2,1)*a(1,2)+Tx(2,2)*a(2,2) 
  aTx(3,2)=Tx(3,1)*a(1,2)+Tx(3,2)*a(2,2) 
  aTx(4,2)=Tx(4,1)*a(1,2)+Tx(4,2)*a(2,2) 
  aTx(5,2)=Tx(5,1)*a(1,2)+Tx(5,2)*a(2,2) 

!  beta = MATMUL(y,aTx)
  beta(1) = y(1)*aTx(1,1)+y(2)*aTx(2,1)+y(3)*aTx(3,1)+y(4)*aTx(4,1)+y(5)*aTx(5,1)
  beta(2) = y(1)*aTx(1,2)+y(2)*aTx(2,2)+y(3)*aTx(3,2)+y(4)*aTx(4,2)+y(5)*aTx(5,2)

!  beta2 = MATMUL(y2,aTx)
  beta2(1) = y2(1)*aTx(1,1)+y2(2)*aTx(2,1)+y2(3)*aTx(3,1)+y2(4)*aTx(4,1)+y2(5)*aTx(5,1)
  beta2(2) = y2(1)*aTx(1,2)+y2(2)*aTx(2,2)+y2(3)*aTx(3,2)+y2(4)*aTx(4,2)+y2(5)*aTx(5,2)

!============== finished least squares regression on doy**3 ==============

  if ((di .lt. 3) .or. (di .gt. 72)) tt = tt + 365.

  eqtime=(beta(1) + beta(2)*tt**3)/60.
  decang=beta2(1) + beta2(2)*tt**3
  latsun=decang

  ut=time
  noon=12.-lon/15.                             ! universal time of noon

  lonsun=-15.*(ut-12.+eqtime)

  t0=(90.-lat)*pi/180.
  t1=(90.-latsun)*pi/180.

  p0=lon*pi/180.
  p1=lonsun*pi/180.

  zz=cos(t0)*cos(t1)+sin(t0)*sin(t1)*cos(p1-p0)
!  zz2=sin(t0)*sin(t1)+cos(t0)*cos(t1)*cos(p1-p0)
  xx=sin(t1)*sin(p1-p0)
  yy=sin(t0)*cos(t1)-cos(t0)*sin(t1)*cos(p1-p0)

  sun_zenith=90-acos(zz)/(pi/180)
  sun_azimuth=atan2(xx,yy)/(pi/180)

  return
end subroutine zensun
