subroutine gengrid_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gengrid_vars
!   prgmmr: treadon          org: np23                date: 2003-11-24 
!
! abstract: initialize and define grid related variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-13  kleist, documentation and cleanup
!   2004-08-04  treadon - add only on use declarations; add intent in/out
!   2006-04-12  treadon - remove nsig,sigl (not used)
!   2006-10-17  kleist  - add coriolis parameter
!   2010-01-12  treadon - add hires_b section
!   2010-03-10  lueken - remove hires_b section
!   2010-03-31  treadon - replace specmod components with sp_a structure
!
!   input argument list:
!
!   output argument list:
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: sinlon,coslon,region_lat,rbs2,&
       rlons,rlats,corlats,nlon,nlat,regional,wgtlats,sp_a
  use constants, only: zero,half,one,four,pi,two,omega
  implicit none

! Declare local variables
  integer(i_kind) i,i1
  real(r_kind) anlon,dlon,pih

  if(regional) then
! This is regional run, so transfer previously defined regional lats, lons
     do i=1,nlon
        rlons(i)=i
     end do

     do i=1,nlat
        rlats(i)=i
     end do

     i1=nlon/4
     do i=1,nlat
        wgtlats(i)=zero
        rbs2(i)=one/cos(region_lat(i,i1))**2
     end do

  else

! This is global run, so get global lons, lats, wgtlats

! Set local constants
     anlon=float(nlon)
     pih=half*pi
     dlon=two*pi/anlon

! Load grid lat,lon arrays.  rbs2 is used in pcp.
     do i=1,nlon
        rlons(i)=float(i-1)*dlon
        coslon(i)=cos(rlons(i))
        sinlon(i)=sin(rlons(i))
     end do

     do i=sp_a%jb,sp_a%je
        i1=i+1
        rlats(i1)=-asin(sp_a%slat(i))
        rbs2(i1)=one/cos(rlats(i1))**2
        wgtlats(i1)=sp_a%wlat(i)

        i1=nlat-i
        rlats(i1)=asin(sp_a%slat(i))
        rbs2(i1)=one/cos(rlats(i1))**2
        wgtlats(i1)=sp_a%wlat(i)
     end do

     rlats(1)=-pih
     rlats(nlat)=pih
   
     wgtlats(1)=zero
     wgtlats(nlat)=zero

     rbs2(1)=zero
     rbs2(nlat)=zero

     do i=1,nlat
        corlats(i)=two*omega*sin(rlats(i))
     end do

  end if  !end if global

  return
end subroutine gengrid_vars
