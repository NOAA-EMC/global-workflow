module deter_sfc_mod
!$$$ module documentation block
!           .      .    .                                       .
! module:   deter_sfc_mod
!   prgmmr: lueken
!
! abstract: subroutine used to determine land surface type
!
! program history log:
!   2011-08-01  lueken - Moved all land surface type subroutines to new module
!   2013-01-23  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!
! subroutines included:
!   sub deter_sfc
!   sub deter_sfc_type
!   sub deter_sfc2
!   sub deter_sfc_fov
!   sub deter_sfc_amsre_low
!   sub deter_zsfc_model
!   sub reduce2full
!   sub init_sfc
!   sub time_int_sfc
!   sub accum_sfc
!   sub calc_sfc
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use satthin, only: sno_full,isli_full,sst_full,soil_moi_full, &
      soil_temp_full,soil_type_full,veg_frac_full,veg_type_full, &
      fact10_full,zs_full,sfc_rough_full,zs_full_gfs
  use constants, only: zero,one,two,one_tenth,deg2rad,rad2deg
  use gridmod, only: nlat,nlon,regional,tll2xy,nlat_sfc,nlon_sfc,rlats_sfc,rlons_sfc, &
      rlats,rlons,dx_gfs,txy2ll,lpl_gfs
  use guess_grids, only: nfldsfc,hrdifsfc,ntguessfc
  use calc_fov_crosstrk, only: npoly, fov_ellipse_crosstrk, inside_fov_crosstrk
  use calc_fov_conical, only: fov_ellipse_conical, inside_fov_conical
  implicit none

! Set default to private
  private
! Set passed variables to public
  public deter_sfc
  public deter_sfc_type
  public deter_sfc2
  public deter_sfc_fov
  public deter_sfc_amsre_low
  public deter_zsfc_model

contains

subroutine deter_sfc(alat,alon,dlat_earth,dlon_earth,obstime,isflg, &
       idomsfc,sfcpct,ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_sfc                     determine land surface type
!   prgmmr: derber           org: np2                date: 2005-01-27
!
! abstract:  determines land surface type based on surrounding land
!            surface types
!
! program history log:
!   2005-01-27 derber
!   2005-03-03 treadon - add implicit none, define zero
!   2006-02-01 parrish  - change names of sno,isli,sst
!
!   input argument list:
!     alat
!     alon
!     obstime- observation time relative to analysis time
!     dlat_earth
!     dlon_earth
!
!   output argument list:
!      isflg    - surface flag
!                 0 sea
!                 1 land
!                 2 sea ice
!                 3 snow
!                 4 mixed
!      sfcpct(0:3)- percentage of 4 surface types
!                 (0) - sea percentage
!                 (1) - land percentage
!                 (2) - sea ice percentage
!                 (3) - snow percentage
!      tsavg - sea surface temperature
!      idomsfc
!      ts
!      dfcr
!      vty,vfr,sty,stp,sm,sn,zz,ff10
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

     implicit none

     real(r_kind)               ,intent(in   ) :: dlat_earth,dlon_earth,obstime,alat,alon
     integer(i_kind)            ,intent(  out) :: isflg,idomsfc
     real(r_kind),dimension(0:3),intent(  out) :: sfcpct
     real(r_kind),dimension(0:3),intent(  out) :: ts
     real(r_kind)               ,intent(  out) :: tsavg,sfcr
     real(r_kind)               ,intent(  out) :: vty,vfr,sty,stp,sm,sn,zz,ff10

     real(r_kind),parameter:: minsnow=one_tenth

     integer(i_kind) istyp00,istyp01,istyp10,istyp11
     integer(i_kind):: itsfc,itsfcp
     integer(i_kind):: ix,iy,ixp,iyp,j
     real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11,dtsfc,dtsfcp,wgtmin
     real(r_kind):: sno00,sno01,sno10,sno11,dlat,dlon
     real(r_kind):: sst00,sst01,sst10,sst11
     real(r_kind),dimension(0:3)::wgtavg

!  First do surface field since it is on model grid
     iy=int(alon); ix=int(alat)
     dy  =alon-iy; dx  =alat-ix
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=dx*dy

     ix=min(max(1,ix),nlat); iy=min(max(0,iy),nlon)
     ixp=min(nlat,ix+1); iyp=iy+1
     if(iy==0) iy=nlon
     if(iyp==nlon+1) iyp=1

!    Interpolate fields which only vary in space (no time component)
!       zz   = surface height
     zz   = zs_full(ix,iy) *w00 + zs_full(ixp,iy) *w10 + &
            zs_full(ix,iyp)*w01 + zs_full(ixp,iyp)*w11

     if(regional)then
!       call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        dlat=alat
        dlon=alon
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd1(dlat,rlats_sfc,nlat_sfc,1)
        call grdcrd1(dlon,rlons_sfc,nlon_sfc,1)
     end if
     iy=int(dlon); ix=int(dlat)
     dy  =dlon-iy; dx  =dlat-ix
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=one-w00-w10-w01

     ix=min(max(1,ix),nlat_sfc); iy=min(max(0,iy),nlon_sfc)
     ixp=min(nlat_sfc,ix+1); iyp=iy+1
     if(iy==0) iy=nlon_sfc
     if(iyp==nlon_sfc+1) iyp=1

!    Get time interpolation factors for surface files
     if(obstime > hrdifsfc(1) .and. obstime <= hrdifsfc(nfldsfc))then
        do j=1,nfldsfc-1
           if(obstime > hrdifsfc(j) .and. obstime <= hrdifsfc(j+1))then
              itsfc=j
              itsfcp=j+1
              dtsfc=(hrdifsfc(j+1)-obstime)/(hrdifsfc(j+1)-hrdifsfc(j))
           end if
        end do
     else if(obstime <=hrdifsfc(1))then
        itsfc=1
        itsfcp=1
        dtsfc=one
     else
        itsfc=nfldsfc
        itsfcp=nfldsfc
        dtsfc=one
     end if
     dtsfcp=one-dtsfc

!    Set surface type flag.  Begin by assuming obs over ice-free water

     istyp00 = isli_full(ix ,iy )
     istyp10 = isli_full(ixp,iy )
     istyp01 = isli_full(ix ,iyp)
     istyp11 = isli_full(ixp,iyp)

     sno00= sno_full(ix ,iy ,itsfc)*dtsfc+sno_full(ix ,iy ,itsfcp)*dtsfcp
     sno01= sno_full(ix ,iyp,itsfc)*dtsfc+sno_full(ix ,iyp,itsfcp)*dtsfcp
     sno10= sno_full(ixp,iy ,itsfc)*dtsfc+sno_full(ixp,iy ,itsfcp)*dtsfcp
     sno11= sno_full(ixp,iyp,itsfc)*dtsfc+sno_full(ixp,iyp,itsfcp)*dtsfcp

     sst00= sst_full(ix ,iy ,itsfc)*dtsfc+sst_full(ix ,iy ,itsfcp)*dtsfcp
     sst01= sst_full(ix ,iyp,itsfc)*dtsfc+sst_full(ix ,iyp,itsfcp)*dtsfcp
     sst10= sst_full(ixp,iy ,itsfc)*dtsfc+sst_full(ixp,iy ,itsfcp)*dtsfcp
     sst11= sst_full(ixp,iyp,itsfc)*dtsfc+sst_full(ixp,iyp,itsfcp)*dtsfcp

!    Interpolate sst to obs location

     tsavg=sst00*w00+sst10*w10+sst01*w01+sst11*w11

     if(istyp00 >=1 .and. sno00 > minsnow)istyp00 = 3
     if(istyp01 >=1 .and. sno01 > minsnow)istyp01 = 3
     if(istyp10 >=1 .and. sno10 > minsnow)istyp10 = 3
     if(istyp11 >=1 .and. sno11 > minsnow)istyp11 = 3

     sfcpct = zero
     sfcpct(istyp00)=sfcpct(istyp00)+w00
     sfcpct(istyp01)=sfcpct(istyp01)+w01
     sfcpct(istyp10)=sfcpct(istyp10)+w10
     sfcpct(istyp11)=sfcpct(istyp11)+w11

     isflg = 0
     if(sfcpct(0) > 0.99_r_kind)then
        isflg = 0
     else if(sfcpct(1) > 0.99_r_kind)then
        isflg = 1
     else if(sfcpct(2) > 0.99_r_kind)then
        isflg = 2
     else if(sfcpct(3) > 0.99_r_kind)then
        isflg = 3
     else
        isflg = 4
     end if

!       vty  = vegetation type
!       sty  = soil type

     ts(0:3)=zero
     wgtavg(0:3)=zero
     vfr=zero
     stp=zero
     sty=zero
     vty=zero
     sm=zero
     sn=zero
     idomsfc=isli_full(ix ,iy )
     wgtmin = w00
     if(istyp00 == 1)then
        vty  = veg_type_full(ix ,iy)
        sty  = soil_type_full(ix ,iy)
        wgtavg(1) = wgtavg(1) + w00
        ts(1)=ts(1)+w00*sst00
        vfr  =vfr  +w00*(veg_frac_full(ix ,iy ,itsfc ) *dtsfc+   &
                         veg_frac_full(ix ,iy ,itsfcp) *dtsfcp)
        stp  =stp  +w00*(soil_temp_full(ix ,iy ,itsfc )*dtsfc+   &
                         soil_temp_full(ix ,iy ,itsfcp)*dtsfcp)
        sm   =sm   +w00*(soil_moi_full(ix ,iy ,itsfc ) *dtsfc+   &
                         soil_moi_full(ix ,iy ,itsfcp) *dtsfcp)
     else if(istyp00 == 2)then
        wgtavg(2) = wgtavg(2) + w00
        ts(2)=ts(2)+w00*sst00
     else if(istyp00 == 3)then
        wgtavg(3) = wgtavg(3) + w00
        ts(3)=ts(3)+w00*sst00
        sn = sn + w00*sno00
     else
        wgtavg(0) = wgtavg(0) + w00
        ts(0)=ts(0)+w00*sst00
     end if
     if(istyp01 == 1)then
        if(wgtmin < w01 .or. (vty == zero .and. sty == zero))then
           vty  = veg_type_full(ix ,iyp)
           sty  = soil_type_full(ix ,iyp)
        end if
        wgtavg(1) = wgtavg(1) + w01
        ts(1)=ts(1)+w01*sst01
        vfr  =vfr  +w01*(veg_frac_full(ix ,iyp,itsfc ) *dtsfc+   &
                         veg_frac_full(ix ,iyp,itsfcp) *dtsfcp)
        stp  =stp  +w01*(soil_temp_full(ix ,iyp,itsfc )*dtsfc+   &
                         soil_temp_full(ix ,iyp,itsfcp)*dtsfcp)
        sm   =sm   +w01*(soil_moi_full(ix ,iyp,itsfc ) *dtsfc+   &
                         soil_moi_full(ix ,iyp,itsfcp) *dtsfcp)
     else if(istyp01 == 2)then
        wgtavg(2) = wgtavg(2) + w01
        ts(2)=ts(2)+w01*sst01
     else if(istyp01 == 3)then
        wgtavg(3) = wgtavg(3) + w01
        ts(3)=ts(3)+w01*sst01
        sn = sn + w01*sno01
     else
        wgtavg(0) = wgtavg(0) + w01
        ts(0)=ts(0)+w01*sst01
     end if
     if(wgtmin < w01)then
        idomsfc=isli_full(ix ,iyp)
        wgtmin = w01
     end if
     if(istyp10 == 1)then
        if(wgtmin < w10 .or. (vty == zero .and. sty == zero))then
           vty  = veg_type_full(ixp,iy)
           sty  = soil_type_full(ixp,iy)
        end if
        wgtavg(1) = wgtavg(1) + w10
        ts(1)=ts(1)+w10*sst10
        vfr  =vfr  +w10*(veg_frac_full(ixp,iy ,itsfc ) *dtsfc+   &
                         veg_frac_full(ixp,iy ,itsfcp) *dtsfcp)
        stp  =stp  +w10*(soil_temp_full(ixp,iy ,itsfc )*dtsfc+   &
                         soil_temp_full(ixp,iy ,itsfcp)*dtsfcp)
        sm   =sm   +w10*(soil_moi_full(ixp,iy ,itsfc ) *dtsfc+   &
                         soil_moi_full(ixp,iy ,itsfcp) *dtsfcp)
     else if(istyp10 == 2)then
        wgtavg(2) = wgtavg(2) + w10
        ts(2)=ts(2)+w10*sst10
     else if(istyp10 == 3)then
        wgtavg(3) = wgtavg(3) + w10
        ts(3)=ts(3)+w10*sst10
        sn = sn + w10*sno10
     else
        wgtavg(0) = wgtavg(0) + w10
        ts(0)=ts(0)+w10*sst10
     end if
     if(wgtmin < w10)then
        idomsfc=isli_full(ixp,iy )
        wgtmin = w10
     end if
     if(istyp11 == 1)then
        if(wgtmin < w11 .or. (vty == zero .and. sty == zero))then
           vty  = veg_type_full(ixp,iyp)
           sty  = soil_type_full(ixp,iyp)
        endif
        wgtavg(1) = wgtavg(1) + w11
        ts(1)=ts(1)+w11*sst11
        vfr  =vfr  +w11*(veg_frac_full(ixp,iyp,itsfc ) *dtsfc+   &
                         veg_frac_full(ixp,iyp,itsfcp) *dtsfcp)
        stp  =stp  +w11*(soil_temp_full(ixp,iyp,itsfc )*dtsfc+   &
                         soil_temp_full(ixp,iyp,itsfcp)*dtsfcp)
        sm   =sm   +w11*(soil_moi_full(ixp,iyp,itsfc ) *dtsfc+   &
                         soil_moi_full(ixp,iyp,itsfcp) *dtsfcp)
     else if(istyp11 == 2)then
        wgtavg(2) = wgtavg(2) + w11
        ts(2)=ts(2)+w11*sst11
     else if(istyp11 == 3)then
        wgtavg(3) = wgtavg(3) + w11
        ts(3)=ts(3)+w11*sst11
        sn = sn + w11*sno11
     else
        wgtavg(0) = wgtavg(0) + w11
        ts(0)=ts(0)+w11*sst11
     end if
     if(wgtmin < w11)then
        idomsfc=isli_full(ixp,iyp)
        wgtmin = w11
     end if

     if(wgtavg(0) > zero)then
        ts(0) = ts(0)/wgtavg(0)
     else
        ts(0) = tsavg
     end if
     if(wgtavg(1) > zero)then
        ts(1) = ts(1)/wgtavg(1)
        sm = sm/wgtavg(1)
        vfr = vfr/wgtavg(1)
        stp = stp/wgtavg(1)
     else
        ts(1) = tsavg
        sm=one
     end if
     if(wgtavg(2) > zero)then
        ts(2) = ts(2)/wgtavg(2)
     else
        ts(2) = tsavg
     end if
     if(wgtavg(3) > zero)then
        ts(3) = ts(3)/wgtavg(3)
        sn = sn/wgtavg(3)
     else
        ts(3) = tsavg
     end if
!    ts(0)=max(ts(0),270._r_kind)
!    ts(2)=min(ts(2),280._r_kind)
!    ts(3)=min(ts(3),280._r_kind)

!    Space-time interpolation of fields from surface wind speed

     ff10=(fact10_full(ix ,iy ,itsfc )*w00+ &
           fact10_full(ixp,iy ,itsfc )*w10+ &
           fact10_full(ix ,iyp,itsfc )*w01+ &
           fact10_full(ixp,iyp,itsfc )*w11)*dtsfc + &
          (fact10_full(ix ,iy ,itsfcp)*w00+ &
           fact10_full(ixp,iy ,itsfcp)*w10+ &
           fact10_full(ix ,iyp,itsfcp)*w01+ &
           fact10_full(ixp,iyp,itsfcp)*w11)*dtsfcp

     sfcr=(sfc_rough_full(ix ,iy ,itsfc )*w00+ &
           sfc_rough_full(ixp,iy ,itsfc )*w10+ &
           sfc_rough_full(ix ,iyp,itsfc )*w01+ &
           sfc_rough_full(ixp,iyp,itsfc )*w11)*dtsfc + &
          (sfc_rough_full(ix ,iy ,itsfcp)*w00+ &
           sfc_rough_full(ixp,iy ,itsfcp)*w10+ &
           sfc_rough_full(ix ,iyp,itsfcp)*w01+ &
           sfc_rough_full(ixp,iyp,itsfcp)*w11)*dtsfcp

     return
end subroutine deter_sfc

subroutine deter_sfc_type(dlat_earth,dlon_earth,obstime,isflg,tsavg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_sfc                     determine land surface type
!   prgmmr: derber           org: np2                date: 2005-01-27
!
! abstract:  determines land surface type based on surrounding land
!            surface types
!
! program history log:
!   2005-01-27 derber
!   2005-03-03 treadon - add implicit none, define zero
!   2006-02-01 parrish  - change names of sno,isli,sst
!
!   input argument list:
!     dlat
!     dlon
!     obstime
!
!   output argument list:
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
!     tsavg
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

     implicit none

     real(r_kind)   ,intent(in   ) :: dlat_earth,dlon_earth,obstime

     integer(i_kind),intent(  out) :: isflg
     real(r_kind)   ,intent(  out) :: tsavg

     logical outside
     integer(i_kind) istyp00,istyp01,istyp10,istyp11
     integer(i_kind):: ix,iy,ixp,iyp,j,itsfc,itsfcp
     real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11,dtsfc
     real(r_kind):: dtsfcp,dlat,dlon
     real(r_kind):: sst00,sst01,sst10,sst11
     real(r_kind):: sno00,sno01,sno10,sno11

     real(r_kind),parameter:: minsnow=one_tenth

     real(r_kind),dimension(0:3):: sfcpct

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd1(dlat,rlats_sfc,nlat_sfc,1)
        call grdcrd1(dlon,rlons_sfc,nlon_sfc,1)
     end if

     iy=int(dlon); ix=int(dlat)
     dy  =dlon-iy; dx  =dlat-ix
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=one-w00-w10-w01

     ix=min(max(1,ix),nlat_sfc); iy=min(max(0,iy),nlon_sfc)
     ixp=min(nlat_sfc,ix+1); iyp=iy+1
     if(iy==0) iy=nlon_sfc
     if(iyp==nlon_sfc+1) iyp=1

!    Get time interpolation factors for surface files
     if(obstime > hrdifsfc(1) .and. obstime <= hrdifsfc(nfldsfc))then
        do j=1,nfldsfc-1
           if(obstime > hrdifsfc(j) .and. obstime <= hrdifsfc(j+1))then
              itsfc=j
              itsfcp=j+1
              dtsfc=(hrdifsfc(j+1)-obstime)/(hrdifsfc(j+1)-hrdifsfc(j))
           end if
        end do
     else if(obstime <=hrdifsfc(1))then
        itsfc=1
        itsfcp=1
        dtsfc=one
     else
        itsfc=nfldsfc
        itsfcp=nfldsfc
        dtsfc=one
     end if
     dtsfcp=one-dtsfc

!    Set surface type flag.  Begin by assuming obs over ice-free water

     istyp00 = isli_full(ix ,iy )
     istyp10 = isli_full(ixp,iy )
     istyp01 = isli_full(ix ,iyp)
     istyp11 = isli_full(ixp,iyp)

     sno00= sno_full(ix ,iy ,itsfc)*dtsfc+sno_full(ix ,iy ,itsfcp)*dtsfcp
     sno01= sno_full(ix ,iyp,itsfc)*dtsfc+sno_full(ix ,iyp,itsfcp)*dtsfcp
     sno10= sno_full(ixp,iy ,itsfc)*dtsfc+sno_full(ixp,iy ,itsfcp)*dtsfcp
     sno11= sno_full(ixp,iyp,itsfc)*dtsfc+sno_full(ixp,iyp,itsfcp)*dtsfcp


     sst00= sst_full(ix ,iy ,itsfc)*dtsfc+sst_full(ix ,iy ,itsfcp)*dtsfcp
     sst01= sst_full(ix ,iyp,itsfc)*dtsfc+sst_full(ix ,iyp,itsfcp)*dtsfcp
     sst10= sst_full(ixp,iy ,itsfc)*dtsfc+sst_full(ixp,iy ,itsfcp)*dtsfcp
     sst11= sst_full(ixp,iyp,itsfc)*dtsfc+sst_full(ixp,iyp,itsfcp)*dtsfcp

!    Interpolate sst to obs location

     tsavg=sst00*w00+sst10*w10+sst01*w01+sst11*w11

     if(istyp00 >=1 .and. sno00 > minsnow)istyp00 = 3
     if(istyp01 >=1 .and. sno01 > minsnow)istyp01 = 3
     if(istyp10 >=1 .and. sno10 > minsnow)istyp10 = 3
     if(istyp11 >=1 .and. sno11 > minsnow)istyp11 = 3

     sfcpct = zero
     sfcpct(istyp00)=sfcpct(istyp00)+w00
     sfcpct(istyp01)=sfcpct(istyp01)+w01
     sfcpct(istyp10)=sfcpct(istyp10)+w10
     sfcpct(istyp11)=sfcpct(istyp11)+w11

     isflg = 0
     if(sfcpct(0) > 0.99_r_kind)then
        isflg = 0
     else if(sfcpct(1) > 0.99_r_kind)then
        isflg = 1
     else if(sfcpct(2) > 0.99_r_kind)then
        isflg = 2
     else if(sfcpct(3) > 0.99_r_kind)then
        isflg = 3
     else
        isflg = 4
     end if
     return
end subroutine deter_sfc_type

subroutine deter_sfc2(dlat_earth,dlon_earth,obstime,idomsfc,tsavg,ff10,sfcr,zz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_sfc                     determine land surface type
!   prgmmr: derber           org: np2                date: 2005-01-27
!
! abstract:  determines land surface type based on surrounding land
!            surface types
!
! program history log:
!   2005-01-27 derber
!   2005-03-03 treadon - add implicit none, define zero
!   2006-02-01 parrish  - change names of sno,isli,sst
!   2010-12-05 pondeca - add local terrain elevation zz as
!                        optional output variable
!
!   input argument list:
!     dlat_earth
!     dlon_earth
!     obstime- observation time relative to analysis time
!
!   output argument list:
!     tsavg - sea surface temperature
!     idomsfc
!     sfcr
!     ff10
!     zz
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

     implicit none

     real(r_kind)   ,intent(in   ) :: dlat_earth,dlon_earth,obstime

     integer(i_kind),intent(  out) :: idomsfc
     real(r_kind)   ,intent(  out) :: tsavg,sfcr
     real(r_kind)   ,intent(  out) :: ff10
     real(r_kind),optional,intent(  out) :: zz

     integer(i_kind):: itsfc,itsfcp
     integer(i_kind):: ix,iy,ixp,iyp,j
     real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11,dtsfc,dtsfcp,wgtmin
     real(r_kind):: sst00,sst01,sst10,sst11,dlat,dlon
     logical outside

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd1(dlat,rlats_sfc,nlat_sfc,1)
        call grdcrd1(dlon,rlons_sfc,nlon_sfc,1)
     end if

     iy=int(dlon); ix=int(dlat)
     dy  =dlon-iy; dx  =dlat-ix
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=dx*dy

     ix=min(max(1,ix),nlat_sfc); iy=min(max(0,iy),nlon_sfc)
     ixp=min(nlat_sfc,ix+1); iyp=iy+1
     if(iy==0) iy=nlon_sfc
     if(iyp==nlon_sfc+1) iyp=1


!    Get time interpolation factors for surface files
     if(obstime > hrdifsfc(1) .and. obstime <= hrdifsfc(nfldsfc))then
        do j=1,nfldsfc-1
           if(obstime > hrdifsfc(j) .and. obstime <= hrdifsfc(j+1))then
              itsfc=j
              itsfcp=j+1
              dtsfc=(hrdifsfc(j+1)-obstime)/(hrdifsfc(j+1)-hrdifsfc(j))
           end if
        end do
     else if(obstime <=hrdifsfc(1))then
        itsfc=1
        itsfcp=1
        dtsfc=one
     else
        itsfc=nfldsfc
        itsfcp=nfldsfc
        dtsfc=one
     end if
     dtsfcp=one-dtsfc

     sst00= sst_full(ix ,iy ,itsfc)*dtsfc+sst_full(ix ,iy ,itsfcp)*dtsfcp
     sst01= sst_full(ix ,iyp,itsfc)*dtsfc+sst_full(ix ,iyp,itsfcp)*dtsfcp
     sst10= sst_full(ixp,iy ,itsfc)*dtsfc+sst_full(ixp,iy ,itsfcp)*dtsfcp
     sst11= sst_full(ixp,iyp,itsfc)*dtsfc+sst_full(ixp,iyp,itsfcp)*dtsfcp

!    Interpolate sst to obs location

     tsavg=sst00*w00+sst10*w10+sst01*w01+sst11*w11

     idomsfc=isli_full(ix ,iy )
     wgtmin = w00
     if(wgtmin < w01 )then
        idomsfc=isli_full(ix ,iyp)
        wgtmin = w01
     end if
     if(wgtmin < w10)then
        idomsfc=isli_full(ixp,iy )
        wgtmin = w10
     end if
     if(wgtmin < w11)then
        idomsfc=isli_full(ixp,iyp)
        wgtmin = w11
     end if
     if((isli_full(ix ,iy ) /= isli_full(ix ,iyp)) .or. &
        (isli_full(ix ,iy ) /= isli_full(ixp,iy )) .or. &
        (isli_full(ix ,iy ) /= isli_full(ixp,iyp)) .or. &
        (isli_full(ixp,iy ) /= isli_full(ix ,iyp)) .or. &
        (isli_full(ixp,iy ) /= isli_full(ixp,iyp)) .or. &
        (isli_full(ix ,iyp) /= isli_full(ixp,iyp)) ) idomsfc = idomsfc+3

!    Space-time interpolation of fields from surface wind speed

     ff10=(fact10_full(ix ,iy ,itsfc )*w00+ &
           fact10_full(ixp,iy ,itsfc )*w10+ &
           fact10_full(ix ,iyp,itsfc )*w01+ &
           fact10_full(ixp,iyp,itsfc )*w11)*dtsfc + &
          (fact10_full(ix ,iy ,itsfcp)*w00+ &
           fact10_full(ixp,iy ,itsfcp)*w10+ &
           fact10_full(ix ,iyp,itsfcp)*w01+ &
           fact10_full(ixp,iyp,itsfcp)*w11)*dtsfcp

     sfcr=(sfc_rough_full(ix ,iy ,itsfc )*w00+ &
           sfc_rough_full(ixp,iy ,itsfc )*w10+ &
           sfc_rough_full(ix ,iyp,itsfc )*w01+ &
           sfc_rough_full(ixp,iyp,itsfc )*w11)*dtsfc + &
          (sfc_rough_full(ix ,iy ,itsfcp)*w00+ &
           sfc_rough_full(ixp,iy ,itsfcp)*w10+ &
           sfc_rough_full(ix ,iyp,itsfcp)*w01+ &
           sfc_rough_full(ixp,iyp,itsfcp)*w11)*dtsfcp

     if(present(zz)) then
        if(regional)then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        else
           dlat=dlat_earth
           dlon=dlon_earth
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        end if

        iy=int(dlon); ix=int(dlat)
        dy  =dlon-iy; dx  =dlat-ix
        dx1 =one-dx;    dy1 =one-dy
        w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=dx*dy

        ix=min(max(1,ix),nlat); iy=min(max(0,iy),nlon)
        ixp=min(nlat,ix+1); iyp=iy+1
        if(iy==0) iy=nlon
        if(iyp==nlon+1) iyp=1

        zz   = zs_full(ix,iy) *w00 + zs_full(ixp,iy) *w10 + &
               zs_full(ix,iyp)*w01 + zs_full(ixp,iyp)*w11
     endif

     return
end subroutine deter_sfc2

subroutine deter_sfc_fov(fov_flag,ifov,instr,ichan,sat_aziang,dlat_earth_deg,&
                         dlon_earth_deg,expansion,obstime,isflg,idomsfc, &
                         sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_sfc_fov           determine surface characteristics
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  determines surface characteristics within a field of view
!            based on model information and the size/shape of the
!            field of view.
!
! program history log:
!   2008-11-04 gayno
!   2009-12-20 gayno - modify to use relative antenna power.
!
!   input argument list:
!     fov_flag        - is this a crosstrack or conical instrument?
!     ichan           - channel number - conical scanners only
!     ifov            - field of view number
!     instr           - instrument number
!     sat_aziang      - satellite azimuth angle
!     dlat_earth_deg  - latitude of fov center (degrees)
!     dlon_earth_deg  - longitude of fov center (degrees)
!     expansion       - fov expansion factor
!     obstime         - observation time
!
!   output argument list:
!     idomsfc  - dominate surface type
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
!     sfcpct(0:3)- percentage of 4 surface types
!                (0) - sea percentage
!                (1) - land percentage
!                (2) - sea ice percentage
!                (3) - snow percentage
!     vfr      - vegetation fraction
!     sty      - dominate soil type
!     vty      - dominate vegetation type
!     stp      - top layer soil temperature
!     sm       - top layer soil moisture
!     ff10     - wind factor
!     sfcr     - surface roughness lenght
!     zz       - terrain height
!     sn       - snow depth
!     ts       - skin temperature for each surface type
!     tsavg    - average skin temperature
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables.
  character(len=*), intent(in   ) :: fov_flag
  integer(i_kind) , intent(in   ) :: ifov, instr
  integer(i_kind) , intent(in   ) :: ichan
  real(r_kind)    , intent(in   ) :: dlat_earth_deg, dlon_earth_deg, sat_aziang
  real(r_kind)    , intent(in   ) :: expansion, obstime

  integer(i_kind) , intent(  out) :: isflg, idomsfc(1)
  real(r_kind)    , intent(  out) :: sfcpct(0:3), sty, vty, vfr, stp, sm
  real(r_kind)    , intent(  out) :: ff10, sfcr, zz, sn, ts(0:3), tsavg

! Declare local variables.
  integer(i_kind)              :: i, ii, iii, j, jj, jjj
  integer(i_kind)              :: is(npoly), js(npoly)
  integer(i_kind)              :: n, nearest_i, nearest_j
  integer(i_kind), allocatable :: max_i(:), min_i(:)
  integer(i_kind)              :: ifull, jstart, jend
  integer(i_kind)              :: itsfc, itsfcp
  integer(i_kind)              :: subgrid_lengths_x, subgrid_lengths_y
  logical                      :: outside
  real(r_kind)                 :: lats_edge_fov(npoly), lons_edge_fov(npoly)
  real(r_kind)                 :: lat_mdl, lon_mdl
  real(r_kind)                 :: lat_rad, lon_rad
  real(r_kind)                 :: dtsfc, dtsfcp
  real(r_kind)                 :: x, xstart, xend, y, ystart, yend
  real(r_kind)                 :: dx_fov, dx_fov_max, dy_fov, power
  real(r_kind)                 :: del, mid, rlon1, rlon2
  real(r_kind), allocatable    :: y_off(:), x_off(:), powerx(:,:)

  type surface2
     sequence
     real(r_kind) :: vfr
     real(r_kind) :: sfcr
     real(r_kind) :: sm
     real(r_kind) :: stp
     real(r_kind) :: ff10
     real(r_kind) :: sn
     real(r_kind) :: ts
  end type surface2

  type(surface2) :: sfc_mdl  ! holds time interpolated surface data

  type surface
     sequence
     real(r_kind) :: vfr
     real(r_kind) :: sm
     real(r_kind) :: stp
     real(r_kind) :: ff10
     real(r_kind) :: sfcr
     real(r_kind) :: zz
     real(r_kind) :: sn
     real(r_kind), dimension(0:3)  :: ts
     real(r_kind), dimension(0:3)  :: count
     real(r_kind), dimension(0:24) :: count_vty
     real(r_kind), dimension(0:16) :: count_sty
  end type surface

  type(surface) :: sfc_sum  ! holds sums during integration across fov

! Get time interpolation factors for surface files

  if(obstime > hrdifsfc(1) .and. obstime <= hrdifsfc(nfldsfc))then
     do j=1,nfldsfc-1
        if(obstime > hrdifsfc(j) .and. obstime <= hrdifsfc(j+1))then
           itsfc=j
           itsfcp=j+1
           dtsfc=(hrdifsfc(j+1)-obstime)/(hrdifsfc(j+1)-hrdifsfc(j))
        end if
     end do
  else if(obstime <=hrdifsfc(1))then
     itsfc=1
     itsfcp=1
     dtsfc=one
  else
     itsfc=nfldsfc
     itsfcp=nfldsfc
     dtsfc=one
  end if
  dtsfcp=one-dtsfc
! The algorithm that locates the fov breaks down if its crosses the pole.
! so, just assign surface characteristics using a nearest neighbor
! approach.

  if (abs(dlat_earth_deg) > 88.0_r_kind) then
!    print*,'USE NEAREST NEIGHBOR NEAR POLE'
     if (regional) then
        lat_rad = dlat_earth_deg*deg2rad
        lon_rad = dlon_earth_deg*deg2rad
        call tll2xy(lon_rad,lat_rad,x,y,outside)
        nearest_i = nint(x)
        nearest_j = nint(y)
     else
        y = dlat_earth_deg*deg2rad
        call grdcrd1(y,rlats_sfc,nlat_sfc,1)
        nearest_j = nint(y)
        jj = nearest_j
        if (jj > nlat_sfc/2) jj = nlat_sfc - nearest_j + 1
        x = (dlon_earth_deg/dx_gfs(jj)) + one
        nearest_i = nint(x)
        call reduce2full(nearest_i,nearest_j,ifull)
        nearest_i = ifull
     end if
     call time_int_sfc(nearest_i,nearest_j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
     call init_sfc(sfc_sum)
     power = one
     call accum_sfc(nearest_i,nearest_j,power,sfc_mdl,sfc_sum)
     call calc_sfc(sfc_sum,isflg,idomsfc,sfcpct,vfr,sty,vty,sm,stp,ff10,sfcr,zz,sn,ts,tsavg)
     return
  endif

! Determine the edge of the fov.  the fov is described by a polygon with
! with npoly-1 vertices.  the routine returns the lat/lon of the intersection
! of the vertices.  the size of the polygon is a function of the
! expansion factor.

  if (fov_flag=="crosstrk")then
     call fov_ellipse_crosstrk(ifov,instr,sat_aziang,dlat_earth_deg,dlon_earth_deg, &
                               lats_edge_fov,lons_edge_fov)
  elseif(fov_flag=="conical")then
     call fov_ellipse_conical(ichan,sat_aziang,dlat_earth_deg,dlon_earth_deg, &
                              lats_edge_fov,lons_edge_fov)
  endif

  if (regional) then
     xstart=999999._r_kind
     xend=-999999._r_kind
     ystart=999999._r_kind
     yend=-999999._r_kind
     do n = 1, npoly
        lat_rad = lats_edge_fov(n)*deg2rad
        lon_rad = lons_edge_fov(n)*deg2rad
        call tll2xy(lon_rad,lat_rad,x,y,outside)
! if any part of fov is outside grid, just set is/js to
! be the near grid point at fov center.  we already know the
! center grid point is inside the grid as that is checked
! in calling routine.
        if (outside) then
!       print*,"FOV ON EDGE OF GRID"
           lat_rad = dlat_earth_deg*deg2rad
           lon_rad = dlon_earth_deg*deg2rad
           call tll2xy(lon_rad,lat_rad,x,y,outside)
           js = nint(y)
           is = nint(x)
           exit
        endif
        xstart=min(xstart,x)
        xend=max(xend,x)
        ystart=min(ystart,y)
        yend=max(yend,y)
        is(n)=nint(x)
        js(n)=nint(y)
     enddo
     jstart=minval(js)
     jend=maxval(js)
     allocate (max_i(jstart:jend))
     allocate (min_i(jstart:jend))
     max_i = -999
     min_i = 999999
     do j = jstart, jend
        do n = 1, npoly
           if (js(n) == j) then
              max_i(j) = max(max_i(j),is(n))
              min_i(j) = min(min_i(j),is(n))
           endif
        enddo
     enddo
  else   ! global grid
!  Locate the fov on the model grid.  in the "j" direction, this is
!  based on the latitudinal extent of the fov.
     yend = maxval(lats_edge_fov)*deg2rad
     call grdcrd1(yend,rlats_sfc,nlat_sfc,1)
     ystart = minval(lats_edge_fov)*deg2rad
     call grdcrd1(ystart,rlats_sfc,nlat_sfc,1)
!  Note two extra rows are added for the n/s poles.
     jstart = nint(ystart)
     jstart = max(jstart,2)
     jend = nint(yend)
     jend = min(jend,nlat_sfc-1)
!  Locate the extent of the fov on the model grid in the "i" direction.  note, the
!  algorithm works on the reduced gaussian grid.  hence, the starting/ending
!  "i" indices are a function of "j".
     allocate (max_i(jstart:jend))
     allocate (min_i(jstart:jend))
     do j = jstart, jend
        jj = j
        if (jj > nlat_sfc/2) jj = nlat_sfc - j + 1
        x = (minval(lons_edge_fov)/dx_gfs(jj)) + one
        nearest_i = nint(x)
        min_i(j) = nearest_i
        x = (maxval(lons_edge_fov)/dx_gfs(jj)) + one
        nearest_i = nint(x)
        max_i(j) = nearest_i
     enddo
  end if  ! is this regional or global

! In this case, the fov is located completely within one grid
! point. this is common when the fov is small compared with
! the model grid resolution.

  if ((jstart == jend) .and. (max_i(jstart) == min_i(jstart))) then
!   print*,'ONLY ONE MODEL POINT WITHIN FOV'
     nearest_j = jstart
     nearest_i = max_i(jstart)
     if (.not. regional) then
        call reduce2full(nearest_i,nearest_j,ifull)
        nearest_i = ifull
     endif
     call time_int_sfc(nearest_i,nearest_j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
     call init_sfc(sfc_sum)
     power = one
     call accum_sfc(nearest_i,nearest_j,power,sfc_mdl,sfc_sum)
     call calc_sfc(sfc_sum,isflg,idomsfc,sfcpct,vfr,sty,vty,sm,stp,ff10,sfcr,zz,sn,ts,tsavg)
     deallocate(max_i,min_i)
     return
  endif

! Find the size of the fov in model grid units.  if the
! fov is small compared to the model grid, there is the
! possibility that there are no model grid boxes inside the fov.
! (a grid box is "inside" the fov if its center is within
! the fov.) this situation can be avoided by
! "chopping" the model grid boxes into smaller pieces, which
! is done below.  first, find the n/s size of the fov
! in grid units.

  if (regional) then
     dy_fov = abs(yend-ystart)
     dx_fov = abs(xend-xstart)
  else  ! global
     dy_fov = abs(yend-ystart)  ! n/s size of fov in grid units.
!  Find the e/w size of the fov in grid units.  note: the resolution
!  of the model decreases towards the poles.  use the max
!  value of model grid spacing in this calculation to make
!  the most conservative estimate as to whether or not to
!  "chop" the model grid boxes.
     dx_fov_max = zero
     do j = jstart,jend
        jj = j
        if (jj > nlat_sfc/2) jj = nlat_sfc - j + 1
        dx_fov_max = max(dx_fov_max, dx_gfs(jj))
     enddo
!  When taking the longitudinal difference, don't worry
!  about greenwich or the dateline as the fov code calculates
!  longitude relative to the center of the fov.
     rlon1 = maxval(lons_edge_fov)
     rlon2 = minval(lons_edge_fov)
     dx_fov = (rlon1-rlon2)/dx_fov_max
  end if  ! is this regional or global?

! if the fov is small compared to the model resolution, there
! is a possibility that there will be no model points located
! within the fov.  to prevent this, the model grid may be
! subdivided into smaller pieces.  this subdivision is
! done separately for each direction.

  subgrid_lengths_x = nint(one/dx_fov) + 1
  subgrid_lengths_y = nint(one/dy_fov) + 1

  99 continue

! If the fov is very small compared to the model grid, it
! is more computationally efficient to take a simple average.

  if (subgrid_lengths_x > 7 .or. subgrid_lengths_y > 7) then
!   print*,'FOV MUCH SMALLER THAN MODEL GRID POINTS, TAKE SIMPLE AVERAGE.'
     call init_sfc(sfc_sum)
     if (regional) then
        do j = jstart, jend
           do i = min_i(j), max_i(j)
              call time_int_sfc(i,j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
              power = one
              call accum_sfc(i,j,power,sfc_mdl,sfc_sum)
           enddo
        enddo
     else  ! global
        do j = jstart, jend
           do i = min_i(j), max_i(j)
              ii = i
              call reduce2full(ii,j,ifull)
              call time_int_sfc(ifull,j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
              power = one
              call accum_sfc(ifull,j,power,sfc_mdl,sfc_sum)
           enddo
        enddo
     endif
     call calc_sfc(sfc_sum,isflg,idomsfc,sfcpct,vfr,sty,vty,sm,stp,ff10,sfcr,zz,sn,ts,tsavg)
     deallocate(max_i,min_i)
     return
  endif

  mid = (float(subgrid_lengths_y)-one)/two + one
  del = one/ float(subgrid_lengths_y)

  allocate (y_off(subgrid_lengths_y))

  do i= 1, subgrid_lengths_y
     y_off(i) = (float(i)-mid)*del
  enddo

  mid = (float(subgrid_lengths_x)-one)/two + one
  del = one / float(subgrid_lengths_x)

  allocate (x_off(subgrid_lengths_x))
  do i= 1, subgrid_lengths_x
     x_off(i) = (float(i)-mid)*del
  enddo

! Determine the surface characteristics by integrating over the
! fov.

  call init_sfc(sfc_sum)

  if (regional) then
     do j = jstart, jend
        do i = min_i(j), max_i(j)
           call time_int_sfc(i,j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
           do jjj = 1, subgrid_lengths_y
              y = float(j) + y_off(jjj)
              do iii = 1, subgrid_lengths_x
                 x = float(i) + x_off(iii)
                 call txy2ll(x,y,lon_rad,lat_rad)
                 lat_mdl = lat_rad*rad2deg
                 lon_mdl = lon_rad*rad2deg
                 if (lon_mdl < zero) lon_mdl = lon_mdl + 360._r_kind
                 if (lon_mdl >= 360._r_kind) lon_mdl = lon_mdl - 360._r_kind
                 if (fov_flag=="crosstrk")then
                    call inside_fov_crosstrk(instr,ifov,sat_aziang, &
                                            dlat_earth_deg,dlon_earth_deg, &
                                            lat_mdl,    lon_mdl,  &
                                            expansion, ichan, power )
                 elseif (fov_flag=="conical")then
                    call inside_fov_conical(instr,ichan,sat_aziang, &
                                           dlat_earth_deg,dlon_earth_deg,&
                                           lat_mdl,    lon_mdl,  &
                                           expansion, power )
                 endif
                 call accum_sfc(i,j,power,sfc_mdl,sfc_sum)
              enddo
           enddo
        enddo
     enddo
  else
     allocate(powerx(subgrid_lengths_x,subgrid_lengths_y))
     do j = jstart, jend
        jj = j
        if (j > nlat_sfc/2) jj = nlat_sfc - j + 1
        do i = min_i(j), max_i(j)
           call reduce2full(i,j,ifull)
           call time_int_sfc(ifull,j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
!$omp parallel do  schedule(dynamic,1)private(jjj,iii,lat_mdl,lon_mdl)
           do jjj = 1, subgrid_lengths_y
              if (y_off(jjj) >= zero) then
                 lat_mdl = (one-y_off(jjj))*rlats_sfc(j)+y_off(jjj)*rlats_sfc(j+1)
              else
                 lat_mdl = (one+y_off(jjj))*rlats_sfc(j)-y_off(jjj)*rlats_sfc(j-1)
              endif
              lat_mdl = lat_mdl * rad2deg
              do iii = 1, subgrid_lengths_x
!           Note, near greenwich, "i" index may be out of range.  that is
!           ok here when calculating longitude even if the value is
!           greater than 360. the ellipse code works from longitude relative
!           to the center of the fov.
                 lon_mdl = (float(i)+x_off(iii) - one) * dx_gfs(jj)
                 if (fov_flag=="crosstrk")then
                    call inside_fov_crosstrk(instr,ifov,sat_aziang, &
                                            dlat_earth_deg,dlon_earth_deg, &
                                            lat_mdl,    lon_mdl,  &
                                            expansion, ichan, powerx(iii,jjj) )
                 elseif (fov_flag=="conical")then
                    call inside_fov_conical(instr,ichan,sat_aziang, &
                                           dlat_earth_deg,dlon_earth_deg,&
                                           lat_mdl,    lon_mdl,  &
                                           expansion, powerx(iii,jjj) )
                 endif
              enddo
           enddo
           do jjj = 1, subgrid_lengths_y
              do iii = 1, subgrid_lengths_x
                 call accum_sfc(ifull,j,powerx(iii,jjj),sfc_mdl,sfc_sum)
              enddo
           enddo
        enddo
     enddo
     deallocate(powerx)
  endif  ! regional or global
  deallocate (x_off, y_off)

! If there were no model points within the fov, the model points need to be
! "chopped" into smaller pieces.

  if (sum(sfc_sum%count) == zero) then
     close(9)
     subgrid_lengths_x = subgrid_lengths_x + 1
     subgrid_lengths_y = subgrid_lengths_y + 1
!    print*,'NO GRID POINTS INSIDE FOV, CHOP MODEL BOX INTO FINER PIECES',subgrid_lengths_x,subgrid_lengths_y
     goto 99
  else
     call calc_sfc(sfc_sum,isflg,idomsfc,sfcpct,vfr,sty,vty,sm,stp,ff10,sfcr,zz,sn,ts,tsavg)
  endif

  deallocate (max_i, min_i)

  return
end subroutine deter_sfc_fov

subroutine deter_sfc_amsre_low(dlat_earth,dlon_earth,isflg,sfcpct)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_sfc_amsre_low           determine land surface type
!   prgmmr: kazumori          org: np2                date: 2005-10-20
!
! abstract:  determines land surface type based on surrounding land
!            surface types for AMSR-E large FOV observation
!
! program history log:
!   2005-10-20 kazumori - refered from ( subroutine deter_sfc )
!   2006-02-01 parrish  - change names of sno,isli,sst
!   2008-05-28 safford  - rm unused vars
!
!   input argument list:
!     dlat_earth   - latitude
!     dlon_earth   - longitude
!
!   output argument list:
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
!      sfcpct(0:3)- percentage of 4 surface types
!                 (0) - sea percentage
!                 (1) - land percentage
!                 (2) - sea ice percentage
!                 (3) - snow percentage
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   implicit none

   real(r_kind)               ,intent(in   ) :: dlat_earth,dlon_earth
   integer(i_kind)            ,intent(  out) :: isflg
   real(r_kind),dimension(0:3),intent(  out) :: sfcpct

   integer(i_kind) jsli,it
   integer(i_kind):: klat1,klon1,klatp1,klonp1
   real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11
   real(r_kind) :: dlat,dlon
   logical :: outside
   integer(i_kind):: klat2,klon2,klatp2,klonp2

!
!  For interpolation, we usually use o points (4points for land sea decision)
!  In case of lowfreq channel (Large FOV), add the check of x points(8 points)
!                                          (klatp2,klon1),(klatp2,klonp1)
!       ---#---x---x---#---  klatp2        (klatp1,klon2),(klatp1,klonp2)
!          |   |   |   |                   (klat1,klon2),(klat1,klonp2)
!       ---x---o---o---x---  klatp1        (klat2,klon1),(klat2,klonp1)
!          |   | + |   |
!       ---x---o---o---x---  klat1
!          |   |   |   |
!       ---#---x---x---#---  klat2
!            klon1   klonp2
!       klon2    klonp1
!
!  In total, 12 points are used to make mean sst and sfc percentage.
!
     it=ntguessfc

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd1(dlat,rlats_sfc,nlat_sfc,1)
        call grdcrd1(dlon,rlons_sfc,nlon_sfc,1)
     end if

     klon1=int(dlon); klat1=int(dlat)
     dx  =dlon-klon1; dy  =dlat-klat1
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

     klat1=min(max(1,klat1),nlat_sfc); klon1=min(max(0,klon1),nlon_sfc)
     if(klon1==0) klon1=nlon_sfc
     klatp1=min(nlat_sfc,klat1+1); klonp1=klon1+1
     if(klonp1==nlon_sfc+1) klonp1=1
     klonp2 = klonp1+1
     if(klonp2==nlon_sfc+1) klonp2=1
     klon2=klon1-1
     if(klon2==0)klon2=nlon_sfc
     klat2=max(1,klat1-1)
     klatp2=min(nlat_sfc,klatp1+1)

!    Set surface type flag.  Begin by assuming obs over ice-free water

     sfcpct = zero

     jsli = isli_full(klat1 ,klon1 )
     if(sno_full(klat1 ,klon1 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klatp1,klon1 )
     if(sno_full(klatp1 ,klon1 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klat1 ,klonp1)
     if(sno_full(klat1 ,klonp1 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klatp1,klonp1)
     if(sno_full(klatp1 ,klonp1 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klatp2,klon1)
     if(sno_full(klatp2 ,klon1 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klatp2,klonp1)
     if(sno_full(klatp2 ,klonp1 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klatp1,klon2)
     if(sno_full(klatp1 ,klon2 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klatp1,klonp2)
     if(sno_full(klatp1 ,klonp2 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klat1,klon2)
     if(sno_full(klat1 ,klon2 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klat1,klonp2)
     if(sno_full(klat1 ,klonp2 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klat2,klon1)
     if(sno_full(klat2 ,klon1 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     jsli = isli_full(klat2,klonp1)
     if(sno_full(klat2 ,klonp1 ,it) > one .and. jsli == 1)jsli=3
     sfcpct(jsli)=sfcpct(jsli)+one

     sfcpct=sfcpct/12.0_r_kind

!     sfcpct(3)=min(sfcpct(3),sfcpct(1))
!     sfcpct(1)=max(zero,sfcpct(1)-sfcpct(3))

     isflg = 0
     if(sfcpct(0) > 0.99_r_kind)then
        isflg = 0
     else if(sfcpct(1) > 0.99_r_kind)then
        isflg = 1
     else if(sfcpct(2) > 0.99_r_kind)then
        isflg = 2
     else if(sfcpct(3) > 0.99_r_kind)then
        isflg = 3
     else
        isflg = 4
     end if

     return

   end subroutine deter_sfc_amsre_low

subroutine deter_zsfc_model(dlat,dlon,zsfc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_zsfc_model         determine model sfc elevation
!   prgmmr: parrish          org: np2                date: 2006-05-23
!
! abstract:  determines model sfc elevation
!
! program history log:
!   2006-05-23 parrish
!
!   input argument list:
!     dlat   - grid relative latitude
!     dlon   - grid relative longitude
!
!   output argument list:
!     zsfc     - model surface elevation (meters)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

  real(r_kind),intent(in   ) :: dlat,dlon
  real(r_kind),intent(  out) :: zsfc

  integer(i_kind):: klat1,klon1,klatp1,klonp1
  real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11
 
  klon1=int(dlon); klat1=int(dlat)
  dx  =dlon-klon1; dy  =dlat-klat1
  dx1 =one-dx;    dy1 =one-dy
  w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy
 
  klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
  if(klon1==0) klon1=nlon
  klatp1=min(nlat,klat1+1); klonp1=klon1+1
  if(klonp1==nlon+1) klonp1=1

! Interpolate zsfc to obs location
  zsfc=w00*zs_full(klat1,klon1 ) + w10*zs_full(klatp1,klon1 ) + &
       w01*zs_full(klat1,klonp1) + w11*zs_full(klatp1,klonp1)
 
  return
end subroutine deter_zsfc_model

subroutine reduce2full(ireduce, j, ifull)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    reduce2full        find "i" index on "full" gfs grid
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  for a given "i" index on the gfs "reduced" grid, find
!            the corresponding "i" index on the "full" grid.
!
! program history log:
!   2008-11-04 gayno
!
!   input argument list:
!     ireduce    - "i" index on reduced gfs grid
!     j          - "j" index (same for both grids)
!
!   output argument list:
!     ifull      - "i" index on full gfs grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables.
  integer(i_kind), intent(in   ) :: ireduce, j
  integer(i_kind), intent(  out) :: ifull

! Declare local variables.
  integer(i_kind)              :: ii, jj, m1, m2
  real(r_kind)                 :: r, x1

  jj = j
  if (j > nlat_sfc/2) jj = nlat_sfc - j + 1
  m2 = lpl_gfs(jj)
  m1 = nlon_sfc
  r=real(m1)/real(m2)
  ii = ireduce
  if (ii <= 0) ii = ii + lpl_gfs(jj)
  if (ii > lpl_gfs(jj)) ii = ii - lpl_gfs(jj)
  x1=(ii-1)*r
  ifull=mod(nint(x1),m1)+1
  return
end subroutine reduce2full

subroutine init_sfc(sfc_sum)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_sfc               initialize surface structure
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  initialize to zero all elements of the surface type
!            data structure.
!
! program history log:
!   2008-11-04 gayno
!
!   input argument list:
!     none
!
!   output argument list:
!     sfc_sum   - holds 'sums' used in the calculation of surface fields
!                 within a fov.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

  type surface
     sequence
     real(r_kind) :: vfr
     real(r_kind) :: sm
     real(r_kind) :: stp
     real(r_kind) :: ff10
     real(r_kind) :: sfcr
     real(r_kind) :: zz
     real(r_kind) :: sn
     real(r_kind), dimension(0:3)  :: ts
     real(r_kind), dimension(0:3)  :: count
     real(r_kind), dimension(0:24) :: count_vty
     real(r_kind), dimension(0:16) :: count_sty
  end type surface

  type(surface), intent(out) :: sfc_sum

! The surface characteristics of a fov are determined by:
!
! sum(sfc_field*ant_power) / sum(ant_power)
!
! This structure holds the various 'sums' and thus must
! be initialized to zero before use.

  sfc_sum%vfr        = zero  ! greenness
  sfc_sum%sm         = zero  ! soil moisture - top layer
  sfc_sum%stp        = zero  ! soil temperature - top layer
  sfc_sum%ff10       = zero  ! wind factor
  sfc_sum%sfcr       = zero  ! roughness length
  sfc_sum%zz         = zero  ! terrain
  sfc_sum%sn         = zero  ! snow depth
  sfc_sum%ts         = zero  ! skin temperature for each surface type
                             ! 0-water,1-land,2-ice,3-snow
  sfc_sum%count      = zero  ! sum of the antenna power for each
                             ! surface type
  sfc_sum%count_vty  = zero  ! vegetation type
  sfc_sum%count_sty  = zero  ! soil type

  return

end subroutine init_sfc

subroutine time_int_sfc(ix,iy,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    time_int_sfc           time interpolate surface data
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  time interpolate surface data to the observation time
!
! program history log:
!   2008-11-04 gayno
!
!   input argument list:
!     ix, iy       - x/y indices of grid point
!     dtsfc/dtsfcp - time interpolation factors
!     itsfc/itsfcp - bounding indices of data
!
!   output argument list:
!     sfc_mdl - holds surface information for a single model point
!               valid at the observation time
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables.
  integer(i_kind), intent(in   ) :: ix,iy,itsfc, itsfcp
  real(r_kind)   , intent(in   ) :: dtsfc, dtsfcp

  type surface2
     sequence
     real(r_kind) :: vfr
     real(r_kind) :: sfcr
     real(r_kind) :: sm
     real(r_kind) :: stp
     real(r_kind) :: ff10
     real(r_kind) :: sn
     real(r_kind) :: ts
  end type surface2

  type(surface2) , intent(  out) :: sfc_mdl

! Note, indices are reversed (y/x).

  sfc_mdl%sn=sno_full(iy,ix,itsfc)*dtsfc+sno_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%vfr=veg_frac_full(iy,ix,itsfc)*dtsfc+veg_frac_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%stp=soil_temp_full(iy,ix,itsfc)*dtsfc+soil_temp_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%sm=soil_moi_full(iy,ix,itsfc)*dtsfc+soil_moi_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%ff10=fact10_full(iy,ix,itsfc)*dtsfc+fact10_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%sfcr=sfc_rough_full(iy,ix,itsfc)*dtsfc+sfc_rough_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%ts=sst_full(iy,ix,itsfc)*dtsfc+sst_full(iy,ix,itsfcp)*dtsfcp

  return
end subroutine time_int_sfc

subroutine accum_sfc(i,j,power,sfc_mdl,sfc_sum)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    accum_sfc           "accumulate" surface fields
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  The surface characteristics of a fov are determined by:
!            sum(sfc_field*ant_power) / sum(ant_power)
!            this routine determines the required "sums".
!
! program history log:
!   2008-11-04 gayno
!
!   input argument list:
!     i, j         - i/j indices of model grid point
!     power        - antenna power
!     sfc_sum      - holds required surface data "sums"
!
!   output argument list:
!     sfc_mdl - holds surface information for a single model point
!               valid at the observation time
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables.
  integer(i_kind), intent(in   ) :: i, j
  real(r_kind)   , intent(in   ) :: power

  type surface2
     sequence
     real(r_kind) :: vfr    ! greenness (veg fraction)
     real(r_kind) :: sfcr   ! roughness length
     real(r_kind) :: sm     ! soil moisture
     real(r_kind) :: stp    ! soil temperature
     real(r_kind) :: ff10   ! wind factor
     real(r_kind) :: sn     ! snow depth
     real(r_kind) :: ts     ! skin temperature
  end type surface2

  type(surface2) , intent(in   ) :: sfc_mdl

  type surface
     sequence
     real(r_kind) :: vfr    ! greenness (veg fraction)
     real(r_kind) :: sm     ! soil moisture
     real(r_kind) :: stp    ! soil temperautre
     real(r_kind) :: ff10   ! wind factor
     real(r_kind) :: sfcr   ! roughness length
     real(r_kind) :: zz     ! terrain height
     real(r_kind) :: sn     ! snow depth
     real(r_kind), dimension(0:3)  :: ts        ! skin temp (each land type)
     real(r_kind), dimension(0:3)  :: count     ! count of each land type
     real(r_kind), dimension(0:24) :: count_vty ! count of each landuse type
     real(r_kind), dimension(0:16) :: count_sty ! count of each soil type
  end type surface

  type(surface)  , intent(inout) :: sfc_sum

! Declare local parameters.
  real(r_kind),parameter:: minsnow=one_tenth

! Declare local variables.
  integer(i_kind)             :: mask, sty, vty

  if (power == zero) return

  mask=isli_full(j,i)
  if (mask>=1.and.sfc_mdl%sn>minsnow) mask=3

  if (mask==1) then  ! bare (non-snow covered) land
     vty=nint(veg_type_full(j,i))
     sfc_sum%count_vty(vty)=sfc_sum%count_vty(vty)+power
     sty=nint(soil_type_full(j,i))
     sfc_sum%count_sty(sty)=sfc_sum%count_sty(sty)+power
     sfc_sum%vfr=sfc_sum%vfr + (power*sfc_mdl%vfr)
     sfc_sum%sm=sfc_sum%sm + (power*sfc_mdl%sm)
     sfc_sum%stp=sfc_sum%stp + (power*sfc_mdl%stp)
  elseif (mask==3) then  ! snow cover land or sea ice
     sfc_sum%sn=sfc_sum%sn + (power*sfc_mdl%sn)
  endif

! wind factor, roughness length and terrain are summed
! across all surface types.
  sfc_sum%ff10=sfc_sum%ff10 + (power*sfc_mdl%ff10)
  sfc_sum%sfcr=sfc_sum%sfcr + (power*sfc_mdl%sfcr)
  if (regional) then
     sfc_sum%zz=sfc_sum%zz + (power*zs_full(j,i))
  else
     sfc_sum%zz=sfc_sum%zz + (power*zs_full_gfs(j,i))
  endif

! keep track of skin temperature for each surface type
  sfc_sum%ts(mask)=sfc_sum%ts(mask) + (power*sfc_mdl%ts)
! keep count of each surface type
  sfc_sum%count(mask) = power + sfc_sum%count(mask)

  return
end subroutine accum_sfc

subroutine calc_sfc(sfc_sum,isflg,idomsfc,sfcpct,vfr,sty,vty,sm, &
                    stp,ff10,sfcr,zz,sn,ts,tsavg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_sfc            calculate surface fields
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  The surface characteristics of a fov are determined by:
!            sum(sfc_field*ant_power) / sum(ant_power)
!
! program history log:
!   2008-11-04 gayno
!
!   input argument list:
!     sfc_sum      - holds required surface data "sums"
!
!   output argument list:
!     idomsfc  - dominate surface type
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
!     sfcpct(0:3)- percentage of 4 surface types
!                (0) - sea percentage
!                (1) - land percentage
!                (2) - sea ice percentage
!                (3) - snow percentage
!     vfr      - vegetation fraction
!     sty      - dominate soil type
!     vty      - dominate vegetation type
!     stp      - top layer soil temperature
!     sm       - top layer soil moisture
!     ff10     - wind factor
!     sfcr     - surface roughness lenght
!     zz       - terrain height
!     sn       - snow depth
!     ts       - skin temperature for each surface type
!     tsavg    - average skin temperature
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables
  integer(i_kind), intent(  out) :: isflg, idomsfc(1)
  real(r_kind)   , intent(  out) :: sm, stp, sty, vty, vfr, sfcpct(0:3)
  real(r_kind)   , intent(  out) :: ff10, sfcr, zz, sn, ts(0:3), tsavg

  type surface
     sequence
     real(r_kind) :: vfr
     real(r_kind) :: sm
     real(r_kind) :: stp
     real(r_kind) :: ff10
     real(r_kind) :: sfcr
     real(r_kind) :: zz
     real(r_kind) :: sn
     real(r_kind), dimension(0:3)  :: ts
     real(r_kind), dimension(0:3)  :: count
     real(r_kind), dimension(0:24) :: count_vty
     real(r_kind), dimension(0:16) :: count_sty
  end type surface

  type(surface)  , intent(in   ) :: sfc_sum

! Declare local variables
  integer(i_kind)   :: itmp(1), n
  real(r_kind)      :: count_tot, count_land, count_sty_tot, &
                       count_snow, count_vty_tot, count_ice, count_water

  count_tot = sum(sfc_sum%count)

! skin temperature over entire fov
  tsavg = sum(sfc_sum%ts(0:3))/count_tot

! landuse is predominate type
  count_vty_tot = sum(sfc_sum%count_vty)
  if (count_vty_tot==zero) then
     vty=zero
  else
     itmp=lbound(sfc_sum%count_vty)-1+maxloc(sfc_sum%count_vty)
     vty=float(itmp(1))
  endif

! soil type is predominate type
  count_sty_tot = sum(sfc_sum%count_sty)
  if (count_sty_tot==zero) then
     sty=zero
  else
     itmp=lbound(sfc_sum%count_sty)-1+maxloc(sfc_sum%count_sty)
     sty=float(itmp(1))
  endif

! fields for bare (non-snow covered) land
  count_land = sfc_sum%count(1)
  if (count_land>zero) then
     vfr = sfc_sum%vfr / count_land
     stp = sfc_sum%stp / count_land
     sm  = sfc_sum%sm / count_land
     ts(1) = sfc_sum%ts(1) / count_land
  else
     vfr = zero
     stp = zero
     sm  = one
     ts(1) = tsavg
  endif

! fields for open water
  count_water=sfc_sum%count(0)
  if(count_water>zero)then
     ts(0)=sfc_sum%ts(0)/count_water
  else
     ts(0)=tsavg
  endif

! fields for non-snow covered sea ice
  count_ice=sfc_sum%count(2)
  if(count_ice>zero)then
     ts(2)=sfc_sum%ts(2)/count_ice
  else
     ts(2)=tsavg
  endif

! fields for snow covered land and sea ice
  count_snow=sfc_sum%count(3)
  if(count_snow>zero)then
     sn=sfc_sum%sn/count_snow
     ts(3) = sfc_sum%ts(3) / count_snow
  else
     sn=zero
     ts(3) = tsavg
  endif

! percent of each surface type
  sfcpct=zero
  do n = 0, 3
     sfcpct(n) = sfc_sum%count(n) / count_tot
  enddo

  idomsfc=lbound(sfcpct)+maxloc(sfcpct)-1

! wind factor, roughness and terrain are determined
! over entire fov.
  ff10 = sfc_sum%ff10/count_tot
  sfcr = sfc_sum%sfcr/count_tot
  zz   = sfc_sum%zz/count_tot

  isflg = 0
  if(sfcpct(0) > 0.99_r_kind)then
     isflg = 0      ! open water
  else if(sfcpct(1) > 0.99_r_kind)then
     isflg = 1       ! bare land
  else if(sfcpct(2) > 0.99_r_kind)then
     isflg = 2   ! bare sea ice
  else if(sfcpct(3) > 0.99_r_kind)then
     isflg = 3   ! snow covered land and sea ice
  else
     isflg = 4   ! mixture
  end if

  return
 end subroutine calc_sfc

end module deter_sfc_mod
