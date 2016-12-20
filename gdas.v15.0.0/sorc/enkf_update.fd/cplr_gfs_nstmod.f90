!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_NSTCouplerMod ---
!
! !DESCRIPTION: This stub provides the default interfaces to the 
!               NST analysis calculations in GSI.
!
! !REVISION HISTORY:
!
!  07Oct2011 Akella/RT - Initial code
!  05Mar2012 Akella    - Create_nst and getnst from satthin are now nst_int_ & nst_set_
!                        Destroy_nst from satthin is nst_final_
!
!EOP
!-------------------------------------------------------------------------

subroutine nst_init_()

     use mpimod,      only: mype
     use gridmod,     only: nlat_sfc,nlon_sfc, nlat, nlon
     use guess_grids, only: nfldnst, ntguesnst
     use gsi_nstcouplermod,     only: tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full,&
                                      c_0_full,c_d_full,w_0_full,w_d_full
     use mpeu_util,   only: die, perr

     implicit none

     if(.not.allocated(tref_full))    allocate(tref_full    (nlat_sfc,nlon_sfc,nfldnst))
     if(.not.allocated(dt_cool_full)) allocate(dt_cool_full (nlat_sfc,nlon_sfc,nfldnst))
     if(.not.allocated(z_c_full))     allocate(z_c_full     (nlat_sfc,nlon_sfc,nfldnst))
     if(.not.allocated(dt_warm_full)) allocate(dt_warm_full (nlat_sfc,nlon_sfc,nfldnst))
     if(.not.allocated(z_w_full))     allocate(z_w_full     (nlat_sfc,nlon_sfc,nfldnst))
     if(.not.allocated(c_0_full))     allocate(c_0_full     (nlat_sfc,nlon_sfc,nfldnst))
     if(.not.allocated(c_d_full))     allocate(c_d_full     (nlat_sfc,nlon_sfc,nfldnst))
     if(.not.allocated(w_0_full))     allocate(w_0_full     (nlat_sfc,nlon_sfc,nfldnst))
     if(.not.allocated(w_d_full))     allocate(w_d_full     (nlat_sfc,nlon_sfc,nfldnst))

     if( ntguesnst < 1 .or. ntguesnst > nfldnst ) then 
            call perr('nst_init','ntguesnst = ',ntguesnst)
            call  die('nst_init')
     endif

     if(mype == 0)write(6,*)'GETNST: enter with nlat_sfc,nlon_sfc=',nlat_sfc,nlon_sfc,&
                            ' and nlat,nlon=',nlat,nlon

end subroutine nst_init_
!*******************************************************************************************

subroutine nst_set_(mype_io)

     use kinds, only: i_kind
     use gridmod, only: use_gfs_nemsio 
     use ncepgfs_io, only: read_gfsnst
     use ncepnems_io, only: read_nemsnst
     use gsi_nstcouplermod, only: tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full,&
                                  c_0_full,c_d_full,w_0_full,w_d_full
     implicit none
     integer(i_kind),  intent(in   ) :: mype_io

     if ( use_gfs_nemsio ) then
        call read_nemsnst(mype_io,tref_full,dt_cool_full,z_c_full, &
                          dt_warm_full,z_w_full,c_0_full,c_d_full,w_0_full,w_d_full)
     else
        call read_gfsnst(mype_io,tref_full,dt_cool_full,z_c_full, &
                         dt_warm_full,z_w_full,c_0_full,c_d_full,w_0_full,w_d_full)
     endif
                         
end subroutine nst_set_
!*******************************************************************************************

subroutine nst_final_ ()

     use gsi_nstcouplermod, only: tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full,&
                                  c_0_full,c_d_full,w_0_full,w_d_full

     if(allocated(tref_full))    deallocate(tref_full)
     if(allocated(dt_cool_full)) deallocate(dt_cool_full)
     if(allocated(z_c_full))     deallocate(z_c_full)
     if(allocated(dt_warm_full)) deallocate(dt_warm_full)
     if(allocated(z_w_full))     deallocate(z_w_full)
     if(allocated(c_0_full))     deallocate(c_0_full)
     if(allocated(c_d_full))     deallocate(c_d_full)
     if(allocated(w_0_full))     deallocate(w_0_full)
     if(allocated(w_d_full))     deallocate(w_d_full)

end subroutine nst_final_
!*******************************************************************************************

subroutine deter_nst_(dlat_earth,dlon_earth,obstime,zob,tref,dtw,dtc,tz_tr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_nst                     determine NSST variable at observation location over water
!   prgmmr: Xu Li           org: np2                date: 2011-04-08
!
! abstract:  determines NSST variables over water surface type based on surrounding surface types
!
! program history log:
!   2011-04-08 Li
!   2013-01-23 parrish - change from grdcrd to grdcrd1 (to allow successful
!
!   input argument list:
!     obstime                             - observation time relative to analysis time
!     dlat_earth                          - earth latitude  in radians
!     dlon_earth                          - earth longitude in radians
!     zob                                 - obs. depth in the water
!
!   output argument list:
!     tref                                - oceanic foundation temperature
!      dtw                                - diurnal warming at depth zob
!      dtc                                - sublayer cooling at depth zob
!    tz_tr                                - d(Tz)/d(tr)
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
     use kinds,       only: r_kind,i_kind
     use constants,   only: zero,one,z_w_max
     use gridmod,     only: nlat,nlon,regional,tll2xy,nlat_sfc,nlon_sfc,rlats_sfc,rlons_sfc
     use guess_grids, only: nfldnst,hrdifnst
     use gsi_nstcouplermod, only: fac_dtl,fac_tsl
     use gsi_nstcouplermod, only: tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full,&
                                  c_0_full,c_d_full,w_0_full,w_d_full
     use satthin, only: isli_full
     implicit none

     real(r_kind), intent(in ) :: dlat_earth,dlon_earth,obstime,zob
     real(r_kind), intent(out) :: tref,dtw,dtc,tz_tr

!    local variables
     real(r_kind):: dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d
     integer(i_kind) istyp00,istyp01,istyp10,istyp11
     integer(i_kind):: itnst,itnstp
     integer(i_kind):: ix,iy,ixp,iyp,j
     real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11,dtnst,dtnstp
     real(r_kind):: tref_00,tref_01,tref_10,tref_11,tr_tmp
     real(r_kind):: dt_cool_00,dt_cool_01,dt_cool_10,dt_cool_11
     real(r_kind):: z_c_00,z_c_01,z_c_10,z_c_11
     real(r_kind):: dt_warm_00,dt_warm_01,dt_warm_10,dt_warm_11
     real(r_kind):: z_w_00,z_w_01,z_w_10,z_w_11,z_w_tmp
     real(r_kind):: c_0_00,c_0_01,c_0_10,c_0_11
     real(r_kind):: c_d_00,c_d_01,c_d_10,c_d_11
     real(r_kind):: w_0_00,w_0_01,w_0_10,w_0_11
     real(r_kind):: w_d_00,w_d_01,w_d_10,w_d_11
     real(r_kind):: wgtavg,dlat,dlon
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
     w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=one-w00-w10-w01

     ix=min(max(1,ix),nlat_sfc); iy=min(max(0,iy),nlon_sfc)
     ixp=min(nlat_sfc,ix+1); iyp=iy+1
     if(iy==0) iy=nlon_sfc
     if(iyp==nlon_sfc+1) iyp=1

!    Get time interpolation factors for nst files
     if(obstime > hrdifnst(1) .and. obstime <= hrdifnst(nfldnst))then
        do j=1,nfldnst-1
           if(obstime > hrdifnst(j) .and. obstime <= hrdifnst(j+1))then
              itnst=j
              itnstp=j+1
              dtnst=(hrdifnst(j+1)-obstime)/(hrdifnst(j+1)-hrdifnst(j))
           end if
        end do
     else if(obstime <=hrdifnst(1))then
        itnst=1
        itnstp=1
        dtnst=one
     else
        itnst=nfldnst
        itnstp=nfldnst
        dtnst=one
     end if
     dtnstp=one-dtnst

!    Set surface type flag.

     istyp00 = isli_full(ix ,iy )
     istyp10 = isli_full(ixp,iy )
     istyp01 = isli_full(ix ,iyp)
     istyp11 = isli_full(ixp,iyp)
!
!    Use the time interpolation factors for nst files
!
     tref_00    = tref_full   (ix ,iy ,itnst)*dtnst + tref_full   (ix ,iy ,itnstp)*dtnstp
     tref_01    = tref_full   (ix ,iyp,itnst)*dtnst + tref_full   (ix ,iyp,itnstp)*dtnstp
     tref_10    = tref_full   (ixp,iy ,itnst)*dtnst + tref_full   (ixp,iy ,itnstp)*dtnstp
     tref_11    = tref_full   (ixp,iyp,itnst)*dtnst + tref_full   (ixp,iyp,itnstp)*dtnstp

     dt_cool_00 = dt_cool_full(ix ,iy ,itnst)*dtnst + dt_cool_full(ix ,iy ,itnstp)*dtnstp
     dt_cool_01 = dt_cool_full(ix ,iyp,itnst)*dtnst + dt_cool_full(ix ,iyp,itnstp)*dtnstp
     dt_cool_10 = dt_cool_full(ixp,iy ,itnst)*dtnst + dt_cool_full(ixp,iy ,itnstp)*dtnstp
     dt_cool_11 = dt_cool_full(ixp,iyp,itnst)*dtnst + dt_cool_full(ixp,iyp,itnstp)*dtnstp

     z_c_00     = z_c_full    (ix ,iy ,itnst)*dtnst + z_c_full    (ix ,iy ,itnstp)*dtnstp
     z_c_01     = z_c_full    (ix ,iyp,itnst)*dtnst + z_c_full    (ix ,iyp,itnstp)*dtnstp
     z_c_10     = z_c_full    (ixp,iy ,itnst)*dtnst + z_c_full    (ixp,iy ,itnstp)*dtnstp
     z_c_11     = z_c_full    (ixp,iyp,itnst)*dtnst + z_c_full    (ixp,iyp,itnstp)*dtnstp

     dt_warm_00 = dt_warm_full(ix ,iy ,itnst)*dtnst + dt_warm_full(ix ,iy ,itnstp)*dtnstp
     dt_warm_01 = dt_warm_full(ix ,iyp,itnst)*dtnst + dt_warm_full(ix ,iyp,itnstp)*dtnstp
     dt_warm_10 = dt_warm_full(ixp,iy ,itnst)*dtnst + dt_warm_full(ixp,iy ,itnstp)*dtnstp
     dt_warm_11 = dt_warm_full(ixp,iyp,itnst)*dtnst + dt_warm_full(ixp,iyp,itnstp)*dtnstp

     z_w_00     = z_w_full    (ix ,iy ,itnst)*dtnst + z_w_full    (ix ,iy ,itnstp)*dtnstp
     z_w_01     = z_w_full    (ix ,iyp,itnst)*dtnst + z_w_full    (ix ,iyp,itnstp)*dtnstp
     z_w_10     = z_w_full    (ixp,iy ,itnst)*dtnst + z_w_full    (ixp,iy ,itnstp)*dtnstp
     z_w_11     = z_w_full    (ixp,iyp,itnst)*dtnst + z_w_full    (ixp,iyp,itnstp)*dtnstp

     c_0_00     = c_0_full    (ix ,iy ,itnst)*dtnst + c_0_full    (ix ,iy ,itnstp)*dtnstp
     c_0_01     = c_0_full    (ix ,iyp,itnst)*dtnst + c_0_full    (ix ,iyp,itnstp)*dtnstp
     c_0_10     = c_0_full    (ixp,iy ,itnst)*dtnst + c_0_full    (ixp,iy ,itnstp)*dtnstp
     c_0_11     = c_0_full    (ixp,iyp,itnst)*dtnst + c_0_full    (ixp,iyp,itnstp)*dtnstp

     c_d_00     = c_d_full    (ix ,iy ,itnst)*dtnst + c_d_full    (ix ,iy ,itnstp)*dtnstp
     c_d_01     = c_d_full    (ix ,iyp,itnst)*dtnst + c_d_full    (ix ,iyp,itnstp)*dtnstp
     c_d_10     = c_d_full    (ixp,iy ,itnst)*dtnst + c_d_full    (ixp,iy ,itnstp)*dtnstp
     c_d_11     = c_d_full    (ixp,iyp,itnst)*dtnst + c_d_full    (ixp,iyp,itnstp)*dtnstp

     w_0_00     = w_0_full    (ix ,iy ,itnst)*dtnst + w_0_full    (ix ,iy ,itnstp)*dtnstp
     w_0_01     = w_0_full    (ix ,iyp,itnst)*dtnst + w_0_full    (ix ,iyp,itnstp)*dtnstp
     w_0_10     = w_0_full    (ixp,iy ,itnst)*dtnst + w_0_full    (ixp,iy ,itnstp)*dtnstp
     w_0_11     = w_0_full    (ixp,iyp,itnst)*dtnst + w_0_full    (ixp,iyp,itnstp)*dtnstp

     w_d_00     = w_d_full    (ix ,iy ,itnst)*dtnst + w_d_full    (ix ,iy ,itnstp)*dtnstp
     w_d_01     = w_d_full    (ix ,iyp,itnst)*dtnst + w_d_full    (ix ,iyp,itnstp)*dtnstp
     w_d_10     = w_d_full    (ixp,iy ,itnst)*dtnst + w_d_full    (ixp,iy ,itnstp)*dtnstp
     w_d_11     = w_d_full    (ixp,iyp,itnst)*dtnst + w_d_full    (ixp,iyp,itnstp)*dtnstp

!    Interpolate nst variables to obs location (water surface only)

     wgtavg  = zero
     tr_tmp  = zero
     dt_cool = zero
     z_c     = zero
     dt_warm = zero
     z_w_tmp = zero
     c_0     = zero
     c_d     = zero
     w_0     = zero
     w_d     = zero

     if (istyp00 == 0)then
        wgtavg  = wgtavg  + w00
        tr_tmp  = tr_tmp  + w00*tref_00
        dt_cool = dt_cool + w00*dt_cool_00
        z_c     = z_c     + w00*z_c_00
        dt_warm = dt_warm + w00*dt_warm_00
        z_w_tmp = z_w_tmp + w00*z_w_00
        c_0     = c_0     + w00*c_0_00
        c_d     = c_d     + w00*c_d_00
        w_0     = w_0     + w00*w_0_00
        w_d     = w_d     + w00*w_d_00
     endif
     if(istyp01 == 0)then
        wgtavg  = wgtavg  + w01
        tr_tmp  = tr_tmp  + w01*tref_01
        dt_cool = dt_cool + w01*dt_cool_01
        z_c     = z_c     + w01*z_c_01
        dt_warm = dt_warm + w01*dt_warm_01
        z_w_tmp = z_w_tmp + w01*z_w_01
        c_0     = c_0     + w01*c_0_01
        c_d     = c_d     + w01*c_d_01
        w_0     = w_0     + w01*w_0_01
        w_d     = w_d     + w01*w_d_01
     end if
     if(istyp10 == 0)then
        wgtavg  = wgtavg  + w10
        tr_tmp  = tr_tmp  + w10*tref_10
        dt_cool = dt_cool + w10*dt_cool_10
        z_c     = z_c     + w10*z_c_10
        dt_warm = dt_warm + w10*dt_warm_10
        z_w_tmp = z_w_tmp + w10*z_w_10
        c_0     = c_0     + w10*c_0_10
        c_d     = c_d     + w10*c_d_10
        w_0     = w_0     + w10*w_0_10
        w_d     = w_d     + w10*w_d_10
     end if
     if(istyp11 == 0)then
        wgtavg  = wgtavg  + w11
        tr_tmp  = tr_tmp  + w11*tref_11
        dt_cool = dt_cool + w11*dt_cool_11
        z_c     = z_c     + w11*z_c_11
        dt_warm = dt_warm + w11*dt_warm_11
        z_w_tmp = z_w_tmp + w11*z_w_11
        c_0     = c_0     + w11*c_0_11
        c_d     = c_d     + w11*c_d_11
        w_0     = w_0     + w11*w_0_11
        w_d     = w_d     + w11*w_d_11
     end if

     if(wgtavg > zero)then
        tr_tmp  = tr_tmp/wgtavg
        tref    = tr_tmp

        z_w_tmp = z_w_tmp/wgtavg
        z_w     = z_w_tmp

        dt_cool = dt_cool/wgtavg
        z_c     = z_c/wgtavg
        dt_warm = dt_warm/wgtavg
        c_0     = c_0/wgtavg
        c_d     = c_d/wgtavg
        w_0     = w_0/wgtavg
        w_d     = w_d/wgtavg

        dtw = fac_dtl*dt_warm*(one-min(zob,z_w)/z_w)
        if ( z_c > zero ) then
          dtc = fac_tsl*dt_cool*(one-min(zob,z_c)/z_c)
        else
          dtc = zero
        endif

        call cal_tztr_(dt_warm,c_0,c_d,w_0,w_d,z_c,z_w,zob,tz_tr)

     end if
end subroutine deter_nst_
!*******************************************************************************************

subroutine cal_tztr_(dt_warm,c_0,c_d,w_0,w_d,zc,zw,z,tztr)
!
! abstract: calculate d(Tz)/d(Ts) with T-Profile info from NSST Model
!
!   prgmmr: li, xu           org: np23                date: 2011-04-08
! input variables
!
! dt_warm :       diurnal warming amount at the surface
! xz      :       DTL depth                           (M)
! c_0     :       coefficint 1 to calculate d(Tc)/d(Ts)
! c_d     :       coefficint 2 to calculate d(Tc)/d(Ts)
! w_0     :       coefficint 1 to calculate d(Tw)/d(Ts)
! w_d     :       coefficint 2 to calculate d(Tw)/d(Ts)
!
! output variables
!
! tztr     :      d(Tz)/d(Tr)

  use kinds, only: r_kind
  use constants, only: one,half,zero
  use gsi_nstcouplermod, only: fac_dtl,fac_tsl
  real(kind=r_kind), intent(in)  :: dt_warm,c_0,c_d,w_0,w_d,zc,zw,z
  real(kind=r_kind), intent(out) :: tztr
! local variables
  real(kind=r_kind) :: c1,c2

  c1 = one-fac_dtl*w_0+fac_tsl*c_0
  c2 = one+fac_tsl*c_0

  tztr = one

  if ( dt_warm > zero .and.  c1 /= zero ) then
    if ( z <= zc  ) then
      tztr = (one+z*(fac_dtl*w_d-fac_tsl*c_d))/c1
    elseif ( z > zc .and. z < zw ) then
      tztr = (one+fac_tsl*c_0+z*fac_dtl*w_d)/c1
    endif
  elseif ( dt_warm == zero .and. c2 /= zero ) then
    if ( z <= zc ) then
      tztr = (one-z*fac_tsl*c_d)/c2
    endif
  endif

  if ( tztr <= one .and. tztr > half ) then
    tztr = tztr
  else
!   write(*,'(a,2I2,2F12.6,F9.3,5F12.6,F8.3,F9.6,F8.3)') ' cal_tztr : ',fac_dtl,fac_tsl,c1,c2,dt_warm,c_0,c_d,w_0,w_d,zc,zw,z,tztr
  endif

end subroutine cal_tztr_
!*******************************************************************************************

!*******************************************************************************************
subroutine skindepth_(obstype,sd_rad)
!
! abstract: Get skin depth (instrument dependent). Ideally, a skin-depth model calculates the channel dependent sd
!
! program history log:
!   2011-04-08  li


 use kinds, only: r_kind
 implicit none
 character(10),     intent(in)  :: obstype
 real(kind=r_kind), intent(out) :: sd_rad

  sd_rad = 0.000015_r_kind
  if ( obstype == 'amsre' .or. obstype == 'amsr2' .or. obstype == 'gmi' ) then
      sd_rad = 0.03_r_kind
  elseif ( obstype == 'amsua' .or. obstype == 'amsub' .or.  obstype == 'ssmis' .or.  obstype == 'ssmi' .or. &
      obstype == 'mhs' .or.  obstype == 'msu' .or.  obstype == 'hsb' ) then
      sd_rad = 0.001_r_kind
  endif

end subroutine skindepth_
!*******************************************************************************************
