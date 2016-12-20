module innovstats
!$$$  module documentation block
!
! module: innovstats                   print ensemble innovation statistics.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:
!
! Public Subroutines:
!  print_innovstats: given obfit_prior and obsprd_prior (observation - 
!   ensemble mean observation variable, ensemble standard deviation of
!   observation variable), print some statistics useful for monitoring.
!
! Public Variables: None
!
! program history log:
!   2009-02-23  Initial version.
!
! attributes:
!   language: f95
!
!$$$

use enkf_obsmod, only:  oberrvar,ob,ensmean_ob,obtype,nobs_conv,nobs_oz,&
                   nobs_sat,nobstot,obloclat,ensmean_obnobc,obpress,stattype,&
                   oberrvar_orig,indxsat
use params, only : latbound
use kinds, only: i_kind, r_kind,r_single
use radinfo, only: jpch_rad,nusis,nuchan
use constants, only: one,zero

implicit none

private

public :: print_innovstats

contains

subroutine print_innovstats(obfit,obsprd)
real(r_single), intent(in) :: obfit(nobstot), obsprd(nobstot)
integer(i_kind) nobst_nh,nobst_sh,nobst_tr,&
 nobspw_nh,nobspw_sh,nobspw_tr,&
 nobsspd_nh,nobsspd_sh,nobsspd_tr,&
 nobsgps_nh,nobsgps_sh,nobsgps_tr,&
 nobsq_nh,nobsq_sh,nobsq_tr,nobswnd_nh,nobswnd_sh,nobswnd_tr,&
 nobsoz_nh,nobsoz_sh,nobsoz_tr,nobsps_sh,nobsps_nh,nobsps_tr,nob
real(r_single) sumps_nh,biasps_nh,sumps_sh,biasps_sh,&
 sumps_tr,biasps_tr,&
 sumps_spread_nh,sumps_spread_sh,sumps_spread_tr,sumps_oberr_nh,&
 sumps_oberr_sh,sumps_oberr_tr,&
 sumt_nh,biast_nh,sumt_spread_nh,sumt_oberr_nh,&
 sumt_sh,biast_sh,sumt_spread_sh,sumt_oberr_sh,&
 sumt_tr,biast_tr,sumt_spread_tr,sumt_oberr_tr,&
 sumq_nh,biasq_nh,sumq_spread_nh,sumq_oberr_nh,&
 sumq_sh,biasq_sh,sumq_spread_sh,sumq_oberr_sh,&
 sumq_tr,biasq_tr,sumq_spread_tr,sumq_oberr_tr,&
 sumspd_nh,biasspd_nh,sumspd_spread_nh,sumspd_oberr_nh,&
 sumspd_sh,biasspd_sh,sumspd_spread_sh,sumspd_oberr_sh,&
 sumspd_tr,biasspd_tr,sumspd_spread_tr,sumspd_oberr_tr,&
 sumgps_nh,biasgps_nh,sumgps_spread_nh,sumgps_oberr_nh,&
 sumgps_sh,biasgps_sh,sumgps_spread_sh,sumgps_oberr_sh,&
 sumgps_tr,biasgps_tr,sumgps_spread_tr,sumgps_oberr_tr,&
 sumpw_nh,biaspw_nh,sumpw_spread_nh,sumpw_oberr_nh,&
 sumpw_sh,biaspw_sh,sumpw_spread_sh,sumpw_oberr_sh,&
 sumpw_tr,biaspw_tr,sumpw_spread_tr,sumpw_oberr_tr,&
 sumoz_nh,biasoz_nh,sumoz_spread_nh,sumoz_oberr_nh,&
 sumoz_sh,biasoz_sh,sumoz_spread_sh,sumoz_oberr_sh,&
 sumoz_tr,biasoz_tr,sumoz_spread_tr,sumoz_oberr_tr,&
 sumwnd_nh,biaswnd_nh,sumwnd_spread_nh,sumwnd_oberr_nh,&
 sumwnd_sh,biaswnd_sh,sumwnd_spread_sh,sumwnd_oberr_sh,&
 sumwnd_tr,biaswnd_tr,sumwnd_spread_tr,sumwnd_oberr_tr
! stuff for computing sat data innovation stats.
real(r_single) sumsprd_sat(jpch_rad),sumerr_sat(jpch_rad), &
     sumfit_sat(jpch_rad),sumfitsq_sat(jpch_rad), &
     predicted_innov,innov
integer(i_kind) nob_sat(jpch_rad),nchan,nn
real(r_single) :: denom
!==> stats for conventional + ozone obs.
if (nobs_conv+nobs_oz > 0) then
  !==> pre-process obs, obs metadata.
  nobsps_nh = 0
  nobsps_sh = 0
  nobsps_tr = 0
  nobst_nh = 0
  nobst_sh = 0
  nobst_tr = 0
  nobsq_nh = 0
  nobsq_sh = 0
  nobsq_tr = 0
  nobsoz_nh = 0
  nobsoz_sh = 0
  nobsoz_tr = 0
  nobswnd_nh = 0
  nobswnd_sh = 0
  nobswnd_tr = 0
  nobspw_nh = 0
  nobspw_sh = 0
  nobspw_tr = 0
  nobsgps_nh = 0
  nobsgps_sh = 0
  nobsgps_tr = 0
  nobsspd_nh = 0
  nobsspd_sh = 0
  nobsspd_tr = 0
  do nob=1,nobs_conv+nobs_oz
     if(oberrvar(nob) < 1.e10_r_single)then
         if (obtype(nob)(1:3) == ' ps') then
            call obstats(obfit(nob),oberrvar_orig(nob),&
                 obsprd(nob),obloclat(nob),&
                 sumps_nh,biasps_nh,sumps_spread_nh,sumps_oberr_nh,nobsps_nh,&
                 sumps_sh,biasps_sh,sumps_spread_sh,sumps_oberr_sh,nobsps_sh,&
                 sumps_tr,biasps_tr,sumps_spread_tr,sumps_oberr_tr,nobsps_tr)
         else if (obtype(nob)(1:3) == '  t' .and. stattype(nob) /= 121) then
            call obstats(obfit(nob),oberrvar_orig(nob),&
                 obsprd(nob),obloclat(nob),&
                 sumt_nh,biast_nh,sumt_spread_nh,sumt_oberr_nh,nobst_nh,&
                 sumt_sh,biast_sh,sumt_spread_sh,sumt_oberr_sh,nobst_sh,&
                 sumt_tr,biast_tr,sumt_spread_tr,sumt_oberr_tr,nobst_tr)
        ! all winds
         else if (obtype(nob)(1:3) == '  u' .or. obtype(nob)(1:3) == '  v') then
        ! only in-situ winds (no sat winds)
        !else if (obtype(nob)(1:3) == '  u' .or. obtype(nob)(1:3) == '  v' .and. &
        !        ((stattype(nob) >= 280 .and. stattype(nob) <= 282) .or. &
        !         (stattype(nob) >= 220 .and. stattype(nob) <= 221) .or. &
        !         (stattype(nob) >= 230 .and. stattype(nob) <= 235) ) then
            call obstats(obfit(nob),oberrvar_orig(nob),&
                 obsprd(nob),obloclat(nob),&
                 sumwnd_nh,biaswnd_nh,sumwnd_spread_nh,sumwnd_oberr_nh,nobswnd_nh,&
                 sumwnd_sh,biaswnd_sh,sumwnd_spread_sh,sumwnd_oberr_sh,nobswnd_sh,&
                 sumwnd_tr,biaswnd_tr,sumwnd_spread_tr,sumwnd_oberr_tr,nobswnd_tr)
         else if (obtype(nob)(1:3) == '  q') then
            call obstats(obfit(nob),oberrvar_orig(nob),&
                 obsprd(nob),obloclat(nob),&
                 sumq_nh,biasq_nh,sumq_spread_nh,sumq_oberr_nh,nobsq_nh,&
                 sumq_sh,biasq_sh,sumq_spread_sh,sumq_oberr_sh,nobsq_sh,&
                 sumq_tr,biasq_tr,sumq_spread_tr,sumq_oberr_tr,nobsq_tr)
         else if (obtype(nob)(1:3) == 'spd') then
            call obstats(obfit(nob),oberrvar_orig(nob),&
                 obsprd(nob),obloclat(nob),&
                 sumspd_nh,biasspd_nh,sumspd_spread_nh,sumspd_oberr_nh,nobsspd_nh,&
                 sumspd_sh,biasspd_sh,sumspd_spread_sh,sumspd_oberr_sh,nobsspd_sh,&
                 sumspd_tr,biasspd_tr,sumspd_spread_tr,sumspd_oberr_tr,nobsspd_tr)
         else if (obtype(nob)(1:3) == 'gps') then
            call obstats(obfit(nob),oberrvar_orig(nob),&
                 obsprd(nob),obloclat(nob),&
                 sumgps_nh,biasgps_nh,sumgps_spread_nh,sumgps_oberr_nh,nobsgps_nh,&
                 sumgps_sh,biasgps_sh,sumgps_spread_sh,sumgps_oberr_sh,nobsgps_sh,&
                 sumgps_tr,biasgps_tr,sumgps_spread_tr,sumgps_oberr_tr,nobsgps_tr)
         else if (obtype(nob)(1:3) == ' pw') then
            call obstats(obfit(nob),oberrvar_orig(nob),&
                 obsprd(nob),obloclat(nob),&
                 sumpw_nh,biaspw_nh,sumpw_spread_nh,sumpw_oberr_nh,nobspw_nh,&
                 sumpw_sh,biaspw_sh,sumpw_spread_sh,sumpw_oberr_sh,nobspw_sh,&
                 sumpw_tr,biaspw_tr,sumpw_spread_tr,sumpw_oberr_tr,nobspw_tr)
         else if (nob > nobs_conv) then
            call obstats(obfit(nob),oberrvar_orig(nob),&
                 obsprd(nob),obloclat(nob),&
                 sumoz_nh,biasoz_nh,sumoz_spread_nh,sumoz_oberr_nh,nobsoz_nh,&
                 sumoz_sh,biasoz_sh,sumoz_spread_sh,sumoz_oberr_sh,nobsoz_sh,&
                 sumoz_tr,biasoz_tr,sumoz_spread_tr,sumoz_oberr_tr,nobsoz_tr)
         end if
      end if
  end do ! loop over non-radiance obs
!--> print innovation statistics for subset of conventional data.
   print *,'conventional obs'
   print *,'region, obtype, nobs, bias, innov stdev, sqrt(S+R), sqrt(S), sqrt(R):'
   call printstats('   all ps',sumps_nh,biasps_nh,sumps_spread_nh,sumps_oberr_nh,nobsps_nh,&
        sumps_sh,biasps_sh,sumps_spread_sh,sumps_oberr_sh,nobsps_sh,&
        sumps_tr,biasps_tr,sumps_spread_tr,sumps_oberr_tr,nobsps_tr)
   call printstats('    all t',sumt_nh,biast_nh,sumt_spread_nh,sumt_oberr_nh,nobst_nh,&
        sumt_sh,biast_sh,sumt_spread_sh,sumt_oberr_sh,nobst_sh,&
        sumt_tr,biast_tr,sumt_spread_tr,sumt_oberr_tr,nobst_tr)
   call printstats('   all uv',sumwnd_nh,biaswnd_nh,sumwnd_spread_nh,sumwnd_oberr_nh,nobswnd_nh,&
        sumwnd_sh,biaswnd_sh,sumwnd_spread_sh,sumwnd_oberr_sh,nobswnd_sh,&
        sumwnd_tr,biaswnd_tr,sumwnd_spread_tr,sumwnd_oberr_tr,nobswnd_tr)
   call printstats('    all q',sumq_nh,biasq_nh,sumq_spread_nh,sumq_oberr_nh,nobsq_nh,&
        sumq_sh,biasq_sh,sumq_spread_sh,sumq_oberr_sh,nobsq_sh,&
        sumq_tr,biasq_tr,sumq_spread_tr,sumq_oberr_tr,nobsq_tr)
   call printstats('  all spd',sumspd_nh,biasspd_nh,sumspd_spread_nh,sumspd_oberr_nh,nobsspd_nh,&
        sumspd_sh,biasspd_sh,sumspd_spread_sh,sumspd_oberr_sh,nobsspd_sh,&
        sumspd_tr,biasspd_tr,sumspd_spread_tr,sumspd_oberr_tr,nobsspd_tr)
   call printstats('   all pw',sumpw_nh,biasq_nh,sumpw_spread_nh,sumpw_oberr_nh,nobspw_nh,&
        sumpw_sh,biaspw_sh,sumpw_spread_sh,sumpw_oberr_sh,nobspw_sh,&
        sumpw_tr,biaspw_tr,sumpw_spread_tr,sumpw_oberr_tr,nobspw_tr)
   call printstats('  all gps',sumgps_nh,biasq_nh,sumgps_spread_nh,sumgps_oberr_nh,nobsgps_nh,&
        sumgps_sh,biasgps_sh,sumgps_spread_sh,sumgps_oberr_sh,nobsgps_sh,&
        sumgps_tr,biasgps_tr,sumgps_spread_tr,sumgps_oberr_tr,nobsgps_tr)
   call printstats(' sbuv2 oz',sumoz_nh,biasoz_nh,sumoz_spread_nh,sumoz_oberr_nh,nobsoz_nh,&
        sumoz_sh,biasoz_sh,sumoz_spread_sh,sumoz_oberr_sh,nobsoz_sh,&
        sumoz_tr,biasoz_tr,sumoz_spread_tr,sumoz_oberr_tr,nobsoz_tr)
end if ! nobs_conv+nobs_oz > 0

!==> stats for satellite brightness temp obs (amsua only).
if (nobs_sat > 0) then
  sumsprd_sat = zero
  sumfit_sat = zero
  sumerr_sat = zero
  sumfitsq_sat = zero
  nob_sat = 0
  nn = 0
  do nob=nobs_conv+nobs_oz+1,nobs_conv+nobs_oz+nobs_sat
     nn = nn + 1
     nchan = indxsat(nn)
     if (oberrvar(nob) < 1.e10_r_single .and. nchan > 0) then
       sumsprd_sat(nchan)=sumsprd_sat(nchan)+obsprd(nob)
       sumerr_sat(nchan)=sumerr_sat(nchan)+oberrvar_orig(nob)
       sumfitsq_sat(nchan)=sumfitsq_sat(nchan)+obfit(nob)**2
       sumfit_sat(nchan)=sumfit_sat(nchan)+obfit(nob)
       nob_sat(nchan)=nob_sat(nchan) + 1
     end if
  end do ! loop over obs

!--> print innovation statistics for amsu-a sat data..
  print *,'satellite brightness temp'
  print *,'instrument, channel #, nobs, bias, innov stdev, sqrt(S+R), sqrt(S), sqrt(R):'
  do nchan=1,jpch_rad
     if (nob_sat(nchan) > 0) then
       denom=one/real(nob_sat(nchan),r_single)
       sumfit_sat(nchan) = sumfit_sat(nchan)*denom
       sumfitsq_sat(nchan) = sumfitsq_sat(nchan)*denom
       sumerr_sat(nchan) = sumerr_sat(nchan)*denom
       sumsprd_sat(nchan) = sumsprd_sat(nchan)*denom
       predicted_innov = sqrt(sumsprd_sat(nchan)+sumerr_sat(nchan))
       !innov = sqrt(sumfitsq_sat(nchan)-sumfit_sat(nchan)**2)
       innov = sqrt(sumfitsq_sat(nchan))
       write(6,9805) trim(adjustl(nusis(nchan))),nuchan(nchan),nob_sat(nchan),sumfit_sat(nchan),innov,&
                  predicted_innov,sqrt(sumsprd_sat(nchan)),&
                  sqrt(sumerr_sat(nchan))
     end if
  end do
9805 format(a20,i4,1x,i5,5(1x,e10.3))
end if !nobs_sat>0
end subroutine print_innovstats

subroutine obstats(obfit,oberrvar,obsprd,obloclat,&
                   sumfit_nh,sumbias_nh,sumspread_nh,sumoberr_nh,nobs_nh,&
                   sumfit_sh,sumbias_sh,sumspread_sh,sumoberr_sh,nobs_sh,&
                   sumfit_tr,sumbias_tr,sumspread_tr,sumoberr_tr,nobs_tr)

  implicit none
  real(r_single), intent(in out) ::  sumfit_nh, sumbias_nh, sumspread_nh, sumoberr_nh,&
       sumfit_tr, sumbias_tr, sumspread_tr, sumoberr_tr,&
       sumfit_sh, sumbias_sh, sumspread_sh, sumoberr_sh
  real(r_single), intent(in) :: obfit, oberrvar, obsprd, obloclat
  integer(i_kind), intent(in out) :: nobs_nh, nobs_sh, nobs_tr

! compute innovation statistics in nh,sh,tropics.

  if (obloclat > latbound) then
     if (nobs_nh == 0) then
       sumfit_nh = obfit**2
       sumbias_nh = obfit
       sumspread_nh = obsprd
       sumoberr_nh = oberrvar
     else
       sumfit_nh = sumfit_nh + obfit**2
       sumbias_nh = sumbias_nh + obfit
       sumspread_nh = sumspread_nh + obsprd
       sumoberr_nh = sumoberr_nh + oberrvar
     end if
     nobs_nh = nobs_nh + 1
  else if (obloclat < -latbound) then
     if (nobs_sh == 0) then
       sumfit_sh = obfit**2
       sumbias_sh = obfit
       sumspread_sh = obsprd
       sumoberr_sh = oberrvar
     else
       sumfit_sh = sumfit_sh + obfit**2
       sumbias_sh = sumbias_sh + obfit
       sumspread_sh = sumspread_sh + obsprd
       sumoberr_sh = sumoberr_sh + oberrvar
     end if
     nobs_sh = nobs_sh + 1
  else
     if (nobs_tr == 0) then
       sumfit_tr = obfit**2
       sumbias_tr = obfit
       sumspread_tr = obsprd
       sumoberr_tr = oberrvar
     else
       sumfit_tr = sumfit_tr + obfit**2
       sumbias_tr = sumbias_tr + obfit
       sumspread_tr = sumspread_tr + obsprd
       sumoberr_tr = sumoberr_tr + oberrvar
     end if
     nobs_tr = nobs_tr + 1
  end if

end subroutine obstats

subroutine printstats(obtype,sum_nh,bias_nh,sum_spread_nh,sum_oberr_nh,nobs_nh,&
              sum_sh,bias_sh,sum_spread_sh,sum_oberr_sh,nobs_sh,&
              sum_tr,bias_tr,sum_spread_tr,sum_oberr_tr,nobs_tr)
  implicit none
  real(r_single), intent(in out) ::  bias_nh, sum_spread_nh, sum_oberr_nh,&
       bias_tr, sum_spread_tr, sum_oberr_tr,&
       bias_sh, sum_spread_sh, sum_oberr_sh, &
       sum_nh,sum_sh,sum_tr
  integer(i_kind), intent(in) :: nobs_nh, nobs_sh, nobs_tr
  character(len=9), intent(in) :: obtype
  real(r_single) :: denom

!   print *,'obtype,nobs_nh,nobs_sh,nobs_tr ',obtype,nobs_nh,nobs_sh,nobs_tr
  if (nobs_nh > 0) then
     denom=one/real(nobs_nh,r_single)
     sum_nh = sum_nh*denom
     bias_nh = bias_nh*denom
     sum_oberr_nh = sum_oberr_nh*denom
     sum_spread_nh = sum_spread_nh*denom
     write(6,9805) &
     'NH',obtype,nobs_nh,bias_nh,sqrt(sum_nh),sqrt(sum_spread_nh+sum_oberr_nh),sqrt(sum_spread_nh),sqrt(sum_oberr_nh)
  end if
  if (nobs_tr > 0) then
     denom=one/real(nobs_tr,r_single)
     sum_tr = sum_tr*denom
     bias_tr = bias_tr*denom
     sum_oberr_tr = sum_oberr_tr*denom
     sum_spread_tr = sum_spread_tr*denom
     write(6,9805) &
     'TR',obtype,nobs_tr,bias_tr,sqrt(sum_tr),sqrt(sum_spread_tr+sum_oberr_tr),sqrt(sum_spread_tr),sqrt(sum_oberr_tr)
  end if
  if (nobs_sh > 0) then
     denom=one/real(nobs_sh,r_single)
     sum_sh = sum_sh*denom
     bias_sh = bias_sh*denom
     sum_oberr_sh = sum_oberr_sh*denom
     sum_spread_sh = sum_spread_sh*denom
     write(6,9805) &
     'SH',obtype,nobs_sh,bias_sh,sqrt(sum_sh),sqrt(sum_spread_sh+sum_oberr_sh),sqrt(sum_spread_sh),sqrt(sum_oberr_sh)
  end if
9805 format(a2,1x,a9,1x,i6,5(1x,e10.3))
end subroutine printstats

end module innovstats
