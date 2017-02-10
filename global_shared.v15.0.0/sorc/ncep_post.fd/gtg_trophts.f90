!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!    (c) University Corporation for Atmospheric Research (UCAR) 2013.  All
!    rights reserved.  The Government's right to use this data and/or
!    software (the "Work") is restricted, per the terms of Cooperative
!    Agreement (ATM (AGS)-0753581 10/1/08) between UCAR and the National
!    Science Foundation, to a *nonexclusive, nontransferable,
!    irrevocable, royalty-free license to exercise or have exercised for
!    or on behalf of the U.S. throughout the world all the exclusive
!    rights provided by copyrights.  Such license, however, does not
!    include the right to sell copies or phonorecords of the copyrighted
!    works to the public.  The Work is provided "AS IS" and without
!    warranty of any kind.  UCAR EXPRESSLY DISCLAIMS ALL OTHER
!    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, ANY IMPLIED WARRANTIES OF
!    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

module gtg_trophts

  use ctlblk_mod, only: jsta,jend,jsta_2l, jend_2u, jsta_m, jend_m, &
       jsta_m2, jend_m2,im,jm,lm, modelname
  use ctlblk_mod, only: SPVAL

  use gtg_config, only : SMALL1,SMALL2,printflag
  use gtg_filter

  implicit none

  real, parameter :: pmax=700.E2
  real, parameter :: bldepth=1500. ! m

  integer :: ic,jc
contains

!-----------------------------------------------------------------------
  subroutine trophts(ztropupper,ztroplower,hgt,trophtm, &
       zm,pm,Tm,um,vm,qvm,Nsqm,PVm,Rim,vws,TI3)
!$$$ SUBPROGRAM DOCUMENTATION BLOCK
!      --- Estimates tropopause heights (m) and corresponding temperature (K)
!      --- using several definitions:
!      --- trophtpv: dynamic tropopause ht (PV ~2 but is NWP model dependent)
!      --- trophtwmo: tropopause ht using lapse rate def. The WMO definition
!          is 2 K/km but this is NWP model dependent
!      --- trophtstab: tropopause ht using max stability N2/N1 ratio > 1.1
!      --- This is NWP model dependent.
!      --- trophtavg: tropopause ht using average of above 3 (m)
!      --- Outputs are stored in TI3 as follows:
!          TI3(i,j,1)=trophtpv (m)
!          TI3(i,j,2)=corresponding tropT (K)
!          TI3(i,j,3)=trophtwmo (m)
!          TI3(i,j,4)=corresponding tropT (K)
!          TI3(i,j,5)=trophtstab (m)
!          TI3(i,j,6)=corresponding tropT (K)
!          TI3(i,j,7)=trophtavg (m)
!          TI3(i,j,8)=corresponding tropT (K)
!          TI3(i,j,9)=speedmax above/below trophtavg (m/s)
!          TI3(i,j,10)=N2/N1)trophtavg (ND)
!          TI3(i,j,11)=Ri2/Ri1)trophtavg (ND)
!          TI3(i,j,12)=distance of speedmax above/below trophtavg (m)
!          TI3(i,j,13)=max vertical shear in vicinity of trophtavg (1/sec)
!          TI3(i,j,14)=smoothed input trophtm (m)
!          TI3(i,j,15)=corresponding tropT (K)
!     --- Input trophtm(nx,ny) is input tropht derived from NWP model output
!$$$
    implicit none
    real, intent(in) :: ztropupper, ztroplower ! boundaries (m)
    real, dimension(IM,jsta_2l:jend_2u), intent(in) :: hgt,trophtm
    real, dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: zm,pm,Tm,um,vm,qvm
    real, dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: Nsqm,PVm,Rim,vws
    real, dimension(IM,jsta_2l:jend_2u,LM),intent(inout) :: TI3 ! actually<LM

    ! work array
    real, dimension(IM,jsta_2l:jend_2u) :: TI12d, TI22d
    integer, dimension(IM,jsta_2l:jend_2u) :: kts
    real :: shr1(LM)

    integer :: i,j,k,kt,ktrop
    real :: tropht,trophtpv,trophtwmo,trophtstab,tropavg,deltrop,trophtms,tropT
    real :: tropmax,tropmin
    real :: zkm,ztum,ztrop,speedk,speedtl,speedtu,ztl,ztu,stabmax,vwsmax
    integer :: navg

    ! to debug PE test (1038,225)
    ic=1038
    jc=225
    if(jsta<=jc .and. jend>=jc) then
       jc=jc
    else
       jc=jend
    end if

!   --- Determine the tropopause height and temperature based on PV change.
    if(printflag>=2) write(*,*) 'computing trophtpv'

    TI12d = SPVAL
    TI22d = SPVAL
    call troppv(zm,pm,PVm,hgt,TI22d)
!   --- Check and try to correct for wild points
    call checktrop(TI22d,TI12d)
!   --- Get k index of trophtpv in TI12d and place in kts
    call tropk(zm,TI12d,kts)
!   --- On output kts contains ktrop(i,j). Get corresponding temperature.
    do j=jsta,jend
    do i=1,IM
       tropT=SPVAL
       kt=-1
       trophtpv=TI12d(i,j)
       if(trophtpv > 0) then
          kt=kts(i,j)
          if(kt>=1 .and. kt<=LM) tropT=Tm(i,j,kt)
       end if
       TI3(i,j,1)=TI12d(i,j)  ! trophtpv
       TI3(i,j,2)=tropT       ! tropTpv
    enddo
    enddo

!   --- Determine the tropopause height and temperature based on the WMO definition. 
    TI12d = SPVAL
    TI22d = SPVAL
    call tropwmo(pm,Tm,zm,qvm,hgt,TI12d)
!   --- Check and try to correct for wild points
    call checktrop(TI12d,TI22d)
!   --- Get k index of trophtwmo and place in kts
    call tropk(zm,TI22d,kts)
!   --- On output kts contains ktrop(i,j). Get corresponding temperature.
    do j=jsta,jend
    do i=1,IM
       tropT=SPVAL
       kt=-1
       trophtwmo=TI22d(i,j)
       if(trophtwmo > 0) then
          kt=kts(i,j)
          if(kt>=1 .and. kt<=LM) tropT=Tm(i,j,kt)
       end if
       TI3(i,j,3)=TI22d(i,j)  ! trophtwmo
       TI3(i,j,4)=tropT       ! tropTwmo
    enddo
    enddo

!    --- Determine the tropopause height and temperature based on stability change. 
    TI12d = SPVAL
!   --- Output tropht is in TI12d, input TI22d contains wmo trop ht 
    call tropN2N1(zm,pm,Nsqm,qvm,hgt,TI22d,TI12d)
!     --- Check and try to correct for wild points
    call checktrop(TI12d,TI22d)
!   --- Get k index of trophtstab (in Ti22d) and place in kts
    call tropk(zm,Ti22d,kts)
!   --- On output kts contains ktrop(i,j). Get corresponding temperature.
    do j=jsta,jend
    do i=1,IM
       tropT=SPVAL
       kt=-1
       trophtstab=TI22d(i,j)
       if(trophtstab>0) then
          kt=kts(i,j)
          if(kt>=1 .and. kt<=LM) tropT=Tm(i,j,kt)
       endif
       TI3(i,j,5)=Ti22d(i,j)  ! trophtstab
       TI3(i,j,6)=tropT       ! tropTstab
    enddo
    enddo

!   --- Add in input trop ht from NWP model, if it is available
    tropmax=-1.0E20
    tropmin=+1.0E20
    do j=jsta,jend
    do i=1,IM
       TI3(i,j,14)=SPVAL
       TI3(i,j,15)=SPVAL
       if(trophtm(i,j)>1.0E+3) then
          Ti12d(i,j)=trophtm(i,j)
          tropmax=MAX(tropmax,trophtm(i,j))
          tropmin=MIN(tropmin,trophtm(i,j))
       endif
    enddo
    enddo
    if(tropmax>1.0E3) then
!      --- Check and try to correct for wild points in input tropthm
       call checktrop(Ti12d,Ti22d)
!      --- Get k index of smoothed trophtm (in Ti22d) and place in kts
       call tropk(zm,Ti22d,kts)
!      --- On output kts contains ktrop(i,j). Get corresponding temperature.
       do j=jsta,jend
       do i=1,IM
          tropT=SPVAL
          kt=-1
          tropht=TI22d(i,j)
          if(tropht>0) then
             kt=kts(i,j)
             if(kt>=1 .and. kt<=LM) tropT=Tm(i,j,kt)
          endif
          TI3(i,j,14)=Ti22d(i,j)  ! trophtms
          TI3(i,j,15)=tropT       ! tropmT
       enddo
       enddo
    endif

!   --- Average 4 trophts
    deltrop=3500.

    do j=jsta,jend
    do i=1,IM
       TI12d(i,j)=SPVAL
       tropavg=0.
       navg=0
       trophtpv=TI3(i,j,1)
       trophtwmo=TI3(i,j,3)
       trophtstab=TI3(i,j,5)
       trophtms=TI3(i,j,14)
!      --- Average trophts that are within deltrop of tropstab
!      --- The default average is trophtstab
       if(trophtstab>1.0E+3) then
          tropavg=trophtstab
          navg=1
       endif
       if((trophtwmo>1.0E+3) .and. (ABS(trophtstab-trophtwmo)<=deltrop)) then
          tropavg=tropavg+trophtwmo
          navg=navg+1
       endif
       if((trophtpv>1.0E+3) .and. (ABS(tropavg/MAX(navg,1)-trophtpv)<=deltrop)) then
          tropavg=tropavg+trophtpv
          navg=navg+1
       endif
       if((trophtms>1.0E+3) .and. (ABS(tropavg/MAX(navg,1)-trophtms)<=deltrop)) then
          tropavg=tropavg+trophtms
          navg=navg+1
       endif
       tropavg=tropavg/MAX(navg,1)
!      --- Check reasonableness of trophtstab if it is the 
!      --- only one used in the average
       if(trophtwmo>=1.0E+3 .and. trophtpv>=1.0E+3) then
          if(ABS(trophtpv-trophtwmo)<=deltrop .and. &
             ABS(trophtstab-(trophtwmo+trophtpv)/2.)>deltrop) then
             tropavg=trophtpv+trophtwmo
             navg=2
             if((trophtms>1.0E+3) .and. &
                (ABS(tropavg/MAX(navg,1)-trophtms)<=deltrop)) then
                tropavg=tropavg+trophtms
                navg=navg+1
             endif
             tropavg=tropavg/MAX(navg,1)
          endif
       endif
!      --- Get vertical index of tropavg
       kt=-1
       do k=LM-2,2,-1 ! GFS is top-bottom, original GTG is bottom-top
          if(zm(i,j,k)>tropavg) exit
          kt=k
       enddo
       if(kt>1) then
          ktrop=kt
          TI12d(i,j)=tropavg
       endif 
    enddo
    enddo

!   --- Check and try to correct for wild points
    TI22d = SPVAL
    call checktrop(TI12d,TI22d)
!   --- Get k index of tropht in TI22d
    call tropk(zm,TI22d,kts)
!   --- On output kts contains ktrop(i,j).
    do j=jsta,jend
    do i=1,IM
       kt=-1
       tropT=SPVAL
       tropavg=TI22d(i,j)
!      --- Get corresponding temperature.
       if(tropavg>0) then
          kt=kts(i,j)
          if(kt>=1 .and. kt<=LM) tropT=Tm(i,j,kt)
       endif
       TI3(i,j,7)=TI22d(i,j)  ! trophtavg
       TI3(i,j,8)=tropT       ! tropTavg
    enddo
    enddo

!   --- Get maximum wind speed in vicinty of trophtavg (+/-2km)
    ztum=ztropupper
    do j=jsta,jend
    do i=1,IM
       Ti12d(i,j)=SPVAL
       ztrop=TI3(i,j,7)  ! trophtavg
       speedtl=0.
       speedtu=0.
       vwsmax=-1.0E20
       ztl=SPVAL
       ztu=SPVAL
       if(ztrop>0.) then
!         call vwscomp(um,vm,zm,nx,ny,nz,i,j,2,nz-1,dudz1,dvdz1,shr1)
!         shr1 is as a parameter
          shr1(1:LM) = vws(i,j,1:LM)
          do k=LM-1,2,-1 ! GFS is top-bottom, original GTG is bottom-top
             zkm=zm(i,j,k)  ! m
             speedk=SPVAL
             if(ABS(zkm-SPVAL)<SMALL1 .or. &
                ABS(um(i,j,k)-SPVAL)<SMALL1 .or. &
                ABS(vm(i,j,k)-SPVAL)<SMALL1) cycle
             if(zkm>ztrop+ztum) exit
!            --- check speed below trop
             if(zkm>=ztrop-ztum .and. zkm<=ztrop) then
                speedk=SQRT(um(i,j,k)**2 + vm(i,j,k)**2)
                if(speedk>speedtl) then
                   speedtl=speedk
                   ztl=zkm-ztrop
                endif
                vwsmax=MAX(shr1(k),vwsmax)
!            --- check speed above trop
             elseif(zkm>=ztrop .and. zkm<=ztrop+ztum) then
                speedk=SQRT(um(i,j,k)**2 + vm(i,j,k)**2)
                if(speedk>speedtu) then
                   speedtu=speedk
                   ztu=zkm-ztrop
                endif
                vwsmax=MAX(shr1(k),vwsmax)
             endif
          enddo
       endif
       if(speedtl>speedtu) then
          speedk=speedtl
          zkm=ztl
       else
          speedk=speedtu
          zkm=ztu
       endif
       TI3(i,j,9)=speedk
       TI3(i,j,12)=zkm
       TI3(i,j,13)=vwsmax
    enddo  ! j loop
    enddo  ! i loop

!     --- Derive trop strength indicators N2/N1, Ri2/Ri1
!     --- Use trophtavg
    do j=jsta,jend
    do i=1,IM
       TI12d(i,j)=TI3(i,j,7)
    enddo
    enddo
    call tropr(zm,Nsqm,Rim,TI12d,ztroplower,ztropupper,TI3)

    return
  end subroutine trophts

!-----------------------------------------------------------------------
  subroutine tropk(z,ztrop,xkt)
!     --- Determines k index such that z(k-1) > ztrop > z(k+1) for all (i,j)
    implicit none

    real, intent(in) :: z(IM,jsta_2l:jend_2u,LM)
    real, intent(in) :: ztrop(IM,jsta_2l:jend_2u)
    integer, intent(inout) :: xkt(IM,jsta_2l:jend_2u)

    integer :: i,j,k,kt
    real :: ztropij

    xkt = -1

    do j=jsta, jend
    do i=1,IM
       xkt(i,j)=-1
       ztropij=ztrop(i,j)
!      --- Don't include uncomputed (i,j) or pts below terrain
       if(ABS(ztropij-SPVAL)<SMALL1) cycle
       kt=-1
       do k=LM-2,2,-1 ! GFS is top-bottom, original GTG is bottom-top
          if(z(i,j,k)>ztropij) exit
          kt=k
       enddo
       if(kt>0) xkt(i,j) = kt
    enddo
    enddo

    return
  end subroutine tropk

!-----------------------------------------------------------------------
  subroutine troppv(z,p,PV,hgt,trophtpv)
!$$$ SUBPROGRAM DOCUMENTATION BLOCK
!     --- Determines the tropopause height (m) using an assigned value
!     --- of the PV as the dynamical definition of the tropopause (=PVK).
!     --- The best value of PVK is model dependent so it can be specified below.
!     --- Going down from the model top, the first layer encountered
!     --- with a |PV| < PVK is taken as the first layer in the troposphere.
!     --- On output trophtpv contains the estimated tropopause height (m).
!     --- pvs(nx,ny,nz) and work(nx,ny,nz) are work arrays.
!$$$
    implicit none

    real, dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: z,p,PV
    real, dimension(IM,jsta_2l:jend_2u), intent(in) :: hgt
    real, dimension(IM,jsta_2l:jend_2u), intent(inout) :: trophtpv

    ! work array
    real, dimension(IM,jsta_2l:jend_2u,LM) :: pvs ! pvs used for smoothed PV

    integer :: n,i,j,k,km1,k1,k2,kk,ktrop,ktlast,kcheck,navg,knp,kdifmin
    integer :: ip1,im1,jp1,jm1
    real :: pvu,pvl
    real :: ztrop,ztroplast,ztropij
    integer :: nftxy,nftz,Filttype
    logical :: found, abnormal
    real ::  pvr(LM)
    integer :: kpeak(LM) ! size of a count, not actual size of LM 
    real, parameter :: PVK=2.0  ! threshold in 10^-6 MKS units (Hoskins, QJRMS, 1985)

    trophtpv= SPVAL

!   --- Smooth input pv
    do k=1,LM
    do j=jsta,jend
    do i=1,IM
       pvs(i,j,k)=SPVAL
       if(ABS(PV(i,j,k)-SPVAL)>SMALL1) pvs(i,j,k)=ABS(PV(i,j,k))
    enddo
    enddo
    enddo
    nftxy=1
    nftz=1
    Filttype=1
    call filt3d(1,LM,nftxy,nftz,Filttype,pvs)

    do j=jend,jsta,-1 ! post is north-south, original GTG is south-north
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          ktlast=-1
          ztroplast=SPVAL

!         --- Get pv ratios
          do k=LM-2,3,-1 ! GFS is top-bottom, original GTG is bottom-top
             pvr(k)=SPVAL 
             if(ABS(pvr(k-1)-SPVAL)<SMALL1 .or. &
                ABS(pvr(k-2)-SPVAL)<SMALL1 .or. &
                ABS(pvr(k+1)-SPVAL)<SMALL1 .or. &
                ABS(pvr(k+2)-SPVAL)<SMALL1) cycle
             pvl=0.5*(pvs(i,j,k+1)+pvs(i,j,k+2)) ! GFS is top-bottom, original GTG is bottom-top
             pvu=0.5*(pvs(i,j,k-1)+pvs(i,j,k-2)) ! GFS is top-bottom, original GTG is bottom-top
             pvr(k)=pvu/MAX(pvl,SMALL1)
          enddo
!         --- Dynamic tropopause is determined when highest three consecutive values
!         --- above PV are above the threshold (PVK).
          ktrop=-1
          ztrop=SPVAL
          abnormal=.false.
          found=.false.
          do k=3,LM-2 ! GFS is top-bottom, original GTG is bottom-top
!         --- Work downward looking for 2 consecutive points below PVK
             km1=k+1 ! GFS is top-bottom, original GTG is bottom-top
             if(p(i,j,k)<0.) cycle
             if(p(i,j,k)>pmax) exit   ! don't consider levels with p>pmax
             if(ABS(z(i,j,k)-SPVAL)<SMALL1) cycle
             if(z(i,j,k)>20.E3) cycle  ! don't consider levels with z>20 km 
             if(z(i,j,k) < (hgt(i,j)+bldepth)) exit
             if(ABS(pvs(i,j,k)-SPVAL)<SMALL1 .or. &
                ABS(pvs(i,j,km1)-SPVAL)<SMALL1) exit
             if((pvs(i,j,k)<PVK).and.(pvs(i,j,km1)<PVK)) then
!            --- Make sure this is not an isolated event
                kcheck=k+10 ! GFS is top-bottom, original GTG is bottom-top 
                kcheck=MIN(kcheck,LM-2) ! GFS is top-bottom, original GTG is bottom-top 
!               --- If pv has increased downward and is now > the threshold,
!               --- move down to the next k level
                do kk=k+1,kcheck ! GFS is top-bottom, original GTG is bottom-top 
                   if(pvs(i,j,kk)>PVK) then
                      abnormal=.true.
                      exit
                   end if
                enddo
                if(abnormal) cycle
                found=.true. 
                ktrop=k
                ztrop=z(i,j,k)
!               --- Check against the last trop ht.  If it is much greater then check
!               --- against a smaller PV threshold.
                if(ztrop>ztroplast+1.0E20 .and. ABS(ztroplast-SPVAL)>SMALL1) then
                   do kk=k+2,kcheck ! GFS is top-bottom, original GTG is bottom-top 
                      if(kk>=LM-1) exit
                      if(pvs(i,j,kk)<PVK-0.5) then
                         ktrop=kk
                         ztrop=z(i,j,kk)
                         exit
                      endif
                   enddo
                endif
             endif
          enddo
!         --- Troppv not found using 2 consecutive points below PVK.  Try
!         --- working down looking for one single point less than PVK.
          if(.not.found) then
             ktrop=2 ! GFS is top-bottom, original GTG is bottom-top
             abnormal=.false.
             do k=3,LM-2 ! GFS is top-bottom, original GTG is bottom-top 
                if(p(i,j,k)<0.) cycle
                if(p(i,j,k)>pmax) exit   ! don't consider levels with p>pmax
                if(ABS(z(i,j,k)-SPVAL)<SMALL1) cycle
                if(z(i,j,k)>20.E3) cycle  ! don't consider levels with z>20 km
                if(z(i,j,k) < (hgt(i,j)+bldepth)) exit
                if(ABS(pvs(i,j,k)-SPVAL)<SMALL1) exit
                if(pvs(i,j,k)<PVK) then
!                  --- Make sure this is not an isolated event
                   kcheck=k+7 ! GFS is top-bottom, original GTG is bottom-top 
                   kcheck=MIN(kcheck,LM-2) ! GFS is top-bottom, original GTG is bottom-top 
                   do kk=k+1,kcheck ! GFS is top-bottom, original GTG is bottom-top 
                      if(pvs(i,j,kk)>PVK) then
                         abnormal=.true.
                         exit
                      end if
                   enddo
                   if(abnormal) cycle
                   found=.true. 
                   ktrop=k
                   ztrop=z(i,j,k)
                endif
             enddo
          endif
!         --- Troppv still not found.  Try looking for maximum PV ratio around trop(i-1)
          if(.not.found) then
             ktrop=-1
             if(ktlast<LM-2 .and. ktlast>0) then
                k1=ktlast+5 ! GFS is top-bottom, original GTG is bottom-top
                k2=ktlast-5 ! GFS is top-bottom, original GTG is bottom-top
                k1=MAX(k1,3)
                k1=MIN(k1,LM-2)
                k2=MAX(k2,3)
                k2=MIN(k2,LM-2)
             else
                k1=LM-2
                k2=3
             endif
             knp=0
             do k=k1,k2,-1 ! GFS is top-bottom, original GTG is bottom-top
                if(z(i,j,k) < (hgt(i,j)+bldepth)) cycle
                if(p(i,j,k)>pmax) cycle   ! don't consider levels with p>pmax
                if(z(i,j,k)>20.E3) cycle  ! don't consider levels with z>20 km
                if(ABS(pvr(k)-SPVAL)<SMALL1 .or. &
                   ABS(pvr(k-1)-SPVAL)<SMALL1 .or. &
                   ABS(pvr(k+1)-SPVAL)<SMALL1) cycle
                if(pvr(k)>pvr(k-1) .and. pvr(k)>pvr(k+1) .and. pvr(k)>1.1) then
                   knp=knp+1
                   kpeak(knp)=k
                endif
             enddo
!            --- Loop over all candidate trops and use the one closest to the
!            --- last trop ht.
             if(knp > 0) then
                if(knp==1 .or. ktlast>=LM-2) then
                   ktrop=kpeak(1)
                   ztrop=z(i,j,ktrop)
                else if(ktlast>0) then
                   kdifmin=LM
                   do n=1,knp
                      k=kpeak(n)
                      if(ABS(k-ktlast)<kdifmin) then
                         kdifmin=ABS(k-ktlast)
                         ktrop=k
                         ztrop=z(i,j,ktrop)
                      endif
                   enddo
                endif
             end if
          end if

          ! Have to leave some 'gaps' of trophtpv at grid point (i-1,j+1) 
          ! when i==1 or j==jend, since trophtpv relies on itself
          if(ktrop>1) then
             ztropij=ztrop
             navg=1
             if(ABS(trophtpv(im1,j)-SPVAL)>SMALL1) then
                ztrop=ztrop+trophtpv(im1,j)
                navg=navg+1
             endif
!             if(ABS(trophtpv(im1,jm1)-SPVAL)>SMALL1) then
!                ztrop=ztrop+trophtpv(im1,jm1)
!                navg=navg+1
!             endif
!             if(ABS(trophtpv(i,jm1)-SPVAL)>SMALL1) then
!                ztrop=ztrop+trophtpv(i,jm1)
!                navg=navg+1
!             endif
             if(ABS(trophtpv(ip1,j)-SPVAL)>SMALL1) then
                ztrop=ztrop+trophtpv(ip1,j)
                navg=navg+1
             endif
             ztrop=ztrop/MAX(navg,1)

             do k=LM-2,3,-1 ! GFS is top-bottom, original GTG is bottom-top 
                if(z(i,j,k)>ztrop) then
                   ktrop=k
                   exit
                endif
             enddo
             trophtpv(i,j)=ztrop
             ktlast=ktrop
             ztroplast=ztrop
          endif ! ktrop>1

          if(printflag>=2 .and. ktrop <= 1) &
               write(*,*) "i,j,ktrop,ztrop,hgt,z,p,pv=", i,j,ktrop,ztrop,hgt(i,j),z(i,j,LM/2),p(i,j,LM/2),pv(i,j,LM/2)


       enddo  ! i loop
    enddo  ! j loop

    if(printflag>=2) write(*,*)'exit troppv: i,j,trophtpv=',ic,jc,trophtpv(ic,jc)

    return
  end subroutine troppv

!-----------------------------------------------------------------------
  subroutine tropwmo(pm,Tm,zm,qvm,hgt,trophtwmo)
!$$$ SUBPROGRAM DOCUMENTATION BLOCK
!     --- Determines the mean tropopause height according to a variant of
!     --- the WMO definition.
!     --- Moving up from the sfc, the lowest level at which the lapse rate
!     --- decreases to a test value of gammat or or less.  The WMO definition
!     --- of gammat is 2K/km.  But this may fail for some models so this
!     --- value can be specified below.
!     --- Input trophtm is NWP model derived tropht (if available).
!     --- On output trophtwmo contains the tropopause ht (m), and gamma
!     --- contains the computed lapse rate.
!     --- work(nx,ny,nz) is a 3d real work array
!$$$
    implicit none

    real, dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: pm,Tm,zm,qvm
    real, dimension(IM,jsta_2l:jend_2u), intent(in) :: hgt
    real, dimension(IM,jsta_2l:jend_2u), intent(inout) :: trophtwmo

    ! work array
    real, dimension(IM,jsta_2l:jend_2u,LM) :: gamma ! gamma used for gamma=-dT/dz

    integer :: i,j,k,kp1,ktrop,ktrop1,ktrop2,ip1,im1,jp1,jm1
    real :: dTdz,dz
    integer :: nftxy,nftz,Filttype
    real :: ztrop,ztroplast,ztropij
    integer :: kmin,kmax,ktlast,knp,kk,kdifmin,navg
    real :: zqv1,zqv2
    integer :: kqv1,kqv2
    real :: qvr(LM)
    integer :: kwc(LM) ! size of a count, not actual size of LM

!   --- Assign lapse rate threshold gammat.  The WMO definition is
!   --- 2E-3 deg K/m, but in NWP model output this is usually too
!   --- restrictive so larger vlaues are required.  Note this is
!   --- expected to be model dependent.  The value used here work
!   --- reasonably well for the WRF RAP 13 km 50 level model.
    real, parameter :: gammat=5.0E-3 ! deg K/m
!   --- Assign humidity thresholds that could reasonably bound the
!   --- tropopause.  Since these values could be dependent on latitude
!   --- the bounds are specified in terms of a relative (to lower layers)
!   --- specific humidity.  Nominally these correspond roughly to 
!   --- 10^-5 < qv < 10^-4 kg/kg.  Note this these are expected to be
!   --- NWP model dependent.  The values used here work reasonably well
!   --- for the WRF RAP 13 km  50 level model.
    real, parameter :: qv1=0.10, qv2=0.008

    trophtwmo=SPVAL

    do j=jsta,jend
    do i=1,IM
!      --- Compute the lapse rate gamma between each pair of model levels.
       do k=LM-1,2,-1 ! GFS is top-bottom, original GTG is bottom-top
          kp1=k-1 ! GFS is top-bottom, original GTG is bottom-top 
          gamma(i,j,k)=SPVAL
!         dTdz=diregzk(T,z,i,j,k,nx,ny,nz)
          if(ABS(zm(i,j,k)-SPVAL)<SMALL1 .or. &
             ABS(zm(i,j,kp1)-SPVAL)<SMALL1) cycle
          dz=zm(i,j,kp1)-zm(i,j,k)
          if(ABS(Tm(i,j,k)-SPVAL)<SMALL1 .or. &
             ABS(Tm(i,j,kp1)-SPVAL)<SMALL1) cycle
          dTdz=(Tm(i,j,kp1)-Tm(i,j,k))/MAX(dz,1.)
!         --- Don't include uncomputed (i,j,k) or pts below terrain
          if(ABS(dTdz-SPVAL)<SMALL1) cycle
          gamma(i,j,k)=-dTdz
       enddo
       gamma(i,j,1)=gamma(i,j,2)
       gamma(i,j,LM)=gamma(i,j,LM-1)
    enddo
    enddo

!   --- Smooth gamma
    nftxy=1
    nftz=2
    Filttype=1
    call filt3d(1,LM,nftxy,nftz,Filttype,gamma)

!   --- Determine candidate tropopauses by scanning from (nominally)
!   --- west to east for each latitude (south to north) line.  Save the
!   --- last trop ht starting with i=1.
    do j=jend_m,jsta_m,-1 ! post is north-south, original GTG is south-north
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          ztroplast=SPVAL
          ktlast=-1

!         --- At the tropopause the specific humidity should fall off
!         --- rapidly.  Determine altitudes and corresponding altitude 
!         --- vertical indices where this seems to be happening.  In general
!         --- this will be a range from zqv1 to zqv2 where the relative
!         --- (to lower levels) specific humidity is between qv1 and qv2.
          call getqvbounds(qv1,qv2,pm(i,j,1:LM),zm(i,j,1:LM),qvm(i,j,1:LM),qvr,kqv1,kqv2)
!         --- Find the lowest candidate tropopause by searching upward 
!         --- from the surface until gamma < gammat
          if(ktlast>1) then
             kmin=ktlast-7
             kmax=ktlast+7
             kmin=MAX(kmin,3)
             kmin=MIN(kmin,LM)
             kmax=MAX(kmax,3)
             kmax=MIN(kmax,LM)
          endif
          kmin=3
          kmax=LM-2

          knp=0
          kwc=-1
          do k=kmax,kmin,-1
             if(ABS(gamma(i,j,k)-SPVAL)<SMALL1) cycle
             if(pm(i,j,k)<0. .or. &
                pm(i,j,k)>pmax) cycle ! don't consider levels with p>pmax
             if(ABS(zm(i,j,k)-SPVAL)<SMALL1 .or. &
                zm(i,j,k) < (hgt(i,j)+bldepth)) cycle
             if(zm(i,j,k)>20.E3) exit ! don't consider levels with z>20 km
!            --- Insist the current value plus the next two higher levels 
!            --- maintain gamma < gammat
             if((gamma(i,j,k)<=gammat) .and. &
                (gamma(i,j,k-1)<=gammat).and. &
                (gamma(i,j,k-2)<=gammat)) then
!               --- Count candidate trops
                knp=knp+1
                kwc(knp)=k+1
             endif
          enddo

          ktrop1=-1

!         --- Search upward in the list of candidate trop heights for location
!         --- where humidity drops off rapidly (from getqvbounds).
!         --- ktrop1 is the candidate k index at this location.  
!         --- If humidity bounds are not available use the last trop ht at (i-1,j)
          if(kqv1>1 .and. kqv2>1) then
             kdifmin=LM
             do kk=1, knp
                k=kwc(kk)
                if(k>=kqv1) cycle
                if((kqv1-k)<kdifmin) then
                   kdifmin=kqv1-k
                   ktrop1=k
                endif
             enddo
          else
!            --- Use last trop ht
             ktrop1=ktlast
          endif
!
!         --- Find the highest candidate tropopause by searching downward
!         --- from the model top and check against definition
          kmax=LM-1
          kmin=3
          if(ktrop1>1) kmax=ktrop1+1! GFS is top-bottom, original GTG is bottom-top

          ktrop2=-1

          do k=kmin,kmax ! GFS is top-bottom, original GTG is bottom-top
             if(ABS(gamma(i,j,k)-SPVAL)<SMALL1) cycle
             if(pm(i,j,k)<0.) cycle
             if(pm(i,j,k)>pmax) exit   ! don't consider levels with p>pmax
             if(ABS(zm(i,j,k)-SPVAL)<SMALL1 .or. &
                zm(i,j,k)>20.E3) cycle  ! don't consider levels with z>20 km
             if(gamma(i,j,k)>gammat) then
                ktrop2=k
                exit
             endif
          enddo

!       --- Determine whether to use the lower or upper tropopause.  
          ktrop=-1
          ztrop=SPVAL
          if(ktrop1>0 .or. ktrop2>0) then  ! at least one trop is found
             if(ktrop1>1 .and. ktrop2<=0) then
                ktrop=ktrop1  ! ktrop1 is valid, ktrop2 is not so use ktrop1
             elseif(ktrop2>1 .and. ktrop1<=0) then
                ktrop=ktrop2  ! ktrop2 is valid, ktrop1 is not so use ktrop2
             elseif(ktrop1>1 .and. ktrop2>1 .and. ktrop1==ktrop2)then
                ktrop=ktrop1  ! ktrop1=ktrop2 is valid, so use ktrop1
             elseif(ktrop1>1 .and. ktrop2>1 .and. ktrop1/=ktrop2)then
!            --- If both the lower and upper trops are valid use the one
!            --- closest to the altitudes between qv1 and qv2.  
!            --- The trop should be somewhere in this altitude range.
                if(kqv1>0 .and. kqv2>0) then
                   if((ktrop1<=kqv1 .and. ktrop1>=kqv2) .and. &
                      (ktrop2>kqv1 .or. ktrop2<kqv2)) then
                      ktrop=ktrop1  ! ktrop1 is reasonable, ktrop2 is not
                   elseif((ktrop2<=kqv1 .and. ktrop2>=kqv2) .and. &
                          (ktrop1>kqv1 .or. ktrop1<kqv2)) then
                      ktrop=ktrop2  ! ktrop2 is reasonable, ktrop1 is not
                   else
                      if(ABS(kqv1-kqv2)<= 2) then
!                        --- if qv limits are close use ktrop closest to kqv
                         if(ABS(ktrop1-kqv1)<ABS(ktrop2-kqv1)) then
                            ktrop=ktrop1
                         else
                            ktrop=ktrop2
                         endif
                      else
!                        --- both are equally reasonable or unreasonble so use lowest
                         ktrop=MAX(ktrop1,ktrop2) ! GFS is top-bottom, original GTG is bottom-top
                      endif
                   endif
                else
!                  --- qv tests provided unreasonable results so use lowest trop estimate
                   ktrop=MAX(ktrop1,ktrop2) ! GFS is top-bottom, original GTG is bottom-top
                endif
             endif
             if(ktrop>1) then
                ztrop=zm(i,j,ktrop)
             else
                if(ktrop1>1) then
                   ktrop=ktrop1
                   ztrop=zm(i,j,ktrop1)
                end if
             endif
          endif
!         --- If search was unsuccessful set the tropht to the last tropht (i-1,j) 
          if(ktrop<=0 .and. ktlast>=5 .and. ktlast<=LM-3) then
             ktrop=ktlast
             ztrop=ztroplast
          else
             ktlast=-1
             ztroplast=SPVAL
             trophtwmo(i,j)=SPVAL
          endif

          ! Have to leave some 'gaps' of trophtwmo at grid point (i-1,j+1) 
          ! when i==1 or j==jend, since trophtwmo relies on itself
          if_ktrop: if(ktrop>1) then
             ztropij=ztrop
             navg=1
             if(ABS(trophtwmo(im1,j)-SPVAL)>SMALL1) then
                ztrop=ztrop+trophtwmo(im1,j)
                navg=navg+1
             endif
!             if(ABS(trophtwmo(im1,jm1)-SPVAL)>SMALL1) then
!                ztrop=ztrop+trophtwmo(im1,jm1)
!                navg=navg+1
!             endif
!             if(ABS(trophtwmo(i,jm1)-SPVAL)>SMALL1) then
!                ztrop=ztrop+trophtwmo(i,jm1)
!                navg=navg+1
!             endif
             if(ABS(trophtwmo(ip1,j)-SPVAL)>SMALL1) then
                ztrop=ztrop+trophtwmo(ip1,j)
                navg=navg+1
             endif
             ztrop=ztrop/MAX(navg,1)

             do k=LM-2,2,-1 ! GFS is top-bottom, original GTG is bottom-top
                if(zm(i,j,k)>ztrop) then
                   ktrop=k
                   exit
                endif
             enddo
             trophtwmo(i,j)=ztrop
             ktlast=ktrop
             ztroplast=ztrop
          endif if_ktrop
       enddo  ! i loop
    enddo  ! j loop

    return
  end subroutine tropwmo

!-----------------------------------------------------------------------
  subroutine tropN2N1(zm,pm,Nsqm,qvm,hgt,ztropwmo,trophtn)
!$$$ SUBPROGRAM DOCUMENTATION BLOCK
!     --- Determines the tropopause height (m) based on abrupt stability
!     --- change near the wmo defined tropopause.
!     --- Input ztropwmo(nx,ny) is input trop ht by WMO definition (m).
!     --- Output trophtn(nx,ny) is ouput trop ht based on stability change (m).
!$$$
    implicit none

    real, dimension(IM,jsta_2l:jend_2u,LM),intent(in) :: zm,pm,Nsqm,qvm
    real, dimension(IM,jsta_2l:jend_2u), intent(in) :: hgt,ztropwmo
    real, dimension(IM,jsta_2l:jend_2u), intent(inout) :: trophtn

    integer :: i,j,k,ip1,im1,jp1,jm1
    integer :: kmin,kmax, kmin1,kmax1
    integer :: navg
    integer :: ktwmo,ktrop,ktlast,kn2,kn1,knp,kk,kdifmin
    real :: rstabtest
    real :: N1sq,N2sq,N2test
    real :: ztrop,troprn,ztroplast,ztropij
    real :: zqv1,zqv2
    integer :: kqv1,kqv2
    real, parameter :: pmax=800.E2
!     --- Assign humidity thresholds that could reasonably bound the
!     --- tropopause.  Since these values could be dependent on latitude
!     --- the bounds are specified in terms of a relative (to lower layers)
!     --- specific humidity.  Nominally these correspond roughly to 
!     --- qv < 0.1 of its low level average (sfc-500mb).  Note this these are expected to be
!     --- NWP model dependent.  The values used here work reasonably well
!     --- for the WRF RAP 13 km  50 level model.
    real, parameter :: qv1=0.10, qv2=3.0E-4
    real, dimension(LM) :: pk,Nsqk,zk,stablk,stabuk,rstabk
    real :: qvr(LM) 
    integer :: kpeak(LM) ! size of a count, not actual size of LM

    trophtn(i,j)= SPVAL

!     --- Determine candidate tropopauses by scanning from (nominally)
!     --- west to east for each latitude (south to north) line.  Save the
!     --- last trop ht starting with i=1.
    do j=jend_m,jsta_m,-1 ! post is north-south, original GTG is south-north
       do i=1,IM
          ip1=i+1
          im1=i-1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          ztroplast=SPVAL
          ktlast=-1

!         --- Check for missing values
          do k=2,LM-1
             Nsqk(k)=SPVAL
             zk(k)  =SPVAL
             pk(k)  =SPVAL
             kpeak(k)=-1    
             pk(k) =pm(i,j,k)
             if(ABS(Nsqm(i,j,k)-SPVAL)<SMALL1 .or. &
                ABS(zm(i,j,k)-SPVAL)<SMALL1) cycle
             Nsqk(k)=Nsqm(i,j,k)
             zk(k)=zm(i,j,k)
          enddo

!       --- At the tropopause the specific humidity should fall off
!       --- rapidly.  Determine altitudes and corresponding altitude 
!       --- vertical indices where this seems to be happening.  In general
!       --- this will be a range from zqv1 to zqv2 where the relative
!       --- (to lower levels) specific humidity is between qv1 and qv2.
          call getqvbounds(qv1,qv2,pm(i,j,1:LM),zm(i,j,1:LM),qvm(i,j,1:LM),qvr,kqv1,kqv2)
!
!       --- Get tropopause height from wmo definition
          ktwmo=-1
          ztropij=ztropwmo(i,j)
          if(.not. ABS(ztropij-SPVAL)<SMALL1) then
             ktwmo=-1
             do k=LM-2,2,-1 ! GFS is top-bottom, original GTG is bottom-top
                if(zm(i,j,k)>ztropij) exit
                ktwmo=k
             enddo
          endif
          if(ktwmo<=0) then
!            --- If valid tropwmo is not available set kt to nz/2
             ktwmo=LM/2
          endif

!         --- Set vertical index ranges to consider based on either the 
!         --- the last index or the index of the wmo trop ht.
          kmin=3
          kmax=LM-1
          if(i==1) then
             kmin=ktwmo-5
             kmax=ktwmo+5
             kmin=MIN(kmin,kqv2-1)
             kmax=MAX(kmax,kqv1+1)
          endif
          kmin=MAX(kmin,3)
          kmin=MIN(kmin,LM-1)
          kmax=MAX(kmax,3)
          kmax=MIN(kmax,LM-1)

!         --- Search for tropopause at this (i,j) based on stability change.
!         --- First determine boundaries in vertical search kmin and kmax based
!         --- on comparisons to ktrop derived from wmo definition and the last
!         --- stability-derived definition.  The minimum N2/N1 ratio to 
!         --- consider a candidate tropopause is rstabtest=1.1.  This value
!         --- may be NWP model dependent.
          rstabtest=1.1

!         --- Get stabilities N1sq,N2sq averaged over depth ztropupper,lower
!         --- and their ratio at every level 
          call tropstab(2,LM-1,zk,Nsqk,stablk,stabuk,rstabk)
!
!         --- Look for an isolated stability maximum between kmin and kmax.
!         --- Search upward to get the lowest two candidate trophts.
          troprn=-1.
          ktrop=-1
          kn1=-1
          kn2=-1
          ztrop=SPVAL
          N2test=1.7E-4
          kn2=-1
          knp=0
          do k=2,LM-1 ! GFS is top-bottom, original GTG is bottom-top
             if(ABS(zk(k)-SPVAL)<SMALL1) cycle
             if(zk(k)>20.E3) cycle ! don't consider levels with z>20 km
             if(ABS(pk(k)-SPVAL)<SMALL1) cycle
             if(pk(k)>pmax) exit   ! don't consider levels with p>pmax
             if(zk(k) < (hgt(i,j)+bldepth)) exit
             N1sq=stablk(k)
             N2sq=stabuk(k)
             if(ABS(N1sq-SPVAL)<=SMALL1 .or. &
                ABS(N2sq-SPVAL)<=SMALL1 .or. &
                ABS(rstabk(k)-SPVAL)<=SMALL1) cycle
!            --- Isolate peaks in stability ratios where the current N2/N1
!            --- ratio exceeds rstabtest.
             if(rstabk(k)>rstabk(k-1) .and. rstabk(k)>rstabk(k+1) .and. &
                rstabk(k)>rstabtest .and. N2sq>=N2test) then
                knp=knp+1
                kpeak(knp)=k
             endif
          enddo

!         --- Loop over all candidate trops and use the one closest to the
!         --- lower humidity bound.
          if(knp==1) then
             ktrop=kpeak(knp)
             ztrop=zk(ktrop)
          else
             kdifmin=LM
             do kk=1,knp
                k=kpeak(kk)
                if(k==kmin .or. k==kmax) cycle
                if(ABS(k-ktwmo)<kdifmin) then
                   kdifmin=ABS(k-ktwmo)
                   ktrop=k
                   ztrop=zk(ktrop)
                endif
             enddo
          endif

!       --- If search was unsuccessful set the tropht to the last tropht 
!       --- (if valid), or to the input wmo tropht (if valid).
          if(ktrop<=0) then
             if(ktlast>=3 .and. ktlast<=LM-1) then
                ktrop=ktlast
                ztrop=zk(ktrop)
             elseif(ktwmo>=3 .and. ktwmo<=LM-1) then
                ktrop=ktwmo
                ztrop=zk(ktwmo)
             endif
          endif

!         --- Set the final tropht and move on to the next i point
          ! Have to leave some 'gaps' of trophtn at grid point (i-1,j+1) 
          ! when i==1 or j==jend, since trophtn relies on itself
          if(ktrop>1) then
             ztropij=ztrop
             navg=1
             if(ABS(trophtn(im1,j)-SPVAL)>SMALL1) then
                ztrop=ztrop+trophtn(im1,j)
                navg=navg+1
             endif
!             if(ABS(trophtn(im1,jm1)-SPVAL)>SMALL1) then
!                ztrop=ztrop+trophtn(im1,jm1)
!                navg=navg+1
!             endif
!             if(ABS(trophtn(i,jm1)-SPVAL)>SMALL1) then
!                ztrop=ztrop+trophtn(i,jm1)
!                navg=navg+1
!             endif
             if(ABS(trophtn(ip1,j)-SPVAL)>SMALL1) then
                ztrop=ztrop+trophtn(ip1,j)
                navg=navg+1
             endif
             ztrop=ztrop/MAX(navg,1)

             do k=LM-2,2,-1 ! GFS is top-bottom, original GTG is bottom-top
                if(zk(k)>ztrop) then
                   ktrop=k
                   exit
                endif
             enddo
          endif
          trophtn(i,j)=ztrop
          ktlast=ktrop
          ztroplast=ztrop

       enddo  ! i loop
    enddo  ! j loop

    return
  end subroutine tropN2N1

!-----------------------------------------------------------------------
  subroutine tropstab(kmin,kmax,zk,Nsqk,stabavglk,stabavguk,rstabk)
!$$$ SUBPROGRAM DOCUMENTATION BLOCK
!     --- Computes average stability above and below each level k
!     --- on a column of data at (i,j) and the ratio of the stabilities
!     --- at k levels between kmin and kmax.
!     --- On output stabavglk is average stability (s^-2) below level k,
!     --- stabavguk is average stability (s^-2) above level k, and 
!     --- rstabk=stabavgu/stabavgl.  
!     --- The averging depth used is zavgu,zavgl (m) and is specified below.
!     --- These may be model dependent.
!$$$
    implicit none
    integer, intent(in) :: kmin,kmax
    real, intent(in) :: zk(LM),Nsqk(LM)
    real, intent(inout) :: stabavglk(LM),stabavguk(LM),rstabk(LM)

    integer :: ktrop
    real :: ztrop,troprn
    integer :: k,kk,klower,kupper
    integer :: navgu,navgl
    real :: stabavgkk,stabavgu,stabavgl
    real :: zkk,dzk
    real, parameter ::  zavgu=4000., zavgl=4000.  ! m

!   --- Initializations
    ztrop=SPVAL
    ktrop=-1
    troprn=-1.0E20
    klower=-1
    kupper=-1
    do k=1,LM
       stabavglk(k)=Nsqk(k)
       stabavguk(k)=Nsqk(k)
       rstabk(k)=SPVAL
    enddo

    do k=kmax,kmin,-1 ! GFS is top-bottom, original GTG is bottom-top
!      --- Compute average stability in the layer between z(k)-zavgl and 
!      --- z(k).  Average over at least two vertical grid points.
       stabavgl=0.
       navgl=0
       do kk=k+1,LM-2 ! GFS is top-bottom, original GTG is bottom-top
          zkk=zk(kk)
          dzk=zk(k)-zkk
          if(dzk>zavgl .and. navgl>=2) exit
!         --- lower layer
          stabavgkk=Nsqk(kk)
          if(ABS(stabavgkk-SPVAL)>SMALL1) then
             stabavgl=stabavgl+stabavgkk
             navgl=navgl+1
             klower=kk
          endif
       enddo
       stabavgl=stabavgl/MAX(navgl,1)
!      --- Compute average stability in the layer between z(k) and 
!      --- z(k)+zavgu.  Average over at least two vertical grid points.
       stabavgu=0.
       navgu=0
       do kk=k-1,2,-1 ! GFS is top-bottom, original GTG is bottom-top
          if(kk<=1) exit
!         --- upper layer
          zkk=zk(kk)
          dzk=zkk-zk(k)
          if(dzk>zavgu .and. navgu>=2) exit
          stabavgkk=Nsqk(kk)
          if(ABS(stabavgkk-SPVAL)>SMALL1) then
             stabavgu=stabavgu+stabavgkk
             navgu=navgu+1
             kupper=kk
          endif
       enddo
       stabavgu=stabavgu/MAX(navgu,1)
       stabavguk(k)=stabavgu
       stabavglk(k)=stabavgl
       rstabk(k)=stabavgu/MAX(stabavgl,1.0E-4)
    enddo  ! k loop

    return
  end subroutine tropstab

!-----------------------------------------------------------------------
  subroutine getqvbounds(qv1,qv2,pm,zm,qvm,qvr,kqv1,kqv2)
!$$$ SUBPROGRAM DOCUMENTATION BLOCK
!     --- At the tropopause the specific humidity should fall off
!     --- rapidly.  This routine determines altitudes and altitude
!     ---  indices closest to the altitudes at grid column (i,j)
!     ---  where this seems to be happening.  In general this will
!     ---  be a range from zqv1 to zqv2 where the
!     --- relative (to lower levels) specific humidity is between the
!     --- input qv1 and qv2 values.  qvr is output relative qv values in 
!     --- the column.
!$$$
! For GFS from top-bottom, kqv1 (lower level) > kqv2 (higher level)

    implicit none
    real,intent(in) :: qv1,qv2
    real, dimension(LM),intent(in) :: pm,zm,qvm
    real, dimension(LM),intent(inout) :: qvr(LM)
    integer, intent(out) :: kqv1,kqv2

    real :: zqv1,zqv2
    integer :: k
    real :: qvavg
    integer :: nqavg

!   --- Initializations
    kqv1=-1
    kqv2=-1
    zqv1=-1.
    zqv2=-1.

!   --- First smooth qv vertically
    do k=2,LM-1
       qvr(k)=(qvm(k+1)+qvm(k)+qvm(k-1))/3.
    enddo
    qvr(1)=qvm(1)
    qvr(LM)=qvm(LM)

!   --- Get qv relative to low level average (levels < 500 mb)
    qvavg=0.
    nqavg=0
    do k=LM-1,2,-1  ! GFS is top-bottom, original GTG is bottom-top
       if(pm(k)<500.E2) exit
       qvavg=qvavg+qvr(k)
       nqavg=nqavg+1
    enddo
    if(qvavg<1.0E-12) return
    qvavg=qvavg/FLOAT(MAX(nqavg,1))
    do k=1,LM
       qvr(k)=qvr(k)/MAX(qvavg,1.0E-12)
    enddo

!   --- Work up from the surface and find the lowest level where 5
!   --- adjacent qvr values are <= qv1 and <= qv2.
    do k=LM-1,5,-1 ! GFS is top-bottom, original GTG is bottom-top
       if(pm(k)<=500.E2) then
          if(zqv1<0 .and. qvr(k)<=qv1 .and. &
             ! GFS is top-bottom, original GTG is bottom-top
             qvr(k-1)<=qv1 .and. qvr(k-2)<=qv1 .and. &
             qvr(k-3)<=qv1 .and. qvr(k-4)<=qv1) then
             kqv1=k
             zqv1=zm(k)
          endif
          if(zqv2<0 .and. qvr(k)<=qv2 .and. &
             qvr(k-1)<=qv2 .and. qvr(k-2)<=qv2 .and. &
             qvr(k-3)<=qv2 .and. qvr(k-4)<=qv2) then
             kqv2=k
             zqv2=zm(k)
          endif
       endif
    enddo
!   --- If the lower altitude was not found try to find the lowest
!   --- levels where one qvr value is <= qv1 and <= qv2.
    if(kqv1<=0) then
       do k=LM-1,2,-1 ! GFS is top-bottom, original GTG is bottom-top
          if(pm(k)<=500.E2) then
             if(zqv1<0 .and. qvr(k)<=qv1) then
                kqv1=k
                zqv1=zm(k)
             endif
          endif
       enddo
    endif
    if(kqv2<=0) then
       do k=LM,1,-1 ! GFS is top-bottom, original GTG is bottom-top
          if(pm(k)<=500.E2) then
             if(zqv2<0 .and. qvr(k)<=qv2) then
                kqv2=k
                zqv2=zm(k)
             endif
          endif
       enddo
    endif

    return
  end subroutine getqvbounds

!-----------------------------------------------------------------------
  subroutine checktrop(zt,ztm)
!   --- Check and try to correct for wild points in zt.  Output ztm is
!   --- 9 pt average of zt
    implicit none

    real, intent(inout) :: zt(IM,jsta_2l:jend_2u)
    real, intent(inout) :: ztm(IM,jsta_2l:jend_2u)

    real :: Ti2d(IM,jsta_2l:jend_2u)
    integer :: navg,i,ii,j,jj,isum
    integer :: nftxy
    integer, parameter :: np=15   ! number of points in x running mean
    real :: tyavg,tterm
    real(kind=8) :: txsum
    integer :: nsum

!   --- Apply median filter.  This should take care of most wild points.
    call medianFilter2D(zt,Ti2d)

    call exch2(Ti2d(1,jsta_2l))

!   --- Apply 15-pt running mean in x to 3-pt average in y
    do i=1,IM
       if(jsta==1) ztm(i,jsta)=Ti2d(i,jsta)
       do j=jsta_m,jend_m
          ztm(i,jsta)=SPVAL
          tyavg=0.
          navg=0
          do jj=j-1,j+1
             if(ABS(Ti2d(i,jj)-SPVAL)<SMALL1) cycle
             tyavg=tyavg+Ti2d(i,jj)
             navg=navg+1
          enddo
          if(navg>0) ztm(i,j)=tyavg/FLOAT(navg)
       enddo
       if(jend==JM) ztm(i,jend)=Ti2d(i,jend)
    enddo

    navg=np/2
    if(modelname == 'GFS') then  ! wrap around
       do j=jsta,jend
       do i=1,IM
          Ti2d(i,j)=ztm(i,j)
          txsum=0.
          nsum=0
          do isum=-navg,+navg
             ii=i+isum
             if(ii>IM) ii=ii-IM
             if(ii<1) ii=ii+IM         
             tterm=ztm(ii,j)
             if(ABS(tterm-SPVAL)<SMALL1) cycle
             nsum=nsum+1
             txsum=txsum+tterm
          enddo
          if(nsum>0) Ti2d(i,j)=txsum/FLOAT(nsum)
       enddo
       enddo
    else
       do j=jsta,jend
       do i=1,IM
          Ti2d(i,j)=ztm(i,j)
          if(i<np/2+1) then
             txsum=0.
             nsum=0
             do isum=i,i+np-1
                tterm=ztm(isum,j)
                if(ABS(tterm-SPVAL)<SMALL1) cycle
                nsum=nsum+1
                txsum=txsum+tterm
             enddo
             if(nsum>0) Ti2d(i,j)=txsum/FLOAT(nsum)
          elseif(i>IM-np/2) then
             txsum=0.
             nsum=0
             do isum=i-np+1,i
                tterm=ztm(isum,j)
                if(ABS(tterm-SPVAL)<SMALL1) cycle
                nsum=nsum+1
                txsum=txsum+tterm
             enddo
             if(nsum>0) Ti2d(i,j)=txsum/FLOAT(nsum)
          else
             txsum=0.
             nsum=0
             do isum=-navg,+navg
                tterm=ztm(i+isum,j)
                if(ABS(tterm-SPVAL)<SMALL1) cycle
                nsum=nsum+1
                txsum=txsum+tterm
             enddo
             if(nsum>0) Ti2d(i,j)=txsum/FLOAT(nsum)
          endif
       enddo
       enddo
    endif

!   --- Smooth several times with 1-2-1 smoother
!   --- Output is in ztm.
    nftxy=8
    call meanFilter2D(nftxy,Ti2d,ztm)

    return
  end subroutine checktrop

!-----------------------------------------------------------------------
  subroutine tropr(zm,Nsqm,Rim,tropht,ztroplower,ztropupper,TI3)
!$$$ SUBPROGRAM DOCUMENTATION BLOCK
!     --- Computes two trop strength indices on the input grid:
!     --- 1) rstab=Nsq2/Nsq1, 2) rRi=Ri2/Ri1.  
!     --- Output is in TI3(i,j,10) and TI3(i,j,11) respectively.
!     --- Input ztroplower,ztropupper (m) specify the averaging depths.
!$$$
    implicit none
    real, intent(in) :: tropht(IM,jsta_2l:jend_2u)
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(in) :: zm,Nsqm,Rim
    real, intent(in) :: ztroplower,ztropupper
    real, intent(inout) :: TI3(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k
    integer :: klower
    real :: zk,ztrop
    real :: Nsq,stabavgu,stabavgl,rstab,rRi
    real :: Ri,Riavgu,Riavgl
    integer :: navgu,navgl

!     --- Compute N^2(z),Ri(z) at (i,j).  
    do j=jsta, jend
    do i=1,IM
       ztrop=tropht(i,j)    ! m
       if(ztrop<=0.) cycle
       stabavgl=0.
       Riavgl=0.
       navgl=0

!      --- Compute average stability and Ri in the layer between
!      --- ztrop-ztroplower to ztrop
       klower=1
       do k=LM-1,1,-1 ! GFS is top-bottom, original GTG is bottom-top
          if(ABS(zm(i,j,k)-SPVAL)<SMALL1) cycle
          zk=zm(i,j,k)  ! m
          Nsq=Nsqm(i,j,k)
          Ri=Rim(i,j,k)
          if(zk>=(ztrop+ztroplower)) then
!             --- ztroplower layer
             if(ABS(Nsq-SPVAL)<SMALL1) cycle
             stabavgl=stabavgl+Nsq
             if(ABS(Ri-SPVAL)<SMALL1) cycle
             Riavgl=Riavgl+Ri
             navgl=navgl+1
             klower=k
             if(zk>ztrop) exit
          endif
       enddo
       stabavgl=stabavgl/MAX(navgl,1)
       Riavgl=Riavgl/MAX(navgl,1)
       stabavgu=0.
       Riavgu=0.
       navgu=0
!
!      --- Compute average stability and Ri in the layer between
!      --- ztrop and ztrop+ztropupper
       stabavgu=0.
       Riavgu=0.
       navgu=0
       klower=MAX(klower-1,1) ! GFS is top-bottom, original GTG is bottom-top
       klower=MIN(klower,LM)  ! GFS is top-bottom, original GTG is bottom-top
       do k=klower,1,-1 ! GFS is top-bottom, original GTG is bottom-top
!         --- ztropupper layer
          if(ABS(zm(i,j,k)-SPVAL)<SMALL1) cycle
          zk=zm(i,j,k)  ! m
          Nsq=Nsqm(i,j,k)
          Ri=Rim(i,j,k)
          if(zk>=ztrop) then
             if(ABS(Nsq-SPVAL)<SMALL1) cycle
             stabavgu=stabavgu+Nsq
             if(ABS(Ri-SPVAL)<SMALL1) cycle
             Riavgu=Riavgu+Ri
             navgu=navgu+1
             if(zk>ztrop+ztropupper) exit
          endif
       enddo  ! k loop
       stabavgu=stabavgu/MAX(navgu,1)
       Riavgu=Riavgu/MAX(navgu,1)
       if(ABS(stabavgl)>1.0E-7) then
          rstab=stabavgu/stabavgl
          TI3(i,j,10)=rstab
       endif
       if(ABS(Riavgl)>1.0E-4) then
          rRi=Riavgu/Riavgl
          TI3(i,j,11)=rRi
       endif
    enddo ! i
    enddo ! j

    return
  end subroutine tropr

end module gtg_trophts
