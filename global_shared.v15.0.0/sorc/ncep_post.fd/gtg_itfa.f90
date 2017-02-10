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

module gtg_itfa
  use ctlblk_mod, only: jsta,jend, jsta_2l,jend_2u,IM,JM,LM, SPVAL
  use gtg_config, only : MAXREGIONS,IDMAX,static_wgt,use_equal_wts
  use gtg_config, only : remap_option,timap,tis,NTI
  use gtg_config, only : clampidxL,clampidxH,clampitfaL,clampitfaH
  use gtg_config, only : printflag
  use gtg_filter

  implicit none

  integer :: ic,jc
contains

  subroutine ITFAcompF(ipickitfa,kregions,ncat,cat,comp_ITFAMWT,comp_ITFADYN,&
                       qitfax,catonly,mwt)
! Computes ITFA combinations as the weighted sum of turbulence indices.
! static_wgt(MAXREGIONS, IDMAX) is from gtg_config

! idQ=491 for itfa CAT combination using dynamic weights
! idQ=492 for itfa CAT combination using static weights
! idQ=493 for itfa MWT combination using static weights
! idQ=494 for MAX(CAT, MWT) itfa combinations using static weights
! idQ=495 for MAX(CAT, MWT) itfa combinations using dynamic weights

    implicit none

    integer,intent(in) :: ipickitfa(MAXREGIONS,IDMAX)
    integer,intent(in) :: kregions(MAXREGIONS,2)
    integer,intent(in) :: ncat
    real,intent(in) :: cat(IM,jsta_2L:jend_2u,LM,ncat)
    logical, intent(in) :: comp_ITFAMWT,comp_ITFADYN
!   qitfax=Output MAX(CAT, MWT) itfa combinations
    real,intent(inout) :: qitfax(IM,jsta_2l:jend_2u,LM)
    real,intent(inout),dimension(IM,jsta_2l:jend_2u,LM),optional :: catonly,mwt

    ! work arrays:
    ! qitfa=Output itfa CAT combination using static/dynamic weights
    ! qitfam=Output itfa MWT combination using static weights
    real, allocatable :: qitfa(:,:,:),qitfam(:,:,:)

    ! for one kregion, save for MWT/CAT
    integer :: kpickitfa(ncat)
    real :: wts(ncat)

    integer :: iregion,idx,kmin,kmax

    write(*,*) 'enter ITFAcompF'

    allocate(qitfa(IM,jsta_2l:jend_2u,LM))
    allocate(qitfam(IM,jsta_2l:jend_2u,LM))

    ! to match NCAR's (321,541)
    ic=321
    jc=jend ! JM-jc+1
    ! Convert to GFS's
    ic=(ic+IM/2)  !from [-180,180] to [0,360]
    if (ic > IM) ic = ic-IM

    ! to debug PE test (1038,225)
    ic=1038
    jc=225
    if(jsta<=jc .and. jend>=jc) then
       jc=jc
    else
       jc=jend
    end if

    qitfa = SPVAL ! default CAT is missing
    qitfam = 0. ! default MWT is 0.0
    qitfax = 0.

    if(use_equal_wts) static_wgt = 1

    loop_iregion: do iregion=1,MAXREGIONS

       kmin=kregions(iregion,2)
       kmax=kregions(iregion,1)

       print *, "itfa iregion=",iregion

       if_ITFADYN: if(comp_ITFADYN) then 
          ! Comute an ITFA based on dynamic weights
          ! not available for current version
          qitfa = SPVAL
       else
          kpickitfa = 0
          wts = 0.
          do idx = 1, ncat
             if(ipickitfa(iregion,idx)<=475 .and. &
                ipickitfa(iregion,idx)>0) then ! CAT
                kpickitfa(idx) = ipickitfa(iregion,idx)
                wts(idx) = static_wgt(iregion,kpickitfa(idx)-399)
             end if
          end do
          if (sum(kpickitfa) == 0) then
             write(*,*) "There is no CAT static indices picked"
          else
             print *, "selected cat idx=",kpickitfa(1:ncat)
             print *, "selected cat weights=",wts(1:ncat)

             ! Compute an ITFA combination using a set of default weight
             call itfasum(iregion,kmin,kmax,ncat,kpickitfa,wts,cat,qitfa)
          end if
       end if if_ITFADYN

      if_ITFAMWT: if(comp_ITFAMWT) then
          kpickitfa = 0
          wts = 0.
          do idx = 1, ncat
             if(ipickitfa(iregion,idx) >= 476 .and. &
                ipickitfa(iregion,idx) <= 490) then ! MWT
                kpickitfa(idx) = ipickitfa(iregion,idx)
                wts(idx)=static_wgt(iregion,kpickitfa(idx)-399)
             end if
          end do
          if (sum(kpickitfa) == 0) then
             write(*,*) "There is no MWT indices picked"
          else
             print *, "selected mwt idx=",kpickitfa(1:ncat)
             print *, "selected mwt weights=",wts(1:ncat)

             ! Compute an ITFA combination using a set of default weight
             call itfasum(iregion,kmin,kmax,ncat,kpickitfa,wts,cat,qitfam)
          end if
       else
          qitfam = 0.
       end if if_ITFAMWT

    end do loop_iregion

    qitfax = SPVAL
    call itfamax(kregions,qitfa,qitfax,catonly)
    qitfax = 0.
    call itfamax(kregions,qitfax,qitfam,mwt)

    if(printflag>=2) write(*,*) "Before itfamax, qitfa=",qitfa(ic,jc,1:LM)
    if(printflag>=2) write(*,*) "Before itfamax, qitfam=",qitfam(ic,jc,1:LM)

    ! Now obtain ITFAMAX=MAX(ITFA,ITFAMWT)
    call itfamax(kregions,qitfa,qitfam,qitfax)

    if(printflag>=2) write(*,*) "After itfamax, qitfa=", qitfax(ic,jc,1:LM)


    deallocate(qitfa)
    deallocate(qitfam)

    return
  end subroutine ITFAcompF

!-----------------------------------------------------------------------
  subroutine itfasum(iregion,kmin,kmax,ncat,kpickitfa,wts,cat,qitfa)
! Given a set of weights wts, computes the itfa combination stored in cat

    implicit none

    integer,intent(in) :: iregion
    integer,intent(in) :: kmin,kmax
    integer,intent(in) :: ncat
    integer,intent(in) :: kpickitfa(ncat)
    real,intent(in)    :: wts(ncat)
    real,intent(in)    :: cat(IM,jsta_2l:jend_2u,LM,ncat)
    real,intent(inout) :: qitfa(IM,jsta_2l:jend_2u,LM)

    integer :: i,j,k,idx
    real :: weight,wtsnorm(ncat)
    real :: qitfalast,qijk,qs,wqs
    ! nitfa is the output number of indices used.
    integer :: nitfa

    if(printflag>=2) then
       write(*,*) 'enter itfasum,iregion='
       write(*,*) 'kpickitfa=',kpickitfa
       write(*,*) 'wts=',wts
    end if

    ! Normalize weigts so sum(weights)=1
    weight = sum(wts)
    if(weight <= 0) then
       write(*,*) "CAT/MWT weight not set for iregion", iregion
       return
    elseif(weight >= 1.) then
       do idx=1,ncat
          wtsnorm(idx) = wts(idx)/weight
       end do
    end if

    if(printflag>=2) write(*,*) "iregion, kpickitfa,normalized weights=", iregion, kpickitfa,wtsnorm
    

!   --- Loop over all 'picked' indices in the sum
    nitfa = 0
    loop_n_idx: do idx=1,ncat
       if(kpickitfa(idx) <= 0) cycle


       weight=wtsnorm(idx)

       if(printflag>=2) write(*,*) "iregion,idx,idQ, kmin,kmax,weight,timap=",iregion,idx, kpickitfa(idx),kmin,kmax,weight,timap(iregion,kpickitfa(idx)-399,1:NTI)

       ! --- Compute the weighted sum and store in qitfa for the current region
       do k=kmin,kmax
       do j=jsta,jend
       do i=1,IM
          if(nitfa == 0) then
             qitfalast = 0.
          else
             qitfalast = qitfa(i,j,k)
             if(ABS(qitfalast-SPVAL) < SMALL1) qitfalast = 0.
          endif

!         remap the raw index value to edr
          qijk = cat(i,j,k,idx)
          call remapq(iregion,kpickitfa(idx)-399,qijk,qs)
          if(ABS(qs-SPVAL)<SMALL1) cycle
          wqs=weight*MAX(qs,0.)
          qitfa(i,j,k)=qitfalast+wqs
          if(printflag>=2 .and. i==ic .and. j==jc) then
             write(*,*) "idx,idQ,i,j,k,cat,qs,wqs,qitfa=",idx,kpickitfa(idx),i,j,k,cat(i,j,k,idx),qs,wqs,qitfa(i,j,k)
          end if
       enddo
       enddo
       enddo
       nitfa = nitfa+1
    end do loop_n_idx

!   Clamp the resultant sum between clampL and clampH
    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM
       qijk=qitfa(i,j,k)
       if(ABS(qijk-SPVAL) < SMALL1) cycle
       qijk=MAX(qijk,clampitfaL)
       qijk=MIN(qijk,clampitfaH)
       qitfa(i,j,k)=qijk
    end do
    end do
    end do

    if(printflag>=2) write(*,*) "after sum, qitfa=",qitfa(ic,jc,1:LM)
    call MergeRegions(iregion,kmax,qitfa)
    if(printflag>=2) write(*,*) "after merge, qitfa=",qitfa(ic,jc,1:LM)

    return
  end subroutine itfasum

!-----------------------------------------------------------------------
  subroutine itfamax(kregions,qitfa,qitfam,qitfax)
! Reads in ITFA (CAT) and ITFA (MWT) and takes max of two, 
! grid point by grid point, and outputs as qitfax.
! qit is a work array.
! If ioutputflag > 0 qitfa is also stored on disk in directory Qdir as idQ.Q. 
! On input ITFADEF or ITFADYN is in qitfa and ITFAMWT is in qitfam.
! The max is only computed between i=imin,imax, j=jmin,jmax,
! k=kmin,kmax and where mask(i,j)>0.

    implicit none

    integer,intent(in) :: kregions(MAXREGIONS,2)
    real,intent(in) :: qitfa(IM,jsta_2l:jend_2u,LM) 
    real,intent(in) :: qitfam(IM,jsta_2l:jend_2u,LM) 
    real,intent(inout) :: qitfax(IM,jsta_2l:jend_2u,LM) 

    integer :: kmin,kmax,iregion
    integer :: i,j,k
    real :: qi,qm,qijk

    integer :: Filttype,nftxy,nftz

    qitfax = SPVAL

    loop_iregion: do iregion=1,MAXREGIONS

       kmin=kregions(iregion,2)
       kmax=kregions(iregion,1)

!      Now obtain ITFAMAX=MAX(ITFA,ITFAMWT)
       do k=kmin,kmax
       do j=JSTA,JEND
       do i=1,IM
          qi=qitfa(i,j,k)  ! CAT index
          qm=qitfam(i,j,k) ! MWT index
          if(ABS(qi-SPVAL)<SMALL1) then
             qijk=qm
          elseif(ABS(qm-SPVAL)<SMALL1) then
             qijk=qi
          else
             qijk=MAX(qi,qm)
          end if
          qijk=MAX(qijk,clampitfaL)
          qijk=MIN(qijk,clampitfaH)
          qitfax(i,j,k)=qijk
       enddo
       enddo
       enddo

!      Merge the regions
       call MergeRegions(iregion,kmax,qitfax)
    end do loop_iregion

!   Perform one smoothing for the blend, and for consistency
!   also smooth ITFA
    Filttype=1
    nftxy=1
    nftz=1
    call filt3d(1,LM,nftxy,nftz,Filttype,qitfax)

    return
  end subroutine itfamax

!-----------------------------------------------------------------------
  subroutine MergeRegions(iregion,kmax,qitfa)
! Merge the boundaries of the regions of multiple regions

    implicit none
     
    integer,intent(in) :: iregion
    integer,intent(in) :: kmax
    real,intent(inout) :: qitfa(IM,jsta_2l:jend_2u,LM) 

    integer :: i,j,k
    real :: qijk,qsum,qk(LM)
    integer :: ksum
    integer :: kbdy
    if(printflag>=2) write(*,*) 'enter MergeRegions'

    if(iregion == 1) return

    kbdy = kmax ! GFS is top-bottom
    kbdy = MAX(kbdy,3)
    kbdy = MIN(kbdy,LM-2)

    do j=jsta,jend
    do i=1,IM

       do k=1,LM
          qk(k)=qitfa(i,j,k) ! save qitfa to qk because qitfa will be changed later
       enddo

       qsum=0.
       ksum=0
       do k=kbdy-1,kbdy+1
          qijk=qk(k)
          if(ABS(qijk-SPVAL)>SMALL1) then
             qsum=qsum+qijk
             ksum=ksum+1
          endif
       enddo
       if(ksum>0) then
          qitfa(i,j,kbdy)=qsum/FLOAT(ksum)
       end if

!      Merge in points below kbdy
       qsum=0.
       ksum=0
       do k=kbdy,kbdy+2
          qijk=qk(k)
          if(ABS(qijk-SPVAL)>SMALL1) then
             qsum=qsum+qijk
             ksum=ksum+1
          endif
       enddo
       if(ksum>0) then
          qitfa(i,j,kbdy+1)=qsum/FLOAT(ksum)
       endif

!      Merge in points above kbdy
       qsum=0.
       ksum=0
       do k=kbdy-2,kbdy
          qijk=qk(k)
          if(ABS(qijk-SPVAL)>SMALL1) then
             qsum=qsum+qijk
             ksum=ksum+1
          endif
       enddo
       if(ksum>0) then
          qitfa(i,j,kbdy-1)=qsum/FLOAT(ksum)
       endif
    enddo
    enddo

    return
  end subroutine MergeRegions

!-----------------------------------------------------------------------
  subroutine remapq(iregion,idx,q,qs)
! Performs mapping of input variable q into output 
! variable qs (0-1).  Input index specific thresholds
! (null,light,moderate,severe) are in timap.  Corresponding
! (0-1) thresholds are in tis.
! If remap_option=1 use piecewise linear mapping
! If remap_option=2 use fit to log-normal PDF

    implicit none

    integer,intent(in) :: iregion,idx
    real,intent(in) :: q
    real,intent(out) :: qs

    real, parameter :: clampidxL=0.0,clampidxH=1.5

    real :: tii(NTI), A(2)

    qs=SPVAL
    if(ABS(q-SPVAL)<=SMALL1) return

    if(remap_option==2) then
!   --- Use fit of indices to log-normal PDF
!   --- Log(epsilon^(1/3)) = a + b Log(I)
       A(1)=timap(iregion,idx,1)
       A(2)=timap(iregion,idx,2)
       call remap2(A,q,qs)
    else
!   --- Default: use piecewise linear remap of entire grid
       tii(1:NTI)=timap(iregion,idx,1:NTI)
       call remap1(NTI,tii,tis,q,qs)
    endif

!   --- clamp the remapped index between clampL and clampH
    qs=MAX(qs,clampidxL)
    qs=MIN(qs,clampidxH)

    return
  end subroutine remapq

!-----------------------------------------------------------------------
  subroutine remap1(n,ti,tis,q,qs)
! Performs linear remap of index value contained in q into
! scaled value in the range (0-1)

    implicit none

    integer, intent(in) :: n
    real, intent(in) :: ti(n),tis(n)
    real, intent(in) :: q
    real, intent(out) :: qs
    real :: slope

    qs=SPVAL
    if(q<ti(2)) then
       slope=(tis(2)-tis(1))/(ti(2)-ti(1))
       qs=tis(1)+slope*(q-ti(1))
    elseif(q<ti(3)) then
       slope=(tis(3)-tis(2))/(ti(3)-ti(2))
       qs=tis(2)+slope*(q-ti(2))
    elseif(q<ti(4)) then
       slope=(tis(4)-tis(3))/(ti(4)-ti(3))
       qs=tis(3)+slope*(q-ti(3))
    else
       slope=(tis(5)-tis(4))/(ti(5)-ti(4))
       qs=tis(4)+slope*(q-ti(4))
    endif
    return
  end subroutine remap1

!-----------------------------------------------------------------------
  subroutine remap2(A,q,qs)
! Fit q to log-normal PDF, output is epsilon^1/3=qs
! Log(epsilon^(1/3)) = a + b Log(I), where a=A(1), b=A(2)

    implicit none

    real,intent(in) :: q
    real,intent(in) :: A(2)
    real,intent(out) :: qs

    real :: ai,bi,qq,logqs

!   --- use fit of indices to log-normal PDF
!   --- Log(epsilon^(1/3)) = a + b Log(I)

    qs=0.
    if(q<1.0E-20) return
    ai=A(1)
    bi=A(2)
    qq=MAX(q,1.0E-20)  ! protect against 0 or neg values
    logqs = ai + bi*ALOG(qq)
    qs = EXP(logqs)

    return
  end subroutine remap2

end module gtg_itfa
