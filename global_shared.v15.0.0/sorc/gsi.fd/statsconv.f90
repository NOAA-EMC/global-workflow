subroutine statsconv(mype,&
     i_ps,i_uv,i_srw,i_t,i_q,i_pw,i_rw,i_dw,i_gps,i_sst,i_tcp,i_lag, &
     i_gust,i_vis,i_pblh,i_wspd10m,i_td2m,i_mxtm,i_mitm,i_pmsl,i_howv, & 
     i_tcamt,i_lcbas,i_cldch,i_ref,bwork,awork,ndata)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    statconv    prints statistics for conventional data
!   prgmmr: derber           org: np23                date: 2003-05-22
!
! abstract: The routine computes and prints statistics regarding the
!           use of conventional observations.  Printed information 
!           includes that about data counts, quality control decisions,
!           statistics based on the innovations, and penalties - all 
!           as a observation type
!
! program history log:
!   2003-05-22  derber
!   2004-06-15  treadon - update documentation
!   2004-07-20  derber - add sst
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase diagnostic array sizes and add
!                         output from nonlinear qc
!   2004-12-23  treadon - use module jfunc to pass jiter,first
!   2005-01-28  cucurull - modify summary output for refractivity to include
!                          the QC checks on incremental refractivity
!   2005-03-23  cucurull - cosmetic changes for refractivity print out
!   2005-04-20  treadon - correct error in wind ntot sum
!   2005-05-27  derber - level output changed
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-12-02  cucurull - cosmetic changes for gps data
!   2006-02-03  derber  - modify for new obs control and to clean up output
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-02  derber  - modify to eliminate dvast and move ob type printing to dtast
!   2008-04-11  safford - rm unused uses
!   2009-02-02  kleist  - add synthetic tc-mslp
!   2009-03-05  meunier - add lagrangean data
!   2011-01-06  cucurull - replace gps_ref/gps_bnd with gps due to a change in the convinfo files gps structure
!                        - maintain dtype information in the output file, add ctype
!   2014-03-19  pondeca - add wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-06-06  carley/zhu - add tcamt and lcbas
!   2015-07-10  pondeca - add cldch
!
!   input argument list:
!     mype     - mpi task number
!     i_ps     - index in awork array holding surface pressure info
!     i_uv     - index in awork array holding wind info
!     i_srw    - index in awork array holding radar wind superobs info
!     i_t      - index in awork array holding temperature info
!     i_q      - index in awork array holding specific humidity info
!     i_pw     - index in awork array holding total precipitable water info
!     i_rw     - index in awork array holding radar radial winds info
!     i_dw     - index in awork array holding doppler lidar winds info
!     i_gps    - index in awork array holding gps info
!     i_sst    - index in awork array holding sst info
!     i_tcp    - index in awork array holding tcps info
!     i_lag    - index in awork array holding lag info
!     i_gust   - index in awork array holding gust info
!     i_vis    - index in awork array holding vis info
!     i_pblh   - index in awork array holding pblh info
!     i_wspd10m- index in awork array holding wspd10m info
!     i_td2m   - index in awork array holding td2m info
!     i_mxtm   - index in awork array holding mxtm info
!     i_mitm   - index in awork array holding mitm info
!     i_pmsl   - index in awork array holding pmsl info
!     i_howv   - index in awork array holding howv info
!     i_tcamt   - index in awork array holding tcamt info
!     i_lcbas   - index in awork array holding lcbas info
!     i_cldch   - index in awork array holding cldch info
!     i_ref    - size of second dimension of awork array
!     bwork    - array containing information for statistics
!     awork    - array containing information for data counts and gross checks
!     ndata(*,1)- number of profiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,three,five
  use obsmod, only: iout_sst,iout_pw,iout_t,iout_rw,iout_dw,&
       iout_srw,iout_uv,iout_gps,iout_ps,iout_q,iout_tcp,iout_lag,&
       iout_gust,iout_vis,iout_pblh,iout_wspd10m,iout_td2m,& 
       iout_mxtm,iout_mitm,iout_pmsl,iout_howv,iout_tcamt,iout_lcbas,iout_cldch,&
       mype_dw,mype_rw,mype_srw,mype_sst,mype_gps,mype_uv,mype_ps,&
       mype_t,mype_pw,mype_q,mype_tcp,ndat,dtype,mype_lag,mype_gust,&
       mype_vis,mype_pblh,mype_wspd10m,mype_td2m,mype_mxtm,mype_mitm,&
       mype_pmsl,mype_howv,mype_tcamt,mype_lcbas,mype_cldch
  use qcmod, only: npres_print,ptop,pbot,ptopq,pbotq
  use jfunc, only: first,jiter
  use gridmod, only: nsig
  use convinfo, only: nconvtype,ioctype
  implicit none

! Declare passed variables
  integer(i_kind)                                  ,intent(in   ) :: mype,i_ps,i_uv,&
       i_srw,i_t,i_q,i_pw,i_rw,i_dw,i_gps,i_sst,i_tcp,i_lag,i_gust,i_vis,i_pblh,&
       i_wspd10m,i_td2m,i_mxtm,i_mitm,i_pmsl,i_howv,i_tcamt,i_lcbas,i_cldch,i_ref
  real(r_kind),dimension(7*nsig+100,i_ref)     ,intent(in   ) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(in   ) :: bwork
  integer(i_kind),dimension(ndat,3)                ,intent(in   ) :: ndata

! Declare local variables
  character(100) mesage

  integer(i_kind) numgrspw,numsst,nsuperp,nump,nhitopo,ntoodif
  integer(i_kind) numgrsq,numhgh,numgust,numvis,numpblh,numwspd10m
  integer(i_kind) numtd2m,nummxtm,nummitm,numpmsl,numhowv,numtcamt,numlcbas,numcldch
  integer(i_kind) ntot,numlow,k,numssm,i,j
  integer(i_kind) numgross,numfailqc,numfailqc_ssmi,nread,nkeep
  integer(i_kind) numfail1_gps,numfail2_gps,numfail3_gps,nreadspd,nkeepspd
  integer(i_kind),dimension(nsig)::num

  real(r_kind) grsmlt,tq,pw,rat,tgps,qmplty,tpw,tdw,rwmplty,trw
  real(r_kind) tmplty,tt,dwmplty,gpsmplty,umplty,tssm,qctssm,tu,tv,tuv
  real(r_kind) vmplty,uvqcplty,rat1,rat2,rat3
  real(r_kind) dwqcplty,tqcplty,qctt,qctrw,rwqcplty,qctdw,qqcplty,qctgps
  real(r_kind) gpsqcplty,tpw3,pw3,qctq
  real(r_kind),dimension(1):: pbotall,ptopall
  
  logical,dimension(nconvtype):: pflag
  character(7):: ctype
!*********************************************************************************
! Initialize constants and variables.

  ptopall(1)=zero; pbotall(1)=2000.0_r_kind
  

! Generate summary statistics as a function of observation type.  
! Extensive comments are given for the winds.  Similar comments
! apply for the remaining observation types in this routine

  pflag=.false.
! Summary report for winds
  if(mype==mype_uv) then

!    Open output file so as to point to correct position in output file
     if(first)then
        open(iout_uv)
     else
        open(iout_uv,position='append')
     end if


!    Compute and write counts, penalties, and ratio of penalty
!    to data counts for each model level
     numssm=nint(awork(6,i_uv)); numgross=nint(awork(4,i_uv))
     umplty=zero; vmplty=zero; uvqcplty=zero ; ntot=0;
     tu=zero; tv=zero ; tuv=zero
     tssm=zero ; qctssm=zero
     nread=0
     nkeep=0
     nreadspd=0
     nkeepspd=0
     do i=1,ndat
        if(dtype(i)== 'uv')then
           nread=nread+ndata(i,2)  
           nkeep=nkeep+ndata(i,3)
        else if(dtype(i)== 'spd')then
           nreadspd=nreadspd+ndata(i,2)
           nkeepspd=nkeepspd+ndata(i,3)
        end if
     end do
     if(nkeep > 0 .or. nkeepspd > 0)then
!       Write header information  
        mesage='current vfit of wind data, ranges in m/s$'

!       Call routine to compute and write count, rms, and penalty information
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'spd' .or. trim(ioctype(j)) == 'uv'
        end do
        call dtast(bwork,npres_print,pbot,ptop,mesage,jiter,iout_uv,pflag)
        numlow      = nint(awork(2,i_uv))
        numhgh      = nint(awork(3,i_uv))
        write(iout_uv,900) 'wind',numhgh,numlow
        numfailqc=nint(awork(21,i_uv))
!       keep a seperate record of numfailqc for ssmi wind speeds
        numfailqc_ssmi=nint(awork(61,i_uv))
        do k=1,nsig
           num(k)=nint(awork(6*nsig+k+100,i_uv))
           rat1=zero
           rat2=zero
           if(num(k) > 0)then
              rat1=awork(4*nsig+k+100,i_uv)/float(num(k))
              rat2=awork(5*nsig+k+100,i_uv)/float(num(k))
           end if
           umplty=umplty+awork(4*nsig+k+100,i_uv)
           vmplty=vmplty+awork(5*nsig+k+100,i_uv)
           ntot=ntot+num(k)
           write(iout_uv,241) 'w',num(k),k,awork(4*nsig+k+100,i_uv),&
                           awork(5*nsig+k+100,i_uv),rat1,rat2
        end do
        do k=1,nsig
           num(k)=nint(awork(6*nsig+k+100,i_uv))
           rat1=zero
           rat3=zero
           if(num(k) > 0)then
              rat1=(awork(4*nsig+k+100,i_uv)+awork(5*nsig+k+100,i_uv))/float(num(k))
              rat3=awork(3*nsig+k+100,i_uv)/float(num(k))
           end if
           uvqcplty=uvqcplty+awork(3*nsig+k+100,i_uv)
           write(iout_uv,240) 'w',num(k),k,awork(4*nsig+k+100,i_uv)+awork(5*nsig+k+100,i_uv), &
                           awork(3*nsig+k+100,i_uv),rat1,rat3
        end do

!       Write statistics  gross checks
        write(iout_uv,920)' number ssm/i winds that fail nonlinear qc =',numfailqc_ssmi
        write(iout_uv,925) 'wind',numgross,numfailqc
!       Write statistics regarding penalties                   
        if(ntot > 0)then
           tu=umplty/float(ntot)
           tv=vmplty/float(ntot)
           tuv=uvqcplty/float(ntot)
        end if
        if(numssm > 0)then
           tssm=awork(5,i_uv)/awork(6,i_uv)
           qctssm=awork(22,i_uv)/awork(6,i_uv)
        end if
     end if
     write(iout_uv,949) 'u',ntot,umplty,tu
     write(iout_uv,949) 'v',ntot,vmplty,tv
     write(iout_uv,950) 'uv',jiter,nread,nkeep,ntot*2
     write(iout_uv,951) 'uv',umplty+vmplty,uvqcplty,tu+tv,tuv
     write(iout_uv,950) 'spd',jiter,nreadspd,nkeepspd,numssm
     write(iout_uv,951) 'spd',awork(5,i_uv),awork(22,i_uv),tssm,qctssm

!    Close unit receiving summary output     
     close(iout_uv)
  end if


! Summary report for radar wind superobs
  if(mype==mype_srw) then
     if(first)then
        open(iout_srw)
     else
        open(iout_srw,position='append')
     end if

     umplty=zero; vmplty=zero; uvqcplty=zero ; ntot=0;
     tu=zero; tv=zero ; tuv=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'srw')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of radar superob wind data, ranges in stderr$'
        do j=1,nconvtype
           pflag(j)= trim(ioctype(j)) == 'srw' 
        end do
        call dtast(bwork,npres_print,pbot,ptop,mesage,jiter,iout_srw,pflag)

        do k=1,nsig
           num(k)=nint(awork(6*nsig+k+100,i_srw))
           rat1=zero
           rat2=zero
           rat3=zero
           if(num(k) > 0)then
              rat1=awork(4*nsig+k+100,i_srw)/float(num(k))
              rat2=awork(5*nsig+k+100,i_srw)/float(num(k))
              rat3=awork(3*nsig+k+100,i_srw)/float(num(k))
           end if
           umplty=umplty+awork(4*nsig+k+100,i_srw)
           vmplty=vmplty+awork(5*nsig+k+100,i_srw)
           uvqcplty=uvqcplty+awork(3*nsig+k+100,i_srw)
           ntot=ntot+num(k)
           write(iout_srw,241) 's',num(k),k,awork(4*nsig+k+100,i_srw),&
                awork(5*nsig+k+100,i_srw),awork(3*nsig+k+100,i_srw),rat1,rat2,rat3
        end do
        numgross=nint(awork(4,i_srw))
        numfailqc=nint(awork(21,i_srw))
        write(iout_srw,925) 'srw',numgross,numfailqc
        if(ntot > 0) then
           tu=umplty/float(ntot)
           tv=vmplty/float(ntot)
           tuv=uvqcplty/float(ntot)
        endif
        numlow      = nint(awork(2,i_srw))
        numhgh      = nint(awork(3,i_srw))
        write(iout_srw,900) 'srw',numhgh,numlow
     end if

     write(iout_srw,950) 'srw1',jiter,nread,nkeep,ntot
     write(iout_srw,951) 'srw1',umplty,uvqcplty,tu,tuv
     write(iout_srw,950) 'srw2',jiter,nread,nkeep,ntot
     write(iout_srw,951) 'srw2',vmplty,uvqcplty,tv,tuv
     
     close(iout_srw)
  end if


! Summary report for gps 
  if (mype==mype_gps)then
     if(first)then
        open(iout_gps)
     else
        open(iout_gps,position='append')
     end if


     gpsmplty=zero; gpsqcplty=zero ; ntot=0
     tgps=zero ; qctgps=zero
     nread=0
     nkeep=0
     ctype=' '
     do i=1,ndat
        if(dtype(i)== 'gps_ref' .or. dtype(i) == 'gps_bnd')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
           ctype=dtype(i)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of gps data in fractional difference$'
        do j=1,nconvtype
            pflag(j)=trim(ioctype(j)) == 'gps'
        end do
        call dtast(bwork,npres_print,pbot,ptop,mesage,jiter,iout_gps,pflag)
        do k=1,nsig
           num(k)=nint(awork(5*nsig+k+100,i_gps))
           rat=zero
           rat3=zero
           if(num(k)>0) then
              rat=awork(6*nsig+k+100,i_gps)/float(num(k))
              rat3=awork(3*nsig+k+100,i_gps)/float(num(k))
           end if
           ntot=ntot+num(k); gpsmplty=gpsmplty+awork(6*nsig+k+100,i_gps)
           gpsqcplty=gpsqcplty+awork(3*nsig+k+100,i_gps)
           write(iout_gps,240)'gps',num(k),k,awork(6*nsig+k+100,i_gps), &
                             awork(3*nsig+k+100,i_gps),rat,rat3
        end do
        numgross=nint(awork(4,i_gps))
        numfailqc=nint(awork(21,i_gps))
        numfail1_gps=nint(awork(22,i_gps))
        numfail2_gps=nint(awork(23,i_gps))
        numfail3_gps=nint(awork(24,i_gps))
        write(iout_gps,925)'gps',numgross,numfailqc
        write(iout_gps,920)' number of gps obs failed stats qc in NH =',numfail1_gps
        write(iout_gps,920)' number of gps obs failed stats qc in SH =',numfail2_gps
        write(iout_gps,920)' number of gps obs failed stats qc in TR =',numfail3_gps

        numlow        = nint(awork(2,i_gps))
        numhgh        = nint(awork(3,i_gps))
        write(iout_gps,900) 'gps',numhgh,numlow
        if(ntot > 0) then
           tgps=gpsmplty/ntot
           qctgps=gpsqcplty/ntot
        endif
     end if

     write(iout_gps,950) ctype,jiter,nread,nkeep,ntot
     write(iout_gps,951) ctype,gpsmplty,gpsqcplty,tgps,qctgps

     close(iout_gps)
  endif


! Summary report for specific humidity
  if(mype==mype_q) then
     if(first)then
        open(iout_q)
     else
        open(iout_q,position='append')
     end if

     mesage='current fit of q data, units in per-cent of guess q-sat$'
     do j=1,nconvtype
        pflag(j)=trim(ioctype(j)) == 'q'  
     end do
     call dtast(bwork,npres_print,pbotq,ptopq,mesage,jiter,iout_q,pflag)

     qmplty=zero; qqcplty=zero ; ntot=0
     tq=zero ; qctq=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'q')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        do k=1,nsig
           num(k)=nint(awork(k+6*nsig+100,i_q))
           rat=zero
           rat3=zero
           if(num(k) > 0)then
              rat=awork(5*nsig+k+100,i_q)/float(num(k))
              rat3=awork(3*nsig+k+100,i_q)/float(num(k))
           end if
           qmplty=qmplty+awork(5*nsig+k+100,i_q)
           qqcplty=qqcplty+awork(3*nsig+k+100,i_q)
           ntot=ntot+num(k)
           write(iout_q,240) 'q',num(k),k,awork(5*nsig+k+100,i_q), &
                                  awork(3*nsig+k+100,i_q),rat,rat3
        end do
        grsmlt=five
        numgrsq=nint(awork(4,i_q))
        numfailqc=nint(awork(21,i_q))
        write(iout_q,924)'  (scaled as precent of guess specific humidity)'
        write(iout_q,925) 'q',numgrsq,numfailqc
        write(iout_q,975) grsmlt,'q',awork(5,i_q)
        numlow      = nint(awork(2,i_q))
        numhgh      = nint(awork(3,i_q))
        write(iout_q,900) 'q',numhgh,numlow
        if(ntot > 0) then
           tq=qmplty/float(ntot)
           qctq=qqcplty/float(ntot)
        end if
     end if

     write(iout_q,950) 'q',jiter,nread,nkeep,ntot
     write(iout_q,951) 'q',qmplty,qqcplty,tq,qctq

     close(iout_q)
  end if


! Summary report for surface pressure
  if(mype==mype_ps) then
     if(first)then
        open(iout_ps)
     else
        open(iout_ps,position='append')
     end if

     nump=nint(awork(5,i_ps))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'ps')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of surface pressure data, ranges in mb$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'ps'  
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_ps,pflag)
 
        numgross=nint(awork(6,i_ps))
        numfailqc=nint(awork(21,i_ps))
        write(iout_ps,925) 'psfc',numgross,numfailqc
        if(nump > 0)then
           pw=awork(4,i_ps)/float(nump)
           pw3=awork(22,i_ps)/float(nump)
        end if
     end if

     write(iout_ps,950) 'psfc',jiter,nread,nkeep,nump
     write(iout_ps,951) 'psfc',awork(4,i_ps),awork(22,i_ps),pw,pw3

     close(iout_ps)
  end if


! Summary report for total precipitable water
  if(mype==mype_pw) then
     if(first)then
        open(iout_pw)
     else
        open(iout_pw,position='append')
     end if

     nsuperp=nint(awork(4,i_pw))

     tpw=zero ; tpw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'pw')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of precip. water data, ranges in mm$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'pw'  
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_pw,pflag)

        numgrspw=nint(awork(6,i_pw))
        numfailqc=nint(awork(21,i_pw))
        grsmlt=three
        tpw=zero
        tpw3=zero
        if(nsuperp > 0)then
           tpw=awork(5,i_pw)/nsuperp
           tpw3=awork(22,i_pw)/nsuperp
        end if
        write(iout_pw,925) 'p.w.',numgrspw,numfailqc
        write(iout_pw,975) grsmlt,'p.w.',awork(7,i_pw)
     end if
     write(iout_pw,950) 'pw',jiter,nread,nkeep,nsuperp
     write(iout_pw,951) 'pw',awork(5,i_pw),awork(22,i_pw),tpw,tpw3

     close(iout_pw)
  end if

! Summary report for conventional sst
  if(mype==mype_sst) then
     if(first)then
        open(iout_sst)
     else
        open(iout_sst,position='append')
     end if

     numsst=nint(awork(5,i_sst))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'sst')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional sst data, ranges in  C$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'sst'  
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_sst,pflag)

        numgross=nint(awork(6,i_sst))
        numfailqc=nint(awork(21,i_sst))
        if(numsst > 0)then
           pw=awork(4,i_sst)/numsst
           pw3=awork(22,i_sst)/numsst
        end if
        write(iout_sst,925) 'sst',numgross,numfailqc
     end if
     write(iout_sst,950) 'sst',jiter,nread,nkeep,numsst
     write(iout_sst,951) 'sst',awork(4,i_sst),awork(22,i_sst),pw,pw3

     close(iout_sst)
  end if

! Summary report for conventional gust
  if(mype==mype_gust) then
     if(first)then
        open(iout_gust)
     else
        open(iout_gust,position='append')
     end if

     numgust=nint(awork(5,i_gust))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'gust')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional gust data, ranges in  m/s$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'gust'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_gust,pflag)

        numgross=nint(awork(6,i_gust))
        numfailqc=nint(awork(21,i_gust))
        if(numgust > 0)then
           pw=awork(4,i_gust)/numgust
           pw3=awork(22,i_gust)/numgust
        end if
        write(iout_gust,925) 'gust',numgross,numfailqc
     end if
     write(iout_gust,950) 'gust',jiter,nread,nkeep,numgust
     write(iout_gust,951) 'gust',awork(4,i_gust),awork(22,i_gust),pw,pw3

     close(iout_gust)
  end if

! Summary report for conventional vis
  if(mype==mype_vis) then
     if(first)then
        open(iout_vis)
     else
        open(iout_vis,position='append')
     end if

     numvis=nint(awork(5,i_vis))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'vis')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional vis data, ranges in  m$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'vis'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_vis,pflag)

        numgross=nint(awork(6,i_vis))
        numfailqc=nint(awork(21,i_vis))
        if(numvis > 0)then
           pw=awork(4,i_vis)/numvis
           pw3=awork(22,i_vis)/numvis
        end if
        write(iout_vis,925) 'vis',numgross,numfailqc
     end if
     write(iout_vis,950) 'vis',jiter,nread,nkeep,numvis
     write(iout_vis,951) 'vis',awork(4,i_vis),awork(22,i_vis),pw,pw3

     close(iout_vis)
  end if

! Summary report for conventional pblh
  if(mype==mype_pblh) then
     if(first)then
        open(iout_pblh)
     else
        open(iout_pblh,position='append')
     end if

     numpblh=nint(awork(5,i_pblh))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'pblh')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional pblh data, ranges in  m$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'pblh'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_pblh,pflag)

        numgross=nint(awork(6,i_pblh))
        numfailqc=nint(awork(21,i_pblh))
        if(numpblh > 0)then
           pw=awork(4,i_pblh)/numpblh
           pw3=awork(22,i_pblh)/numpblh
        end if
        write(iout_pblh,925) 'pblh',numgross,numfailqc
     end if
     write(iout_pblh,950) 'pblh',jiter,nread,nkeep,numpblh
     write(iout_pblh,951) 'pblh',awork(4,i_pblh),awork(22,i_pblh),pw,pw3

     close(iout_pblh)
  end if

! Summary report for conventional wspd10m
  if(mype==mype_wspd10m) then
     if(first)then
        open(iout_wspd10m)
     else
        open(iout_wspd10m,position='append')
     end if

     numwspd10m=nint(awork(5,i_wspd10m))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'wspd10m')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional wspd10m data, ranges in  m/s$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'wspd10m'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_wspd10m,pflag)

        numgross=nint(awork(6,i_wspd10m))
        numfailqc=nint(awork(21,i_wspd10m))
        if(numwspd10m > 0)then
           pw=awork(4,i_wspd10m)/numwspd10m
           pw3=awork(22,i_wspd10m)/numwspd10m
        end if
        write(iout_wspd10m,925) 'wspd10m',numgross,numfailqc
     end if
     write(iout_wspd10m,950) 'wspd10m',jiter,nread,nkeep,numwspd10m
     write(iout_wspd10m,951) 'wspd10m',awork(4,i_wspd10m),awork(22,i_wspd10m),pw,pw3

     close(iout_wspd10m)
  end if

! Summary report for conventional td2m
  if(mype==mype_td2m) then
     if(first)then
        open(iout_td2m)
     else
        open(iout_td2m,position='append')
     end if

     numtd2m=nint(awork(5,i_td2m))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'td2m')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional td2m data, ranges in K $'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'td2m'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_td2m,pflag)

        numgross=nint(awork(6,i_td2m))
        numfailqc=nint(awork(21,i_td2m))
        if(numtd2m > 0)then
           pw=awork(4,i_td2m)/numtd2m
           pw3=awork(22,i_td2m)/numtd2m
        end if
        write(iout_td2m,925) 'td2m',numgross,numfailqc
     end if
     write(iout_td2m,950) 'td2m',jiter,nread,nkeep,numtd2m
     write(iout_td2m,951) 'td2m',awork(4,i_td2m),awork(22,i_td2m),pw,pw3

     close(iout_td2m)
  end if

! Summary report for conventional mxtm
  if(mype==mype_mxtm) then
     if(first)then
        open(iout_mxtm)
     else
        open(iout_mxtm,position='append')
     end if

     nummxtm=nint(awork(5,i_mxtm))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'mxtm')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional mxtm data, ranges in K $'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'mxtm'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_mxtm,pflag)

        numgross=nint(awork(6,i_mxtm))
        numfailqc=nint(awork(21,i_mxtm))
        if(nummxtm > 0)then
           pw=awork(4,i_mxtm)/nummxtm
           pw3=awork(22,i_mxtm)/nummxtm
        end if
        write(iout_mxtm,925) 'mxtm',numgross,numfailqc
     end if
     write(iout_mxtm,950) 'mxtm',jiter,nread,nkeep,nummxtm
     write(iout_mxtm,951) 'mxtm',awork(4,i_mxtm),awork(22,i_mxtm),pw,pw3

     close(iout_mxtm)
  end if

! Summary report for conventional mitm
  if(mype==mype_mitm) then
     if(first)then
        open(iout_mitm)
     else
        open(iout_mitm,position='append')
     end if

     nummitm=nint(awork(5,i_mitm))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'mitm')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional mitm data, ranges in K $'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'mitm'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_mitm,pflag)

        numgross=nint(awork(6,i_mitm))
        numfailqc=nint(awork(21,i_mitm))
        if(nummitm > 0)then
           pw=awork(4,i_mitm)/nummitm
           pw3=awork(22,i_mitm)/nummitm
        end if
        write(iout_mitm,925) 'mitm',numgross,numfailqc
     end if
     write(iout_mitm,950) 'mitm',jiter,nread,nkeep,nummitm
     write(iout_mitm,951) 'mitm',awork(4,i_mitm),awork(22,i_mitm),pw,pw3

     close(iout_mitm)
  end if

! Summary report for conventional pmsl
  if(mype==mype_pmsl) then
     if(first)then
        open(iout_pmsl)
     else
        open(iout_pmsl,position='append')
     end if

     numpmsl=nint(awork(5,i_pmsl))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'pmsl')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional pmsl data, ranges in hPa $'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'pmsl'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_pmsl,pflag)

        numgross=nint(awork(6,i_pmsl))
        numfailqc=nint(awork(21,i_pmsl))
        if(numpmsl > 0)then
           pw=awork(4,i_pmsl)/numpmsl
           pw3=awork(22,i_pmsl)/numpmsl
        end if
        write(iout_pmsl,925) 'pmsl',numgross,numfailqc
     end if
     write(iout_pmsl,950) 'pmsl',jiter,nread,nkeep,numpmsl
     write(iout_pmsl,951) 'pmsl',awork(4,i_pmsl),awork(22,i_pmsl),pw,pw3

     close(iout_pmsl)
  end if

! Summary report for conventional howv
  if(mype==mype_howv) then
     if(first)then
        open(iout_howv)
     else
        open(iout_howv,position='append')
     end if

     numhowv=nint(awork(5,i_howv))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'howv')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional howv data, ranges in m $'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'howv'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_howv,pflag)

        numgross=nint(awork(6,i_howv))
        numfailqc=nint(awork(21,i_howv))
        if(numhowv > 0)then
           pw=awork(4,i_howv)/numhowv
           pw3=awork(22,i_howv)/numhowv
        end if
        write(iout_howv,925) 'howv',numgross,numfailqc
     end if
     write(iout_howv,950) 'howv',jiter,nread,nkeep,numhowv
     write(iout_howv,951) 'howv',awork(4,i_howv),awork(22,i_howv),pw,pw3

     close(iout_howv)
  end if

! Summary report for tcamt
  if(mype==mype_tcamt) then
     if(first)then
        open(iout_tcamt)
     else
        open(iout_tcamt,position='append')
     end if

     numtcamt=nint(awork(5,i_tcamt))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'tcamt')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional tcamt data, ranges in  %$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'tcamt'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_tcamt,pflag)

        numgross=nint(awork(6,i_tcamt))
        numfailqc=nint(awork(21,i_tcamt))
        if(numtcamt > 0)then
           pw=awork(4,i_tcamt)/numtcamt
           pw3=awork(22,i_tcamt)/numtcamt
        end if
        write(iout_tcamt,925) 'tcamt',numgross,numfailqc
     end if
     write(iout_tcamt,950) 'tcamt',jiter,nread,nkeep,numtcamt
     write(iout_tcamt,951) 'tcamt',awork(4,i_tcamt),awork(22,i_tcamt),pw,pw3

     close(iout_tcamt)
  end if

! Summary report for lcbas
  if(mype==mype_lcbas) then
     if(first)then
        open(iout_lcbas)
     else
        open(iout_lcbas,position='append')
     end if

     numlcbas=nint(awork(5,i_lcbas))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'lcbas')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional lcbas data, ranges in  m$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'lcbas'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_lcbas,pflag)

        numgross=nint(awork(6,i_lcbas))
        numfailqc=nint(awork(21,i_lcbas))
        if(numlcbas > 0)then
           pw=awork(4,i_lcbas)/numlcbas
           pw3=awork(22,i_lcbas)/numlcbas
        end if
        write(iout_lcbas,925) 'lcbas',numgross,numfailqc
     end if
     write(iout_lcbas,950) 'lcbas',jiter,nread,nkeep,numlcbas
     write(iout_lcbas,951) 'lcbas',awork(4,i_lcbas),awork(22,i_lcbas),pw,pw3

     close(iout_lcbas)
  end if

! Summary report for conventional cldch
  if(mype==mype_cldch) then
     if(first)then
        open(iout_cldch)
     else
        open(iout_cldch,position='append')
     end if

     numcldch=nint(awork(5,i_cldch))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'cldch')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of conventional cldch data, ranges in  m$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'cldch'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_cldch,pflag)

        numgross=nint(awork(6,i_cldch))
        numfailqc=nint(awork(21,i_cldch))
        if(numcldch > 0)then
           pw=awork(4,i_cldch)/numcldch
           pw3=awork(22,i_cldch)/numcldch
        end if
        write(iout_cldch,925) 'cldch',numgross,numfailqc
     end if
     write(iout_cldch,950) 'cldch',jiter,nread,nkeep,numcldch
     write(iout_cldch,951) 'cldch',awork(4,i_cldch),awork(22,i_cldch),pw,pw3

     close(iout_cldch)
  end if

! Summary report for temperature  
  if (mype==mype_t)then
     if(first)then
        open(iout_t)
     else
        open(iout_t,position='append')
     end if

     tmplty=zero; tqcplty=zero ; ntot=0
     tt=zero ; qctt=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 't')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of temperature data, ranges in K $'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 't'  
        end do
        call dtast(bwork,npres_print,pbot,ptop,mesage,jiter,iout_t,pflag)
        do k=1,nsig
           num(k)=nint(awork(5*nsig+k+100,i_t))
           rat=zero ; rat3=zero
           if(num(k) > 0) then
              rat=awork(6*nsig+k+100,i_t)/float(num(k))
              rat3=awork(3*nsig+k+100,i_t)/float(num(k))
           end if
           ntot=ntot+num(k); tmplty=tmplty+awork(6*nsig+k+100,i_t)
           tqcplty=tqcplty+awork(3*nsig+k+100,i_t)
           write(iout_t,240) 't',num(k),k,awork(6*nsig+k+100,i_t), &
                                          awork(3*nsig+k+100,i_t),rat,rat3
        end do
        numgross=nint(awork(4,i_t))
        numfailqc=nint(awork(21,i_t))
        write(iout_t,925) 'temp',numgross,numfailqc
        numlow      = nint(awork(2,i_t))
        numhgh      = nint(awork(3,i_t))
        write(iout_t,900) 't',numhgh,numlow
        if(ntot > 0) then
           tt=tmplty/ntot
           qctt=tqcplty/ntot
        end if
     end if

     write(iout_t,950) 't',jiter,nread,nkeep,ntot
     write(iout_t,951) 't',tmplty,tqcplty,tt,qctt
     

     close(iout_t)
  endif


! Summary report for doppler lidar winds
  if(mype==mype_dw) then
     if(first)then
        open(iout_dw)
     else
        open(iout_dw,position='append')
     end if

     dwmplty=zero; dwqcplty=zero ; ntot=0
     tdw=zero ; qctdw=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'dw')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current vfit of lidar wind data, ranges in m/s$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'dw' 
        end do
        call dtast(bwork,npres_print,pbot,ptop,mesage,jiter,iout_dw,pflag)
 
        do k=1,nsig
           num(k)=nint(awork(k+5*nsig+100,i_dw))
           rat=zero
           rat3=zero
           if(num(k) > 0) then
              rat=awork(6*nsig+k+100,i_dw)/float(num(k))
              rat3=awork(3*nsig+k+100,i_dw)/float(num(k))
           end if
           ntot=ntot+num(k)
           dwmplty=dwmplty+awork(6*nsig+k+100,i_dw)
           dwqcplty=dwqcplty+awork(3*nsig+k+100,i_dw)
           write(iout_dw,240) 'r',num(k),k,awork(6*nsig+k+100,i_dw), &
                                           awork(3*nsig+k+100,i_dw),rat,rat3
        end do
        numgross=nint(awork(4,i_dw))
        numfailqc=nint(awork(21,i_dw))
        if(ntot > 0) then
           tdw=dwmplty/float(ntot)
           qctdw=dwqcplty/float(ntot)
        end if
        write(iout_dw,925) 'dw',numgross,numfailqc
        numlow       = nint(awork(2,i_dw))
        numhgh       = nint(awork(3,i_dw))
        write(iout_dw,900) 'dw',numhgh,numlow
     end if

     write(iout_dw,950) 'dw',jiter,nread,nkeep,ntot
     write(iout_dw,951) 'dw',dwmplty,dwqcplty,tdw,qctdw
     
     close(iout_dw)
  end if


! Summary report for radar radial winds
  if(mype==mype_rw) then
     if(first)then
        open(iout_rw)
     else
        open(iout_rw,position='append')
     end if
     
     rwmplty=zero; rwqcplty=zero ; ntot=0
     trw=zero ; qctrw=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'rw')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current vfit of radar wind data, ranges in m/s$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'rw' 
        end do
        call dtast(bwork,npres_print,pbot,ptop,mesage,jiter,iout_rw,pflag)
     
        numgross=nint(awork(4,i_rw))
        numfailqc=nint(awork(21,i_rw))
        do k=1,nsig
           num(k)=nint(awork(k+5*nsig+100,i_rw))
           rat=zero
           rat3=zero
           if(num(k) > 0) then
              rat=awork(6*nsig+k+100,i_rw)/float(num(k))
              rat3=awork(3*nsig+k+100,i_rw)/float(num(k))
           end if
           ntot=ntot+num(k)
           rwmplty=rwmplty+awork(6*nsig+k+100,i_rw)
           rwqcplty=rwqcplty+awork(3*nsig+k+100,i_rw)
           write(iout_rw,240) 'r',num(k),k,awork(6*nsig+k+100,i_rw), &
                                           awork(3*nsig+k+100,i_rw),rat,rat3
        end do
        if(ntot > 0) then
           trw=rwmplty/float(ntot)
           qctrw=rwqcplty/float(ntot)
        end if
        write(iout_rw,925) 'rw',numgross,numfailqc
        numlow       = nint(awork(2,i_rw))
        numhgh       = nint(awork(3,i_rw))
        nhitopo      = nint(awork(5,i_rw))
        ntoodif      = nint(awork(6,i_rw))
        write(iout_rw,900) 'rw',numhgh,numlow
        write(iout_rw,905) 'rw',nhitopo,ntoodif
     end if
     write(iout_rw,950) 'rw',jiter,nread,nkeep,ntot
     write(iout_rw,951) 'rw',rwmplty,rwqcplty,trw,qctrw
     
     close(iout_rw)
  end if

  if(mype==mype_tcp) then
     if(first)then
        open(iout_tcp)
     else
        open(iout_tcp,position='append')
     end if

     nump=nint(awork(5,i_tcp))
     pw=zero ; pw3=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'tcp')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of surface pressure data, ranges in mb$'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'tcp'
        end do
        call dtast(bwork,1,pbotall,ptopall,mesage,jiter,iout_tcp,pflag)

        numgross=nint(awork(6,i_tcp))
        numfailqc=nint(awork(21,i_tcp))
        write(iout_tcp,925) 'psfc',numgross,numfailqc

        if(nump > 0)then
           pw=awork(4,i_tcp)/float(nump)
           pw3=awork(22,i_tcp)/float(nump)
        end if
     end if

     write(iout_tcp,950) 'psfc',jiter,nread,nkeep,nump
     write(iout_tcp,951) 'psfc',awork(4,i_tcp),awork(22,i_tcp),pw,pw3

     close(iout_tcp)
  end if

 ! Summary report for lagrangian
  if (mype==mype_lag)then
     if(first)then
        open(iout_lag)
     else
        open(iout_lag,position='append')
     end if

     tmplty=zero; tqcplty=zero ; ntot=0
     tt=zero ; qctt=zero
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'lag')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nkeep > 0)then
        mesage='current fit of lagangian data, ranges in m $'
        do j=1,nconvtype
           pflag(j)=trim(ioctype(j)) == 'lag'
        end do
        call dtast(bwork,npres_print,pbot,ptop,mesage,jiter,iout_lag,pflag)
        do k=1,nsig
           num(k)=nint(awork(6*nsig+k+100,i_lag))
           rat=zero ; rat3=zero
           if(num(k) > 0) then
              rat=awork(4*nsig+k+100,i_lag)/float(num(k))
              rat3=awork(3*nsig+k+100,i_lag)/float(num(k))
           end if
           ntot=ntot+num(k); tmplty=tmplty+awork(4*nsig+k+100,i_lag)
           tqcplty=tqcplty+awork(3*nsig+k+100,i_lag)
           write(iout_lag,240) 'lag',num(k),k,awork(4*nsig+k+100,i_lag), &
                                          awork(3*nsig+k+100,i_lag),rat,rat3
        end do
        numgross=nint(awork(4,i_lag))
        numfailqc=nint(awork(21,i_lag))
        write(iout_lag,925) 'lag',numgross,numfailqc
       ! numlow      = nint(awork(2,i_t))
       ! numhgh      = nint(awork(3,i_t))
       ! write(iout_lag,900) 't',numhgh,numlow
        if(ntot > 0) then
           tt=tmplty/ntot
           qctt=tqcplty/ntot
        end if
     end if

     write(iout_lag,950) 'lag',jiter,nread,nkeep,ntot
     write(iout_lag,951) 'lag',tmplty,tqcplty,tt,qctt

     close(iout_lag)
  endif

 


! Format statements used above
111 format('obs lev   num     rms         bias        sumges       sumobs        cpen')
240 format(' num(',A1,') = ',i6,' at lev ',i4,' pen,qcpen,cpen,cqcpen = ',6(g12.5,1x))
241 format(' num(',A1,') = ',i6,' at lev ',i4,' upen,vpen,cupen,cvpen = ',6(g12.5,1x))
900 format(' number of ',a5,' obs extrapolated above',&
         ' top sigma layer=',i8,/,10x,' number extrapolated below',&
         ' bottom sigma layer=',i8)
905 format(' number of ',a5,' obs with station elevation > 2km = ',i8,/, &
         ' number with abs(guess topography-station elevation) > 200m = ',i8)
920 format(a44,i7)
924 format(a50)
925 format(' number of ',a5,' obs that failed gross test = ',I5,' nonlin qc test = ',I5)
949 format(' number of ',a5,' obs = ',i6,' pen= ',e25.18,' cpen= ',g13.6)
950 format(' type ',a7,' jiter ',i3,' nread ',i9,' nkeep ',i7,' num ',i7)
951 format(' type ',a7,' pen= ',e25.18,' qcpen= ',e25.18,' r= ',g13.6,' qcr= ',g13.6)
952 format(t5,'it',t13,'sat',t21,'# read',t32,'# keep',t42,'# assim',&
         t52,'penalty',t67,'cpen')
975 format(' grsmlt=',f7.1,' number of bad ',a5,' obs=',f8.0)
  
  return
end subroutine statsconv
