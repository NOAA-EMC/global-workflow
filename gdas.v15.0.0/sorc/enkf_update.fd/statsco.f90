subroutine statsco(stats_co,bwork,awork,ndata)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    statsco     prints statistics for carbon monoxide 
!   prgmmr: derber           org: np23                date: 2003-05-22
!
! abstract: The routine computes and prints statistics regarding the
!           use of carbon monoxide observations.  Printed information includes
!           that about data counts, quality control decisions,
!           statistics based on the innovations, and penalties - all
!           as a function of observation and satellite type
!
! program history log:
!   2003-05-22  derber
!   2004-06-15  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-12-23  treadon - use module jfunc to pass jiter,first
!   2005-11-29  derber - move ozmz array from obsmod to guess_grids
!   2006-02-03  derber  - modify for new obs control and clean up output
!   2007-02-21  sienkiewicz - bring in stats for ozone level data
!   2007-05-30  h.liu   - remove ozmz
!   2010-04023  tangborn - create statsco
!
!   input argument list:
!     stats_co - array holding sums from various statistical output
!     ndata(*,1)- number of observation keep for processing
!     ndata(*,2)- number of observation read
!     ndata(*,3)- number of observations keep after read
!     bwork    - array containing information for statistics (level o3)
!     awork    - array containing information for data counts and gross checks (level o3)
!
!   output argument list:
!     stats_co - array holding sums from various statistical output
!     bwork    - array containing information for statistics (level o3)
!     awork    - array containing information for data counts and gross checks (level o3)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  use obsmod, only: ndat,iout_co,dtype,dsis,dplat,ditype
  use coinfo, only: error_co,nusis_co,nulev,iuse_co,jpch_co
  use jfunc, only: jiter
  use qcmod, only: npres_print,pboto3, ptopo3
  use convinfo, only: nconvtype,ioctype
  use gridmod, only: nsig
  implicit none

! Declare passed variables
  integer(i_kind),dimension(ndat,3)                ,intent(in   ) :: ndata
  real(r_kind),dimension(9,jpch_co)                ,intent(inout) :: stats_co
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  real(r_kind),dimension(7*nsig+100)        ,intent(inout) :: awork


! Declare local variables
  logical,dimension(ndat):: idisplay
  integer(i_kind) j,iasim,i,icerr,ii
  real(r_kind) svar,rsum,stdev,cpen,penalty_all,qcpenalty_all
  real(r_kind),dimension(ndat):: rpenal,qcpenal
  integer(i_kind),dimension(ndat):: icount_asim,iqccount_asim

  real(r_kind) o3plty, o3qcplty, o3t, qco3t, rat, rat3

  integer(i_kind) ntot, nread, nkeep, numgross, numfailqc, k, numlow, numhgh
  integer(i_kind),dimension(nsig)::num

  logical,dimension(nconvtype):: pflag

  character(100) mesage

!******************************************************************************
! Compute and print statistics for carbon monoxide data
  open(iout_co,position='append')
  write(iout_co,850) jiter
850 format('OUTER ITERATION jiter=',i5)


! Print total penalty over all satellites
  penalty_all=zero
  qcpenalty_all=zero
  do i=1,jpch_co
     iasim = nint(stats_co(7,i))
     if (iasim>0) then
        penalty_all=penalty_all+stats_co(5,i)
        qcpenalty_all=qcpenalty_all+stats_co(8,i)
     endif
  end do
  write(iout_co,*)'carbon monoxide total   penalty_all=',penalty_all
  write(iout_co,*)'carbon monoxide total qcpenalty_all=',qcpenalty_all

! Print counts, bias, rms, stndev as a function of level
  icount_asim=0
  iqccount_asim=0
  rpenal=zero
  qcpenal=zero
  idisplay=.false.
  do ii=1,ndat
     if(ditype(ii) == 'co')then
        do i = 1,jpch_co
           iasim = nint(stats_co(1,i))
           if (iasim > 0 .and. nusis_co(i) == dsis(ii) ) then
              if (iuse_co(i)==1) then
                 icount_asim(ii) = icount_asim(ii) + iasim
                 rpenal(ii) = rpenal(ii) + stats_co(5,i)
                 iqccount_asim(ii) = iqccount_asim(ii) + nint(stats_co(9,i))
                 qcpenal(ii) = qcpenal(ii) + stats_co(8,i)
              end if
              idisplay(ii)=.true.
           end if
        end do
     end if
  end do
  do i = 1,jpch_co
     iasim = nint(stats_co(1,i))
     if (iasim > 0) then
        svar = error_co(i)
        if (iuse_co(i)/=1) svar = -svar
        rsum = one/float(iasim)
        icerr = nint(stats_co(2,i))
        do j=3,6   ! j=3=obs-mod(w_biascor)
                   ! j=4=(obs-mod(w_biascor))**2
                   ! j=5=penalty contribution
                   ! j=6=obs

           stats_co(j,i) = stats_co(j,i)*rsum
        end do
        stats_co(4,i) = sqrt(stats_co(4,i))
        if (iasim > 1) then
           stdev  = sqrt(stats_co(4,i)*stats_co(4,i)-stats_co(3,i)*stats_co(3,i))
        else
           stdev = zero
        end if
        write(iout_co,1102) i,nulev(i),nusis_co(i),iasim,icerr,svar,&
             stats_co(6,i),stats_co(6,i)-stats_co(3,i),stats_co(3,i),&
             stats_co(5,i),stats_co(4,i),stdev
     endif
  end do

! Write obs count to runtime output file
  write(iout_co,1109)
  do i=1,ndat
     if (idisplay(i)) then
        cpen=zero
        if (icount_asim(i)>0) cpen=rpenal(i)/float(icount_asim(i))
        write(iout_co,1115) jiter,dplat(i),dtype(i),ndata(i,2), &
             ndata(i,3),icount_asim(i),rpenal(i),cpen,qcpenal(i),iqccount_asim(i)
     endif
  end do
2000 format(a7,2x,A4,6x,8(a7,1x))
2010 format(i7,1x,A10,1x,8(i7,1x))
2011 format(3x,f16.8,7(i7,1x))
2012 format(7x,A7,5x,7(a7,1x))
2999 format(' Illegal satellite type ')
1102 format(1x,i4,i4,1x,a20,2i7,1x,f8.3,1x,6(f11.7,1x))
1109 format(t5,'it',t11,'sat',t22,'inst',t36,'# read',t46,'# keep',t55,'# assim',&
          t63,'penalty',t78,'cpen',t88,'qcpen',t101,'qcfail')
1115 format('o-g',1x,i2.2,1x,'co ',a10,1x,a10,1x,3(i9,1x),3(g12.5,1x),i8)

! End of carbon monoxide diagnostic print block.

! carbon monoxide level data diagnostics

  o3plty=zero; o3qcplty=zero ; ntot=0
  o3t=zero ; qco3t = zero;
  nread = 0
  nkeep = 0
  do i=1,ndat
     if (dtype(i)== 'o3lev') then
        nread=nread+ndata(i,2)
        nkeep=nkeep+ndata(i,3)
     end if
  end do
  if (nkeep > 0) then
     mesage='current fit of carbon monoxide level data, ranges in ppmv $'
     do j = 1,nconvtype
        pflag(j)=trim(ioctype(j)) == 'o3lev'
     end do
     call dtast(bwork,npres_print,pboto3,ptopo3,mesage,jiter,iout_co,pflag)
     do k=1,nsig
        num(k)=nint(awork(5*nsig+k+100))
        rat=zero ; rat3=zero
        if(num(k) > 0) then
           rat=awork(6*nsig+k+100)/float(num(k))
           rat3=awork(3*nsig+k+100)/float(num(k))
        end if
        ntot=ntot+num(k); o3plty=o3plty+awork(6*nsig+k+100)
        o3qcplty=o3qcplty+awork(3*nsig+k+100)
        write(iout_co,240) 'o3l',num(k),k,awork(6*nsig+k+100), &
             awork(3*nsig+k+100),rat,rat3
     end do
     numgross=nint(awork(4))
     numfailqc=nint(awork(21))
     write(iout_co,925) 'o3l',numgross,numfailqc
     numlow = nint(awork(2))
     numhgh = nint(awork(3))
     write(iout_co,900) 'o3l',numhgh,numlow
     o3t=o3plty/ntot
     qco3t=o3qcplty/ntot
  end if

  write(iout_co,950) 'o3l',jiter,nread,nkeep,ntot
  write(iout_co,951) 'o3l',o3plty,o3qcplty,o3t,qco3t

  close(iout_co)

240 format(' num(',A,') = ',i6,' at lev ',i4,' pen,qcpen,cpen,cqcpen = ',6(g12.5,1x))
925 format(' number of ',a5,' obs that failed gross test = ',I5,' nonlin qc test = ',I5)
900 format(' number of ',a5,' obs extrapolated above',&
         ' top sigma layer=',i8,/,10x,' number extrapolated below',&
         ' bottom sigma layer=',i8)
950 format(' type ',a5,' jiter ',i3,' nread ',i7,' nkeep ',i7,' num ',i7)
951 format(' type ',a5,' pen= ',e25.18,' qcpen= ',g13.6,' r= ',g13.6,' qcr= ',g13.6)

! End of routine
  return
end subroutine statsco
