subroutine statsoz(stats_oz,ndata)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    statsoz     prints statistics for ozone
!   prgmmr: derber           org: np23                date: 2003-05-22
!
! abstract: The routine computes and prints statistics regarding the
!           use of ozone observations.  Printed information includes
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
!
!   input argument list:
!     stats_oz - array holding sums from various statistical output
!     ndata(*,1)- number of observation keep for processing
!     ndata(*,2)- number of observation read
!     ndata(*,3)- number of observations keep after read
!
!   output argument list:
!     stats_oz - array holding sums from various statistical output
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  use obsmod, only: ndat,iout_oz,dtype,dsis,dplat,ditype
  use ozinfo, only: error_oz,nusis_oz,nulev,iuse_oz,jpch_oz
  use jfunc, only: jiter
  implicit none

! Declare passed variables
  integer(i_kind),dimension(ndat,3)                ,intent(in   ) :: ndata
  real(r_kind),dimension(9,jpch_oz)                ,intent(inout) :: stats_oz


! Declare local variables
  logical,dimension(ndat):: idisplay
  integer(i_kind) j,iasim,i,icerr,ii
  real(r_kind) svar,rsum,stdev,cpen,penalty_all,qcpenalty_all
  real(r_kind),dimension(ndat):: rpenal,qcpenal
  integer(i_kind),dimension(ndat):: icount_asim,iqccount_asim

!******************************************************************************
! Compute and print statistics for ozone data
  open(iout_oz,position='append')
  write(iout_oz,850) jiter
850 format('OUTER ITERATION jiter=',i5)


! Print total penalty over all satellites
  penalty_all=zero
  qcpenalty_all=zero
  do i=1,jpch_oz
     iasim = nint(stats_oz(7,i))
     if (iasim>0) then
        penalty_all=penalty_all+stats_oz(5,i)
        qcpenalty_all=qcpenalty_all+stats_oz(8,i)
     endif
  end do
  write(iout_oz,100)'ozone total   penalty_all=',penalty_all
  write(iout_oz,100)'ozone total qcpenalty_all=',qcpenalty_all
100 format(a26,1x,g25.18)

! Print counts, bias, rms, stndev as a function of level
  icount_asim=0
  iqccount_asim=0
  rpenal=zero
  qcpenal=zero
  idisplay=.false.
  do ii=1,ndat
     if(ditype(ii) == 'ozone')then
        do i = 1,jpch_oz
           iasim = nint(stats_oz(1,i))
           if (iasim > 0 .and. nusis_oz(i) == dsis(ii) ) then
              if (iuse_oz(i)==1) then
                 icount_asim(ii) = icount_asim(ii) + iasim
                 rpenal(ii) = rpenal(ii) + stats_oz(5,i)
                 iqccount_asim(ii) = iqccount_asim(ii) + nint(stats_oz(9,i))
                 qcpenal(ii) = qcpenal(ii) + stats_oz(8,i)
              end if
              idisplay(ii)=.true.
           end if
        end do
     end if
  end do
  do i = 1,jpch_oz
     iasim = nint(stats_oz(1,i))
     if (iasim > 0) then
        svar = error_oz(i)
        if (iuse_oz(i)/=1) svar = -svar
        rsum = one/real(iasim,r_kind)
        icerr = nint(stats_oz(2,i))
        do j=3,6   ! j=3=obs-mod(w_biascor)
                   ! j=4=(obs-mod(w_biascor))**2
                   ! j=5=penalty contribution
                   ! j=6=obs

           stats_oz(j,i) = stats_oz(j,i)*rsum
        end do
        stats_oz(4,i) = sqrt(stats_oz(4,i))
        if (iasim > 1) then
           stdev  = sqrt(stats_oz(4,i)*stats_oz(4,i)-stats_oz(3,i)*stats_oz(3,i))
        else
           stdev = zero
        end if
        write(iout_oz,1102) i,nulev(i),nusis_oz(i),iasim,icerr,svar,&
             stats_oz(6,i),stats_oz(6,i)-stats_oz(3,i),stats_oz(3,i),&
             stats_oz(5,i),stats_oz(4,i),stdev
     endif
  end do

! Write obs count to runtime output file
  write(iout_oz,1109)
  do i=1,ndat
     if (idisplay(i)) then
        cpen=zero
        if (icount_asim(i)>0) cpen=rpenal(i)/real(icount_asim(i),r_kind)
        write(iout_oz,1115) jiter,dplat(i),dtype(i),ndata(i,2), &
             ndata(i,3),icount_asim(i),rpenal(i),cpen,qcpenal(i),iqccount_asim(i)
     endif
  end do
1102 format(1x,i4,i4,1x,a20,2i7,1x,f8.3,1x,6(e11.4,1x))
1109 format(t5,'it',t11,'sat',t22,'inst',t36,'# read',t46,'# keep',t55,'# assim',&
          t63,'penalty',t78,'cpen',t88,'qcpen',t101,'qcfail')
1115 format('o-g',1x,i2.2,1x,'oz ',a10,1x,a10,1x,3(i9,1x),3(g12.5,1x),i8)

! End of ozone diagnostic print block.

  close(iout_oz)

! End of routine
  return
end subroutine statsoz
