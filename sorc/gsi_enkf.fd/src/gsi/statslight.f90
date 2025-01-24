subroutine statslight(mype,i_light,bwork,awork,i_ref,ndata)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    statslight    prints statistics for lightning data
!   prgmmr: k apodaca <karina.apodaca@colostate.edu> 
!      org: CSU/CIRA, Data Assimilation Group
!     date: 2016-04-06
!
! abstract: The routine computes and prints statistics regarding the
!           use of lightning observations.  Printed information 
!           includes that about data counts, quality control decisions,
!           statistics based on the innovations, and penalties.
!
! program history log:
!   2016-04-05  apodaca - 
!
!   input argument list:
!     mype       - mpi task number
!     i_light    - index in awork array holding lightning info
!     bwork      - array containing information for statistics
!     awork      - array containing information for data counts and gross checks
!     ndata(*,1) - number of profiles retained for further processing
!     ndata(*,2) - number of observations read
!     ndata(*,3) - number of observations keep after read
!
!   output argument list:
!
! attributes:
!   language: Fortran 90 and/or higher
!    machine:  
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,three,five
  use obsmod, only: iout_light,&
       mype_light,&
       ndat,dtype
  use qcmod, only: npres_print,ptop,pbot
  use jfunc, only: first,jiter
  use gridmod, only: nsig
  use lightinfo, only: nulight,nlighttype
  implicit none

! Declare passed variables
  integer(i_kind) i_ref,numgrslight,nsuperl
  integer(i_kind)                                   ,intent(in   ) :: mype,i_light
  real(r_kind),dimension(7*nsig+100,i_ref)          ,intent(in   ) :: awork
  real(r_kind),dimension(npres_print,nlighttype,5,3),intent(in   ) :: bwork
  integer(i_kind),dimension(ndat,3)                 ,intent(in   ) :: ndata

! Declare local variables
  character(100) mesage

  integer(i_kind) i,j
  integer(i_kind) numfailqc,nread,nkeep

  real(r_kind) grsmlt,tlight
  real(r_kind) tlight3
  
  logical,dimension(nlighttype):: pflag

!*********************************************************************************

! Generate statistics Summary report for lightning flash rate

  if(mype==mype_light) then
     nread=0
     nkeep=0
     do i=1,ndat
        if(dtype(i)== 'light')then
           nread=nread+ndata(i,2)
           nkeep=nkeep+ndata(i,3)
        end if
     end do
     if(nread > 0)then
        pflag=.FALSE.
        if(first)then
           open(iout_light)
        else
           open(iout_light,position='append')
        end if

        nsuperl=nint(awork(4,i_light))
        tlight=zero ; tlight3=zero
        if(nkeep > 0)then
           mesage='current fit of lightning  data, range in #hits km-2 hr-1$'
           do j=1,nlighttype
              pflag(j)=trim(nulight(j)) == 'light'
           enddo  
        
           call dtast(bwork,1,pbot,ptop,mesage,jiter,iout_light,pflag)

           numgrslight=nint(awork(6,i_light))
           numfailqc=nint(awork(21,i_light))
           grsmlt=three
           tlight=zero
           if(nsuperl > 0)then
              tlight=awork(5,i_light)/nsuperl
              tlight3=awork(22,i_light)/nsuperl
           end if
           write(iout_light,925) 'light',numgrslight,numfailqc
           write(iout_light,975) grsmlt,'light',awork(7,i_light)
        end if
        write(iout_light,950) 'light',jiter,nread,nkeep,nsuperl
        write(iout_light,951) 'light',awork(5,i_light),awork(22,i_light),tlight,tlight3

        close(iout_light)
     end if
  end if



! Format statements used above
111 format('obs lev   num     rms         bias        sumges       sumobs        cpen')
240 format(' num(',A1,') = ',i6,' at lev ',i4,' pen,qcpen,cpen,cqcpen = ',6(g12.5,1x))
241 format(' num(',A1,') = ',i6,' at lev ',i4,' upen,vpen,cupen,cvpen = ',6(g12.5,1x))
900 format(' number of ',a5,' obs extrapolated above',&
         ' top sigma layer=',i8,/,10x,' number extrapolated below',&
         ' bottom sigma layer=',i8)
920 format(a44,i7)
924 format(a50)
925 format(' number of ',a5,' obs that failed gross test = ',I5,' nonlin qc test = ',I5)
949 format(' number of ',a5,' obs = ',i7,' pen= ',e25.18,' cpen= ',g13.6)
950 format(' type ',a7,' jiter ',i3,' nread ',i7,' nkeep ',i7,' num ',i7)
951 format(' type ',a7,' pen= ',e25.18,' qcpen= ',e25.18,' r= ',g13.6,' qcr= ',g13.6)
952 format(t5,'it',t13,'sat',t21,'# read',t32,'# keep',t42,'# assim',&
         t52,'penalty',t67,'cpen')
975 format(' grsmlt=',f7.1,' number of bad ',a5,' obs=',f8.0)
  
  return


end subroutine statslight
