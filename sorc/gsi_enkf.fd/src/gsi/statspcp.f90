subroutine statspcp(aivals,ndata)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    statspcp   prints statistics for precipitation obs
!   prgmmr: derber          org: np23                 date: 2003-05-22
!
! abstract: The routine computes and prints statistics regarding the
!           use of precipitation observations.  Printed information
!           includes that about data counts, quality control decisions, 
!           statistics based on the innovations, and penalties - all 
!           as a function of precipitation observation and satellite
!           type
!
! program history log:
!   2003-05-22  derber
!   2004-06-15  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase diagnostic array sizes and add
!                         output from nonlinear qc
!   2004-12-23  treadon - use module jfunc to pass jiter,first
!   2005-03-09  parrish - add output of count of precip obs rejected 
!                         by nonlinear qc
!   2005-09-28  derber  - replace dtype parameter with obstype_all
!   2006-02-03  derber  - modify for new obs control and to clean up output
!   2006-07-28  derber  - use r1000 from constants
!
!   input argument list:
!     aivals   - array holding sums for various statistical output
!     ndata(*,1)- number of observation keep for processing
!     ndata(*,2)- number of observation read
!     ndata(*,3)- number of observations keep after read
!
!   output argument list:
!     aivals   - array holding sums for various statistical output
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

! Include data type module
  use kinds, only: r_kind,i_kind
  use pcpinfo, only: dtphys,deltim
  use obsmod, only: dtype,ndat,iout_pcp,dplat,ditype
  use jfunc, only: jiter,first
  use constants, only: zero,half,one,two,r1000,r3600
  implicit none

! Declare passed variables
  integer(i_kind),dimension(ndat,3) ,intent(in   ) :: ndata

  real(r_kind)   ,dimension(40,ndat),intent(inout) :: aivals

! Declare local variables
  integer(i_kind) i,j,is
  integer(i_kind) iread,ikeep,iasim
  integer(i_kind) nsphys
  integer(i_kind) iland
  integer(i_kind) isum,icoast,icerr
  integer(i_kind) isnoice,itotal,ireduce
  integer(i_kind) kdt,iratio,idzmax
  integer(i_kind) igradsml,igradbig,idetect
  integer(i_kind) inumw,inum,iclip,itoss
  integer(i_kind) ntossqc

  real(r_kind) sdv,rterm
  real(r_kind) pcpobs0,pcpnbc0,pcpbc0,pcpobs1,pcpnbc1,pcpbc1
  real(r_kind) pcpsas0,pcpsas1,rsum
  real(r_kind) frain,dtp,dtf
  real(r_kind) fhour,rtime
  real(r_kind) penalty_all,qcpenalty_all
  real(r_kind) rmmhr
  real(r_kind),dimension(ndat):: rpen,cpen,qcpen,qccpen

  logical,dimension(ndat):: display


! Initialize variables


! Open unit to receive printout
  open(iout_pcp,position='append')

! On first outer iteration, write constant parameters
  if (first ) then
     nsphys = max(int(two*deltim/dtphys+0.9999_r_kind),1)
     dtp    = two*deltim/nsphys
     dtf    = half*dtp
     frain  = dtf/dtp
     kdt    = 1
     fhour  = kdt*deltim/r3600
     rtime  = one/(r3600*fhour)
     rmmhr  = r1000*rtime * r3600
     write(iout_pcp,100)'deltim,dtphys=',deltim,dtphys
     write(iout_pcp,100)'dtp,dtf      =',dtp,dtf
     write(iout_pcp,100)'frain,fhour  =',frain,fhour
     write(iout_pcp,100)'rtime,rmmhr  =',rtime,rmmhr
     write(iout_pcp,110)'nsphys,kdt   =',nsphys,kdt
100  format(a14,1x,2(g25.18,1x))
110  format(a14,1x,2(i6,1x))

  endif


! Loop over all observational data types
  display=.false.
! Print information regarding qc.
  write(iout_pcp,*)' ' 
  write(iout_pcp,2000) 'obtype','num','numw',&
        'itotal','ireduce','iland','isnoice','icoast',&
        'grd<e-10','grd>e4','ratio>3','smooth',&
        'thrhold'
2000 format(a10,1x,15(a8,1x))
  do is=1,ndat
     inum    = nint(aivals(2,is))
     display(is) = ditype(is) == 'pcp' 

!    If precipitation observation has nonzero number of obs, generate output
     if (inum > 0 .and. display(is)) then
        inumw   = nint(aivals(1,is))
        itotal  = nint(aivals(3,is))
        
        ireduce = nint(aivals(4,is))
        iland   = nint(aivals(5,is))
        isnoice = nint(aivals(6,is))
        icoast  = nint(aivals(7,is))
        
        iclip   = nint(aivals(12,is))
        
        igradsml = nint(aivals(26,is))
        igradbig = nint(aivals(27,is))
        iratio   = nint(aivals(28,is))
        idzmax   = nint(aivals(29,is))
        idetect  = nint(aivals(30,is))
        
        write(iout_pcp,2010) dtype(is),inum,inumw,&
             itotal,ireduce,iland,isnoice,icoast,&
             igradsml,igradbig,iratio,idzmax,idetect
2010    format(a10,1x,12(i8,1x))
     end if
  end do
        

  write(iout_pcp,2001) 'obtype','qc-flag','satpcp','pcpnbc',&
       'pcpsas','pcplrg','pcpbc'
2001 format(a7,1x,a7,1x,5(a18,1x),a6)
  penalty_all=zero
  qcpenalty_all=zero
  ntossqc=0
  do is=1,ndat
     inum    = nint(aivals(2,is))

!    If precipitation observation has nonzero number of obs, generate output
     if (inum > 0 .and. display(is)) then
        rterm=zero
        if (aivals(25,is) > zero) rterm=one/aivals(25,is)
        pcpobs0 = aivals(21,is)*rterm
        pcpnbc0 = aivals(22,is)*rterm
        pcpbc0  = aivals(23,is)*rterm
        pcpsas0 = aivals(24,is)*rterm
        ikeep   = nint(aivals(25,is))

        if (aivals(35,is) > zero) rterm=one/aivals(35,is)
        pcpobs1 = aivals(31,is)*rterm
        pcpnbc1 = aivals(32,is)*rterm
        pcpbc1  = aivals(33,is)*rterm
        pcpsas1 = aivals(34,is)*rterm
        itoss   = nint(aivals(35,is))
        
        write(iout_pcp,2013)dtype(is),'keep',ikeep,pcpobs0,pcpnbc0,pcpsas0,&
             pcpnbc0-pcpsas0,pcpbc0
        write(iout_pcp,2013)dtype(is),'toss',itoss,pcpobs1,pcpnbc1,pcpsas1,&
             pcpnbc1-pcpsas1,pcpbc1
2013    format(a10,1x,a4,1x,i6,1x,5(g19.12,1x))
        
     
     
!       Accumualte total penalty
        penalty_all=penalty_all+aivals(15,is)
        qcpenalty_all=qcpenalty_all+aivals(39,is)
        ntossqc=ntossqc+nint(aivals(40,is))
     end if
  end do

  do is=1,ndat
     inum    = nint(aivals(2,is))

!    If precipitation observation has nonzero number of obs, generate output
     if (inum > 0 .and. display(is)) then
        write(iout_pcp,2012) dtype(is),aivals(15,is)
2012    format(A10,'  penalty     = ',g19.12)
        
     end if
  end do

! Print total precipitation penalty
  write(iout_pcp,*)' '
  write(iout_pcp,3000)penalty_all
3000 format('pcp total   penalty_all  =',g25.18)
  write(iout_pcp,3001)qcpenalty_all
3001 format('pcp total qcpenalty_all  =',g25.18)
  write(iout_pcp,3002)ntossqc
3002 format('pcp total failed nonlinqc=',i8)


! Print counts, bias, rms, stndev as a function of observation type
  do is = 1,ndat
     isum = nint(aivals(11,is))
     if (isum > 0 .and. display(is)) then
        rpen(is) = aivals(15,is)
        qcpen(is) = aivals(39,is)
        rsum  = one/real(isum,r_kind)
        icerr = nint(aivals(12,is))
        do j=13,16
           aivals(j,is)=aivals(j,is)*rsum
        end do
        aivals(14,is) = sqrt(aivals(14,is))
        cpen(is) = aivals(15,is)
        qccpen(is) = aivals(39,is)
        if (isum > 1) then
           sdv = sqrt(aivals(14,is)*aivals(14,is)- &
                aivals(13,is)*aivals(13,is))
        else
           sdv = zero
        endif
        write(iout_pcp,1102) dtype(is),dplat(is),isum,icerr,&
             aivals(16,is),aivals(13,is),aivals(15,is),aivals(14,is),sdv
1102    format(a10,1x,a10,1x,2(i6,1x),6(g17.10,1x))
     else
        rpen(is)   = zero
        qcpen(is)  = zero
        cpen(is)   = zero
        qccpen(is) = zero
     end if
  end do
  
  write(iout_pcp,1109)
1109 format(t5,'it',t13,'sat',t28,'# read',t39,'# keep',t49,'# assim',&
          t58,'penalty',t74,'cpen',t82,'qcpnlty',t95,'qccpen')
  do i=1,ndat
     if (aivals(11,i)>zero .and. display(i)) then
        iread=ndata(i,2)
        ikeep=nint(aivals(1,i))
        iasim=nint(aivals(11,i))
        write(iout_pcp,1115) jiter,dtype(i),iread,ikeep,iasim,&
             rpen(i),cpen(i),qcpen(i),qccpen(i)
     endif
  end do
1115 format('o-g',1x,i2.2,1x,'pcp',2x,a10,2x,3(i9,2x),4(g12.5,1x))
  
  
! Close output unit
  close(iout_pcp)


! End of routine
  return
end subroutine statspcp
