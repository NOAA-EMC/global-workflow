subroutine statsrad(aivals,stats,ndata)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    statsrad    prints statistics for radiance data
!   prgmmr: derber           org: np23                date: 2003-05-22
!
! abstract: The routine computes and prints statistics regarding the
!           use of radiance (brightness temperature) observations.  
!           Printed information includes that about data counts, 
!           quality control decisions, statistics based on the 
!           innovations, and penalties - all as a function of 
!           channel, sensor, and satellite.
!
! program history log:
!   2003-05-22  derber
!   2004-06-15  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase diagnostic array sizes and add
!                         output from nonlinear qc
!   2004-11-23  treadon - change 1102 format statement
!   2004-12-23  treadon - use module jfunc to pass jiter
!   2005-01-20  okamoto - add displaying statistics of ssmi/amsre/ssmis
!   2005-07-06  derber  - add mhs and hirs4
!   2005-09-28  derber  - replace dtype parameter with obstype_all
!   2005-10-20  kazumori - add amsre
!   2006-02-03  derber - modify to simplify and new obs control
!   2008-04-11  safford - rm unused vars, comment out unused labels
!
!   input argument list:
!     aivals   - array holding sums fro various statistical output
!     ndata(*,1)- number of observation keep for processing
!     ndata(*,2)- number of observation read
!     ndata(*,3)- number of observations keep after read
!     stats
!
!   output argument list:
!     stats
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  use obsmod, only: dtype,iout_rad,ndat,dplat,ditype
  use radinfo, only: varch,iuse_rad,nuchan,jpch_rad,nusis
  use jfunc, only: jiter
  implicit none

! Declare passed variables
  real(r_kind)   ,dimension(40,ndat)   ,intent(in   ) :: aivals
  real(r_kind)   ,dimension(7,jpch_rad),intent(inout) :: stats
  integer(i_kind),dimension(ndat,3)    ,intent(in   ) :: ndata

! Declare local variables
  integer(i_kind) iobs2,i,j,is,iasim
  integer(i_kind) iland,isnoice,icoast,ireduce,ivarl,nlgross,nlgross_all
  integer(i_kind) icerr
  
  real(r_kind) penalty_all,qcpenalty_all,rpenal,qcpenal
  real(r_kind) svar,rsum,stdev,cpen,qcpen
  
  logical,dimension(ndat):: idisplay

  character(10)::obstype


! Open unit to receive printout
  open(iout_rad,position='append')

! Loop over all observational data types
  penalty_all=zero
  qcpenalty_all=zero
  nlgross_all=0
  idisplay=.false.
  do is=1,ndat

!    If radiance observation type and has nonzero number of obs, generate output
     if(ditype(is) == 'rad' ) then 
        obstype=dtype(is)
        iobs2 = nint(aivals(1,is))
        penalty_all=penalty_all+aivals(40,is)
        qcpenalty_all=qcpenalty_all+aivals(39,is)
        nlgross_all = nlgross_all + nint(aivals(2,is))
        idisplay(is) = .true.
        if(iobs2 > 0)then

           nlgross = nint(aivals(2,is))
           iland   = nint(aivals(3,is))
           isnoice = nint(aivals(4,is))
           icoast  = nint(aivals(5,is))
           ireduce = nint(aivals(6,is))
           ivarl   = nint(aivals(7,is))
           
           write(iout_rad,4000) 'sat','type','penalty','nobs','iland', &
                'isnoice','icoast','ireduce','ivarl','nlgross'
           write(iout_rad,4001) dplat(is),obstype,aivals(40,is), &
                 iobs2,iland,isnoice,icoast,ireduce,ivarl,nlgross
           write(iout_rad,4002) 'qcpenalty','qc1','qc2','qc3','qc4','qc5','qc6','qc7'    
           write(iout_rad,4003) aivals(39,is),(nint(aivals(i,is)),i=8,14)
4000       format(2x,a3,7x,a4,14x,a7,1x,8(a7,1x))
4001       format(1x,a10,1x,a10,f16.8,8(i7,1x))
4002       format(28x,a9,1x,8(a7,1x))
4003       format(22x,f16.8,8(i7,1x))
           
           write(iout_rad,'(/)')
        end if
     end if
     
  end do
  write(iout_rad,100)'rad total   penalty_all=',penalty_all
  write(iout_rad,100)'rad total qcpenalty_all=',qcpenalty_all
  write(iout_rad,100)'rad total failed nonlinqc=',nlgross_all
100 format(a26,1x,g25.18)

! Print counts, bias, rms, stndev as a function of channel.
  do i = 1,jpch_rad
     iasim = nint(stats(1,i))
     if (iasim > 0) then
        svar = varch(i)
        if (iuse_rad(i) < 1) svar=-svar
        rsum = one/float(iasim)
        icerr = nint(stats(2,i))
        do j=3,6   ! j=3=obs-mod(w_biascor)
                   ! j=4=(obs-mod(w_biascor))**2
                   ! j=5=penalty contribution
                   ! j=6=obs-mod(w/o_biascor)
           stats(j,i) = stats(j,i)*rsum
        end do
        stats(4,i) = sqrt(stats(4,i))
        if (iasim > 1) then
           stdev  = sqrt(stats(4,i)*stats(4,i)-stats(3,i)*stats(3,i))
        else
           stdev = zero
        end if
        write(iout_rad,1102) i,nuchan(i),nusis(i),iasim,icerr,svar,&
             stats(6,i),stats(3,i),stats(5,i),stats(4,i),stdev
     endif
  end do

! Write obs count to runtime output file
  write(iout_rad,1109)
  do i=1,ndat
     if(idisplay(i))then
        iobs2 = nint(aivals(38,i))
        qcpenal = aivals(39,i)
        rpenal = aivals(40,i)
        cpen=zero
        qcpen=zero
        if (iobs2 > 0) then
           cpen=rpenal/aivals(38,i)
           qcpen=qcpenal/aivals(38,i)
        end if
        write(iout_rad,1115) jiter,dplat(i),dtype(i),ndata(i,2), &
             ndata(i,3),iobs2,rpenal,qcpenal,cpen,qcpen
     endif
  end do
2000 format(a3,9x,a4,8x,8(a7,1x))
2010 format(a10,1x,A10,3x,8(i7,1x))
2011 format(8x,f16.8,8(i7,1x))
2012 format(12x,A7,5x,8(a7,1x))
2999 format(' Illegal satellite type ')
1102 format(1x,i4,i5,1x,a16,2i7,1x,f10.3,1x,6(f11.7,1x))
1109 format(t5,'it',t13,'satellite',t23,'instrument',t38, &
          '# read',t49,'# keep',t59,'# assim',&
          t68,'penalty',t81,'qcpnlty',t95,'cpen',t105,'qccpen')
1115 format('o-g',1x,i2.2,1x,'rad',2x,2A10,2x,3(i9,2x),4(g12.5,1x))

! Close output unit
  close(iout_rad)


! End of routine
  return
end subroutine statsrad
