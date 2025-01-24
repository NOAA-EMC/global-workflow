subroutine sumslightbias(dlight,lightges0,mype,nobs,nobs_loc,sum_loc) 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: sumslightbias  calculation of variance in parallel (MPI). 

!   prgmmr: k apodaca <karina.apodaca@colostate.edu>
!      org: CSU/CIRA, Data Assimilation Group
!     date: 2016-09-08
!
! abstract:  This subroutine computes a local summation and the number of       
!            observations assigned to a given mpi task (geographic region). 
!
!            The former calculations are used as input in the bias correction
!            procedure applied to the nonlinear observation operator for
!            lightning flash rate, which is included in the "setuprhsall.f90" 
!            subroutine.
!
! program history log:
!   2016-09-08  apodaca  -  first version of sumslightbias 
!
!---
!
!   input argument list:
!     eps0  - guess value of lightning flash rate
!     mype  - mpi task id
!
!   output argument list:
!     sum_loc  - array containing  the summation, over all observations, of 
!                the logarithmic transformation of the observed lightning 
!                flash rate, divided by the forward model of lightning 
!                flash rate. 
!
!     nobs_loc - number of observations per given mpi task (CPU)
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:  
!
  use kinds, only: r_kind,r_single,r_double,i_kind
  use constants, only: zero
  implicit none

! Declare local variables

  real(r_kind)                    :: dlight
  real(r_kind)                    :: eps0
  real(r_kind)                    :: lightges0
  integer(i_kind)                 :: i,nobs
  integer(i_kind),intent(inout)   :: nobs_loc,mype
  real(r_kind),intent(out)        :: sum_loc

! File(s) for postprocessing
  character :: post_file*40


!---
! Online bias correction, as in Apodaca et al. (2014);
! eps = eps0 * exp[ (1/nobs) * sum(log(y/(eps0*h(x)))) / (1+r0/w0)]

! In this program, sum(log(y/(eps0*h(x)))) and the commulative count
! of lightning observations are calculated (nobs_loc).


!-- set initial bias parameter values

  eps0=1._r_kind

  sum_loc=zero
  nobs_loc=zero
      
!-- save sums here
!-- for each i there is an associated error(i), lightges0(i), and dlight(i)
!-- Step 1:  Estimate sums
  

! Open file with lightning output for local sums

  write(post_file,199)mype
199 format('sums_lfr_',i3.3,'.bin')
  open(unit=200,file=trim(post_file),form='formatted',action='read')

  do i=1,nobs
        
     read(200,*)nobs_loc,dlight,lightges0

     nobs=nobs_loc
        
     if(((eps0*lightges0) > 0_r_kind).and.(lightges0 > 0_r_kind).and.(dlight >0_r_kind)) then
         
        nobs_loc=nobs_loc+1
        sum_loc=sum_loc+log(dlight/(eps0*lightges0))

     endif

  enddo

close(unit=200)

end subroutine sumslightbias
