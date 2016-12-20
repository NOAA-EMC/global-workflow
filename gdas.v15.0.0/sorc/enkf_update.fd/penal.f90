subroutine penal(xhat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    penal       oberror tuning
!   prgmmr: wu               org: np23                date: 2005-08-26
!
! abstract: randomized estimation of Tr(KH) and Tr(HK) and 
!            adaptive tuning  
!
!
! program history log:
!   2005-08-15  wu - oberror tuning
!   2008-03-24  wu - use convinfo ikx as index for oberr tune
!   2008-05-27  safford - rm unused vars
!   2008-12-03  todling - update in light of state vector and obs binning
!   2010-05-13  todling - update to use gsi_bundle
!
! usage: intt(st,rt)
!   input argument list:
!     xhat    - increment in grid space
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_sum,mpi_rtype,mype
  use constants, only: zero,one
  use gsi_4dvar, only: nobs_bins
  use obsmod, only: qhead,qptr,thead,tptr,whead,wptr,pshead,psptr
  use jfunc, only: jiterstart,jiter
  use convinfo, only: ictype,nconvtype,ioctype
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer 
  implicit none

! Declare passed variables

  type(gsi_bundle),intent(in   ) :: xhat

! Declare passed variables
  real(r_kind),save,dimension(33,200) ::  penalty,trace

! Declare local variables
  real(r_kind) err2

  integer(i_kind) i,n,k,ibin,ier,istatus
  real(r_kind) tpenalty(33,nconvtype),ttrace(33,nconvtype)
  real(r_kind) valu,valv,val,so(33,nconvtype),cat_num(33,nconvtype),sosum,tcat_num(33,nconvtype)
  integer(i_kind) itype,ncat,k1
  real(r_kind),pointer,dimension(:):: xhat_u,xhat_v,xhat_q,xhat_t,xhat_p

! Get pointers and return if not found
  ier=0
  call gsi_bundlegetpointer(xhat,'u' ,xhat_u,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat,'v' ,xhat_v,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat,'q' ,xhat_q,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat,'tv',xhat_t,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat,'ps',xhat_p,istatus);ier=istatus+ier
  if(ier/=0) return

  ncat=nconvtype*33

  if(jiter==jiterstart)then
     trace=zero
     penalty=zero

     do ibin=1,nobs_bins

!       Moisture
        qptr => qhead(ibin)%head
        do while (associated(qptr))
           n=qptr%kx
           itype=ictype(n)

           if(itype==120)then
              k1=qptr%k1
           else
              k1=1 
           endif
 
           err2=qptr%raterr2*qptr%err2
!          err=sqrt(err2)
!          Forward model
           val= qptr%wij(1)* xhat_q(qptr%ij(1))+qptr%wij(2)* xhat_q(qptr%ij(2))&
               +qptr%wij(3)* xhat_q(qptr%ij(3))+qptr%wij(4)* xhat_q(qptr%ij(4))&
               +qptr%wij(5)* xhat_q(qptr%ij(5))+qptr%wij(6)* xhat_q(qptr%ij(6))&
               +qptr%wij(7)* xhat_q(qptr%ij(7))+qptr%wij(8)* xhat_q(qptr%ij(8))

           trace(k1,n)=trace(k1,n)-qptr%qpertb*val*err2
           penalty(k1,n)=penalty(k1,n)+(val-qptr%res)**2*err2
           qptr => qptr%llpoint
        end do
!       if(mype==29)write(0,*)'q2 trace,pen=',trace(k1,n),penalty(k1,n),k1,n

!       Temperature
        tptr => thead(ibin)%head
        do while (associated(tptr))
           n=tptr%kx
           itype=ictype(n)

           if(itype==120)then
              k1=tptr%k1
           else
              k1=1
           endif

           err2=tptr%raterr2*tptr%err2
!          err=sqrt(err2)
!          Forward model
           val= tptr%wij(1)* xhat_t(tptr%ij(1))+tptr%wij(2)* xhat_t(tptr%ij(2))&
               +tptr%wij(3)* xhat_t(tptr%ij(3))+tptr%wij(4)* xhat_t(tptr%ij(4))&
               +tptr%wij(5)* xhat_t(tptr%ij(5))+tptr%wij(6)* xhat_t(tptr%ij(6))&
               +tptr%wij(7)* xhat_t(tptr%ij(7))+tptr%wij(8)* xhat_t(tptr%ij(8))

           trace(k1,n)=trace(k1,n)-tptr%tpertb*val*err2
           penalty(k1,n)=penalty(k1,n)+(val-tptr%res)**2*err2
           tptr => tptr%llpoint
        end do
     
!       Surface pressure
        psptr => pshead(ibin)%head
        do while (associated(psptr))
           n=psptr%kx
           itype=ictype(n)
           k1=1

           err2=psptr%raterr2*psptr%err2
!          err=sqrt(err2)
!          Forward model
           val= psptr%wij(1)* xhat_p(psptr%ij(1))+psptr%wij(2)* xhat_p(psptr%ij(2))&
               +psptr%wij(3)* xhat_p(psptr%ij(3))+psptr%wij(4)* xhat_p(psptr%ij(4))

           trace(k1,n)=trace(k1,n)-psptr%ppertb*val*err2
           penalty(k1,n)=penalty(k1,n)+(val-psptr%res)**2*err2
           psptr => psptr%llpoint
        end do

!       Winds
        wptr => whead(ibin)%head
        do while (associated(wptr))
           n=wptr%kx
           itype=ictype(n)

           if(itype==220 .or. itype==223 .or. itype==233 .or. itype==245)then
              k1=wptr%k1
           else
              k1=1
           endif
 
           err2=wptr%raterr2*wptr%err2
!          err=sqrt(err2)
!          Forward model
           valu= wptr%wij(1)* xhat_u(wptr%ij(1))+wptr%wij(2)* xhat_u(wptr%ij(2))&
                +wptr%wij(3)* xhat_u(wptr%ij(3))+wptr%wij(4)* xhat_u(wptr%ij(4))&
                +wptr%wij(5)* xhat_u(wptr%ij(5))+wptr%wij(6)* xhat_u(wptr%ij(6))&
                +wptr%wij(7)* xhat_u(wptr%ij(7))+wptr%wij(8)* xhat_u(wptr%ij(8))
           valv= wptr%wij(1)* xhat_v(wptr%ij(1))+wptr%wij(2)* xhat_v(wptr%ij(2))&
                +wptr%wij(3)* xhat_v(wptr%ij(3))+wptr%wij(4)* xhat_v(wptr%ij(4))&
                +wptr%wij(5)* xhat_v(wptr%ij(5))+wptr%wij(6)* xhat_v(wptr%ij(6))&
                +wptr%wij(7)* xhat_v(wptr%ij(7))+wptr%wij(8)* xhat_v(wptr%ij(8))

           trace(k1,n)=trace(k1,n)-(wptr%upertb*valu+wptr%vpertb*valv)*err2
           penalty(k1,n)=penalty(k1,n)+((valu-wptr%ures)**2+(valv-wptr%vres)**2)*err2
           wptr => wptr%llpoint
        end do

     end do ! ibin
     

  else ! jiter
     cat_num=zero

     do ibin=1,nobs_bins
 
!       Moisture
!       ratiomin=one
        qptr => qhead(ibin)%head
        do while (associated(qptr))
           n=qptr%kx
           itype=ictype(n)

           if(itype==120)then
              k1=qptr%k1
           else
              k1=1
           endif

           err2=qptr%raterr2*qptr%err2
!          err=sqrt(err2)
!          Forward model
           val= qptr%wij(1)* xhat_q(qptr%ij(1))+qptr%wij(2)* xhat_q(qptr%ij(2))&
               +qptr%wij(3)* xhat_q(qptr%ij(3))+qptr%wij(4)* xhat_q(qptr%ij(4))&
               +qptr%wij(5)* xhat_q(qptr%ij(5))+qptr%wij(6)* xhat_q(qptr%ij(6))&
               +qptr%wij(7)* xhat_q(qptr%ij(7))+qptr%wij(8)* xhat_q(qptr%ij(8))

           cat_num(k1,n)=cat_num(k1,n)+one
           trace(k1,n)=trace(k1,n)+qptr%qpertb*val*err2
           qptr => qptr%llpoint
        end do
     
!       if(mype==29)write(0,*)'q2 trace,pen=',trace(k1,n),cat_num(k1,n),k1,n
!       Temperature
        tptr => thead(ibin)%head
        do while (associated(tptr))
           n=tptr%kx
           itype=ictype(n)

           if(itype==120)then
              k1=tptr%k1
           else
              k1=1
           endif

           err2=tptr%raterr2*tptr%err2
!          err=sqrt(err2)
!          Forward model
           val= tptr%wij(1)* xhat_t(tptr%ij(1))+tptr%wij(2)* xhat_t(tptr%ij(2))&
               +tptr%wij(3)* xhat_t(tptr%ij(3))+tptr%wij(4)* xhat_t(tptr%ij(4))&
               +tptr%wij(5)* xhat_t(tptr%ij(5))+tptr%wij(6)* xhat_t(tptr%ij(6))&
               +tptr%wij(7)* xhat_t(tptr%ij(7))+tptr%wij(8)* xhat_t(tptr%ij(8))

           cat_num(k1,n)=cat_num(k1,n)+one
           trace(k1,n)=trace(k1,n)+tptr%tpertb*val*err2
           tptr => tptr%llpoint
        end do
!       Surface pressure
        psptr => pshead(ibin)%head
        do while (associated(psptr))
           n=psptr%kx
           itype=ictype(n)
           k1=1

           err2=psptr%raterr2*psptr%err2
!          err=sqrt(err2)
!          Forward model
           val= psptr%wij(1)* xhat_p(psptr%ij(1))+psptr%wij(2)* xhat_p(psptr%ij(2))&
               +psptr%wij(3)* xhat_p(psptr%ij(3))+psptr%wij(4)* xhat_p(psptr%ij(4))

           cat_num(k1,n)=cat_num(k1,n)+one
           trace(k1,n)=trace(k1,n)+psptr%ppertb*val*err2
           psptr => psptr%llpoint
        end do
!       Winds
        wptr => whead(ibin)%head
        do while (associated(wptr))
           n=wptr%kx
           itype=ictype(n)

           if(itype==220 .or. itype==223 .or. itype==233 .or. itype==245)then
              k1=wptr%k1
           else
              k1=1
           endif

           err2=wptr%raterr2*wptr%err2
!          err=sqrt(err2)
!          Forward model
           valu= wptr%wij(1)* xhat_u(wptr%ij(1))+wptr%wij(2)* xhat_u(wptr%ij(2))&
                +wptr%wij(3)* xhat_u(wptr%ij(3))+wptr%wij(4)* xhat_u(wptr%ij(4))&
                +wptr%wij(5)* xhat_u(wptr%ij(5))+wptr%wij(6)* xhat_u(wptr%ij(6))&
                +wptr%wij(7)* xhat_u(wptr%ij(7))+wptr%wij(8)* xhat_u(wptr%ij(8))
           valv= wptr%wij(1)* xhat_v(wptr%ij(1))+wptr%wij(2)* xhat_v(wptr%ij(2))&
                +wptr%wij(3)* xhat_v(wptr%ij(3))+wptr%wij(4)* xhat_v(wptr%ij(4))&
                +wptr%wij(5)* xhat_v(wptr%ij(5))+wptr%wij(6)* xhat_v(wptr%ij(6))&
                +wptr%wij(7)* xhat_v(wptr%ij(7))+wptr%wij(8)* xhat_v(wptr%ij(8))

           cat_num(k1,n)=cat_num(k1,n)+one
           trace(k1,n)=trace(k1,n)+(wptr%upertb*valu+wptr%vpertb*valv)*err2
           wptr => wptr%llpoint
        end do
     
        do n=1,nconvtype
           do k=1,33
              trace(k,n)=cat_num(k,n)-trace(k,n)
           enddo
        enddo

     end do ! ibin

     call mpi_allreduce(trace,ttrace,ncat,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce(penalty,tpenalty,ncat,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     call mpi_allreduce(cat_num,tcat_num,ncat,mpi_rtype,mpi_sum, &
          mpi_comm_world,ierror)
     if(mype==0)then
        do n=1,nconvtype 
           write(233,*)'obs type=',ictype(n),trim(ioctype(n))
           do k=1,33
              if(tcat_num(k,n)>zero .and. tcat_num(k,n)<10._r_kind)write(223,*)k,n,tcat_num(k,n)
              write(233,*)k,n,tpenalty(k,n),ttrace(k,n),tcat_num(k,n)
           enddo
        enddo

        so=one
        do n=1,nconvtype 
           do k=1,33
              if(ttrace(k,n) /= zero .and. tcat_num(k,n)>10._r_kind) then
                 so(k,n)=tpenalty(k,n)/ttrace(k,n)
                 write(234,*)k,n,ictype(n),trim(ioctype(n)),so(k,n)
              endif
              if(so(k,n) >= zero) then
                 so(k,n)=sqrt(so(k,n))
              else
                 so(k,n)=one
              endif
           enddo
        enddo
        sosum=zero
        do i=1,ncat
           sosum=sosum+(so(i,1)-one)**2
        enddo
        write(235,*)'sosum=',sosum
     endif ! mype

     call mpi_finalize(ierror)
     stop
  endif ! jiter
  return
end subroutine penal
