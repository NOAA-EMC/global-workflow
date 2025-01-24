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
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2018-10-02  wu - re-arrange dimensions since subtypes are used in convinfo but not in errtable
!                    select height dependent (or not) base on amount of the observation
!                    sfc obs tuning is not height dependent
!                    put back code to calculate tuning corf's and write out the new errtable
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
  use m_obsNode, only: obsNode
  use m_qNode , only:  qNode, qNode_typecast, qNode_nextcast
  use m_tNode , only:  tNode, tNode_typecast, tNode_nextcast
  use m_wNode , only:  wNode, wNode_typecast, wNode_nextcast
  use m_psNode, only: psNode,psNode_typecast,psNode_nextcast
  use m_obsdiags, only: obOper_headNode
  use gsi_obOperTypeManager, only: iobOper_q
  use gsi_obOperTypeManager, only: iobOper_t
  use gsi_obOperTypeManager, only: iobOper_w
  use gsi_obOperTypeManager, only: iobOper_ps

  use converr, only:etabl
  use jfunc, only: jiterstart,jiter
  use convinfo, only: ictype
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables

  type(gsi_bundle),intent(in   ) :: xhat

! Declare passed variables
  integer(i_kind), parameter    :: ld=300
  integer(i_kind), parameter    :: np=33
  integer(i_kind), parameter    :: nv=4

  real(r_kind),save,dimension(np,ld,nv) ::  penalty,trace

! Declare local variables
  real(r_kind) err2

  integer(i_kind) i,n,k,ibin,ier,istatus
  real(r_kind) tpenalty(np,ld,nv),ttrace(np,ld,nv)
  real(r_kind) valu,valv,val,so(np,ld,nv),sosum
  integer(i_kind) itype,ncat,k1,m,l
  real(r_kind) cat_num(np,ld,nv),tcat_num(np,ld,nv),cat_numt(ld,nv)
  real(r_kind),pointer,dimension(:):: xhat_u,xhat_v,xhat_q,xhat_t,xhat_p
  character(2) obtype(nv)

  type( qNode),pointer::  qptr
  type( tNode),pointer::  tptr
  type( wNode),pointer::  wptr
  type(psNode),pointer:: psptr

! Get pointers and return if not found
  obtype(1)='ps'
  obtype(2)='t '
  obtype(3)='q '
  obtype(4)='uv'
  ier=0
  call gsi_bundlegetpointer(xhat,'u' ,xhat_u,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat,'v' ,xhat_v,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat,'q' ,xhat_q,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat,'tv',xhat_t,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xhat,'ps',xhat_p,istatus);ier=istatus+ier
  if(ier/=0) return

  ncat=np*ld*nv

  if(jiter==jiterstart)then
     trace=zero
     penalty=zero

     do ibin=1,nobs_bins

!       Moisture
        !!qptr =>  qNode_typecast(obsLList_headNode(qhead(ibin)))
        !anode => obsLList_headNode(qhead(ibin))
        !qptr  => qNode_typecast(anode)
        !anode => null()
        qptr  => qNode_typecast(obOper_headNode(iobOper_q,ibin))
        m=3
        do while (associated(qptr))
           n=qptr%kx
           itype=ictype(n)
           n=itype

           if(itype > 179)then
              k1=1
           else
              k1=qptr%k1
           endif

           err2=qptr%raterr2*qptr%err2
!          Forward model
           val= qptr%wij(1)* xhat_q(qptr%ij(1))+qptr%wij(2)* xhat_q(qptr%ij(2))&
               +qptr%wij(3)* xhat_q(qptr%ij(3))+qptr%wij(4)* xhat_q(qptr%ij(4))&
               +qptr%wij(5)* xhat_q(qptr%ij(5))+qptr%wij(6)* xhat_q(qptr%ij(6))&
               +qptr%wij(7)* xhat_q(qptr%ij(7))+qptr%wij(8)* xhat_q(qptr%ij(8))

           trace(k1,n,m)=trace(k1,n,m)-qptr%qpertb*val*err2
           penalty(k1,n,m)=penalty(k1,n,m)+(val-qptr%res)**2*err2
           qptr =>  qNode_nextcast(qptr)
        end do

!       Temperature
        !!tptr =>  tNode_typecast(obsLList_headNode(thead(ibin)))
        !anode => obsLList_headNode(thead(ibin))
        !tptr  => tNode_typecast(anode)
        !anode => null()
        tptr  => tNode_typecast(obOper_headNode(iobOper_t,ibin))
        m=2
        do while (associated(tptr))
           n=tptr%kx
           itype=ictype(n)
           n=itype

           if(itype > 179)then
              k1=1
           else
              k1=tptr%k1
           endif

           err2=tptr%raterr2*tptr%err2
!          Forward model
           val= tptr%wij(1)* xhat_t(tptr%ij(1))+tptr%wij(2)* xhat_t(tptr%ij(2))&
               +tptr%wij(3)* xhat_t(tptr%ij(3))+tptr%wij(4)* xhat_t(tptr%ij(4))&
               +tptr%wij(5)* xhat_t(tptr%ij(5))+tptr%wij(6)* xhat_t(tptr%ij(6))&
               +tptr%wij(7)* xhat_t(tptr%ij(7))+tptr%wij(8)* xhat_t(tptr%ij(8))

           trace(k1,n,m)=trace(k1,n,m)-tptr%tpertb*val*err2
           penalty(k1,n,m)=penalty(k1,n,m)+(val-tptr%res)**2*err2
           tptr =>  tNode_nextcast(tptr)
        end do

!       Surface pressure
        !!psptr => psNode_typecast(obsLList_headNode(pshead(ibin)))
        !anode => obsLList_headNode(pshead(ibin))
        !psptr => psNode_typecast(anode)
        !anode => null()
        psptr  => psNode_typecast(obOper_headNode(iobOper_ps,ibin))
        m=1
        do while (associated(psptr))
           n=psptr%kx
           itype=ictype(n)
           k1=1
           n=itype

           err2=psptr%raterr2*psptr%err2
!          Forward model
           val= psptr%wij(1)* xhat_p(psptr%ij(1))+psptr%wij(2)* xhat_p(psptr%ij(2))&
               +psptr%wij(3)* xhat_p(psptr%ij(3))+psptr%wij(4)* xhat_p(psptr%ij(4))

           trace(k1,n,m)=trace(k1,n,m)-psptr%ppertb*val*err2
           penalty(k1,n,m)=penalty(k1,n,m)+(val-psptr%res)**2*err2
           psptr => psNode_nextcast(psptr)
        end do

!       Winds
        !!wptr =>  wNode_typecast(obsLList_headNode(whead(ibin)))
        !anode => obsLList_headNode(whead(ibin))
        !wptr  =>  wNode_typecast(anode)
        !anode => null()
        wptr  => wNode_typecast(obOper_headNode(iobOper_w,ibin))
        m=4
        do while (associated(wptr))
           n=wptr%kx
           itype=ictype(n)
           n=itype

           if(itype > 279)then
              k1=1
           else
              k1=wptr%k1
           endif

           err2=wptr%raterr2*wptr%err2
!          Forward model
           valu= wptr%wij(1)* xhat_u(wptr%ij(1))+wptr%wij(2)* xhat_u(wptr%ij(2))&
                +wptr%wij(3)* xhat_u(wptr%ij(3))+wptr%wij(4)* xhat_u(wptr%ij(4))&
                +wptr%wij(5)* xhat_u(wptr%ij(5))+wptr%wij(6)* xhat_u(wptr%ij(6))&
                +wptr%wij(7)* xhat_u(wptr%ij(7))+wptr%wij(8)* xhat_u(wptr%ij(8))
           valv= wptr%wij(1)* xhat_v(wptr%ij(1))+wptr%wij(2)* xhat_v(wptr%ij(2))&
                +wptr%wij(3)* xhat_v(wptr%ij(3))+wptr%wij(4)* xhat_v(wptr%ij(4))&
                +wptr%wij(5)* xhat_v(wptr%ij(5))+wptr%wij(6)* xhat_v(wptr%ij(6))&
                +wptr%wij(7)* xhat_v(wptr%ij(7))+wptr%wij(8)* xhat_v(wptr%ij(8))

           trace(k1,n,m)=trace(k1,n,m)-(wptr%upertb*valu+wptr%vpertb*valv)*err2
           penalty(k1,n,m)=penalty(k1,n,m)+((valu-wptr%ures)**2+(valv-wptr%vres)**2)*err2
           wptr =>  wNode_nextcast(wptr)
        end do

     end do ! ibin

  else ! jiter
     cat_num=zero

     do ibin=1,nobs_bins

!       Moisture
!       ratiomin=one
        !!qptr =>  qNode_typecast(obsLList_headNode(qhead(ibin)))
        !anode => obsLList_headNode(qhead(ibin))
        !qptr  => qNode_typecast(anode)
        !anode => null()
        qptr  => qNode_typecast(obOper_headNode(iobOper_q,ibin))
        m=3
        do while (associated(qptr))
           n=qptr%kx
           itype=ictype(n)
           n=itype

           if(itype > 179)then
              k1=1
           else
              k1=qptr%k1
           endif

           err2=qptr%raterr2*qptr%err2
!          Forward model
           val= qptr%wij(1)* xhat_q(qptr%ij(1))+qptr%wij(2)* xhat_q(qptr%ij(2))&
               +qptr%wij(3)* xhat_q(qptr%ij(3))+qptr%wij(4)* xhat_q(qptr%ij(4))&
               +qptr%wij(5)* xhat_q(qptr%ij(5))+qptr%wij(6)* xhat_q(qptr%ij(6))&
               +qptr%wij(7)* xhat_q(qptr%ij(7))+qptr%wij(8)* xhat_q(qptr%ij(8))

           cat_num(k1,n,m)=cat_num(k1,n,m)+one
           trace(k1,n,m)=trace(k1,n,m)+qptr%qpertb*val*err2
           qptr =>  qNode_nextcast(qptr)
        end do

!       Temperature
        !!tptr =>  tNode_typecast(obsLList_headNode(thead(ibin)))
        !anode => obsLList_headNode(thead(ibin))
        !tptr  => tNode_typecast(anode)
        !anode => null()
        tptr  => tNode_typecast(obOper_headNode(iobOper_t,ibin))
        m=2
        do while (associated(tptr))
           n=tptr%kx
           itype=ictype(n)
           n=itype

           if(itype>179 )then
              k1=1
           else
              k1=tptr%k1
           endif

           err2=tptr%raterr2*tptr%err2
!          Forward model
           val= tptr%wij(1)* xhat_t(tptr%ij(1))+tptr%wij(2)* xhat_t(tptr%ij(2))&
               +tptr%wij(3)* xhat_t(tptr%ij(3))+tptr%wij(4)* xhat_t(tptr%ij(4))&
               +tptr%wij(5)* xhat_t(tptr%ij(5))+tptr%wij(6)* xhat_t(tptr%ij(6))&
               +tptr%wij(7)* xhat_t(tptr%ij(7))+tptr%wij(8)* xhat_t(tptr%ij(8))

           cat_num(k1,n,m)=cat_num(k1,n,m)+one
           trace(k1,n,m)=trace(k1,n,m)+tptr%tpertb*val*err2
           tptr =>  tNode_nextcast(tptr)
        end do

!       Surface pressure
        !!psptr => psNode_typecast(obsLList_headNode(pshead(ibin)))
        !anode => obsLList_headNode(pshead(ibin))
        !psptr => psNode_typecast(anode)
        !anode => null()
        psptr  => psNode_typecast(obOper_headNode(iobOper_ps,ibin))
        m=1
        do while (associated(psptr))
           n=psptr%kx
           itype=ictype(n)
           k1=1
           n=itype

           err2=psptr%raterr2*psptr%err2
!          Forward model
           val= psptr%wij(1)* xhat_p(psptr%ij(1))+psptr%wij(2)* xhat_p(psptr%ij(2))&
               +psptr%wij(3)* xhat_p(psptr%ij(3))+psptr%wij(4)* xhat_p(psptr%ij(4))

           cat_num(k1,n,m)=cat_num(k1,n,m)+one
           trace(k1,n,m)=trace(k1,n,m)+psptr%ppertb*val*err2
           psptr => psNode_nextcast(psptr)
        end do
!       Winds
        !!wptr =>  wNode_typecast(obsLList_headNode(whead(ibin)))
        !anode => obsLList_headNode(whead(ibin))
        !wptr  => wNode_typecast(anode)
        !anode => null()
        wptr  => wNode_typecast(obOper_headNode(iobOper_w,ibin))
        m=4
        do while (associated(wptr))
           n=wptr%kx
           itype=ictype(n)
           n=itype

           if(itype > 279)then
              k1=1
           else
              k1=wptr%k1
           endif

           err2=wptr%raterr2*wptr%err2
!          Forward model
           valu= wptr%wij(1)* xhat_u(wptr%ij(1))+wptr%wij(2)* xhat_u(wptr%ij(2))&
                +wptr%wij(3)* xhat_u(wptr%ij(3))+wptr%wij(4)* xhat_u(wptr%ij(4))&
                +wptr%wij(5)* xhat_u(wptr%ij(5))+wptr%wij(6)* xhat_u(wptr%ij(6))&
                +wptr%wij(7)* xhat_u(wptr%ij(7))+wptr%wij(8)* xhat_u(wptr%ij(8))
           valv= wptr%wij(1)* xhat_v(wptr%ij(1))+wptr%wij(2)* xhat_v(wptr%ij(2))&
                +wptr%wij(3)* xhat_v(wptr%ij(3))+wptr%wij(4)* xhat_v(wptr%ij(4))&
                +wptr%wij(5)* xhat_v(wptr%ij(5))+wptr%wij(6)* xhat_v(wptr%ij(6))&
                +wptr%wij(7)* xhat_v(wptr%ij(7))+wptr%wij(8)* xhat_v(wptr%ij(8))

           cat_num(k1,n,m)=cat_num(k1,n,m)+one
           trace(k1,n,m)=trace(k1,n,m)+(wptr%upertb*valu+wptr%vpertb*valv)*err2
           wptr =>  wNode_nextcast(wptr)
        end do

        do m=1,nv
           do n=100,299
              do k=1,np
                 trace(k,n,m)=cat_num(k,n,m)-trace(k,n,m)
              enddo
           enddo
        enddo

     end do ! ibin
     call mpi_reduce(trace,ttrace,size(trace),mpi_rtype,mpi_sum,0, &
          mpi_comm_world,ierror)
     call mpi_reduce(penalty,tpenalty,size(penalty),mpi_rtype,mpi_sum,0, &
          mpi_comm_world,ierror)
     call mpi_reduce(cat_num,tcat_num,size(cat_num),mpi_rtype,mpi_sum,0, &
          mpi_comm_world,ierror)

     if(mype==0)then
        cat_numt=zero
        do m=1,nv
           do i=100,299
              do k=1,np
                 cat_numt(i,m)=cat_numt(i,m)+tcat_num(k,i,m)
              enddo
           enddo
        enddo

        so=one
        do m=1,nv
           do n=100,299
              if(cat_numt(n,m)>zero)then
                 write(333,*)'obs type=',n,obtype(m)
                 do k=1,np
                    if(tcat_num(k,n,m)>3._r_kind .and. ttrace(k,n,m) /= zero )then
                       write(333,*)k,tpenalty(k,n,m),ttrace(k,n,m),int(tcat_num(k,n,m))
                       so(k,n,m)=tpenalty(k,n,m)/ttrace(k,n,m)
                       if(so(k,n,m) >= zero) then
                          so(k,n,m)=sqrt(so(k,n,m))
                          write(334,*)k,n,obtype(m),so(k,n,m),int(tcat_num(k,n,m))
                       endif
                    endif
                 enddo
              endif
           enddo
        enddo

        sosum=zero
        do i=1,ncat
           sosum=sosum+(so(i,1,1)-one)**2
        enddo
        write(335,*)'sosum=',sosum

!       Update etabl
        do l=100,299
           do n=1,nv
              m=n
              if(n==1)m=5
              if(cat_numt(l,n)>zero)then
                 if( (m==3 .and. l<180 )  .or. &
                     (m==2 .and. l<180 ) .or. &
                     (m==4 .and. l<280 )  ) then
                    write(335,*)l,obtype(n),'33',cat_numt(l,n)
                    do k=1,np
                       if( etabl(l,k,m) < 1.e8_r_single) etabl(l,k,m)=etabl(l,k,m)*so(k,l,n)
                    end do
                 else
                    write(335,*)l,obtype(n),'1',cat_numt(l,n)
                    do k=1,np
                       if( etabl(l,k,m) < 1.e8_r_single) etabl(l,k,m)=etabl(l,k,m)*so(1,l,n)
                    end do
                 endif
              endif
           enddo
        enddo

! Write out err table
        open(59,file='errtable_out',form='formatted')
        rewind 59
        do l=100,299
           if(etabl(l,1,1)==1100._r_single)then
              write(59,100)l
              do k=1,np
                 write(59,110)(etabl(l,k,i),i=1,6)
              end do
           endif !  etable1=1100
        end do
        close(59)

     endif ! mype

     call mpi_finalize(ierror)
     stop
  endif ! jiter

100 format(1x,i3,' OBSERVATION TYPE')
110 format(1x,6e12.5)

  return
end subroutine penal
