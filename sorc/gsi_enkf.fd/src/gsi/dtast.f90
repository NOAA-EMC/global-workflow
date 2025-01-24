subroutine dtast(work1,nlev,pbot,ptop,mesage,jiter,iout,pflag)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dtast       print table of scalar innovations
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: print table of scalar innovations by data type.
!
! program history log:
!   1990-10-11  parrish
!   1998-04-05  weiyu yang
!   2004-06-15  treadon - reformat documenation
!   2004-08-25  derber - remove hardwire of ntype and add intent
!   2004-10-12  parrish - modifications for nonlinear qc
!   2005-07-27  derber  - add print of monitoring and reject data
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-03  derber  - modify to print individual ob types
!   2008-06-04  safford - rm unused var
!   2016-12-20  pondeca - adjust output format for vis & cldch
!
!   input argument list:
!     work1    - array containing innovation (o-g) sums
!     nlev     - number of pressure levels
!     pbot     - pressure at bottom of layer
!     ptop     - pressure at top of layer
!     mesage   - message to appear at top of table ($ signals end)
!     jiter    - external iteration
!     iout     - unit to which to write statistics
!     pflag    - flag whether to use this data
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use convinfo, only: nconvtype,ictype,icsubtype,ioctype
  use qcmod, only: npres_print
  implicit none
   
  integer(i_kind)                                     ,intent(in   ) :: iout,nlev,jiter
  real(r_kind)   ,dimension(npres_print,nconvtype,5,3),intent(in   ) :: work1
  real(r_kind)   ,dimension(nlev)                     ,intent(in   ) :: pbot,ptop
  logical        ,dimension(nconvtype)                ,intent(in   ) :: pflag
  character(100)                                      ,intent(in   ) :: mesage

  character(3),dimension(3):: typx
  integer(i_kind) i,ilin,j,imsg,k,nn
  integer(i_kind),dimension(nlev):: countall
  integer(i_kind),dimension(nlev,nconvtype,3):: count
  real(r_kind),dimension(nlev):: rmsall,biasall,ratall,qcratall
  real(r_kind),dimension(nlev):: rmsx,biasx,ratx,qcratx
  real(r_kind),dimension(nlev,nconvtype,3):: rms,bias,rat,qcrat
  logical vis_or_cldch 

  ! Initialize variables
  count=0; rms=zero; bias=zero; rat=zero; qcrat=zero 
  typx(1)='asm' ! used or assimilated
  typx(2)='rej' ! rejected
  typx(3)='mon' ! monitored

  ! First, print message and level information

  imsg=max(1,index(mesage,'$')-1)
  ilin=max(imsg,min(nlev*11+34,240))
  write(iout,'(a)') mesage(1:imsg)
  if (nlev > 1) then
     write(iout,800) '',  '',   '',   '',   '',    'ptop',(ptop(k),k=1,nlev)
     write(iout,800) 'it','obs','use','typ','styp','pbot',(pbot(k),k=1,nlev)
  end if
  write(iout,'(240(a1))') ('-',i=1,ilin)

  ! Transfer to local work arrays.  Compute statistics
  do j = 1,nconvtype
     if(pflag(j))then
        do i = 1,nlev
           count(i,j,1) = nint(work1(i,j,1,1))    ! data count used
           count(i,j,2) = nint(work1(i,j,1,2))    ! data count rejected
           count(i,j,3) = nint(work1(i,j,1,3))    ! data count monitored
           if(count(i,j,1) > 0)then
              bias(i,j,1)  = work1(i,j,2,1)        ! bias used
              rms(i,j,1)   = work1(i,j,3,1)        ! rms used
              rat(i,j,1)   = work1(i,j,4,1)        ! penalty used
              qcrat(i,j,1) = work1(i,j,5,1)        ! nonlin qc penalty used
           end if
           if(count(i,j,2) > 0)then
              bias(i,j,2)  = work1(i,j,2,2)        ! bias rejected
              rms(i,j,2)   = work1(i,j,3,2)        ! rms rejected
              rat(i,j,2)   = work1(i,j,4,2)        ! penalty rejected
              qcrat(i,j,2) = work1(i,j,5,2)        ! nonlin qc penalty rejected
           end if
           if(count(i,j,3) > 0)then
              bias(i,j,3)  = work1(i,j,2,3)        ! bias monitored
              rms(i,j,3)   = work1(i,j,3,3)        ! rms monitored
              rat(i,j,3)   = work1(i,j,4,3)        ! penalty monitored
              qcrat(i,j,3) = work1(i,j,5,3)        ! nonlin qc penalty monitored
           end if
        end do
     end if
  end do

  ! Print statistics for single level obs (e.g., surface pressure)     
  if (nlev == 1) then

     vis_or_cldch=.false.

     write(iout,600) ptop(1),pbot(1)
     write(iout,700) 'it','obs','use','typ','styp','count','bias','rms','cpen','qcpen'
     do nn=1,3
        countall(1)=0
        biasall(1)=zero
        rmsall(1)=zero
        ratall(1)=zero
        qcratall(1)=zero
        do i=1,nconvtype
           if(pflag(i) .and. count(1,i,nn) > 0)then
              biasx(1)=bias(1,i,nn)/count(1,i,nn)
              rmsx(1)=sqrt(rms(1,i,nn)/count(1,i,nn))
              ratx(1)=rat(1,i,nn)/count(1,i,nn)
              qcratx(1)=qcrat(1,i,nn)/count(1,i,nn)
              countall(1)=countall(1)+count(1,i,nn)
              biasall(1)=biasall(1)+bias(1,i,nn)
              rmsall(1)=rmsall(1)+rms(1,i,nn)
              ratall(1)=ratall(1)+rat(1,i,nn)
              qcratall(1)=qcratall(1)+qcrat(1,i,nn)
              if (trim(ioctype(i))=='vis' .or. trim(ioctype(i))=='cldch') then
                 write(iout,901) jiter,trim(ioctype(i)),typx(nn),ictype(i),icsubtype(i),count(nlev,i,nn),biasx(1),rmsx(1),ratx(1),qcratx(1)
                 vis_or_cldch=.true.
                else
                 write(iout,701) jiter,trim(ioctype(i)),typx(nn),ictype(i),icsubtype(i),count(nlev,i,nn),biasx(1),rmsx(1),ratx(1),qcratx(1)
              endif
           end if
        end do
        if(countall(1) > 0)then
           biasx(1)=biasall(1)/countall(1)
           rmsx(1)=sqrt(rmsall(1)/countall(1))
           ratx(1)=ratall(1)/countall(1)
           qcratx(1)=qcratall(1)/countall(1)
           if (vis_or_cldch) then
              write(iout,902) jiter,'',typx(nn),'all','',countall(1),biasx(1),rmsx(1),ratx(1),qcratx(1)
             else
              write(iout,702) jiter,'',typx(nn),'all','',countall(1),biasx(1),rmsx(1),ratx(1),qcratx(1)
           endif
        end if
     end do

  ! Print statistics for multi-level obs
  else ! if ( nlev == 1 )

     do nn=1,3
        countall=0
        biasall=zero
        rmsall=zero
        ratall=zero
        qcratall=zero
        do i = 1,nconvtype
           if(pflag(i) .and. count(nlev,i,nn) > 0)then
              biasx=zero
              rmsx=zero
              ratx=zero
              qcratx=zero
              do k=1,nlev
                 if(count(k,i,nn) > 0)then
                    biasx(k)=bias(k,i,nn)/count(k,i,nn)
                    rmsx(k)=sqrt(rms(k,i,nn)/count(k,i,nn))
                    ratx(k)=rat(k,i,nn)/count(k,i,nn)
                    qcratx(k)=qcrat(k,i,nn)/count(k,i,nn)
                    countall(k)=countall(k)+count(k,i,nn)
                    biasall(k)=biasall(k)+bias(k,i,nn)
                    rmsall(k)=rmsall(k)+rms(k,i,nn)
                    ratall(k)=ratall(k)+rat(k,i,nn)
                    qcratall(k)=qcratall(k)+qcrat(k,i,nn)
                 end if
              end do
              write(iout,801) jiter,trim(ioctype(i)),typx(nn),ictype(i),icsubtype(i),'count',(count(k,i,nn),k=1,nlev)
              write(iout,802) jiter,trim(ioctype(i)),typx(nn),ictype(i),icsubtype(i),'bias', (biasx(k),     k=1,nlev)
              write(iout,802) jiter,trim(ioctype(i)),typx(nn),ictype(i),icsubtype(i),'rms',  (rmsx(k),      k=1,nlev)
              write(iout,802) jiter,trim(ioctype(i)),typx(nn),ictype(i),icsubtype(i),'cpen', (ratx(k),      k=1,nlev)
              write(iout,802) jiter,trim(ioctype(i)),typx(nn),ictype(i),icsubtype(i),'qcpen',(qcratx(k),    k=1,nlev)
           end if
        end do
        if(countall(nlev) > 0)then
           biasx=zero
           rmsx=zero
           ratx=zero
           qcratx=zero
           do k=1,nlev
              if(countall(k) > 0)then
                 biasx(k)=biasall(k)/countall(k)
                 rmsx(k)=sqrt(rmsall(k)/countall(k))
                 ratx(k)=ratall(k)/countall(k)
                 qcratx(k)=qcratall(k)/countall(k)
              end if
           end do
           write(iout,803) jiter,'',typx(nn),'all','','count',(countall(k),k=1,nlev)
           write(iout,804) jiter,'',typx(nn),'all','','bias', (biasx(k),   k=1,nlev)
           write(iout,804) jiter,'',typx(nn),'all','','rms',  (rmsx(k),    k=1,nlev)
           write(iout,804) jiter,'',typx(nn),'all','','cpen', (ratx(k),    k=1,nlev)
           write(iout,804) jiter,'',typx(nn),'all','','qcpen',(qcratx(k),  k=1,nlev)
        end if
     end do

  endif ! if ( nlev == 1 )

600  format(1x,'pressure levels (hPa)=',f6.1,1x,f6.1)
700  format(1x,'o-g',1x,a2,  1x,a7,1x,a3,1x,a3,  1x,a4,  1x,a9,1x, 4(a10,  1x))
701  format(1x,'o-g',1x,i2.2,1x,a7,1x,a3,1x,i3.3,1x,i4.4,1x,i9,1x, 4(e10.3,1x))
702  format(1x,'o-g',1x,i2.2,1x,a7,1x,a3,1x,a3,  1x,a4,  1x,i9,1x, 4(e10.3,1x))
800  format(1x,'o-g',1x,a2,  1x,a7,1x,a3,1x,a3,  1x,a4,  1x,a5,1x,12(e10.3,1x))
801  format(1x,'o-g',1x,i2.2,1x,a7,1x,a3,1x,i3.3,1x,i4.4,1x,a5,1x,12(i10,  1x))
802  format(1x,'o-g',1x,i2.2,1x,a7,1x,a3,1x,i3.3,1x,i4.4,1x,a5,1x,12(e10.3,1x))
803  format(1x,'o-g',1x,i2.2,1x,a7,1x,a3,1x,a3,  1x,a4,  1x,a5,1x,12(i10,  1x))
804  format(1x,'o-g',1x,i2.2,1x,a7,1x,a3,1x,a3,  1x,a4,  1x,a5,1x,12(e10.3,1x))
901  format(1x,'o-g',1x,i2.2,1x,a7,1x,a3,1x,i3.3,1x,i4.4,1x,i9,1x, 2(e10.3,1x),2(e10.3,1x))
902  format(1x,'o-g',1x,i2.2,1x,a7,1x,a3,1x,a3,  1x,a4,  1x,i9,1x, 2(e10.3,1x),2(e10.3,1x))

  return
end subroutine dtast
