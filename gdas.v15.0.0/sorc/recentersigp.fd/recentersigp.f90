program recentersigp
!$$$  main program documentation block
!
! program:  recentersigp               recenter
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  Read NCEP GFS spectral sigma file from a file, 
!            remove mean specified from another file, add a 
!            new mean specified from a third file, and write
!            out result to a fourth file.
!
! program history log:
!   2009-02-23  Initial version.
!
! usage:
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!
!$$$

  USE SIGIO_MODULE
  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrec,&
       nemsio_writerec,nemsio_readrecv,nemsio_writerecv,nemsio_getrechead

  implicit none

  include "mpif.h"

  real,parameter:: zero=0.0_4

  TYPE(SIGIO_HEAD) :: SIGHEADI,SIGHEADO,SIGHEADMI,SIGHEADMO
  TYPE(SIGIO_DATA) :: SIGDATAI,SIGDATAO,SIGDATAMI,SIGDATAMO
  logical:: nemsio, sigio
  character*500 filename_meani,filename_meano,filenamein,filenameout
  character*3 charnanal
  character(len=4) charnin
  character(16),dimension(:),allocatable:: fieldname_di,fieldname_mi,fieldname_mo
  character(16),dimension(:),allocatable:: fieldlevtyp_di,fieldlevtyp_mi,fieldlevtyp_mo
  integer,dimension(:),allocatable:: fieldlevel_di,fieldlevel_mi,fieldlevel_mo,orderdi,ordermi
  integer nsigi,nsigo,iret,mype,mype1,npe,nanals,ierr
  integer:: nrec,latb,lonb,levs,npts,n,i,k,nn
  real,allocatable,dimension(:):: rwork1d
  real,allocatable,dimension(:,:)   :: rwork1di,rwork1do,rwork1dmi,rwork1dmo

  type(nemsio_gfile) :: gfilei, gfileo, gfilemi, gfilemo

! Initialize mpi
  call MPI_Init(ierr)

! mype is process number, npe is total number of processes.
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,ierr)

  if (mype==0) call w3tagb('RECENTERSIGP',2011,0319,0055,'NP25')
  
  NSIGI=21
  NSIGO=61

! read data from this file
  call getarg(1,filenamein)

! subtract this mean
  call getarg(2,filename_meani)

! then add to this mean
  call getarg(3,filename_meano)

! and put in this file.
  call getarg(4,filenameout)

! how many ensemble members to process
  call getarg(5,charnin)
  read(charnin,'(i4)') nanals
  
  if (mype==0) then
     write(6,*)'RECENTERSIGP:  PROCESS ',nanals,' ENSEMBLE MEMBERS'
     write(6,*)'filenamein=',trim(filenamein)
     write(6,*)'filename_meani=',trim(filename_meani)
     write(6,*)'filename_meano=',trim(filename_meano)
     write(6,*)'filenameout=',trim(filenameout)
  endif

  sigio=.false.
  nemsio=.false.

  mype1 = mype+1
  if (mype1 <= nanals) then
     call nemsio_init(iret)
     call sigio_srohdc(nsigi,trim(filename_meani),  &
          sigheadmi,sigdatami,iret)
     if (iret == 0 ) then
        sigio = .true.
        write(6,*)'Read sigio ',trim(filename_meani),' iret=',iret
     else
        call nemsio_open(gfilemi,trim(filename_meani),'READ',iret)
        if (iret == 0 ) then
           nemsio = .true.
           write(6,*)'Read nemsio ',trim(filename_meani),' iret=',iret
           call nemsio_getfilehead(gfilemi, nrec=nrec, dimx=lonb, dimy=latb, dimz=levs, iret=iret)
           write(6,*)' lonb=',lonb,' latb=',latb,' levs=',levs,' nrec=',nrec
        else
           write(6,*)'***ERROR*** ',trim(filenamein),' contains unrecognized format.  ABORT'
        endif
     endif
     if (.not.nemsio .and. .not.sigio) goto 100
     if (mype==0) write(6,*)'processing files with nemsio=',nemsio,' sigio=',sigio


     if (sigio) then
        call sigio_srohdc(nsigi,trim(filename_meano),  &
             sigheadmo,sigdatamo,iret)
        write(charnanal,'(i3.3)') mype1
        call sigio_srohdc(nsigi,trim(filenamein)//"_mem"//charnanal,  &
             sigheadi,sigdatai,iret)
        sigheado = sigheadmo 
        call sigio_aldata(sigheado,sigdatao,iret)
        
        sigdatao%hs = sigdatai%hs
        sigdatao%ps = sigdatai%ps - sigdatami%ps + sigdatamo%ps
        sigdatao%t = sigdatai%t - sigdatami%t + sigdatamo%t
        sigdatao%z = sigdatai%z - sigdatami%z + sigdatamo%z
        sigdatao%d = sigdatai%d - sigdatami%d + sigdatamo%d
        sigdatao%q = sigdatai%q - sigdatami%q + sigdatamo%q
        
        call sigio_swohdc(nsigo,trim(filenameout)//"_mem"//charnanal,sigheado,sigdatao,iret)
        write(6,*)'task mype=',mype,' process ',trim(filenameout)//"_mem"//charnanal,' iret=',iret
     
     elseif (nemsio) then
        call nemsio_open(gfilemo,trim(filename_meano),'READ',iret)
        write(charnanal,'(i3.3)') mype1
        call nemsio_open(gfilei,trim(filenamein)//"_mem"//charnanal,'READ',iret)

        gfileo=gfilemo
        call nemsio_open(gfileo,trim(filenameout)//"_mem"//charnanal,'WRITE',iret)
        
        npts=lonb*latb
        allocate(rwork1di(npts,nrec))
        allocate(rwork1dmi(npts,nrec))
        allocate(rwork1dmo(npts,nrec))
        allocate(rwork1do(npts,nrec))

        allocate(fieldname_di(nrec), fieldlevtyp_di(nrec),fieldlevel_di(nrec))
        allocate(fieldname_mi(nrec),fieldlevtyp_mi(nrec),fieldlevel_mi(nrec))
        allocate(fieldname_mo(nrec),fieldlevtyp_mo(nrec),fieldlevel_mo(nrec))
        allocate(orderdi(nrec),ordermi(nrec))

        do n=1,nrec
           call nemsio_readrec(gfilei, n,rwork1di(:,n),iret) ! member analysis
           call nemsio_getrechead(gfilei,n,fieldname_di(n),fieldlevtyp_di(n),fieldlevel_di(n),iret)
        end do
        do n=1,nrec
           call nemsio_readrec(gfilemi,n,rwork1dmi(:,n),iret) ! ensemble mean analysis
           call nemsio_getrechead(gfilemi,n,fieldname_mi(n),fieldlevtyp_mi(n),fieldlevel_mi(n),iret)
        end do
        do n=1,nrec
           call nemsio_readrec(gfilemo,n,rwork1dmo(:,n),iret) ! chgres hi-res analysis
           call nemsio_getrechead(gfilemo,n,fieldname_mo(n),fieldlevtyp_mo(n),fieldlevel_mo(n),iret)
        end do


        call getorder(fieldname_mo,fieldname_di,fieldlevtyp_mo,fieldlevtyp_di,fieldlevel_mo,fieldlevel_di,nrec,orderdi)
        call getorder(fieldname_mo,fieldname_mi,fieldlevtyp_mo,fieldlevtyp_mi,fieldlevel_mo,fieldlevel_mi,nrec,ordermi)

!       Recenter ensemble member about chgres hi-res analysis
        do n=1,nrec
           do i=1,npts
              rwork1do(i,n) = rwork1di(i,orderdi(n)) - rwork1dmi(i,ordermi(n)) + rwork1dmo(i,n)
           end do
        end do

!       Write recentered member analysis using ordering of chgres hi-res analysis fields
        do n=1,nrec
           call nemsio_writerec(gfileo,n,rwork1do(:,n),iret)
        end do
        deallocate(rwork1di)
        deallocate(rwork1dmi)
        deallocate(rwork1dmo)
        deallocate(rwork1do)

        call nemsio_close(gfilemi,iret)
        call nemsio_close(gfilemo,iret)

!       Preserve orography
        allocate(rwork1d(npts))
        rwork1d=zero
        call nemsio_readrecv(gfilei,'hgt','sfc',1,rwork1d,iret)
        call nemsio_writerecv(gfileo,'hgt','sfc',1,rwork1d,iret)
        deallocate(rwork1d)

        call nemsio_close(gfilei,iret)
        call nemsio_close(gfileo,iret)
        write(6,*)'task mype=',mype,' process ',trim(filenameout)//"_mem"//charnanal,' iret=',iret
           
     endif

! Jump here if more mpi processors than files to process
  else
     write (6,*) 'no files to process for mpi task = ',mype
  end if  ! end if mype

100 continue
  call MPI_Barrier(MPI_COMM_WORLD,ierr)

  if (mype1 <= nanals .and. .not.nemsio .and. .not.sigio) then
     call MPI_Abort(MPI_COMM_WORLD,98,iret)
     stop
  endif

  if (mype==0) call w3tage('RECENTERSIGP')

  call MPI_Finalize(ierr)
  if (mype .eq. 0 .and. ierr .ne. 0) then
     print *, 'MPI_Finalize error status = ',ierr
  end if

END program recentersigp

subroutine getorder(flnm1,flnm2,fllevtyp1,fllevtyp2,fllev1,fllev2,nrec,order)
  integer nrec
  character(16):: flnm1(nrec),flnm2(nrec),fllevtyp1(nrec),fllevtyp2(nrec)
  integer ::  fllev1(nrec),fllev2(nrec)
  integer, intent(out) ::  order(nrec)
  
  integer i,j
  
  order=0
  do i=1,nrec
     doloopj: do j=1,nrec
        if(flnm1(i)==flnm2(j).and.fllevtyp1(i)==fllevtyp2(j).and.fllev1(i)==fllev2(j)) then
           order(i)=j
           exit doloopj
        endif
     enddo doloopj
  enddo
  
end subroutine getorder
