!!!@PROCESS NOEXTCHK
subroutine getIVariable(fileName,DateStr,dh,VarName,VarBuff,IM,JSTA_2L,JEND_2U,LM,IM1,JS,JE,LM1)


!     SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    getIVariable    Read data from WRF output
!   PRGRMMR: MIKE BALDWIN    ORG: NSSL/SPC   DATE: 2002-04-08
!
! ABSTRACT:  THIS ROUTINE READS DATA FROM A WRF OUTPUT FILE
!   USING WRF I/O API.
!   .
!
! PROGRAM HISTORY LOG:
! 02-10-31  H CHUANG - MODIFY TO READ WRF INTEGER BINARY OUTPUT
!
! USAGE:    CALL getIVariable(fileName,DateStr,dh,VarName,VarBuff,IM,JSTA_2L,JEND_2U,LM,IM1,JS,JE,LM1)
!
!   INPUT ARGUMENT LIST:
!     fileName : Character(len=256) : name of WRF output file
!     DateStr  : Character(len=19)  : date/time of requested variable
!     dh :  integer                 : data handle
!     VarName :  Character(len=31)  : variable name
!     IM :  integer  : X dimension of data array
!     JSTA_2L :  integer  : start Y dimension of data array
!     JEND_2U :  integer  : end Y dimension of data array
!     LM :  integer  : Z dimension of data array
!     IM1 :  integer  : amount of data pulled in X dimension 
!     JS :  integer  : start Y dimension of amount of data array pulled
!     JE :  integer  : end Y dimension of amount of data array pulled
!     LM1 :  integer  : amount of data pulled in Z dimension
!
!   data is flipped in the Z dimension from what is originally given
!   the code requires the Z dimension to increase with pressure
!
!   OUTPUT ARGUMENT LIST:
!     VarBuff : integer(IM,JSTA_2L:JEND_2U,LM) : requested data array
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       WRF I/O API
!       NETCDF

 ! This subroutine reads the values of the variable named VarName into the buffer
 ! VarBuff. VarBuff is filled with data only for I=1,IM1 and for J=JS,JE
 ! and for L=1,Lm1, presumably this will be
 ! the portion of VarBuff that is needed for this task.

   use wrf_io_flags_mod, only:
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   implicit none
!
   character(len=256) ,intent(in) :: fileName
   character(len=19) ,intent(in) :: DateStr
   integer ,intent(in) :: dh
   character(*) ,intent(in) :: VarName
   integer,intent(out) :: VarBuff(IM,JSTA_2L:JEND_2U,LM)
   integer,intent(in) :: IM,LM,JSTA_2L,JEND_2U
   integer,intent(in) :: IM1,LM1,JS,JE
   integer :: ndim
   integer :: WrfType,i,j,l,ll
   integer, dimension(4) :: start_index, end_index
   character (len= 4) :: staggering
   character (len= 3) :: ordering
   character (len=80), dimension(3) :: dimnames
   integer, allocatable, dimension(:,:,:,:) :: data
!   real, allocatable, dimension(:,:,:,:) :: data
   integer :: ierr
   character(len=132) :: Stagger

   start_index = 1
   end_index = 1
     write(*,*)'fileName,DateStr,dh,VarName in getVariable= ',fileName,DateStr,dh,VarName
   call ext_int_get_var_info(dh,TRIM(VarName),ndim,ordering,Stagger,start_index,end_index,WrfType,ierr)
     write(*,*)'VarName,end_index(1,2,3)= ',VarName,end_index(1),end_index(2),end_index(3)   
   IF ( ierr /= 0 ) THEN
     write(*,*)'Error: ',ierr,TRIM(VarName),' not found in ',fileName
!CHUANG make sure data=0 when not found in wrf output
     data=0.
   VarBuff=0.
     go to 27
   ENDIF
   write(*,*)'WrfType in getIVariable= ',WrfType
!   if( WrfType /= WRF_REAL) then !Ignore if not a real variable
!     write(*,*) 'Error: Not a real variable',WrfType
!     return
!   endif

!  write(*,'(A9,1x,I1,3(1x,I3),1x,A,1x,A)')&
!           trim(VarName), ndim, end_index(1), end_index(2), end_index(3), &
!           trim(ordering), trim(DateStr)
   allocate(data (end_index(1), end_index(2), end_index(3), 1))
   call ext_int_read_field(dh,DateStr,TRIM(VarName),data,WrfType,0,0,0,ordering,&
                             staggering, dimnames , &
                             start_index,end_index, & !dom 
                             start_index,end_index, & !mem
                             start_index,end_index, & !pat
                             ierr)
   IF ( ierr /= 0 ) THEN
     write(*,*)'Error reading ',Varname,' from ',fileName
     write(*,*)' ndim = ', ndim
     write(*,*)' end_index(1) ',end_index(1)
     write(*,*)' end_index(2) ',end_index(2)
     write(*,*)' end_index(3) ',end_index(3)
   ENDIF
!CHUANG: accomendate different array arrangement with different ndim
   if(ndim.eq.1)then
    if(lm1.gt.end_index(1))write(*,*) 'Err:',Varname,' LM1=',lm1,&
                ' but data dim=',end_index(1)
   else if(ndim.eq.2)then
    if (im1.gt.end_index(1)) write(*,*) 'Err:',Varname,' IM1=',im1,&
                ' but data dim=',end_index(1)
    if (je.gt.end_index(2)) write(*,*) 'Err:',Varname,' JE=',je,&
                ' but data dim=',end_index(2)
   else if(ndim.eq.3)then
    if (im1.gt.end_index(1)) write(*,*) 'Err:',Varname,' IM1=',im1,&
                ' but data dim=',end_index(1)
    if (je.gt.end_index(3)) write(*,*) 'Err:',Varname,' JE=',je,&
                ' but data dim=',end_index(3)
    if (lm1.gt.end_index(2)) write(*,*) 'Err:',Varname,' LM1=',lm1,&
                ' but data dim=',end_index(2)
   end if

   if (ndim.gt.3) then
     write(*,*) 'Error: ndim = ',ndim
   endif 

   if (ndim .eq. 0)then
    VarBuff(1,1,1)=data(1,1,1,1)
   else if(ndim .eq. 1)then
    do l=1,lm
      VarBuff(1,1,l)=data(l,1,1,1)
    end do
   else if(ndim .eq. 2)then
    do i=1,im1
      do j=js,je
       VarBuff(i,j,1)=data(i,j,1,1)
      enddo
    enddo
   else if(ndim .eq. 3)then
    do l=1,lm1
     ll=lm1-l+1  ! flip the z axis not sure about soil
     do i=1,im1
      do j=js,je
!hc       VarBuff(i,j,l)=data(i,j,ll,1)
       VarBuff(i,j,l)=data(i,ll,j,1)
      enddo
     enddo
!     write(*,*) Varname,' L ',l,': = ',data(1,1,ll,1)
    enddo
   end if
 27 continue
   deallocate(data)
   return

end subroutine getIVariable
