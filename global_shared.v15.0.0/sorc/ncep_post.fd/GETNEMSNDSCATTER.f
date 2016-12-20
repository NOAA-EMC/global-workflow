      subroutine getnemsandscatter(me,nfile,im,jm,jsta,jsta_2l  &
      ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,VcoordName &
      ,l,impf,jmpf,nframe,buf)
!      
      use nemsio_module, only: nemsio_gfile, nemsio_readrecvw34
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      character(len=20),intent(in) :: VarName,VcoordName
      real,intent(in)    :: spval
      integer,intent(in) :: me,im,jm,jsta_2l,jend_2u,jsta, &
                            MPI_COMM_COMP,l,impf,jmpf,nframe
      integer,intent(in) :: ICNT(0:1023), IDSP(0:1023)
      real,intent(out)   :: buf(im,jsta_2l:jend_2u)
      integer            :: iret,i,j,idiff,jj
      real dummy(im,jm)
!     real dummy2(impf,jmpf)
      real, allocatable:: dum1d(:)
      
      if(me == 0) then
!        nframe=nframed2*2
        allocate(dum1d((impf)*(jmpf)))
        idiff = (impf-im)/2
        call nemsio_readrecvw34(nfile,trim(VarName)                      &
            ,trim(VcoordName),l,data=dum1d,nframe=nframe,iret=iret)
!	if(trim(VarName)=='tmp')print*,'in getnems debug: ',impf,jmpf, &
!	nframe,trim(VarName),trim(VcoordName),l	 
        if (iret /= 0) then
          print*,VarName,VcoordName,l," not found in NEMS file-Assigned missing values"
!$omp parallel do private(i,j)
          do j=1,jm
            do i=1,im
              dummy(i,j) = spval
            enddo
          enddo
        else 
!$omp parallel do private(i,j,jj)
          do j=1,jm
            jj= (j-1)*impf + idiff
            do i=1,im
              dummy(i,j) = dum1d(jj+i)
              if(dummy(i,j) >= 9.9E20) dummy(i,j) = spval

!	      if(trim(VarName)=='tmp' .and. j==72)print*,  &
!	      'T before scatter',i,j,dummy(i,j) 
! 	        dummy(i,j)=dummy2(i+nframed2,j+nframed2)

            end do
          end do
        end if
        deallocate(dum1d)
      end if

      call mpi_scatterv(dummy(1,1),icnt,idsp,mpi_real                     &
                       ,buf(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)
      
      RETURN
      END
