      subroutine getgbandscatter(me,iunit,im,jm,im_jm,jsta,jsta_2l        &
      ,jend_2u,MPI_COMM_COMP,icnt,idsp,spval,VarName,jpds,jgds,kpds,buf)
!      
      implicit none
      INCLUDE "mpif.h"
!
      character(len=20),intent(in) :: VarName
      real,intent(in)       :: spval
      integer,intent(in)    :: me,iunit,im,jm,im_jm,jsta_2l,jend_2u,jsta, &
                               MPI_COMM_COMP
      integer,intent(in)    :: ICNT(0:1023), IDSP(0:1023)
      integer,intent(in)    :: JPDS(200),JGDS(200)
      integer,intent(inout) :: KPDS(200)
      real,intent(out)      :: buf(im,jsta_2l:jend_2u)
      integer               :: kf,k,iret,i,j
      integer KGDS(200) 
      LOGICAL*1 LB(IM,JM)
      real dummy(im,jm)
      
      if(me == 0) then
        call getgb(iunit,0,im_jm,0,jpds,jgds,kf &
                  ,k,kpds,kgds,lb,dummy,iret)
        if (iret /= 0) then
          print*,VarName," not found in file-Assigned missing values"
!$omp parallel do private(i,j)
          do j=1,jm
            do i=1,im
              dummy(i,j) = spval
            end do
          end do
        else
!$omp parallel do private(i,j)
          do j=1,jm
            do i=1,im    
              if(.not.lb(i,j)) dummy(i,j) = spval
            end do
          end do 
        end if
      end if
                                                                                          
      call mpi_scatterv(dummy(1,1),icnt,idsp,mpi_real                     &
                       ,buf(1,jsta),icnt(me),mpi_real,0,MPI_COMM_COMP,iret)
      
       RETURN
       END    
