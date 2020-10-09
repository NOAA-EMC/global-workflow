      subroutine read_nemsio(gfile,im,jm,levs,
     &           VarName,LayName,Varout,iret)
!! This subroutine reads either 2d or 3d nemsio data
!! 12/12/2019 Guang Ping Lou 

      use nemsio_module
      implicit none
      include 'mpif.h'
      type(nemsio_gfile) :: gfile
      character(len=20)  :: VarName,LayName
      integer,intent(in) :: im,jm,levs
      real,intent(out)   :: Varout(im,jm,levs)
      real,dimension(im*jm) :: dum1d
      integer            :: iret,i,j,k,jj

       print*,'read_nemsio,im,jm,levs'
       print*,             im,jm,levs
       print*,'VarName=',trim(VarName)
       print*,'LayName=',trim(LayName)
      if(levs > 1) then
      do k =1, levs
      call nemsio_readrecvw34(gfile,trim(VarName),
     &          trim(LayName),k,data=dum1d,iret=iret)
        !print*,"VarName,k= ",trim(VarName), k
        if (iret /= 0) then
          print*,trim(VarName)," not found"
       else
          do j=1,jm
            jj= (j-1)*im
            do i=1,im
              Varout(i,j,k) = dum1d(jj+i)
            end do
          end do
          end if
        enddo

         else
      call nemsio_readrecvw34(gfile,trim(VarName),
     &          trim(LayName),1,data=dum1d,iret=iret)
        !print*,"VarName= ",trim(VarName)
        if (iret /= 0) then
         print*,trim(VarName)," not found"
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            Varout(i,j,1) = dum1d(jj+i)
          end do
        end do
        endif

      end if

      end subroutine read_nemsio

