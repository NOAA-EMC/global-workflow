      subroutine read_netcdf(ncid,im,jm,levs,
     &   VarName,Varout,Zreverse,iret)
!! This subroutine reads either 2d or 3d NetCDF data
!! 12/12/2019 Guang Ping Lou 

      use netcdf
      implicit none
      include 'mpif.h'
      character(len=20),intent(in) :: VarName
      character(len=3),intent(in) :: Zreverse
      integer,intent(in) :: ncid,im,jm,levs
      real,intent(out)   :: Varout(im,jm,levs)
      real               :: dummy3d(im,jm,levs)
      integer            :: iret,i,j,k,id_var,kk

      if(levs > 1) then
        iret = nf90_inq_varid(ncid,trim(VarName),id_var)
        !print*,stat,varname,id_var
        iret = nf90_get_var(ncid,id_var,dummy3d)
        if (iret /= 0) then
          print*,VarName," not found"
       else
!For FV3GFS NetCDF output, vertical layers need to be reversed
      if(Zreverse == "yes" ) then
            do k = 1, levs
             kk=levs-k+1
             do j=1, jm
              do i=1, im
               Varout(i,j,k) = dummy3d(i,j,kk)
              enddo
             enddo
            enddo
        else
            do k = 1, levs
             do j=1, jm
              do i=1, im
               Varout(i,j,k) = dummy3d(i,j,k)
              enddo
             enddo
            enddo
         endif
        endif

         else
        iret = nf90_inq_varid(ncid,trim(VarName),id_var)
        !print*,stat,varname,id_var
        iret = nf90_get_var(ncid,id_var,Varout(:,:,1))
        if (iret /= 0) then
          print*,VarName," not found"
        endif

      end if

      end subroutine read_netcdf

