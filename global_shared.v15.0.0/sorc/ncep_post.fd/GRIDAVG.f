      subroutine H2U(ingrid,outgrid)
! This subroutine interpolates from H points onto U points
! Author: CHUANG, EMC, Dec. 2010

      use ctlblk_mod, only: spval, jsta, jend, jsta_m, jend, me, num_procs, jm,&
              im, jsta_2l, jend_2u , jend_m
      use gridspec_mod, only: gridtype

      implicit none

      INCLUDE "mpif.h"
      integer:: i,j,ie,iw
      real,dimension(IM,JSTA_2L:JEND_2U),intent(in)::ingrid
      real,dimension(IM,JSTA_2L:JEND_2U),intent(out)::outgrid
      outgrid=spval
      if(GRIDTYPE == 'A')THEN
       do j=jsta,jend
        do i=1,im
	 outgrid(i,j)=ingrid(i,j)
	end do
       end do
      else IF(GRIDTYPE == 'E')THEN
       call exch(ingrid(1,jsta_2l))
       DO J=JSTA_M,JEND_M
        DO I=2,IM-1
	 IE=I+MOD(J,2)
         IW=IE-1
         outgrid(i,j)=(ingrid(IW,J)+ingrid(IE,J)+ingrid(I,J+1)+ingrid(I,J-1))/4.0
	end do
       end do
      ELSE IF(GRIDTYPE == 'B')THEN
       call exch(ingrid(1,jsta_2l))
       DO J=JSTA,JEND_M
        DO I=1,IM-1
	 outgrid(i,j)=(ingrid(i,j)+ingrid(i,j+1)+ingrid(i+1,j)+ingrid(i+1,j+1))/4.0
	end do
       end do
! Fill in boundary points because hysplit fails when 10 m wind has bitmaps
       do j=jsta,jend_m
        outgrid(im,j)=outgrid(im-1,j)
       end do
       IF(me == (num_procs-1) .and. jend_2u >= jm) then
        DO I=1,IM
         outgrid(i,jm) = outgrid(i,jm-1)
        END DO
       END IF      
      ELSE IF(GRIDTYPE == 'C')THEN
       DO J=JSTA,JEND
        DO I=1,IM-1
          outgrid(i,j)=(ingrid(i,j)+ingrid(i+1,j))/2.0
        end do
       end do
      end if 
      	 
      return 
      end
	
      subroutine H2V(ingrid,outgrid)
! This subroutine interpolates from H points onto V points
! Author: CHUANG, EMC, Dec. 2010
      use ctlblk_mod, only: spval, jsta, jend, jsta_m, jend_m, im, jsta_2l, jend_2u
      use gridspec_mod, only: gridtype
      implicit none
      INCLUDE "mpif.h"
      integer:: i,j,ie,iw
      real,dimension(IM,JSTA_2L:JEND_2U),intent(in)::ingrid
      real,dimension(IM,JSTA_2L:JEND_2U),intent(out)::outgrid
      outgrid=spval
      if(GRIDTYPE == 'A')THEN
       do j=jsta,jend
        do i=1,im
          outgrid(i,j)=ingrid(i,j)
        end do
       end do
      else IF(GRIDTYPE == 'E')THEN
       call exch(ingrid(1,jsta_2l))
       DO J=JSTA_M,JEND_M
        DO I=2,IM-1
	 IE=I+MOD(J,2)
         IW=IE-1
         outgrid(i,j)=(ingrid(IW,J)+ingrid(IE,J)+ingrid(I,J+1)+ingrid(I,J-1))/4.0
	end do
       end do
      ELSE IF(GRIDTYPE == 'B')THEN
       call exch(ingrid(1,jsta_2l))
       DO J=JSTA,JEND_M
        DO I=1,IM-1
	 outgrid(i,j)=(ingrid(i,j)+ingrid(i,j+1)+ingrid(i+1,j)+ingrid(i+1,j+1))/4.0
	end do
       end do
      ELSE IF(GRIDTYPE == 'C')THEN
       call exch(ingrid(1,jsta_2l))
       DO J=JSTA,JEND_M
        DO I=1,IM
	 outgrid(i,j)=(ingrid(i,j)+ingrid(i,j+1))/2.0
	end do
       end do 
      end if 
      
      return 
      end

      subroutine U2H(ingrid,outgrid)
! This subroutine interpolates from U points onto H points
! Author: CHUANG, EMC, Dec. 2010
      use ctlblk_mod, only: spval, jsta, jend, jsta_m, jend_m, im, jsta_2l, jend_2u
      use gridspec_mod, only: gridtype
      implicit none
      INCLUDE "mpif.h"
      integer:: i,j,ie,iw
      real,dimension(IM,JSTA_2L:JEND_2U),intent(in)::ingrid
      real,dimension(IM,JSTA_2L:JEND_2U),intent(out)::outgrid
      outgrid=spval
      if(GRIDTYPE == 'A')THEN
       do j=jsta,jend
        do i=1,im
	 outgrid(i,j)=ingrid(i,j)
	end do
       end do
      else IF(GRIDTYPE == 'E')THEN
       call exch(ingrid(1,jsta_2l))
       DO J=JSTA_M,JEND_M
        DO I=2,IM-1
	 IE=I+MOD(J+1,2)
         IW=IE-1
         outgrid(i,j)=(ingrid(IW,J)+ingrid(IE,J)+ingrid(I,J+1)+ingrid(I,J-1))/4.0
	end do
       end do
      ELSE IF(GRIDTYPE == 'B')THEN
       call exch(ingrid(1,jsta_2l))
       DO J=JSTA_M,JEND_M
        DO I=2,IM-1
	 outgrid(i,j)=(ingrid(i-1,j-1)+ingrid(i,j-1)+ingrid(i-1,j)+ingrid(i,j))/4.0
	end do
       end do
      ELSE IF(GRIDTYPE == 'C')THEN
       DO J=JSTA,JEND
        DO I=2,IM
	 outgrid(i,j)=(ingrid(i-1,j)+ingrid(i,j))/2.0
	end do
       end do       
      end if 
      	 
      return 
      end        
      	 
      subroutine V2H(ingrid,outgrid)
! This subroutine interpolates from V points onto H points
! Author: CHUANG, EMC, Dec. 2010
      use ctlblk_mod, only: spval, jsta, jend, jsta_m, jend_m, im, jsta_2l, jend_2u
      use gridspec_mod, only: gridtype
      implicit none
      INCLUDE "mpif.h"
      integer:: i,j,ie,iw
      real,dimension(IM,JSTA_2L:JEND_2U),intent(in)::ingrid
      real,dimension(IM,JSTA_2L:JEND_2U),intent(out)::outgrid
      outgrid=spval
      if(GRIDTYPE == 'A')THEN
       do j=jsta,jend
        do i=1,im
	 outgrid(i,j)=ingrid(i,j)
	end do
       end do
      else IF(GRIDTYPE == 'E')THEN
       call exch(ingrid(1,jsta_2l))
       DO J=JSTA_M,JEND_M
        DO I=2,IM-1
	 IE=I+MOD(J,2)
         IW=IE-1
         outgrid(i,j)=(ingrid(IW,J)+ingrid(IE,J)+ingrid(I,J+1)+ingrid(I,J-1))/4.0
	end do
       end do
      ELSE IF(GRIDTYPE == 'B')THEN
       call exch(ingrid(1,jsta_2l))
       DO J=JSTA_M,JEND_M
        DO I=2,IM-1
	 outgrid(i,j)=(ingrid(i-1,j-1)+ingrid(i,j-1)+ingrid(i-1,j)+ingrid(i,j))/4.0
	end do
       end do
      ELSE IF(GRIDTYPE == 'C')THEN
       call exch(ingrid(1,jsta_2l))
       DO J=JSTA_M,JEND
        DO I=1,IM
	 outgrid(i,j)=(ingrid(i,j-1)+ingrid(i,j))/2.0
	end do
       end do 
      end if 
      	 
      return 
      end	 	         
        
