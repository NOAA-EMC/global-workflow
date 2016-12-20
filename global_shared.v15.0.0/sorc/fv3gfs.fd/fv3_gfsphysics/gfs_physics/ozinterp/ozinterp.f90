      subroutine read_o3data (lozp, pl_c)
      use machine,    only: kind_phys
      use mpp_mod,    only: get_unit, mpp_pe, mpp_root_pe
      use fms_io_mod, only: close_file
      use ozne_def
!--- in/out
      integer, intent(inout) :: lozp, pl_c
!--- locals
      integer :: i, n, k
      real(kind=4), allocatable, dimension(:) :: pl_lat4, pl_pres4
      real(kind=4), allocatable, dimension(:) :: pl_time4, tempin

      kozpl = get_unit()
      open(unit=kozpl,file='INPUT/global_o3prdlos.f77', form='unformatted', convert='big_endian')

!--- read in indices
!---
      read (kozpl) pl_coeff, latsozp, levozp, timeoz
      if (mpp_pe() == mpp_root_pe()) then
        write(*,*) 'Reading in o3data from global_o3prdlos.f77 '
        write(*,*) '      pl_coeff = ', pl_coeff
        write(*,*) '       latsozp = ', latsozp
        write(*,*) '        levozp = ', levozp
        write(*,*) '        timeoz = ', timeoz
      endif

!--- read in data
!---   pl_lat   -  latitude of data        (-90 to 90)
!---   pl_pres  -  vertical pressure level (mb)
!---   pl_time  -  time coordinate         (days)
!---
      allocate (pl_lat(latsozp), pl_pres(levozp),pl_time(timeoz+1))
      allocate (pl_lat4(latsozp), pl_pres4(levozp),pl_time4(timeoz+1))
      rewind (kozpl)
      read (kozpl) pl_coeff, latsozp, levozp, timeoz, pl_lat4, pl_pres4, pl_time4
      pl_pres(:) = pl_pres4(:)
!---  convert pressure levels from mb to ln(Pa)
      pl_pres(:) = log(100.0*pl_pres(:))
      pl_lat(:)  = pl_lat4(:)
      pl_time(:) = pl_time4(:)
      deallocate (pl_lat4, pl_pres4, pl_time4)

!--- read in ozplin which is in order of (lattitudes, ozone levels, coeff number, time)
!--- assume latitudes is on a uniform gaussian grid
!---
      allocate (tempin(latsozp))
      allocate (ozplin(latsozp,levozp,pl_coeff,timeoz))
      DO i=1,timeoz
        do n=1,pl_coeff
          DO k=1,levozp
            READ(kozpl) tempin
            ozplin(:,k,n,i) = tempin(:)
          ENDDO
        enddo
      ENDDO
      deallocate (tempin)

      call close_file(kozpl)

      pl_c = pl_coeff
      lozp = levozp

      end subroutine read_o3data


      SUBROUTINE setindxoz(latd,nlats,gaul,jindx1,jindx2,ddy)
!
      USE MACHINE , ONLY : kind_phys
      use ozne_def , only : jo3 => latsozp, pl_lat
!
      implicit none
!
      integer latd, nlats, JINDX1(latd),JINDX2(latd)
      real(kind=kind_phys) GAUL(nlats),DDY(latd)
!
      integer i,j,lat
!
      DO J=1,nlats
        jindx2(j) = jo3 + 1
        do i=1,jo3
          if (gaul(j) < pl_lat(i)) then
            jindx2(j) = i
            exit
          endif
        enddo
        jindx1(j) = max(jindx2(j)-1,1)
        jindx2(j) = min(jindx2(j),jo3)
        if (jindx2(j) .ne. jindx1(j)) then
          DDY(j) = (gaul(j)           - pl_lat(jindx1(j))) &
                 / (pl_lat(jindx2(j)) - pl_lat(jindx1(j)))
        else
          ddy(j) = 1.0
        endif
!     print *,' j=',j,' gaul=',gaul(j),' jindx12=',jindx1(j),
!    &jindx2(j),' pl_lat=',pl_lat(jindx1(j)),pl_lat(jindx2(j))
!    &,' ddy=',ddy(j)
!csela if(me.eq.0) print*,'1st ddy(j,1) ddy(j,2),j=',ddy(j,1),ddy(j,2),j
 
      ENDDO
 
!csela do j=1,nlats
!csela if(me.eq.0) print*,'x1(j,1) jindx1(j,2)',jindx1(j,1),jindx1(j,2),j
!csela if(me.eq.0) print*,'x2(j,1) jindx2(j,2)',jindx2(j,1),jindx2(j,2),j
!csela enddo
!csela do j=1,nlats
!csela  if(me.eq.0) print*,'ddy(j,1) ddy(j,2)',ddy(j,1),ddy(j,2)
!csela enddo
!cyt   if(me.eq.0) print*,'completed setindxoz for nasa prod. and diss'
 
      RETURN
      END
!
!**********************************************************************
!
      SUBROUTINE ozinterpol(me,latd,nlats,IDATE,FHOUR, &
                            jindx1,jindx2,ozpljn,ozplout,ddy)
!
      USE MACHINE , ONLY : kind_phys
      use ozne_def
      implicit none
      integer             iday,j,j1,j2,l,latd,nc,n1,n2
      real(kind=kind_phys) fhour,tem, tx1, tx2
!
 
      integer  JINDX1(LATD), JINDX2(LATD)
      integer  me,idate(4),nlats
      integer  IDAT(8),JDAT(8)
!
      real(kind=kind_phys) ozpljn(latsozp,levozp,pl_coeff,timeoz)
      real(kind=kind_phys) DDY(LATD)
      real(kind=kind_phys) ozplout(LATD,levozp,pl_coeff)
      real(kind=kind_phys) RINC(5), rjday
      integer jdow, jdoy, jday
      real(4) rinc4(5)
      integer w3kindreal,w3kindint
!
      IDAT=0
      IDAT(1)=IDATE(4)
      IDAT(2)=IDATE(2)
      IDAT(3)=IDATE(3)
      IDAT(5)=IDATE(1)
      RINC=0.
      RINC(2)=FHOUR
      call w3kind(w3kindreal,w3kindint)
      if(w3kindreal==4) then
        rinc4=rinc
        CALL W3MOVDAT(RINC4,IDAT,JDAT)
      else
        CALL W3MOVDAT(RINC,IDAT,JDAT)
      endif
!
      jdow = 0
      jdoy = 0
      jday = 0
      call w3doxdat(jdat,jdow,jdoy,jday)
      rjday = jdoy + jdat(5) / 24.
      IF (RJDAY .LT. PL_time(1)) RJDAY = RJDAY+365.
!
      n2 = timeoz + 1
      do j=1,timeoz
        if (rjday .lt. pl_time(j)) then
          n2 = j
          exit
        endif
      enddo
      n1 = n2 - 1
      if (n1 <= 0)     n1 = n1 + timeoz
      if (n2 > timeoz) n2 = n2 - timeoz

!
!     if (me .eq. 0) print *,' n1=',n1,' n2=',n2,' rjday=',rjday
!    &,'pl_time=',pl_time(n1),pl_time(n2)
!

      tx1 = (pl_time(n2) - rjday) / (pl_time(n2) - pl_time(n1))
      tx2 = 1.0 - tx1
!
      do nc=1,pl_coeff
        DO L=1,levozp
          DO J=1,nlats
            J1  = JINDX1(J)
            J2  = JINDX2(J)
            TEM = 1.0 - DDY(J)
            ozplout(j,L,nc) = & 
            tx1*(TEM*ozplin(J1,L,nc,n1)+DDY(J)*ozplin(J2,L,nc,n1)) & 
          + tx2*(TEM*ozplin(J1,L,nc,n2)+DDY(J)*ozplin(J2,L,nc,n2))
          ENDDO
        ENDDO
      enddo
!
      RETURN
      END
