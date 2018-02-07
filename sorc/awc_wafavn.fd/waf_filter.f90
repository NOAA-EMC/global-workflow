! waf_filter.f90
! contains miscellaneous filters
! equivalent potential temperature
! George Trojan, SAIC/EMC/NCEP, February 2007
! Last update: 03/07/07

module waf_filter

use kinds

implicit none

public
private big_value

real(kind=r_kind), parameter :: big_value = 1.0e12

contains
!----------------------------------------------------------------------------
SUBROUTINE FILTER_SC(IMAXIN, JJMAXIN, JMAXIN, JPASS, KGDSIN, FIN, &
    LIN, IRET, lower_limit, upper_limit)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
!   SUBPROGRAM: FILTER_SC
!   PRGMMR: BALDWIN          ORG: NP22        DATE: 98-08-11  
!
! ABSTRACT: FILTER_SC FILTERS A SCALAR FIELD.  IT TREATS THE STAGGERED
!           E-GRID SPECIALLY (201,203).
!
! PROGRAM HISTORY LOG:
!   98-08-11  BALDWIN     ORIGINATOR
!   03-03-18  MANIKIN     ADDED IN CODE TO PREVENT NEGATIVE RH VALUES
!                            AND VALUES GREATER THAN 100
!
! USAGE:  CALL FILTER_SC (IMAXIN,JJMAXIN,JMAXIN,JPASS,KGDSIN,
!    &                      FIN,LIN,IRET,lower_limit,upper_limit)
!
!   INPUT:
!         IMAXIN            INTEGER - MAX X DIMENSION OF INPUT GRID
!         JJMAXIN           INTEGER - MAX Y DIMENSION OF INPUT GRID
!         JMAXIN            INTEGER - MAX DIMENSION OF FIN
!         JPASS             INTEGER - NUMBER OF FILTER PASSES TO MAKE
!         KGDSIN(22)        INTEGER - KGDS FOR THIS GRID
!         FIN(JMAXIN)       REAL    - SCALAR TO FILTER
!         LIN(JMAXIN)       LOGICAL*1 - BITMAP CORRESPONDING TO FIN
!         lower_limit       real, optional - to prevent output values out of
!                                             range
!         upper_limit
!
!   OUTPUT:
!         FIN(JMAXIN)       REAL    - SCALAR TO FILTER
!         IRET              INTEGER - RETURN CODE
!
!   RETURN CODES:
!     IRET =   0 - NORMAL EXIT
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : CRAY J-916
!
!$$$
    integer, intent(in) :: imaxin, jjmaxin, jmaxin, jpass
    INTEGER, intent(in) :: KGDSIN(22)
    REAL(kind=r_kind), intent(inout) :: FIN(JMAXIN)
    logical(kind=1), intent(in) :: LIN(JMAXIN)
    integer, intent(out) :: iret
    real(kind=r_kind), optional :: lower_limit, upper_limit

    integer :: i, ip, j, kk, l, imx, imax, jmx
    INTEGER, dimension(JJMAXIN) :: IHE, IHW
    LOGICAL(kind=1), dimension(IMAXIN,JJMAXIN) :: LFLT, LFLT2
    REAL(kind=r_kind), dimension(IMAXIN,JJMAXIN) :: Z, Z1

    IRET=0

    IF (JPASS.GT.0) THEN
!
!     CALCULATE THE I-INDEX EAST-WEST INCREMENTS FOR 201/203 GRIDS
!
        IF (KGDSIN(1).EQ.203.OR.KGDSIN(1).EQ.201) THEN
            DO J=1,JJMAXIN
                IHE(J)=MOD(J+1,2)
                IHW(J)=IHE(J)-1
            ENDDO
        ENDIF
!
! **  FILTER SCALAR   25-PT BLECK FILTER               
!
        IMX=KGDSIN(2)
        JMX=KGDSIN(3)
        IF (KGDSIN(1).EQ.201) THEN
            IMX=KGDSIN(7)
            JMX=KGDSIN(8)
        ENDIF
!
        IF (KGDSIN(1).EQ.201) THEN
            KK=0
            DO J=1,JMX
                IMAX=IMX-MOD(J+1,2)
                DO I=1,IMX
                    IF (I.LE.IMAX) KK=KK+1
                    Z1(I,J)=FIN(KK)
                    LFLT(I,J)=LIN(KK)
                ENDDO
            ENDDO
        ELSE
            DO J=1,JMX
                DO I=1,IMX
                    KK=(J-1)*IMX+I
                    Z1(I,J)=FIN(KK)
                    LFLT(I,J)=LIN(KK)
                ENDDO
            ENDDO
        ENDIF
!
!  ONLY FILTER IF ALL 25 PTS ARE WITHIN BITMAP
!
        IF (KGDSIN(1).NE.203.AND.KGDSIN(1).NE.201) THEN
            DO J = 1,JMX-4
                DO I = 1,IMX-4
                    LFLT2(I+2,J+2)=LFLT(I  ,J  ).AND.LFLT(I+1,J  ).AND. &
                        LFLT(I+2,J  ).AND.LFLT(I+3,J  ).AND.LFLT(I+4,J  ).AND. &
                        LFLT(I  ,J+1).AND.LFLT(I+1,J+1).AND.LFLT(I+2,J+1).AND. &
                        LFLT(I+3,J+1).AND.LFLT(I+4,J+1).AND.LFLT(I  ,J+2).AND. &
                        LFLT(I+1,J+2).AND.LFLT(I+2,J+2).AND.LFLT(I+3,J+2).AND. &
                        LFLT(I+4,J+2).AND.LFLT(I  ,J+3).AND.LFLT(I+1,J+3).AND. &
                        LFLT(I+2,J+3).AND.LFLT(I+3,J+3).AND.LFLT(I+4,J+3).AND. &
                        LFLT(I  ,J+4).AND.LFLT(I+1,J+4).AND.LFLT(I+2,J+4).AND. &
                        LFLT(I+3,J+4).AND.LFLT(I+4,J+4)
                ENDDO
            ENDDO
        ELSE
            DO J = 1,JMX-8
                DO I = 1,IMX-4
                    LFLT2(I+2,J+4) = LFLT(I+2,J+4).AND. &
                        LFLT(I+2+IHE(J),J+3).AND.LFLT(I+2+IHW(J),J+3).AND. &
                        LFLT(I+2+IHE(J),J+5).AND.LFLT(I+2+IHW(J),J+5).AND. &
                        LFLT(I+1,J+6).AND.LFLT(I+1,J+2).AND. &
                        LFLT(I+3,J+6).AND.LFLT(I+3,J+2).AND. &
                        LFLT(I+1,J+4).AND.LFLT(I+3,J+4).AND. &
                        LFLT(I+2,J+2).AND.LFLT(I+2,J+6).AND. &
                        LFLT(I+2+IHE(J),J+1).AND.LFLT(I+2+IHW(J),J+1).AND. &
                        LFLT(I+1+IHW(J),J+3).AND.LFLT(I+3+IHE(J),J+3).AND. &
                        LFLT(I+1+IHW(J),J+5).AND.LFLT(I+3+IHE(J),J+5).AND. &
                        LFLT(I+2+IHE(J),J+7).AND.LFLT(I+2+IHW(J),J+7).AND. &
                        LFLT(I+2,J).AND.LFLT(I+4,J+4).AND. &
                        LFLT(I,J+4).AND.LFLT(I+2,J+8)
                ENDDO
            ENDDO
        ENDIF
!
        DO IP = 1,JPASS
            IF (KGDSIN(1).NE.203.AND.KGDSIN(1).NE.201) THEN
                DO J = 1,JMX-4
                    DO I = 1,IMX-4
                        IF (LFLT2(I+2,J+2)) THEN
                            Z(I+2,J+2) = 0.279372 * Z1(I+2,J+2) &
                                +0.171943*((Z1(I+1,J+2)+Z1(I+2,J+1))+(Z1(I+3,J+2)+Z1(I+2,J+3))) &
                                -0.006918*((Z1(I  ,J+2)+Z1(I+2,J  ))+(Z1(I+4,J+2)+Z1(I+2,J+4))) &
                                +0.077458*((Z1(I+1,J+1)+Z1(I+3,J+3))+(Z1(I+3,J+1)+Z1(I+1,J+3))) &
                                -0.024693*((Z1(I+1,J  )+Z1(I+3,J  ))+(Z1(I  ,J+1)+Z1(I+4,J+1)) &
                                         +(Z1(I  ,J+3)+Z1(I+4,J+3))+(Z1(I+1,J+4)+Z1(I+3,J+4))) &
                                -0.01294 *((Z1(I  ,J  )+Z1(I+4,J  ))+(Z1(I  ,J+4)+Z1(I+4,J+4)))
                        ELSE
                            Z(I+2,J+2) = Z1(I+2,J+2)
                        ENDIF
                    ENDDO
                ENDDO
                DO J= 3,JMX-2
                    DO I= 3,IMX-2
                        Z1(I,J) = Z(I,J)
                    ENDDO
                ENDDO
            ELSE
                DO J = 1,JMX-8
                    DO I = 1,IMX-4
                        IF (LFLT2(I+2,J+4)) THEN
                            Z(I+2,J+4) = 0.279372 * Z1(I+2,J+4) &
                                +0.171943* (Z1(I+2+IHE(J),J+3)+Z1(I+2+IHW(J),J+3)+ &
                                            Z1(I+2+IHE(J),J+5)+Z1(I+2+IHW(J),J+5)) &
                                -0.006918* (Z1(I+1,J+6)+Z1(I+1,J+2)+Z1(I+3,J+6)+Z1(I+3,J+2)) &
                                +0.077458* (Z1(I+1,J+4)+Z1(I+3,J+4)+Z1(I+2,J+2)+Z1(I+2,J+6)) &
                                -0.024693* (Z1(I+2+IHE(J),J+1)+Z1(I+2+IHW(J),J+1) + &
                                            Z1(I+1+IHW(J),J+3)+Z1(I+3+IHE(J),J+3) + &
                                            Z1(I+1+IHW(J),J+5)+Z1(I+3+IHE(J),J+5) + &
                                            Z1(I+2+IHE(J),J+7)+Z1(I+2+IHW(J),J+7)) &
                                -0.01294 * (Z1(I+2,J)+Z1(I+4,J+4)+Z1(I,J+4)+Z1(I+2,J+8))
                        ELSE
                            Z(I+2,J+4) = Z1(I+2,J+4)
                        ENDIF
                    ENDDO
                ENDDO
                DO J= 5,JMX-4
                    DO I= 3,IMX-2
                        Z1(I,J) = Z(I,J)
                    ENDDO
                ENDDO
            ENDIF
        ENDDO
!
        IF (KGDSIN(1).EQ.201) THEN
            KK=0
            DO J=1,JMX
                IMAX=IMX-MOD(J+1,2)
                DO I=1,IMX
                    IF (I.LE.IMAX) KK=KK+1
                    FIN(KK)=Z1(I,J)
                ENDDO
            ENDDO
        ELSE
            DO J=1,JMX
                DO I=1,IMX
                    KK=(J-1)*IMX+I
                    FIN(KK)=Z1(I,J)
                ENDDO
            ENDDO
        ENDIF
    ENDIF

    if (present(lower_limit)) &
        where (FIN < lower_limit) fin = lower_limit
    if (present(upper_limit)) &
        where (FIN > upper_limit) fin = upper_limit
end subroutine filter_sc

!----------------------------------------------------------------------------
subroutine filter_bleck(npass, nx, ny, values, msng, lb, ub)
! a wrapper to the above subroutine. 
! Not applicable to staggered grids
    integer, intent(in) :: npass
    integer, intent(in) :: nx, ny
    real(kind=r_kind), dimension(nx, ny), intent(inout) :: values
    real(kind=r_kind), optional :: msng, lb, ub

    integer :: iret
    logical(kind=1), dimension(nx, ny) :: mask
    integer, dimension(22) :: gds

    gds(1) = 1 ! does not matter unless 211 or 213
    gds(2) = nx
    gds(3) = ny
    mask = .true.
    if (present(msng)) then
        where (values == msng) mask = .false.
    end if
    call filter_sc(nx, ny, nx*ny, npass, gds, values, mask, iret, lb, ub)
end subroutine filter_bleck

!----------------------------------------------------------------------------
subroutine filter_avg2d(nx, ny, array, dx, dy, delta, msng, mask, &
    amean, amax, amin)
! calculates mean and max values in a cell of size delta
! assumes lat-lon input grid 
    integer, intent(in) :: nx, ny ! input grid dimensions
    real(kind=r_kind), dimension(nx,ny), intent(in) :: array ! input array
    real(kind=r_kind), dimension(ny), intent(in):: dx ! x- grid dimensions
    real(kind=r_kind), intent(in) :: dy ! y- grid dimension
    real(kind=r_kind), intent(in) :: delta ! cell size to calculate mean 
        ! and max
    real(kind=r_kind), intent(in) :: msng ! missing value
    logical, dimension(nx,ny), intent(in), optional :: mask
    real(kind=r_kind), dimension(nx,ny), intent(out), optional :: &
        amean, amax, amin ! calculated mean, min and max grids

    integer :: i, j, num_avg, ii, jj, istart, iend, jstart, jend, num_x, num_y
    real(kind=r_kind) :: cur_mean, cur_min, cur_max
    logical :: do_mean, do_min, do_max

    num_y = int(delta/dy) ! number of y points in the cell is 2*num_y+1
    do_mean = present(amean)
    do_min = present(amin)
    do_max = present(amax)
    ! initialize output arrays, depending on passed options
    if (present(mask)) then
        if (do_mean) then
            where (mask) 
                amean = msng
            elsewhere
                amean = array
            end where
        end if
        if (do_min) then
            where (mask) 
                amin = msng
            elsewhere
                amin = array
            end where
        end if
        if (do_max) then
            where (mask) 
                amax = msng
            elsewhere
                amax = array
            end where
        end if
    else
        if (do_mean) amean = msng
        if (do_min) amin = msng
        if (do_max) amax = msng
    end if
    ! dx(1) and dx(ny) is a negative small value (-1.2151593E-03 if ny=721),
    ! this makes the num_x a negative (and large) value which has no sense.
    ! solution: use dx(2) as dx(1), and use dx(ny-1) as dx(ny)
    do j = 1, ny
        num_x = int(delta/dx(j)) ! number of x points in the cell is 2*num_x+1
        if(j ==1) num_x = int(delta/dx(2))
        if(j ==ny) num_x = int(delta/dx(ny-1))
        jstart = max(1, j-num_y)
        jend = min(ny, j+num_y)
        do i = 1, nx
            if (present(mask) .and. .not. mask(i,j)) cycle
            istart = i - num_x
            if (istart < 1) istart = istart + nx
            iend = i + num_x
            if (iend > nx) iend = iend - nx
            cur_mean = 0.0
            cur_max = -big_value
            cur_min = big_value
            num_avg = 0 ! number of points for average
            if (istart < iend) then
                do jj = jstart, jend
                    do ii = istart, iend
                        if (array(ii,jj) == msng .or. present(mask) &
                            .and. .not. mask(ii,jj)) cycle
                        num_avg = num_avg + 1
                        cur_mean = cur_mean + array(ii,jj)
                        if (cur_max < array(ii,jj)) cur_max = array(ii,jj)
                        if (cur_min > array(ii,jj)) cur_min = array(ii,jj)
                    end do
                end do
            else
                do jj = jstart, jend
                    do ii = istart, nx
                        if (array(ii,jj) == msng .or. present(mask) &
                            .and. .not. mask(ii,jj)) cycle
                        num_avg = num_avg + 1
                        cur_mean = cur_mean + array(ii,jj)
                        if (cur_max < array(ii,jj)) cur_max = array(ii,jj)
                        if (cur_min > array(ii,jj)) cur_min = array(ii,jj)
                    end do
                    do ii = 1, iend
                        if (array(ii,jj) == msng .or. present(mask) &
                            .and. .not. mask(ii,jj)) cycle
                        num_avg = num_avg + 1
                        cur_mean = cur_mean + array(ii,jj)
                        if (cur_max < array(ii,jj)) cur_max = array(ii,jj)
                        if (cur_min > array(ii,jj)) cur_min = array(ii,jj)
                    end do
                end do
            end if
            if (num_avg > 0) then
                if (do_mean) amean(i,j) = cur_mean/num_avg
                if (do_max) amax(i,j) = cur_max
                if (do_min) amin(i,j) = cur_min
            end if
        end do
    end do
end subroutine filter_avg2d

!----------------------------------------------------------------------------
subroutine filter_avg3d(nx, ny, np, lvl, array, dx, dy, dp, delta_xy, &
    delta_p, msng, mask, amean, amax, amin)
! calculates mean and max values in a cell of size delta
! assumes lat-lon input grid 
    integer, intent(in) :: nx, ny, np ! input grid dimensions
    integer, intent(in) :: lvl ! index to pressure level
    real(kind=r_kind), dimension(nx,ny,np), intent(in) :: array ! input array
    real(kind=r_kind), dimension(ny), intent(in):: dx ! x- grid dimensions
    real(kind=r_kind), intent(in) :: dy ! y- grid dimension
    integer, intent(in) :: dp ! p - grid dimension
    real(kind=r_kind), intent(in) :: delta_xy, delta_p ! cell size 
        ! to calculate mean and max
    logical, dimension(nx,ny,np), intent(in), optional :: mask
    real(kind=r_kind), intent(in) :: msng ! missing value
    real(kind=r_kind), dimension(nx,ny), intent(out), optional :: &
        amean, amax, amin ! calculated mean and max grids

    integer :: i, j, num_avg, ii, jj, kk, istart, iend, jstart, jend, num_x, &
        num_y, num_p
    real(kind=r_kind) :: cur_mean, cur_max, cur_min
    logical :: do_mean, do_min, do_max

    num_p = int(delta_p/dp)
    if (lvl-num_p < 1 .or. lvl+num_p > np) then
        print *, 'Program BUG'
        stop 99
    end if
    num_y = int(delta_xy/dy) ! number of y points in the cell is 2*num_y+1
    do_mean = present(amean)
    do_min = present(amin)
    do_max = present(amax)
    ! initialize output arrays, depending on passed options
    if (present(mask)) then
        if (do_mean) then
            where (mask(:,:,lvl)) 
                amean = msng
            elsewhere
                amean = array(:,:,lvl)
            end where
        end if
        if (do_min) then
            where (mask(:,:,lvl)) 
                amin = msng
            elsewhere
                amin = array(:,:,lvl)
            end where
        end if
        if (do_max) then
            where (mask(:,:,lvl)) 
                amax = msng
            elsewhere
                amax = array(:,:,lvl)
            end where
        end if
    else
        if (do_mean) amean = msng
        if (do_min) amin = msng
        if (do_max) amax = msng
    end if
    ! dx(1) and dx(ny) is a negative small value (-1.2151593E-03 if ny=721),
    ! this makes the num_x a negative (and large) value which has no sense.
    ! solution: use dx(2) as dx(1), and use dx(ny-1) as dx(ny)
    do j = 1, ny
        num_x = int(delta_xy/dx(j)) ! number of x points in the cell is 
                                    ! 2*num_x+1
        if(j ==1) num_x = int(delta_xy/dx(2))
        if(j ==ny) num_x = int(delta_xy/dx(ny-1))
        jstart = max(1, j-num_y)
        jend = min(ny, j+num_y)
        do i = 1, nx
            if (present(mask) .and. .not. mask(i,j,lvl)) cycle
            istart = i - num_x
            if (istart < 1) istart = istart + nx
            iend = i + num_x
            if (iend > nx) iend = iend - nx
            cur_mean = 0.0
            cur_max = -big_value
            cur_min = big_value
            num_avg = 0
            if (istart < iend) then
                do kk = lvl-num_p, lvl+num_p
                    do jj = jstart, jend
                        do ii = istart, iend
                            if (array(ii,jj,kk) == msng .or. &
                                (present(mask) .and. .not. mask(ii,jj,kk))) cycle
                            num_avg = num_avg + 1
                            cur_mean = cur_mean + array(ii,jj,kk)
                            if (cur_max < array(ii,jj,kk)) &
                                cur_max = array(ii,jj,kk)
                            if (cur_min > array(ii,jj,kk)) &
                                cur_min = array(ii,jj,kk)
                        end do
                    end do
                end do
            else
                do kk = lvl-num_p, lvl+num_p
                    do jj = jstart, jend
                        do ii = istart, nx
                            if (array(ii,jj,kk) == msng .or. &
                                (present(mask) .and. .not. mask(ii,jj,kk))) cycle
                            num_avg = num_avg + 1
                            cur_mean = cur_mean + array(ii,jj,kk)
                            if (cur_max < array(ii,jj,kk)) &
                                cur_max = array(ii,jj,kk)
                            if (cur_min > array(ii,jj,kk)) &
                                cur_min = array(ii,jj,kk)
                        end do
                        do ii = 1, iend
                            if (array(ii,jj,kk) == msng .or. &
                                (present(mask) .and. .not. mask(ii,jj,kk))) cycle
                            num_avg = num_avg + 1
                            cur_mean = cur_mean + array(ii,jj,kk)
                            if (cur_max < array(ii,jj,kk)) &
                                cur_max = array(ii,jj,kk)
                            if (cur_min > array(ii,jj,kk)) &
                                cur_min = array(ii,jj,kk)
                        end do
                    end do
                end do
            end if
            if (num_avg > 0) then
                if (do_mean) amean(i,j) = cur_mean/num_avg
                if (do_max) amax(i,j) = cur_max
                if (do_min) amin(i,j) = cur_min
            end if
        end do
    end do
end subroutine filter_avg3d

end module waf_filter
