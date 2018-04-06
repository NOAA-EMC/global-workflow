      SUBROUTINE MDL2THANDPV(kth,kpv,th,pv)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    MDL2THANDPV       VERT INTRP OF MODEL LVLS TO ISENTROPIC AND PV
!   PRGRMMR: CHUANG           ORG: W/NP22     DATE: 07-03-26       
!     
! ABSTRACT:
!     FOR MOST APPLICATIONS THIS ROUTINE IS THE WORKHORSE
!     OF THE POST PROCESSOR.  IN A NUTSHELL IT INTERPOLATES
!     DATA FROM MODEL TO THETA AND PV SURFACES.  
!   .     
!   PROGRAM HISTORY
!     11-02-06  J. WANG ADD GRIB2 OPTION
!     14-03-06  S. Moorthi - updated for threading and some optimization
!     16-12-19  G.P. Lou - Added A-grid regional models
!     
!
! USAGE:    CALL MDL2THANDPV
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST: 
!     NONE       
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       PVETC   - 
!       P2TH   - 
!       P2PV    - 
!       COMMON   - CTLBLK
!                  RQSTFLD
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90
!     MACHINE : IBM SP
!$$$  
!
!
      use vrbls3d, only: pmid, t, uh, q, vh, zmid, omga, pint
      use vrbls2d, only: f
      use masks, only: gdlat, gdlon, dx, dy
      use physcons, only: con_eps, con_epsm1
      use params_mod, only: dtr, small, erad, d608, rhmin
      use CTLBLK_mod, only: spval, lm, jsta_2l, jend_2u, jsta_2l, grib, cfld, datapd, fld_info,&
              im, jm, jsta, jend, jsta_m, jend_m, modelname, global,gdsdegr,me
      use RQSTFLD_mod, only: iget, lvls, id, iavblfld, lvlsxml
      use gridspec_mod, only: gridtype,dyval
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
!     DECLARE VARIABLES.
!     
      integer,intent(in)     :: kth, kpv
      real,   intent(in)     :: th(kth), pv(kpv)
      real, dimension(im,jm) :: grid1, grid2
      real, dimension(kpv)   :: pvpt, pvpb

      LOGICAL IOOMG,IOALL
      LOGICAL LTH(KTH), LPV(KPV)

      REAL, allocatable:: DUM1D1(:), DUM1D2(:), DUM1D3(:),DUM1D4(:)   &
      ,                   DUM1D5(:), DUM1D6(:), DUM1D7(:),DUM1D8(:)   &
      ,                   DUM1D9(:), DUM1D10(:),DUM1D11(:)            &
      ,                   DUM1D12(:),DUM1D13(:),DUM1D14(:)
!      
      real, dimension(IM,JSTA:JEND,KTH) :: UTH, VTH, HMTH, TTH, PVTH, &
                                           SIGMATH, RHTH, OTH
      real, dimension(IM,JSTA:JEND,KPV) :: UPV, VPV, HPV, TPV, PPV, SPV
!
      real, allocatable :: wrk1(:,:), wrk2(:,:), wrk3(:,:), wrk4(:,:), cosl(:,:)
      real, allocatable :: tuv(:,:,:),pmiduv(:,:,:)
!
      integer, dimension(im) :: iw, ie
      integer I,J,L,K,lp,imb2,ip1,im1,ii,jj,jmt2,ihw,ihe
      real    DVDX,DUDY,UAVG,TPHI, es, qstl, eradi, tem
      real,external :: fpvsnew
!
!     
!******************************************************************************
!
!     START MDL2TH. 
!     
!     SET TOTAL NUMBER OF POINTS ON OUTPUT GRID.
!
!---------------------------------------------------------------
!
!     *** PART I ***
!
!     VERTICAL INTERPOLATION OF EVERYTHING ELSE.  EXECUTE ONLY
!     IF THERE'S SOMETHING WE WANT.
!
      IF((IGET(332) > 0).OR.(IGET(333) > 0).OR.     &
         (IGET(334) > 0).OR.(IGET(335) > 0).OR.     &
         (IGET(336) > 0).OR.(IGET(337) > 0).OR.     &
         (IGET(338) > 0).OR.(IGET(339) > 0).OR.     &
         (IGET(340) > 0).OR.(IGET(341) > 0).OR.     &
         (IGET(351) > 0).OR.(IGET(352) > 0).OR.     &
         (IGET(353) > 0).OR.(IGET(378) > 0) ) THEN
!
!---------------------------------------------------------------------
!***
!***  BECAUSE SIGMA LAYERS DO NOT GO UNDERGROUND, DO ALL INTERPOLATION ABOVE GROUND NOW.
!***
!
        do k=1,kpv
          pvpt(k) = 5000.0         ! top limit for PV search
          pvpb(k) = 0.8            ! Bottome limit for PV search in sigma
        enddo

        do k=1,kth
!$omp parallel do private(i,j)
          do j=jsta,jend
            do i=1,im
              UTH(i,j,k)     = SPVAL
              VTH(i,j,k)     = SPVAL
              HMTH(i,j,k)    = SPVAL
              TTH(i,j,k)     = SPVAL
              PVTH(i,j,k)    = SPVAL
              SIGMATH(i,j,k) = SPVAL
              RHTH(i,j,k)    = SPVAL
              OTH(i,j,k)     = SPVAL
            enddo
          enddo
        enddo
        do k=1,kpv
!$omp parallel do private(i,j)
          do j=jsta,jend
            do i=1,im
              UPV(i,j,k)     = SPVAL
              VPV(i,j,k)     = SPVAL
              HPV(i,j,k)     = SPVAL 
              TPV(i,j,k)     = SPVAL
              PPV(i,j,k)     = SPVAL
              SPV(i,j,k)     = SPVAL
            enddo
          enddo
        enddo
        ALLOCATE(DUM1D1(LM), DUM1D2(LM), DUM1D3(LM),DUM1D4(LM))
        ALLOCATE(DUM1D5(LM), DUM1D6(LM))              !TV and Vorticity
        ALLOCATE(DUM1D7(LM), DUM1D8(LM), DUM1D9(LM),DUM1D10(LM))
        ALLOCATE(DUM1D11(LM),DUM1D12(LM),DUM1D13(LM))
        ALLOCATE(DUM1D14(LM))
!
        DO L=1,LM 
          CALL EXCH(PMID(1:IM,JSTA_2L:JEND_2U,L))
          CALL EXCH(T(1:IM,JSTA_2L:JEND_2U,L))
          CALL EXCH(UH(1:IM,JSTA_2L:JEND_2U,L))
        END DO
        CALL EXCH(GDLAT(1,JSTA_2L))

!       print *,' JSTA_2L=',JSTA_2L,' JSTA=',JSTA_2L,' JEND_2U=', &
!    &JEND_2U,' JEND=',JEND,' IM=',IM
!     print *,' GDLATa=',gdlat(1,:)
!     print *,' GDLATb=',gdlat(im,:)
!	 
        allocate (wrk1(im,jsta:jend), wrk2(im,jsta:jend),         &
     &            wrk3(im,jsta:jend), cosl(im,jsta_2l:jend_2u))
        allocate (wrk4(im,jsta:jend))
        imb2  = im /2
        eradi = 1.0 / erad

!!        IF(MODELNAME == 'GFS' .or. global) THEN
        IF(GRIDTYPE == 'A')THEN
!$omp parallel do private(i)
          do i=1,im
            ie(i) = i + 1
            iw(i) = i - 1
          enddo
          iw(1)  = im
          ie(im) = 1
!
!$omp parallel do private(i,j,ip1,im1)
          DO J=JSTA,JEND
            do i=1,im
              ip1 = ie(i)
              im1 = iw(i)
              cosl(i,j) = cos(gdlat(i,j)*dtr)
              IF(cosl(i,j) >= SMALL) then
                wrk1(i,j) = eradi / cosl(i,j)
              else
                wrk1(i,j) = 0.
              end if
              if(i == im .or. i == 1)then
                wrk2(i,j) = 1.0 / ((360.+GDLON(ip1,J)-GDLON(im1,J))*DTR)!1/dlam
              else
                wrk2(i,j) = 1.0 / ((GDLON(ip1,J)-GDLON(im1,J))*DTR)     !1/dlam
              end if
              wrk4(i,j) = wrk1(i,j) * wrk2(i,j)   ! 1/dx
            enddo
          enddo
!         CALL EXCH(cosl(1,JSTA_2L))
          CALL EXCH(cosl)

!$omp  parallel do private(i,j,ii,tem)
          DO J=JSTA,JEND
            if (j == 1) then
              do i=1,im
                ii = i + imb2
                if (ii > im) ii = ii - im
                wrk3(i,j) = 1.0 / ((180.-GDLAT(i,J+1)-GDLAT(II,J))*DTR) !1/dphi
              enddo
            elseif (j == JM) then
              do i=1,im
                ii = i + imb2
                if (ii > im) ii = ii - im
                wrk3(i,j) = 1.0 / ((180.+GDLAT(i,J-1)+GDLAT(II,J))*DTR) !1/dphi
              enddo
            else
     !print *,' j=',j,' GDLATJm1=',gdlat(:,j-1)
     !print *,' j=',j,' GDLATJp1=',gdlat(:,j+1)
              do i=1,im
                tem = GDLAT(I,J-1) - GDLAT(I,J+1)
                if (abs(tem) > small) then
                  wrk3(i,j) = 1.0 / (tem*DTR)     !1/dphi
                else
                  wrk3(i,j) = 0.0
                endif
              enddo
            endif
         !if (j == 181) print*,' wrk3=',wrk3(126,j),' gdlat=',&
         !           GDLAT(126,J-1), gdlat(126,j+1)
          enddo  
        else  !!global?
!$omp  parallel do private(i,j)
          DO J=JSTA_m,Jend_m
            DO I=2,im-1
              wrk2(i,j) = 0.5 / DX(I,J)
              wrk3(i,j) = 0.5 / DY(I,J)
            END DO
          END DO
        endif

! need to put T and P on V points for computing dp/dx for e grid
        IF(GRIDTYPE == 'E')THEN
          allocate(tuv(1:im,jsta_2l:jend_2u,lm))
          allocate(pmiduv(1:im,jsta_2l:jend_2u,lm))
          do l=1,lm
            call h2u(t(1:im,jsta_2l:jend_2u,l),tuv(1:im,jsta_2l:jend_2u,l))
            call h2u(pmid(1:im,jsta_2l:jend_2u,l),pmiduv(1:im,jsta_2l:jend_2u,l))
          end do
        end if

!add A-grid regional models 
        IF(GRIDTYPE == 'A')THEN
        IF(MODELNAME == 'GFS' .or. global) THEN
!!$omp  parallel do private(i,j,ip1,im1,ii,jj,l,es,dum1d1,dum1d2,dum1d3,dum1d4,dum1d5,dum1d6,dum1d14,tem)
          DO J=JSTA,JEND
            DO I=1,IM
              ip1 = ie(i)
              im1 = iw(i)
              ii = i + imb2
              if (ii > im) ii = ii - im

              IF(J == 1) then               ! Near North pole
                IF(cosl(i,j) >= SMALL) THEN !not a pole point
                  tem = wrk3(i,j) * eradi
!$omp parallel do private(l,es)
                  DO L=1,LM
                    DUM1D5(L)  = T(I,J,L)*(1.+D608*Q(I,J,L))                  !Tv
                    ES         = min(FPVSNEW(T(I,J,L)),PMID(I,J,L))
                    DUM1D14(L) = Q(I,J,L) * (PMID(I,J,L)+CON_EPSM1*ES)/(CON_EPS*ES)   ! RH
                    DUM1D1(L)  = (PMID(ip1,J,L)- PMID(im1,J,L)) * wrk4(i,j) !dp/dx
                    DUM1D3(L)  = (T(ip1,J,L)   - T(im1,J,L))    * wrk4(i,j) !dt/dx
                    DUM1D2(L)  = (PMID(II,J,L) - PMID(I,J+1,L)) * tem       !dp/dy
                    DUM1D4(L)  = (T(II,J,L)    - T(I,J+1,L))    * tem       !dt/dy
                    DUM1D6(L)  = ((VH(ip1,J,L)-VH(im1,J,L))*wrk2(i,j)             &
     &                         +  (UH(II,J,L)*COSL(II,J)                          &
     &                         +   UH(I,J+1,L)*COSL(I,J+1))*wrk3(i,j))*wrk1(i,j)  &
     &                         + F(I,J)
                  END DO
                ELSE !pole point, compute at j=2
                  jj = 2
                  tem = wrk3(i,jj) * eradi
!$omp parallel do private(l,es)
                  DO L=1,LM
                    DUM1D5(L)  = T(I,J,L)*(1.+D608*Q(I,J,L))                !Tv
                    ES         = min(FPVSNEW(T(I,J,L)),PMID(I,J,L))
                    DUM1D14(L) = Q(I,J,L) * (PMID(I,J,L)+CON_EPSM1*ES)/(CON_EPS*ES)   ! RH
                    DUM1D1(L)  = (PMID(ip1,jj,L)- PMID(im1,jj,L)) * wrk4(i,jj) !dp/dx
                    DUM1D3(L)  = (T(ip1,jj,L)   - T(im1,jj,L))    * wrk4(i,jj) !dt/dx
                    DUM1D2(L)  = (PMID(I,J,L)-PMID(I,Jj+1,L)) * tem            !dp/dy
                    DUM1D4(L)  = (T(I,J,L)   - T(I,Jj+1,L))   * tem            !dt/dy
                    DUM1D6(L)  = ((VH(ip1,Jj,L)-VH(im1,Jj,L))*wrk2(i,jj)              &
     &                         +  (UH(I,J,L)*COSL(I,J)                                &
                               +   UH(I,Jj+1,L)*COSL(I,Jj+1))*wrk3(i,jj))*wrk1(i,jj)  &
     &                         +  F(I,Jj)
                  END DO
                END IF 
              ELSE IF(J == JM) THEN         ! Near South Pole
                IF(cosl(i,j) >= SMALL) THEN !not a pole point
                  tem = wrk3(i,j) * eradi
!$omp parallel do private(l,es)
                  DO L=1,LM
                    DUM1D5(L)  = T(I,J,L)*(1.+D608*Q(I,J,L))                  !Tv
                    ES         = min(FPVSNEW(T(I,J,L)),PMID(I,J,L))
                    DUM1D14(L) = Q(I,J,L) * (PMID(I,J,L)+CON_EPSM1*ES)/(CON_EPS*ES)   ! RH
                    DUM1D1(L)  = (PMID(ip1,J,L)- PMID(im1,J,L)) * wrk4(i,j) !dp/dx
                    DUM1D3(L)  = (T(ip1,J,L)   - T(im1,J,L))    * wrk4(i,j) !dt/dx
                    DUM1D2(L)  = (PMID(I,J-1,L)-PMID(II,J,L))   * tem       !dp/dy
                    DUM1D4(L)  = (T(I,J-1,L)-T(II,J,L))         * tem       !dt/dy
                    DUM1D6(L)  = ((VH(ip1,J,L)-VH(im1,J,L))* wrk2(i,j)            &
     &                         +  (UH(I,J-1,L)*COSL(I,J-1)                        &
     &                         +   UH(II,J,L)*COSL(II,J))*wrk3(i,j))*wrk1(i,j)    &
     &                         + F(I,J)
                  END DO
                ELSE !pole point, compute at j=jm-1
                  jj = jm-1
                  tem = wrk3(i,jj) * eradi
!$omp parallel do private(l,es)
                  DO L=1,LM
                    DUM1D5(L)  = T(I,J,L)*(1.+D608*Q(I,J,L))                !Tv
                    ES         = min(FPVSNEW(T(I,J,L)),PMID(I,J,L))
                    DUM1D14(L)  = Q(I,J,L) * (PMID(I,J,L)+CON_EPSM1*ES)/(CON_EPS*ES)   ! RH
                    DUM1D1(L)  = (PMID(ip1,jj,L)- PMID(im1,jj,L)) * wrk4(i,jj) !dp/dx
                    DUM1D3(L)  = (T(ip1,jj,L)   - T(im1,jj,L))    * wrk4(i,jj) !dt/dx
                    DUM1D2(L)  = (PMID(I,JJ-1,L)-PMID(I,J,L))     * tem        !dp/dy
                    DUM1D4(L)  = (T(I,Jj-1,L)-T(I,J,L))           * tem        !dt/dy
                    DUM1D6(L)  = ((VH(ip1,Jj,L)-VH(im1,Jj,L))*wrk2(i,jj)        &
     &                         +  (UH(I,Jj-1,L)*COSL(I,Jj-1)                    &
     &                          +   UH(I,J,L)*COSL(I,J))*wrk3(i,jj))*wrk1(i,jj)  &
     &                         + F(I,Jj)  
                  END DO
                END IF
              ELSE
                tem = wrk3(i,j) * eradi
!$omp parallel do private(l,es)
                DO L=1,LM
                  DUM1D5(L)  = T(I,J,L)*(1.+D608*Q(I,J,L))                    !Tv
                  ES         = min(FPVSNEW(T(I,J,L)),PMID(I,J,L))
                  DUM1D14(L) = Q(I,J,L) * (PMID(I,J,L)+CON_EPSM1*ES)/(CON_EPS*ES)   ! RH
                  DUM1D1(L)  = (PMID(ip1,J,L)- PMID(im1,J,L)) * wrk4(i,j) !dp/dx
                  DUM1D3(L)  = (T(ip1,J,L)   - T(im1,J,L))    * wrk4(i,j) !dt/dx
!     if (j >= 181) print *,' i=',i,' tem=',tem,' pmid=',pmid(i,j-1,l)&
!    ,pmid(i,j-1,l),' l=',l,' j=',j
                  DUM1D2(L)  = (PMID(I,J-1,L)-PMID(I,J+1,L))  * tem        !dp/dy
                  DUM1D4(L)  = (T(I,J-1,L)-T(I,J+1,L))        * tem        !dt/dy
                  DUM1D6(L)  = ((VH(ip1,J,L)-VH(im1,J,L))* wrk2(i,j)             &
     &                       -  (UH(I,J-1,L)*COSL(I,J-1)                         &
     &                       -   UH(I,J+1,L)*COSL(I,J+1))*wrk3(i,j))*wrk1(i,j)   &
     &                       + F(I,J)  
                END DO
              END IF
  

              IF(I==IM/2 .AND. J==JM/2)then 
                PRINT*,'SAMPLE PVETC INPUT ',                                 &
                       'p,dpdx,dpdy,tv,dtdx,dtdy,h,u,v,vort= '
                DO L=1,LM
                  print*,pmid(i,j,l),dum1d1(l),dum1d2(l),dum1d5(l)      &
                        ,dum1d3(l),dum1d4(l),zmid(i,j,l),uh(i,j,l),vh(i,j,l)  &
                        ,dum1d6(l)
                end do
              end if

              CALL PVETC(LM,PMID(I,J,1:LM),DUM1D1,DUM1D2                      &
                        ,DUM1D5,DUM1D3,DUM1D4,ZMID(I,J,1:LM),UH(I,J,1:LM)     &
                        ,VH(I,J,1:LM),DUM1D6                                  &
                        ,DUM1D7,DUM1D8,DUM1D9,DUM1D10,DUM1D11,DUM1D12,DUM1D13)!output

              IF(I==IM/2 .AND. J==JM/2)then 
                PRINT*,'SAMPLE PVETC OUTPUT '  &
                       ,'hm,s,bvf2,pvn,theta,sigma,pvu= '
                DO L=1,LM
                  print*,dum1d7(l),dum1d8(l),dum1d9(l),dum1d10(l),dum1d11(l) &
                        ,dum1d12(l),dum1d13(l)
                end do
              end if 

              IF((IGET(332) > 0).OR.(IGET(333) > 0).OR.                &
                 (IGET(334) > 0).OR.(IGET(335) > 0).OR.                &
                 (IGET(351) > 0).OR.(IGET(352) > 0).OR.                &
                 (IGET(353) > 0).OR.(IGET(378) > 0))THEN
! interpolate to isentropic levels     	
                CALL P2TH(LM,DUM1D11,UH(I,J,1:LM),VH(I,J,1:LM)          &
                         ,DUM1D7,T(I,J,1:LM),DUM1D13,DUM1D12,DUM1D14    &
                         ,OMGA(I,J,1:LM),KTH,TH                         &
                         ,LTH,UTH(I,J,1:KTH),VTH(I,J,1:KTH)             &
!output
                         ,HMTH(I,J,1:KTH)                               &
                         ,TTH(I,J,1:KTH),PVTH(I,J,1:KTH)                &
                         ,SIGMATH(I,J,1:KTH),RHTH(I,J,1:KTH)            &
                         ,OTH(I,J,1:KTH))!output
              END IF
! interpolate to PV levels
              IF((IGET(336) > 0).OR.(IGET(337) > 0).OR.  &
                 (IGET(338) > 0).OR.(IGET(339) > 0).OR.  &
                 (IGET(340) > 0).OR.(IGET(341) > 0)) THEN
                CALL P2PV(LM,DUM1D13,ZMID(I,J,1:LM),T(I,J,1:LM),PMID(I,J,1:LM)     &
                         ,UH(I,J,1:LM),VH(I,J,1:LM),KPV,PV,PVPT,PVPB*PINT(I,J,LM+1)&
                         ,LPV,UPV(I,J,1:KPV),VPV(I,J,1:KPV),HPV(I,J,1:KPV)         &
!output
                         ,TPV(I,J,1:KPV),PPV(I,J,1:KPV),SPV(I,J,1:KPV) )   !output
              END IF        

            END DO
          END DO         

!-----------------------------------------------------------------
!add A-grid regional models
         ELSE     !regional models start here (GFS or global ends here)
          DO J=JSTA_m,Jend_m
            JMT2=JM/2+1
            TPHI=(J-JMT2)*(DYVAL/gdsdegr)*DTR
            DO I=2,im-1
               ip1 = i + 1
               im1 = i - 1
                tem = wrk3(i,j) * eradi
!$omp parallel do private(l,es)
                DO L=1,LM
                  DUM1D5(L)  = T(I,J,L)*(1.+D608*Q(I,J,L))                    !Tv
                  ES         = min(FPVSNEW(T(I,J,L)),PMID(I,J,L))
                  DUM1D14(L) = Q(I,J,L) * (PMID(I,J,L)+CON_EPSM1*ES)/(CON_EPS*ES)   ! RH
                  DUM1D1(L)  = (PMID(ip1,J,L)- PMID(im1,J,L)) * wrk4(i,j) !dp/dx
                  DUM1D3(L)  = (T(ip1,J,L)   - T(im1,J,L))    * wrk4(i,j) !dt/dx
                  DUM1D2(L)  = (PMID(I,J+1,L)-PMID(I,J-1,L))  * tem        !dp/dy
                  DUM1D4(L)  = (T(I,J+1,L)-T(I,J-1,L))        * tem        !dt/dy
                  DUM1D6(L)  = ((VH(ip1,J,L)-VH(im1,J,L))* wrk2(i,j)             &
     &                       -  (UH(I,J+1,L)*COSL(I,J+1)                         &
     &                       -   UH(I,J-1,L)*COSL(I,J-1))*wrk3(i,j))*wrk1(i,j)   &
     &                       + F(I,J)  
                END DO

              IF(I==IM/2 .AND. J==JM/2)then 
                PRINT*,'SAMPLE PVETC INPUT for regional ',             &
                       'p,dpdx,dpdy,tv,dtdx,dtdy,h,u,v,vort ',         &
                       'JSTA_m,Jend_m, L= '
                DO L=1,LM
                  print*,pmid(i,j,l),dum1d1(l),dum1d2(l),dum1d5(l)      &
                        ,dum1d3(l),dum1d4(l),zmid(i,j,l),uh(i,j,l),vh(i,j,l)  &
                        ,dum1d6(l),JSTA_m,Jend_m,L
                end do
              end if

              CALL PVETC(LM,PMID(I,J,1:LM),DUM1D1,DUM1D2                      &
                        ,DUM1D5,DUM1D3,DUM1D4,ZMID(I,J,1:LM),UH(I,J,1:LM)     &
                        ,VH(I,J,1:LM),DUM1D6                                  &
                        ,DUM1D7,DUM1D8,DUM1D9,DUM1D10,DUM1D11,DUM1D12,DUM1D13)!output

              IF(I==IM/2 .AND. J==JM/2)then 
                PRINT*,'SAMPLE PVETC OUTPUT '  &
                       ,'hm,s,bvf2,pvn,theta,sigma,pvu,pvort= '
                DO L=1,LM
                  print*,dum1d7(l),dum1d8(l),dum1d9(l),dum1d10(l),dum1d11(l) &
                        ,dum1d12(l),dum1d13(l),DUM1D6(l)
                end do
              end if 

              IF((IGET(332) > 0).OR.(IGET(333) > 0).OR.                &
                 (IGET(334) > 0).OR.(IGET(335) > 0).OR.                &
                 (IGET(351) > 0).OR.(IGET(352) > 0).OR.                &
                 (IGET(353) > 0).OR.(IGET(378) > 0))THEN
! interpolate to isentropic levels     	
                CALL P2TH(LM,DUM1D11,UH(I,J,1:LM),VH(I,J,1:LM)          &
                         ,DUM1D7,T(I,J,1:LM),DUM1D13,DUM1D12,DUM1D14    &
                         ,OMGA(I,J,1:LM),KTH,TH                         &
                         ,LTH,UTH(I,J,1:KTH),VTH(I,J,1:KTH)             &
!output
                         ,HMTH(I,J,1:KTH)                               &
                         ,TTH(I,J,1:KTH),PVTH(I,J,1:KTH)                &
                         ,SIGMATH(I,J,1:KTH),RHTH(I,J,1:KTH)            &
                         ,OTH(I,J,1:KTH))!output
              END IF
! interpolate to PV levels
              IF((IGET(336) > 0).OR.(IGET(337) > 0).OR.  &
                 (IGET(338) > 0).OR.(IGET(339) > 0).OR.  &
                 (IGET(340) > 0).OR.(IGET(341) > 0)) THEN
                CALL P2PV(LM,DUM1D13,ZMID(I,J,1:LM),T(I,J,1:LM),PMID(I,J,1:LM)     &
                         ,UH(I,J,1:LM),VH(I,J,1:LM),KPV,PV,PVPT,PVPB*PINT(I,J,LM+1)&
                         ,LPV,UPV(I,J,1:KPV),VPV(I,J,1:KPV),HPV(I,J,1:KPV)         &
!output
                         ,TPV(I,J,1:KPV),PPV(I,J,1:KPV),SPV(I,J,1:KPV) )   !output
              END IF        

           ENDDO
          ENDDO
  
         ENDIF    !regional models and A-grid end here
!-----------------------------------------------------------------
        ELSE IF (GRIDTYPE == 'B')THEN
          DO L=1,LM
            CALL EXCH(VH(1:IM,JSTA_2L:JEND_2U,L))
          END DO
          DO J=JSTA_m,Jend_m
            JMT2=JM/2+1
            TPHI=(J-JMT2)*(DYVAL/gdsdegr)*DTR
            DO I=2,im-1
               ip1 = i + 1
               im1 = i - 1
               DO L=1,LM
                 DUM1D5(L) = T(I,J,L)*(1.+D608*Q(I,J,L))                 !TV
                 ES        = MIN(FPVSNEW(T(I,J,L)),PMID(I,J,L))
                 QSTL      = CON_EPS*ES/(PMID(I,J,L)+CON_EPSM1*ES)
                 DUM1D14(L) = Q(I,J,L)/QSTL                              !RH
                 DUM1D1(L)  = (PMID(ip1,J,L)- PMID(im1,J,L)) * wrk2(i,j) !dp/dx
                 DUM1D3(L)  = (T(ip1,J,L)   - T(im1,J,L))    * wrk2(i,j) !dt/dx
                 DUM1D2(L)  = (PMID(I,J+1,L)-PMID(I,J-1,L))  * wrk3(i,j) !dp/dy
                 DUM1D4(L)  = (T(I,J+1,L)-T(I,J-1,L))        * wrk3(i,j) !dt/dy
                 DVDX       = (0.5*(VH(I,J,L)+VH(I,J-1,L))-0.5*(VH(IM1,J,L) &
                            + VH(IM1,J-1,L)))*wrk2(i,j)*2.0
                 DUDY       = (0.5*(UH(I,J,L)+UH(I-1,J,L))-0.5*(UH(I,J-1,L) &
                            + UH(I-1,J-1,L)))*wrk3(i,j)*2.0
                 UAVG       = 0.25*(UH(IM1,J-1,L)+UH(IM1,J,L)               &
     &                      + UH(I,J-1,L)+UH(I,J,L))
!  is there a (f+tan(phi)/erad)*u term?
                 DUM1D6(L)  = DVDX - DUDY + F(I,J) + UAVG*TAN(TPHI)/ERAD !vort

               END DO

!               IF(I==IM/2 .AND. J==JM/2)then
!                 PRINT*,'SAMPLE PVETC INPUT '                           &
!                  ,'p,dpdx,dpdy,tv,dtdx,dtdy,h,u,v,vort= '
!                 DO L=1,LM
!                   print*,pmid(i,j,l),dum1d1(l),dum1d2(l),dum1d5(l)     &
!                   ,dum1d3(l),dum1d4(l),zmid(i,j,l),uh(i,j,l),vh(i,j,l) &
!                   ,dum1d6(l)
!                 end do
!              end if

              CALL PVETC(LM,PMID(I,J,1:LM),DUM1D1,DUM1D2                &
                  ,DUM1D5,DUM1D3,DUM1D4,ZMID(I,J,1:LM),UH(I,J,1:LM)     &
                  ,VH(I,J,1:LM),DUM1D6                                  &
                  ,DUM1D7,DUM1D8,DUM1D9,DUM1D10,DUM1D11,DUM1D12,DUM1D13)!output

!              IF(I==IM/2 .AND. J==JM/2)then
!                PRINT*,'SAMPLE PVETC OUTPUT '  &
!                 ,'hm,s,bvf2,pvn,theta,sigma,pvu= '
!                DO L=1,LM
!                  print*,dum1d7(l),dum1d8(l),dum1d9(l),dum1d10(l),dum1d11(l) &
!                    ,dum1d12(l),dum1d13(l)
!                end do
!              end if
              IF((IGET(332).GT.0).OR.(IGET(333).GT.0).OR.               &
                 (IGET(334).GT.0).OR.(IGET(335).GT.0).OR.               &
                 (IGET(351).GT.0).OR.(IGET(352).GT.0).OR.               &
                 (IGET(353).GT.0).OR.(IGET(378).GT.0))THEN
! interpolate to isentropic levels
                CALL P2TH(LM,DUM1D11,UH(I,J,1:LM),VH(I,J,1:LM)          &
                         ,DUM1D7,T(I,J,1:LM),DUM1D13,DUM1D12,DUM1D14    &
                         ,OMGA(I,J,1:LM),KTH,TH                         &
                         ,LTH,UTH(I,J,1:KTH),VTH(I,J,1:KTH)             &
!output
                         ,HMTH(I,J,1:KTH)                               &
                         ,TTH(I,J,1:KTH),PVTH(I,J,1:KTH)                &
                         ,SIGMATH(I,J,1:KTH),RHTH(I,J,1:KTH)            &
                         ,OTH(I,J,1:KTH))!output
              END IF
! interpolate to PV levels
              IF((IGET(336).GT.0).OR.(IGET(337).GT.0).OR.  &
                (IGET(338).GT.0).OR.(IGET(339).GT.0).OR.  &
                (IGET(340).GT.0).OR.(IGET(341).GT.0))THEN
                CALL P2PV(LM,DUM1D13,ZMID(I,J,1:LM),T(I,J,1:LM),PMID(I,J,1:LM)  &
                         ,UH(I,J,1:LM),VH(I,J,1:LM),KPV,PV,PVPT,PVPB*PINT(I,J,LM+1)    &
                         ,LPV,UPV(I,J,1:KPV),VPV(I,J,1:KPV),HPV(I,J,1:KPV)             &
!output
                        ,TPV(I,J,1:KPV),PPV(I,J,1:KPV),SPV(I,J,1:KPV) )   !output
              END IF
            END DO
          END DO
        ELSE IF (GRIDTYPE == 'E')THEN
          DO J=JSTA_m,Jend_m
            JMT2 = JM/2+1
            TPHI = (J-JMT2)*(DYVAL/gdsdegr)*DTR
            IHW= - MOD(J,2)
            IHE  = IHW + 1
            DO I=2,im-1
              ip1 = i + 1
              im1 = i - 1
              DO L=1,LM
                DUM1D5(L)=T(I,J,L)*(1.+D608*Q(I,J,L)) !TV
                ES=FPVSNEW(T(I,J,L))
                ES=MIN(ES,PMID(I,J,L))
                QSTL=CON_EPS*ES/(PMID(I,J,L)+CON_EPSM1*ES)
                DUM1D14(L)=Q(I,J,L)/QSTL !RH
                DUM1D1(L) = (PMIDUV(i+ihe,J,L)- PMIDUV(i+ihw,J,L))*wrk2(i,j) !dp/dx
                DUM1D3(L) = (TUV(i+ihe,J,L) - TUV(i+ihw,J,L)) * wrk2(i,j)    !dt/dx
                DUM1D2(L) = (PMIDUV(i,J+1,L)- PMIDUV(i,J-1,L))*wrk3(i,j)!dp/dy
                DUM1D4(L)=  (TUV(i,J+1,L)- TUV(i,J-1,L))*wrk3(i,j)!dt/dy
                DVDX=(VH(I+IHE,J,L)-VH(I+IHW,J,L))* wrk2(i,j)
                DUDY=(UH(I,J+1,L)-UH(I,J-1,L))* wrk3(i,j)
                UAVG=0.25*(UH(I+IHE,J,L)+UH(I+IHW,J,L)+UH(I,J-1,L)+UH(I,J+1,L))
!  is there a (f+tan(phi)/erad)*u term?
                DUM1D6(L)=DVDX-DUDY+F(I,J)+UAVG*TAN(TPHI)/ERAD !vort
              END DO

              IF(I==IM/2 .AND. J==JM/2)then
                PRINT*,'SAMPLE PVETC INPUT '  &
                 ,'p,dpdx,dpdy,tv,dtdx,dtdy,h,u,v,vort= '
                DO L=1,LM
                  print*,pmid(i,j,l),dum1d1(l),dum1d2(l),dum1d5(l)       &
                   ,dum1d3(l),dum1d4(l),zmid(i,j,l),uh(i,j,l),vh(i,j,l)  &
                   ,dum1d6(l)
                 end do
              end if

              CALL PVETC(LM,PMID(I,J,1:LM),DUM1D1,DUM1D2                &
                  ,DUM1D5,DUM1D3,DUM1D4,ZMID(I,J,1:LM),UH(I,J,1:LM)     &
                  ,VH(I,J,1:LM),DUM1D6                                  &
                  ,DUM1D7,DUM1D8,DUM1D9,DUM1D10,DUM1D11,DUM1D12,DUM1D13)!output

              IF(I==IM/2 .AND. J==JM/2)then
                PRINT*,'SAMPLE PVETC OUTPUT '  &
                 ,'hm,s,bvf2,pvn,theta,sigma,pvu= '
                DO L=1,LM
                  print*,dum1d7(l),dum1d8(l),dum1d9(l),dum1d10(l),dum1d11(l) &
                    ,dum1d12(l),dum1d13(l)
                end do
              end if
              IF((IGET(332) > 0).OR.(IGET(333) > 0).OR.                 &
                 (IGET(334) > 0).OR.(IGET(335) > 0).OR.                 &
                 (IGET(351) > 0).OR.(IGET(352) > 0).OR.                 &
                 (IGET(353) > 0).OR.(IGET(378) > 0))THEN
! interpolat e to isentropic levels
                CALL P2TH(LM,DUM1D11,UH(I,J,1:LM),VH(I,J,1:LM)          &
                         ,DUM1D7,T(I,J,1:LM),DUM1D13,DUM1D12,DUM1D14    &
                         ,OMGA(I,J,1:LM),KTH,TH                         &
                         ,LTH,UTH(I,J,1:KTH),VTH(I,J,1:KTH)             &
!output 
                         ,HMTH(I,J,1:KTH)                               &
                         ,TTH(I,J,1:KTH),PVTH(I,J,1:KTH)                &
                         ,SIGMATH(I,J,1:KTH),RHTH(I,J,1:KTH)            &
                         ,OTH(I,J,1:KTH))!output
              END IF
! interpolate to PV levels
              IF((IGET(336) > 0) .OR. (IGET(337) > 0).OR.        &
                 (IGET(338) > 0) .OR. (IGET(339) > 0).OR.        &
                 (IGET(340) > 0) .OR. (IGET(341) > 0)) THEN
                CALL P2PV(LM,DUM1D13,ZMID(I,J,1:LM),T(I,J,1:LM),PMID(I,J,1:LM)  &
                  ,UH(I,J,1:LM),VH(I,J,1:LM),KPV,PV,PVPT,PVPB*PINT(I,J,LM+1)    &
                  ,LPV,UPV(I,J,1:KPV),VPV(I,J,1:KPV),HPV(I,J,1:KPV)             &
!output
                  ,TPV(I,J,1:KPV),PPV(I,J,1:KPV),SPV(I,J,1:KPV) )   !output
              END IF
            END DO
          END DO

        END IF ! for different grids


!             print *,'in mdlthpv,af P2PV,tpv=',maxval(TPV(I,J,1:KPV)),  &
!           minval(TPV(I,J,1:KPV)),'kpv=',kpv,'zmid=',maxval(ZMID(1:im,jsta:jend,1:LM)),  &
!           minval(ZMID(1:im,jsta:jend,1:LM)),'uth=',maxval(uth(1:im,jsta:jend,1:kth)),  &
!           minval(uth(1:im,jsta:jend,1:kth)),'vth=',maxval(vth(1:im,jsta:jend,1:kth)), &
!           minval(vth(1:im,jsta:jend,1:kth)),'uth(1,1,1)=',uth(1,1,1:kth)
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!        *** PART II *** Write out fields
!---------------------------------------------------------------------
!
!     
        DO LP=1,KTH
!***  ISENTROPIC U AND/OR V WIND
!

         IF(IGET(332) > 0 .OR. IGET(333) > 0)THEN
           IF(LVLS(LP,IGET(332)) > 0 .OR. LVLS(LP,IGET(333)) > 0)THEN
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = UTH(I,J,LP)
                 GRID2(I,J) = VTH(I,J,LP)
               ENDDO
             ENDDO
             if(grib=='grib1')then
               ID(1:25)=0
               ID(11)=NINT(TH(LP))
               IF(IGET(332) > 0)CALL GRIBIT(IGET(332),LP,GRID1,IM,JM)
               ID(1:25)=0
               ID(11)=NINT(TH(LP))
               IF(IGET(333) > 0) CALL GRIBIT(IGET(333),LP,GRID2,IM,JM)
             elseif(grib=='grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld = IAVBLFLD(IGET(332))
               fld_info(cfld)%lvl  = LVLSXML(lp,IGET(332))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
               cfld = cfld + 1
               fld_info(cfld)%ifld = IAVBLFLD(IGET(333))
               fld_info(cfld)%lvl  = LVLSXML(lp,IGET(333))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID2(i,jj)
                 enddo
               enddo
             endif
          ENDIF
        ENDIF

!***  OUTPUT ISENTROPIC T
!
        IF(IGET(334) > 0)THEN
          IF(LVLS(LP,IGET(334)) > 0)THEN
!!$omp  parallel do
! GFS use lon avg as one scaler value for pole point
!            JJ=1
!            IF(JJ>=jsta .and. JJ<=jend .and. cosl(1,JJ)<SMALL)then
!	      WORK=0.
!              DO I=1,IM
!                WORK=TTH(I,JJ,LP)+WORK
!              END DO
!              DO I=1,IM
!                TTH(I,JJ,LP)=WORK/IM
!              END DO
!	    END IF
!	    JJ=JM
!            IF(JJ>=jsta .and. JJ<=jend .and. cosl(1,JJ)<SMALL)then
!	      WORK=0.
!              DO I=1,IM
!                WORK=TTH(I,JJ,LP)+WORK
!              END DO
!              DO I=1,IM
!                TTH(I,JJ,LP)=WORK/IM
!              END DO
!	    END IF 
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = TTH(I,J,LP)              
              ENDDO
            ENDDO
            if(grib=='grib1')then
              ID(1:25)=0
              ID(11)=NINT(TH(LP))
              CALL GRIBIT(IGET(334),LP,GRID1,IM,JM)
            elseif(grib=='grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(334))
              fld_info(cfld)%lvl=LVLSXML(lp,IGET(334))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF
!     
!***  ISENTROPIC PV
!
        IF(IGET(335) > 0) THEN
          IF(LVLS(LP,IGET(335)) > 0)THEN
            call poleavg(IM,JM,JSTA,JEND,SMALL,COSL(1:IM,JSTA:JEND)  &
                        ,SPVAL,PVTH(1:IM,JSTA:JEND,LP))
             IF(1>=jsta .and. 1<=jend)print*,'PVTH at N POLE= '       &
               ,pvth(1,1,lp),pvth(im/2,1,lp)                          &
               ,pvth(10,10,lp),pvth(im/2,10,lp),SPVAL,grib,LP
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 IF(PVTH(I,J,LP) /= SPVAL)THEN
                   GRID1(I,J) = PVTH(I,J,LP)*1.0E-6
                 ELSE
                   GRID1(I,J) = PVTH(I,J,LP)
                 END IF
               ENDDO
             ENDDO
           if(grib=='grib1')then
             ID(1:25)=0
             ID(11)=NINT(TH(LP))
             CALL GRIBIT(IGET(335),LP,GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(335))
            fld_info(cfld)%lvl=LVLSXML(lp,IGET(335))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
          ENDIF
        ENDIF
!     
!***  ISENTROPIC Montgomery function
!
        IF(IGET(353) > 0) THEN
          IF(LVLS(LP,IGET(353)) > 0)THEN
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = HMTH(I,J,LP)
               ENDDO
             ENDDO
           if(grib=='grib1')then
             ID(1:25)=0
             ID(11)=NINT(TH(LP))
             CALL GRIBIT(IGET(353),LP,GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(353))
            fld_info(cfld)%lvl=LVLSXML(lp,IGET(353))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
          ENDIF
        ENDIF
!     
!***  ISENTROPIC static stability
!
        IF(IGET(351) > 0) THEN
          IF(LVLS(LP,IGET(351)) > 0)THEN
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = SIGMATH(I,J,LP)
               ENDDO
             ENDDO
            if(grib=='grib1') then
             ID(1:25)=0
             ID(11)=NINT(TH(LP))
             CALL GRIBIT(IGET(351),LP,GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(351))
            fld_info(cfld)%lvl=LVLSXML(lp,IGET(351))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
          ENDIF
        ENDIF
!     
!***  ISENTROPIC RH
!
        IF(IGET(352) > 0) THEN
          IF(LVLS(LP,IGET(352)) > 0)THEN
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 IF(RHTH(I,J,LP) /= SPVAL) THEN
                   GRID1(I,J) = 100.0 * MIN(1.,MAX(RHmin,RHTH(I,J,LP)))
                 ELSE
                   GRID1(I,J) = SPVAL
                 END IF
               ENDDO
             ENDDO
           if(grib=='grib1') then
             ID(1:25)=0
             ID(11)=NINT(TH(LP))
             CALL GRIBIT(IGET(352),LP,GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(352))
            fld_info(cfld)%lvl=LVLSXML(lp,IGET(352))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
          ENDIF
        ENDIF
!     
!***  ISENTROPIC OMEGA
!
        IF(IGET(378) > 0) THEN
          IF(LVLS(LP,IGET(378)) > 0)THEN
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = OTH(I,J,LP)
               ENDDO
             ENDDO
             if(grib=='grib1') then
             ID(1:25)=0
             ID(11)=NINT(TH(LP))
             CALL GRIBIT(IGET(378),LP,GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(378))
            fld_info(cfld)%lvl=LVLSXML(lp,IGET(378))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
          ENDIF
        ENDIF
       END DO ! end loop for isentropic levels
! Lopp through PV levels now      
       DO LP=1,KPV
!***  U AND/OR V WIND on PV surface
!
        IF(IGET(336) > 0.OR.IGET(337) > 0)THEN
          IF(LVLS(LP,IGET(336)) > 0.OR.LVLS(LP,IGET(337)) > 0)THEN
! GFS use lon avg as one scaler value for pole point
            call poleavg(IM,JM,JSTA,JEND,SMALL,COSL(1:IM,JSTA:JEND)   &
                        ,SPVAL,VPV(1:IM,JSTA:JEND,LP))
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = UPV(I,J,LP)
                 GRID2(I,J) = VPV(I,J,LP)
               ENDDO
            ENDDO
            if(grib=='grib1') then
            ID(1:25)=0
            ID(11)=NINT(PV(LP)*1000.)
            IF(IGET(336) > 0) CALL GRIBIT(IGET(336),LP,GRID1,IM,JM)
            ID(1:25)=0
            ID(11)=NINT(PV(LP)*1000.)
            IF(IGET(337) > 0) CALL GRIBIT(IGET(337),LP,GRID2,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(336))
            fld_info(cfld)%lvl=LVLSXML(lp,IGET(336))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(337))
            fld_info(cfld)%lvl=LVLSXML(lp,IGET(337))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID2(i,jj)
              enddo
            enddo
           endif
          ENDIF
        ENDIF

!***  T on constant PV
!

        IF(IGET(338) > 0)THEN
          IF(LVLS(LP,IGET(338)) > 0)THEN
! GFS use lon avg as one scaler value for pole point
            call poleavg(IM,JM,JSTA,JEND,SMALL,COSL(1:IM,JSTA:JEND)  &
                        ,SPVAL,TPV(1:IM,JSTA:JEND,LP))
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = TPV(I,J,LP)              
              ENDDO
            ENDDO
            if(grib=='grib1') then
            ID(1:25)=0
            ID(11)=NINT(PV(LP)*1000.)
            CALL GRIBIT(IGET(338),LP,GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(338))
            fld_info(cfld)%lvl=LVLSXML(lp,IGET(338))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
          ENDIF
        ENDIF
!     
!***  Height on constant PV
!
          IF(IGET(339) > 0) THEN
            IF(LVLS(LP,IGET(339)) > 0)THEN
! GFS use lon avg as one scaler value for pole point
              call poleavg(IM,JM,JSTA,JEND,SMALL,COSL(1:IM,JSTA:JEND)  &
                          ,SPVAL,HPV(1:IM,JSTA:JEND,LP))
!$omp parallel do private(i,j)
               DO J=JSTA,JEND
                 DO I=1,IM
                   GRID1(I,J) = HPV(I,J,LP)
                 ENDDO
               ENDDO
              if(grib=='grib1') then
                ID(1:25)=0
                ID(11)=NINT(PV(LP)*1000.)
                CALL GRIBIT(IGET(339),LP,GRID1,IM,JM)
              elseif(grib=='grib2') then
               cfld=cfld+1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(339))
               fld_info(cfld)%lvl=LVLSXML(lp,IGET(339))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
              endif
            ENDIF
          ENDIF
!     
!***  Pressure on constant PV
!
          IF(IGET(340) > 0) THEN
            IF(LVLS(LP,IGET(340)) > 0)THEN
! GFS use lon avg as one scaler value for pole point
              call poleavg(IM,JM,JSTA,JEND,SMALL,COSL(1:IM,JSTA:JEND)  &
                          ,SPVAL,PPV(1:IM,JSTA:JEND,LP)) 
!$omp parallel do private(i,j)
               DO J=JSTA,JEND
                 DO I=1,IM
                   GRID1(I,J) = PPV(I,J,LP)
                 ENDDO
               ENDDO
               if(grib=='grib1') then
               ID(1:25)=0
               ID(11)=NINT(PV(LP)*1000.)
               CALL GRIBIT(IGET(340),LP,GRID1,IM,JM)
             elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(340))
              fld_info(cfld)%lvl=LVLSXML(lp,IGET(340))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
             endif
            ENDIF
          ENDIF
!     
!***  Wind Shear on constant PV
!
          IF(IGET(341) > 0) THEN
            IF(LVLS(LP,IGET(341)) > 0)THEN
! GFS use lon avg as one scaler value for pole point
              call poleavg(IM,JM,JSTA,JEND,SMALL,COSL(1:IM,JSTA:JEND)   &
                          ,SPVAL,SPV(1:IM,JSTA:JEND,LP))
!$omp parallel do private(i,j)
               DO J=JSTA,JEND
                 DO I=1,IM
                   GRID1(I,J) = SPV(I,J,LP)
                 ENDDO
               ENDDO
               if(grib=='grib1') then
               ID(1:25)=0
               ID(11)=NINT(PV(LP)*1000.)
               CALL GRIBIT(IGET(341),LP,GRID1,IM,JM)
             elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(341))
              fld_info(cfld)%lvl=LVLSXML(lp,IGET(341))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
             endif
            ENDIF
          ENDIF

         END DO ! end loop for constant PV levels
       
         DEALLOCATE(DUM1D1,DUM1D2,DUM1D3,DUM1D4,DUM1D5,DUM1D6,DUM1D7, &
                    DUM1D8,DUM1D9,DUM1D10,DUM1D11,DUM1D12,DUM1D13,    &
                    DUM1D14,wrk1, wrk2, wrk3, wrk4, cosl)

      END IF ! end of selection for isentropic and constant PV fields	
      if(me==0)print *,'end of MDL2THandpv'
!
!     
!     END OF ROUTINE.
!
      RETURN
      END
