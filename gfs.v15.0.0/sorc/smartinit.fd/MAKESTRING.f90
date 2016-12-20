      SUBROUTINE MAKESTRING(IRAIN,ISNOW,IZR,IIP,LIFT,PROB,GDIN,STRING,GRIDWX,VALIDPT)
      use grddef
      use constants
      TYPE (GINFO), INTENT(IN) :: GDIN
      INTEGER,   INTENT(IN) :: IRAIN(:,:),ISNOW(:,:),IZR(:,:),IIP(:,:)
      REAL,      INTENT(IN) :: LIFT(:,:),PROB(:,:)
      REAL,      INTENT(INOUT) ::  GRIDWX(:,:)
      LOGICAL,   INTENT(IN)  ::  VALIDPT(:,:)
      CHARACTER*50, INTENT(INOUT) :: STRING(:,:)
      CHARACTER*50  ,TRACK(200),TEMP
      REAL,      ALLOCATABLE ::  WX(:,:),THUNDER(:,:)
      INTEGER START
      INTERFACE
       SUBROUTINE mkwxstr(cwx,probwx,thnd,str)
!      set wx prob text
       REAL thnd,probwx
       CHARACTER cwx*3,cwxstr*15,str*50,setstr*50
       END SUBROUTINE mkwxstr
       FUNCTION setstr(cwstr,thnd)
!      set wx type and prob text
       REAL thnd
       CHARACTER cwstr*15,setstr*50
       END FUNCTION setstr
     END INTERFACE

      IM=GDIN%imax;JM=GDIN%jmax
!  weather type
!  1-rain 2-snow 3-frzrain 4-sleet 5-rw 6-sw 7-frzrainshower
!    8-sleetshower we won't use mixes
         ALLOCATE(WX(IM,JM),THUNDER(IM,JM),stat=kret)
         print *,'IN WXSTRING, lift,irain',maxval(lift),maxval(irain)
         WX=0.;GRIDWX=0.
         WHERE (IRAIN .EQ. 1) WX=1.
         WHERE (ISNOW .EQ. 1) WX=2.
         WHERE (IZR .EQ. 1) WX=3.
         WHERE (IIP .EQ. 1) WX=4.
         WHERE (WX.GT.0. .AND. LIFT.LT.2) WX=WX+4.

!      thunder is totally separate from PoP, only related to
!       the instability. SChc  for LI <-1, Chc for LI<-3,
!       Lkly for LI<-5, Def for LI<-8

         WHERE (LIFT.LE.-8.);     THUNDER=4.
         ELSE WHERE (LIFT.LE.-5.); THUNDER=3.
         ELSE WHERE (LIFT.LE.-3);  THUNDER=2.
         ELSE WHERE (LIFT.LE.-1);  THUNDER=1.
         ELSE WHERE; THUNDER=0.
         ENDWHERE
         print *,'IN WXSTRING, wx,thunder,prob ',maxval(wx),maxval(thunder),maxval(prob)
      
        DO J=1,JM
        DO I=1,IM
         IF (WX(I,J).EQ. 0.) THEN
           STRING(I,J)="<NoCov>:<NoWx>:<NoInten>:<NoVis>:"
           GRIDWX(I,J)=0.
         ELSE IF (WX(I,J).EQ.1) THEN
           call mkwxstr("R  ",prob(i,j),thunder(i,j),string(i,j))
         ELSE IF (WX(I,J).EQ.2) THEN
          call mkwxstr("S  ",prob(i,j),thunder(i,j),string(i,j))
         ELSE IF (WX(I,J).EQ.3) THEN
           call mkwxstr("ZR ",prob(i,j),thunder(i,j),string(i,j))
         ELSE IF (WX(I,J).EQ.4) THEN
           call mkwxstr("IP ",prob(i,j),thunder(i,j),string(i,j))
         ELSE IF (WX(I,J).EQ.5) THEN
           call mkwxstr("RW ",prob(i,j),thunder(i,j),string(i,j))
         ELSE IF (WX(I,J).EQ.6) THEN
           call mkwxstr("SW ",prob(i,j),thunder(i,j),string(i,j))
         ELSE IF (WX(I,J).EQ.7) THEN
           call mkwxstr("ZRW",prob(i,j),thunder(i,j),string(i,j))
         ELSE IF (WX(I,J).EQ.8) THEN
           call mkwxstr("IPW",prob(i,j),thunder(i,j),string(i,j))
         ELSE
           STRING(I,J)="<NoCov>:<NoWx>:<NoInten>:<NoVis>:"
           GRIDWX(I,J)=0.
         ENDIF
      
       ENDDO
       ENDDO

       TRACK(1)="<NoCov>:<NoWx>:<NoInten>:<NoVis>:"
       TRACK(2:200)="empty"

       START=2
       DO J=1,JM
       DO I=1,IM
         temp=string(i,j)
         do l=1,200
           if (temp.EQ.TRACK(L)) GOTO 444
         enddo
         if(start.gt.200) stop 'TRACK ARRAY EXCEEDED IN WXSTRING'
         TRACK(START)=TEMP
         START=START+1
 444     CONTINUE
       ENDDO
       ENDDO       

       do J=1,JM
       do I=1,IM
         if(validpt(i,j)) then
         do l=1,200
          if (STRING(I,J).EQ.TRACK(L)) THEN
           GRIDWX(I,J)=L
           goto 555
          endif
         enddo
  555    continue
         else
           GRIDWX(I,J)=SPVAL
         endif
       ENDDO   
       ENDDO      

       RETURN
       END

       SUBROUTINE mkwxstr(cwx,probwx,thnd,str)
!      set wx prob text
       REAL thnd,probwx
       CHARACTER cwx*3,cwxstr*15,str*50,setstr*50

       IF (PROBWX.LT.25.) THEN
         cwxstr="SChc:"//TRIM(cwx)
       ELSE IF (PROBWX.LT.55.) THEN
         cwxstr="Chc:"//TRIM(cwx)
       ELSE IF (PROB.LT.75.) THEN
         cwxstr="Lklyc:"//TRIM(cwx)
       ELSE
         cwxstr="Def:"//TRIM(cwx)
       ENDIF
       str=setstr(cwxstr,thnd)
       RETURN
       END SUBROUTINE mkwxstr

       FUNCTION setstr(cwstr,thnd)
!      set wx type and prob text
       REAL thnd
       CHARACTER cwstr*15,setstr*50

       IF (THND.EQ.0.) THEN
         setstr=CWSTR//":-:<NoVis>:"
       ELSE IF (THND.EQ.1.) THEN
         setstr=CWSTR//":-:<NoVis>^SChc:T:-:<NoVis>:"
       ELSE IF (THND.EQ.2.) THEN
         setstr=CWSTR//":-:<NoVis>^Chc:T:-:<NoVis>:"
       ELSE IF (THND.EQ.3.) THEN
         setstr=CWSTR//":-:<NoVis>^Lkly:T:-:<NoVis>:"
       ELSE
         setstr=CWSTR//":-:<NoVis>^Def:T:-:<NoVis>:"
       ENDIF
       END FUNCTION setstr
