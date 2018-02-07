       SUBROUTINE SEPAR(XD,XM)
C
C  SEPERATES A FIELD INTO HURRICANE COMPONENT AND REMAINDER
C

       PARAMETER( NMX=24,nmx1=nmx+1,nmx2=nmx*2,nmx6=nmx*6)
       PARAMETER (IMX=41 , JMX=41)
       DIMENSION XR(NMX),XD(IMX,JMX)
CC
       COMMON /POSIT/  XOLD,YOLD,XCORN,YCORN
       COMMON /XXX/  XF(IMX,JMX),XC,YC,DX,DY
       dimension XM(IMX,JMX)
C
c new arrays
        dimension b(nmx),w(nmx),ab(nmx,nmx1),ipvt(nmx)
     1      ,wrk(nmx6),iwrk(nmx2)
        common /matrix/ a(nmx,nmx),capd2
        common /vect/rovect(nmx),xvect(nmx),yvect(nmx)
c
        DATA XR/24*0./
C
C  XC,YC ARE HURRICANE COORDINATES
C  RO  IS RADIUS AT WHICH HURRICANE COMPONENT OF FIELD GOES TO ZERO
C  XR ARRAY CONTAINS THE FIELD VALUES OF 12 EQUALLY SPACED POINTS
C     ON CIRCLE OF RADIUS RO CENTERED AT XC,YC
C
c  set ro to be max value of rovect
c
        ro=0.
        do 22 i=1,nmx
        ro=amax1(ro,rovect(i))
22       continue
          PI = 4.*ATAN(1.0)
       PI180 = 4.*ATAN(1.0)/180.
c qliu       FACT =  COS(YOLD*PI180)
       FACT = 1.0
c        DDEL=1.0*PI180
c        DTHA=1.0*PI180
CC
CC   XC IS THE I POSITION OF THE CENTER OF THE OLD VORTEX
CC   YC IS THE J POSITION OF THE CENTER OF THE OLD VORTEX
CC   DDEL IS THE LONG. IN RADIANS OF THE OUTER NEST
CC   DTHA IS THE LAT.  IN RADIANS OF THE OUTER NEST
CC
c no fact here
c      DX=FACT*DDEL/PI180
c
c       dx=ddel/pi180
c       DY=DTHA/PI180
       dx=1.0
       DY=1.0
cc
       XC = (XOLD-XCORN)*DX
       YC = (YOLD-YCORN)*DY
       IS=INT((XC-RO/fact)/DX) +1.
       IE=INT((XC+RO/fact)/DX + 1.)
       JS=INT((YC-RO)/DY) +1.
       JE=INT((YC+RO)/DY + 1.)
C
        DO 1 J = 1 , JMX
        DO 1 I = 1 , IMX
          XF(I,J)  = XD(I,J)
1       CONTINUE
C
C  SUBROUTINE BOUND COMPUTES FIELD VALUES OF ARRAY XR USING
C         BILINEAR INTERPOLATION
C
c 
        CALL BOUND(NMX,XR,rovect)
      
C
c  xrop(nmx) are the interpolated values of the disturbance
c   field at the rovect pts
c
c romax is the maximum value in rovect(nmx). Within the loop a local
c ro is computed for use in the separation. At the start of the loop
c ro is again set to romax to define the domain.
c
c
c
        w=0.
        romax=ro
C
        DO 10 IX=IS,IE
        DO 11 JY=JS,JE
             ro=romax
c            X=XC-RO +DX*(IX-IS)
c            Y=YC-RO +DY*(JY-JS)
             X= DX*float(IX) -1.
             Y= DY*float(JY) -1.
              delx=(x-xc)*fact
              dely=(y-yc)
             DR=SQRT((delx)**2 +(dely)**2)
             IF(DR.GT.RO)GOTO11
               IF(delx.ne.0.) THETA=ATAN((dely)/(delx))
               if(delx.eq.0..and.dely.lt.0.)theta=270.*pi180
               if(delx.eq.0..and.dely.gt.0.)theta=90. *pi180
             IF(delx.LT.0.)THETA=THETA+PI
             IF(THETA.LT.0.)THETA=2.*PI+THETA
             N1=INT(THETA*NMX/(2.*PI))
             IF(N1.GT.nmx)write(6,*)N1,THETA*57.296
             IF(N1.LT.0)write(6,*) N1,THETA*57.296
             N2=N1+2
             IF(N2.GT.NMX)N2=N2-NMX
             DELTH=THETA- 2.*PI*FLOAT(N1)/FLOAT(NMX)
c
             ro=delth*float(nmx)/(2.*pi)*(rovect(n2)-rovect(n1+1))
     1             +rovect(n1+1)
             IF(DR.GT.ro)GOTO11
             XRO=DELTH*FLOAT(NMX)/(2.*PI)*(XR(N2)-XR(N1+1)) +XR(N1+1)
CC
c Now add new code to compute distance from each gridpt. to rovect pts
c
             do 12 ip=1,nmx
             dpij= (fact*(x-xvect(ip)))**2 +(y-yvect(ip))**2
             b(ip)=exp(-dpij/capd2)
12           continue
c
c
             do 44 ip=1,nmx
               do 43 jp=1,nmx
43               ab(ip,jp)=a(ip,jp)
               ab(ip,nmx1)=b(ip)
44           continue
c
c solve system using constrained least squares method
c
             call wnnls(ab,nmx,0,nmx,nmx,0,1.,w,rnm,md,iwrk,wrk)
c
             temp=0.
             do 20 ip=1,nmx
             temp=temp +w(ip)*xr(ip)
20           continue
c            xh(ix,jy)=xf(ix,jy)-temp
c qliu             xd(ix,jy)=temp
             xm(ix,jy)=temp
11     CONTINUE
10     CONTINUE
c       write(6,*) 'qliu test2'
       do j=1,jmx
       do i=1,imx
c          write(6,*) xf(i,j),xd(i,j),xf(i,j)-xd(i,j),i,j
       end do
       end do 
       RETURN
       END
