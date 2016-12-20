        SUBROUTINE BOUND(NMX,XR,ro)
C 
        PARAMETER (IMX=41 , JMX=41)
C 
        DIMENSION XR(NMX),ro(nmx)
        COMMON  /XXX/  XF(IMX,JMX),XC,YC,DX,DY
        COMMON  /POSIT/ XOLD,YOLD
        PI = 4.*ATAN(1.0)
c        fact=cos(yold*pi/180.)
        fact=1.0
        DO 10 I=1,NMX
        THETA= 2.*PI*FLOAT(I-1)/FLOAT(NMX)
        X=RO(i)/fact*COS(THETA)+XC +1.
        Y=RO(i)*SIN(THETA)+YC +1.
        IX=INT(X/DX)
        IY=INT(Y/DY)
        IX1=IX+1
        IY1=IY+1
        P=X/DX-FLOAT(IX)
        Q=Y/DY-FLOAT(IY)
        XR(I)=(1.-P)*(1.-Q)*XF(IX,IY) +(1.-P)*Q*XF(IX,IY+1)
     1      +  (1.-Q)*P*XF(IX+1,IY) + P*Q*XF(IX+1,IY+1)
c        write(6,*) 'QLIU TEST, BOUND=',XR(I),XF(IX,IY),XF(IX,IY+1),
c     2          XF(IX+1,IY),XF(IX+1,IY+1),I

10     CONTINUE
         RETURN
         END
