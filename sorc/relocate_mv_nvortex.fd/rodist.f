        subroutine rodist
        parameter(nmx=24)
        common /vect/rovect(nmx),xvect(nmx),yvect(nmx)
       COMMON /POSIT/  XOLD,YOLD,XCORN,YCORN
c
c        write(6,*) 'rovect',rovect
        pi=4.0*atan(1.0)
        PI180 = 4.*ATAN(1.0)/180.
        yo=yold*pi180
c qliu        fact=cos(yo)
        fact=1.0
        xc=xold-xcorn
        yc=yold-ycorn
c
        do 10 ip=1,nmx
c
        theta=float(ip-1)/float(nmx)*2.*pi
        r=rovect(ip)
c
        xvect(ip)=r*cos(theta)/fact +xc
        yvect(ip)=r*sin(theta) +yc
10      continue
c
        return
        end
