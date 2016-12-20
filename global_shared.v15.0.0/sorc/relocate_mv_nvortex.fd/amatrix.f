        subroutine amatrix
        parameter(nmx=24)
        common /matrix/ a(nmx,nmx),capd2
       COMMON /POSIT/  XOLD,YOLD,XCORN,YCORN
        common /vect/rovect(nmx),xvect(nmx),yvect(nmx)
c
        PI180 = 4.*ATAN(1.0)/180.
        yo=yold*pi180
c qliu        fact=cos(yo)
        fact=1.0
c       capd2=(3.15)*(3.15)
        capd2=(2.25)*(2.25)
        do 10 ip=1,nmx
        do 10 jp=ip,nmx
         dpij=(fact*(xvect(ip)-xvect(jp)))**2 +(yvect(ip)-yvect(jp))**2
          a(ip,jp)= exp(-dpij/capd2)
          a(jp,ip)= a(ip,jp)
10      continue
100     format(5f8.4)
        return
        end
