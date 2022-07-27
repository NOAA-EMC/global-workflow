      SUBROUTINE SVP(QS,ES,P,T)                                         
C                                                                       
C  THIS IS THE BASE ROUTINE FOR ALL MOIST PROCESS CALCULATION           
C  GIVEN P (IN PASCALS) AND T (IN DEGREES K), THE SATURATION VAPOR      
C  PRESSURE ES (IN PASCALS) AND SATURATION MIXING RATIO ARE             
C  CALCULATED                                                           
C                                                                       
C  19 SEPTEMBER, 1988     HUA-LU PAN                                    
C                                                                       
      REAL L0                                                           
      parameter (cp=1004.6,cl=4185.5,cpv=1846.0)
      parameter (rv=461.5,l0=2.500e6,t0=273.16,es0=610.78)
      parameter (cps=2106.0,hfus=3.3358e5,rd=287.05)
      parameter (fact1=(CPV - CL) / RV,fact1i=(cps-cl)/rv)
      parameter (fact2=(L0 + (CL - CPV) * T0) / RV)
      parameter (fact2i=(L0 + hfus + (CL - cps) * T0) / RV)
      parameter (fact3=1. / T0,eps=rd/rv,tmix=t0-20.)
      if(t.ge.t0) then
        ES = ES0 * (T / T0) ** FACT1 *
     &       EXP ( FACT2 * (FACT3 - 1. / T))
      elseif (t.lt.tmix) then
        es = es0 * (t / t0) ** fact1i *
     &     exp ( fact2i * (fact3 - 1. / t))
      else
        w = (t - tmix) / (t0 - tmix)
        ES1 = ES0 * (T / T0) ** FACT1 *
     &       EXP ( FACT2 * (FACT3 - 1. / T))
        es2 = es0 * (t / t0) ** fact1i *
     &     exp ( fact2i * (fact3 - 1. / t))
        es = w * es1 + (1.-w) * es2
      endif
      QS = EPS * ES / (P - (1.-EPS) * ES)                               
      RETURN                                                            
      END                                                               
