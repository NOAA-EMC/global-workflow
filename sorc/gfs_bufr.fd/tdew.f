      SUBROUTINE TDEW(TD,T,Q,P)                                         
C                                                                       
C  THIS ROUTINE COMPUTES DEW POINT TEMPERATURE IN DEGREE K GIVEN        
C  INPUT OF T, Q, AND P. THE ALGORITHM USES THE SAME SATURATION         
C  VAPOR PRESSURE FORMULA AS IN VECTOR KUO SCHEME.                      
C                                                                       
C  A NEWTON-RALPSON ITERATION SCHEME IS USED                            
C                                                                       
      REAL L0                                                           
      parameter (cp=1004.6,cl=4185.5,cpv=1846.0)
      parameter (rv=461.5,l0=2.500e6,t0=273.16,es0=610.78)
      parameter (cps=2106.0,hfus=3.3358e5,rd=287.05)
      parameter (fact1=(CPV - CL) / RV,fact1i=(cps-cl)/rv)
      parameter (fact2=(L0 + (CL - CPV) * T0) / RV)
      parameter (fact2i=(L0 + hfus + (CL - cps) * T0) / RV)
      parameter (fact3=1. / T0,eps=rd/rv,tmix=t0-20.)
      EVP = P / (1. - EPS + EPS / Q)                                    
      TNEW = T                                                          
      ITER = 0                                                          
 1    continue
      if(tnew.ge.t0) then
        ES = ES0 * (TNEW / T0) ** FACT1 *                                 
     1       EXP ( FACT2 * (FACT3 - 1. / TNEW))                           
        DESDT = ES * (FACT1 / TNEW + FACT2 / TNEW ** 2)                   
      elseif(tnew.lt.tmix) then
        ES = ES0 * (TNEW / T0) ** FACT1i *                                 
     1       EXP ( FACT2i * (FACT3 - 1. / TNEW))                           
        DESDT = ES * (FACT1i / TNEW + FACT2i / TNEW ** 2)                   
      else
        w = (tnew - tmix) / (t0 - tmix)
        ES1 = ES0 * (TNEW / T0) ** FACT1 *                                 
     1       EXP ( FACT2 * (FACT3 - 1. / TNEW))                           
        DESDT1 = ES1 * (FACT1 / TNEW + FACT2 / TNEW ** 2)                   
        ES2 = ES0 * (TNEW / T0) ** FACT1i *                                 
     1       EXP ( FACT2i * (FACT3 - 1. / TNEW))                           
        DESDT2 = ES2 * (FACT1i / TNEW + FACT2i / TNEW ** 2)                   
        es = w * es1 + (1. - w) * es2
        desdt = w * desdt1 + (1. - w) * desdt2
      endif
      DES = ES - EVP                                                    
      TNEW1 = TNEW - DES / DESDT                                        
      ITER = ITER + 1                                                   
      IF(ABS(TNEW1-TNEW).LE..01) GOTO 2                                 
      IF(ITER.GT.30) THEN                                               
        WRITE(6,*) ' DEW POINT TEMPERATURE COMPUTATION DID NOT'         
        WRITE(6,*) ' CONVERGE, T, Q, P =', T, Q, P                      
        STOP ' ABEND 901'                                               
      ENDIF                                                             
      TNEW = TNEW1                                                      
      GOTO 1                                                            
 2    TD = TNEW1                                                        
      IF(TD .GT. T) TD=T
      RETURN                                                            
      END                                                               
