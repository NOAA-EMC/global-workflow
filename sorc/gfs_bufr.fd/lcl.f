      SUBROUTINE LCL(TLCL,PLCL,T,P,Q)                                   
C                                                                       
C  LIFTING CONDENSATION LEVEL ROUTINE                                   
C                                                                       
      REAL L0, KAPPA                                                    
      parameter (dtdp=4.5e-4,kappa=.286,g=9.81)
      parameter (cp=1004.6,cl=4185.5,cpv=1846.0)
      parameter (rv=461.5,l0=2.500e6,t0=273.16,es0=610.78)
      parameter (cps=2106.0,hfus=3.3358e5,rd=287.05)
      parameter (fact1=(CPV - CL) / RV,fact1i=(cps-cl)/rv)
      parameter (fact2=(L0 + (CL - CPV) * T0) / RV)
      parameter (fact2i=(L0 + hfus + (CL - cps) * T0) / RV)
      parameter (fact3=1. / T0,eps=rd/rv,tmix=t0-20.)
      DESDT(ES,T) = ES * (FACT1 / T + FACT2 / T ** 2)                   
      DESDTi(ES,T) = ES * (FACT1i / T + FACT2i / T ** 2)                   
      ITER = 0                                                          
      CALL TDEW(TG,T,Q,P)                                               
 5    CALL SVP(QS,ES,P,TG)                                              
      DES = DESDT(ES,TG)                                                
      if(tg.ge.t0) then
        des = desdt(es,tg)
      elseif(tg.lt.tmix) then
        des = desdti(es,tg)
      else
        w = (tg - tmix) / (t0 - tmix)
        des = w * desdt(es,tg) + (1.-w) * desdti(es,tg)
      endif
      FT = P * (TG / T) ** KAPPA                                        
      DFT = KAPPA * FT / TG                                             
      GT = (EPS + Q * (1. - EPS)) * ES - Q * FT                         
      DGT = (EPS + Q * (1. - EPS)) * DES - Q * DFT                      
      DTG = GT / DGT                                                    
c     WRITE(6,*) ' ITER, DTG =', ITER, DTG                              
      TG = TG - DTG                                                     
      IF(ABS(DTG).LT..1) GOTO 10                                        
      ITER = ITER + 1                                                   
      IF(ITER.GT.30) THEN                                               
        WRITE(6,*) ' LCL ITERATION DIVERGES'                            
        STOP 'ABEND 101'                                                
      ENDIF                                                             
      GOTO 5                                                            
 10   TLCL = TG                                                         
      PLCL = P * (TLCL / T) ** KAPPA                                    
      RETURN                                                            
      END                                                               
