      subroutine landcmsk(IK,JK,GLON,GLAT,ZDATG,IFLAG,lsflag,kst) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      parameter (IK=384,JK=190,ijdim=IK*JK,NSG=24000,NST=10)
      parameter (NSG=54000,NST=10)
      DIMENSION ZDATG(IK,JK),GLON(IK,JK),GLAT(IK,JK) 
      DIMENSION ING(NSG),JNG(NSG)
      CHARACTER ST_NAME(NST)*3
      COMMON /TR/ING,JNG,IB
      COMMON /NHC2/MDX,MDY
      COMMON /NHC3/AMDX,AMDY
      COMMON /STNAME/ST_NAME
      COMMON /CHEN/KUNIT,ITIM
c
      lsflag = 1

      DO I = 1,IB
        IW = ING(I)
        JW = JNG(I)
        IF(ZDATG(IW,JW).gt.500.)then
          iflag = 1
cnew          MDX=0
cnew          MDY=0
cnew          AMDX=0.
cnew          AMDY=0.
          write(6,*)' Filter domain topography height > 500 m'
     1           ,', storm name = ', ST_NAME(KST),
     2           ', forecast time = ',ITIM,'h',
     3            ', only wind field is relocated'
          go to 50
        END IF
      END DO        
 
 50   continue


      end

