      subroutine la2la(gridi,imi,jmi,grido,imo,jmo)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    LA2LA       interpolation routine for regulat grids
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 88-03-24                     
C                                                                               
C ABSTRACT: PREVENTS UNNECESSARY REACCESSING OF ON84 DATA FIELDS.               
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   88-03-24  CAVANAUGH                                                         
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE                                 
C                                                                               
C USAGE:    CALL LABLCK (IPTR, *)                                               
C   INPUT ARGUMENT LIST:                                                        
C     IPTR     -                                                                
C                                                                               
C   OUTPUT ARGUMENT LIST:                                                       
C     IPTR     -                                                                
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: FORTRAN 90
C                                                                               
C$$$                                                                            
      dimension gridi(imi,jmi), grido(imo,jmo)
      dimension iindx1(1000), iindx2(1000)
      dimension jindx1(500), jindx2(500)
      dimension ddx(1000), ddy(500)
      real nlat
c
      dphi = 180. / float(jmi)
      dpho = 180. / float(jmo)
      dxin = 360. / float(imi)
      dxout = 360. / float(imo)
c
      wlon = 0.
      do i = 1, imo
        alamd = wlon + float(i-1) * dxout
        if(alamd.lt.0.) alamd = alamd + 360.
        if(alamd.gt.360.) alamd = alamd - 360.
        i1 = alamd / dxin + 1.0001
        iindx1(i) = i1
        i2 = i1 + 1
        if(i2.gt.imi) i2 = 1
        iindx2(i) = i2
        ddx(i) = (alamd-float(i1-1)*dxin)/dxin
      enddo
c
c  we are going southward
c  for convinience, we set lat to 0 at north pole and positive southward
c    as is in the 1-deg by 1 deg grid
c
      nlat = 0.
      do j = 1, jmo
        alatd = nlat + float(j-1) * dpho
        j1 = alatd / dphi + 1.0001
        if(j1.lt.1) j1 = 1
        jindx1(j) = j1
        j2 = j1 + 1
        if(j2.gt.jmi) j2 = jmi
        jindx2(j) = j2
        ddy(j) = (alatd-float(j1-1)*dphi)/dphi
      enddo
      do j = 1, jmo
        y = ddy(j)
        j1 = jindx1(j)
        j2 = jindx2(j)
        do i = 1, imo
          x = ddx(i)
          i1 = iindx1(i)
          i2 = iindx2(i)
          w1 = (1. - y) * (1. - x)
          w2 = (1. - y) * x
          w3 = y * (1. - x)
          w4 = y * x
          grido(i,j) = w1 * gridi(i1,j1) + w2 * gridi(i2,j1)
     &               + w3 * gridi(i1,j2) + w4 * gridi(i2,j2)
        enddo
      enddo
      return
      end
