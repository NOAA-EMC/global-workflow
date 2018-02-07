      subroutine readgrib(grido,kp5,kp6,kp7,lgb,lgi,khem)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:   readgrib     get grib records 
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
C   LANGUAGE: FORTRAN                                                           
C   MACHINE:  NAS                                                               
C                                                                               
C$$$                                                                            
      parameter (im=360,jm=181,ijm=im*jm)
      integer kpds(100), jpds(100), kgds(30), jgds(30)
      logical lbms(im,jm)
      dimension gridi(im,jm), grido(145,37)
      dimension grid(144,73)

C                                                                               
C                                                                               
      lupgb = lgb
      lupgi = lgi

      lskip = -1
      do jj = 1, 100
        jpds(jj) = -1
      enddo
      do jj = 1, 30
        jgds(jj) = -1
      enddo
c
c  Extract 1-deg by 1-deg field 
c
      jpds(5) = kp5
      jpds(6) = kp6
      jpds(7) = kp7
      call getgb(lupgb,lupgi,ijm,-1,jpds,jgds,ndata,
     &  lskip,kpds,kgds,lbms,gridi,ierr)
      IF(IERR.NE.0) GO TO 500
c
c  interpolate to 2.5 by 2.5 deg field
c
      call la2la(gridi,im,jm,grid,144,73)
c
c  put in hemisphere grid and reverse the order
c
      jlm = 38
      if(khem.eq.2) jlm = 74
!$OMP PARALLEL DO

      do j = 1, 37
        jin = jlm - j
        do i = 1, 144
          grido(i,j) = grid(i,jin)
        enddo
        grido(145,j) = grido(1,j)
      enddo
      return
 500  print *, ' getgb error'
      print *,'  lupgb = ', lupgb, ' lupgi = ', lupgi
      print *, ' ierr =', ierr
      call errexit (2)
      end
