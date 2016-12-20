        subroutine updown(sl,zoncoef)
        use resol_def
        implicit none
        integer k,kd,ku
        real(kind=kind_evod) sl(levs),zoncoef(levs)
        real(kind=kind_evod)  xd(levs),xu(levs),sf(levs)
 
 
        do k=1,levs
          kd=max(k-1,1)
          ku=min(k+1,levs)
          sf(k)=sl(k)/(sl(ku)-sl(kd))/sqrt(2.)
        enddo
 
 
          xu(levs)=zoncoef(levs)
        do k=1,levs-1
          xu(k)=zoncoef(k+1)
        enddo
        xd(1)=zoncoef(1)
        do k=2,levs
          xd(k)=zoncoef(k-1)
        enddo
 
      do k=1,levs
        zoncoef(k)=(xu(k)-xd(k))*sf(k)
      enddo
 
      return
      end
 
 
