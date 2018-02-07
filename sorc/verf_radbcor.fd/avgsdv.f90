subroutine avgsdv(count,sum1,sum2,rmiss)
  implicit none
  real count,sum1,sum2,rmiss
  real avg,sdv,rterm1,rterm2,svar
  
  if (count>0.) then
     rterm1 = 1./count
     avg    = rterm1*sum1
     if (count>1.) then
        rterm2 = 1./(count-1.)
        svar   = (count*sum2-sum1*sum1)*rterm1*rterm2
        if (svar>0.) then
           sdv = sqrt(svar)
        else
           sdv = rmiss
        endif
     else
        sdv = rmiss
     endif
  else
     count = rmiss
     avg   = rmiss
     sdv   = rmiss
  endif

  sum1=avg
  sum2=sdv

  return
end subroutine avgsdv

