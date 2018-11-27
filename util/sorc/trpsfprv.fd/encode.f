       subroutine encode(icv,iv)
c      packs 1-4 byte integer into first four bytes of 
c     an integer variable AS ASCII!!.  Replaces
c    HDS encode function
c    for this purpose
c   WARNING.  WHAT THIS CODE IS DOING IS NOT 
c  A GOOD IDEA, CALLER LOGIC SHOULD BE CHANGED.
c  OR CALLER LOADED WITH SUCH LOGIC SHOULD BE
c  REPLACED
       character*8 cv
       integer kv
       equivalence (kv,cv)
       write(cv(1:4),9) iv
  9    format(i4)
       icv=kv
       return
       end
     
