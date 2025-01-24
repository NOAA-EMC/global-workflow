real(r_kind), dimension(:,:,:),pointer:: V
real(r_kind), dimension(:,:,:),pointer:: VALL
real(r_kind), dimension(:,:,:),pointer:: HALL
real(r_kind), dimension(:,:,:),pointer:: a_diff_f
real(r_kind), dimension(:,:,:),pointer:: a_diff_h
real(r_kind), dimension(:,:,:),pointer:: b_diff_f
real(r_kind), dimension(:,:,:),pointer:: b_diff_h
real(r_kind), dimension(:,:),pointer:: p_eps
real(r_kind), dimension(:,:),pointer:: p_del
real(r_kind), dimension(:,:),pointer:: p_sig
real(r_kind), dimension(:,:),pointer:: p_rho
real(r_kind), dimension(:,:,:),pointer:: paspx
real(r_kind), dimension(:,:,:),pointer:: paspy
real(r_kind), dimension(:,:,:),pointer:: pasp1
real(r_kind), dimension(:,:,:,:),pointer:: pasp2
real(r_kind), dimension(:,:,:,:,:),pointer:: pasp3
real(r_kind), dimension(:,:,:),pointer:: vpasp2
real(r_kind), dimension(:,:,:),pointer:: hss2
real(r_kind), dimension(:,:,:,:),pointer:: vpasp3
real(r_kind), dimension(:,:,:,:),pointer:: hss3
real(r_kind), dimension(:),pointer:: ssx
real(r_kind), dimension(:),pointer:: ssy
real(r_kind), dimension(:),pointer:: ss1
real(r_kind), dimension(:,:),pointer:: ss2
real(r_kind), dimension(:,:,:),pointer:: ss3
integer(fpi), dimension(:,:,:),pointer:: dixs
integer(fpi), dimension(:,:,:),pointer:: diys
integer(fpi), dimension(:,:,:),pointer:: dizs
integer(fpi), dimension(:,:,:,:),pointer:: dixs3
integer(fpi), dimension(:,:,:,:),pointer:: diys3
integer(fpi), dimension(:,:,:,:),pointer:: dizs3
integer(fpi), dimension(:,:,:,:),pointer:: qcols
integer(i_kind),dimension(:),pointer:: iref,jref
integer(i_kind),dimension(:),pointer:: Lref,Lref_h
real(r_kind),dimension(:),pointer:: cvf1,cvf2,cvf3,cvf4
real(r_kind),dimension(:),pointer:: cvh1,cvh2,cvh3,cvh4
real(r_kind),dimension(:),pointer:: cx0,cx1,cx2,cx3
real(r_kind),dimension(:),pointer:: cy0,cy1,cy2,cy3
real(r_kind),dimension(:),pointer:: p_coef,q_coef
real(r_kind),dimension(:),pointer:: a_coef,b_coef
real(r_kind),dimension(:,:),pointer:: cf00,cf01,cf02,cf03           &
                                     ,cf10,cf11,cf12,cf13           &
                                     ,cf20,cf21,cf22,cf23           &
                                     ,cf30,cf31,cf32,cf33
