################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	anomgb
FOPTS=  -O3   -qrealsize=8 -qnosave
LOPTS=  -qsmp -lessl
INCS=	
OBJS=	anomgb.o
LIBS=  -L$(LIBDIR) -lw3_d -lip_d -lsp_d -lbacio_4	
################################################################################
$(CMD):	$(OBJS)
	ncepxlf $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):	$(INCS)
anomgb.o:	anomgb.f
	$(FCMP) -c $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
