SHELL = /bin/sh

# Flag variable so the configure.nems knows we're in NEMS.  This is
# needed by the FV3 to allow the NEMS configure file to be the same
# file as the FV3 configure file.
export InNemsMakefile=YES

# variables that need to move between makefiles on different levels of the build tree
# via shell environment variables
export TOP := $(shell pwd)

# check if configure.nems file exists

exist := $(wildcard ./conf/configure.nems)
ifneq ($(strip $(exist)),)
  include ./conf/configure.nems
else
  $(error Error: !!!!!! Configure file is missing. Please create ./conf/configure.nems  by running ./configure !!!!!!)
endif

# conditionally read in the paths for external components

ifndef NEMSAppBuilder
  exist := $(wildcard ./conf/externals.nems)
  ifneq ($(strip $(exist)),)
    include ./conf/externals.nems
  else
    $(error Error: !!!!!! Externals file is missing. Please create ./conf/externals.nems  by running ./configure !!!!!!)
  endif
endif

# Need a variable that contains only a comma for some of the operations below
COMMA := ,

# external COMP dependencies ###################################################
# ATMs
ifneq (,$(findstring $(COMMA)satm$(COMMA),$(COMP)))
  include $(satm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SATM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)xatm$(COMMA),$(COMP)))
  include $(xatm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XATM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)datawam$(COMMA),$(COMP)))
  include $(datawam_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_DATAWAM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)gsm$(COMMA),$(COMP)))
  include $(gsm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_GSM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)fv3$(COMMA),$(COMP)))
  include $(fv3_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_FV3=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)nmmb(COMMA),$(COMP)))
  include $(nmmb_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_NMMB=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
# OCNs
ifneq (,$(findstring $(COMMA)socn$(COMMA),$(COMP)))
  include $(socn_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SOCN=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)xocn$(COMMA),$(COMP)))
  include $(xocn_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XOCN=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)hycom$(COMMA),$(COMP)))
  include $(hycom_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_HYCOM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)mom5$(COMMA),$(COMP)))
  include $(mom5_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_MOM5=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)pom$(COMMA),$(COMP)))
  include $(pom_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_POM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
# SEAICEs
ifneq (,$(findstring $(COMMA)sice$(COMMA),$(COMP)))
  include $(sice_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SICE=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)xice$(COMMA),$(COMP)))
  include $(xice_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XICE=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)cice$(COMMA),$(COMP)))
  include $(cice_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_CICE=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
# WAVs
ifneq (,$(findstring $(COMMA)swav$(COMMA),$(COMP)))
  include $(swav_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SWAV=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)xwav$(COMMA),$(COMP)))
  include $(xwav_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XWAV=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)ww3$(COMMA),$(COMP)))
  include $(ww3_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_WW3=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
# LNDs
ifneq (,$(findstring $(COMMA)slnd$(COMMA),$(COMP)))
  include $(slnd_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SLND=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)xlnd$(COMMA),$(COMP)))
  include $(xlnd_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XLND=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)noah$(COMMA),$(COMP)))
  include $(noah_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_NOAH=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)noahmp$(COMMA),$(COMP)))
  include $(noahmp_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_NOAHMP=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)lis$(COMMA),$(COMP)))
  include $(lis_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_LIS=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
# IPMs
ifneq (,$(findstring $(COMMA)sipm$(COMMA),$(COMP)))
  include $(sipm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SIPM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)xipm$(COMMA),$(COMP)))
  include $(xipm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XIPM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)ipe$(COMMA),$(COMP)))
  include $(ipe_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_IPE=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)dataipe$(COMMA),$(COMP)))
  include $(dataipe_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_DATAIPE=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
# HYDs
ifneq (,$(findstring $(COMMA)shyd$(COMMA),$(COMP)))
  include $(shyd_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SHYD=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)xhyd$(COMMA),$(COMP)))
  include $(xhyd_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XHYD=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)wrfhydro$(COMMA),$(COMP)))
  include $(wrfhydro_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_WRFHYDRO=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
################################################################################
##$(info $$DEP_LINK_OBJS is [${DEP_LINK_OBJS}])

CPPFLAGS += $(DEP_FRONTS)

TARGET = ../exe/NEMS.x

INCS = -IENS_Cpl \
	-I. \
       $(DEP_INCS)

MAIN = MAIN_NEMS.o

OBJS = module_NEMS_UTILS.o \
       module_MEDIATOR_methods.o \
       module_MEDIATOR.o \
       module_MEDIATOR_SpaceWeather.o \
       module_EARTH_INTERNAL_STATE.o \
       module_EARTH_GRID_COMP.o \
       module_NEMS_INTERNAL_STATE.o \
       module_NEMS_GRID_COMP.o \
       module_NEMS_Rusage.o

COBJS = nems_c_rusage.o

MAKEFILE = makefile

#
# GOCART specific compilation variables/flags
#
GOCART_MODE=stub
ESMADIR=$(CHEM_DIR)
#gjt: comment out the following line because it unconditionally uses CHEM_INC
#gjt: THIS NEEDS TO BE FIXED!
-include $(CHEM_INC)/NCEP_base.mk

ifeq ($(GOCART_MODE),stub)
	LIBGOCART=
else
	LIBGOCART=$(LIB_GOCART)
		CHEM_LIB=$(CHEM_DIR)_full/$(CHEM_VER)
		CHEM_MOD=$(CHEM_LIB)/mod
		CHEM_INC=$(CHEM_LIB)/include
		PHYS_LIB=$(PHYS_DIR)_full/$(CHEM_VER)
		PHYS_MOD=$(PHYS_LIB)/mod
		PHYS_INC=$(PHYS_LIB)/include
endif

ifeq ($(PHYS_MODE),compile)
        PHYS_LIB = $(TOP)/atmos/gsm/gsmphys
        PHYS_INC = $(TOP)/atmos/gsm/gsmphys
        PHYS_DIR = $(TOP)/atmos/gsm/gsmphys
endif
ifeq ($(CHEM_MODE),compile)
        CHEM_LIB = $(TOP)/../../chem/gocart
        CHEM_INC = $(TOP)/../../chem/gocart/src/Config
        CHEM_DIR = $(TOP)/../../chem
	CHEM_MOD = $(TOP)/../../chem/gocart/${ARCH}/include
ESMADIR=$(CHEM_LIB)
#gjt: THIS NEEDS TO BE FIXED!
#include $(ESMADIR)/src/Config/NCEP_base.mk
endif



export GOCART_MODE
ESMADIR=$(CHEM_LIB)
#gjt: comment out the following line because it unconditionally uses CHEM_INC
#gjt: THIS NEEDS TO BE FIXED!
#-include $(CHEM_INC)/NCEP_base.mk
#
# log info
#
WHOAMI  := $(shell whoami)
CMP_DATE := $(shell date)

SVN_URL := $(shell git remote show -n origin | grep 'Fetch URL:' | sed 's,^[^:]*: ,,g' )
SVN_REV := $(shell git show HEAD | head -1 | cut -f 2 -d ' ' | cut -c1-12 )

ifeq ($(SVN_URL),)
  SVN_REV := $(shell svnversion -n .)
  SVN_URL := $(shell svn info | grep URL | cut -f 2 -d ' ' | head -1 )
endif

CMP_YEAR := $(shell date +"%Y" )
CMP_JD := $(shell date +"%j" )

CPPFLAGS += -D'SVN_INFO="($(WHOAMI)) $(CMP_DATE) r$(SVN_REV) $(SVN_URL)"'

ifdef CMP_YEAR
  CPPFLAGS += -D'CMP_YEAR=$(CMP_YEAR)'
endif
ifdef CMP_JD
  CPPFLAGS += -D'CMP_JD=$(CMP_JD)'
endif

################################################################################
#
# Ask user which target he/she wants to build
#
################################################################################
all:
	@echo "       CHEM_MODi----------->" $(CHEM_MOD)
	@echo " gmake nmm_gsm ----------------------------- NMM and GSM - without the GOCART"
	@echo " gmake gen --------------------------------- GEN "
	@echo " gmake gsm_gen_post GOCART_MODE=full ------- GEN and GSM - with the GOCART and post"
	@echo " gmake nmm_gsm GOCART_MODE=full ------------ NMM and GSM - with the GOCART"
	@echo " gmake nmm --------------------------------- NMM "
	@echo " gmake nmm_post ---------------------------- NMM - with post"
	@echo " gmake gsm --------------------------------- GSM - without the GOCART"
	@echo " gmake gsm_post ---------------------------- GSM - with post without the GOCART"
	@echo " gmake gsm GOCART_MODE=full ---------------- GSM - with the GOCART"
	@echo " gmake fim --------------------------------- FIM "
#	@echo " gmake fim_gsm ----------------------------- FIM and GSM - without the GOCART"
#	@echo " gmake fim_gsm GOCART_MODE=full ------------ FIM and GSM - with the GOCART"
	@echo
	@echo "In order to build NEMS with external components, all of the above targets support"
	@echo "the optional COMP variable. Currently supported values are: "
	@echo "\"satm\", \"xatm\";"
	@echo "\"socn\", \"xocn\", \"mom5\", \"hycom\", \"pom\";"
	@echo "\"sice\", \"xice\", \"cice\";"
	@echo "\"swav\", \"xwav\", \"ww3\";"
	@echo "\"slnd\", \"xlnd\", \"noah\", \"noahmp\";"
	@echo "\"sipm\", \"xipm\", \"ipe\";"
	@echo "\"shyd\", \"xhyd\", \"wrfhydro\"."
	@echo
	@echo "E.g. \"gmake nmm_gsm COMP=satm,xatm,socn,xocn,mom5,hycom,sice,xice,cice,swav,xwav,slnd,xlnd,sipm,xipm,shyd,xhyd\"."
	@echo

################################################################################
#
# NEMS
#
################################################################################

LIBS_NEMS = $(DEP_LINK_OBJS)             \
            ENS_Cpl/ENS_Cpl.a

nems: nems_libs $(OBJS) $(COBJS) $(MAIN)
	echo libgocart is $(LIBGOCART)
	echo extlibs is $(EXTLIBS)
	$(FC) $(SMP) -o $(TARGET) $(MAIN) $(OBJS) $(COBJS) $(LIBS_NEMS) $(EXTLIBS) $(LIBGOCART)
	@echo "$(TARGET) is created."
        
nems_libs:
	cd ENS_Cpl             && gmake stub
        

################################################################################
#
# Clean everything
#
################################################################################

clean:
	$(RM) -f *.f90 *.lst *.o *.mod lm map $(TARGET)
	cd ENS_Cpl             && gmake clean

.SUFFIXES:
.SUFFIXES: .F90 .f90 .o

.F90.f90:
	$(CPP) $(CPPFLAGS) $< > $*.f90

$(COBJS) : %.o: %.c 
	$(CC) -c $*.c

$(OBJS): %.o: %.f90
	$(FC) $(FFLAGS) $(INCS) -c $*.f90
	$(RM) $*.f90

MAIN_NEMS.o: %.o: %.f90
	$(FC) $(FFLAGS) $(INCS) -c $*.f90
	$(RM) $*.f90

