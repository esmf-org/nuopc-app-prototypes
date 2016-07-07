# GNU Makefile template for user ESMF application

################################################################################
################################################################################
## This Makefile must be able to find the "esmf.mk" Makefile fragment in the  ##
## 'include' line below. Following the ESMF User's Guide, a complete ESMF     ##
## installation should ensure that a single environment variable "ESMFMKFILE" ##
## is made available on the system. This variable should point to the         ##
## "esmf.mk" file.                                                            ##
##                                                                            ##
## This example Makefile uses the "ESMFMKFILE" environment variable.          ##
##                                                                            ##
## If you notice that this Makefile cannot find variable ESMFMKFILE then      ##
## please contact the person responsible for the ESMF installation on your    ##
## system.                                                                    ##
## As a work-around you can simply hardcode the path to "esmf.mk" in the      ##
## include line below. However, doing so will render this Makefile a lot less ##
## flexible and non-portable.                                                 ##
################################################################################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)
include $(COMP)

ifneq (,$(findstring .h,$(ESMF_DEP_FRONT)))
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_H_COMP=\"$(ESMF_DEP_FRONT)\" -DFRONT_H_ATMF_SS=SetServices_$(subst .h,,$(ESMF_DEP_FRONT))
else ifneq (,$(findstring .so,$(ESMF_DEP_FRONT)))
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SO_COMP=\"$(ESMF_DEP_FRONT)\"
else ifneq ($(ESMF_DEP_FRONT)x,x)
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_COMP=$(ESMF_DEP_FRONT)
endif
DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))

ifneq ($(FRONT_COMP_LABEL)x,x)
DEP_FRONTS := $(DEP_FRONTS) -DFRONT_COMP_LABEL=$(FRONT_COMP_LABEL)
endif

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(DEP_FRONTS) $(DEP_INCS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(DEP_FRONTS) $(DEP_INCS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(DEP_FRONTS) $(DEP_INCS) $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	$(ESMF_CXXCOMPILER) -c $(DEP_FRONTS) $(DEP_INCS) $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<


# -----------------------------------------------------------------------------
nuopcExplorerApp: nuopcExplorerApp.o nuopcExplorerDriver.o $(DEP_LINK_OBJS)
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(DEP_SHRD_PATH) $(DEP_SHRD_LIBS) $(ESMF_F90ESMFLINKLIBS)
	rm -f *.o

# module dependencies:
nuopcExplorerApp.o: nuopcExplorerDriver.o
nuopcExplorerDriver.o: $(DEP_CMPL_OBJS)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info output
dust:
	rm -f PET*.ESMF_LogFile
clean:
	rm -f nuopcExplorerApp *.o *.mod
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

output:
	@echo "ESMF_DEP_FRONT     =" $(ESMF_DEP_FRONT)
	@echo "ESMF_DEP_INCPATH   =" $(ESMF_DEP_INCPATH)
	@echo "ESMF_DEP_CMPL_OBJS =" $(ESMF_DEP_CMPL_OBJS)
	@echo "ESMF_DEP_LINK_OBJS =" $(ESMF_DEP_LINK_OBJS)

edit:
	nedit nuopcExplorerScript nuopcExplorerApp.F90  nuopcExplorerDriver.F90 explorer.config &
