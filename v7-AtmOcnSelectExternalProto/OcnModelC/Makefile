# GNU Makefile template for user ESMF component

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

################################################################################
# Need a variable that contains only a comma for some of the operations below
COMMA := ,

################################################################################
# Include self-describing dependencies and accumulate into local variables

include ../OcnCommon/ocnCommon.mk
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_OCNCOMMON=$(ESMF_DEP_FRONT)
DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))

include OcnSub/ocnSub.mk
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_OCNSUB=$(ESMF_DEP_FRONT)
DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))

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

.PRECIOUS: %.so
%.mk : %.so
	@echo "# ESMF self-describing build dependency makefile fragment" > $@
	@echo >> $@
	@echo "ESMF_DEP_FRONT     = "$<           >> $@
	@echo "ESMF_DEP_INCPATH   = "             >> $@
	@echo "ESMF_DEP_CMPL_OBJS = "             >> $@
	@echo "ESMF_DEP_LINK_OBJS = "             >> $@
	@echo "ESMF_DEP_SHRD_PATH = "             >> $@
	@echo "ESMF_DEP_SHRD_LIBS = "             >> $@

# -----------------------------------------------------------------------------
ocn.mk:

# - build everything (including dependencies) into a single shared object
ocn.so: ocn.o $(DEP_CMPL_OBJS)
	$(ESMF_CXXLINKER) -shared -o $@ $< $(DEP_LINK_OBJS) $(DEP_SHRD_PATH) $(DEP_SHRD_LIBS)
	rm -f $<

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info edit
dust:
	rm -f PET*.ESMF_LogFile
clean:
	rm -f *.o *.so *.mod *.mk
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

edit:
	nedit ocn.F90 &
