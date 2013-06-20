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

################################################################################
# Include self-describing dependencies and accumulate into local variables

include ATM-A/atmA.mk
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_ATMA=$(ESMF_DEP_FRONT)
DEP_INCS      := $(DEP_INCS) -I$(ESMF_DEP_INCPATH)
DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)

include ATM-B/atmB.mk
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_ATMB=$(ESMF_DEP_FRONT)
DEP_INCS      := $(DEP_INCS) -I$(ESMF_DEP_INCPATH)
DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)

include OcnModelA/ocn.mk
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_OCNA=$(ESMF_DEP_FRONT)
DEP_INCS      := $(DEP_INCS) -I$(ESMF_DEP_INCPATH)
DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)

include OcnModelB/ocn.mk
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_OCNB=$(ESMF_DEP_FRONT)
DEP_INCS      := $(DEP_INCS) -I$(ESMF_DEP_INCPATH)
DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(DEP_FRONTS) $(DEP_INCS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(DEP_FRONTS) $(DEP_INCS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(DEP_FRONTS) $(DEP_INCS) $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

% : %.C
	$(ESMF_CXXCOMPILER) -c $(DEP_FRONTS) $(DEP_INCS) $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<


# -----------------------------------------------------------------------------
esmApp: esmApp.o esm.o $(DEP_LINK_OBJS)
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)

# module dependencies:
esmApp.o: esm.o
esm.o: $(DEP_CMPL_OBJS)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info edit
dust:
	rm -f PET*.ESMF_LogFile
clean:
	rm -f esmApp *.o *.mod
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

edit:
	nedit esmApp.F90 esm.F90 &
