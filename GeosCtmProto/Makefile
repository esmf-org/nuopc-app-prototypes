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

# add MAPL includes and libs
MAPL_INSTALL_ROOT = $(ESMF_DIR)/src/addon/MAPL5_1/Linux
MAPL_INCLUDEPATHS = -I$(MAPL_INSTALL_ROOT)/include/MAPL_Base -I$(ESMF_DIR)/src/include
MAPL_LINKPATHS    = -L$(MAPL_INSTALL_ROOT)/lib
MAPL_LINKLIBS     = -lMAPL_Base -lMAPL_Base_stubs -lMAPL_cfio_r4 -lGMAO_mpeu

ESMF_F90COMPILEPATHS      += $(MAPL_INCLUDEPATHS)
ESMF_F90LINKPATHS         += $(MAPL_LINKPATHS)
ESMF_F90ESMFLINKLIBS          += $(MAPL_LINKLIBS)

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<


# -----------------------------------------------------------------------------
#ctmApp: ctmApp.o driver.o ctm.o history.o extdata.o ectm.o ptracer.o advcore.o NUOPC_Generic.o
ctmApp: ctmApp.o driver.o ctm.o ectm.o ptracer.o advcore.o NUOPC_Generic.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS) 

# module dependencies:
ctmApp.o: driver.o 
#driver.o: ctm.o history.o extdata.o
driver.o: ctm.o 
#ctm.o: ectm.o ptracer.o advcore.o
ctm.o: ectm.o ptracer.o advcore.o
ectm.o: NUOPC_Generic.o
ptracer.o: NUOPC_Generic.o
advcore.o: NUOPC_Generic.o

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info edit
dust:
	rm -f PET*.ESMF_LogFile *.nc
clean:
	rm -f ctmApp *.o *.mod
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

