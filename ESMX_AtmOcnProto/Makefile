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

# strip quotes around the ESMF_INTERNAL_MPIRUN value
ESMF_INTERNAL_MPIRUN := $(shell echo $(ESMF_INTERNAL_MPIRUN))

################################################################################
################################################################################
EXE = install/bin/esmx_app
.PHONY: $(EXE)

$(EXE):
	$(ESMF_APPSDIR)/ESMX_Builder -v

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info run
dust:
	rm -f PET*.ESMF_LogFile *.nc *.stdout
clean:
	rm -rf build install
distclean: dust clean
	cd TaWaS; make distclean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

run:
	$(ESMF_INTERNAL_MPIRUN) -np 4 ./$(EXE)
runAlt:
	$(ESMF_INTERNAL_MPIRUN) -np 4 ./$(EXE) esmxRunAlt.yaml
