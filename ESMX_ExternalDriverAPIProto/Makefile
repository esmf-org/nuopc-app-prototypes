ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)

# strip quotes around the ESMF_INTERNAL_MPIRUN value
ESMF_INTERNAL_MPIRUN := $(shell echo $(ESMF_INTERNAL_MPIRUN))

build/externalApp: externalApp.F90 esmxBuild.yaml
	cmake -H. -Bbuild -DESMF_ESMXDIR=$(ESMF_ESMXDIR)
	cmake --build ./build

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info run
dust:
	rm -f PET*.ESMF_LogFile *.nc *.stdout
clean:
	rm -rf build
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

run:
	$(ESMF_INTERNAL_MPIRUN) -np 4 ./build/externalApp
