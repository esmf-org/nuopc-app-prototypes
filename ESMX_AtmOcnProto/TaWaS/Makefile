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

lib%.so: %.o
	$(ESMF_CXXLINKER) -shared -o $@ $<

lib%.a: %.o
	ar cr $@ $<

.PRECIOUS: lib%.so lib%.a

%.cmake : lib%.a
	@echo "# Self-describing NUOPC Component for ESMX" > $@
	@echo >> $@
	@echo "add_library($* STATIC IMPORTED)"           >> $@
	@echo "set_target_properties($* PROPERTIES"       >> $@
	@echo "  IMPORTED_LOCATION             `pwd`/$<"  >> $@
	@echo "  INTERFACE_INCLUDE_DIRECTORIES `pwd`"     >> $@
	@echo "#  INTERFACE_LINK_DIRECTORIES    <not-needed-here>"    >> $@
	@echo "#  INTERFACE_LINK_LIBRARIES      <not-needed-here>"    >> $@
	@echo ")" >> $@
	@echo "target_link_libraries(esmx_driver PUBLIC $*)"  >> $@

# -----------------------------------------------------------------------------
all: tawas.cmake

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info edit
dust:
	rm -f PET*.ESMF_LogFile
clean:
	rm -f *.o *.a *.so *.mod *.mk tawas.cmake
distclean: dust clean

rebuild:
	$(MAKE) distclean
	$(MAKE) all

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

edit:
	nedit tawas.F90 &
