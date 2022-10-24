# NUOPC Application Prototypes

The NUOPC Application Prototypes demonstrate a wide range of features implemented by the [NUOPC](https://github.com/esmf-org/esmf/tree/develop/src/addon/NUOPC) layer. The aim of the prototypes is to provide example and template codes that are directly applicable to real world situations. The focus is on coupling through the NUOPC standards, and not on model details. For this reason *no* physically realistic model codes are included!

The structure of the repository is such that each top-level directory holds a self-contained prototype example. The name of the directory indicates roughly the focus or structure of the specific example itself. E.g. `AtmOcnPetListProto` is a case with two components (ATM and OCN), where the components are created on specific PET lists.

The [ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX) layer is built on top of ESMF and NUOPC. The idea is to make it as simple as possible for a user to build, run, and test NUOPC based systems, often without having to write any extra code beyond the NUOPC-compliant models. **We strongly recommend that anybody interested in building coupled systems with NUOPC starts their journey by looking at the ESMX prototypes first!** These prototypes are easy to identify by the `ESMX_` prefix of the directory names.
