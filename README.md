# NUOPC Application Prototypes

The NUOPC Application Prototypes demonstrate a wide range of features implemented by the [NUOPC](https://github.com/esmf-org/esmf/tree/develop/src/addon/NUOPC) layer. The goal of these prototypes is to provide example and template code directly applicable to real-world situations. The focus is strictly on coupling through NUOPC standards rather than model details. For this reason, **no** physically realistic model codes are included!

The repository is structured so that each top-level directory contains a self-contained prototype example. Directory names broadly indicate the design pattern or focus of each case. For example, `AtmOcnPetListProto` demonstrates a system with two components (`ATM` and `OCN`) created on specific PET lists. Details for each case are provided in the `README.md` file within its respective directory.

## ESMX

The [ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX) layer is built on top of ESMF and NUOPC. The goal of ESMX is to make it as simple as possible to build, run, and test NUOPC-based systems, often without having to write any code beyond the NUOPC-compliant models.

**We strongly recommend that anyone interested in building coupled systems with NUOPC starts their journey by looking at the ESMX prototypes first!** These prototypes are easily identified by the `ESMX_` prefix in their directory names.

## Test Environment

Building and running the application prototypes requires a complete ESMF library installation. The following environment variables direct the test infrastructure:

- **`ESMFMKFILE`**: Must point to the `esmf.mk` file of the ESMF installation.
- **`CMAKE_PREFIX_PATH`**: Must contain the path to the ESMF installation root directory. Alternatively, with modern CMake, **`ESMF_ROOT`** can be set instead.
