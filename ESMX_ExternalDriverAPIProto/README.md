# ESMX_ExternalDriverAPIProto

[ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX) is used to implement a coupled system with two components (ATM and OCN). The `ESMX_Driver` is instantiated by an external application layer through the [External NUOPC Interface](https://earthsystemmodeling.org/docs/nightly/develop/NUOPC_refdoc/node3.html#SECTION00038000000000000000).

## Primary Artifacts

Files and sub-directories that implement the fundamental concept demonstrated by the prototype. These are the primary artifacts to look at and to pattern actual user code after.

- `externalApp.F90` - Top level Fortran `program` scope. This layer calls into the `ESMX_Driver` through the "External NUOPC Interface".
- `CMakeLists.txt`  - Top level CMake build that defines the rules of how to build the executable with its dependencies. This file references the ESMX build layer.
- `esmxBuild.yaml`  - Standard ESMX YAML file describing the build dependencies of `ESMX_Driver` on components Lumo and Tawas.
- `esmxRun.yaml`  - Standard ESMX YAML file describing the run configuration: Lumo is OCN, Tawas is ATM, and defining the run sequence.
- `Lumo`            - Example OCN component. It uses a simple CMake based build system.
- `TaWaS`           - Example ATM component. It uses a GNU Make based build system.

1. Build the `externalApp` executable by using CMake directly from the command line:
   ```
   cmake -S. -Bbuild -DESMF_ESMXDIR=$ESMF_ESMXDIR -DCMAKE_INSTALL_PREFIX=install
   cmake --build ./build
   cmake --install ./build
   ```
   This assumes that the `ESMF_ESMXDIR` shell variable was set according to the desired ESMF installation. E.g. by inspecting `more $ESMFMKFILE`.
2. Run the `./install/bin/esmx_app` executable on 4 PETs using the appropriate MPI launch procedure. E.g.:
   ```
   mpirun -np 4 ./install/bin/externalApp
   ```

## Secondary Artifacts

Files that are needed for the integration into ESMF's automated testing infrastructure for regression testing. These artifacts might be interesting to look at, but generally should *not* be used as patterns to follow in actual projects.

- `Makefile`        - GNU Makefile that defines targets that are used by the automated ESMF regression testing script.

### Usage

1. The default target of the `Makefile` calls the `CMake` command line tools to build the external executable:
   ```
   make
   ```
2. The `run` target of the `Makefile` uses the MPI launch procedure - identified by ESMF - to run `./install/bin/externalApp` on 4 PETs.
   ```
   make run
   ```
3. The `distclean` target of the `Makefile` removes all of the generated files.
   ```
   make distclean
   ```
