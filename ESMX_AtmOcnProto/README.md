# ESMX_AtmOcnProto

[ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX) is used to implement a coupled system with two components (ATM and OCN). Both components are provided as NUOPC-compliant models. 

## Primary Artifacts

Files and sub-directories that implement the fundamental concept demonstrated by the prototype. These are the primary artifacts to look at and to pattern actual user code after.

- `esmxBuild.yaml`  - Standard ESMX YAML file describing the build dependencies of the `esmx_app` (the executable) on components Lumo and Tawas.
- `esmxRun.yaml`  - Standard ESMX YAML file describing the run configuration: Lumo is OCN, Tawas is ATM, and defining the run sequence.
- `Lumo`            - Example OCN component. It uses a simple CMake based build system.
- `TaWaS`           - Example ATM component. It uses a GNU Make based build system.

### Usage

1. Build the ESMX executable by using the command line tool:
   ```
   ESMX_Builder
   ```
   This assumes that the `bin` directory of the desired ESMF installation is present in the user's `PATH` environemnt variable. The `ESMX_Builder` tool first compiles all the required sub-components, and then links it into the final executable.
2. Run the `./install/bin/esmx_app` executable on 4 PETs using the appropriate MPI launch procedure. E.g.:
   ```
   mpirun -np 4 ./install/bin/esmx_app
   ```
   Or using the alternative `esmxRun-Alt.yaml` configuration that uses `ESMX_Data` for `ATM`.
   ```
   mpirun -np 4 ./install/bin/esmx_app esmxRunAlt.yaml
   ```
## Secondary Artifacts

Files that are needed for the integration into ESMF's automated testing infrastructure for regression testing. These artifacts might be interesting to look at, but generally should *not* be used as patterns to follow in actual projects.

- `Makefile`        - GNU Makefile that defines targets that are used by the automated ESMF regression testing script.

### Usage

1. The default target of the `Makefile` calls the `ESMX_Builder` command line tool to build the ESMX executable:
   ```
   make
   ```
2. The `run` target of the `Makefile` uses the MPI launch procedure - identified by ESMF - to run `./install/bin/esmx_app` on 4 PETs.
   ```
   make run
   ```
   Or to run with the alternative `esmxRun-Alt.yaml` configuration that uses `ESMX_Data` for `ATM`.
   ```
   make runalt
   ```
3. The `distclean` target of the `Makefile` removes all of the generated files.
   ```
   make distclean
   ```
