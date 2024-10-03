# ESMX_SingleModelInFortran

[ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX) is used to implement an uncoupled application, consisting of only a single model component (SiMoCo). Here the SiMoCo component is implemented as a NUOPC component in Fortran. The build system of SiMoCo is CMake based, and thus hooks easily into the ESMX build procedure. The SiMoCo component is built in form of a shared library. It can be used through direct linking into the ESMX application, or by dynamic loading at run-time. Both options are available through ESMX, and demonstrated here.

## Primary Artifacts

Files and sub-directories that implement the fundamental concept demonstrated by the prototype. These are the primary artifacts to look at and to pattern actual user code after.

- `SiMoCo`            - Simple NUOPC-compliant Model Component, utilizing a CMake based build system.
- `esmxBuild.yaml`    - Standard ESMX YAML file describing the build dependencies of the `esmx_app` (the executable) on SiMoCo via the direct linking approach.
- `esmxRun.yaml`      - Standard ESMX YAML file describing the run configuration suitable for the direct linking approach.
- `esmxBuildDL.yaml`- Standard ESMX YAML file describing the build dependencies of the `esmx_app` (the executable) on SiMoCo via the dynamic loading at run-time approach.
- `esmxRunDL.yaml`  - Standard ESMX YAML file describing the run configuration suitable for the dynamic loading at run-time approach.

### Usage

1. Build the ESMX executable by using the command line tool:
   ```
   ESMX_Builder
   ```
   This assumes that the `bin` directory of the desired ESMF installation is present in the user's `PATH` environemnt variable. Using `ESMX_Builder` with the default `esmxBuild.yaml` file first builds the SiMoCo component, and then links it into the final executable. Alternatively, using `ESMX_Builder` with `esmxBuildDL.yaml`, builds SiMoCo, but does not link it into the final executable.
2. Run the `./install/bin/esmx_app` executable on 4 PETs using the appropriate MPI launch procedure. E.g.:
   ```
   mpirun -np 4 ./install/bin/esmx_app
   ```
   Or using the alternative `esmxRunDL.yaml` configuration that loads the SiMoCo component dynamically at run-time:
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
   Alternatively build the ESMX executable via the `DL` target to use SiMoCo via dynamic linking at run-time:
   ```
   make DL
   ```
2. The `run` target of the `Makefile` uses the MPI launch procedure - identified by ESMF - to run `./install/bin/esmx_app` on 4 PETs.
   ```
   make run
   ```
   Or to run with the alternative `esmxRunDL.yaml` configuration that utilizes the dynamic linking approach to load SiMoCo at run-time:
   ```
   make runDL
   ```
3. The `distclean` target of the `Makefile` removes all of the generated files.
   ```
   make distclean
   ```
