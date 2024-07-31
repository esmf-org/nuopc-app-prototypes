# ESMX_StartHereProto

Start with this prototype example if you are new to [ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX) and want to learn the basic usage of the tool. The prototype exclusively leverages features that are built into ESMX itself. Since no external code is built into the ESMX executable, this prototype does *not* require an ESMX build configuration file, such as `esmxRun.yaml`!

## Primary Artifacts

Files and sub-directories that implement the fundamental concepts demonstrated by the prototype. These are the primary artifacts to look at and to pattern actual user code after.

- `esmxRun.yaml`         - The absolute simplest ESMX YAML file describing a run configuration for running the ESMX application, driving the system from `startTime` to `stopTime`.
- `esmxRunStep1.yaml`    - Still just driving the ESMX application, from `startTime` to `stopTime`, without any actual components present. However, demonstrate the use of a few very useful optional options under the `ESMX:App` section.
- `esmxRunStep2.yaml`    - Add the `ESMX:Driver` section. Use it to specify driver attributes.
- `esmxRunStep3.yaml`    - Add the `componentList` key to the `ESMX:Driver` section. Configure a single component instance `ABC` using the built-in `ESMX_Data` component.
- `esmxRunStep4.yaml`    - Specify two components under the `componentList`. Both are configured as instances of the built-in `ESMX_Data` component. Instance `ABC` contains one export field, which is impored by instance `DEF`. An explicit `runSequence` with two-way coupling is coded. Two `ESMF_RUNTIME_*` environment variables are specified to enable ESMF profiling for this example. The profiling data is written to file `ESMF_Proflile.summary` at the end of the execution.

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
   Or using any of the alternative configurations, e.g.
   ```
   mpirun -np 4 ./install/bin/esmx_app esmxRunStep1.yaml
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
   Or to run with an alternative configuration, e.g. `esmxRunStep1.yaml`.
   ```
   make runStep1
   ```
3. The `distclean` target of the `Makefile` removes all of the generated files.
   ```
   make distclean
   ```
