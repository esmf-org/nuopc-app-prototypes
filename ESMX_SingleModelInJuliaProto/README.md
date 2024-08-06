# ESMX_SingleModelInJulia

## Overview

This single model component (SiMoCoJulia) prototype demonstrates how a Julia-based component can be coupled to a NUOPC system. The (very simple) model itself is written in Julia, with the NUOPC cap in C. (See ESMX_SingleModelInCProto for the similar C-only version.) The prototype demonstrates both calls from the C cap into Julia, and from Julia back to C (to perform an ESMF function call). The Julia code demonstrates storing and updating a persistent model state between calls from C to Julia. The CMake-based build demonstrates a method for getting the paths needed for both of these inter-language directions.

For simplicity, this prototype is implemented using [ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX). The build system of SiMoCoJulia is CMake based, and thus hooks easily into the ESMX build procedure. The C portion of SiMoCoJulia is built in the form of a shared library, which is included in ESMX via dynamic loading at run-time.

## Primary Artifacts

Files and sub-directories that implement the fundamental concept demonstrated by the prototype. These are the primary artifacts to look at and to pattern actual user code after.

- `SiMoCoJulia/simocojulia_model.jl` - The Julia-based component model code
- `SiMoCoJulia/simocojulia_cap.c`    - The C-based NUOPC cap, providing the interface beteen the Julia-based component and the NUOPC system
- `SiMoCoJulia/simocojulia_utils.c`  - C-based utilities that can be called from Julia as well as C
- `SiMoCoJulia/CMakeLists.txt`       - The CMake-based build system for SiMoCoJulia (see also the Find utilities under `SiMoCoJulia/cmake/`)

## Secondary Artifacts

Files that are needed for the integration into ESMF's automated testing infrastructure for regression testing. For this example, ESMX-related pieces are considered secondary artifacts, since the main purpose of the example is to demonstrate coupling a Julia-based component. These artifacts might be interesting to look at, but are less central to the concepts and patterns that this prototype is demonstrating.

- `Makefile`         - GNU Makefile that defines targets that are used by the automated ESMF regression testing script. These targets can also be used to build and run this prototype manually.
- `esmxBuildDL.yaml` - Standard ESMX YAML file describing the build dependencies of the `esmx_app` (the executable) on SiMoCoJulia via the dynamic loading at run-time approach.
- `esmxRunDL.yaml`   - Standard ESMX YAML file describing the run configuration suitable for the dynamic loading at run-time approach.

## Usage

Building and running this example requires that you have Julia installed.

1. Build the SiMoCoJulia library and the ESMX executable:
   ```
   make DL
   ```

2. Run the example using the MPI launch procedure - identified by ESMF - to run `./install/bin/esmx_app` on 4 PETs:
   ```
   make runDL
   ```

   A key indicator of success is seeing lines like "model_state times_called: 1", then 2, 3 and 4 in the PET ESMF log files.

3. The `distclean` target of the `Makefile` removes all of the generated files:
   ```
   make distclean
   ```

## Design notes

The key general question in the design of this Julia-based component is how to handle the interface between C and Julia code: how much interaction to allow between these languages, where exactly the break between C and Julia should occur, etc. It is possible both to call Julia from C (https://docs.julialang.org/en/v1/manual/embedding/) and to call C from Julia (https://docs.julialang.org/en/v1/manual/calling-c-and-fortran-code/). However, there are some awkward and sometimes poorly-documented aspects of this language interoperability, such as converting variables between C types and Julia types. This seems especially awkward for C function arguments or return values that are structs; as noted in https://docs.julialang.org/en/v1/manual/calling-c-and-fortran-code/#mapping-c-types-to-julia, "no C header files are used anywhere in the process of calling C functions: you are responsible for making sure that your Julia types and call signatures accurately reflect those in the C header file".

The general decision here has been to not have any direct interaction between Julia and ESMF. The rationale for this is that calls to ESMF functions from Julia, especially those requiring non-basic types or various ESMF constants, feel challenging, error-prone and potentially problematic to maintain if some things (structs and possibly constants) need to have parallel definitions in Julia due to not having access to ESMF's C header files. There are two main specific implications of this decision:

1. The NUOPC cap is written entirely in C. The cap has a lot of NUOPC / ESMF calls, so it seems simpler to have this cap in C rather than having some / all of the cap in Julia and requiring numerous calls from Julia back to the ESMF C API. For this simple model, there is very little interaction needed between this C cap and the Julia code. However, even in more realistic cases, it seems likely that the separation between the cap and the main component code could be designed to have relatively few interaction points between the two languages (fewer, anyway, than there are between the cap and ESMF); and where there are interaction points, they would be under the complete control and maintenance of the component model developers (so, for example, if a struct is passed between the C cap and the Julia component, the definition of this struct would reside entirely in the component model repository, making it easier to keep the C and Julia definitions of the struct in sync).

2. Where there are calls desired from the Julia code to ESMF (in this example, the calls to `ESMC_LogWrite`), these are wrapped in C code that resides in the component model. This isolates the Julia code from details of the ESMF API, allowing the component model developers to control the interface between Julia and C code and keep this interface as simple as possible.

It's possible that, with more complex and realistic examples, some of these design choices would be adjusted. For example, it might make sense to allow some direct calls from Julia to ESMF rather than wrapping each of these calls in a C function in the component.

## Specific implementation notes

### Specifying build options needed to invoke Julia code

Calling Julia code from C requires adding include and link paths. This is done in two places:

First, when building the SiMoCoJulia library, this is done from the CMake, leveraging `FindJulia.cmake` and then adding:

```cmake
  target_include_directories(SiMoCoJulia PRIVATE ${Julia_INCLUDE_DIRS})
  target_link_libraries(SiMoCoJulia PUBLIC ${Julia_LIBRARY})
```

Second, when linking the ESMX executable, we again need to include the Julia library. This is handled from the top-level Makefile via:
```makefile
JL_SHARE := $(shell julia -e 'print(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))')
LDFLAGS += $(shell $(JL_SHARE)/julia-config.jl --ldflags)
LDFLAGS += $(shell $(JL_SHARE)/julia-config.jl --ldlibs)
```

and then adding these `LDFLAGS` when invoking `ESMX_Builder` so that they will be picked up by the CMake process that builds the ESMX executable.

(In this case, the specification of `LDFLAGS` at this top level makes `target_link_libraries(SiMoCoJulia PUBLIC ${Julia_LIBRARY})` unnecessary, but we still include that for illustrative purposes.)

### Specifying important paths needed for the C-Julia interface

For the C-based cap to call the Julia code, the C code needs to know the full path to the Julia code. For the Julia code to call C functions, the Julia code needs to know the full path to the library containing these C functions.

Both of these paths are determined by the CMake build and passed to the C code as compile-time definitions:

```cmake
target_compile_definitions(SiMoCoJulia PRIVATE PATH_TO_SOURCE="${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_definitions(SiMoCoJulia PRIVATE PATH_TO_LIBRARY="$<TARGET_FILE:SiMoCoJulia>")
```

`PATH_TO_LIBRARY` is later passed into the Julia code. Note that, in this example, this library contains all of the C code for the component, including both the C-based NUOPC cap and the C-based wrappers to the ESMF API. (The latter are the only pieces called from the Julia code.)

### Initializing the Julia runtime

As noted in the comment in the `initialize_julia` function, the call to `jl_init` should only be done once per process. If we are guaranteed to only have a single Julia-based component, then it can be called from the cap as is done here. But if there's a chance that we may have multiple Julia components, then we should do something different to ensure it only gets called once. This could mean moving this call into ESMF somehow, or at least having ESMF support some way of checking whether jl_init has already been called... the latter could be done in a way that avoids requiring ESMF to have any julia calls itself, e.g.,:

```c
if (!ESMC_Is_Julia_Initialized()) {
  jl_init();
  ESMC_Set_Jula_Initialized()
}
```

where ESMC_Set_Julia_Initialized and ESMC_Is_Julia_Initialized simply set and get a boolean flag.

One other piece that is not illustrated here is: It is recommended to include this line in a C file that is part of the final executable (not in a shared library):

```c
JULIA_DEFINE_FAST_TLS // only define this once, in an executable (not in a shared library) if you want fast code.
```

### Handling errors from ESMF

For now, we have not yet implemented error handling of the ESMF calls invoked from Julia (specifically, from the `ESMC_LogWrite` call invoked from `write_logmsg`).

### Use of MPI

This example runs using 4 MPI processes, but this simple Julia component isn't yet parallelized.