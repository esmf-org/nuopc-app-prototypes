# ==============================================================================
# Earth System Modeling Framework
# Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
# Massachusetts Institute of Technology, Geophysical Fluid Dynamics
# Laboratory, University of Michigan, National Centers for Environmental
# Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
# NASA Goddard Space Flight Center.
# Licensed under the University of Illinois-NCSA License.
# ==============================================================================

mutable struct State
   times_called::Int

   # The model state probably isn't the most appropriate place to put this path, but for
   # this simple example, we're putting it there for convenience.
   path_to_c_library::String
end

# Initialize times_called to garbage so we ensure that init is called
model_state = State(-10, "")

function init(path_to_c_library::AbstractString)
   model_state.times_called = 0
   model_state.path_to_c_library = path_to_c_library
   msg = "path to C library: " * path_to_c_library
   @ccall model_state.path_to_c_library.write_logmsg(msg::Cstring)::Cvoid
end

function run()
   model_state.times_called += 1
   msg = "model_state times_called: " * string(model_state.times_called)
   @ccall model_state.path_to_c_library.write_logmsg(msg::Cstring)::Cvoid
end