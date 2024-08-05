# ==============================================================================
# Earth System Modeling Framework
# Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
# Massachusetts Institute of Technology, Geophysical Fluid Dynamics
# Laboratory, University of Michigan, National Centers for Environmental
# Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
# NASA Goddard Space Flight Center.
# Licensed under the University of Illinois-NCSA License.
# ==============================================================================

mutable struct State
   times_called::Int
   path_to_c_library::String
end

# Initialize to garbage so we ensure that init is called
model_state = State(-10, "")

function init(path_to_c_library::AbstractString)
   model_state.times_called = 0
   model_state.path_to_c_library = path_to_c_library
   println("path to C library: ", path_to_c_library)
end

function run()
   model_state.times_called += 1
   println("model_state times_called: ", model_state.times_called)
end