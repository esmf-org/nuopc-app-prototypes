mutable struct State
   times_called::Int
end

# Initialize to garbage so we ensure that init is called
model_state = State(-10)

function init()
   model_state.times_called = 0
end

function run()
   model_state.times_called += 1
   println("model_state times_called: ", model_state.times_called)
end