module emulator

abstract type BusDevice end
memrange(d::BusDevice) = range(d.range.first, d.range.second, step=1)

#= Devices =#
include("ram.jl")
#= Bus =#
include("bus.jl")
#= CPU =#
include("6502.jl")
include("instructions.jl")


end # module
