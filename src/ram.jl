mutable struct Ram <: BusDevice
    range::Pair{UInt16,UInt16}

    memory::Vector{UInt8}

    function Ram(lo::UInt16, hi::UInt16) 
        r = new()
        r.range = lo=>hi
        r.memory = zeros(64*1024)
    end
end

function clear(r::Ram)
    r.memory = zeros(64*1024)
end

read(r::Ram, addr::UInt16) = r.memory[addr]

function write(r::Ram, addr::UInt16, data::UInt8)
    r.memory[addr] = data
end