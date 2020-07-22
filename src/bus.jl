mutable struct Bus
    devices::Vector{BusDevice}
    addr::UInt16
    data::UInt8

    function Bus()
        b = new()

        b.devices = [Ram(0x0000, 0xFFFF)]
    end
end 

function write(bus::Bus, addr::UInt16, data::UInt8)
    for device in bus.devices
        if addr in memrange(device)
            write(device, addr, data)
            return
        end
    end
end

function read(bus::Bus, addr::UInt16; bReadonly::Bool = false)::UInt8
    for device in bus.devices
        if addr in memrange(device)
            return read(device, addr)
        end
    end
    return 0x00
end