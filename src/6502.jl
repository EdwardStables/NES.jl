
#=
Address: 16 bit
Data: 8 bit
=#

#= Flags =#
#=
C: Carry 
Z: Zero
I: Interrupt Disable
D: Decimal (no effect for NES)
B: 'B Flag' bit 1
U: 'B Flag' bit 2
V: Overflow
N: Negative
=#
@enum FLAGS C Z I D B U V N

struct Operation
    name::String
    action::Function
    addrmode::Function
    cycles::UInt8
end

mutable struct cpu_6502 <: BusDevice
    bus::Bus
    
    #= Registers =#
    a::UInt8 #Accumulator
    x::UInt8 #X Register
    y::UInt8 #Y Register
    stkp::UInt8 #Stack Pointer
    pc::UInt16 #Program Counter
    status::Vector{Bool} #Status Register

    #= Helper variables =#
    fetched::UInt8
    addr_abs::UInt16
    addr_rel::UInt16
    opcode::UInt8
    cycles::UInt8

    temp::UInt16 #used in many places to avoid extra allocations 


    function cpu_6502()
        c = new()
        c.bus = Bus

        c.a      = 0x00
        c.x      = 0x00
        c.y      = 0x00
        c.stkp   = 0xFF
        c.pc     = 0x0000
        c.status = fill(false, 8)

        fetched  = 0x00
        addr_abs = 0x0000
        addr_rel = 0x0000
        opcode   = 0x00
        cycles   = 0x00
    end
end

read(cpu::cpu_6502, addr::UInt16)::UInt8 = read(cpu.bus, addr)
write(cpu::cpu_6502, addr::UInt16, data::UInt8) = write(cpu.bus, addr, data)

function getStatusWord(cpu::cpu_6502)
    v = 0x00
    for (i,f) in enumerate(Iterators.reverse(cpu.status))
        v += UInt8(f) << (i-1)
    end
    return v
end

function setStatusWord(cpu::cpu_6502, word::UInt8)
    cpu.status = [b == '1' for b in word]
end

getFlag(cpu::cpu_6502, f::FLAGS)::Bool = cpu.status[f]

#If a numerical value is given then set flag if non-zero
setFlag(cpu::cpu_6502, f::FLAGS, n::UInt) = setFlag(cpu, f, n != 0)
function setFlag(cpu::cpu_6502, f::FLAGS, s::Bool) 
    cpu.status[f] = s
end

#= Interface =#
function clock(cpu::cpu_6502)
    if cpu.cycles == 0
        cpu.opcode = read(cpu, cpu.pc)
        cpu.pc += 1

        operation = load_opcode(cpu.opcode)

        fetch(cpu, operation)
        
        #= todo these functions =#
        cpu.cycles = operation.cycles
        extra_cycle_1 = operation.addrmode(cpu)
        extra_cycle_2 = operation.action(cpu)

        cpu.cycles += extra_cycle_1 & extra_cycle_2
    end
    cpu.cycles -= 1
end

function reset(cpu::cpu_6502) 
    cpu.a = 0x00
    cpu.x = 0x00
    cpu.y = 0x00

    cpu.stkp = 0xFF
    status = setStatusWord(cpu, 0x00)


    #6502 reads address at 0xFFFC on reset
    addr = 0xFFFC
    cpu.pc = read(cpu, addr)
    cpu.pc |= UInt16(read(cpu, addr+1)) << 8

    cpu.addr_abs = 0x0000
    cpu.addr_rel = 0x0000
    cpu.fetched = 0x00

    #simulate the time taken for an interrupt
    cpu.cycles = 8
end
function irq(cpu::cpu_6502) 
    if getFlag(cpu, I) == 0
        write(cpu, 0x0100 + cpu.stkp, (cpu.pc & 0xFF00) >> 8)
        cpu.stkp -= 1
        write(cpu, 0x0100 + cpu.stkp, cpu.pc & 0x00FF)
        cpu.stkp -= 1

        setFlag(cpu, B, 0)
        setFlag(cpu, U, 1)
        setFlag(cpu, I, 1)

        write(cpu, 0x0100 + cpu.stkp, getStatusWord(cpu))
        cpu.stkp -= 1

        addr = 0xFFFE
        cpu.pc = read(cpu, addr)
        cpu.pc |= UInt16(read(cpu, addr+1)) << 8

        cpu.cycles = 7

    end
end

function nmi(cpu::cpu_6502)
    write(cpu, 0x0100 + cpu.stkp, (cpu.pc & 0xFF00) >> 8)
    cpu.stkp -= 1
    write(cpu, 0x0100 + cpu.stkp, cpu.pc & 0x00FF)
    cpu.stkp -= 1

    setFlag(cpu, B, 0)
    setFlag(cpu, U, 1)
    setFlag(cpu, I, 1)

    write(cpu, 0x0100 + cpu.stkp, getStatusWord(cpu))
    cpu.stkp -= 1

    addr = 0xFFFA
    cpu.pc = read(cpu, addr)
    cpu.pc |= UInt16(read(cpu, addr+1)) << 8

    cpu.cycles = 8
end

function fetch(cpu::cpu_6502, op::Operation) 
    #If address mode is implied then fetched has already been
    #set to acc
    if op.addrmode != IMP
        cpu.fetched = read(cpu, cpu.addr_abs)
    end
end