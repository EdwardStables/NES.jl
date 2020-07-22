#= Addressing Modes =#

#Illegal
function XXX(cpu::cpu_6502)::UInt8
    return 0
end

#Implied
function IMP(cpu::cpu_6502)::UInt8
    #Wraps accumulator addressing by setting `fetched` to the accumulator
    cpu.fetched = cpu.a
    #addr_abs isn't accessed as no additional addresses are needed
    return 0
end

#Immediate
function IMM(cpu::cpu_6502)::UInt8
    #The byte following the instruction contains the operand itself
    cpu.addr_abs = cpu.pc
    #Must advance program counter
    cpu.pc += 1
    return 0
end

#Zero-page
function ZPO(cpu::cpu_6502)::UInt8
    #Mask to give zero-page
    cpu.addr_abs = read(cpu, cpu.pc) & 0x00FF
    cpu.pc += 1
    return 0
end

#Zero-page-indexed to X
function ZPX(cpu::cpu_6502)::UInt8
    cpu.addr_abs = (read(cpu, cpu.pc) + cpu.x) & 0x00FF
    cpu.pc += 1
    return 0
end
#Zero-page-indexed to Y
function ZPY(cpu::cpu_6502)::UInt8
    cpu.addr_abs = (read(cpu, cpu.pc) + cpu.y) & 0x00FF
    cpu.pc += 1
    return 0
end

#Relative
function REL(cpu::cpu_6502)::UInt8
    #Set relative address to specified location
    cpu.addr_rel = read(cpu, cpu.pc)
    cpu.pc += 1

    #addr_rel is unsigned but offset can be negative, therefore implement two's comp
    if cpu.addr_rel & 0x80
        cpu.addr_rel |= 0xFF00
    end

    return 0
end
#Absolute
function ABS(cpu::cpu_6502)::UInt8
    #Second byte gives least signifcant byte of abs address
    cpu.addr_abs = read(cpu, cpu.pc)
    cpu.pc += 1
    #Third byte gives most signifcant byte of abs address
    #(have to force promotion to UInt16 otherwise left shift doesn't work)
    cpu.addr_abs += UInt16(read(cpu, cpu.pc)) << 8
    cpu.pc += 1
    return 0
end

#Absolute-indexed to X
function ABX(cpu::cpu_6502)::UInt8
    cpu.addr_abs = read(cpu, cpu.pc)
    cpu.pc += 1
    cpu.addr_abs += UInt16(read(cpu, cpu.pc)) << 8
    cpu.pc += 1

    cpu.addr_abs += cpu.x

    #Return 1 if offset causes paging
    if (cpu.addr_abs && 0xFF00) != ((cpu.addr_abs - cpu.x) && 0xFF00)
        return 1
    else
        return 0
    end
end

#Absolute-indexed to Y
function ABY(cpu::cpu_6502)::UInt8
    cpu.addr_abs = read(cpu, cpu.pc)
    cpu.pc += 1
    cpu.addr_abs += UInt16(read(cpu, cpu.pc)) << 8
    cpu.pc += 1

    cpu.addr_abs += cpu.y

    if (cpu.addr_abs && 0xFF00) != ((cpu.addr_abs - cpu.y) && 0xFF00)
        return 1
    else
        return 0
    end
end

#Indirect
function IND(cpu::cpu_6502)::UInt8
    #Two bytes of the instruction describe the memory location of the
    #LSB of the target memory location
    cpu.temp = read(cpu, cpu.pc)
    cpu.pc += 1
    cpu.temp += UInt16(read(cpu, cpu.pc)) << 8
    cpu.pc += 1


    #NOTE 6502 BUG HERE
    #If LSB of instruction is 0xFF then the memory location of the MSB of the 
    #target data is on a different page, but 6502 doesn't actually implement this

    #Load LSB as normal
    cpu.addr_abs = read(cpu, cpu.temp)

    if cpu.temp & 0x00FF == 0xFF #Simulate bug
        # the `+1` makes temp's LSB be 0x00, but doesn't affect the MSB
        cpu.addr_abs += UInt16(read(cpu, cpu.temp & 0xFF00)) << 8
    else #don't need to simulate bug
        # MSB is located in the following memory location as expected
        cpu.addr_abs += UInt16(read(cpu, cpu.temp + 1)) << 8
    end

    return 0
end
function IZX(cpu::cpu_6502)::UInt8
    cpu.temp = read(cpu, cpu.pc) + cpu.x
    cpu.pc += 1

    cpu.addr_abs = read(cpu, cpu.temp & 0x00FF) 
    cpu.addr_abs += UInt16(read(cpu, (cpu.temp + 1) & 0x00FF)) << 8

    return 0
end
function IZY(cpu::cpu_6502)::UInt8
    #temp : lower order byte of the effective address
    cpu.temp = read(cpu, cpu.pc)
    cpu.pc += 1
     
    cpu.addr_abs = read(cpu, cpu.temp & 0x00FF)
    cpu.addr_abs += UInt16(read(cpu, (cpu.pc + 1) & 0x00FF)) << 8

    cpu.addr_abs += cpu.y


    if (cpu.addr_abs & 0xFF00) != (cpu.addr_abs - cpu.y & 0xFF00)
        return 1
    else
        return 0
    end
end

#= Instructions =#
#No operation
function NOP(cpu::cpu_6502)::UInt8
    return 0
end

#Loading

#Load acc with mem
function LDA(cpu::cpu_6502)::UInt8
    cpu.a = cpu.fetched

    setFlag(cpu, Z, cpu.a == 0x00)
    setFlag(cpu, N, cpu.a & 0x80)

    return 1
end

#Load x with mem
function LDX(cpu::cpu_6502)::UInt8
    cpu.x = cpu.fetched

    setFlag(cpu, Z, cpu.x == 0x00)
    setFlag(cpu, N, cpu.x & 0x80)

    return 1
end

#Load y with mem
function LDY(cpu::cpu_6502)::UInt8
    cpu.y = cpu.fetched

    setFlag(cpu, Z, cpu.y == 0x00)
    setFlag(cpu, N, cpu.y & 0x80)

    return 1
end

#Store acc in mem
function STA(cpu::cpu_6502)::UInt8
    write(cpu, cpu.addr_abs, cpu.a)
    return 0
end

#Store x in mem
function STX(cpu::cpu_6502)::UInt8
    write(cpu, cpu.addr_abs, cpu.x)
    return 0
end

#Store y in mem
function STY(cpu::cpu_6502)::UInt8
    write(cpu, cpu.addr_abs, cpu.y)
    return 0
end

#Transfer acc to x
function TAX(cpu::cpu_6502)::UInt8
    cpu.x = cpu.a

    setFlag(cpu, Z, cpu.x == 0x00)
    setFlag(cpu, N, cpu.x & 0x80)

    return 0
end

#Transfer acc to y
function TAY(cpu::cpu_6502)::UInt8
    cpu.y = cpu.a

    setFlag(cpu, Z, cpu.y == 0x00)
    setFlag(cpu, N, cpu.y & 0x80)

    return 0
end

#Transfer y to acc
function TXA(cpu::cpu_6502)::UInt8
    cpu.a = cpu.x

    setFlag(cpu, Z, cpu.a == 0x00)
    setFlag(cpu, N, cpu.a & 0x80)

    return 0
end

#Transfer y to acc
function TYA(cpu::cpu_6502)::UInt8
    cpu.a = cpu.y

    setFlag(cpu, Z, cpu.a == 0x00)
    setFlag(cpu, N, cpu.a & 0x80)

    return 0
end

#Transfer sp to x
function TSX(cpu::cpu_6502)::UInt8
    cpu.x = cpu.stkp

    setFlag(cpu, Z, cpu.x == 0x00)
    setFlag(cpu, N, cpu.x & 0x80)

    return 0
end

#Transfer x to sp
function TXS(cpu::cpu_6502)::UInt8
    cpu.stkp = cpu.x
    return 0
end




#Arithmetic
#Add
function ADC(cpu::cpu_6502)::UInt8
    #A + M + C
    #if msb is set then may be negative
    #if inputs are two +ve numbers and result is -ve then overflow
    #if inputs are +ve and -ve then can't overflow
    #if inputs are two -ve numbers and result is +ve then overflow

    cpu.temp = UInt16(cpu.a) + UInt16(cpu.fetched) + UInt16(getFlag(cpu, C))

    #Set Flags
    setFlag(cpu, C, cpu.temp > 255)
    setFlag(cpu, Z, (cpu.temp &0x00FF) == 0)
    setFlag(cpu, N, cpu.temp & 0x80)
    setFlag(cpu, V, 0x0080 & (~(UInt16(cpu.a)⊻UInt16(cpu.fetched)) & (UInt16(cpu.a)⊻cpu.temp)))

    cpu.a = cpu.temp & 0x00FF
    return 1
end

#Sub
function SBC(cpu::cpu_6502)::UInt8
    #A - M - C
    cpu.temp = UInt16(cpu.fetched) & 0xFFFF + 0x0001
    cpu.temp += UInt16(getFlag(cpu, C)) & 0xFFFF + 0x0001

    cpu.temp += UInt16(cpu.a)

    setFlag(cpu, C, cpu.temp > 255)
    setFlag(cpu, Z, (cpu.temp & 0x00FF) == 0)
    setFlag(cpu, N, cpu.temp & 0x0080)
    setFlag(cpu, V, 0x0080 & (~(UInt16(cpu.a)⊻UInt16(cpu.fetched)) & (UInt16(cpu.a)⊻cpu.temp)))

    cpu.a = cpu.temp & 0x00FF
    return 1
end

#Decrement mem
function DEC(cpu::cpu_6502)::UInt8
    cpu.temp = cpu.fetched - 1
    write(cpu, cpu.addr_abs, cpu.temp)

    setFlag(cpu, Z, cpu.temp == 0x0000)
    setFlag(cpu, N, cpu.temp & 0x0080)

    return 0
end

#Decrement x
function DEX(cpu::cpu_6502)::UInt8
    cpu.x -= 1

    setFlag(cpu, Z, cpu.x == 0x0000)
    setFlag(cpu, N, cpu.x & 0x0080)

    return 0
end

#Decrement y
function DEY(cpu::cpu_6502)::UInt8
    cpu.y -= 1

    setFlag(cpu, Z, cpu.y == 0x0000)
    setFlag(cpu, N, cpu.y & 0x0080)

    return 0
end

#increment mem
function INC(cpu::cpu_6502)::UInt8
    cpu.temp = cpu.fetched + 1
    write(cpu, cpu.addr_abs, cpu.temp)

    setFlag(cpu, Z, cpu.temp == 0x0000)
    setFlag(cpu, N, cpu.temp & 0x0080)

    return 0
end

#increment x
function INX(cpu::cpu_6502)::UInt8
    cpu.x += 1

    setFlag(cpu, Z, cpu.x == 0x0000)
    setFlag(cpu, N, cpu.x & 0x0080)

    return 0
end

#increment y
function INY(cpu::cpu_6502)::UInt8
    cpu.y += 1

    setFlag(cpu, Z, cpu.y == 0x0000)
    setFlag(cpu, N, cpu.y & 0x0080)

    return 0
end

#Flag set and clear
#clear carry
function CLC(cpu::cpu_6502)::UInt8
    setFlag(cpu, C, false)
    return 0
end

#set carry
function SEC(cpu::cpu_6502)::UInt8
    setFlag(cpu, C, true)
    return 0
end

#clear interrupt disable
function CLI(cpu::cpu_6502)::UInt8
    setFlag(cpu, I, false)
    return 0
end

#set interrupt disable
function SEI(cpu::cpu_6502)::UInt8
    setFlag(cpu, I, true)
    return 0
end

#clear overflow
function CLV(cpu::cpu_6502)::UInt8
    setFlag(cpu, V, false)
    return 0
end

#clear decimal
function CLD(cpu::cpu_6502)::UInt8
    setFlag(cpu, D, false)
    return 0
end

#set decimal
function SED(cpu::cpu_6502)::UInt8
    setFlag(cpu, D, true)
    return 0
end

#Branching

#Branch on plus
function BPL(cpu::cpu_6502)::UInt8
    if !getFlag(cpu, N)
        cpu.cycles += 1 #Add 1 clock cycle if branch is taken
        cpu.addr_abs = cpu.pc + cpu.addr_rel

        if (cpu.addr_abs & 0xFF00) != (cpu.pc & 0xFF00)
            cpu.cycles += 1 #Add 1 more clock cycle if page changes
        end

        cpu.pc = cpu.addr_abs
    end
    return 0
end

#Branch on minus
function BMI(cpu::cpu_6502)::UInt8
    if getFlag(cpu, N)
        cpu.cycles += 1 #Add 1 clock cycle if branch is taken
        cpu.addr_abs = cpu.pc + cpu.addr_rel

        if (cpu.addr_abs & 0xFF00) != (cpu.pc & 0xFF00)
            cpu.cycles += 1 #Add 1 more clock cycle if page changes
        end

        cpu.pc = cpu.addr_abs
    end
    return 0
end

#Branch on overflow clear 
function BVC(cpu::cpu_6502)::UInt8
    if !getFlag(cpu, V)
        cpu.cycles += 1 #Add 1 clock cycle if branch is taken
        cpu.addr_abs = cpu.pc + cpu.addr_rel

        if (cpu.addr_abs & 0xFF00) != (cpu.pc & 0xFF00)
            cpu.cycles += 1 #Add 1 more clock cycle if page changes
        end

        cpu.pc = cpu.addr_abs
    end
    return 0
end

#Branch on overflow set
function BVS(cpu::cpu_6502)::UInt8
    if getFlag(cpu, V)
        cpu.cycles += 1 #Add 1 clock cycle if branch is taken
        cpu.addr_abs = cpu.pc + cpu.addr_rel

        if (cpu.addr_abs & 0xFF00) != (cpu.pc & 0xFF00)
            cpu.cycles += 1 #Add 1 more clock cycle if page changes
        end

        cpu.pc = cpu.addr_abs
    end
    return 0
end

#Branch on carry clear 
function BCC(cpu::cpu_6502)::UInt8
    if !getFlag(cpu, C)
        cpu.cycles += 1 #Add 1 clock cycle if branch is taken
        cpu.addr_abs = cpu.pc + cpu.addr_rel

        if (cpu.addr_abs & 0xFF00) != (cpu.pc & 0xFF00)
            cpu.cycles += 1 #Add 1 more clock cycle if page changes
        end

        cpu.pc = cpu.addr_abs
    end
    return 0
end

#Branch on carry set
function BCS(cpu::cpu_6502)::UInt8
    if getFlag(cpu, C)
        cpu.cycles += 1 #Add 1 clock cycle if branch is taken
        cpu.addr_abs = cpu.pc + cpu.addr_rel

        if (cpu.addr_abs & 0xFF00) != (cpu.pc & 0xFF00)
            cpu.cycles += 1 #Add 1 more clock cycle if page changes
        end

        cpu.pc = cpu.addr_abs
    end
    return 0
end

#Branch on equal
function BEQ(cpu::cpu_6502)::UInt8
    if getFlag(cpu, Z)
        cpu.cycles += 1 #Add 1 clock cycle if branch is taken
        cpu.addr_abs = cpu.pc + cpu.addr_rel

        if (cpu.addr_abs & 0xFF00) != (cpu.pc & 0xFF00)
            cpu.cycles += 1 #Add 1 more clock cycle if page changes
        end

        cpu.pc = cpu.addr_abs
    end
    return 0
end

#Branch on not equal
function BNE(cpu::cpu_6502)::UInt8
    if !getFlag(cpu, Z)
        cpu.cycles += 1 #Add 1 clock cycle if branch is taken
        cpu.addr_abs = cpu.pc + cpu.addr_rel

        if (cpu.addr_abs & 0xFF00) != (cpu.pc & 0xFF00)
            cpu.cycles += 1 #Add 1 more clock cycle if page changes
        end

        cpu.pc = cpu.addr_abs
    end
    return 0
end

#Jump
#jump to new location
function JMP(cpu::cpu_6502)::UInt8
    cpu.pc = cpu.addr_abs

    return 0
end

#jump to  new location and save return address to stack
function JSR(cpu::cpu_6502)::UInt8
    cpu.temp = cpu.pc - 1
    write(cpu, 0x0100 + cpu.stkp, UInt8(cpu.temp >> 8))
    cpu.stkp -= 1
    write(cpu, 0x0100 + cpu.stkp, UInt8(cpu.temp & 0x00FF))
    cpu.stkp -= 1

    cpu.pc = cpu.fetched

    return 0
end

function RTS(cpu::cpu_6502)::UInt8
    cpu.temp = read(cpu, 0x0100 + cpu.stkp + 1)
    cpu.stkp += 1
    cpu.temp += UInt16(read(cpu, 0x0100 + cpu.stkp + 1)) << 8
    cpu.stkp += 1

    cpu.pc = cpu.temp

    return 0
end

#Stack
#Push acc to stack
function PHA(cpu::cpu_6502)::UInt8
    write(cpu, 0x0100 + cpu.stkp, cpu.a)
    cpu.stkp -= 1
    return 0
end

#Pop acc from stack
function PLA(cpu::cpu_6502)::UInt8
    cpu.a = read(cpu, 0x0100 + cpu.stkp + 1) 
    cpu.stkp += 1

    setFlag(cpu, Z, cpu.a == 0x00)
    setFlag(cpu, N, cpu.a & 0x80)

    return 0
end

#Push status to stack
function PHP(cpu::cpu_6502)::UInt8
    write(cpu, 0x0100 + cpu.stkp, getStatusWord(cpu))
    cpu.stkp -= 1
    return 0
end

#Pop status from stack
function PLP(cpu::cpu_6502)::UInt8
    read(cpu, 0x0100 + cpu.stkp + 1) 
    cpu.stkp += 1
    return 0
end


#Bit logic
#And mem with acc
function AND(cpu::cpu_6502)::UInt8
    cpu.a &= cpu.fetched
    setFlag(cpu, Z, cpu.a == 0x00)
    setFlag(cpu, N, cpu.a & 0x80)
    return 1
end

#Or mem with acc
function ORA(cpu::cpu_6502)::UInt8
    cpu.a |= cpu.fetched

    setFlag(cpu, Z, cpu.a == 0x00)
    setFlag(cpu, N, cpu.a & 0x80)

    return 1
end

#Xor mem with acc
function EOR(cpu::cpu_6502)::UInt8
    cpu.a = cpu.a ⊻ cpu.fetched

    setFlag(cpu, Z, cpu.a == 0x00)
    setFlag(cpu, N, cpu.a & 0x80)

    return 1
end

#Arithmetic shift left
function ASL(cpu::cpu_6502)::UInt8
    cpu.temp = UInt16(cpu.fetched) << 1

    setFlag(cpu, Z, cpu.temp & 0x00FF == 0x00)
    setFlag(cpu, C, cpu.temp & 0x0100)
    setFlag(cpu, N, cpu.temp & 0x80)

    if cpu.opcode == IMP
        cpu.a = cpu.temp & 0x00FF
    else
        write(cpu, cpu.addr_abs, cpu.temp & 0x00FF)
    end

    return 0
end

#Logical shift right
function LSR(cpu::cpu_6502)::UInt8
    cpu.temp = UInt16(cpu.fetched) >>> 1

    setFlag(cpu, Z, cpu.temp & 0x00FF == 0x00)
    setFlag(cpu, C, cpu.fetched & 0x01)
    setFlag(cpu, N, cpu.temp & 0x0080)

    if cpu.opcode == IMP
        cpu.a = cpu.temp & 0x00FF
    else
        write(cpu, cpu.addr_abs, cpu.temp & 0x00FF)
    end

    return 0
end

function ROL(cpu::cpu_6502)::UInt8
    #cast to 16 bit first
    cpu.temp = cpu.fetched
    #bitrotate is part of 1.5
    cpu.temp = bitrotate(cpu.temp, 1)
    #lsb should be the carry bit
    cpu.temp += getFlag(cpu, C) ? 0x0001 : 0x0000

    setFlag(cpu, Z, cpu.temp & 0x00FF == 0x00)
    setFlag(cpu, C, cpu.temp & 0x0100)
    setFlag(cpu, N, cpu.temp & 0x0080)

    if cpu.opcode.addrmode == IMP
        cpu.a = cpu.temp & 0x00FF
    else
        write(cpu, cpu.addr_abs, cpu.temp & 0x00FF)
    end

    return 0
end

function ROR(cpu::cpu_6502)::UInt8
    #bitrotate is part of 1.5
    cpu.temp = bitrotate(cpu.fetched, -1)
    #lsb should be the carry bit
    cpu.temp += getFlag(cpu, C) ? 0x0080 : 0x0000

    setFlag(cpu, Z, cpu.temp & 0x00FF == 0x00)
    setFlag(cpu, C, cpu.temp & 0x8000)
    setFlag(cpu, N, cpu.temp & 0x0080)

    if cpu.opcode.addrmode == IMP
        cpu.a = cpu.temp & 0x00FF
    else
        write(cpu, cpu.addr_abs, cpu.temp & 0x00FF)
    end

    return 0
end


#Comparison
#Test bits in memory against acc
function BIT(cpu::cpu_6502)::UInt8
    setFlag(cpu, N, cpu.fetched & 0x80)
    setFlag(cpu, V, cpu.fetched & 0x40)
    cpu.temp = cpu.a & cpu.fetched
    setFlag(cpu, Z, cpu.temp == 0x00)
    return 0
end

#Compare mem with acc
function CMP(cpu::cpu_6502)::UInt8
    setFlag(cpu, C, cpu.a >= cpu.fetched)
    setFlag(cpu, Z, cpu.a == cpu.fetched)
    cpu.temp = cpu.a - cpu.fetched
    setFlag(cpu, N, cpu.temp & 0x0080)
    return 1
end

#Compare mem with x 
function CPX(cpu::cpu_6502)::UInt8
    setFlag(cpu, C, cpu.x >= cpu.fetched)
    setFlag(cpu, Z, cpu.x == cpu.fetched)
    cpu.temp = cpu.x - cpu.fetched
    setFlag(cpu, N, cpu.temp & 0x0080)
    return 1
end

#Compare mem with y
function CPY(cpu::cpu_6502)::UInt8
    setFlag(cpu, C, cpu.y >= cpu.fetched)
    setFlag(cpu, Z, cpu.y == cpu.fetched)
    cpu.temp = cpu.y - cpu.fetched
    setFlag(cpu, N, cpu.temp & 0x0080)
    return 1
end

#Iterrupt

#Break
function BRK(cpu::cpu_6502)::UInt8
    cpu.pc += 1
    setFlag(cpu, I, true)

    write(cpu, 0x0100 + cpu.stkp, ((cpu.pc & 0xFF00) >> 8) & 0x00FF)
    cpu.stkp -= 1
    write(cpu, 0x0100 + cpu.stkp, cpu.pc & 0x00FF)
    cpu.stkp -= 1

    setFlag(cpu, B, true)
    #B flag doesn't exist in hardware, is introduced here for being pushed to the stack
    write(cpu, 0x0100 + cpu.stkp, getStatusWord(cpu))
    cpu.stkp -= 1
    #Immediately set it back to false as it doesn't have hardware
    setFlag(cpu, B, false)

    #ISR address is stored at 0xFFFF,0xFFFE (MSB, LSB)
    cpu.pc = UInt16(read(cpu, 0xFFFF)) << 8 | UInt16(read(cpu, 0xFFFE))

    return 0
end

#Return from interrupt
function RTI(cpu::cpu_6502)::UInt8
    #Pop status
    setStatusWord(cpu, read(cpu, 0x0100 + cpu.stkp + 1))
    cpu.stkp += 1

    cpu.status &= ~UInt8(emulator.B)
    cpu.status &= ~UInt8(emulator.U)

    cpu.pc = read(cpu, 0x0100 + cpu.stkp + 1)
    cpu.stkp += 1
    cpu.status |= UInt16(read(cpu, 0x0100 + cpu.stkp + 1)) << 8
    cpu.stkp += 1

    return 0
end

 
#= Opcodes =#

const O = Operation
const lookup = [
    O( "BRK", BRK, IMM, 7 ) O( "ORA", ORA, IZX, 6 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "???", NOP, IMP, 3 ) O( "ORA", ORA, ZPO, 3 ) O( "ASL", ASL, ZPO, 5 ) O( "???", XXX, IMP, 5 ) O( "PHP", PHP, IMP, 3 ) O( "ORA", ORA, IMM, 2 ) O( "ASL", ASL, IMP, 2 ) O( "???", XXX, IMP, 2 ) O( "???", NOP, IMP, 4 ) O( "ORA", ORA, ABS, 4 ) O( "ASL", ASL, ABS, 6 ) O( "???", XXX, IMP, 6 );
    O( "JSR", JSR, ABS, 6 ) O( "AND", AND, IZX, 6 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "BIT", BIT, ZPO, 3 ) O( "AND", AND, ZPO, 3 ) O( "ROL", ROL, ZPO, 5 ) O( "???", XXX, IMP, 5 ) O( "PLP", PLP, IMP, 4 ) O( "AND", AND, IMM, 2 ) O( "ROL", ROL, IMP, 2 ) O( "???", XXX, IMP, 2 ) O( "BIT", BIT, ABS, 4 ) O( "AND", AND, ABS, 4 ) O( "ROL", ROL, ABS, 6 ) O( "???", XXX, IMP, 6 );
    O( "BMI", BMI, REL, 2 ) O( "AND", AND, IZY, 5 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "???", NOP, IMP, 4 ) O( "AND", AND, ZPX, 4 ) O( "ROL", ROL, ZPX, 6 ) O( "???", XXX, IMP, 6 ) O( "SEC", SEC, IMP, 2 ) O( "AND", AND, ABY, 4 ) O( "???", NOP, IMP, 2 ) O( "???", XXX, IMP, 7 ) O( "???", NOP, IMP, 4 ) O( "AND", AND, ABX, 4 ) O( "ROL", ROL, ABX, 7 ) O( "???", XXX, IMP, 7 );
    O( "RTI", RTI, IMP, 6 ) O( "EOR", EOR, IZX, 6 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "???", NOP, IMP, 3 ) O( "EOR", EOR, ZPO, 3 ) O( "LSR", LSR, ZPO, 5 ) O( "???", XXX, IMP, 5 ) O( "PHA", PHA, IMP, 3 ) O( "EOR", EOR, IMM, 2 ) O( "LSR", LSR, IMP, 2 ) O( "???", XXX, IMP, 2 ) O( "JMP", JMP, ABS, 3 ) O( "EOR", EOR, ABS, 4 ) O( "LSR", LSR, ABS, 6 ) O( "???", XXX, IMP, 6 );
    O( "BVC", BVC, REL, 2 ) O( "EOR", EOR, IZY, 5 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "???", NOP, IMP, 4 ) O( "EOR", EOR, ZPX, 4 ) O( "LSR", LSR, ZPX, 6 ) O( "???", XXX, IMP, 6 ) O( "CLI", CLI, IMP, 2 ) O( "EOR", EOR, ABY, 4 ) O( "???", NOP, IMP, 2 ) O( "???", XXX, IMP, 7 ) O( "???", NOP, IMP, 4 ) O( "EOR", EOR, ABX, 4 ) O( "LSR", LSR, ABX, 7 ) O( "???", XXX, IMP, 7 );
    O( "RTS", RTS, IMP, 6 ) O( "ADC", ADC, IZX, 6 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "???", NOP, IMP, 3 ) O( "ADC", ADC, ZPO, 3 ) O( "ROR", ROR, ZPO, 5 ) O( "???", XXX, IMP, 5 ) O( "PLA", PLA, IMP, 4 ) O( "ADC", ADC, IMM, 2 ) O( "ROR", ROR, IMP, 2 ) O( "???", XXX, IMP, 2 ) O( "JMP", JMP, IND, 5 ) O( "ADC", ADC, ABS, 4 ) O( "ROR", ROR, ABS, 6 ) O( "???", XXX, IMP, 6 );
    O( "BVS", BVS, REL, 2 ) O( "ADC", ADC, IZY, 5 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "???", NOP, IMP, 4 ) O( "ADC", ADC, ZPX, 4 ) O( "ROR", ROR, ZPX, 6 ) O( "???", XXX, IMP, 6 ) O( "SEI", SEI, IMP, 2 ) O( "ADC", ADC, ABY, 4 ) O( "???", NOP, IMP, 2 ) O( "???", XXX, IMP, 7 ) O( "???", NOP, IMP, 4 ) O( "ADC", ADC, ABX, 4 ) O( "ROR", ROR, ABX, 7 ) O( "???", XXX, IMP, 7 );
    O( "???", NOP, IMP, 2 ) O( "STA", STA, IZX, 6 ) O( "???", NOP, IMP, 2 ) O( "???", XXX, IMP, 6 ) O( "STY", STY, ZPO, 3 ) O( "STA", STA, ZPO, 3 ) O( "STX", STX, ZPO, 3 ) O( "???", XXX, IMP, 3 ) O( "DEY", DEY, IMP, 2 ) O( "???", NOP, IMP, 2 ) O( "TXA", TXA, IMP, 2 ) O( "???", XXX, IMP, 2 ) O( "STY", STY, ABS, 4 ) O( "STA", STA, ABS, 4 ) O( "STX", STX, ABS, 4 ) O( "???", XXX, IMP, 4 );
    O( "BCC", BCC, REL, 2 ) O( "STA", STA, IZY, 6 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 6 ) O( "STY", STY, ZPX, 4 ) O( "STA", STA, ZPX, 4 ) O( "STX", STX, ZPY, 4 ) O( "???", XXX, IMP, 4 ) O( "TYA", TYA, IMP, 2 ) O( "STA", STA, ABY, 5 ) O( "TXS", TXS, IMP, 2 ) O( "???", XXX, IMP, 5 ) O( "???", NOP, IMP, 5 ) O( "STA", STA, ABX, 5 ) O( "???", XXX, IMP, 5 ) O( "???", XXX, IMP, 5 );
    O( "LDY", LDY, IMM, 2 ) O( "LDA", LDA, IZX, 6 ) O( "LDX", LDX, IMM, 2 ) O( "???", XXX, IMP, 6 ) O( "LDY", LDY, ZPO, 3 ) O( "LDA", LDA, ZPO, 3 ) O( "LDX", LDX, ZPO, 3 ) O( "???", XXX, IMP, 3 ) O( "TAY", TAY, IMP, 2 ) O( "LDA", LDA, IMM, 2 ) O( "TAX", TAX, IMP, 2 ) O( "???", XXX, IMP, 2 ) O( "LDY", LDY, ABS, 4 ) O( "LDA", LDA, ABS, 4 ) O( "LDX", LDX, ABS, 4 ) O( "???", XXX, IMP, 4 );
    O( "BCS", BCS, REL, 2 ) O( "LDA", LDA, IZY, 5 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 5 ) O( "LDY", LDY, ZPX, 4 ) O( "LDA", LDA, ZPX, 4 ) O( "LDX", LDX, ZPY, 4 ) O( "???", XXX, IMP, 4 ) O( "CLV", CLV, IMP, 2 ) O( "LDA", LDA, ABY, 4 ) O( "TSX", TSX, IMP, 2 ) O( "???", XXX, IMP, 4 ) O( "LDY", LDY, ABX, 4 ) O( "LDA", LDA, ABX, 4 ) O( "LDX", LDX, ABY, 4 ) O( "???", XXX, IMP, 4 );
    O( "CPY", CPY, IMM, 2 ) O( "CMP", CMP, IZX, 6 ) O( "???", NOP, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "CPY", CPY, ZPO, 3 ) O( "CMP", CMP, ZPO, 3 ) O( "DEC", DEC, ZPO, 5 ) O( "???", XXX, IMP, 5 ) O( "INY", INY, IMP, 2 ) O( "CMP", CMP, IMM, 2 ) O( "DEX", DEX, IMP, 2 ) O( "???", XXX, IMP, 2 ) O( "CPY", CPY, ABS, 4 ) O( "CMP", CMP, ABS, 4 ) O( "DEC", DEC, ABS, 6 ) O( "???", XXX, IMP, 6 );
    O( "BNE", BNE, REL, 2 ) O( "CMP", CMP, IZY, 5 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "???", NOP, IMP, 4 ) O( "CMP", CMP, ZPX, 4 ) O( "DEC", DEC, ZPX, 6 ) O( "???", XXX, IMP, 6 ) O( "CLD", CLD, IMP, 2 ) O( "CMP", CMP, ABY, 4 ) O( "NOP", NOP, IMP, 2 ) O( "???", XXX, IMP, 7 ) O( "???", NOP, IMP, 4 ) O( "CMP", CMP, ABX, 4 ) O( "DEC", DEC, ABX, 7 ) O( "???", XXX, IMP, 7 );
    O( "CPX", CPX, IMM, 2 ) O( "SBC", SBC, IZX, 6 ) O( "???", NOP, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "CPX", CPX, ZPO, 3 ) O( "SBC", SBC, ZPO, 3 ) O( "INC", INC, ZPO, 5 ) O( "???", XXX, IMP, 5 ) O( "INX", INX, IMP, 2 ) O( "SBC", SBC, IMM, 2 ) O( "NOP", NOP, IMP, 2 ) O( "???", SBC, IMP, 2 ) O( "CPX", CPX, ABS, 4 ) O( "SBC", SBC, ABS, 4 ) O( "INC", INC, ABS, 6 ) O( "???", XXX, IMP, 6 );
    O( "BEQ", BEQ, REL, 2 ) O( "SBC", SBC, IZY, 5 ) O( "???", XXX, IMP, 2 ) O( "???", XXX, IMP, 8 ) O( "???", NOP, IMP, 4 ) O( "SBC", SBC, ZPX, 4 ) O( "INC", INC, ZPX, 6 ) O( "???", XXX, IMP, 6 ) O( "SED", SED, IMP, 2 ) O( "SBC", SBC, ABY, 4 ) O( "NOP", NOP, IMP, 2 ) O( "???", XXX, IMP, 7 ) O( "???", NOP, IMP, 4 ) O( "SBC", SBC, ABX, 4 ) O( "INC", INC, ABX, 7 ) O( "???", XXX, IMP, 7 )
]


function load_opcode(opcode::UInt8)
    str = string(opcode,base=16)
    msd = parse(Int,str[1], base=16) + 1
    lsd = parse(Int,str[2], base=16) + 1

    return lookup[msd, lsd]
end