#include <stdio.h>
#include <stdlib.h>
#include "sim86.h"

memory MEMORY = {};
b8 SIMULATE = true;
state STATE = {};
state OLD_STATE = {};
char Flag_Names[] = {'O', 'D', 'I', 'T', 'S', 'Z', '\000', 'A', '\000', 'P', '\000', 'C'};
char *Byte_Registers[] = {"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"};
char *Word_Registers[] = {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di"};
char *Segment_Reg_Names[] = {"es", "cs", "ss", "ds"};
char *Rm_Mem_Table[] = {"bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx"};
char *Instr_Names[] = {"add", "or", "adc", "sbb", "and", "sub", "xor", "cmp", "mov"};
char *Jump_Names[] = {"jo", "jno", "jb", "jnb", "je", "jne", "jbe", "jnbe", "js", "jns", "jp", "jnp", "jl", "jnl", "jle", "jnle"};
char *Loop_Names[] = {"loopnz", "loopz", "loop", "jcxz"};

instr All_Instrs[256] = {};
instr Handled_Instrs[] = {
{0x0, ADD, I_REG_REGMEM, 3},
{0x1, ADD, I_REG_REGMEM, 3},
{0x2, ADD, I_REG_REGMEM, 3},
{0x3, ADD, I_REG_REGMEM, 3},
{0x4, ADD, I_ACC, 1},
{0x5, ADD, I_ACC, 2},
{0x28, SUB, I_REG_REGMEM, 3},
{0x29, SUB, I_REG_REGMEM, 3},
{0x2A, SUB, I_REG_REGMEM, 3},
{0x2B, SUB, I_REG_REGMEM, 3},
{0x2C, SUB,I_ACC, 1},
{0x2D, SUB,I_ACC, 2},
{0x38, CMP, I_REG_REGMEM, 3},
{0x39, CMP, I_REG_REGMEM, 3},
{0x3A, CMP, I_REG_REGMEM, 3},
{0x3B, CMP, I_REG_REGMEM, 3},
{0x3C, CMP, I_ACC, 1},
{0x3D, CMP, I_ACC, 2},
{0x70, JMP, I_JUMP},
{0x71, JMP, I_JUMP},
{0x72, JMP, I_JUMP},
{0x73, JMP, I_JUMP},
{0x74, JMP, I_JUMP},
{0x75, JMP, I_JUMP},
{0x76, JMP, I_JUMP},
{0x77, JMP, I_JUMP},
{0x78, JMP, I_JUMP},
{0x79, JMP, I_JUMP},
{0x7A, JMP, I_JUMP},
{0x7B, JMP, I_JUMP},
{0x7C, JMP, I_JUMP},
{0x7D, JMP, I_JUMP},
{0x7E, JMP, I_JUMP},
{0x7F, JMP, I_JUMP},
{0x80, ANY, I_IMM_REGMEM, 4},
{0x81, ANY, I_IMM_REGMEM, 5},
{0x82, ANY, I_IMM_REGMEM, 4},
{0x83, ANY, I_IMM_REGMEM, 4},
{0x88, MOV, I_REG_REGMEM, 3},
{0x89, MOV, I_REG_REGMEM, 3},
{0x8A, MOV, I_REG_REGMEM, 3},
{0x8B, MOV, I_REG_REGMEM, 3},
{0x8C, MOV, I_REG_REGMEM, 3},
{0x8E, MOV, I_REG_REGMEM, 3},
{0xA0, MOV, I_ACC, 2},
{0xA1, MOV, I_ACC, 2},
{0xA2, MOV, I_ACC, 2},
{0xA3, MOV, I_ACC, 2},
{0xB0, MOV, I_MOV, 1},
{0xB1, MOV, I_MOV, 1},
{0xB2, MOV, I_MOV, 1},
{0xB3, MOV, I_MOV, 1},
{0xB4, MOV, I_MOV, 1},
{0xB5, MOV, I_MOV, 1},
{0xB6, MOV, I_MOV, 1},
{0xB7, MOV, I_MOV, 1},
{0xB8, MOV, I_MOV, 2},
{0xB9, MOV, I_MOV, 2},
{0xBA, MOV, I_MOV, 2},
{0xBB, MOV, I_MOV, 2},
{0xBC, MOV, I_MOV, 2},
{0xBD, MOV, I_MOV, 2},
{0xBE, MOV, I_MOV, 2},
{0xBF, MOV, I_MOV, 2},
{0xC6, MOV, I_IMM_REGMEM, 4},
{0xC7, MOV, I_IMM_REGMEM, 5},
{0xE0, ANY, I_LOOP, 1},
{0xE1, ANY, I_LOOP, 1},
{0xE2, ANY, I_LOOP, 1},
{0xE3, ANY, I_LOOP, 1}

};


i16 U8ToI16(u8 high, u8 low) {
    i16 result = ((i16)high << 8 | low);
    return result;
}

u8 PopBuffer(buffer *buff) {
    buff->index = STATE.ip;
    u8 result = *(u8*)(buff->buffer + buff->index);
    STATE.ip += 1;
    return result;
}

static inline void SetIP(i8 disp) {
    STATE.ip += disp;
}

static inline u16 GetRmValues(u8 reg) {
    u16 values[8] = {};
    values[0] = STATE.bx.full + STATE.si.full;
    values[1] = STATE.bx.full + STATE.di.full;
    values[2] = STATE.bp.full + STATE.si.full;
    values[3] = STATE.bp.full + STATE.di.full;
    values[4] = STATE.si.full;
    values[5] = STATE.di.full;
    values[6] = STATE.bp.full;
    values[7] = STATE.bx.full;
    return values[reg];
}

void PrintFlags(Flags flags, char *string) {
    u16 flags_state = flags.all;
    u8 valid_flags[12] = {1,1,1,1,1,1,0,1,0,1,0,1};
    char flags_changed[9] = {};
    for (int i = ARRAY_SIZE(valid_flags) - 1, count = 0; i >= 0; i--) {
        b8 valid = valid_flags[i];
        b8 state = flags_state & 0b1;
        if (valid && state) {
            count += sprintf(flags_changed + count, "%c ", Flag_Names[i]);
        }
        flags_state >>= 1;
    }
    sprintf(string, "%s", flags_changed);
}

void PrintState(char *asm_string, b8 show_diff) {
    char *msg = (asm_string) ? asm_string : "\nFinal Registers";

    printf("%s:\n", msg);
    for (int i = 0; i < ARRAY_SIZE(STATE.registers); i++) {
        u16 value = STATE.registers[i].full;
        u16 old_value = (show_diff) ? OLD_STATE.registers[i].full : value;
        u16 diff = (value != old_value);
        if (diff) {
            printf("\t\t%s: 0x%hx -> 0x%hx\n", Word_Registers[i], old_value, value);
        } else if (value) {
            printf("\t\t%s: 0x%hx\n", Word_Registers[i], value);
        }
    }

    for (int i = 0; i < ARRAY_SIZE(STATE.segment_registers); i++) {
        u16 value = STATE.segment_registers[i].full;
        u16 old_value = (show_diff) ? OLD_STATE.segment_registers[i].full : value;
        u16 diff = (value != old_value);
        if (diff) {
            printf("\t%s: 0x%x\n", Segment_Reg_Names[i], STATE.segment_registers[i].full);
        }
    }

    printf("\t\tip: 0x%hx -> 0x%hx\n", OLD_STATE.ip, STATE.ip);

    char flags_state[10] = {};
    PrintFlags(STATE.flags, flags_state);
    if (show_diff && OLD_STATE.flags.all ^ STATE.flags.all) {
        char flags_old[10] = {};
        PrintFlags(OLD_STATE.flags, flags_old);
        printf("\t\tFlags: %s -> %s\n", flags_old, flags_state);
    } else if (!asm_string){
        printf("\tFlags: %s\n", flags_state);
    }
}

static inline reg* GetRegister(operand op) {
     reg* result = 0;
    if (op.wide) {
        if (op.segment) {
            result = &STATE.segment_registers[op.reg];
        } else {
            result = &STATE.registers[op.reg];
        }
    } else if (op. reg > 3) {
        result = &STATE.registers[op.reg % 4];
    } else {
        result = &STATE.registers[op.reg];
    }

    return result;
}

static inline u8 Parity(u8 byte) {
    u8 result = 0;
    for (int i = 0; i < 8; i++) {
        result += byte & 0b1;
        byte >>= 1;
    }
    return !(result % 2);
}

// does the given operaton on the numbers given and returns modified flags
// TODO make this function work for both 16bit/8bit numbers
op_result OpSetFlags(sim_op dst, sim_op src, instr inst) {
    op_result result = {.use = true};
    i16 dst16 = dst.value.full;
    i16 src16 = src.value.full;

    u8 dst_sign = (dst.wide) ? dst16 >> 15 : dst16 >> 7;
    u8 src_sign = (src.wide) ? src16 >> 15 : src16 >> 7;

    b8 aflag = false;
    b8 cflag = false;
    b8 oflag = false;

    i16 value = 0;
    switch (inst.name) {
    case ADD: {
        value = dst16 + src16;

        aflag = (value & 0x0F) < (dst16 & 0x0F);

        if (dst.wide) {
            cflag = (u32)((u16)dst16 + (u16)src16) > 0xFFFF;
            oflag = (dst_sign == src_sign) && (value >> 15 != dst_sign);
        } else {
            cflag = (u16)((u8)dst16 + (u8)src16) > 0xFF;
            oflag = (dst_sign == src_sign) && (value >> 7 != dst_sign);
        }

        break;
    }
    case CMP:
    case SUB: {
        value = dst16 - src16;
        aflag = (value & 0xFF) > (dst16 & 0xFF);

        if (dst.wide) {
            cflag = (u32)((u16)dst16 - (u16)src16) > 0xFFFF;
            oflag = (dst_sign != src_sign) && (value >> 15 != dst_sign);

        } else {
            cflag = (u16)((u8)dst16 - (u8)src16) > 0xFF;
            oflag = (dst_sign != src_sign) && (value >> 7 != dst_sign);
        }

        if (inst.name == CMP) {
            result.use = false;
        }
        break;
    }
    }

    b8 sflag = (dst.wide) ? value >> 15 : value >> 7;
    b8 pflag = Parity(value);
    b8 zflag = value == 0;

    result.value = value;
    result.flags.c = cflag;
    result.flags.z = zflag;
    result.flags.s = sflag;
    result.flags.o = oflag;
    result.flags.p = pflag;
    result.flags.a = aflag;

    return result;
}

void DecodeRegInstruction(b1 byte, b2 byte2, buffer *code_buffer,
                          buffer *asm_buffer) {
    instr instr = All_Instrs[byte.full];
    char *instr_string = Instr_Names[instr.name];
    if (instr.name == ANY) {
        instr_string = Instr_Names[byte2.reg];
        instr.name = byte2.reg;
    }

    char *dst_string = (byte.w) ? Word_Registers[byte2.rm] : Byte_Registers[byte2.rm];
    char *src_string = (byte.w) ? Word_Registers[byte2.reg] : Byte_Registers[byte2.reg];

    char data_str[32] = {};
    reg *dst_register = &STATE.registers[byte2.rm];
    reg *src_register = &STATE.registers[byte2.reg];
    sim_op dst = { .value = *dst_register, .wide = true};
    sim_op src = { .value = *src_register, .wide = true};
    
    if (instr.type == I_IMM_REGMEM) {
        src.value.lo = PopBuffer(code_buffer);
        // Sign extension ops
        if (byte.w && instr.bytes_used == 5) {
            src.value.hi = PopBuffer(code_buffer);
        } else {
            src.value.full = (i16)src.value.lo;
        }

        sprintf(data_str, "%hd", src.value.full);
        src_string = data_str;
    }

    char asm_string[32] = {};
    // Segment mov
    if ((byte.full == 0x8C) || (byte.full == 0x8E)) {
        /* dst = (operand){.reg = byte2.rm, .wide = true}; */
        /* src = (operand){.reg = byte2.reg, .wide = true, .segment = true}; */
        dst_string = Word_Registers[byte2.rm];
        src_string = Segment_Reg_Names[byte2.reg];
        if (byte.d) {
            /* SWAP(src, dst); */
            SWAP(src_string, dst_string);
        }
    }

    
    sprintf(asm_string, "%s %s, %s", instr_string, dst_string, src_string);

    if (SIMULATE) {
        if (instr.name == MOV) {
            *dst_register = *src_register;
        } else {
            op_result result = OpSetFlags(dst, src, instr);
            if (result.use) {
              dst_register->full = result.value;
            }
            STATE.flags = result.flags;
        }

    } else {
        asm_buffer->index +=
            sprintf(asm_buffer->buffer + asm_buffer->index, "%s\n", asm_string);
    }

    PrintState(asm_string, true);
    return;
}

void DecodeMemoryInstruction(b1 byte1, b2 byte2, buffer *code_buffer, buffer *asm_buffer) {
    instr instr = All_Instrs[byte1.full];
    char *instr_string = Instr_Names[instr.name];
    if (instr.name == ANY) {
        instr_string = Instr_Names[byte2.reg];
        instr.name = byte2.reg;
    }

    char *dst_string = Rm_Mem_Table[byte2.rm];
    char *src_string = (byte1.w) ? Word_Registers[byte2.reg] : Byte_Registers[byte2.reg];
    b8 m_mode16bit = (byte2.mod == MOD_MEM && byte2.rm == 0b110) ? true : false;
    reg disp = {};
    
    if (byte2.mod == MOD_MEM_8) {
        disp.lo = PopBuffer(code_buffer);
    } else if ((m_mode16bit) || byte2.mod == MOD_MEM_16) {
        disp.lo = PopBuffer(code_buffer);
        disp.hi = PopBuffer(code_buffer);
    }

    char mem_addr[32] = {};
    if (byte2.mod == MOD_MEM) {
        sprintf(mem_addr, "[%s]", dst_string);
        if (m_mode16bit) {
            sprintf(mem_addr, "[%hd]", disp.full);
        }
    } else {
        sprintf(mem_addr, "[%s + %d]", dst_string, (byte2.mod == MOD_MEM_8) ? disp.lo : disp.full);
    }
    
    dst_string = mem_addr;

    // TODO might be able to simplify this and src_op/dst_op
    reg *dst_register = (reg *)&MEMORY.slot[GetRmValues(byte2.rm) + disp.full];
    reg *src_register = &STATE.registers[byte2.reg]; 
    sim_op dst_op = { .value = *dst_register, .wide = true};
    sim_op src_op = { .value = *src_register, .wide = true};
    
    char data_str[16] = {};
    if (instr.type == I_IMM_REGMEM) {
        src_op.value.lo = PopBuffer(code_buffer);

        // TODO Look for better way to deal with sign extension of 8 bit values rather that have bytes_used for all instructions
        if (byte1.w && instr.bytes_used == 5) {
            src_op.value.hi = PopBuffer(code_buffer);
            src_op.wide = true;
        }

        sprintf(data_str, "%s %hd", (byte1.w) ? "word" : "byte", src_op.value.full);
        src_string = data_str;
    } else if (byte1.d) {
        SWAP(dst_op, src_op);
        SWAP(dst_register, src_register);
        SWAP(dst_string, src_string);
    }

    char asm_string[32] = {};
    sprintf(asm_string, "%s %s, %s\n", instr_string, dst_string, src_string);

    // Operation does not need to modify flags
    if (SIMULATE) {
        if (instr.name == MOV) {
            *dst_register = src_op.value;
            // Operation modifies flags
        } else {
            op_result result = OpSetFlags(dst_op, src_op, instr);
            if (result.use) {
                dst_register->full = result.value;
            }
            STATE.flags = result.flags;
        }

        PrintState(asm_string, true);
    } else {
        asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s", asm_string);
    }
    return;
}

// NOTE(Peter) Look into simplifying the Acc functions or moving it to the rest of the RegImm_RegMem function
void Acc(b1 byte, buffer *code_buffer, buffer *asm_buffer) {
    instr instr = All_Instrs[byte.full];
    char *dst = (byte.w) ? "ax" : "al";
    u8 data_lo = PopBuffer(code_buffer);
    i16 data = (i8)data_lo;
    char *instr_name = Instr_Names[instr.name];
    // mov instructions always require an address lo/hi
    if (byte.w || instr.name == MOV) {
        u8 data_hi = PopBuffer(code_buffer);
        data = U8ToI16(data_hi, data_lo);
    }

    char command[16] = {};
    if (instr.name == MOV) {
        sprintf(command, "[%hd]", data);
    } else {
        sprintf(command, "%hd", data);
    }

    char *src = command;
    if (byte.d) {
        SWAP(src, dst);
    }

    if (SIMULATE) {

    } else {
        asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s %s, %s\n", instr_name, dst, src);
    }
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("No Filename passed to program. Exiting\n");
        return 0;
    }

    char *filename = argv[1];
    char *fileout = argv[2] ? argv[2] : "asm-out/asm_out.asm";
    FILE *fp = fopen(filename, "rb");
    if (!fp) {
        return 1;
    }

    u8 file_buffer[512] = {};
    u64 bytes_read = fread(file_buffer, 1, sizeof(file_buffer), fp);
    if (ferror(fp)) {
        fprintf(stderr, "Error opening BINARY IN file.\n");
        return 1;
    }

    fclose(fp);
    for (int i = 0; i < ARRAY_SIZE(Handled_Instrs); i++) {
        instr inst = Handled_Instrs[i];
        All_Instrs[inst.b1] = inst;
    }

    buffer code_buffer = {.buffer = &file_buffer, .size = ARRAY_SIZE(file_buffer)};
    char out_buffer[512];
    /* char out_buffer[1024 * 1024]; */
    buffer asm_buffer = {.buffer = &out_buffer, .size = ARRAY_SIZE(out_buffer) };
    asm_buffer.index = sprintf(asm_buffer.buffer, ";;From file: %s\nbits 16\n", filename);

    int i = 0;
    while (STATE.ip < bytes_read) {
        b1 byte = {.full = PopBuffer(&code_buffer)};
        char command[32] = {};
        instr instr = All_Instrs[byte.full];
        switch(instr.type) {
            case I_ACC: {
                Acc(byte, &code_buffer, &asm_buffer);
                break;
            }
            case I_REG_REGMEM:
            case I_IMM_REGMEM: {
                b2 byte2 = {.byte = PopBuffer(&code_buffer)};
                if (byte2.mod == MOD_REG) {
                    DecodeRegInstruction(byte, byte2, &code_buffer, &asm_buffer);
                } else {
                    DecodeMemoryInstruction(byte, byte2, &code_buffer, &asm_buffer);
                }
                break;
            }
            case I_LOOP:
            case I_JUMP: {
                u8 inst_type = byte.full & 0b00001111;
                char *instr_name = (instr.type == I_JUMP) ? Jump_Names[inst_type] : Loop_Names[inst_type];
                jump_type jmptype = inst_type;
                loop_type looptype = inst_type;
                i8 disp = PopBuffer(&code_buffer);
                char asm_string[32] = {};
                // NOTE(Peter) Remember reading that we needed to add two because of the way jumps are encoded but forgot the details
                // look into it and place reason here.
                sprintf(asm_string, "%s $+2%+hd", instr_name, disp);
                if (SIMULATE) {
                    
                } else {
                    asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "%s\n", asm_string);
                }

                if (!SIMULATE) {
                    break;
                }
                if (instr.type == I_JUMP) {
                    instr_name = Jump_Names[inst_type];
                    jmptype = inst_type;
                    switch (jmptype) {
                        case JNE: {
                            if (!STATE.flags.z) {
                                SetIP(disp);
                            }
                            break;
                        }
                        case JE: {
                            if (STATE.flags.z) {
                                SetIP(disp);
                            }
                            break;
                        }
                        case JP: {
                            if (STATE.flags.p) {
                                SetIP(disp);
                            }
                            break;
                        }
                        case JB: {
                            if (STATE.flags.c) {
                                SetIP(disp);
                            }
                            break;
                        }

                        /* default: { */
                        /*     printf("JMP not handled: 0x%hx", byte.full); */
                        /* } */
                    }
                } else {
                    instr_name = Loop_Names[inst_type];
                    looptype = inst_type;
                    switch(looptype) {
                        case LOOPNZ: {
                            STATE.cx.full -= 1;
                            if (STATE.cx.full != 0 && !STATE.flags.z) {
                                SetIP(disp);
                            }
                            break;
                        }
                        /* default: { */
                        /*     printf("LOOP not handled: 0x%hx", byte.full); */
                        /* } */
                    }
                }

                PrintState(asm_string, true);
                break;
            }
            case I_MOV: {
                // cannot have a variable name reg, since reg is also a type
                u8 wide = ((byte.full & 0x0F) >> 3);
                u8 reg_ = byte.full & 0b00000111;
                /* operand dst = {.wide = wide, .reg = reg_}; */
                /* operand src = {.wide = wide, .reg = reg_, .data.lo = PopBuffer(&code_buffer)}; */
                reg src_register = {.lo = PopBuffer(&code_buffer)};
                char *dst_name = Byte_Registers[reg_];
                if (wide) {
                    src_register.hi = PopBuffer(&code_buffer);
                    dst_name = Word_Registers[reg_];
                } 

                char asm_string[32] = {};
                sprintf(asm_string, "mov %s, %hd", dst_name, src_register.full);
                if (SIMULATE) {
                    STATE.registers[reg_] = src_register;
                } else {
                    asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "%s\n", asm_string);
                }
                PrintState(asm_string, true);
                break;
            }
        }

        OLD_STATE = STATE;
    }

    if (!SIMULATE) {
        printf("ASM File: Index: %d \n%s", asm_buffer.index, (char*)asm_buffer.buffer);
    }
    FILE *out = fopen(fileout, "w");
    if (!out) {
        fprintf(stderr, "Error opening ASM OUT file.\n");
        return 1;
    }

    fprintf(out, "%s", (char*)asm_buffer.buffer);
    fclose(out);
    if (SIMULATE) {
        PrintState(NULL, false);
    }

    return 0;
}
