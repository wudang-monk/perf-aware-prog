#include <stdio.h>
#include <stdlib.h>
#include "sim86.h"

u8 BYTES_USED = 0;
FILE *LOG;
memory MEMORY = {};
b8 SIMULATE = false;
state STATE = {};
state OLD_STATE = {};
char Flag_Names[] = {'O', 'D', 'I', 'T', 'S', 'Z', '\000', 'A', '\000', 'P', '\000', 'C'};
char *Byte_Registers[] = {"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"};
char *Word_Registers[] = {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di"};
char *Segment_Reg_Names[] = {"es", "cs", "ss", "ds"};
char *Rm_Mem_Table[] = {"bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx"};
char *Instr_Names[] = {"add", "or", "adc", "sbb", "and", "sub", "xor", "cmp", "mov", "jmp", "push", "pop"};
char *Jump_Names[] = {"jo", "jno", "jb", "jnb", "je", "jne", "jbe", "jnbe", "js", "jns", "jp", "jnp", "jl", "jnl", "jle", "jnle"};
char *Loop_Names[] = {"loopnz", "loopz", "loop", "jcxz"};
char *Mod_Names[] = {"MEM", "MEM 8", "MEM 16", "REG"};

instr All_Instrs[256] = {};
instr Handled_Instrs[] = {
{0x0, ADD, I_REG_REGMEM, true},
{0x1, ADD, I_REG_REGMEM, true},
{0x2, ADD, I_REG_REGMEM, true},
{0x3, ADD, I_REG_REGMEM, true},
{0x4, ADD, I_ACC, true},
{0x5, ADD, I_ACC, true},
{0x28, SUB, I_REG_REGMEM, true},
{0x29, SUB, I_REG_REGMEM, true},
{0x2A, SUB, I_REG_REGMEM, true},
{0x2B, SUB, I_REG_REGMEM, true},
{0x2C, SUB,I_ACC, true},
{0x2D, SUB,I_ACC, true},
{0x38, CMP, I_REG_REGMEM, false},
{0x39, CMP, I_REG_REGMEM, false},
{0x3A, CMP, I_REG_REGMEM, false},
{0x3B, CMP, I_REG_REGMEM, false},
{0x3C, CMP, I_ACC, false},
{0x3D, CMP, I_ACC, false},

{0x50, PUSH, I_PUSH, false},
{0x51, PUSH, I_PUSH, false},
{0x52, PUSH, I_PUSH, false},
{0x53, PUSH, I_PUSH, false},
{0x54, PUSH, I_PUSH, false},
{0x55, PUSH, I_PUSH, false},
{0x56, PUSH, I_PUSH, false},
{0x57, PUSH, I_PUSH, false},

{0x58, POP, I_POP, false},
{0x59, POP, I_POP, false},
{0x5A, POP, I_POP, false},
{0x5B, POP, I_POP, false},
{0x5C, POP, I_POP, false},
{0x5D, POP, I_POP, false},
{0x5E, POP, I_POP, false},
{0x5F, POP, I_POP, false},

{0x70, JMP, I_JUMP, false},
{0x71, JMP, I_JUMP, false},
{0x72, JMP, I_JUMP, false},
{0x73, JMP, I_JUMP, false},
{0x74, JMP, I_JUMP, false},
{0x75, JMP, I_JUMP, false},
{0x76, JMP, I_JUMP, false},
{0x77, JMP, I_JUMP, false},
{0x78, JMP, I_JUMP, false},
{0x79, JMP, I_JUMP, false},
{0x7A, JMP, I_JUMP, false},
{0x7B, JMP, I_JUMP, false},
{0x7C, JMP, I_JUMP, false},
{0x7D, JMP, I_JUMP, false},
{0x7E, JMP, I_JUMP, false},
{0x7F, JMP, I_JUMP, false},
{0x80, ANY, I_IMM_REGMEM, true},
{0x81, ANY, I_IMM_REGMEM, true},
{0x82, ANY, I_IMM_REGMEM, true},
{0x83, ANY, I_IMM_REGMEM, true},
{0x88, MOV, I_REG_REGMEM, true},
{0x89, MOV, I_REG_REGMEM, true},
{0x8A, MOV, I_REG_REGMEM, true},
{0x8B, MOV, I_REG_REGMEM, true},
{0x8C, MOV, I_REG_REGMEM, true},
{0x8E, MOV, I_REG_REGMEM, true},
{0xA0, MOV, I_ACC, false},
{0xA1, MOV, I_ACC, false},
{0xA2, MOV, I_ACC, false},
{0xA3, MOV, I_ACC, false},
{0xB0, MOV, I_MOV, false},
{0xB1, MOV, I_MOV, false},
{0xB2, MOV, I_MOV, false},
{0xB3, MOV, I_MOV, false},
{0xB4, MOV, I_MOV, false},
{0xB5, MOV, I_MOV, false},
{0xB6, MOV, I_MOV, false},
{0xB7, MOV, I_MOV, false},
{0xB8, MOV, I_MOV, false},
{0xB9, MOV, I_MOV, false},
{0xBA, MOV, I_MOV, false},
{0xBB, MOV, I_MOV, false},
{0xBC, MOV, I_MOV, false},
{0xBD, MOV, I_MOV, false},
{0xBE, MOV, I_MOV, false},
{0xBF, MOV, I_MOV, false},
{0xC6, MOV, I_IMM_REGMEM, false},
{0xC7, MOV, I_IMM_REGMEM, false},
{0xE0, ANY, I_LOOP, true},
{0xE1, ANY, I_LOOP, true},
{0xE2, ANY, I_LOOP, true},
{0xE3, ANY, I_LOOP, true},
{0xFF, PUSH, I_IMM_REGMEM, false}

};

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

void Push(reg op, b8 wide) {
    i16 index = STATE.sp.full;
    index -= 1;
    MEMORY.slot[index] = op.lo;
    if (wide) {
        index -= 2;
        MEMORY.slot[index] = op.hi;
    }
    STATE.sp.full = index;
}

void Pop(reg op, b8 wide) {
    i16 index = STATE.sp.full;
    op.lo = MEMORY.slot[index];
    index += 1;
    if (wide) {
        op.hi = MEMORY.slot[index];
        index += 1;
    }

    STATE.sp.full = index;
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
    } else if (op.reg > 3) {
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
op_result OpSetFlags(reg dst, reg src, instr inst, b8 wide) {
    op_result result = {};
    i16 dst16 = dst.full;
    i16 src16 = src.full;

    u8 dst_sign = (wide) ? dst16 >> 15 : dst16 >> 7;
    u8 src_sign = (wide) ? src16 >> 15 : src16 >> 7;

    b8 aflag = false;
    b8 cflag = false;
    b8 oflag = false;

    i16 value = 0;
    switch (inst.name) {
    case ADD: {
        value = dst16 + src16;

        aflag = (value & 0x0F) < (dst16 & 0x0F);

        if (wide) {
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

        if (wide) {
            cflag = (u32)((u16)dst16 - (u16)src16) > 0xFFFF;
            oflag = (dst_sign != src_sign) && (value >> 15 != dst_sign);

        } else {
            cflag = (u16)((u8)dst16 - (u8)src16) > 0xFF;
            oflag = (dst_sign != src_sign) && (value >> 7 != dst_sign);
        }

        break;
    }
    }

    b8 sflag = (wide) ? value >> 15 : value >> 7;
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

void Simulate(instr instr, operand dst, operand src, char *asm_string) {

    /* operand dst_op = {.reg = byte2.rm, .wide = byte1.w}; */
    /* operand src_op = {.reg = byte2.reg, .wide = byte1.w}; */
    /* i16 mem_slot = GetRmValues(byte2.rm) + disp.full; */
    /* reg *dst_register = (reg *)&MEMORY.slot[mem_slot]; */
    /* reg *src_register = &STATE.registers[byte2.reg]; */
    /* dst_op.data = *dst_register; */
    /* src_op.data = *GetRegister(src_op); */


    if (SIMULATE) {
        if (!instr.flagmod) {
            operand op = {.byte = instr.byte1};
            switch (instr.name) {
                case MOV:{
                    b8 wide = instr.byte1 & 1;
                    dst.data.full = (wide) ? src.data.full: src.data.lo;
                    break;
                }
                case PUSH:{
                    Push(dst.data, dst.wide);
                    break;
                }
                case POP:{
                    Pop(dst.data, dst.wide);
                    break;
                }
            }
        } else {
            op_result result = OpSetFlags(dst.data, src.data, instr, dst.wide);
            dst.data.full = result.value;
            STATE.flags = result.flags;
        }

        PrintState(asm_string, true);
    }
}

// NOTE(Peter) Look into simplifying the Acc functions or moving it to the rest of the RegImm_RegMem function
void Acc(b1 byte, buffer *code_buffer, buffer *asm_buffer) {
    instr instr = All_Instrs[byte.full];
    char *dst_string = (byte.w) ? "ax" : "al";
    reg data = { .lo = PopBuffer(code_buffer)};
    char *instr_name = Instr_Names[instr.name];
    // mov instructions always require an address lo/hi
    if (byte.w || instr.name == MOV) {
        data.hi = PopBuffer(code_buffer);
    }

    char command[16] = {};
    sprintf(command, (instr.name == MOV) ? "[%hd]": "%hd", data.full);

    char *src_string = command;
    if (byte.d) {
        SWAP(src_string, dst_string);
    }

    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s %s, %s\n", instr_name, dst_string, src_string);
}

void WriteAsm(b1 byte1, b2 byte2, reg disp, reg data, buffer *asm_buffer, b8 isflipped, b8 has_data) {
    instr inst = All_Instrs[byte1.full];
    inst.name = (inst.name == ANY) ? byte2.reg : inst.name;
    char *instr_string = Instr_Names[inst.name];
    char *dst_string = (byte2.mod == MOD_REG) ? ((byte1.w) ? Word_Registers[byte2.rm] : Byte_Registers[byte2.rm]) : Rm_Mem_Table[byte2.rm];
    char *src_string = (byte1.w) ? Word_Registers[byte2.reg] : Byte_Registers[byte2.reg];
    b8 m_mode16bit = (byte2.mod == MOD_MEM && byte2.rm == 0b110) ? true : false;

    char mem_addr[32] = {};
    if (byte2.mod != MOD_REG) {
        if (byte2.mod == MOD_MEM) {
            sprintf(mem_addr, "[%s]", dst_string);
            if (m_mode16bit) {
                sprintf(mem_addr, "[%hd]", disp.full);
            }
        } else {
            sprintf(mem_addr, "[%s + %d]", dst_string, (byte2.mod == MOD_MEM_8) ? disp.lo : disp.full);
        }
        dst_string = mem_addr;
    }

    char data_string[32] = {};
    if (has_data) {
        sprintf(data_string, "%s %hd", (byte1.w) ? "word" : "byte", data.full);
        src_string = data_string;
    }

    if (isflipped) {
        SWAP(src_string, dst_string);
    }

    if ((byte1.full == 0x8C) || (byte1.full == 0x8E)) {
        dst_string = Word_Registers[byte2.rm];
        src_string = Segment_Reg_Names[byte2.reg];
        if (byte1.d) {
            SWAP(src_string, dst_string);
        }
    }

    char asm_string[32] = {};
    sprintf(asm_string, "%s %s, %s", instr_string, dst_string, src_string);
    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s\n", asm_string);
}


void DecodeInstruction(b1 byte1, b2 byte2, buffer* code_buffer, buffer* asm_buffer) {
    b8 m_mode16bit = (byte2.mod == MOD_MEM && byte2.rm == 0b110) ? true : false;

    reg disp = {};
    if (byte2.mod == MOD_MEM_8) {
        disp.lo = PopBuffer(code_buffer);
    } else if ((m_mode16bit) || byte2.mod == MOD_MEM_16) {
        disp.lo = PopBuffer(code_buffer);
        disp.hi = PopBuffer(code_buffer);
    }

    reg data = {};
    b8 has_data = false;
    if (All_Instrs[byte1.full].type == I_IMM_REGMEM) {
        has_data = true;
        data.lo = PopBuffer(code_buffer);
        if ((byte1.full == 0x81) || (byte1.full == 0xC7)) {
            data.hi = PopBuffer(code_buffer);
        } else {
            data.full = (i16)data.lo;
        }
    }

    b8 isflipped = false;
    if (!has_data && byte2.mod != MOD_REG && byte1.d) {
        isflipped = true;
        /* SWAP(src_string, dst_string); */
    }

    WriteAsm(byte1, byte2, disp, data, asm_buffer, isflipped, has_data);
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
        All_Instrs[inst.byte1] = inst;
    }

    buffer code_buffer = {.buffer = &file_buffer, .size = ARRAY_SIZE(file_buffer)};
    char out_buffer[2048];
    buffer asm_buffer = {.buffer = &out_buffer, .size = ARRAY_SIZE(out_buffer) };
    asm_buffer.index = sprintf(asm_buffer.buffer, ";;From file: %s\nbits 16\n", filename);

    // Setup stack
    STATE.sp.full = (i16)60000;

    LOG = fopen("log.txt", "a");
    if (!LOG) {
        fprintf(stderr, "Error opening LOG file.\n");
    }

    fprintf(LOG, "FILE: %s\n", filename);

    int i = 0;
    while (STATE.ip < bytes_read) {
        BYTES_USED = 0;
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
                DecodeInstruction(byte, byte2, &code_buffer, &asm_buffer);
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
                        case LOOP: {
                            STATE.cx.full -= 1;
                            if (STATE.cx.full != 0) {
                                SetIP(disp);
                            }
                            break;
                        }
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
        /* char *file = "sim86_mem_out.data"; */
        /* FILE *mem_out = fopen(file, "wb"); */
        /* if (!mem_out) { */
        /*     fprintf(stderr, "Error opening ASM OUT file.\n"); */
        /*     return 1; */
        /* } */

        /* fwrite(&MEMORY.slot[0], sizeof(u8), ARRAY_SIZE(MEMORY.slot), mem_out); */
        /* fclose(mem_out); */
    }



    return 0;
}
