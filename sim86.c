#include <stdio.h>
#include <stdlib.h>
#include "sim86.h"

FILE *LOG;
memory MEMORY = {};
b8 SIMULATE = true;
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

instr All_Instrs[256] = {};
instr Handled_Instrs[] = {
{0x0, ADD, I_REG_REGMEM},
{0x1, ADD, I_REG_REGMEM},
{0x2, ADD, I_REG_REGMEM},
{0x3, ADD, I_REG_REGMEM},
{0x4, ADD, I_ACC},
{0x5, ADD, I_ACC},
{0x28, SUB, I_REG_REGMEM},
{0x29, SUB, I_REG_REGMEM},
{0x2A, SUB, I_REG_REGMEM},
{0x2B, SUB, I_REG_REGMEM},
{0x2C, SUB,I_ACC},
{0x2D, SUB,I_ACC},
{0x38, CMP, I_REG_REGMEM},
{0x39, CMP, I_REG_REGMEM},
{0x3A, CMP, I_REG_REGMEM},
{0x3B, CMP, I_REG_REGMEM},
{0x3C, CMP, I_ACC},
{0x3D, CMP, I_ACC},

{0x50, PUSH, I_PUSH},
{0x51, PUSH, I_PUSH},
{0x52, PUSH, I_PUSH},
{0x53, PUSH, I_PUSH},
{0x54, PUSH, I_PUSH},
{0x55, PUSH, I_PUSH},
{0x56, PUSH, I_PUSH},
{0x57, PUSH, I_PUSH},

{0x58, POP, I_POP},
{0x59, POP, I_POP},
{0x5A, POP, I_POP},
{0x5B, POP, I_POP},
{0x5C, POP, I_POP},
{0x5D, POP, I_POP},
{0x5E, POP, I_POP},
{0x5F, POP, I_POP},

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
{0x80, VAR, I_IMM_REGMEM},
{0x81, VAR, I_IMM_REGMEM},
{0x82, VAR, I_IMM_REGMEM},
{0x83, VAR, I_IMM_REGMEM},
{0x88, MOV, I_REG_REGMEM},
{0x89, MOV, I_REG_REGMEM},
{0x8A, MOV, I_REG_REGMEM},
{0x8B, MOV, I_REG_REGMEM},
{0x8C, MOV, I_REG_REGMEM},
{0x8E, MOV, I_REG_REGMEM},
{0xA0, MOV, I_ACC},
{0xA1, MOV, I_ACC},
{0xA2, MOV, I_ACC},
{0xA3, MOV, I_ACC},
{0xB0, MOV, I_MOV},
{0xB1, MOV, I_MOV},
{0xB2, MOV, I_MOV},
{0xB3, MOV, I_MOV},
{0xB4, MOV, I_MOV},
{0xB5, MOV, I_MOV},
{0xB6, MOV, I_MOV},
{0xB7, MOV, I_MOV},
{0xB8, MOV, I_MOV},
{0xB9, MOV, I_MOV},
{0xBA, MOV, I_MOV},
{0xBB, MOV, I_MOV},
{0xBC, MOV, I_MOV},
{0xBD, MOV, I_MOV},
{0xBE, MOV, I_MOV},
{0xBF, MOV, I_MOV},
{0xC6, MOV, I_IMM_REGMEM},
{0xC7, MOV, I_IMM_REGMEM},
{0xE0, LOOP, I_LOOP},
{0xE1, LOOP, I_LOOP},
{0xE2, LOOP, I_LOOP},
{0xE3, LOOP, I_LOOP},
{0xFF, PUSH, I_IMM_REGMEM}
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

static inline void WriteAsm(char* asm_string, buffer *asm_buffer) {
    if (!SIMULATE) {
        asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s\n", asm_string);
    }
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
    if (SIMULATE) {
        operand op = {.byte = instr.byte1};
        switch (instr.name) {
            case MOV:{
                b8 wide = instr.byte1 & 1;
                dst.src_reg->full = (wide) ? src.data.full: src.data.lo;
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
            case ADD: {
                op_result result = OpSetFlags(dst.data, src.data, instr, dst.wide);
                dst.src_reg->full = result.value;
                STATE.flags = result.flags;
                break;
            }
            case SUB: {
                op_result result = OpSetFlags(dst.data, src.data, instr, dst.wide);
                dst.src_reg->full = result.value;
                STATE.flags = result.flags;
                break;
            }
            case CMP: {
                op_result result = OpSetFlags(dst.data, src.data, instr, dst.wide);
                STATE.flags = result.flags;
                break;
            }
            case JMP: {
                jmp_type jmp = instr.byte1 & 0x0F;
                switch (jmp) {
                    case J_JNE: {
                        if (!STATE.flags.z) {
                            SetIP(src.data.lo);
                        }
                        break;
                    }
                    case J_JE: {
                        if (STATE.flags.z) {
                            SetIP(src.data.lo);
                        }
                        break;
                    }
                    case J_JP: {
                        if (STATE.flags.p) {
                            SetIP(src.data.lo);
                        }
                        break;
                    }
                    case J_JB: {
                        if (STATE.flags.c) {
                            SetIP(src.data.lo);
                        }
                        break;
                    }
                        /* default: { */
                        /*     printf("JMP not handled: 0x%hx", byte.full); */
                        /* } */
                }
                break;
            }
            case LOOP: {
                loop_type type = instr.byte1 & 0x0F;
                switch(type) {
                    case L_LOOP: {
                        STATE.cx.full -= 1;
                        if (STATE.cx.full != 0) {
                            SetIP(src.data.lo);
                        }
                        break;
                    }
                    case L_LOOPNZ: {
                        STATE.cx.full -= 1;
                        if (STATE.cx.full != 0 && !STATE.flags.z) {
                            SetIP(src.data.lo);
                        }
                        break;
                    }
                        /* default: { */
                        /*     printf("LOOP not handled: 0x%hx", byte.full); */
                        /* } */
                }

                break;
            }
        }
        PrintState(asm_string, true);
    }
}

void DecodeOneByte(b1 byte1, buffer* code_buffer, buffer* asm_buffer) {
    instr inst = All_Instrs[byte1.full];
    operand dst_op = {.wide = byte1.w};
    operand src_op = {.wide = byte1.w};
    char asm_string[32] = {};
    switch(inst.type) {
        case I_ACC: {
            char *dst_string = (byte1.w) ? "ax" : "al";
            char *instr_name = Instr_Names[inst.name];
            src_op.data.lo = PopBuffer(code_buffer);
            dst_op.src_reg = GetRegister(dst_op);
            dst_op.data = *dst_op.src_reg;

            if (byte1.w || inst.name == MOV) {
                src_op.data.hi = PopBuffer(code_buffer);
            }

            char command[16] = {};
            sprintf(command, (inst.name == MOV) ? "[%hd]": "%hd", src_op.data.full);

            char *src_string = command;
            if (byte1.d) {
                SWAP(src_op, dst_op);
                SWAP(src_string, dst_string);
            }
            sprintf(asm_string, "%s %s, %s", instr_name, dst_string, src_string);
            break;
        }
        case I_MOV: {
            u8 wide = ((byte1.full & 0x0F) >> 3);
            u8 mov_reg = byte1.full & 0b00000111;
            char *dst_name = (wide) ? Word_Registers[mov_reg] : Byte_Registers[mov_reg];
            dst_op.reg = mov_reg;
            dst_op.wide = wide;
            dst_op.src_reg = GetRegister(dst_op);
            dst_op.data = *dst_op.src_reg;

            src_op.data.lo = PopBuffer(code_buffer);

            if (wide) {
                src_op.data.hi = PopBuffer(code_buffer);
            }

            sprintf(asm_string, "mov %s, %hd", dst_name, src_op.data.full);
            break;
        }
        case I_LOOP:
        case I_JUMP: {
            u8 inst_type = byte1.full & 0b00001111;
            char *instr_name = (inst.type == I_JUMP) ? Jump_Names[inst_type] : Loop_Names[inst_type];
            src_op.data.lo = PopBuffer(code_buffer);
            sprintf(asm_string, "%s $+2%+hd", instr_name, src_op.data.lo);
            break;
        }
    }
    Simulate(inst, dst_op, src_op, asm_string);
    WriteAsm(asm_string, asm_buffer);
}

void DecodeTwoByte(b1 byte1, buffer* code_buffer, buffer* asm_buffer) {
    b2 byte2 = {.byte = PopBuffer(code_buffer)};
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

    instr inst = All_Instrs[byte1.full];
    inst.name = (inst.name == VAR) ? byte2.reg : inst.name;
    char *instr_string = Instr_Names[inst.name];
    char *dst_string = (byte2.mod == MOD_REG) ? ((byte1.w) ? Word_Registers[byte2.rm] : Byte_Registers[byte2.rm]) : Rm_Mem_Table[byte2.rm];

    operand dst_op = {.reg = byte2.rm, .wide = byte1.w, .segment = false};
    dst_op.src_reg = GetRegister(dst_op);
    dst_op.data = *dst_op.src_reg;

    operand src_op = {.reg = byte2.reg, .wide = byte1.w, .segment = false};
    src_op.src_reg = GetRegister(src_op);
    src_op.data = *src_op.src_reg;

    char *src_string = (byte1.w) ? Word_Registers[byte2.reg] : Byte_Registers[byte2.reg];

    char mem_addr[32] = {};
    if (byte2.mod != MOD_REG) {
        if (byte2.mod == MOD_MEM) {
            sprintf(mem_addr, "[%s]", dst_string);
            if (m_mode16bit) {
                sprintf(mem_addr, "[%hd]", disp.full);
                dst_op.src_reg = (reg*)&MEMORY + disp.full;
            }
        } else {
            sprintf(mem_addr, "[%s + %d]", dst_string, (byte2.mod == MOD_MEM_8) ? disp.lo : disp.full);
            dst_op.src_reg = (reg*)((u8*)&MEMORY + GetRmValues(byte2.rm) + disp.full);
        }
        dst_string = mem_addr;
        dst_op.data = *dst_op.src_reg;
    }

    char data_string[32] = {};
    if (has_data) {
        char *size = (byte1.w) ? "word" : "byte";
        sprintf(data_string, "%s %hd", size, data.full);
        src_string = data_string;
        src_op.data = data;
    }

    if (!has_data && byte2.mod != MOD_REG && byte1.d) {
        SWAP(src_string, dst_string);
        SWAP(src_op, dst_op);
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
    Simulate(inst, dst_op, src_op, asm_string);
    WriteAsm(asm_string, asm_buffer);
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
        b1 byte1 = {.full = PopBuffer(&code_buffer)};
        char command[32] = {};
        instr inst = All_Instrs[byte1.full];
        if ((inst.type == I_REG_REGMEM) || (inst.type == I_IMM_REGMEM)) {
            DecodeTwoByte(byte1, &code_buffer, &asm_buffer);
        } else {
            DecodeOneByte(byte1, &code_buffer, &asm_buffer);
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
