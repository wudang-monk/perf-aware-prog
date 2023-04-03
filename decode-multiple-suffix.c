#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof(arr[0]))
#define true 1
#define false 0

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef uint8_t b8;

typedef enum {
MOD_MEM = 0b0,
MOD_MEM_8 = 0b1,
MOD_MEM_16 = 0b10,
MOD_REG = 0b11
}mod;

typedef enum {
I_ACC = 1,
I_IMM_REGMEM = 2,
I_REG_REGMEM = 3,
I_MOV = 4,
I_JUMP = 5,
I_LOOP = 6
}inst_type;

// (NOTE) these enums are currently only used for mod reg r/m functions that do not encode the instruction in the reg field
typedef enum {
ADD = 0,
OR,
ADC,
SBB,
AND,
SUB = 5,
XOR,
CMP = 7,
MOV = 8,
ANY = 9,
JMP = 10
}op_type;

typedef union {
    struct {
        u8 lo;
        u8 hi;
    };
    u16 full;
}reg;

typedef struct {
    reg before;
    reg after;
}reg_state;

typedef struct {
    reg data;
    union {
        struct {
            u8 reg : 3;
            u8 wide : 1;
            u8 immediate : 1;
            u8 unused : 3;
        };
        u8 byte;
    };
}operand;

typedef union {
    struct {
        u8 rm: 3;
        u8 reg: 3;
        mod mod: 2;
    };
    u8 byte;
}b2;

typedef struct {
    u8 b1;
    op_type name;
    inst_type type;
    u8 bytes_used;
}instr;

typedef union {
    struct {
        u8 w: 1;
        u8 d: 1;
        u8 opcode: 6;
    };
    u8 byte;
}b1;

typedef struct {
    void *buffer;
    u32 index;
    u32 size;
}buffer;

u8 CARRY_FLAG = 0;
char *byte_registers[] = {"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"};
char *word_registers[] = {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di"};
char *rm_mem_table[] = {"bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx"};
char *Instr_Names[] = {"add", "or", "adc", "sbb", "and", "sub", "xor", "cmp", "mov"};
char *Jump_Names[] = {"jo", "jno", "jb", "jnb", "je", "jne", "jbe", "jnbe", "js", "jns", "jp", "jnp", "jl", "jnl", "jle", "jnle"};
char *Loop_Names[] = {"loopnz", "loopz", "loop", "jcxz"};
reg Registers[8] = {};

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
    u8 result = *(u8*)(buff->buffer + buff->index);
    ++buff->index;
    return result;
}

void PrintRegisters() {
    printf("Final Registers\n");
    for (int i = 0; i < ARRAY_SIZE(Registers); i++) {
        printf("\t%s: %x\n", word_registers[i], Registers[i].full);
    }
}

static inline u16 GetRegisterState(u8 reg_slot, b8 wide) {
    u16 result = 0;
    if (wide) {
        result = Registers[reg_slot].full;
    } else if (reg_slot > 3) {
        //
        result = Registers[reg_slot % 4].hi;
    } else {
        result = Registers[reg_slot].lo;
    }

    return result;
}

void Command(operand dst, operand src, instr inst, char *asm_string) {
    u16 dst_data = 0;
    u16 src_data = 0;

    if (dst.immediate) {
        if (dst.wide) {
            src_data = dst.data.full;
        } else {
            src_data = dst.data.lo;
        }
    } else {
        dst_data = GetRegisterState(dst.reg, dst.wide);
        src_data = GetRegisterState(src.reg, src.wide);
    }

    char *instr_name = Instr_Names[inst.name];
    switch(inst.name) {
        case ADD: {
            dst_data += src.data.full;
            break;
        }
        case ADC: {
            dst_data += src_data + CARRY_FLAG;
            break;
        }
        case SBB: {
            dst_data -= src_data - CARRY_FLAG;
            break;
        }
        case OR: {
            dst_data |= src_data;
            break;
        }
        case AND: {
            dst_data &= src_data;
            break;
        }
        case SUB: {
            dst_data -= src_data;
            break;
        }
        case XOR: {
            dst_data ^= src_data;
            break;
        }
        case CMP: {
            dst_data = (dst_data == src_data);
            break;
        }
        case MOV: {
            dst_data = src_data;
            break;
        }
        default: {
            printf("COMMAND: %d, NOT Handled\n", inst.name);
        }
    }

    u8 dst_reg = (!dst.wide && dst.reg > 3) ? dst.reg % 4 : dst.reg;
    u16 before = Registers[dst_reg].full;
    if (dst.wide) {
        Registers[dst.reg].full = dst_data;
    } else if (dst.reg > 3) {
        Registers[dst.reg % 4].hi = dst_data;
    } else {
        Registers[dst.reg].lo = dst_data;
    }

    printf("%s ; ", asm_string);
    printf("%s: %x -> %x\n", instr_name, before, Registers[dst_reg].full);
}

void RegIMM_RegMem(b1 byte, buffer *code_buffer, buffer *asm_buffer, inst_type type) {
    b2 byte2 = {.byte = PopBuffer(code_buffer)};
    instr instr = All_Instrs[byte.byte];
    char *instr_name = (instr.name == ANY) ? Instr_Names[byte2.reg] : Instr_Names[instr.name];
    char *rm = rm_mem_table[byte2.rm];
    char *src_name = (byte.w) ? word_registers[byte2.reg] : byte_registers[byte2.reg];
    reg data = {};
    reg disp = {};

    // Register to register
    if (byte2.mod == MOD_REG) {
        char data_str[32] = {};
        operand dst = {};
        operand src = {};
        rm = (byte.w) ? word_registers[byte2.rm] : byte_registers[byte2.rm];
        dst.reg = byte2.rm;
        src.reg = byte2.reg;
        dst.wide = byte.w;
        src.wide = byte.w;

        if (type == I_IMM_REGMEM) {
            dst.immediate = true;
            u8 data_lo = PopBuffer(code_buffer);
            data.lo = (i8)data_lo;
            if (byte.w && instr.bytes_used == 5) {
                u8 data_hi = PopBuffer(code_buffer);
                data.full = U8ToI16(data_hi, data_lo);
            }

            dst.data.full = data.full;
            sprintf(data_str, "%hd", data.full);
            src_name = data_str;
        }

        char asm_string[32] = {};
        sprintf(asm_string, "%s %s, %s", instr_name, rm, src_name);
        Command(dst, src, instr, asm_string);
        asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s\n", asm_string);
        return;
    }

    /* i16 disp = 0; */
    u8 mem_mode_16bit_disp = (byte2.mod == MOD_MEM && byte2.rm == 0b110) ? 1 : 0;

    if (byte2.mod == MOD_MEM_8) {
        i8 disp_lo = PopBuffer(code_buffer);
        disp.full = (i16)disp_lo;
    } else if ((mem_mode_16bit_disp) || byte2.mod == MOD_MEM_16) {
        u8 disp_lo = PopBuffer(code_buffer);
        u8 disp_hi = PopBuffer(code_buffer);
        disp.full = U8ToI16(disp_hi, disp_lo);
    }

    // Memory displacement
    char mem_addr[32] = {};
    sprintf(mem_addr, "[%s + %hd]", rm, disp.full);
    if (byte2.mod == MOD_MEM) {
        sprintf(mem_addr, "[%s]", rm);
        if (mem_mode_16bit_disp) {
            sprintf(mem_addr, "[%hd]", disp.full);
        }
    }

    char *src = src_name;
    char *dst = mem_addr;

    // Immediate to Register/Mem
    if (type == I_IMM_REGMEM) {
        u8 data_lo = PopBuffer(code_buffer);
        data.lo = (i8)data_lo;

        if (byte.w && instr.bytes_used == 5) {
            u8 data_hi = PopBuffer(code_buffer);
            data.full = U8ToI16(data_hi, data_lo);
        }

        char data_str[16] = {};
        sprintf(data_str, "%s %hd", (byte.w) ? "word" : "byte", data.full);
        src = data_str;
    } else if (byte.d) {
        char *tmp = src;
        src = dst;
        dst = tmp;
    }

    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s %s, %s\n", instr_name, dst, src);

    return;
}

void Acc(b1 byte, buffer *code_buffer, buffer *asm_buffer) {
    instr instr = All_Instrs[byte.byte];
    char *dst = (byte.w) ? "ax" : "al";
    u8 data_lo = PopBuffer(code_buffer);
    i16 data = (i8)data_lo;
    char *instr_name = Instr_Names[instr.name];
    char command[16] = {};
    // mov instructions always require an address lo/hi
    if (byte.w || instr.name == MOV) {
        u8 data_hi = PopBuffer(code_buffer);
        data = U8ToI16(data_hi, data_lo);
    }

    if (instr.name == MOV) {
        sprintf(command, "[%hd]", data);
    } else {
        sprintf(command, "%hd", data);
    }

    char *src = command;
    if (byte.d) {
        char *tmp = src;
        src = dst;
        dst = tmp;
    }
    
    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s %s, %s\n", instr_name, dst, src);

    printf("ACC: [%d, hex: %x]\n", byte.byte, byte.byte);
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
    char out_buffer[4096];
    buffer asm_buffer = {.buffer = &out_buffer, .size = ARRAY_SIZE(out_buffer) };
    asm_buffer.index = sprintf(asm_buffer.buffer, ";;From file: %s\nbits 16\n", filename);

    int i = 0;
    while (code_buffer.index < bytes_read) {
        b1 byte = {.byte = PopBuffer(&code_buffer)};
        char command[32] = {};
        instr instr = All_Instrs[byte.byte];
        switch(instr.type) {
            case I_ACC: {
                Acc(byte, &code_buffer, &asm_buffer);
                break;
            }
            case I_REG_REGMEM:
            case I_IMM_REGMEM: {
                RegIMM_RegMem(byte, &code_buffer, &asm_buffer, instr.type);
                break;
            }
            case I_LOOP:
            case I_JUMP: {
                u8 inst_type = byte.byte & 0b00001111;
                char *instr_name = (instr.type == I_JUMP) ? Jump_Names[inst_type] : Loop_Names[inst_type];
                i8 inc_8 = PopBuffer(&code_buffer);
                asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "%s $+2%+hd\n", instr_name, inc_8);
                break;
            }
            case I_MOV: {
                /* u8 wide = (byte.byte >> 3) & 0b00001; */
                operand op = {.byte = byte.byte};
                op.immediate = true;
                /* u8 reg = byte.byte & 0b00000111; */
                char *dst = (op.wide) ? word_registers[op.reg] : byte_registers[op.reg];
                u8 data_lo = PopBuffer(&code_buffer);
                i16 data = (i8)data_lo;

                if (op.wide) {
                    u8 data_hi = PopBuffer(&code_buffer);
                    data = U8ToI16(data_hi, data_lo);
                }

                op.data.full = data;
                char asm_string[32] = {};
                sprintf(asm_string, "mov %s, %hx", dst, data);
                Command(op, (operand){}, instr, asm_string);
                asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "%s\n", asm_string);
                /* printf("MOV: %s, %hd", dst, data); */
                break;
            }
        }
    }

    /* printf("ASM File: Index: %d \n%s", asm_buffer.index, (char*)asm_buffer.buffer); */
    FILE *out = fopen(fileout, "w");
    if (!out) {
        fprintf(stderr, "Error opening ASM OUT file.\n");
        return 1;
    }

    fprintf(out, "%s", (char*)asm_buffer.buffer);
    fclose(out);
    PrintRegisters();

    return 0;
}
