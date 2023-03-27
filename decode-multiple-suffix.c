#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof(arr[0]))
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;

typedef enum {
MOD_MEM = 0b0,
MOD_MEM_8 = 0b1,
MOD_MEM_16 = 0b10,
MOD_REG = 0b11
}mod;

typedef enum {
I_ACC = 1,
I_BYTE2 = 2,
I_M_R_RM = 3,
I_MOV = 4,
I_JUMP = 5,
I_LOOP = 6
}inst_type;

// (NOTE) these enums are currently only used for mod reg r/m functions that do not encode the instruction in the reg field
typedef enum {
ADD = 0,
/* OR, */
/* ADC, */
/* SBB, */
/* AND, */
SUB = 5,
/* XOR, */
CMP = 7,
MOV = 8,
ANY = 9,
JMP = 10
}op_type;

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
}op_and_type;

typedef union {
    struct {
        u8 w: 1;
        u8 d: 1;
        u8 opcode: 6;
    };
    u8 byte;
}b1;

/* typedef union { */
/*     struct { */
/*         u16 w : 1; */
/*         u16 d : 1; */
/*         u16 opcode: 6; */
/*         u16 rm: 3; */
/*         u16 reg: 3; */
/*         u16 mod: 2; */
/*     }; */
/*     struct { */
/*         u8 b1; */
/*         u8 byte2; */
/*     }; */
/*     u8 bytes[2]; */
/* }reg_reg; */

/* typedef union { */
/*     struct { */
/*         u8 reg : 3; */
/*         u8 w: 1; */
/*         u8 op_code: 4; */
/*     }; */
/*     u8 byte; */
/* }im_reg; */

typedef struct {
    void *buffer;
    u32 index;
    u32 size;
}buffer;

char *byte_registers[] = {"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"};
char *word_registers[] = {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di"};
char *rm_mem_table[] = {"bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx"};
char *Instr_Names[] = {"add", "or", "adc", "sbb", "and", "sub", "xor", "cmp", "mov"};
char *Jump_Names[] = {"jo", "jno", "jb", "jnb", "je", "jne", "jbe", "jnbe", "js", "jns", "jp", "jnp", "jl", "jnl", "jle", "jnle"};
char *Loop_Names[] = {"loopnz", "loopz", "loop", "jcxz"};

op_and_type Handled_Instrs[] = {
{0x0, ADD, I_M_R_RM},
{0x1, ADD, I_M_R_RM},
{0x2, ADD, I_M_R_RM},
{0x3, ADD, I_M_R_RM},
{0x4, ADD, I_ACC},
{0x5, ADD, I_ACC},
{0x28, SUB, I_M_R_RM},
{0x29, SUB, I_M_R_RM},
{0x2A, SUB, I_M_R_RM},
{0x2B, SUB, I_M_R_RM},
{0x2C, SUB,I_ACC},
{0x2D, SUB,I_ACC},
{0x38, CMP, I_M_R_RM},
{0x39, CMP, I_M_R_RM},
{0x3A, CMP, I_M_R_RM},
{0x3B, CMP, I_M_R_RM},
{0x3C, CMP, I_ACC},
{0x3D, CMP, I_ACC},
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
{0x80, ANY, I_BYTE2},
{0x81, ANY, I_BYTE2},
{0x82, ANY, I_BYTE2},
{0x83, ANY, I_BYTE2},
{0x88, MOV, I_M_R_RM},
{0x89, MOV, I_M_R_RM},
{0x8A, MOV, I_M_R_RM},
{0x8B, MOV, I_M_R_RM},
{0x8C, MOV, I_ACC},
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
{0xC6, MOV, I_BYTE2},
{0xC7, MOV, I_BYTE2},
{0xE0, ANY, I_LOOP},
{0xE1, ANY, I_LOOP},
{0xE2, ANY, I_LOOP},
{0xE3, ANY, I_LOOP}

};

op_and_type All_Instrs[256] = {};


i16 U8ToI16(u8 high, u8 low) {
    i16 result = ((i16)high << 8 | low);
    return result;
}

u8 PopFromBuffer(buffer *buff) {
    u8 result = *(u8*)(buff->buffer + buff->index);
    ++buff->index;
    return result;
}

void ModRegRm(b1 byte, buffer *code_buffer, buffer* asm_buffer) {
    b2 byte2 = {.byte = PopFromBuffer(code_buffer)};
    op_and_type instr = All_Instrs[byte.byte];
    char *instr_name = Instr_Names[instr.name];
    char *rm = rm_mem_table[byte2.rm];
    char *reg = (byte.w) ? word_registers[byte2.reg] : byte_registers[byte2.reg];

    if (byte2.mod == MOD_REG) {
        rm = (byte.w) ? word_registers[byte2.rm] : byte_registers[byte2.rm];
        asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s %s, %s\n", instr_name, rm, reg);
        return;
    }

    char mem_addr[16] = {};
    i16 disp = 0;
    u8 mem_mode_16bit_disp = (byte2.mod == MOD_MEM & byte2.rm == 0b110) ? 1 : 0;

    if (byte2.mod == MOD_MEM_8) {
        i8 disp_lo = PopFromBuffer(code_buffer);
        disp = (i16)disp_lo;
    } else if ((mem_mode_16bit_disp) || (byte2.mod == MOD_MEM_16)) {
        u8 disp_lo = PopFromBuffer(code_buffer);
        u8 disp_hi = PopFromBuffer(code_buffer);
        disp = U8ToI16(disp_hi, disp_lo);
    }

    if (byte2.mod == MOD_MEM) {
        if (mem_mode_16bit_disp) {
            sprintf(mem_addr, "[%hd]", disp);
        } else {
            sprintf(mem_addr, "[%s]", rm);
        }
    } else {
        sprintf(mem_addr, "[%s + %hd]", rm, disp);
    }
    char *src = reg;
    char *dst = mem_addr;

    if (byte.d) {
        char *tmp = src;
        src = dst;
        dst = tmp;
    }

    printf("MOD_REG_RM: [%d, hex: %x]\n", byte.byte, byte.byte);
    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s %s, %s\n", instr_name, dst, src);
}

void AccInstr(b1 byte, buffer *code_buffer, buffer *asm_buffer) {
    op_and_type instr = All_Instrs[byte.byte];
    char *dst = (byte.w) ? "ax" : "al";
    u8 data_lo = PopFromBuffer(code_buffer);
    i16 data = (i8)data_lo;
    char *instr_name = Instr_Names[instr.name];
    char command[16] = {};
    if (byte.w) {
        u8 data_hi = PopFromBuffer(code_buffer);
        data = U8ToI16(data_hi, data_lo);
    }
    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s %s, %hd\n", instr_name, dst, data);

    printf("ACC: [%d, hex: %x]\n", byte.byte, byte.byte);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("No Filename passed to program. Exiting\n");
        return 0;
    }
    char *filename = argv[1];
    char *fileout = argv[2] ? argv[2] : "asm_out.asm";
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
        op_and_type inst = Handled_Instrs[i];
        All_Instrs[inst.b1] = inst;
    }

    buffer code_buffer = {.buffer = &file_buffer, .size = ARRAY_SIZE(file_buffer)};
    char out_buffer[4096];
    buffer asm_buffer = {.buffer = &out_buffer, .size = ARRAY_SIZE(out_buffer) };
    asm_buffer.index = sprintf(asm_buffer.buffer, ";;From file: %s\nbits 16\n", filename);

    int i = 0;
    while (code_buffer.index < bytes_read) {
        b1 byte = {.byte = PopFromBuffer(&code_buffer)};
        char command[32] = {};
        op_and_type instr = All_Instrs[byte.byte];
        switch(instr.type) {
            case I_ACC: {
                AccInstr(byte, &code_buffer, &asm_buffer);
                break;
            }
            case I_M_R_RM: {
                ModRegRm(byte, &code_buffer, &asm_buffer);
                break;
            }
            case I_BYTE2: {
                b2 byte2 = {.byte = PopFromBuffer(&code_buffer)};
                char *instr_name = (instr.name == ANY) ? Instr_Names[byte2.reg] : Instr_Names[instr.name];
                char *rm = rm_mem_table[byte2.rm];
                if (byte2.mod == MOD_REG) {
                    rm = (byte.w) ? word_registers[byte2.rm] : byte_registers[byte2.rm];
                    /* asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "%s %s, %s\n", instr_name, rm, reg); */
                    /* break; */
                }

                i16 disp = 0;
                u8 mem_mode_16bit_disp = (byte2.mod == MOD_MEM && byte2.rm == 0b110) ? 1 : 0;

                if (byte2.mod == MOD_MEM_8) {
                    i8 disp_lo = PopFromBuffer(&code_buffer);
                    disp = (i16)disp_lo;
                } else if ((mem_mode_16bit_disp) || byte2.mod == MOD_MEM_16) {
                    u8 disp_lo = PopFromBuffer(&code_buffer);
                    u8 disp_hi = PopFromBuffer(&code_buffer);
                    disp = U8ToI16(disp_hi, disp_lo);
                }

                char mem_addr[32] = {};
                if (byte2.mod == MOD_MEM) {
                    if (mem_mode_16bit_disp) {
                        sprintf(mem_addr, "[%hd]", disp);
                    } else {
                        sprintf(mem_addr, "[%s]", rm);
                    }
                } else {
                    sprintf(mem_addr, "[%s + %hd]", rm, disp);
                }

                char *dst = mem_addr;

                u8 data_lo = PopFromBuffer(&code_buffer);
                i16 data = (i8)data_lo;
                if (byte.w) {
                    u8 data_hi = PopFromBuffer(&code_buffer);
                    data = U8ToI16(data_hi, data_lo);
                }

                char data_str[16] = {};
                if (byte2.mod == MOD_REG) {
                    dst = rm;
                    sprintf(data_str, "%hd",  data);
                } else {
                    sprintf(data_str, "%s %hd", (byte.w) ? "word" : "byte", data);
                }
                printf("BYTE2: [%d, hex: %x]\n", byte.byte, byte.byte);
                asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "%s %s, %s\n", instr_name, dst, data_str);

                break;
            }
            case I_LOOP:
            case I_JUMP: {
                u8 inst_type = byte.byte & 0b00001111;
                char *instr_name = (instr.type == I_JUMP) ? Jump_Names[inst_type] : Loop_Names[inst_type];
                i8 inc_8 = PopFromBuffer(&code_buffer);
                asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "%s $+2%+hd\n", instr_name, inc_8);
                break;
            }
            default: {
                printf("OP: %d, %x, NOT handled\n", byte.byte, byte.byte);
                break;
            }
        }
    }

    printf("ASM File: Index: %d \n%s", asm_buffer.index, (char*)asm_buffer.buffer);
    FILE *out = fopen(fileout, "w");
    if (!out) {
        fprintf(stderr, "Error opening ASM OUT file.\n");
        return 1;
    }

    fprintf(out, "%s", (char*)asm_buffer.buffer);
    fclose(out);

    return 0;
}

