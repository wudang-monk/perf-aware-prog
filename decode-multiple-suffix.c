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
ACC = 1,
BYTE2 = 2,
M_R_RM = 3,
MOV_I = 4
}inst_type;

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
ANY = 9
}op_type;

typedef union {
    struct {
        u8 rm: 3;
        u8 reg: 3;
        mod mod: 2;
    };
    u8 byte;
}mod_reg_rm;

typedef struct {
    u8 byte1;
    op_type name;
    inst_type type;
}op_and_type;

typedef union {
    struct {
        u16 w : 1;
        u16 d : 1;
        u16 opcode: 6;
        u16 rm: 3;
        u16 reg: 3;
        u16 mod: 2;
    };
    struct {
        u8 byte1;
        u8 byte2;
    };
    u8 bytes[2];
}reg_reg;

typedef union {
    struct {
        u8 reg : 3;
        u8 w: 1;
        u8 op_code: 4;
    };
    u8 byte;
}im_reg;

typedef struct {
    void *buffer;
    u32 index;
    u32 size;
}buffer;

char *byte_registers[] = {"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"};
char *word_registers[] = {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di"};
char *rm_mem_table[] = {"bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx"};
char *Instr_Names[] = {"add", "or", "adc", "sbb", "and", "sub", "xor", "cmp", "mov"};


op_and_type Handled_Instrs[] = {
{0x0, ADD, M_R_RM},
{0x1, ADD, M_R_RM},
{0x2, ADD, M_R_RM},
{0x3, ADD, M_R_RM},
{0x4, ADD, ACC},
{0x5, ADD, ACC},
{0x28, SUB, M_R_RM},
{0x29, SUB, M_R_RM},
{0x2A, SUB, M_R_RM},
{0x2B, SUB, M_R_RM},
{0x2C, SUB,ACC},
{0x2D, SUB,ACC},
{0x38, CMP, M_R_RM},
{0x39, CMP, M_R_RM},
{0x3A, CMP, M_R_RM},
{0x3B, CMP, M_R_RM},
{0x3C, CMP, ACC},
{0x3D, CMP, ACC},
{0x80, ANY, BYTE2},
{0x81, ANY, BYTE2},
{0x82, ANY, BYTE2},
{0x83, ANY, BYTE2},
{0x88, MOV, M_R_RM},
{0x89, MOV, M_R_RM},
{0x8A, MOV, M_R_RM},
{0x8B, MOV, M_R_RM},
{0x8C, MOV, ACC},
{0xA0, MOV, ACC},
{0xA1, MOV, ACC},
{0xA2, MOV, ACC},
{0xA3, MOV, ACC},
{0xB0, MOV, MOV_I},
{0xB1, MOV, MOV_I},
{0xB2, MOV, MOV_I},
{0xB3, MOV, MOV_I},
{0xB4, MOV, MOV_I},
{0xB5, MOV, MOV_I},
{0xB6, MOV, MOV_I},
{0xB7, MOV, MOV_I},
{0xB8, MOV, MOV_I},
{0xB9, MOV, MOV_I},
{0xBA, MOV, MOV_I},
{0xBB, MOV, MOV_I},
{0xBC, MOV, MOV_I},
{0xBD, MOV, MOV_I},
{0xBE, MOV, MOV_I},
{0xBF, MOV, MOV_I},
{0xC6, MOV, BYTE2},
{0xC7, MOV, BYTE2}
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

void ModRegRm(u8 byte1, buffer *code_buffer) {
    u8 wide = byte1 & 0b00000001;
    u8 reverse = byte1 & 0b00000010;
    mod_reg_rm byte2 = {.byte = PopFromBuffer(code_buffer)};
    char command[16] = {};
    op_and_type instr = All_Instrs[byte1];
    char *instr_name = Instr_Names[instr.name];
    switch(byte2.mod) {
        case MOD_MEM:
        case MOD_MEM_8:
        case MOD_MEM_16: {
            char *rm = rm_mem_table[byte2.rm];
            char *reg = (wide) ? word_registers[byte2.reg] : byte_registers[byte2.reg];
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
                sprintf(mem_addr, "[%s]", rm);
            } else {
                sprintf(mem_addr, "[%s + %hd]", rm, disp);
            }
            char *src = reg;
            char *dst = mem_addr;

            if (mem_mode_16bit_disp) {
                sprintf(command, "%s %s, [%hd]", instr_name, reg, disp);
            } else {
                if (reverse) {
                    char *tmp = src;
                    src = dst;
                    dst = tmp;
                }
                sprintf(command, "%s %s, %s", instr_name, dst, src);
            }
            printf("Command: %s\n", command);
            break;
        }
        case MOD_REG: {
            printf("MOD Register mode: NOT Implemented\n");
            break;
        }
    }
}


void AccInstr(u8 byte1, buffer *code_buffer) {
    u8 wide = byte1 & 0b00000001;
    op_and_type instr = All_Instrs[byte1];
    char *dst = "al";
    u8 data_lo = PopFromBuffer(code_buffer);
    i16 data = (i8)data_lo;
    char command[16] = {};

    if (instr.name == MOV) {
        u8 data_hi = PopFromBuffer(code_buffer);
        data = U8ToI16(data_hi, data_lo);
        char mem_addr[8] = {};
        sprintf(mem_addr, "[%hd]", data);
        char *src = mem_addr;
        u8 reverse = byte1 & 0b00000010;

        if (reverse) {
            char *tmp = dst;
            dst = src;
            src = tmp;
        }
        sprintf(command, "mov %s, %s", dst, src);

    } else {
        if (wide) {
            dst = "ax";
            u8 data_hi = PopFromBuffer(code_buffer);
            data = U8ToI16(data_hi, data_lo);
        }

        char *instr_name = Instr_Names[instr.name];
        sprintf(command, "%s %s, %hd", instr_name, dst, data);
    }
    printf("Command: %s\n", command);
}


/* void Process_BYTE2_OP(u8 byte, buffer *code_buffer) { */
/*     mod_reg_rm byte_op = {.byte = Pop_From_Buffer(code_buffer)}; */
/*     char *op_name = op_names[byte_op.reg]; */
/*     char command[32] = {}; */
/*     char data_str[16] = {}; */
/*     char rm_str[16] = {}; */
/*     char *rm; */
/*     switch(byte_op.mod) { */
/*         case MOD_MEM: { */
/*             rm = rm_mem_table[byte_op.rm]; */
/*             /\* char *reg = byte_registers[byte_op.reg]; *\/ */
/*             u8 wide = byte & 0b00000001; */
/*             u8 sign_ext = byte & 0b00000010; */
/*             u8 data_lo = Pop_From_Buffer(code_buffer); */
/*             i16 data = 0; */
/*             char *data_size = "byte"; */
/*             if (wide) { */
/*                 data_size = "word"; */
/*                 u8 data_hi = Pop_From_Buffer(code_buffer); */
/*                 data = U8_To_I16(data_hi, data_lo); */
/*                 if (sign_ext == 0) { */
/*                     printf("Sign extension!!!!\n"); */
/*                 } */
/*             } else { */
/*                 data = (i8)data_lo; */
/*             } */

/*             sprintf(rm_str, "[%s]", rm); */
/*             sprintf(command, "%s %s, %s, %hd\n", op_name, rm_str, data_size, data); */
/*             printf("BYTE2_OP: MOD_MEM %s", command); */
/*             break; */
/*         } */
/*         case MOD_MEM_8: { */
/*             rm = rm_mem_table[byte_op.rm]; */
/*             u8 wide = byte & 0b00000001; */
/*             u8 sign_ext = byte & 0b00000010; */
/*             i8 disp = Pop_From_Buffer(code_buffer); */
/*             u8 data_lo = Pop_From_Buffer(code_buffer); */
/*             char *data_size = "byte"; */
/*             i16 data = data_lo; */

/*             if (wide) { */
/*                 data_size = "word"; */
/*                 u8 data_hi = Pop_From_Buffer(code_buffer); */
/*                 data = U8_To_I16(data_hi, data_lo); */
/*                 if (sign_ext == 0) { */
/*                     printf("Sign extension!!!!\n"); */
/*                 } */
/*             } */

/*             char rm_str[16] = {}; */
/*             char *sum_or_minus = (disp > 0) ? "+" : "-"; */
/*             sprintf(rm_str, "[%s %s %hd]", rm, sum_or_minus, abs(disp)); */
/*             sprintf(command, "%s %s, %s %hd\n", op_name, rm_str, data_size, data); */
/*             printf("BYTE2_OP: MOD_MEM_8 %s", command); */

/*             break; */
/*         } */
/*         case MOD_MEM_16: { */
/*             rm = rm_mem_table[byte_op.rm]; */
/*             u8 wide = byte & 0b00000001; */
/*             u8 sign_ext = byte & 0b00000010; */
/*             u8 disp_lo = Pop_From_Buffer(code_buffer); */
/*             u8 disp_hi = Pop_From_Buffer(code_buffer); */
/*             u8 data_lo = Pop_From_Buffer(code_buffer); */
/*             i16 disp = U8_To_I16(disp_hi, disp_lo); */
/*             i16 data = data_lo; */
/*             char *data_size = "byte"; */
/*             if (wide) { */
/*                 data_size = "word"; */
/*                 u8 data_hi = Pop_From_Buffer(code_buffer); */
/*                 data = U8_To_I16(data_hi, data_lo); */
/*                 if (sign_ext == 0) { */
/*                     printf("Sign extension!!!!\n"); */
/*                 } */
/*             } */

/*             char rm_str[16] = {}; */
/*             char *sum_or_minus = (disp > 0) ? "+" : "-"; */
/*             sprintf(rm_str, "[%s %s %hd]", rm, sum_or_minus, abs(disp)); */
/*             sprintf(command, "%s %s, %s %hd\n", op_name, rm_str, data_size, data); */
/*             printf("BYTE2_OP: MOD_MEM_16 %s", command); */
/*             break; */
/*         } */
/*         case MOD_REG: { */
/*             rm = byte_registers[byte_op.rm]; */
/*             u8 wide = byte & 0b00000001; */
/*             u8 sign_ext = byte & 0b00000010; */
/*             u8 data_lo = Pop_From_Buffer(code_buffer); */
/*             i16 data = data_lo; */
/*             if (wide) { */
/*                 rm = word_registers[byte_op.rm]; */
/*                 if (sign_ext == 0) { */
/*                     u8 data_hi = Pop_From_Buffer(code_buffer); */
/*                     data = U8_To_I16(data_hi, data_lo); */
/*                 } */
/*             } */
/*             sprintf(command, "%s %s, %hd\n", op_name, rm, data); */
/*             printf("BYTE2_OP: MOD_REG %s", command); */
/*             break; */
/*         } */
/*     } */
/* } */


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

    u8 file_buffer[256] = {};
    u64 bytes_read = fread(file_buffer, 1, sizeof(file_buffer), fp);
    if (ferror(fp)) {
        fprintf(stderr, "Error opening BINARY IN file.\n");
        return 1;
    }

    fclose(fp);
    for (int i = 0; i < ARRAY_SIZE(Handled_Instrs); i++) {
        op_and_type inst = Handled_Instrs[i];
        All_Instrs[inst.byte1] = inst;
    }

    buffer code_buffer = {.buffer = &file_buffer, .size = 256};
    char out_buffer[1024];
    buffer asm_buffer = {.buffer = &out_buffer, .size = 1024 };
    asm_buffer.index = sprintf(out_buffer, ";;From file: %s\nbits 16\n", filename);

    int i = 0;
    while (code_buffer.index < bytes_read) {
        u8 byte1 = PopFromBuffer(&code_buffer);
        /* printf("BYTE: %d\n", byte); */
        op_and_type op = All_Instrs[byte1];
        switch(op.type) {
            case ACC: {
                AccInstr(byte1, &code_buffer);
                break;
            }
            case M_R_RM: {
                ModRegRm(byte1, &code_buffer);
                break;
            }
            /* case M_R_RM: { */
            /*     char *op_name = op_names[op.name]; */
            /*     printf("MOD_REG_RM:  "); */
            /*     mod_reg_rm byte2 = {.byte = Pop_From_Buffer(&code_buffer)}; */
            /*     switch(byte2.mod) { */
            /*         case MOD_MEM: { */
            /*             u8 wide = byte1 & 0b00000001; */
            /*             char *reg = (wide) ? word_registers[byte2.reg] : byte_registers[byte2.reg]; */
            /*             char *rm = rm_mem_table[byte2.rm]; */
            /*             u8 sign_ext = byte1 & 0b00000010; */
            /*             char rm_str[16] = {}; */
            /*             char data_str[16] = {}; */
            /*             sprintf(rm_str, "[%s]", rm); */
            /*             rm = rm_str; */

            /*             if (byte2.rm == 0b110) { */
            /*                 printf("16bit displacement\n"); */
            /*                 u8 disp_lo = Pop_From_Buffer(&code_buffer); */
            /*                 u8 disp_hi = Pop_From_Buffer(&code_buffer); */
            /*                 i16 disp = U8_To_I16(disp_hi, disp_lo); */
            /*                 sprintf(rm_str, "[%hd]", disp); */

            /*             } */

            /*             if (sign_ext) { */
            /*                 char *tmp = reg; */
            /*                 reg = rm; */
            /*                 rm = tmp; */
            /*             } */
            /*             /\* sprintf(command, "%s %s, %hd\n", op_name, rm_str, reg); *\/ */
            /*             /\* printf("MOD_MEM:  \n"); *\/ */
            /*             printf("MOD_MEM:  %s %s, %s\n", op_name, rm, reg); */

            /*             break; */
            /*         } */
            /*         case MOD_MEM_8: { */
            /*             u8 wide = byte1 & 0b00000001; */
            /*             char *reg = (wide) ? word_registers[byte2.reg] : byte_registers[byte2.reg]; */
            /*             char *rm = rm_mem_table[byte2.rm]; */
            /*             u8 sign_ext = byte1 & 0b00000010; */
            /*             i8 data_lo = Pop_From_Buffer(&code_buffer); */
            /*             char rm_str[16] = {}; */
            /*             sprintf(rm_str, "[%s + %hd]", rm, data_lo); */
            /*             rm = rm_str; */

            /*             if (sign_ext) { */
            /*                 char *tmp = reg; */
            /*                 reg = rm; */
            /*                 rm = tmp; */
            /*             } */
            /*             /\* sprintf(command, "%s %s, %hd\n", op_name, rm_str, reg); *\/ */
            /*             printf("MOD_MEM_8:  %s %s, %s\n", op_name, rm, reg); */
            /*             break; */
            /*         } */
            /*         case MOD_MEM_16: { */
            /*             u8 wide = byte1 & 0b00000001; */
            /*             char *reg = (wide) ? word_registers[byte2.reg] : byte_registers[byte2.reg]; */
            /*             char *rm = rm_mem_table[byte2.rm]; */
            /*             u8 sign_ext = byte1 & 0b00000010; */
            /*             u8 data_lo = Pop_From_Buffer(&code_buffer); */
            /*             u8 data_hi = Pop_From_Buffer(&code_buffer); */
            /*             i16 data = U8_To_I16(data_hi, data_lo); */

            /*             char rm_str[16] = {}; */
            /*             sprintf(rm_str, "[%s %s %d]", rm, (data > 0) ? "+" : "-", data); */
            /*             rm = rm_str; */

            /*             if (sign_ext) { */
            /*                 char *tmp = reg; */
            /*                 reg = rm; */
            /*                 rm = tmp; */
            /*             } */
            /*             /\* sprintf(command, "%s %s, %hd\n", op_name, rm_str, reg); *\/ */
            /*             printf("MOD_MEM_16:  %s %s, %s\n", op_name, rm, reg); */
            /*             break; */
            /*         } */
            /*         case MOD_REG: { */
            /*             u8 wide = byte1 & 0b00000001; */
            /*             char *reg = (wide) ? word_registers[byte2.reg] : byte_registers[byte2.reg]; */
            /*             char *rm = (wide) ? word_registers[byte2.rm] : byte_registers[byte2.rm]; */
            /*             u8 sign_ext = byte1 & 0b00000010; */
            /*             char rm_str[16] = {}; */

            /*             if (sign_ext) { */
            /*                 char *tmp = reg; */
            /*                 reg = rm; */
            /*                 rm = tmp; */
            /*             } */
            /*             /\* sprintf(command, "%s %s, %hd\n", op_name, rm_str, reg); *\/ */
            /*             printf("MOD_REG:  %s %s, %s\n", op_name, rm, reg); */

            /*             break; */
            /*         } */

            /*     } */

            /*     break; */
            /* } */
            /* case MOV_I: { */
            /*     char *op_name = op_names[op.name]; */
            /*     u8 data_lo = Pop_From_Buffer(&code_buffer); */
            /*     u8 wide = (byte1 >> 3) & 0b00001; */
            /*     u8 mov_reg = byte1 & 0b00000111; */
            /*     char *reg = byte_registers[mov_reg]; */
            /*     i16 data = data_lo; */
            /*     if (wide) { */
            /*         u8 data_hi = Pop_From_Buffer(&code_buffer); */
            /*         data = U8_To_I16(data_hi, data_lo); */
            /*         reg = word_registers[mov_reg]; */
            /*     } */
            /*     printf("MOV_IMM: %s %s, %hd\n", op_name, reg, data); */
            /*     break; */
            /* } */
            default: {
                printf("OP: %d, %x, NOT handled\n", byte1, byte1);
                break;
            }
        }

    }

    /* printf("ASM File:\n%s", out_buffer); */
    /* FILE *out = fopen(fileout, "w"); */
    /* if (!out) { */
    /*     fprintf(stderr, "Error opening ASM OUT file.\n"); */
    /*     return 1; */
    /* } */

    /* fprintf(out, "%s", (char*)asm_buffer.buffer); */
    /* fclose(out); */

    return 0;
}

