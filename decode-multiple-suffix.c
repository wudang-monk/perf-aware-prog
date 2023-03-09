#include <stdio.h>
#include <stdint.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;

typedef enum {
MEM_MODE = 0b0,
MEM_MODE_8 = 0b1,
MEM_MODE_16 = 0b10,
REG_MODE = 0b11
}mod;

typedef enum {
REG_REG = 0b1000,
IM_REG_MEM = 0b1100,
IM_REG = 0b1011,
MEM_ACC = 0b1010,
}mov_higbits;

typedef union {
    struct {
        u16 w : 1;
        u16 d : 1;
        u16 opcode : 6;
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
char *rm_table[] = {"bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx"};

void Write_To_Buffer(reg_reg cmd, i16 disp, buffer *asm_buffer) {
    char *reg;
    char *rm;

    if (cmd.w) {
        reg = word_registers[cmd.reg];
        rm = word_registers[cmd.rm];
    } else {
        reg = byte_registers[cmd.reg];
        rm = byte_registers[cmd.rm];
    }

    if (cmd.mod != REG_MODE) {
        char rm_buffer[32] = {};
        rm = rm_table[cmd.rm];
        if (cmd.rm == 0b110 && cmd.mod == MEM_MODE) {
            sprintf(rm_buffer, "[%hd]", disp);
        } else if (disp != 0){
            sprintf(rm_buffer, "[%s + %hd]", rm, disp);
        } else {
            sprintf(rm_buffer, "[%s]", rm);
        }
        rm = rm_buffer;
    }

    char *src = reg;
    char *dst = rm;
    if (cmd.d) {
        src = rm;
        dst = reg;
    }

    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "mov %s, %s\n", dst, src);
}

i16 U8_To_I16(u8 high, u8 low) {
    i16 result = ((i16)high << 8 | low);
    return result;
}

u8 Pop_From_Buffer(buffer *buff) {
    u8 result = *(u8*)(buff->buffer + buff->index);
    ++buff->index;
    return result;
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

    u8 file_buffer[256] = {};
    u64 bytes_read = fread(file_buffer, 1, sizeof(file_buffer), fp);
    if (ferror(fp)) {
        fprintf(stderr, "Error opening BINARY IN file.\n");
        return 1;
    }

    fclose(fp);

    buffer code_buffer = {.buffer = &file_buffer, .size = 256};
    char out_buffer[1024];
    buffer asm_buffer = {.buffer = &out_buffer, .size = 1024 };
    asm_buffer.index = sprintf(out_buffer, ";;File: %s\nbits 16\n", filename);

    int i = 0;
    while (code_buffer.index < bytes_read) {
        u8 byte = Pop_From_Buffer(&code_buffer);
        /* byte_lh byte = (byte_lh)file_buffer[i]; */
        int count = 1;
        switch(byte >> 4) {
            case REG_REG: {
                reg_reg cmd = {};
                cmd.byte1 = byte;
                cmd.byte2 = Pop_From_Buffer(&code_buffer);

                switch(cmd.mod) {
                    case MEM_MODE: {
                        char *reg = (cmd.w) ? word_registers[cmd.reg] : byte_registers[cmd.reg];
                        char *rm = rm_table[cmd.rm];
                        char rm_buffer[32] = {};
                        printf("REG_REG MEM_MODE: %x\n", cmd.byte1);
                        if (cmd.rm == 0b110) {
                            /* sprintf(rm_buffer, "[%hd]", disp); */
                            u8 disp_lo = Pop_From_Buffer(&code_buffer);
                            u8 disp_hi = Pop_From_Buffer(&code_buffer);
                            i16 disp = U8_To_I16(disp_hi, disp_lo);
                            Write_To_Buffer(cmd, disp, &asm_buffer);
                        } else {
                            Write_To_Buffer(cmd, 0, &asm_buffer);
                        }
                        break;
                    }
                    case MEM_MODE_8: {
                        char *reg = (cmd.w) ? word_registers[cmd.reg] : byte_registers[cmd.reg];
                        char *rm = rm_table[cmd.rm];
                        char rm_buffer[32] = {};
                        printf("REG_REG MEM_MODE_8: %x\n", cmd.byte1);
                        i8 disp = Pop_From_Buffer(&code_buffer);
                        Write_To_Buffer(cmd, disp, &asm_buffer);
                        break;
                    }
                    case MEM_MODE_16: {
                        printf("REG_REG MEM_MODE_16: %x\n", cmd.byte1);
                        char *reg = (cmd.w) ? word_registers[cmd.reg] : byte_registers[cmd.reg];
                        char *rm = rm_table[cmd.rm];
                        char rm_buffer[32] = {};
                        u8 disp_lo = Pop_From_Buffer(&code_buffer);
                        u8 disp_hi = Pop_From_Buffer(&code_buffer);
                        u16 disp = U8_To_I16(disp_hi, disp_lo);
                        Write_To_Buffer(cmd, disp, &asm_buffer);
                        break;
                    }
                    case REG_MODE: {
                        printf("REG_REG REG_MODE: %x\n", cmd.byte1);
                        char *reg = (cmd.w) ? word_registers[cmd.reg] : byte_registers[cmd.reg];
                        char *rm = (cmd.w) ? word_registers[cmd.rm] : byte_registers[cmd.rm];
                        if (cmd.d) {
                            char *tmp = reg;
                            reg = rm;
                            rm = tmp;
                        }
                        asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, %s\n", rm, reg);
                        break;
                    }
                }
                break;
            }
            case IM_REG_MEM: {
                reg_reg cmd = {};
                cmd.byte1 = byte;
                cmd.byte2 = Pop_From_Buffer(&code_buffer);
                char rm_buffer[32] = {};
                char *rm = rm_table[cmd.rm];
                switch(cmd.mod) {
                    case MEM_MODE: {
                        u8 data_lo = Pop_From_Buffer(&code_buffer);
                        sprintf(rm_buffer, "[%s]", rm);
                        if (cmd.w) {
                            u8 data_hi = Pop_From_Buffer(&code_buffer);
                            i16 data = U8_To_I16(data_hi, data_lo);
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, word %hd\n", rm_buffer, data);
                            /* printf("IM_REG_MEM: MEM_MODE: mov %s, word %hd\n", rm_buffer, data); */
                        } else {
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, byte %hd\n", rm_buffer, (i8)data_lo);
                            /* printf("IM_REG_MEM: MEM_MODE: move %s, byte %hd\n", rm_buffer, (i8)data_lo); */
                        }
                        break;
                    }
                    case MEM_MODE_8: {
                        u8 disp_lo = Pop_From_Buffer(&code_buffer);
                        u8 data_lo = Pop_From_Buffer(&code_buffer);
                        sprintf(rm_buffer, "[%s + %hd]", rm, (i8)disp_lo);
                        if (cmd.w) {
                            u8 data_hi = Pop_From_Buffer(&code_buffer);
                            i16 data = U8_To_I16(data_hi, data_lo);
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, word %hd\n", rm_buffer, data);
                            /* printf("IM_REG_MEG: MEM_MODE_8: mov %s, word %hd\n", rm_buffer, data); */
                        } else {
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, byte %hd\n", rm_buffer, (i8)data_lo);
                            /* printf("IM_REG_MEG: MEM_MODE_8: mov %s, byte %hd\n", rm_buffer, (i8)data_lo); */
                        }
                        break;
                    }
                    case MEM_MODE_16: {
                        u8 disp_lo = Pop_From_Buffer(&code_buffer);
                        u8 disp_hi = Pop_From_Buffer(&code_buffer);
                        i16 disp = U8_To_I16(disp_hi, disp_lo);
                        u8 data_lo = Pop_From_Buffer(&code_buffer);
                        sprintf(rm_buffer, "[%s + %hd]", rm, disp);
                        if (cmd.w) {
                            u8 data_hi = Pop_From_Buffer(&code_buffer);
                            i16 data = U8_To_I16(data_hi, data_lo);
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, word %hd\n", rm_buffer, data);
                            /* printf("IM_REG_MEG: MEM_MODE_16: mov %s, word %hd\n", rm_buffer, data); */
                        } else {
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, byte %hd\n", rm_buffer, (i8)data_lo);
                            /* printf("IM_REG_MEG: MEM_MODE_16: mov %s, byte %hd\n", rm_buffer, (i8)data_lo); */
                        }

                        break;
                    }
                    case REG_MODE: {
                        printf("IM_REG_MEM: REG_MODE: %x\n", cmd.byte1);
                        break;
                    }
                }
                break;
            }
            case IM_REG: {
                im_reg cmd = {};
                cmd.byte = byte;
                printf("IM_REG: %x\n", cmd.byte);
                char *reg = byte_registers[cmd.reg];
                u8 data_lo = Pop_From_Buffer(&code_buffer);
                if (cmd.w) {
                    reg = word_registers[cmd.reg];
                    u8 data_hi = Pop_From_Buffer(&code_buffer);
                    u16 data_16 = U8_To_I16(data_hi, data_lo);
                    asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, %hd\n", reg, data_16);
                } else {
                    asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, %ud\n", reg, data_lo);
                }

                break;
            }
            case MEM_ACC:
                Pop_From_Buffer(&code_buffer);
                Pop_From_Buffer(&code_buffer);
                printf("MEM_ACC: %x\n", byte);
                count += 2;
                break;
            default:
                printf("Cannot handle instruction: %x\n", byte);
                break;
        }
    }

    printf("ASM File:\n%s", out_buffer);
    FILE *out = fopen(fileout, "w");
    if (!out) {
        fprintf(stderr, "Error opening ASM OUT file.\n");
        return 1;
    }

    fprintf(out, "%s", (char*)asm_buffer.buffer);
    fclose(out);

    return 0;
}
