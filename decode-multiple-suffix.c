#include <stdio.h>
#include <stdint.h>

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
    char *reg = (cmd.w) ? word_registers[cmd.reg] : byte_registers[cmd.reg];
    char *rm = (cmd.w) ? word_registers[cmd.rm] : byte_registers[cmd.rm];

    if (cmd.mod != MOD_REG) {
        char rm_buffer[32] = {};
        rm = rm_table[cmd.rm];
        if (cmd.rm == 0b110 && cmd.mod == MOD_MEM) {
            sprintf(rm_buffer, "[%hd]", disp);
        } else if (disp != 0){
            sprintf(rm_buffer, "[%s + %hd]", rm, disp);
        } else {
            sprintf(rm_buffer, "[%s]", rm);
        }
        rm = rm_buffer;
    }

    if (cmd.d) {
        char *temp = reg;
        reg = rm;
        rm = temp;
    }

    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "mov %s, %s\n", rm, reg);
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
    asm_buffer.index = sprintf(out_buffer, ";;From file: %s\nbits 16\n", filename);

    int i = 0;
    while (code_buffer.index < bytes_read) {
        u8 byte = Pop_From_Buffer(&code_buffer);
        int count = 1;
        switch(byte >> 4) {
            case REG_REG: {
                reg_reg cmd = {};
                cmd.byte1 = byte;
                cmd.byte2 = Pop_From_Buffer(&code_buffer);

                switch(cmd.mod) {
                    case MOD_MEM: {
                        i16 disp = 0;
                        if (cmd.rm == 0b110) {
                            u8 disp_lo = Pop_From_Buffer(&code_buffer);
                            u8 disp_hi = Pop_From_Buffer(&code_buffer);
                            disp = U8_To_I16(disp_hi, disp_lo);
                        }
                        Write_To_Buffer(cmd, disp, &asm_buffer);
                        break;
                    }
                    case MOD_MEM_8: {
                        i8 disp = Pop_From_Buffer(&code_buffer);
                        Write_To_Buffer(cmd, disp, &asm_buffer);
                        break;
                    }
                    case MOD_MEM_16: {
                        u8 disp_lo = Pop_From_Buffer(&code_buffer);
                        u8 disp_hi = Pop_From_Buffer(&code_buffer);
                        u16 disp = U8_To_I16(disp_hi, disp_lo);
                        Write_To_Buffer(cmd, disp, &asm_buffer);
                        break;
                    }
                    case MOD_REG: {
                        Write_To_Buffer(cmd, 0, &asm_buffer);
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
                    case MOD_MEM: {
                        u8 data_lo = Pop_From_Buffer(&code_buffer);
                        sprintf(rm_buffer, "[%s]", rm);
                        if (cmd.w) {
                            u8 data_hi = Pop_From_Buffer(&code_buffer);
                            i16 data = U8_To_I16(data_hi, data_lo);
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, word %hd\n", rm_buffer, data);
                        } else {
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, byte %hd\n", rm_buffer, (i8)data_lo);
                        }
                        break;
                    }
                    case MOD_MEM_8: {
                        u8 disp_lo = Pop_From_Buffer(&code_buffer);
                        u8 data_lo = Pop_From_Buffer(&code_buffer);
                        sprintf(rm_buffer, "[%s + %hd]", rm, (i8)disp_lo);
                        if (cmd.w) {
                            u8 data_hi = Pop_From_Buffer(&code_buffer);
                            i16 data = U8_To_I16(data_hi, data_lo);
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, word %hd\n", rm_buffer, data);
                        } else {
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, byte %hd\n", rm_buffer, (i8)data_lo);
                        }
                        break;
                    }
                    case MOD_MEM_16: {
                        u8 disp_lo = Pop_From_Buffer(&code_buffer);
                        u8 disp_hi = Pop_From_Buffer(&code_buffer);
                        i16 disp = U8_To_I16(disp_hi, disp_lo);
                        u8 data_lo = Pop_From_Buffer(&code_buffer);
                        sprintf(rm_buffer, "[%s + %hd]", rm, disp);
                        if (cmd.w) {
                            u8 data_hi = Pop_From_Buffer(&code_buffer);
                            i16 data = U8_To_I16(data_hi, data_lo);
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, word %hd\n", rm_buffer, data);
                        } else {
                            asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, byte %hd\n", rm_buffer, (i8)data_lo);
                        }

                        break;
                    }
                    case MOD_REG: {
                        printf("IM_REG_MEM: REG_MODE: %x\n", cmd.byte1);
                        break;
                    }
                }
                break;
            }
            case IM_REG: {
                im_reg cmd = {};
                cmd.byte = byte;
                char *reg = byte_registers[cmd.reg];
                u8 data_lo = Pop_From_Buffer(&code_buffer);
                i16 data = data_lo;
                if (cmd.w) {
                    reg = word_registers[cmd.reg];
                    u8 data_hi = Pop_From_Buffer(&code_buffer);
                    data = U8_To_I16(data_hi, data_lo);
                }
                asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, %hd\n", reg, data);
                break;
            }
            case MEM_ACC: {
                reg_reg cmd = {};
                cmd.byte1 = byte;
                u8 addr_lo = Pop_From_Buffer(&code_buffer);
                u8 addr_hi = Pop_From_Buffer(&code_buffer);
                i16 address = U8_To_I16(addr_hi, addr_lo);
                char *acc = (cmd.w) ? "ax" : "al";
                if (cmd.d) {
                    asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov [%hd], %s\n",  address, acc);
                } else {
                    asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, [%hd]\n",  acc, address);
                }
                break;
            }
            default: {
                printf("Cannot handle instruction: %x\n", byte);
                break;
            }
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
