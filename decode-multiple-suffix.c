#include <stdio.h>
#include <stdint.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef enum {
MEM_MODE = 0b0,
MEM_MODE_8 = 0b1,
MEM_MODE_16 = 0b10,
REG_MODE = 0b11
}mod;

typedef union {
    struct {
        u8 low : 4;
        u8 high : 4;
    };
    u8 whole;
}byte_lh;

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

void Print_Binary(u8 byte) {
    u8 bits[8] = {};
    int i = 7;
    int count = 0;
    int tmp_num = byte;
    while (tmp_num > 0) {
        bits[i] = tmp_num % 2;
        tmp_num /= 2;
        i--;
        count++;
    }

    printf("%d   (%d bits, #x%x, #b", byte, count, byte);
    for (int j = 0; j < 8; j++) {
        printf("%d", bits[j]);
    }
    printf(")\n");
}

void Print_Instruction(reg_reg cmd) {
    printf("\nOPCODE: ");
    Print_Binary(cmd.opcode);
    printf("D:      ");
    Print_Binary(cmd.d);
    printf("W:      ");
    Print_Binary(cmd.w);
    printf("MOD:    ");
    Print_Binary(cmd.mod);
    printf("REG:    ");
    Print_Binary(cmd.reg);
    printf("R/M:    ");
    Print_Binary(cmd.rm);
    printf("Byte1:   ");
    Print_Binary(cmd.byte1);
    printf("Byte2:   ");
    Print_Binary(cmd.byte2);
}

char *byte_registers[] = {"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"};
char *word_registers[] = {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di"};
char *rm_table[] = {"bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx"};

void Write_To_Buffer(char *reg, char *rm, reg_reg cmd, u16 disp, buffer *asm_buffer) {
    reg = byte_registers[cmd.reg];
    rm = byte_registers[cmd.rm];

    if (cmd.w) {
        reg = word_registers[cmd.reg];
        rm = word_registers[cmd.rm];
    }

    if (cmd.mod != REG_MODE) {
        rm = rm_table[cmd.rm];
        char rm_buffer[32] = {};
        int count = 0;
        if (disp > 0) {
            count += sprintf(rm_buffer, "[%s + %hd]", rm, disp);
        } else {
            count += sprintf(rm_buffer, "[%s]", rm);
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

    char out_buffer[1024];
    buffer asm_buffer = {.buffer = &out_buffer, .size = 1024 };
    asm_buffer.index = sprintf(out_buffer, ";;File: %s\nbits 16\n", filename);

    int i = 0;
    while (i < bytes_read) {
        byte_lh byte = (byte_lh)file_buffer[i];
        int count = 1;
        switch(byte.high) {
            case REG_REG: {
                reg_reg cmd = {};
                cmd.byte1 = file_buffer[i];
                cmd.byte2 = file_buffer[i + 1];
                char *reg;
                char *rm;

                switch(cmd.mod) {
                    case MEM_MODE: {
                        Write_To_Buffer(reg, rm, cmd, 0, &asm_buffer);
                        break;
                    }
                    case MEM_MODE_8: {
                        u8 disp = file_buffer[i + 2];
                        Write_To_Buffer(reg, rm, cmd, disp, &asm_buffer);
                        u16 test = file_buffer[i + 2];
                        printf("Mem_mode_8 u16: %d, combined: %d\n", test, disp);

                        count += 1;
                        break;
                    }
                    case MEM_MODE_16: {
                        u8 disp_low = file_buffer[i + 2];
                        u8 disp_high = file_buffer[i + 3];
                        u16 disp = ((u16)disp_high << 8) | disp_low;
                        u16 test = file_buffer[i + 2];
                        printf("Mem_mode_16 u16: %d, combined: %d\n", test, disp);

                        Write_To_Buffer(reg, rm, cmd, disp, &asm_buffer);
                        count += 2;
                        break;
                    }
                    case REG_MODE: {
                        Write_To_Buffer(reg, rm, cmd, 0, &asm_buffer);
                        break;
                    }
                }
                count += 1;
                break;
            }
            case IM_REG_MEM: {
                im_reg cmd = {};
                cmd.byte = file_buffer[i];
                char *reg = byte_registers[cmd.reg];
                if (cmd.w) {
                    reg = word_registers[cmd.reg];
                }
                printf("IM_REG_MEM: %s   \n", reg);
                break;
            }
            case IM_REG: {
                im_reg cmd = {};
                cmd.byte = file_buffer[i];
                char *reg = byte_registers[cmd.reg];
                u8 data_low = file_buffer[i + 1];
                if (cmd.w) {
                    reg = word_registers[cmd.reg];
                    u8 data_high = file_buffer[i + 2];
                    u16 data_16 = ((u16)data_high << 8) | data_low;
                    u16 test = (u16)file_buffer[i + 1];
                    printf("IM_reg u16: %d, combiner: %d\n", test, data_16);
                    asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, %hd\n", reg, data_16);
                    count += 1;
                } else {
                    asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "mov %s, %ud\n", reg, data_low);
                }

                count += 1;
                break;
            }
            case MEM_ACC:
                printf("MEM_ACC: %x\n", byte.whole);
                break;
            default:
                printf("Cannot handle instruction: %x\n", byte.whole);
                break;
        }
        i += count;
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
