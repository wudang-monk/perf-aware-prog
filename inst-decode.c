#include <stdio.h>
#include <stdint.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint64_t u64;

typedef union {
    struct {
        u16 w : 1;
        u16 d : 1;
        u16 opcode : 6;
        u16 rm: 3;
        u16 reg: 3;
        u16 mod: 2;
    };
    u16 command;
    u8 cmds[2];
}inst16;

// Helper functions to help us see if things are working right
/* void Print_Binary(u8 byte) { */
/*     u8 bits[8] = {}; */
/*     int i = 7; */
/*     int count = 0; */
/*     int tmp_num = byte; */
/*     while (tmp_num > 0) { */
/*         bits[i] = tmp_num % 2; */
/*         tmp_num /= 2; */
/*         i--; */
/*         count++; */
/*     } */

/*     printf("%d   (%d bits, #x%x, #b", byte, count, byte); */
/*     for (int j = 0; j < 8; j++) { */
/*         printf("%d", bits[j]); */
/*     } */
/*     printf(")\n"); */
/* } */

/* void Print_Instruction(inst16 cmd) { */
/*     printf("OPCODE: "); */
/*     Print_Binary(cmd.opcode); */
/*     printf("D:      "); */
/*     Print_Binary(cmd.d); */
/*     printf("W:      "); */
/*     Print_Binary(cmd.w); */
/*     printf("MOD:    "); */
/*     Print_Binary(cmd.mod); */
/*     printf("REG:    "); */
/*     Print_Binary(cmd.reg); */
/*     printf("R/M:    "); */
/*     Print_Binary(cmd.rm); */
/*     printf("First:   "); */
/*     Print_Binary(cmd.cmds[0]); */
/*     printf("Second:   "); */
/*     Print_Binary(cmd.cmds[1]); */

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

    u8 buffer[256] = {};
    u64 bytes_read = fread(buffer, 1, sizeof(buffer), fp);
    if (ferror(fp)) {
        fprintf(stderr, "Error opening BINARY IN file.\n");
        return 1;
    }

    fclose(fp);

    char *word_registers[] = {"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"};
    char *byte_registers[] = {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di"};

    char asm_buffer[1024];
    int asm_buffer_index = sprintf(asm_buffer, ";;File: %s\nbits 16\n", filename);
    for (int i = 0; i < bytes_read; i += 2) {
        inst16 cmd = {};
        cmd.cmds[0] = buffer[i];
        cmd.cmds[1] = buffer[i + 1];
        // Decode instruction
        char *reg = (cmd.w) ? byte_registers[cmd.reg] : word_registers[cmd.reg];
        char *rm = (cmd.w) ? byte_registers[cmd.rm] : word_registers[cmd.rm];
        char *src = reg;
        char *dst = rm;
        if (cmd.d) {
            src = rm;
            dst = reg;
        }
        /* Print_Instruction(cmd); */
        /* printf("SRC: %s, DST: %s\n", src, dst); */
        // Write out instruction to asm buffer
        asm_buffer_index += sprintf(asm_buffer + asm_buffer_index, "mov %s, %s\n", dst, src);
    }
    // Write out buffer to file to compare
    FILE *out = fopen(fileout, "w");
    if (!out) {
        fprintf(stderr, "Error opening ASM OUT file.\n");
        return 1;
    }

    fprintf(out, "%s", asm_buffer);
    fclose(out);

    return 0;
}
