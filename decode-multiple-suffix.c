#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof(arr[0]))
#define SWAP(x, y) do { \
    typeof(x) temp = (x); \
    (x) = (y); \
    (y) = temp; \
} while (0)
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
OF_ADD,
OF_SUB
}of_type;

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

typedef union {
    struct {
        u16 c : 1;
        u16 unused3 : 1;
        u16 p : 1;
        u16 unused2 : 1;
        u16 a : 1;
        u16 unused1 : 1;
        u16 z : 1;
        u16 s : 1;
        u16 t : 1;
        u16 i : 1;
        u16 d : 1;
        u16 o : 1;
        u16 unused : 4;
    };
    u16 all;
}flags;

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
        i8 lo;
        i8 hi;
    };
    i16 full;
}reg;

typedef struct {
    reg data;
    union {
        struct {
            u8 reg : 3;
            u8 wide : 1;
            u8 immediate : 1;
            u8 segment : 1;
            u8 unused : 2;
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

flags FLAGS = {};
char Flag_Names[] = {'O', 'D', 'I', 'T', 'S', 'Z', '\000', 'A', '\000', 'P', '\000', 'C'};
char *Byte_Registers[] = {"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"};
char *Word_Registers[] = {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di"};
char *Segment_Reg_Names[] = {"es", "cs", "ss", "ds"};
char *Rm_Mem_Table[] = {"bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx"};
char *Instr_Names[] = {"add", "or", "adc", "sbb", "and", "sub", "xor", "cmp", "mov"};
char *Jump_Names[] = {"jo", "jno", "jb", "jnb", "je", "jne", "jbe", "jnbe", "js", "jns", "jp", "jnp", "jl", "jnl", "jle", "jnle"};
char *Loop_Names[] = {"loopnz", "loopz", "loop", "jcxz"};
reg Registers[8] = {};
u16 Segment_Registers[4] = {};
u16 IP = 0;
u16 IP_Last = 0;

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
    IP += 1;
    buff->index = IP;
    return result;
}

void PrintFlags(u16 flags_state, char *string) {
    u8 valid_flags[12] = {1,1,1,1,1,1,0,1,0,1,0,1};
    /* flags flags_copy = {.all = FLAGS.all}; */
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

void PrintRegisters() {
    printf("Registers\n");
    for (int i = 0; i < ARRAY_SIZE(Registers); i++) {
        u16 value = Registers[i].full;
        if (value) {
            printf("\t%s: 0x%x\n", Word_Registers[i], value);
        }
    }

    for (int i = 0; i < ARRAY_SIZE(Segment_Registers); i++) {
        u16 value = Segment_Registers[i];
        if (value) {
            printf("\t%s: 0x%x\n", Segment_Reg_Names[i], Segment_Registers[i]);
        }
    }
    // print IP register
    printf("\t%s: 0x%x\n", "ip", IP);
    char flags_string[10] = {};
    PrintFlags(FLAGS.all, flags_string);
    printf("Flags: %s\n", flags_string);
}

static inline i16 GetRegisterState(operand op) {
    i16 result = 0;
    if (op.wide) {
        if (op.segment) {
            result = Segment_Registers[op.reg];
        } else {
            result = Registers[op.reg].full;
        }
    } else if (op. reg > 3) {
        result = Registers[op.reg % 4].hi;
    } else {
        result = Registers[op.reg].lo;
    }

    return result;
}

static inline u8 Parity(u8 byte) {
    u8 result = 0;
    for (int i = 0; i < 8; i++) {
        result += byte & 0b1;
        byte >>= 1;
    }
    return (result % 2);
}

static inline u8 OF(u16 num_a, u16 num_b, of_type) {
    u8 result = 0;
    u8 sign_a = (num_a >> 15);
    u8 sign_b = (num_b >> 15);
    u8 sign_a_b_equal = !(sign_a ^ sign_b);
    if (OF_ADD) {
        u8 sign_sum = ((num_a + num_b) >> 15);
        if (sign_a_b_equal) {
            result = sign_sum != sign_a;
        }
    } else {
        u8 sign_sub = ((num_a - num_b) >> 15);
        u8 sign_sub_b_equal = !(sign_sub ^ sign_b);
        if (sign_sub_b_equal) {
            result = sign_a != sign_sub;
        }
    }
    /* printf("%s: %d, %d -> %d   OF: %s\n", addition ? "ADD" : "SUB", num_a, num_b, (num_a + num_b), result ? "true" : "false"); */
    return result;
}

void Command(operand dst, operand src, instr inst, char *asm_string) {
    i16 dst_data = (dst.immediate) ? dst.data.full : GetRegisterState(dst);
    i16 src_data = (src.immediate) ? src.data.full : GetRegisterState(src);

    u8 dst_reg = (!dst.wide && dst.reg > 3) ? dst.reg % 4 : dst.reg;
    u16 before = Registers[dst_reg].full;
    flags before_flags = FLAGS;
    char *reg_name = NULL;
    if (dst.segment) {
        before = Segment_Registers[dst.reg];
    }

    char *instr_name = Instr_Names[inst.name];
    switch(inst.name) {
        case ADD: {
            i16 dst_before = dst_data;
            FLAGS.o = OF(dst_before, src_data, OF_ADD);
            dst_data += src_data;
            FLAGS.c = ((dst_before & 0xFF00) + (src_data & 0xFF00)) > (dst_data & 0xFF00) ? true : false;
            FLAGS.z = (dst_data == 0) ? true : false;
            FLAGS.s = (dst_data & 0x8000) ? true : false;
            FLAGS.p = (Parity(dst_data)) ? false : true;
            FLAGS.a = ((dst_data & 0x000F) < (dst_before & 0x000F)) ? true : false;
            break;
        }
        case ADC: {
            dst_data += src_data + FLAGS.c;
            break;
        }
        case SBB: {
            dst_data -= src_data - FLAGS.c;
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
            i16 dst_before = dst_data;
            FLAGS.o = OF(dst_before, src_data, OF_SUB);
            dst_data -= src_data;
            FLAGS.c = ((dst_before & 0xFF00) - (src_data & 0xFF00)) < (dst_data & 0xFF00) ? true : false;
            FLAGS.z = (dst_data == 0) ? true : false;
            FLAGS.s = (dst_data & 0x8000) ? true : false;
            FLAGS.p = (Parity(dst_data)) ? false : true;
            FLAGS.a = ((dst_data & 0x000F) > (dst_before & 0x00F0) >> 4) ? true : false;
            break;
        }
        case XOR: {
            dst_data ^= src_data;
            break;
        }
        case CMP: {
            i16 dst_before = dst_data;
            FLAGS.s = ((dst_data - src_data) & 0x8000) ? true : false;
            FLAGS.z = ((dst_data - src_data) == 0) ? true : false;
            FLAGS.s = ((dst_data - src_data) & 0x8000) ? true : false;
            FLAGS.o = OF(dst_before, src_data, OF_SUB);
            FLAGS.p = (Parity((dst_data - src_data))) ? false : true;
            FLAGS.a = ((dst_data & 0x000F) > (dst_before & 0x000F) >> 4) ? true : false;
            break;
        }
        case MOV: {
            dst_data = src_data;
            break;
        }
        default: {
            printf("Emulator COMMAND: %d, NOT Handled\n", inst.name);
        }
    }

    if (dst.segment) {
        reg_name = Segment_Reg_Names[dst.reg];
        Segment_Registers[dst.reg] = dst_data;
    } else if (dst.wide) {
        reg_name = Word_Registers[dst.reg];
        Registers[dst.reg].full = dst_data;
    } else if (dst.reg > 3) {
        reg_name = Byte_Registers[dst.reg % 4];
        Registers[dst.reg % 4].hi = dst_data;
    } else {
        reg_name = Byte_Registers[dst.reg];
        Registers[dst.reg].lo = dst_data;
    }

    printf("%s ; ", asm_string);
    printf("%s: 0x%x -> 0x%x ip:0x%x -> 0x%x ", reg_name, before, Registers[dst_reg].full, IP_Last, IP);
    flags flag_diff = {.all = before_flags.all ^ FLAGS.all};
    if (flag_diff.all) {
        char flags_before[10] = {};
        char flags_after[10] = {};
        PrintFlags(before_flags.all, flags_before);
        PrintFlags(FLAGS.all, flags_after);
        printf("Flags: %s -> %s\n", flags_before, flags_after);
    } else {
        printf("\n");
    }
}

void RegIMM_RegMem(b1 byte, buffer *code_buffer, buffer *asm_buffer, inst_type type) {
    b2 byte2 = {.byte = PopBuffer(code_buffer)};
    instr instr = All_Instrs[byte.byte];
    char *instr_string = Instr_Names[instr.name];
    if (instr.name == ANY) {
        instr_string = Instr_Names[byte2.reg];
        instr.name = byte2.reg;
    }

    char *dst_string = Rm_Mem_Table[byte2.rm];
    char *src_string = (byte.w) ? Word_Registers[byte2.reg] : Byte_Registers[byte2.reg];
    reg data = {};

    // Register to register
    if (byte2.mod == MOD_REG) {
        char data_str[32] = {};
        operand dst = {.reg = byte2.rm, .wide = byte.w};
        operand src = {.reg = byte2.reg, .wide = byte.w};
        dst_string = (byte.w) ? Word_Registers[byte2.rm] : Byte_Registers[byte2.rm];

        if (type == I_IMM_REGMEM) {
            u8 data_lo = PopBuffer(code_buffer);
            data.full = (i8)data_lo;
            if (byte.w && instr.bytes_used == 5) {
                u8 data_hi = PopBuffer(code_buffer);
                data.full = U8ToI16(data_hi, data_lo);
            }

            src.data.full = data.full;
            src.immediate = true;
            sprintf(data_str, "%hd", data.full);
            src_string = data_str;
        }

        char asm_string[32] = {};
        if ((byte.byte == 0x8C) || (byte.byte == 0x8E)) {
            // Segment mov
            dst = (operand){.reg = byte2.rm, .wide = true};
            src = (operand){.reg = byte2.reg, .wide = true, .segment = true};
            dst_string = Word_Registers[dst.reg];
            src_string = Segment_Reg_Names[src.reg];
            if (byte.d) {
                SWAP(src, dst);
                SWAP(src_string, dst_string);
            }
        }

        sprintf(asm_string, "%s %s, %s", instr_string, dst_string, src_string);
        Command(dst, src, instr, asm_string);
        asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s\n", asm_string);
        return;
    }

    // Memory displacement
    u8 mem_mode_16bit_disp = (byte2.mod == MOD_MEM && byte2.rm == 0b110) ? 1 : 0;
    reg disp = {};

    if (byte2.mod == MOD_MEM_8) {
        i8 disp_lo = PopBuffer(code_buffer);
        disp.full = (i16)disp_lo;
    } else if ((mem_mode_16bit_disp) || byte2.mod == MOD_MEM_16) {
        u8 disp_lo = PopBuffer(code_buffer);
        u8 disp_hi = PopBuffer(code_buffer);
        disp.full = U8ToI16(disp_hi, disp_lo);
    }

    char mem_addr[32] = {};
    sprintf(mem_addr, "[%s + %hd]", dst_string, disp.full);
    if (byte2.mod == MOD_MEM) {
        sprintf(mem_addr, "[%s]", dst_string);
        if (mem_mode_16bit_disp) {
            sprintf(mem_addr, "[%hd]", disp.full);
        }
    }

    dst_string = mem_addr;

    // Immediate to Register/Mem
    if (type == I_IMM_REGMEM) {
        u8 data_lo = PopBuffer(code_buffer);
        data.full = (i8)data_lo;

        if (byte.w && instr.bytes_used == 5) {
            u8 data_hi = PopBuffer(code_buffer);
            data.full = U8ToI16(data_hi, data_lo);
        }

        char data_str[16] = {};
        sprintf(data_str, "%s %hd", (byte.w) ? "word" : "byte", data.full);
        src_string = data_str;
    } else if (byte.d) {
        SWAP(dst_string, src_string);
    }

    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s %s, %s\n", instr_string, dst_string, src_string);

    return;
}
// NOTE(Peter) Look into simplifying the Acc functions or moving it to the rest of the RegImm_RegMem function
void Acc(b1 byte, buffer *code_buffer, buffer *asm_buffer) {
    instr instr = All_Instrs[byte.byte];
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
    
    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s %s, %s\n", instr_name, dst, src);
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
        IP_Last = IP;
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
                u8 wide = (byte.byte >> 3) & 0b00001;
                u8 reg = byte.byte & 0b00000111;
                operand dst = {.wide = wide, .reg = reg};
                operand src = {.wide = wide, .reg = reg};
                char *dst_name = (dst.wide) ? Word_Registers[dst.reg] : Byte_Registers[dst.reg];
                u8 data_lo = PopBuffer(&code_buffer);
                i16 data = (i8)data_lo;

                if (dst.wide) {
                    u8 data_hi = PopBuffer(&code_buffer);
                    data = U8ToI16(data_hi, data_lo);
                }

                src.data.full = data;
                src.immediate = true;
                char asm_string[32] = {};
                sprintf(asm_string, "mov %s, %hd", dst_name, data);
                Command(dst, src, instr, asm_string);
                asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "%s\n", asm_string);
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
