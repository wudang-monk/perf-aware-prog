#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof(arr[0]))
#define SWAP(x, y) do {                         \
        typeof(x) temp = (x);                   \
        (x) = (y);                              \
        (y) = temp;                             \
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
OP_ADD,
OP_SUB,
}math_type;

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

typedef enum {
JO,
JNO,
JB,
JNB,
JE,
JNE,
JBE,
JNBE,
JS,
JNS,
JP,
JNP,
JL,
JNL,
JLE,
JNLE
}jump_type;

typedef enum {
LOOPNZ,
LOOPZ,
LOOP,
JCXZ
}loop_type;

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
}Flags;

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
    i16 value;
    Flags flags;
}op_result;

typedef struct {
    u16 value;
    b8 memory;
    b8 wide;
}op_test;

typedef struct {
    reg data;
    union {
        struct {
            u8 reg : 3;
            u8 wide : 1;
            u8 bits_hi : 1;
            u8 segment : 1;
            u8 memory : 1;
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
    // TODO(Peter) bytes_used should be temporary until we have a better understanding of this
    u8 bytes_used;
    b8 wide;
}instr;

typedef union {
    struct {
        u8 w: 1;
        u8 d: 1;
        u8 opcode: 6;
    };
    u8 full;
}b1;

typedef struct {
    void *buffer;
    u32 index;
    u32 size;
}buffer;

typedef struct {
    union {
        struct {
            reg ax;
            reg cx;
            reg dx;
            reg bx;
            reg sp;
            reg bp;
            reg si;
            reg di;
        };
        reg registers[8];
    };
    union {
        struct {
            u16 es;
            u16 cs;
            u16 ss;
            u16 ds;
        };
        u16 segment_registers[4];
    };
    Flags flags;
    u16 ip;
}state;

typedef struct {
    u8 slot[1000];
    u8 size;
}memory;

memory MEMORY = {};
b8 SIMULATE = false;
state STATE = {};
state OLD_STATE = {};
char Flag_Names[] = {'O', 'D', 'I', 'T', 'S', 'Z', '\000', 'A', '\000', 'P', '\000', 'C'};
char *Byte_Registers[] = {"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"};
char *Word_Registers[] = {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di"};
char *Segment_Reg_Names[] = {"es", "cs", "ss", "ds"};
char *Rm_Mem_Table[] = {"bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx"};
char *Instr_Names[] = {"add", "or", "adc", "sbb", "and", "sub", "xor", "cmp", "mov"};
char *Jump_Names[] = {"jo", "jno", "jb", "jnb", "je", "jne", "jbe", "jnbe", "js", "jns", "jp", "jnp", "jl", "jnl", "jle", "jnle"};
char *Loop_Names[] = {"loopnz", "loopz", "loop", "jcxz"};

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
    buff->index = STATE.ip;
    u8 result = *(u8*)(buff->buffer + buff->index);
    STATE.ip += 1;
    return result;
}

static inline void SetIP(i8 disp) {
    STATE.ip += disp;
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

void PrintState(char *asm_string) {
    b8 show_diff = false;
    char *msg = (asm_string) ? asm_string : "\nFinal Registers";
    if (SIMULATE) {
        show_diff = true;
    }

    printf("%s:\n", msg);
    for (int i = 0; i < ARRAY_SIZE(STATE.registers); i++) {
        u16 value = STATE.registers[i].full;
        u16 old_value = (show_diff) ? OLD_STATE.registers[i].full : value;
        u16 diff = (value ^ old_value);
        if (diff) {
            printf("\t\t%s: 0x%hx -> 0x%hx\n", Word_Registers[i], old_value, value);
        } else if (value) {
            printf("\t\t%s: 0x%hx\n", Word_Registers[i], value);
        }
    }

    for (int i = 0; i < ARRAY_SIZE(STATE.segment_registers); i++) {
        u16 value = STATE.segment_registers[i];
        u16 old_value = (show_diff) ? OLD_STATE.segment_registers[i] : value;
        u16 diff = (value ^ old_value);
        if (diff) {
            printf("\t%s: 0x%x\n", Segment_Reg_Names[i], STATE.segment_registers[i]);
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

static inline i16 GetRegisterState(operand op) {
    i16 result = 0;
    if (op.wide) {
        if (op.segment) {
            result = STATE.segment_registers[op.reg];
        } else {
            result = STATE.registers[op.reg].full;
        }
    } else if (op. reg > 3) {
        result = STATE.registers[op.reg % 4].hi;
    } else {
        result = STATE.registers[op.reg].lo;
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

u16 FromMemory(op_test op) {
    u16 result = 0;
    u8 mem_lo = MEMORY.slot[op.value];
    result = (i8)mem_lo;
    u8 mem_hi = 0;
    if (op.wide) {
        mem_hi = MEMORY.slot[op.value + 1];
        result = (i16)mem_hi << 8 | mem_lo;
    }

    return result;
}


// does the given operaton on the numbers given and returns modified flags
// TODO make this function work for both 16bit/8bit numbers
op_result OpSetFlags(op_test dst, op_test src, instr inst) {
    op_result result = {};
    i16 dst16 = (dst.memory) ? FromMemory(dst) : dst.value;
    i16 src16 = (src.memory) ? FromMemory(src) : src.value;

    u8 dst_sign = (dst.wide) ? dst16 >> 15 : dst16 >> 7;
    u8 src_sign = (src.wide) ? src16 >> 15 : src16 >> 7;

    b8 aflag = false;
    b8 cflag = false;
    b8 oflag = false;

    i16 value = 0;
    switch (inst.name) {
        case ADD: {
        value = dst16 + src16;

        aflag = (value & 0x0F) < (dst16 & 0x0F);

        if (dst.wide) {
            cflag = (u32)((u16)dst16 + (u16)src16) > 0xFFFF;
            oflag = (dst_sign == src_sign) && (value >> 15 != dst_sign);
        } else {
            cflag = (u16)((u8)dst16 + (u8)src16) > 0xFF;
            oflag = (dst_sign == src_sign) && (value >> 7 != dst_sign);
        }

        break;
    }
    case SUB: {
        value = dst16 - src16;
        aflag = (value & 0xFF) > (dst16 & 0xFF);

        if (dst.wide) {
            cflag = (u32)((u16)dst16 - (u16)src16) > 0xFFFF;
            oflag = (dst_sign != src_sign) && (value >> 15 != dst_sign);

        } else {
            cflag = (u16)((u8)dst16 - (u8)src16) > 0xFF;
            oflag = (dst_sign != src_sign) && (value >> 7 != dst_sign);
        }

        break;
    }
    }

    b8 sflag = (dst.wide) ? value >> 15 : value >> 7;
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

/* void SimCommand(operand dst, operand src, instr inst, char *asm_string) { */
/*     if (!SIMULATE) { */
/*         return; */
/*     } */
/*     i16 dst_data = (dst.immediate) ? dst.data.full : GetRegisterState(dst); */
/*     i16 src_data = (src.immediate) ? src.data.full : GetRegisterState(src); */


/*     u8 dst_reg = (!dst.wide && dst.reg > 3) ? dst.reg % 4 : dst.reg; */
/*     Flags before_flags = STATE.flags; */
/*     char *reg_name = NULL; */

/*     char *instr_name = Instr_Names[inst.name]; */
/*     switch(inst.name) { */
/*         case ADD: { */
/*             STATE.flags = SetFlags(dst_data, src_data, ADD); */
/*             dst_data += src_data; */
/*             break; */
/*         } */
/*         case ADC: { */
/*             dst_data += src_data + STATE.flags.c; */
/*             break; */
/*         } */
/*         case SBB: { */
/*             dst_data -= src_data - STATE.flags.c; */
/*             break; */
/*         } */
/*         case OR: { */
/*             dst_data |= src_data; */
/*             break; */
/*         } */
/*         case AND: { */
/*             dst_data &= src_data; */
/*             break; */
/*         } */
/*         case SUB: { */
/*             STATE.flags = SetFlags(dst_data, src_data, OP_SUB); */
/*             dst_data -= src_data; */

/*             break; */
/*         } */
/*         case XOR: { */
/*             dst_data ^= src_data; */
/*             break; */
/*         } */
/*         case CMP: { */
/*             STATE.flags = SetFlags(dst_data, src_data, OP_SUB); */
/*             break; */
/*         } */
/*         case MOV: { */
/*             dst_data = src_data; */
/*             break; */
/*         } */
/*         default: { */
/*             printf("Emulator COMMAND: %d, NOT Handled\n", inst.name); */
/*         } */
/*     } */

/*     if (dst.segment) { */
/*         reg_name = Segment_Reg_Names[dst.reg]; */
/*         STATE.segment_registers[dst.reg] = dst_data; */
/*     } else if (dst.wide) { */
/*         reg_name = Word_Registers[dst.reg]; */
/*         STATE.registers[dst.reg].full = dst_data; */
/*     } else if (dst.reg > 3) { */
/*         reg_name = Byte_Registers[dst.reg % 4]; */
/*         STATE.registers[dst.reg % 4].hi = dst_data; */
/*     } else { */
/*         reg_name = Byte_Registers[dst.reg]; */
/*         STATE.registers[dst.reg].lo = dst_data; */
/*     } */

/*     PrintState(asm_string); */
/* } */


void DecodeRegInstruction(b1 byte, b2 byte2, buffer *code_buffer, buffer *asm_buffer) {
    instr instr = All_Instrs[byte.full];
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

        if (instr.type == I_IMM_REGMEM) {
            /* src.immediate = true; */
            src.data.lo =  PopBuffer(code_buffer);
            // Sign extension ops
            if (byte.w && instr.bytes_used == 5) {
                src.data.hi = PopBuffer(code_buffer);
            } else {
                src.data.full = (i16)src.data.lo;
            }

            sprintf(data_str, "%hd", src.data.full);
            src_string = data_str;
        }

        char asm_string[32] = {};
        if ((byte.full == 0x8C) || (byte.full == 0x8E)) {
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
        /* SimCommand(dst, src, instr, asm_string); */
        asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s\n", asm_string);
        return;
    }
}

void DecodeMemoryInstruction(b1 fst_byte, b2 snd_byte, buffer *code_buffer, buffer *asm_buffer) {
    instr instr = All_Instrs[fst_byte.full];
    char *instr_string = Instr_Names[instr.name];
    if (instr.name == ANY) {
        instr_string = Instr_Names[snd_byte.reg];
        instr.name = snd_byte.reg;
    }

    char *dst_string = Rm_Mem_Table[snd_byte.rm];
    char *src_string = (fst_byte.w) ? Word_Registers[snd_byte.reg] : Byte_Registers[snd_byte.reg];
    reg data = {};
    u8 mem_mode_16bit_disp = (snd_byte.mod == MOD_MEM && snd_byte.rm == 0b110) ? true : false;
    reg disp = {};

    if (snd_byte.mod == MOD_MEM_8) {
        i8 disp_lo = PopBuffer(code_buffer);
        disp.full = (i16)disp_lo;
    } else if ((mem_mode_16bit_disp) || snd_byte.mod == MOD_MEM_16) {
        u8 disp_lo = PopBuffer(code_buffer);
        u8 disp_hi = PopBuffer(code_buffer);
        disp.full = U8ToI16(disp_hi, disp_lo);
    }

    char mem_addr[32] = {};
    sprintf(mem_addr, "[%s + %hd]", dst_string, disp.full);
    if (snd_byte.mod == MOD_MEM) {
        sprintf(mem_addr, "[%s]", dst_string);
        if (mem_mode_16bit_disp) {
            sprintf(mem_addr, "[%hd]", disp.full);
        }
    }

    dst_string = mem_addr;

    // Immediate to Register/Mem
    op_result result = {};
    char data_str[16] = {};
    if (instr.type == I_IMM_REGMEM) {
        u8 data_lo = PopBuffer(code_buffer);
        op_test dst_op = {.memory = true, .value = disp.full};
        op_test src_op = { .value = (i8)data_lo};
        reg *test = (reg*)&MEMORY.slot[disp.full];
        u8 data_hi = 0;

        // TODO Look for better way to deal with sign extension of 8 bit values rather that have bytes_used for all instructions
        if (fst_byte.w && instr.bytes_used == 5) {
            data_hi = PopBuffer(code_buffer);
            src_op.value = (i16)data_hi << 8 | data_lo;
            src_op.wide = true;
        }

        if (instr.name == MOV) {
            if (src_op.wide) {
                MEMORY.slot[disp.full] = data_lo;
                MEMORY.slot[disp.full + 1] = data_hi;
            } else {
                MEMORY.slot[disp.full] = data_lo;
            }
        } else {
            result = OpSetFlags(dst_op, src_op, instr);
        }

        sprintf(data_str, "%s %hd", (fst_byte.w) ? "word" : "byte", src_op.value);
        src_string = data_str;
    } else if (fst_byte.d) {
        SWAP(dst_string, src_string);
    }
    char asm_string[32] = {};
    sprintf(asm_string, "%s %s, %s\n", instr_string, dst_string, src_string);
    asm_buffer->index += sprintf(asm_buffer->buffer + asm_buffer->index, "%s", asm_string);

    return;
}

// NOTE(Peter) Look into simplifying the Acc functions or moving it to the rest of the RegImm_RegMem function
void Acc(b1 byte, buffer *code_buffer, buffer *asm_buffer) {
    instr instr = All_Instrs[byte.full];
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
    while (STATE.ip < bytes_read) {
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
                if (byte2.mod == MOD_REG) {
                    DecodeRegInstruction(byte, byte2, &code_buffer, &asm_buffer);
                } else {
                    DecodeMemoryInstruction(byte, byte2, &code_buffer, &asm_buffer);
                }
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
                asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "%s\n", asm_string);

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

                PrintState(asm_string);
                break;
            }
            case I_MOV: {
                u8 wide = ((byte.full & 0x0F) >> 3);
                u8 reg = byte.full & 0b00000111;
                operand dst = {.wide = wide, .reg = reg};
                operand src = {.wide = wide, .reg = reg, .data.lo = PopBuffer(&code_buffer)};
                char *dst_name = NULL;
                if (dst.wide) {
                    src.data.hi = PopBuffer(&code_buffer);
                    dst_name = Word_Registers[dst.reg];
                } else {
                    dst_name = Byte_Registers[dst.reg];
                }

                char asm_string[32] = {};
                sprintf(asm_string, "mov %s, %hd", dst_name, src.data.full);
                /* SimCommand(dst, src, instr, asm_string); */
                asm_buffer.index += sprintf(asm_buffer.buffer + asm_buffer.index, "%s\n", asm_string);
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
        PrintState(NULL);
    }

    return 0;
}
