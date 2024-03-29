#ifndef SIM86_H
#define SIM86_H

#include <stdint.h>


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
I_LOOP = 6,
I_PUSH = 7,
I_POP = 8,
I_RET = 9,
I_INC = 10,
}inst_type;

typedef enum {
J_JO,
J_JNO,
J_JB,
J_JNB,
J_JE,
J_JNE,
J_JBE,
J_JNBE,
J_JS,
J_JNS,
J_JP,
J_JNP,
J_JL,
J_JNL,
J_JLE,
J_JNLE
}jmp_type;

typedef enum {
L_LOOPNZ,
L_LOOPZ,
L_LOOP,
L_JCXZ
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
VAR = 9,
JMP = 10,
LOOP = 11,
PUSH = 12,
POP = 13,
TEST = 14,
RET = 15,
INC = 16,
DEC = 17,
LOG_1 = 18,
}op_name;

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
    reg data;
    reg* src_reg;
    union {
        struct {
            u8 memory : 1;
            u8 wide : 1;
            u8 segment : 1;
            u8 hi : 1;
            u8 unused : 4;
        };
        u8 byte;
    };
}operand;

typedef struct {
    operand dst;
    operand src;
}decode_result;

typedef union {
    struct {
        u8 rm: 3;
        u8 reg: 3;
        mod mod: 2;
    };
    u8 byte;
}b2;

typedef union {
    struct {
        u8 w: 1;
        u8 d: 1;
        u8 opcode: 6;
    };
    u8 full;
}b1;

typedef struct {
    u8 byte1;
    op_name name;
    inst_type type;
}instr;


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
            reg es;
            reg cs;
            reg ss;
            reg ds;
        };
        reg segment_registers[4];
    };
    Flags flags;
    u16 ip;
}state;

typedef struct {
    u8 slot[65536];
    u8 size;
}memory;

#endif //SIM86_H
