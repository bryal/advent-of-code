# Advent of Code, Day 10, Part 2
#   in RISC-V 64 (IM) Assembly!

# Various notes:
#
# RISC-V assembly references:
# - https://github.com/riscv-non-isa/riscv-asm-manual/blob/master/riscv-asm.md
# - https://michaeljclark.github.io/asm.html
# - https://michaeljclark.github.io/isa
#
# Syscalls.
# Arguments to Linux syscalls are passed in registers a0 - a5.
# The syscall is specified in register a7, and called with instruction `ecall`.
# List of Linux syscalls and their integer identifier values:
#   https://jborza.com/post/2021-05-11-riscv-linux-syscalls/
#
# Registers.
# - aN regs are for function args & temporaries. CALLER saved.
# - tN regs are for temporaries. CALLER saved.
# - s0 reg is for frame pointer. Callee saved.
# - sN regs are for variables. Callee saved.
# - ra reg is for return address. CALLER saved.
# - and a bunch more, like sp for the stack pointer.

.equ    BUF_SIZE, 1024
.equ    SYSCALL_READ, 63
.equ    SYSCALL_WRITE, 64
.equ    SYSCALL_EXIT, 93
.equ    OPCODE_NOOP, 0
.equ    OPCODE_ADDX, 1
.equ    READ_ERROR, -1
.equ    SPRITE_RADIUS, 1
.equ    TRUE, 1
.equ    FALSE, 0

.section .data
input:          .zero BUF_SIZE
input_len:      .dword 0

.section .rodata
s_newline:      .string "\n"
s_hash:         .string "#"
s_dot:          .string "."
s_minus:        .string "-"
s_0:            .string "0"
s_9:            .string "9"
s_noop:         .string "noop"

.section .text
.global _start      # Provide program starting address to linker

_start:
        # The linker assumes that if this symbol is defined, then the
        # gp register contains that value, which it can then use to
        # relax accesses to global symbols within that 12-bit range.
        .option push
        .option norelax
        la gp, __global_pointer$
        .option pop

        # Read problem input from stdin to buffer `input` in .data section
        mv      a0, zero        # fd, 0 = stdin
        la      a1, input
        li      a2, BUF_SIZE
        li      a7, SYSCALL_READ
        ecall
        la      t0, input_len
        sw      a0, 0(t0)       # input_len = read(0, input, BUF_SIZE)

        # global register variables
        li      s1, 0           # Cycle
        li      s2, 1           # X
        li      s3, 0           # Index in input

main_loop:
        jal     ra, read_instr  # => a0 <- opcode, a1 <- arg0, ...

case_noop:
        li      t0, OPCODE_NOOP
        bne     a0, t0, case_addx # if opcode is noop
        jal     ra, cycle       # noop takes one cycle
        j       main_loop

case_addx:
        li      t0, OPCODE_ADDX
        bne     a0, t0, end     # if opcode is addx
        mv      s4, a1
        jal     ra, cycle       # addx takes 2 cycles
        jal     ra, cycle
        add     s2, s2, s4       # X += addx arg
        j       main_loop

end:    li      a0, 0
        li      a7, SYSCALL_EXIT
        ecall                   # exit program with code 0


# Subroutine to execute one cycle for the CRT to draw
cycle:
        addi    sp, sp, -8
        sd      ra, 0(sp)       # push return address
        
        li      t0, 40          # line width
        rem     s5, s1, t0      # column_index
        bnez    s5, 1f          # If column_index is 0
        la      a0, s_newline
        jal     ra, print_1     # Print newline
1:      sub     a0, s5, s2      # column_index - x
        jal     ra, abs         # distance from sprite centre
        mv      t0, a0
        li      t1, SPRITE_RADIUS
        la      a0, s_hash      # by default, select char '#' (sprite visible)
        ble     t0, t1, 2f      # if distance > SPRITE_RADIUS
        la      a0, s_dot       # sprite is not visible, select char '.'
2:      jal     ra, print_1     # in the current column, print either '#' or '.'
        addi    s1, s1, 1       # cycle += 1
        
        ld      ra, 0(sp)       # pop return address
        addi    sp, sp, 8
        ret


# Subroutine to parse an instruction line from the input, returning
# the opcode and its arguments. At end of input, returns a negative
# opcode.
read_instr:
        addi    sp, sp, -8
        sd      ra, 0(sp)       # push return address

        ld      t0, input_len
        addi    t0, t0, -4      # end minus the shortest instr (noop)
        bleu    s3, t0, 1f      # if there are no more instrs
        li      a0, READ_ERROR
        j       read_instr_end  # return READ_ERROR as the opcode, indicating end of input
        
1:      la      a0, s_noop      # "noop"
        la      a1, input
        add     a1, a1, s3      # &input[i]
        li      a2, 4           # len("noop")
        jal     ra, is_prefix   # is_prefix("noop", &input[i], len("noop"))
        addi    s3, s3, 5       # increment i by len("noop\n") or len("addx ")
        beqz    a0, 2f          # if current line starts with "noop"
        li      a0, OPCODE_NOOP
        j       read_instr_end

2:      # if it's not noop, assume it's an addx
        jal     ra, read_signed # read the arg to addx
        addi    s3, s3, 1       # i += len("\n")
        mv      a1, a0          # a1 <- first arg of instr
        li      a0, OPCODE_ADDX

read_instr_end:
        ld      ra, 0(sp)       # pop return address
        addi    sp, sp, 8
        ret


# Subroutine that returns whether the first string argument is a
# prefix of the second.
is_prefix: # (prefix, string, count) -> bool
        bgtz    a2, 1f          # if count == 0
        li      a0, TRUE
        ret                     # no chars differed, return true
1:      addi    a2, a2, -1      # count -= 1
        lb      t0, 0(a0)       
        lb      t1, 0(a1)
        addi    a0, a0, 1
        addi    a1, a1, 1
        beq     t0, t1, is_prefix # if chars differ
        li      a0, FALSE
        ret                     # return false
        

# Subroutine to parse a signed integer from the input
read_signed:
        li      a0, 0           # n = 0
        la      t0, input
        add     t1, t0, s3
        lb      t1, 0(t1)       # input[i]
        lb      t2, s_minus     # '-'
        li      t3, FALSE
        bne     t2, t1, 1f      # if input[i] == '-'
        li      t3, TRUE        # t3 <- int is signed
        addi    s3, s3, 1       # i += 1
        
1:      ld      t2, input_len
        blt     s3, t2, 2f      # if end of input
        j       5f
2:      add     t1, t0, s3
        lb      t1, 0(t1)       # input[i]
        lb      t2, s_0         # '0'
        bge     t1, t2, 3f      # if input[i] < '0'
        j       5f
3:      lb      t2, s_9         # '9'
        ble     t1, t2, 4f      # if input[i] > '9'
        j       5f
4:      li      a1, 10
        mul     a0, a0, a1      # n *= 10
        lb      t2, s_0
        sub     t1, t1, t2      # digit = input[i] - '0'
        add     a0, a0, t1      # n += digit
        addi    s3, s3, 1       # i += 1
        j       1b
        
5:      beqz    t3, 6f
        neg     a0, a0
6:      ret
        

# Subroutine to compute the absolute value of a signed integer
abs:
        bgez    a0, 1f
        neg     a0, a0
1:      ret


# Subroutine to print a single character of the given string to stdout
print_1:
        addi    sp, sp, -8
        sd      ra, 0(sp)       # push return address
        li      a1, 1
        jal     ra, print_n
        ld      ra, 0(sp)       # pop return address
        addi    sp, sp, 8
        ret


# Subroutine to print `count` characters of the given string to stdout
print_n: # (str, count)
        mv      a2, a1
        mv      a1, a0
        li      a0, 1           # fd stdout
        li      a7, SYSCALL_WRITE # syscall write
        ecall
        ret
