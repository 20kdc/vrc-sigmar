.section .stack, "aw", @nobits

.globl _stack_start
.globl _stack_end

.align 4
_stack_end:
.space 0x4000
_stack_start:
