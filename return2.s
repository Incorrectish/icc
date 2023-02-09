.globl main
main:
movl $0, %eax
not %eax
neg %eax
xorl $1, %eax
ret