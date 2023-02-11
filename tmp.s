.globl main
main:
pushl $2
pushl $2
popl %eax
popl %ebx
addl %ebx, %eax
pushl %eax
pushl $4
popl %eax
popl %ebx
imul %ebx, %eax
pushl %eax
pushl $8
popl %eax
popl %ebx
addl %ebx, %eax
pushl %eax
popl %eax
ret
