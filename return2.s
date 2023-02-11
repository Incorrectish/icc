main: 
.globl main
pushl $2
pushl $2
popl %eax
addl %eax, (%esp)
pushl $4
popl %eax
imul %eax, (%esp)
pushl $8
popl %eax
addl %eax, (%esp)
popl %eax
ret 
