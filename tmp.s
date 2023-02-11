main: 
.globl main
movq $4, $2, %rax
popq %rbx
cltd 
idiv %rbx
movq %rax, %rax
ret 

