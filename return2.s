.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq -8(%rsp),%rax
popq %rbp
ret 
