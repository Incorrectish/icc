.globl main
main: 
pushq %rbp
movq %rsp,%rbp
pushq $1
movq $5,%rbx
popq %rax
cqo 
idivq %rbx
movq %rdx,%rax
addq $0,%rsp
popq %rbp
ret 
