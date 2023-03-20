.globl main
main: 
push %rbp
mov %rsp,%rbp
movl $1,-4(%rbp)
subq $4,%rsp
.L0: 
movl -4(%rbp),%eax
imull $2,%eax
movl %eax,-4(%rbp)
add $0,%rsp
movl -4(%rbp),%eax
cmp $11,%rax
mov $0,%rax
setl %al
cmpl $0,%eax
je .L1
jmp .L0
.L1: 
movl -4(%rbp),%eax
# $4 being deallocated
add $4,%rsp
pop %rbp
ret 
