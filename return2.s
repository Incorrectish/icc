.globl main
main: 
push %rbp
mov %rsp,%rbp
movl $0,-4(%rbp)
subq $4,%rsp
# start of if
movl -4(%rbp),%eax
cmpl $0,%eax
je .L0
# start of conditional body
movl $2,-8(%rbp)
subq $4,%rsp
movl -8(%rbp),%eax
# $8 being deallocated
add $8,%rsp
pop %rbp
ret 
add $4,%rsp
# start of else
jmp .L1
.L0: 
# start of else body
movl $3,-8(%rbp)
subq $4,%rsp
# start of if
movl -4(%rbp),%eax
subq $4,%rsp
movl %eax,(%rsp)
movl -8(%rbp),%eax
movl %eax,%ecx
movl (%rsp),%eax
addq $4,%rsp
cmpl %ecx,%eax
movq $0,%rax
setl %al
cmpl $0,%eax
je .L2
# start of conditional body
movl $4,%eax
# $8 being deallocated
add $8,%rsp
pop %rbp
ret 
add $0,%rsp
# start of else
jmp .L3
.L2: 
# start of else body
movl $5,%eax
# $8 being deallocated
add $8,%rsp
pop %rbp
ret 
add $0,%rsp
.L3: 
add $4,%rsp
.L1: 
movl -4(%rbp),%eax
# $4 being deallocated
add $4,%rsp
pop %rbp
ret 
