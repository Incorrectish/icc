.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $5,%rax
pushq %rax
call foo
addq $8,%rsp
# $0 being returned
addq $0,%rsp
popq %rbp
ret 
.globl foo
foo: 
pushq %rbp
movq %rsp,%rbp
# start of if
movq 16(%rbp),%rax
pushq %rax
movq $0,%rax
movq %rax,%rcx
popq %rax
cmpq %rcx,%rax
movq $0,%rax
setle %al
cmpq $0,%rax
je .L0
# start of conditional body
movq 16(%rbp),%rax
# $0 being returned
addq $0,%rsp
popq %rbp
ret 
addq $0,%rsp
.L0: 
movq 16(%rbp),%rax
pushq %rax
movq 16(%rbp),%rax
pushq %rax
movq $1,%rax
movq %rax,%rcx
popq %rax
subq %rcx,%rax
pushq %rax
call bar
addq $8,%rsp
movq %rax,%rcx
popq %rax
addq %rcx,%rax
# $0 being returned
addq $0,%rsp
popq %rbp
ret 
.globl bar
bar: 
pushq %rbp
movq %rsp,%rbp
# start of if
movq 16(%rbp),%rax
pushq %rax
movq $0,%rax
movq %rax,%rcx
popq %rax
cmpq %rcx,%rax
movq $0,%rax
setle %al
cmpq $0,%rax
je .L1
# start of conditional body
movq 16(%rbp),%rax
# $0 being returned
addq $0,%rsp
popq %rbp
ret 
addq $0,%rsp
.L1: 
movq 16(%rbp),%rax
pushq %rax
movq 16(%rbp),%rax
pushq %rax
movq $2,%rax
movq %rax,%rcx
popq %rax
cqo 
idivq %rcx
pushq %rax
call bar
addq $8,%rsp
movq %rax,%rcx
popq %rax
addq %rcx,%rax
# $0 being returned
addq $0,%rsp
popq %rbp
ret 
