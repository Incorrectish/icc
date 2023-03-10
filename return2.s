.globl fib
fib: 
pushq %rbp
movq %rsp,%rbp
movq 16(%rbp),%rax
movq %rax,%rbx
movq $0,%rax
xchg %rbx,%rax
cmpq %rax,%rbx
movq $0,%rax
sete %al
cmpq $0,%rax
je .L0
movq $1,%rax
jmp .L1
.L0: 
movq 16(%rbp),%rax
movq %rax,%rbx
movq $1,%rax
xchg %rbx,%rax
cmpq %rax,%rbx
movq $0,%rax
sete %al
cmpq $0,%rax
movq $0,%rax
setne %al
.L1: 
cmpq $0,%rax
je .L2
movq 16(%rbp),%rax
popq %rbp
ret 
addq $0,%rsp
jmp .L3
.L2: 
pushq %rbx
movq 16(%rbp),%rax
movq %rax,%rbx
movq $1,%rax
xchg %rbx,%rax
subq %rbx,%rax
pushq %rax
call fib
addq $8,%rsp
popq %rbx
movq %rax,%rbx
pushq %rbx
movq 16(%rbp),%rax
movq %rax,%rbx
movq $2,%rax
xchg %rbx,%rax
subq %rbx,%rax
pushq %rax
call fib
addq $8,%rsp
popq %rbx
xchg %rbx,%rax
addq %rbx,%rax
popq %rbp
ret 
addq $0,%rsp
.L3: 
addq $0,%rsp
xorq %rax,%rax
popq %rbp
ret 
.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $10,%rax
movq %rax,-8(%rbp)
subq $8,%rsp
pushq %rbx
movq -8(%rbp),%rax
pushq %rax
call fib
addq $8,%rsp
popq %rbx
addq $8,%rsp
popq %rbp
ret 
