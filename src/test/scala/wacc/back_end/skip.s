.data
@ length of .L.str0
	.word 12
.L.str0:
	.asciz "Hello World!"
.text
.global main
main:
push {fp, lr}
push {r8, r10, r12}
	mov fp, sp
	ldr r8, =.L.str0
	push {r8}
	pop {r8}
	mov r8, r8
	mov r0, r8
	@ statement primitives do not return results (but will clobber r0/rax)
	bl _prints
	mov r0, #0
	pop {r8, r10, r12}
	pop {fp, pc}

.data
@ length of .L._prints_str0
	.word 4
.L._prints_str0:
	.asciz "%.*s"
.text
_prints:
	push {lr}
	mov r2, r0
	ldr r1, [r0, #-4]
	ldr r0, =.L._prints_str0
	bl printf
	mov r0, #0
	bl fflush
	pop {pc}