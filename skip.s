.data
.text
.global main
main:
	push {fp, lr}
	push {r8, r10, r12}
	mov fp, sp
	mov r0, #0
	pop {r8, r10, r12}
	pop {fp, pc}
