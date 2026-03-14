.model tiny
.code

ascii_0		equ	30 
org 100h

start:
	mov ah, 09h
	mov dx, offset Init
	int 21h

	mov ah, 08h
	int 21h		; get char to al
	mov ah, 0

	cmp ax, 31h
	je .L1
	cmp ax, 32h
	je .L2
	cmp ax, 33h
	je .L3
	cmp ax, 34h
	je .L4
	jmp .L5
	
.L1:
	mov dx, offset Choose_1
	jmp .L6
.L2:
	mov dx, offset Choose_2
	jmp .L6
.L3:
	mov dx, offset Choose_3
	jmp .L6
.L4:
	mov dx, offset Choose_4
	jmp .L6
.L5:
	mov dx, offset Err_Input
	jmp .L6
.L6:
	mov ah, 09h
	mov al, 0h
	int 21h
	jmp .End_Pr
	
.End_Pr:
	mov ax, 4C00h  
	int 21h

Init:
	db 'Hello. Type number (1-4) and I tell you one phrase'
	db 0dh, 0ah, '$'

Choose_1:	
	db 'Poltorashka Meow$'

Choose_2:
	db 'Doom Slayer$'

Choose_3:
	db 'Macan - xyesos$'

Choose_4:
	db 'Thanks for zero BRS in cem$'

Err_Input:
	db "You don't type number in range (1-4), so I break$"

end 	start

