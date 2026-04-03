
.model tiny
.code
org 100h

start:
		mov ax, 0b800h		; adr video data
		mov es, ax		
		xor bx, bx
		
		mov byte ptr es:[bx], 03h
		mov byte ptr es:[bx + 1], 4eh or 80h 	; merge consts

		mov ax, 4c00h
		int 21h

end		start

