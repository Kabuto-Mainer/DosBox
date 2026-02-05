; Meow

.model tiny
.code
org 100h

start:
		mov ah, 09h	
		mov dx, offset Hello 
		int 21h			; Dos Fn - print string [21h]
	
		mov ax, 4C00h	
		int 21h			; Dos Fn - exit (al) [21h]
		
Hello:		db 0dh, 0ah, 'MEOW MEOW'
		db 0dh, 0ah, 'I am hungry$'

end		start

 
