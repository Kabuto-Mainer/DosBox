.model tiny
.code

org 100h

Start:		
	mov ah, 09h
	mov dx, offset Hell
	int 21h

Hell:	db 'MEOW$'
	
	mov ax, 4C00h
	int 21h

end	Start:

