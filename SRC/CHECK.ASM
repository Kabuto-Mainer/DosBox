
.model tiny
.code
org 100h

Start:	
		mov ah, 02h		; number of dos fn 
		mov dx, 010ch			
		int 21h			; call dos fn
	
		mov ax, 4C00h		; dos fn exit (4C - exit, 00 - value exit)
		int 21h			; call dos fn

Hell:		db 02h, '$'

end		Start

Hell:		db 02h, '$' 
		
