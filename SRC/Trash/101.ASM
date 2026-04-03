.model tiny
;==============================================================================
dataseg
		X_cord	dw	10
		Y_cord	dw	10
		Len		dw	10
		Hight	dw	5

		Buffer	dw	0
		;Text				Amount Str     String		Len Text
		Text	dw	2	dup (1, 	offset STD_STR, 	1)

		;Symbols			 Line   Column	Fon		Mask Text
		Chars 	dw	5	dup	(4cc4h, 4cb3h, 	4c03h,	4c00h)
		Tangles	dw	4	dup (4cdah, 4cbfh, 	4cc0h, 	4cd9h)
.code
Null_Char	equ		'$'
Locals @@
org 100h

;==============================================================================
start:
		cld				;set track

		xor dx, dx
		mov bx, 80h
		mov dl, [bx]
		add dx, 81h
		mov al, Null_Char
		push cs
		pop es
		mov di, 81h

		call ctr_dt_text

		mov cx, 0b800h
		mov es, cx		;init video data address

;--------------------------------------------------------------------------------

		push bx

;--------------------------------------------------------------------------------

	;find left hight tangle
		xor dx, dx
		mov ax, Y_cord
		mov bl, 160
		mul bl
		add dx, ax
		mov ax, X_cord
		mov bl, 2
		mul bl
		add dx, ax

;--------------------------------------------------------------------------------

		pop cx
		mov [Buffer], cx

;--------------------------------------------------------------------------------

	;fill fon
		mov ax, [Chars+4]
		mov cx, Hight
		mov bx, Len
		mov di, dx
		call fill_fon

;--------------------------------------------------------------------------------

		mov cx, [Buffer]
		mov [Buffer], dx  	;need to be saved
		call prt_dt_text

		mov dx, [Buffer]

;--------------------------------------------------------------------------------

	;draw lines
		mov ax, [Chars+0]
		mov cx, Len
		push cx
		mov di, dx
		push di

		call draw_line
		mov bx, di

		pop di
		pop cx
		push bx					;we need to save it

		mov bx, Hight
		push ax
		mov ax, 160
		mul bl
		add di, ax
		pop ax

		call draw_line

		mov ax, [Chars+2]
		mov di, dx
		mov cx, Hight
		call draw_column

		pop di
		sub di, 2
		push di

		mov cx, Hight
		call draw_column

;--------------------------------------------------------------------------------

	;set tangles
		mov di, dx
		mov ax, [Tangles+0]
		mov es:[di], ax

		push ax

		mov cx, Len
		sub cx, 1
		mov ax, 2
		mul cl
		push ax
		add di, ax
		mov cx, [Tangles+2]
		mov es:[di], cx

		mov di, dx
		mov bx, Hight
		mov al, 160
		mul bl
		add di, ax
		mov cx, [Tangles+4]
		mov es:[di], cx

		pop bx
		add di, bx
		mov cx, [Tangles+6]
		mov es:[di], cx


;--------------------------------------------------------------------------------

;--------------------------------------------------------------------------------

		mov ax, 4c00h
		int 21h

;==============================================================================
;----------------------------------------
;Draw Column with symbol `ax`
;Entry:
;		ax - symbol
;		es:[di] - start memory address
;		cx - len column
;Return:
;		-
;Destroy:
;		bx, di, cx
;----------------------------------------
draw_column 	PROC
@@next:
		mov bx, 160
		add di, bx
		mov word ptr es:[di], ax

		loop @@next
		ret

draw_column		ENDP

;----------------------------------------
;Draw line by `ax`
;Entry:
;		ax - symbol
;		cx - len
;		es:[di] - stat video memory
;Return:
;		-
;Destroy:
;		cx, di
;----------------------------------------
draw_line		PROC
		rep stosw
		ret
draw_line		ENDP


;----------------------------------------
;Fill fon of place
;Entry:
;		ax - symbol
;		bx - hight
;		cx - len
;		es:[di] - start memory address
;Return:
;		-
;Destroy:
;		bx, cx, di
;----------------------------------------
fill_fon		PROC

@@next:
		xchg bx, cx
		push di
		push cx
		rep stosw
		pop cx
		pop di
		add di, 160
		xchg bx, cx

		loop @@next
		ret

fill_fon		ENDP




print_text		PROC
		;al - color fone
		;cx - len
		;es:[bx] - start address
		;di - string address

		mov ah, al
@@next:
		mov al, byte ptr [di]
		mov es:[bx], ax
		inc di
		inc bx
		inc bx

		loop @@next
		ret

print_text		ENDP

;----------------------------------------
;Find len fo string
;Entry:
;		al - symbol of end
;		es:[di] - start string address
;Return:
;		es:[di] - address of symbol after end
;		cx - len string
;Destroy:
;		cx
;----------------------------------------
Strlen			PROC
		xor cx, cx
		sub cl, 1		;cx = 00FFh

		repne scasb
		neg cl
		sub cx, 1
		ret

Strlen			ENDP



ctr_dt_text		PROC
		;es:[di] - start address
		;bx in this proc - counter of strings
		;dx - max address
		;al - end symbol

;return:
;(addresses are just examples)
;0100h	di 	- 	address (1)
;0098h	cx	- 	len (1)
;0096h	di	-	address (2)
;0094h	cx	-	len	(2)

		pop si
		xor bx, bx

@@next:
		add di, 1
		push di
		call Strlen
		push ax
		sub di, 1
		mov ax, [Chars+6]
		mov [di], ax
		add di, 1
		pop ax
		cmp di, dx
		ja @@end

		cmp cx, [Len]
		ja @@inc_1

@@inc_2:

		inc bx
		push cx
		jmp @@next

@@end:
		pop di
		mov ax, [Len]
		add ax, 3
		mov [Len], ax

		push bx
		add bx, 3
		mov [Hight], bx
		pop bx

		push si
		ret

@@inc_1:
		mov [Len], cx
		jmp @@inc_2

ctr_dt_text		ENDP




prt_dt_text		PROC
		;dx - left hight tangle
		;cx - amount strings

		pop si
		add dx, 160
@@next:
		add dx, 160
		mov ax, Len
		pop bx			;get len string
		push bx
		sub ax, bx
		sar ax, 1		;ax=ax/2
		mov bx, 2
		mul bl
		pop bx

		pop di			;get string address
		push dx
		add dx, ax		;dx - start address
		push cx			;save cx
		push dx
		mov cx, bx
		mov bx, [Chars+6]
		mov al, bh
		pop bx
		call print_text

		pop cx
		pop dx

		loop @@next
		push si
		ret

prt_dt_text		ENDP


;==============================================================================

;==============================================================================
;Segment const
;==============================================================================

STD_STR		db	'Poltorashka$'



end 		start


