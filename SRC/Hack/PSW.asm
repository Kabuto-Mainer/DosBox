.model tiny
.386
Locals @@

CHAR_END_INPUT  equ     0dh
END_CHAR        equ     0h

.code
org 100h

start:
        jmp real_start

INPUT_PASSWORD        db   '00000000' ; 1   dup(0, 0, 0, 0, 0, 0, 0, 0)
REAL_PASSWORD         db   '00000000'

real_start:
        push cs
        pop es

        mov di, offset REAL_PASSWORD
        mov cx, 8
        call get_hash
        push ax

        mov di, offset STANDARD_INIT_PHRASE
        call print_std_out

        mov di, offset INPUT_PASSWORD
        call get_std_inp

        push cs
        pop es

        mov di, offset INPUT_PASSWORD
        mov cx, 8
        call get_hash

        pop bx
        cmp ax, bx

        je not_success
        mov di, offset STANDARD_SUCCESS
        call print_std_out

        mov ax, 4C00h
        int 21h

not_success:
        mov di, offset STANDARD_FAILED
        call print_std_out

        mov ax, 4C00h
        int 21h

;------------------------------------------------------------------------------
; get_std_inp
;
; get text from standard input
; Entry:
;       es:[di] - start address
; Destroy:
;       di
;------------------------------------------------------------------------------
get_std_inp     PROC
        push ax

@@next:
        mov ah, 01h
        int 21h

        cmp al, CHAR_END_INPUT
        je @@end

        mov byte ptr es:[di], al
        inc di
        jmp @@next

@@end:
        pop ax
        ret
                ENDP

;------------------------------------------------------------------------------
; print_std_inp
;
; print text to standard output
; Entry:
;       es:[di] - start text
; Destroy:
;       di
;------------------------------------------------------------------------------
print_std_out   PROC
        push ax dx
        mov ah, 06h     ;dos output func

@@next:
        mov dl, byte ptr es:[di]
        cmp dl, END_CHAR
        je @@end
        int 21h
        inc di
        jmp @@next

@@end:
        pop dx ax
        ret
                ENDP


;------------------------------------------------------------------------------
; get_hash
;
; Entry:
;       es:[di] - start address of string
;       cx - len string
; Return:
;       ax - hash
; Destroy:
;       di
;------------------------------------------------------------------------------
get_hash                PROC
        push bx
        xor bx, bx
        xor ax, ax

@@next:
        mov byte ptr bl, es:[di]
        add ax, bx
        inc di
        loop @@next

        pop bx
        ret
                        ENDP


STANDARD_INIT_PHRASE    db      'Pls, enter your password', 0ah, END_CHAR
STANDARD_SUCCESS        db      'Your password is true', 0ah, END_CHAR
STANDARD_FAILED         db      'UUUUU! Meow', 0ah, END_CHAR

end             start
