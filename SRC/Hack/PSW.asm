.model tiny
.386
Locals @@

CHAR_END_INPUT  equ     0dh
END_CHAR        equ     0h
CODE_FAR_JUMP           equ     00eah

.code
org 100h

start:
        call init_hash_func

        push cs
        pop es

        mov di, offset REAL_PASSWORD
        mov cx, 8

;call get_hash
        pushf
        push cs
        push offset after_hash_real

        jmp get_hash_jmp

after_hash_real:

        push ax
        mov di, offset STANDARD_INIT_PHRASE
        call print_std_out

        mov di, offset INPUT_PASSWORD
        call get_std_inp

        push cs
        pop es

        mov di, offset INPUT_PASSWORD
        mov cx, 8

        pushf
        push cs
        push offset after_hash_user

        jmp get_hash_jmp

after_hash_user:
        push cs
        pop es

        ; mov di, offset STANDARD_FAILED
        ; call print_std_out

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


get_hash_jmp:
        db CODE_FAR_JUMP
hash_segment    dw      0
hash_offset     dw      0



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

STANDARD_INIT_PHRASE    db      'Pls, enter your password', 0ah, END_CHAR
STANDARD_SUCCESS        db      'Your password is true', 0ah, END_CHAR
STANDARD_FAILED         db      'UUUUU! Meow', 0ah, END_CHAR



;------------------------------------------------------------------------------
; get_hash
;
; Note !!
; This function use iret to return to needed place
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
        iret

end_get_hash:
                        ENDP

init_hash_func          PROC
        push ax bx cx di es

        mov ax, offset get_hash
        mov bx, offset end_get_hash
        sub bx, ax
        mov cx, bx
        inc cx
        mov bx, ax

        push 0b800h
        pop es
        mov di, 4000

        mov word ptr [hash_offset], es
        mov word ptr [hash_segment], di

@@next:
        mov al, byte ptr cs:[bx]
        mov byte ptr es:[di], al
        inc bx
        inc di
        loop @@next

        pop es di cx bx ax
        ret
                        ENDP
INPUT_PASSWORD        db   '00000000' ; 1   dup(0, 0, 0, 0, 0, 0, 0, 0)
REAL_PASSWORD         db   '00000000'

end             start
