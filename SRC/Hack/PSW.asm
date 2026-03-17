.model tiny
.386
Locals @@

CHAR_END_INPUT  equ     0dh
END_CHAR        equ     0h
CODE_FAR_JUMP           equ     00eah

.code
org 100h

start:
        push cs
        pop es
        mov ax, offset timer
        mov cx, 0
        call replace_irq_address
        mov word ptr standard_offset, ax
        mov word ptr standard_segment, es

        push cs
        pop es
        mov ax, offset keyboard
        mov cx, 1
        call replace_irq_address
        mov word ptr standard_key_offset, ax
        mov word ptr standard_key_segment, es

        call init_hash_func

        push cs
        pop es

        mov di, offset REAL_PASSWORD
        mov cx, 8

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

        pop bx
        cmp ax, bx

        je not_success
        mov cs:[ACCESS_STATUS], 2
        mov cs:[IS_STOP], 0FFFFh
        mov di, offset STANDARD_SUCCESS
        call print_std_out

        mov ax, 3100h
        mov dx, offset end_resident
        shr dx, 4
        inc dx
        int 21h

not_success:
        mov cs:[ACCESS_STATUS], 1
        mov cs:[IS_STOP], 0FFFFh
        mov di, offset STANDARD_FAILED
        call print_std_out

        mov ax, 3100h
        mov dx, offset end_resident
        shr dx, 4
        inc dx
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


INPUT_PASSWORD        db   '00000000' ; 1   dup(0, 0, 0, 0, 0, 0, 0, 0)
REAL_PASSWORD         db   'poltorashka'

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
        push ax bx cx dx di es

        mov ax, offset get_hash
        mov bx, offset end_get_hash
        sub bx, ax
        mov cx, bx
        inc cx
        mov bx, ax

        push 0160h
        pop es
        mov dx, es:[1658h]
        xor dh, dh
        mov di, 4000
        add di, dx

        mov word ptr [hash_offset], es
        mov word ptr [hash_segment], di

@@next:
        mov al, byte ptr cs:[bx]
        mov byte ptr es:[di], al
        inc bx
        inc di
        loop @@next

        pop es di dx cx bx ax
        ret
                        ENDP


resident:
timer:
        push ax bx cx di si

        mov ax, cs:[IS_STOP]
        cmp ax, 00h
        je stop_music

        mov ax, word ptr cs:[MAX_DELAY]
        mov bx, word ptr cs:[CUR_DEL]
        cmp ax, bx
        jne skip_note

        mov word ptr cs:[CUR_DEL], 0
        jmp play_note

skip_note:
        inc bx
        mov word ptr cs:[CUR_DEL], bx
        jmp restore_and_exit

play_note:
        mov bx, word ptr cs:[CURRENT_SAMPLE]
        shl bx, 1
        mov ax, cs:[DATA + bx]

        cmp ax, 0
        je turn_off_speaker

        mov al, 0B6h
        out 43h, al

        out 42h, al
        mov al, ah
        out 42h, al

        in al, 61h
        or al, 3
        out 61h, al
        jmp next_sample

turn_off_speaker:
        in al, 61h
        and al, 11111100b
        out 61h, al

next_sample:
        mov bx, word ptr cs:[CURRENT_SAMPLE]
        inc bx
        mov ax, word ptr cs:[AMOUNT_SAMPLES]
        cmp bx, ax
        jl update_sample

        mov bx, 0

update_sample:
        mov word ptr cs:[CURRENT_SAMPLE], bx

restore_and_exit:
        pop si di cx bx ax

        db CODE_FAR_JUMP
standard_offset     dw      0
standard_segment    dw      0


stop_music:
        in al, 61h
        and al, 11111100b
        out 61h, al
        jmp restore_and_exit

AMOUNT_SAMPLES  dw      32
CURRENT_SAMPLE  dw      0
MAX_DELAY       dw      3
CUR_DEL         dw      0
IS_STOP         dw      0h
ACCESS_STATUS   dw      0


DATA    dw 9698, 8608, 8126, 7240, 6450, 8126, 6450, 6450
        dw 6834, 8609, 6834, 6834, 7240, 9122, 7240, 7240
        dw 9698, 8609, 8126, 7240, 6450 ,8126 ,6450, 5119
        dw 5424, 6450, 9126, 6450, 5424, 5424, 5424, 5424
        dw 9698, 8608, 8126, 7240, 6450, 8126, 6450, 6450
        dw 6834, 8609, 6834, 6834, 7240, 9122, 7240, 7240
        dw 9698, 8609, 8126, 7240, 6450 ,8126 ,6450, 5119
        dw 5424, 6450, 9126, 6450, 5424, 5424, 5424, 5424

;
; timer:
;         push ax bx di
;         mov ax, cs:[IS_STOP]
;         cmp ax, 00FFh
;         je pop_plus_int
;
;         mov ax, word ptr cs:[MAX_DELAY]
;         mov bx, word ptr cs:[CUR_DEL]
;         cmp ax, bx
;         jne delay
;
;         mov word ptr cs:[CUR_DEL], 0
;         jmp not_delay
;
; delay:
;         inc bx
;         mov word ptr cs:[CUR_DEL], bx
;         jmp pop_plus_int
;
; not_delay:
;
;         mov al, 0b6h
;         out 43h, al
;
;         mov bx, word ptr cs:[CURRENT_SAMPLE]
;         shl bx, 1                   ; Умножаем на 2 (размер WORD)
;         mov ax, cs:[DATA + bx]
;         mov ax, cs:[ACCESS_STATUS]
;         cmp ax, 1
;         je nothing_to_add
;
; add_to_not_access:
;         add bx, cs:[AMOUNT_SAMPLES]
;
; nothing_to_add:
;
;         mov al, cs:[bx]
;         out 42h, al
;         mov al, cs:[bx + 1]
;         out 42h, al
;
;         in al, 61h
;         or al, 3
;         out 61h, al
;
;         mov bx, word ptr cs:[CURRENT_SAMPLE]
;         mov ax, word ptr cs:[AMOUNT_SAMPLES]
;
;         cmp ax, bx
;         jne not_overload
;
;         mov word ptr cs:[CURRENT_SAMPLE], 0
;         jmp pop_plus_int
;
; not_overload:
;         mov ax, cs:[CURRENT_SAMPLE]
;         inc ax
;         mov cs:[CURRENT_SAMPLE], ax
;
; pop_plus_int:
;         pop di bx ax
;
; standard_int:
;         db CODE_FAR_JUMP
; standard_offset     dw      0
; standard_segment    dw      0
;
; AMOUNT_SAMPLES  dw      3
; CURRENT_SAMPLE  dw      0
;
; MAX_DELAY   dw      5
; CUR_DEL dw      0
; IS_STOP dw      00FFh
; ACCESS_STATUS   dw      0
;
; DATA    dw   1000, 1000, 1000, 1000, 1000, 10000
;         dw   1000, 1000, 1000, 1000, 1000, 10000

; DATA    dw 9698, 8608, 8126, 7240, 6450, 8126, 6450, 6450
;         dw 6834, 8609, 6834, 6834, 7240, 9122, 7240, 7240
;         dw 9698, 8609, 8126, 7240, 6450 ,8126 ,6450, 5119
;         dw 5424, 6450, 9126, 6450, 5424, 5424, 5424, 5424
;         dw 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0
;         dw 9698, 8608, 8126, 7240, 6450, 8126, 6450, 6450
;         dw 6834, 8609, 6834, 6834, 7240, 9122, 7240, 7240
;         dw 9698, 8609, 8126, 7240, 6450 ,8126 ,6450, 5119
;         dw 5424, 6450, 9126, 6450, 5424, 5424, 5424, 5424
;         dw 0   , 0   , 0   , 0   , 0   , 0   , 0   , 0


; DATA dw 6088, 6088,    0,    0, 6088, 6088,    0,    0, 6088, 6088
;      dw    0,    0, 3835, 3835,    0,    0, 5119, 5119,    0,    0
;      dw 6088, 6088,    0,    0, 3835, 3835,    0,    0, 5119, 5119
;      dw    0,    0, 6088, 6088, 6088, 6088,    0,    0, 6088, 6088
;      dw    0,    0, 4561, 4561,    0,    0, 4561, 4561,    0,    0
;      dw 4561, 4561,    0,    0, 4063, 4063,    0,    0, 5119, 5119
;      dw    0,    0, 6088, 6088,    0,    0, 3835, 3835,    0,    0
;      dw 5119, 5119,    0,    0, 6088, 6088, 6088, 6088,    0,    0
;      dw 6088, 6088, 3044, 3044,    0,    0, 3044, 3044,    0,    0
;      dw 3044, 3044,    0,    0, 3835, 3835,    0,    0, 5119, 5119
;      dw    0,    0, 6088, 6088,    0,    0, 3835, 3835,    0,    0
;      dw 5119, 5119,    0,    0, 6088, 6088, 6088, 6088,    0,    0
;      dw 6088, 6088,    0,    0,    0,    0,    0,    0, 4561, 4561
;      dw    0,    0, 4561, 4561,    0,    0, 4561, 4561,    0,    0
;      dw 4063, 4063,    0,    0, 5119, 5119,    0,    0, 6088, 6088
;      dw    0,    0, 3835, 3835,    0,    0, 5119, 5119,    0,    0
;      dw 6088, 6088, 6088, 6088,    0,    0, 6088, 6088,    0,    0
;      dw 3044, 3044,    0,    0, 3044, 3044,    0,    0, 3044, 3044
;      dw    0,    0, 3835, 3835,    0,    0, 5119, 5119,    0,    0
;      dw 6088, 6088,    0,    0, 3835, 3835,    0,    0, 5119, 5119
;      dw    0,    0, 5119, 5119,    0,    0, 6088, 6088, 6088, 6088
;      dw    0,    0, 6088, 6088,    0,    0,    0,    0,    0,    0
;      dw 3044, 3044,    0,    0, 3044, 3044,    0,    0, 3835, 3835
;      dw    0,    0, 5119, 5119,    0,    0, 6088, 6088,    0,    0
;      dw 3835, 3835,    0,    0, 5119, 5119,    0,    0, 6088, 6088


keyboard:
        push ax
        mov ax, cs:[ACCESS_STATUS]
        cmp ax, 0
        pop ax
        je standard_key

        push ax
        in al, 60h
        cmp al, 2 ;1fh - scan code of 'S'
        je start_msc

        cmp al, 3
        je stop_msc

        pop ax

standard_key:
        db CODE_FAR_JUMP
standard_key_offset     dw      0
standard_key_segment    dw      0

start_msc:
        mov ax, 0h
        mov cs:[IS_STOP], ax
        pop ax
        jmp standard_key

stop_msc:
        mov ax, 1h
        mov cs:[IS_STOP], ax
        pop ax
        jmp standard_key
end_resident:

;------------------------------------------------------------------------------
; replace_irq_address
;
; Entry:
;       ax - new offset of irq
;       es - new segment of irq
;       cx - number interrupt
; Return:
;       ax - old offset of irq
;       es - old segment of irq
; Destroy:
;       -
;------------------------------------------------------------------------------
replace_irq_address     PROC
        push di dx bx cx

        mov di, 8
        add di, cx
        shl di, 2

        push es
        pop cx

        mov dx, 0
        push dx
        pop es
        mov bx, es:[di]
        mov dx, es:[di + 2]

        cli
        mov es:[di], ax
        mov es:[di + 2], cx
        sti

        mov ax, bx
        push dx
        pop es

        pop cx bx dx di
        ret
                        ENDP

end             start
