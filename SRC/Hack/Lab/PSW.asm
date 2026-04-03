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
        mov cs:[IS_STOP], 0h
        mov di, offset STANDARD_SUCCESS
        call print_std_out

        mov ax, 3100h
        mov dx, offset end_resident
        shr dx, 4
        inc dx
        int 21h

not_success:
        mov cs:[ACCESS_STATUS], 1
        mov cs:[IS_STOP], 0h
        mov ax, cs:[PLAY_MUSIC]
        add ax, cs:[AMOUNT_SAMPLES]
        add ax, cs:[AMOUNT_SAMPLES]
        mov cs:[PLAY_MUSIC], ax
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

;
; INPUT_PASSWORD        db   '00000000' ; 1   dup(0, 0, 0, 0, 0, 0, 0, 0)
; REAL_PASSWORD         db   'poltorashka'

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
        push 0b800h
        pop es

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
        cmp ax, 01h
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
        mov ax, cs:[ACCESS_STATUS]
        cmp ax, 1
        je plus_msc
        add bx, cs:[AMOUNT_SAMPLES]

plus_msc:

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

AMOUNT_SAMPLES  dw      128
CURRENT_SAMPLE  dw      0
MAX_DELAY       dw      3
CUR_DEL         dw      0
IS_STOP         dw      1h
ACCESS_STATUS   dw      0

PLAY_MUSIC      dw      DATA

;
; DATA    dw 9698, 8608, 8126, 7240, 6450, 8126, 6450, 6450
;         dw 6834, 8609, 6834, 6834, 7240, 9122, 7240, 7240
;         dw 9698, 8609, 8126, 7240, 6450 ,8126 ,6450, 5119
;         dw 5424, 6450, 9126, 6450, 5424, 5424, 5424, 5424
;         dw 6088, 6088, 6088, 3835, 5119, 6088, 3835, 5119
;         dw 6088, 6088, 6088, 3835, 5119, 6088, 3835, 5119
;         dw 4561, 4561, 4561, 4063, 5119, 6088, 3835, 5119
;         dw 6088, 6088, 3044, 3044, 3044, 3835, 5119, 6088


DATA    dw 4832, 4063, 3225, 4832, 4063, 3225, 4832, 4063  ; 01-08
        dw 3225, 4832, 4063, 3225, 4832, 4063, 3225, 4832  ; 09-16
        dw 4561, 3619, 3044, 4561, 3619, 3044, 4561, 3619  ; 17-24
        dw 3044, 4561, 3619, 3044, 4561, 3619, 3044, 4561  ; 25-32
        dw 4063, 3225, 2712, 4063, 3225, 2712, 4063, 3225  ; 33-40
        dw 2712, 4063, 3225, 2712, 4063, 3225, 2712, 4063  ; 41-48
        dw 3619, 3044, 2416, 3619, 3044, 2416, 3619, 3044  ; 49-56
        dw 2416, 3619, 3044, 2416, 3619, 3044, 2416, 3619  ; 57-64
        dw 4832, 4063, 3225, 4832, 4063, 3225, 4832, 4063  ; 65-72
        dw 3225, 4832, 4063, 3225, 4832, 4063, 3225, 4832  ; 73-80
        dw 4561, 3619, 3044, 4561, 3619, 3044, 4561, 3619  ; 81-88
        dw 3044, 4561, 3619, 3044, 4561, 3619, 3044, 4561  ; 89-96
        dw 3225, 2712, 2280, 3225, 2712, 2280, 3225, 2712  ; 97-104
        dw 2280, 3225, 2712, 2280, 3225, 2712, 2280, 3225  ; 105-112
        dw 2712, 2416, 2280, 2712, 2416, 2280, 2712, 2416  ; 113-120
        dw 2280, 2712, 2416, 2280, 2712, 2416, 2280, 2280  ; 121-128
        dw 6088, 6088, 6088, 3835, 5119, 6088, 3835, 5119  ; 01-08
        dw 6088, 6088, 6088, 3835, 5119, 6088, 3835, 5119  ; 09-16
        dw 4561, 4561, 4561, 4063, 5119, 6088, 3835, 5119  ; 17-24
        dw 6088, 6088, 3044, 3044, 3044, 3835, 5119, 6088  ; 25-32
        dw 6088, 6088, 6088, 3835, 5119, 6088, 3835, 5119  ; 33-40
        dw 4561, 4561, 4561, 4063, 5119, 6088, 3835, 5119  ; 41-48
        dw 6088, 6088, 3044, 3044, 3044, 3835, 5119, 6088  ; 49-56
        dw 3835, 3835, 3835, 3044, 3835, 4561, 4561, 3835  ; 57-64
        dw 6088, 6088, 6088, 3835, 5119, 6088, 3835, 5119  ; 65-72
        dw 6088, 6088, 6088, 3835, 5119, 6088, 3835, 5119  ; 73-80
        dw 4561, 4561, 4561, 4063, 5119, 6088, 3835, 5119  ; 81-88
        dw 6088, 6088, 3044, 3044, 3044, 3835, 5119, 6088  ; 89-96
        dw 5119, 5119, 5119, 4561, 5119, 6088, 5119, 4561  ; 97-104
        dw 3835, 3835, 3835, 3044, 3835, 4561, 4561, 3835  ; 105-112
        dw 6088, 6088, 6088, 6088, 6088, 6088, 6088, 6088  ; 113-120
        dw 6088, 6088, 6088, 6088, 6088, 6088, 6088, 6088  ; 121-128

; DATA dw 6088   6088,    0,    0, 6088, 6088,    0,    0, 6088, 6088
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
        cmp al, 1ch;1fh - scan code of 'S'
        je stop_msc

        pop ax

standard_key:
        db CODE_FAR_JUMP
standard_key_offset     dw      0
standard_key_segment    dw      0

; switcher:
;         mov ax, cs:[IS_STOP]
;         cmp ax, 1h
;         je start_msc
;         mov cs:[IS_STOP], 1h
;         ; in al, 61h
;         ; and al, 11111100b
;         ; out 61h, al
;         pop ax
;         jmp standard_key
;
; start_msc:
;         mov cs:[IS_STOP], 0h
;         pop ax
;         jmp standard_key

stop_msc:
        cmp cs:[ACCESS_STATUS], 1
        jne b1
        push ax bx cx di es
        ; call crack_fon
        pop es di cx bx ax

b1:
        mov cs:[ACCESS_STATUS], 0h
        mov cs:[IS_STOP], 1h
        in al, 61h
        and al, 11111100b
        out 61h, al
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

crack_fon               PROC
        push 0b800h
        pop es
        xor di, di
        mov cx, 4000

@@next:
        mov ax, word ptr cs:[bx]
        mov word ptr es:[di], ax
        inc bx
        inc bx
        inc di
        inc di
        loop @@next

        ret
                        ENDP


INPUT_PASSWORD        db   '00000000' ; 1   dup(0, 0, 0, 0, 0, 0, 0, 0)
REAL_PASSWORD         db   'poltorashka'

end             start
