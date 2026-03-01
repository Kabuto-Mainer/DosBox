.model tiny
.386
Locals @@

.code
org 100h

start:
    ; mov ax, 0b800h
    ; mov es, ax
    ; mov ax, 5
    ; mov bx, 5
    ; mov cx, 160
    ; mov dx, 160
    ; mov di, 0
    ; mov si, 40
    ; call copy_area
    ; mov ax, 0b800h
    ; mov es, ax
    ; mov ax, 5
    ; mov bx, 5
    ; mov cx, 160
    ; mov dx, 160
    ; mov di, 0
    ; mov si, 80
    ; call copy_area
    mov es, word ptr cs:[START_TABLE_BUFFER + 0]
    call compare_buffer


    mov ax, 4c00h
    int 21h

START_FON_BUFFER dw 0b800h, 80
START_TABLE_BUFFER dw 0b800h, 40
START_VIDEO_BUFFER dw 0b800h, 0
LEN_FORMAT equ 4
AMOUNT_REGISTERS equ 3


;------------------------------------------------------------------------------
; copy_area
;
; Draw same area
; Entry:
;       ax - hight area
;       bx - len area
;       cx - step of hight of in area
;       dx - step of hight of out area
;       es - start segment
;       di - start address in area
;       si - start address out area
; Destroy:
;       -
;------------------------------------------------------------------------------
copy_area               PROC
        push ax di si

@@next_out_circle:
        push di si
        push bx


@@next_in_circle:
        push ax
        ; mov ax, 4c00h
        ; mov word ptr es:[di], ax
        mov ax, word ptr es:[di]
        mov word ptr es:[si], ax
        pop ax
        add di, 2
        add si, 2
        dec bx
        cmp bx, 0
        jne @@next_in_circle

        pop bx
        pop si di
        add si, dx
        add di, cx

        dec ax
        cmp ax, 0
        jne @@next_out_circle

        pop si di ax
        ret
                        ENDP



compare_buffer          PROC

        push ax bx cx dx di es

        mov bx, cs:[START_TABLE_BUFFER + 2]
        mov dx, cs:[START_VIDEO_BUFFER + 2]
        mov di, cs:[START_FON_BUFFER + 2]
        mov cx, LEN_FORMAT
        add cx, 1
        mov ax, AMOUNT_REGISTERS
        add ax, 2

@@next_out_circle:

        push bx dx di
        push cx

@@next_in_circle:
        push ax cx

        mov es, word ptr cs:[START_TABLE_BUFFER + 0]
        mov ax, es:[bx]

        mov es, word ptr cs:[START_VIDEO_BUFFER + 0]
        push di
        mov di, dx
        mov cx, es:[di]
        pop di

        cmp cx, ax
        jne @@replace

@@not_replace:

        add bx, 2
        add dx, 2
        add di, 2

        pop cx ax
        dec ax
        cmp ax, 0
        jne @@next_in_circle
        mov ax, AMOUNT_REGISTERS
        add ax, 2

        pop cx
        dec cx
        cmp cx, 0
        pop di dx bx
        je @@end

        add dx, 160
        add bx, 160
        add di, 160

        jmp @@next_out_circle


@@replace:
        mov es, word ptr cs:[START_FON_BUFFER + 0]
        mov word ptr es:[di], cx

        mov es, word ptr cs:[START_VIDEO_BUFFER + 0]
        push di
        mov di, dx
        mov word ptr es:[di], ax
        pop di

        jmp @@not_replace

@@end:
        pop es di dx cx bx ax
        ret
                        ENDP

end     start
