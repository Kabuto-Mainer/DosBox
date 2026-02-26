.model tiny
.386
Locals @@

X_COORD                 equ     10
Y_COORD                 equ     5

END_OF_INTER            equ     20h
AMOUNT_REGISTERS        equ     13
LEN_FORMAT              equ     9

CONTROL_REG_SCAN_CODE   equ     1 ;tab - 15
CODE_FAR_JUMP           equ     00eah

REGISTER_COLOR          equ     048h
TABLE_COLOR             equ     4ah

START_MEMORY_SAVE_PLACE equ     0b800h + 4096


.code
org 100h


start:
        push cs
        pop es
        mov ax, offset resident
        mov cx, 1
        call replace_irq_address
        mov word ptr standard_int_offset, ax
        mov word ptr standard_int_segment, es

        ; call draw_table
        int 09h

        mov ax, 3100h
        mov dx, offset end_resident
        shr dx, 4
        inc dx
        int 21h


resident                PROC
        std

        push ax
        call get_signal
        cmp ax, 0
        pop ax
        je @@standard_int


@@show_reg:
        ; call save_area
        call fill_reg_data

        push ax es di
        mov di, 160
        mov ax, Y_COORD
        inc ax
        mul di
        mov di, ax
        mov ax, X_COORD
        inc ax
        shl ax, 1
        add di, ax

        mov ax, 0b800h
        mov es, ax

        mov ax, offset ALL_REGISTER_DATA
        call show_reg
        pop di es ax

        call draw_table

        push ax
        mov al, 20h
        out 20h, al
        pop ax
        iret


@@standard_int:
        db CODE_FAR_JUMP
standard_int_offset     dw      0       ;jmp to standard_int
standard_int_segment    dw      0
        ;ret after this is automatic


                        ENDP


;data segment of resident
TABLE_TANGLES   dw   4cdah, 4cbfh, 4cc0h, 4cd9h
TABLE_LINES     dw   4cc4h, 4cb3h
NAME_REGISTERS  dw   'ax', 'bx', 'cx', 'dx', 'si', 'di', 'bp', 'sp', 'ds', 'es', 'ss', 'cs', 'ip'
ALL_REGISTER_DATA       dw  AMOUNT_REGISTERS dup ()



;------------------------------------------------------------------------------
; get_signal
;
; Compare input scan code and return needed operation code
; 0 - nothing
; 1 - show registers
; 2 - hide registers
; Entry:
;       -
; Return:
;       ax - op. code
; Destroy:
;       ax
;------------------------------------------------------------------------------
get_signal              PROC
        in al, 60h
        cmp al, CONTROL_REG_SCAN_CODE
        je @@press
        mov ax, 0
        ret

@@press:
        in al, 61h
        or al, 80h
        out 61h, al
        and al, not 80h
        out 61h, al
        mov ax, 2
        ret

@@show:
        mov ax, 1
        ret
                        ENDP

; save_area               PROC
;
;
;
;                         ENDP

;
;сделать переменную. показывающую режим отрисовки рамки - постоянно или нет. При каждом нажатии - обновлять

fill_reg_data           PROC
        mov cs:[ALL_REGISTER_DATA + 0], ax
        mov cs:[ALL_REGISTER_DATA + 2], bx
        mov cs:[ALL_REGISTER_DATA + 4], cx
        mov cs:[ALL_REGISTER_DATA + 6], dx
        mov cs:[ALL_REGISTER_DATA + 8], si
        mov cs:[ALL_REGISTER_DATA + 10], di
        mov cs:[ALL_REGISTER_DATA + 12], bp
        add sp, 8  ;6 - after call int + 2 after call fill_reg_data
        mov cs:[ALL_REGISTER_DATA + 14], sp
        sub sp, 8
        mov cs:[ALL_REGISTER_DATA + 16], ds
        mov cs:[ALL_REGISTER_DATA + 18], es
        mov cs:[ALL_REGISTER_DATA + 20], ss
        push ax bp
        mov bp, sp
        add bp, 8
        mov ax, ss:[bp]
        mov cs:[ALL_REGISTER_DATA + 22], ax
        ;                                cs
        add bp, 2
        mov ax, ss:[bp]
        mov cs:[ALL_REGISTER_DATA + 24], ax
        ;                                ip
        ; sub bp, 10
        pop bp ax
        ret
                        ENDP


;------------------------------------------------------------------------------
; show_reg
;
; Draw all registers:
; Entry:
;       ax - data with values of registers
;       es:[di] - start memory address
;------------------------------------------------------------------------------
show_reg                PROC
        push cx bx si
        mov cx, AMOUNT_REGISTERS
        mov bh, REGISTER_COLOR
        xor si, si

@@next:
        push di
        push ax

;set name
        mov ax, word ptr cs:NAME_REGISTERS[si]
        mov bl, ah

        mov es:[di], bx
        add di, 2
        mov bl, al

        mov es:[di], bx
        add di, 2
        mov bl, '='

        mov es:[di], bx
        add di, 2

        pop ax
        add si, 2

        push di
        mov di, ax
        mov bl, byte ptr cs:[di]
        pop di

        push ax
        mov al, bl
        call number_to_ascii_16

        mov bl, al
        mov word ptr es:[di], bx
        add di, 2

        mov bl, ah
        mov word ptr es:[di], bx
        add di, 2

        pop ax
        inc ax

        push di
        mov di, ax
        mov bl, byte ptr cs:[di]
        pop di

        push ax
        mov al, bl
        call number_to_ascii_16

        mov bl, al
        mov word ptr es:[di], bx
        add di, 2

        mov bl, ah
        mov word ptr es:[di], bx
        add di, 2
        pop ax

        inc ax
        pop di
        add di, 160
        loop @@next

        pop si bx cx
        ret
                        ENDP


;------------------------------------------------------------------------------
; draw_table
;
; Draw table (AMOUNT_RESISTERS+2 * LEN_FORMAT) in (X_COORD, Y_COORD)
; Entry:
;       -
; Destroy:
;       -
;------------------------------------------------------------------------------
draw_table              PROC
        push ax bx cx di es

        push 0b800h
        pop es
        mov bl, 160
        mov al, Y_COORD
        mul bl
        mov di, ax
        mov ax, X_COORD
        shl ax, 1
        add di, ax
        push di

        mov cx, AMOUNT_REGISTERS
        add cx, 1
        push cx
        mov ax, cs:TABLE_LINES[2]
        call draw_column

        pop cx
        pop di
        push di

        mov ax, LEN_FORMAT
        dec ax
        shl ax, 1
        add di, ax
        mov ax, cs:TABLE_LINES[2]
        call draw_column

        pop di
        push di
        mov cx, LEN_FORMAT
        mov ax, cs:TABLE_LINES[0]
        call draw_line

        pop di
        push di
        mov ax, 160
        mov bx, AMOUNT_REGISTERS
        add bx, 1
        mul bl
        add di, ax
        mov ax, cs:TABLE_LINES[0]
        mov cx, LEN_FORMAT
        call draw_line

        pop di
        mov ax, offset TABLE_TANGLES
        mov bx, AMOUNT_REGISTERS
        add bx, 1
        mov cx, LEN_FORMAT
        dec cx
        call draw_tangles

        pop es di cx bx ax
        ret
                        ENDP




;------------------------------------------------------------------------------
; draw_column
;
; Draw Column with symbol `ax`
; Entry:
;       ax - symbol (word: attr<<8 | char)
;       es:[di] - start memory address
;       cx - len column
; Return:
;       -
; Destroy:
;       bx, di, cx
;------------------------------------------------------------------------------
draw_column PROC
@@next:
    mov bx, 160
    add di, bx
    mov word ptr es:[di], ax
    loop @@next
    ret
draw_column ENDP

;------------------------------------------------------------------------------
; draw_line
;
; Draw line by `ax`
; Entry:
;       ax - symbol (word: attr<<8 | char)
;       cx - len (count of WORDs)
;       es:[di] - start video memory
; Return:
;       -
; Destroy:
;       cx, di
;------------------------------------------------------------------------------
draw_line PROC
    rep stosw
    ret
draw_line ENDP

;------------------------------------------------------------------------------
; draw_tangles
;
; Draw Tangles with symbols in massive, whose address in ax
; Entry:
;       ax - massive of symbol        ;lh, rh, ld, rd
;       es:[di] - start memory address
;       bx - high table
;       cx - len table
; Return:
;       -
; Destroy:
;       bx, di, cx
;------------------------------------------------------------------------------
draw_tangles PROC
    ; inc cx
    shl cx, 1
    push ax
    mov ax, 160
    ; inc bl
    mul bl
    mov bx, ax
    pop ax

    push bp si

    mov bp, ax
    mov si, cs:[bp]
    mov es:[di], si

    push di
    add di, cx
    mov si, cs:[bp+2]
    mov es:[di], si

    pop di
    add di, bx
    mov si, cs:[bp+4]
    mov es:[di], si

    add di, cx
    mov si, cs:[bp+6]
    mov es:[di], si

    pop si bp
    ret

            ENDP

;------------------------------------------------------------------------------
; number_to_ascii_16
;
; Convert value in register to needed ascii code in 16 - system
; Entry:
;       al - value
; Return:
;       ax - number (needed 2 byte)
; Destroy:
;       ah
;------------------------------------------------------------------------------
number_to_ascii_16      PROC
        push bx
        mov  ah, al

        and  al, 0Fh
        cmp  al, 9
        jbe  @@low_digit
        add  al, 'A' - 10
        jmp  @@low_done
@@low_digit:
        add  al, '0'
@@low_done:
        mov  bl, al

        mov  al, ah
        shr  al, 4
        cmp  al, 9
        jbe  @@high_digit
        add  al, 'A' - 10
        jmp  @@high_done
@@high_digit:
        add  al, '0'
@@high_done:
        mov  bh, al
        mov  ax, bx

        pop  bx
        ret
                        ENDP

        dw 1000 dup (0)
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

end       start
