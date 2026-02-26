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
draw_line MACRO
        rep stosw
        ENDM

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

        xor di, di
        mov dx, 160
        mov ax, Y_COORD
        mul dl
        add di, dx

        mov dx, 2
        mov ax, Y_COORD
        mul dl
        add dx, di

        call init_buffer_address

        ; call draw_table
        int 09h

        mov ax, 3100h
        mov dx, offset end_resident
        shr dx, 4
        inc dx
        int 21h


resident                PROC
        push ax
        call get_signal
        cmp ax, 0
        pop ax
        je @@standard_int
        cmp ax, 1
        jmp @@show_reg
        ; jmp @@hide


;to timer
@@show_reg:

        call compare_buffer
        jmp @@show

@@init_reg:
        push ax bx cx dx es di si
        mov ax, AMOUNT_REGISTERS
        add ax, 2
        mov bx, LEN_FORMAT
        add bx, 1
        mov cx, 160
        mov dx, 160
        push 0b800h
        pop es
        mov di, cs:[START_VIDEO_BUFFER + 2]
        mov si, cs:[START_FON_BUFFER + 2]
        call copy_area
        pop si di es dx cx bx ax

@@show:
        call fill_reg_data

;         mov di, 160
;         mov ax, Y_COORD
;         inc ax
;         mul di
;         mov di, ax
;         mov ax, X_COORD
;         inc ax
;         shl ax, 1
;         add di, ax
;
;         mov ax, 0b800h
;         mov es, ax

        push ax es di
        mov es, cs:[START_TABLE_BUFFER + 0]
        mov di, cs:[START_TABLE_BUFFER + 2]

        mov ax, offset ALL_REGISTER_DATA
        call show_reg
        pop di es ax

        call draw_table

        push ax bx cx dx es di si
        mov ax, AMOUNT_REGISTERS
        add ax, 2
        mov bx, LEN_FORMAT
        add bx, 1
        mov cx, 160
        mov dx, 160
        push 0b800h
        pop es
        mov di, cs:[START_TABLE_BUFFER + 2]
        mov si, cs:[START_VIDEO_BUFFER + 2]
        call copy_area
        pop si di es dx cx bx ax
        jmp @@own_end

@@hide:
        push ax bx cx dx es di si
        mov ax, AMOUNT_REGISTERS
        add ax, 2
        mov bx, LEN_FORMAT
        add bx, 1
        mov cx, 160
        mov dx, 160
        push 0b800h
        pop es
        mov di, cs:[START_FON_BUFFER + 2]
        mov si, cs:[START_VIDEO_BUFFER + 2]
        call copy_area
        pop si di es dx cx bx ax

@@own_end:

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
ACTIVE_FLAG db  0


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

        push ax
        mov al, ACTIVE_FLAG
        neg al
        cmp al, 0
        je @@show
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
        draw_line

        pop di
        push di
        mov ax, 160
        mov bx, AMOUNT_REGISTERS
        add bx, 1
        mul bl
        add di, ax
        mov ax, cs:TABLE_LINES[0]
        mov cx, LEN_FORMAT
        draw_line

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
    mov si, cs:[bp+6]; dx - left hight tangle
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


START_VIDEO_BUFFER  dw 0, 0
START_TABLE_BUFFER  dw 0, 0
START_FON_BUFFER    dw 0, 0


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


;------------------------------------------------------------------------------
; compare_buffer
;
; Compare VIDEO_BUFFER and TABLE_BUFFER and different copy to FON_BUFFER
; Entry:
;       -
; Destroy:
;       -
;------------------------------------------------------------------------------
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
        push ax

        mov es, word ptr cs:[START_TABLE_BUFFER + 0]
        mov ax, es:[bx]

        mov es, word ptr cs:[START_VIDEO_BUFFER + 0]
        push di
        mov dx, di
        mov cx, es:[di]
        pop di

        cmp cx, ax
        jne @@replace

@@not_replace:

        add bx, 2
        add dx, 2
        add di, 2

        pop ax
        dec ax
        cmp ax, 0
        jne @@next_in_circle
        mov ax, AMOUNT_REGISTERS
        add ax, 2

        pop cx
        dec cx
        cmp cx, 0
        je @@end

        pop di dx bx
        add dx, 160
        add bx, 160
        add di, 160

        jmp @@next_out_circle


@@replace:
        mov es, word ptr cs:[START_FON_BUFFER + 0]
        mov word ptr es:[di], ax

        mov es, word ptr cs:[START_VIDEO_BUFFER + 0]
        push di
        mov di, dx
        mov word ptr es:[di], cx
        pop di

        jmp @@not_replace

@@end:
        pop es di dx cx bx ax
        ret
                        ENDP


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


;------------------------------------------------------------------------------
; init_buffer_address
;
; fill buffers address
; Entry:
;       dx - left hight tangle of table
; Destroy:
;       -
;------------------------------------------------------------------------------
init_buffer_address     PROC
        push ax
        mov ax, 0b800h
        mov [START_TABLE_BUFFER + 0], ax
        mov [START_VIDEO_BUFFER + 0], ax
        mov [START_FON_BUFFER + 0], ax

        mov [START_TABLE_BUFFER + 2], dx
        mov [START_VIDEO_BUFFER + 2], 4096
        mov [START_FON_BUFFER + 0], 4096 * 2

        pop ax
        ret
                        ENDP


end       start
