model tiny
.386
Locals !!

CONTROL_REG_SCAN_CODE   equ     15 ;tab - 15
CODE_FAR_JUMP           equ     00eah
NEEDED_ADDRESS          equ     100h
REPLACE_ADDRESS         equ     102h

start:
        push cs
        pop es
        mov ax, offset resident
        mov cx, 1
        call replace_irq_address
        mov word ptr standard_int_offset, ax
        mov word ptr standard_int_segment, es


resident:
        push ax
        call get_signal
        cmp ax, 0
        pop ax
        je !!standard_int

        push ax
        mov ax, [sp + 4]
        cmp ax, NEEDED_ADDRESS





!!standard_int:
        db CODE_FAR_JUMP
standard_int_offset     dw      0       ;jmp to standard_int
standard_int_segment    dw      0
        ;ret after this is automatic





;------------------------------------------------------------------------------
; get_signal
;
; Compare input scan code and return needed operation code
; 0 - nothing
; 1 - press
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

!!press:
        in al, 61h
        or al, 80h
        out 61h, al
        and al, not 80h
        out 61h, al

        mov al, 1
        ret
                        ENDP


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


end                 start
