.model tiny
.386

CODE_FAR_JUMP           equ     00eah

.code
org 100h

start:
        push cs
        pop es
        mov ax, offset resident
        mov cx, 21h
        call replace_irq_address
        mov word ptr standard_int_offset, ax
        mov word ptr standard_int_segment, es

        mov ax, 3100h
        mov dx, offset end_resident
        shr dx, 4
        inc dx
        int 21h


resident:
        push ax
        cmp ah, 01h
        pop ax
        je crack
        jmp standard_int

SUCCESS_FLAG dw 0
CURRENT_SYMBOL dw 0

standard_int:
        db CODE_FAR_JUMP
standard_int_offset     dw      0
standard_int_segment    dw      0

crack:
        push ax
        mov ax, cs:[SUCCESS_FLAG]
        cmp ax, 0
        pop ax
        jne standard_int
        push ax
        mov ax, cs:[CURRENT_SYMBOL]
        cmp ax, 102
        pop ax
        je overflow

        push ax
        mov ax, cs:[CURRENT_SYMBOL]
        add ax, 1
        mov cs:[CURRENT_SYMBOL], ax
        pop ax

        mov al, 0h
        iret

overflow:
        mov al, 0Dh
        mov cs:[SUCCESS_FLAG], ax
        iret

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

        mov di, 0
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
