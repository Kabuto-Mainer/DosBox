.model tiny
.386

CODE_FAR_JUMP           equ     00eah

.code
org 100h

start:
        push cs
        pop es
        mov ax, offset resident
        mov cx, 1
        call replace_irq_address
        mov word ptr standard_08h_offset, ax
        mov word ptr standard_08h_segment, es

        mov ax, 3100h
        mov dx, offset end_resident
        shr dx, 4
        inc dx
        int 21h


resident:
        push ax
        in al, 60h
        cmp al, 2 ;2eh - scan code of 'C'

        pop ax
        je crack

standard_08h:
        db CODE_FAR_JUMP
standard_08h_offset     dw      0
standard_08h_segment    dw      0

crack:
        mov dx, 18ebh
        jmp standard_08h

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
