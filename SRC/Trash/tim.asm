.model tiny
.code
.386
org 100h

VIDEO_MEM_START_ADDR      equ 0b800h
LENGTH_VIDEO_MEM_LINE     equ 80d
SCAN_CODE_COLOR           equ 4eh

EOI                       equ 20h

SHOW_REG_BUTTON_SCAN_CODE equ 29h

DIFFERENCE_NUM_ASCII_L9   equ 48d
DIFFERENCE_NUM_ASCII_G9   equ 55d


Start: jmp nonResident

; --------------------------------------------------------------------------
; new int 09 handler part
; --------------------------------------------------------------------------


;-----------------------------------------------------------------------
; new Function performed on int 09h draw regs values on '`' button press
; Entry: no
; Exit:  no
; Exp:   no
; Destr: no
;-----------------------------------------------------------------------

keyboardInt PROC
    pop ax
    pop bx
    pop cx

    push cx
    push bx
    push ax

    push VIDEO_MEM_START_ADDR
    pop es
    mov di, (LENGTH_VIDEO_MEM_LINE * 5d + LENGTH_VIDEO_MEM_LINE / 2) * 2


    mov es:[di], ch
    mov es:[di+2], cl
    mov es:[di+4], ah
    mov es:[di+6], al
    mov es:[di+8], bh
    mov es:[di+10], bl

    push bx
    call drawReg

    push ax bx es
    call keyboardIntPreamble

    call handleScanCode

    call resetControllers

    pop es bx ax
    db 0eah
OLD_INT09_HANDLER: dd 0

keyboardInt ENDP

;-----------------------------------------------------------------------
; preamble of int 09h function
; Entry: no
; Exit:  no
; Exp:   no
; Destr: es, bx, ah
;-----------------------------------------------------------------------

keyboardIntPreamble PROC

    push VIDEO_MEM_START_ADDR
    pop es
    mov bx, (LENGTH_VIDEO_MEM_LINE * 5d + LENGTH_VIDEO_MEM_LINE / 2) * 2   ; set start of video mem to write to the 5th line center

    mov ah, SCAN_CODE_COLOR  ;set color for scan-code to show

    ret
keyboardIntPreamble ENDP

;-----------------------------------------------------------------------
; read info from keyboard port 60h and
; if pressed needed button calls handler
; Entry: no
; Exit:  no
; Exp:   no
; Destr: al
;-----------------------------------------------------------------------

handleScanCode PROC
    in al, 60h

    cmp al, SHOW_REG_BUTTON_SCAN_CODE
    jne ??notNeededSC
        ; mov es:[bx], ax

        push di
        call drawReg
        add sp, 2h
    ??notNeededSC:

    ret
handleScanCode ENDP

; CDECL
;-----------------------------------------------------------------------
; new Function performed on int 09h draw regs values on '`' button press
; Entry: [bp - 4] = reg value to draw
; Exit:  no
; Exp:   no
; Destr: no
;-----------------------------------------------------------------------

drawReg PROC
    push bp
    mov bp, sp

    push ax bx cx dx si di

    ; init cycle

    mov cx, 1d

    xor dx,dx
    mov dx, 0f000h   ; mask for reg nibble

    mov bx, (LENGTH_VIDEO_MEM_LINE * 5d + LENGTH_VIDEO_MEM_LINE / 2) * 2

    ??startDrawingCycle:
    cmp cx, 5d
    jge ??endDrawingCycle
        mov ax, ss:[bp + 04h]
        and ax, dx

        mov si, 4d
        sub si, cx
        shl si, 2

        push cx
        mov cx, si
        shr ax, cl
        pop cx

        call convertNibbleToASCII

        ; mov byte ptr es:[bx], al
        ; mov byte ptr es:[bx + 1], SCAN_CODE_COLOR

        add bx, 2
        shr dx, 4
        inc cx
    jmp ??startDrawingCycle
    ??endDrawingCycle:

    pop di si dx cx bx ax

    mov sp, bp
    pop bp
    ret
drawReg ENDP

;-----------------------------------------------------------------------
; converts number in nibble to ASCII
; Entry: ax = nibble
;
; Exit:  ax = ASCII code
; Exp:   no
; Destr: ax
;-----------------------------------------------------------------------

convertNibbleToASCII PROC
    ; case <= 9

    cmp ax, 9d
    jg ??G9
        add ax, DIFFERENCE_NUM_ASCII_L9
        jmp ??end

    ??G9:

    ; case > 9

    add ax, DIFFERENCE_NUM_ASCII_G9

    ??end:
    ret
convertNibbleToASCII ENDP

;-----------------------------------------------------------------------
; reset controllers to original state
; Entry: no
; Exit:  no
; Exp:   no
; Destr: al
;-----------------------------------------------------------------------
resetControllers PROC
    ; reset keyboard controller MSB to guarantee that keyboard controller will continue to gen scan-codes
    in al, 61h
    or al, 80h
    out 61h, al
    and al, not 80h
    out 61h, al

    ; send EOI to interrupt controller
    mov al, EOI
    out 20h, al

    ret
resetControllers ENDP





;-----------------------------------------------------------------
; Part to put residental programm in memory
;-----------------------------------------------------------------

nonResident:
    pop ax
    pop bx
    pop cx

    push cx
    push bx
    push ax

    push VIDEO_MEM_START_ADDR
    pop es
    mov bx, (LENGTH_VIDEO_MEM_LINE * 5d + LENGTH_VIDEO_MEM_LINE / 2) * 2

    mov es:[bx], ax

    call addOldIntHandlerJmp

    call replaceInt09Handler

    call keepProgAsResident


addOldIntHandlerJmp PROC
    push 0
    pop ds
    mov si, 4 * 09h
    mov ax, cs
    mov es, ax
    mov di, offset OLD_INT09_HANDLER
    movsd

    ret
addOldIntHandlerJmp ENDP

replaceInt09Handler PROC
    push 0
    pop es
    mov bx, 4 * 09h
    cli
    mov word ptr es:[bx], offset keyboardInt

    mov ax, cs
    mov word ptr es:[bx + 2], ax
    sti

    ret
replaceInt09Handler ENDP

keepProgAsResident PROC
    mov ax, 3100h
    mov dx, offset nonResident
    shr dx, 4
    inc dx
    int 21h

    ret
keepProgAsResident ENDP

end Start
