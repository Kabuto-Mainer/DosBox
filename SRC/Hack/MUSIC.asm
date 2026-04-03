.model tiny
.386
Locals @@

CODE_FAR_JUMP           equ     00eah

.code
org 100h

start:
        mov ax, offset resident
        push cs
        pop es
        xor cx, cx
        call replace_irq_address

        mov word ptr standard_offset, ax
        mov word ptr standard_segment, es

        mov ax, 3100h
        mov dx, offset end_resident
        shr dx, 4
        inc dx
        int 21h

resident:
        push ax bx di
        mov ax, word ptr cs:[MAX_DELAY]
        mov bx, word ptr cs:[CUR_DEL]
        cmp ax, bx
        jne delay

        mov word ptr cs:[CUR_DEL], 0
        jmp not_delay

delay:
        inc bx
        mov word ptr cs:[CUR_DEL], bx
        pop di bx ax
        jmp standard_int

not_delay:

        mov al, 0b6h
        out 43h, al

        mov ax, word ptr cs:[CURRENT_SAMPLE]
        mov bx, 2
        mul bl
        mov di, ax
        mov bx, word ptr cs:[di]
        add bx, offset DATA
        mov al, cs:[bx]
        out 42h, al
        mov al, cs:[bx + 1]
        out 42h, al

        in al, 61h
        or al, 3
        out 61h, al

        mov bx, word ptr cs:[CURRENT_SAMPLE]
        mov ax, word ptr cs:[AMOUNT_SAMPLES]

        cmp ax, bx
        jne not_overload

        mov word ptr cs:[CURRENT_SAMPLE], 0
        pop di bx ax
        jmp standard_int

not_overload:
        mov ax, cs:[CURRENT_SAMPLE]
        inc ax
        mov cs:[CURRENT_SAMPLE], ax

        pop di bx ax

standard_int:
        db CODE_FAR_JUMP
standard_offset     dw      0
standard_segment    dw      0

print_art       PROC
        push ax cx es di
        push 0b800h
        pop es

        mov di, 160h


                ENDP

AMOUNT_SAMPLES  dw      250
CURRENT_SAMPLE  dw      0

MAX_DELAY   dw      3
CUR_DEL dw      0

;
; DATA dw    0,    0, 4832, 4832,    0,    0, 4832, 4832,    0,    0
;      dw 4832, 4832, 4063, 4063, 3225, 3225, 4832, 4832,    0,    0
;      dw 4832, 4832, 4063, 4063, 3225, 3225, 4832, 4832,    0,    0
;      dw 4832, 4832, 4063, 4063, 3225, 3225, 4832, 4832,    0,    0
;      dw 4832, 4832, 4063, 4063, 3225, 3225, 4832, 4832,    0,    0
;      dw 4561, 4561, 3619, 3619, 3044, 3044, 4561, 4561,    0,    0
;      dw 4561, 4561, 3619, 3619, 3044, 3044, 4561, 4561,    0,    0
;      dw 4561, 4561, 3619, 3619, 3044, 3044, 4561, 4561,    0,    0
;      dw 4561, 4561, 3619, 3619, 3044, 3044, 4561, 4561,    0,    0
;      dw 4063, 4063, 3225, 3225, 2712, 2712, 4063, 4063,    0,    0
;      dw 4063, 4063, 3225, 3225, 2712, 2712, 4063, 4063,    0,    0
;      dw 4063, 4063, 3225, 3225, 2712, 2712, 4063, 4063,    0,    0
;      dw 4063, 4063, 3225, 3225, 2712, 2712, 4063, 4063,    0,    0
;      dw 3619, 3619, 3044, 3044, 2416, 2416, 3619, 3619,    0,    0
;      dw 3619, 3619, 3044, 3044, 2416, 2416, 3619, 3619,    0,    0
;      dw 3619, 3619, 3044, 3044, 2416, 2416, 3619, 3619,    0,    0
;      dw 3619, 3619, 3044, 3044, 2416, 2416, 3619, 3619,    0,    0
;      dw 3225, 3225, 2712, 2712, 2280, 2280, 3225, 3225,    0,    0
;      dw 3225, 3225, 2712, 2712, 2280, 2280, 3225, 3225,    0,    0
;      dw 3225, 3225, 2712, 2712, 2280, 2280, 3225, 3225,    0,    0
;      dw 3225, 3225, 2712, 2712, 2280, 2280, 3225, 3225,    0,    0
;      dw 2416, 2280, 2416, 2280, 2416, 2280, 2416, 2280,    0,    0
;      dw 2416, 2280, 2416, 2280, 2416, 2280, 2416, 2280,    0,    0
;      dw 2280, 2280, 2280, 2280, 2280, 2280, 2280, 2280,    0,    0

DATA dw 6088, 6088,    0,    0, 6088, 6088,    0,    0, 6088, 6088  ; 01. G3, G3, G3 (Dum, Dum, Dum)
     dw    0,    0, 3835, 3835,    0,    0, 5119, 5119,    0,    0  ; 02. Eb4, Bb3 (Da-da)
     dw 6088, 6088,    0,    0, 3835, 3835,    0,    0, 5119, 5119  ; 03. G3, Eb4, Bb3 (Da-da-da)
     dw    0,    0, 6088, 6088, 6088, 6088,    0,    0, 6088, 6088  ; 04. G3 Long (Daaaaa)
     dw    0,    0, 4561, 4561,    0,    0, 4561, 4561,    0,    0  ; 05. C4, C4, C4 (Dum, Dum, Dum)
     dw 4561, 4561,    0,    0, 4063, 4063,    0,    0, 5119, 5119  ; 06. C4, D4, Bb3 (Da-da)
     dw    0,    0, 6088, 6088,    0,    0, 3835, 3835,    0,    0  ; 07. G3, Eb4, Bb3 (Da-da-da)
     dw 5119, 5119,    0,    0, 6088, 6088, 6088, 6088,    0,    0  ; 08. Bb3, G3 Long (Daaaaa)
     dw 6088, 6088, 3044, 3044,    0,    0, 3044, 3044,    0,    0  ; 09. G3, G4, G4 (Octave Up)
     dw 3044, 3044,    0,    0, 3835, 3835,    0,    0, 5119, 5119  ; 10. G4, Eb4, Bb3
     dw    0,    0, 6088, 6088,    0,    0, 3835, 3835,    0,    0  ; 11. G3, Eb4, Bb3
     dw 5119, 5119,    0,    0, 6088, 6088, 6088, 6088,    0,    0  ; 12. Bb3, G3 Long
     dw 6088, 6088,    0,    0,    0,    0,    0,    0, 4561, 4561  ; 13. Pause, C4 start
     dw    0,    0, 4561, 4561,    0,    0, 4561, 4561,    0,    0  ; 14. C4, C4, C4
     dw 4063, 4063,    0,    0, 5119, 5119,    0,    0, 6088, 6088  ; 15. D4, Bb3, G3
     dw    0,    0, 3835, 3835,    0,    0, 5119, 5119,    0,    0  ; 16. Eb4, Bb3
     dw 6088, 6088, 6088, 6088,    0,    0, 6088, 6088,    0,    0  ; 17. G3 Long + Break
     dw 3044, 3044,    0,    0, 3044, 3044,    0,    0, 3044, 3044  ; 18. G4, G4, G4 (Finale)
     dw    0,    0, 3835, 3835,    0,    0, 5119, 5119,    0,    0  ; 19. Eb4, Bb3
     dw 6088, 6088,    0,    0, 3835, 3835,    0,    0, 5119, 5119  ; 20. G3, Eb4, Bb3
     dw    0,    0, 5119, 5119,    0,    0, 6088, 6088, 6088, 6088  ; 21. Bb3, G3 Long
     dw    0,    0, 6088, 6088,    0,    0,    0,    0,    0,    0  ; 22. G3 End + Pause
     dw 3044, 3044,    0,    0, 3044, 3044,    0,    0, 3835, 3835  ; 23. G4, G4, Eb4
     dw    0,    0, 5119, 5119,    0,    0, 6088, 6088,    0,    0  ; 24. Bb3, G3
     dw 3835, 3835,    0,    0, 5119, 5119,    0,    0, 6088, 6088  ; 25. Eb4, Bb3, G3 (Final)

;
; CAT dw \
; dw      "                   .-."
; dw      "                  / / "
; dw      "                 / |  "
; dw      "   |\     ._ ,-""  `. "
; dw      "   | |,,_/  7        ;"
; dw      " `;=     ,=(     ,  / "
; dw      "  |`q  q  ` |    \_,| "
; dw      " .=; <> _ ; /  ,/'/ | "
; dw      "';|\,j_ \;=\ ,/   `-' "
; dw      "    `--'_|\  )        "
; dw      "   ,' | /  ;'         "
; dw      "  (,,/ (,,/      fsc  "






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
