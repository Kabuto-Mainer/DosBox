.model tiny

;==============================================================================
; DATA SEGMENT
;==============================================================================

;tasks:
;       - сделать перебор аргументов через jump-таблицу
;       -

dataseg
    ;Coordinates
    X_cord      dw  10
    Y_cord      dw  10

    ;Params
    Len         dw  0
    Hight       dw  0

    ;Buffer for save values
    Buffer      dw  0

    ;Symbols              Line    Column  Fon     Mask Text
    Chars       dw  4 dup (4CC4h, 4CB3h,  4C03h,  4C00h)

    ; Corners (tangles)
    Tangles     dw  4 dup (4CDAh, 4CBFh,  4CC0h,  4CD9h)

.code

Null_Char   equ '$'
Flag_Char   equ '-'
Locals @@
org 100h

;==============================================================================
; CODE
;==============================================================================

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
draw_line   MACRO      ;marcoc
    rep stosw
            ENDM

;========================================  START  ================================
start:
    cld                         ; set track

    ; -------------------------------------------------------------------------
    ; Prepare for parsing command tail
    ; -------------------------------------------------------------------------
    xor dx, dx
    mov bx, 80h
    mov dl, [bx]
    add dx, 81h
    mov al, Null_Char

    push cs
    pop  es
    mov di, 81h

    call ctr_dt_text

    ; -------------------------------------------------------------------------
    ; Init video memory segment
    ; -------------------------------------------------------------------------
    mov cx, 0B800h
    mov es, cx

;--------------------------------------------------------------------------------

    push bx

;--------------------------------------------------------------------------------
; Find left high tangle
;--------------------------------------------------------------------------------
    xor dx, dx

    mov ax, Y_cord
    mov bl, 160
    mul bl
    add dx, ax

    mov ax, X_cord
    mov bl, 2
    mul bl
    add dx, ax

;--------------------------------------------------------------------------------
; Save amount strings in command line
;--------------------------------------------------------------------------------
    pop cx
    mov [Buffer], cx

;--------------------------------------------------------------------------------
; Fill background
; Entry for fill_fon (as used here):
;   ax = symbol
;   cx = Hight
;   bx = Len
;   es:di = start address
;--------------------------------------------------------------------------------
    mov ax, [Chars+4]
    mov cx, Hight
    mov bx, Len
    mov di, dx
    call fill_fon

;--------------------------------------------------------------------------------
; Print parsed lines
; prt_dt_text uses:
;   dx = left high tangle
;   cx = amount strings
;--------------------------------------------------------------------------------
    mov cx, [Buffer]
    mov [Buffer], dx            ; need to be saved
    call prt_dt_text

    mov dx, [Buffer]

;--------------------------------------------------------------------------------
; Draw lines
;--------------------------------------------------------------------------------
    mov ax, [Chars+0]            ; line symbol
    mov cx, Len
    push cx
    mov di, dx
    push di

    draw_line
    mov bx, di

    pop di
    pop cx
    push bx                      ; we need to save it

    mov bx, Hight
    push ax
    mov ax, 160
    mul bl
    add di, ax
    pop ax

    draw_line

    mov ax, [Chars+2]            ; column symbol
    mov di, dx
    mov cx, Hight
    call draw_column

    pop di
    sub di, 2
    push di

    mov cx, Hight
    call draw_column

;--------------------------------------------------------------------------------
; Set tangles
;--------------------------------------------------------------------------------
    mov di, dx
    mov ax, [Tangles+0]
    mov es:[di], ax

    push ax

    mov cx, Len
    sub cx, 1
    mov ax, 2
    mul cl
    push ax
    add di, ax
    mov cx, [Tangles+2]
    mov es:[di], cx

    mov di, dx
    mov bx, Hight
    mov al, 160
    mul bl
    add di, ax
    mov cx, [Tangles+4]
    mov es:[di], cx

    pop bx
    add di, bx
    mov cx, [Tangles+6]
    mov es:[di], cx

;--------------------------------------------------------------------------------

    mov ax, 4C00h
    int 21h

;==============================================================================
; PROCEDURES
;==============================================================================

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
; fill_fon
;
; Fill fon of place
; Entry:
;       ax - symbol (word: attr<<8 | char)
;       bx - hight
;       cx - len
;       es:[di] - start memory address
; Return:
;       -
; Destroy:
;       bx, cx, di
;------------------------------------------------------------------------------
fill_fon PROC
@@next:
    xchg bx, cx
    push di
    push cx
    rep stosw
    pop cx
    pop di
    add di, 160
    xchg bx, cx
    loop @@next
    ret
fill_fon ENDP

;------------------------------------------------------------------------------
; print_text
;
; Print string
; Entry (your convention):
;       al - color fon
;       cx - len
;       es:[bx] - start video address
;       cs:[di] - string address
; Return:
;       -
; Destroy:
;       ax, bx, cx, di
;------------------------------------------------------------------------------
print_text PROC
    mov ah, al
@@next:
    mov al, byte ptr [di]
    mov es:[bx], ax
    inc di
    inc bx
    inc bx
    loop @@next
    ret
print_text ENDP

;------------------------------------------------------------------------------
; Strlen
;
; Find len fo string
; Entry:
;       al - symbol of end
;       es:[di] - start string address
; Return:
;       es:[di] - address of symbol after end
;       cx - len string
; Destroy:
;       cx
;------------------------------------------------------------------------------
Strlen PROC
    xor cx, cx
    sub cl, 1                  ; cx = 00FFh
    repne scasb
    neg cl
    sub cx, 1
    ret
Strlen ENDP

;------------------------------------------------------------------------------
; ctr_dt_text
;
; Parse command line and fill stack to print text
; Entry:
;       es:[di] - start address
;       bx in this proc - counter of strings
;       dx - max address
;       al - end symbol
;
; Return:
; (addresses are just examples)
; 0100h  di  - address (1)
; 0098h  cx  - len (1)
; 0096h  di  - address (2)
; 0094h  cx  - len (2)
; Destroy:
;       ax, bx, cx, di, si
; Note:
;       This function must called in start
;       of program, because it destroys a lot of registers
;       and we can't use stack to save its
;------------------------------------------------------------------------------
ctr_dt_text PROC
    pop si
    xor bx, bx
    ; cmp di, dx
    ; je @@standard
    jmp @@next

@@standard:
    push 80h
    push 1
    mov bx, 1
    mov [Len], 2
    ret

@@next:
    inc di
    cmp byte ptr es:[di], Flag_Char
    je @@flag

    push di
    call Strlen

    push ax
    dec di
    mov ax, [Chars+6]
    mov [di], ax
    inc di
    pop ax

    cmp di, dx
    ja  @@end

    cmp cx, [Len]
    ja  @@inc_1

@@inc_2:
    inc bx
    dec cx
    push cx
    jmp @@next

@@end:
    pop di

    mov ax, [Len]
    add ax, 3
    mov [Len], ax

    push bx
    add bx, 3
    mov [Hight], bx
    pop bx

    push si
    ret

@@inc_1:
    mov [Len], cx
    jmp @@inc_2

@@flag:
    inc di
    cmp byte ptr es:[di], 'x'
    je @@x
    cmp byte ptr es:[di], 'y'
    je @@y
    cmp byte ptr es:[di], 'r'
    je @@r
    cmp byte ptr es:[di], 't'
    je @@t
    cmp byte ptr es:[di], 'f'
    je @@f

    jmp @@next

@@x:
    add di, 2
    push ax
    call two_char_to_num
    xor ah, ah
    mov [X_cord], ax
    pop ax
    jmp @@next

@@y:
    add di, 2
    push ax
    call two_char_to_num
    xor ah, ah
    mov [Y_cord], ax
    pop ax
    jmp @@next

@@r:
    add di, 2
    push ax
    call two_char_to_num
    xor ah, ah

    mov byte ptr [Chars+0], al
    mov byte ptr [Chars+2], al
    mov byte ptr [Tangles+0], al
    mov byte ptr [Tangles+2], al
    mov byte ptr [Tangles+4], al
    mov byte ptr [Tangles+6], al

    pop ax
    jmp @@next

@@f:
    add di, 2
    push ax
    call two_char_to_num
    xor ah, ah

    mov byte ptr [Chars+4], al
    pop ax
    jmp @@next

@@t:
    add di, 2
    push ax
    call two_char_to_num
    xor ah, ah

    mov byte ptr [Chars+6], al
    pop ax
    jmp @@next

ctr_dt_text ENDP

;------------------------------------------------------------------------------
; prt_dt_text
;
; Print text after ctr_dt_text
; Entry:
;       dx - left hight tangle
;       cx - amount strings
; Destroy:
;       ax, bx, cx, dx, di, si
;------------------------------------------------------------------------------
prt_dt_text PROC
    pop si
    add dx, 160

@@next:
    add dx, 160

    mov ax, Len
    pop bx                      ; get len string
    push bx
    sub ax, bx
    sar ax, 1                   ; ax = ax/2
    mov bx, 2
    mul bl
    pop bx

    pop di                      ; get string address

    push dx
    add dx, ax                  ; dx - start address

    push cx                     ; save cx
    push dx

    mov cx, bx
    mov bx, [Chars+6]
    mov al, bh
    pop bx

    call print_text

    pop cx
    pop dx

    loop @@next

    push si
    ret
prt_dt_text ENDP

;------------------------------------------------------------------------------
; ascii_to_num
;
; Transform ascii byte to number
; Entry:
;       al - symbol
; Return:
;       al - value
;------------------------------------------------------------------------------
ascii_to_num    PROC
    cmp al, '0'
    jna @@error
    cmp al, '9'
    jna @@number
    cmp al, 'A'
    jb @@error
    cmp al, 'F'
    jna @@UPchar
    cmp al, 'a'
    jb @@error
    cmp al, 'f'
    jna @@DOWNchar

@@error:
    ret

@@number:
    sub al, '0'
    ret

@@UPchar:
    sub al, 'A'
    add al, 0Ah
    ret

@@DOWNchar:
    sub al, 'a'
    add al, 0Ah
    ret

ascii_to_num    ENDP


;------------------------------------------------------------------------------
; two_char_to_num
;
; Transform two symbols to num (8 bit)
; Entry:
;       es:[di] - start address
; Return:
;       al - num
;    ```di += 2```
; Destroy:
;       ax
;------------------------------------------------------------------------------
two_char_to_num PROC

    mov al, byte ptr es:[di]
    call ascii_to_num
    mov ah, al
    inc di
    mov al, byte ptr es:[di]
    call ascii_to_num
    shl ah, 4
    add al, ah
    inc di
    ret

two_char_to_num ENDP



set_char_gen    PROC
    ;al - number ascii symbol
    ;es:[dx] - start data





set_char_gen    ENDP

;==============================================================================
; CONST / STRINGS
;==============================================================================

STD_STR  db 'Poltorashka$'

end start
