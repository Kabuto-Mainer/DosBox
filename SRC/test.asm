.model tiny
.code

org 100h

start:
        mov cx, 100h
        mov dx, 200h

next:
        in al, 60h
        cmp al, 15
        jne next

        mov ax, 4C00h
        int 21h
end     start
