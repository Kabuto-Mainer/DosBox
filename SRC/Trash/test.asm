.model tiny
.code

org 100h

start:
        mov ax, 1111h
        mov bx, 2222h
        mov cx, 3333h
        mov dx, 4444h
        mov si, 5555h
        mov di, 6666h
        push 7777h
        pop es

next:
        in al, 60h
        cmp al, 2
        jne next

        mov ax, 4C00h
        int 21h
end     start
