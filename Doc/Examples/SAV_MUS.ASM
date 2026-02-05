;*************************************************
; SAV_MUS.ASM     Version 2.0     12/20/90 00:17am (c) Ded
;*************************************************
my_program segment
assume cs:my_program, ds:my_program
org 100h
;------------------------------------------------
	port_b       equ  61h         	; Адрес порта В Программируемого
					; Периферийного интерфейса
	command_reg  equ  43h         	; Порт команд микросхемы 8255
	channel_2    equ  42h         	; Порт канала 2 микросхемы 8255

	DV      equ 1190000             ; Делитель для частоты звука
	TACT    equ 3                   ; Длительность нот в 1/18.2 - секундных
					; интервалах

	DOd1    equ DV/138              ; Делители для звуков октавы (ноты)
	RE1     equ DV/146              ;
	MI1     equ DV/164              ;
	FAd1    equ DV/185              ;
	SOL1    equ DV/196              ;
	SOLd1   equ DV/207              ;
	LA1     equ DV/220              ;
	SI1     equ DV/247              ;

	DOd2    equ DV/277              ;
	RE2     equ DV/294              ;

	PAUSE   equ 10                  ; Пропуск такта (звучание с частотой
					; 119 кГц)
;================================================
start:					; Начало программы
	mov ah, 09h                     ; Выводим на экран сообщение 1
	lea dx, mess1
	int 21h
 	call set_timer                  ; Устанавливаем свой обработчик таймера
	call initsound                  ; Подготавливаем микросхему звука
	cld                             ; SI будет увеличиваться в ходе цикла
	mov cx, NSOUNDS                 ; В CX - число нот
	lea si, mus_tab                 ; В SI - адрес таблицы музык. фразы
next:	lodsw                           ; Загружаем ноту
	call sound                      ; Издаем звук
	call delay                      ; Ждем 1 такт
	loop next                       ; На следующий цикл
	call nosound                    ; Все ноты отзвучали - выключаем звук
	call restore_timer              ; Восстанавливаем исходный обработчик
					; таймера
	mov ah, 09h                     ; Печатаем на экране сообщение 2
	lea dx, mess2
	int 21h
	mov ax, 4c00h                   ; Выходим в ДОС с кодом 0 (Все O.k.)
	int 21h  	                ; Выходим...

	mus_tab dw PAUSE, RE2,RE2,RE2,RE2           ; Музыкальная фраза
	        dw        DOd2,DOd2,DOd2
		dw        SOLd1
		dw        SI1
		dw PAUSE, LA1
		dw PAUSE, SOL1
		dw PAUSE, MI1
		dw PAUSE, RE1,RE1
		dw        DOd1
		dw        RE1
		dw        FAd1,FAd1
		dw        MI1
		dw        FAd1
		dw        MI1,MI1,MI1,MI1
		dw        RE1,RE1,RE1

	NSOUNDS equ ($-mus_tab)/2         ; Число нот

	mess1 db 'Copyright (c) Ded, 1990. '
	      db 'Welcome to PC music!', 13,10, '$'
	mess2 db 'Thank you for attention.$'
;================================================
new08 proc far                          ; Наш обработчик таймера. По истечении
					; определенного времени сбрасывает в 0
					; флаг задержки delay_flag (см. про-
					; цедуру delay, которая им управляется)
	inc cs:number_ticks             ; Увеличиваем число тиков (вызовов)
	cmp cs:number_ticks, TACT       ; Время подошло?
	jb exit08                       ; Нет - выходим
	mov cs:delay_flag, 0            ; Да - 1) обнуляем флаг задержки
	mov cs:number_ticks, 0          ;      2) сбрасываем число тиков
exit08:	jmp old08                       ; Выход: вызываем исходный обработчик
					; таймера

	old08        dd ?               ; Адрес старого обработчика таймера
	number_ticks dw 0               ; Счетчик тиков (вызовов)
	delay_flag   dw 0               ; Флаг окончания задержки
new08 endp
;-------------------------------------------------
set_timer proc                          ; Установка собственного обработчика
					; прерывания таймера
	mov ax, 3508h                   ; Функция ДОС #35 - Дать адрес вектора
					; прерывания #08
	int 21h                         ; Передаем ДОС
	mov word ptr old08, bx          ; Запоминаем адрес старого обработчика:
	mov word ptr old08+2, es        ; сначала смещение, потом сегмент
	mov ax, 2508h                   ; Функция ДОС #25 - Установить вектор
	lea dx, new08                   ; Адрес нового обработчика таймера
	int 21h                         ; Устанавливаем свой обработчик
	ret                             ;
set_timer endp                          ; (Destroys AX, BX, DX, ES)
;-------------------------------------------------
restore_timer proc                      ; Восстанавливаем старый обработчик
	push ds                         ; Сохраняем DS
	mov ax, 2508h                   ; Функция ДОС #25 - Установить вектор
	lds dx, old08                   ; Адрес старого обработчика таймера
	int 21h                         ; Передаем ДОС
	pop ds                          ; Восстанавливаем DS
	ret                             ;
restore_timer endp                      ; (Destroys AX, DX)
;-------------------------------------------------
initsound proc                          ; Подготовка микросхемы 8255
	in al, port_b     		; Читаем из порта В
   	or al, 3                        ; Устанавливаем 3 нижних бита
   	out port_b, al                  ; Посылаем обратно
     	mov al, 10110110b               ; Устанавливаем режим общения
					; с каналом 2 микросхемы 8255
     	out command_reg, al             ; Посылаем код режима общения
	ret                             ;
initsound endp                          ; (Destroys AX)
;-------------------------------------------------
sound proc near                         ; Устанавливаем частоту звучания
   	out channel_2, al               ; Посылаем нижний байт частоты
   	mov al, ah                      ; Команда out использует только AL
   	out channel_2, al               ; Посылаем верхний байт
	ret                             ;
sound endp                              ; (Destroys AX)
;-------------------------------------------------
nosound proc                            ;
	in   al, port_b     		; Сбрасывем 3 нижних бита в порту B
   	and  al, 11111100b     		; Периферийного Интерфейса
   	out  port_b, al                 ;
	ret                             ;
nosound endp                            ; (Destroys AX)
;------------------------------------------------
delay proc                              ; Ждем, пока таймер не сбросит delay_flag
delay1:	cmp delay_flag, 0               ; Флаг сброшен?
	jne delay1                      ; Нет - ждем, пока сбросится
	mov delay_flag, 1               ; Да - снова устанавливаем флаг
	ret                             ; и возвращаемся
delay endp                              ;
;------------------------------------------------
my_program ends
;************************************************
end start