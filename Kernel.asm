     name "kernel"
; this is a very basic example
; of a tiny operating system.
;
; this is kernel module!
;
; it is assumed that this machine
; code is loaded by 'micro-os_loader.asm'
; from floppy drive from:
;   cylinder: 0
;   sector: 2
;   head: 0


;=================================================
; how to test micro-operating system:
;   1. compile micro-os_loader.asm
;   2. compile micro-os_kernel.asm
;   3. compile writebin.asm
;   4. insert empty floppy disk to drive a:
;   5. from command prompt type:
;        writebin loader.bin
;        writebin kernel.bin /k
;=================================================

; directive to create bin file:
#make_bin#

; where to load? (for emulator. all these values are saved into .binf file)
#load_segment=0800#
#load_offset=0000#

; these values are set to registers on load, actually only ds, es, cs, ip, ss, sp are
; important. these values are used for the emulator to emulate real microprocessor state 
; after micro-os_loader transfers control to this kernel (as expected).
#al=0b#
#ah=00#
#bh=00#
#bl=00#
#ch=00#
#cl=02#
#dh=00#
#dl=00#
#ds=0800#
#es=0800#
#si=7c02#
#di=0000#
#bp=0000#
#cs=0800#
#ip=0000#
#ss=07c0#
#sp=03fe#

  

; this macro prints a char in al and advances
; the current cursor position:
putc    macro   char
        push    ax
        mov     al, char
        mov     ah, 0eh
        int     10h     
        pop     ax
endm


; sets current cursor position:
gotoxy  macro   col, row
        push    ax
        push    bx
        push    dx
        mov     ah, 02h
        mov     dh, row
        mov     dl, col
        mov     bh, 0
        int     10h
        pop     dx
        pop     bx
        pop     ax
endm


print macro x, y, attrib, sdat
LOCAL   s_dcl, skip_dcl, s_dcl_end
    pusha
    mov dx, cs
    mov es, dx
    mov ah, 13h
    mov al, 1
    mov bh, 0
    mov bl, attrib
    mov cx, offset s_dcl_end - offset s_dcl
    mov dl, x
    mov dh, y
    mov bp, offset s_dcl
    int 10h
    popa
    jmp skip_dcl
    s_dcl DB sdat
    s_dcl_end DB 0
    skip_dcl:    
endm



; kernel is loaded at 0800:0000 by micro-os_loader
org 0000h

; skip the data and function delaration section:
jmp start 
; The first byte of this jump instruction is 0E9h
; It is used by to determine if we had a sucessful launch or not.
; The loader prints out an error message if kernel not found.
; The kernel prints out "F" if it is written to sector 1 instead of sector 2.
           



;==== data section =====================

; welcome message:
msg  db "Y",129,"cehon-os",96,"a ho",159, "geldiniz!" ,0 

cmd_size        equ 10    ; size of command_buffer
command_buffer  db cmd_size dup("b")
clean_str       db cmd_size dup(" "), 0
prompt          db ">", 0

; commands:
chelp    db "help", 0
chelp_tail: 

ccls     db "cls", 0
ccls_tail:
   
ccalculate    db "calculate", 0
ccalculate_tail:
  
crainbow db "rainbow", 0
crainbow_tail: 

cmouse db "mouse", 0
cmouse_tail:

cquit    db "quit", 0
cquit_tail: 

cexit    db "exit", 0
cexit_tail:

creboot  db "reboot", 0
creboot_tail:

help_msg db "Y",129,"cehon-os",96,"u tercih etti",167,"iniz i",135,"in t",159,"ekk",129,"rler!", 0Dh,0Ah
         db "Desteklenen komutlar",141,"n k",141,"sa listesi:", 0Dh,0Ah
         db "help   - bu listeyi yazd",141,"r.", 0Dh,0Ah
         db "cls    - ekran",141," temizle.", 0Dh,0Ah
         db "calculate   - girilen iki say",141," ile hesaplama yapar.", 0Dh,0Ah
         db "rainbow - g",148,"kku",159,"a",167,141," ",135,"izer.", 0Dh,0Ah
         db "mouse -  fare ile" ,135,"izim yapar..  ", 0Dh,0Ah
         db "reboot - makineyi yeniden ba",159,"lat.", 0Dh,0Ah
         db "quit   - reboot ile ayn",141,".", 0Dh,0Ah 
         db "exit   - quit ile ayn",141,".", 0Dh,0Ah

unknown  db "komut gir: " ,0    

;======================================

start:

; set data segment:
push    cs
pop     ds

; set default video mode 80x25:
mov     ah, 00h
mov     al, 03h
int     10h

; blinking disabled for compatibility with dos/bios,
; emulator and windows prompt never blink.
mov     ax, 1003h
mov     bx, 0      ; disable blinking.
int     10h


; *** the integrity check  ***
cmp [0000], 0E9h
jz integrity_check_ok
integrity_failed:  
mov     al, 'F'
mov     ah, 0eh
int     10h  
; wait for any key...
mov     ax, 0
int     16h
; reboot...
mov     ax, 0040h
mov     ds, ax
mov     w.[0072h], 0000h
jmp	0ffffh:0000h	 
integrity_check_ok:
nop
; *** ok ***
              


; clear screen:
call    clear_screen
                     
                       
; print out the message:
lea     si, msg
call    print_string


eternal_loop:
call    get_command

call    process_cmd

; make eternal loop:
jmp eternal_loop


;===========================================
get_command proc near

; set cursor position to bottom
; of the screen:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]

gotoxy  0, al

; clear command line:
lea     si, clean_str
call    print_string

gotoxy  0, al

; show prompt:
lea     si, prompt 
call    print_string


; wait for a command:
mov     dx, cmd_size    ; buffer size.
lea     di, command_buffer
call    get_string


ret
get_command endp
;===========================================

process_cmd proc    near

;//// check commands here ///
; set es to ds
push    ds
pop     es

cld     ; forward compare.

; compare command buffer with 'help'
lea     si, command_buffer
mov     cx, chelp_tail - offset chelp   ; size of ['help',0] string.
lea     di, chelp
repe    cmpsb
je      help_command

; compare command buffer with 'cls'
lea     si, command_buffer
mov     cx, ccls_tail - offset ccls  ; size of ['cls',0] string.
lea     di, ccls
repe    cmpsb
jne     not_cls
jmp     cls_command
not_cls: 
; compare command buffer with 'calculate'
lea     si, command_buffer
mov     cx, ccalculate_tail - offset ccalculate   ; size of ['calculate',0] string.
lea     di, ccalculate
repe    cmpsb
je      calculate_command 

; compare command buffer with 'mouse'
lea     si, command_buffer
mov     cx, cmouse_tail - offset cmouse   ; size of ['mouse',0] string.
lea     di, cmouse
repe    cmpsb
je      mouse_command 

; compare command buffer with 'rainbow'
lea     si, command_buffer
mov     cx, crainbow_tail - offset crainbow ; size of ['rainbow',0] string.
lea     di, crainbow
repe    cmpsb
je      rainbow_command


; compare command buffer with 'quit'
lea     si, command_buffer
mov     cx, cquit_tail - offset cquit ; size of ['quit',0] string.
lea     di, cquit
repe    cmpsb
je      reboot_command

; compare command buffer with 'exit'
lea     si, command_buffer
mov     cx, cexit_tail - offset cexit ; size of ['exit',0] string.
lea     di, cexit
repe    cmpsb
je      reboot_command

; compare command buffer with 'reboot'
lea     si, command_buffer
mov     cx, creboot_tail - offset creboot  ; size of ['reboot',0] string.
lea     di, creboot
repe    cmpsb
je      reboot_command

; ignore empty lines
cmp     command_buffer, 0
jz      processed


;////////////////////////////

; if gets here, then command is
; unknown...

mov     al, 1
call    scroll_t_area

; set cursor position just
; above prompt line:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]
dec     al
gotoxy  0, al

lea     si, unknown
call    print_string

lea     si, command_buffer
call    print_string

mov     al, 1
call    scroll_t_area

jmp     processed

; +++++ 'help' command ++++++
help_command:

; scroll text area 9 lines up:
mov     al, 9
call    scroll_t_area

; set cursor position 9 lines
; above prompt line:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]
sub     al, 9
gotoxy  0, al

lea     si, help_msg
call    print_string

mov     al, 1
call    scroll_t_area

jmp     processed




; +++++ 'cls' command ++++++
cls_command:
call    clear_screen
jmp     processed



; +++++ 'calculate' command ++++++
calculate_command:  
 

PUT    MACRO   char
        PUSH    AX
        MOV     AL, char
        MOV     AH, 0Eh
        INT     10h     
        POP     AX
ENDM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





jmp start_




msg0 db "not: hesap makinesi sadece tam say",141,"lar ile ",135,"al",141,159,141,"r.",0Dh,0Ah
msg0end:                                                
msg1 db 0Dh,0Ah, 0Dh,0Ah, "ilk say",141,"y",141," giriniz:" 
msg1end:
msg2 db "i",159,"lem se",135,"iniz: +  -  *  /     : " 
msg2end:
msg3 db  "ikinci say",141,"y",141," giriniz: "
msg3end:
msg4 db  0dh,0ah , 'hesaplama sonucu: ' 
msg4end:                                                                                                                                       
msg5 db  0dh,0ah ,"hesap makinesini kulland",141,167,141,"n",141,"z i",135,"in te",159,"ekk",129,"rler! herhangi bir tu",159,"a bas",141,"n... ", 0Dh,0Ah 
msg5end:
err1 db  "yanlis islem!", 0Dh,0Ah , ''
err1end:
smth db  "ba",159,"ka" "bir" ,159,"ey.... " 
smthend:

; operator can be: '+','-','*','/' or 'q' to exit in the middle.
opr db '?'

; first and second number:
num1 dw ?
num2 dw ?



start_:
mov al, 1
mov bh, 0
mov bl, 0000_1010b
mov cx, msg0end - offset msg0  
mov dl, 0
mov dh, 0
push cs
pop es
mov bp, offset msg0 
push cs
pop es
mov ah, 13h
int 10h


mov al, 1
mov bh, 0
mov bl, 0000_1010b
mov cx, msg1end - offset msg1 
mov dl, 0
mov dh, 2
push cs
pop es 
mov bp, offset msg1 
mov ah, 13h
int 10h 
mov ah, 01h    ; output string at ds:dx
int 16h      




  




call scan_num

; store first number:
mov num1, cx 



; new line:
put 0Dh
put 0Ah




mov al, 1
mov bh, 0
mov bl, 000_1010b
mov cx, msg2end - offset msg2  
mov dl, 0
mov dh, 4
push cs
pop es
mov bp, offset msg2 
push cs
pop es
mov ah, 13h
int 10h


; get operator:
mov ah, 00h    ; output string at ds:dx
int 16h      
mov opr, al



; new line:
put 0Dh
put 0Ah


cmp opr, 'q'      ; q - exit in the middle.
je exit

cmp opr, '*'
jb wrong_opr
cmp opr, '/'
ja wrong_opr







mov al, 1
mov bh, 0
mov bl, 0000_1010b
mov cx, msg3end - offset msg3 
mov dl, 0
mov dh, 6
push cs
pop es 
mov bp, offset msg3 
mov ah, 13h
int 10h 
mov ah, 01h    ; output string at ds:dx
int 16h  




call scan_num


; store second number:
mov num2, cx 




mov al, 1
mov bh, 0
mov bl, 0000_1010b
mov cx, msg4end - offset msg4  
mov dl, 0
mov dh, 8
push cs
pop es
mov bp, offset msg4
push cs
pop es
mov ah, 13h
int 10h 




; calculate:





cmp opr, '+'
je do_plus

cmp opr, '-'
je do_minus

cmp opr, '*'
je do_mult

cmp opr, '/'
je do_div



wrong_opr:
mov al, 1
mov bh, 0
mov bl, 0000_1010b
mov cx, err1 - offset err1  
mov dl, 0
mov dh, 6
push cs
pop es
mov bp, offset err1 
push cs
pop es
mov ah, 13h
int 10h
mov ah, 00h    
int 16h      
mov opr, al 


exit_:

mov al, 1
mov bh, 0
mov bl, 0000_1010b
mov cx, msg5end - offset msg5 
mov dl, 0
mov dh, 10
push cs
pop es 
mov bp, offset msg5 
mov ah, 13h
int 10h  


; wait for any key...
 
mov ah, 00h    
int 16h 

ret  





do_plus:


mov ax, num1
add ax, num2
call print_num    ; print ax value.

jmp exit_



do_minus:

mov ax, num1
sub ax, num2
call print_num    ; print ax value.

jmp exit




do_mult:

mov ax, num1
imul num2 ; (dx ax) = ax * num2. 
call print_num    ; print ax value.


jmp exit_




do_div:

mov dx, 0
mov ax, num1
idiv num2  ; ax = (dx ax) / num2.
cmp dx, 0
jnz approx
call print_num    ; print ax value.
jmp exit
approx:
call print_num    ; print ax value.
mov al, 1
mov bh, 0
mov bl, 0000_1010b
mov cx, smthend - offset smthend ; calculate message size.
mov dl, 0
mov dh, 12
push cs
pop es 
mov bp, offset smth 
mov ah, 13h
int 10h   
jmp exit_




SCAN_NUM        PROC    NEAR
        PUSH    DX
        PUSH    AX
        PUSH    SI
        
        MOV     CX, 0

       
        MOV     CS:make_minus, 0

next_digit:

       
        MOV     AH, 00h
        INT     16h
       
        MOV     AH, 0Eh
        INT     10h

       
        CMP     AL, '-'
        JE      set_minus

      
        CMP     AL, 0Dh  
        JNE     not_cr
        JMP     stop_input
not_cr:


        CMP     AL, 8                   
        JNE     backspace_checked
        MOV     DX, 0                   
        MOV     AX, CX                  
        DIV     CS:ten                 
        MOV     CX, AX
        PUT    ' '                     
        PUT    8                     
        JMP     next_digit
backspace_checked:


    
        CMP     AL, '0'
        JAE     ok_AE_0
        JMP     remove_not_digit
ok_AE_0:        
        CMP     AL, '9'
        JBE     ok_digit
remove_not_digit:       
        PUT    8       
        PUT    ' '   
        PUT    8               
        JMP     next_digit        
ok_digit:


        
        PUSH    AX
        MOV     AX, CX
        MUL     CS:ten                  
        MOV     CX, AX
        POP     AX

      
        CMP     DX, 0
        JNE     too_big

       
        SUB     AL, 30h

     
        MOV     AH, 0
        MOV     DX, CX      
        ADD     CX, AX
        JC      too_big2    

        JMP     next_digit

set_minus:
        MOV     CS:make_minus, 1
        JMP     next_digit

too_big2:
        MOV     CX, DX     
        MOV     DX, 0       
too_big:
        MOV     AX, CX
        DIV     CS:ten  
        MOV     CX, AX
        PUT    8       
        PUT    ' '     
        PUT    8               
        JMP     next_digit 
        
        
stop_input:
      
        CMP     CS:make_minus, 0
        JE      not_minus
        NEG     CX
not_minus:

        POP     SI
        POP     AX
        POP     DX
        RET
make_minus      DB      ?     
SCAN_NUM        ENDP







PRINT_NUM       PROC    NEAR
        PUSH    DX
        PUSH    AX

        CMP     AX, 0
        JNZ     not_zero

        PUT    '0'
        JMP     printed

not_zero:
        
   
        CMP     AX, 0
        JNS     positive
        NEG     AX

        PUT    '-'

positive:
        CALL    PRINT_NUM_UNS
printed_:
        POP     AX
        POP     DX
        RET
PRINT_NUM       ENDP




PRINT_NUM_UNS   PROC    NEAR
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX

        MOV     CX, 1

        
        MOV     BX, 10000       

       
        CMP     AX, 0
        JZ      print_zero

begin_print:

        
        CMP     BX,0
        JZ      end_print

       
        CMP     CX, 0
        JE      calc
        
        CMP     AX, BX
        JB      skip
calc:
        MOV     CX, 0   

        MOV     DX, 0
        DIV     BX      

        
        ADD     AL, 30h    
        PUT    AL


        MOV     AX, DX  

skip:
        
        PUSH    AX
        MOV     DX, 0
        MOV     AX, BX
        DIV     CS:ten  
        MOV     BX, AX
        POP     AX

        JMP     begin_print
        
print_zero:
        PUT    '0'
        
end_print:

        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
PRINT_NUM_UNS   ENDP



ten             DW      10      







GET_STRING_      PROC    NEAR
PUSH    AX
PUSH    CX
PUSH    DI
PUSH    DX

MOV     CX, 0                   

CMP     DX, 1                  
JBE     empty_buffer_            

DEC     DX                      




wait_for_key_:

MOV     AH, 0                   
INT     16h

CMP     AL, 0Dh                 
JZ      exit_GET_STRING_


CMP     AL, 8                   
JNE     add_to_buffer_
JCXZ    wait_for_key_            
DEC     CX
DEC     DI
PUT    8                      
PUT    ' '                     
PUT    8                       
JMP     wait_for_key_

add_to_buffer_:

        CMP     CX, DX         
        JAE     wait_for_key_    

        MOV     [DI], AL
        INC     DI
        INC     CX
        
        
        MOV     AH, 0Eh
        INT     10h

JMP     wait_for_key_


exit_GET_STRING_:


MOV     [DI], 0

empty_buffer_:

POP     DX
POP     DI
POP     CX
POP     AX
RET
GET_STRING_      ENDP





jmp processed



; +++ 'quit', 'exit', 'reboot' +++
reboot_command:
call    clear_screen
print 5,2,0011_1111b," please eject any floppy disks "
print 5,3,0011_1111b," and press any key to reboot... "
mov ax, 0  ; wait for any key....
int 16h

; store magic value at 0040h:0072h:
;   0000h - cold boot.
;   1234h - warm boot.
mov     ax, 0040h
mov     ds, ax
mov     w.[0072h], 0000h ; cold boot.
jmp	0ffffh:0000h	 ; reboot!

; ++++++++++++++++++++++++++

processed:
ret

;===========================================

; scroll all screen except last row
; up by value specified in al

scroll_t_area   proc    near

mov dx, 40h
mov es, dx  ; for getting screen parameters.
mov ah, 06h ; scroll up function id.
mov bh, 07  ; attribute for new lines.
mov ch, 0   ; upper row.
mov cl, 0   ; upper col.
mov di, 84h ; rows on screen -1,
mov dh, es:[di] ; lower row (byte).
dec dh  ; don't scroll bottom line.
mov di, 4ah ; columns on screen,
mov dl, es:[di]
dec dl  ; lower col.
int 10h

ret
scroll_t_area   endp

;===========================================




; get characters from keyboard and write a null terminated string 
; to buffer at DS:DI, maximum buffer size is in DX.
; 'enter' stops the input.
get_string      proc    near
push    ax
push    cx
push    di
push    dx

mov     cx, 0                   ; char counter.

cmp     dx, 1                   ; buffer too small?
jbe     empty_buffer            ;

dec     dx                      ; reserve space for last zero.


;============================
; eternal loop to get
; and processes key presses:

wait_for_key:

mov     ah, 0                   ; get pressed key.
int     16h

cmp     al, 0Dh                 ; 'return' pressed?
jz      exit


cmp     al, 8                   ; 'backspace' pressed?
jne     add_to_buffer
jcxz    wait_for_key            ; nothing to remove!
dec     cx
dec     di
putc    8                       ; backspace.
putc    ' '                     ; clear position.
putc    8                       ; backspace again.
jmp     wait_for_key

add_to_buffer:

        cmp     cx, dx          ; buffer is full?
        jae     wait_for_key    ; if so wait for 'backspace' or 'return'...

        mov     [di], al
        inc     di
        inc     cx
        
        ; print the key:
        mov     ah, 0eh
        int     10h

jmp     wait_for_key
;============================

exit:


mov     [di], 0

empty_buffer:

pop     dx
pop     di
pop     cx
pop     ax
ret
get_string      endp




print_string proc near
push    ax      
push    si     

next_char:      
        mov     al, [si]
        cmp     al, 0
        jz      printed
        inc     si
        mov     ah, 0eh 
        int     10h
        jmp     next_char
printed:

pop     si      
pop     ax      

ret
print_string endp



clear_screen proc near
        push    ax      
        push    ds      
        push    bx      
        push    cx      
        push    di      

        mov     ax, 40h
        mov     ds, ax  
        mov     ah, 06h 
        mov     al, 0   
        mov     bh, 1001_1111b  
        mov     ch, 0   
        mov     cl, 0   
        mov     di, 84h 
        mov     dh, [di] 
        mov     di, 4ah 
        mov     dl, [di]
        dec     dl      
        int     10h

        
        mov     bh, 0   
        mov     dl, 0   
        mov     dh, 0   
        mov     ah, 02
        int     10h

        pop     di      
        pop     cx      
        pop     bx      
        pop     ds      
        pop     ax      

        ret
clear_screen endp

;+++++ 'rainbow' ++++++
rainbow_command:
                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                         
                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                       

Rainbow_screen proc near                                                                                                                                                                                                                                                                                                
    mov     ax, 3                                                                                                                                                                                                                                                                                                                          
    int     10h                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                       
    mov     ax, 1003h                                                                                                                                                                                                                                                                                                                      
    mov     bx, 0                                                                                                                                                                                                                                                                                                       
    int     10h                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                           
    mov     dl, 0                                                                                                                                                                                                                                                                                                         
    mov     dh, 0                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                           
    mov     bl, 0                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                                           
    jmp     next_chara                                                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                                                                           
    next_rowa:                                                                                                                                                                                                                                                                                                                              
    inc     dh                                                                                                                                                                                                                                                                                                                             
    cmp     dh, 16                                                                                                                                                                                                                                                                                                                         
    je      stop_print                                                                                                                                                                                                                                                                                                                     
    mov     dl, 0                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                           
    next_chara:                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                         
    mov     ah, 02h                                                                                                                                                                                                                                                                                                                        
    int     10h                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                           
    mov     al, ' '                                                                                                                                                                                                                                                                                                                        
    mov     bh, 0                                                                                                                                                                                                                                                                                                                          
    mov     cx, 1                                                                                                                                                                                                                                                                                                                          
    mov     ah, 09h                                                                                                                                                                                                                                                                                                                        
    int     10h                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                           
    inc     bl                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                           
    inc     dl                                                                                                                                                                                                                                                                                                                             
    cmp     dl, 16                                                                                                                                                                                                                                                                                                                         
    je      next_rowa                                                                                                                                                                                                                                                                                                                      
    jmp     next_chara                                                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                                                                           
    stop_print:                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                        
    mov     dl, 10                                                                                                                                                                                                                                                                                                                
    mov     dh, 5                                                                                                                                                                                                                                                                                                                    
    mov     ah, 02h                                                                                                                                                                                                                                                                                                                        
    int     10h                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                         
    mov     al, 'x'                                                                                                                                                                                                                                                                                                                        
    mov     ah, 0eh                                                                                                                                                                                                                                                                                                                        
    int     10h                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                 
    mov ah, 0                                                                                                                                                                                                                                                                                                                              
    int 16h                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                           
    ret
Rainbow_screen endp   
jmp processed 

;+++++ 'mouse' ++++++
mouse_command:
; mouse drawing.
; press left mouse button to draw.


jmp startm

oldX dw -1
oldY dw 0


startm:
mov ah, 00
mov al, 13h        
int 10h


mov ax, 0
int 33h
cmp ax, 0




check_mouse_button:
mov ax, 3
int 33h
shr cx, 1       
cmp bx, 1
jne xor_cursor:
mov al, 1111b   ; pixel color
jmp draw_pixel
xor_cursor:
cmp oldX, -1
je not_required
push cx
push dx
mov cx, oldX
mov dx, oldY
mov ah, 0dh     ; get pixel.
int 10h
xor al, 1111b   ; pixel color
mov ah, 0ch     ; set pixel
int 10h
pop dx
pop cx
not_required:
mov ah, 0dh     ; get pixel.
int 10h
xor al, 1111b   ; pixel color
mov oldX, cx
mov oldY, dx
draw_pixel:
mov ah, 0ch     ; set pixel
int 10h
check_esc_key:
mov dl, 255
mov ah, 6
int 10h
cmp al, 27     
jne check_mouse_button


stopm:

mov ax, 3 
int 10h

mov ah, 1
mov ch, 0
mov cl, 8
int 10h
mov dx, offset msg
mov ah, 9
int 10h
mov ah, 0
int 16h
ret

msgm db " Herhangi bir tu",159,"a bas",141,"n...."

jmp processed 