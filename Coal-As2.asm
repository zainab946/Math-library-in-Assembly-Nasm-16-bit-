;Name  : Zainab Zahid
;CMS_ID: 343162
;----------------------------------------------------------------------NASM 16 BIT MATH LIBRARY-----------------------------------------------------------------------------------------------------
 [org 0x0100] 
 x   : dd 0
num  : dd 0
perp : dd 0
base : dd 0
a    : dd 0
b    : dd 0
msg  : dw ''
iota : dd 0
str2 : dw ''
Cstr1: db ''             	 ;string1 to compare for function25         
lenCstr1: equ   $-Cstr1 	 ;length str1
Cstr2 :db ''             	 ;string2 to compare for function25
lenCstr2: equ   $-Cstr2  	 ;length str2
msg1:      db   'EQAUl',10 	 ;msg1       
lenmsg1:   equ   $-msg1 	 ;length msg1
var1   :   dd 0
var2   :   dd 0
var3   :   dd 0
var4   :   dw 0
infin  :   dd 0
msg2:      db   'NOT EQUAL',10  ;msg2
lenmsg2:   equ   $-msg2  	 ;length msg2
buff   :   dw 100  
bufflen:   equ   $-buff             ;buffer is just an array
data
string :   dw ''   ;for uppercase function23
;---------------------1-SINE FUNCTION---------------------------------
;1-SINE FUNCTION
sin:                     ;√(1-x^2)
      push bp
      mov bp,sp
      push dword [x]
      pusha              ;push all general purpose registers
      mov cx,1              
      mov bx,[x]         ;moving the value of x into bx register
      mov dx,[x]         ;moving the value of x into dx register to take square
      mul dx             ; x^2  is moved in dx
      sub cx,dx          ;1-x^2
      call isqrt         ;√1-x^2
      popa               ;pop all general purpose registers
      pop dword [x]
      mov sp,bp
      pop bp
      ret
;---------------------2-COS FUNCTION---------------------------------
;2-COS FUNCTION
cos:                     ;√1-x^2
       push bp
       mov bp,sp
       push dword [x]
       pusha             ;push all general purpose registers
                 
       mov cx,1
       mov bx,[x]
       mov dx,[x]        ;moving the value of x into dx register to take square
       mul dx            ; x^2  is moved in dx
       sub cx,dx         ;1-x^2
       call isqrt        ;√1-x^2
       popa              ;pop all general purpose registers
       pop dword [x]
       mov sp,bp
       pop bp
       ret
;-------------------3-TANGENT FUNCTION-----------------------------
tan:                    ;1+x^2
       push bp
       mov bp,sp
       push dword [x]
       pusha            ;push all gprs
       mov cx,1
       mov bx,[x]
       mov dx,[x]
       mul dx           ;x^2
       add cx,dx        ;1+x^2
       popa             ;pop all general purpose registers
       pop dword [x] 
       mov sp,bp
       pop bp
       ret
;----------------4-SQURARE ROOT FUNCTION(sqrt)---------------------
isqrt:
        push bp
        mov bp,sp
        push dword [num]
        pusha           ;push all general purpose registers      
        mov ax, [num]
        xor bx, bx
        bsr cx, ax	 ;bit scan reverse instruction
        and cl, 0feh		
        mov dx, 1
        shl dx, cl      ;shift left 
refine:
        mov si, bx
        add si, dx
        cmp si, ax
        ja @@
        sub ax, si
        shr bx, 1
        add bx, dx                                                                                                             
        jmp next
@@:
        shr bx, 1
next :
        shr dx, 2
        jnz refine
        mov ax, bx
        popa
        pop dword [num]
        mov sp,bp
        pop bp
        ret
;-----------------5-INT ABSOLUTE FUNCTION---------------------------
 iabs:                ;|a|
 	push bp
 	mov bp,sp
        push ax       ;cdq copies the sign of the register ax to register dx. 
        push dx     
        cdq           ;dx will be 0xFFFF which denotes -1. The xor operation with the origin
        xor ax, dx    ;number will change nothing if it is a positive number 
        sub ax, dx    ;Xor will have no effect
        pop dx
        pop ax
        mov sp,bp
        pop bp
        ret
;------------------6-POWER FUNCTION(pow)------------------------------
 power:                 
        push bp
        mov bp,sp
        sub sp,4
        mov bx,[bp+8]   ;put the first element in bx that is the base
        mov cx,[bp+12]  ;put the second element in cx that is the exponent
        mov [bp-4],bx   ;store the current result
 
 power_loop_start:
        cmp cx,1        ;if the power is one return
        je end_power
        mov ax,[bp-4]   ;move the result in ax
        imul ax,bx      ;multiply the current result by the base number
        mov [bp-4],ax   ;store the current result
        dec cx
        jmp power_loop_start
 
end_power:
       mov ax,[bp-4]
       mov sp,bp
       pop bp
       ret
;-------------------7-HYPOTENUSE(hypot)---------------------------------- 
hyp:                    ; hyp=√perp^2+base^2
       push bp
       mov bp,sp
       push dword [perp]
       push dword [base]
       pusha  
       mov [perp],ax
       mov [perp],bx    ;moving perp in ax and bx registers for taking square
       mul bx           ;perp^2 in bx
       mov [base],cx
       mov [base],dx 
       mul dx           ;base^2 in dx
       add bx,dx        ;perp^2+base^2
       call isqrt       ;√perp^2+base^2
       mov ax,bx        ;store the result back in ax
       popa             ;pop all general purpose registers
       pop dword [base]
       pop dword [perp]
       mov sp,bp
       pop bp
       ret
;--------------------8-FLOOR FUNCTION(floor)------------------------------
floor: 
	push bp
	mov bp,sp
	push dword [var1]
	push dword [var2]
	push dword [var3]
	push word  [var4]
	pusha
	fld  dword [var3]	;loading floating point value into floating point unit register st	
	fadd st1 ,st0          ;pg#964 of intel ref manual
	fadd   dword [infin]
	fistp word [var4]	;storing integer pg# 959 of intel ref manual
	sar  word [var4],1
	popa
	pop dword [var1]
	pop dword [var2]
	pop dword [var3]
	pop word [var4]
	mov sp,bp
	pop bp
	ret
;-----------------9-Floating ABSOLUTE FUNCTION---------------------------
fabs2: 
	push bp
	mov bp,sp
	push dword [var1]
	push dword [var2]
	pusha 
	movups xmm0,[var1]
	movups xmm1,[var2]					
	xorps xmm0,xmm1
	subps xmm1,xmm0
	popa
	pop dword [var1]
	pop dword [var2]
	mov sp,bp
	pop bp
	ret
;--------------------10-ARC COS FUNCTION--------------------------------
arccos:                  ;-1/√(1-x^2)
       push bp
       mov bp,sp
       push dword [x]
       pusha             ;push all general purpose registers
                 
       mov cx,1
       mov [x],bx
       mov [x],dx        ;moving the value of x into dx register to take square
       mul dx            ; x^2  is moved in dx
       sub cx,dx         ;1-x^2
       call isqrt        ;√1-x^2
       mov ax,-1 
       div cx            ;-1/sqrt(1-x^2)
       mov ax,cx         ;mov the result back in ax
       popa              ;pop all general purpose registers
       pop dword [x]
       mov sp,bp
       pop bp
       ret
;---------------------11-ARC SINE FUCNTION-------------------------------
arcsin:                  ;1/√(1-x^2)
      push bp
      mov bp,sp
      push dword [x]
      pusha              ;push all general purpose registers
      mov cx,1              
      mov [x],bx         ;moving the value of x into bx register
      mov [x],dx         ;moving the value of x into dx register to take square
      mul dx             ; x^2  is moved in dx
      sub cx,dx          ;1-x^2
      call isqrt         ;√1-x^2
      mov ax,1 
      div cx             ;1/√(1-x^2)
      mov ax,cx          ;mov the result back in ax
      popa               ;pop all general purpose registers
      pop dword [x]
      mov sp,bp
      pop bp
      ret
;-------------------12-ARC TANGENT FUNCTION(atan)-----------------------------
arctan:                  ;1/1+x^2
       push bp
       mov bp,sp
       push dword [x]
       pusha             ;push all gprs
       mov cx,1
       mov [x],bx
       mov [x],dx
       mul dx            ;x^2
       add cx,dx        ;1+x^2
       mov ax,1
       div cx            ;1/1+bx^2
       mov ax,cx         ;move the result in ax
       popa              ;pop all general purpose registers
       pop dword [x]
       mov sp,bp
       pop bp
       ret
;---------------------13-ARC TAN2(atan2)------------------------------------
atan2: 
	push bp
	mov bp,sp
	push dword [x]
	push dword [a]
	push dword [b]
	pusha
	mov [a],ax
	mov [b],bx
	div bx            ;div ax\bx== double a/double b
	mov [x], bx       ;move the resultant value in x variable
	call arctan       ;call arctan function on the result to achieve atan2
	popa
	pop dword [b]
	pop dword [a]
	pop dword [x]
	mov sp,bp
	pop bp
	ret
;---------------------14-CEIL Function(ceil)-----------------------------------
ceil:

	push bp 
	mov bp,sp
	push dword [var1]
	push dword [var2]
	push dword [var3]
	push word  [var4]
	pusha
	fld  dword [var3]
	fadd st1,st0
	fsubr dword [infin] 	;reverse subtracting to make the ceiling possible;
	fistp word [var4]      ;because of logic from this link https://www.geeksforgeeks.org/find-ceil-ab-without-using-ceil-func/
	sar word [var4],1
	neg word [var4]
	popa
	pop dword [var1]
	pop dword [var2]
	pop dword [var3]
	pop word [var4]
	mov sp,bp
	pop bp
	ret
;----------------15-COS HYPERBOLIC FUNCTION(cosh)-------------------------------
cosh:		    ;cos(ix)
       push bp
       mov bp,sp
       push dword [x]
       pusha                  ;push all general purpose registers
       mov ax,[x]
       mov bx,-1
       call isqrt             ;√-1 that is equal to iota now iota is stored in bx
       mul ax                 ;result of i*x will be stored in ax
       call cos               ;cos(ix) --result stored in ax
       popa
       pop dword [x]
       pop dword [iota]
       mov sp,bp
       pop bp
       ret   
;-----------------16-TAN HYPERBOLIC FUNCTION(tanh)-------------------------------
tanh:		              ;-itan(ix)
       push bp
       mov bp,sp
       push dword [iota]
       push dword [x]
       pusha                  ;push all general purpose registers
       mov ax,[x]
       mov bx,-1              
       call isqrt             ;√-1 that is equal to iota now iota is stored in bx
       mov [iota],bx          ; store the value in iota variable 
       mul ax                 ;result of i*x will be stored in ax
       call tan               ;tan(ix) --result stored in ax
       mov bx,-1
       mov dx,[iota]    
       mul dx                 ;this gives us -i
       mul ax                 ;this gives us  -itan(ix) (ax contained tan(ix) and dx was containing -i)
       popa
       pop dword [x]
       pop dword [iota]
       mov sp,bp
       pop bp
       ret
;--------------------17-LOG FUNCTION(log)-----------------------
log: 		              ;ln=1/x i.e., formula of natural log 
	push bp
	mov bp,sp
	push dword [x]
	pusha
	mov cx,1
	mov ax,[x]
	div ax                ;1/x
	mov dx,ax 	       ;mov the result back in ax
	popa 
	pop dword [x]
	mov sp,bp
	pop bp
	ret	
;---------------18-Random Number Function(rand)------------------
randgen: 
                              ; generate a rand no using the system time
randstart:
       mov ah, 00h  	       ; interrupts to get system time        
       int 1AH      	       ; CX:DX now hold number of clock ticks    
       mov bh, 57             ; set limit to 57 (ASCII for 9) 
       mov ah, dl  
       cmp ah, bh             ; compare with value in  DL,      
       ja randstart           ; if more, regenerate. if not, continue... 
       mov bh, 49             ; set limit to 48 (ASCII FOR 0)
       mov ah, dl  
       cmp ah, bh             ; compare with value in DL
       jb randstart           ; if less, regenerate.
       mov ah, 2h             ; call interrupt to display a value in DL
       int 21h    
       ret
;---------------19-INT LENGTH OF STRING----------------------------
lengthofstring:
       push bp 
       mov bp,sp 
       push es 
       push cx 
       push di 
       les di, [bp+4]       ; point es:di to string 
       mov cx, 0xffff       ; load maximum number in cx 
       xor al, al           ; load a zero in al 
       repne scasb          ; find zero in the string 
       mov ax, 0xffff       ; load maximum number in ax 
       sub ax, cx           ; find change in cx 
       dec ax               ; exclude null from length 
       pop di 
       pop cx 
       pop es
       mov sp,bp 
       pop bp 
       ret 4
              
;--------------20-PRINT STRING FUNCTION---------------------------
printstring:
       push bp
       mov bp,sp
       push word [msg]
       pusha
       mov dx,msg           ;move the msg in dx   
       call lengthofstring  ;call len function instead of moving length in cx
       mov bx,1             
       mov ah,0x40          ;write
       int 0x21             ;interupt call
       popa
       pop word [msg]
       popa
       mov sp,bp
       ret
;--------------21-Read(read)--------------------------------------
read:
       push bp
       mov bp,sp
       push word[buff]
       pusha
       xor ax, ax         ; ax <- 0 (syscall number for 'read')
       xor di, di         ; di <- 0 (stdin file descriptor)
       mov si,buff        ; si <- address of the buffer.  lea rsi, [rel buffer]
       mov bx,bufflen     ; dx <- size of the buffer
       syscall            ; execute  read equal to int21h
       popa
       pop word [buff]
       mov sp,bp
       pop bp 
       ret
;----------------22-STRING COPY(strcpy)-----------------------------
StringCopy:
       push bp
       mov bp,sp
       push word [str2]
       push word  [buff]
       pusha 
       
start:	
       ;mov ax, [data]
       mov ds, ax
       lea si, [str2]    ;lea (load effective address) instruction is
       lea di, [buff]    ;used to put a memory address into the destination
;copy str1 to bufferb
cpy_nxt:
       mov bl,[si]       ;copy source to destination
       mov [di],bl
       inc si            ;increment source and destination
       inc di	
       dec cx            ;decrement count
       jnz cpy_nxt       ;if not zero goto next bit
      
quit:
       popa
       pop word [buff]
       pop word [str2]
       mov sp,bp
       pop bp
       ret
;---------------23-UPPERCASE FUNCTION(uppercase_str)-----------------
UpperCase:
       push bp
       mov bp,sp
       push word [string]
       pusha         
       mov ax, data
       mov ds, ax          
upper_case: 
       mov bx,string
       cmp [bx],  byte '$'
       je done
; check if it's a lower case letter:
       cmp byte  [bx], 'a'
       jb ok
       cmp byte [bx], 'z'
       ja ok
       and byte  [bx], 11011111b
ok:
       inc bx ; next char.
       jmp upper_case   
done:
       lea dx, [string]
       mov ah, 09h
       int 21h
       popa
       pop word [string]
       mov sp,bp
       ret
;---------------24-LOWERCASE FUNCTION(lowercase_str)--------------------
LowerCase:
       push bp
       mov bp,sp
       push word [string]
       pusha
                      
       mov ax, data
       mov ds, ax          
lowercase: 
       mov bx,string
       cmp [bx],  byte '$'
       je done
; check if it's a Uppercase letter:
       cmp byte  [bx], 'A'
       jb yes
       cmp byte [bx], 'Z'
       ja yes
       sub byte  [bx], 0x20
yes:
       inc bx ; next char.
       jmp lowercase   
done1:
       lea dx, [string]
       mov ah, 09h
       int 21h
       popa
       pop word [string]
       mov sp,bp
       ret
;----------------25-STRING EQUAL(isEqual)-----------------------------
StringEqual:
      push bp
      mov bp,sp
      pusha
      mov si,Cstr1        ;Move string1 to compare in si
      mov di,Cstr2        ;Move string2 to compare in di
      mov cx,lenCstr2+1
      cld                 ;Clears the direction flag; affects no other flags or registers. Causes all subsequent string operations to increment the index registers
      repe cmpsb          ;Repe stands for repeat string operation and cmpsb is used to compare
      jecxz equal         ;Jump short if CX register is 0

 
notequal:
      mov ax,4
      mov bx,1
      mov cx,msg2
      mov dx,lenmsg2
      int 0x21
      jmp exit

equal:       
     mov ax,4
     mov bx,1
     mov cx,msg1
     mov dx,lenmsg1
     int 0x21
exit:
    ret
;-------------26-STRING TO INT(str2int)-----------------------------
StringToInt:
      mov  cx,0    ;count digits in string 
find_dollar:                                          
     inc  cx       ;count digits
     inc  si       ;next char
     mov  bl,[si]
     cmp  bl, '$'
     jne find_dollar ;if bl!='$'jump.
     dec  si       ;BECAUSE IT WAS OVER '$', NOT OVER THE LAST DIGIT.

;CONVERT STRING.
     mov  bx, 0
     mov  bp, 1    ;MULTIPLE OF 10 TO MULTIPLY EVERY DIGIT.
repeat:         
;CONVERT CHARACTER.                    
     mov  ax, 0    ;now ax=a'
     mov  al,[si]  ;char to convert.
     sub  al, 48   ;convert ascii char to digit
     mul  bp       ;ax*bp= dx:ax
     add  bx,ax    ;add result to bx
     mov  ax,bp
     mov  bp,10
     mul  bp      ;ax*10 =dx:ax
     mov  bp,ax   ;new multiple of 10.  

     dec  si      ;next char to process
     loop repeat  ;if not zero then repeat
     ret
;-------------27-INT TO STRING(int2str)------------------------------  
IntTostring:
     push bp
     mov bp,sp
     push ax
     push bx
     push cx
     mov bx,10   ;extracting digits by dividing with 10
     mov cx,0    ;counter for extracting digits
c1:       	    ;cycle1
     mov dx,0    ;necessary to div by bx.
     div bx      ;dx:ax/10 =ax:quotient dx:remainder.
     push dx     ;preserve digit extracted by dl
     inc cx      ;increase the counter for every digit extracted
     cmp ax,0    ;
     jne c1      ;if not zero jmp  
;rereive rem digits.
      mov  si,[buff] ;offset buffer
c2:  		    ;cycle2
      pop dx        
      add dl,48    ;convert digit to char.
      mov [si],dl
      inc si
      loop c2 
      pop cx
      pop bx
      pop ax
      mov sp,bp
      pop bp 
      ret
;--------------------THE END---------------------------------------------
