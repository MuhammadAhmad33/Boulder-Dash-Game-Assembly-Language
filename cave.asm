[org 0x100] 

jmp start 

file_buffer: times 2000 db 0

scoresize:dw 6
scorecount:dw 0
tickcount: dw 0
check: dw 0
index: dw 1440 ;player pos
restore: dw 0
oldisr: dd 0
stop: dw 0
flag: dw 0

player_pos: dw 0 

intro   : db "    >                        BOULDER DASH NUCES EDITION          <$",0
sgame   : db ">                          START GAME  (YOU HAVE 3 LIVES)                      <$",0
cavename: db " Enter cave fle name or press enter to use the default: $",0
message : db " Error in File read                                                            $",0
message1: db " Press any key to start game:                                                  $",0
message2: db " Incomplete File                                                               $",0
message3: db " Error in File open                                                            $",0
message4: db "   Using default file                                                            $",0

close: db "CLOSING FILE $",0

messagee: db " File Not Found $",0

buffer: db 64 
		db 64
		times 64 db 0
		
paus : db " GAME PAUSED $",0
res :  db " PLAY GAME   $",0
paus_size: db 46

life: db "Remaining Lives:$",0
lifesize:dw 16
lifecount:dw 3
nextlev : db ' LEVEL CLEARED !',0
string1 :db 'GAME END!',0
string2 :db "LEVEL CLEARED!$",0
str1:db 'cave1.txt',0  

str2:db 'Score:$' ,0

flag1: dw 0

score: db "Score:"
bell:db 7,'$'

;---------------------------------------------------------



start: 

call clrscr
;----------------------

; push intro
; call print 
; push sgame
; call print 
; push cavename
; call print 
; ;-----------------------


; ;-----------------------------------

; mov ah,0Ah			; buffered input
; mov dx,buffer
; int 21h

; ;-----------------------------------

; mov dx,	buffer+2
; add dx, [buffer+1]
; mov dx,	'0'          ;replace CR with 0

; mov ah,3dh
; mov al,0
; mov dx , buffer+2
; int 21h

; jc endl1 ; working

; ;----------------------------------
; mov bx,ax

; mov ah,3Fh			;READ file
; mov bx,buffer+2
; mov cx, 1600
; mov dx, file_buffer
; int 21h

; jc end2 ; file read
; cmp ax,1600 
; jl end3	; file incomplete read
; ;----------------------------------

; ; default:		; using default file

; ; mov ah,9
; ; mov dx,message4
; ; int 21h

; ; mov ah,3Fh
; ; mov bx,str1
; ; mov cx, 1600

; ; mov dx, file_buffer
; ; int 21h
; ;----------------------------------

; push message1
; call print
; jmp ll3

; end3:
; push message2		;incomplete file
; call print
; jmp ll3

; end2:
; push message		;error in file read
; call print 
; jmp ll3

; endl1:
; push messagee ; file not found
; call print
; jmp end1

; ll3:
; jmp end1

;***********************************************************************************************

call clrscr
;push message4 ; using default file 
;call print

mov ah,3Dh  ;OPEN FILE
mov al,0;read mode
mov dx,str1
int 21h  

jc endl2 

mov bx,ax			; moving file handle
mov ah,3Fh  ;READ FILE
mov cx,2000 
mov dx,file_buffer
int 21h 


jc endllllll ; Error in file read

; cmp ax,cx
; jl end_l3	;incomplete file

; push message1						;press any key to start game
; call print 						
; in al,0x60

; close_file:
; mov ah,3Eh  ;CLOSE FILE
; int 21h 

; jc end_error1

push intro
call print
;call printboundary
call printlayout

mov ch,32  ;make cursor invisible
mov ah,1
int 10h

; mov ah,09
; mov dx,str2
; int 21h 

call lifeBoard
call ScoreBoard

xor ax, ax
mov es, ax
mov ax, [es:9*4]
mov [oldisr], ax
mov ax, [es:9*4+2]
mov [oldisr+2], ax
xor ax, ax
mov es, ax
cli
mov word [es:9*4], kbisr
mov [es:9*4+2], cs
sti
jmp l7
end_error1:
jmp end_error
end_l3:
jmp endl3

l7:
cmp word[flag],1
jne l7

mov ax,[flag]

jmp X
endllllll:
jmp end11

endl2:
; push messagee		;file not found
; call print 
jmp end1

; cmp word[flag1],1
; je nxt
X:
call clrscr
push string1
push 9
call printstr

; nxt:
; call clrscr
; push string2
; push 14
; call printstr

mov ax,[oldisr]
mov bx,[oldisr+2]
cli
mov [es:9*4],ax 
mov [es:9*4+2],bx
sti
jmp end1

endl3:
push message2	;incomplete file
call print

end12:
push messagee		;file not found 
call print
jmp end1

end11:
; push message		;error in file read
; call print 
jmp end1


end_error:
push close
call print 


end1:
mov ax,0x4c00 
int 21h   
;=====================================================clear screen====================================================== 




printboundary:
mov ax,0xb800
mov es,ax
mov di,480
mov ax,0x0723
loop1:mov word[es:di],ax
add di,2
cmp di,638
jne loop1
mov word[es:di],ax
mov cx,0
ml1:add di,160
mov word[es:di],ax
add cx,1
cmp cx,21
jne ml1
;mov word [es:di],ax
mov cx,79
ml2:sub di,2
mov word[es:di],ax
sub cx,1
jnz ml2
mov cx,21
mle2:sub di,160
mov word[es:di],ax
sub cx,1
jnz mle2








printlayout:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	mov ax, 0xb800
	mov es, ax 						; point es to video base
	
	mov si, file_buffer 			; point si to string
	mov cx,1600
	mov di, 320 					; location
	mov ah,0x07
	
nextchar3:
	mov al, [si] 					; load next char of string
	
	;checks--------------------------------------------------

	c1:
	cmp al,	120 					; x -> dirt (free move)
	jne c2
	mov word [es:di], 0x7000  			; grey ____________________________
	jmp nn
	
	c2:
	cmp al, 0x52 					; R -> rockford (location)
	jne c3
	mov word[es:di], 0x0402 				; smiley ____________________________________________
	mov word[player_pos],di
	jmp nn
	
	c3:
	cmp al, 84 					; T -> exit (exit door)
	jne c4
	mov word[es:di], 027Fh				; Green
	jmp nn	
	
	c4:
	cmp al, 0x42 					; B -> boulder ( come under to die )
	jne c5
	mov word[es:di], 0x0509			; boulder
	jmp nn
	
	c5:
	cmp al, 0x44 					; D -> diamond (score ++)
	jne c6 
	mov word[es:di], 0x0304 			; blue
	jmp nn	
	
	c6:
	cmp al, 87 					; W -> wall (can not move out of it)
	jne c7
	mov word[es:di], 0xE600		; Wall
	jmp nn
	
	c7:
	mov word[es:di],0x05		;otherwise print wall

nn:
	add di, 2 						; move to next screen location
	add si, 1 						; move to next char in memory
	loop nextchar3
	
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 
	
;____________________________________________


clrscr:
        push es
		push ax
		push cx
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		xor di, di ; point di to top left column
		mov ax, 0x0720 ; space char in normal attribute
		mov cx, 2000 ; number of screen locations
		cld ; auto increment mode
		rep stosw ; clear the whole screen
		pop di
		pop cx
		pop ax
		pop es
		ret
		
ScoreBoard:
		pusha
		push 0xb800
		pop es
		mov si,score
		mov cx,[scoresize]
		mov di,140
		mov ah,7
		cld
		SB:
		    lodsb
			stosw
			loop SB
		
		mov bx,[scorecount]
		push bx
		call printnum

		
		popa
		ret


lifeBoard:
		pusha
		push 0xb800
		pop es
		mov si,score
		
		mov si,life 
		mov cx,[lifesize]
		mov di,4
		mov ah,7
		cld
		SB1:
		    lodsb
			stosw
			loop SB1
		
		mov bx,[lifecount]
		push bx
		call printlives

		popa
		ret



printlives: 
  ;      push bp
;		mov bp, sp
;		push es
;		push ax
;		push bx
;		push cx
;		push dx
;		push di
;		
;		mov di, 36
;		mov ax, 0xb800
;		mov es, ax ; point es to video base
;		mov dx, [bp+4] ; load number in ax
;		
;		mov dh, 0x07 ; use normal attribute
;		mov [es:di], dx ; print char on screen
;		
;		pop di
;		pop dx
;		pop cx
;;		pop bx
;		pop ax
;		pop es
;		pop bp
;		ret 2



push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit1: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit1 ; if no divide it again
mov di, 36
nextpos3: pop dx ; remove a digit from the stack
mov dh, 0x07 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos3 ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 2
;____________________________________________


printnum: 
        push bp
		mov bp, sp
		push es
		push ax
		push bx
		push cx
		push dx
		push di
		mov di,152
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov ax, [bp+4] ; load number in ax
		mov bx, 10 ; use base 10 for division
		mov cx, 0 ; initialize count of digits
		nextdigit: mov dx, 0 ; zero upper half of dividend
		div bx ; divide by 10
		add dl, 0x30 ; convert digit into ascii value
		push dx ; save ascii value on stack
		inc cx ; increment count of values
		cmp ax, 0 ; is the quotient zero
		jnz nextdigit ; if no divide it again
		nextpos: pop dx ; remove a digit from the stack
		mov dh, 0x07 ; use normal attribute
		mov [es:di], dx ; print char on screen
		add di, 2 ; move to next screen location
		loop nextpos ; repeat for all digits on stack
		pop di
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
		ret 2
		
		
		
printstr: push bp
		mov bp,sp
		push es
		push ax
		push cx
		push si
		push di
		mov ax,0xb800
		mov es,ax
		mov di,1990
		mov si,[bp+6]
		mov cx,[bp+4]
		mov ah,0x87

		cld
		again: mov al,[si]
		mov[es:di],ax
		add di,2
		add si,1
		loop again

		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp

		ret 4

printstr2: 
		push bp
		mov bp,sp
		push es
		push ax
		push cx
		push si
		push di
		mov ax,0xb800
		mov es,ax
		mov di,[bp+8]
		mov si,[bp+6]
		mov cx,[bp+4]
		mov ah,0x87

		cld
		again1: mov al,[si]
		mov[es:di],ax
		add di,2
		add si,1
		loop again1

		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp

		ret 6
;***************************************************************
printbellsound:
mov ah,9
mov dx,bell
int 21h
ret



;##############################################################
nextlevel1:
push ax
push es

  in al,0x60
  cmp al,0x01 ;esc key
      jne gamenextlev
      mov cx,4
	mov [cs:check],cx
	call function_ret1


gamenextlev:
call start
;*************************************************************************************************************************
MovePlayer:
	push es
	push ax
	push bx
	push dx
	push cx
	push di
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x0402						;Player
	mov cx,[cs:check]

	
	cmp cx, 0							;right
	je right
	cmp cx, 1							;down
	je down1
	cmp cx, 2							;left
	je left3
	cmp cx, 3							;up
	je up3
	cmp cx, 5							;game pause
	je game_paused 						
	cmp cx, 6							;resume game
	je resume							
	cmp cx, 4							;exit
	jne function_ret2
	mov word[flag],1			;terminating condition
	jmp function_ret2
	
	game_paused:
	push 140
	push paus
	push paus_size
	call printstr2
	jmp function_ret2
	
	resume:
	push 140
	push res
	push paus_size
	call printstr2
	jmp function_ret2
	
	down1:
		jmp down
	right:
		mov di, [cs:index]
		mov bx, word[cs:restore]
		mov word[es:di], bx	
		cmp bx,0x0304					;Diamond
		jne skip3
		;update scoreboard
		mov word[es:di],0x0720
		mov dx,[scorecount]
		inc dx
		mov [scorecount],dx
		push dx
		call printnum
		jmp skip3
		
	left3:
	jmp left2
	up3: 
		jmp up1	
	function_ret2:
		jmp function_ret1
		skip3:
			cmp word[es:di-160],0x0509				;under boulder
			jne cont
			call printbellsound
			mov word[es:di-160],0x0409
			mov dx,[lifecount]
			dec dx
			mov [lifecount],dx
			push dx
			call printlives
			
			
			
			cmp word[lifecount],0
			je th
			
			cmp word[es:di],02FEh						;-------DOOR
			jne cont 
			mov word[flag1],1
			
			th:
			call printbellsound
			mov word[flag],1			;terminating condition
			jmp cont
			
	left2: 
		jmp left1		
	up2: 
		jmp up1	
	function_ret1:
		jmp function_ret
		
	
			cont:
			mov word[es:di],0x0720
			add di, 2
			cmp word[es:di],0x0509		; check for boulder (can not move in boulder)
			je l1
			cmp word[es:di],0xE600
			je l1
			cmp word[es:di],0x027F  ;if bell
			je bellcheck
			; check for wall (can not move in wall)
			cmp di,158
			jne skip1
			call printbellsound
			mov di,0
		l1:
		call printbellsound
		ls2:
		sub di,2
		mov word[es:di],0x0402
		jmp match_di1
		skip1:							;update
			mov [cs:index], di
			mov bx, word[es:di]
			mov word[cs:restore], bx
			mov word[es:di], ax
	match_di1:
		jmp function_ret
	left1:
		jmp left

	;-------
	bellcheck:
call clrscr
push nextlev
push 15
call printstr
    ;call nextlevel1
    ;--------------
    
    
    	up1:
		jmp up
	down:
		mov di, [cs:index]						;restore
		mov bx, word[cs:restore]
		mov word[es:di], bx		
		cmp bx,0x0304							;diamond
		jne skip4
												;update scoreboard
		mov word[es:di],0x0720
		mov dx,[scorecount]
		inc dx
		mov [scorecount],dx
		push dx
		call printnum
		skip4:
			cmp word[es:di-160],0x0509 		; checking for boulder above
			jne cont1
			call printbellsound
			mov word[es:di-160],0x0409
			mov dx,[lifecount]
			dec dx
			mov [lifecount],dx
			push dx
			call printlives
			
			cmp word[lifecount],0
			je th1
						
			cmp word[es:di],02FEh
			jne cont1 
			mov word[flag1],1 ; for next level

			
			th1:
			call printbellsound
			mov word[flag],1			;terminating condition
			jmp cont1
		
		cont1:
			mov word[es:di],0x0720
			add di, 160
			cmp word[es:di],0x0509
			je l2
			cmp word[es:di],0xE600
			je l2
			cmp word[es:di],0x027F  ;if bell
			je bellcheck
			cmp di,4000
			jna skip
			call printbellsound
			sub di,4000
		l2:
		call printbellsound
		sub di,160
		mov word[es:di],0x0402
		jmp match_di2		
		skip:
												;update values
			mov [cs:index], di
			mov bx, word[es:di]
			mov word[cs:restore], bx
			mov word[es:di], ax


	match_di2:
			jmp function_ret
	left:
												;restore
		mov di, [cs:index]
		mov bx, word[cs:restore]
		mov word[es:di], bx	
		cmp bx,0x0304							;diamond
		jne skip5
												;update score
		mov word[es:di],0x0720
		mov dx,[scorecount]
		inc dx
		mov [scorecount],dx
		push dx
		call printnum
		skip5:
			cmp word[es:di-160],0x0509						;TERMINATE ON under BOULDER
			jne cont2
			call printbellsound
			mov word[es:di-160],0x0409
			mov dx,[lifecount]
			dec dx
			mov [lifecount],dx
			push dx
			call printlives
			

			cmp word[lifecount],0
			je th2
			
			cmp word[es:di],02FEh						;-------DOOR
			jne cont2 
			mov word[flag1],1
						
			th2:
			call printbellsound
			mov word[flag],1			;terminating condition
			jmp cont2
			
			
		
		cont2:
												;CONTINUE AND UPDATE
			mov word[es:di],0x0720
			sub di, 2
			cmp word[es:di],0x0509
			je l3
			cmp word[es:di],0xE600
			je l3
			cmp word[es:di],0x027F  ;if bell
			je bellcheck
			mov [cs:index], di
			mov bx, word[es:di]
			mov word[cs:restore], bx
			mov word[es:di], ax
	
	match_di3:
			jmp function_ret
	
	l3:call printbellsound
		add di,2
		mov word[es:di],0x0402
		jmp match_di3

	up:
												;RESTORE VALUES
		mov di, [cs:index]
		mov bx, word[cs:restore]
		mov word[es:di], bx
		cmp bx,0x0304							;diamond
		jne skip6
												;UPDATE Score
		mov word[es:di],0x0720
		mov dx,[scorecount]
		inc dx
		mov [scorecount],dx
		push dx
		call printnum
		skip6:	
												;TERMINATE under boulder
		cmp word[es:di-160],0x0509
		jne cont3
		call printbellsound
		mov word[es:di-160],0x0409
		mov dx,[lifecount]
		dec dx
		mov [lifecount],dx
		push dx
		call printlives
			
		cmp word[lifecount],0
		je th3
		
		cmp word[es:di],02FEh				;-------DOOR
		jne cont3 
		mov word[flag1],1
		
		th3:call printbellsound
		mov word[flag],1			;terminating condition
		jmp cont3
		

		cont3:
												;CONTINUE AND UPDATE
		mov word[es:di],0x0720
		sub di, 160
		cmp word[es:di],0xE600
		je l4
		cmp word[es:di],0x0509
		je l4
		cmp word[es:di],0x027F  ;if bell
			je bellcheck
		cmp di,4160
		jna skip2
		add di,4000
		skip2:
		mov [cs:index], di
		mov bx, word[es:di]
		mov word[cs:restore], bx
		mov word[es:di], ax

	match_di4:
			jmp function_ret
			
	l4:call printbellsound 
	add di,160
		mov word[es:di],0x0402
		jmp match_di4
		
	function_ret:
		pop di
		pop cx
		pop dx
		pop bx
		pop ax
		pop es
		
	ret

;************************************************************************************
;keyboard Interrupt
kbisr:
	push ax
	push es
	in al, 0x60

	cmp al,0x50;Down Arrow
	jne nextcmp
	mov cx,1
	mov [cs:check], cx
	call MovePlayer
	nextcmp:;Right Arrow
		cmp al,0x4D
		jne nextcmp2
		mov cx,0
		mov [cs:check],cx
		call MovePlayer
	nextcmp2:;Left Arrow
		cmp al,0x4B
		jne nextcmp3
		mov cx,2
		mov [cs:check],cx
		call MovePlayer
	nextcmp3:;Up Arrow
		cmp al,0x48
		jne nextcmp4
		mov cx,3
		mov [cs:check],cx
		call MovePlayer
	nextcmp4:
		cmp al,0x01 ;esc key
		jne nextcmp5 	
		mov cx,4
		mov [cs:check],cx
		call MovePlayer

	nextcmp6:
		cmp al,0x19 ;PAUSED key
		jne nextcmp5 	
		mov cx,6
		mov [cs:check],cx
		call MovePlayer
		
	nextcmp5:
		cmp al,0x13 ;PAUSED key
		jne ending 	
		mov cx,5
		mov [cs:check],cx
			
	ending:
		pop es
		pop ax
		jmp far [cs:oldisr]
		
;____________________________________________


	
print: 
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	mov ax, 0xb800
	mov es, ax 						; point es to video base
	
	mov si, [bp+4] 					; point si to string
	mov ah,0x07
nextchar: 
	cmp al,"$"
	je ee
	mov al, [si] 					; load next char of string
	mov [es:di], ax 				; show this char on screen
	add di, 2 						; move to next screen location
	add si, 1 						; move to next char in string
	jmp nextchar
	ee:
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 2
