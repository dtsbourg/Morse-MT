; =============================================== ;
; file: ad2.asm
; Intro : Main implementation file for microcontrollers projet
; Contains : main routines for morse decoder
; Authors : Group M3 ( Dylan Bourgeois & Tristan Besson)
; Last modified : 27.05.2014 @ 14:16
; =============================================== ;

.include "m103def.inc"		; include AVR port/bit definitions
.include "macros.asm"		; include macro definitions
.include "definitions.asm"	; include register/constant definitions

; DEFINE : PORTS
.equ 	MORSE_PORT = 2		; define analog port to be used

; DEFINE : LEVELS
.equ	LEVEL_NB_PICS = 0x40	; define level for number of pics (64)
.equ	LEVEL_SND = 63		; define sound strength level

; DEFINE : PRESCALERS
.equ	AD_PS = 5		; define AD prescaler = 5 (f=125kHz)
.equ	TIMER_PS = 2		; define timer prescaler = 2 (T=131ms)

; DEFINE : ARITHMETIC CONSTANTS
.equ	SIGNED = 127		; define constant to bring back counter to signed

; DEFINE : SYMBOLS
.equ	SHORT_SYMB = 0b01	; define buffer symbolic for short signal
.equ	LONG_SYMB = 0b10	; define buffer symbolic for long signal

;============== Interrupt Vector Table ===============
.org	0 
	rjmp	reset  
.org	OVF1addr
	rjmp	analyse
.org	0x30

short:
.db	"short",0

long:
.db	"long ",0

;============== Interrupt Service Routine ============	
analyse:
	ldi	_w,LEVEL_NB_PICS ; _w=64 (nb de pics)
	sub	c0,_w	; compare nb of pics / seuil
	brlo	PC+5	; if nb de pics > seuil, inc counter
	clr	c1
	inc	d2	
	clr	c0	; clear pic counter
	reti		; and leave
	inc	c1		
	rcall	shortlong ; else sound is over, call routine to see if s or l
	clr	d2	; clear after compare
	clr	c0	; clear pic counter
	reti		; return for interrupt
	
;============== Initialisation Reset =================
reset:
	LDSP	RAMEND		; set up stack pointer (SP)
	
	OUTI	DDRB,0xff	; set LEDs as output in debug mode
	
	OUTI	ASSR,(1<<AS0)	; enable AD type 0
	OUTI	ADCSR,(1<<ADEN)+AD_PS ; AD Enable, PS=CK/2^5=CK/32	
	OUTI	ADMUX,MORSE_PORT; select channel MORSE_PORT
	
	OUTI	TIMSK,(1<<TOIE1) ; Enable quartz timer (timer1)
	OUTI 	TCCR1B,TIMER_PS	; PS=CK/4, overflow period : 131 ms
	sei
	
	rcall	clr_init
	rcall	LCD_init	; initialize the LCD
	
	rjmp	main		; jump ahead to the main program

;============= Init routines ========================
; init counter : clear ALL the counters !	
clr_init:
	clr	b0		; sound level value
	clr	b1		; seuil value
	clr	b3		; character counter
	clr	c0		; compteur de pics
	clr	c1		; void counter
	clr	c2		; did counter start on same pulse
	clr	c3		; should restart timer 
	clr	d0		; ACDH
	clr	d1		; ACDL
	clr	d2		; count pulses
	clr	d3		; buffer
	ret	

; clear timer counter register	
clr_timer:
	;OUTI	TCNT1H,0
	;OUTI	TCNT1L,0
	;OUTI	TCNT0,0
	ret

;============= External ==========================
.include "lcd.asm"		; include the LCD routines
.include "printf.asm"		; include formatted printing routines
.include "alphabet.asm"
		
		
;============= Main Program ==========================
main:
	sbi	ADCSR,ADSC	; AD start conversion
	WP1	ADCSR,ADSC	; wait if ADIF=0 
	in	d0,ADCL		; read low byte first
	in	d1,ADCH		; read high byte  second
	LSR_2	d0
	mov 	b0,d0		; round up to use 1 byte
	LSL_6	d1
	add	b0,d1
	subi	b0,SIGNED	; force b0 to signed int for subtraction
	ldi	b1,LEVEL_SND	; valeur de seuil
	sub	b0,b1		; soustraction seuil
	brmi	PC+2		; PC+2 si inferieur seuil
	rcall	sound		; if > seuil, sound
	rcall	nosound		; if < seuil, no sound
	rjmp	main

;============= Sound processing ==========================
; Called if sound is detected	
sound:	
	mov	w,c2
	ldi	a0,1
	tst	c3		; tst if timer has been started on this pic
	brne	PC+3		; if not, jump to
	inc	c3		; if so, set to 1
	rcall	clr_timer	; and reset timer
	sub	w,a0		; tst if counter has been inc on this pic 
	breq	PC+3		; if it has jump to
	inc	c2		; if it hasn't set to 1
	inc	c0		; and inc pic counter
	ret

; Called if sound is not detected
nosound:
	clr	c2		; clear counters
	ret

; Determine is sound is short or long	
shortlong:
	out	PORTB,d3
	ldi	w,3		; check for 3 empty in a row
	sub	w,c1		
	brne	PC+3
	rcall	sendbuffer	; if so, send buffer
	ret			; and return
	tst	d2		; test if pulse counter is empty
	brne	PC+3		; to avoid unnecessary computation
	clr	c3
	ret
	ldi	w,0b1		; load 1 into temp reg
	sub	w,d2		; 1-counter=?
	brne	PC+3		; if counter!=1, branch to test ?=3
	rcall	buffershort	; and buffer short signal
	ret			; then return
	ldi	w,0b11		; load 3 into tmp
	sub	w,d2		; 3-counter=?
	brne	PC+2		; if counter !=3, ret
	rcall	bufferlong	; buffer long signal
	ret

;============= Buffering ==========================
; Buffer a short symbol
buffershort:
	ldi	w,SHORT_SYMB	; load 01 for short pulse
	LSL_2	d3		; buffer << 2
	add	d3,w		; insert 01 to LSBs
	inc	b3 
	ret

; Buffer a long symbol
bufferlong:
	ldi	w,LONG_SYMB	; load 10 for short pulse
	LSL_2	d3		; buffer << 2
	add	d3,w		; insert 10 to LSBs
	inc	b3
	ret

; Send buffer to be decoded	
sendbuffer:
	tst	d3		; check for no signal
	brne	PC+2		; if empty, return
	ret
	tst	d3		; check for empty buffer
	breq	PC+4		; if empty jump decode step
	rcall	alphabet	; decode with alphabet
	clr	d3		; clear d3 after decoding
	clr	b3		; clear char counter after decoding
	ret
	
; Alphabet decoder routine
alphabet:
	mov     r20,d3		; place buffer in temp register
	andi	r20,0x01	; Test if letter finish with a Dash
	brne    PC+3		; if so, ignore this line
	rjmp	alphabet_dash
	ret
	rjmp	alphabet_dot	; if not, jump here		   
	ret
; Alphabet decoder subroutine (part of alphabet starting with dash)
alphabet_dash:
	; " x x x - "
   	mov     r20,d3		
   	andi	r20,0x0c        ; Test if letter has only one character
    	brne    PC+3		
    	rcall	print_t
    	ret

   	mov     r20,d3		
    	andi    r20,0x04	; Test if letter has a Dot as 2nd character
    	brne    PC+23		; if so, jump far far away

        ; " x x - - "
        mov     r20,d3
        andi    r20,0x30
        brne    PC+3
        rcall	print_m
        ret
        
        mov	r20,d3
        andi	r20,0x10
        brne	PC+8
        
        ; " x - - - "
        
        mov     r20,d3
        andi    r20,0xc0
        brne    PC+3
        rcall	print_o
        ret
        
        rcall	print_j
        ret
        
        ; " x ° - - "
        
        mov     r20,d3
        andi    r20,0xc0
        brne	PC+3
        rcall	print_w
        ret
        
        rcall	print_y
        ret
        
        ; " x x ° - "
        
        mov     r20,d3
        andi    r20,0x30
        brne    PC+3
        rcall	print_a
        ret
        
        mov     r20,d3
        andi    r20,0x10
        brne    PC+11
       
       	
	; " x - ° - "
	
	mov     r20,d3
        andi    r20,0xc0
        brne	PC+3
        rcall	print_k
        ret
        
        rcall	print_q
        ret
        
        ; " x ° ° - "
        
        mov	r20,d3
        andi	r20,0xc0
        brne	PC+3
        rcall	print_u
        ret
        
        mov	r20,d3
        andi	r20,0x40
        brne	PC+3
        
        rcall	print_x
        ret
        
        rcall	print_v
        ret

; Alphabet decoder subroutine (part of alphabet starting with dot)
alphabet_dot:

	;" x x x ° "
	
	mov     r20,d3		
   	andi	r20,0x0c        ; Test if letter has only one character
    	brne    PC+3		
    	rcall	print_e
    	ret

   	mov     r20,d3		
    	andi    r20,0x04	; Test if letter has a Dot as 2nd character
    	brne    PC+28		; if so, jump far far away

        ; " x x - ° "
        mov     r20,d3
        andi    r20,0x30
        brne    PC+3
        rcall	print_n
        ret
        
        mov	r20,d3
        andi	r20,0x10
        brne	PC+8
        
        ; " x - - ° "
        
        mov     r20,d3
        andi    r20,0xc0
        brne    PC+3
        rcall	print_g
        ret
        
        rcall	print_p
        ret
        
        ; " x ° - ° "
        
        mov     r20,d3
        andi    r20,0xc0
        brne    PC+3
        rcall	print_r
        ret
        
        mov	r20,d3
       	andi	r20,0x40
       	brne	PC+3
       	rcall	print_c
       	ret
       	
        rcall	print_f
        ret
        
        ; " x x ° ° "
        
        mov     r20,d3
        andi    r20,0x30
        brne    PC+3
        rcall	print_i
        ret
        
        mov     r20,d3
        andi    r20,0x10
        brne    PC+11
       
       	
	; " x - ° ° "
	
	mov     r20,d3
        andi    r20,0xc0
        brne    PC+3
        rcall	print_d
        ret
        
        mov	r20,d3
        andi	r20,0x40
        brne	PC+3
        
        rcall	print_z
        ret
        
        rcall	print_l
        ret
        
        ; " x ° ° ° "
        
        mov	r20,d3
        andi	r20,0xc0
        brne	PC+3
        rcall	print_s
        ret
        
        mov	r20,d3
        andi	r20,0x40
        brne	PC+3
        
        rcall	print_b
        ret
        
        rcall	print_h
        ret	

;============= Printing characters to LCD ==========================
; print char to LCD
.macro	PRINT_CHAR
	ldi	r16,short
	ldi	zl,low(2*@0)
	ldi	zh,high(2*@0)
	rcall	LCD_putstring
.endmacro

; print string to LCD	
LCD_putstring:
	lpm
	tst	r0
	breq	done
	mov	a0,r0
	rcall	LCD_putc
	adiw	zl,1
	rjmp	LCD_putstring

done:ret


print_a:
	PRINT_CHAR	_a
	ret
	
print_b:
	PRINT_CHAR	_b
	ret
	
print_c:
	PRINT_CHAR	_c
	ret
	
print_d:
	PRINT_CHAR	_d
	ret
	
print_e:
	PRINT_CHAR	_e
	ret
	
print_f:
	PRINT_CHAR	_f
	ret
	
print_g:
	PRINT_CHAR	_g
	ret
	
print_h:
	PRINT_CHAR	_h
	ret
	
print_i:
	PRINT_CHAR	_i
	ret
	
print_j:
	PRINT_CHAR	_j
	ret
	
print_k:
	PRINT_CHAR	_k
	ret
	
print_l:
	PRINT_CHAR	_l
	ret
	
print_m:
	PRINT_CHAR	_m
	ret
	
print_n:
	PRINT_CHAR	_n
	ret
	
print_o:
	PRINT_CHAR	_o
	ret
	
print_p:
	PRINT_CHAR	_p
	ret
	
print_q:
	PRINT_CHAR	_q
	ret
	
print_r:
	PRINT_CHAR	_r
	ret
	
print_s:
	PRINT_CHAR	_s
	ret
	
print_t:
	PRINT_CHAR	_t
	ret
	
print_u:
	PRINT_CHAR	_u
	ret
	
print_v:
	PRINT_CHAR	_v
	ret
	
print_w:
	PRINT_CHAR	_w
	ret
	
print_x:
	PRINT_CHAR	_x
	ret
	
print_y:
	PRINT_CHAR	_y
	ret
	
print_z:
	PRINT_CHAR	_z
	ret
	
		