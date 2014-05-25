; file	ad.asm
.include "m103def.inc"		; include AVR port/bit definitions
.include "macros.asm"		; include macro definitions
.include "definitions.asm"	; include register/constant definitions

.equ 	MORSE_PORT = 2


;============== Interrupt Vector Table ===============
.org	0 
	rjmp	reset  
.org	OVF0addr
	rjmp	analyse
.org	0x30

son:
.db	"sound",0

;============== Interrupt Service Routine ============	

analyse:
	ldi	_w,0x1e
	mov	_u,c0
	sub	_u,_w
	brmi	PC+3
	clr	c0
	rcall	printcourt
	reti
	
;============== Initialisation Reset =================
reset:
	LDSP	RAMEND		; set up stack pointer (SP)
	
	OUTI	PORTB,0xff
	OUTI	DDRB,0xff
	
	OUTI	ADCSR,(1<<ADEN)+1; AD Enable, PS=CK/2	
	OUTI	ADMUX,MORSE_PORT; select channel MORSE_PORT
	
	OUTI	TIMSK,(1<<TOIE0)
	OUTI	ASSR,(1<<AS0)
	OUTI 	TCCR0,2
	sei
	
	clr	c0		; compteur de pics 
	clr	d1	
	rcall	LCD_init	; initialize the LCD
	
	rjmp	main		; jump ahead to the main program

.include "lcd.asm"		; include the LCD routines
.include "printf.asm"		; include formatted printing routines
		
		
;============= Main Program ==========================
main:
	sbi	ADCSR,ADSC	; AD start conversion
	WP1	ADCSR,ADSC	; wait if ADIF=0 
	in	d0,ADCL		; read low byte first
	in	d1,ADCH		; read high byte  second
	lsr	d0
	lsr	d0 
	mov 	b0,d0		; round up to use 1 byte
	lsl	d1
	lsl	d1
	lsl	d1
	lsl	d1
	lsl	d1
	lsl	d1
	add	b0,d1
	subi	b0,127
	ldi	b1,63		; valeur de seuil
	sub	b0,b1		; soustraction seuil
	brmi	PC+2		; PC+2 si inferieur seuil
	rcall	print
	bclr	1
	rcall	printno
	rjmp	main

printcourt:
	;rcall	LCD_clear
	ldi	r16,son
	ldi	zl,low(2*son)
	ldi	zh,high(2*son)
	rcall	LCD_putstring
	
LCD_putstring:
	lpm
	tst	r0
	breq	done
	mov	a0,r0
	rcall	LCD_putc
	adiw	zl,1
	rjmp	LCD_putstring

done:ret
	
printno:
	;CLT
;	PRINTF	LCD 
;.db	CR,CR,"SOUND1=",FDEC,c,0
	ret
	
print:
	brts 	PC+3		; branch si T (=au dessus du seuil) est set
	bset	1		; sinon, set T
	inc	c0		; inc compteur de pics
	;tst	d1
	;breq	PC+4
;	PRINTF	LCD
;.db	CR,CR,"SOUND2=",FDEC,c,"  ",0 
	ret
	;inc	d1
	;OUTI	TIMSK,(1<<TOIE0)
	;rjmp	main
	
