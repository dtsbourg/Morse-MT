; file	ad.asm
.include "m103def.inc"		; include AVR port/bit definitions
.include "macros.asm"		; include macro definitions
.include "definitions.asm"	; include register/constant definitions

.equ	meas_ready = 0
.equ 	MORSE_PORT = 2


 ; init : reset
reset:
	LDSP	RAMEND		; set up stack pointer (SP)
	
	OUTI	ADCSR,(1<<ADEN)+1; AD Enable, PS=CK/2	
	OUTI	ADMUX,MORSE_PORT; select channel MORSE_PORT
	
	rcall	LCD_init	; initialize the LCD
	clr	c0		; compteur de pics	
	rjmp	main		; jump ahead to the main program

.include "lcd.asm"		; include the LCD routines
.include "printf.asm"		; include formatted printing routines
	
main:
	sbi	ADCSR,ADSC	; AD start conversion
	WP1	ADCSR,ADSC	; wait if ADIF=0 
	in	a0,ADCL		; read low byte first
	in	a1,ADCH		; read high byte second
	mov 	b0,a0>>2	; round up to use 1 byte
	add	b0,a1<<6
	subi	b0,127
	ldi	r25,63		; valeur de seuil
	sub	b0,r25		; soustraction seuil
	brmi	PC+2		; PC+2 si inferieur seuil
	rcall	print
	rcall	printno
	 	

printno:
	CLT
	PRINTF	LCD 
.db	CR,CR,"SOUND =", FDEC,c,"  ",0
	rjmp	main
	
print:
	brts 	PC+3		; branch si T (=au dessus du seuil) est set
	SET
	inc	c0		; compteur de pics
	PRINTF	LCD
.db	CR,CR,"SOUND =", FDEC,c,"  ",0 
	rjmp	main