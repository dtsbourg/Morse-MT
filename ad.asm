; file	ad.asm
.include "m103def.inc"		; include AVR port/bit definitions
.include "macros.asm"		; include macro definitions
.include "definitions.asm"	; include register/constant definitions

reset:
	LDSP	RAMEND		; set up stack pointer (SP)
	OUTI	DDRB,0x00	; configure portB to output
	rcall	LCD_init	; initialize the LCD
	
	.equ MORSE_PORT = 2
	OUTI	ADCSR,(1<<ADEN)+1; AD Enable, PS=CK/2	
	OUTI	ADMUX,MORSE_PORT	; select channel MORSE_PORT	
	rjmp	main		; jump ahead to the main program
	
.include "lcd.asm"		; include the LCD routines
.include "printf.asm"		; include formatted printing routines

main:
	sbi	ADCSR,ADSC	; AD start conversion
	WP1	ADCSR,ADSC	; wait if ADIF=0
	in	a0,ADCL		; read low byte first
	in	a1,ADCH		; read high byte second
	mov 	b0,a0>>2
	add	b0,a1<<6
	PRINTF	LCD		; print formatted
.db	CR,CR,"WTF= ",FDEC,b,0
	WAIT_MS	200
	rjmp 	main	
