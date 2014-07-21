;--------------------------------------------------------
; File Created by SDCC : free open source ANSI-C Compiler
; Version 3.3.1 #8898 (Nov 28 2013) (Linux)
; This file was generated Sun Dec  1 20:18:51 2013
;--------------------------------------------------------
; PIC port for the 14-bit core
;--------------------------------------------------------
;	.file	"blinking-led.c"
	list	p=16f690
	radix dec
	include "p16f690.inc"
;--------------------------------------------------------
; config word(s)
;--------------------------------------------------------
	__config 0x30d4
;--------------------------------------------------------
; external declarations
;--------------------------------------------------------
	extern	_STATUSbits
	extern	_PORTAbits
	extern	_PORTBbits
	extern	_PORTCbits
	extern	_INTCONbits
	extern	_PIR1bits
	extern	_PIR2bits
	extern	_T1CONbits
	extern	_T2CONbits
	extern	_SSPCONbits
	extern	_CCP1CONbits
	extern	_RCSTAbits
	extern	_PWM1CONbits
	extern	_ECCPASbits
	extern	_ADCON0bits
	extern	_OPTION_REGbits
	extern	_TRISAbits
	extern	_TRISBbits
	extern	_TRISCbits
	extern	_PIE1bits
	extern	_PIE2bits
	extern	_PCONbits
	extern	_OSCCONbits
	extern	_OSCTUNEbits
	extern	_MSKbits
	extern	_SSPMSKbits
	extern	_SSPSTATbits
	extern	_WPUbits
	extern	_WPUAbits
	extern	_IOCbits
	extern	_IOCAbits
	extern	_WDTCONbits
	extern	_TXSTAbits
	extern	_SPBRGbits
	extern	_SPBRGHbits
	extern	_BAUDCTLbits
	extern	_ADCON1bits
	extern	_WPUBbits
	extern	_IOCBbits
	extern	_VRCONbits
	extern	_CM1CON0bits
	extern	_CM2CON0bits
	extern	_CM2CON1bits
	extern	_ANSELbits
	extern	_ANSELHbits
	extern	_EECON1bits
	extern	_PSTRCONbits
	extern	_SRCONbits
	extern	_INDF
	extern	_TMR0
	extern	_PCL
	extern	_STATUS
	extern	_FSR
	extern	_PORTA
	extern	_PORTB
	extern	_PORTC
	extern	_PCLATH
	extern	_INTCON
	extern	_PIR1
	extern	_PIR2
	extern	_TMR1
	extern	_TMR1L
	extern	_TMR1H
	extern	_T1CON
	extern	_TMR2
	extern	_T2CON
	extern	_SSPBUF
	extern	_SSPCON
	extern	_CCPR
	extern	_CCPR1L
	extern	_CCPR1H
	extern	_CCP1CON
	extern	_RCSTA
	extern	_TXREG
	extern	_RCREG
	extern	_PWM1CON
	extern	_ECCPAS
	extern	_ADRESH
	extern	_ADCON0
	extern	_OPTION_REG
	extern	_TRISA
	extern	_TRISB
	extern	_TRISC
	extern	_PIE1
	extern	_PIE2
	extern	_PCON
	extern	_OSCCON
	extern	_OSCTUNE
	extern	_PR2
	extern	_MSK
	extern	_SSPADD
	extern	_SSPMSK
	extern	_SSPSTAT
	extern	_WPU
	extern	_WPUA
	extern	_IOC
	extern	_IOCA
	extern	_WDTCON
	extern	_TXSTA
	extern	_SPBRG
	extern	_SPBRGH
	extern	_BAUDCTL
	extern	_ADRESL
	extern	_ADCON1
	extern	_EEDAT
	extern	_EEDATA
	extern	_EEADR
	extern	_EEDATH
	extern	_EEADRH
	extern	_WPUB
	extern	_IOCB
	extern	_VRCON
	extern	_CM1CON0
	extern	_CM2CON0
	extern	_CM2CON1
	extern	_ANSEL
	extern	_ANSELH
	extern	_EECON1
	extern	_EECON2
	extern	_PSTRCON
	extern	_SRCON
	extern	___sdcc_saved_fsr
	extern	__sdcc_gsinit_startup
;--------------------------------------------------------
; global declarations
;--------------------------------------------------------
	global	_spin
	global	_isr
	global	_main
	global	_led

	global PSAVE
	global SSAVE
	global WSAVE
	global STK04
	global STK03
	global STK02
	global STK01
	global STK00

sharebank udata_ovr 0x0070
PSAVE	res 1
SSAVE	res 1
WSAVE	res 1
STK04	res 1
STK03	res 1
STK02	res 1
STK01	res 1
STK00	res 1

;--------------------------------------------------------
; global definitions
;--------------------------------------------------------
;--------------------------------------------------------
; absolute symbol definitions
;--------------------------------------------------------
;--------------------------------------------------------
; compiler-defined variables
;--------------------------------------------------------
UDL_blinking_led_0	udata
r0x1006	res	1
r0x1005	res	1
r0x1004	res	1
r0x1003	res	1
r0x1007	res	1
r0x1008	res	1
r0x1009	res	1
r0x100A	res	1
r0x100B	res	1
;--------------------------------------------------------
; initialized data
;--------------------------------------------------------

ID_blinking_led_0	idata
_led
	db	0x00, 0x00

;--------------------------------------------------------
; overlayable items in internal ram 
;--------------------------------------------------------
;	udata_ovr
;--------------------------------------------------------
; reset vector 
;--------------------------------------------------------
STARTUP	code 0x0000
	nop
	pagesel __sdcc_gsinit_startup
	goto	__sdcc_gsinit_startup
;--------------------------------------------------------
; interrupt and initialization code
;--------------------------------------------------------
c_interrupt	code	0x4
__sdcc_interrupt
;***
;  pBlock Stats: dbName = I
;***
;entry:  _isr	;Function start
; 0 exit points
;1 compiler assigned register :
;   r0x100B
;; Starting pCode block
_isr	;Function start
; 0 exit points
;	.line	24; "blinking-led.c"	void isr(void) __interrupt 0
	MOVWF	WSAVE
	SWAPF	STATUS,W
	CLRF	STATUS
	MOVWF	SSAVE
	MOVF	PCLATH,W
	CLRF	PCLATH
	MOVWF	PSAVE
	MOVF	FSR,W
	BANKSEL	___sdcc_saved_fsr
	MOVWF	___sdcc_saved_fsr
;	.line	26; "blinking-led.c"	T0IF = 0;
	BANKSEL	_INTCONbits
	BCF	_INTCONbits,2
;	.line	28; "blinking-led.c"	RC0 = led;
	BANKSEL	_led
	MOVF	_led,W
	BANKSEL	r0x100B
	MOVWF	r0x100B
	RRF	r0x100B,W
	BTFSC	STATUS,0
	GOTO	_00001_DS_
	BANKSEL	_PORTCbits
	BCF	_PORTCbits,0
_00001_DS_
	BTFSS	STATUS,0
	GOTO	_00002_DS_
	BANKSEL	_PORTCbits
	BSF	_PORTCbits,0
_00002_DS_
;	.line	29; "blinking-led.c"	if (led) { led = 0; } else { led = 1; }
	BANKSEL	_led
	MOVF	_led,W
	IORWF	(_led + 1),W
	BTFSC	STATUS,2
	GOTO	_00111_DS_
	CLRF	_led
	CLRF	(_led + 1)
	GOTO	_00113_DS_
_00111_DS_
	MOVLW	0x01
	BANKSEL	_led
	MOVWF	_led
	CLRF	(_led + 1)
_00113_DS_
	BANKSEL	___sdcc_saved_fsr
	MOVF	___sdcc_saved_fsr,W
	BANKSEL	FSR
	MOVWF	FSR
	MOVF	PSAVE,W
	MOVWF	PCLATH
	CLRF	STATUS
	SWAPF	SSAVE,W
	MOVWF	STATUS
	SWAPF	WSAVE,F
	SWAPF	WSAVE,W
END_OF_INTERRUPT
	RETFIE	

;--------------------------------------------------------
; code
;--------------------------------------------------------
code_blinking_led	code
;***
;  pBlock Stats: dbName = M
;***
;entry:  _main	;Function start
; 2 exit points
;has an exit
;; Starting pCode block
_main	;Function start
; 2 exit points
;	.line	35; "blinking-led.c"	SCS = 1;
	BANKSEL	_OSCCONbits
	BSF	_OSCCONbits,0
;	.line	37; "blinking-led.c"	IRCF2 = 1; IRCF1 = 1; IRCF0 = 1;
	BSF	_OSCCONbits,6
	BSF	_OSCCONbits,5
	BSF	_OSCCONbits,4
;	.line	39; "blinking-led.c"	ANSEL = 0x00;
	BANKSEL	_ANSEL
	CLRF	_ANSEL
;	.line	40; "blinking-led.c"	ANSELH = 0x00;
	CLRF	_ANSELH
;	.line	43; "blinking-led.c"	TRISC = 0x00;
	BANKSEL	_TRISC
	CLRF	_TRISC
;	.line	44; "blinking-led.c"	PORTC = 0x00;
	BANKSEL	_PORTC
	CLRF	_PORTC
;	.line	47; "blinking-led.c"	T0CS = 0;
	BANKSEL	_OPTION_REGbits
	BCF	_OPTION_REGbits,5
;	.line	49; "blinking-led.c"	TMR0 = 0;
	BANKSEL	_TMR0
	CLRF	_TMR0
;	.line	52; "blinking-led.c"	PSA = 0;
	BANKSEL	_OPTION_REGbits
	BCF	_OPTION_REGbits,3
;	.line	54; "blinking-led.c"	PS2 = 1; PS1 = 1; PS0 = 0;
	BSF	_OPTION_REGbits,2
	BSF	_OPTION_REGbits,1
	BCF	_OPTION_REGbits,0
;	.line	57; "blinking-led.c"	T0IE = 1;
	BANKSEL	_INTCONbits
	BSF	_INTCONbits,5
;	.line	59; "blinking-led.c"	GIE = 1;
	BSF	_INTCONbits,7
_00120_DS_
	GOTO	_00120_DS_
	RETURN	
; exit point of _main

;***
;  pBlock Stats: dbName = C
;***
;entry:  _spin	;Function start
; 2 exit points
;has an exit
;11 compiler assigned registers:
;   r0x1003
;   STK00
;   r0x1004
;   STK01
;   r0x1005
;   STK02
;   r0x1006
;   r0x1007
;   r0x1008
;   r0x1009
;   r0x100A
;; Starting pCode block
_spin	;Function start
; 2 exit points
;	.line	18; "blinking-led.c"	void spin(uint32_t cnt) {
	BANKSEL	r0x1003
	MOVWF	r0x1003
	MOVF	STK00,W
	MOVWF	r0x1004
	MOVF	STK01,W
	MOVWF	r0x1005
	MOVF	STK02,W
	MOVWF	r0x1006
_00105_DS_
;	.line	19; "blinking-led.c"	while (cnt-- > 0) { }
	BANKSEL	r0x1006
	MOVF	r0x1006,W
	MOVWF	r0x1007
	MOVF	r0x1005,W
	MOVWF	r0x1008
	MOVF	r0x1004,W
	MOVWF	r0x1009
	MOVF	r0x1003,W
	MOVWF	r0x100A
	MOVLW	0xff
	ADDWF	r0x1006,F
	MOVLW	0xff
	BTFSS	STATUS,0
	ADDWF	r0x1005,F
	MOVLW	0xff
	BTFSS	STATUS,0
	ADDWF	r0x1004,F
	MOVLW	0xff
	BTFSS	STATUS,0
	ADDWF	r0x1003,F
	MOVF	r0x1007,W
	IORWF	r0x1008,W
	IORWF	r0x1009,W
	IORWF	r0x100A,W
	BTFSS	STATUS,2
	GOTO	_00105_DS_
	RETURN	
; exit point of _spin


;	code size estimation:
;	   90+   20 =   110 instructions (  260 byte)

	end
