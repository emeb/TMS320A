;-----------------------------------------------------------;
; HOSTSPEC.ASM						    ;
; Written By: Keith Larson				    ;
;	      TMS320Cxx DSP Applications Engineer	    ;
;	      Texas Instruments 			    ;
;							    ;
; USE WITH THE DSKL [G]RAPH OPTION.  THIS OPTION IS DESIGNED;
; TO RECEIVE A BYTE FROM THE DSK AND OUTPUT IT GRAPHICLY.   ;
; RUNNING THIS PROGRAM ON DSKD WILL CRASH THE DEBUGGER!     ;
;							    ;
; A spectrum analyzer using the DSK and your Host PC.	    ;
; This code does NOT work with DSKD!  It is run from within ;
; DSKL using DSK_COMM to send data to the host via the RS232;
; link.  It is slower than DSK_SPEC, but you do not need an ;
; oscilliscope! 					    ;
;							    ;
; To run this program, first assemble it using DSKA.	    ;
; Then start DSKL, select 'X' for load and execute.  Then   ;
; select 'G' for graphics.  To exit, hit any key (except Q) ;
; since the keyboard is not flushed and 'Q' will quit DSKL  ;
; entirely!						    ;
;-----------------------------------------------------------;
YES	.set	1		;
NO	.set	0		;
FFT_S		.set	256	;
;-----------------------------------------------------------;
AIC_1		.set	0x0C18	;TB =TA = 6    0000110000011000=0x0C18
AIC_2		.set	0x0205	;TA'=TA'= 1    0000001000000101=0x0205
AIC_3		.set	0x264e	;RB =TB = 0x13 0010011001001110=0x264c 44 khz
AIC_CMD 	.set	0x0003	;   COMMAND    0000000000000011=0x0083
;-----------------------------------------------------------;
BCMD		.set	0xFA10	;JUMP CMD
BXMIT		.set	0xFA12	;JUMP XMIT
BXMIT16 	.set	0xFA14	;JUMP XMIT16
BRECV		.set	0xFA16	;JUMP RECV
BRECV16 	.set	0xFA18	;JUMP RECV16
BCXMIT		.set	0xFA1A	;JUMP CXMIT
;----------------------------------------------------------
STAT1		.set	0x72	;
ACCU_lo 	.set	0x78	;
ACCU_hi 	.set	0x79	;
REAL		.set	0x7a	;
IMAG		.set	0x7b	;
TEMPX		.set	0x7c	;
AUX0		.set	0x7d
AUX1		.set	0x7e
;----------------------------------------------------------------
;    SECONDARY VECTOR TABLE LOACTED IN B0 PROGRAM RAM
;----------------------------------------------------------------
	.include  "mmregs.inc"	;     > USERCODE SHOULD NOT OVERWRITE DSKD  <
	.ps	0xfa00		;     > VECTORS.  ON LOAD, INT2 IS RESTORED <
       ;B	start		;RS   > BY DSKD, BUT TRAP IS NOT	    <
       ;B	start		;INT0
       ;B	start		;INT1
       ;B	start		;INT2  > DSKD LOAD IGNORES INT2 VECTOR
       ;B	start		;TINT
	.ps	0fa0ah		;
	B	RINT		;RINT  Branch to receive interrupt routine
	eint			;XINT  XINT is only for timing, so just return
	ret			;
      ; Begin TRAP/DSKD Kernal	;DSKD load does not restore this code!
;----------------------------------------------------------------
;    APPLICATION CODE IS LOCATED ABOVE DSKD KERNAL
;----------------------------------------------------------------
	.ps	0xFB00		;
	.entry			;
;----------------------------------------------------------------
start:	sxf
	ssxm
	sovm			; catch accumulator overflows
	ldpk	0		; All direct addressing is to MMRs and B2
	fort	0		; Serial port : 16 bit
	rtxm			;	      : ext. FSX
	sfsm			;	      ; burst mode
	lack	0x80		; AIC reset by pulsing /BR (Global Data)
	sach	DXR		; send 0 to DXR (AIC)
	sacl	GREG		; 256 * 100 nS /BR pulse
	lrlk	AR0,0xFFFF	;
	rptk	255		; read junk from address 0xFFFF
	lac	*,0,AR0 	;
	conf	1		; B1,B3 as DRAM if direct bootload
;--------------------------------
AIC_RS	lack	0x20		; Turn on XINT
	sacl	IMR		;
	idle			;
	lalk	AIC_1		; Load each AIC configuration word
	call	AIC_2nd 	; and load it into the AIC
	lalk	AIC_2		;
	call	AIC_2nd 	;
	lalk	AIC_3		;
	call	AIC_2nd 	;
	lalk	AIC_CMD 	;
	call	AIC_2nd 	;
;----------------------------------------------------------------
	lark	AR7,0		; Buffer initialy filled
	lack	0x10		; AIC RINT
	sacl	IMR		; where INT0 indicates EOC (End Of Conv)
	;---------------------------------------------------------------
	lark	AR7,0		; Buffer initialy filled
FFT:	lrlk	AR0,FFT_S/2	;
	larp	AR0		; start FFT with AR0=FFTSize
new_stg lrlk	AR1,_D_base	; AR1 is the TOP BFLY address
	lrlk	AR2,_D_base	; AR2 is the BOT BFLY address
	lrlk	AR3,_T_base+1	; AR3 is the TWiddle pointer
	lrlk	AR4,FFT_S/2	; AR4 counts DFT blocks
	b	n_DFT2,*,AR1	;
DFT:	mar	*BR0+,AR5	; complete circular buffer for TW's
	lark	AR5,1		; set up DFT loop with *BR0+/BANZ
	mar	*BR0+,AR1	; using 1 cuts *BR0+ loop in half!
	;----------------------------------------
	; AR1=Top AR2=Bottom AR3=Twiddle
	;----------------------------------------
BFLY:	lac	*,14,AR2	;(imag1+imag2)/4
	add	*,14,AR1	;
	sach	*+,1,AR2	;store TOP imag
	sub	*,15		;(imag1-imag2)/2
	sach	*+,1,AR1	;store BOT imag
	lac	*,14,AR2	;(real1+real2)/4
	add	*,14,AR1	;
	sach	*+,1,AR2	;store TOP real
	sub	*,15		;(real1-real2)/2
	sach	*,1,AR5 	;store BOT real
	banz	OK,*BR0+,AR3	;If at DFT end quit early
	;------------------------
	mar	*+,AR2		;clean up TW base (xxx0000+1)
	mar	*+		;modify BOTom DATA pointer
	mar	*0+		;
	mar	*0+,AR1 	;
n_DFT2: mar	*0+		;modify the TOP pointer
	mar	*0+,AR4 	;
	banz	DFT,*0-,AR3	;dec DFT block count AR4 by OFFset
	larp	AR0		;
	mar	*BR0+		;
	banz	new_stg,*	;if OFFset was 1, now cleared
	b	endFFT		;
	 ;-------------------------
OK	lt	*-,AR2		;TREG=TWR     *NOTE* Twiddles are Q15
	mpy	*-		;PREG=REAL*TWR
	ltp	*+,AR3		;TREG=IMAG     ACCU=REAL*TWR
	mpy	*		;PREG=IMAG*TWI		      AR2=R AR3=I
	lts	*+,AR2		;TREG=TWI      ACCU=REAL*TWR-IMAG*TWI
	mpy	*		;PREG=REAL*TWI
	sach	*-,1,AR2     ;<<;
	ltp	*,AR3		;TREG=IMAG     ACCU=REAL*TWI
	mpy	*BR0+,AR2	;PREG=IMAG*TWR
	apac			;	       ACCU=IMAG*TWR+REAL*TWI
	sach	*+,1,AR2     ;<<;
	b	BFLY,*+,AR1	;
	;------------------------------------------------------------
endFFT: larp	AR2		;Transform REAL & IMAG to log magnitude
	lrlk	AR2,_D_base	;AR3=FFT data pointer
	lrlk	AR3,FFT_S-1	;AR5=FFT loop counter
	lrlk	AR0,FFT_S
	;-----------------------------------------------------------;
	; WINDOW: Performs post FFT raised cosine windowing!	    ;
	; This is done by using the frequency coefficients of the   ;
	; window in a convolution filter of the spectrum.	    ;
	;-----------------------------------------------------------;
      ;mar	*BR0+	       ; don't start at DC
more_MAG
	mar	 *BR0-		;  -IMAG[-1]  1-COS(nwt/N)	 + 1
	lac	 *BR0+,15	;   IMAG[-0]  filter by post	 |
	subh	 *BR0+		;  +IMAG[+1]  convolution    <--+++-->
	add	 *BR0-,15	;   IMAG			+ + -.5
	sach	 IMAG		;
	mar	 *+		;   REAL
	mar	 *BR0-		;  -REAL[-1]
	lac	 *BR0+,15	;   REAL[-0]  X[-1] -2*X[0] + X[1]
	subh	 *BR0+		;  +REAL[+1]
	add	 *BR0-,15,AR1	;   REAL
	sach	 REAL		;
	sqra	IMAG		;IMAG & REAL can be at most 0x7fff Q15
	ltp	REAL		;MPY will result (at most) in max positive
	mpy	REAL		;
	apac			;output is positive Q30
	addk	0x1		;Set up a floor value; log(0) not legal!
	lark	AR1,22		;pre-scaling exponent shifts Y axis
	rptk	31		;
	norm	*-		;
	larp	AR2		;
	mar	*BR0-	  ;-REAL;dump log(f) into oldest REAL (odd addr)
	sach	*,2		;clr explicit 1.0 and sign bit from mant
	zals	*		;load into ACCU_lo
	sar	AR1,*		;then append exponent (AR1)
	addh	*		;
	rptk	10		;jam result into ACCU_hi
	sfl			;If needed, Use ADDH to saturate overflow
     ;	sach	*		;
     ;	addh	*		;
	sach	*		;
	lac	*		;
	andk	0xfffc,0	;
	sacl	*BR0+		; REAL
	mar	*-		; IMAG
	mar	*BR0+,AR3	;+IMAG
	banz	more_MAG,*-,AR2 ;keep going until all done
	;--------------------------------------------------------
BITREV: lrlk	AR0,FFT_S	;Now perform Output bit reversal
	lrlk	AR1,_D_base	;by moving the magnitude, which
	lrlk	AR2,_D_base+1	;is in the REAL slots, into the
	lrlk	AR3,FFT_S-1	;IMAG slots of the FFT data array
more_BR lac	*+		;load the magnitude
	mar	*+,AR1		;
	sacl	*BR0+,0,AR3	;move it to an open IMAG slot
	banz	more_BR,*-,AR2	;more data to move?
	;--------------------------------------------------------
MOVE_IO larp	AR7		;wait until buffer is full
	banz	MOVE_IO,*,AR2	;(AR7 is decremented by ISR)
	;------------------------
	lrlk	AR3,_D_base	;AR3=FFT data pointer
	lrlk	AR4,_B_base	;AR4=BUFF data pointer
	lrlk	AR5,FFT_S-1	;AR5=FFT loop counter
	lrlk	AR6,_B_base	;AR6=ISR BUFF data pointer
	lrlk	AR7,FFT_S-1	;AR7=ISR BUFF loop counter
	;-------------------------
	dint
	zac
	call	DAT2HOST
	larp	AR2
more_IO
	lar	AR2,*,AR3	;Get A/D value from buffer
	lac	*,0,AR4 	;ACCU= log magnitude (from even address)
	sacl	*+,0,AR3	;

	rptk	7
	sfr
	ork	1
	call	DAT2HOST
	larp	AR3

	zac			;
	sach	*+,0		;IMAG=0
	sar	AR2,*+,AR5	;

	banz	more_IO,*-,AR4	;
	eint			; BUFF clear so enable INT's
	b	FFT		;
;-----------------------------------------------------------------
RINT:	sst1	STAT1		;Recover ARP from ARB by LST1 last
	larp	AR7		;AR6 = current buffer position
	banz	more_buf,*-,AR6 ;if buffer is full RET w/o EINT
	lark	AR7,0		;
	lst1	STAT1		;
	ret			;
more_buf			;
	sacl	ACCU_lo 	;Use NORM start val to adj Y offset
	sach	ACCU_hi 	;post log convert scaling ajsts magnitude
	zalh	*		;Get value
	sach	DXR		;
	;------------------------
	lac	DRR		;
	bit	TEMPX,15	;Inverting every other input aliases the
	bbz	NO_NVRT 	;frequency domain, swapping DC and Nyquist!
	neg			;
NO_NVRT 			;
	sacl	*+		;<<< store DRR, and point to next
	lac	TEMPX		;
	xork	1		;
	sacl	TEMPX		;
	zalh	ACCU_hi 	;
	adds	ACCU_lo 	;
	lst1	STAT1		;
	eint			;
	ret			;
******************************************************************
AIC_2nd adlk	6,15		;set ACCU_hi = 3 for secondary XMIT
	idle			;Wait for a XINT
	sach	DXR		;
	idle			;ACCU_hi requests 2nd XMIT
	sacl	DXR		;
	idle			;ACCU_lo sets up registers
	sacl	DXR,2		;close command with LSB = 00
	idle			;
	eint			;
	ret			;
********************************************************************
DAT2HOST sacl	  ACCU_lo	;
	 sach	  ACCU_hi	;
	 sar	  AR0,AUX0	;
	 sar	  AR1,AUX1	;
	 call	  BXMIT 	;
	 zals	  ACCU_lo	;
	 addh	  ACCU_hi	;
	 lar	  AR0,AUX0	;
	 lar	  AR1,AUX1	;
	 ret			;
;====================================================================
	.listoff		;
	.ds	0x400		;NOTE: Twiddles are relocated to
	.include "dsk_twid.inc" ;      0x400 (B2) using CONF 1
	.liston
	.end
