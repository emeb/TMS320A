;-----------------------------------------------------------;
; DSK_SPEC.ASM                                              ;
; (C) 1992-93                                               ;
; Texas Instruments Inc.                                    ;
; By: Keith Larson                                          ;
;     TMS320Cxx DSP Applications                            ;
;                                                           ;
; Performs a self-test in native code and transmits RS232   ;
; messages back to the host                                 ;
;-----------------------------------------------------------;
YES     .set    1
NO      .set    0
;----------------------------------------------------------------
BCMD            .set   0xFA10   ;JUMP CMD
BXMIT           .set   0xFA12   ;JUMP XMIT
BXMIT16         .set   0xFA14   ;JUMP XMIT16
BRECV           .set   0xFA06   ;JUMP RECV
BRECV16         .set   0xFA08   ;JUMP RECV16
;----------------------------------------------------------
TA              .set    09h     ;TA =RA = 6h 0000110000011000
RA              .set    09h     ;
TAP             .set    01h     ;TAP=RAP= 1h 0000001000000101
RAP             .set    01h     ;
TB              .set   018h     ;TB =RB =13h 0001101000110110
RB              .set   018h     ;
CTRL            .set   000h     ;CONTROL= 0h 0000000000000011
;-----------------------------------------------------------
TADDR           .set   0x67     ;
CHAR            .set   0x68     ;
STAT0           .set   0x69     ;
STAT1           .set   0x6a     ;
SCRATCH         .set   0x6b     ;
ACCU_lo         .set   0x6c     ;
CMEM            .set   0x6e     ;
AD1             .set   0x70     ;
AD2             .set   0x71     ;
AD3             .set   0x72     ;
;----------------------------------------------------------------
        .include "mmregs.inc"
        .ps     0xFA0A          ; rewrite RINT and XINT
        b       RINT            ;
XINT:   ret                     ;
        ret                     ;
;----------------------------------------------------------------
        .ps     0xfAD0          ;
RINT    sst1    STAT1
        lst1    STAT1
        eint
        ret
;----------------------------------------------------------------
AICLB   .set    00001011b       ;
        .entry
        ldpk    0               ;
sendagn lrlk    AR7,RS232OK     ;
        call    SendMSG         ;
        rsxm                    ;
        conf    1               ;
        lark    AR5,0x60        ;start of B1 (data)
        lark    AR6,0x1f        ;length of B1
        call    CHKMEM          ;
        lrlk    AR5,0x400       ;start of B1 (data)
        lrlk    AR6,0x3FF       ;length of B1
        call    CHKMEM          ;start of B3 (data) is in AR5
;------------------------------------------------------------------
AIC_RST lack    0x80            ;
        sach    DXR             ; clear the DXR
        sacl    GREG            ;
        lrlk    AR0,0xffff      ; Setup a read from global memory
        larp    AR0             ;
        rptk    31              ; pulse /BR low for 32 cycles
        lt      *               ; resetting the AIC
        ;-----------------------
        lalk    RA,2            ; setup xA vals
        adlk    TA,9            ;
        call    AIC2nd          ;
     call    LF              ;
        ;-----------------------
        lalk    RAP,2           ; setup xAP vals
        adlk    TAP,9           ;
        addk    1               ;
        call    AIC2nd          ;
     lrlk    AR7,AIC         ;
     call    SendMSG         ;
        ;-----------------------
        lalk    RB,2            ; setup xB vals
        adlk    TB,9            ;
        addk    2               ;
        call    AIC2nd          ;
     call    LF              ;
        ;-----------------------
        lack    AICLB           ; Set up AIC control register
        call    AIC2nd          ;
;--------------------------------
; Find loopback conversion slope
;--------------------------------
        ssxm                    ; Endpoint
        lalk    0x7000          ; Loopback +0.875%
        call    L_BACK          ;
        sacl    AD1             ;
        lac     DXR             ; Loopback -0.875%
        neg                     ;
        call    L_BACK          ;
        sacl    AD2             ;
        zac                     ; Loopback +0.000%
        call    L_BACK          ;
        sacl    AD3             ;
        ;------------------------
	lac     AD1             ; Check 3 point linearity
        add     AD2             ;
        sfr                     ;
        sub     AD3             ;
	sacl    AD3
	call    XMITHEX         ;
	call    LF              ;

	lrlk    AR7,AIC         ;
	call    SendMSG         ;

	lac     AD3
	abs                     ;
	subk    0x40            ; Max allowable error for 2^-10 linearity
	blz     AICOK           ;
        call    BAD             ;
        b       sendagn         ;
AICOK   call    OK              ;
        b       sendagn         ;
;--------------------------------
L_BACK  sacl    DXR             ;
        call    XMITHEX         ;
        lac     DRR             ;
        call    XMITHEX         ;
        call    LF              ;
        ret                     ;
;===================================================================
AIC2nd  ork     0xC000,2        ; ACC = 0x0003XXXX
        lark    AR7,0x20        ;
        sar     AR7,IMR         ; XINT=on RINT=off
        sar     AR7,DXR         ; Clear DXR LSB's
        idle                    ; XINT flag may already be set
        sach    DXR             ; request AIC register XMIT
        idle                    ;
        sacl    DXR             ; send new register value
        idle                    ;
        sacl    DXR,2           ; clear LSB's
        lack    0x10            ;
        sacl    IMR             ; XINT=off RINT=on
        ret
;-------------------------------------------
;CHKMEM ARx=start data address  ARy=length
;-------------------------------------------
CHKMEM  lrlk    AR7,MEM         ;
        call    SendMSG         ;send message to host 'Checking mem @'
        call    PRT_AR5         ;
        ;----------------------------------
CHKMEM2 larp    AR5             ;
        lac     *               ;get DATA
        xork    0x5555          ;DATA ^ BITMASK1
        sacl    *               ;
        xor     *               ;
        bnz     BADMEM          ;OK?
        lac     *               ;
        xork    0x5555          ;(DATA ^ BITMASK1)^BITMASK1 = DATA
        sacl    *               ;restore original DATA
        xork    0xAAAA          ;DATA ^ BITMASK2
        sacl    *               ;
        xor     *               ;
        bnz     BADMEM          ;OK?
        lac     *               ;
        xork    0xAAAA          ;(DATA ^ BITMASK2)^BITMASK2 = DATA
        sacl    *+              ;restore original DATA
        ;------------------------
MEMOK   call WIDGET             ; Special FX, toggle <,> characters
        ;------------------------
        larp    AR6             ;
        banz    CHKMEM2,*-      ;Check next location
        call    PRT_AR5         ;
        call    OK              ;
        ret                     ;
        ;------------------------
BADMEM  call    PRT_AR5,*+      ;
        call    BAD             ;
        larp    AR6             ;
        banz    CHKMEM2,*-      ;Check next location
        ret                     ;
;==================================================================
; RS232 MESSAGING ROUTINES
;==================================================================
WIDGET  lalk    0x083C          ;< with bs
        sar     AR5,TADDR       ;
        bit     TADDR,15        ;toggle slash for special effect!
        bbz     FSLASH          ;
        lalk    0x083E          ;> with bs
FSLASH  call    BXMIT16         ;
        ret                     ;
;--------------------------------
PRT_AR5 sar     AR5,ACCU_lo     ;
        lac     ACCU_lo         ;
;       call    XMITHEX         ;
;       ret                     ;
;--------------------------------
XMITHEX sacl    ACCU_lo         ;
        sacl    SCRATCH         ;
        lalk    0x7830          ; xmit '0x' via RS232
        call    BXMIT16         ;
        lark    AR4,3           ; shift upper char to ACCU_hi
XMITHX2 lac     SCRATCH,4       ;
        sacl    SCRATCH         ; save lower 3 chars
        sach    CHAR            ; save upper 1 char
        lac     CHAR            ; load char
        andk    0xF             ; Convert 0-9 and A-F to ASCII
        subk    10              ;
        blz     LT_A            ;
        addk    7               ;
LT_A    addk    0x3A ; 57       ;
        call    BXMIT           ; xmit the ASCII character
        larp    AR4             ;
        banz    XMITHX2,*-      ; next char?
        lalk    0x2020          ; SP/SP
        call    BXMIT16         ;
        lac     ACCU_lo         ;
        ret                     ;
;--------------------------------
BAD     lrlk    AR7,BAD_MSG     ;
        call    SendMSG         ;
        call    HENTER          ;
        ret                     ;
;--------------------------------
OK      lrlk    AR7,OK_MSG      ;
        call    SendMSG         ;
;--------------------------------
HENTER  lrlk    AR7,ENTER       ;
        call    SendMSG         ;
        ret                     ;
;--------------------------------
LF      sacl    ACCU_lo         ;
        lack    0xA             ;
        call    BXMIT           ;
        lac     ACCU_lo         ;
        ret                     ;
;--------------------------------
SendMSG dint                    ;
        sar     AR7,TADDR       ;
        lac     TADDR           ;
        tblr    CHAR            ; swap bytes for xmit
        lac     CHAR            ;
        rptk    7               ; send MSB's
        sfr                     ;
        call    s_char          ;
        lac     CHAR            ; send LSB's
        call    s_char          ;
        larp    AR7             ;
        b       SendMSG,*+      ;
        ;------------------------
s_char  andk    0xff            ; if 0 terminate after sending
        subk    0xff            ; if 0xff terminate NO send
	bz      POP_RET         ;
	addk    0xff            ;
	bnz     VCHAR           ;
	popd    TADDR           ; If 0 pop TOS for smaller code size!
VCHAR   call    BXMIT           ; transmit char
	ret                     ;
POP_RET pop                     ;
	ret                     ;
;----------------------------------------------------------------
;RS232OK         .string 0x1b,"[2JDSK-RS232 Loop",9,9," OK"
;----------------------------------------------------------------
RS232OK         .string      "\fDSK-RS232 Loop\t\t\tOK"
ENTER           .string      "\t>enter<",0
AIC             .string      0xA,"AIC Loopback :",-1
MEM             .string      0xA,"C26 MEM Check:",-1
OK_MSG          .string      "\tOK",-1
BAD_MSG         .string      "\tBAD\a",-1
;----------------------------------------------------------------
        .end

