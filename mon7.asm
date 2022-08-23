; mon7.asm
; 320bytes 65C02 monitor program for ACIA W65C51N 
; and virtual 6502 / Assembler         
; https://www.masswerk.at/6502/assembler.html
; (C) 2022 Andre Adrian
; 2022-08-13 based on mon5, with RTS/CTS, with IRQ
; 2022-08-15 256Bytes ACIA RX buffer
; 2022-08-16 same API for 256bytes and 320bytes monitor

; 1.8432MHz clock, 9600bps 8n1 symbol -> 1920 cycles
; constants
  PUTCDLY = 383     ; unit 5 cyles
  
  ACIABASE = $8000
  ACIA_RX = ACIABASE
  ACIA_TX = ACIABASE
  ACIA_STATUS = ACIABASE+1
  ACIA_COMMAND = ACIABASE+2
  ACIA_CONTROL = ACIABASE+3
  
  LF = $0A
  CR = $0D
  SPACE = $20
  COLON = ':
  BACKSLSH = '\
  PRINT = 'p
  RUN = 'r

; zero page
  monbyte =  $0   ; zp 1 byte used by getbyte/putbyte
  monword =  $1   ; zp 2 bytes used by getword/putword
  monwrptr = $3   ; zp 2 bytes write pointer used by getc
  monrdptr = $5   ; zp 2 bytes read pointer used by getc
  monbfcnt = $7   ; zp 1 byte buf count used by getc
  montmpy =  $8   ; zp 1 byte tmp used by getc

; other
  monbuf = $7f00  ; ram 256 bytes used by getc
  nmihook = monbuf-3  ; ram 3 bytes used by mon

; ************************************
; monitor  
  .org $fec0
  
reset:
  ; init cpu, zero page
  cld               ; binary mode
  ldx #$ff
  txs               ; stack=$01ff
  lda #<monbuf
  sta monwrptr
  sta monrdptr
  sta monbfcnt
  lda #>monbuf
  sta monwrptr+1
  sta monrdptr+1
  ; init acia
  lda #%00001001    ; parity disable, no echo, /rts=low, interrupts, /dtr=low
  sta ACIA_COMMAND
  lda #%00011110    ; 1 stop bit, 8 data bits, baud rate, 9600 bps
  sta ACIA_CONTROL
  cli           ; enable irq and brk
main:
  jsr putcrlf
  lda #BACKSLSH
  jsr putc
  jsr getword
  jsr echoc     ; expect COLON or RUN or PRINT
  ldy #0
  cmp #COLON
  bne main1
cmdcolon:
  jsr echoc     ; expect SPACE or CR or LF
  cmp #SPACE
  bne main      ; assume CR or LF
  jsr getbyte
  sta (monword),y
  iny
  jmp cmdcolon
main1:
  cmp #RUN
  bne main2
  jsr putcrlf
  ; push monword-1 on stack because rts returns to address+1
  ; push hi byte, then lo byte
  sec
  lda monword
  sbc #1
  tax           ; save lo result
  lda monword+1
  sbc #0
  pha           ; first push hi byte
  txa
  pha           ; then lo byte
  rts           ; jmp (monword)
main2:          ; assume PRINT
  lda #SPACE
  jsr putc
  lda (monword),y
  jsr putbyte
  iny
  cpy #8
  bne main2
  beq reset     ; maybe trace used it

trace:
  jsr putcrlf
  lda #'!
  jsr putc
  tsx     ; s is top_of_stack minus 1
  inx     ; correct to top_of_stack
  stx monword
  ldy #1
  sty monword+1
  dey     ; y=0
  beq main2     ; bra

monirq:
; on stack psr, pcl, pch
  pha
  txa
  pha
  tya
  pha     ; on stack y, x, a, psr, pcl, pch
  ; 65C02 does not set bit 4 in PSR after BRK opcode
  bit ACIA_STATUS
  bpl trace         ; no ACIA irq, assume BRK opcode
  lda ACIA_RX
  ldy #0            ; monbuf[monwrptr++] = a
  sta (monwrptr),y
  inc monwrptr
  inc monbfcnt
  lda #128          ; high water mark
  cmp monbfcnt
  bne monirq1
  lda #%00001000    ; parity disable, no echo, /rts=low, interrupts, /dtr=high
  sta ACIA_COMMAND
monirq1:
  pla         ; restore y, x, a, psr, pcl, pch
  tay
  pla
  tax
  pla
  rti

getca:
; jsr acia input, return a=char
  lda monbfcnt
  beq getca
  sty montmpy        ; phy
  ldy #0            ; monbuf[monrdptr++] = a
  lda (monrdptr),y
;  pha
  inc monrdptr
  dec monbfcnt
;  lda #16           ; low water mark
;  cmp monbfcnt
  bne getc1         ; low water mark == 0
  ldy #%00001001    ; parity disable, no echo, /rts=low, interrupts, /dtr=low
  sty ACIA_COMMAND
getc1:
;  pla
  ldy montmpy
  rts
  
  .org $ff7E  ; make mon api stable

getc:
  jmp getca

echoc:
; jsr acia input with echo, return a=char
  jsr getc

putc:
; jsr acia output, a=char
; W65C51N datasheet: "A delay should be used to insure that the shift 
; register is empty before the TDR/TSR is reloaded"
  pha           ; save a
  lda #0        ; 256
putc1:
  sbc #1        ; don't care for carry
  bne putc1
  lda #PUTCDLY-256
putc2:
  sbc #1
  bne putc2
  pla           ; restore a
  sta ACIA_TX
  rts

asc2hex:
; jsr convert ASCII to hex, a=ascii, return a=hex
  cmp #$60  ; is lower char?
  bcc asc2hex1
  sbc #$20  ; 'a'-'A'
asc2hex1:
  sec
  sbc #$30  ; '0'
  cmp #$A
  bcc asc2hex2
  sbc #7    ; 'A'-'9'
asc2hex2:
  rts
  
hex2asc:
; jsr convert hex to ASCII, a=hex, return a=ascii
  clc
  adc #$30  ; '0'
  cmp #$3A  ; '9'+1
  bcc hex2asc1
  adc #6    ; 'A'-'9'-Carry
hex2asc1:
  rts
  
getbyte:
; jsr input 2 Hex digits in ASCII, return a=byte
  jsr echoc
  jsr asc2hex
  asl
  asl
  asl
  asl
  sta monbyte
  jsr echoc
  jsr asc2hex
  ora monbyte
  rts
  
putbyte:
; jsr output 2 Hex digits as ASCII, a=byte
  sta monbyte
  lsr
  lsr
  lsr
  lsr
  jsr hex2asc
  jsr putc
  lda monbyte
  and #$0f
  jsr hex2asc
  jmp putc  ; rts via putc
  
getword:
; jsr input 4 Hex digits in ASCII, return monword=word
  jsr getbyte
  sta monword+1
  jsr getbyte
  sta monword
  rts
  
putword:
; jsr output 4 Hex digits as ASCII, monword=word
  lda monword+1
  jsr putbyte
  lda monword
  jmp putbyte  ; rts via putbyte
  
putcrlf:
; jsr output Carrige Return, Line Feed
  lda #CR
  jsr putc
  lda #LF
  jmp putc      ; rts via putc

; ************************************
; vectors

  .org $fffa
  
  .word	nmihook ; (NMI vector)
  .word	reset		; (RESET vector)
  .word	monirq  ; (IRQ vector)

  .end