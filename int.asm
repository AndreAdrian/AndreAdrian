; int.asm
; 6502 16bit signed, unsigned integer package with 32bit */ scaling operator
; size 512bytes
; (C) 2022 Andre Adrian
; Assembler:
; https://www.masswerk.at/6502/assembler.html

; 2022-08-02 first version
; 2022-08-08 add divide
; 2022-08-12 add puts
; 2022-08-15 for mon7
; 2022-08-16 add uns2dec
; 2022-08-17 add intdiv, int2dec
; 2022-08-18 add dec2int
; 2022-08-19 add gets
; 2022-08-23 no gap between mon zp and int zp variables

; import mon7 9600bps/38400bps, RTS/CTS, 256bytes rx buf
  monbyte = $0    ; 1 byte used by getbyte/putbyte
  monword = $1    ; 2 bytes used by getword/putword
  monwrptr= $3    ; 2 bytes buf write pointer used by getc
  monrdptr= $5    ; 2 bytes buf read pointer used by getc
  monbfcnt= $7    ; 1 byte buf count used by getc
  montmpy = $8    ; 1 byte tmp used by getc
  nmihook = $7EFD ; 3 bytes used by mon
  monbuf  = $7F00 ; 256 bytes buf used by getc
  getc    = $FF7E ; jsr acia input, return a=char
  echoc   = $FF81 ; jsr acia input with echo, return a=char
  putc    = $FF84 ; jsr acia output, a=char
  asc2hex = $FF96 ; jsr convert ASCII to hex, a=ascii, return a=hex
  hex2asc = $FFA6 ; jsr convert hex to ASCII, a=hex, return a=ascii
  getbyte = $FFB0 ; jsr input 2 Hex digits in ASCII, return a=byte
  putbyte = $FFC5 ; jsr output 2 Hex digits as ASCII, a=byte
  getword = $FFDB ; jsr input 4 Hex digits in ASCII, return monword=word
  putword = $FFE6 ; jsr output 4 Hex digits as ASCII, monword=word
  putcrlf = $FFF0 ; jsr output Carrige Return, Line Feed

; mon aliases
  inttmpy = monbyte   ; 1 byte used by uns2dec
  strptr  = monword   ; 2 bytes used by puts, dec2int, int2dec, uns2dec

; int
  intsign = $9   ; 1 byte used by dec2int, intdiv, i32div
  intarg  = $A   ; 2 bytes 16bit argument
  intacc  = $C   ; 4 bytes 32bit accumulator (mul, div need 32bit)

; int aliases
  plier   = intacc    ; in multiplier
  plicand = intarg    ; in multiplicand
  dividend= intacc    ; in (denominator)
  divisor = intarg    ; in (numerator)
  result  = intacc    ; out low(plier * plicand) = dividend / divisor
  resulth = intacc+2  ; out high(plier * plicand)
  remaindr= intacc+2  ; out dividend % divisor

; const
  NUL = 0     ; ASCIIZ terminal char
  LF  = $0A   ; ASCII line feed
  CR  = $0D   ; ASCII carrige return
  SPC = $20   ; ASCII space

; Program im ROM
  .org $fcc0  ; 512 bytes below monitor $fec0

; ************************************
; int.asm implementation (512bytes)

; subroutine naming:
; puts, gets = console output ASCIIZ string, console input ASCIIZ string
; dec2int = ASCIIZ decimal to signed or unsigned 16bit
; int2dec, uns2dec = signed 16bit to ASCIIZ decimal, unsigned 16bit to ASCIIZ decimal
; the following subroutines are written type (int, uns, i32, u32) operation (mul, div)
; and optional argument passing (a) like intdiva
; int, uns, i32, u32 = signed 16bit, unsigned 16bit, signed 32bit, unsigned 32bit
; mul, div = multiplication, division (do addition, subtraction inline)
; optional a = use register a, x for low byte, high byte argument

getsa:
; in: console, out: x=high, a=low ASCII pointer len<256
  sta strptr
  stx strptr+1

gets:
; in: console, out: strptr ASCIIZ pointer len<256
  ldy #0
: jsr echoc
  sta (strptr),y
  iny
  cmp #SPC
  bcc :+
  bcs :-
: jmp uns2dece    ; append NUL

putsa:
; in: x=high, a=low ASCIIZ pointer len<256, out: console
  sta strptr
  stx strptr+1

puts:
; in: strptr ASCIIZ pointer len<256, out: console
  ldy #0
: lda (strptr),y
  beq :+
  jsr putc
  iny
  bne :-     ; bra
: rts

dec2inta:
; signed/unsigned decimal ASCII to 16bit signed/unsigned
; ASCII number is terminated by char<'0' like SPC, CR, LF
; in: x=high, a=low ASCII pointer len<256, out: intacc
  sta strptr
  stx strptr+1

dec2int:
; signed/unsigned decimal ASCII to 16bit signed/unsigned
; ASCII number is terminated by char<'0' like SPC, CR, LF
; in: strptr ASCIIZ pointer len<256, out: intacc
  ldy #0
  sty intsign
  sty intacc
  sty intacc+1
  lda (strptr),y
  cmp #'-
  bne :+
  lda #$80
  sta intsign
  jsr strptrpp
: lda (strptr),y
  iny
  sec             ; ASCII code to digit value
  sbc #'0
  bcc :+          ; if a<'0 then return
  pha
  jsr intm10      ; shift decimal left
  pla
  clc             ; add as last digit
  adc intacc
  sta intacc
  bcc :-
  inc intacc+1
  bcc :-          ; bra
: lda intsign
  bpl :+
  jsr intaccng
: rts

strptrpp:
; strptr++
  clc
  lda strptr
  adc #1          ; note inc does not set carry
  sta strptr
  bcc :-          ; if result<$100 then skip
  inc strptr+1
: rts

int2deca:
; signed 16bit to signed decimal ASCII
; in: intacc, out: x=high, a=low ASCIIZ pointer len<256
  sta strptr
  stx strptr+1

int2dec:
; signed 16bit to signed decimal ASCII
; in: intacc, out: strptr ASCIIZ pointer len<256
  lda intacc+1
  bpl :+
  jsr intaccng
  ldy #0
  lda #'-
  sta (strptr),y
  jsr strptrpp
: jmp uns2dec

uns2deca:
; unsigned 16bit to unsigned decimal ASCII
; in: intacc, out: x=high, a=low ASCIIZ pointer len<256
  sta strptr
  stx strptr+1

uns2dec:
; unsigned 16bit to unsigned decimal ASCII
; in: intacc out: strptr ASCIIZ pointer len<256
  lda #0
  sta inttmpy     ; 2nd y
  ldy #'0         ; do 10000s
: sec             ; provisional sub
  lda intacc
  sbc #<10000
  tax             ; store low sub result to tmp
  lda intacc+1
  sbc #>10000
  bcc :+          ; if intacc<10000 then ignore sub result
  stx intacc      ; else store sub result permanent
  sta intacc+1
  iny             ; the "remainder"
  bne :-          ; bra
: jsr uns2dech
  ldy #'0         ; do 1000s
: sec
  lda intacc
  sbc #<1000
  tax
  lda intacc+1
  sbc #>1000
  bcc :+
  stx intacc
  sta intacc+1
  iny
  bne :-
: jsr uns2dech
  ldy #'0         ; do 100s
: sec
  lda intacc
  sbc #<100
  tax
  lda intacc+1
  sbc #>100
  bcc :+
  stx intacc
  sta intacc+1
  iny
  bne :-
: jsr uns2dech
  ldy #'0         ; do 10s
: sec
  lda intacc
  sbc #<10
  tax
  lda intacc+1
  sbc #>10
  bcc :+
  stx intacc
  sta intacc+1
  iny
  bne :-
: jsr uns2dech
  clc             ; do 1s
  lda intacc
  adc #'0
  ldy inttmpy
  sta (strptr),y
  iny
uns2dece:
  lda #NUL        ; append NUL
  sta (strptr),y
  rts

uns2dech:
; private
; print without leading zeros, but with trailing zeros (tricky)
  tya
  ldy inttmpy
  bne :+          ; if not first char, skip no leading '0's test
  cmp #'0
  beq :++         ; no leading '0's test
: sta (strptr),y
  inc inttmpy
: rts

intm10:
unsm10:
; signed/unsigned integer multiply const 10
; in: intacc, out: intacc *= 10
  lda intacc      ; intacc *= 2
  asl
  sta intacc
  sta intarg      ; intarg = intacc
  lda intacc+1
  rol
  sta intacc+1
  sta intarg+1
  asl intacc      ; intacc *= 4
  rol intacc+1
  asl intacc
  rol intacc+1

intadd:
unsadd:
; signed/unsigned integer add
; in: intacc, intarg
; out: intacc += intarg
  clc
  lda intacc
  adc intarg
  sta intacc
  lda intacc+1
  adc intarg+1
  sta intacc+1
  rts

intmula:
unsmula:
; signed/unsigned integer multiply 16bit*16bit=32bit
; in: x=high, a=low value, intarg, out: 32bit (intacc, resulth)
  sta intacc
  stx intacc+1

intmul:
unsmul:
; signed/unsigned integer multiply 16bit*16bit=32bit
; in: intacc, intarg out: 32bit (intacc, resulth)

; 1986-01, Bob Sander-Cederlof, Fast 6502 & 65802 Multiply Routines
; http://www.txbobsc.com/aal/1986/aal8601.html#a5

  lda #0
  sta resulth
  sta resulth+1
  ldx #16
: lda plier     ; check next bit of multiplier
  lsr
  bcc :+        ; ...don't add multiplicand
  clc           ; 16bit add
  lda resulth
  adc plicand
  sta resulth
  lda resulth+1
  adc plicand+1
  sta resulth+1
: ror resulth+1 ; 32bit shift right
  ror resulth
  ror result+1
  ror result
  dex
  bne :--
  rts

intargng:
; intarg = -intarg = 0 - intarg
  sec
  lda #0
  sbc intarg
  sta intarg
  lda #0
  sbc intarg+1
  sta intarg+1
  rts

intaccng:
; intacc = -intacc = 0 - intacc
  sec
  lda #0
  sbc intacc
  sta intacc
  lda #0
  sbc intacc+1
  sta intacc+1
  rts

i32accng:
; 32bit intacc = -intacc = 0 - intacc
  sec
  ldx #0
  txa
  sbc intacc
  sta intacc
  txa
  sbc intacc+1
  sta intacc+1
  txa
  sbc intacc+2
  sta intacc+2
  txa
  sbc intacc+3
  sta intacc+3
  rts

intdiva:
; signed integer divide 16bit/16bit=16bit
; in: x=high, a=low value, intarg, out: intacc=acc/arg, remaindr=acc%arg
  sta intacc
  stx intacc+1

intdiv:
; signed integer divide 16bit/16bit=16bit
; in: intacc, intarg, out: intacc=acc/arg, remaindr=acc%arg
  lda intacc+1    ; calc result sign
  eor intarg+1
  sta intsign     ; save it
  lda intacc+1
  bpl :+          ; jump if intacc is positiv
  jsr intaccng
: lda intarg+1
  bpl :+          ; jump if intarg is positiv
  jsr intargng
: jsr unsdiv

intdivco:         ; common part
; private
  lda intsign
  bpl :+          ; jump if intsign is positiv
  jsr intaccng
: rts

i32div:
; signed integer divide 32bit/16bit=16bit
; in: 32bit (intacc, remaindr), intarg out: intacc=acc/arg, remaindr=acc%arg
  lda intacc+3
  eor intarg+1
  sta intsign
  lda intacc+3
  bpl :+
  jsr i32accng
i32divco:         ; common
: lda intarg+1
  bpl :+          ; jump if intarg is positiv
  jsr intargng
: jsr u32div
  jmp intdivco

unsdiva:
; unsigned integer divide 16bit/16bit=16bit
; in: x=high, a=low value, intarg, out: intacc=acc/arg, remaindr=acc%arg
  sta intacc
  stx intacc+1

unsdiv:
; unsigned integer divide 16bit/16bit=16bit
; in: intacc, intarg, out: intacc=acc/arg, remaindr=acc%arg

  lda #0          ;preset remainder to 0
  sta remaindr
  sta remaindr+1

u32div:
; unsigned integer divide 32bit/16bit=16bit
; in: 32bit (intacc, remaindr), intarg, out: intacc=acc/arg, remaindr=acc%arg

; https://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
; with my 32bit/16bit extension

  ldy #16
u32div2:
  asl dividend    ;32bit shift left
  rol dividend+1
  rol remaindr
  rol remaindr+1
  bcs u32div5     ;if bit31==1 then do 32bit/16bit extension
  sec             ;provisional 16bit sub
  lda remaindr
  sbc divisor
  tax             ;store result to tmp
  lda remaindr+1
  sbc divisor+1
  bcc u32div4     ;if remaindr<divisor then ignore sub result
u32div3:
  stx remaindr    ;else store sub result permanent
  sta remaindr+1
  inc result      ;and INCrement result cause divisor fit in 1 times
u32div4:
  dey
  bne u32div2
  rts
u32div5:          ;32bit/16bit extension
  sec             ;permanent 16bit sub
  lda remaindr
  sbc divisor
  tax
  lda remaindr+1
  sbc divisor+1
  jmp u32div3     ;bypass opcode bcc u32div4

  .byte 0         ; align to 8 bytes

  .end
