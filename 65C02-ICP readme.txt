The 65C02-ICP is my little 65C02 computer. I have 32kByte RAM, 8kByte EEPROM, a (buggy) WDC 65C51N ACIA and 7.3728MHz clock.
Like Ben Eater I use a single 74HC00 for glue logic. And I have in-circuit-programming. This is the big difference to other 
6502 single board computers. I need no EEPROM programmer. I have two CP2102 USB-to-UART bridges. One I use to program the 
EEPROM in-circuit, the other I use to communicate between my MS-Windows computer via TeraTerm terminal emulator program and 
my 65C02 with 38400bps.
My monitor program - mon7.asm - uses CTS/RTS hardware handshake, interrupt driven receive and a 256 Byte reeive buffer to
work around the 65C51N TDRE (transmit data register empty) bug. I assume my monitor is compatible to other 6551.
The monitor has three commands. Download bytes into memory with the : command, examine memory with the p command and execute
a program with the r command. If the program executes a BRK opcode, the monitor prints a trace. That is the contents of the
Y, X, A, P registers and the PC plus two more bytes on the stack. The monitor is 320Bytes. The mon7 api is:

  monbyte = $0    ; 1 byte used by getbyte/putbyte
  monword = $1    ; 2 bytes used by getword/putword
  monwrptr= $3    ; 2 bytes buf write pointer used by getc
  monrdptr= $5    ; 2 bytes buf read pointer used by getc
  monbfcnt= $7    ; 1 byte buf count used by getc
  montmpy = $8    ; 1 byte tmp used by getc
  nmihook = $7EFD ; 3 bytes used by mon
  monbuf  = $7F00 ; 256 bytes buf used by getc
  getc    = $FF7E ; jsr console (65C51) input, out: a=char
  echoc   = $FF81 ; jsr console input with echo, out: a=char
  putc    = $FF84 ; jsr console output, in: a=char
  asc2hex = $FF96 ; jsr convert ASCII to hex, in: a=ascii, out: a=hex
  hex2asc = $FFA6 ; jsr convert hex to ASCII, in: a=hex, return a=ascii
  getbyte = $FFB0 ; jsr input 2 Hex digits in ASCII, out: a=byte
  putbyte = $FFC5 ; jsr output 2 Hex digits as ASCII, in: a=byte
  getword = $FFDB ; jsr input 4 Hex digits in ASCII, out: monword=word
  putword = $FFE6 ; jsr output 4 Hex digits as ASCII, in: monword=word
  putcrlf = $FFF0 ; jsr output Carrige Return, Line Feed

The second program is int.asm. This is a 16Bit integer package with some basic ASCIIZ string subroutines. For subroutine
arguments (parameters) I use register a=low byte, x=high byte. You can place the parameters into "well known" zero page
variables and call the subroutines. The 16Bit integer package is fast. I used the fastest mul and div subroutine from the
net and I used my own fast decimal to integer conversation subroutine. The same dec2int idea I used in the 1980s on my Z80 
32Bit integer package. The int api is:

  intsign = $9   ; 1 byte used by dec2int, intdiv, i32div
  intarg  = $A   ; 2 bytes 16bit argument
  intacc  = $C   ; 4 bytes 32bit accumulator (mul, div need 32bit)
  getsa   = $FCC0 ; in: console, out: x=high, a=low ASCII pointer len<256
  gets    = $FCC4 ; in: console, out: strptr ASCIIZ pointer len<256
  putsa   = $FCD5 ; in: x=high, a=low ASCIIZ pointer len<256, out: console
  puts    = $FCD9 ; in: strptr ASCIIZ pointer len<256, out: console
  ; signed/unsigned decimal ASCII to 16bit signed/unsigned
  dec2inta= $FCE6 ; in: x=high, a=low ASCII pointer len<256, out: intacc
  dec2int = $FCEA ; in: strptr ASCIIZ pointer len<256, out: intacc
  ; signed 16bit to signed decimal ASCII
  int2deca= $FD2B ; in: intacc, out: x=high, a=low ASCIIZ pointer len<256
  int2dec = $FD2F ; in: intacc, out: strptr ASCIIZ pointer len<256
  ; unsigned 16bit to unsigned decimal ASCII
  uns2deca= $FD42 ; in: intacc, out: x=high, a=low ASCIIZ pointer len<256
  uns2dec = $FD46 ; in: intacc out: strptr ASCIIZ pointer len<256
  intmula = $FDEB ; in: x=high, a=low value, intarg, out: 32bit (intacc, resulth)
  unsmula = $FDEB ; in: x=high, a=low value, intarg, out: 32bit (intacc, resulth)
  intmul  = $FDEF ; in: intacc, intarg out: 32bit (intacc, resulth)
  unsmul  = $FDEF ; in: intacc, intarg out: 32bit (intacc, resulth)
  intdiva = $FE49 ; in: x=high, a=low value, intarg, out: intacc=acc/arg, remaindr=acc%arg
  intdiv  = $FE4D ; in: intacc, intarg, out: intacc=acc/arg, remaindr=acc%arg
  i32div  = $FE6C ; in: 32bit (intacc, remaindr), intarg, out: intacc=acc/arg, remaindr=acc%arg
  unsdiva = $FE86 ; in: x=high, a=low value, intarg, out: intacc=acc/arg, remaindr=acc%arg
  unsdiv  = $FE8A ; in: intacc, intarg, out: intacc=acc/arg, remaindr=acc%arg
  u32div  = $FE90 ; in: 32bit (intacc, remaindr), intarg, out: intacc=acc/arg, remaindr=acc%arg

The int program is 512Bytes. There are no addition or subtraction subroutines. You write these better
as inline. My int package can do scaling operation. That is a multiplication followed by a division
with a 32Bit intermediate result. Some reader will known the Forth programming language command */.

Last but to least is int_t.asm, a test program for the int package. A typical output of the program
is:


