The 65C02-ICP is my little 65C02 computer. I have 32kByte RAM, 8kByte EEPROM, a (buggy) WDC 65C51N ACIA and 7.3728MHz clock.
Like Ben Eater I use a single 74HC00 for glue logic. And I have build in in-circuit-programming. This is the big difference
to other 6502 single board computers. I have two CP2102 USB-to-UART bridges. One I use to program the EEPROM in-circuit,
the other I use to communicate from my MS-Windows computer via TeraTerm terminal emulator program to my 65C02 with 38400bps.
