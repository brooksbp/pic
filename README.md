8-bit PIC16 MCU development in Haskell.

Quickstart:

```bash
$ cabal sandbox init
$ cabal install --only-dependencies --enable-tests
$ cabal build
$ cabal test
```

Examples:

```bash
alias ghc-sandbox='ghc -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d'
```

```bash
# Parse and show Intel Hex.

$ ghc-sandbox examples/IntelHex_parse.hs
$ ./examples/IntelHex_parse etc/empty.hex
:020000040000FA
:1000000000008A110328D330F6000030F70080305A
:100010008A110000B2208A11000083120313A1008C
:100020007708A000D530A2000030A300831203138C
:100030002008A4002108A500FF30A007031CA1038D
:100040002408250403199B2804302207A400A501D5
:10005000A50D2308A5072408F6002508F700803021
:100060008A110000B2208A11000083120313A70036
...
```

```bash
# Show disassembled machine instructions from Intel Hex.

$ ghc-sandbox examples/MachInst.hs 
[brian@kale ~/code/pic]$ ./examples/MachInst etc/empty.hex 
000000   0000     nop
000001   118a     bcf   0xa, 0x3
000002   2803     goto  0x3
000003   30d3     movlw 0xd3
000004   00f6     movwf 0x76
000005   3000     movlw 0x0
000006   00f7     movwf 0x77
000007   3080     movlw 0x80
000008   118a     bcf   0xa, 0x3
000009   0000     nop
00000a   20b2     call  0xb2
00000b   118a     bcf   0xa, 0x3
00000c   0000     nop
00000d   1083     bcf   0x3, 0x5
...
```

```bash
# Parse, preprocess, and show linker script file.

$ ghc-sandbox examples/LinkerScript.hs 
$ ./examples/LinkerScript etc/16f690_g.lkr 
LIBPATH .
CODEPAGE NAME=page0 START=0x0 END=0x7ff
CODEPAGE NAME=page1 START=0x800 END=0xfff
CODEPAGE NAME=.idlocs START=0x2000 END=0x2003 PROTECTED
CODEPAGE NAME=devid START=0x2006 END=0x2006 PROTECTED
CODEPAGE NAME=.config START=0x2007 END=0x2007 PROTECTED
CODEPAGE NAME=.calib START=0x2008 END=0x2008 PROTECTED
CODEPAGE NAME=eedata START=0x2100 END=0x21ff PROTECTED
DATABANK NAME=sfr0 START=0x0 END=0x1f PROTECTED
DATABANK NAME=sfr1 START=0x80 END=0x9f PROTECTED
DATABANK NAME=sfr2 START=0x100 END=0x11f PROTECTED
DATABANK NAME=sfr3 START=0x180 END=0x19f PROTECTED
DATABANK NAME=gpr0 START=0x20 END=0x6f
DATABANK NAME=gpr1 START=0xa0 END=0xef
DATABANK NAME=gpr2 START=0x120 END=0x16f
SHAREBANK NAME=gprnobnk START=0x70 END=0x7f
SHAREBANK NAME=gprnobnk START=0xf0 END=0xff PROTECTED
SHAREBANK NAME=gprnobnk START=0x170 END=0x17f PROTECTED
SHAREBANK NAME=gprnobnk START=0x1f0 END=0x1ff PROTECTED
SECTION NAME=PROG0 ROM=page0
SECTION NAME=PROG1 ROM=page1
SECTION NAME=IDLOCS ROM=.idlocs
SECTION NAME=CALIBR ROM=.calib
SECTION NAME=DEEPROM ROM=eedata
```

TODO

Assembly lang (.asm)
- AST
- Parser
- Printer

.asm + .lkr -> .hex

.asm -> .o
.o + .lkr -> .hex

.asm directives
