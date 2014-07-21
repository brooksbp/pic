// Simple example for Microchip 16F690 Processor
//
// sdcc -mpic14 -p16f690 --stack-size 8 --use-non-free blinking-led.c
//
// /usr/local/share/sdcc/non-free/include/pic14/pic16f690.h
//
// pk2cmd -PPIC16f690 -Fblinking-led.hex -M -A5.0 -T
// pk2cmd -PPIC16f690 -E

#include <pic16f690.h>
#include <stdint.h>

static __code uint16_t __at (_CONFIG) configword1 = 
    _INTRC_OSC_NOCLKOUT & _WDT_OFF &
    _PWRTE_OFF & _MCLRE_OFF & _CP_OFF &
    _BOR_OFF & _IESO_OFF & _FCMEN_OFF;

void spin(uint32_t cnt) {
  while (cnt-- > 0) { }
}

int led = 0;

void isr(void) __interrupt 0
{
  T0IF = 0;

  RC0 = led;
  if (led) { led = 0; } else { led = 1; }
  //spin(100000);
}

void main(void) {
  // Internal oscillator
  SCS = 1;
  // 8 MHz
  IRCF2 = 1; IRCF1 = 1; IRCF0 = 1;
  // All pins Digital I/O
  ANSEL = 0x00;
  ANSELH = 0x00;

  // Port C pins are Output initialized to V_IL
  TRISC = 0x00;
  PORTC = 0x00;

  // T0 timer mode
  T0CS = 0;
  // Clear TMR0
  TMR0 = 0;

  // Prescaler assigned to Timer0
  PSA = 0;
  // Prescaler rate 1:32
  PS2 = 1; PS1 = 1; PS0 = 0;
  
  // Enable TMR0 interrupts
  T0IE = 1;
  // Enable all interrupts
  GIE = 1;
  
  for (;;) {
    /* ... */
  }
}
