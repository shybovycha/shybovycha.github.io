---
layout: post
title: 'Electronics experiments'
date: '2018-02-26T18:01:00+01:00'
tags: [development-tools, git, version-control]
---

Q: What is the difference between PORTB / DDRB / PINB?
A:

{blockquote}
From a hardware perspective, a "port" usually refers to one of the (usually) several eight-bit groups of parallel I/O pins. The behaviors of the pins in such a group are controlled by three dedicated 8-bit storage locations in the subset of the AVR's "RAM space" commonly referred to as "I/O registers". Three such storage locations are devoted to the control of each "port"s pins. For arbitrary "port X", these locations would be named DDRX, PORTX, and PINX.

The bits of the "DDRX" control byte determine whether the pins of Port X are inputs or outputs. DDRX registers default at powerup reset to having all their bits cleared, which configures all the pins of the corresponding ports as INPUTS. Setting bit(s) in the DDRX register to 1s configure the corresponding port pin(s) as OUTPUTS.

The contents of the PORTX register will determine what data is driven out by those pins of port X that are currently (as determined by the bits of the DDRX register) configured as outputs.

As an extra wrinkle, a PORTX bit that is set does have a subtle effect on the corresponding port pin, even if the associated DDRX bit says, "Be an input" (is clear): it will cause a weak pullup resistor to be connected to the corresponding port X pin. This is useful for minimizing external component useage when interfacing mechanical switches: the switch is used to connect a pulled-up port pin to ground or not.
PORTX register bits also all default to being cleared on powerup.

The PINX register is usually just a read-only digital high/low representation of the electrical signals present on the corresponding port X pins, regardless of what's driving them.

All this is explained better than I can put it in the I/O ports section of the data sheet for any AVR you want to look at.
{endblockquote}

Q: How do I set or clear a specific bit (by its index) in a number in C?
A: 

* to set a bit, use `NUMBER |= (1 << bitIndex)`
* to clear a bit, use `NUMBER &= ~(1 << bitIndex)`

I'll explain these by showing couple of examples:

```
N = 01001110
bitIndex = 0

1 << bitIndex = 00000001

  01001110
| 00000001
= 01001111

~(1 << bitIndex) = 11111110

  01001110
& 11111110
= 01001110
```

```
N = 01001110
bitIndex = 2

1 << bitIndex = 00000100

  01001110
| 00000100
= 01001111

~(1 << bitIndex) = 11111011

  01001110
& 11111011
= 01001010
```

Q: How do I distinguish between pins on input interrupt? / Some pins do not trigger interrupt?
A: use `volatile` variable to store the previous state of your port (-s) on each interrupt and then, in the interrupt handler code,
compare the current state of a port with the stored value, to determine which pins have changed their state.

Consider this simple code:

```c
#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>

#define F_CPU 16000000UL

unsigned char prevBtnState = 0;

#define BTN1 1 << PB3
#define BTN2 1 << PB4

#define LED1 1 << PB1
#define LED2 1 << PB2

void interruptInit() {
    // pin change interrupt enable
    GIMSK = (1 << PCIE);

    // pin change interrupt enabled for PCINT4 (PB3, pin 2) && PCINT3 (PB4, pin 3)
    PCMSK = (1 << PCINT3) | (1 << PCINT4);

    // enable interrupts
    sei();
}

ISR(PCINT0_vect) {
    unsigned char btnState = PINB & PCMSK;

    if ((btnState & BTN1) && !(prevBtnState & BTN1)) {
        PORTB &= ~LED1;
    } else if (!(btnState & BTN1)) {
        PORTB |= LED1;
    }

    if ((btnState & BTN2) && !(prevBtnState & BTN2)) {
        PORTB &= ~LED2;
    } else if (!(btnState & BTN2)) {
        PORTB |= LED2;
    }

    prevBtnState = btnState;
}

int main() {
    DDRB = LED1 | LED2;
    PORTB = 0;

    interruptInit();

    while (1);

    return 0;
}
```
