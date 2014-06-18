
#pragma once

//
// NOTE:    Incomplete!!!
// TODO:    Complete!!!

// Helpers
#define REG32(x)        *((volatile unsigned int *)(x))
#define REG16(x)        *((volatile unsigned short *)(x))
#define REG8(x)         *((volatile unsigned char *)(x))

///////////////////////////////////////////////////////////
// Uart
//
#define UART0_IRQ       10
#define UART1_IRQ       11

#define UART0_BASE      0x40000000
#define UART1_BASE      0x40010000
#define UARTRBR         0x0         // Receiver buffer register
#define UARTTHR         0x0         // Transmit holding register
#define UARTDLR         0x0         // Divisor register LSB
#define UARTDMR         0x04        // Divisor register MSB
#define UARTDFR         0x3C        // Fractional divisor register
#define UARTIER         0x04        // Interrupt enable register
#define UARTIEM         0x24        // Multi-mode interrupt enable register
#define UARTIIR         0x08        // Interrupt id register
#define UARTIIM         0x28        // Multi-mode interrupt id register
#define UARTFCR         0x08        // Fifo ctrl register
#define UARTLCR         0x0C        // Line ctrl register
#define UARTLSR         0x14        // Line status register

// UARTIER bits
#define UARTEDSSI       (1 << 3)    // Modem status interrupt enable
#define UARTELSI        (1 << 2)    // Receiver line status interrupt enable
#define UARTBEI         (1 << 1)    // X-mitter holding reg empty int enable
#define UARTBFI         (1 << 0)    // Received data interrupt enable

// UARTLCR bits
#define UARTDLAB        (1 << 7)    // Divisor latch enable
#define UARTSB          (1 << 6)    // Set Break
#define UARTSP          (1 << 5)    // Stick parity
#define UARTEPS         (1 << 4)    // Even parity select
#define UARTPEN         (1 << 3)    // Parity enable
#define UARTSTB         (1 << 2)    // Num stop bits
#define UARTWLS1        (1 << 1)    // Word length select
#define UARTWLS0        (1 << 0)    // Word length select

// UARTLSR bits
#define UARTFIER        (1 << 7)    // Frame error
#define UARTTEMT        (1 << 6)    // Transmit empty
#define UARTTHRE        (1 << 5)    // Transmitter holding empty
#define UARTBI          (1 << 4)    // Break interrupt
#define UARTFE          (1 << 3)    // Framing error
#define UARTPE          (1 << 2)    // Parity error
#define UARTOE          (1 << 1)    // Overrun error
#define UARTDR          (1 << 0)    // Data ready

///////////////////////////////////////////////////////////
// MSSGPIO
//
#define MSSGPIO_BASE    0x40013000

#define MSSGPIOCFG(x)   (x << 2)    // MSSGPIO config register, 0 - 31
#define MSSGPIOIN       0x84
#define MSSGPIOOUT      0x88        // MSSGPIO out

// MSSGPIOCFG bits
#define GPIOINTTYPE2    (1 << 7)    // Interrupt type 
#define GPIOINTTYPE1    (1 << 6)    //
#define GPIOINTTYPE0    (1 << 5)    //
#define GPIOINTEN       (1 << 3)    // Interrupt enable
#define GPIOOUTBEN      (1 << 2)    // Output buffer enable
#define GPIOINEN        (1 << 1)    // Input enable
#define GPIOOUTREN      (1 << 0)    // Output register enable

///////////////////////////////////////////////////////////
// System
//
#define SYSTEM_BASE     0xE000E000

//////////////////////////////////
// Sys timer
#define STCTRL          0x10        // System timer control register
#define STRELOAD        0x14        // System timer reload register
#define STCURRENT       0x18        // System timer count

// STCTRL bits
#define STCOUNT         (1 << 16)   
#define STCLK_SRC       (1 << 2)    // System timer clk source select
#define STINTEN         (1 << 1)    // System timer interrupt enable
#define STENABLE        (1 << 0)    // System timer enable

//////////////////////////////////
// Interrupt control
#define INEN0           0x100       // Interrupt set enable register
#define INDIS0          0x180       // Interrupt clear enable register
#define INPEND0         0x200       // Interrupt set pending register
#define INUNPEND0       0x280       // Interrupt clear pending register
#define INACTIVE0       0x300       // Interrupt active register
#define INCTRL          0xD04       // Interrupt control register

// INCTRL bits   
#define PENDSTSET       (1 << 26)
#define PENDSTCLR       (1 << 25)
