
#pragma once

// Helpers
#define REG32(x)        *((volatile unsigned int *)(x))

///////////////////////////////////////////////////////////
// Uart
//
#define UART0_IRQ       5
#define UART1_IRQ       6

#define UART0_BASE      0x4000C000
#define UART1_BASE      0x4000D000
#define UARTDR          0x0         // Data register
#define UARTFR          0x18        // Flag register
#define UARTIBRD        0x24        // Divisor register
#define UARTLCRH        0x2C        // Line control register
#define UARTCTL         0x30        // UART control register
#define UARTIM          0x38        // Interrupt mask register
#define UARTRIS         0x3C        // Interrupt raw status register
#define UARTMIS         0x40        // Interrupt masked status register
#define UARTICR         0x44        // Interrupt clear register

// UARTDR bits (7 - 0 is data) when FIFO is enabled
#define UARTDROE        (1 << 11)   // Overrun error
#define UARTDRBE        (1 << 10)   // Break error
#define UARTDRPE        (1 << 9)    // Parity error
#define UARTDRFE        (1 << 8)    // Framing error

// UARTFR bits
#define UARTFRTXFE      (1 << 7)    // Tx FIFO empty
#define UARTFRRXFF      (1 << 6)    // Rx FIFO full
#define UARTFRTXFF      (1 << 5)    // Tx FIFO full
#define UARTFRRXFE      (1 << 4)    // Rx FIFO empty
#define UARTFRBUSY      (1 << 3)    // Tx Busy

// UARTLCRH bits
#define UARTLCRHSPS     (1 << 7)    // Stick parity
#define UARTLCRHWLEN1   (1 << 6)    // Word length, high bit
#define UARTLCRHWLEN0   (1 << 5)    //  "       " , low bit
#define UARTLCRHFEN     (1 << 4)    // FIFO enable
#define UARTLCRHSTP2    (1 << 3)    // 2-stop bits enable
#define UARTLCRHEPS     (1 << 2)    // Even parity select
#define UARTLCRHPEN     (1 << 1)    // Parity enable
#define UARTLCRHBRK     (1 << 0)    // Send break

// UARTCTL bits
#define UARTCTLRXE      (1 << 9)    // Receive enable
#define UARTCTLTXE      (1 << 8)    // Transmit enable
#define UARTCTLLBE      (1 << 7)    // Loop back enable
#define UARTCTLEN       (1 << 0)    // UART enable

// UARTIM, UARTRIS, UARTMIS and UARTICR bits
#define UARTOE          (1 << 10)   // Overrun error
#define UARTBE          (1 << 9)    // Break error
#define UARTPE          (1 << 8)    // Parity error
#define UARTFE          (1 << 7)    // Frame error
#define UARTRT          (1 << 6)    // RX timeout
#define UARTTX          (1 << 5)    // TX
#define UARTRX          (1 << 4)    // RX


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
