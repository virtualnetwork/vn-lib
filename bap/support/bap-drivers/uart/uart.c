
#include "uart.h"

int32_t uartenable(uint32_t uartbase, uint32_t baudrate)
{
    uint16_t div = (APB0_FREQ/(8 * baudrate) + 1)/2;

    REG32(uartbase + UARTLCR) = UARTDLAB | UARTWLS1 | UARTWLS0;
    REG32(uartbase + UARTDLR) = div;
    REG32(uartbase + UARTDMR) = div >> 8;
    REG32(uartbase + UARTLCR) &= ~UARTDLAB;
    
    return 0;
}

int32_t uartputc(uint32_t uartbase, uint8_t data)
{
    while ((REG32(uartbase + UARTLSR) & UARTTEMT) == 0);
    REG32(uartbase + UARTRBR) = data;

    return 0;
}

int32_t uartputs(uint32_t uartbase, const char *str)
{
    int32_t cnt = 0;

    while (*str) {
        if (uartputc(uartbase, *str) < 0) {
            cnt = -1;
            break;
        }
        str++;
        cnt++;
    }

    return cnt;
}

int32_t uartgetc(uint32_t uartbase)
{   
    while ((REG32(uartbase + UARTLSR) & UARTDR) == 0);
    
    return REG32(uartbase + UARTTHR);
}
