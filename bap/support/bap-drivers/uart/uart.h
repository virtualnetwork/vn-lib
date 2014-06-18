
#pragma once

#include <stdint.h>
#include "hw-defs.h"
#include "hw-regs.h"

int32_t uartenable(uint32_t uartbase, uint32_t baudrate);
int32_t uartputc(uint32_t uartbase, uint8_t data);
int32_t uartputs(uint32_t uartbase, const char *str);
int32_t uartgetc(uint32_t uartbase);
