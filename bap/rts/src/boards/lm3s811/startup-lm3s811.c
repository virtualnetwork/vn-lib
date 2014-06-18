#include <stdint.h>


#pragma weak _nmi_handler
#pragma weak _hardfault_handler
#pragma weak _memman_handler
#pragma weak _busfault_handler
#pragma weak _usagefault_handler
#pragma weak _svcall_handler
#pragma weak _dbg_monitor
#pragma weak _pendsv_handler
#pragma weak _vector_table
#pragma weak irq_handler_ada

extern uint32_t _etext;
extern uint32_t _data;
extern uint32_t _edata;
extern uint32_t _bss;
extern uint32_t _ebss;
extern uint32_t _stack_org;

extern int main();
void _reset();
void _nmi_handler();
void _hardfault_handler();
void _memman_handler();
void _busfault_handler();
void _usagefault_handler();
void _svcall_handler();
void _dbg_monitor();
void _pendsv_handler();
void irq_handler_ada();

void __gnat_last_chance_handler (char *source_location, int line);

// vector table (expand with supported IRQ's or use common handler as-is)
__attribute__ ((section(".vectors"))) void *vector_table[] = {
    &_stack_org,
    _reset,
    _nmi_handler,
    _hardfault_handler,
    _memman_handler,
    _busfault_handler,
    _usagefault_handler,
    0,
    0,
    0,
    0,
    _svcall_handler,
    _dbg_monitor,
    0,
    _pendsv_handler,
    [15 ... 45] = irq_handler_ada
};

void _reset()
{
    uint32_t *rp;
    uint32_t *wp;

    // Move data part from nvmem to ram
    for (rp = &_etext, wp = &_data; wp < &_edata; rp++, wp++) {
        *wp = *rp;
    }

    // init bss section
    for (wp = &_bss; wp < &_ebss; wp++) {
        *wp = 0;
    }

    // jmp to main
    main();

    while (1);
}

void _nmi_handler()
{
    while (1);
}

void _hardfault_handler()
{
    while (1);
}

void _memman_handler()
{
    while (1);
}

void _busfault_handler()
{
    while (1);
}

void _usagefault_handler()
{
    while (1);
}

void _svcall_handler()
{
    while (1);
}

void _dbg_monitor()
{
    while (1);
}

void _pendsv_handler()
{
    while (1);
}

void _systick_handler()
{
    while (1);
}

void irq_handler_ada()
{
    while (1);
}

void __gnat_last_chance_handler (char *source_location, int line)
{
    while (1);
    // return;
}
