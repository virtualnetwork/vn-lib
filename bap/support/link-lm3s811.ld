
MEMORY
{
    FLASH (rx) : ORIGIN = 0x00000000, LENGTH = 64K
    SRAM (rwx) : ORIGIN = 0x20000000, LENGTH = 8K
}

_stack_org = ORIGIN(SRAM) + LENGTH(SRAM);
__stack_end = _stack_org;
__stack_start = _ebss;

SECTIONS
{
    .text :
    {
        *(.vectors)
        *(.text*)
        *(.rodata*)
        . = ALIGN(4);
        _etext = .;
    } > FLASH

    .ARM.exidx :
    {
        *(.ARM.exidx* .gnu.linkonce.armexidx.*)
        . = ALIGN(4);
    } > FLASH

    _init_data = .;

    .data : AT(_init_data)
    {
        _data = .;
        *(.data*)
        . = ALIGN(4);
        _edata = .;
    } > SRAM

    .bss :
    {
        _bss = .;
        *(.bss*)
        . = ALIGN(4);
        _ebss = .;
    } > SRAM

    _end = .;
}
