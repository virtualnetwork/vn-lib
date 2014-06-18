/**
  @file context_switch-bb.c
  @brief Context switch handler for ARM Cortex-M

  This file is part of the BAP Code Educational Library (BAPCOLIB).\n
  This is the Context Switch Handler for ARM Cortex-M family.
  @copyright BAP - Bruhnspace Advanced Projects AB 2014
  @version 0.1
  @date 2014-03-23
  @author Magnus Norgren magnus@adv.bruhnspace.com
  @bug No known bug
  @todo Something
  @todo Something more
  @section LICENSE
    BAPCOLIB is free software: you can redistribute it and/or modify\n
    it under the terms of the GNU General Public License as published by\n
    the Free Software Foundation, either version 3 of the License, or\n
    (at your option) any later version.\n
  \n
    BAPCOLIB is distributed in the hope that it will be useful,\n
    but WITHOUT ANY WARRANTY; without even the implied warranty of\n
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n
    GNU General Public License for more details.\n
  \n
    You should have received a copy of the GNU General Public License\n
    along with BAPCOLIB.  If not, see <http://www.gnu.org/licenses/>.
*/

void context_switch()
{
    // Initiate pendsv exception
    *(volatile unsigned long *)0xE000ED04 |= (1 << 28);
}

// Switch context
__attribute__ ((naked)) void _pendsv_handler()
{
    asm volatile (
        // Save remaining registers
        "stmdb sp!, {r4-r11}    \n"

        // Save stack pointer for current task
        "ldr r0, =__gnat_running_thread_table           \n"
        "ldr r1, [r0]           \n"
        "str sp, [r1]           \n"

        // Restore stack pointer for next task
        "ldr r2, =first_thread_table         \n"
        "ldr r2, [r2]           \n"
        "ldr sp, [r2]           \n"

        // Set next task as running
        "str r2, [r0]           \n"

        // Restore remaining registers
        "ldmia sp!, {r4-r11}    \n"
        "bx lr                  \n"
    );

/*
    asm volatile (
        // Save remaining registers
        "stmdb sp!, {r4-r11}    \n"

        // Save stack pointer for current task
        "ldr r0, Lrun           \n"
        "ldr r1, [r0]           \n"
        "str sp, [r1]           \n"

        // Restore stack pointer for next task
        "ldr r2, Lfirst         \n"
        "ldr r2, [r2]           \n"
        "ldr sp, [r2]           \n"

        // Set next task as running task in Lrun
        "str r2, [r0]           \n"

        // Restore remaining registers
        "ldmia sp!, {r4-r11}    \n"
        "bx lr                  \n"

        "Lrun:   .word   __gnat_running_thread_table    \n"
        "Lfirst: .word   first_thread_table             \n"
    );
*/
}
