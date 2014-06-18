------------------------------------------------------------------------------
--                                                                          --
--                     RAVENSCAR for ARM Cortex M3                          --
--                     File for TI Stellaris lm3s811                        --
--                           UART Driver file                               --
--                                                                          --
--           Copyright (C) 2014, BAP - Bruhnspace Advanced Projects AB      --
--                                                                          --
-- This is free software; you can  redistribute it  and/or modify it under  --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. This is distributed in the hope that it will be useful, but WITH-  --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines constants and primitives used for handling the
--  peripherals available in the BAP QEMU TI Stellaris lm3s811 target board.
pragma Restrictions (No_Elaboration_Code);

with Interfaces, System;
use Interfaces;

package Driver_Uart is
   pragma Preelaborate;

   subtype Register_32 is Interfaces.Unsigned_32;

   ----------------------------------
   -- IRQ AND PERIPHERAL ADDRESSES --
   ----------------------------------
   UART0_IRQ         : constant := 5;   -- Uart0 Irq
   UART1_IRQ         : constant := 6;   -- Uart1 Irq

   --  UART0 Base address
   UART0_BASE        : constant := 16#4000C000#;

   --  UART1 Base address
   UART1_BASE        : constant := 16#4000D000#;

   -----------------------------
   -- UART REGISTERS OFFSETS  --
   -----------------------------
   UART_DR           : constant := 16#00#;    -- Data reg
   UART_FR           : constant := 16#18#;    -- Flag reg
   UART_LCR_H        : constant := 16#2C#;    -- Line control reg (Hi)
   UART_CTL          : constant := 16#30#;    -- Control reg
   UART_IM           : constant := 16#38#;    -- Interrupt mask reg

   --  UART_FR bits
   --  (FIFO handling not needed for QEMU. Flag register, minimal impl)
   UART_FR_BUSY      : constant := 2 ** 3;            -- Tx Busy
   UART_FR_RXFE      : constant := 2 ** 4;            -- Rx FIFO empty
   UART_FR_TXFF      : constant := 2 ** 5;            -- Tx FIFO full
   UART_FR_RXFF      : constant := 2 ** 6;            -- Rx FIFO full
   UART_FR_TXFE      : constant := 2 ** 7;            -- TX FIFO Empty

   --  UART_LCR_H bits
   UART_LCR_H_FEN    : constant := 2 ** 4;            -- FIFO enable
   UART_LCR_H_WLEN0  : constant := 2 ** 5;            -- Word length, low bit
   UART_LCR_H_WLEN1  : constant := 2 ** 6;            -- Word length, high bit

   --  UART_CTL bits
   UART_CTL_EN       : constant := 2 ** 0;            -- UART enable
   UART_CTL_LBE      : constant := 2 ** 7;            -- Loopback enable
   UART_CTL_TXE      : constant := 2 ** 8;            -- Transmit enable
   UART_CTL_RXE      : constant := 2 ** 9;            -- Receive enable

   --   UART_IM bits
   UART_RX           : constant := 2 ** 4;            -- RX int enable
   UART_TX           : constant := 2 ** 5;            -- TX int enable

   Initialized       : Boolean := False;

   procedure Uart_Driver_Initialize;
   function Uart_Driver_Is_Rx_Ready return Boolean;
   function Uart_Driver_Is_Tx_Ready return Boolean;
   function Uart_Driver_Get return Character;
   procedure Uart_Driver_Put (C : Character);

end Driver_Uart;
