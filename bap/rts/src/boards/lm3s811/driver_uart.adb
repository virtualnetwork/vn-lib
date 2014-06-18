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

package body Driver_Uart is
   --  pragma Preelaborate;

   procedure Uart_Driver_Initialize is
      --  Add baudrate settings (doesn't matter as long target is QEMU)
      LCRH_Reg : Register_32;
      for LCRH_Reg'Address use System'To_Address (UART0_BASE + UART_LCR_H);
      pragma Atomic (LCRH_Reg);

      CT_Reg : Register_32;
      for CT_Reg'Address use System'To_Address (UART0_BASE + UART_CTL);
      pragma Atomic (CT_Reg);
   begin
      LCRH_Reg := LCRH_Reg or UART_LCR_H_WLEN1 or UART_LCR_H_WLEN0;
      CT_Reg := CT_Reg or UART_CTL_TXE or UART_CTL_RXE or UART_CTL_EN;
      Initialized := True;
   end Uart_Driver_Initialize;

  function Uart_Driver_Is_Rx_Ready return Boolean is
      FR_Reg : Register_32;
      for FR_Reg'Address use System'To_Address (UART0_BASE + UART_FR);
      pragma Atomic (FR_Reg);
   begin
      if (FR_Reg and UART_FR_RXFE) = 0 then
         return True;
      else
         return False;
      end if;
   end Uart_Driver_Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Uart_Driver_Is_Tx_Ready return Boolean is
      FR_Reg : Register_32;
      for FR_Reg'Address use System'To_Address (UART0_BASE + UART_FR);
      pragma Atomic (FR_Reg);
   begin
      if (FR_Reg and UART_FR_BUSY) = 0 then
         return True;
      else
         return False;
      end if;
   end Uart_Driver_Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Uart_Driver_Put (C : Character) is
      Data_Reg : Character;
      for Data_Reg'Address use System'To_Address (UART0_BASE + UART_DR);
      pragma Atomic (Data_Reg);
   begin
      Data_Reg := C;
   end Uart_Driver_Put;

   function Uart_Driver_Get return Character is
      Data_Reg : Character;
      for Data_Reg'Address use System'To_Address (UART0_BASE + UART_DR);
      pragma Atomic (Data_Reg);
   begin
      return Data_Reg;
   end Uart_Driver_Get;

end Driver_Uart;
