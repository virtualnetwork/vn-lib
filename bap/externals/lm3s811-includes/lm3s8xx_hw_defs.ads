------------------------------------------------------------------------------
--                                                                          --
--                     GNAT RAVENSCAR for ARM Cortex M3                     --
--                     File for TI Stellaris lm3s811                        --
--                                                                          --
--                    Copyright (C) 2010-2011, AdaCore                      --
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

package lm3s8xx_hw_defs is
   pragma Preelaborate;

   -----------------------------
   -- UART HARDWARE REGISTERS --
   ----------------------------- 
   Uart0_Irq        : constant Integer := 16#05#;              -- Uart0 Irq mask
   Uart0_Base       : constant Integer := 16#4000C000#;        -- UART0 Base address
   Uart_Data_Reg    : constant Integer := 16#00#;              -- UART Data register

end lm3s8xx_hw_defs;
