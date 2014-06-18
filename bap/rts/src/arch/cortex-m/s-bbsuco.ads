------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--      S Y S T E M . B B . P E R I P H E R A L S . R E G I S T E R S       --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2011, AdaCore                     --
--           Copyright (C) 2014, BAP - Bruhnspace Advanced Projects AB      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The porting of GNARL to bare board  targets was initially developed by   --
-- the Real-Time Systems Group at the Technical University of Madrid.       --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides the appropriate mapping for the system registers for
--  ARM Cortex-M family of devices

--  pragma Restrictions (No_Elaboration_Code);

with Interfaces;

package System.BB.Board_Support.Cortexm is
pragma Preelaborate;

   subtype Register_32 is Interfaces.Unsigned_32;

   --  Cortex M3 System Base address
   SYSTEM_BASE       : constant := 16#E000E000#;

   -----------------------------
   -- SYSTEM REGISTERS        --
   -----------------------------
   --   System tick timer registers
   ST_CTRL           : constant := 16#10#;   -- System timer control reg
   ST_RELOAD         : constant := 16#14#;   -- System timer reload reg
   ST_CURRENT        : constant := 16#18#;   -- System timer count

   --   ST_CTRL bits
   ST_ENABLE         : constant := 2 ** 0;   -- System timer enable
   ST_INTEN          : constant := 2 ** 1;   -- System timer interrupt enable
   ST_CLK_SRC        : constant := 2 ** 2;   -- System timer clk source select
   ST_COUNT          : constant := 2 ** 16;  -- Active ?

   --   Interrupt control registers
   IN_EN0            : constant := 16#100#;  -- Interrupt set enable reg
   IN_DIS0           : constant := 16#180#;  -- Interrupt clear enable reg
   IN_PEND0          : constant := 16#200#;  -- Interrupt set pending reg
   IN_UNPEND0        : constant := 16#280#;  -- Interrupt clear pending reg
   IN_ACTIVE0        : constant := 16#300#;  -- Interrupt active register
   IN_CTRL           : constant := 16#D04#;  -- Interrupt control register

   --   Interrupt control INCTRL bits
   PEND_STCLR        : constant := 2 ** 25;
   PEND_STSET        : constant := 2 ** 26;

   --
   --
   --  Exception identifiers

   M3_ID_STT         : constant := 15;  --  System Tick Timer Exception (15)

end System.BB.Board_Support.Cortexm;
