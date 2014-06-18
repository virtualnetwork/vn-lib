------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ P R I M I T I V E S            --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2011, AdaCore                     --
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

with Interfaces; use Interfaces;

with System;
with System.Machine_Code;
with System.Storage_Elements;
with System.BB.Threads.Queues;
with System.BB.Interrupts;
with System.BB.Protection;
with System.BB.Board_Support;

with Ada.Unchecked_Conversion;

with System.BB.Board_Support.Cortexm; use System.BB.Board_Support.Cortexm;

package body System.BB.CPU_Primitives is

--   procedure Dbg (N : Character);
--   pragma Import (C, Dbg);

   package SSE renames System.Storage_Elements;
   use type SSE.Integer_Address;
   use type SSE.Storage_Offset;

   ----------------
   -- Local data --
   ----------------

   SP    : constant Range_Of_Context :=  0;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer            : not null access Context_Buffer;
      Program_Counter   : System.Address;
      Argument          : System.Address;
      Stack_Pointer     : System.Address)
   is
      Xpsr_Save   : System.Address;
      for Xpsr_Save'Address   use Stack_Pointer - System'To_Address (4 * 1);
      pragma Atomic (Xpsr_Save);

      Pc_Save     : System.Address;
      for Pc_Save'Address     use Stack_Pointer - System'To_Address (4 * 2);
      pragma Atomic (Pc_Save);

      R0_Save     : System.Address;
      for R0_Save'Address     use Stack_Pointer - System'To_Address (4 * 8);
      pragma Atomic (R0_Save);
   begin
      Buffer (SP) := Stack_Pointer - System'To_Address (4 * 16);

      Xpsr_Save   := 16#01000000#;
      Pc_Save     := Program_Counter + System'To_Address (1);
      R0_Save     := Argument;
   end Initialize_Context;

   -------------------------------
   -- Initialize_Floating_Point --
   -------------------------------

   procedure Initialize_Floating_Point is
   begin
      --  There is no floating point unit and therefore we have a null body

      null;
   end Initialize_Floating_Point;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      System.Machine_Code.Asm ("cpsid i", Volatile => True);
      System.Machine_Code.Asm ("isb", Volatile => True);
      System.Machine_Code.Asm ("dsb", Volatile => True);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts
     (Level : System.BB.Parameters.Interrupt_Level)
   is
      pragma Unreferenced (Level);
   begin
      System.Machine_Code.Asm ("cpsie i", Volatile => True);
      System.Machine_Code.Asm ("isb", Volatile => True);
      System.Machine_Code.Asm ("dsb", Volatile => True);
   end Enable_Interrupts;

end System.BB.CPU_Primitives;
