------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . I N T E R R U P T S                --
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

with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with System.BB.CPU_Primitives;
with System.BB.Protection;
with System.BB.Threads;
with System.BB.Threads.Queues;
with System.BB.Board_Support.Cortexm; use System.BB.Board_Support.Cortexm;
with Interfaces; use Interfaces;

with GNAT.IO;  --  DGB and system exception printouts

package body System.BB.Interrupts is

   procedure Dbg (N : Character);
   pragma Import (C, Dbg);
   pragma Unreferenced (Dbg);

   use type System.Storage_Elements.Storage_Offset;

   procedure Default_Isr (Id : Interrupt_ID);
   --  Default handlers.

   function Context_Switch_Needed return Boolean;
   pragma Import (Asm, Context_Switch_Needed, "context_switch_needed");

   ----------------
   -- Local data --
   ----------------

   Interrupt_Handlers : array (HW_Interrupt_ID) of Interrupt_Handler;

   Interrupt_Being_Handled : Interrupt_ID := No_Interrupt;
   pragma Atomic (Interrupt_Being_Handled);
   --  Interrupt_Being_Handled contains the interrupt currently being
   --  handled in the system, if any. It is equal to No_Interrupt when no
   --  interrupt is handled. Its value is updated by the trap handler.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Irq_Handler;
   pragma Export (C, Irq_Handler, "irq_handler_ada");
   --  This wrapper procedure is in charge of setting the appropriate
   --  software priorities before calling the user-defined handler.

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler (Handler : Interrupt_Handler;
                             Id      : Interrupt_ID) is
   begin
      --  Check that we are attaching to a real interrupt

      pragma Assert (Id /= No_Interrupt);

      --  Copy the user's handler to the appropriate place within the table

      Interrupt_Handlers (Id) := Handler;

      --
      --  Might need to enable M3 NVIC interrupt here???
      --

   end Attach_Handler;

   -----------------------
   -- Current_Interrupt --
   -----------------------

   function Current_Interrupt return Interrupt_ID is
   begin
      return Interrupt_Being_Handled;
   end Current_Interrupt;

   -----------------------
   -- Interrupt_Wrapper --
   -----------------------

   procedure Irq_Handler
   is
      Vec             : Unsigned_32;
      Interrupt       : HW_Interrupt_ID;
      Self_Id         : constant Threads.Thread_Id := Threads.Thread_Self;
      Caller_Priority : constant Any_Priority :=
                          Threads.Get_Priority (Self_Id);

      Previous_Interrupt_Level : constant Interrupt_ID :=
                                   Interrupt_Being_Handled;

      Control_Reg : Unsigned_32;
      for Control_Reg'Address use System'To_Address (SYSTEM_BASE + IN_CTRL);
      pragma Atomic (Control_Reg);
   begin
      --  Get current interrupt

      Vec := Control_Reg and 16#3F#;

      if Vec > Unsigned_32 (Interrupt_ID'Last) or (Vec >= 1 and Vec < 15) then
         GNAT.IO.Put (
            " System Exception or unassigned IRQ:" &
            Integer'Image (Integer (Vec)) &
            " Halting!" &
            ASCII.LF &
            ASCII.CR
         );

         while True loop
            null;
         end loop;
      end if;

      Interrupt := Interrupt_ID (Vec);

      --  Store the interrupt being handled

      Interrupt_Being_Handled := Integer (Interrupt);

      --  Then, we must set the appropriate software priority corresponding
      --  to the interrupt being handled. It comprises also the appropriate
      --  interrupt masking.

      Threads.Queues.Change_Priority (Self_Id, Max_Interrupt_Priority);

      --  Call the user handler
--      if Interrupt /= 1 then
--         Dbg_Hand (Vec);
--         Dbg ('H');
--      end if;
      Interrupt_Handlers (Interrupt).all (Interrupt);

      --  Restore the software priority to the state before the interrupt
      --  happened. Interrupt unmasking is not done here (it will be done
      --  later by the interrupt epilogue).

      Threads.Queues.Change_Priority (Self_Id, Caller_Priority);

      --  Restore the interrupt that was being handled previously (if any)

      Interrupt_Being_Handled := Previous_Interrupt_Level;

      --  AIC_EOICR := 1;

      if Context_Switch_Needed then
         CPU_Primitives.Context_Switch;
      end if;
   end Irq_Handler;

   ----------------------------
   -- Within_Interrupt_Stack --
   ----------------------------

   function Within_Interrupt_Stack
     (Stack_Address : System.Address) return Boolean
   is
      pragma Unreferenced (Stack_Address);
   begin
      --  Always return false as the task stack is always used.
      return False;
   end Within_Interrupt_Stack;

   procedure Default_Isr (Id : Interrupt_ID) is
      pragma Unreferenced (Id);
   begin
      null;
   end Default_Isr;

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   procedure Initialize_Interrupts is
   begin
      --  Disable all interrupt lines.
      --  AIC_IDCR := 16#FFFFFFFF#;

      --  No interrupt lines handled by the FIQ handler.
      --  AIC_FFDR := 16#FFFFFFFF#;

      --  No pending interrupts.
      --  AIC_ICCR := 16#FFFFFFFF#;

      --  Not handling an interrupt.
      --  AIC_EOICR := 1;

      for I in HW_Interrupt_ID loop
         Attach_Handler (Default_Isr'Access, I);
      end loop;

      --  AIC_SVR (lm3s811_ID_FIQ) := 32;
      --  AIC_SPU := 33;
   end Initialize_Interrupts;

   procedure Enable_Interrupt (Id : HW_Interrupt_ID) is
   begin
      --  AIC_IECR := 2 ** Id;
      null;
   end Enable_Interrupt;

   procedure Disable_Interrupt (Id : HW_Interrupt_ID) is
   begin
      --  AIC_IDCR := 2 ** Id;
      null;
   end Disable_Interrupt;

end System.BB.Interrupts;
