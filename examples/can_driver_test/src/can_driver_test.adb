------------------------------------------------------------------------------
--  This file is part of VN-Lib.
--
--  VN-Lib is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  VN-Lib is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with VN-Lib.  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright 2014, Nils Brynedal Ignell (nils.brynedal@gmail.com)

-- Summary:
-- This is an old test of the CAN drivers, might not work.
------------------------------------------------------------------------------

with GNAT.IO;

package body CAN_Driver_Test is

   procedure Init is
   begin
--        System.BB.Interrupts.Attach_Handler(Handler'Access, System.BB.Interrupts.Interrupt_ID(16));


--        System.BB.Interrupts.Attach_Handler(Handler'Access, System.BB.Interrupts.Interrupt_ID(32));


--        System.BB.Interrupts.Attach_Handler(TestHandler'Access, System.BB.Interrupts.Interrupt_ID(30));
--        System.BB.Interrupts.Attach_Handler(TestHandler'Access, System.BB.Interrupts.Interrupt_ID(31));
--        System.BB.Interrupts.Attach_Handler(TestHandler'Access, System.BB.Interrupts.Interrupt_ID(32));
--        System.BB.Interrupts.Attach_Handler(TestHandler'Access, System.BB.Interrupts.Interrupt_ID(23));
--        System.BB.Interrupts.Attach_Handler(TestHandler'Access, System.BB.Interrupts.Interrupt_ID(24));

      null;

   end Init;


   procedure Handler(ID : System.BB.Interrupts.Interrupt_ID) is
      intID : Integer := Integer(ID);
   begin
      GNAT.IO.Put_Line("Handler called, ID= " & intID'Img);
   end Handler;

   procedure TestHandler(ID : System.BB.Interrupts.Interrupt_ID) is
   begin
      GNAT.IO.Put_Line("Test-Handler called");
   end TestHandler;

end CAN_Driver_Test;
