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
------------------------------------------------------------------------------

-- Summary:
-- This package contains procedures for sending and receiving CAN messages
-- as well as for conversion between logical and physical representations
-- of CAN messages.
-- This code interfaces with driver code written in C.

-- Please note: This code will not compile unless it is compiled with the correct makefile,
-- the BAP repository needs to be included.

with Interfaces;
with Interfaces.C;

with VN;
with VN.Communication;
with VN.Communication.CAN;

with VN.Communication.CAN.CAN_Filtering;

with Physical_Logical;

with System.BB.Interrupts;

package VN.Communication.CAN.CAN_Driver is

   procedure Send(message : VN.Communication.CAN.CAN_Message_Logical; status : out VN.Send_Status);

   function Send_Buffer_Full return Boolean;

   procedure Receive(message : out VN.Communication.CAN.CAN_Message_Logical; status : out VN.Receive_Status);

   procedure Update_Filters(filterAccess : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Access);

   --  private --ToDo: The things below are only public when testing, the test project can_driver_test uses these things.

   type CAN_Message_Physical is new Physical_Logical.CAN_Message_Physical;
   type CAN_Message_Physical_Access is new Physical_Logical.CAN_Message_Physical_Access;

   --will return 1 on success
   function SendPhysical(msg : Physical_Logical.CAN_Message_Physical_Access) return Interfaces.C.int;
   pragma Import(C, SendPhysical, "Send_CAN_Message");

   --returns 1 if message was received, 0 otherwise
   function ReceivePhysical(msg : Physical_Logical.CAN_Message_Physical_Access) return Interfaces.C.int;
   pragma Import(C, ReceivePhysical, "Receive_CAN_Message");

   procedure Test_CAN_Send;
   pragma Import(C, Test_CAN_Send, "Test_Send");

   function Test return Interfaces.C.int; 
   pragma Import(C, Test, "test");

private

   package CANPack renames VN.Communication.CAN;
   package CAN_Message_Buffers is new Buffers(Physical_Logical.CAN_Message_Physical);

   function CAN_Init return Interfaces.C.int; 
   pragma Import(C, CAN_Init, "Init_CAN");


   --Will return 1 on success
   function Set_CAN_Filter(mailbox_number : Interfaces.C.unsigned_char;
                           mask : Interfaces.C.unsigned; template : Interfaces.C.unsigned)
                           return Interfaces.C.int; 
   pragma Import(C, Set_CAN_Filter, "Set_Filter");

   procedure Init;


   SIZE : constant Integer := 40;

   SendBuffer    : CAN_Message_Buffers.Buffer(SIZE);
   ReceiveBuffer : CAN_Message_Buffers.Buffer(SIZE);

end VN.Communication.CAN.CAN_Driver;
