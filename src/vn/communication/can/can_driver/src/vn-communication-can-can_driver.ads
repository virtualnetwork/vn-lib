-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
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


with System.BB.Interrupts;

package VN.Communication.CAN.CAN_Driver is

   procedure Send(message : VN.Communication.CAN.CAN_Message_Logical; status : out VN.Send_Status);

   function Send_Buffer_Full return Boolean;

   procedure Receive(message : out VN.Communication.CAN.CAN_Message_Logical; status : out VN.Receive_Status);

   procedure Update_Filters(filterAccess : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Access);

--  private --ToDo: The things below are only public when testing, the test project can_driver_test uses these things.

   type Data_Array is array(0..7) of Interfaces.C.signed_char;

   type CAN_Message_Physical is
      record
         ID		: Interfaces.C.unsigned;
         Length   	: Interfaces.C.unsigned;
         Data     	: Data_Array;
      end record;
   pragma Convention (C, CAN_Message_Physical);

   type CAN_Message_Physical_Access is access all CAN_Message_Physical;

   --will return 1 on success
   function SendPhysical(msg : CAN_Message_Physical_Access) return Interfaces.C.int;
   pragma Import(C, SendPhysical, "Send_CAN_Message");

   --returns 1 if message was received, 0 otherwise
   function ReceivePhysical(msg : CAN_Message_Physical_Access) return Interfaces.C.int;
   pragma Import(C, ReceivePhysical, "Receive_CAN_Message");


   -- Remove this when compiling for PC, keep when compiling for SmartFusion2:
   procedure Test_CAN_Send;
   pragma Import(C, Test_CAN_Send, "Test_Send");

   function Test return Interfaces.C.int; -- Remove this when compiling for PC, keep when compiling for SmartFusion2
   pragma Import(C, Test, "test");

private

   package CANPack renames VN.Communication.CAN;
   package CAN_Message_Buffers is new Buffers(CAN_Message_Physical);

   function CAN_Init return Interfaces.C.int; -- Remove this when compiling for PC, keep when compiling for SmartFusion2
   pragma Import(C, CAN_Init, "Init_CAN");


   procedure PhysicalToLogical(msgIn : CAN_Message_Physical; msgOut : out CANPack.CAN_Message_Logical);

   procedure LogicalToPhysical(msgIn : CANPack.CAN_Message_Logical; msgOut : out CAN_Message_Physical);

 --  procedure CANHandler(ID : System.BB.Interrupts.Interrupt_ID); -- Remove this when compiling for PC, keep when compiling for SmartFusion2

   procedure Init;


   SIZE : constant Integer := 40;

   SendBuffer    : CAN_Message_Buffers.Buffer(SIZE);
   ReceiveBuffer : CAN_Message_Buffers.Buffer(SIZE);

end VN.Communication.CAN.CAN_Driver;
