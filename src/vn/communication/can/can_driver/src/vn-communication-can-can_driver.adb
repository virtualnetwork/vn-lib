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
use Interfaces;

with GNAT.IO;
use GNAT.IO;

with Ada.Unchecked_Conversion;

package body VN.Communication.CAN.CAN_Driver is

   function signedChar_To_Unsigned_8 is new Ada.Unchecked_Conversion(Interfaces.C.signed_char, Interfaces.Unsigned_8);
   function Unsigned_8_To_signedChar is new Ada.Unchecked_Conversion(Interfaces.Unsigned_8, Interfaces.C.signed_char);

   procedure Send(message : VN.Communication.CAN.CAN_Message_Logical;
                  status : out VN.Send_Status) is

      physicalMessage : CAN_Message_Physical;
   begin

      -- ToDo: This is just for testing, right now the CAN drivers don't work so we'll have to pretend we sent the message without doing so:
      status := VN.OK;
      return;

      LogicalToPhysical(message, physicalMessage);

      if CAN_Message_Buffers.Full(SendBuffer) then
         status := VN.ERROR_BUFFER_FULL;
      else
         status := VN.OK;
         CAN_Message_Buffers.Insert(physicalMessage, SendBuffer);
      end if;
   end Send;

   function Send_Buffer_Full return Boolean is
   begin
      return CAN_Message_Buffers.Full(SendBuffer);
   end Send_Buffer_Full;

   procedure Receive(message : out VN.Communication.CAN.CAN_Message_Logical;
                     status : out VN.Receive_Status) is

      physicalMessage : CAN_Message_Physical;
   begin

      -- ToDo: This is just for testing, right now the CAN drivers don't work so we'll have to pretend we just didn't receive a message
    status := VN.NO_MSG_RECEIVED;
    return;

      if CAN_Message_Buffers.Empty(ReceiveBuffer) then
         status := VN.NO_MSG_RECEIVED;
      else
         CAN_Message_Buffers.Remove(physicalMessage, ReceiveBuffer);
         PhysicalToLogical(physicalMessage, message);

         if CAN_Message_Buffers.Empty(ReceiveBuffer) then
            status := VN.MSG_RECEIVED_NO_MORE_AVAILABLE;
         else
            status := VN.MSG_RECEIVED_MORE_AVAILABLE;
         end if;
      end if;
   end Receive;

   procedure Update_Filters(filterAccess : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Access) is
   begin
      null; -- ToDo: Implement...
   end Update_Filters;

   procedure PhysicalToLogical(msgIn : CAN_Message_Physical; msgOut : out CANPack.CAN_Message_Logical) is
      msgPrio : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.ID);
      msgType : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.ID);
      msgSender   : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.ID);
      msgReceiver : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.ID);

      PRIORITY_POWER : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32(Natural(CANPack.CAN_Message_Prio'Last) + 1);
      TYPE_POWER     : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32(Natural(CANPack.CAN_Message_Type'Last) + 1);
      SENDER_POWER   : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32(Natural(CANPack.CAN_Address_Sender'Last) + 1);
      RECEIVER_POWER : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32(Natural(CANPack.CAN_Address_Receiver'Last) + 1);

      POWER28 : constant Interfaces.C.unsigned := Interfaces.C.unsigned(2 ** 28);

      use Interfaces.C;
   begin


      if msgIn.ID >= POWER28 then
         msgOut.isNormal := false;
         msgOut.SenderUCID := CANPack.UCID(msgIn.ID - POWER28);
      else
         msgOut.isNormal := true;

         msgPrio     := Interfaces.Shift_Right(msgPrio, 	CANPack.OFFSET_CAN_PRIORITY) 	rem PRIORITY_POWER;
         msgType     := Interfaces.Shift_Right(msgType, 	CANPack.OFFSET_CAN_TYPE) 	rem TYPE_POWER;
         msgSender   := Interfaces.Shift_Right(msgSender, 	CANPack.OFFSET_CAN_SENDER) 	rem SENDER_POWER;
         msgReceiver := Interfaces.Shift_Right(msgReceiver,  	CANPack.OFFSET_CAN_RECEIVER) 	rem RECEIVER_POWER;

         msgOut.msgPrio  := CANPack.CAN_Message_Prio(msgPrio);
         msgOut.msgType  := CANPack.CAN_Message_Type(msgType);
         msgOut.Sender   := CANPack.CAN_Address_Sender(msgSender);
         msgOut.Receiver := CANPack.CAN_Address_Receiver(msgReceiver);
      end if;

      if msgIn.Length > 8 then --should never be true, check just in case
         msgOut.Length := CANPack.DLC_Type(8);
      else
         msgOut.Length := CANPack.DLC_Type(msgIn.Length);
      end if;

      for i in 0 .. msgOut.Length - 1 loop
         msgOut.Data(msgOut.Data'First + i) :=
           signedChar_To_Unsigned_8(msgIn.Data(msgIn.Data'First + Integer(i)));
      end loop;
   end PhysicalToLogical;


   procedure LogicalToPhysical(msgIn : CANPack.CAN_Message_Logical; msgOut : out CAN_Message_Physical) is
      msgPrio 	  : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.msgPrio);
      msgType 	  : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.msgType);
      msgSender   : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.Sender);
      msgReceiver : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.Receiver);

      POWER28 	  : constant Interfaces.C.unsigned := Interfaces.C.unsigned(2 ** 28);

      use Interfaces.C;
   begin

      msgOut.Length := Interfaces.C.unsigned(msgIn.Length);

      if msgIn.isNormal then
         msgOut.ID := Interfaces.C.unsigned(Interfaces.Shift_Left(msgPrio, CANPack.OFFSET_CAN_PRIORITY) +
                                                           Interfaces.Shift_Left(msgType,  CANPack.OFFSET_CAN_TYPE) +
                                                           Interfaces.Shift_Left(msgSender,   CANPack.OFFSET_CAN_SENDER) +
                                                           Interfaces.Shift_Left(msgReceiver, CANPack.OFFSET_CAN_RECEIVER));
      else
         msgOut.ID := Interfaces.C.unsigned(msgIn.SenderUCID) + POWER28;
      end if;

      for i in 0 .. Integer(msgIn.Length) - 1 loop
         msgOut.Data(msgOut.Data'First + i) :=
           Unsigned_8_To_signedChar(msgIn.Data(msgIn.Data'First + CANPack.DLC_Type(i)));
      end loop;
   end LogicalToPhysical;


-- Remove this when compiling for PC, keep when compiling for SmartFusion2:
--     procedure CANHandler(ID : System.BB.Interrupts.Interrupt_ID) is
--     begin
--        null; --ToDo: Implement this...
--     end CANHandler;

   procedure Init is
      ret : Interfaces.Integer_32;
   begin
      ret := Interfaces.Integer_32(CAN_Init); -- Remove this when compiling for PC, keep when compiling for SmartFusion2

      GNAT.IO.New_Line(2);

      if ret = 0 then
         GNAT.IO.Put_Line("CAN initiated successfully");
      else

         GNAT.IO.Put_Line("CAN initiated with error code= " & ret'Img);
      end if;
    --  System.BB.Interrupts.Attach_Handler(CANHandler'Access, System.BB.Interrupts.Interrupt_ID(32));
   end Init;


   -- ToDo: This is just for testing, to make sure the C-code does not interfere:
   function SendPhysical(msg : CAN_Message_Physical_Access) return Interfaces.C.int is
   begin
      return 1;
   end SendPhysical;

   function ReceivePhysical(msg : CAN_Message_Physical_Access) return Interfaces.C.int is
   begin
      return 0;
   end ReceivePhysical;

begin
   Init;
end VN.Communication.CAN.CAN_Driver;
