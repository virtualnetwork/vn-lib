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

package body VN.Communication.CAN.CAN_Driver is

   procedure Send(message : VN.Communication.CAN.CAN_Message_Logical;
                  status : out VN.Send_Status) is

      physicalMessage : CAN_Message_Physical;
   begin
      LogicalToPhysical(message, physicalMessage);

      if CAN_Message_Buffers.Full(SendBuffer) then
         status := VN.ERROR_BUFFER_FULL;
      else
         status := VN.OK;
         CAN_Message_Buffers.Insert(physicalMessage, SendBuffer);
      end if;
   end Send;

   procedure Receive(message : out VN.Communication.CAN.CAN_Message_Logical;
                     status : out VN.Receive_Status) is

      physicalMessage : CAN_Message_Physical;
   begin

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

      msgOut.Length := CANPack.DLC_Type(msgIn.Length);

      for i in msgOut.Data'First .. msgOut.Data'First + msgOut.Length - 1 loop
         msgOut.Data(i) := Interfaces.Unsigned_8(msgIn.Data(Integer(i)));
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

      for i in msgOut.Data'First .. msgOut.Data'First + Integer(msgOut.Length) - 1 loop
         msgOut.Data(i) := Interfaces.C.signed_char(msgIn.Data(CANPack.DLC_Type(i)));
      end loop;
   end LogicalToPhysical;

   procedure CANHandler(ID : System.BB.Interrupts.Interrupt_ID) is
   begin
      null; --ToDo: Implement this...
   end CANHandler;

   procedure Init is
   begin
      System.BB.Interrupts.Attach_Handler(CANHandler'Access, System.BB.Interrupts.Interrupt_ID(32));
   end Init;

begin
   Init;
end VN.Communication.CAN.CAN_Driver;
