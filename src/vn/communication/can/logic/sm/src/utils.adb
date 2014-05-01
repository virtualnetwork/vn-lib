-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- This package contains help functions for testing of VN.Communication.CAN.Logic.SM.

with Interfaces;

package body Utils is

   procedure Logical_To_Physical(msgIn : CANPack.CAN_Message_Logical; msgIDOut : out CANPack.CAN_message_ID) is
      msgPrio 	  : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.msgPrio);
      msgType 	  : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.msgType);
      msgSender   : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.Sender);
      msgReceiver : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.Receiver);

      POWER28 	  : constant Interfaces.C.unsigned := Interfaces.C.unsigned(2 ** 28);

      use Interfaces.C;
   begin
      if msgIn.isNormal then
         msgIDOut := Interfaces.C.unsigned(Interfaces.Shift_Left(msgPrio, CANPack.OFFSET_CAN_PRIORITY) +
                                              Interfaces.Shift_Left(msgType,  CANPack.OFFSET_CAN_TYPE) +
                                              Interfaces.Shift_Left(msgSender,   CANPack.OFFSET_CAN_SENDER) +
                                              Interfaces.Shift_Left(msgReceiver, CANPack.OFFSET_CAN_RECEIVER));
      else
         msgIDOut := Interfaces.C.unsigned(msgIn.SenderUCID) + POWER28;
      end if;
   end To_Physical;


   function Filter_CAN_Message(msg : VN.Communication.CAN.CAN_Message_Logical;
                               filter : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type) return boolean is
   begin

   end Filter_CAN_Message;
end Utils;
