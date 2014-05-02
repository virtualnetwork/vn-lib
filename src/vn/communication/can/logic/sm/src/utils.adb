-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- This package contains help functions for testing of VN.Communication.CAN.Logic.SM.

with Interfaces;
use Interfaces;

package body Utils is

   procedure To_Physical(msgIn : CANPack.CAN_Message_Logical; msgIDOut : out CANPack.CAN_message_ID) is
      msgPrio 	  : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.msgPrio);
      msgType 	  : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.msgType);
      msgSender   : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.Sender);
      msgReceiver : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.Receiver);

      POWER28 	  : constant CANPack.CAN_message_ID := CANPack.CAN_message_ID(2 ** 28);

      use CANPack;
   begin
      if msgIn.isNormal then
         msgIDOut := CANPack.CAN_message_ID(Interfaces.Shift_Left(msgPrio, CANPack.OFFSET_CAN_PRIORITY) +
                                              Interfaces.Shift_Left(msgType,  CANPack.OFFSET_CAN_TYPE) +
                                              Interfaces.Shift_Left(msgSender,   CANPack.OFFSET_CAN_SENDER) +
                                              Interfaces.Shift_Left(msgReceiver, CANPack.OFFSET_CAN_RECEIVER));
      else
         msgIDOut := CANPack.CAN_message_ID(msgIn.SenderUCID) + POWER28;
      end if;
   end To_Physical;


   function Filter_CAN_Message(msg : VN.Communication.CAN.CAN_Message_Logical;
                               filter : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type) return boolean is
      msgID : CANPack.CAN_message_ID;
      template, mask : CANPack.CAN_message_ID;
      isUsed : Boolean;

      use CANPack;
      temp : CANPack.CAN_message_ID;
   begin
      To_Physical(msg, msgID);

      for i in VN.Communication.CAN.CAN_Filtering.Filter_ID_Type'Range loop
         filter.Get_Filter(i, template, mask, isUsed);

         if isUsed then
            temp := msgID xor template;
            temp := temp and mask;
            if temp = 0 then
               return true;
            end if;
         end if;
      end loop;
      return false;
   end Filter_CAN_Message;
end Utils;
