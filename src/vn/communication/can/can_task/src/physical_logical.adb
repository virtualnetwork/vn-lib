-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- This package contains functions for conversion between logical
-- and physical representations of CAN messages.

package body Physical_Logical is
   procedure PhysicalToLogical(msgIn : CAN_Defs.CAN_Message; msgOut : out Lowlevel.CAN_Message_Logical) is
      msgPrio : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.ID.Identifier);
      msgType : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.ID.Identifier);
      msgSender   : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.ID.Identifier);
      msgReceiver : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.ID.Identifier);

      PRIORITY_POWER : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32(Natural(Lowlevel.CAN_Message_Prio'Last) + 1);
      TYPE_POWER     : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32(Natural(Lowlevel.CAN_Message_Type'Last) + 1);
      SENDER_POWER   : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32(Natural(Lowlevel.CAN_Address_Sender'Last) + 1);
      RECEIVER_POWER : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32(Natural(Lowlevel.CAN_Address_Receiver'Last) + 1);

      POWER28 : CAN_Defs.CAN_Identifier := CAN_Defs.CAN_Identifier(2 ** 28);

      CAN_MESSAGE_NOT_EXTENDED : exception;

      use Lowlevel;
   begin

      if not msgIn.ID.isExtended then
         raise CAN_MESSAGE_NOT_EXTENDED;
      else
         if msgIn.ID.Identifier >= POWER28 then
            msgOut.isNormal := false;
            msgOut.SenderUCID := Lowlevel.UCID(msgIn.ID.Identifier - POWER28);

         else
            msgOut.isNormal := true;

            msgPrio := Interfaces.Shift_Right(msgPrio, Lowlevel.OFFSET_CAN_PRIORITY) 	rem PRIORITY_POWER;
            msgType := Interfaces.Shift_Right(msgType, Lowlevel.OFFSET_CAN_TYPE) 	rem TYPE_POWER;
            msgSender   := Interfaces.Shift_Right(msgSender, Lowlevel.OFFSET_CAN_SENDER) 	rem SENDER_POWER;
            msgReceiver := Interfaces.Shift_Right(msgReceiver, Lowlevel.OFFSET_CAN_RECEIVER) rem RECEIVER_POWER;

            msgOut.msgPrio := Lowlevel.CAN_Message_Prio(msgPrio);
            msgOut.msgType := Lowlevel.CAN_Message_Type(msgType);
            msgOut.Sender  := Lowlevel.CAN_Address_Sender(msgSender);
            msgOut.Receiver := Lowlevel.CAN_Address_Receiver(msgReceiver);
         end if;

         msgOut.Length := Lowlevel.DLC_Type(msgIn.Len);

         for i in msgOut.Data'First .. msgOut.Data'First + msgOut.Length - 1 loop
            msgOut.Data(i) := msgIn.Data(CAN_Defs.DLC_Type(i));
         end loop;
      end if;
   end PhysicalToLogical;


   procedure LogicalToPhysical(msgIn : Lowlevel.CAN_Message_Logical; msgOut : out CAN_Defs.CAN_Message) is
      msgPrio : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.msgPrio);
      msgType : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.msgType);
      msgSender   : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.Sender);
      msgReceiver : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(msgIn.Receiver);

      POWER28 : CAN_Defs.CAN_Identifier := CAN_Defs.CAN_Identifier(2 ** 28);
   begin

      msgOut.Len := CAN_Defs.DLC_Type(msgIn.Length);

      if msgIn.isNormal then
         msgOut.ID.Identifier := CAN_Defs.CAN_Identifier(Interfaces.Shift_Left(msgPrio, Lowlevel.OFFSET_CAN_PRIORITY) +
                                                           Interfaces.Shift_Left(msgType,  Lowlevel.OFFSET_CAN_TYPE) +
                                                           Interfaces.Shift_Left(msgSender,   Lowlevel.OFFSET_CAN_SENDER) +
                                                           Interfaces.Shift_Left(msgReceiver, Lowlevel.OFFSET_CAN_RECEIVER));
      else

         msgOut.ID.Identifier := CAN_Defs.CAN_Identifier(msgIn.SenderUCID) + POWER28;
      end if;

      for i in msgOut.Data'First .. msgOut.Data'First + msgOut.Len - 1 loop
         msgOut.Data(i) := msgIn.Data(Lowlevel.DLC_Type(i));
      end loop;

      msgOut.ID.isExtended := true;

   end LogicalToPhysical;

end Physical_Logical;
