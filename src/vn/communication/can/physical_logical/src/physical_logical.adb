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

-- Date: 2014-XX-XX
-- Summary:
-- Physical_Logical converts between physical and logical representations of CAN messages.

with Interfaces;
use Interfaces;

with VN.Communication.CAN;
use VN.Communication.CAN;

with Ada.Unchecked_Conversion;

package body Physical_Logical is

function signedChar_To_Unsigned_8 is new Ada.Unchecked_Conversion(Interfaces.C.signed_char, Interfaces.Unsigned_8);
   function Unsigned_8_To_signedChar is new Ada.Unchecked_Conversion(Interfaces.Unsigned_8, Interfaces.C.signed_char);


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

end Physical_Logical;
