-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Physical_Logical converts between physical and logical representations of CAN messages.

with VN.Communication.CAN;
with Interfaces.C;

package Physical_Logical is

   package CANPack renames VN.Communication.CAN;

   type Data_Array is array(0..7) of Interfaces.C.signed_char;

   type CAN_Message_Physical is
      record
         ID		: Interfaces.C.unsigned;
         Length   	: Interfaces.C.unsigned;
         Data     	: Data_Array;
      end record;
   pragma Convention (C, CAN_Message_Physical);

   type CAN_Message_Physical_Access is access all CAN_Message_Physical;

   procedure PhysicalToLogical(msgIn : CAN_Message_Physical; msgOut : out CANPack.CAN_Message_Logical);

   procedure LogicalToPhysical(msgIn : CANPack.CAN_Message_Logical; msgOut : out CAN_Message_Physical);

end Physical_Logical;
