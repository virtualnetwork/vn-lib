-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- This package contains help functions for testing of VN.Communication.CAN.Logic.SM.

with VN;
with VN.Communication;
with VN.Communication.CAN;

with VN.Communication.CAN.CAN_Filtering;

package Utils is

   package CANPack renames VN.Communication.CAN;

   procedure To_Physical(msgIn : CANPack.CAN_Message_Logical; msgIDOut : out CANPack.CAN_message_ID);

   function Filter_CAN_Message(msg : CANPack.CAN_message_ID;
                               filter : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type) return boolean;

end Utils;
