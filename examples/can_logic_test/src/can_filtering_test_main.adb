-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Test implementation of VN.Communication.CAN.CAN_Filtering.

with Utils;
with VN;
with VN.Communication.CAN;
with VN.Communication.CAN.CAN_Filtering;

procedure CAN_Filtering_Test_Main is
   theFilter : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type;
   temp : VN.Communication.CAN.CAN_Filtering.Filter_ID_Type;

   msg : VN.Communication.CAN.CAN_Message_Logical;

   template, mask, msgID : VN.Communication.CAN.CAN_message_ID;
   isUsed : boolean;
begin

   msg.isNormal := false;
   theFilter.Create_Filter(temp, VN.Communication.CAN.CAN_message_ID(2 ** 28), VN.Communication.CAN.CAN_message_ID(2 ** 28));

   if not Utils.Filter_CAN_Message(msg, theFilter) then
      VN.Text_IO.Put_Line("RequestCANAddress msg incorrectly filtered out");
   end if;

   msg.isNormal := true;
   msg.Receiver := 0;
   msg.Sender := 0;
   msg.msgPrio := 0;
   msg.msgType := 0;

   theFilter.Create_Transmission_Filter(temp, 0);

   theFilter.Get_Filter(temp, template, mask, isUsed);

   Utils.To_Physical(msg, msgID);

   VN.Text_IO.Put_Line("isUsed = " & isUsed'Img & " template= " & template'Img & " mask= " & mask'Img & " msgID= " & msgID'img);

   if not Utils.Filter_CAN_Message(msg, theFilter) then
      VN.Text_IO.Put_Line("Transmission msg incorrectly filtered out");
   end if;

   VN.Text_IO.Put_Line("Test done");

end CAN_Filtering_Test_Main;
