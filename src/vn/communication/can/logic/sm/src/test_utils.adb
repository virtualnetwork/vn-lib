

with Utils;
with VN;
with VN.Communication.CAN;
with VN.Communication.CAN.CAN_Filtering;

procedure Test_Utils is
   theFilter : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type;
   temp : VN.Communication.CAN.CAN_Filtering.Filter_ID_Type;

   msg : VN.Communication.CAN.CAN_Message_Logical;
begin

   msg.isNormal := false;
   theFilter.Create_Filter(temp, VN.Communication.CAN.CAN_message_ID(2 ** 28), VN.Communication.CAN.CAN_message_ID(2 ** 28));

   if not Utils.Filter_CAN_Message(msg, theFilter) then
      VN.Text_IO.Put_Line("RequestCANAddress msg incorrectly filtered out");
   end if;

   VN.Text_IO.Put_Line("Test done");

end Test_Utils;
