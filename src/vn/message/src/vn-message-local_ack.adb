package body VN.Message.Local_Ack is

   procedure To_Basic(Local_Ack_VN_Msg: in VN_Message_Local_Ack;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Local_Ack := Local_Ack_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      null;
   end To_Basic;

   procedure To_Local_Ack(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Local_Ack_VN_Msg: out VN_Message_Local_Ack) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Local_Ack_VN_Msg'Address;
   begin
      null;
   end To_Local_Ack;

end VN.Message.Local_Ack;
