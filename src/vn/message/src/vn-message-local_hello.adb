package body VN.Message.Local_Hello is

   procedure To_Basic(Local_Hello_VN_Msg: in VN_Message_Local_Hello;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Local_Hello := Local_Hello_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      null;
   end To_Basic;

   procedure To_Local_Hello(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Local_Hello_VN_Msg: out VN_Message_Local_Hello) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Local_Hello_VN_Msg'Address;
   begin
      null;
   end To_Local_Hello;

end VN.Message.Local_Hello;
