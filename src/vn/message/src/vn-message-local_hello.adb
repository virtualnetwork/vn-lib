package body VN.Message.Local_Hello is

   procedure Cast_To_Basic(Local_Hello_VN_Msg: in VN_Message_Local_Hello;
                           Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Local_Hello := Local_Hello_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      null;
   end Cast_To_Basic;

   procedure Cast_To_Local_Hello(
                              Basic_VN_Msg: in VN_Message_Basic;
                              Local_Hello_VN_Msg: out VN_Message_Local_Hello)
                              is
   begin
      null;
   end Cast_To_Local_Hello;


end VN.Message.Local_Hello;
