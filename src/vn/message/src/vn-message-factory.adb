package body VN.Message.Factory is

   function Create(VN_Msg_Type: in VN_Message_Type) return VN_Message_Basic is
      VN_Msg: VN_Message_Basic;
   begin
      return VN_Msg;
   end Create;

end VN.Message.Factory;
