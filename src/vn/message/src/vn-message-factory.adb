with VN.Message.Local_Hello;

package body VN.Message.Factory is

   function Create(VN_Msg_Type: in VN_Message_Type)
                              return VN_Message_Access is
      New_Message: VN_Message_Access;
   begin
      if VN_Msg_Type = Type_Local_Hello then
         New_Message := new VN.Message.Local_Hello.VN_Message_Local_Hello;
      end if;
      return New_Message;
   end Create;

end VN.Message.Factory;
