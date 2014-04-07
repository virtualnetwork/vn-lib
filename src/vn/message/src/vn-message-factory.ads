package VN.Message.Factory is

   function Create(VN_Msg_Type: in VN_Message_Type)
                              return VN_Message_Access;

end VN.Message.Factory;
