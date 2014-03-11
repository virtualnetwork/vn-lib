package VN_Message.Factory is

   type Message_Type is (Local_Hello);

   procedure Make_VN_Message(Message: in out VN_Message'Class; Msg_Type: Message_Type);

end VN_Message.Factory;
