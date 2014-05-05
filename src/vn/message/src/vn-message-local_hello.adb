package body VN.Message.Local_Hello is

   procedure To_Basic(Local_Hello_VN_Msg: in VN_Message_Local_Hello;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Local_Hello := Local_Hello_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      Basic_VN_Msg.Header.Message_Type := Type_Basic;
   end To_Basic;

   procedure To_Local_Hello(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Local_Hello_VN_Msg: out VN_Message_Local_Hello) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Local_Hello_VN_Msg'Address;
      Payload_Length : VN_Length := VN_Length(MAX_PAYLOAD_SIZE -
                                    LOCAL_HELLO_UNKNOWN_PAYLOAD_SIZE);
   begin
      Local_Hello_VN_Msg.Header.Message_Type    := Type_Local_Hello;
      Local_Hello_VN_Msg.Header.Opcode          := OPCODE_LOCAL_HELLO;
      Local_Hello_VN_Msg.Header.Payload_Length  := Payload_Length;
   end To_Local_Hello;

end VN.Message.Local_Hello;
