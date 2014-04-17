package body VN.Message.Assign_Address is

   procedure To_Basic(Assign_Address_VN_Msg: in VN_Message_Assign_Address;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Assign_Address := Assign_Address_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      Basic_VN_Msg.Header.Message_Type := Type_Basic;
   end To_Basic;

   procedure To_Assign_Address(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Assign_Address_VN_Msg: out VN_Message_Assign_Address) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Assign_Address_VN_Msg'Address;
      Payload_Length : VN_Length := VN_Length(MAX_PAYLOAD_SIZE -
                                    ASSIGN_ADDRESS_UNKNOWN_PAYLOAD_SIZE);
   begin
      Assign_Address_VN_Msg.Header.Message_Type    := Type_Assign_Address;
      Assign_Address_VN_Msg.Header.Opcode          := OPCODE_ASSIGN_ADDR;
      Assign_Address_VN_Msg.Header.Payload_Length  := Payload_Length;
   end To_Assign_Address;

end VN.Message.Assign_Address;
