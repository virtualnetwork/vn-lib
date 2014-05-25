package body VN.Message.Probe_Reply is

   procedure To_Basic(Probe_Reply_VN_Msg: in VN_Message_Probe_Reply;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Probe_Reply := Probe_Reply_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      Basic_VN_Msg.Header.Message_Type := Type_Basic;
   end To_Basic;

   procedure To_Probe_Reply(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Probe_Reply_VN_Msg: out VN_Message_Probe_Reply) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Probe_Reply_VN_Msg'Address;
      Payload_Length : VN_Length := VN_Length(MAX_PAYLOAD_SIZE -
                                    PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE);
   begin
      Probe_Reply_VN_Msg.Header.Message_Type    := Type_Probe_Reply;
      Probe_Reply_VN_Msg.Header.Opcode          := OPCODE_REQUEST_ADDR_BLOCK;
      Probe_Reply_VN_Msg.Header.Payload_Length  := Payload_Length;
   end To_Probe_Reply;

end VN.Message.Probe_Reply;
