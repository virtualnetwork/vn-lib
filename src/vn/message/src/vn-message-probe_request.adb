package body VN.Message.Probe_Request is

   procedure To_Basic(Probe_Request_VN_Msg: in VN_Message_Probe_Request;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Probe_Request := Probe_Request_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      Basic_VN_Msg.Header.Message_Type := Type_Basic;
   end To_Basic;

   procedure To_Probe_Request(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Probe_Request_VN_Msg: out VN_Message_Probe_Request) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Probe_Request_VN_Msg'Address;
      Payload_Length : VN_Length := VN_Length(MAX_PAYLOAD_SIZE -
                                    PROBE_REQUEST_UNKNOWN_PAYLOAD_SIZE);
   begin
      Probe_Request_VN_Msg.Header.Message_Type    := Type_Probe_Request;
      Probe_Request_VN_Msg.Header.Opcode          := OPCODE_REQUEST_ADDR_BLOCK;
      Probe_Request_VN_Msg.Header.Payload_Length  := Payload_Length;
   end To_Probe_Request;

end VN.Message.Probe_Request;
