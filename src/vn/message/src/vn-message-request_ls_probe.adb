package body VN.Message.Request_LS_Probe is

   procedure To_Basic(Request_LS_Probe_VN_Msg: in VN_Message_Request_LS_Probe;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Request_LS_Probe := Request_LS_Probe_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      Basic_VN_Msg.Header.Message_Type := Type_Basic;
   end To_Basic;

   procedure To_Request_LS_Probe(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Request_LS_Probe_VN_Msg: out VN_Message_Request_LS_Probe) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Request_LS_Probe_VN_Msg'Address;
      Payload_Length : VN_Length := VN_Length(MAX_PAYLOAD_SIZE -
                                    REQUEST_LS_PROBE_UNKNOWN_PAYLOAD_SIZE);
   begin
      Request_LS_Probe_VN_Msg.Header.Message_Type    := Type_Request_LS_Probe;
      Request_LS_Probe_VN_Msg.Header.Opcode          := OPCODE_REQUEST_LS_PROBE;
      Request_LS_Probe_VN_Msg.Header.Payload_Length  := Payload_Length;
   end To_Request_LS_Probe;

end VN.Message.Request_LS_Probe;
