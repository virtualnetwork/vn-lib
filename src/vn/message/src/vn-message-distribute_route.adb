package body VN.Message.Distribute_Route is

   procedure To_Basic(Distribute_Route_VN_Msg: in VN_Message_Distribute_Route;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Distribute_Route := Distribute_Route_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      Basic_VN_Msg.Header.Message_Type := Type_Basic;
   end To_Basic;

   procedure To_Distribute_Route(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Distribute_Route_VN_Msg: out VN_Message_Distribute_Route) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Distribute_Route_VN_Msg'Address;
      Payload_Length : VN_Length := VN_Length(MAX_PAYLOAD_SIZE -
                                    DISTRIBUTE_ROUTE_UNKNOWN_PAYLOAD_SIZE);
   begin
      Distribute_Route_VN_Msg.Header.Message_Type    := Type_Distribute_Route;
      Distribute_Route_VN_Msg.Header.Opcode          := 16#72#;
      Distribute_Route_VN_Msg.Header.Payload_Length  := Payload_Length;
   end To_Distribute_Route;

end VN.Message.Distribute_Route;
