package body VN_Message.Local_Hello is

   -- Get_Payload
   function Get_Payload(Message: VN_Message_Local_Hello) return VN_Payload is
   begin
      return Message.Payload;
   end Get_Payload;

   -- Set_Payload
   procedure Set_Payload(Message: in out VN_Message_Local_Hello; Payload: VN_Payload) is
   begin
      -- TODO: Set payload to specific fields in the message type.
      Message.Payload := Payload;
   end Set_Payload;

end VN_Message.Local_Hello;
