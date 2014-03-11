package body VN_Message.Empty is

   -- Get_Payload
   function Get_Payload(Message: VN_Message_Empty) return VN_Payload is
   begin
      return Message.Payload;
   end Get_Payload;

   -- Set_Payload
   procedure Set_Payload(Message: in out VN_Message_Empty; Payload: VN_Payload) is
   begin
      Message.Payload := Payload;
   end Set_Payload;

end VN_Message.Empty;
