package body VN.Message.Local_Ack is

   -- Get_Status
   function Get_Status(Message: VN_Message_Local_Ack) return VN_Status is
   begin
      return Message.Status;
   end Get_Status;

   -- Set_Status
   procedure Set_Status(Message: out VN_Message_Local_Ack; Status: VN_Status) is
   begin
      Message.Status := Status;
   end Set_Status;

   overriding
   procedure Initialize(This: in out VN_Message_Local_Ack) is
   begin
      This.Header.Message_Type := Type_Local_Ack;
      This.Header.Opcode := 16#21#;
   end;

end VN.Message.Local_Ack;
