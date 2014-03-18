package VN_Message.Local_Ack is

   type VN_Message_Local_Ack is new VN_Message_Basic with private;

   function Get_Status(Message: VN_Message_Local_Ack) return VN_Status;
   procedure Set_Status(Message: out VN_Message_Local_Ack; Status: VN_Status);

private

   type VN_Message_Local_Ack is new VN_Message_Basic with
      record
        Status: VN_Status := 0;
      end record;

   overriding
   procedure Initialize(This: in out VN_Message_Local_Ack);

   overriding
   procedure Finalize(This: in out VN_Message_Local_Ack) is null;

end VN_Message.Local_Ack;
