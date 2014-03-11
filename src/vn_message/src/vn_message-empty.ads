package VN_Message.Empty is

   type VN_Message_Empty is new VN_Message with null record;

   function Get_Payload(Message: VN_Message_Empty) return VN_Payload;
   procedure Set_Payload(Message: in out VN_Message_Empty; Payload: VN_Payload);

end VN_Message.Empty;
