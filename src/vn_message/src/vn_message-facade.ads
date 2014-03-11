package VN_Message.Facade is

   -- Enum of different VN_Messages.
   type Message_Type is (Local_Hello);

   -- VN_Message_Empty
   type VN_Message_Empty is new VN_Message with null record;

   function Get_Payload(Message: VN_Message_Empty) return VN_Payload;
   procedure Set_Payload(Message: in out VN_Message_Empty; Payload: VN_Payload);

   procedure Cast_Message_To(Message: in out VN_Message'Class; Msg_Type: Message_Type);

end VN_Message.Facade;
