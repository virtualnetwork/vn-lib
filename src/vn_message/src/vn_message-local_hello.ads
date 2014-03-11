package VN_Message.Local_Hello is

   type VN_Message_Local_Hello is new VN_Message with null record;
   -- TODO Add records that are specific to the Local_Hello Message payload.

   function Get_Payload(Message: VN_Message_Local_Hello) return VN_Payload;
   procedure Set_Payload(Message: in out VN_Message_Local_Hello; Payload: VN_Payload);

end VN_Message.Local_Hello;
