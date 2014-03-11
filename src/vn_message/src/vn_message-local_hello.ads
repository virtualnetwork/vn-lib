package VN_Message.Local_Hello is

   type VN_Message_Local_Hello is new VN_Message with private;

   function Get_Payload(Message: VN_Message_Local_Hello) return VN_Payload;
   procedure Set_Payload(Message: in out VN_Message_Local_Hello; Payload: VN_Payload);

private

   type VN_Message_Local_Hello is new VN_Message with
      record
        CUUID: VN_CUUID;
        Component_Type: VN_Component_Type;
      end record;

end VN_Message.Local_Hello;
