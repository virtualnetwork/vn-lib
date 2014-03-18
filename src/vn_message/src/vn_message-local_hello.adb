package body VN_Message.Local_Hello is

   -- Get_CUUID
   function Get_CUUID(Message: VN_Message_Local_Hello) return VN_CUUID is
   begin
      return Message.CUUID;
   end Get_CUUID;

   -- Set_CUUID
   procedure Set_CUUID(Message: out VN_Message_Local_Hello; CUUID: VN_CUUID) is
   begin
      Message.CUUID := CUUID;
   end Set_CUUID;

   -- Get_Component_Type
   function Get_Component_Type(Message: VN_Message_Local_Hello)
                                 return VN_Component_Types is
   begin
      return Message.Component_Type;
   end Get_Component_Type;

   -- Set_Component_Type
   procedure Set_Component_Type(Message: out VN_Message_Local_Hello;
                                 Component_Type: VN_Component_Types) is
   begin
      Message.Component_Type := Component_Type;
   end Set_Component_Type;

   overriding
   procedure Initialize(This: in out VN_Message_Local_Hello) is
   begin
      This.Header.Message_Type := Type_Local_Hello;
      This.Header.Opcode := 16#20#;
   end;

end VN_Message.Local_Hello;
