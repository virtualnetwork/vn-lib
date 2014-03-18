package VN_Message.Local_Hello is

   type VN_Message_Local_Hello is new VN_Message_Basic with private;

   function Get_CUUID(Message: VN_Message_Local_Hello) return VN_CUUID;
   procedure Set_CUUID(Message: out VN_Message_Local_Hello; CUUID: VN_CUUID);

   function Get_Component_Type(Message: VN_Message_Local_Hello)
                                 return VN_Component_Types;

   procedure Set_Component_Type(Message: out VN_Message_Local_Hello;
                                 Component_Type: VN_Component_Types);
private

   type VN_Message_Local_Hello is new VN_Message_Basic with
      record
        CUUID: VN_CUUID := 0;
        Component_Type: VN_Component_Types := Unknown;
      end record;

   overriding
   procedure Initialize(This: in out VN_Message_Local_Hello);

   overriding
   procedure Finalize(This: in out VN_Message_Local_Hello) is null;

end VN_Message.Local_Hello;
