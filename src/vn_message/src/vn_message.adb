package body VN_Message is

   -- Get_Version
   function Get_Version(Message: VN_Message_Basic) return VN_Version is
   begin
      return Message.Header.Version;
   end Get_Version;

   -- Set_Version
   procedure Set_Version(Message: out VN_Message_Basic; Version: VN_Version ) is
   begin
      Message.Header.Version := Version;
   end Set_Version;

   function Get_Checksum(Message: in VN_Message_Basic) return VN_Checksum is
   begin
      return Message.Checksum;
   end Get_Checksum;

   procedure Update_Checksum(Message: in out VN_Message_Basic) is
   begin
      Message.Checksum := 5; -- TODO: Update the checksum with a proper
                                    -- value.
   end Update_Checksum;

end VN_Message;
