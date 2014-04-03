package body VN.Message is

   -- Get_Version
   function Get_Version(Message: VN_Message_Basic) return VN_Version is
   begin
--        return Message.Header.Version;
      return VN_Version(0);
   end Get_Version;

   -- Set_Version
   procedure Set_Version(Message: out VN_Message_Basic; Version: VN_Version ) is
   begin
--        Message.Header.Version := Version;
      null;
   end Set_Version;

   function Get_Checksum(Message: in VN_Message_Basic) return VN_Checksum is
   begin
--        return Message.Checksum;
      return VN_Checksum(0);
   end Get_Checksum;

   procedure Update_Checksum(Message: in out VN_Message_Basic) is
   begin
--        Message.Checksum := 5; -- TODO: Update the checksum with a proper
                                    -- value.
      null;
   end Update_Checksum;

   procedure Assignment (destination : out VN_Message_Basic; source : in VN_Message_Basic) is
   begin
      destination.Header := source.Header;
      destination.Checksum := source.Checksum;
   end Assignment;

end VN.Message;
