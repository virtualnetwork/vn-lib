package body VN.Message is

   -- Get_Version
   function Get_Version(Message: VN_Message_Access) return VN_Version is
   begin
      return Message.Header.Version;
   end Get_Version;

   -- Set_Version
   procedure Set_Version(Message: in out VN_Message_Access; Version: VN_Version ) is
   begin
      if Message /= null then
         Message.Header.Version := Version;
      end if;
   end Set_Version;

   function Get_Checksum(Message: in VN_Message_Access) return VN_Checksum is
   begin
      return Message.Checksum;
   end Get_Checksum;

   procedure Update_Checksum(Message: in out VN_Message_Access) is
   begin
      Message.Checksum := 5; -- TODO: Update the checksum with a proper
                                    -- value.
   end Update_Checksum;

   procedure Free(Message: in out VN_Message_Access) is
   begin
      if Message /= null then
         Free_Message_Access(Message);
      end if;
   end Free;

end VN.Message;
