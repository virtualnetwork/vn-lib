package body VN.Message is

   procedure Serialize(Message : in VN_Message_Basic; buffer : out VN_Message_Byte_Array) is
      tempMsg : VN_Message_Basic := Message;
      for tempMsg'Address use buffer'Address;
   begin
      null;
   end Serialize;

   procedure Deserialize(Message : out VN_Message_Basic; buffer : in VN_Message_Byte_Array) is
      tempMsg : VN_Message_Basic;
      -- TODO: Fix this warning.
      -- Use pragma Import for "tempMsg" to suppress initialization
      -- Pragma Import(tempMsg);
      for tempMsg'Address use buffer'Address;
   begin
      Message := tempMsg;
   end DeSerialize;


--   procedure Update_Checksum(Message: in out VN_Message_Access) is
--   begin
--      Message.Checksum := 5; -- TODO: Update the checksum with a proper
--                                    -- value.
--   end Update_Checksum;
--

end VN.Message;
