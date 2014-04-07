

package body Message is

   procedure Serialize(Message : in VN_Message_Basic; buffer : out VN_Message_Byte_Array) is
      tempMsg : VN_Message_Basic := Message;
      for tempMsg'Address use buffer'Address;
   begin
      null;
   end Serialize;

   procedure DeSerialize(Message : out VN_Message_Basic; buffer : in VN_Message_Byte_Array) is
      tempMsg : VN_Message_Basic;
    --  Pragma Import(tempMsg);--use pragma Import for "tempMsg" to suppress initialization
      for tempMsg'Address use buffer'Address;
   begin
      Message := tempMsg;
   end DeSerialize;

end Message;

