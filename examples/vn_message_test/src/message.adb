

package body Message is

   procedure Serialize(Message : in VN_Message_Basic; buffer : out Buffer_Array_Type) is
      tempMsg : VN_Message_Basic := Message;
      for tempMsg'Address use buffer'Address;
   begin
      null;
   end Serialize;

   procedure DeSerialize(Message : out VN_Message_Basic; buffer : in Buffer_Array_Type) is
      tempMsg : VN_Message_Basic;
      for tempMsg'Address use buffer'Address;
   begin
      Message := tempMsg;
   end DeSerialize;

end Message;

