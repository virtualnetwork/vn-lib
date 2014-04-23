
package body VN.Message is

   procedure Serialize(Message : in VN_Message_Basic; buffer : out VN_Message_Byte_Array) is
      tempMsg : VN_Message_Basic;
      pragma Warnings(Off, tempMsg);
      for tempMsg'Address use buffer'Address;
   begin
      tempMsg := Message;
      Update_Checksum(tempMsg);
   end Serialize;

   procedure Deserialize(Message : out VN_Message_Basic; buffer : in VN_Message_Byte_Array) is
      tempBuffer : VN_Message_Byte_Array;
      for tempBuffer'Address use Message'Address;

      receivedChecksum : VN_Checksum;
   begin
      tempBuffer := buffer;
      receivedChecksum := Message.Checksum;

      Update_Checksum(Message);

      if receivedChecksum /= Message.Checksum then
         raise VN.Message.VN_CHECKSUM_ERROR with "Received checksum= " &
           receivedChecksum'Img & ", acctual checksum= " & Message.Checksum'Img;
      end if;

   end DeSerialize;

   procedure Update_Checksum(Message: in out VN_Message_Basic) is

      use Interfaces;

      wordArray : Word_Array_Type;
      for wordArray'Address use Message'Address;

      byteArray : VN_Message_Byte_Array;
      for byteArray'Address use Message'Address;

      sum    : Interfaces.Unsigned_32 := 0;
      ret    : Interfaces.Unsigned_16;
      length : integer := Integer(Message.Header.Payload_Length) + HEADER_SIZE;
   begin

      for i in 1 .. length / 2 loop
         sum := sum + Interfaces.Unsigned_32(wordArray(i));
      end loop;

      if length rem 2 = 1 then
         sum := sum + Interfaces.Unsigned_32(byteArray(length));
      end if;

      sum := sum + Interfaces.Shift_Right(sum, 16); -- add the carry bits to the answer
      sum := sum and 16#FFFF#;
      ret := not Interfaces.Unsigned_16(sum);

      Message.Checksum := VN_Checksum(ret);
   end Update_Checksum;

end VN.Message;
