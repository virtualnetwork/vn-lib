------------------------------------------------------------------------------
--  This file is part of VN-Lib.
--
--  VN-Lib is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  VN-Lib is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with VN-Lib.  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright 2014, Nils Brynedal Ignell (nils.brynedal@gmail.com) and
--  Christoffer Holmstedt (christoffer.holmstedt@gmail.com).
------------------------------------------------------------------------------

-- Summary: The VN.Message encapsulate all functionalities regarding VN messages.

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
      if length > MAX_PAYLOAD_SIZE then
         length := MAX_PAYLOAD_SIZE;
      end if;

      for i in 1 .. length / 2 loop
         sum := sum + Interfaces.Unsigned_32(wordArray(i));
      end loop;

      if length rem 2 = 1 then
         sum := sum + Interfaces.Unsigned_32(byteArray(length)); --ToDo: Bit padding, to the left of to the right?
      end if;

      sum := sum + Interfaces.Shift_Right(sum, 16); -- add the carry bits to the answer
      sum := sum and 16#FFFF#;
      ret := not Interfaces.Unsigned_16(sum);
      Message.Checksum := VN_Checksum(ret);
   end Update_Checksum;

end VN.Message;
