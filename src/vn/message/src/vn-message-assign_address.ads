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
--  Copyright 2014 Christoffer Holmstedt (christoffer.holmstedt@gmail.com)
------------------------------------------------------------------------------

package VN.Message.Assign_Address is

   ASSIGN_ADDRESS_UNKNOWN_PAYLOAD_SIZE : constant integer :=
     MAX_PAYLOAD_SIZE - CUUID_SIZE - VN_LOGICAL_ADDRESS_SIZE;

   type VN_Assign_Address_Unknown_Payload is Array(1 ..
                              ASSIGN_ADDRESS_UNKNOWN_PAYLOAD_SIZE) of
                              Interfaces.Unsigned_8;

   type VN_Message_Assign_Address is
      record
         Header                  : VN_Header;
         Unknown_Payload         : VN_Assign_Address_Unknown_Payload;
         CUUID                   : VN_CUUID;
         Assigned_Address        : VN_Logical_Address;
         Checksum                : VN_Checksum;
      end record;

   for VN_Message_Assign_Address use record
      Header            at 0 range 0 .. (HEADER_SIZE * 8 - 1);

      CUUID		at 0 range HEADER_SIZE * 8 ..
                                   (HEADER_SIZE + CUUID_SIZE) * 8 - 1;

      Assigned_Address  at 0 range (HEADER_SIZE + CUUID_SIZE) * 8 ..
        			    (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE) * 8 - 1;

      Unknown_Payload   at 0 range (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE) * 8 ..
        				(HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE +
           				 ASSIGN_ADDRESS_UNKNOWN_PAYLOAD_SIZE) * 8 - 1;

      Checksum          at 0 range (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE +
                                      ASSIGN_ADDRESS_UNKNOWN_PAYLOAD_SIZE) * 8 ..
        				(HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE +
             				 ASSIGN_ADDRESS_UNKNOWN_PAYLOAD_SIZE + CHECKSUM_SIZE) * 8 - 1;
   end record;

   for VN_Message_Assign_Address'Alignment use 2;

   procedure To_Basic(
               Assign_Address_VN_Msg: in VN_Message_Assign_Address;
               Basic_VN_Msg: out VN_Message_Basic);

   procedure To_Assign_Address(
               Basic_VN_Msg: in VN_Message_Basic;
               Assign_Address_VN_Msg: out VN_Message_Assign_Address);

end VN.Message.Assign_Address;

