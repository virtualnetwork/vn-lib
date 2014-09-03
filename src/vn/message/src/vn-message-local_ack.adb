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

package body VN.Message.Local_Ack is

   procedure To_Basic(Local_Ack_VN_Msg: in VN_Message_Local_Ack;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Local_Ack := Local_Ack_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      Basic_VN_Msg.Header.Message_Type := Type_Basic;
   end To_Basic;

   procedure To_Local_Ack(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Local_Ack_VN_Msg: out VN_Message_Local_Ack) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Local_Ack_VN_Msg'Address;
      Payload_Length : VN_Length := VN_Length(MAX_PAYLOAD_SIZE -
                                    LOCAL_ACK_UNKNOWN_PAYLOAD_SIZE);
   begin
      Local_Ack_VN_Msg.Header.Message_Type    := Type_Local_Ack;
      Local_Ack_VN_Msg.Header.Opcode          := OPCODE_LOCAL_ACK;
      Local_Ack_VN_Msg.Header.Payload_Length  := Payload_Length;
   end To_Local_Ack;

end VN.Message.Local_Ack;
