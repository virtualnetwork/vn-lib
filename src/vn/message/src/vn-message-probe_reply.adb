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

package body VN.Message.Probe_Reply is

   procedure To_Basic(Probe_Reply_VN_Msg: in VN_Message_Probe_Reply;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Probe_Reply := Probe_Reply_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      Basic_VN_Msg.Header.Message_Type := Type_Basic;
   end To_Basic;

   procedure To_Probe_Reply(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Probe_Reply_VN_Msg: out VN_Message_Probe_Reply) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Probe_Reply_VN_Msg'Address;
      Payload_Length : VN_Length := VN_Length(MAX_PAYLOAD_SIZE -
                                    PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE);
   begin
      Probe_Reply_VN_Msg.Header.Message_Type    := Type_Probe_Reply;
      Probe_Reply_VN_Msg.Header.Opcode          := OPCODE_PROBE_REPLY;
      Probe_Reply_VN_Msg.Header.Payload_Length  := Payload_Length;
   end To_Probe_Reply;

end VN.Message.Probe_Reply;
