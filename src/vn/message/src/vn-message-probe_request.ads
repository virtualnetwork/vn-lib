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

package VN.Message.Probe_Request is

   PROBE_REQUEST_UNKNOWN_PAYLOAD_SIZE :
                                    constant integer := MAX_PAYLOAD_SIZE -
                                                DIALOG_IDENTIFIER_SIZE -
                                                REPLY_COUNT_SIZE -
                                                REPLY_PERIOD_SIZE;

   type VN_Probe_Request_Unknown_Payload is Array(1 ..
                              PROBE_REQUEST_UNKNOWN_PAYLOAD_SIZE) of
                              Interfaces.Unsigned_8;

   type VN_Message_Probe_Request is
      record
         Header                  : VN_Header;
         Unknown_Payload         : VN_Probe_Request_Unknown_Payload;
         Dialog_Identifier       : VN_Dialog_Identifier;
         Reply_Count             : VN_Reply_Count;
         Reply_Period            : VN_Reply_Period;
         Checksum                : VN_Checksum;
      end record;

   for VN_Message_Probe_Request use record
      Header            at 0 range 0 .. HEADER_SIZE * 8 - 1;

      Dialog_Identifier at 0 range HEADER_SIZE * 8 ..
                                   (HEADER_SIZE + DIALOG_IDENTIFIER_SIZE) * 8 - 1;

      Reply_Count       at 0 range (HEADER_SIZE + DIALOG_IDENTIFIER_SIZE) * 8 ..
        (HEADER_SIZE + DIALOG_IDENTIFIER_SIZE + REPLY_COUNT_SIZE) * 8 - 1;

      Reply_Period at 0 range (HEADER_SIZE + DIALOG_IDENTIFIER_SIZE + REPLY_COUNT_SIZE) * 8 ..
        (HEADER_SIZE + DIALOG_IDENTIFIER_SIZE + REPLY_COUNT_SIZE + REPLY_PERIOD_SIZE) * 8 - 1;

      Unknown_Payload   at 0 range (HEADER_SIZE + DIALOG_IDENTIFIER_SIZE + REPLY_COUNT_SIZE + REPLY_PERIOD_SIZE) * 8 ..
					(HEADER_SIZE + DIALOG_IDENTIFIER_SIZE + REPLY_COUNT_SIZE + REPLY_PERIOD_SIZE + PROBE_REQUEST_UNKNOWN_PAYLOAD_SIZE) * 8 - 1;

      Checksum          at 0 range (HEADER_SIZE + DIALOG_IDENTIFIER_SIZE + REPLY_COUNT_SIZE + REPLY_PERIOD_SIZE + PROBE_REQUEST_UNKNOWN_PAYLOAD_SIZE) * 8 ..
          (HEADER_SIZE + DIALOG_IDENTIFIER_SIZE + REPLY_COUNT_SIZE + REPLY_PERIOD_SIZE + PROBE_REQUEST_UNKNOWN_PAYLOAD_SIZE + CHECKSUM_SIZE) * 8 - 1;

   end record;

   for VN_Message_Probe_Request'Alignment use 2;

   procedure To_Basic(
               Probe_Request_VN_Msg: in VN_Message_Probe_Request;
               Basic_VN_Msg: out VN_Message_Basic);

   procedure To_Probe_Request(
               Basic_VN_Msg: in VN_Message_Basic;
               Probe_Request_VN_Msg: out VN_Message_Probe_Request);

end VN.Message.Probe_Request;

