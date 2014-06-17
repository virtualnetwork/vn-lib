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

package VN.Message.Probe_Reply is

   -- CUUID_SIZE twice for XUUID
   PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE :
                                    constant integer := MAX_PAYLOAD_SIZE -
                                                         CUUID_SIZE -
                                                         CUUID_SIZE -
                                                         FAULT_INDICATOR_SIZE - 
                                                         UPTIME_SIZE -
                                                         DIALOG_IDENTIFIER_SIZE;
   type VN_Probe_Reply_Unknown_Payload is Array(1 ..
                              PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE) of
                              Interfaces.Unsigned_8;

   type VN_Message_Probe_Reply is
      record
         Header                  : VN_Header;
         Unknown_Payload         : VN_Probe_Reply_Unknown_Payload;
         CUUID                   : VN_CUUID;
         XUUID                   : VN_CUUID; -- TODO: Implement XUUID properly
         Fault_Indicator         : VN_Fault_Indicator;
         Uptime                  : VN_Uptime;
         Dialog_Identifier       : VN_Dialog_Identifier;
         Checksum                : VN_Checksum;
      end record;

   for VN_Message_Probe_Reply use record
      Header            at 0 range 0 .. HEADER_SIZE * 8 - 1;

      CUUID             at 0 range HEADER_SIZE * 8 ..
                                   (HEADER_SIZE + CUUID_SIZE) * 8 - 1;

      -- "CUUID Size" twice cause XUUID is the same size but the proper type
      -- is not implemented yet.
      XUUID             at 0 range (HEADER_SIZE + CUUID_SIZE) * 8 ..
                                   (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE) * 8 - 1;

      Fault_Indicator   at 0 range (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE) * 8 ..
                                   (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE + FAULT_INDICATOR_SIZE) * 8 - 1;

      Uptime at 0 range (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE + FAULT_INDICATOR_SIZE) * 8 ..
                                   (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE + FAULT_INDICATOR_SIZE + UPTIME_SIZE) * 8 - 1;

      Dialog_Identifier at 0 range (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE + FAULT_INDICATOR_SIZE + UPTIME_SIZE) * 8 ..
                                   (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE + FAULT_INDICATOR_SIZE + UPTIME_SIZE + DIALOG_IDENTIFIER_SIZE) * 8 - 1;

      Unknown_Payload   at 0 range (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE + FAULT_INDICATOR_SIZE + UPTIME_SIZE + DIALOG_IDENTIFIER_SIZE) * 8 ..
        (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE + FAULT_INDICATOR_SIZE + UPTIME_SIZE + DIALOG_IDENTIFIER_SIZE + PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE) * 8 - 1;

      Checksum          at 0 range (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE + FAULT_INDICATOR_SIZE + UPTIME_SIZE + DIALOG_IDENTIFIER_SIZE + PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE) * 8 ..
          (HEADER_SIZE + CUUID_SIZE + CUUID_SIZE + FAULT_INDICATOR_SIZE + UPTIME_SIZE + DIALOG_IDENTIFIER_SIZE + PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE + CHECKSUM_SIZE) * 8 - 1;
   end record;

   for VN_Message_Probe_Reply'Alignment use 2;

   procedure To_Basic(
               Probe_Reply_VN_Msg: in VN_Message_Probe_Reply;
               Basic_VN_Msg: out VN_Message_Basic);

   procedure To_Probe_Reply(
               Basic_VN_Msg: in VN_Message_Basic;
               Probe_Reply_VN_Msg: out VN_Message_Probe_Reply);

end VN.Message.Probe_Reply;

