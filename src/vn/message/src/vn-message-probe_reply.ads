package VN.Message.Probe_Reply is

   PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE :
                                    constant integer := MAX_PAYLOAD_SIZE - CUUID_SIZE;

   type VN_Probe_Reply_Unknown_Payload is Array(1 ..
                              PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE) of
                              Interfaces.Unsigned_8;

   type VN_Message_Probe_Reply is
      record
         Header                  : VN_Header;
         Unknown_Payload         : VN_Probe_Reply_Unknown_Payload;
         CUUID                   : VN_CUUID;
         Checksum                : VN_Checksum;
      end record;

   for VN_Message_Probe_Reply use record
      Header            at 0 range 0 .. HEADER_SIZE * 8 - 1;

      CUUID             at 0 range HEADER_SIZE * 8 ..
                                   (HEADER_SIZE + CUUID_SIZE) * 8 - 1;

      Unknown_Payload   at 0 range (HEADER_SIZE + CUUID_SIZE) * 8 ..
        (HEADER_SIZE + CUUID_SIZE + PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE) * 8 - 1;

      Checksum          at 0 range (HEADER_SIZE + CUUID_SIZE +
                                      PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE) * 8 ..
          (HEADER_SIZE + CUUID_SIZE + PROBE_REPLY_UNKNOWN_PAYLOAD_SIZE + CHECKSUM_SIZE) * 8 - 1;
   end record;

   for VN_Message_Probe_Reply'Alignment use 2;

   procedure To_Basic(
               Probe_Reply_VN_Msg: in VN_Message_Probe_Reply;
               Basic_VN_Msg: out VN_Message_Basic);

   procedure To_Probe_Reply(
               Basic_VN_Msg: in VN_Message_Basic;
               Probe_Reply_VN_Msg: out VN_Message_Probe_Reply);

end VN.Message.Probe_Reply;

