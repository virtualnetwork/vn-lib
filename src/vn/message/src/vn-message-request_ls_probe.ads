package VN.Message.Request_LS_Probe is

   REQUEST_LS_PROBE_UNKNOWN_PAYLOAD_SIZE :
                                    constant integer := MAX_PAYLOAD_SIZE - CUUID_SIZE;

   type VN_Request_LS_Probe_Unknown_Payload is Array(1 ..
                              REQUEST_LS_PROBE_UNKNOWN_PAYLOAD_SIZE) of
                              Interfaces.Unsigned_8;

   type VN_Message_Request_LS_Probe is
      record
         Header                  : VN_Header;
         Unknown_Payload         : VN_Request_LS_Probe_Unknown_Payload;
         CUUID                   : VN_CUUID;
         Checksum                : VN_Checksum;
      end record;

   for VN_Message_Request_LS_Probe use record
      Header            at 0 range 0 .. HEADER_SIZE * 8 - 1;

      CUUID             at 0 range HEADER_SIZE * 8 ..
                                   (HEADER_SIZE + CUUID_SIZE) * 8 - 1;

      Unknown_Payload   at 0 range (HEADER_SIZE + CUUID_SIZE) * 8 ..
        (HEADER_SIZE + CUUID_SIZE + REQUEST_LS_PROBE_UNKNOWN_PAYLOAD_SIZE) * 8 - 1;

      Checksum          at 0 range (HEADER_SIZE + CUUID_SIZE +
                                      REQUEST_LS_PROBE_UNKNOWN_PAYLOAD_SIZE) * 8 ..
          (HEADER_SIZE + CUUID_SIZE + REQUEST_LS_PROBE_UNKNOWN_PAYLOAD_SIZE + CHECKSUM_SIZE) * 8 - 1;
   end record;

   for VN_Message_Request_LS_Probe'Alignment use 2;

   procedure To_Basic(
               Request_LS_Probe_VN_Msg: in VN_Message_Request_LS_Probe;
               Basic_VN_Msg: out VN_Message_Basic);

   procedure To_Request_LS_Probe(
               Basic_VN_Msg: in VN_Message_Basic;
               Request_LS_Probe_VN_Msg: out VN_Message_Request_LS_Probe);

end VN.Message.Request_LS_Probe;

