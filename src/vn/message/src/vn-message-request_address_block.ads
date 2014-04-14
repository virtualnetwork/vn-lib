package VN.Message.Request_Address_Block is

   REQUEST_ADDRESS_BLOCK_UNKNOWN_PAYLOAD_SIZE :
                                    constant integer := MAX_PAYLOAD_SIZE - 16;

   type VN_Request_Address_Block_Unknown_Payload is Array(1 ..
                              REQUEST_ADDRESS_BLOCK_UNKNOWN_PAYLOAD_SIZE) of
                              Interfaces.Unsigned_8;

   type VN_Message_Request_Address_Block is
      record
         Header                  : VN_Header;
         Unknown_Payload         : VN_Request_Address_Block_Unknown_Payload;
         CUUID                   : VN_CUUID;
         Checksum                : VN_Checksum;
      end record;

   for VN_Message_Request_Address_Block use record
      Header            at 0 range (CHECKSUM_SIZE * 8 +
                                    CUUID_SIZE * 8 +
                                    REQUEST_ADDRESS_BLOCK_UNKNOWN_PAYLOAD_SIZE * 8) ..
                                   (CHECKSUM_SIZE * 8 +
                                    CUUID_SIZE * 8 +
                                    REQUEST_ADDRESS_BLOCK_UNKNOWN_PAYLOAD_SIZE * 8 +
                                    HEADER_SIZE * 8 - 1);

      Unknown_Payload   at 0 range (CHECKSUM_SIZE * 8 +
                                    CUUID_SIZE * 8) ..
                                   (CHECKSUM_SIZE * 8 +
                                    CUUID_SIZE * 8 +
                                    REQUEST_ADDRESS_BLOCK_UNKNOWN_PAYLOAD_SIZE * 8 - 1);

      CUUID             at 0 range (CHECKSUM_SIZE * 8) ..
                                   (CHECKSUM_SIZE * 8 +
                                    CUUID_SIZE * 8 - 1);

      Checksum          at 0 range 0 .. (CHECKSUM_SIZE * 8 - 1);
   end record;

   for VN_Message_Request_Address_Block'Alignment use 1;

   procedure To_Basic(
               Request_Address_Block_VN_Msg: in VN_Message_Request_Address_Block;
               Basic_VN_Msg: out VN_Message_Basic);

   procedure To_Request_Address_Block(
               Basic_VN_Msg: in VN_Message_Basic;
               Request_Address_Block_VN_Msg: out VN_Message_Request_Address_Block);

end VN.Message.Request_Address_Block;

