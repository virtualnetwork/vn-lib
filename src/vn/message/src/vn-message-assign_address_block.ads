package VN.Message.Assign_Address_Block is

   ASSIGN_ADDRESS_BLOCK_UNKNOWN_PAYLOAD_SIZE : constant integer := MAX_PAYLOAD_SIZE - CUUID_SIZE -
     									VN_LOGICAL_ADDRESS_SIZE - RESPONSE_TYPE_SIZE;

   type VN_Assign_Address_Block_Unknown_Payload is Array(1 ..
                              ASSIGN_ADDRESS_BLOCK_UNKNOWN_PAYLOAD_SIZE) of
                              Interfaces.Unsigned_8;

   type VN_Message_Assign_Address_Block is
      record
         Header                  : VN_Header;
         Unknown_Payload         : VN_Assign_Address_Block_Unknown_Payload;
         CUUID                   : VN_CUUID;
         Assigned_Base_Address   : VN_Logical_Address;
         Response_Type           : VN_Response_Type;
         Checksum                : VN_Checksum;
      end record;

   for VN_Message_Assign_Address_Block use record
      Header            at 0 range 0 .. HEADER_SIZE * 8 - 1;

      CUUID             at 0 range HEADER_SIZE * 8 .. (HEADER_SIZE + CUUID_SIZE) * 8 - 1;

      Assigned_Base_Address at 0 range (HEADER_SIZE + CUUID_SIZE) * 8 ..
        (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE) * 8 - 1;

      Response_Type     at 0 range (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE) * 8 ..
        (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE + RESPONSE_TYPE_SIZE) * 8 - 1;

      Unknown_Payload   at 0 range (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE + RESPONSE_TYPE_SIZE) * 8 ..
					(HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE +
                                     RESPONSE_TYPE_SIZE + ASSIGN_ADDRESS_BLOCK_UNKNOWN_PAYLOAD_SIZE) * 8 - 1

      Checksum          at 0 range (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE +
                                        RESPONSE_TYPE_SIZE + ASSIGN_ADDRESS_BLOCK_UNKNOWN_PAYLOAD_SIZE) * 8 ..
            			(HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE +
                			RESPONSE_TYPE_SIZE + ASSIGN_ADDRESS_BLOCK_UNKNOWN_PAYLOAD_SIZE + CHECKSUM_SIZE) * 8 - 1;
   end record;

   for VN_Message_Assign_Address_Block'Alignment use 1;

   procedure To_Basic(
               Assign_Address_Block_VN_Msg: in VN_Message_Assign_Address_Block;
               Basic_VN_Msg: out VN_Message_Basic);

   procedure To_Assign_Address_Block(
               Basic_VN_Msg: in VN_Message_Basic;
               Assign_Address_Block_VN_Msg: out VN_Message_Assign_Address_Block);

end VN.Message.Assign_Address_Block;

