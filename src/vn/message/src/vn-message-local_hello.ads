package VN.Message.Local_Hello is

   LOCAL_HELLO_UNKNOWN_PAYLOAD_SIZE : constant integer := MAX_PAYLOAD_SIZE - 17;

   type VN_Local_Hello_Unknown_Payload is Array(1 ..
                                          LOCAL_HELLO_UNKNOWN_PAYLOAD_SIZE) of
                                          Interfaces.Unsigned_8;

   type VN_Message_Local_Hello is
      record
         Header           : VN_Header;
         CUUID            : VN_CUUID;
         Component_Type   : VN_Component_Type;
         Checksum         : VN_Checksum;
         Unknown_Payload  : VN_Local_Hello_Unknown_Payload;
      end record;

--     for VN_Message_Local_Hello use record
--        Header            at 0 range (CHECKSUM_SIZE * 8 +
--                                      COMPONENT_TYPE_SIZE * 8 +
--                                      CUUID_SIZE * 8 +
--                                      LOCAL_HELLO_UNKNOWN_PAYLOAD_SIZE * 8) ..
--                                     (CHECKSUM_SIZE * 8 +
--                                      COMPONENT_TYPE_SIZE * 8 +
--                                      CUUID_SIZE * 8 +
--                                      LOCAL_HELLO_UNKNOWN_PAYLOAD_SIZE * 8 +
--                                      HEADER_SIZE * 8 - 1);
--
--        Unknown_Payload   at 0 range (CHECKSUM_SIZE * 8 +
--                                      COMPONENT_TYPE_SIZE * 8 +
--                                      CUUID_SIZE * 8) ..
--                                     (CHECKSUM_SIZE * 8 +
--                                      COMPONENT_TYPE_SIZE * 8 +
--                                      CUUID_SIZE * 8 +
--                                      LOCAL_HELLO_UNKNOWN_PAYLOAD_SIZE * 8 - 1);
--
--        CUUID             at 0 range (CHECKSUM_SIZE * 8 +
--                                      COMPONENT_TYPE_SIZE * 8) ..
--                                     (CHECKSUM_SIZE * 8 +
--                                      COMPONENT_TYPE_SIZE * 8 +
--                                      CUUID_SIZE * 8 - 1);
--
--        Component_Type    at 0 range (CHECKSUM_SIZE * 8) ..
--                                     (CHECKSUM_SIZE * 8 +
--                                      COMPONENT_TYPE_SIZE * 8 - 1);
--
--        Checksum      at 0 range 0 .. (CHECKSUM_SIZE * 8 - 1);
--     end record;

   for VN_Message_Local_Hello use record
      Header            at 0 range 0 .. HEADER_SIZE * 8 - 1;

      CUUID             at 0 range HEADER_SIZE * 8 .. (HEADER_SIZE + CUUID_SIZE) * 8 - 1;

      Component_Type    at 0 range HEADER_SIZE * 8 + CUUID_SIZE * 8..
        HEADER_SIZE * 8 + CUUID_SIZE * 8 + COMPONENT_TYPE_SIZE * 8 - 1;

      Checksum      at 0 range (HEADER_SIZE + CUUID_SIZE + COMPONENT_TYPE_SIZE) * 8 ..
        (HEADER_SIZE + CUUID_SIZE + COMPONENT_TYPE_SIZE + CHECKSUM_SIZE) * 8 - 1;

      Unknown_Payload   at 0 range (HEADER_SIZE + CUUID_SIZE + COMPONENT_TYPE_SIZE + CHECKSUM_SIZE) * 8 ..
        (HEADER_SIZE + CUUID_SIZE + COMPONENT_TYPE_SIZE + CHECKSUM_SIZE + LOCAL_HELLO_UNKNOWN_PAYLOAD_SIZE) * 8 - 1;
   end record;


   for VN_Message_Local_Hello'Alignment use 1;

   procedure To_Basic(Local_Hello_VN_Msg: in VN_Message_Local_Hello;
                      Basic_VN_Msg: out VN_Message_Basic);

   procedure To_Local_Hello(Basic_VN_Msg: in VN_Message_Basic;
                            Local_Hello_VN_Msg: out VN_Message_Local_Hello);

end VN.Message.Local_Hello;

