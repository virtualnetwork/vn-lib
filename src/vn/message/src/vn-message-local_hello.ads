package VN.Message.Local_Hello is

   type VN_Message_Local_Hello is
      record
         Header           : VN_Header;
         Unknown_Payload  : VN_Payload_Byte_Array;
         CUUID            : VN_CUUID;
         Component_Type   : VN_Component_Type;
         Checksum         : VN_Checksum;
      end record;

   for VN_Message_Local_Hello use record
      Header            at 0 range (16 + MAX_PAYLOAD_SIZE * 8) ..
                                   (16 + MAX_PAYLOAD_SIZE * 8 + 135);
      Unknown_Payload   at 0 range 152 .. (15 + MAX_PAYLOAD_SIZE * 8);
      CUUID             at 0 range 24 .. 151;
      Component_Type    at 0 range 16 .. 23;
      Checksum          at 0 range 0 .. 15;
   end record;

end VN.Message.Local_Hello;

