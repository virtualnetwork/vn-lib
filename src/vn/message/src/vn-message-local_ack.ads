package VN.Message.Local_Ack is

   LOCAL_ACK_UNKNOWN_PAYLOAD_SIZE : constant integer := MAX_PAYLOAD_SIZE - 1;

   type VN_Local_Ack_Unknown_Payload is Array(1 ..
                                       LOCAL_ACK_UNKNOWN_PAYLOAD_SIZE) of
                                       Interfaces.Unsigned_8;

   type VN_Message_Local_Ack is
      record
         Header           : VN_Header;
         Unknown_Payload  : VN_Local_Ack_Unknown_Payload;
         Status           : VN_Status;
         Checksum         : VN_Checksum;
      end record;

   for VN_Message_Local_Ack use record
      Header            at 0 range (CHECKSUM_SIZE * 8 +
                                    STATUS_SIZE * 8 +
                                    LOCAL_ACK_UNKNOWN_PAYLOAD_SIZE * 8) ..
                                   (CHECKSUM_SIZE * 8 +
                                    STATUS_SIZE * 8 +
                                    LOCAL_ACK_UNKNOWN_PAYLOAD_SIZE * 8 +
                                    HEADER_SIZE * 8 - 1);

      Unknown_Payload   at 0 range (CHECKSUM_SIZE * 8 +
                                    STATUS_SIZE * 8) ..
                                   (CHECKSUM_SIZE * 8 +
                                    STATUS_SIZE * 8 +
                                    LOCAL_ACK_UNKNOWN_PAYLOAD_SIZE * 8 - 1);

      Status            at 0 range (CHECKSUM_SIZE * 8) ..
                                   (CHECKSUM_SIZE * 8 +
                                    STATUS_SIZE * 8 - 1);

      Checksum          at 0 range 0 .. (CHECKSUM_SIZE * 8 - 1);
   end record;

   for VN_Message_Local_Ack'Alignment use 1;

   procedure To_Basic(Local_Ack_VN_Msg: in VN_Message_Local_Ack;
                      Basic_VN_Msg: out VN_Message_Basic);

   procedure To_Local_Ack(Basic_VN_Msg: in VN_Message_Basic;
                          Local_Ack_VN_Msg: out VN_Message_Local_Ack);

end VN.Message.Local_Ack;

