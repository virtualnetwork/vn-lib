package VN.Message.Distribute_Route is

   DISTRIBUTE_ROUTE_UNKNOWN_PAYLOAD_SIZE :
                                    constant integer := MAX_PAYLOAD_SIZE - CUUID_SIZE - VN_LOGICAL_ADDRESS_SIZE - COMPONENT_TYPE_SIZE;

   type VN_Distribute_Route_Unknown_Payload is Array(1 ..
                              DISTRIBUTE_ROUTE_UNKNOWN_PAYLOAD_SIZE) of
                              Interfaces.Unsigned_8;

   type VN_Message_Distribute_Route is
      record
         Header                  : VN_Header;
         Unknown_Payload         : VN_Distribute_Route_Unknown_Payload;
         CUUID                   : VN_CUUID;
         Component_Address       : VN_Logical_Address;
         Component_Type          : VN_Component_Type;
         Checksum                : VN_Checksum;
      end record;

   for VN_Message_Distribute_Route use record
      Header            at 0 range 0 .. HEADER_SIZE * 8 - 1;

      CUUID             at 0 range HEADER_SIZE * 8 .. (HEADER_SIZE + CUUID_SIZE) * 8 - 1;

      Component_Address at 0 range (HEADER_SIZE + CUUID_SIZE) * 8 ..
        (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE) * 8 - 1;

      Component_Type    at 0 range (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE) * 8 ..
                              (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE + COMPONENT_TYPE_SIZE) * 8 - 1;

      Unknown_Payload   at 0 range (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE + COMPONENT_TYPE_SIZE) * 8 ..
        				(HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE +
           				 COMPONENT_TYPE_SIZE + DISTRIBUTE_ROUTE_UNKNOWN_PAYLOAD_SIZE) * 8 - 1;


      Checksum          at 0 range (HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE +
                                      COMPONENT_TYPE_SIZE + DISTRIBUTE_ROUTE_UNKNOWN_PAYLOAD_SIZE) * 8 ..
          			(HEADER_SIZE + CUUID_SIZE + VN_LOGICAL_ADDRESS_SIZE +
           			 COMPONENT_TYPE_SIZE + DISTRIBUTE_ROUTE_UNKNOWN_PAYLOAD_SIZE + CHECKSUM_SIZE) * 8 - 1;
   end record;

   for VN_Message_Distribute_Route'Alignment use 1;

   procedure To_Basic(
               Distribute_Route_VN_Msg: in VN_Message_Distribute_Route;
               Basic_VN_Msg: out VN_Message_Basic);

   procedure To_Distribute_Route(
               Basic_VN_Msg: in VN_Message_Basic;
               Distribute_Route_VN_Msg: out VN_Message_Distribute_Route);

end VN.Message.Distribute_Route;

