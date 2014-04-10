with Interfaces;

package VN.Message is

   -- Enum of different VN_Messages types.
   type VN_Message_Type is (Type_Basic,
                            Type_Local_Hello,
                            Type_Local_Ack,
                            Type_Request_Address_Block,
                            Type_Assign_Address_Block,
                            Type_Assign_Address);
   for VN_Message_Type'Size use 8;

   type VN_Serializiation_Type is (TXT, XML);
   for VN_Serializiation_Type'Size use 8;

   type VN_Component_Type is (CAS, LS, SM_L, SM_x, SM_Gateway, Unknown);
   for VN_Component_Type'Size use 8;

   type VN_Version is mod 2 ** 8;
   for VN_Version'Size use 8;

   type VN_Priority is mod 2 ** 8;
   for VN_Priority'Size use 8;

   type VN_Length is mod 2 ** 16;
   for VN_Length'Size use 16;

   type VN_Flags is mod 2 ** 16;
   for VN_Flags'Size use 16;

   type VN_Opcode is mod 2 ** 8;
   for VN_Opcode'Size use 8;

   type VN_Payload is mod 2 ** 8;
   for VN_Payload'Size use 8;

   type VN_Checksum is mod 2 ** 16;
   for VN_Checksum'Size use 16;

   type VN_Ext_Header_Length is mod 2 ** 8;
   for VN_Ext_Header_Length'Size use 8;

   -- Other VN fields used in multiple derived types
   type VN_Status is mod 2 ** 8;
   for VN_Status'Size use 8;

   type VN_Response_Type is (Valid, Invalid);
   for VN_Response_Type'Size use 8;

   HEADER_SIZE      : constant integer := 17;
   CHECKSUM_SIZE    : constant integer := 2;
   MAX_PAYLOAD_SIZE : constant integer := 1024;

   COMPONENT_TYPE_SIZE     : constant integer := 1;
   CUUID_SIZE              : constant integer := 16;
   STATUS_SIZE             : constant integer := 1;
   RESPONSE_TYPE_SIZE      : constant integer := 1;
   VN_LOGICAL_ADDRESS_SIZE : constant integer := 4;

   type VN_Header is
      record
         Message_Type   : VN_Message_Type := Type_Basic;
         Version        : VN_Version := 16#01#;
         Priority       : VN_Priority;
         Payload_Length : VN_Length;
         Destination    : VN_Logical_Address;
         Source         : VN_Logical_Address;
         Flags          : VN_Flags := 16#0000#;
         Opcode         : VN_Opcode;
         Ext_Header     : VN_Ext_Header_Length := 16#00#;
      end record;

   for VN_Header use record
      Message_Type      at 0 range 128 .. 135;
      Version           at 0 range 120 .. 127;
      Priority          at 0 range 112 .. 119;
      Payload_Length    at 0 range 96 .. 111;
      Destination       at 0 range 64 .. 95;
      Source            at 0 range 32 .. 63;
      Flags             at 0 range 16 .. 31;
      Opcode            at 0 range 8 .. 15;
      Ext_Header        at 0 range 0 .. 7;
   end record;

   type VN_Payload_Byte_Array is array (1 .. MAX_PAYLOAD_SIZE)
                                    of Interfaces.Unsigned_8;

   type VN_Message_Basic is
      record
         Header   : VN_Header;
         Payload  : VN_Payload_Byte_Array;
         Checksum : VN_Checksum;
      end record;

   for VN_Message_Basic use record
      Header        at 0 range (CHECKSUM_SIZE * 8 + MAX_PAYLOAD_SIZE * 8) ..
                               (CHECKSUM_SIZE * 8 +
                                MAX_PAYLOAD_SIZE * 8 +
                                HEADER_SIZE * 8 - 1);
      Payload       at 0 range (CHECKSUM_SIZE * 8) ..
                               (CHECKSUM_SIZE * 8 + MAX_PAYLOAD_SIZE * 8 - 1);
      Checksum      at 0 range 0 .. (CHECKSUM_SIZE * 8 - 1);
   end record;

   for VN_Message_Basic'Alignment use 1;

   type VN_Message_Byte_Array is array (1 .. VN_Message_Basic'Size)
                                          of Interfaces.Unsigned_8;

   procedure Serialize(Message : in VN_Message_Basic;
                       buffer : out VN_Message_Byte_Array);

   procedure Deserialize(Message : out VN_Message_Basic;
                         buffer : in VN_Message_Byte_Array);

   procedure Update_Checksum(Message: in out VN_Message_Basic);

end VN.Message;
