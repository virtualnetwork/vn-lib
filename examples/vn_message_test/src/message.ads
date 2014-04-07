with Interfaces;

package Message is

   -- Enum of different VN_Messages types.
   type VN_Message_Type is (Type_Basic, Type_Local_Hello, Type_Local_Ack);
   for VN_Message_Type'Size use 8;

   type VN_Serializiation_Types is (TXT, XML);
   type VN_Component_Types is (CAS, LS, SM_L, SM_x, SM_Gateway, Unknown);
   
   -- VN_Header parts
   type VN_Logical_Address is mod 2 ** 32; -- MOVED TO VN.ADS!!!
   for VN_Logical_Address'Size use 32;

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

   type VN_Header(Payload_Size : VN_Length) is private;

   -- Other VN fields used in multiple derived types
   type VN_Status is mod 2 ** 8;
   for VN_Status'Size use 8;

   type VN_Message_Basic(Payload_Size : VN_Length) is tagged private;

   type Buffer_Array_Type is array (VN_Length range <>) of Interfaces.Unsigned_8;

   procedure Serialize(Message : in VN_Message_Basic; buffer : out Buffer_Array_Type);

   procedure DeSerialize(Message : out VN_Message_Basic; buffer : in Buffer_Array_Type);

private 

   type VN_Header(Payload_Size : VN_Length) is
      record
         -- Extended Header not implemented.
         Message_Type   : VN_Message_Type := Type_Basic;
         Version        : VN_Version := 1;
         Priority       : VN_Priority;
         Payload_Length : VN_Length := Payload_Size;
         Destination    : VN_Logical_Address;
         Source         : VN_Logical_Address;
         Flags          : VN_Flags := 0;
         Opcode         : VN_Opcode;
         Ext_Header	: VN_Ext_Header_Length := 0; --Value          : Positive := 1;????????
      end record;

   for VN_Header use record
      Message_Type 	at 0 range 128 .. 135;
      Version 		at 0 range 120 .. 127;
      Priority 		at 0 range 112 .. 119;
      Payload_Length 	at 0 range 96 .. 111;
      Destination 	at 0 range 64 .. 95;
      Source 		at 0 range 32 .. 63;
      Flags 		at 0 range 16 .. 31;
      Opcode 		at 0 range 8 .. 15;
      Ext_Header 	at 0 range 0 .. 7;
   end record;


   type VN_Message_Basic(Payload_Size : VN_Length) is tagged --new Ada.Finalization.Controlled with
      record
         Header   : VN_Header(Payload_Size);
         Payload  : Buffer_Array_Type(VN_Length(1)..Payload_Size);
         Checksum : VN_Checksum;
      end record;

   pragma Pack(VN_Message_Basic);
   
end Message;

