with Interfaces;

package Message is



   -- Enum of different VN_Messages types.
   type VN_Message_Type is (Type_Basic, Type_Local_Hello, Type_Local_Ack);

   type VN_Serializiation_Types is (TXT, XML);
   type VN_Component_Types is (CAS, LS, SM_L, SM_x, SM_Gateway, Unknown);


   

   -- VN_Header parts
   type VN_Logical_Address is mod 2 ** 32; -- MOVED TO VN.ADS!!!
   type VN_Version is mod 2 ** 8;
   type VN_Priority is mod 2 ** 8;
   type VN_Length is mod 2 ** 16;
   type VN_Flags is mod 2 ** 16;
   type VN_Opcode is mod 2 ** 8;

   type VN_Payload is mod 2 ** 8;
   type VN_Checksum is mod 2 ** 16;
   type VN_Header(Payload_Size : VN_Length) is private;

   -- Other VN fields used in multiple derived types
   type VN_Status is mod 2 ** 8;

   type VN_Message_Basic(Payload_Size : VN_Length) is tagged limited private;

   type Buffer_Array_Type is array (VN_Length range <>) of Interfaces.Unsigned_8;

   procedure Serialize(Message : in VN_Message_Basic; buffer : in out Buffer_Array_Type);

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
         Value          : Positive := 1;
      end record;

   type VN_Message_Basic(Payload_Size : VN_Length) is tagged limited --new Ada.Finalization.Controlled with
      record
         Header   : VN_Header(Payload_Size);
         payload  : Buffer_Array_Type(VN_Length(1)..Payload_Size);
         Checksum : VN_Checksum;
      end record;

end Message;

