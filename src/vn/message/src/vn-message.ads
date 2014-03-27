with Ada.Finalization;

package VN.Message is

   -- Enum of different VN_Messages types.
   type VN_Message_Type is (Type_Basic, Type_Local_Hello, Type_Local_Ack);

   type VN_Serializiation_Types is (TXT, XML);
   type VN_Component_Types is (CAS, LS, SM_L, SM_x, SM_Gateway, Unknown);

   type VN_Header is private;
   type VN_Payload is mod 2 ** 8;
   type VN_Checksum is mod 2 ** 16;

   -- VN_Header parts
 --  type VN_Logical_Address is mod 2 ** 32; -- MOVED TO VN.ADS!!!
   type VN_Version is mod 2 ** 8;
   type VN_Priority is mod 2 ** 8;
   type VN_Length is mod 2 ** 16;
   type VN_Flags is mod 2 ** 16;
   type VN_Opcode is mod 2 ** 8;

   -- Other VN fields used in multiple derived types
   type VN_Status is mod 2 ** 8;

   -- VN_Payload parts in derived types of VN_Mesage
  -- type VN_CUUID is mod 2 ** 64; -- FIX: Should be 128 bits.  -- MOVED TO VN.ADS!!!
   -- type VN_CUUID is mod 2 ** 128; -- TODO: How to represent 128 bits properly. 
  -- type VN_Component_Type is mod 2 ** 8;  

   -- Communication types
--   type Send_Status is (OK, -- MOVED TO VN.ADS!!!
--                        ERROR_UNKNOWN,
--                        ERROR_BUFFFER_OVERFLOW,
--                        ERROR_NO_ADDRESS_RECEIVED);

--   type Receive_Status is (OK, -- MOVED TO VN.ADS!!!
--                           ERROR_UNKNOWN);

   -- VN_Message
   type VN_Message_Basic is tagged private;

   -- VN_Version
   function Get_Version(Message: VN_Message_Basic) return VN_Version;
   procedure Set_Version(Message: out VN_Message_Basic; Version: VN_Version);

   -- VN_Checksum
   function Get_Checksum(Message: VN_Message_Basic) return VN_Checksum;
   procedure Update_Checksum(Message: in out VN_Message_Basic);

private
   type VN_Message_Basic is new Ada.Finalization.Controlled with
      record
         Header   : VN_Header;
         Checksum : VN_Checksum;
      end record;

   type VN_Header is
      record
         -- Extended Header not implemented.
         Message_Type   : VN_Message_Type := Type_Basic;
         Version        : VN_Version := 1;
         Priority       : VN_Priority;
         Payload_Length : VN_Length;
         Destination    : VN_Logical_Address;
         Source         : VN_Logical_Address;
         Flags          : VN_Flags := 0;
         Opcode         : VN_Opcode;
         Value          : Positive := 1;
      end record;

   overriding
   procedure Initialize(This: in out VN_Message_Basic) is null;

   overriding
   procedure Finalize(This: in out VN_Message_Basic) is null;

end VN.Message;
