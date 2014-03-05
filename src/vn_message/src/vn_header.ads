package VN_Header is

   type VN_Header is tagged private;

   type VN_Logical_Address is range 0 .. 31;

   type VN_Header_Version is range 0 .. 7;
   type VN_Header_Priority is range 0 .. 7;
   type VN_Header_Length is range 0 .. 15;
   type VN_Header_Flags is range 0 .. 15;
   type VN_Header_Opcode is range 0 .. 7;

   function Version(This: in VN_Header) return Natural;

   procedure Set_Version(This: in out VN_Header; Version: in Natural);

private
   type VN_Header is tagged
      record
         -- Version        : VN_Header_Version := 1; -- Temporary commented
         -- away.
         Version        : Natural := 1;
         Priority       : VN_Header_Priority;
         Payload_Length : VN_Header_Length;
         Destination    : VN_Logical_Address;
         Source         : VN_Logical_Address;
         Flags          : VN_Header_Flags := 0;
         Opcode         : VN_Header_Opcode;
         Value          : Positive := 1;
         -- Extended Header not implemented.
      end record;

end VN_Header;
