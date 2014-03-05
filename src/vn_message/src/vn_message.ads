with VN_Header;

package VN_Message is

   type VN_Message is tagged private;
   type VN_Footer is tagged private;

   type VN_Checksum is range 0 .. 65535;

   procedure Get_VN_Message (This: out VN_Message);
--
--    function Get_VN_Header(This: in VN_Message) return Positive;
-- procedure Set_VN_Header(This: out VN_Message; Value: in Positive);

   procedure Set_Version(This: in out VN_Message; Version: in Natural);
   function Version(This: in VN_Message) return Natural;

private
   type VN_Message is tagged
      record
         Header : VN_Header.VN_Header;
         Footer : VN_Footer;
      end record;

   type VN_Footer is tagged
      record
         Checksum : VN_Checksum := 0;
      end record;

end VN_Message;
