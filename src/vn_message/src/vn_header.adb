package body VN_Header is

   function Version(This: in VN_Header) return Natural is
   begin
      return This.Version;
   end Version;

   procedure Set_Version(This: in out VN_Header; Version: in Natural) is
   begin
      This.Version := Version;
   end Set_Version;

end VN_Header;
