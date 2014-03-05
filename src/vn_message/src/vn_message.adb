with VN_Header;

package body VN_Message is

   procedure Get_VN_Message (This: out VN_Message) is
      VN_Msg : VN_Message := This;
   begin
      null;
   end Get_VN_Message;

   function Version(This: in VN_Message) return Natural is
   begin
      return This.Header.Version;
   end Version;

   procedure Set_Version(This: in out VN_Message; Version: in Natural) is
   begin
      This.Header.Set_Version(Version);
   end Set_Version;

--   procedure Set_VN_Header(This: out VN_Message; Value: in Positive) is
--   begin
--      Set_Header_Value(This.Header, Value);
--   end Set_VN_Header;
--
--   procedure Set_Header_Value(This: out VN_Header; Value: in Positive) is
--   begin
--      This.Value := Value;
--   end Set_Header_Value;

end VN_Message;

