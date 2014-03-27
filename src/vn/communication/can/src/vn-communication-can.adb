package body VN.Communication.CAN is

   function "=" (Left : CAN_Address_Sender; Right : CAN_Address_Receiver) return Boolean is
   begin
      return right = left;
   end "=";

   function "=" (Left : CAN_Address_Receiver; Right : CAN_Address_Sender) return Boolean is
      use Interfaces;
      u8Left  : Interfaces.Unsigned_8 := Interfaces.Unsigned_8(Left);
      u8Right : Interfaces.Unsigned_8 := Interfaces.Unsigned_8(Right);
   begin
      return u8Left = u8Right;
   end "=";

   function Convert (x : CAN_Address_Sender) return CAN_Address_Receiver is
      y : integer := Integer(x);
   begin
      return CAN_Address_Receiver(y);
   end Convert;

end VN.Communication.CAN;
