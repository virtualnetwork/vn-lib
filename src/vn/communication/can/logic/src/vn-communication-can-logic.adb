with Ada.Text_IO;

package body VN.Communication.CAN.Logic is

   function "=" (Left : CAN_Address_Sender; Right : CAN_Address_Receiver) return Boolean is
   begin
      return right = left;
   end "=";

   function "=" (Left : CAN_Address_Receiver; Right : CAN_Address_Sender) return Boolean is
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

   procedure DebugOutput(str : String; level : Integer; newLine : boolean := true) is
   begin
      if level <= GIVE_DEBUG_OUTPUT then
         Ada.Text_IO.Put(str);
         if newLine then
            Ada.Text_IO.New_Line;
         end if;
      end if;
   end DebugOutput;

end VN.Communication.CAN.Logic;
