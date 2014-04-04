
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: 
-- VN.Communication.CAN is the subnet specific layer for CAN. It
-- resides under the Protocol_Routing layer. The VN.Communication.CAN 
-- package will decide to which CAN address to send a VN message sent by 
-- the Protocol_Routing layer.
-- All messages received over CAN will be delivered to the 
-- Protocol_Routing layer.

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
