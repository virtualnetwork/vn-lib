
with GNAT.IO;
use GNAT.IO;

with VN.Communication;
with VN.Communication.CAN;
use VN.Communication.CAN;
with VN.Communication.Routing_Table;

procedure Routing_Table_Test is

   package pack is new VN.Communication.Routing_Table(VN.Communication.CAN.CAN_Address_Sender);
   table : pack.Table_Type(VN.VN_Logical_Address(1000));

   found : boolean;

   canAddressIn, canAddressOut : VN.Communication.CAN.CAN_Address_Sender;
   logAddress : VN.VN_Logical_Address;
begin

   canAddressIn := 13;
   logAddress := 13;
   pack.Insert(table, logAddress, canAddressIn);
   pack.Search(table, logAddress, canAddressOut, found);

   if canAddressIn /= canAddressOut or not found then
      Put_Line("ERROR !!! canAddressIn= " & canAddressIn'img & " canAddressOut= " & canAddressOut'img & " found= " & found'img);
   end if;
--   Put_Line("canAddress= " & canAddress'img & " found= " & found'img);

   canAddressIn := 120;
   logAddress := 12033;
   pack.Insert(table, logAddress, canAddressIn);
   pack.Search(table, logAddress, canAddressOut, found);

   if canAddressIn /= canAddressOut or not found then
      Put_Line("ERROR !!! canAddressIn= " & canAddressIn'img & " canAddressOut= " & canAddressOut'img & " found= " & found'img);
   end if;

   canAddressIn := 0;
   logAddress := 234;
   pack.Insert(table, logAddress, canAddressIn);
   pack.Search(table, logAddress, canAddressOut, found);

   if canAddressIn /= canAddressOut or not found then
      Put_Line("ERROR !!! canAddressIn= " & canAddressIn'img & " canAddressOut= " & canAddressOut'img & " found= " & found'img);
   end if;

   Put_Line("Test completed");
end Routing_Table_Test;
