Pragma Profile (Ravenscar);

with GNAT.IO;
use GNAT.IO;

with VN;
use VN;

with VN.Communication;
with VN.Communication.CAN;
use VN.Communication.CAN;
with VN.Communication.Routing_Table;

with Interfaces;
use Interfaces;

procedure Routing_Table_Test is

   package pack is new VN.Communication.Routing_Table(VN.Communication.CAN.CAN_Address_Sender);
   table : pack.Table_Type(VN.VN_Logical_Address(1000));


   passed : Boolean := true;
 --  canAddressIn, canAddressOut : VN.Communication.CAN.CAN_Address_Sender;
 --  logAddress : VN.VN_Logical_Address;


   function Test(logAddress : VN.VN_Logical_Address;
                 canAddressIn : VN.Communication.CAN.CAN_Address_Sender;
                expectedIndex : Natural) return boolean is
      canAddressOut : VN.Communication.CAN.CAN_Address_Sender;
      found : boolean;
   begin
      pack.Insert(table, logAddress, canAddressIn);
      pack.Search(table, logAddress, canAddressOut, found);
      if canAddressIn /= canAddressOut or not found or pack.NumberOfEntries(table) /= expectedIndex then
         Put_Line("ERROR !!! canAddressIn= " & canAddressIn'img & " canAddressOut= "
                  & canAddressOut'img & " found= " & found'img &
                    " NumberOfEntries= " & pack.NumberOfEntries(table)'img &
                    " expectedIndex= " & expectedIndex'img);
         return false;
      end if;
      return true;
   end Test;


begin

--     passed := passed and Test(13, 13, 1);
--     passed := passed and Test(12033, 12, 2);

   for i in VN.Communication.CAN.CAN_Address_Sender'Range loop
      passed := passed and Test(VN.VN_Logical_Address(i), i, Integer(i) + 1);
   end loop;

   for i in VN.Communication.CAN.CAN_Address_Sender'Range loop
      passed := passed and Test(VN.VN_Logical_Address(i) + 1000, i,
                                Integer(i) + 2 + Integer(VN.Communication.CAN.CAN_Address_Sender'Last));
   end loop;

   for i in VN.Communication.CAN.CAN_Address_Sender'Range loop
      passed := passed and Test(VN.VN_Logical_Address(i) * 100, i,
                                Integer(i) + 3 + Integer(VN.Communication.CAN.CAN_Address_Sender'Last) * 2);
   end loop;

   if passed then
      Put_Line("Test passed");
   else
      Put_Line("Test failed");
   end if;
end Routing_Table_Test;
