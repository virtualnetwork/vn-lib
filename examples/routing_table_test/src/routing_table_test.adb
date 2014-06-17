------------------------------------------------------------------------------
--  This file is part of VN-Lib.
--
--  VN-Lib is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  VN-Lib is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with VN-Lib.  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright 2014, Nils Brynedal Ignell (nils.brynedal@gmail.com)
------------------------------------------------------------------------------

-- Summary:
-- Test of the routing table.


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

--  with System.BB.Interrupts; -- Remove when compiling for PC, keep when compiling for SmartFusion2

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

   type Protocol_Address_Type is (CAN_Subnet, Application_Layer); -- for testing
   package pack2 is new VN.Communication.Routing_Table(Protocol_Address_Type);
   table2 : pack2.Table_Type(VN.VN_Logical_Address(500));-- for testing
   found : boolean;
   address : Protocol_Address_Type;
begin

   GNAT.IO.New_Line(2);
   GNAT.IO.Put_Line("Routing table test started");

   pack2.Insert(Table2, 1337, CAN_Subnet);
   pack2.Search(Table2, 1337, address, found);
   if found and then address = CAN_Subnet then
      Put_Line("Minitest passed");
   else
      Put_Line("Minitest failed");
   end if;

   pack2.Search(Table2, 1336, address, found);
   if found then
      Put_Line("Extra test failed");
   end if;

   pack2.Search(Table2, 16, address, found);
   if found then
      Put_Line("Extra test failed");
   end if;

   pack2.Search(Table2, 11, address, found);
   if found then
      Put_Line("Extra test failed");
   end if;

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
