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
--  Copyright 2014 Nils Brynedal Ignell (nils.brynedal@gmail.com)
------------------------------------------------------------------------------

-- ToDo: Only a simple implementation, could be optimized.


with System.HTable;
with VN;

generic
   type Generic_Address_Type is private; -- e.g. CAN address, UDP-port, etc.

package VN.Communication.Temp_CUUID_Routing is
   pragma Elaborate_Body;

   function Number_Of_Entries return Natural;

   procedure Insert(CUUID : VN.VN_CUUID;
                    Generic_Address : Generic_Address_Type);

   procedure Search(CUUID : VN.VN_CUUID;
                    Generic_Address : out Generic_Address_Type;
                    found : out Boolean);


private

   LENGTH : constant integer := 256; --should not be more than 2^32-1

   type Header_Num is range 0..LENGTH-1;

   procedure CUUID_To_String(cuuid : in VN.VN_CUUID; str : out String);

   function Hash_CUUID(cuuid : in VN.VN_CUUID) return Header_Num;

   type Element;

   type Element_ptr is access all Element;

   NULL_PTR : constant Element_ptr := null;

   type Element is
      record
         key 	 : VN.VN_CUUID;
         address : Generic_Address_Type;
         next    : Element_ptr;
      end record;

   procedure Set_Next (E : Element_ptr; Next : Element_ptr);
   function  Next     (E : Element_ptr) return Element_ptr;

   function Get_Key (E : Element_ptr) 		return VN.VN_CUUID;
   function Hash    (F : VN.VN_CUUID)      	return Header_Num;

   package CUUID_Hashing is new System.HTable.Static_HTable(Header_Num => Header_Num, Element => Element,
                                                            Elmt_Ptr => Element_ptr, Null_Ptr => NULL_PTR,
                                                            Set_Next => Set_Next, Next => Next, Key => VN.VN_CUUID,
                                                            Get_Key => Get_Key, Hash => Hash, Equal => "=");

   numberOfEntries : Header_Num := 0;

   type Entry_Array is array(Header_Num) of aliased Element;
   AllEntries : Entry_Array;

end VN.Communication.Temp_CUUID_Routing;
