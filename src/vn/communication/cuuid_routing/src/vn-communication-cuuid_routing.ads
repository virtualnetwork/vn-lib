-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Simple implementation of routing table. A CUUID is mapped to 
-- a generic type of address, for example a CAN address, UDP-port, etc.
-- A better implementation is recommended in the future.

-- ToDo: Only a simple implementation, could be optimized.


with HTable;
with VN;

generic
   type Generic_Address_Type is private; -- e.g. CAN address, UDP-port, etc.

package VN.Communication.CUUID_Routing is
   pragma Elaborate_Body;

   type Table_Type is private;

   function Number_Of_Entries(this : Table_Type) return Natural;

   procedure Insert(this : in out Table_Type;
                    CUUID : VN.VN_CUUID;
                    Generic_Address : Generic_Address_Type);

   procedure Search(this : in out Table_Type;
                    CUUID : VN.VN_CUUID;
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

   package CUUID_Hashing is new HTable.Static_HTable(Header_Num => Header_Num, Element => Element,
                                                     Elmt_Ptr => Element_ptr, Null_Ptr => NULL_PTR,
                                                     Set_Next => Set_Next, Next => Next, Key => VN.VN_CUUID,
                                                     Get_Key => Get_Key, Hash => Hash, Equal => "=");

   type Entry_Array is array(Header_Num) of aliased Element;   

   type Table_Type is 
      record
         myTable : CUUID_Hashing.Table;
         AllEntries : Entry_Array;
         numberOfEntries : Header_Num := 0;
      end record;

end VN.Communication.CUUID_Routing;
