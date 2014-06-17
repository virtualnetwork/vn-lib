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

with Interfaces;
use Interfaces;

package body VN.Communication.Temp_CUUID_Routing is

   function Number_Of_Entries return Natural is
   begin
        return Natural(numberOfEntries);
   end Number_Of_Entries;

   procedure Insert(CUUID : VN.VN_CUUID;
                    Generic_Address : Generic_Address_Type) is

      HASH_TABLE_OVERFOW : exception;
      temp : Element_ptr;
   begin

      temp := CUUID_Hashing.Get(CUUID);

      if temp /= NULL_PTR then -- the CUUID WAS in the has table already
         temp.address := Generic_Address;

      else -- the CUUID was NOT in the has table already
         if numberOfEntries = Header_Num'Last then
            raise HASH_TABLE_OVERFOW;

         else

            AllEntries(numberOfEntries).key 	:= CUUID;
            AllEntries(numberOfEntries).address := Generic_Address;

            if numberOfEntries = Header_Num'Last - 1 then
               AllEntries(numberOfEntries).next := AllEntries(Header_Num'First)'Access;
            else
               AllEntries(numberOfEntries).next := AllEntries(numberOfEntries + 1)'Access;
            end if;

            CUUID_Hashing.Set(AllEntries(numberOfEntries)'Access);
            numberOfEntries := numberOfEntries + 1;
         end if;
      end if;
   end Insert;


   procedure Search(CUUID : VN.VN_CUUID;
                    Generic_Address : out Generic_Address_Type;
                    found : out Boolean) is
      temp : Element_ptr;
   begin
      temp := CUUID_Hashing.Get(CUUID);

      if temp = NULL_PTR then
         found := false;
      else
         found := true;
         Generic_Address := temp.address;
      end if;
   end Search;


   procedure CUUID_To_String(cuuid : in VN.VN_CUUID; str : out String) is
      u8 : Interfaces.Unsigned_8;
      c : Character;
      for c'Address use u8'Address;
   begin
      for i in 0..15 loop
         u8 := cuuid(cuuid'First + i);
         str(str'First + i) := c;
      end loop;
   end CUUID_To_String;

   function Hash_CUUID(cuuid : in VN.VN_CUUID) return Header_Num is
      function Hash_my_CUUID is new System.HTable.Hash(Header_Num);

      str : String(1 .. 16);
   begin
      CUUID_To_String(cuuid, str);
      return Hash_my_CUUID(str);
   end Hash_CUUID;

   procedure Set_Next (E : Element_ptr; Next : Element_ptr) is
   begin
      E.next := Next;
   end Set_Next;

   function  Next (E : Element_ptr) return Element_ptr is
   begin
      return E.next;
   end Next;

   function Get_Key (E : Element_ptr) return VN.VN_CUUID is
   begin
      return E.key;
   end Get_Key;

   function Hash(F : VN.VN_CUUID) return Header_Num is
      type Unsigned_32_Array is array(1..4) of Interfaces.Unsigned_32;
      arr : Unsigned_32_Array;
      for arr'Address use F'Address;
      for arr'Alignment use 4;
      pragma Warnings(Off, arr);

      total : Interfaces.Unsigned_32 := 0;
   begin
      for i in arr'Range loop
         total := total + arr(i);
      end loop;
      total := total mod Interfaces.Unsigned_32(Header_Num'Last);

      return Header_Num(total);
   end Hash;

end VN.Communication.Temp_CUUID_Routing;
