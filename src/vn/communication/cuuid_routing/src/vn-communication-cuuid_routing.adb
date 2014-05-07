-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Simple implementation of routing table. A CUUID is mapped to
-- a generic type of address, for example a CAN address, UDP-port, etc.
-- A better implementation is recommended in the future.

-- ToDo: Only a simple implementation, could be optimized.

with Interfaces;
use Interfaces;

package body VN.Communication.CUUID_Routing is

   function Number_Of_Entries(this : Table_Type) return Natural is
   begin
        return Natural(this.numberOfEntries);
   end Number_Of_Entries;

   procedure Insert(this : in out Table_Type;
                    CUUID : VN.VN_CUUID;
                    Generic_Address : Generic_Address_Type) is

      HASH_TABLE_OVERFOW : exception;
      temp : Element_ptr;
   begin

      temp := CUUID_Hashing.Get(this.myTable, CUUID);

      if temp /= NULL_PTR then -- the CUUID WAS in the has table already
         temp.address := Generic_Address;

      else -- the CUUID was NOT in the has table already
         if this.numberOfEntries = Header_Num'Last then
            raise HASH_TABLE_OVERFOW;

         else

            this.AllEntries(this.numberOfEntries).key     := CUUID;
            this.AllEntries(this.numberOfEntries).address := Generic_Address;

            if this.numberOfEntries = Header_Num'Last - 1 then
               this.AllEntries(this.numberOfEntries).next := this.AllEntries(Header_Num'First)'Unchecked_Access;
            else
               this.AllEntries(this.numberOfEntries).next :=
                 this.AllEntries(this.numberOfEntries + 1)'Unchecked_Access;
            end if;

            CUUID_Hashing.Set(this.myTable, this.AllEntries(this.numberOfEntries)'Unchecked_Access);
            this.numberOfEntries := this.numberOfEntries + 1;
         end if;
      end if;
   end Insert;


   procedure Search(this : in out Table_Type;
                    CUUID : VN.VN_CUUID;
                    Generic_Address : out Generic_Address_Type;
                    found : out Boolean) is
      temp : Element_ptr;
   begin

      temp :=  CUUID_Hashing.Get(this.myTable, CUUID);

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
      function Hash_my_CUUID is new HTable.Hash(Header_Num);

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

end VN.Communication.CUUID_Routing;
