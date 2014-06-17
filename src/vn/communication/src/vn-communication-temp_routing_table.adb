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

package body VN.Communication.Temp_Routing_Table is

   function NumberOfEntries(this : in Table_Type) return Natural is
   begin
      return this.count;
   end NumberOfEntries;

   procedure Insert(this : in out Table_Type;
                    Logical_Address : VN.VN_Logical_Address;
                    Generic_Address : Generic_Address_Type) is

      ROUTING_TABLE_OVERFLOW : exception;
      index : VN.VN_Logical_Address := Logical_Address rem this.Capacity;
   begin
      for i in index..this.Values'Last loop
         if not this.Values(i).isUsed or else
           this.Values(i).Logical_Address = Logical_Address then

            this.Values(i).isUsed := true;
            this.Values(i).Logical_Address := Logical_Address;
            this.Values(i).Generic_Address := Generic_Address;
            this.count := this.count + 1;

            return;
         end if;
      end loop;

      raise ROUTING_TABLE_OVERFLOW;
   end Insert;

   procedure Search(this : in Table_Type;
                    Logical_Address : VN.VN_Logical_Address;
                    Generic_Address : out Generic_Address_Type;
                    found : out Boolean) is

      index : VN.VN_Logical_Address := Logical_Address rem this.Capacity;
   begin
      for i in index..this.Values'Last loop
         if this.Values(i).isUsed then
            if this.Values(i).Logical_Address = Logical_Address then
               Generic_Address := this.Values(i).Generic_Address;
               found := true;

               return;
            end if;
         end if;
      end loop;
      found := false;
   end Search;

end VN.Communication.Temp_Routing_Table;
