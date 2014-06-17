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
-- Simple implementation of routing table. A logical address is mapped to 
-- a generic type of address, for example a CAN address, UDP-port, etc.
-- A better implementation is recommended in the future.

-- ToDo: Only a simple implementation, could be optimized.
-- ToDo: This implementation only handles one route between to points.

with Interfaces;
use Interfaces;

package body VN.Communication.Routing_Table is

   function NumberOfEntries(this : in Table_Type) return Natural is
   begin
      return this.count;
   end NumberOfEntries;

   procedure Insert(this : in out Table_Type;
                    Logical_Address : VN.VN_Logical_Address;
                    Generic_Address : Generic_Address_Type;
                    isDirect 	    : Boolean := false) is

      ROUTING_TABLE_OVERFLOW : exception;
      index : VN.VN_Logical_Address := Logical_Address rem this.Capacity;
   begin
      for i in index..this.Values'Last loop
         if not this.Values(i).isUsed or else
           (this.Values(i).Logical_Address = Logical_Address and (isDirect or not this.Values(i).isDirect)) then

            this.Values(i).isUsed := true;
            this.Values(i).Logical_Address := Logical_Address;
            this.Values(i).Generic_Address := Generic_Address;
            this.Values(i).isDirect 	   := isDirect;
            this.count := this.count + 1;

            return;
         elsif this.Values(i).Logical_Address = Logical_Address and (not isDirect or this.Values(i).isDirect) then
            return;
         end if;
      end loop;

      VN.Text_IO.Put_Line("ROUTING_TABLE_OVERFLOW"); 
      loop
         null;
      end loop;

      raise ROUTING_TABLE_OVERFLOW;
   end Insert;

   procedure Search(this : in Table_Type;
                    Logical_Address : VN.VN_Logical_Address;
                    Generic_Address : out Generic_Address_Type;
                    found    : out Boolean;
                    isDirect : access Boolean := null) is

      index : VN.VN_Logical_Address := Logical_Address rem this.Capacity;
   begin
      found := false;
      for i in index..this.Values'Last loop
         if this.Values(i).isUsed then
            if this.Values(i).Logical_Address = Logical_Address then
               Generic_Address := this.Values(i).Generic_Address;
               found    := true;

               if isDirect /= null then
                  isDirect.all := this.Values(i).isDirect;
               end if;

               return;
            end if;
         else 
            return;
         end if;
      end loop;
   end Search;

end VN.Communication.Routing_Table;
