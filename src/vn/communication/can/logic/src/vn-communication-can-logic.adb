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
-- VN.Communication.CAN.Logic is a package that implements the logic 
-- of the VN-CAN protocol itself. 

with VN;

package body VN.Communication.CAN.Logic is

   procedure DebugOutput(str : String; level : Integer; newLine : boolean := true) is
   begin
      if level <= GIVE_DEBUG_OUTPUT then
         VN.Text_IO.Put(str);
         if newLine then
            VN.Text_IO.New_Line;
         end if;
      end if;
   end DebugOutput;
   
end VN.Communication.CAN.Logic;
