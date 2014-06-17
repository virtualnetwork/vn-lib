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

-- Summary: This is an example that shows how to include the VN-CAN subnet code.


pragma Profile(Ravenscar);

with System;
with System.Machine_Code;
with Ada.Real_Time;
use Ada.Real_Time;

with Task_Test;

with GNAT.IO;

procedure Task_Test_Main is

   t : Ada.Real_Time.Time;

begin

   GNAT.IO.Put_Line("Hello world, Task_Test_Main started!");

   t := Ada.Real_Time.Clock;
   loop

      t := t + Ada.Real_Time.Milliseconds(1000);
      delay until t;
   end loop;
end Task_Test_Main;
