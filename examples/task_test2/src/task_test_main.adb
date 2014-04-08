
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:


pragma Profile(Ravenscar);

with System;
with Ada.Real_Time;
use Ada.Real_Time;

with Task_Test;

with GNAT.IO;

procedure Task_Test_Main is
   pragma Priority (7);

   t : Ada.Real_Time.Time;
begin


--     t := Ada.Real_Time.Clock;
--     delay until t + Ada.Real_Time.Seconds(7);

   GNAT.IO.Put_Line("Hello world, Task_Test_Main started!");

   t := Ada.Real_Time.Clock;
   loop

      GNAT.IO.Put_Line("Task_Test_Main running!");

      t := t + Ada.Real_Time.Milliseconds(1000);
      delay until t;
   end loop;
end Task_Test_Main;
