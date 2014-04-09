
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:


pragma Profile(Ravenscar);

with System;
with System.Machine_Code;
with Ada.Real_Time;
use Ada.Real_Time;

--  with Task_Test;

with Test_No_Tasks;

with GNAT.IO;

procedure Task_Test_Main is
   pragma Priority (3);

   t : Ada.Real_Time.Time;

begin


--     t := Ada.Real_Time.Clock;
--     delay until t + Ada.Real_Time.Seconds(7);

   GNAT.IO.Put_Line("Hello world, Task_Test_Main started! Doing hard coded test");

   t := Ada.Real_Time.Clock;
   loop



      t := t + Ada.Real_Time.Milliseconds(1000);
      -- delay until t;
      while Clock < t loop
         System.Machine_Code.Asm ("wfe", Volatile => True);
         null;
      end loop;

      Test_No_Tasks.Update;

   end loop;
end Task_Test_Main;
