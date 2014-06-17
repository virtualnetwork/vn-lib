
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: This is an old test of Ada Tasks. May not work.


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
