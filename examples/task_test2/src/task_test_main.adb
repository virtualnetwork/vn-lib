
pragma Profile(Ravenscar);

with System;
with Ada.Real_Time;
use Ada.Real_Time;

with Task_Test;

with GNAT.IO;

procedure Task_Test_Main is
   pragma Priority (1);

   t : Ada.Real_Time.Time;
begin

   t := Ada.Real_Time.Clock;
   loop

      GNAT.IO.Put_Line("Hello world");

      t := t + Ada.Real_Time.Milliseconds(1000);
      delay until t;
   end loop;
end Task_Test_Main;
