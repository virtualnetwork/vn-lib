
pragma Profile(Ravenscar);

with System;
with Ada.Real_Time;

with Task_Test;

with GNAT.IO;

procedure Task_Test_Main is


begin

   GNAT.IO.Put_Line("Hello world");
   null;
end Task_Test_Main;
