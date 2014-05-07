
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing_Test_Node is a test for the Protocol_Routing package.
-- It resembles a Node on the CAN network.

pragma Profile (Ravenscar);


with Ada.Real_Time;
use Ada.Real_Time;

with Protocol_Routing_Node_Test;


procedure Protocol_Routing_Node_Test_Main is

   now : Ada.Real_Time.Time;
begin

   GNAT.IO.New_Line(2);
   GNAT.IO.Put_Line("Hello world! Protocol_Routing_Node_Test started!");

   loop
      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Seconds(100);
   end loop;
end Protocol_Routing_Node_Test_Main;
