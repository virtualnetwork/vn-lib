
pragma Profile (Ravenscar);


with Ada.Real_Time;
use Ada.Real_Time;

with GNAT.IO;
use GNAT.IO;

with VN.Message;

with Protocol_Routing_Test;


procedure Protocol_Routing_Test_Main is


   msg : VN.Message.VN_Message_Basic;
   Status: VN.Send_Status;

   now : Ada.Real_Time.Time;

begin

   GNAT.IO.New_Line(2);
   GNAT.IO.Put_Line("Hello world! Protocol_Routing_Test started!");

  -- now := Ada.Real_Time.Clock;
  -- delay until now + Ada.Real_Time.Milliseconds(4000);

 --  GNAT.IO.Put_Line("4 second wait ended.");

   msg.Header.Source := 5;
   msg.Header.Destination := 1337;

   Protocol_Routing_Test.myInterface.Send(msg, Status);

   GNAT.IO.Put_Line("VN message written to send buffer");

   GNAT.IO.Put_Line("Main function entering infinte wait.");
   loop
      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Milliseconds(1000);
      GNAT.IO.Put_Line("<Main function hearbeat>");
   end loop;
end Protocol_Routing_Test_Main;
