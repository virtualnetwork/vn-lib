
pragma Profile(Ravenscar);

with Ada.Real_Time;
use Ada.Real_Time;

with VN;
with VN.Message;
with VN.Communication;
with VN.Communication.CAN;

--with Malloc;

with GNAT.IO;

with VN.Communication.CAN.Logic.SM;

with Protocol_Routing_Test;

procedure Task_Test_Main is


   msg : VN.Message.VN_Message_Basic;
   Status: VN.Send_Status;

   now : Ada.Real_Time.Time;

begin
   now := Ada.Real_Time.Clock;
   delay until now + Ada.Real_Time.Milliseconds(4000);

   msg.Update_Source(5);
   msg.Update_Destination(1337);

   Protocol_Routing_Test.myInterface.Send(msg, Status);

   GNAT.IO.Put_Line("Message sent");

end Task_Test_Main;
