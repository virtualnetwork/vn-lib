
pragma Profile (Ravenscar);

with GNAT.IO;
use GNAT.IO;

with VN.Message;

with Protocol_Routing_Test;


procedure Protocol_Routing_Test_Main is


   msg : VN.Message.VN_Message_Basic;
   Status: VN.Send_Status;
begin

   msg.Update_Source(5);
   msg.Update_Destination(7);

   Protocol_Routing_Test.myInterface.Send(msg, Status);


end Protocol_Routing_Test_Main;
