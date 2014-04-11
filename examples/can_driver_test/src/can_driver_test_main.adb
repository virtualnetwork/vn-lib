
pragma Profile (Ravenscar);

with Interfaces.C;
use Interfaces.C;

with Ada.Real_Time;
use Ada.Real_Time;

with GNAT.IO;
use GNAT.IO;

with CAN_Driver;


procedure CAN_Driver_Test_Main is
   now : Ada.Real_Time.Time;

   physMsgSend : aliased CAN_Driver.CAN_Message_Physical;
   physMsgReceive  : aliased CAN_Driver.CAN_Message_Physical;
   t : integer := 0;

   sendStatus : Integer;
begin

   physMsgSend.ID := 1338;
   physMsgSend.Length := 8;

   physMsgSend.Data(0) := 2;
   physMsgSend.Data(1) := 3;
   physMsgSend.Data(2) := 4;
   physMsgSend.Data(3) := 5;
   physMsgSend.Data(4) := 1;
   physMsgSend.Data(5) := 1;
   physMsgSend.Data(6) := 1;
   physMsgSend.Data(7) := 1;

   now := Ada.Real_Time.Clock;
   delay until now + Ada.Real_Time.Milliseconds(4000);

   loop
      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Milliseconds(1000);

   --   t := Integer(CAN_Driver.Test);
   --   GNAT.IO.Put_Line("Test= " & t'img);

--        CAN_Driver.CAN_Get_Msg_Filter_Mask(1, 2, 3);

      sendStatus := Integer(CAN_Driver.Send(physMsgSend'Unchecked_Access));

      GNAT.IO.Put_Line("Message sent sendStatus=" & sendStatus'img);
      GNAT.IO.New_Line;

      if CAN_Driver.Receive(physMsgReceive'Unchecked_Access) = 1 then
         GNAT.IO.Put_Line("Message received");
         GNAT.IO.Put_Line("ID= " & physMsgReceive.ID'Img &
                            "Length= " & physMsgReceive.Length'Img);

         GNAT.IO.Put_Line("Data(0)= " & physMsgReceive.Data(0)'img);
         GNAT.IO.Put_Line("Data(1)= " & physMsgReceive.Data(1)'img);
         GNAT.IO.Put_Line("Data(2)= " & physMsgReceive.Data(2)'img);
         GNAT.IO.Put_Line("Data(3)= " & physMsgReceive.Data(3)'img);
         GNAT.IO.Put_Line("Data(4)= " & physMsgReceive.Data(4)'img);
         GNAT.IO.Put_Line("Data(5)= " & physMsgReceive.Data(5)'img);
         GNAT.IO.Put_Line("Data(6)= " & physMsgReceive.Data(6)'img);
         GNAT.IO.Put_Line("Data(7)= " & physMsgReceive.Data(7)'img);
      end if;
   end loop;

end CAN_Driver_Test_Main;
