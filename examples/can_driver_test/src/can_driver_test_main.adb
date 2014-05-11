
pragma Profile (Ravenscar);

with Interfaces.C;
use Interfaces.C;

with Ada.Real_Time;
use Ada.Real_Time;

with GNAT.IO;
use GNAT.IO;

with VN;
use VN;

with VN.Communication.CAN.CAN_Driver;
with CAN_Driver_Test;

with VN.Communication.CAN.Logic;

procedure CAN_Driver_Test_Main is
   now : Ada.Real_Time.Time;

   physMsgSend : aliased VN.Communication.CAN.CAN_Driver.CAN_Message_Physical;
   physMsgReceive  : aliased VN.Communication.CAN.CAN_Driver.CAN_Message_Physical;
   t : integer := 0;

   sendStatus : Integer;
   x : Interfaces.C.signed_char;
   y : Interfaces.Unsigned_8 := 6;

   logMsgSend : aliased VN.Communication.CAN.CAN_Message_Logical;
   logMsgReceive  : aliased VN.Communication.CAN.CAN_Message_Logical;

   logSendStatus : VN.Send_Status;
   logReceiveStatus : VN.Receive_Status;

   procedure Test is
      isSender : Boolean := true;

   begin

      if isSender then
         loop

         end loop;
      else
         loop

         end loop;
      end if;
   end Test;

begin

   Test;

--     loop
--        now := Ada.Real_Time.Clock;
--        delay until now + Ada.Real_Time.Milliseconds(1000);
--        GNAT.IO.Put_Line("Program in ENVM started!");
--     end loop;

--     CAN_Driver_Test.Init;

   physMsgSend.ID := 138;
   physMsgSend.Length := 8;

   x := 5;

   physMsgSend.Data(0) := x;
   physMsgSend.Data(1) := x;
   physMsgSend.Data(2) := x;
   physMsgSend.Data(3) := x;
   physMsgSend.Data(4) := x;
   physMsgSend.Data(5) := x;
   physMsgSend.Data(6) := x;
   physMsgSend.Data(7) := x;



   logMsgSend.isNormal := true;
   logMsgSend.msgType := VN.Communication.CAN.Logic.FLOW_CONTROL;
   logMsgSend.Receiver := VN.Communication.CAN.CAN_Address_Receiver(6);
   logMsgSend.Sender := VN.Communication.CAN.CAN_Address_Sender(32);
   logMsgSend.Length := VN.Communication.CAN.DLC_Type(8);


   logMsgSend.Data(1) := y;
   logMsgSend.Data(2) := y;
   logMsgSend.Data(3) := y;
   logMsgSend.Data(4) := y;
   logMsgSend.Data(5) := y;
   logMsgSend.Data(6) := y;
   logMsgSend.Data(7) := y;
   logMsgSend.Data(8) := y;

   now := Ada.Real_Time.Clock;
   delay until now + Ada.Real_Time.Milliseconds(4000);



   GNAT.IO.Put_Line("NEW TEST");
   GNAT.IO.New_Line;


   loop
--        t := Integer(VN.Communication.CAN.CAN_Driver.Test);
--        GNAT.IO.Put_Line("Test= " & t'img);
--
--        sendStatus := Integer(VN.Communication.CAN.CAN_Driver.SendPhysical(physMsgSend'Unchecked_Access));
--        GNAT.IO.Put_Line("Message sent sendStatus=" & sendStatus'img);
--
--  --        VN.Communication.CAN.CAN_Driver.Test_CAN_Send;
--  --        GNAT.IO.Put_Line("Test_CAN_Send");
--
--        GNAT.IO.New_Line;
--
--        if VN.Communication.CAN.CAN_Driver.ReceivePhysical(physMsgReceive'Unchecked_Access) = 1 then
--           GNAT.IO.Put_Line("Message received");
--           GNAT.IO.Put_Line("ID= " & physMsgReceive.ID'Img &
--                              "Length= " & physMsgReceive.Length'Img);
--
--           GNAT.IO.Put_Line("Data(0)= " & physMsgReceive.Data(0)'img);
--           GNAT.IO.Put_Line("Data(1)= " & physMsgReceive.Data(1)'img);
--           GNAT.IO.Put_Line("Data(2)= " & physMsgReceive.Data(2)'img);
--           GNAT.IO.Put_Line("Data(3)= " & physMsgReceive.Data(3)'img);
--           GNAT.IO.Put_Line("Data(4)= " & physMsgReceive.Data(4)'img);
--           GNAT.IO.Put_Line("Data(5)= " & physMsgReceive.Data(5)'img);
--           GNAT.IO.Put_Line("Data(6)= " & physMsgReceive.Data(6)'img);
--           GNAT.IO.Put_Line("Data(7)= " & physMsgReceive.Data(7)'img);
--        end if;

      VN.Communication.CAN.CAN_Driver.Send(logMsgSend, logSendStatus);
      if logSendStatus = VN.OK then
         GNAT.IO.Put_Line("Message sent sendStatus= OK");
      else
         GNAT.IO.Put_Line("Message sent ERROR");
      end if;

      VN.Communication.CAN.CAN_Driver.Receive(logMsgReceive, logReceiveStatus);

      if logReceiveStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or logReceiveStatus = VN.MSG_RECEIVED_MORE_AVAILABLE then
         GNAT.IO.Put_Line("Message received");
         GNAT.IO.Put_Line(" isNormal= " & logMsgReceive.isNormal'Img &
                            " msgType= " & logMsgReceive.msgType'Img &
                            " Receiver= " & logMsgReceive.Receiver'Img &
                            " Sender= " & logMsgReceive.Sender'Img &
                            " Length= " & logMsgReceive.Length'Img);
         for i in 1 .. logMsgReceive.Length loop
            GNAT.IO.Put(logMsgReceive.Data(i)'Img);
         end loop;
         GNAT.IO.New_Line(2);
      end if;

      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Milliseconds(1000);
   end loop;

end CAN_Driver_Test_Main;
