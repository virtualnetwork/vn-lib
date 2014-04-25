
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing_Test_Main is a test for the Protocol_Routing package.

pragma Profile (Ravenscar);


with Ada.Real_Time;
use Ada.Real_Time;

with GNAT.IO;
use GNAT.IO;

with VN.Message;
with VN.Message.Assign_Address;
with VN.Message.Request_Address_Block;
with VN.Message.Factory;

with Protocol_Routing_Test;


procedure Protocol_Routing_Test_Main is


   msg	     : VN.Message.VN_Message_Basic;
   msgAssign : VN.Message.Assign_Address.VN_Message_Assign_Address;
   msgReq    : VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;
   Status    : VN.Send_Status;

   now : Ada.Real_Time.Time;

   use VN; -- for if Status = VN.OK then
begin

   GNAT.IO.New_Line(2);
   GNAT.IO.Put_Line("Hello world! Protocol_Routing_Test started!");

   now := Ada.Real_Time.Clock;
   delay until now + Ada.Real_Time.Milliseconds(5000);

   GNAT.IO.Put_Line("5 second wait ended.");

   VN.Message.Assign_Address.To_Assign_Address
     (VN.Message.Factory.Create(VN.Message.Type_Assign_Address), msgAssign);

   VN.Message.Request_Address_Block.To_Request_Address_Block
     (VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block), msgReq);

   msgReq.Header.Source := 5;
   msgReq.Header.Destination := 1337;
   msgReq.CUUID := (others => 43);

   msgAssign.Header.Source := 5;
   msgAssign.Header.Destination := 2;
   msgAssign.CUUID := (others => 42);
   msgAssign.Assigned_Address := 1;

--  -------------------

   VN.Message.Assign_Address.To_Basic(msgAssign, msg);
   Protocol_Routing_Test.myInterface.Send(msg, Status);

   GNAT.IO.Put("VN message written to send buffer");
   if Status = VN.OK then
      GNAT.IO.Put(", Status = VN.OK");

   elsif Status = VN.ERROR_BUFFER_FULL then
      GNAT.IO.Put(", Status = VN.ERROR_BUFFER_FULL");

   elsif Status = VN.ERROR_NO_ADDRESS_RECEIVED then
      GNAT.IO.Put(", Status = VN.ERROR_NO_ADDRESS_RECEIVED");
   end if;
   GNAT.IO.Put_Line("");

--  -------------------

--     VN.Message.Request_Address_Block.To_Basic(msgReq, msg);
--     Protocol_Routing_Test.myInterface.Send(msg, Status);
--
--     GNAT.IO.Put("VN message written to send buffer");
--     if Status = VN.OK then
--        GNAT.IO.Put(", Status = VN.OK");
--
--     elsif Status = VN.ERROR_BUFFER_FULL then
--        GNAT.IO.Put(", Status = VN.ERROR_BUFFER_FULL");
--
--     elsif Status = VN.ERROR_NO_ADDRESS_RECEIVED then
--        GNAT.IO.Put(", Status = VN.ERROR_NO_ADDRESS_RECEIVED");
--     end if;
--     GNAT.IO.Put_Line("");


   GNAT.IO.Put_Line("Main function entering infinte wait.");
   loop
      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Milliseconds(3000);
      GNAT.IO.Put_Line("<Main function hearbeat>");
   end loop;
end Protocol_Routing_Test_Main;
