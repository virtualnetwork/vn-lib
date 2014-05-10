
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
use VN.Message;

with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with VN.Message.Request_Address_Block;
with VN.Message.Factory;

with Protocol_Routing_Test;


procedure Protocol_Routing_Test_Main is


   msg	     : VN.Message.VN_Message_Basic;
   msgAssign : VN.Message.Assign_Address.VN_Message_Assign_Address;
   msgReq    : VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;
   sendStatus    : VN.Send_Status;
   recStatus : VN.Receive_Status;

   msgLocalHello : VN.Message.Local_Hello.VN_Message_Local_Hello;
   msgReqAddrBlock   : VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;

   now : Ada.Real_Time.Time;

   use VN; -- for if Status = VN.OK then
begin

   GNAT.IO.New_Line(2);
   GNAT.IO.Put_Line("Hello world! Protocol_Routing_Test started!");

--     now := Ada.Real_Time.Clock;
--     delay until now + Ada.Real_Time.Milliseconds(5000);

  -- GNAT.IO.Put_Line("5 second wait ended.");

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

--     VN.Message.Assign_Address.To_Basic(msgAssign, msg);
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

--  -------------------

--     VN.Message.Request_Address_Block.To_Basic(msgReq, msg);
--     Protocol_Routing_Test.myInterface.Send(msg, sendStatus);
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
--  -------------------

   GNAT.IO.Put_Line("Main function entering infinte wait.");
   loop
      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Milliseconds(500);
  --    GNAT.IO.Put_Line("<Main function hearbeat>");

      Protocol_Routing_Test.myInterface.Receive(msg, recStatus);

      while recStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or recStatus = VN.MSG_RECEIVED_MORE_AVAILABLE loop
         GNAT.IO.Put_Line("Main function (SM_L) received VN message, Opcode=" & msg.Header.Opcode'Img);

         if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
            VN.Message.Local_Hello.To_Local_Hello(msg, msgLocalHello);
            VN.Text_IO.Put("LocalHello, CUUID(1)= " & msgLocalHello.CUUID(1)'img & " component type = ");

            if msgLocalHello.Component_Type = VN.Message.CAS then
               VN.Text_IO.Put_Line("CAS");
            elsif msgLocalHello.Component_Type = VN.Message.SM_L then
               VN.Text_IO.Put_Line("SM_L");
            elsif  msgLocalHello.Component_Type = VN.Message.LS then
               VN.Text_IO.Put_Line("LS");
            elsif msgLocalHello.Component_Type = VN.Message.SM_Gateway then
               VN.Text_IO.Put_Line("SM_Gateway");
            elsif msgLocalHello.Component_Type = VN.Message.SM_x then
               VN.Text_IO.Put_Line("SM_Gateway");
            elsif msgLocalHello.Component_Type = VN.Message.Other then
               VN.Text_IO.Put_Line("Other");
            end if;

            msg := VN.Message.Factory.Create(VN.Message.Type_Assign_Address);
            VN.Message.Assign_Address.To_Assign_Address(msg, msgAssign);

            msgAssign.Header.Destination := VN.LOGICAL_ADDRES_UNKNOWN;
            msgAssign.Header.Source := 10;
            msgAssign.CUUID := msgLocalHello.CUUID;
            msgAssign.Assigned_Address := 20 + VN.VN_Logical_Address(msgLocalHello.CUUID(1));

            VN.Message.Assign_Address.To_Basic(msgAssign, msg);
            Protocol_Routing_Test.myInterface.Send(msg, sendStatus);

            VN.Text_IO.Put_Line("SM_L assinged Logical address " & msgAssign.Assigned_Address'Img &
              " to CUUD(1)= " & msgAssign.CUUID(1)'Img);

         elsif msg.Header.Opcode = VN.Message.OPCODE_REQUEST_ADDR_BLOCK then

            VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);

            VN.Text_IO.Put_Line("Request_Address_Block received, CUUID(1)= " & msgReqAddrBlock.CUUID(1)'img &
                          " Sender= " & msgReqAddrBlock.Header.Source'Img & " Sent to " & msgReqAddrBlock.Header.Destination'Img);
            VN.Text_IO.New_Line;

         end if;

         Protocol_Routing_Test.myInterface.Receive(msg, recStatus);
      end loop;

   end loop;
end Protocol_Routing_Test_Main;
