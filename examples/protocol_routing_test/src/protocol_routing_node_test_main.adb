
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing_Test_Node is a test for the Protocol_Routing package.
-- It resembles a Node on the CAN network.

pragma Profile (Ravenscar);

with GNAT.IO;
with Ada.Real_Time;
use Ada.Real_Time;

with VN;
use VN;

with VN.Message;
use VN.Message;

with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with VN.Message.Request_Address_Block;
with VN.Message.Factory;
with VN.Message.Distribute_Route;

with Protocol_Routing_Node_Test;


procedure Protocol_Routing_Node_Test_Main is
   sendStatus : VN.Send_Status;
   recStatus  : VN.Receive_Status;

   msg : VN.Message.VN_Message_Basic;
   msgAssign : VN.Message.Assign_Address.VN_Message_Assign_Address;
   msgRoute  : VN.Message.Distribute_Route.VN_Message_Distribute_Route;

   msgLocalHello : VN.Message.Local_Hello.VN_Message_Local_Hello;
   msgReqAddrBlock   : VN.Message.Request_Address_Block.VN_Message_Request_Address_Block;

   now : Ada.Real_Time.Time;
   myAddress : VN.VN_Logical_Address;
begin


   VN.Message.Assign_Address.To_Assign_Address
     (VN.Message.Factory.Create(VN.Message.Type_Assign_Address), msgAssign);

   VN.Message.Request_Address_Block.To_Request_Address_Block
     (VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block), msgReqAddrBlock);

   VN.Message.Distribute_Route.To_Distribute_Route
     (VN.Message.Factory.Create(VN.Message.Type_Distribute_Route), msgRoute);

   GNAT.IO.New_Line(2);
   GNAT.IO.Put_Line("Hello world! Protocol_Routing_Node_Test started!");

   loop
      now := Ada.Real_Time.Clock;
      delay until now + Ada.Real_Time.Milliseconds(500);
      GNAT.IO.Put_Line("<Main function hearbeat>");

--        Protocol_Routing_Node_Test.CANInterface.Receive(msg, recStatus);
--
--        while recStatus = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or recStatus = VN.MSG_RECEIVED_MORE_AVAILABLE loop
--
--           if msg.Header.Opcode = VN.Message.OPCODE_LOCAL_HELLO then
--              VN.Message.Local_Hello.To_Local_Hello(msg, msgLocalHello);
--              VN.Text_IO.Put("Application received LocalHello, CUUID(1)= " & msgLocalHello.CUUID(1)'img & " component type = ");
--
--              if msgLocalHello.Component_Type = VN.Message.CAS then
--                 VN.Text_IO.Put_Line("CAS");
--              elsif msgLocalHello.Component_Type = VN.Message.SM_L then
--                 VN.Text_IO.Put_Line("SM_L");
--              elsif  msgLocalHello.Component_Type = VN.Message.LS then
--                 VN.Text_IO.Put_Line("LS");
--              elsif msgLocalHello.Component_Type = VN.Message.SM_Gateway then
--                 VN.Text_IO.Put_Line("SM_Gateway");
--              elsif msgLocalHello.Component_Type = VN.Message.SM_x then
--                 VN.Text_IO.Put_Line("SM_x");
--              elsif msgLocalHello.Component_Type = VN.Message.Other then
--                 VN.Text_IO.Put_Line("Other");
--              end if;
--
--           elsif msg.Header.Opcode = VN.Message.OPCODE_ASSIGN_ADDR then
--
--              VN.Message.Assign_Address.To_Assign_Address(msg, msgAssign);
--              myAddress := msgAssign.Assigned_Address;
--
--              VN.Text_IO.Put_Line("Assign address message received from logical address " & msg.Header.Source'Img & " was assigned logical address " & myAddress'Img);
--
--              --Respond with a Request_Address_Block message, just for testing:
--              msg := VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block);
--              VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);
--
--              msgReqAddrBlock.Header.Destination := msgAssign.Header.Source;
--              msgReqAddrBlock.Header.Source := msgAssign.Assigned_Address;
--              msgReqAddrBlock.CUUID := Protocol_Routing_Node_Test.C1;
--
--              VN.Message.Request_Address_Block.To_Basic(msgReqAddrBlock, msg);
--
--              VN.Text_IO.Put_Line("Responds with Request_Address_Block message");
--              Protocol_Routing_Node_Test.CANInterface.Send(msg, sendStatus);
--
--           elsif msg.Header.Opcode = VN.Message.OPCODE_REQUEST_ADDR_BLOCK then
--
--              VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);
--
--              VN.Text_IO.Put_Line("Request_Address_Block message received, CUUID(1)= " & msgReqAddrBlock.CUUID(1)'img &
--                                    " Sender= " & msgReqAddrBlock.Header.Source'Img & " sent to " & msgReqAddrBlock.Header.Destination'Img);
--
--              VN.Text_IO.New_Line;
--           elsif msg.Header.Opcode = VN.Message.OPCODE_DISTRIBUTE_ROUTE then
--              VN.Message.Distribute_Route.To_Distribute_Route(msg, msgRoute);
--
--              VN.Text_IO.Put_Line("Distribute_Route message received, CUUID(1)= " & msgRoute.CUUID(1)'img &
--                                    " logical address = " & msgRoute.Component_Address'Img &
--                                    " Sender= " & msgRoute.Header.Source'Img & " sent to " & msgRoute.Header.Destination'Img);
--
--              --Respond with a Request_Address_Block message, just for testing:
--              msg := VN.Message.Factory.Create(VN.Message.Type_Request_Address_Block);
--              VN.Message.Request_Address_Block.To_Request_Address_Block(msg, msgReqAddrBlock);
--
--              msgReqAddrBlock.Header.Destination := msgRoute.Component_Address;
--              msgReqAddrBlock.Header.Source := myAddress;
--              msgReqAddrBlock.CUUID := Protocol_Routing_Node_Test.C1;
--
--              VN.Message.Request_Address_Block.To_Basic(msgReqAddrBlock, msg);
--
--              VN.Text_IO.Put_Line("Responds (just for testing) with Request_Address_Block message");
--              Protocol_Routing_Node_Test.CANInterface.Send(msg, sendStatus);
--
--           end if;
--
--           Protocol_Routing_Node_Test.CANInterface.Receive(msg, recStatus);
--        end loop;
   end loop;
end Protocol_Routing_Node_Test_Main;
